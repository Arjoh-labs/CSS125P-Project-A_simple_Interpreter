
import re
import sys

# ----------------------------------------------------------------------
# The "dictionary" of our language
# ----------------------------------------------------------------------
KEYWORDS = {"integer", "double", "if", "output"}           # reserved words
SYMBOLS = ["<<", "==", "!=", ":=", ":", ";", "=", "+", "-", "<", ">", "(", ")"]
QUOTES = '"\u201c\u201d'                                   # " and Word's curly quotes
SINGLE_DIGIT_ONLY = True                                   # spec: single digit integers


class HLError(Exception):
    """One mistake found in the HL program."""
    def __init__(self, line, message):
        super().__init__(message)
        self.line = line
        self.message = message


# ----------------------------------------------------------------------
# JOB 1: remove the spaces
# ----------------------------------------------------------------------
def remove_spaces(text):
    """Remove spaces/tabs but keep the words inside "quotes" and keep new lines."""
    out, inside = [], False
    for ch in text:
        if ch in QUOTES:
            inside = not inside
        if ch in " \t\r" and not inside:
            continue
        out.append(ch)
    return "".join(out)


# ----------------------------------------------------------------------
# JOB 2: cut the text into tiny pieces called TOKENS (the "lexer")
# ----------------------------------------------------------------------
def tokenize(text, errors):
    tokens, i, line, n = [], 0, 1, len(text)
    while i < n:
        c = text[i]
        if c == "\n":
            line += 1; i += 1; continue
        if c.isspace():
            i += 1; continue

        if c in QUOTES:                                     # a string
            j = i + 1
            while j < n and text[j] not in QUOTES and text[j] != "\n":
                j += 1
            if j >= n or text[j] == "\n":
                errors.append(HLError(line, "string is missing its closing quote"))
                i = j; continue
            tokens.append(("STRING", text[i + 1:j], line)); i = j + 1; continue

        if c.isdigit():                                     # a number
            s = re.match(r"\d+(\.\d+)?", text[i:]).group()
            tokens.append(("DOUBLE" if "." in s else "INT", s, line)); i += len(s); continue

        if c.isalpha() or c == "_":                         # a word
            w = re.match(r"[A-Za-z_]\w*", text[i:]).group()
            if w.lower() in KEYWORDS:
                tokens.append(("KEYWORD", w.lower(), line))
            else:
                tokens.append(("ID", w, line))
            i += len(w); continue

        for s in SYMBOLS:                                   # a symbol (longest first)
            if text.startswith(s, i):
                tokens.append(("SYMBOL", s, line)); i += len(s); break
        else:
            errors.append(HLError(line, f"unknown character '{c}'")); i += 1
    return tokens


# ----------------------------------------------------------------------
# JOB 3: check the grammar (the "parser") and build a to-do list
# ----------------------------------------------------------------------
class Parser:
    """
    Grammar (the rules of HL):
      declaration := ID ':' ('integer'|'double') ';'
      assignment  := ID (':=' | '=') expression ';'
      output      := 'output' '<<' (STRING | expression) ';'
      if          := 'if' '(' expression (> | < | == | !=) expression ')' statement
      expression  := operand (('+' | '-') operand)*
      operand     := INT | DOUBLE | ID
    """
    def __init__(self, tokens):
        self.t, self.pos = tokens, 0
        self.errors, self.program, self.types = [], [], {}

    # --- little helpers -------------------------------------------------
    def peek(self):
        return self.t[self.pos] if self.pos < len(self.t) else ("EOF", "", self.last_line())

    def last_line(self):
        return self.t[-1][2] if self.t else 1

    def next(self):
        tok = self.peek(); self.pos += 1; return tok

    def expect(self, value, what=None):
        kind, val, line = self.peek()
        if val != value or kind in ("STRING", "EOF"):
            raise HLError(line, f"expected '{value}'" + (f" {what}" if what else "") + f" but found '{val or 'end of file'}'")
        return self.next()

    # --- the main loop: one statement at a time -------------------------
    def parse(self):
        while self.pos < len(self.t):
            try:
                self.program.append(self.statement(allow_decl=True))
            except HLError as e:
                self.errors.append(e)
                # skip ahead to the end of the broken statement (unless we already passed its ';')
                if not (self.pos > 0 and self.t[self.pos - 1][1] == ";"):
                    while self.pos < len(self.t) and self.next()[1] != ";":
                        pass
        return self.program

    def statement(self, allow_decl=False):
        kind, val, line = self.peek()
        if kind == "KEYWORD" and val == "output":
            return self.output_stmt()
        if kind == "KEYWORD" and val == "if":
            return self.if_stmt()
        if kind == "ID":
            nxt = self.t[self.pos + 1][1] if self.pos + 1 < len(self.t) else ""
            if nxt == ":" :
                if not allow_decl:
                    raise HLError(line, "a declaration cannot be inside an 'if'")
                return self.declaration()
            return self.assignment()
        raise HLError(line, f"unexpected '{val or 'end of file'}' at start of a statement")

    def declaration(self):
        _, name, line = self.next()
        self.expect(":")
        kind, typ, tline = self.next()
        if kind != "KEYWORD" or typ not in ("integer", "double"):
            raise HLError(tline, "expected a data type ('integer' or 'double')")
        self.expect(";")
        if name in self.types:
            raise HLError(line, f"variable '{name}' is already declared")
        self.types[name] = typ
        return ("decl", name, typ)

    def assignment(self):
        _, name, line = self.next()
        kind, op, oline = self.peek()
        if op not in (":=", "=") or kind != "SYMBOL":
            raise HLError(oline, f"expected ':=' after '{name}' but found '{op or 'end of file'}'")
        self.next()
        node, typ = self.expression()
        self.expect(";")
        if name not in self.types:
            raise HLError(line, f"variable '{name}' was not declared")
        if self.types[name] == "integer" and typ == "double":
            raise HLError(line, f"cannot put a double value into integer '{name}'")
        return ("assign", name, node)

    def output_stmt(self):
        self.next()
        self.expect("<<", "after 'output'")
        kind, val, line = self.peek()
        if kind == "STRING":
            self.next(); self.expect(";")
            return ("out_str", val)
        node, _ = self.expression()
        self.expect(";")
        return ("out_val", node)

    def if_stmt(self):
        self.next()
        self.expect("(", "after 'if'")
        left, _ = self.expression()
        kind, op, line = self.next()
        if kind != "SYMBOL" or op not in (">", "<", "==", "!="):
            raise HLError(line, "expected a comparison (>, <, ==, !=)")
        right, _ = self.expression()
        self.expect(")")
        body = self.statement()
        return ("if", left, op, right, body)

    def expression(self):
        node, typ = self.operand()
        while self.peek()[0] == "SYMBOL" and self.peek()[1] in ("+", "-"):
            op = self.next()[1]
            rnode, rtyp = self.operand()
            typ = "double" if "double" in (typ, rtyp) else "integer"
            node = ("bin", op, node, rnode)
        return node, typ

    def operand(self):
        kind, val, line = self.next()
        if kind == "INT":
            if SINGLE_DIGIT_ONLY and len(val) > 1:
                raise HLError(line, f"integer '{val}' has more than one digit")
            return ("num", int(val)), "integer"
        if kind == "DOUBLE":
            if len(val.split(".")[1]) > 2:
                raise HLError(line, f"double '{val}' has more than 2 decimal places")
            return ("num", float(val)), "double"
        if kind == "ID":
            if val not in self.types:
                raise HLError(line, f"variable '{val}' was not declared")
            return ("var", val), self.types[val]
        raise HLError(line, f"expected a number or variable but found '{val or 'end of file'}'")


# ----------------------------------------------------------------------
# Run the program (only when there are no errors)
# ----------------------------------------------------------------------
def evaluate(node, memory):
    if node[0] == "num":
        return node[1]
    if node[0] == "var":
        return memory[node[1]]
    _, op, left, right = node
    l, r = evaluate(left, memory), evaluate(right, memory)
    result = l + r if op == "+" else l - r
    return round(result, 2) if isinstance(result, float) else result


def show(value):
    return f"{value:.2f}" if isinstance(value, float) else str(value)


def run(program, types):
    memory = {}

    def do(stmt):
        kind = stmt[0]
        if kind == "decl":
            memory[stmt[1]] = 0.0 if stmt[2] == "double" else 0
        elif kind == "assign":
            value = evaluate(stmt[2], memory)
            memory[stmt[1]] = float(value) if types[stmt[1]] == "double" else value
        elif kind == "out_str":
            print(stmt[1])
        elif kind == "out_val":
            print(show(evaluate(stmt[1], memory)))
        elif kind == "if":
            _, left, op, right, body = stmt
            l, r = round(evaluate(left, memory), 2), round(evaluate(right, memory), 2)
            if {">": l > r, "<": l < r, "==": l == r, "!=": l != r}[op]:
                do(body)

    for stmt in program:
        do(stmt)


# ----------------------------------------------------------------------
# main
# ----------------------------------------------------------------------
def main():
    if len(sys.argv) != 2:
        print("Usage: python HLInt.py <program.HL>"); return
    try:
        with open(sys.argv[1], encoding="utf-8-sig") as f:
            source = f.read()
    except OSError:
        print(f"Cannot open '{sys.argv[1]}'"); return

    # Job 1
    with open("NOSPACES.TXT", "w", encoding="utf-8") as f:
        f.write(remove_spaces(source))

    # Job 2
    errors = []
    tokens = tokenize(source, errors)
    with open("RES_SYM.TXT", "w", encoding="utf-8") as f:
        for kind, val, _ in tokens:
            if kind in ("KEYWORD", "SYMBOL"):
                f.write(val + "\n")

    # Job 3
    parser = Parser(tokens)
    program = parser.parse()
    errors += parser.errors
    if errors:
        print("ERROR")
        for e in sorted(errors, key=lambda e: e.line):
            print(f"  line {e.line}: {e.message}")
    else:
        run(program, parser.types)
        print("NO ERROR(S) FOUND")


if __name__ == "__main__":
    main()
