class Token:
    INTEGER = 'INTEGER'; NAME = 'NAME'; DOT = 'DOT'
    LPAREN = 'LPAREN'; RPAREN = 'RPAREN'
    LBRACE = 'LBRACE'; RBRACE = 'RBRACE'
    LBRACKET = 'LBRACKET'; RBRACKET = 'RBRACKET'
    EQUAL = 'EQUAL'; COMMA = 'COMMA'
    EOF = 'EOF'

    def __init__(self, type_, value=None):
        self.type = type_; self.value = value
    def __repr__(self): return f'Token({self.type}, {self.value})'

class Lexer:
    def __init__(self, text):
        self.text, self.pos = text, 0
        self.current = text[0] if text else None

    def advance(self):
        self.pos += 1
        self.current = self.text[self.pos] if self.pos < len(self.text) else None

    def skipws(self):
        while self.current and self.current.isspace(): self.advance()

    def integer(self):
        s = ''
        while self.current and self.current.isdigit():
            s += self.current
            self.advance()
        return int(s)

    def name(self):
        s = ''
        while self.current and self.current.isalpha():
            s += self.current
            self.advance()
        return s

    def get_next_token(self):
        while self.current:
            if self.current.isspace():
                self.skipws(); continue
            if self.current.isdigit(): return Token(Token.INTEGER, self.integer())
            if self.current.isalpha(): return Token(Token.NAME, self.name())
            if self.current == '.':
                self.advance()
                return Token(Token.DOT, '.')
            if self.current == '(': self.advance(); return Token(Token.LPAREN, '(')
            if self.current == ')': self.advance(); return Token(Token.RPAREN, ')')
            if self.current == '{': self.advance(); return Token(Token.LBRACE, '{')
            if self.current == '}': self.advance(); return Token(Token.RBRACE, '}')
            if self.current == '[': self.advance(); return Token(Token.LBRACKET, '[')
            if self.current == ']': self.advance(); return Token(Token.RBRACKET, ']')
            if self.current == '=': self.advance(); return Token(Token.EQUAL, '=')
            if self.current == ',': self.advance(); return Token(Token.COMMA, ',')
            raise SyntaxError(f'Unknown char: {self.current}')
        return Token(Token.EOF, None)