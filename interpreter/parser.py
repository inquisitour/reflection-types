from lexer import Lexer, Token
from ast_nodes import Integer, Variable, Lambda, Application, Record, Cond, Access

class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.cur = lexer.get_next_token()
        self.peek = lexer.get_next_token()
        self.in_paren = False  # track parentheses for lambda

    def eat(self, ttype):
        if self.cur.type == ttype:
            self.cur = self.peek
            self.peek = self.lexer.get_next_token()
        else:
            raise SyntaxError(f'Expected {ttype}, got {self.cur.type}')

    def parse(self):
        return self.parse_expr()

    def parse_expr(self):
        # Lambda only inside parentheses
        if self.in_paren and self.cur.type == Token.NAME and self.peek.type == Token.DOT:
            param = self.cur.value
            self.eat(Token.NAME)
            self.eat(Token.DOT)
            return Lambda(param, self.parse_expr())
        # Conditional
        if self.cur.type == Token.NAME and self.cur.value == 'cond':
            self.eat(Token.NAME)
            cond_node = self.parse_basic()
            then_node = self.parse_basic()
            else_node = self.parse_basic()
            return Cond(cond_node, then_node, else_node)
        return self.parse_apply()

    def parse_apply(self):
        node = self.parse_basic()
        # Left-associative application
        while self.cur.type in (Token.INTEGER, Token.NAME, Token.LPAREN, Token.LBRACE, Token.LBRACKET):
            node = Application(node, self.parse_basic())
        return node

    def parse_basic(self):
        # Literals, variables, parentheses, records
        if self.cur.type == Token.INTEGER:
            value = self.cur.value
            self.eat(Token.INTEGER)
            node = Integer(value)
        elif self.cur.type == Token.NAME:
            name = self.cur.value
            self.eat(Token.NAME)
            node = Variable(name)
        elif self.cur.type == Token.LPAREN:
            self.eat(Token.LPAREN)
            prev = self.in_paren
            self.in_paren = True
            node = self.parse_expr()
            self.in_paren = prev
            self.eat(Token.RPAREN)
        elif self.cur.type in (Token.LBRACE, Token.LBRACKET):
            eager = (self.cur.type == Token.LBRACKET)
            self.eat(self.cur.type)
            bindings = []
            # parse bindings
            while self.cur.type not in (Token.RBRACE if not eager else Token.RBRACKET):
                field = self.cur.value
                self.eat(Token.NAME)
                self.eat(Token.EQUAL)
                expr = self.parse_expr()
                bindings.append((field, expr))
                if self.cur.type == Token.COMMA:
                    self.eat(Token.COMMA)
                else:
                    break
            self.eat(Token.RBRACE if not eager else Token.RBRACKET)
            node = Record(bindings, eager)
        else:
            raise SyntaxError(f'Unexpected token: {self.cur}')

        # Field access postfix
        while self.cur.type == Token.DOT:
            self.eat(Token.DOT)
            if self.cur.type != Token.NAME:
                raise SyntaxError("Expected field name after '.'")
            field = self.cur.value
            self.eat(Token.NAME)
            node = Access(node, field)

        return node
