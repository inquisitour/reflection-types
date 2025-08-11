from lexer import Lexer
from parser import Parser
from evaluator import Evaluator

class Interpreter:
    def __init__(self):
        self.ev = Evaluator()

    def eval_text(self, text: str):
        parser = Parser(Lexer(text))
        ast = parser.parse()
        return self.ev.eval(ast)
