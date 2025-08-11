class ASTNode: pass
class Integer(ASTNode):
    def __init__(self, value): self.value = value
class Variable(ASTNode):
    def __init__(self, name): self.name = name
class Lambda(ASTNode):
    def __init__(self, param, body): self.param = param; self.body = body
class Application(ASTNode):
    def __init__(self, func, arg): self.func = func; self.arg = arg
class Record(ASTNode):
    def __init__(self, bindings, eager): self.bindings = bindings; self.eager = eager
class Cond(ASTNode):
    def __init__(self, cond, then, else_): self.cond = cond; self.then = then; self.else_ = else_
class Access(ASTNode):
    def __init__(self, record, field): self.record = record; self.field = field
