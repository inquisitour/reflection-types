class Value: pass

class IntegerValue(Value):
    def __init__(self, value): self.value = value

class FunctionValue(Value):
    def __init__(self, param, body, env):
        self.param = param; self.body = body; self.env = env

class BuiltinValue(Value):
    def __init__(self, name, func, arity, args=None):
        self.name = name; self.func = func; self.arity = arity; self.args = args or []

class RecordValue(Value):
    def __init__(self, parent_env, vals, exprs, eager):
        self.env = parent_env
        self.eager = eager
        self.vals = vals       # dict of name->Value (eager)
        self.exprs = exprs     # dict of name->AST (lazy)

class Thunk(Value):
    def __init__(self, stmt, env, evaluator):
        self.stmt = stmt; self.env = env; self.evaluator = evaluator
        self._value = None; self._done = False
    def force(self):
        if not self._done:
            self._value = self.evaluator.eval(self.stmt, self.env)
            self._done = True
        return self._value
    

class EnvironmentWrapper(Value):
    """Wraps a value with an environment context for record applications"""
    def __init__(self, value, env):
        self.value = value
        self.env = env