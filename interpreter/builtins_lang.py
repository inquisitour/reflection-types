from values import BuiltinValue

def _plus(x, y): return x + y

def _minus(x, y): return x - y

def _mult(x, y): return x * y

def _div(x, y): return x // y

# fresh built-ins for each environment
def get_builtins():
    return {
        'plus':  BuiltinValue('plus',  _plus,  2),
        'minus': BuiltinValue('minus', _minus, 2),
        'mult':  BuiltinValue('mult',  _mult,  2),
        'div':   BuiltinValue('div',   _div,   2),
    }