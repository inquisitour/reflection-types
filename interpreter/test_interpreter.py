import unittest
from interpreter import Interpreter
from values import IntegerValue

class TestInterpreter(unittest.TestCase):
    def setUp(self):
        self.i = Interpreter()

    def eval(self, text):
        res = self.i.eval_text(text)
        return res.value if isinstance(res, IntegerValue) else res

    def test_int(self): self.assertEqual(self.eval('42'), 42)
    def test_lambda(self): self.assertEqual(self.eval('(x.x) 5'), 5)
    def test_plus(self): self.assertEqual(self.eval('plus 2 3'), 5)

    def test_access_eager(self): self.assertEqual(self.eval('[a=1] a'), 1)
    def test_access_lazy(self): self.assertEqual(self.eval('{a=1} a'), 1)
    def test_field_access(self): self.assertEqual(self.eval('[r={x=10}, y = r.x] y'), 10)

if __name__ == '__main__':
    unittest.main()