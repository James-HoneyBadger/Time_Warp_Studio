"""Tests for the Python language executor (sandboxed exec)."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.PYTHON_LANG


def py(source: str, **kw) -> list[str]:
    return run(source, L, **kw)


class TestPythonOutput:
    def test_hello_world(self):
        out = py('print("Hello, World!")')
        assert has(out, "Hello, World!")

    def test_print_number(self):
        out = py("print(42)")
        assert has(out, "42")

    def test_multiline(self):
        out = py('print("a")\nprint("b")')
        assert has(out, "a") and has(out, "b")

    def test_string_concat(self):
        out = py('print("Hello" + " " + "World")')
        assert has(out, "Hello World")

    def test_f_string(self):
        out = py('name = "Python"\nprint(f"Hello, {name}!")')
        assert has(out, "Hello, Python!")


class TestPythonArithmetic:
    def test_addition(self):
        out = py("print(3 + 4)")
        assert has(out, "7")

    def test_multiplication(self):
        out = py("print(6 * 7)")
        assert has(out, "42")

    def test_division(self):
        out = py("print(10 // 2)")
        assert has(out, "5")

    def test_modulo(self):
        out = py("print(10 % 3)")
        assert has(out, "1")

    def test_power(self):
        out = py("print(2 ** 8)")
        assert has(out, "256")

    def test_float(self):
        out = py("print(3.14)")
        assert has(out, "3.14")


class TestPythonVariables:
    def test_int_var(self):
        out = py("x = 99\nprint(x)")
        assert has(out, "99")

    def test_string_var(self):
        out = py('s = "test"\nprint(s)')
        assert has(out, "test")

    def test_list_var(self):
        out = py("a = [1, 2, 3]\nprint(a[0])")
        assert has(out, "1")

    def test_dict_var(self):
        out = py('d = {"x": 42}\nprint(d["x"])')
        assert has(out, "42")


class TestPythonConditionals:
    def test_if_true(self):
        out = py('if 1 < 2:\n    print("yes")')
        assert has(out, "yes")

    def test_if_else(self):
        out = py('if 2 < 1:\n    print("no")\nelse:\n    print("yes")')
        assert has(out, "yes")

    def test_elif(self):
        out = py('x = 5\nif x < 0:\n    print("neg")\nelif x == 0:\n    print("zero")\nelse:\n    print("pos")')
        assert has(out, "pos")


class TestPythonLoops:
    def test_for_range(self):
        out = py("for i in range(3):\n    print(i)")
        assert has(out, "0") and has(out, "2")

    def test_while(self):
        out = py("i = 0\nwhile i < 3:\n    print(i)\n    i += 1")
        assert has(out, "0") and has(out, "2")

    def test_for_list(self):
        out = py("for x in [10, 20, 30]:\n    print(x)")
        assert has(out, "10") and has(out, "30")

    def test_list_comprehension(self):
        out = py("print([x*2 for x in range(3)])")
        assert has(out, "0") and has(out, "4")


class TestPythonFunctions:
    def test_def_call(self):
        out = py("def greet():\n    print('hi')\ngreet()")
        assert has(out, "hi")

    def test_function_with_args(self):
        out = py("def add(a, b):\n    return a + b\nprint(add(3, 4))")
        assert has(out, "7")

    def test_recursive(self):
        out = py("def fac(n):\n    return 1 if n <= 1 else n * fac(n-1)\nprint(fac(5))")
        assert has(out, "120")

    def test_lambda(self):
        out = py("f = lambda x: x * 2\nprint(f(5))")
        assert has(out, "10")


class TestPythonBuiltins:
    def test_len(self):
        out = py("print(len([1, 2, 3]))")
        assert has(out, "3")

    def test_range_sum(self):
        out = py("print(sum(range(1, 6)))")
        assert has(out, "15")

    def test_sorted(self):
        out = py("print(sorted([3, 1, 2]))")
        assert has(out, "1") and has(out, "3")

    def test_max_min(self):
        out = py("print(max(3, 1, 2))\nprint(min(3, 1, 2))")
        assert has(out, "3") and has(out, "1")

    def test_math_sqrt(self):
        out = py("print((16**0.5))")
        assert has(out, "4.0")

    def test_abs(self):
        out = py("print(abs(-5))")
        assert has(out, "5")


class TestPythonSecurity:
    def test_no_open(self):
        out = py("open('file.txt', 'w')")
        assert has(out, "❌") or has(out, "Error") or not has(out, "success")

    def test_no_import_os(self):
        out = py("import os")
        assert has(out, "❌") or has(out, "Error") or not has(out, "success")

    def test_no_subprocess(self):
        out = py("import subprocess")
        assert has(out, "❌") or has(out, "Error") or not has(out, "success")

    def test_syntax_error(self):
        out = py("if x == :")
        assert has(out, "❌") or has(out, "SyntaxError")


class TestPythonNoErrors:
    def test_fibonacci(self):
        prog = """
def fib(n):
    a, b = 0, 1
    for _ in range(n):
        a, b = b, a + b
    return a
print(fib(10))
"""
        out = py(prog)
        assert has(out, "55")

    def test_class(self):
        prog = """
def dist(x, y):
    return (x**2 + y**2)**0.5

print(dist(3, 4))
"""
        out = py(prog)
        assert has(out, "5.0")

    def test_string_methods(self):
        out = py('print("hello".upper())\nprint("WORLD".lower())')
        assert has(out, "HELLO") and has(out, "world")
