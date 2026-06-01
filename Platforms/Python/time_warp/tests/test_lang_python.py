"""Comprehensive tests for the Python (sandboxed) executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.PYTHON_LANG


def py(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = py('print("Hello, World!")')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_fstring():
    out = py('name = "World"\nprint(f"Hello, {name}!")')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_print_number():
    out = py("print(42)")
    assert no_errors(out)
    assert has(out, "42")


def test_print_float():
    out = py("print(3.14)")
    assert no_errors(out)
    assert has(out, "3.14")


# ---------------------------------------------------------------------------
# Variables and arithmetic
# ---------------------------------------------------------------------------


def test_arithmetic():
    out = py(
        "a, b = 10, 3\n"
        "print(a + b)\n"
        "print(a - b)\n"
        "print(a * b)\n"
        "print(a % b)\n"
        "print(a // b)\n"
    )
    assert no_errors(out)
    assert has(out, "13", "7", "30", "1", "3")


def test_power():
    out = py("print(2 ** 10)")
    assert no_errors(out)
    assert has(out, "1024")


# ---------------------------------------------------------------------------
# String operations
# ---------------------------------------------------------------------------


def test_string_upper():
    out = py('print("hello".upper())')
    assert no_errors(out)
    assert has(out, "HELLO")


def test_string_len():
    out = py('print(len("Hello"))')
    assert no_errors(out)
    assert has(out, "5")


def test_string_split():
    out = py('for w in "a b c".split(): print(w)')
    assert no_errors(out)
    assert has(out, "a", "b", "c")


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------


def test_if_elif_else():
    out = py(
        "x = 7\n"
        "if x > 10:\n"
        "    print('big')\n"
        "elif x > 4:\n"
        "    print('medium')\n"
        "else:\n"
        "    print('small')\n"
    )
    assert no_errors(out)
    assert has(out, "medium")


def test_for_range():
    out = py("for i in range(1, 4):\n    print(i)")
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_while_loop():
    out = py(
        "i = 0\n"
        "while i < 3:\n"
        "    print(i)\n"
        "    i += 1\n"
    )
    assert no_errors(out)
    assert has(out, "0", "1", "2")


# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------


def test_def_function():
    out = py(
        "def square(n):\n"
        "    return n * n\n"
        "print(square(7))\n"
    )
    assert no_errors(out)
    assert has(out, "49")


def test_lambda():
    out = py("double = lambda n: n * 2\nprint(double(6))")
    assert no_errors(out)
    assert has(out, "12")


def test_recursive():
    out = py(
        "def factorial(n):\n"
        "    return 1 if n <= 1 else n * factorial(n - 1)\n"
        "print(factorial(5))\n"
    )
    assert no_errors(out)
    assert has(out, "120")


# ---------------------------------------------------------------------------
# Lists
# ---------------------------------------------------------------------------


def test_list_comprehension():
    out = py("print([x * 2 for x in range(1, 4)])")
    assert no_errors(out)
    assert has(out, "2", "4", "6")


def test_list_methods():
    out = py(
        "lst = [3, 1, 4, 1, 5]\n"
        "print(len(lst))\n"
        "print(sorted(lst))\n"
    )
    assert no_errors(out)
    assert has(out, "5")


# ---------------------------------------------------------------------------
# Dictionaries
# ---------------------------------------------------------------------------


def test_dict():
    out = py(
        'p = {"name": "Alice", "age": 30}\n'
        "print(p['name'])\n"
        "print(p['age'])\n"
    )
    assert no_errors(out)
    assert has(out, "Alice", "30")


# ---------------------------------------------------------------------------
# Security: dangerous builtins are blocked
# ---------------------------------------------------------------------------


def test_exec_is_blocked():
    out = py('exec("import os")')
    # exec is not in safe_builtins — must raise NameError
    assert not no_errors(out)  # must produce an error


def test_import_os_blocked():
    out = py("import os\nprint(os.getcwd())")
    # os is not in the allowed import list
    assert not no_errors(out)


def test_subclasses_escape_blocked():
    # Classic sandbox escape: object.__subclasses__() finds subprocess.Popen
    out = py(
        "subs = object.__subclasses__()\n"
        "Popen = [c for c in subs if c.__name__ == 'Popen'][0]\n"
        "p = Popen(['id'], stdout=-1, stderr=-1)\n"
        "out, _ = p.communicate()\n"
        "print(out.decode())"
    )
    # Must be blocked by the AST safety checker
    assert not no_errors(out)
    assert any("__subclasses__" in line for line in out)


def test_globals_escape_blocked():
    # function.__globals__ leaks the outer scope
    out = py("f = lambda: None\nprint(f.__globals__)")
    assert not no_errors(out)
    assert any("__globals__" in line for line in out)


def test_class_definition_works():
    # __build_class__ is available so class defs are supported
    out = py(
        "class Point:\n"
        "  def __init__(self, x, y):\n"
        "    self.x = x\n"
        "    self.y = y\n"
        "  def __repr__(self):\n"
        "    return f'({self.x},{self.y})'\n"
        "p = Point(3, 4)\n"
        "print(p)"
    )
    assert no_errors(out)
    assert has(out, "3", "4")


def test_list_comprehension():
    out = py("x = [i ** 2 for i in range(1, 6)]\nprint(x)")
    assert no_errors(out)
    assert has(out, "1", "4", "9", "16", "25")


def test_fstring():
    out = py("def greet(name):\n    return f'Hello, {name}!'\nprint(greet('World'))")
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_sorted_list():
    out = py("print(sorted([3, 1, 4, 1, 5, 9, 2, 6]))")
    assert no_errors(out)
    assert has(out, "1", "2", "3", "4", "5", "6", "9")


def test_lambda_expression():
    out = py("f = lambda x: x*x\nprint(f(7))")
    assert no_errors(out)
    assert has(out, "49")


def test_list_comprehension():
    out = py("print([x*2 for x in range(5)])")
    assert no_errors(out)
    assert has(out, "0", "2", "4", "6", "8")


def test_set_unique():
    out = py("print(len(set([1,1,2,2,3])))")
    assert no_errors(out)
    assert has(out, "3")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "python" / "hello.py"
    ).read_text()
    out = py(src)
    assert ok(out)
