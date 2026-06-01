"""Comprehensive tests for the Pascal executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.PASCAL


def pascal(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = pascal(
        "program Hello;\n"
        "begin\n"
        "  writeln('Hello, World!');\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_writeln_integer():
    out = pascal(
        "program Nums;\n"
        "begin\n"
        "  writeln(42);\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "42")


def test_write_no_newline():
    out = pascal(
        "program WNL;\n"
        "begin\n"
        "  write('Hello');\n"
        "  writeln(' World');\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "Hello", "World")


# ---------------------------------------------------------------------------
# Variables and arithmetic
# ---------------------------------------------------------------------------


def test_integer_variables():
    out = pascal(
        "program Vars;\n"
        "var x, y: integer;\n"
        "begin\n"
        "  x := 10;\n"
        "  y := 3;\n"
        "  writeln(x + y);\n"
        "  writeln(x * y);\n"
        "  writeln(x mod y);\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "13", "30", "1")


def test_string_variable():
    out = pascal(
        "program Str;\n"
        "var name: string;\n"
        "begin\n"
        "  name := 'Pascal';\n"
        "  writeln(name);\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "Pascal")


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------


def test_if_then_else():
    out = pascal(
        "program Cond;\n"
        "var x: integer;\n"
        "begin\n"
        "  x := 7;\n"
        "  if x > 5 then writeln('BIG') else writeln('SMALL');\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "BIG")


def test_for_loop():
    out = pascal(
        "program Loop;\n"
        "var i: integer;\n"
        "begin\n"
        "  for i := 1 to 3 do writeln(i);\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_while_loop():
    out = pascal(
        "program WL;\n"
        "var i: integer;\n"
        "begin\n"
        "  i := 1;\n"
        "  while i <= 3 do\n"
        "  begin\n"
        "    writeln(i);\n"
        "    i := i + 1;\n"
        "  end;\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "1", "2", "3")


# ---------------------------------------------------------------------------
# Procedures and functions
# ---------------------------------------------------------------------------


def test_procedure():
    out = pascal(
        "program Proc;\n"
        "procedure greet(name: string);\n"
        "begin\n"
        "  writeln('Hello, ', name, '!');\n"
        "end;\n"
        "begin\n"
        "  greet('World');\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_function():
    out = pascal(
        "program Func;\n"
        "function square(n: integer): integer;\n"
        "begin\n"
        "  square := n * n;\n"
        "end;\n"
        "begin\n"
        "  writeln(square(7));\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "49")


def test_recursive_factorial():
    out = pascal(
        "program Fact;\n"
        "function factorial(n: integer): integer;\n"
        "begin\n"
        "  if n <= 1 then factorial := 1\n"
        "  else factorial := n * factorial(n-1);\n"
        "end;\n"
        "begin\n"
        "  writeln(factorial(5));\n"
        "  writeln(factorial(10));\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "120")
    assert has(out, "3628800")


def test_recursive_fibonacci():
    out = pascal(
        "program Fib;\n"
        "function fib(n: integer): integer;\n"
        "begin\n"
        "  if n = 0 then fib := 0\n"
        "  else if n = 1 then fib := 1\n"
        "  else fib := fib(n-1) + fib(n-2);\n"
        "end;\n"
        "begin\n"
        "  writeln(fib(0));\n"
        "  writeln(fib(1));\n"
        "  writeln(fib(7));\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "0")
    assert has(out, "1")
    assert has(out, "13")


def test_repeat_until():
    out = pascal(
        "program RepUntil;\n"
        "var i: integer;\n"
        "begin\n"
        "  i := 1;\n"
        "  repeat\n"
        "    writeln(i);\n"
        "    i := i + 1;\n"
        "  until i > 3;\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "1")
    assert has(out, "2")
    assert has(out, "3")


def test_nested_if_else():
    out = pascal(
        "program NestedIf;\n"
        "var x: integer;\n"
        "begin\n"
        "  x := 5;\n"
        "  if x > 10 then writeln('big')\n"
        "  else if x > 3 then writeln('medium')\n"
        "  else writeln('small');\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "medium")


def test_case_statement():
    out = pascal(
        "program t;\n"
        "var x: integer;\n"
        "begin\n"
        "  x := 2;\n"
        "  case x of\n"
        "    1: writeln('one');\n"
        "    2: writeln('two');\n"
        "    3: writeln('three');\n"
        "  end;\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "two")


def test_case_with_else():
    out = pascal(
        "program t;\n"
        "var x: integer;\n"
        "begin\n"
        "  x := 9;\n"
        "  case x of\n"
        "    1: writeln('one');\n"
        "    2: writeln('two');\n"
        "    else writeln('other');\n"
        "  end;\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "other")


def test_string_functions():
    out = pascal(
        "program t;\n"
        "begin\n"
        "  writeln(length('hello'));\n"
        "  writeln(upcase('world'));\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "5", "WORLD")


def test_ord_chr():
    out = pascal(
        "program test;\nbegin\n  writeln(ord('A'));\n  writeln(chr(66));\nend."
    )
    assert no_errors(out)
    assert has(out, "65", "B")


def test_mod_div():
    out = pascal(
        "program test;\nvar x: integer;\nbegin\n  x := 10;\n  writeln(x mod 3);\n  writeln(x div 3);\nend."
    )
    assert no_errors(out)
    assert has(out, "1", "3")


def test_sqr_abs():
    out = pascal(
        "program test;\nbegin\n  writeln(sqr(7));\n  writeln(abs(-42));\nend."
    )
    assert no_errors(out)
    assert has(out, "49", "42")


def test_odd_function():
    out = pascal(
        "program t;\n"
        "begin\n"
        "  writeln(odd(7));\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "1")


def test_trunc_function():
    out = pascal(
        "program t;\n"
        "begin\n"
        "  writeln(trunc(3.7));\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "3")


def test_for_loop_write():
    out = pascal(
        "program t;\n"
        "var i: integer;\n"
        "begin\n"
        "  for i := 1 to 5 do\n"
        "    write(i, ' ');\n"
        "  writeln;\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "1", "2", "3", "4", "5")


def test_upcase():
    out = pascal(
        "program t;\n"
        "begin\n"
        "  writeln(upcase('hello'));\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "HELLO")


def test_chr_function():
    out = pascal(
        "program t;\n"
        "begin\n"
        "  writeln(chr(65));\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "A")


def test_ord_function():
    out = pascal(
        "program t;\n"
        "begin\n"
        "  writeln(ord('A'));\n"
        "end."
    )
    assert no_errors(out)
    assert has(out, "65")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "pascal" / "hello.pas"
    ).read_text()
    out = pascal(src)
    assert no_errors(out)
    assert ok(out)
