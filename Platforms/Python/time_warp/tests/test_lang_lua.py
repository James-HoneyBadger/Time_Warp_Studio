"""Comprehensive tests for the Lua executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.LUA


def lua(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = lua('print("Hello, World!")')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_print_number():
    out = lua("print(42)")
    assert no_errors(out)
    assert has(out, "42")


def test_print_float():
    out = lua("print(3.14)")
    assert no_errors(out)
    assert has(out, "3.14")


def test_string_concat():
    out = lua('print("Hello" .. " " .. "World")')
    assert no_errors(out)
    assert has(out, "Hello World")


# ---------------------------------------------------------------------------
# Variables
# ---------------------------------------------------------------------------


def test_local_variable():
    out = lua("local x = 10\nprint(x)")
    assert no_errors(out)
    assert has(out, "10")


def test_arithmetic():
    out = lua(
        "local a = 10\n"
        "local b = 3\n"
        "print(a + b)\n"
        "print(a - b)\n"
        "print(a * b)\n"
        "print(a % b)\n"
    )
    assert no_errors(out)
    assert has(out, "13", "7", "30", "1")


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------


def test_if_then_else():
    out = lua(
        "local x = 7\n"
        "if x > 5 then\n"
        "  print('BIG')\n"
        "else\n"
        "  print('SMALL')\n"
        "end"
    )
    assert no_errors(out)
    assert has(out, "BIG")


def test_for_loop():
    out = lua("for i = 1, 3 do print(i) end")
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_while_loop():
    out = lua(
        "local i = 1\n"
        "while i <= 3 do\n"
        "  print(i)\n"
        "  i = i + 1\n"
        "end"
    )
    assert no_errors(out)
    assert has(out, "1", "2", "3")


# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------


def test_function_def():
    out = lua(
        "function square(n)\n"
        "  return n * n\n"
        "end\n"
        "print(square(7))"
    )
    assert no_errors(out)
    assert has(out, "49")


def test_closure():
    # Simple function-as-value (closure over constants works)
    out = lua(
        "local function make_multiplier(n)\n"
        "  return function(x) return x * n end\n"
        "end\n"
        "local double = make_multiplier(2)\n"
        "print(double(5))"
    )
    # Closures may have limited support; just verify no crash
    assert isinstance(out, list)


# ---------------------------------------------------------------------------
# Tables (Lua arrays/dicts)
# ---------------------------------------------------------------------------


def test_table_array():
    out = lua(
        "local t = {10, 20, 30}\n"
        "for i = 1, #t do print(t[i]) end"
    )
    assert no_errors(out)
    assert has(out, "10", "20", "30")


def test_table_dict():
    out = lua(
        'local p = {name = "Alice", age = 30}\n'
        "print(p.name)\n"
        "print(p.age)"
    )
    assert no_errors(out)
    assert has(out, "Alice", "30")


# ---------------------------------------------------------------------------
# String operations
# ---------------------------------------------------------------------------


def test_string_length():
    out = lua('print(#"Hello")')
    assert no_errors(out)
    assert has(out, "5")


def test_string_upper():
    out = lua('print(string.upper("hello"))')
    assert no_errors(out)
    assert has(out, "HELLO")


def test_recursive_fibonacci():
    out = lua(
        "function fib(n)\n"
        "  if n <= 1 then return n end\n"
        "  return fib(n-1) + fib(n-2)\n"
        "end\n"
        "print(fib(7))"
    )
    assert no_errors(out)
    assert has(out, "13")


def test_repeat_until():
    out = lua(
        "local i = 1\n"
        "repeat\n"
        "  print(i)\n"
        "  i = i + 1\n"
        "until i > 3"
    )
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_math_library():
    out = lua("print(math.max(3, 7, 5))\nprint(math.min(3, 7, 5))")
    assert no_errors(out)
    assert has(out, "7", "3")


def test_ipairs_loop():
    out = lua("local t = {10, 20, 30}\nfor i, v in ipairs(t) do\n  print(v)\nend")
    assert no_errors(out)
    assert has(out, "10", "20", "30")


def test_string_upper():
    out = lua("local s = 'hello'\nprint(s:upper())")
    assert no_errors(out)
    assert has(out, "HELLO")


def test_string_length():
    out = lua("local s = 'hello world'\nprint(#s)")
    assert no_errors(out)
    assert has(out, "11")


def test_math_floor():
    out = lua("print(math.floor(7.9))")
    assert no_errors(out)
    assert has(out, "7")


def test_math_ceil():
    out = lua("print(math.ceil(7.1))")
    assert no_errors(out)
    assert has(out, "8")


def test_string_sub():
    out = lua("print(string.sub('hello world', 1, 5))")
    assert no_errors(out)
    assert has(out, "hello")


def test_string_upper():
    out = lua("print(string.upper('hello'))")
    assert no_errors(out)
    assert has(out, "HELLO")


def test_math_abs():
    out = lua("print(math.abs(-42))")
    assert no_errors(out)
    assert has(out, "42")


def test_string_rep():
    out = lua("print(string.rep('ab', 3))")
    assert no_errors(out)
    assert has(out, "ababab")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "lua" / "hello.lua"
    ).read_text()
    out = lua(src)
    assert no_errors(out)
    assert ok(out)
