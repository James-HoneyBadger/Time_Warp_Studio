"""Comprehensive tests for the JavaScript executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.JAVASCRIPT


def js(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = js('console.log("Hello, World!");')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_print_number():
    out = js("console.log(42);")
    assert no_errors(out)
    assert has(out, "42")


def test_template_literal():
    out = js('const x = 5; console.log(`value is ${x}`);')
    assert no_errors(out)
    assert has(out, "value is 5")


# ---------------------------------------------------------------------------
# Variables and arithmetic
# ---------------------------------------------------------------------------


def test_let_const():
    out = js(
        "let a = 10;\n"
        "const b = 3;\n"
        "console.log(a + b);\n"
        "console.log(a * b);\n"
        "console.log(a % b);\n"
    )
    assert no_errors(out)
    assert has(out, "13", "30", "1")


def test_string_concat():
    out = js('let s = "Hello" + " " + "World"; console.log(s);')
    assert no_errors(out)
    assert has(out, "Hello World")


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------


def test_if_else():
    out = js(
        "let x = 7;\n"
        "if (x > 5) { console.log('BIG'); } else { console.log('SMALL'); }"
    )
    assert no_errors(out)
    assert has(out, "BIG")


def test_for_loop():
    out = js("for (let i = 1; i <= 3; i++) { console.log(i); }")
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_while_loop():
    out = js(
        "let i = 0;\n"
        "while (i < 3) { console.log(i); i++; }"
    )
    assert no_errors(out)
    assert has(out, "0", "1", "2")


# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------


def test_function_declaration():
    out = js(
        "function square(n) { return n * n; }\n"
        "console.log(square(7));"
    )
    assert no_errors(out)
    assert has(out, "49")


def test_arrow_function():
    out = js(
        "const double = n => n * 2;\n"
        "console.log(double(6));"
    )
    assert no_errors(out)
    assert has(out, "12")


def test_recursive():
    out = js(
        "function factorial(n) { return n <= 1 ? 1 : n * factorial(n - 1); }\n"
        "console.log(factorial(5));"
    )
    assert no_errors(out)
    assert has(out, "120")


# ---------------------------------------------------------------------------
# Arrays
# ---------------------------------------------------------------------------


def test_array_map():
    out = js(
        "const arr = [1, 2, 3];\n"
        "arr.map(x => x * 2).forEach(x => console.log(x));"
    )
    assert no_errors(out)
    assert has(out, "2", "4", "6")


def test_array_filter():
    out = js(
        "const arr = [1, 2, 3, 4, 5];\n"
        "arr.filter(x => x % 2 === 0).forEach(x => console.log(x));"
    )
    assert no_errors(out)
    assert has(out, "2", "4")


# ---------------------------------------------------------------------------
# Objects
# ---------------------------------------------------------------------------


def test_object_access():
    out = js(
        'const obj = { name: "Alice", age: 30 };\n'
        "console.log(obj.name);\n"
        "console.log(obj.age);"
    )
    assert no_errors(out)
    assert has(out, "Alice", "30")


def test_array_push():
    out = js("let a = [1, 2, 3]; a.push(4); console.log(a.length); console.log(a[3]);")
    assert no_errors(out)
    assert has(out, "4")


def test_ternary_operator():
    out = js("let x = 7; console.log(x > 5 ? 'big' : 'small');")
    assert no_errors(out)
    assert has(out, "big")


def test_string_to_upper():
    out = js("let s = 'hello'; console.log(s.toUpperCase());")
    assert no_errors(out)
    assert has(out, "HELLO")


def test_foreach_loop():
    out = js("[10, 20, 30].forEach(x => console.log(x));")
    assert no_errors(out)
    assert has(out, "10", "20", "30")


def test_object_property():
    out = js("let obj = {name: 'Alice', age: 30}; console.log(obj.name);")
    assert no_errors(out)
    assert has(out, "Alice")


def test_math_pow():
    out = js("console.log(Math.pow(2, 8));")
    assert no_errors(out)
    assert has(out, "256")


def test_string_to_upper():
    out = js("let s = 'hello world'; console.log(s.toUpperCase());")
    assert no_errors(out)
    assert has(out, "HELLO WORLD")


def test_array_reduce():
    out = js("console.log([1, 2, 3].reduce((a, b) => a + b, 0));")
    assert no_errors(out)
    assert has(out, "6")


def test_math_max():
    out = js("console.log(Math.max(3, 1, 4, 1, 5));")
    assert no_errors(out)
    assert has(out, "5")


def test_exponentiation():
    out = js("console.log(2 ** 10);")
    assert no_errors(out)
    assert has(out, "1024")


def test_array_filter():
    out = js("let a = [1,2,3,4,5]; console.log(a.filter(x => x % 2 === 0));")
    assert no_errors(out)
    assert has(out, "2", "4")


def test_string_repeat():
    out = js("console.log('ab'.repeat(3));")
    assert no_errors(out)
    assert has(out, "ababab")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "javascript" / "hello.js"
    ).read_text()
    out = js(src)
    assert no_errors(out)
    assert ok(out)
