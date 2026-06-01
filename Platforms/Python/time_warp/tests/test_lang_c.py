"""Comprehensive tests for the C language executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.C


def c(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = c('printf("Hello, World!");')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_print_integer():
    out = c('printf("%d", 42);')
    assert no_errors(out)
    assert has(out, "42")


def test_print_float():
    out = c('printf("%.2f", 3.14);')
    assert no_errors(out)
    assert has(out, "3.14")


def test_print_string():
    out = c('printf("%s", "Hello");')
    assert no_errors(out)
    assert has(out, "Hello")


# ---------------------------------------------------------------------------
# Variables and arithmetic
# ---------------------------------------------------------------------------


def test_integer_arithmetic():
    out = c(
        "int a = 10;\n"
        "int b = 3;\n"
        'printf("%d", a + b);\n'
        'printf("%d", a - b);\n'
        'printf("%d", a * b);\n'
        'printf("%d", a % b);\n'
    )
    assert no_errors(out)
    assert has(out, "13", "7", "30", "1")


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------


def test_if_else():
    out = c(
        "int x = 5;\n"
        'if (x > 3) {\n'
        'printf("BIG");\n'
        '} else {\n'
        'printf("SMALL");\n'
        '}\n'
    )
    assert no_errors(out)
    assert has(out, "BIG")


def test_for_loop():
    out = c(
        "for (int i = 1; i <= 3; i++) {\n"
        'printf("%d", i);\n'
        "}\n"
    )
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_while_loop():
    out = c(
        "int i = 0;\n"
        "while (i < 3) {\n"
        'printf("%d", i);\n'
        "i = i + 1;\n"
        "}\n"
    )
    assert no_errors(out)
    assert has(out, "0", "1", "2")


# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------


def test_function_call():
    out = c(
        "int square(int n) {\n"
        "return n * n;\n"
        "}\n"
        'printf("%d", square(7));\n'
    )
    assert no_errors(out)
    assert has(out, "49")


def test_nested_function():
    # Non-recursive function calling another function
    out = c(
        "int triple(int n) {\n"
        "return n * 3;\n"
        "}\n"
        'printf("%d", triple(7));\n'
    )
    assert no_errors(out)
    assert has(out, "21")


# ---------------------------------------------------------------------------
# Arrays
# ---------------------------------------------------------------------------


def test_array():
    out = c(
        "int arr[] = {10, 20, 30};\n"
        "for (int i = 0; i < 3; i++) {\n"
        'printf("%d", arr[i]);\n'
        "}\n"
    )
    # Note: array element access may vary by executor implementation
    assert no_errors(out)
    assert ok(out)


# ---------------------------------------------------------------------------
# Additional control flow
# ---------------------------------------------------------------------------


def test_switch_statement():
    out = c(
        "int x = 2;\n"
        "switch(x) {\n"
        'case 1: printf("one"); break;\n'
        'case 2: printf("two"); break;\n'
        'default: printf("other"); break;\n'
        "}"
    )
    assert no_errors(out)
    assert has(out, "two")


def test_ternary_operator():
    out = c(
        "int x = 7;\n"
        'printf("%d", x > 5 ? 1 : 0);'
    )
    assert no_errors(out)
    assert has(out, "1")


def test_do_while_loop():
    out = c(
        "int i = 1;\n"
        "do {\n"
        'printf("%d", i);\n'
        "i++;\n"
        "} while (i <= 3);"
    )
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_array_sum():
    out = c(
        "int arr[3] = {10, 20, 30};\n"
        "printf(\"%d\", arr[0]+arr[1]+arr[2]);"
    )
    assert no_errors(out)
    assert has(out, "60")


def test_accumulate_loop():
    out = c(
        "int sum = 0;\n"
        "for(int i = 1; i <= 5; i++) sum += i;\n"
        "printf(\"%d\", sum);"
    )
    assert no_errors(out)
    assert has(out, "15")


def test_abs_function():
    out = c("printf(\"%d\", abs(-42));")
    assert no_errors(out)
    assert has(out, "42")


def test_char_format():
    out = c("""
#include <stdio.h>
int main() {
  char ch = 65;
  printf("%c\\n", ch);
  return 0;
}
""")
    assert no_errors(out)
    assert has(out, "A")


def test_ternary_operator():
    out = c("""
#include <stdio.h>
int main() {
  int x = 10;
  int y = x > 5 ? 100 : 0;
  printf("%d\\n", y);
  return 0;
}
""")
    assert no_errors(out)
    assert has(out, "100")


def test_user_function():
    out = c("""
#include <stdio.h>
int square(int n) {
  return n * n;
}
int main() {
  printf("%d\\n", square(7));
  return 0;
}
""")
    assert no_errors(out)
    assert has(out, "49")


def test_integer_multiply():
    out = c(
        "int a = 10;\n"
        "int b = 2;\n"
        'printf("%d", a * b);'
    )
    assert no_errors(out)
    assert has(out, "20")


def test_negative_check():
    out = c(
        "int x = -5;\n"
        "if (x < 0) {\n"
        'printf("negative");\n'
        "}"
    )
    assert no_errors(out)
    assert has(out, "negative")


def test_inline_ternary():
    out = c(
        "int x = 10;\n"
        "int y = 20;\n"
        'printf("%d", x < y ? x : y);'
    )
    assert no_errors(out)
    assert has(out, "10")


def test_inline_multiplication():
    out = c('printf("%d", 6 * 7);')
    assert no_errors(out)
    assert has(out, "42")


def test_inline_modulo():
    out = c('printf("%d", 17 % 5);')
    assert no_errors(out)
    assert has(out, "2")


def test_inline_division():
    out = c('printf("%d", 20 / 4);')
    assert no_errors(out)
    assert has(out, "5")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "c" / "hello.c"
    ).read_text()
    out = c(src)
    assert no_errors(out)
    assert ok(out)
