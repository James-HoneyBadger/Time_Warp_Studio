"""Comprehensive tests for the LISP/Scheme executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.LISP


def lisp(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = lisp('(display "Hello, World!") (newline)')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_display_number():
    out = lisp("(display 42) (newline)")
    assert no_errors(out)
    assert has(out, "42")


def test_multiple_display():
    out = lisp(
        '(display "Alpha") (newline)\n'
        '(display "Beta") (newline)'
    )
    assert no_errors(out)
    assert has(out, "Alpha", "Beta")


# ---------------------------------------------------------------------------
# Arithmetic
# ---------------------------------------------------------------------------


def test_addition():
    out = lisp("(display (+ 3 4)) (newline)")
    assert no_errors(out)
    assert has(out, "7")


def test_subtraction():
    out = lisp("(display (- 10 3)) (newline)")
    assert no_errors(out)
    assert has(out, "7")


def test_multiplication():
    out = lisp("(display (* 6 7)) (newline)")
    assert no_errors(out)
    assert has(out, "42")


def test_nested_arithmetic():
    out = lisp("(display (+ (* 2 3) (- 10 5))) (newline)")
    assert no_errors(out)
    assert has(out, "11")


def test_expt():
    out = lisp("(display (expt 2 10)) (newline)")
    assert no_errors(out)
    assert has(out, "1024")


# ---------------------------------------------------------------------------
# Define and lambda
# ---------------------------------------------------------------------------


def test_define_variable():
    out = lisp("(define x 42)\n(display x) (newline)")
    assert no_errors(out)
    assert has(out, "42")


def test_define_function():
    out = lisp(
        "(define (square n) (* n n))\n"
        "(display (square 7)) (newline)"
    )
    assert no_errors(out)
    assert has(out, "49")


def test_lambda():
    out = lisp(
        "(define double (lambda (n) (* n 2)))\n"
        "(display (double 6)) (newline)"
    )
    assert no_errors(out)
    assert has(out, "12")


def test_recursive_factorial():
    out = lisp(
        "(define (factorial n)\n"
        "  (if (<= n 1) 1 (* n (factorial (- n 1)))))\n"
        "(display (factorial 5)) (newline)"
    )
    assert no_errors(out)
    assert has(out, "120")


# ---------------------------------------------------------------------------
# Conditionals
# ---------------------------------------------------------------------------


def test_if_true():
    out = lisp('(display (if (> 5 3) "YES" "NO")) (newline)')
    assert no_errors(out)
    assert has(out, "YES")


def test_if_false():
    out = lisp('(display (if (< 5 3) "YES" "NO")) (newline)')
    assert no_errors(out)
    assert has(out, "NO")


def test_cond():
    out = lisp(
        "(define x 5)\n"
        "(cond\n"
        "  ((> x 10) (display \"big\") (newline))\n"
        "  ((> x 3)  (display \"medium\") (newline))\n"
        "  (else     (display \"small\") (newline)))"
    )
    assert no_errors(out)
    assert has(out, "medium")


# ---------------------------------------------------------------------------
# Lists
# ---------------------------------------------------------------------------


def test_car_cdr():
    out = lisp(
        "(define lst '(1 2 3))\n"
        "(display (car lst)) (newline)\n"
        "(display (cadr lst)) (newline)"
    )
    assert no_errors(out)
    assert has(out, "1", "2")


def test_cons():
    out = lisp("(display (cons 1 '(2 3))) (newline)")
    assert no_errors(out)
    assert has(out, "1")


def test_list_length():
    out = lisp("(display (length '(a b c d))) (newline)")
    assert no_errors(out)
    assert has(out, "4")


# ---------------------------------------------------------------------------
# Strings
# ---------------------------------------------------------------------------


def test_string_append():
    out = lisp('(display (string-append "Hello" ", " "World!")) (newline)')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_string_length():
    out = lisp('(display (string-length "Hello")) (newline)')
    assert no_errors(out)
    assert has(out, "5")


def test_map_squares():
    out = lisp("(display (map (lambda (x) (* x x)) '(1 2 3 4))) (newline)")
    assert no_errors(out)
    assert has(out, "1", "4", "9", "16")


def test_list_length():
    out = lisp("(display (length '(a b c d e))) (newline)")
    assert no_errors(out)
    assert has(out, "5")


def test_car_cdr():
    out = lisp("(display (car '(10 20 30))) (newline)")
    assert no_errors(out)
    assert has(out, "10")


def test_sum_multiple():
    out = lisp("(display (+ 10 20 30)) (newline)")
    assert no_errors(out)
    assert has(out, "60")


def test_max_of_list():
    out = lisp("(display (max 3 7 1 5)) (newline)")
    assert no_errors(out)
    assert has(out, "7")


def test_list_reverse():
    out = lisp("(display (reverse '(1 2 3 4 5))) (newline)")
    assert no_errors(out)
    assert has(out, "5", "4", "3", "2", "1")


def test_apply_sum():
    out = lisp("(display (apply + '(1 2 3 4 5))) (newline)")
    assert no_errors(out)
    assert has(out, "15")


def test_let_star_binding():
    out = lisp("(let* ((x 3)(y (* x x))) (display y) (newline))")
    assert no_errors(out)
    assert has(out, "9")


def test_assoc_lookup():
    out = lisp("(display (assoc 'b '((a 1)(b 2)(c 3)))) (newline)")
    assert no_errors(out)
    assert has(out, "b", "2")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "lisp" / "hello.scm"
    ).read_text()
    out = lisp(src)
    assert no_errors(out)
    assert ok(out)
