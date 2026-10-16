"""Comprehensive tests for the Scheme executor."""

from __future__ import annotations

from .conftest_lang import no_errors, run

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState

LANG = Language.SCHEME


def scheme(source: str) -> list[str]:
    return run(source, LANG)


def test_display_and_newline():
    out = scheme('(display "Hello, World!")\n(newline)\n')
    assert out == ["Hello, World!"]
    assert no_errors(out)


def test_arithmetic_is_prefix():
    assert scheme("(display (* 6 7))\n(newline)\n") == ["42"]
    assert scheme("(display (+ 1 2 3))\n(newline)\n") == ["6"]
    assert scheme("(display (- 10 4))\n(newline)\n") == ["6"]


def test_define_and_call_procedure():
    assert scheme("(define (square n) (* n n))\n(display (square 7))\n(newline)\n") == [
        "49"
    ]


def test_recursion():
    src = (
        "(define (fact n)\n  (if (= n 0) 1 (* n (fact (- n 1)))))\n"
        "(display (fact 5))\n(newline)\n"
    )
    assert scheme(src) == ["120"]


def test_cond_expression():
    src = (
        '(define (grade n)\n  (cond ((>= n 90) "A")\n        ((>= n 80) "B")\n'
        '        (else "C")))\n(display (grade 85))\n(newline)\n'
    )
    assert scheme(src) == ["B"]


def test_let_and_lambda():
    assert scheme("(display (let ((x 3) (y 4)) (+ x y)))\n(newline)\n") == ["7"]
    assert scheme("(display ((lambda (x) (* x x)) 9))\n(newline)\n") == ["81"]


def test_named_let_loop():
    assert scheme(
        "(display (let loop ((i 0) (total 0))\n"
        "  (if (> i 4) total (loop (+ i 1) (+ total i)))))\n(newline)\n"
    ) == ["10"]


def test_list_operations():
    assert scheme("(display (list 1 2 3))\n(newline)\n") == ["(1 2 3)"]
    assert scheme("(display (car (list 1 2 3)))\n(newline)\n") == ["1"]
    assert scheme("(display (cdr (list 1 2 3)))\n(newline)\n") == ["(2 3)"]
    assert scheme("(display (length (list 1 2 3)))\n(newline)\n") == ["3"]
    assert scheme("(display (append (list 1 2) (list 3 4)))\n(newline)\n") == [
        "(1 2 3 4)"
    ]


def test_map_and_filter():
    assert scheme(
        "(display (map (lambda (x) (* x x)) (list 1 2 3 4)))\n(newline)\n"
    ) == ["(1 4 9 16)"]
    assert scheme(
        "(display (filter odd? (list 1 2 3 4 5)))\n(newline)\n"
    ) == ["(1 3 5)"]


def test_quote_and_quasiquote():
    assert scheme("(display '(a b c))\n(newline)\n") == ["(a b c)"]
    assert scheme("(display `(1 ,(+ 1 1)))\n(newline)\n") == ["(1 2)"]


def test_string_operations():
    assert scheme('(display (string-append "foo" "bar"))\n(newline)\n') == ["foobar"]
    assert scheme('(display (string-upcase "abc"))\n(newline)\n') == ["ABC"]


def test_string_output_is_unquoted_by_display():
    assert scheme('(display "(a b)")\n(newline)\n') == ["(a b)"]


def test_write_quotes_strings():
    assert scheme('(write "hi")\n(newline)\n') == ['"hi"']


def test_booleans_and_predicates():
    assert scheme("(display (null? '()))\n(newline)\n") == ["#t"]
    assert scheme("(display (pair? (list 1)))\n(newline)\n") == ["#t"]
    assert scheme("(display (number? 'a))\n(newline)\n") == ["#f"]


def test_turtle_graphics_draws_square():
    interp = Interpreter(Language.SCHEME)
    turtle = TurtleState()
    interp.load_program(
        "(define (square size)\n  (define (loop n)\n    (when (> n 0)\n"
        "      (forward size)\n      (right 90)\n      (loop (- n 1))))\n"
        "  (loop 4))\n(square 100)\n",
        Language.SCHEME,
    )
    interp.execute(turtle)
    assert len(turtle.lines) == 4


def test_errors_are_reported():
    out = scheme("(undefined-procedure 1 2)\n")
    assert any(line.startswith("❌") for line in out)


def test_defun_alias_is_accepted():
    assert scheme("(defun (cube n) (* n n n))\n(display (cube 3))\n(newline)\n") == [
        "27"
    ]
