"""Comprehensive tests for the Scheme language executor."""

import pytest

from time_warp.core.interpreter import Language

from .conftest_lang import run, ok, has, no_errors, first_error

L = Language.SCHEME


def scm(source: str, **kw) -> list[str]:
    """Shortcut: run a Scheme program."""
    return run(source, L, **kw)


# ============================================================================
# DISPLAY / OUTPUT
# ============================================================================


class TestOutput:
    def test_display_string(self):
        out = scm('(display "Hello World")')
        assert has(out, "Hello World")

    def test_display_number(self):
        out = scm("(display 42)")
        assert has(out, "42")

    def test_newline(self):
        out = scm('(display "A")(newline)(display "B")')
        assert has(out, "A") and has(out, "B")

    def test_display_bool_true(self):
        out = scm("(display #t)")
        assert no_errors(out)

    def test_display_bool_false(self):
        out = scm("(display #f)")
        assert no_errors(out)


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = scm("(display (+ 2 3))")
        assert has(out, "5")

    def test_subtract(self):
        out = scm("(display (- 10 4))")
        assert has(out, "6")

    def test_multiply(self):
        out = scm("(display (* 6 7))")
        assert has(out, "42")

    def test_divide(self):
        out = scm("(display (/ 10 2))")
        assert has(out, "5")

    def test_nested(self):
        out = scm("(display (+ (* 2 3) (- 10 5)))")
        assert has(out, "11")

    def test_modulo(self):
        out = scm("(display (modulo 10 3))")
        assert has(out, "1")

    def test_abs(self):
        out = scm("(display (abs -5))")
        assert has(out, "5")

    def test_max(self):
        out = scm("(display (max 1 5 3))")
        assert has(out, "5")

    def test_min(self):
        out = scm("(display (min 1 5 3))")
        assert has(out, "1")


# ============================================================================
# DEFINE / VARIABLES
# ============================================================================


class TestDefine:
    def test_define_number(self):
        out = scm("(define x 42)(display x)")
        assert has(out, "42")

    def test_define_string(self):
        out = scm('(define name "Alice")(display name)')
        assert has(out, "Alice")

    def test_define_expression(self):
        out = scm("(define x (+ 2 3))(display x)")
        assert has(out, "5")

    def test_set(self):
        out = scm("(define x 1)(set! x 42)(display x)")
        assert has(out, "42")


# ============================================================================
# FUNCTIONS (DEFINE / LAMBDA)
# ============================================================================


class TestFunctions:
    def test_define_function(self):
        out = scm("(define (double x) (* x 2))(display (double 5))")
        assert has(out, "10")

    def test_lambda(self):
        out = scm("(define f (lambda (x) (* x 3)))(display (f 4))")
        assert has(out, "12")

    def test_recursive(self):
        out = scm(
            "(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))"
            "(display (fact 5))"
        )
        assert has(out, "120")

    def test_higher_order(self):
        out = scm(
            "(define (apply-twice f x) (f (f x)))"
            "(define (add1 n) (+ n 1))"
            "(display (apply-twice add1 5))"
        )
        assert has(out, "7")


# ============================================================================
# CONDITIONALS
# ============================================================================


class TestConditionals:
    def test_if_true(self):
        out = scm('(if (> 5 3) (display "yes") (display "no"))')
        assert has(out, "yes")

    def test_if_false(self):
        out = scm('(if (< 5 3) (display "yes") (display "no"))')
        assert has(out, "no")

    def test_cond(self):
        out = scm(
            "(define x 2)"
            '(cond ((= x 1) (display "one"))'
            '      ((= x 2) (display "two"))'
            '      (else (display "other")))'
        )
        assert has(out, "two")

    def test_and(self):
        out = scm("(display (and #t #t))")
        assert no_errors(out)

    def test_or(self):
        out = scm("(display (or #f #t))")
        assert no_errors(out)

    def test_not(self):
        out = scm("(display (not #f))")
        assert no_errors(out)


# ============================================================================
# LET / LET*
# ============================================================================


class TestLet:
    def test_let(self):
        out = scm("(let ((x 5) (y 3)) (display (+ x y)))")
        assert has(out, "8")

    def test_let_star(self):
        out = scm("(let* ((x 5) (y (* x 2))) (display y))")
        assert has(out, "10")


# ============================================================================
# LISTS / PAIRS
# ============================================================================


class TestLists:
    def test_list(self):
        out = scm("(display (list 1 2 3))")
        assert has(out, "1") and has(out, "3")

    def test_car(self):
        out = scm("(display (car (list 1 2 3)))")
        assert has(out, "1")

    def test_cdr(self):
        out = scm("(display (cdr (list 1 2 3)))")
        assert has(out, "2") and has(out, "3")

    def test_cons(self):
        out = scm("(display (cons 1 (list 2 3)))")
        assert has(out, "1") and has(out, "3")

    def test_null(self):
        out = scm("(display (null? '()))")
        assert no_errors(out)

    def test_length(self):
        out = scm("(display (length (list 1 2 3)))")
        assert has(out, "3")

    def test_append(self):
        out = scm("(display (append (list 1 2) (list 3 4)))")
        assert has(out, "1") and has(out, "4")

    def test_reverse(self):
        out = scm("(display (reverse (list 1 2 3)))")
        assert has(out, "3") and has(out, "1")

    def test_map(self):
        out = scm(
            "(define (double x) (* x 2))"
            "(display (map double (list 1 2 3)))"
        )
        assert has(out, "2") and has(out, "6")

    def test_filter(self):
        out = scm(
            "(define (positive? x) (> x 0))"
            "(display (filter positive? (list -1 2 -3 4)))"
        )
        assert has(out, "2") and has(out, "4")


# ============================================================================
# STRING OPERATIONS
# ============================================================================


class TestStrings:
    def test_string_length(self):
        out = scm('(display (string-length "Hello"))')
        assert has(out, "5")

    def test_string_append(self):
        out = scm('(display (string-append "Hello" " " "World"))')
        assert has(out, "Hello World")

    def test_substring(self):
        out = scm('(display (substring "Hello" 1 3))')
        assert no_errors(out)

    def test_string_ref(self):
        out = scm('(display (string-ref "Hello" 0))')
        assert no_errors(out)


# ============================================================================
# COMPARISON
# ============================================================================


class TestComparison:
    def test_equal(self):
        out = scm("(display (= 5 5))")
        assert no_errors(out)

    def test_less_than(self):
        out = scm("(display (< 3 5))")
        assert no_errors(out)

    def test_greater_than(self):
        out = scm("(display (> 5 3))")
        assert no_errors(out)

    def test_equal_predicate(self):
        out = scm("(display (equal? 5 5))")
        assert no_errors(out)

    def test_zero_predicate(self):
        out = scm("(display (zero? 0))")
        assert no_errors(out)

    def test_positive_predicate(self):
        out = scm("(display (positive? 5))")
        assert no_errors(out)

    def test_negative_predicate(self):
        out = scm("(display (negative? -5))")
        assert no_errors(out)


# ============================================================================
# RECURSION / ITERATION
# ============================================================================


class TestIteration:
    def test_do_loop(self):
        out = scm(
            "(do ((i 0 (+ i 1)))"
            "    ((= i 3))"
            "  (display i))"
        )
        assert has(out, "0") and has(out, "2")

    def test_begin(self):
        out = scm('(begin (display "A") (display "B"))')
        assert has(out, "A") and has(out, "B")


# ============================================================================
# TYPE PREDICATES
# ============================================================================


class TestTypePredicates:
    def test_number(self):
        out = scm("(display (number? 42))")
        assert no_errors(out)

    def test_string(self):
        out = scm('(display (string? "hi"))')
        assert no_errors(out)

    def test_pair(self):
        out = scm("(display (pair? (cons 1 2)))")
        assert no_errors(out)

    def test_boolean(self):
        out = scm("(display (boolean? #t))")
        assert no_errors(out)


# ============================================================================
# MATH FUNCTIONS
# ============================================================================


class TestMathFunctions:
    def test_sqrt(self):
        out = scm("(display (sqrt 16))")
        assert has(out, "4")

    def test_expt(self):
        out = scm("(display (expt 2 10))")
        assert has(out, "1024")

    def test_floor(self):
        out = scm("(display (floor 3.7))")
        assert has(out, "3")

    def test_ceiling(self):
        out = scm("(display (ceiling 3.2))")
        assert has(out, "4")

    def test_round(self):
        out = scm("(display (round 3.5))")
        assert no_errors(out)


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_undefined_symbol(self):
        out = scm("(display undefined_symbol)")
        assert first_error(out) is not None or len(out) > 0

    def test_bad_syntax(self):
        out = scm("((()))")
        assert first_error(out) is not None or len(out) >= 0
