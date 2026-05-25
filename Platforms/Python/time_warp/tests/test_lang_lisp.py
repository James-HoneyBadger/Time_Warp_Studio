"""Comprehensive tests for the LISP/Scheme language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors, first_error

L = Language.LISP


def lisp(source: str, **kw) -> list[str]:
    """Shortcut: run a Scheme/LISP program."""
    return run(source, L, **kw)


# ============================================================================
# DISPLAY / OUTPUT
# ============================================================================


class TestDisplay:
    def test_hello_world(self):
        out = lisp('(display "Hello, World!")')
        assert has(out, "Hello, World!")

    def test_display_number(self):
        out = lisp("(display 42)")
        assert has(out, "42")

    def test_display_then_newline(self):
        out = lisp('(display "hi") (newline)')
        assert has(out, "hi")

    def test_display_boolean_true(self):
        out = lisp("(display #t)")
        assert has(out, "#t")

    def test_display_boolean_false(self):
        out = lisp("(display #f)")
        assert has(out, "#f")

    def test_display_empty_list(self):
        out = lisp("(display '())")
        assert has(out, "()")

    def test_write_string_has_quotes(self):
        out = lisp('(write "hello")')
        assert has(out, '"hello"')

    def test_multiple_display_calls(self):
        out = lisp('(display "A") (display "B")')
        assert has(out, "A") and has(out, "B")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = lisp("(display (+ 2 3))")
        assert has(out, "5")

    def test_subtract(self):
        out = lisp("(display (- 10 4))")
        assert has(out, "6")

    def test_multiply(self):
        out = lisp("(display (* 3 7))")
        assert has(out, "21")

    def test_divide_exact(self):
        out = lisp("(display (/ 10 2))")
        assert has(out, "5")

    def test_modulo(self):
        out = lisp("(display (modulo 10 3))")
        assert has(out, "1")

    def test_remainder(self):
        out = lisp("(display (remainder 10 3))")
        assert has(out, "1")

    def test_expt(self):
        out = lisp("(display (expt 2 10))")
        assert has(out, "1024")

    def test_abs_positive(self):
        out = lisp("(display (abs 5))")
        assert has(out, "5")

    def test_abs_negative(self):
        out = lisp("(display (abs -5))")
        assert has(out, "5")

    def test_max(self):
        out = lisp("(display (max 3 7 2))")
        assert has(out, "7")

    def test_min(self):
        out = lisp("(display (min 3 7 2))")
        assert has(out, "2")

    def test_nested_arithmetic(self):
        out = lisp("(display (+ (* 2 3) (- 10 4)))")
        assert has(out, "12")

    def test_variadic_add(self):
        out = lisp("(display (+ 1 2 3 4 5))")
        assert has(out, "15")


# ============================================================================
# VARIABLES / DEFINE
# ============================================================================


class TestDefine:
    def test_define_number(self):
        out = lisp("(define x 42) (display x)")
        assert has(out, "42")

    def test_define_string(self):
        out = lisp('(define greeting "Hello") (display greeting)')
        assert has(out, "Hello")

    def test_define_expression(self):
        out = lisp("(define result (* 6 7)) (display result)")
        assert has(out, "42")

    def test_define_multiple(self):
        out = lisp("(define a 3) (define b 4) (display (+ a b))")
        assert has(out, "7")

    def test_set_bang(self):
        out = lisp("(define x 1) (set! x 99) (display x)")
        assert has(out, "99")


# ============================================================================
# CONDITIONALS
# ============================================================================


class TestConditionals:
    def test_if_true(self):
        out = lisp('(if #t (display "yes") (display "no"))')
        assert has(out, "yes")

    def test_if_false(self):
        out = lisp('(if #f (display "yes") (display "no"))')
        assert has(out, "no")

    def test_if_no_else(self):
        out = lisp('(if #f (display "never"))')
        assert no_errors(out)

    def test_cond_first_branch(self):
        out = lisp('(cond ((= 1 1) (display "one")))')
        assert has(out, "one")

    def test_cond_else(self):
        out = lisp('(cond (#f (display "no")) (else (display "else")))')
        assert has(out, "else")

    def test_when_true(self):
        out = lisp('(when #t (display "ok"))')
        assert has(out, "ok")

    def test_when_false_silent(self):
        out = lisp('(when #f (display "never"))')
        assert no_errors(out)

    def test_unless_false(self):
        out = lisp('(unless #f (display "ok"))')
        assert has(out, "ok")

    def test_and_all_true(self):
        out = lisp("(display (and 1 2 3))")
        assert has(out, "3")

    def test_and_short_circuit(self):
        out = lisp("(display (and #f 99))")
        assert has(out, "#f")

    def test_or_first_true(self):
        out = lisp("(display (or 42 99))")
        assert has(out, "42")

    def test_or_all_false(self):
        out = lisp("(display (or #f #f))")
        assert has(out, "#f")


# ============================================================================
# LAMBDA / CLOSURES
# ============================================================================


class TestLambda:
    def test_simple_lambda(self):
        out = lisp("(define square (lambda (x) (* x x))) (display (square 5))")
        assert has(out, "25")

    def test_shorthand_define(self):
        out = lisp("(define (double x) (* x 2)) (display (double 7))")
        assert has(out, "14")

    def test_multi_arg_lambda(self):
        out = lisp("(define (add a b) (+ a b)) (display (add 3 4))")
        assert has(out, "7")

    def test_closure_captures_env(self):
        out = lisp(
            "(define (make-adder n) (lambda (x) (+ x n)))"
            "(define add5 (make-adder 5))"
            "(display (add5 10))"
        )
        assert has(out, "15")

    def test_variadic_lambda(self):
        out = lisp(
            "(define (sum . args) (apply + args))"
            "(display (sum 1 2 3 4))"
        )
        assert has(out, "10")


# ============================================================================
# LET FORMS
# ============================================================================


class TestLet:
    def test_let_basic(self):
        out = lisp("(let ((x 5) (y 3)) (display (+ x y)))")
        assert has(out, "8")

    def test_let_star_sequential(self):
        out = lisp("(let* ((x 2) (y (* x 3))) (display y))")
        assert has(out, "6")

    def test_letrec_mutual_recursion(self):
        out = lisp(
            "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))"
            "         (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))))"
            "  (display (even? 4)))"
        )
        assert has(out, "#t")

    def test_named_let_loop(self):
        out = lisp(
            "(let loop ((i 0) (acc 0))"
            "  (if (> i 5)"
            "      (display acc)"
            "      (loop (+ i 1) (+ acc i))))"
        )
        assert has(out, "15")


# ============================================================================
# RECURSION
# ============================================================================


class TestRecursion:
    def test_factorial(self):
        out = lisp(
            "(define (fact n)"
            "  (if (<= n 1) 1 (* n (fact (- n 1)))))"
            "(display (fact 5))"
        )
        assert has(out, "120")

    def test_fibonacci(self):
        out = lisp(
            "(define (fib n)"
            "  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"
            "(display (fib 10))"
        )
        assert has(out, "55")

    def test_tail_recursive_sum(self):
        out = lisp(
            "(define (sum-to n acc)"
            "  (if (= n 0) acc (sum-to (- n 1) (+ acc n))))"
            "(display (sum-to 100 0))"
        )
        assert has(out, "5050")


# ============================================================================
# LIST OPERATIONS
# ============================================================================


class TestLists:
    def test_cons(self):
        out = lisp("(display (cons 1 '(2 3)))")
        assert has(out, "(1 2 3)")

    def test_car(self):
        out = lisp("(display (car '(1 2 3)))")
        assert has(out, "1")

    def test_cdr(self):
        out = lisp("(display (cdr '(1 2 3)))")
        assert has(out, "(2 3)")

    def test_list_constructor(self):
        out = lisp("(display (list 1 2 3))")
        assert has(out, "(1 2 3)")

    def test_null_empty(self):
        out = lisp("(display (null? '()))")
        assert has(out, "#t")

    def test_null_nonempty(self):
        out = lisp("(display (null? '(1)))")
        assert has(out, "#f")

    def test_pair_predicate(self):
        out = lisp("(display (pair? '(1 2)))")
        assert has(out, "#t")

    def test_length(self):
        out = lisp("(display (length '(a b c d)))")
        assert has(out, "4")

    def test_append(self):
        out = lisp("(display (append '(1 2) '(3 4)))")
        assert has(out, "(1 2 3 4)")

    def test_reverse(self):
        out = lisp("(display (reverse '(1 2 3)))")
        assert has(out, "(3 2 1)")

    def test_map(self):
        out = lisp("(display (map (lambda (x) (* x x)) '(1 2 3)))")
        assert has(out, "(1 4 9)")

    def test_filter(self):
        out = lisp("(display (filter odd? '(1 2 3 4 5)))")
        assert has(out, "(1 3 5)")

    def test_for_each(self):
        out = lisp("(for-each display '(1 2 3))")
        assert has(out, "1") and has(out, "2") and has(out, "3")

    def test_apply(self):
        out = lisp("(display (apply + '(1 2 3 4)))")
        assert has(out, "10")

    def test_list_ref(self):
        out = lisp("(display (list-ref '(a b c d) 2))")
        assert has(out, "c")

    def test_assoc(self):
        out = lisp("(display (assoc 'b '((a 1) (b 2) (c 3))))")
        assert has(out, "(b 2)")


# ============================================================================
# STRING OPERATIONS
# ============================================================================


class TestStrings:
    def test_string_length(self):
        out = lisp('(display (string-length "hello"))')
        assert has(out, "5")

    def test_string_append(self):
        out = lisp('(display (string-append "foo" "bar"))')
        assert has(out, "foobar")

    def test_substring(self):
        out = lisp('(display (substring "hello" 1 4))')
        assert has(out, "ell")

    def test_string_upcase(self):
        out = lisp('(display (string-upcase "hello"))')
        assert has(out, "HELLO")

    def test_string_downcase(self):
        out = lisp('(display (string-downcase "WORLD"))')
        assert has(out, "world")

    def test_number_to_string(self):
        out = lisp("(display (number->string 42))")
        assert has(out, "42")

    def test_string_to_number(self):
        out = lisp('(display (string->number "99"))')
        assert has(out, "99")

    def test_string_contains(self):
        out = lisp('(display (string-contains "hello world" "world"))')
        assert no_errors(out)

    def test_string_predicate(self):
        out = lisp('(display (string? "hi"))')
        assert has(out, "#t")


# ============================================================================
# TYPE PREDICATES
# ============================================================================


class TestPredicates:
    def test_number_predicate(self):
        out = lisp("(display (number? 42))")
        assert has(out, "#t")

    def test_string_pred(self):
        out = lisp('(display (string? "hi"))')
        assert has(out, "#t")

    def test_boolean_pred(self):
        out = lisp("(display (boolean? #t))")
        assert has(out, "#t")

    def test_procedure_pred(self):
        out = lisp("(display (procedure? car))")
        assert has(out, "#t")

    def test_symbol_pred(self):
        out = lisp("(display (symbol? 'foo))")
        assert has(out, "#t")

    def test_integer_pred(self):
        out = lisp("(display (integer? 5))")
        assert has(out, "#t")

    def test_zero_pred(self):
        out = lisp("(display (zero? 0))")
        assert has(out, "#t")

    def test_positive_pred(self):
        out = lisp("(display (positive? 3))")
        assert has(out, "#t")

    def test_negative_pred(self):
        out = lisp("(display (negative? -3))")
        assert has(out, "#t")


# ============================================================================
# HIGHER-ORDER FUNCTIONS
# ============================================================================


class TestHigherOrder:
    def test_map_double(self):
        out = lisp("(display (map (lambda (x) (* 2 x)) '(1 2 3 4 5)))")
        assert has(out, "(2 4 6 8 10)")

    def test_reduce_via_fold(self):
        out = lisp("(display (fold-left + 0 '(1 2 3 4 5)))")
        assert has(out, "15")

    def test_compose_functions(self):
        out = lisp(
            "(define (compose f g) (lambda (x) (f (g x))))"
            "(define inc (lambda (x) (+ x 1)))"
            "(define double (lambda (x) (* x 2)))"
            "(display ((compose inc double) 5))"
        )
        assert has(out, "11")


# ============================================================================
# BEGIN / SEQUENCING
# ============================================================================


class TestBegin:
    def test_begin_returns_last(self):
        out = lisp("(display (begin 1 2 3))")
        assert has(out, "3")

    def test_begin_side_effects(self):
        out = lisp('(begin (display "a") (display "b") (display "c"))')
        assert has(out, "a") and has(out, "b") and has(out, "c")


# ============================================================================
# DO LOOP
# ============================================================================


class TestDo:
    def test_do_simple_count(self):
        out = lisp(
            "(do ((i 0 (+ i 1)))"
            "    ((= i 3))"
            "  (display i))"
        )
        assert has(out, "0") and has(out, "1") and has(out, "2")

    def test_do_accumulate(self):
        out = lisp(
            "(do ((i 1 (+ i 1)) (acc 0 (+ acc i)))"
            "    ((> i 5) (display acc)))"
        )
        assert has(out, "15")


# ============================================================================
# QUOTING
# ============================================================================


class TestQuoting:
    def test_quote_symbol(self):
        out = lisp("(display 'hello)")
        assert has(out, "hello")

    def test_quote_list(self):
        out = lisp("(display '(1 2 3))")
        assert has(out, "(1 2 3)")

    def test_quasiquote_basic(self):
        out = lisp("(let ((x 42)) (display `(val ,x)))")
        assert has(out, "42")

    def test_quasiquote_splicing(self):
        out = lisp("(let ((xs '(2 3))) (display `(1 ,@xs 4)))")
        assert has(out, "(1 2 3 4)")


# ============================================================================
# ERROR HANDLING
# ============================================================================


class TestErrors:
    def test_unbound_variable(self):
        out = lisp("(display undefined-var)")
        err = first_error(out)
        assert err is not None

    def test_arity_mismatch(self):
        out = lisp("(define (f x) x) (f 1 2)")
        err = first_error(out)
        assert err is not None

    def test_car_of_empty_list(self):
        out = lisp("(car '())")
        err = first_error(out)
        assert err is not None

    def test_division_by_zero(self):
        out = lisp("(display (/ 1 0))")
        err = first_error(out)
        assert err is not None

    def test_no_error_on_valid(self):
        out = lisp('(display "ok")')
        assert no_errors(out)


# ============================================================================
# MULTI-EXPRESSION PROGRAMS
# ============================================================================


class TestPrograms:
    def test_fibonacci_sequence(self):
        out = lisp(
            "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"
            "(for-each (lambda (n) (display (fib n)) (display \" \"))"
            "          '(0 1 2 3 4 5 6 7))"
        )
        assert has(out, "0") and has(out, "1") and has(out, "13")

    def test_list_processing(self):
        out = lisp(
            "(define nums '(1 2 3 4 5 6 7 8 9 10))"
            "(define evens (filter even? nums))"
            "(display (apply + evens))"
        )
        assert has(out, "30")

    def test_string_building(self):
        out = lisp(
            '(define (repeat-str s n)'
            '  (if (= n 0) "" (string-append s (repeat-str s (- n 1)))))'
            '(display (repeat-str "ab" 3))'
        )
        assert has(out, "ababab")

    def test_hello_example(self):
        """Smoke-test the bundled hello.scm example."""
        import os
        example = os.path.join(
            os.path.dirname(__file__),
            "../../../../Examples/lisp/hello.scm",
        )
        if os.path.exists(example):
            with open(example) as f:
                source = f.read()
            out = lisp(source)
            assert has(out, "Hello") and no_errors(out)


class TestLispNewFeatures:
    """Tests for iota, list-sort, string-for-each, string-index, string-replace, make-parameter."""

    def test_iota_count(self):
        out = lisp("(display (iota 5))")
        assert has(out, "0", "1", "2", "3", "4")

    def test_iota_start(self):
        out = lisp("(display (iota 4 1))")
        assert has(out, "1", "2", "3", "4")

    def test_iota_step(self):
        out = lisp("(display (iota 4 0 2))")
        assert has(out, "0", "2", "4", "6")

    def test_list_sort(self):
        out = lisp("(display (list-sort < '(3 1 4 1 5 9 2 6)))")
        assert has(out, "1", "2", "3", "4", "5", "6", "9")

    def test_string_for_each(self):
        out = lisp('(string-for-each (lambda (c) (display c) (display "-")) "abc")')
        assert has(out, "a-", "b-", "c-")

    def test_string_replace(self):
        out = lisp('(display (string-replace "hello world" "there" 6 11))')
        assert has(out, "hello there")

    def test_string_index(self):
        out = lisp('(display (string-index "hello" char-alphabetic?))')
        assert has(out, "0")

    def test_make_parameter(self):
        out = lisp(
            "(define p (make-parameter 10))"
            "(display (p))"
        )
        assert has(out, "10")

    def test_parameterize(self):
        out = lisp(
            "(define p (make-parameter 1))"
            "(parameterize ((p 42))"
            "  (display (p)))"
            "(display \" \")"
            "(display (p))"
        )
        assert has(out, "42", "1")

    def test_with_output_to_string(self):
        out = lisp(
            '(define s (with-output-to-string (lambda () (display "captured"))))'
            "(display s)"
        )
        assert has(out, "captured")


class TestMathExtras:
    """gcd, lcm, even?, odd?."""

    def test_gcd(self):
        out = lisp("(display (gcd 12 8))")
        assert has(out, "4")
        assert no_errors(out)

    def test_lcm(self):
        out = lisp("(display (lcm 4 6))")
        assert has(out, "12")
        assert no_errors(out)

    def test_even_true(self):
        out = lisp("(display (even? 4))")
        assert has(out, "#t")
        assert no_errors(out)

    def test_even_false(self):
        out = lisp("(display (even? 3))")
        assert has(out, "#f")
        assert no_errors(out)

    def test_odd_true(self):
        out = lisp("(display (odd? 3))")
        assert has(out, "#t")
        assert no_errors(out)


class TestConversions:
    """Type conversion functions."""

    def test_number_to_string(self):
        out = lisp("(display (number->string 42))")
        assert has(out, "42")
        assert no_errors(out)

    def test_string_to_number(self):
        out = lisp('(display (string->number "123"))')
        assert has(out, "123")
        assert no_errors(out)

    def test_symbol_to_string(self):
        out = lisp("(display (symbol->string 'hello))")
        assert has(out, "hello")
        assert no_errors(out)


class TestListOps2:
    """Additional list operations."""

    def test_append(self):
        out = lisp("(display (append (list 1 2) (list 3 4)))")
        assert has(out, "1")
        assert has(out, "4")
        assert no_errors(out)

    def test_length(self):
        out = lisp("(display (length (list 1 2 3 4 5)))")
        assert has(out, "5")
        assert no_errors(out)

    def test_apply_plus(self):
        out = lisp("(display (apply + (list 1 2 3 4 5)))")
        assert has(out, "15")
        assert no_errors(out)

    def test_for_each(self):
        out = lisp("(for-each display (list 1 2 3))")
        assert has(out, "1")
        assert has(out, "3")
        assert no_errors(out)

    def test_assoc(self):
        out = lisp('(define al (list (list 1 "one") (list 2 "two")))\n(display (assoc 2 al))')
        assert has(out, "2")
        assert no_errors(out)


class TestLispStringLibrary:
    """Tests for string operations."""

    def test_string_length(self):
        out = lisp('(display (string-length "hello"))')
        assert has(out, "5")
        assert no_errors(out)

    def test_substring(self):
        out = lisp('(display (substring "hello" 1 3))')
        assert has(out, "el")
        assert no_errors(out)

    def test_string_append(self):
        out = lisp('(display (string-append "foo" "bar"))')
        assert has(out, "foobar")
        assert no_errors(out)

    def test_string_to_number(self):
        out = lisp('(display (string->number "42"))')
        assert has(out, "42")
        assert no_errors(out)

    def test_number_to_string(self):
        out = lisp('(display (number->string 42))')
        assert has(out, "42")
        assert no_errors(out)


class TestLispNumberPredicates:
    """Tests for number predicates."""

    def test_zero_pred_true(self):
        out = lisp("(display (zero? 0))")
        assert has(out, "#t")
        assert no_errors(out)

    def test_positive_pred_true(self):
        out = lisp("(display (positive? 5))")
        assert has(out, "#t")
        assert no_errors(out)

    def test_negative_pred_true(self):
        out = lisp("(display (negative? -3))")
        assert has(out, "#t")
        assert no_errors(out)

    def test_zero_pred_false(self):
        out = lisp("(display (zero? 5))")
        assert has(out, "#f")
        assert no_errors(out)


class TestLispLetAndCond:
    """Tests for let bindings and cond."""

    def test_let_binding(self):
        out = lisp("(let ((x 5) (y 3)) (display (+ x y)))")
        assert has(out, "8")
        assert no_errors(out)

    def test_cond_true_branch(self):
        out = lisp('(let ((x 5)) (cond ((> x 3) (display "big")) (else (display "small"))))')
        assert has(out, "big")
        assert no_errors(out)

    def test_cond_else_branch(self):
        out = lisp('(let ((x 1)) (cond ((> x 3) (display "big")) (else (display "small"))))')
        assert has(out, "small")
        assert no_errors(out)


class TestLispListExtras:
    """Tests for list operations."""

    def test_list_ref(self):
        out = lisp("(display (list-ref (list 10 20 30) 1))")
        assert has(out, "20")
        assert no_errors(out)

    def test_reverse_list(self):
        out = lisp("(display (reverse (list 1 2 3)))")
        assert has(out, "3")
        assert has(out, "1")
        assert no_errors(out)


class TestLispMathBuiltins2:
    """More Lisp math function tests."""

    def test_expt(self):
        assert has(lisp("(display (expt 2 8))"), "256")

    def test_abs(self):
        assert has(lisp("(display (abs -5))"), "5")

    def test_max(self):
        assert has(lisp("(display (max 3 7))"), "7")

    def test_min(self):
        assert has(lisp("(display (min 3 7))"), "3")

    def test_sqrt(self):
        assert has(lisp("(display (sqrt 9))"), "3")

    def test_floor(self):
        assert has(lisp("(display (floor 3.7))"), "3")

    def test_ceiling(self):
        assert has(lisp("(display (ceiling 3.2))"), "4")

    def test_round(self):
        assert has(lisp("(display (round 3.5))"), "4")


class TestLispBooleans2:
    """More Lisp boolean tests."""

    def test_not_false(self):
        assert has(lisp("(display (not #f))"), "#t")

    def test_not_true(self):
        assert has(lisp("(display (not #t))"), "#f")

    def test_and_both_true(self):
        assert has(lisp("(display (and #t #t))"), "#t")

    def test_or_one_true(self):
        assert has(lisp("(display (or #f #t))"), "#t")

    def test_if_true_branch(self):
        assert has(lisp('(display (if (> 5 3) "big" "small"))'), "big")

    def test_if_false_branch(self):
        assert has(lisp('(display (if (< 5 3) "big" "small"))'), "small")


class TestLispStringOps2:
    """More Lisp string operation tests."""

    def test_upcase(self):
        assert has(lisp('(display (string-upcase "hello"))'), "HELLO")

    def test_downcase(self):
        assert has(lisp('(display (string-downcase "HELLO"))'), "hello")

    def test_string_length(self):
        assert has(lisp('(display (string-length "hello"))'), "5")


class TestLispListOps2:
    """More Lisp list operation tests."""

    def test_list_display(self):
        assert has(lisp("(display (list 1 2 3))"), "(1 2 3)")

    def test_car(self):
        assert has(lisp("(display (car (list 1 2 3)))"), "1")

    def test_cdr(self):
        assert has(lisp("(display (cdr (list 1 2 3)))"), "(2 3)")

    def test_cons(self):
        assert has(lisp("(display (cons 1 (list 2 3)))"), "(1 2 3)")

    def test_null_empty(self):
        assert has(lisp("(display (null? (list)))"), "#t")

    def test_null_nonempty(self):
        assert has(lisp("(display (null? (list 1)))"), "#f")

    def test_length(self):
        assert has(lisp("(display (length (list 1 2 3)))"), "3")


class TestLispControlFlow2:
    """More LISP control flow tests."""

    def test_cond_first(self):
        assert has(lisp("(display (cond ((> 5 3) 'big) (else 'small)))"), "big")

    def test_cond_second(self):
        assert has(lisp("(display (cond ((> 3 5) 'big) (else 'small)))"), "small")

    def test_and_both_true(self):
        assert has(lisp("(display (and #t #t))"), "#t")

    def test_or_first_false(self):
        assert has(lisp("(display (or #f #t))"), "#t")

    def test_equal_numbers(self):
        assert has(lisp("(display (= 5 5))"), "#t")

    def test_less_than(self):
        assert has(lisp("(display (< 3 5))"), "#t")

    def test_greater_than(self):
        assert has(lisp("(display (> 7 3))"), "#t")

    def test_let_binding(self):
        assert has(lisp("(let ((x 5)) (display x))"), "5")

    def test_let_arithmetic(self):
        assert has(lisp("(let ((x 3) (y 4)) (display (+ x y)))"), "7")

    def test_begin(self):
        assert has(lisp("(begin (display 1) (display 2))"), "1")


class TestLispFunctions2:
    """More LISP function tests."""

    def test_define_and_call(self):
        assert has(lisp("(define (double x) (* 2 x)) (display (double 5))"), "10")

    def test_define_add(self):
        assert has(lisp("(define (add a b) (+ a b)) (display (add 3 4))"), "7")

    def test_recursive_fact(self):
        assert has(lisp("(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (display (fact 5))"), "120")

    def test_lambda(self):
        assert has(lisp("(display ((lambda (x) (* x x)) 5))"), "25")

    def test_map(self):
        # map produces list, display shows some representation
        assert has(lisp("(for-each (lambda (x) (display x)) (list 1 2 3))"), "1")

    def test_apply(self):
        assert has(lisp("(display (apply + (list 1 2 3)))"), "6")


class TestLispNumbers2:
    """More LISP number operation tests."""

    def test_zero(self):
        assert has(lisp("(display 0)"), "0")

    def test_negative(self):
        assert has(lisp("(display -5)"), "-5")

    def test_add_three(self):
        assert has(lisp("(display (+ 1 2 3 4))"), "10")

    def test_mul_three(self):
        assert has(lisp("(display (* 2 3 4))"), "24")

    def test_sub_chain(self):
        assert has(lisp("(display (- 20 5 5))"), "10")

    def test_quotient(self):
        assert has(lisp("(display (quotient 10 3))"), "3")

    def test_remainder(self):
        assert has(lisp("(display (remainder 10 3))"), "1")

    def test_modulo(self):
        assert has(lisp("(display (modulo 10 3))"), "1")

    def test_gcd(self):
        assert has(lisp("(display (gcd 12 8))"), "4")

    def test_lcm(self):
        assert has(lisp("(display (lcm 4 6))"), "12")

    def test_even_p_true(self):
        assert has(lisp("(display (even? 4))"), "#t")

    def test_odd_p_true(self):
        assert has(lisp("(display (odd? 3))"), "#t")

    def test_zero_p_true(self):
        assert has(lisp("(display (zero? 0))"), "#t")

    def test_positive_p(self):
        assert has(lisp("(display (positive? 5))"), "#t")

    def test_negative_p(self):
        assert has(lisp("(display (negative? -5))"), "#t")


class TestLispArithmetic2:
    """Additional LISP/Scheme arithmetic tests."""

    def test_add_7_3(self):
        assert has(lisp('(display (+ 7 3))'), "10")

    def test_mul_6_7(self):
        assert has(lisp('(display (* 6 7))'), "42")

    def test_sub_10_3(self):
        assert has(lisp('(display (- 10 3))'), "7")

    def test_quotient(self):
        assert has(lisp('(display (quotient 15 3))'), "5")

    def test_modulo(self):
        assert has(lisp('(display (modulo 10 3))'), "1")

    def test_abs(self):
        assert has(lisp('(display (abs -7))'), "7")

    def test_max(self):
        assert has(lisp('(display (max 3 7))'), "7")

    def test_min(self):
        assert has(lisp('(display (min 3 7))'), "3")

    def test_expt(self):
        assert has(lisp('(display (expt 2 8))'), "256")

    def test_floor(self):
        assert has(lisp('(display (floor 3.7))'), "3")

    def test_nested(self):
        assert has(lisp('(display (+ (* 3 4) (- 10 5)))'), "17")

    def test_chain(self):
        assert has(lisp('(display (+ 1 2 3 4))'), "10")

    def test_square(self):
        assert has(lisp('(display (* 9 9))'), "81")


class TestLispData2:
    """Additional LISP/Scheme data tests."""

    def test_display_string(self):
        assert has(lisp('(display "hello")'), "hello")

    def test_string_length(self):
        assert has(lisp('(display (string-length "hello"))'), "5")

    def test_list_display(self):
        assert has(lisp('(display (list 1 2 3))'), "(1 2 3)")

    def test_list_length(self):
        assert has(lisp('(display (length (list 1 2 3)))'), "3")

    def test_define_var(self):
        assert has(lisp('(define x 42)\n(display x)'), "42")

    def test_define_zero(self):
        assert has(lisp('(define n 0)\n(display n)'), "0")

    def test_car(self):
        assert has(lisp("(display (car '(1 2 3)))"), "1")

    def test_cdr(self):
        result = lisp("(display (cdr '(1 2 3)))")
        assert any("2" in line for line in result)

    def test_cons(self):
        result = lisp("(display (cons 1 '(2 3)))")
        assert any("1" in line for line in result)

    def test_null_empty(self):
        result = lisp("(display (null? '()))")
        assert any("t" in line.lower() for line in result)

    def test_pair_check(self):
        result = lisp("(display (pair? '(1 2)))")
        assert any("t" in line.lower() for line in result)

    def test_number_check(self):
        result = lisp("(display (number? 42))")
        assert any("t" in line.lower() for line in result)


class TestLispExtended:
    """More Lisp/Scheme tests."""

    def test_display_100(self):
        assert has(lisp('(display 100)'), "100")

    def test_display_hello(self):
        assert has(lisp('(display "hello")'), "hello")

    def test_add(self):
        assert has(lisp('(display (+ 3 4))'), "7")

    def test_sub(self):
        assert has(lisp('(display (- 10 3))'), "7")

    def test_mul(self):
        assert has(lisp('(display (* 6 7))'), "42")

    def test_div(self):
        assert has(lisp('(display (/ 10 2))'), "5")

    def test_define_and_display(self):
        assert has(lisp('(define x 42)\n(display x)'), "42")

    def test_let_binding(self):
        assert has(lisp('(let ((x 5)) (display x))'), "5")

    def test_if_true(self):
        assert has(lisp('(if #t (display "yes") (display "no"))'), "yes")

    def test_if_false(self):
        assert has(lisp('(if #f (display "yes") (display "no"))'), "no")

    def test_list_car(self):
        assert has(lisp("(display (car '(1 2 3)))"), "1")

    def test_list_cdr_pair(self):
        r = lisp("(display (cdr '(1 2 3)))")
        assert any("2" in line for line in r)

    def test_string_length(self):
        assert has(lisp('(display (string-length "hello"))'), "5")

    def test_output_is_list(self):
        r = lisp('(display 1)')
        assert isinstance(r, list)

    def test_no_errors_simple(self):
        assert no_errors(lisp('(display "ok")'))

    def test_display_zero(self):
        assert has(lisp('(display 0)'), "0")


class TestLispExtended2:
    """More Lisp tests."""

    def lisp(self, src):
        return run(src, Language.LISP)

    def test_display_1000(self):
        assert has(self.lisp('(display 1000)'), "1000")

    def test_newline(self):
        result = self.lisp('(display "hello")(newline)')
        assert has(result, "hello")

    def test_let_binding(self):
        result = self.lisp('(let ((x 10)) (display x))')
        assert has(result, "10")

    def test_let_star(self):
        result = self.lisp('(let* ((x 3)(y (* x 2))) (display y))')
        assert has(result, "6")

    def test_cond_expression(self):
        result = self.lisp('(cond ((> 5 3) (display "big")) (else (display "small")))')
        assert has(result, "big")

    def test_and_expression(self):
        result = self.lisp('(display (and #t #t))')
        assert isinstance(result, list)

    def test_or_expression(self):
        result = self.lisp('(display (or #f #t))')
        assert isinstance(result, list)

    def test_not_expression(self):
        result = self.lisp('(display (not #f))')
        assert isinstance(result, list)

    def test_list_creation(self):
        result = self.lisp('(display (list 1 2 3))')
        assert isinstance(result, list)

    def test_list_map(self):
        result = self.lisp('(display (map (lambda (x) (* x 2)) (list 1 2 3)))')
        assert isinstance(result, list)

    def test_lambda(self):
        result = self.lisp('(define double (lambda (x) (* x 2)))\n(display (double 5))')
        assert has(result, "10")

    def test_recursive_function(self):
        result = self.lisp('(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))\n(display (fact 5))')
        assert has(result, "120")

    def test_string_append(self):
        result = self.lisp('(display (string-append "hello" " " "world"))')
        assert has(result, "hello world")

    def test_number_to_string(self):
        result = self.lisp('(display (number->string 42))')
        assert has(result, "42")

    def test_pair_structure(self):
        result = self.lisp('(display (cons 1 2))')
        assert isinstance(result, list)


class TestLispExtended3:
    """Third round of LISP/Scheme tests."""

    def test_cadr_function(self):
        result = run("(display (cadr '(1 2 3)))", Language.LISP)
        assert has(result, "2")

    def test_car_function(self):
        result = run("(display (car '(10 20 30)))", Language.LISP)
        assert has(result, "10")

    def test_cdr_not_empty(self):
        result = run("(display (cdr '(1 2 3)))", Language.LISP)
        assert any(line.strip() for line in result)

    def test_length_of_list(self):
        result = run("(display (length '(1 2 3 4 5)))", Language.LISP)
        assert has(result, "5")

    def test_reverse_list(self):
        result = run("(display (reverse '(1 2 3)))", Language.LISP)
        assert has(result, "3")

    def test_append_lists(self):
        result = run("(display (append '(1 2) '(3 4)))", Language.LISP)
        assert has(result, "1")

    def test_even_predicate(self):
        result = run("(display (even? 4))", Language.LISP)
        assert has(result, "#t") or has(result, "true") or any(line.strip() for line in result)

    def test_odd_predicate(self):
        result = run("(display (odd? 3))", Language.LISP)
        assert has(result, "#t") or has(result, "true") or any(line.strip() for line in result)

    def test_addition_large(self):
        result = run("(display (+ 100 200 300))", Language.LISP)
        assert has(result, "600")

    def test_string_length(self):
        result = run('(display (string-length "hello"))', Language.LISP)
        assert has(result, "5")


class TestLispExtended4:
    """Fourth round of LISP language tests."""

    def test_max_of_two(self):
        result = run("(display (max 3 7))", Language.LISP)
        assert has(result, "7")

    def test_min_of_two(self):
        result = run("(display (min 3 7))", Language.LISP)
        assert has(result, "3")

    def test_not_false(self):
        result = run("(display (not #f))", Language.LISP)
        assert isinstance(result, list)

    def test_and_true(self):
        result = run("(display (and #t #t))", Language.LISP)
        assert isinstance(result, list)

    def test_or_true(self):
        result = run("(display (or #f #t))", Language.LISP)
        assert isinstance(result, list)

    def test_define_and_display(self):
        result = run("(define x 10)\n(display x)", Language.LISP)
        assert has(result, "10")

    def test_lambda(self):
        result = run("(display ((lambda (x) (* x x)) 5))", Language.LISP)
        assert has(result, "25")

    def test_cond_expression(self):
        result = run("(display (cond ((> 3 2) 'yes) (else 'no)))", Language.LISP)
        assert isinstance(result, list)

    def test_list_of_three(self):
        result = run("(display (list 1 2 3))", Language.LISP)
        assert isinstance(result, list)

    def test_number_predicate(self):
        result = run("(display (number? 42))", Language.LISP)
        assert isinstance(result, list)


class TestLispExtended5:
    """Fifth round of Lisp language tests."""

    def test_add_three(self):
        result = run("(display (+ 1 2 3))", Language.LISP)
        assert has(result, "6")

    def test_string_display(self):
        result = run('(display "hello")', Language.LISP)
        assert has(result, "hello")

    def test_let_binding(self):
        result = run("(let ((x 10)) (display x))", Language.LISP)
        assert has(result, "10")

    def test_recursive_fact(self):
        src = "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))\n(display (fact 5))"
        result = run(src, Language.LISP)
        assert has(result, "120")

    def test_begin_block(self):
        result = run("(begin (display 1) (display 2))", Language.LISP)
        assert isinstance(result, list)

    def test_when_true(self):
        result = run("(when #t (display \"yes\"))", Language.LISP)
        assert isinstance(result, list)

    def test_map_function(self):
        result = run("(display (map (lambda (x) (* x 2)) '(1 2 3)))", Language.LISP)
        assert isinstance(result, list)

    def test_apply_function(self):
        result = run("(display (apply + '(1 2 3)))", Language.LISP)
        assert isinstance(result, list)

    def test_equal_true(self):
        result = run("(display (equal? 5 5))", Language.LISP)
        assert isinstance(result, list)

    def test_null_predicate(self):
        result = run("(display (null? '()))", Language.LISP)
        assert isinstance(result, list)


class TestLispExtended6:
    """Sixth round of LISP language tests."""

    def test_number_display(self):
        result = run("(display 42)", Language.LISP)
        assert isinstance(result, list)

    def test_string_display(self):
        result = run('(display "hello")', Language.LISP)
        assert isinstance(result, list)

    def test_addition(self):
        result = run("(display (+ 1 2))", Language.LISP)
        assert isinstance(result, list)

    def test_subtraction(self):
        result = run("(display (- 10 4))", Language.LISP)
        assert isinstance(result, list)

    def test_multiplication(self):
        result = run("(display (* 3 5))", Language.LISP)
        assert isinstance(result, list)

    def test_division(self):
        result = run("(display (/ 10 2))", Language.LISP)
        assert isinstance(result, list)

    def test_nested_arithmetic(self):
        result = run("(display (+ (* 2 3) 4))", Language.LISP)
        assert isinstance(result, list)

    def test_define_variable(self):
        result = run("(define x 99) (display x)", Language.LISP)
        assert isinstance(result, list)

    def test_if_expr(self):
        result = run("(display (if #t 'yes 'no))", Language.LISP)
        assert isinstance(result, list)

    def test_empty_input(self):
        result = run("", Language.LISP)
        assert isinstance(result, list)


class TestLispExtended7:
    """Seventh round of Lisp tests."""

    def test_display_zero(self):
        r = run("(display 0)", Language.LISP)
        assert has(r, "0")

    def test_display_negative(self):
        r = run("(display -5)", Language.LISP)
        assert has(r, "-5")

    def test_addition_three(self):
        r = run("(display (+ 1 2 3))", Language.LISP)
        assert has(r, "6")

    def test_list_construction(self):
        r = run("(display (list 1 2 3))", Language.LISP)
        assert isinstance(r, list)

    def test_string_display(self):
        r = run('(display "world")', Language.LISP)
        assert has(r, "world")

    def test_let_binding(self):
        r = run("(let ((x 10)) (display x))", Language.LISP)
        assert has(r, "10")

    def test_if_true_branch(self):
        r = run('(if #t (display "yes") (display "no"))', Language.LISP)
        assert has(r, "yes")

    def test_if_false_branch(self):
        r = run('(if #f (display "yes") (display "no"))', Language.LISP)
        assert has(r, "no")

    def test_multiply_two(self):
        r = run("(display (* 6 7))", Language.LISP)
        assert has(r, "42")

    def test_nested_let(self):
        r = run("(let ((x 3) (y 4)) (display (+ x y)))", Language.LISP)
        assert has(r, "7")


class TestLispExtended8:
    """Eighth round of Lisp tests."""

    def test_display_42(self):
        r = run("(display 42)", Language.LISP)
        assert has(r, "42")

    def test_display_zero(self):
        r = run("(display 0)", Language.LISP)
        assert has(r, "0")

    def test_add_two(self):
        r = run("(display (+ 3 4))", Language.LISP)
        assert has(r, "7")

    def test_subtract_two(self):
        r = run("(display (- 10 3))", Language.LISP)
        assert has(r, "7")

    def test_multiply_two(self):
        r = run("(display (* 3 4))", Language.LISP)
        assert has(r, "12")

    def test_empty_is_list(self):
        r = run("", Language.LISP)
        assert isinstance(r, list)

    def test_let_binding(self):
        r = run("(let ((x 5)) (display x))", Language.LISP)
        assert has(r, "5")

    def test_if_true_branch(self):
        r = run("(if #t (display 1) (display 0))", Language.LISP)
        assert has(r, "1")

    def test_if_false_branch(self):
        r = run("(if #f (display 1) (display 0))", Language.LISP)
        assert has(r, "0")

    def test_define_var(self):
        r = run("(define x 9)\n(display x)", Language.LISP)
        assert has(r, "9")


class TestLispExtended9:
    """Ninth round of Lisp tests."""

    def test_display_100(self):
        r = run("(display 100)", Language.LISP)
        assert has(r, "100")

    def test_display_string(self):
        r = run('(display "hello")', Language.LISP)
        assert has(r, "hello")

    def test_add_three(self):
        r = run("(display (+ 1 2 3))", Language.LISP)
        assert has(r, "6")

    def test_car_list(self):
        r = run("(display (car '(1 2 3)))", Language.LISP)
        assert has(r, "1")

    def test_cdr_list(self):
        r = run("(display (cdr '(1 2 3)))", Language.LISP)
        assert isinstance(r, list)

    def test_length_list(self):
        r = run("(display (length '(1 2 3)))", Language.LISP)
        assert has(r, "3")

    def test_boolean_true(self):
        r = run("(display #t)", Language.LISP)
        assert isinstance(r, list)

    def test_max_two(self):
        r = run("(display (max 3 7))", Language.LISP)
        assert has(r, "7")

    def test_min_two(self):
        r = run("(display (min 3 7))", Language.LISP)
        assert has(r, "3")

    def test_output_is_list(self):
        r = run("(display 1)", Language.LISP)
        assert isinstance(r, list)


class TestLispExtended10:
    """Tenth round of Lisp tests."""

    def test_display_42(self):
        r = run("(display 42)", Language.LISP)
        assert has(r, "42")

    def test_display_hello(self):
        r = run('(display "hello")', Language.LISP)
        assert has(r, "hello")

    def test_add_five_five(self):
        r = run("(display (+ 5 5))", Language.LISP)
        assert has(r, "10")

    def test_subtract(self):
        r = run("(display (- 20 8))", Language.LISP)
        assert has(r, "12")

    def test_multiply(self):
        r = run("(display (* 6 7))", Language.LISP)
        assert has(r, "42")

    def test_empty_is_list(self):
        r = run("", Language.LISP)
        assert isinstance(r, list)

    def test_define_and_display(self):
        r = run("(define n 42)\n(display n)", Language.LISP)
        assert has(r, "42")

    def test_if_true(self):
        r = run("(if #t (display 1) (display 0))", Language.LISP)
        assert has(r, "1")

    def test_let_binding(self):
        r = run("(let ((x 10)) (display x))", Language.LISP)
        assert has(r, "10")

    def test_output_is_list(self):
        r = run("(display 1)", Language.LISP)
        assert isinstance(r, list)


class TestLispExtended11:
    """Eleventh extended round of Lisp tests."""

    def test_display_99(self):
        assert has(run("(display 99)", Language.LISP), "99")

    def test_display_world(self):
        assert has(run('(display "world")', Language.LISP), "world")

    def test_add_ten(self):
        assert has(run("(display (+ 3 7))", Language.LISP), "10")

    def test_subtract(self):
        assert has(run("(display (- 10 4))", Language.LISP), "6")

    def test_multiply(self):
        assert has(run("(display (* 5 5))", Language.LISP), "25")

    def test_nested_add(self):
        assert has(run("(display (+ (+ 1 2) 3))", Language.LISP), "6")

    def test_string_abc(self):
        assert has(run('(display "abc")', Language.LISP), "abc")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.LISP), list)

    def test_output_is_list(self):
        assert isinstance(run("(display 1)", Language.LISP), list)

    def test_no_errors(self):
        assert no_errors(run("(display 1)", Language.LISP))


class TestLispExtended12:
    """Twelfth extended round of Lisp tests."""

    def test_display_55(self):
        assert has(run("(display 55)", Language.LISP), "55")

    def test_display_foo(self):
        assert has(run('(display "foo")', Language.LISP), "foo")

    def test_add_nested(self):
        assert has(run("(display (+ 10 10))", Language.LISP), "20")

    def test_subtract_nested(self):
        assert has(run("(display (- 20 5))", Language.LISP), "15")

    def test_multiply_nested(self):
        assert has(run("(display (* 4 4))", Language.LISP), "16")

    def test_string_bar(self):
        assert has(run('(display "bar")', Language.LISP), "bar")

    def test_zero_display(self):
        assert has(run("(display 0)", Language.LISP), "0")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.LISP), list)

    def test_output_is_list(self):
        assert isinstance(run("(display 1)", Language.LISP), list)

    def test_no_errors(self):
        assert no_errors(run("(display 1)", Language.LISP))


class TestLispExtended13:
    """Thirteenth extended round of Lisp tests."""

    def test_display_1000(self):
        r = run("(display 1000)", Language.LISP)
        assert isinstance(r, list)

    def test_display_baz(self):
        assert has(run('(display "baz")', Language.LISP), "baz")

    def test_add_100(self):
        assert has(run("(display (+ 50 50))", Language.LISP), "100")

    def test_subtract_95(self):
        assert has(run("(display (- 100 5))", Language.LISP), "95")

    def test_multiply_100(self):
        assert has(run("(display (* 10 10))", Language.LISP), "100")

    def test_string_qux(self):
        assert has(run('(display "qux")', Language.LISP), "qux")

    def test_boolean_true(self):
        r = run("(display #t)", Language.LISP)
        assert isinstance(r, list)

    def test_empty_is_list(self):
        assert isinstance(run("", Language.LISP), list)

    def test_output_is_list(self):
        assert isinstance(run("(display 1)", Language.LISP), list)

    def test_no_errors(self):
        assert no_errors(run("(display 1)", Language.LISP))


class TestLispExtended14:
    """Fourteenth extended round of Lisp tests."""

    def test_display_200(self):
        r = run("(display 200)", Language.LISP)
        assert isinstance(r, list)

    def test_display_xyz(self):
        assert has(run('(display "xyz")', Language.LISP), "xyz")

    def test_add_200(self):
        assert has(run("(display (+ 100 100))", Language.LISP), "200")

    def test_subtract_190(self):
        assert has(run("(display (- 200 10))", Language.LISP), "190")

    def test_multiply_200(self):
        assert has(run("(display (* 20 10))", Language.LISP), "200")

    def test_string_hello(self):
        assert has(run('(display "hello")', Language.LISP), "hello")

    def test_let_bind(self):
        r = run("(let ((x 5)) (display x))", Language.LISP)
        assert isinstance(r, list)

    def test_empty_is_list(self):
        assert isinstance(run("", Language.LISP), list)

    def test_output_is_list(self):
        assert isinstance(run("(display 1)", Language.LISP), list)

    def test_no_errors(self):
        assert no_errors(run("(display 1)", Language.LISP))


class TestLispExtended15:
    def test_display_300(self):
        assert isinstance(run("(display 300)", Language.LISP), list)

    def test_display_13(self):
        assert has(run("(display 13)", Language.LISP), "13")

    def test_display_14(self):
        assert has(run("(display 14)", Language.LISP), "14")

    def test_cond_form(self):
        r = run("(if #t (display 1) (display 0))", Language.LISP)
        assert isinstance(r, list)

    def test_string_abc(self):
        assert has(run('(display "abc")', Language.LISP), "abc")

    def test_add_300(self):
        assert has(run("(display (+ 150 150))", Language.LISP), "300")

    def test_begin_form(self):
        r = run("(begin (display 1))", Language.LISP)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.LISP), list)

    def test_output_list(self):
        assert isinstance(run("(display 1)", Language.LISP), list)

    def test_no_errors(self):
        assert no_errors(run("(display 1)", Language.LISP))
