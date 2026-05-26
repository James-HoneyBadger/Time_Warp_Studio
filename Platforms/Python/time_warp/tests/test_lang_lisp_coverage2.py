"""
Comprehensive coverage tests for Lisp/Scheme language executor (2nd pass).
Targets specific uncovered lines in lisp.py.
"""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors, first_error

L = Language.LISP


def lisp(source: str, **kw) -> list[str]:
    return run(source, L, **kw)


# ============================================================================
# DOTTED PAIRS (Reader lines 99-115)
# ============================================================================


class TestDottedPairs:
    def test_display_dotted_pair(self):
        out = lisp("(display (cons 1 2))")
        assert has(out, "(1 . 2)")

    def test_read_dotted_pair_literal(self):
        out = lisp("(display '(1 . 2))")
        assert has(out, "(1 . 2)")

    def test_improper_list_display(self):
        out = lisp("(display '(1 2 . 3))")
        assert has(out, "(1 2 . 3)")

    def test_dotted_pair_car(self):
        out = lisp("(display (car '(1 . 2)))")
        assert has(out, "1")

    def test_dotted_pair_cdr(self):
        out = lisp("(display (cdr '(1 . 2)))")
        assert has(out, "2")

    def test_list_qmark_improper(self):
        out = lisp("(display (list? (cons 1 2)))")
        assert has(out, "#f")

    def test_list_qmark_proper(self):
        out = lisp("(display (list? '(1 2 3)))")
        assert has(out, "#t")

    def test_pair_qmark_dotted(self):
        out = lisp("(display (pair? (cons 1 2)))")
        assert has(out, "#t")

    def test_write_dotted_pair(self):
        out = lisp("(write '(1 . 2))")
        assert has(out, "(1 . 2)")

    def test_nested_dotted_pair(self):
        out = lisp("(display (cons (cons 1 2) 3))")
        assert has(out, "((1 . 2) . 3)")


# ============================================================================
# QUOTE SHORTCUTS (Reader lines 136-153)
# ============================================================================


class TestQuoteShortcuts:
    def test_quote_shortcut_symbol(self):
        out = lisp("(display 'hello)")
        assert has(out, "hello")

    def test_quasiquote_basic(self):
        out = lisp("(let ((x 42)) (display `(value ,x)))")
        assert has(out, "(value 42)")

    def test_unquote_splicing(self):
        out = lisp("(let ((lst '(2 3))) (display `(1 ,@lst 4)))")
        assert has(out, "(1 2 3 4)")

    def test_quasiquote_nested_list(self):
        out = lisp("(display `(a b c))")
        assert has(out, "(a b c)")

    def test_quasiquote_no_unquote(self):
        out = lisp("(display `hello)")
        assert has(out, "hello")

    def test_quasiquote_empty_splice(self):
        out = lisp("(let ((lst '())) (display `(1 ,@lst 2)))")
        assert has(out, "(1 2)")


# ============================================================================
# RADIX NUMBERS / PARSE ATOM (lines ~147-260)
# ============================================================================


class TestRadixNumbers:
    def test_binary_literal(self):
        out = lisp("(display #b1010)")
        assert has(out, "10")

    def test_octal_literal(self):
        out = lisp("(display #o17)")
        assert has(out, "15")

    def test_hex_literal(self):
        out = lisp("(display #xff)")
        assert has(out, "255")

    def test_decimal_prefix(self):
        out = lisp("(display #d10)")
        assert has(out, "10")

    def test_rational_number(self):
        out = lisp("(display (+ 0.5 0.5))")
        assert has(out, "1")

    def test_float_repr(self):
        out = lisp("(display 1.5)")
        assert has(out, "1.5")

    def test_exact_float(self):
        out = lisp("(display 2.0)")
        assert has(out, "2.0")

    def test_number_to_string_hex(self):
        out = lisp("(display (number->string 255 16))")
        assert has(out, "ff")

    def test_number_to_string_binary(self):
        out = lisp("(display (number->string 10 2))")
        assert has(out, "1010")

    def test_number_to_string_octal(self):
        out = lisp("(display (number->string 8 8))")
        assert has(out, "10")

    def test_string_to_number_hex(self):
        out = lisp('(display (string->number "ff" 16))')
        assert has(out, "255")

    def test_string_to_number_binary(self):
        out = lisp('(display (string->number "1010" 2))')
        assert has(out, "10")

    def test_string_to_number_invalid(self):
        out = lisp('(display (string->number "abc"))')
        assert has(out, "#f")


# ============================================================================
# CHARACTERS (data type internals + builtins)
# ============================================================================


class TestCharacters:
    def test_char_space_repr(self):
        out = lisp("(write #\\space)")
        assert has(out, "#\\space")

    def test_char_newline_repr(self):
        out = lisp("(write #\\newline)")
        assert has(out, "#\\newline")

    def test_char_tab_repr(self):
        out = lisp("(write #\\tab)")
        assert has(out, "#\\tab")

    def test_char_display(self):
        out = lisp("(display #\\a)")
        assert has(out, "a")

    def test_char_to_integer(self):
        out = lisp("(display (char->integer #\\a))")
        assert has(out, "97")

    def test_integer_to_char(self):
        out = lisp("(display (integer->char 65))")
        assert has(out, "A")

    def test_char_alphabetic(self):
        out = lisp("(display (char-alphabetic? #\\a))")
        assert has(out, "#t")

    def test_char_numeric(self):
        out = lisp("(display (char-numeric? #\\5))")
        assert has(out, "#t")

    def test_char_whitespace(self):
        out = lisp("(display (char-whitespace? #\\space))")
        assert has(out, "#t")

    def test_char_upper_case(self):
        out = lisp("(display (char-upper-case? #\\A))")
        assert has(out, "#t")

    def test_char_lower_case(self):
        out = lisp("(display (char-lower-case? #\\a))")
        assert has(out, "#t")

    def test_char_upcase(self):
        out = lisp("(display (char-upcase #\\a))")
        assert has(out, "A")

    def test_char_downcase(self):
        out = lisp("(display (char-downcase #\\A))")
        assert has(out, "a")

    def test_char_equal(self):
        out = lisp("(display (char=? #\\a #\\a))")
        assert has(out, "#t")

    def test_char_less_than(self):
        out = lisp("(display (char<? #\\a #\\b))")
        assert has(out, "#t")

    def test_char_greater_than(self):
        out = lisp("(display (char>? #\\b #\\a))")
        assert has(out, "#t")

    def test_char_less_eq(self):
        out = lisp("(display (char<=? #\\a #\\a))")
        assert has(out, "#t")

    def test_char_greater_eq(self):
        out = lisp("(display (char>=? #\\z #\\a))")
        assert has(out, "#t")

    def test_char_predicate(self):
        out = lisp("(display (char? #\\a))")
        assert has(out, "#t")

    def test_string_to_list_chars(self):
        out = lisp('(display (string->list "ab"))')
        assert has(out, "a") and has(out, "b")

    def test_list_to_string_from_chars(self):
        out = lisp("(display (list->string (list #\\h #\\i)))")
        assert has(out, "hi")


# ============================================================================
# VECTORS (self-eval + builtins)
# ============================================================================


class TestVectors:
    def test_vector_literal(self):
        out = lisp("(display (vector 1 2 3))")
        assert has(out, "#(1 2 3)")

    def test_make_vector(self):
        out = lisp("(display (make-vector 3 0))")
        assert has(out, "#(0 0 0)")

    def test_vector_ref(self):
        out = lisp("(display (vector-ref (vector 10 20 30) 1))")
        assert has(out, "20")

    def test_vector_set(self):
        out = lisp("(let ((v (vector 1 2 3))) (vector-set! v 1 99) (display v))")
        assert has(out, "99")

    def test_vector_length(self):
        out = lisp("(display (vector-length (vector 'a 'b 'c 'd)))")
        assert has(out, "4")

    def test_vector_fill(self):
        out = lisp("(let ((v (make-vector 3 0))) (vector-fill! v 7) (display v))")
        assert has(out, "#(7 7 7)")

    def test_vector_to_list(self):
        out = lisp("(display (vector->list (vector 1 2 3)))")
        assert has(out, "(1 2 3)")

    def test_list_to_vector(self):
        out = lisp("(display (list->vector '(4 5 6)))")
        assert has(out, "#(4 5 6)")

    def test_vector_copy(self):
        out = lisp("(display (vector-copy (vector 1 2 3 4) 1 3))")
        assert has(out, "#(2 3)")

    def test_vector_predicate(self):
        out = lisp("(display (vector? (vector 1 2)))")
        assert has(out, "#t")

    def test_vector_write(self):
        out = lisp("(write (vector 1 2 3))")
        assert has(out, "#(1 2 3)")


# ============================================================================
# CASE FORM (lines 503-525)
# ============================================================================


class TestCase:
    def test_case_first_match(self):
        out = lisp('(case 1 ((1) (display "one")) ((2) (display "two")) (else (display "other")))')
        assert has(out, "one")

    def test_case_second_match(self):
        out = lisp('(case 2 ((1) (display "one")) ((2) (display "two")) (else (display "other")))')
        assert has(out, "two")

    def test_case_else(self):
        out = lisp('(case 99 ((1) (display "one")) (else (display "other")))')
        assert has(out, "other")

    def test_case_multiple_datums(self):
        out = lisp('(case 3 ((1 2 3) (display "small")) (else (display "big")))')
        assert has(out, "small")

    def test_case_no_match_no_else(self):
        out = lisp('(display (case 99 ((1) "one") ((2) "two")))')
        # No match and no else returns ()
        assert no_errors(out)

    def test_case_symbol_key(self):
        out = lisp("(case 'b ((a) (display \"A\")) ((b) (display \"B\")))")
        assert has(out, "B")


# ============================================================================
# COND WITH => (lines 538-542)
# ============================================================================


class TestCondArrow:
    def test_cond_arrow(self):
        out = lisp("(display (cond (1 => (lambda (x) (* x 10)))))")
        assert has(out, "10")

    def test_cond_arrow_returns_val(self):
        out = lisp("(display (cond ((+ 2 3) => (lambda (x) (+ x 1)))))")
        assert has(out, "6")

    def test_cond_else_empty(self):
        out = lisp("(display (cond (#f 1) (else)))")
        assert no_errors(out)

    def test_cond_single_test_val(self):
        out = lisp("(display (cond (42)))")
        assert has(out, "42")

    def test_and_no_args(self):
        out = lisp("(display (and))")
        assert has(out, "#t")

    def test_or_no_args(self):
        out = lisp("(display (or))")
        assert has(out, "#f")

    def test_and_short_circuit(self):
        out = lisp("(display (and 1 #f 3))")
        assert has(out, "#f")

    def test_or_short_circuit(self):
        out = lisp("(display (or #f 42 #f))")
        assert has(out, "42")

    def test_when_true_body(self):
        out = lisp('(when #t (display "yes"))')
        assert has(out, "yes")

    def test_when_false_body(self):
        out = lisp('(when #f (display "no"))')
        assert no_errors(out)

    def test_unless_false_body(self):
        out = lisp('(unless #f (display "yes"))')
        assert has(out, "yes")

    def test_unless_true_body(self):
        out = lisp('(unless #t (display "no"))')
        assert no_errors(out)

    def test_begin_empty(self):
        out = lisp("(begin)")
        assert no_errors(out)


# ============================================================================
# DEFINE FUNCTION SHORTHAND (lines 610-657)
# ============================================================================


class TestDefineFunctionShorthand:
    def test_define_function(self):
        out = lisp("(define (double x) (* x 2)) (display (double 5))")
        assert has(out, "10")

    def test_define_function_multi_body(self):
        out = lisp("(define (f x) (+ x 1) (* x 2)) (display (f 3))")
        assert has(out, "6")

    def test_define_function_varargs(self):
        out = lisp("(define (sum . args) (fold-left + 0 args)) (display (sum 1 2 3 4))")
        assert has(out, "10")

    def test_define_function_mixed_args(self):
        out = lisp("(define (f x . rest) (cons x rest)) (display (f 1 2 3))")
        assert has(out, "(1 2 3)")

    def test_define_lambda_naming(self):
        # Lambda assigned to variable should inherit name
        out = lisp("(define f (lambda (x) x)) (display (f 42))")
        assert has(out, "42")

    def test_lambda_rest_params(self):
        out = lisp("(define f (lambda args args)) (display (f 1 2 3))")
        assert has(out, "(1 2 3)")

    def test_define_no_body_error(self):
        out = lisp("(define (f x))")
        assert has(out, "❌")

    def test_arity_too_few_error(self):
        out = lisp("(define (f x y) (+ x y)) (f 1)")
        assert has(out, "❌")

    def test_arity_too_many_error(self):
        out = lisp("(define (f x) x) (f 1 2 3)")
        assert has(out, "❌")


# ============================================================================
# MULTI-BODY LAMBDA / _apply_tco (lines 805-819)
# ============================================================================


class TestMultiBodyLambda:
    def test_multi_body_last_value(self):
        out = lisp("(display ((lambda (x) (+ x 1) (* x 2)) 5))")
        assert has(out, "10")

    def test_multi_body_side_effect(self):
        out = lisp('((lambda (x) (display x) (display " done")) 42)')
        assert has(out, "42") and has(out, "done")

    def test_define_multi_body(self):
        out = lisp("(define (f x) (define y (* x 2)) (+ y 1)) (display (f 3))")
        assert has(out, "7")

    def test_map_with_multi_body_lambda(self):
        out = lisp("(display (map (lambda (x) (+ x 1) (* x 2)) '(1 2 3)))")
        assert has(out, "(2 4 6)")

    def test_apply_with_extra_args(self):
        out = lisp("(display (apply + 1 2 '(3 4)))")
        assert has(out, "10")

    def test_apply_single_list(self):
        out = lisp("(display (apply + '(1 2 3)))")
        assert has(out, "6")

    def test_apply_empty_list(self):
        out = lisp("(display (apply + '()))")
        assert has(out, "0")


# ============================================================================
# NAMED LET / DO LOOP (lines 653-748)
# ============================================================================


class TestNamedLetAndDo:
    def test_named_let_factorial(self):
        out = lisp("(display (let fact ((n 5) (acc 1)) (if (= n 0) acc (fact (- n 1) (* acc n)))))")
        assert has(out, "120")

    def test_named_let_sum(self):
        out = lisp("(display (let loop ((i 0) (s 0)) (if (> i 5) s (loop (+ i 1) (+ s i)))))")
        assert has(out, "15")

    def test_do_basic(self):
        out = lisp("(display (do ((i 0 (+ i 1))) ((= i 5) i)))")
        assert has(out, "5")

    def test_do_sum(self):
        out = lisp("(display (do ((i 1 (+ i 1)) (s 0 (+ s i))) ((> i 5) s)))")
        assert has(out, "15")

    def test_do_with_commands(self):
        out = lisp("(do ((i 0 (+ i 1))) ((= i 3)) (display i))")
        assert has(out, "0") and has(out, "1") and has(out, "2")

    def test_do_no_result(self):
        # Test clause with no result expressions
        out = lisp("(do ((i 0 (+ i 1))) ((= i 2)))")
        assert no_errors(out)


# ============================================================================
# DEFINE-SYNTAX / SYNTAX-RULES (lines 735-939)
# ============================================================================


class TestDefineSyntax:
    def test_simple_macro(self):
        out = lisp("""
(define-syntax my-when
  (syntax-rules ()
    ((_ test body ...)
     (if test (begin body ...) (quote ())))))
(my-when #t (display "macro-ok"))
""")
        assert has(out, "macro-ok")

    def test_macro_with_keyword(self):
        out = lisp("""
(define-syntax swap!
  (syntax-rules ()
    ((_ a b)
     (let ((tmp a))
       (set! a b)
       (set! b tmp)))))
(define x 1)
(define y 2)
(swap! x y)
(display x)
""")
        assert has(out, "2")

    def test_macro_ellipsis(self):
        out = lisp("""
(define-syntax my-list
  (syntax-rules ()
    ((_ x ...)
     (list x ...))))
(display (my-list 1 2 3 4))
""")
        assert has(out, "(1 2 3 4)")

    def test_macro_no_match_error(self):
        out = lisp("""
(define-syntax strict
  (syntax-rules (=>)
    ((_ x => y) (cons x y))))
(strict 1 2)
""")
        assert has(out, "❌")

    def test_nested_macro(self):
        out = lisp("""
(define-syntax inc!
  (syntax-rules ()
    ((_ x) (set! x (+ x 1)))))
(define n 0)
(inc! n)
(inc! n)
(display n)
""")
        assert has(out, "2")


# ============================================================================
# DEFINE-RECORD-TYPE (lines 940-1043)
# ============================================================================


class TestDefineRecordType:
    def test_basic_record(self):
        out = lisp("""
(define-record-type point
  (make-point x y)
  point?
  (x point-x)
  (y point-y))
(define p (make-point 3 4))
(display (point-x p))
""")
        assert has(out, "3")

    def test_record_predicate(self):
        out = lisp("""
(define-record-type foo
  (make-foo val)
  foo?
  (val foo-val))
(define f (make-foo 42))
(display (foo? f))
""")
        assert has(out, "#t")

    def test_record_predicate_false(self):
        out = lisp("""
(define-record-type bar
  (make-bar x)
  bar?
  (x bar-x))
(display (bar? 42))
""")
        assert has(out, "#f")

    def test_record_with_setter(self):
        out = lisp("""
(define-record-type mutable-point
  (make-mpoint x y)
  mpoint?
  (x mpoint-x set-mpoint-x!)
  (y mpoint-y set-mpoint-y!))
(define p (make-mpoint 1 2))
(set-mpoint-x! p 10)
(display (mpoint-x p))
""")
        assert has(out, "10")

    def test_record_multiple_fields(self):
        out = lisp("""
(define-record-type person
  (make-person name age)
  person?
  (name person-name)
  (age person-age))
(define p (make-person "Alice" 30))
(display (person-name p))
(display " ")
(display (person-age p))
""")
        assert has(out, "Alice") and has(out, "30")


# ============================================================================
# PARAMETERIZE (lines 750+)
# ============================================================================


class TestParameterize:
    def test_make_parameter_and_call(self):
        out = lisp("""
(define p (make-parameter 10))
(display (p))
""")
        assert has(out, "10")

    def test_parameterize_changes_value(self):
        out = lisp("""
(define p (make-parameter 1))
(parameterize ((p 42))
  (display (p)))
""")
        assert has(out, "42")

    def test_parameterize_restores_value(self):
        out = lisp("""
(define p (make-parameter 1))
(parameterize ((p 99))
  (display "inside"))
(display (p))
""")
        assert has(out, "inside") and has(out, "1")

    def test_make_parameter_with_converter(self):
        # Converters must be builtins (Python callables), not Scheme lambdas
        out = lisp("""
(define p (make-parameter 3 number->string))
(display (p))
""")
        assert has(out, "3")

    def test_parameterize_multiple_params(self):
        out = lisp("""
(define a (make-parameter 1))
(define b (make-parameter 2))
(parameterize ((a 10) (b 20))
  (display (+ (a) (b))))
""")
        assert has(out, "30")


# ============================================================================
# VALUES / CALL-WITH-VALUES (lines 774-782)
# ============================================================================


class TestValues:
    def test_values_single(self):
        out = lisp("(call-with-values (lambda () (values 42)) (lambda (x) (display x)))")
        assert has(out, "42")

    def test_values_multiple(self):
        out = lisp("(call-with-values (lambda () (values 1 2 3)) +)")
        # Should return 6 but no display — call-with-values returns result
        out2 = lisp("(display (call-with-values (lambda () (values 1 2 3)) +))")
        assert has(out2, "6")

    def test_values_display(self):
        out = lisp("(display (values 1 2 3))")
        assert no_errors(out)

    def test_call_with_values_basic(self):
        out = lisp("(display (call-with-values (lambda () 5) (lambda (x) (* x 2))))")
        assert has(out, "10")


# ============================================================================
# QUASIQUOTE ADVANCED (lines 831-867)
# ============================================================================


class TestQuasiquoteAdvanced:
    def test_nested_quasiquote(self):
        out = lisp("(display `(a `(b ,(+ 1 2))))")
        assert has(out, "a")

    def test_quasiquote_splicing_empty(self):
        out = lisp("(let ((x '())) (display `(start ,@x end)))")
        assert has(out, "(start end)")

    def test_quasiquote_multiple_splices(self):
        out = lisp("(let ((a '(1 2)) (b '(3 4))) (display `(,@a ,@b)))")
        assert has(out, "(1 2 3 4)")

    def test_quasiquote_nested_eval(self):
        out = lisp("(let ((n 5)) (display `(the answer is ,(* n n))))")
        assert has(out, "25")

    def test_unquote_nested(self):
        out = lisp("(define x 10) (display `(x is ,x))")
        assert has(out, "10")


# ============================================================================
# EQV? (line 1043+)
# ============================================================================


class TestEqv:
    def test_eqv_same_int(self):
        out = lisp("(display (eqv? 1 1))")
        assert has(out, "#t")

    def test_eqv_different_ints(self):
        out = lisp("(display (eqv? 1 2))")
        assert has(out, "#f")

    def test_eqv_different_types(self):
        out = lisp('(display (eqv? 1 "1"))')
        assert has(out, "#f")

    def test_eqv_booleans(self):
        out = lisp("(display (eqv? #t #t))")
        assert has(out, "#t")

    def test_eqv_symbols(self):
        out = lisp("(display (eqv? 'a 'a))")
        assert has(out, "#t")

    def test_eq_symbols(self):
        out = lisp("(display (eq? 'hello 'hello))")
        assert has(out, "#t")

    def test_equal_lists(self):
        out = lisp("(display (equal? '(1 2 3) '(1 2 3)))")
        assert has(out, "#t")


# ============================================================================
# FORMAT (lines 1595-1657)
# ============================================================================


class TestFormat:
    def test_format_tilde_a(self):
        out = lisp('(format #t "hello ~a" "world")')
        assert has(out, "hello world")

    def test_format_tilde_s(self):
        out = lisp('(format #t "~s" "hello")')
        assert has(out, '"hello"')

    def test_format_tilde_percent(self):
        out = lisp('(display "line1") (format #t "~%") (display "line2")')
        assert has(out, "line1") and has(out, "line2")

    def test_format_tilde_tilde(self):
        out = lisp('(format #t "100~~")')
        assert has(out, "100~")

    def test_format_tilde_d(self):
        out = lisp('(format #t "~d items" 42)')
        assert has(out, "42 items")

    def test_format_tilde_D_uppercase(self):
        out = lisp('(format #t "~D" 99)')
        assert has(out, "99")

    def test_format_tilde_A_uppercase(self):
        out = lisp('(format #t "~A" 123)')
        assert has(out, "123")

    def test_format_tilde_S_uppercase(self):
        out = lisp('(format #t "~S" 42)')
        assert has(out, "42")

    def test_format_return_string(self):
        out = lisp('(display (format #f "val=~a" 42))')
        assert has(out, "val=42")

    def test_format_return_string_no_display(self):
        out = lisp('(define s (format #f "~a+~a" 1 2)) (display s)')
        assert has(out, "1+2")

    def test_format_unknown_directive(self):
        out = lisp('(format #t "~z")')
        assert has(out, "~z") or no_errors(out)

    def test_format_no_args(self):
        out = lisp('(format #t "plain text")')
        assert has(out, "plain text")

    def test_format_multiple_a_args(self):
        out = lisp('(format #t "~a ~a ~a" 1 2 3)')
        assert has(out, "1 2 3")


# ============================================================================
# CALL/CC (lines 1685-1694)
# ============================================================================


class TestCallCC:
    def test_callcc_escape(self):
        out = lisp("(display (call/cc (lambda (k) (k 42) 999)))")
        assert has(out, "42")

    def test_callcc_no_escape(self):
        out = lisp("(display (call/cc (lambda (k) 99)))")
        assert has(out, "99")

    def test_call_with_current_continuation(self):
        out = lisp("(display (call-with-current-continuation (lambda (k) (k 7))))")
        assert has(out, "7")

    def test_callcc_in_list(self):
        out = lisp("""
(define result
  (call/cc (lambda (return)
    (for-each (lambda (x)
                (when (= x 3) (return x)))
              '(1 2 3 4 5))
    -1)))
(display result)
""")
        assert has(out, "3")

    def test_callcc_default_escape(self):
        # Call escape with no arg — returns ()
        out = lisp("(display (call/cc (lambda (k) (k))))")
        assert no_errors(out)


# ============================================================================
# WITH-EXCEPTION-HANDLER (line 1714)
# ============================================================================


class TestWithExceptionHandler:
    def test_exception_handler_called(self):
        out = lisp("""
(with-exception-handler
  (lambda (e) (display "caught"))
  (lambda () (error "test error")))
""")
        assert has(out, "caught")

    def test_exception_handler_no_error(self):
        out = lisp("""
(with-exception-handler
  (lambda (e) (display "error"))
  (lambda () (display "ok")))
""")
        assert has(out, "ok")

    def test_raise_error(self):
        out = lisp('(raise "my error")')
        assert has(out, "❌")

    def test_with_exception_handler_value(self):
        out = lisp("""
(display
  (with-exception-handler
    (lambda (e) 0)
    (lambda () (error "boom"))))
""")
        assert has(out, "0")


# ============================================================================
# WITH-OUTPUT-TO-STRING (lines 1725-1735)
# ============================================================================


class TestWithOutputToString:
    def test_with_output_to_string_basic(self):
        out = lisp('(display (with-output-to-string (lambda () (display "captured"))))')
        assert has(out, "captured")

    def test_with_output_to_string_multiple(self):
        out = lisp('(display (with-output-to-string (lambda () (display "a") (display "b"))))')
        assert has(out, "ab")

    def test_with_output_to_string_empty(self):
        out = lisp("(display (with-output-to-string (lambda () #f)))")
        assert no_errors(out)

    def test_open_output_string(self):
        out = lisp("""
(define p (open-output-string))
(write-string "hello" p)
(display (get-output-string p))
""")
        assert has(out, "hello")


# ============================================================================
# PARTIAL LINE ACCUMULATION (_print with newlines, lines 1582-1585)
# ============================================================================


class TestPartialLines:
    def test_display_with_newline_in_string(self):
        out = lisp('(display "line1\\nline2")')
        assert has(out, "line1") and has(out, "line2")

    def test_write_string_direct(self):
        # write-string without port arg just silently ignores (overridden by port version)
        out = lisp('(write-string "hello world")')
        assert no_errors(out)

    def test_display_no_newline_then_newline(self):
        out = lisp('(display "hello") (newline)')
        assert has(out, "hello")

    def test_print_multiple_args(self):
        out = lisp("(print 1 2 3)")
        assert has(out, "1") and has(out, "2") and has(out, "3")

    def test_println_basic(self):
        out = lisp('(println "hello")')
        assert has(out, "hello")


# ============================================================================
# DYNAMIC-WIND (lines 1685+)
# ============================================================================


class TestDynamicWind:
    def test_dynamic_wind_basic(self):
        out = lisp("""
(dynamic-wind
  (lambda () (display "before"))
  (lambda () (display "during"))
  (lambda () (display "after")))
""")
        assert has(out, "before") and has(out, "during") and has(out, "after")

    def test_dynamic_wind_always_after(self):
        out = lisp("""
(dynamic-wind
  (lambda () (display "B"))
  (lambda () (display "D") 42)
  (lambda () (display "A")))
""")
        assert has(out, "B") and has(out, "D") and has(out, "A")


# ============================================================================
# HASH TABLES (lines 1685+)
# ============================================================================


class TestHashTables:
    def test_make_hash_table(self):
        out = lisp("(define h (make-hash-table)) (display h)")
        assert no_errors(out)

    def test_hash_table_set_and_ref(self):
        out = lisp("""
(define h (make-hash-table))
(hash-table-set! h 'key "value")
(display (hash-table-ref h 'key))
""")
        assert has(out, "value")

    def test_hash_table_default(self):
        out = lisp("""
(define h (make-hash-table))
(display (hash-table-ref h 'missing "default"))
""")
        assert has(out, "default")

    def test_hash_table_delete(self):
        out = lisp("""
(define h (make-hash-table))
(hash-table-set! h 'x 1)
(hash-table-delete! h 'x)
(display (hash-table-ref h 'x "gone"))
""")
        assert has(out, "gone")

    def test_hash_table_to_alist(self):
        out = lisp("""
(define h (make-hash-table))
(hash-table-set! h 'a 1)
(display (hash-table->alist h))
""")
        assert no_errors(out)

    def test_hash_table_keys(self):
        out = lisp("""
(define h (make-hash-table))
(hash-table-set! h 'foo 1)
(display (hash-table-keys h))
""")
        assert no_errors(out)

    def test_hash_table_values(self):
        out = lisp("""
(define h (make-hash-table))
(hash-table-set! h 'x 99)
(display (hash-table-values h))
""")
        assert has(out, "99")


# ============================================================================
# TURTLE STUBS (lines 1582-1657 - stub turtle registration)
# ============================================================================


class TestTurtleStubs:
    def test_forward_stub(self):
        out = lisp("(forward 10)")
        assert no_errors(out)

    def test_backward_stub(self):
        out = lisp("(backward 10)")
        assert no_errors(out)

    def test_fd_stub(self):
        out = lisp("(fd 20)")
        assert no_errors(out)

    def test_bk_stub(self):
        out = lisp("(bk 20)")
        assert no_errors(out)

    def test_right_stub(self):
        out = lisp("(right 90)")
        assert no_errors(out)

    def test_left_stub(self):
        out = lisp("(left 90)")
        assert no_errors(out)

    def test_rt_stub(self):
        out = lisp("(rt 45)")
        assert no_errors(out)

    def test_lt_stub(self):
        out = lisp("(lt 45)")
        assert no_errors(out)

    def test_penup_stub(self):
        out = lisp("(penup)")
        assert no_errors(out)

    def test_pendown_stub(self):
        out = lisp("(pendown)")
        assert no_errors(out)

    def test_pu_stub(self):
        out = lisp("(pu)")
        assert no_errors(out)

    def test_pd_stub(self):
        out = lisp("(pd)")
        assert no_errors(out)

    def test_home_stub(self):
        out = lisp("(home)")
        assert no_errors(out)

    def test_setpos_stub(self):
        out = lisp("(setpos 10 20)")
        assert no_errors(out)

    def test_setx_stub(self):
        out = lisp("(setx 50)")
        assert no_errors(out)

    def test_sety_stub(self):
        out = lisp("(sety 50)")
        assert no_errors(out)

    def test_setheading_stub(self):
        out = lisp("(setheading 90)")
        assert no_errors(out)

    def test_hideturtle_stub(self):
        out = lisp("(hideturtle)")
        assert no_errors(out)

    def test_showturtle_stub(self):
        out = lisp("(showturtle)")
        assert no_errors(out)

    def test_pencolor_stub(self):
        out = lisp("(pencolor 255 0 0)")
        assert no_errors(out)

    def test_color_stub(self):
        out = lisp("(color 0 255 0)")
        assert no_errors(out)

    def test_bgcolor_stub(self):
        out = lisp("(bgcolor 0 0 0)")
        assert no_errors(out)

    def test_clearscreen_stub(self):
        out = lisp("(clearscreen)")
        assert no_errors(out)

    def test_cs_stub(self):
        out = lisp("(cs)")
        assert no_errors(out)

    def test_circle_stub(self):
        out = lisp("(circle 50)")
        assert no_errors(out)

    def test_xcor_stub(self):
        out = lisp("(display (xcor))")
        assert no_errors(out)

    def test_ycor_stub(self):
        out = lisp("(display (ycor))")
        assert no_errors(out)

    def test_heading_stub(self):
        out = lisp("(display (heading))")
        assert no_errors(out)


# ============================================================================
# HELPER FUNCTIONS (_setcar, _setcdr, _is_proper_list, _append, etc.)
# ============================================================================


class TestHelperFunctions:
    def test_setcar(self):
        out = lisp("(define p (cons 1 2)) (set-car! p 99) (display p)")
        assert has(out, "99")

    def test_setcdr(self):
        out = lisp("(define p (cons 1 2)) (set-cdr! p 99) (display p)")
        assert has(out, "99")

    def test_setcar_error_not_pair(self):
        out = lisp("(set-car! 5 1)")
        assert has(out, "❌")

    def test_setcdr_error_not_pair(self):
        out = lisp("(set-cdr! 5 1)")
        assert has(out, "❌")

    def test_is_proper_list_true(self):
        out = lisp("(display (list? '(1 2 3)))")
        assert has(out, "#t")

    def test_is_proper_list_improper(self):
        out = lisp("(display (list? '(1 . 2)))")
        assert has(out, "#f")

    def test_is_proper_list_nil(self):
        out = lisp("(display (list? '()))")
        assert has(out, "#t")

    def test_append_no_args(self):
        out = lisp("(display (append))")
        assert has(out, "()")

    def test_append_single(self):
        out = lisp("(display (append '(1 2 3)))")
        assert has(out, "(1 2 3)")

    def test_append_two_lists(self):
        out = lisp("(display (append '(1 2) '(3 4)))")
        assert has(out, "(1 2 3 4)")

    def test_append_with_empty(self):
        out = lisp("(display (append '() '(1 2)))")
        assert has(out, "(1 2)")

    def test_list_tail(self):
        out = lisp("(display (list-tail '(a b c d) 2))")
        assert has(out, "(c d)")

    def test_list_tail_error(self):
        out = lisp("(list-tail '(1 2) 5)")
        assert has(out, "❌")

    def test_assoc_found(self):
        out = lisp("(display (assoc 2 '((1 a) (2 b) (3 c))))")
        assert has(out, "(2 b)")

    def test_assoc_not_found(self):
        out = lisp("(display (assoc 99 '((1 a) (2 b))))")
        assert has(out, "#f")

    def test_assq(self):
        out = lisp("(display (assq 'b '((a 1) (b 2) (c 3))))")
        assert has(out, "(b 2)")

    def test_assv(self):
        out = lisp("(display (assv 2 '((1 x) (2 y))))")
        assert has(out, "(2 y)")

    def test_member_found(self):
        out = lisp("(display (member 3 '(1 2 3 4 5)))")
        assert has(out, "(3 4 5)")

    def test_memq(self):
        out = lisp("(display (memq 'b '(a b c)))")
        assert has(out, "(b c)")

    def test_memv(self):
        out = lisp("(display (memv 2 '(1 2 3)))")
        assert has(out, "(2 3)")

    def test_member_not_found(self):
        out = lisp("(display (member 99 '(1 2 3)))")
        assert has(out, "#f")


# ============================================================================
# HIGHER-ORDER FUNCTIONS (_map with multiple lists, _fold, _reduce, sort)
# ============================================================================


class TestHigherOrderAdvanced:
    def test_map_multiple_lists(self):
        out = lisp("(display (map + '(1 2 3) '(4 5 6)))")
        assert has(out, "(5 7 9)")

    def test_for_each_multiple_lists(self):
        out = lisp("(for-each (lambda (a b) (display (+ a b))) '(1 2) '(3 4))")
        assert has(out, "4") and has(out, "6")

    def test_fold_left(self):
        out = lisp("(display (fold-left - 100 '(1 2 3)))")
        assert has(out, "94")

    def test_fold_right(self):
        out = lisp("(display (fold-right cons '() '(1 2 3)))")
        assert has(out, "(1 2 3)")

    def test_reduce(self):
        out = lisp("(display (reduce + 0 '(1 2 3 4 5)))")
        assert has(out, "15")

    def test_for_all_true(self):
        out = lisp("(display (for-all number? '(1 2 3)))")
        assert has(out, "#t")

    def test_for_all_false(self):
        out = lisp("(display (for-all number? '(1 \"a\" 3)))")
        assert has(out, "#f")

    def test_exists_true(self):
        out = lisp("(display (exists string? '(1 \"hello\" 3)))")
        assert has(out, "#t")

    def test_exists_false(self):
        out = lisp("(display (exists string? '(1 2 3)))")
        assert has(out, "#f")

    def test_sort_numeric(self):
        out = lisp("(display (sort '(3 1 4 1 5 9 2 6) <))")
        assert has(out, "(1 1 2 3 4 5 6 9)")

    def test_sort_with_comparator(self):
        out = lisp("(display (list-sort > '(3 1 4 1 5)))")
        assert has(out, "(5 4 3 1 1)")

    def test_sort_strings(self):
        out = lisp('(display (sort \'("banana" "apple" "cherry") string<?))')
        assert has(out, "apple")


# ============================================================================
# STRING OPERATIONS (missing builtins)
# ============================================================================


class TestStringAdvanced:
    def test_string_contains(self):
        out = lisp('(display (string-contains "hello world" "world"))')
        assert has(out, "#t")

    def test_string_contains_ci(self):
        out = lisp('(display (string-contains-ci "Hello" "hello"))')
        assert has(out, "#t")

    def test_string_split(self):
        out = lisp('(display (string-split "a,b,c" ","))')
        assert has(out, "(a b c)")

    def test_string_join(self):
        out = lisp('(display (string-join \'("a" "b" "c") "-"))')
        assert has(out, "a-b-c")

    def test_string_trim(self):
        out = lisp('(display (string-trim "  hello  "))')
        assert has(out, "hello")

    def test_string_index_found(self):
        out = lisp('(display (string-index "hello" char-alphabetic?))')
        assert has(out, "0")

    def test_string_index_not_found(self):
        out = lisp('(display (string-index "123" char-alphabetic?))')
        assert has(out, "#f")

    def test_string_index_with_start(self):
        out = lisp('(display (string-index "hello" char-alphabetic? 2))')
        assert has(out, "2")

    def test_string_for_each(self):
        out = lisp('(string-for-each (lambda (c) (display c)) "abc")')
        assert has(out, "a") and has(out, "b") and has(out, "c")

    def test_string_replace(self):
        out = lisp('(display (string-replace "hello world" "everyone" 6 11))')
        assert has(out, "hello everyone")

    def test_string_ci_equal(self):
        out = lisp('(display (string-ci=? "Hello" "HELLO"))')
        assert has(out, "#t")

    def test_string_from_chars(self):
        out = lisp("(display (string #\\h #\\i))")
        assert has(out, "hi")

    def test_string_to_symbol(self):
        out = lisp('(display (string->symbol "hello"))')
        assert has(out, "hello")

    def test_symbol_to_string(self):
        out = lisp("(display (symbol->string 'hello))")
        assert has(out, "hello")


# ============================================================================
# MATH ADVANCED (missing math builtins)
# ============================================================================


class TestMathAdvanced:
    def test_sin(self):
        out = lisp("(display (sin 0))")
        assert has(out, "0")

    def test_cos(self):
        out = lisp("(display (cos 0))")
        assert has(out, "1")

    def test_tan(self):
        out = lisp("(display (tan 0))")
        assert has(out, "0")

    def test_asin(self):
        out = lisp("(display (asin 0))")
        assert has(out, "0")

    def test_acos(self):
        out = lisp("(display (acos 1))")
        assert has(out, "0")

    def test_atan_one_arg(self):
        out = lisp("(display (atan 0))")
        assert has(out, "0")

    def test_atan_two_args(self):
        out = lisp("(display (atan 0 1))")
        assert has(out, "0")

    def test_log_natural(self):
        out = lisp("(display (log 1))")
        assert has(out, "0")

    def test_log_with_base(self):
        out = lisp("(display (log 100 10))")
        assert has(out, "2")

    def test_exp(self):
        out = lisp("(display (exp 0))")
        assert has(out, "1")

    def test_floor_div(self):
        out = lisp("(display (floor/ 10 3))")
        assert no_errors(out)

    def test_gcd(self):
        out = lisp("(display (gcd 12 8))")
        assert has(out, "4")

    def test_lcm(self):
        out = lisp("(display (lcm 4 6))")
        assert has(out, "12")

    def test_gcd_list(self):
        out = lisp("(display (gcd 12 18 24))")
        assert has(out, "6")

    def test_lcm_list(self):
        out = lisp("(display (lcm 4 6 8))")
        assert has(out, "24")

    def test_gcd_zero(self):
        out = lisp("(display (gcd))")
        assert has(out, "0")

    def test_lcm_zero(self):
        out = lisp("(display (lcm))")
        assert has(out, "1")

    def test_exact_to_inexact(self):
        out = lisp("(display (exact->inexact 1))")
        assert no_errors(out)

    def test_inexact_to_exact(self):
        out = lisp("(display (inexact->exact 1.5))")
        assert no_errors(out)

    def test_zero_pred(self):
        out = lisp("(display (zero? 0))")
        assert has(out, "#t")

    def test_positive_pred(self):
        out = lisp("(display (positive? 5))")
        assert has(out, "#t")

    def test_negative_pred(self):
        out = lisp("(display (negative? -3))")
        assert has(out, "#t")

    def test_odd_pred(self):
        out = lisp("(display (odd? 3))")
        assert has(out, "#t")

    def test_even_pred(self):
        out = lisp("(display (even? 4))")
        assert has(out, "#t")

    def test_exact_pred(self):
        out = lisp("(display (exact? 3))")
        assert has(out, "#t")

    def test_inexact_pred(self):
        out = lisp("(display (inexact? 3.14))")
        assert has(out, "#t")

    def test_pi_constant(self):
        out = lisp("(display pi)")
        assert has(out, "3.14")

    def test_e_constant(self):
        out = lisp("(display e)")
        assert has(out, "2.71")

    def test_nan_constant(self):
        out = lisp("(display (number? nan))")
        assert has(out, "#t")

    def test_inf_constant(self):
        out = lisp("(display (number? inf))")
        assert has(out, "#t")


# ============================================================================
# CAR/CDR COMBINATIONS (caar, cadr, etc.)
# ============================================================================


class TestCxrCombinations:
    def test_caar(self):
        out = lisp("(display (caar '((1 2) 3)))")
        assert has(out, "1")

    def test_cadr(self):
        out = lisp("(display (cadr '(1 2 3)))")
        assert has(out, "2")

    def test_cdar(self):
        out = lisp("(display (cdar '((1 2) 3)))")
        assert has(out, "(2)")

    def test_cddr(self):
        out = lisp("(display (cddr '(1 2 3)))")
        assert has(out, "(3)")

    def test_caaar(self):
        out = lisp("(display (caaar '(((1)))))")
        assert has(out, "1")

    def test_caddr(self):
        out = lisp("(display (caddr '(1 2 3 4)))")
        assert has(out, "3")

    def test_cadddr(self):
        out = lisp("(display (cadddr '(1 2 3 4)))")
        assert has(out, "4")

    def test_caddar(self):
        out = lisp("(display (caddar '((1 2 3))))")
        assert has(out, "3")


# ============================================================================
# PARSE ERROR HANDLING
# ============================================================================


class TestParseErrors:
    def test_unmatched_paren(self):
        out = lisp("(define x 1")
        assert has(out, "❌")

    def test_unexpected_close_paren(self):
        out = lisp(")")
        assert has(out, "❌")

    def test_unbound_variable(self):
        out = lisp("(display undefined-var)")
        assert has(out, "❌")

    def test_not_a_procedure(self):
        out = lisp("(1 2 3)")
        assert has(out, "❌")

    def test_set_bang_unbound(self):
        out = lisp("(set! nonexistent 1)")
        assert has(out, "❌")

    def test_car_not_pair(self):
        out = lisp("(car 5)")
        assert has(out, "❌")

    def test_cdr_not_pair(self):
        out = lisp("(cdr 5)")
        assert has(out, "❌")

    def test_error_builtin(self):
        out = lisp('(error "test message" 1 2)')
        assert has(out, "❌")

    def test_recursion_error(self):
        # Use non-TCO recursion to trigger Python's recursion limit
        out = lisp("(define (f x) (+ 1 (f (+ x 1)))) (f 0)")
        assert has(out, "❌")

    def test_if_too_few_args(self):
        out = lisp("(if)")
        assert has(out, "❌")

    def test_cannot_evaluate_non_pair(self):
        # SchemeProcedure is not head - use a non-callable
        out = lisp("(define v (vector 1 2 3)) (v 0)")
        assert has(out, "❌")


# ============================================================================
# MISC / EDGE CASES
# ============================================================================


class TestMiscEdgeCases:
    def test_void_returns_nil(self):
        out = lisp("(void) (display 42)")
        assert has(out, "42")

    def test_gensym_unique(self):
        out = lisp("""
(define s1 (gensym))
(display (symbol? s1))
""")
        assert has(out, "#t")

    def test_make_string(self):
        out = lisp('(display (string-length (make-string 5 #\\x)))')
        assert has(out, "5")

    def test_make_string_default(self):
        out = lisp("(display (string-length (make-string 3)))")
        assert has(out, "3")

    def test_port_pred_false(self):
        out = lisp('(display (port? "not a port"))')
        assert has(out, "#f")

    def test_procedure_pred_lambda(self):
        out = lisp("(display (procedure? (lambda (x) x)))")
        assert has(out, "#t")

    def test_procedure_pred_builtin(self):
        out = lisp("(display (procedure? car))")
        assert has(out, "#t")

    def test_procedure_pred_not(self):
        out = lisp("(display (procedure? 42))")
        assert has(out, "#f")

    def test_read_stub(self):
        out = lisp("(display (read))")
        assert no_errors(out)

    def test_read_line_stub(self):
        out = lisp('(display (read-line))')
        assert no_errors(out)

    def test_eof_object_pred(self):
        out = lisp('(display (eof-object? "x"))')
        assert has(out, "#f")

    def test_boolean_pred(self):
        out = lisp("(display (boolean? #t))")
        assert has(out, "#t")

    def test_symbol_pred(self):
        out = lisp("(display (symbol? 'hello))")
        assert has(out, "#t")

    def test_integer_pred(self):
        out = lisp("(display (integer? 42))")
        assert has(out, "#t")

    def test_real_pred(self):
        out = lisp("(display (real? 3.14))")
        assert has(out, "#t")

    def test_number_pred(self):
        out = lisp("(display (number? 42))")
        assert has(out, "#t")

    def test_string_pred(self):
        out = lisp('(display (string? "hello"))')
        assert has(out, "#t")

    def test_list_ref(self):
        out = lisp("(display (list-ref '(a b c d) 2))")
        assert has(out, "c")

    def test_list_copy(self):
        out = lisp("(display (list-copy '(1 2 3)))")
        assert has(out, "(1 2 3)")

    def test_iota_basic(self):
        out = lisp("(display (iota 5))")
        assert has(out, "(0 1 2 3 4)")

    def test_iota_with_start(self):
        out = lisp("(display (iota 3 10))")
        assert has(out, "(10 11 12)")

    def test_iota_with_step(self):
        out = lisp("(display (iota 4 0 5))")
        assert has(out, "(0 5 10 15)")

    def test_scheme_repr_float(self):
        out = lisp("(write 3.14)")
        assert has(out, "3.14")

    def test_scheme_repr_vector(self):
        out = lisp("(write (vector 1 2 3))")
        assert has(out, "#(1 2 3)")

    def test_scheme_repr_string_escapes(self):
        out = lisp('(write "hello\\nworld")')
        assert has(out, "\\n")

    def test_empty_list_is_not_pair(self):
        out = lisp("(display (pair? '()))")
        assert has(out, "#f")

    def test_null_predicate(self):
        out = lisp("(display (null? '()))")
        assert has(out, "#t")

    def test_null_non_empty(self):
        out = lisp("(display (null? '(1)))")
        assert has(out, "#f")
