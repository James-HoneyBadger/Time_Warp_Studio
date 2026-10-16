// SPDX-License-Identifier: MIT
//
// Tests for the Scheme port.
//
// Expectation strings were produced by running the Python reference (see
// tools/scheme_probe.py), so a failure here means the port drifted from it.
// Where the port deliberately differs, the test names say so and the divergence
// is listed in native/README.md.

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "check.hpp"
#include "timewarp/scheme/host.hpp"
#include "timewarp/scheme/machine.hpp"
#include "timewarp/scheme/printer.hpp"
#include "timewarp/scheme/reader.hpp"
#include "timewarp/scheme/value.hpp"

namespace {

using timewarp::scheme::Arena;
using timewarp::scheme::Machine;
using timewarp::scheme::NullHost;

/// The output of running a program, exactly as `execute_scheme()` renders it.
struct Run {
    std::string output;
    std::size_t peak_frames = 0;
};

class RecordingHost final : public timewarp::scheme::Host {
public:
    std::vector<std::string> calls;

    bool tick(std::uint64_t steps) override {
        steps_ += steps;
        return !fail_;
    }
    void set_fail(bool fail) { fail_ = fail; }
    void set_limit(std::uint64_t limit) {
        limit_ = limit;
        have_limit_ = true;
    }

    void forward(double amount) override { record("forward", amount); }
    void back(double amount) override { record("back", amount); }
    void right(double amount) override { record("right", amount); }
    void left(double amount) override { record("left", amount); }
    void goto_xy(double x, double y) override { record2("goto", x, y); }
    void set_heading(double amount) override { record("setheading", amount); }
    void set_pen_width(double amount) override { record("pensize", amount); }
    void set_color(int r, int g, int b) override {
        calls.push_back("color(" + std::to_string(r) + "," + std::to_string(g) +
                        "," + std::to_string(b) + ")");
    }
    void pen_up() override { calls.emplace_back("penup"); }
    void pen_down() override { calls.emplace_back("pendown"); }
    void home() override { calls.emplace_back("home"); }
    void clear() override { calls.emplace_back("clear"); }
    void circle(double radius) override { record("circle", radius); }
    void show_turtle() override { calls.emplace_back("showturtle"); }
    void hide_turtle() override { calls.emplace_back("hideturtle"); }

private:
    void record(const char* name, double amount) {
        calls.push_back(std::string{name} + "(" + std::to_string(amount) + ")");
    }
    void record2(const char* name, double x, double y) {
        calls.push_back(std::string{name} + "(" + std::to_string(x) + "," +
                        std::to_string(y) + ")");
    }
    std::uint64_t steps_ = 0;
    std::uint64_t limit_ = 0;
    bool have_limit_ = false;
    bool fail_ = false;
};

Run run_with_host(std::string_view source, timewarp::scheme::Host& host) {
    Arena arena;
    Machine machine{arena, host};
    Run result;
    result.output = machine.execute(source);
    result.peak_frames = machine.peak_frame_depth();
    return result;
}

std::string run(std::string_view source) {
    NullHost host;
    return run_with_host(source, host).output;
}

/// Runs a program *without* the error-to-string wrapper, so a failure
/// propagates as an exception the test can assert on.
std::string run_throwing(std::string_view source) {
    Arena arena;
    NullHost host;
    Machine machine{arena, host};
    return machine.run(source);
}

/// Asserts that `program` produces exactly `expected`.
void expect(std::string_view program, std::string expected) {
    const std::string actual = run(program);
    ++::tw::test::g_checks;
    if (actual != expected) {
        ::tw::test::fail(__FILE__, __LINE__,
                         std::string{"for "} + std::string{program} +
                             "\n     expected " + ::tw::test::show(expected) +
                             "\n     got      " + ::tw::test::show(actual));
    }
}

// ---------------------------------------------------------------------------
// Reader
// ---------------------------------------------------------------------------

void test_reader_tokens() {
    const auto tokens = timewarp::scheme::tokenize("(+ 1 2) ; trailing\n'x");
    TW_CHECK_EQ(tokens.size(), std::size_t{7});
    TW_CHECK_EQ(tokens[0], std::string{"("});
    TW_CHECK_EQ(tokens[3], std::string{"2"});
    TW_CHECK_EQ(tokens[4], std::string{")"});
    TW_CHECK_EQ(tokens[5], std::string{"'"});
}

void test_reader_string_token_keeps_quotes() {
    // The reference re-quotes the decoded body so `_atom` can spot a string.
    const auto tokens = timewarp::scheme::tokenize("\"a\\nb\"");
    TW_CHECK_EQ(tokens.size(), std::size_t{1});
    TW_CHECK_EQ(tokens[0], std::string{"\"a\nb\""});
}

void test_reader_bar_symbol() {
    const auto tokens = timewarp::scheme::tokenize("|a b|");
    TW_CHECK_EQ(tokens.size(), std::size_t{1});
    TW_CHECK_EQ(tokens[0], std::string{"a b"});
}

void test_reader_errors() {
    TW_CHECK_THROWS(run_throwing("(display (+ 1"), timewarp::scheme::ReaderError);
    TW_CHECK_THROWS(run_throwing("(display \"abc)"),
                    timewarp::scheme::ReaderError);
    TW_CHECK_THROWS(run_throwing(")"), timewarp::scheme::ReaderError);
    TW_CHECK_THROWS(run_throwing("(display |abc)"),
                    timewarp::scheme::ReaderError);
    expect("(display (+ 1", "❌ Scheme error: missing closing )\n");
    expect("(display \"abc)", "❌ Scheme error: unterminated string literal\n");
    expect("'", "❌ Scheme error: unexpected end of input\n");
}

void test_reader_atoms() {
    // Integers stay exact, reals stay reals; the distinction is observable.
    expect("(display (list 3 3.5 3.0))", "(3 3.5 3)\n");
    expect("(display (list #t #true #f #false))", "(#t #t #f #f)\n");
    expect("(display #\\a)", "a\n");
    // `#\x` yields a plain string, and a string nested inside a list is
    // written quoted -- with the control characters left literal, exactly as
    // the reference's `_write_str` does.
    expect("(display (list #\\space #\\newline #\\tab))",
           "(\" \" \"\n\" \"\t\")\n");
    // `.` must stay a symbol: `float(".")` raises in Python.
    expect("(display '.)", ".\n");
    // `1e5` is not an integer literal, so it parses as a real.
    expect("(display 1e5)", "100000\n");
    expect("(display (list .5 5.))", "(0.5 5)\n");
}

// ---------------------------------------------------------------------------
// Evaluator basics
// ---------------------------------------------------------------------------

void test_arithmetic() {
    expect("(display (+ 1 2 3))", "6\n");
    expect("(display (+))", "0\n");
    expect("(display (*))", "1\n");
    expect("(display (- 5))", "-5\n");
    expect("(display (- 5 2))", "3\n");
    expect("(display (/ 6 3))", "2\n");
    expect("(display (/ 1 2))", "0.5\n");
    expect("(display (list (quotient 7 2) (quotient -7 2)))", "(3 -3)\n");
    expect("(display (list (remainder -7 2) (modulo -7 2)))", "(-1 1)\n");
    expect("(display (list (expt 2 10) (expt 2 -1)))", "(1024 0.5)\n");
    expect("(display (list (abs -3) (abs -3.5)))", "(3 3.5)\n");
    expect("(display (list (min 3 1 2) (max 3 1 2)))", "(1 3)\n");
    // Python's round() is half-to-even.
    expect("(display (list (round 0.5) (round 1.5) (round 2.5) (round -0.5)))",
           "(0 2 2 0)\n");
    expect("(display (list (floor 2.7) (ceiling 2.1) (truncate -2.7)))",
           "(2 3 -2)\n");
    expect("(display (list (exact->inexact 5) (inexact->exact 5.7)))",
           "(5 5)\n");
}

void test_integer_is_distinct_from_real() {
    expect("(display (list (integer? 3) (integer? 3.0) (integer? (/ 6 3))))",
           "(#t #f #f)\n");
    expect("(display (list (number? 3) (number? 3.0) (number? #t)))",
           "(#t #t #f)\n");
}

void test_division_by_zero() {
    expect("(display (/ 1 0))", "❌ Scheme error: division by zero\n");
    expect("(display (modulo 1 0))", "❌ Scheme error: division by zero\n");
}

void test_truthiness() {
    // Only #f is false, so 0, () and "" are all true.
    expect("(display (list (if 0 't 'f) (if '() 't 'f) (if \"\" 't 'f)))",
           "(t t t)\n");
    expect("(display (list (not 0) (not #f) (not '())))", "(#f #t #f)\n");
}

void test_equality_quirks() {
    // Python compares a bool with a number, and a symbol with a string.
    expect("(display (list (equal? #t 1) (equal? #f 0) (eqv? #t 1)))",
           "(#t #t #t)\n");
    expect("(display (list (equal? '(1 2) '(1 2)) (eq? '(1 2) '(1 2))))",
           "(#t #f)\n");
    expect("(display (equal? (vector 1 2) (vector 1 2)))", "#t\n");
    expect("(display (equal? 'abc \"abc\"))", "#t\n");
}

void test_booleans_and_conditionals() {
    expect("(display (list (and) (or) (and 1 2) (or #f 3) (and 1 #f)))",
           "(#t #f 2 3 #f)\n");
    expect("(display (list (if #t 1) (if #f 1)))", "(1 #<unspecified>)\n");
    expect("(display (list (when #t 1 2) (unless #f 3)))", "(2 3)\n");
    expect("(display (cond ((assoc 'b '((a 1) (b 2))) => cadr) (else 'no)))",
           "2\n");
    expect("(display (cond (7) (else 1)))", "7\n");
}

void test_case_is_fixed() {
    // The reference could never run a `case` form: it called `_to_py_list()` on
    // parsed code and always raised "expected a proper list".  Because this
    // port represents code as pairs throughout, that bug cannot occur.
    expect("(display (case 1 ((1) 'one) (else 'other)))", "one\n");
    expect("(display (case 9 ((1) 'one) (else 'other)))", "other\n");
    expect("(display (case 'b ((a) 1) ((b c) 2) (else 3)))", "2\n");
    expect("(display (case 5 ((1 2 3) 'low) ((4 5 6) 'mid) (else 'high)))",
           "mid\n");
    expect("(display (case 99 ((1) 'one)))", "#<unspecified>\n");
}

void test_binding_forms() {
    expect("(display (let ((x 2) (y 3)) (* x y)))", "6\n");
    expect("(display (let* ((x 2) (y (* x 3))) (list x y)))", "(2 6)\n");
    expect("(display (letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5)))",
           "120\n");
    expect("(display (let loop ((i 0) (acc 1)) (if (= i 5) acc (loop (+ i 1) (* acc 2)))))",
           "32\n");
    // `let*` sees earlier bindings; plain `let` does not.
    expect("(display (let ((x 1)) (let ((x 2) (y x)) (list x y))))",
           "(2 1)\n");
}

void test_do_loop() {
    // `acc` sees the *already stepped* `i`, because the reference assigns each
    // step straight into the loop frame as it evaluates it.
    expect("(display (do ((i 0 (+ i 1)) (acc '() (cons i acc))) ((= i 4) acc)))",
           "(4 3 2 1)\n");
    // `do` initialisers are evaluated against the frame being built, so `j`
    // sees `i`.  Steps assign one at a time, so a later step sees an earlier
    // update.  Both quirks come from the reference.
    expect("(display (do ((i 3 (+ i 1)) (j i)) ((> i 3) (list i j))))", "(4 3)\n");
}

void test_quasiquote() {
    expect("(display `(1 ,(+ 1 1) ,@(list 3 4) 5))", "(1 2 3 4 5)\n");
    expect("(display `(a ,(* 2 3)))", "(a 6)\n");
    // Depth > 1 leaves unquote forms literal.
    expect("(display `(a `(b ,(c ,(+ 1 2)))))",
           "(a (quasiquote (b (unquote (c (unquote (+ 1 2)))))))\n");
    expect("(display `(1 ,@'() 2))", "(1 2)\n");
}

void test_quasiquote_nested_evaluation() {
    // An unquote that itself evaluates a quasiquote must not clobber the outer
    // template's partially built accumulator.
    expect("(display `(a ,(let ((x 5)) `(b ,x)) c))", "(a (b 5) c)\n");
    expect("(display `(,(+ 1 2) ,@(map (lambda (x) (* x x)) '(2 3))))",
           "(3 4 9)\n");
}

void test_quote() {
    expect("(display '(1 2 3))", "(1 2 3)\n");
    expect("(display '())", "()\n");
    expect("(display ''a)", "(quote a)\n");
}

// ---------------------------------------------------------------------------
// Procedures
// ---------------------------------------------------------------------------

void test_closures_and_rest_parameters() {
    expect("(define (f x . r) (list x r)) (display (f 1 2 3))", "(1 (2 3))\n");
    expect("(display ((lambda (a b) (list a b)) 1))", "(1 ())\n");
    expect("(display ((lambda (a) a) 1 2 3))", "1\n");
    expect("(define (adder n) (lambda (x) (+ x n))) (display ((adder 3) 4))",
           "7\n");
}

void test_mutation() {
    expect("(define x 1) (set! x 2) (display x)", "2\n");
    // `set!` defines an unbound variable rather than failing.
    expect("(set! zzz 5) (display zzz)", "5\n");
    expect("(define n 0) "
           "(define (bump) (set! n (+ n 1)) n) "
           "(bump) (bump) (display n)",
           "2\n");
}

void test_define_returns_symbol() {
    expect("(display (define q 1))", "q\n");
    expect("(define x) (display x)", "#<unspecified>\n");
}

void test_higher_order() {
    expect("(display (map + '(1 2 3) '(10 20 30)))", "(11 22 33)\n");
    expect("(display (map + '(1 2 3) '(10 20)))", "(11 22)\n");
    expect("(display (apply + 1 2 '(3 4)))", "10\n");
    expect("(display (apply list))", "()\n");
    expect("(display (list (filter odd? '(1 2 3 4 5)) (foldl + 0 '(1 2 3 4))))",
           "((1 3 5) 10)\n");
    expect("(for-each display '(1 2 3))", "123\n");
    expect("(display (map (lambda (x) (* x x)) '()))", "()\n");
}

void test_lists() {
    expect("(display (list (length '(1 2 3)) (reverse '(1 2 3))))", "(3 (3 2 1))\n");
    expect("(display (append '(1) '(2) '(3)))", "(1 2 3)\n");
    expect("(display (append))", "()\n");
    expect("(display (append '(1 2)))", "(1 2)\n");
    // The final argument becomes the tail, so it need not be a list.
    expect("(display (append '(1) 2))", "(1 . 2)\n");
    expect("(display (list (member 2 '(1 2 3)) (assoc 'b '((a 1) (b 2)))))",
           "((2 3) (b 2))\n");
    expect("(display (list (member 9 '(1 2)) (assoc 9 '((a 1)))))", "(#f #f)\n");
    expect("(display (list (caar '((1 2) 3)) (cadr '(1 2 3)) (cddr '(1 2 3))))",
           "(1 2 (3))\n");
    expect("(display (list (list-tail '(1 2 3) 1) (list-tail '(1 2 3) 99)))",
           "((2 3) ())\n");
    expect("(display (list-ref '(1 2 3) -1))", "3\n");
    // There is no dotted-pair syntax: `.` is just a symbol, so this is a
    // four-element list.  Verified against the reference.
    expect("(display (length '(1 2 . 3)))", "4\n");
    expect("(display (length '(1 . 2)))", "3\n");
    // An improper list cannot be a `length` argument -- reachable via `append`,
    // whose final argument becomes the tail.
    expect("(display (length (append '(1) 2)))",
           "\u274c Scheme error: expected a proper list\n");
}

void test_vectors() {
    expect("(display (vector 1 2 3))", "#(1 2 3)\n");
    expect("(display (list (vector-length (vector 1 2)) (vector-ref (vector 1 2) 1)))",
           "(2 2)\n");
    expect("(display (list (vector->list (vector 1 2)) (list->vector '(1 2))))",
           "((1 2) #(1 2))\n");
    expect("(display (list (make-vector 3) (make-vector 2 9)))",
           "(#(0 0 0) #(9 9))\n");
    expect("(display (vector 1 (vector 2 3)))", "#(1 #(2 3))\n");
}

void test_strings() {
    expect("(display (list (string-length \"abc\") (substring \"hello\" 1 3)))",
           "(3 \"el\")\n");
    // Python slice clamping, including from the end.
    expect("(display (list (substring \"hello\" 1 99) (substring \"hello\" -3 99)))",
           "(\"ello\" \"llo\")\n");
    expect("(display (list (string->number \"12\") (string->number \"1.5\") "
           "(string->number \"zz\") (string->number \"12\" 16)))",
           "(12 1.5 #f 12)\n");
    expect("(display (list (number->string 12) (number->string 1.5)))",
           "(\"12\" \"1.5\")\n");
    expect("(display (list (symbol->string 'abc) (string->symbol \"x\")))",
           "(\"abc\" x)\n");
    expect("(display (list (string-upcase \"ab\") (string-downcase \"AB\")))",
           "(\"AB\" \"ab\")\n");
    expect("(display (string->list \"ab\"))", "(\"a\" \"b\")\n");
    expect("(display (list (string-ci=? \"AB\" \"ab\") (string<? \"a\" \"b\")))",
           "(#t #t)\n");
    expect("(display (string-append \"a\" \"b\" \"c\"))", "abc\n");
}

void test_output_modes() {
    // display leaves a top-level string unquoted, but nested strings stay
    // quoted, because `_display_str` delegates to `_write_str` for compounds.
    expect("(display \"hi\")", "hi\n");
    expect("(display (list \"hi\"))", "(\"hi\")\n");
    expect("(write \"hi\")", "\"hi\"\n");
    expect("(write #\\a)", "\"a\"\n");
    expect("(write '(1 \"a\"))", "(1 \"a\")\n");
    expect("(newline)", "\n");
    expect("(display car)", "#<procedure car>\n");
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

void test_error_messages() {
    expect("(display zzz)", "❌ Scheme error: unbound variable: zzz\n");
    expect("(display (car 5))", "❌ Scheme error: car: not a pair\n");
    expect("(display (5 6))",
           "❌ Scheme error: attempt to call non-procedure: 5\n");
    expect("(display ())", "❌ Scheme error: cannot evaluate ()\n");
    expect("(display (error 'bad \"thing\" 3))",
           "❌ Scheme error: bad thing 3\n");
    expect("(display (+ 1 #t))",
           "❌ Scheme error: add: expected a number, got boolean\n");
}

void test_errors_that_escaped_the_reference_now_raise_properly() {
    // The reference let these Python exceptions escape `execute_scheme()`
    // entirely, so a program crashed the executor instead of reporting a
    // Scheme error.  Measured with tools/scheme_probe.py.
    expect("(display (sqrt -4))", "❌ Scheme error: sqrt: math domain error\n");
    expect("(display (list-ref '(1 2) 9))",
           "❌ Scheme error: list-ref: index out of range\n");
    expect("(display (vector-ref (vector 1) 9))",
           "❌ Scheme error: vector-ref: index out of range\n");
    expect("(display (car))",
           "❌ Scheme error: car: expected 1 argument(s), got 0\n");
}

void test_step_budget() {
    RecordingHost host;
    host.set_limit(0);
    host.set_fail(true);
    Arena arena;
    Machine machine{arena, host};
    const std::string out =
        machine.execute("(define (loop n) (loop (+ n 1))) (loop 0)");
    TW_CHECK(out.find("❌ Scheme error: step budget exhausted") == 0);
}

// ---------------------------------------------------------------------------
// The point of the port: no native recursion
// ---------------------------------------------------------------------------

void test_tail_calls_are_constant_space() {
    // 1,000,000 Scheme calls.  The reference aborted at roughly 200 with
    // "Maximum recursion depth exceeded"; this must run in bounded depth.
    NullHost host;
    const Run result = run_with_host(
        "(define (loop n) (if (= n 0) 'done (loop (- n 1))))"
        "(display (loop 1000000))",
        host);
    TW_CHECK_EQ(result.output, std::string{"done\n"});
    // Only the Halt frame plus a handful of transient ones at any moment.
    TW_CHECK(result.peak_frames < 32);
}

void test_non_tail_recursion_uses_the_heap() {
    // Non-tail recursion genuinely needs one continuation per level, but they
    // live in the machine's vector, not on the native stack.
    const std::string output = run(
        "(define (sum n) (if (= n 0) 0 (+ n (sum (- n 1)))))"
        "(display (sum 100000))");
    TW_CHECK_EQ(output, std::string{"5000050000\n"});
}

void test_deep_accumulator_recursion() {
    const std::string output = run(
        "(define (mk n acc) (if (= n 0) acc (mk (- n 1) (cons n acc))))"
        "(display (length (mk 200000 '())))");
    TW_CHECK_EQ(output, std::string{"200000\n"});
}

void test_deeply_nested_data_prints_without_recursing() {
    // 200,000 levels of nesting: the printer uses an explicit work stack, and
    // so does `equal?`.
    const std::string program =
        "(define (nest n x) (if (= n 0) x (nest (- n 1) (list x))))"
        "(display (nest 200000 'a))";
    const std::string output = run(program);
    TW_CHECK_EQ(output.size(), std::size_t{400001 + 1});  // parens + 'a' + \n
    TW_CHECK(output.front() == '(');
    TW_CHECK(output[200000] == 'a');
}

void test_deep_equal_does_not_recurse() {
    const std::string output = run(
        "(define (nest n x) (if (= n 0) x (nest (- n 1) (list x))))"
        "(display (equal? (nest 100000 'a) (nest 100000 'a)))");
    TW_CHECK_EQ(output, std::string{"#t\n"});
}

void test_reader_handles_deep_nesting() {
    // 20,000 nested parens, quoted so it is data rather than 20,000 nested
    // calls.  The parser is iterative too, so this neither throws nor
    // overflows the native stack.
    std::string source(20000, '(');
    source += "1";
    source += std::string(20000, ')');
    const std::string output = run("(display (length '" + source + "))");
    TW_CHECK_EQ(output, std::string{"1\n"});
}

// ---------------------------------------------------------------------------
// Host integration
// ---------------------------------------------------------------------------

void test_integer_overflow_promotes_instead_of_wrapping() {
    // Regression: the first version of `mul_overflow` computed `a * b` and
    // tested `product / b != a`.  Signed overflow is undefined, GCC folds that
    // test away, and the port silently produced wrapped values -- `(expt 2 63)`
    // printed -9223372036854775808 and `(* 4611686018427387904 4)` printed 0.
    // Every expectation below was confirmed against the reference.
    expect("(display (expt 2 63))", "9223372036854775808\n");
    expect("(display (* 4611686018427387904 4))", "18446744073709551616\n");
    expect("(display (+ 9223372036854775807 1))", "9223372036854775808\n");
    expect("(display (- -9223372036854775808 1))", "-9223372036854775808\n");
    // Still exact below the boundary.
    expect("(display (* 4611686018427387903 2))", "9223372036854775806\n");
}

void test_large_integral_reals_print_every_digit() {
    // `_default_float()` renders an integral real as `str(int(x))`, which is
    // exact far beyond 64 bits.  Printing these in exponent form would differ
    // from the reference, so the digits are expanded rather than cast.
    expect("(display 9223372036854775808.0)", "9223372036854775808\n");
    expect("(display (expt 2 63))", "9223372036854775808\n");
    expect("(display 2.5e20)", "250000000000000000000\n");
    expect("(display 1e30)", "1000000000000000019884624838656\n");
    expect("(display 1e5)", "100000\n");
    // Non-integral reals still use the shortest round-trip form.
    expect("(display (list 1e30 1.5 0.1))",
           "(1000000000000000019884624838656 1.5 0.1)\n");
}

void test_turtle_calls_reach_the_host() {
    RecordingHost host;
    const Run result = run_with_host(
        "(forward 10) (right 90) (fd 5) (penup) (home) (color 1 2 3) "
        "(goto 1 2) (circle 4)",
        host);
    TW_CHECK_EQ(result.output, std::string{});
    TW_CHECK_EQ(host.calls.size(), std::size_t{8});
    TW_CHECK_EQ(host.calls[0], std::string{"forward(10.000000)"});
    TW_CHECK_EQ(host.calls[1], std::string{"right(90.000000)"});
    TW_CHECK_EQ(host.calls[2], std::string{"forward(5.000000)"});
    TW_CHECK_EQ(host.calls[3], std::string{"penup"});
    TW_CHECK_EQ(host.calls[4], std::string{"home"});
    TW_CHECK_EQ(host.calls[5], std::string{"color(1,2,3)"});
    TW_CHECK_EQ(host.calls[6], std::string{"goto(1.000000,2.000000)"});
    TW_CHECK_EQ(host.calls[7], std::string{"circle(4.000000)"});
}

void test_arena_reclaims_everything() {
    Arena arena;
    NullHost host;
    Machine machine{arena, host};
    TW_CHECK_EQ(machine.execute("(display 1)"), std::string{"1\n"});
    TW_CHECK(arena.bytes_allocated() > 0);
    TW_CHECK(arena.symbol_count() > 0);
    arena.release();
    TW_CHECK_EQ(arena.bytes_allocated(), std::size_t{0});
}

}  // namespace

int main() {
    test_reader_tokens();
    test_reader_string_token_keeps_quotes();
    test_reader_bar_symbol();
    test_reader_errors();
    test_reader_atoms();

    test_arithmetic();
    test_integer_is_distinct_from_real();
    test_division_by_zero();
    test_truthiness();
    test_equality_quirks();
    test_booleans_and_conditionals();
    test_case_is_fixed();
    test_binding_forms();
    test_do_loop();
    test_quasiquote();
    test_quasiquote_nested_evaluation();
    test_quote();

    test_closures_and_rest_parameters();
    test_mutation();
    test_define_returns_symbol();
    test_higher_order();
    test_lists();
    test_vectors();
    test_strings();
    test_output_modes();

    test_error_messages();
    test_errors_that_escaped_the_reference_now_raise_properly();
    test_step_budget();

    test_tail_calls_are_constant_space();
    test_non_tail_recursion_uses_the_heap();
    test_deep_accumulator_recursion();
    test_deeply_nested_data_prints_without_recursing();
    test_deep_equal_does_not_recurse();
    test_reader_handles_deep_nesting();

    test_integer_overflow_promotes_instead_of_wrapping();
    test_large_integral_reals_print_every_digit();
    test_turtle_calls_reach_the_host();
    test_arena_reclaims_everything();
    return tw::test::summary("scheme");
}
