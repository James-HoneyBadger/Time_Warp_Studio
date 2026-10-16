// SPDX-License-Identifier: MIT
//
// Evaluator conformance tests.
//
// Every numeric expectation was produced by running the Python reference
// implementation (``ExpressionEvaluator``) with the same inputs, so the port is
// verified against observed behaviour rather than against a re-reading of it.

#include <optional>
#include <string>
#include <string_view>

#include "check.hpp"
#include "timewarp/expr/errors.hpp"
#include "timewarp/expr/evaluator.hpp"

using timewarp::expr::DivisionByZeroError;
using timewarp::expr::EvalContext;
using timewarp::expr::EvalError;
using timewarp::expr::ExpressionEvaluator;
using timewarp::expr::LexError;
using timewarp::expr::ParseError;

namespace {

/// Captures an exception message ("" when nothing was thrown).
template <class F>
std::string message_of(F&& thunk) {
    try {
        thunk();
    } catch (const std::exception& error) {
        return error.what();
    }
    return {};
}

/// Evaluator wired up exactly like the probe run against the reference.
ExpressionEvaluator make_evaluator() {
    static const double kTable[] = {0.0, 10.0, 20.0, 30.0};

    EvalContext context;
    context.variables = {{"X", 10.0}, {"A", 4.0}, {"B", 0.0}};
    context.arrays = {{"TAB", kTable}};
    context.string_variables = {{"NAME$", "Time Warp"}, {"OTHER$", "Time Warp"}};
    context.special_variables = [](std::string_view name)
        -> std::optional<double> {
        if (name == "TIMER") {
            return 42.0;
        }
        return std::nullopt;
    };

    ExpressionEvaluator evaluator{std::move(context)};
    evaluator.set_array("EXTRA", kTable);
    return evaluator;
}

void test_arithmetic_and_precedence() {
    auto evaluator = make_evaluator();

    TW_CHECK_EQ(evaluator.evaluate("2 + 3 * X"), 32.0);
    TW_CHECK_EQ(evaluator.evaluate("2 + 3 * 4"), 14.0);
    TW_CHECK_EQ(evaluator.evaluate("(2 + 3) * 4"), 20.0);
    TW_CHECK_EQ(evaluator.evaluate("2 * -3"), -6.0);
    TW_CHECK_EQ(evaluator.evaluate("2 ** 3"), 8.0);
    // '^' is right-associative: 2^(3^2) == 512, not (2^3)^2 == 64.
    TW_CHECK_EQ(evaluator.evaluate("2 ^ 3 ^ 2"), 512.0);
    TW_CHECK_EQ(evaluator.evaluate("-5 + 2"), -3.0);
    TW_CHECK_EQ(evaluator.evaluate("10 / 4"), 2.5);
}

void test_integer_division_family() {
    auto evaluator = make_evaluator();

    // '//' floors (Python semantics).
    TW_CHECK_EQ(evaluator.evaluate("7 // 2"), 3.0);
    TW_CHECK_EQ(evaluator.evaluate("-7 // 2"), -4.0);
    // '\' truncates toward zero (BASIC semantics) -- a real behavioural split.
    TW_CHECK_EQ(evaluator.evaluate("7 \\ 2"), 3.0);
    TW_CHECK_EQ(evaluator.evaluate("-7 \\ 2"), -3.0);
    // '%' / MOD are floored: the sign follows the divisor.
    TW_CHECK_EQ(evaluator.evaluate("7 % 3"), 1.0);
    TW_CHECK_EQ(evaluator.evaluate("-7 % 3"), 2.0);
    TW_CHECK_EQ(evaluator.evaluate("7 MOD 3"), 1.0);
    TW_CHECK_EQ(evaluator.evaluate("-7 MOD 3"), 2.0);
    TW_CHECK_EQ(evaluator.evaluate("7 REM 3"), 1.0);
    // REM is the truncated counterpart: the sign follows the dividend.
    TW_CHECK_EQ(evaluator.evaluate("-7 REM 3"), -1.0);
}

void test_bitwise_and_logical() {
    auto evaluator = make_evaluator();

    TW_CHECK_EQ(evaluator.evaluate("5 OR 2"), 7.0);
    TW_CHECK_EQ(evaluator.evaluate("5 XOR 3"), 6.0);
    TW_CHECK_EQ(evaluator.evaluate("6 AND 3"), 2.0);
    TW_CHECK_EQ(evaluator.evaluate("1 << 4"), 16.0);
    TW_CHECK_EQ(evaluator.evaluate("256 >> 4"), 16.0);
    // NOT is a unary prefix operator yielding 1.0 / 0.0.
    TW_CHECK_EQ(evaluator.evaluate("NOT 0"), 1.0);
    TW_CHECK_EQ(evaluator.evaluate("NOT 5"), 0.0);
    // AND has higher precedence than OR, lower than '+'.
    TW_CHECK_EQ(evaluator.evaluate("1 + 2 AND 3"), 3.0);
    TW_CHECK_EQ(evaluator.evaluate("A & B | 1"), 1.0);
}

void test_comparisons() {
    auto evaluator = make_evaluator();

    TW_CHECK_EQ(evaluator.evaluate("2 + 3 > 4"), 1.0);
    TW_CHECK_EQ(evaluator.evaluate("2 + 3 > 9"), 0.0);
    TW_CHECK_EQ(evaluator.evaluate("3 = 3"), 1.0);
    TW_CHECK_EQ(evaluator.evaluate("3 <> 4"), 1.0);
    TW_CHECK_EQ(evaluator.evaluate("3 <> 3"), 0.0);
    TW_CHECK_EQ(evaluator.evaluate("1 < 2 AND 3 > 2"), 1.0);
    TW_CHECK_EQ(evaluator.evaluate("1 <= 1"), 1.0);
    TW_CHECK_EQ(evaluator.evaluate("1 >= 2"), 0.0);
}

void test_math_builtins() {
    auto evaluator = make_evaluator();

    TW_CHECK_NEAR(evaluator.evaluate("SIN(PI / 2)"), 1.0, 1e-12);
    TW_CHECK_NEAR(evaluator.evaluate("COS(0)"), 1.0, 1e-12);
    TW_CHECK_EQ(evaluator.evaluate("SQRT(16)"), 4.0);
    TW_CHECK_EQ(evaluator.evaluate("SQR(9)"), 3.0);
    TW_CHECK_EQ(evaluator.evaluate("ABS(0 - 5)"), 5.0);
    // INT floors (Python math.floor), so INT(-3.7) == -4.
    TW_CHECK_EQ(evaluator.evaluate("INT(3.7)"), 3.0);
    TW_CHECK_EQ(evaluator.evaluate("INT(-3.7)"), -4.0);
    // FIX rounds toward zero.
    TW_CHECK_EQ(evaluator.evaluate("FIX(3.7)"), 3.0);
    TW_CHECK_EQ(evaluator.evaluate("FIX(-3.7)"), -3.0);
    // CINT / ROUND are half-to-even, matching Python's round().
    TW_CHECK_EQ(evaluator.evaluate("CINT(2.5)"), 2.0);
    TW_CHECK_EQ(evaluator.evaluate("CINT(3.5)"), 4.0);
    TW_CHECK_EQ(evaluator.evaluate("ROUND(2.5)"), 2.0);
    TW_CHECK_EQ(evaluator.evaluate("TRUNC(-3.7)"), -3.0);
    TW_CHECK_EQ(evaluator.evaluate("SGN(-9)"), -1.0);
    TW_CHECK_EQ(evaluator.evaluate("SGN(9)"), 1.0);
    TW_CHECK_EQ(evaluator.evaluate("SGN(0)"), 0.0);
    TW_CHECK_EQ(evaluator.evaluate("MIN(3, 7)"), 3.0);
    TW_CHECK_EQ(evaluator.evaluate("MAX(3, 7)"), 7.0);
    TW_CHECK_EQ(evaluator.evaluate("POW(2, 10)"), 1024.0);
    TW_CHECK_EQ(evaluator.evaluate("ATAN2(0, 1)"), 0.0);
    TW_CHECK_EQ(evaluator.evaluate("EXP(0)"), 1.0);
    TW_CHECK_EQ(evaluator.evaluate("LOG(1)"), 0.0);
    TW_CHECK_EQ(evaluator.evaluate("LOG10(1000)"), 3.0);
    TW_CHECK_EQ(evaluator.evaluate("LOG2(8)"), 3.0);
    TW_CHECK_EQ(evaluator.evaluate("FLOOR(2.9)"), 2.0);
    TW_CHECK_EQ(evaluator.evaluate("CEIL(2.1)"), 3.0);
    TW_CHECK_EQ(evaluator.evaluate("VAL(7)"), 7.0);
    TW_CHECK_EQ(evaluator.evaluate("FLOAT(7)"), 7.0);
    TW_CHECK_EQ(evaluator.evaluate("INTEGER(-3.7)"), -3.0);
}

void test_constants() {
    auto evaluator = make_evaluator();

    TW_CHECK_NEAR(evaluator.evaluate("PI"), 3.141592653589793, 1e-15);
    TW_CHECK_NEAR(evaluator.evaluate("E"), 2.718281828459045, 1e-15);
    TW_CHECK_NEAR(evaluator.evaluate("TAU"), 6.283185307179586, 1e-15);
    TW_CHECK_EQ(evaluator.evaluate("TRUE"), 1.0);
    TW_CHECK_EQ(evaluator.evaluate("FALSE"), 0.0);
    TW_CHECK_EQ(evaluator.evaluate("PI > 3"), 1.0);
    // Constants are only consulted when no variable shadows them.
    evaluator.set_variable("PI", 3.0);
    TW_CHECK_EQ(evaluator.evaluate("PI"), 3.0);
}

void test_variables_arrays_and_strings() {
    auto evaluator = make_evaluator();

    // Type suffixes are stripped for numeric lookups: X% and X share a slot.
    TW_CHECK_EQ(evaluator.evaluate("X% + 1"), 11.0);
    TW_CHECK_EQ(evaluator.evaluate("X# + 1"), 11.0);
    // Variables are case-insensitive.
    TW_CHECK_EQ(evaluator.evaluate("x + 1"), 11.0);
    evaluator.set_variable("count", 3.0);
    TW_CHECK_EQ(evaluator.evaluate("COUNT * 2"), 6.0);

    // Array access, with both a literal and a variable index.
    TW_CHECK_EQ(evaluator.evaluate("TAB(2)"), 20.0);
    TW_CHECK_EQ(evaluator.evaluate("TAB(A - 1)"), 30.0);
    TW_CHECK_EQ(evaluator.evaluate("EXTRA(1) + 1"), 11.0);
    // A=4, and the table holds four elements, so the reference's explicit
    // bounds check rejects it (negative indices too).
    TW_CHECK_THROWS(evaluator.evaluate("TAB(A)"), EvalError);
    TW_CHECK_THROWS(evaluator.evaluate("TAB(4)"), EvalError);
    TW_CHECK_THROWS(evaluator.evaluate("TAB(0 - 1)"), EvalError);

    // String variables feed LEN(); unknown ones read as "".
    TW_CHECK_EQ(evaluator.evaluate("LEN(NAME$)"), 9.0);
    TW_CHECK_EQ(evaluator.evaluate("LEN(MISSING$)"), 0.0);
    TW_CHECK_EQ(evaluator.evaluate("NAME$ = OTHER$"), 1.0);
    TW_CHECK_EQ(evaluator.evaluate("NAME$ = MISSING$"), 0.0);

    evaluator.set_string_variable("num$", "  12.5  ");
    TW_CHECK_EQ(evaluator.evaluate("VAL(num$)"), 12.5);
    TW_CHECK_EQ(evaluator.evaluate("FLOAT(num$)"), 12.5);
}

void test_special_variables() {
    auto evaluator = make_evaluator();
    TW_CHECK_EQ(evaluator.evaluate("TIMER + 1"), 43.0);

    // Without a hook the name is simply unknown.
    ExpressionEvaluator plain;
    TW_CHECK_THROWS(plain.evaluate("TIMER"), EvalError);
    TW_CHECK_EQ(message_of([&] { (void)plain.evaluate("TIMER"); }),
                std::string{"Undefined variable: TIMER"});
}

void test_random() {
    auto evaluator = make_evaluator();

    // RND/RAND are dynamic generators, both bare and in call form.
    for (int i = 0; i < 64; ++i) {
        const double bare = evaluator.evaluate("RND");
        TW_CHECK(bare >= 0.0 && bare < 1.0);
        const double called = evaluator.evaluate("RND(1)");
        TW_CHECK(called >= 0.0 && called < 1.0);
        const double rand_named = evaluator.evaluate("RAND");
        TW_CHECK(rand_named >= 0.0 && rand_named < 1.0);
    }
    // RANDOM(n) is uniform over [0, n).
    for (int i = 0; i < 64; ++i) {
        const double bounded = evaluator.evaluate("RANDOM(10)");
        TW_CHECK(bounded >= 0.0 && bounded < 10.0);
    }
    TW_CHECK_EQ(evaluator.evaluate("RANDOM(0)"), 0.0);
    TW_CHECK_EQ(evaluator.evaluate("RANDOM(-5)"), 0.0);
}

void test_error_messages() {
    auto evaluator = make_evaluator();

    // Lexing.
    TW_CHECK_THROWS(evaluator.evaluate("1 @ 2"), LexError);
    TW_CHECK_EQ(message_of([&] { (void)evaluator.evaluate("1 @ 2"); }),
                std::string{"Unexpected character: @"});

    // Parsing.
    TW_CHECK_THROWS(evaluator.evaluate("(1 + 2"), ParseError);
    TW_CHECK_EQ(message_of([&] { (void)evaluator.evaluate("(1 + 2"); }),
                std::string{"Mismatched parentheses"});
    TW_CHECK_THROWS(evaluator.evaluate("1 + 2)"), ParseError);

    // Evaluation.
    TW_CHECK_THROWS(evaluator.evaluate(""), EvalError);
    TW_CHECK_EQ(message_of([&] { (void)evaluator.evaluate(""); }),
                std::string{"Invalid expression"});
    TW_CHECK_THROWS(evaluator.evaluate("1 +"), EvalError);
    TW_CHECK_THROWS(evaluator.evaluate("Q"), EvalError);
    TW_CHECK_EQ(message_of([&] { (void)evaluator.evaluate("Q"); }),
                std::string{"Undefined variable: Q"});
    TW_CHECK_THROWS(evaluator.evaluate("NOPE(1)"), EvalError);
    TW_CHECK_EQ(message_of([&] { (void)evaluator.evaluate("NOPE(1)"); }),
                std::string{"Unknown function: NOPE"});
    TW_CHECK_THROWS(evaluator.evaluate("LEN(5)"), EvalError);
    TW_CHECK_THROWS(evaluator.evaluate("MIN(1)"), EvalError);
    // A bare string result is not a number.
    TW_CHECK_THROWS(evaluator.evaluate("NAME$"), EvalError);
}

void test_division_by_zero() {
    auto evaluator = make_evaluator();

    TW_CHECK_THROWS(evaluator.evaluate("1 / 0"), DivisionByZeroError);
    TW_CHECK_EQ(message_of([&] { (void)evaluator.evaluate("1 / 0"); }),
                std::string{"Division by zero"});
    TW_CHECK_THROWS(evaluator.evaluate("1 // 0"), DivisionByZeroError);
    TW_CHECK_EQ(message_of([&] { (void)evaluator.evaluate("1 // 0"); }),
                std::string{"Division by zero (integer division)"});
    TW_CHECK_THROWS(evaluator.evaluate("1 % 0"), DivisionByZeroError);
    TW_CHECK_EQ(message_of([&] { (void)evaluator.evaluate("1 % 0"); }),
                std::string{"Division by zero (MOD)"});
    TW_CHECK_THROWS(evaluator.evaluate("1 REM 0"), DivisionByZeroError);
    TW_CHECK_EQ(message_of([&] { (void)evaluator.evaluate("1 REM 0"); }),
                std::string{"Division by zero (REM)"});
    TW_CHECK_THROWS(evaluator.evaluate("1 \\ 0"), DivisionByZeroError);
    TW_CHECK_THROWS(evaluator.evaluate("0 ^ -1"), DivisionByZeroError);

    // DivisionByZeroError is catchable as EvalError / ExprError.
    TW_CHECK_THROWS(evaluator.evaluate("5 / (3 - 3)"), EvalError);
}

void test_domain_errors() {
    auto evaluator = make_evaluator();

    // Python's math module raises ValueError for these; leaking NaN into the
    // BASIC console instead would be a regression.
    TW_CHECK_THROWS(evaluator.evaluate("SQRT(0 - 1)"), EvalError);
    TW_CHECK_EQ(message_of([&] { (void)evaluator.evaluate("SQRT(0 - 1)"); }),
                std::string{"math domain error"});
    TW_CHECK_THROWS(evaluator.evaluate("LOG(0)"), EvalError);
    TW_CHECK_THROWS(evaluator.evaluate("LOG2(0)"), EvalError);
    TW_CHECK_THROWS(evaluator.evaluate("ASIN(2)"), EvalError);
    TW_CHECK_THROWS(evaluator.evaluate("ACOS(0 - 2)"), EvalError);
    TW_CHECK_THROWS(evaluator.evaluate("(0 - 8) ^ 0.5"), EvalError);

    // Integer operands outside the 64-bit range are rejected rather than
    // producing undefined behaviour (Python would use a bignum).
    TW_CHECK_THROWS(
        evaluator.evaluate("999999999999999999999999 AND 1"), EvalError);
    TW_CHECK_EQ(
        message_of(
            [&] { (void)evaluator.evaluate("999999999999999999999999 AND 1"); }),
        std::string{"integer operand out of 64-bit range"});

    // Shared limitation with the reference: the number scanner consumes only
    // digits and dots, so exponent notation is *not* a numeric literal.
    TW_CHECK_EQ(message_of([&] { (void)evaluator.evaluate("1e300"); }),
                std::string{"Undefined variable: E300"});
}

void test_phased_api() {
    ExpressionEvaluator evaluator;
    evaluator.set_variable("X", 2.0);

    // Keep the source alive: tokens borrow from it.
    const std::string source = "X * X";
    const auto tokens = evaluator.tokenize(source);
    TW_CHECK_EQ(tokens.size(), std::size_t{3});
    const auto rpn = evaluator.to_rpn(tokens);
    TW_CHECK_EQ(rpn.size(), std::size_t{3});

    TW_CHECK_EQ(evaluator.evaluate_rpn(rpn), 4.0);
    evaluator.set_variable("X", 3.0);
    // Same compiled program, new variable value.
    TW_CHECK_EQ(evaluator.evaluate_rpn(rpn), 9.0);
}

void test_token_cache() {
    ExpressionEvaluator evaluator;
    evaluator.set_variable("X", 1.0);

    TW_CHECK_EQ(evaluator.cached_expression_count(), std::size_t{0});
    TW_CHECK_EQ(evaluator.evaluate("X + 1"), 2.0);
    TW_CHECK_EQ(evaluator.cached_expression_count(), std::size_t{1});
    // The cache holds tokens only, so the new value is observed.
    evaluator.set_variable("X", 5.0);
    TW_CHECK_EQ(evaluator.evaluate("X + 1"), 6.0);
    TW_CHECK_EQ(evaluator.cached_expression_count(), std::size_t{1});
    TW_CHECK_EQ(evaluator.evaluate("X + 2"), 7.0);
    TW_CHECK_EQ(evaluator.cached_expression_count(), std::size_t{2});

    evaluator.clear_token_cache();
    TW_CHECK_EQ(evaluator.cached_expression_count(), std::size_t{0});
    TW_CHECK_EQ(evaluator.evaluate("X + 1"), 6.0);
}

void test_token_budget_and_mutation() {
    ExpressionEvaluator evaluator;
    evaluator.set_max_tokens(3);
    TW_CHECK_THROWS(evaluator.evaluate("1+2+3"), LexError);
    TW_CHECK_EQ(evaluator.evaluate("1+2"), 3.0);

    evaluator.set_max_tokens(ExpressionEvaluator::kDefaultMaxTokens);
    TW_CHECK_EQ(evaluator.evaluate("1+2+3"), 6.0);
}

}  // namespace

int main() {
    test_arithmetic_and_precedence();
    test_integer_division_family();
    test_bitwise_and_logical();
    test_comparisons();
    test_math_builtins();
    test_constants();
    test_variables_arrays_and_strings();
    test_special_variables();
    test_random();
    test_error_messages();
    test_division_by_zero();
    test_domain_errors();
    test_phased_api();
    test_token_cache();
    test_token_budget_and_mutation();
    return tw::test::summary("evaluator");
}
