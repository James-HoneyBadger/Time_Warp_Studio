// SPDX-License-Identifier: MIT
//
// Lexer conformance tests.
//
// Every expectation below was cross-checked against the Python reference
// (``ExpressionEvaluator._tokenize``) by running the original implementation,
// so a failure here means the port diverged -- except where the header comment
// marks an intentional change (0x/0o literals).

#include <span>
#include <string>
#include <vector>

#include "check.hpp"
#include "timewarp/expr/errors.hpp"
#include "timewarp/expr/lexer.hpp"

using timewarp::expr::LexError;
using timewarp::expr::Lexer;
using timewarp::expr::Token;
using timewarp::expr::TokenType;

namespace {

const Lexer& lexer() {
    static const Lexer instance{};
    return instance;
}

/// Compact rendering such as ``NUMBER(1)OPERATOR(+)NUMBER(2)``.
std::string render(std::span<const Token> tokens) {
    std::string out;
    for (const Token& token : tokens) {
        out += token.describe();
    }
    return out;
}

std::string lex(std::string_view source) {
    // Copy the source into a stable buffer so the returned string is built
    // while the views are still alive.
    std::string owned{source};
    return render(lexer().tokenize(owned));
}

void test_whitespace_and_empty() {
    TW_CHECK_EQ(lex(""), std::string{});
    TW_CHECK_EQ(lex("   \t\n  "), std::string{});
    TW_CHECK_EQ(lex("  1  "), std::string{"NUMBER(1)"});
}

void test_decimal_literals() {
    TW_CHECK_EQ(lex("1"), std::string{"NUMBER(1)"});
    TW_CHECK_EQ(lex("42"), std::string{"NUMBER(42)"});
    TW_CHECK_EQ(lex("1.5"), std::string{"NUMBER(1.5)"});
    TW_CHECK_EQ(lex(".5"), std::string{"NUMBER(0.5)"});
    TW_CHECK_EQ(lex("1."), std::string{"NUMBER(1)"});
    TW_CHECK_EQ(lex("0"), std::string{"NUMBER(0)"});
    TW_CHECK_EQ(lex("3.14159"), std::string{"NUMBER(3.14159)"});
    // Python's float() rejects a second decimal point, and so must we.
    TW_CHECK_THROWS(lexer().tokenize("1.2.3"), LexError);
}

void test_radix_literals() {
    // BASIC &H prefix: works in both implementations.
    TW_CHECK_EQ(lex("&HFF"), std::string{"NUMBER(255)"});
    TW_CHECK_EQ(lex("&hff"), std::string{"NUMBER(255)"});
    TW_CHECK_EQ(lex("&H0A"), std::string{"NUMBER(10)"});

    // DELIBERATE DIVERGENCE: the reference's 0x/0o branch was unreachable, so
    // `0x10` lexed as NUMBER(0) + VARIABLE(X10) and raised "Undefined variable:
    // X10" at evaluation time.  The port restores the documented intent.
    TW_CHECK_EQ(lex("0xFF"), std::string{"NUMBER(255)"});
    TW_CHECK_EQ(lex("0X10"), std::string{"NUMBER(16)"});
    TW_CHECK_EQ(lex("0o17"), std::string{"NUMBER(15)"});
    TW_CHECK_EQ(lex("0O7"), std::string{"NUMBER(7)"});

    // A prefix with no digits is malformed in both implementations.
    TW_CHECK_THROWS(lexer().tokenize("&H"), LexError);
    TW_CHECK_THROWS(lexer().tokenize("0x"), LexError);
    TW_CHECK_THROWS(lexer().tokenize("0o"), LexError);
}

void test_identifiers_and_suffixes() {
    TW_CHECK_EQ(lex("X"), std::string{"VARIABLE(X)"});
    TW_CHECK_EQ(lex("count"), std::string{"VARIABLE(count)"});
    // The suffix is part of the variable's spelling.
    TW_CHECK_EQ(lex("X%"), std::string{"VARIABLE(X%)"});
    TW_CHECK_EQ(lex("N&"), std::string{"VARIABLE(N&)"});
    TW_CHECK_EQ(lex("S!"), std::string{"VARIABLE(S!)"});
    TW_CHECK_EQ(lex("D#"), std::string{"VARIABLE(D#)"});
    TW_CHECK_EQ(lex("A$"), std::string{"VARIABLE(A$)"});
    TW_CHECK_EQ(lex("_tmp"), std::string{"VARIABLE(_tmp)"});
    TW_CHECK_EQ(lex("X1"), std::string{"VARIABLE(X1)"});
}

void test_keyword_operators() {
    TW_CHECK_EQ(lex("AND"), std::string{"OPERATOR(AND)"});
    TW_CHECK_EQ(lex("and"), std::string{"OPERATOR(AND)"});
    TW_CHECK_EQ(lex("aNd"), std::string{"OPERATOR(AND)"});
    TW_CHECK_EQ(lex("OR"), std::string{"OPERATOR(OR)"});
    TW_CHECK_EQ(lex("NOT"), std::string{"OPERATOR(NOT)"});
    TW_CHECK_EQ(lex("MOD"), std::string{"OPERATOR(MOD)"});
    TW_CHECK_EQ(lex("XOR"), std::string{"OPERATOR(XOR)"});
    TW_CHECK_EQ(lex("REM"), std::string{"OPERATOR(REM)"});
    TW_CHECK_EQ(lex("DIV"), std::string{"OPERATOR(//)"});
    // Non-keywords that merely start with one stay variables.
    TW_CHECK_EQ(lex("ANDY"), std::string{"VARIABLE(ANDY)"});
    TW_CHECK_EQ(lex("MODEL"), std::string{"VARIABLE(MODEL)"});
}

void test_functions() {
    TW_CHECK_EQ(lex("SIN("), std::string{"FUNCTION(SIN)LEFT_PAREN()"});
    TW_CHECK_EQ(lex("sin(1)"),
                std::string{"FUNCTION(sin)LEFT_PAREN()NUMBER(1)RIGHT_PAREN()"});
    // A suffix on a function name is consumed and dropped, matching the
    // reference: LEN$(A$) calls LEN.
    TW_CHECK_EQ(lex("LEN$(A$)"),
                std::string{"FUNCTION(LEN)LEFT_PAREN()VARIABLE(A$)RIGHT_PAREN()"});
    TW_CHECK_EQ(lex("f(1, 2)"),
                std::string{"FUNCTION(f)LEFT_PAREN()NUMBER(1)COMMA()NUMBER(2)"
                            "RIGHT_PAREN()"});
}

void test_two_character_operators() {
    TW_CHECK_EQ(lex("**"), std::string{"OPERATOR(^)"});
    TW_CHECK_EQ(lex("//"), std::string{"OPERATOR(//)"});
    TW_CHECK_EQ(lex("<<"), std::string{"OPERATOR(<<)"});
    TW_CHECK_EQ(lex(">>"), std::string{"OPERATOR(>>)"});
    TW_CHECK_EQ(lex("/\\"), std::string{"OPERATOR(AND)"});
    TW_CHECK_EQ(lex("\\/"), std::string{"OPERATOR(OR)"});
    TW_CHECK_EQ(lex("2 ** 3"),
                std::string{"NUMBER(2)OPERATOR(^)NUMBER(3)"});
}

void test_unary_detection() {
    TW_CHECK_EQ(lex("-5"), std::string{"OPERATOR(u-)NUMBER(5)"});
    TW_CHECK_EQ(lex("+5"), std::string{"OPERATOR(u+)NUMBER(5)"});
    TW_CHECK_EQ(lex("~5"), std::string{"OPERATOR(u~)NUMBER(5)"});
    TW_CHECK_EQ(lex("2 * -3"),
                std::string{"NUMBER(2)OPERATOR(*)OPERATOR(u-)NUMBER(3)"});
    TW_CHECK_EQ(lex("(-3)"),
                std::string{"LEFT_PAREN()OPERATOR(u-)NUMBER(3)RIGHT_PAREN()"});
    TW_CHECK_EQ(lex("f(-3)"),
                std::string{"FUNCTION(f)LEFT_PAREN()OPERATOR(u-)NUMBER(3)"
                            "RIGHT_PAREN()"});
    TW_CHECK_EQ(lex("2 > -3"),
                std::string{"NUMBER(2)COMPARISON(>)OPERATOR(u-)NUMBER(3)"});
    // Trailing binary use of the same glyphs.
    TW_CHECK_EQ(lex("2 - 3"), std::string{"NUMBER(2)OPERATOR(-)NUMBER(3)"});
    TW_CHECK_EQ(lex("2-3"), std::string{"NUMBER(2)OPERATOR(-)NUMBER(3)"});
}

void test_backslash_and_bitwise_glyphs() {
    // `\` is integer division when binary and bitwise complement when unary.
    TW_CHECK_EQ(lex("7 \\ 2"),
                std::string{"NUMBER(7)OPERATOR(\\)NUMBER(2)"});
    TW_CHECK_EQ(lex("\\ 2"), std::string{"OPERATOR(u~)NUMBER(2)"});
    // `&` and `|` degrade to AND/OR.
    TW_CHECK_EQ(lex("A & B"),
                std::string{"VARIABLE(A)OPERATOR(AND)VARIABLE(B)"});
    TW_CHECK_EQ(lex("A | B"),
                std::string{"VARIABLE(A)OPERATOR(OR)VARIABLE(B)"});
}

void test_comparisons() {
    TW_CHECK_EQ(lex("3 = 3"),
                std::string{"NUMBER(3)COMPARISON(==)NUMBER(3)"});
    TW_CHECK_EQ(lex("3 <> 4"),
                std::string{"NUMBER(3)COMPARISON(!=)NUMBER(4)"});
    TW_CHECK_EQ(lex("1 <= 2"),
                std::string{"NUMBER(1)COMPARISON(<=)NUMBER(2)"});
    TW_CHECK_EQ(lex("1 >= 2"),
                std::string{"NUMBER(1)COMPARISON(>=)NUMBER(2)"});
    TW_CHECK_EQ(lex("1<2"), std::string{"NUMBER(1)COMPARISON(<)NUMBER(2)"});
    TW_CHECK_EQ(lex("1>2"), std::string{"NUMBER(1)COMPARISON(>)NUMBER(2)"});
}

void test_punctuation_and_errors() {
    TW_CHECK_EQ(lex("(1)"),
                std::string{"LEFT_PAREN()NUMBER(1)RIGHT_PAREN()"});
    TW_CHECK_EQ(lex("(1,2)"),
                std::string{"LEFT_PAREN()NUMBER(1)COMMA()NUMBER(2)"
                            "RIGHT_PAREN()"});
    TW_CHECK_THROWS(lexer().tokenize("@"), LexError);
    TW_CHECK_THROWS(lexer().tokenize("1 @ 2"), LexError);
    // '$' is only valid as a variable suffix, never standalone.
    TW_CHECK_THROWS(lexer().tokenize("1 $ 2"), LexError);
}

void test_token_budget() {
    const Lexer tight{3};
    TW_CHECK_EQ(tight.tokenize("1+2").size(), std::size_t{3});
    // Preserved quirk: the reference checked the budget at the top of every
    // loop iteration, *before* skipping whitespace, so a trailing blank after
    // the budget is exhausted still trips the limit.
    TW_CHECK_THROWS(tight.tokenize("1+2 "), LexError);
    TW_CHECK_THROWS(tight.tokenize("1+2+3"), LexError);

    const Lexer wide{1000};
    std::string long_expression;
    for (int i = 0; i < 400; ++i) {
        long_expression += "1+";
    }
    long_expression += "1";
    TW_CHECK_EQ(wide.tokenize(long_expression).size(), std::size_t{801});
}

void test_token_value_semantics() {
    const std::string owned{"X"};
    const Token from_source = Token::make(TokenType::Variable, owned);
    const Token same = Token::make(TokenType::Variable, "X");
    TW_CHECK(from_source == same);
    TW_CHECK_EQ(from_source.describe(), std::string{"VARIABLE(X)"});
    TW_CHECK(Token::make_number(1.0) != Token::make_number(2.0));
    TW_CHECK(Token::make_number(1.0) == Token::make_number(1.0));
    TW_CHECK(Token::make(TokenType::LeftParen).is_punctuation());
}

}  // namespace

int main() {
    test_whitespace_and_empty();
    test_decimal_literals();
    test_radix_literals();
    test_identifiers_and_suffixes();
    test_keyword_operators();
    test_functions();
    test_two_character_operators();
    test_unary_detection();
    test_backslash_and_bitwise_glyphs();
    test_comparisons();
    test_punctuation_and_errors();
    test_token_budget();
    test_token_value_semantics();
    return tw::test::summary("lexer");
}
