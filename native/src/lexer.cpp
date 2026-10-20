// SPDX-License-Identifier: MIT
#include "timewarp/expr/lexer.hpp"

#include <algorithm>
#include <cstdint>
#include <limits>
#include <string>

#include "timewarp/core/charconv.hpp"
#include "timewarp/expr/detail/ascii.hpp"
#include "timewarp/expr/errors.hpp"

namespace timewarp::expr {
namespace {

using detail::digit_value;
using detail::iequals;
using detail::is_alnum;
using detail::is_alpha;
using detail::is_digit;
using detail::is_hex_digit;
using detail::is_octal_digit;
using detail::is_space;
using detail::is_type_suffix;

// ---------------------------------------------------------------------------
// Source helpers
// ---------------------------------------------------------------------------

/// True when ``source[i]`` and ``source[i + 1]`` are exactly ``a`` and ``b``.
[[nodiscard]] constexpr bool matches2(std::string_view source, std::size_t i,
                                      char a, char b) noexcept {
    return i + 2 <= source.size() && source[i] == a && source[i + 1] == b;
}

// ---------------------------------------------------------------------------
// Keyword operators
// ---------------------------------------------------------------------------

/// The word operators the reference lexer folds into operator tokens.
enum class KeywordOp : std::uint8_t { None, And, Or, Not, Mod, Xor, Div, Rem };

/// Length-dispatched, case-insensitive keyword lookup.
///
/// The reference built ``name.upper()`` (allocating) purely to test membership
/// in a 7-element tuple; comparing in place is both cheaper and allocation
/// free.  Sizing on the length first keeps this to one or two compares for the
/// overwhelmingly common "not a keyword" case.
[[nodiscard]] constexpr KeywordOp classify_keyword(std::string_view name) noexcept {
    switch (name.size()) {
        case 2:
            return iequals(name, "OR") ? KeywordOp::Or : KeywordOp::None;
        case 3:
            if (iequals(name, "AND")) return KeywordOp::And;
            if (iequals(name, "NOT")) return KeywordOp::Not;
            if (iequals(name, "MOD")) return KeywordOp::Mod;
            if (iequals(name, "XOR")) return KeywordOp::Xor;
            if (iequals(name, "DIV")) return KeywordOp::Div;
            if (iequals(name, "REM")) return KeywordOp::Rem;
            return KeywordOp::None;
        default:
            return KeywordOp::None;
    }
}

/// Canonical operator spelling for a keyword operator.
///
/// ``DIV`` becomes ``//`` and ``REM`` keeps its own spelling, exactly as in the
/// reference (``REM`` is *not* the same operator as ``MOD``: it is a truncated
/// remainder, whereas ``MOD`` is a floored modulus).
[[nodiscard]] constexpr std::string_view keyword_spelling(KeywordOp op) noexcept {
    switch (op) {
        case KeywordOp::And: return "AND";
        case KeywordOp::Or:  return "OR";
        case KeywordOp::Not: return "NOT";
        case KeywordOp::Mod: return "MOD";
        case KeywordOp::Xor: return "XOR";
        case KeywordOp::Div: return "//";
        case KeywordOp::Rem: return "REM";
        case KeywordOp::None: break;
    }
    return {};
}

// ---------------------------------------------------------------------------
// Numeric literals
// ---------------------------------------------------------------------------

[[nodiscard]] double parse_decimal(std::string_view text) {
    double value = 0.0;
    const char* first = text.data();
    const char* last = text.data() + text.size();
    // Locale-independent, unlike `strtod` -- see timewarp/core/charconv.hpp.
    const char* end = timewarp::core::parse_double(first, last, value);
    // ``1.2.3`` must be rejected exactly like Python's float("1.2.3"); the
    // scanner stops at the second '.', so requiring full consumption is the
    // faithful check.
    if (end != last) {
        throw LexError("Invalid number literal: " + std::string{text});
    }
    return value;
}

/// Parses a digit run in ``base`` (2..36) as a double.
///
/// Fast path: an unsigned 64-bit parse, which is exact.  Longer literals fall
/// back to incremental accumulation so that oversized hex masks behave like
/// Python's arbitrary-precision integers instead of failing.
[[nodiscard]] double parse_radix(std::string_view digits, unsigned base,
                                 std::string_view literal) {
    if (digits.empty()) {
        throw LexError("Invalid " + std::string{literal} + " literal");
    }
    std::uint64_t exact = 0;
    const auto result = std::from_chars(digits.data(),
                                        digits.data() + digits.size(), exact,
                                        static_cast<int>(base));
    if (result.ec == std::errc{} && result.ptr == digits.data() + digits.size()) {
        return static_cast<double>(exact);
    }
    if (result.ec == std::errc::result_out_of_range) {
        double accumulated = 0.0;
        for (const char digit : digits) {
            accumulated = accumulated * static_cast<double>(base) +
                          static_cast<double>(digit_value(digit));
        }
        return accumulated;
    }
    throw LexError("Invalid " + std::string{literal} + " literal");
}

}  // namespace

// ---------------------------------------------------------------------------
// tokenize
// ---------------------------------------------------------------------------

// This mirrors the reference scanner step for step; the branch count is
// inherent to a hand-written lexer for a language with this many literals.
// NOLINTNEXTLINE(readability-function-cognitive-complexity)
std::vector<Token> Lexer::tokenize(std::string_view source) const {
    std::vector<Token> tokens;
    // Worst case is roughly one token per two characters (`1+2+3`); reserving
    // avoids repeated reallocation for the long expressions BASIC programs
    // generate.
    tokens.reserve(std::min<std::size_t>(source.size() / 2 + 1, max_tokens_ + 1));

    const std::size_t size = source.size();
    std::size_t i = 0;

    while (i < size) {
        // Preserved quirk: the reference checked the token budget at the top of
        // every iteration, before skipping whitespace, so a trailing space
        // after the budget is exhausted is also an error.
        if (tokens.size() >= max_tokens_) {
            throw LexError("Expression too complex (max " +
                           std::to_string(max_tokens_) + " tokens)");
        }

        const char ch = source[i];

        // ---- whitespace ---------------------------------------------------
        if (is_space(ch)) {
            ++i;
            continue;
        }

        // ---- radix-prefixed integer literals ------------------------------
        // Checked before the decimal branch: `0x10` must not lex as `0` + `X10`.
        if (ch == '&' && i + 1 < size &&
            (source[i + 1] == 'H' || source[i + 1] == 'h')) {
            const std::size_t start = i + 2;
            std::size_t end = start;
            while (end < size && is_hex_digit(source[end])) {
                ++end;
            }
            tokens.push_back(Token::make_number(
                parse_radix(source.substr(start, end - start), 16, "hex")));
            i = end;
            continue;
        }

        if (ch == '0' && i + 1 < size &&
            (source[i + 1] == 'x' || source[i + 1] == 'X' ||
             source[i + 1] == 'o' || source[i + 1] == 'O')) {
            const bool hexadecimal = source[i + 1] == 'x' || source[i + 1] == 'X';
            const std::size_t start = i + 2;
            std::size_t end = start;
            while (end < size && (hexadecimal ? is_hex_digit(source[end])
                                              : is_octal_digit(source[end]))) {
                ++end;
            }
            const std::string_view literal = hexadecimal ? "0x" : "0o";
            tokens.push_back(Token::make_number(
                parse_radix(source.substr(start, end - start),
                            hexadecimal ? 16U : 8U, literal)));
            i = end;
            continue;
        }

        // ---- decimal literals ---------------------------------------------
        if (is_digit(ch) || ch == '.') {
            const std::size_t start = i;
            while (i < size && (is_digit(source[i]) || source[i] == '.')) {
                ++i;
            }
            tokens.push_back(
                Token::make_number(parse_decimal(source.substr(start, i - start))));
            continue;
        }

        // ---- identifiers, keywords, functions, variables ------------------
        if (is_alpha(ch) || ch == '_') {
            const std::size_t start = i;
            while (i < size && (is_alnum(source[i]) || source[i] == '_')) {
                ++i;
            }
            const std::string_view name = source.substr(start, i - start);

            // Optional BASIC type suffix.  It is consumed here in all cases but
            // only *retained* for variables: the reference dropped it for
            // keywords and function names, so `LEN$(A$)` is the function LEN.
            const bool has_suffix = i < size && is_type_suffix(source[i]);
            if (has_suffix) {
                ++i;
            }

            const KeywordOp keyword = classify_keyword(name);
            if (keyword != KeywordOp::None) {
                tokens.push_back(
                    Token::make(TokenType::Operator, keyword_spelling(keyword)));
                continue;
            }

            if (i < size && source[i] == '(') {
                tokens.push_back(Token::make(TokenType::Function, name));
                continue;
            }

            // A variable's spelling spans the identifier *and* its suffix,
            // which are contiguous in the source -- so the suffix costs no
            // extra storage and the evaluator can recover it from the view.
            const std::size_t length = (i - start);
            tokens.push_back(
                Token::make(TokenType::Variable, source.substr(start, length)));
            continue;
        }

        // ---- two-character operators --------------------------------------
        if (matches2(source, i, '*', '*')) {
            tokens.push_back(Token::make(TokenType::Operator, "^"));
            i += 2;
            continue;
        }
        if (matches2(source, i, '/', '/')) {
            tokens.push_back(Token::make(TokenType::Operator, "//"));
            i += 2;
            continue;
        }
        if (matches2(source, i, '<', '<')) {
            tokens.push_back(Token::make(TokenType::Operator, "<<"));
            i += 2;
            continue;
        }
        if (matches2(source, i, '>', '>')) {
            tokens.push_back(Token::make(TokenType::Operator, ">>"));
            i += 2;
            continue;
        }
        if (matches2(source, i, '/', '\\')) {
            tokens.push_back(Token::make(TokenType::Operator, "AND"));
            i += 2;
            continue;
        }
        if (matches2(source, i, '\\', '/')) {
            tokens.push_back(Token::make(TokenType::Operator, "OR"));
            i += 2;
            continue;
        }

        // ---- single-character operators -----------------------------------
        if (ch == '+' || ch == '-' || ch == '*' || ch == '/' || ch == '%' ||
            ch == '^' || ch == '\\' || ch == '~' || ch == '|' || ch == '&') {
            const Token* previous = tokens.empty() ? nullptr : &tokens.back();
            const bool is_unary =
                previous == nullptr ||
                previous->type == TokenType::Operator ||
                previous->type == TokenType::LeftParen ||
                previous->type == TokenType::Comma ||
                previous->type == TokenType::Comparison;

            if (is_unary && (ch == '+' || ch == '-' || ch == '~')) {
                // Unary plus / minus / bitwise complement: `u` prefix keeps
                // these distinct from their binary counterparts in the
                // shunting-yard pass.
                switch (ch) {
                    case '+': tokens.push_back(Token::make(TokenType::Operator, "u+")); break;
                    case '-': tokens.push_back(Token::make(TokenType::Operator, "u-")); break;
                    default:  tokens.push_back(Token::make(TokenType::Operator, "u~")); break;
                }
            } else if (ch == '\\') {
                // `\` is integer division in binary position and a bitwise
                // complement in unary position (BASIC heritage).
                tokens.push_back(
                    Token::make(TokenType::Operator, is_unary ? "u~" : "\\"));
            } else if (ch == '&') {
                tokens.push_back(Token::make(TokenType::Operator, "AND"));
            } else if (ch == '|') {
                tokens.push_back(Token::make(TokenType::Operator, "OR"));
            } else {
                tokens.push_back(
                    Token::make(TokenType::Operator, source.substr(i, 1)));
            }
            ++i;
            continue;
        }

        // ---- comparisons --------------------------------------------------
        if (ch == '<' || ch == '>' || ch == '=' || ch == '!') {
            const std::size_t start = i;
            ++i;
            if (i < size && (source[i] == '>' || source[i] == '=')) {
                ++i;
            }
            std::string_view spelling = source.substr(start, i - start);
            if (spelling == "<>") {
                spelling = "!=";  // BASIC not-equal -> canonical spelling
            } else if (spelling == "=") {
                spelling = "==";  // BASIC equality test -> comparison, not assign
            }
            tokens.push_back(Token::make(TokenType::Comparison, spelling));
            continue;
        }

        // ---- punctuation --------------------------------------------------
        if (ch == '(') {
            tokens.push_back(Token::make(TokenType::LeftParen));
            ++i;
            continue;
        }
        if (ch == ')') {
            tokens.push_back(Token::make(TokenType::RightParen));
            ++i;
            continue;
        }
        if (ch == ',') {
            tokens.push_back(Token::make(TokenType::Comma));
            ++i;
            continue;
        }

        throw LexError(std::string{"Unexpected character: "} + ch);
    }

    return tokens;
}

}  // namespace timewarp::expr
