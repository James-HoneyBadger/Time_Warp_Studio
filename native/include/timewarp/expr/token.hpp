// SPDX-License-Identifier: MIT
//
// Lexeme representation for the Time Warp expression engine.
//
// Port of ``time_warp.utils.expression_evaluator.Token``.
//
// Design note (why this is not a ``std::variant``):
//   The Python token carried an ``Any`` payload that was either a ``float`` or
//   a ``str``.  Modelling that as ``std::variant<double, std::string>`` would
//   force an owned string per identifier and make every token non-trivially
//   copyable, i.e. a heap allocation per identifier per evaluation.
//
//   Instead ``Token`` is a trivially-copyable 32-byte value type whose
//   ``text`` member is a *non-owning* view: into the expression source for
//   identifiers, and into static storage for the canonical operator
//   spellings.  Tokenising therefore performs zero allocations, and a token
//   vector is a flat, memcpy-able, cache-friendly buffer.

#pragma once

#include <cstdint>
#include <string>
#include <string_view>

namespace timewarp::expr {

/// Lexical categories produced by the tokenizer.
///
/// Mirrors ``Token.Type`` from the Python reference one-for-one.  ``Operator``
/// and ``Comparison`` are kept distinct because the shunting-yard pass
/// distinguishes them.
enum class TokenType : std::uint8_t {
    Number,      ///< numeric literal in ``number``
    Variable,    ///< identifier in ``text`` (may include a BASIC type suffix)
    Function,    ///< identifier in ``text``, followed by ``(`` in the source
    Operator,    ///< arithmetic / bitwise / logical operator spelling in ``text``
    Comparison,  ///< relational operator spelling in ``text``
    LeftParen,   ///< ``(``
    RightParen,  ///< ``)``
    Comma,       ///< ``,``
};

/// Stable spelling for a token type; used in diagnostics and tests.
[[nodiscard]] constexpr std::string_view token_type_name(TokenType type) noexcept {
    switch (type) {
        case TokenType::Number:     return "NUMBER";
        case TokenType::Variable:   return "VARIABLE";
        case TokenType::Function:   return "FUNCTION";
        case TokenType::Operator:   return "OPERATOR";
        case TokenType::Comparison: return "COMPARISON";
        case TokenType::LeftParen:  return "LEFT_PAREN";
        case TokenType::RightParen: return "RIGHT_PAREN";
        case TokenType::Comma:      return "COMMA";
    }
    return "UNKNOWN";
}

/// A single lexeme.
///
/// Lifetime contract: ``text`` borrows from the buffer handed to
/// ``Lexer::tokenize`` (or, for operators, from static storage).  A token
/// vector must not outlive the source it was produced from.
struct Token {
    TokenType type{TokenType::Number};
    double number{0.0};
    std::string_view text{};

    /// Numeric literal token.
    [[nodiscard]] static constexpr Token make_number(double value) noexcept {
        return Token{TokenType::Number, value, {}};
    }

    /// Punctuation / operator token whose spelling is needed downstream.
    ///
    /// ``spelling`` must outlive the token (string literal or source view).
    [[nodiscard]] static constexpr Token make(TokenType type,
                                              std::string_view spelling) noexcept {
        return Token{type, 0.0, spelling};
    }

    /// Punctuation token with no payload (parens, comma).
    [[nodiscard]] static constexpr Token make(TokenType type) noexcept {
        return Token{type, 0.0, {}};
    }

    [[nodiscard]] constexpr bool is_punctuation() const noexcept {
        return type == TokenType::LeftParen || type == TokenType::RightParen ||
               type == TokenType::Comma;
    }

    /// Human-readable rendering such as ``NUMBER(42)`` or ``OPERATOR(//)``.
    [[nodiscard]] std::string describe() const;
};

/// Structural equality of two tokens.  ``text`` is compared by value, so
/// tokens built from different buffers still compare equal.
[[nodiscard]] bool operator==(const Token& lhs, const Token& rhs) noexcept;
[[nodiscard]] inline bool operator!=(const Token& lhs, const Token& rhs) noexcept {
    return !(lhs == rhs);
}

}  // namespace timewarp::expr
