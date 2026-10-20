// SPDX-License-Identifier: MIT
#include "timewarp/expr/token.hpp"

#include "timewarp/core/charconv.hpp"

namespace timewarp::expr {

std::string Token::describe() const {
    std::string out{token_type_name(type)};
    out.push_back('(');
    switch (type) {
        case TokenType::Number:
            // std::to_string gives 6 significant digits; use a round-tripping
            // rendering so diagnostics are unambiguous.
            {
                char buffer[32] = {};
                const std::size_t length =
                    timewarp::core::format_double(buffer, buffer + sizeof(buffer), number);
                out.append(buffer, length);
            }
            break;
        default:
            out.append(text.data(), text.size());
            break;
    }
    out.push_back(')');
    return out;
}

bool operator==(const Token& lhs, const Token& rhs) noexcept {
    // Short-circuit on type first: it rejects most mismatches without touching
    // the payload, and keeps `number` comparisons off the common path.
    if (lhs.type != rhs.type) {
        return false;
    }
    if (lhs.type == TokenType::Number) {
        return lhs.number == rhs.number;
    }
    return lhs.text == rhs.text;
}

}  // namespace timewarp::expr
