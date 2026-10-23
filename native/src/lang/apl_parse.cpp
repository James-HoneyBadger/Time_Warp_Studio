// SPDX-License-Identifier: MIT
//
// The tokenizer and the run state: what a glyph is, where a name ends, and the
// variables, functions and definition a program has in flight.

#include "apl_internal.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace apl_detail {

// ---------------------------------------------------------------------------
// Tokens
// ---------------------------------------------------------------------------

const std::vector<std::string>& glyphs() {
    static const std::vector<std::string> table = {
        "⍳", "⍴", "⌽", "↑", "↓", "⍉", "∈", "∧", "∨", "⍲", "⍱", "≠",
        "≤", "≥", "⍋", "⍒", "⍕", "←", "⍝", "+",  "-",  "×",  "÷",  "*",
        "⍟", "⌈", "⌊", "|",  ",",  "~",  "=",  "<",  ">",  "?",  "/",
        "\\", "(", ")"};
    return table;
}

bool is_digit(char character) {
    return std::isdigit(static_cast<unsigned char>(character)) != 0;
}

bool is_name_start(char character) {
    return std::isalpha(static_cast<unsigned char>(character)) != 0 ||
           character == '_';
}

bool is_name_char(char character) {
    return std::isalnum(static_cast<unsigned char>(character)) != 0 ||
           character == '_';
}

std::vector<std::string> tokenize(std::string_view text) {
    std::vector<std::string> tokens;
    std::size_t index = 0;
    while (index < text.size()) {
        const char character = text[index];
        if (is_space(character)) {
            ++index;
            continue;
        }
        if (character == '\'' || character == '"') {
            const std::size_t closing = text.find(character, index + 1);
            if (closing == std::string_view::npos) {
                throw AplError("unterminated string");
            }
            tokens.push_back(std::string{text.substr(index, closing - index + 1)});
            index = closing + 1;
            continue;
        }
        const bool negative = starts_with(text.substr(index), "¯");
        if (negative || is_digit(character) ||
            (character == '.' && index + 1 < text.size() && is_digit(text[index + 1]))) {
            const std::size_t start = index;
            if (negative) {
                index += 2;  // ¯ is two bytes in UTF-8
            }
            while (index < text.size() &&
                   (is_digit(text[index]) || text[index] == '.')) {
                ++index;
            }
            if (index < text.size() && (text[index] == 'e' || text[index] == 'E')) {
                ++index;
                if (index < text.size() && starts_with(text.substr(index), "¯")) {
                    index += 2;
                }
                while (index < text.size() && is_digit(text[index])) {
                    ++index;
                }
            }
            tokens.push_back(std::string{text.substr(start, index - start)});
            continue;
        }
        if (is_name_start(character)) {
            const std::size_t start = index;
            while (index < text.size() && is_name_char(text[index])) {
                ++index;
            }
            tokens.push_back(upper(text.substr(start, index - start)));
            continue;
        }
        const std::string_view rest = text.substr(index);
        std::string two;
        for (const char* spelling : {">=", "<=", "!=", "=="}) {
            if (starts_with(rest, spelling)) {
                two = spelling;
                break;
            }
        }
        if (!two.empty()) {
            tokens.push_back(two == ">="   ? "≥"
                             : two == "<=" ? "≤"
                             : two == "!=" ? "≠"
                                           : "=");
            index += 2;
            continue;
        }
        bool matched = false;
        for (const std::string& glyph : glyphs()) {
            if (starts_with(rest, glyph)) {
                tokens.push_back(glyph);
                index += glyph.size();
                matched = true;
                break;
            }
        }
        if (matched) {
            continue;
        }
        throw AplError("cannot read '" + std::string(1, character) + "'");
    }
    return tokens;
}

bool is_function(const std::string& token, const AplState* state) {
    const std::string shouted = upper(token);
    if (turtle_words().count(shouted) != 0) {
        return true;
    }
    const std::string plain = canonical(token);
    if (in_list(scalars(), plain) || in_list(structural(), plain)) {
        return true;
    }
    return state != nullptr && state->functions.count(shouted) != 0;
}

bool is_operand(const std::string& token, const AplState* state) {
    if (!token.empty() && (token.front() == '\'' || token.front() == '"')) {
        return true;
    }
    if (starts_with(token, "¯") || token.front() == '.' || is_digit(token.front())) {
        return true;
    }
    if (token == "(") {
        return false;
    }
    if (token == ")") {
        return true;
    }
    if (is_function(token, state)) {
        return false;
    }
    if (in_list(glyphs(), token) || token == "←") {
        return false;
    }
    return true;
}

Value value_of(const std::string& token, const std::map<std::string, Value>& variables) {
    if (!token.empty() && (token.front() == '\'' || token.front() == '"')) {
        return Value::word(token.substr(1, token.size() - 2));
    }
    if (starts_with(token, "¯") || is_digit(token.front()) || token.front() == '.') {
        std::string number = replace_all(token, "¯", "-");
        if (number.find('e') != std::string::npos ||
            number.find('E') != std::string::npos) {
            return Value::numbered(std::strtod(number.c_str(), nullptr));
        }
        if (number.find('.') != std::string::npos) {
            return Value::numbered(std::strtod(number.c_str(), nullptr));
        }
        return Value::numbered(std::strtod(number.c_str(), nullptr));
    }
    const auto found = variables.find(token);
    if (found == variables.end()) {
        return Value::nothing();
    }
    return found->second;
}

}  // namespace apl_detail
}  // namespace timewarp::lang
