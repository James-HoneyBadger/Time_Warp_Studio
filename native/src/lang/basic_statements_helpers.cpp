// SPDX-License-Identifier: MIT

#include "basic_statements_internal.hpp"
//
// The BASIC statement dispatch, ported from `execute_basic` in
// `time_warp/languages/basic.py`.
//
// The order of the tests is behaviour, not style: `PRINT USING` has to be read
// before `PRINT`, `LINE INPUT` before `INPUT`, and the implicit-`LET` test
// before `COLOR`, or a statement silently becomes a different one. It is copied
// as it stands for that reason.

#include "basic_internal.hpp"

#include "timewarp/io/gamepad.hpp"
#include "timewarp/lang/logo.hpp"

#include <algorithm>
#include <thread>
#include <chrono>
#include <cctype>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <regex>
#include <string>
#include <utility>

namespace timewarp::lang::basic {

// -- helpers shared with basic.cpp -----------------------------------------

bool is_all_digits(std::string_view text) {
    if (text.empty()) {
        return false;
    }
    for (const char character : text) {
        if (std::isdigit(static_cast<unsigned char>(character)) == 0) {
            return false;
        }
    }
    return true;
}

/// The reference's `is_logo_line`: a statement worth handing to Logo.
bool is_logo_line(std::string_view command) {
    const std::string text = trim(command);
    if (text.empty()) {
        return false;
    }
    const std::size_t space = text.find_first_of(" \t");
    const std::string first = upper(text.substr(0, space));
    if (is_logo_command(first)) {
        return true;
    }
    if (text.rfind(":", 0) == 0 || text.front() == '"' || text.front() == ';') {
        return true;
    }
    // A bracketed body is a Logo block however its head is spelled.
    return text.find('[') != std::string::npos || text.find(']') != std::string::npos;
}

bool division_by_zero(std::string_view message) {
    return message.rfind("Division by zero", 0) == 0;
}

std::string strip_quotes(std::string_view text) {
    std::size_t start = 0;
    std::size_t end = text.size();
    while (start < end && text[start] == '"') {
        ++start;
    }
    while (end > start && text[end - 1] == '"') {
        --end;
    }
    return std::string{text.substr(start, end - start)};
}

bool has_string_concat(std::string_view expression) {
    bool in_string = false;
    char quote = '\0';
    int depth = 0;
    bool has_text = false;
    bool has_plus = false;
    for (const char character : expression) {
        if (in_string) {
            if (character == quote) {
                in_string = false;
            }
        } else if (character == '"' || character == '\'') {
            in_string = true;
            quote = character;
            has_text = true;
        } else if (character == '(') {
            ++depth;
        } else if (character == ')') {
            --depth;
        } else if (character == '+' && depth == 0) {
            has_plus = true;
        }
    }
    if (expression.find('$') != std::string_view::npos) {
        has_text = true;
    }
    return has_text && has_plus;
}

bool mentions_string_function(std::string_view upper_expression) {
    static const char* const heads[] = {
        "LEN(",     "LEFT(",   "RIGHT(",    "MID(",       "INSTR(",    "UPPER(",
        "LOWER(",   "LCASE(",  "LCASE$(",   "UCASE(",     "UCASE$(",   "TRIM(",
        "LTRIM(",   "RTRIM(",  "LPAD(",     "LPAD$(",     "RPAD(",     "RPAD$(",
        "FORMAT(",  "FORMAT$(", "ENVIRON(", "ENVIRON$(",  "REPEAT(",   "REPEAT$(",
        "SPACE(",   "SPACE$(", "STRING$(",  "STR(",       "VAL(",      "CHR(",
        "CHR$("};
    for (const char* head : heads) {
        if (upper_expression.rfind(head, 0) == 0) {
            return true;
        }
        // `MID$(...` is the documented spelling; the reference's pattern allows
        // the `$` on every name, so it is allowed here on every name too.
        const std::string name{head};
        if (name.size() >= 2 && name.compare(name.size() - 2, 2, "$(") != 0) {
            const std::string with_dollar = name.substr(0, name.size() - 1) + "$(";
            if (upper_expression.rfind(with_dollar, 0) == 0) {
                return true;
            }
        }
    }
    return false;
}

std::optional<std::string> string_operand(Machine& machine, std::string_view expression) {
    const std::string expr = trim(expression);
    if (expr.size() >= 2 && expr.front() == '"' && expr.back() == '"') {
        return expr.substr(1, expr.size() - 2);
    }
    if (!expr.empty() && expr.back() == '$') {
        const auto found = machine.interpreter().variables().find(upper(expr));
        if (found != machine.interpreter().variables().end() && found->second.is_text()) {
            return found->second.text();
        }
        return std::string{};
    }
    if (expr.find("$(") != std::string::npos) {
        try {
            Strings strings{machine.interpreter(), machine.state(), &machine};
            return strings.evaluate(expr);
        } catch (const BasicError&) {
            return std::nullopt;
        }
    }
    return std::nullopt;
}

int try_string_comparison(Machine& machine, std::string_view condition) {
    const std::string text{condition};
    if (text.find('$') == std::string::npos && text.find("\"\"") == std::string::npos) {
        return -1;
    }
    struct Operator {
        const char* symbol;
        int kind;  // 0 ne, 1 ge, 2 le, 3 eq, 4 lt, 5 gt
    };
    static const Operator operators[] = {
        {"<>", 0}, {">=", 1}, {"<=", 2}, {"=", 3}, {"<", 4}, {">", 5}};
    for (const Operator& op : operators) {
        const std::string symbol{op.symbol};
        const std::size_t at = text.find(symbol);
        if (at == std::string::npos) {
            continue;
        }
        const auto lhs = string_operand(machine, text.substr(0, at));
        const auto rhs = string_operand(machine, text.substr(at + symbol.size()));
        if (!lhs.has_value() || !rhs.has_value()) {
            continue;
        }
        const int comparison = lhs->compare(*rhs);
        switch (op.kind) {
            case 0:
                return comparison != 0 ? 1 : 0;
            case 1:
                return comparison >= 0 ? 1 : 0;
            case 2:
                return comparison <= 0 ? 1 : 0;
            case 3:
                return comparison == 0 ? 1 : 0;
            case 4:
                return comparison < 0 ? 1 : 0;
            default:
                return comparison > 0 ? 1 : 0;
        }
    }
    return -1;
}

std::string substitute_undefined(std::string_view expression, std::string_view name) {
    std::string result;
    const std::string text{expression};
    const auto word = [](char character) {
        return std::isalnum(static_cast<unsigned char>(character)) != 0 || character == '_';
    };
    std::size_t index = 0;
    while (index < text.size()) {
        const bool before = index == 0 || !word(text[index - 1]);
        if (before && text.compare(index, name.size(), name) == 0) {
            const std::size_t after = index + name.size();
            if (after >= text.size() || !word(text[after])) {
                result.push_back('0');
                index = after;
                continue;
            }
        }
        result.push_back(text[index]);
        ++index;
    }
    return result;
}

/// `int(text)`: whole-string, optional sign, no fraction.
std::optional<long> parse_integer(std::string_view text) {
    const std::string trimmed = trim(text);
    if (trimmed.empty()) {
        return std::nullopt;
    }
    const char* begin = trimmed.c_str();
    char* end = nullptr;
    errno = 0;
    const long value = std::strtol(begin, &end, 10);
    if (end == begin || *end != '\0' || errno == ERANGE) {
        return std::nullopt;
    }
    return value;
}

std::string join(const std::vector<std::string>& values, std::string_view separator) {
    std::string result;
    for (std::size_t index = 0; index < values.size(); ++index) {
        if (index != 0) {
            result += separator;
        }
        result += values[index];
    }
    return result;
}

bool starts_with(std::string_view text, std::string_view prefix) {
    return text.rfind(prefix, 0) == 0;
}

/// `text.substr(from)` that answers "" instead of throwing when the command is
/// shorter than the keyword -- `CLOSE` is five characters and its handler slices
/// from six.
std::string tail(std::string_view text, std::size_t from) {
    return from >= text.size() ? std::string{} : std::string{text.substr(from)};
}

}  // namespace timewarp::lang::basic
