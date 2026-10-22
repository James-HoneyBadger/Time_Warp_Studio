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

std::string Machine::print(const std::string& args_in) {
    if (trim(args_in).empty()) {
        return "\n";
    }
    const std::string args = trim(args_in);
    const bool suppress_newline = args.back() == ';' || args.back() == ',';
    const std::vector<std::string> parts = split_top(args_in, ";,");
    if (parts.empty()) {
        return "\n";
    }
    // The reference's `_print_expression_failure`: an unset numeric variable is
    // 0, and anything else becomes an error line that replaces the whole PRINT.
    const auto numeric = [this](const std::string& item,
                                std::string& error) -> std::optional<double> {
        try {
            return eval(item);
        } catch (const std::exception& thrown) {
            const std::string message = thrown.what();
            if (division_by_zero(message)) {
                throw;
            }
            if (starts_with(message, "Undefined variable: ")) {
                const std::string name = trim(message.substr(20));
                const std::string retried = substitute_undefined(item, name);
                if (retried != item) {
                    try {
                        return eval(retried);
                    } catch (const std::exception& inner) {
                        const std::string inner_message = inner.what();
                        if (division_by_zero(inner_message)) {
                            error = "❌ Division by zero\n";
                            return std::nullopt;
                        }
                        error = "❌ " + inner_message + "\n";
                        return std::nullopt;
                    }
                }
                return 0.0;
            }
            // Anything else is reported rather than turned into a value: the
            // reference hands the evaluator's own words to the learner, which
            // is how a mistyped function shows up as `❌ Unexpected character`.
            error = "❌ " + message + "\n";
            return std::nullopt;
        }
    };

    std::vector<std::string> items;
    for (const std::string& raw : parts) {
        const std::string item = trim(raw);
        const std::string item_upper = upper(item);

        // A `FUNCTION GRADE$(M)` is a text function too, and its name is not in the
        // builtin list this test knows about.
        const auto user_text_function = [this](const std::string& upper_item) {
            const std::size_t open = upper_item.find('(');
            if (open == std::string::npos) {
                return false;
            }
            const std::string head = upper_item.substr(0, open);
            return !head.empty() && head.back() == '$' &&
                   state_.functions.find(head) != state_.functions.end();
        };
        if (has_string_concat(item) || mentions_string_function(item_upper) ||
            user_text_function(item_upper)) {
            try {
                Strings strings{interpreter_, state_, this};
                items.push_back(strings.evaluate(item));
                continue;
            } catch (const BasicError&) {
                std::string error;
                const std::optional<double> value = numeric(item, error);
                if (!error.empty()) {
                    return error;
                }
                items.push_back(format_numeric(value.value_or(0.0), ""));
                continue;
            }
        }

        if (item.size() >= 2 && item.front() == '"' && item.back() == '"' &&
            item.find('"', 1) == item.size() - 1) {
            items.push_back(item.substr(1, item.size() - 2));
            continue;
        }
        if (item_upper == "INKEY$") {
            items.push_back(key_text());
            continue;
        }
        if (item_upper == "TIME$") {
            items.push_back(clock_time_text());
            continue;
        }
        if (item_upper == "DATE$") {
            items.push_back(clock_date_text());
            continue;
        }
        if (item_upper == "TIMER") {
            items.push_back(std::to_string(static_cast<long long>(clock_timer_seconds())));
            continue;
        }
        if (starts_with(item_upper, "TYPE$(") && item.back() == ')') {
            const std::string name = trim(item.substr(6, item.size() - 7));
            const auto [base, type] = resolve(name);
            (void)base;
            items.push_back(upper(type == "double" ? "DOUBLE" : type));
            continue;
        }
        if (item_upper.back() == '$' ||
            (item_upper.back() == ')' && item_upper.find("$(") != std::string::npos)) {
            const std::size_t open = item.find('(');
            if (open != std::string::npos && item.back() == ')') {
                const std::string array_name = upper(item.substr(0, open));
                const std::string index_expr = item.substr(open + 1, item.size() - open - 2);
                const auto array = state_.string_arrays.find(array_name);
                if (array != state_.string_arrays.end()) {
                    // The reference evaluates the index (`BED$(I)` reads the
                    // loop variable); only a failure to evaluate is an empty
                    // field.
                    long index = 0;
                    bool numeric_index = false;
                    try {
                        index = static_cast<long>(eval(index_expr));
                        numeric_index = true;
                    } catch (const std::exception&) {
                        numeric_index = false;
                    }
                    if (!numeric_index || index < 0 ||
                        static_cast<std::size_t>(index) >= array->second.size()) {
                        items.push_back({});
                    } else {
                        items.push_back(array->second[static_cast<std::size_t>(index)]);
                    }
                } else {
                    items.push_back({});
                }
                continue;
            }
            items.push_back(text_of(item));
            continue;
        }
        if (mentions_string_function(item_upper)) {
            try {
                Strings strings{interpreter_, state_, this};
                items.push_back(strings.evaluate(item));
            } catch (const BasicError&) {
                std::string error;
                const std::optional<double> value = numeric(item, error);
                if (!error.empty()) {
                    return error;
                }
                items.push_back(format_numeric(value.value_or(0.0), ""));
            }
            continue;
        }

        // Typed numeric variable, then a plain one, then an expression.
        static const std::string suffixes{"%&!#"};
        if (!item_upper.empty() && suffixes.find(item_upper.back()) != std::string::npos) {
            const auto [base, type] = resolve(item_upper);
            const std::optional<double> value = number_of(base);
            if (!value.has_value()) {
                items.push_back({});
            } else {
                items.push_back(format_numeric(*value, type));
            }
            continue;
        }
        const auto direct = number_of(item_upper);
        if (direct.has_value()) {
            const auto [base, type] = resolve(item_upper);
            (void)base;
            items.push_back(format_numeric(*direct, type));
            continue;
        }
        if (resolve(item_upper).second == "string") {
            items.push_back(text_of(resolve(item_upper).first + "$"));
            continue;
        }
        std::string error;
        const std::optional<double> value = numeric(item, error);
        if (!error.empty()) {
            return error;
        }
        items.push_back(format_numeric(value.value_or(0.0), ""));
    }
    const std::string result = join(items, " ");
    return suppress_newline ? result : result + "\n";
}

std::string Machine::print_using(const std::string& args) {
    const std::size_t separator = args.find(';');
    if (separator == std::string::npos) {
        return "❌ PRINT USING: missing ';' separator\n";
    }
    const std::string format_part = trim(args.substr(0, separator));
    const std::string values_part = trim(args.substr(separator + 1));

    std::string format;
    if (format_part.size() >= 2 && format_part.front() == '"' && format_part.back() == '"') {
        format = format_part.substr(1, format_part.size() - 2);
    } else {
        const std::string key = upper(format_part);
        const auto found = interpreter_.variables().find(key);
        if (found != interpreter_.variables().end() && found->second.is_text()) {
            format = found->second.text();
        } else {
            format = interpreter_.interpolate_text(format_part);
        }
    }

    // Values are separated by `;` as well as `,`: `PRINT USING "##  ##"; 3; 7` is
    // the documented spelling, and splitting on commas alone made the whole tail one
    // expression, which then evaluated to zero.
    std::vector<std::string> expressions;
    for (const std::string& field : split_top(values_part, ";,")) {
        if (!trim(field).empty()) {
            expressions.push_back(trim(field));
        }
    }

    const auto string_of = [this](const std::string& expr) {
        const std::string trimmed = trim(expr);
        if (trimmed.size() >= 2 && trimmed.front() == '"' && trimmed.back() == '"') {
            return trimmed.substr(1, trimmed.size() - 2);
        }
        const auto found = interpreter_.variables().find(upper(trimmed));
        if (found != interpreter_.variables().end() && found->second.is_text()) {
            return found->second.text();
        }
        return interpreter_.interpolate_text(expr);
    };

    std::string output;
    std::size_t expression_index = 0;
    for (std::size_t index = 0; index < format.size();) {
        const char character = format[index];
        if (expression_index >= expressions.size()) {
            output += format.substr(index);
            break;
        }
        if (std::strchr("#+-$*", character) != nullptr) {
            std::size_t scan = index;
            bool has_decimal = false;
            // A space is a separator, not part of the mask: `## ##` is two fields,
            // and swallowing the space made it one field that ate both values.
            while (scan < format.size() &&
                   std::strchr("#.,+-$*", format[scan]) != nullptr) {
                if (format[scan] == '.') {
                    has_decimal = true;
                }
                ++scan;
            }
            const std::string field = format.substr(index, scan - index);
            double value = 0.0;
            try {
                value = eval(expressions[expression_index]);
            } catch (const std::exception&) {
                value = 0.0;
            }
            ++expression_index;
            std::string formatted;
            if (has_decimal) {
                const std::size_t dot = field.find('.');
                const int places = static_cast<int>(field.size() - dot - 1);
                char buffer[128];
                std::snprintf(buffer, sizeof buffer, "%0.*f", places, value);
                formatted = buffer;
            } else {
                char buffer[128];
                std::snprintf(buffer, sizeof buffer, "%0.0f", value);
                formatted = buffer;
            }
            const auto hashes = std::count(field.begin(), field.end(), '#');
            std::size_t sign = 0;
            while (sign < formatted.size() &&
                   (formatted[sign] == '+' || formatted[sign] == '-')) {
                ++sign;
            }
            const auto unsigned_length = static_cast<long>(formatted.size() - sign);
            if (hashes > unsigned_length) {
                const auto target =
                    static_cast<std::size_t>(hashes + (value < 0.0 ? 1 : 0));
                while (formatted.size() < target) {
                    formatted.insert(formatted.begin(), ' ');
                }
            }
            output += formatted;
            index = scan;
            continue;
        }
        if (character == '!') {
            const std::string value = string_of(expressions[expression_index]);
            ++expression_index;
            output += value.empty() ? " " : value.substr(0, 1);
            ++index;
            continue;
        }
        if (character == '&') {
            output += string_of(expressions[expression_index]);
            ++expression_index;
            ++index;
            continue;
        }
        if (character == '\\' && index + 1 < format.size() && format[index + 1] == '\\') {
            std::size_t end = format.find('\\', index + 1);
            if (end == std::string::npos) {
                end = index + 2;
            }
            const auto width = static_cast<long>(end - index + 1);
            std::string value = string_of(expressions[expression_index]);
            ++expression_index;
            if (static_cast<long>(value.size()) > width) {
                value.resize(static_cast<std::size_t>(width));
            }
            while (static_cast<long>(value.size()) < width) {
                value.push_back(' ');
            }
            output += value;
            index = end + 1;
            continue;
        }
        output.push_back(character);
        ++index;
    }
    return output + "\n";
}

// -- assignment ------------------------------------------------------------

}  // namespace timewarp::lang::basic
