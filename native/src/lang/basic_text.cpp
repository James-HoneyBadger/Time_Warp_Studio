// SPDX-License-Identifier: MIT
//
// BASIC's text handling: the helpers the statement dispatch shares, and the
// string expression evaluator (`utils/string_evaluator.py`) it calls.
//
// The evaluator is ported as it behaves rather than as it was intended: an
// expression it cannot read is returned as its own source text, because that is
// what a learner sees today and the port is checked against that.

#include "basic_internal.hpp"

#include "timewarp/core/charconv.hpp"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <string>

namespace timewarp::lang::basic {
namespace {

// -- UTF-8 ----------------------------------------------------------------
//
// Python counts and slices strings by code point, so `LEN("é")` is 1 and
// `LEFT$("héllo", 2)` is "hé". Byte-wise helpers would answer 2 and "hÃ".

std::size_t rune_length(unsigned char lead) {
    if (lead < 0x80) {
        return 1;
    }
    if ((lead & 0xE0) == 0xC0) {
        return 2;
    }
    if ((lead & 0xF0) == 0xE0) {
        return 3;
    }
    if ((lead & 0xF8) == 0xF0) {
        return 4;
    }
    return 1;
}

std::size_t rune_count(std::string_view text) {
    std::size_t count = 0;
    for (std::size_t index = 0; index < text.size();) {
        index += rune_length(static_cast<unsigned char>(text[index]));
        ++count;
    }
    return count;
}

/// Byte offset of code point *index*, clamped to the end.
std::size_t rune_offset(std::string_view text, std::size_t index) {
    std::size_t offset = 0;
    for (std::size_t seen = 0; seen < index && offset < text.size(); ++seen) {
        offset += rune_length(static_cast<unsigned char>(text[offset]));
    }
    return offset;
}

std::string rune_slice(std::string_view text, std::size_t start, std::size_t length) {
    const std::size_t from = rune_offset(text, start);
    const std::size_t to = rune_offset(text, start + length);
    return std::string{text.substr(std::min(from, text.size()),
                                   to > from ? to - from : 0)};
}

void append_rune(std::string& text, long code_point) {
    if (code_point < 0) {
        return;
    }
    const auto point = static_cast<unsigned long>(code_point);
    if (point < 0x80) {
        text.push_back(static_cast<char>(point));
    } else if (point < 0x800) {
        text.push_back(static_cast<char>(0xC0 | (point >> 6)));
        text.push_back(static_cast<char>(0x80 | (point & 0x3F)));
    } else if (point < 0x10000) {
        text.push_back(static_cast<char>(0xE0 | (point >> 12)));
        text.push_back(static_cast<char>(0x80 | ((point >> 6) & 0x3F)));
        text.push_back(static_cast<char>(0x80 | (point & 0x3F)));
    } else {
        text.push_back(static_cast<char>(0xF0 | (point >> 18)));
        text.push_back(static_cast<char>(0x80 | ((point >> 12) & 0x3F)));
        text.push_back(static_cast<char>(0x80 | ((point >> 6) & 0x3F)));
        text.push_back(static_cast<char>(0x80 | (point & 0x3F)));
    }
}

long first_rune(std::string_view text) {
    if (text.empty()) {
        return 0;
    }
    const auto lead = static_cast<unsigned char>(text[0]);
    const std::size_t length = rune_length(lead);
    if (length == 1 || text.size() < length) {
        return lead;
    }
    long value = lead & (0xFF >> (length + 1));
    for (std::size_t index = 1; index < length; ++index) {
        value = (value << 6) | (static_cast<unsigned char>(text[index]) & 0x3F);
    }
    return value;
}

bool identifier_ish(std::string_view text) {
    if (text.empty()) {
        return false;
    }
    for (const char character : text) {
        if (!std::isalnum(static_cast<unsigned char>(character))) {
            return false;
        }
    }
    return true;
}

bool all_alpha(std::string_view text) {
    if (text.empty()) {
        return false;
    }
    for (const char character : text) {
        if (!std::isalpha(static_cast<unsigned char>(character))) {
            return false;
        }
    }
    return true;
}

/// The leading `NAME` or `NAME$` of a `NAME(` form, or "" when it is not one.
std::string function_head(std::string_view expression) {
    std::size_t index = 0;
    while (index < expression.size() &&
           std::isalpha(static_cast<unsigned char>(expression[index]))) {
        ++index;
    }
    if (index == 0) {
        return {};
    }
    if (index < expression.size() && expression[index] == '$') {
        ++index;
    }
    std::size_t look = index;
    while (look < expression.size() && is_space(expression[look])) {
        ++look;
    }
    if (look >= expression.size() || expression[look] != '(') {
        return {};
    }
    return std::string{expression.substr(0, index)};
}

/// Index of the `)` matching the `(` at *open*, or `npos`.
std::size_t matching_paren(std::string_view text, std::size_t open) {
    int depth = 0;
    char quote = '\0';
    for (std::size_t index = open; index < text.size(); ++index) {
        const char character = text[index];
        if (quote != '\0') {
            if (character == quote) {
                quote = '\0';
            }
            continue;
        }
        if (character == '"' || character == '\'') {
            quote = character;
        } else if (character == '(') {
            ++depth;
        } else if (character == ')') {
            --depth;
            if (depth == 0) {
                return index;
            }
        }
    }
    return std::string::npos;
}

std::string repeat(std::string_view text, long count) {
    std::string result;
    for (long index = 0; index < count; ++index) {
        result.append(text);
    }
    return result;
}

/// A `%`-style format applied to one value, as `FORMAT$` needs.
std::string percent_format(std::string_view format, const std::vector<std::string>& numbers,
                           const std::vector<std::string>& texts, bool& ok) {
    std::string result;
    std::size_t argument = 0;
    for (std::size_t index = 0; index < format.size(); ++index) {
        if (format[index] != '%') {
            result.push_back(format[index]);
            continue;
        }
        ++index;
        if (index < format.size() && format[index] == '%') {
            result.push_back('%');
            continue;
        }
        std::string spec{"%"};
        bool left = false;
        bool zero = false;
        bool plus = false;
        while (index < format.size() &&
               (format[index] == '-' || format[index] == '0' || format[index] == '+' ||
                format[index] == ' ')) {
            left = left || format[index] == '-';
            zero = zero || format[index] == '0';
            plus = plus || format[index] == '+';
            ++index;
        }
        std::string width;
        while (index < format.size() && std::isdigit(static_cast<unsigned char>(format[index]))) {
            width.push_back(format[index]);
            ++index;
        }
        std::string precision;
        bool has_precision = false;
        if (index < format.size() && format[index] == '.') {
            has_precision = true;
            ++index;
            while (index < format.size() &&
                   std::isdigit(static_cast<unsigned char>(format[index]))) {
                precision.push_back(format[index]);
                ++index;
            }
        }
        if (index >= format.size()) {
            ok = false;
            return format.empty() ? result : std::string{format};
        }
        const char kind = format[index];
        std::string rendered;
        if (kind == 'd' || kind == 'i' || kind == 'f' || kind == 'g' || kind == 'e' ||
            kind == 'x' || kind == 'X' || kind == 'o') {
            if (argument >= numbers.size()) {
                ok = false;
                return std::string{format};
            }
            char buffer[128];
            std::string spec_text{"%"};
            if (left) {
                spec_text += "-";
            }
            if (zero) {
                spec_text += "0";
            }
            if (plus) {
                spec_text += "+";
            }
            spec_text += width;
            if (has_precision) {
                spec_text += ".";
                spec_text += precision;
            }
            spec_text.push_back(kind == 'i' ? 'd' : kind);
            const double value = std::strtod(numbers[argument].c_str(), nullptr);
            if (kind == 'd' || kind == 'i' || kind == 'x' || kind == 'X' || kind == 'o') {
                const long long integer = static_cast<long long>(value);
                std::snprintf(buffer, sizeof buffer, spec_text.c_str(), integer);
            } else {
                std::snprintf(buffer, sizeof buffer, spec_text.c_str(), value);
            }
            rendered = buffer;
            ++argument;
        } else if (kind == 's') {
            if (argument >= texts.size()) {
                ok = false;
                return std::string{format};
            }
            rendered = texts[argument];
            ++argument;
            if (has_precision) {
                const auto limit = std::stoul(precision);
                if (rendered.size() > limit) {
                    rendered.resize(limit);
                }
            }
            if (!width.empty()) {
                const auto target = std::stoul(width);
                while (rendered.size() < target) {
                    if (left) {
                        rendered.push_back(' ');
                    } else {
                        rendered.insert(rendered.begin(), ' ');
                    }
                }
            }
        } else {
            ok = false;
            return std::string{format};
        }
        if (kind != 's' && !width.empty()) {
            const auto target = std::stoul(width);
            while (rendered.size() < target) {
                if (left) {
                    rendered.push_back(' ');
                } else if (zero) {
                    rendered.insert(rendered.begin() + (rendered.size() > 0 &&
                                                                 (rendered[0] == '-' ||
                                                                  rendered[0] == '+' ||
                                                                  rendered[0] == ' ')
                                                             ? 1
                                                             : 0),
                                    '0');
                } else {
                    rendered.insert(rendered.begin(), ' ');
                }
            }
        }
        result += rendered;
    }
    // Python's `%` raises "not all arguments converted" when a value is left
    // over, which is how `FORMAT$("#.##", 3.14)` falls through to the mask
    // conversion instead of printing the mask itself.
    ok = argument == numbers.size();
    return result;
}

/// The reference's `_convert_mask`: `###.##` becomes `%6.2f`, `###` becomes
/// `%3d`.
///
/// `FORMAT$` accepts either a `%` conversion or a `#` mask, and the mask is
/// only tried after the `%` form fails. The width counts `#` characters; an
/// integer mask's width deliberately counts a `+` too, because the reference
/// only strips `-` when it measures one.
std::string convert_format_masks(std::string_view format) {
    std::string result;
    std::size_t index = 0;
    while (index < format.size()) {
        if (format[index] != '#' && format[index] != '-' && format[index] != '+') {
            result.push_back(format[index]);
            ++index;
            continue;
        }
        const std::size_t start = index;
        while (index < format.size() &&
               (format[index] == '#' || format[index] == '-' || format[index] == '+')) {
            ++index;
        }
        const std::string mask{format.substr(start, index - start)};
        const std::size_t dot = mask.find('.');
        const std::string integer = dot == std::string::npos ? mask : mask.substr(0, dot);
        std::size_t precision = 0;
        bool has_precision = false;
        if (index < format.size() && format[index] == '.') {
            has_precision = true;
            ++index;
            while (index < format.size() && format[index] == '#') {
                ++index;
                ++precision;
            }
        }
        std::size_t width = 0;
        for (const char character : integer) {
            if (character == '#') {
                ++width;
            } else if (character == '+' && !has_precision) {
                // The reference strips only '-' when it measures an integer
                // mask, so a '+' is counted as width.
                ++width;
            }
        }
        result += mask.front() == '-' ? "%-" : "%";
        result += std::to_string(width);
        if (has_precision) {
            result += "." + std::to_string(precision) + "f";
        } else {
            result.push_back('d');
        }
    }
    return result;
}

}  // namespace

// -- text -----------------------------------------------------------------

bool is_space(char character) {
    return std::isspace(static_cast<unsigned char>(character)) != 0;
}

std::string trim(std::string_view text) {
    std::size_t start = 0;
    std::size_t end = text.size();
    while (start < end && is_space(text[start])) {
        ++start;
    }
    while (end > start && is_space(text[end - 1])) {
        --end;
    }
    return std::string{text.substr(start, end - start)};
}

std::string rstrip(std::string_view text) {
    std::size_t end = text.size();
    while (end > 0 && is_space(text[end - 1])) {
        --end;
    }
    return std::string{text.substr(0, end)};
}

std::string lstrip(std::string_view text) {
    std::size_t start = 0;
    while (start < text.size() && is_space(text[start])) {
        ++start;
    }
    return std::string{text.substr(start)};
}

std::string upper(std::string_view text) {
    std::string result{text};
    for (char& character : result) {
        character = static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
    }
    return result;
}

std::string lower(std::string_view text) {
    std::string result{text};
    for (char& character : result) {
        character = static_cast<char>(std::tolower(static_cast<unsigned char>(character)));
    }
    return result;
}

bool is_keyword(std::string_view command, std::string_view keyword) {
    if (command == keyword) {
        return true;
    }
    return command.size() > keyword.size() && command.rfind(keyword, 0) == 0 &&
           command[keyword.size()] == ' ';
}

std::string strip_comment(std::string_view args) {
    bool in_quotes = false;
    for (std::size_t index = 0; index < args.size(); ++index) {
        const char character = args[index];
        if (character == '"') {
            in_quotes = !in_quotes;
            continue;
        }
        if (in_quotes) {
            continue;
        }
        const std::string rest{args.substr(index)};
        const std::string head = upper(std::string_view{rest}.substr(0, 6));
        if (head.rfind(": REM", 0) == 0) {
            return rstrip(args.substr(0, index));
        }
        if (head.rfind(":REM", 0) == 0) {
            return rstrip(args.substr(0, index));
        }
    }
    return std::string{args};
}

std::vector<std::string> split_statements(std::string_view command) {
    std::vector<std::string> statements;
    std::string current;
    bool in_quotes = false;
    for (const char character : command) {
        if (character == '"') {
            in_quotes = !in_quotes;
            current.push_back(character);
        } else if (character == ':' && !in_quotes) {
            statements.push_back(trim(current));
            current.clear();
        } else {
            current.push_back(character);
        }
    }
    if (!trim(current).empty()) {
        statements.push_back(trim(current));
    }
    return statements;
}

std::vector<std::string> split_top(std::string_view text, std::string_view separators) {
    std::vector<std::string> parts;
    std::string current;
    bool in_quotes = false;
    int depth = 0;
    for (const char character : text) {
        if (character == '"') {
            in_quotes = !in_quotes;
            current.push_back(character);
            continue;
        }
        if (in_quotes) {
            current.push_back(character);
            continue;
        }
        if (character == '(') {
            ++depth;
        } else if (character == ')') {
            --depth;
        }
        if (depth == 0 && separators.find(character) != std::string_view::npos) {
            if (!trim(current).empty()) {
                parts.push_back(trim(current));
            }
            current.clear();
            continue;
        }
        current.push_back(character);
    }
    if (!trim(current).empty()) {
        parts.push_back(trim(current));
    }
    return parts;
}

bool parse_float(std::string_view text, double& value) {
    const std::string trimmed = trim(text);
    if (trimmed.empty()) {
        return false;
    }
    const char* begin = trimmed.c_str();
    char* end = nullptr;
    errno = 0;
    const double parsed = std::strtod(begin, &end);
    if (end == begin || *end != '\0') {
        return false;
    }
    value = parsed;
    return true;
}

std::string format_general(double value) {
    if (std::isnan(value)) {
        return "nan";
    }
    if (std::isinf(value)) {
        return value < 0 ? "-inf" : "inf";
    }
    char buffer[64];
    const std::size_t length =
        timewarp::core::format_double_general(buffer, buffer + sizeof buffer, value, 6);
    if (length == 0) {
        return "0";
    }
    return std::string{buffer, length};
}

std::string format_numeric(double value, std::string_view preferred) {
    if (preferred == "int" || preferred == "long") {
        if (std::isfinite(value) && std::fabs(value) < 9.0e18) {
            return std::to_string(static_cast<long long>(value));
        }
        return format_general(std::trunc(value));
    }
    return format_general(value);
}

std::string printed_number(double value) {
    // `str(3.0)` is "3.0" in Python and "3" in every C++ formatter.
    if (std::isfinite(value) && value == std::trunc(value) && std::fabs(value) < 9.0e18) {
        return std::to_string(static_cast<long long>(value)) + ".0";
    }
    char buffer[64];
    const std::size_t length =
        timewarp::core::format_double(buffer, buffer + sizeof buffer, value);
    return std::string{buffer, length};
}

// -- the string evaluator --------------------------------------------------

Strings::Strings(Interpreter& interpreter, const State& state)
    : interpreter_{interpreter}, state_{state} {}

bool Strings::looks_numeric(std::string_view text) {
    double value = 0.0;
    return parse_float(text, value);
}

std::string Strings::as_text(const Argument& argument) {
    if (const auto* text = std::get_if<std::string>(&argument)) {
        return *text;
    }
    const double value = std::get<double>(argument);
    if (std::isfinite(value) && value == std::trunc(value) && std::fabs(value) < 9.0e18) {
        return std::to_string(static_cast<long long>(value));
    }
    return printed_number(value);
}

std::string Strings::string_variable(std::string_view name) const {
    const std::string key{name};
    const auto found = interpreter_.variables().find(key);
    if (found != interpreter_.variables().end() && found->second.is_text()) {
        return found->second.text();
    }
    return {};
}

std::string Strings::array_element(std::string_view name, std::string_view index) const {
    const std::string array_name = upper(name);
    if (array_name.empty() || array_name.back() != '$') {
        return {};
    }
    const auto array = state_.string_arrays.find(array_name);
    if (array == state_.string_arrays.end()) {
        return {};
    }
    const std::string key = trim(index);
    double position = 0.0;
    const auto variable = interpreter_.variables().find(upper(key));
    if (variable != interpreter_.variables().end() && variable->second.is_number()) {
        position = variable->second.number();
    } else if (!parse_float(key, position)) {
        return {};
    }
    const auto slot = static_cast<long>(position);
    if (slot < 0 || static_cast<std::size_t>(slot) >= array->second.size()) {
        return {};
    }
    return array->second[static_cast<std::size_t>(slot)];
}

std::string Strings::evaluate(std::string_view expression) {
    return evaluate_expr(trim(expression));
}

std::string Strings::evaluate_expr(std::string_view input) {
    const std::string expr = trim(input);
    if (expr.size() >= 2 && ((expr.front() == '"' && expr.back() == '"') ||
                             (expr.front() == '\'' && expr.back() == '\''))) {
        const std::string inner = expr.substr(1, expr.size() - 2);
        if (inner.find(expr.front()) == std::string::npos) {
            return inner;
        }
    }
    if (!expr.empty() && expr.back() == '$') {
        std::string bare = expr.substr(0, expr.size() - 1);
        bare.erase(std::remove(bare.begin(), bare.end(), '_'), bare.end());
        if (identifier_ish(bare)) {
            return string_variable(expr);
        }
    }
    const std::size_t marker = expr.find("$(");
    if (marker != std::string::npos && expr.back() == ')') {
        const std::size_t open = expr.find('(', marker);
        const std::string name = upper(expr.substr(0, open));
        // `MID$("HELLO", 1, 3)` contains `$(` too. The reference only reads an
        // element when the name is a string array the program DIMensioned, and
        // otherwise falls through to the function call; without that test every
        // string function with a `$` in its name returned "".
        if (!name.empty() && name.back() == '$' &&
            state_.string_arrays.find(name) != state_.string_arrays.end()) {
            return array_element(name, expr.substr(open + 1, expr.size() - open - 2));
        }
    }
    if (!function_head(expr).empty()) {
        return evaluate_function(expr);
    }
    if (expr.find('&') != std::string::npos) {
        return concatenate(expr, '&');
    }
    if (expr.find('+') != std::string::npos && !looks_numeric(expr)) {
        return concatenate(expr, '+');
    }
    if (all_alpha(expr) &&
        std::all_of(expr.begin(), expr.end(), [](char character) {
            return std::isalnum(static_cast<unsigned char>(character)) != 0;
        })) {
        return string_variable(upper(expr));
    }
    return expr;
}

std::vector<std::string> Strings::parse_arguments(std::string_view text) const {
    std::vector<std::string> arguments;
    std::string current;
    int depth = 0;
    char quote = '\0';
    for (const char character : text) {
        if (quote != '\0') {
            current.push_back(character);
            if (character == quote) {
                quote = '\0';
            }
            continue;
        }
        if (character == '"' || character == '\'') {
            quote = character;
            current.push_back(character);
        } else if (character == '(') {
            ++depth;
            current.push_back(character);
        } else if (character == ')') {
            --depth;
            current.push_back(character);
        } else if (character == ',' && depth == 0) {
            arguments.push_back(trim(current));
            current.clear();
        } else {
            current.push_back(character);
        }
    }
    if (!current.empty()) {
        arguments.push_back(trim(current));
    }
    return arguments;
}

Strings::Argument Strings::evaluate_arg(std::string_view input) {
    const std::string argument = trim(input);
    if (!function_head(argument).empty()) {
        return evaluate_function(argument);
    }
    double number = 0.0;
    if (parse_float(argument, number)) {
        return number;
    }
    const auto variable = interpreter_.variables().find(upper(argument));
    if (variable != interpreter_.variables().end() && variable->second.is_number()) {
        return variable->second.number();
    }
    return evaluate_expr(argument);
}
std::string Strings::concatenate(std::string_view expression, char op) {
    std::vector<std::string> parts;
    std::string current;
    int depth = 0;
    char quote = '\0';
    for (std::size_t index = 0; index < expression.size(); ++index) {
        const char character = expression[index];
        if (quote != '\0') {
            current.push_back(character);
            if (character == quote) {
                quote = '\0';
            }
            continue;
        }
        if (character == '"' || character == '\'') {
            quote = character;
            current.push_back(character);
        } else if (character == '(') {
            ++depth;
            current.push_back(character);
        } else if (character == ')') {
            --depth;
            current.push_back(character);
        } else if (depth == 0 && character == op) {
            parts.push_back(current);
            current.clear();
        } else {
            current.push_back(character);
        }
    }
    if (!current.empty()) {
        parts.push_back(current);
    }
    std::string result;
    for (const std::string& part : parts) {
        result += evaluate_expr(trim(part));
    }
    return result;
}

std::string Strings::evaluate_function(std::string_view input) {
    const std::string expr = trim(input);
    const std::size_t open = expr.find('(');
    if (open == std::string::npos) {
        return expr;
    }
    std::string name = upper(trim(expr.substr(0, open)));
    if (!name.empty() && name.back() == '$') {
        name.pop_back();
    }
    const std::size_t close = matching_paren(expr, open);
    if (close == std::string::npos) {
        throw BasicError("Mismatched parentheses in: " + expr);
    }
    const std::vector<std::string> arguments =
        parse_arguments(expr.substr(open + 1, close - open - 1));

    const auto text_at = [&](std::size_t index) {
        if (index >= arguments.size()) {
            throw BasicError(name + " requires more arguments");
        }
        return as_text(evaluate_arg(arguments[index]));
    };
    const auto count_at = [&](std::size_t index, long fallback) {
        if (index >= arguments.size()) {
            return fallback;
        }
        const Argument value = evaluate_arg(arguments[index]);
        if (const auto* number = std::get_if<double>(&value)) {
            return static_cast<long>(*number);
        }
        double parsed = 0.0;
        if (parse_float(std::get<std::string>(value), parsed)) {
            return static_cast<long>(parsed);
        }
        return fallback;
    };

    if (name == "LEN") {
        if (arguments.empty()) {
            throw BasicError("LEN requires 1 argument");
        }
        return std::to_string(rune_count(text_at(0)));
    }
    if (name == "LEFT") {
        if (arguments.size() < 2) {
            throw BasicError("LEFT requires 2 arguments");
        }
        const std::string source = text_at(0);
        const long length = std::max(0L, count_at(1, 0));
        return rune_slice(source, 0, static_cast<std::size_t>(length));
    }
    if (name == "RIGHT") {
        if (arguments.size() < 2) {
            throw BasicError("RIGHT requires 2 arguments");
        }
        const std::string source = text_at(0);
        const long length = count_at(1, 0);
        if (length <= 0) {
            return {};
        }
        const auto total = static_cast<long>(rune_count(source));
        const long start = std::max(0L, total - length);
        return rune_slice(source, static_cast<std::size_t>(start),
                          static_cast<std::size_t>(total - start));
    }
    if (name == "MID") {
        if (arguments.size() < 2) {
            throw BasicError("MID requires at least 2 arguments");
        }
        const std::string source = text_at(0);
        long start = count_at(1, 1);
        const auto total = static_cast<long>(rune_count(source));
        long length = arguments.size() >= 3 ? count_at(2, total) : total;
        start = std::max(1L, start);
        length = std::max(0L, length);
        return rune_slice(source, static_cast<std::size_t>(start - 1),
                          static_cast<std::size_t>(length));
    }
    if (name == "INSTR") {
        if (arguments.size() < 2) {
            throw BasicError("INSTR requires 2 arguments");
        }
        const std::string haystack = text_at(0);
        const std::string needle = text_at(1);
        if (needle.empty()) {
            return "1";
        }
        const std::size_t found = haystack.find(needle);
        return found == std::string::npos ? "0" : std::to_string(found + 1);
    }
    if (name == "UPPER" || name == "UCASE" || name == "UCASES") {
        if (arguments.empty()) {
            throw BasicError("UPPER requires 1 argument");
        }
        return upper(text_at(0));
    }
    if (name == "LOWER" || name == "LCASE" || name == "LCASES") {
        if (arguments.empty()) {
            throw BasicError("LOWER requires 1 argument");
        }
        return lower(text_at(0));
    }
    if (name == "TRIM") {
        if (arguments.empty()) {
            throw BasicError("TRIM requires 1 argument");
        }
        return trim(text_at(0));
    }
    if (name == "LTRIM") {
        if (arguments.empty()) {
            throw BasicError("LTRIM requires 1 argument");
        }
        return lstrip(text_at(0));
    }
    if (name == "RTRIM") {
        if (arguments.empty()) {
            throw BasicError("RTRIM requires 1 argument");
        }
        return rstrip(text_at(0));
    }
    if (name == "STR") {
        if (arguments.empty()) {
            throw BasicError("STR requires 1 argument");
        }
        const Argument value = evaluate_arg(arguments[0]);
        if (const auto* number = std::get_if<double>(&value)) {
            if (*number == std::trunc(*number) && std::fabs(*number) < 9.0e18) {
                return std::to_string(static_cast<long long>(*number));
            }
            return printed_number(*number);
        }
        double parsed = 0.0;
        if (parse_float(std::get<std::string>(value), parsed)) {
            if (parsed == std::trunc(parsed) && std::fabs(parsed) < 9.0e18) {
                return std::to_string(static_cast<long long>(parsed));
            }
            return printed_number(parsed);
        }
        try {
            const double computed = interpreter_.evaluate_expression(arguments[0]);
            if (computed == std::trunc(computed) && std::fabs(computed) < 9.0e18) {
                return std::to_string(static_cast<long long>(computed));
            }
            return printed_number(computed);
        } catch (const std::exception&) {
            return std::get<std::string>(value);
        }
    }
    if (name == "VAL") {
        if (arguments.empty()) {
            throw BasicError("VAL requires 1 argument");
        }
        const Argument value = evaluate_arg(arguments[0]);
        if (const auto* number = std::get_if<double>(&value)) {
            return printed_number(*number);
        }
        const std::string source = trim(std::get<std::string>(value));
        std::size_t index = 0;
        if (index < source.size() && (source[index] == '+' || source[index] == '-')) {
            ++index;
        }
        std::size_t digits = 0;
        while (index < source.size() &&
               std::isdigit(static_cast<unsigned char>(source[index]))) {
            ++index;
            ++digits;
        }
        if (index < source.size() && source[index] == '.') {
            ++index;
            while (index < source.size() &&
                   std::isdigit(static_cast<unsigned char>(source[index]))) {
                ++index;
                ++digits;
            }
        }
        if (digits == 0) {
            return "0";
        }
        const double parsed = std::strtod(source.substr(0, index).c_str(), nullptr);
        if (parsed == std::trunc(parsed) && std::fabs(parsed) < 9.0e18) {
            return std::to_string(static_cast<long long>(parsed));
        }
        return printed_number(parsed);
    }
    if (name == "CHR") {
        if (arguments.empty()) {
            throw BasicError("CHR requires 1 argument");
        }
        std::string result;
        append_rune(result, count_at(0, 0));
        return result;
    }
    if (name == "ASC") {
        if (arguments.empty()) {
            throw BasicError("ASC requires 1 argument");
        }
        const std::string source = text_at(0);
        return source.empty() ? "0" : std::to_string(first_rune(source));
    }
    if (name == "TAB" || name == "SPC" || name == "SPACE") {
        if (arguments.empty()) {
            return {};
        }
        return repeat(" ", std::max(0L, count_at(0, 0)));
    }
    if (name == "STRING") {
        if (arguments.size() < 2) {
            throw BasicError("STRING requires 2 arguments");
        }
        const long count = std::max(0L, count_at(0, 0));
        const Argument source = evaluate_arg(arguments[1]);
        std::string unit = as_text(source);
        if (const auto* number = std::get_if<double>(&source)) {
            unit.clear();
            append_rune(unit, static_cast<long>(*number));
        }
        if (unit.empty()) {
            return {};
        }
        return repeat(rune_slice(unit, 0, 1), count);
    }
    if (name == "LPAD" || name == "RPAD") {
        if (arguments.size() < 2) {
            throw BasicError(name + " requires at least 2 arguments");
        }
        const std::string source = as_text(evaluate_arg(arguments[0]));
        const long width = count_at(1, 0);
        std::string pad{" "};
        if (arguments.size() >= 3) {
            const std::string supplied = as_text(evaluate_arg(arguments[2]));
            if (!supplied.empty()) {
                pad = rune_slice(supplied, 0, 1);
            }
        }
        const auto total = static_cast<long>(rune_count(source));
        const long target = std::max(total, width);
        const long needed = target - total;
        if (needed <= 0) {
            return source;
        }
        if (name == "LPAD") {
            return repeat(pad, needed) + source;
        }
        return source + repeat(pad, needed);
    }
    if (name == "FORMAT") {
        if (arguments.empty()) {
            return {};
        }
        const std::string format = as_text(evaluate_arg(arguments[0]));
        std::vector<std::string> numbers;
        std::vector<std::string> texts;
        for (std::size_t index = 1; index < arguments.size(); ++index) {
            const Argument value = evaluate_arg(arguments[index]);
            if (const auto* number = std::get_if<double>(&value)) {
                char buffer[64];
                const std::size_t written =
                    timewarp::core::format_double(buffer, buffer + sizeof buffer, *number);
                numbers.emplace_back(buffer, written);
                texts.emplace_back(numbers.back());
            } else {
                numbers.push_back(trim(std::get<std::string>(value)));
                texts.push_back(std::get<std::string>(value));
            }
        }
        bool ok = false;
        std::string formatted = percent_format(format, numbers, texts, ok);
        if (!ok) {
            // The reference tries the `#` mask only after the `%` form failed.
            const std::string converted = convert_format_masks(format);
            if (converted != format) {
                formatted = percent_format(converted, numbers, texts, ok);
            }
        }
        if (ok) {
            return formatted;
        }
        return numbers.empty() ? format : numbers.front();
    }
    if (name == "ENVIRON") {
        if (arguments.empty()) {
            return {};
        }
        const std::string variable = as_text(evaluate_arg(arguments[0]));
        const char* value = std::getenv(variable.c_str());
        return value == nullptr ? std::string{} : std::string{value};
    }
    if (name == "REPEAT") {
        if (arguments.size() < 2) {
            throw BasicError("REPEAT requires 2 arguments");
        }
        const std::string unit = as_text(evaluate_arg(arguments[0]));
        return repeat(unit, std::max(0L, count_at(1, 0)));
    }
    throw BasicError("Unknown string function: " + name);
}

}  // namespace timewarp::lang::basic
