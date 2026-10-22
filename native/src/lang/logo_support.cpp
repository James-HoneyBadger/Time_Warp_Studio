// SPDX-License-Identifier: MIT
//
// The preamble `LogoMachine` is built on: Python-shaped string and number
// formatting, and the word lists the reference keeps as module constants.
#include "logo_internal.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <random>
#include <utility>

namespace timewarp::lang {
namespace logo_detail {

// ---------------------------------------------------------------------------

bool is_space(char character) {
    return character == ' ' || character == '\t' || character == '\n' ||
           character == '\r' || character == '\f' || character == '\v';
}

std::string upper(std::string_view text) {
    std::string result{text};
    for (char& character : result) {
        character = static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
    }
    return result;
}

std::string lstrip(std::string_view text) {
    std::size_t start = 0;
    while (start < text.size() && is_space(text[start])) {
        ++start;
    }
    return std::string{text.substr(start)};
}

std::string rstrip(std::string_view text) {
    std::size_t end = text.size();
    while (end > 0 && is_space(text[end - 1])) {
        --end;
    }
    return std::string{text.substr(0, end)};
}

std::string strip(std::string_view text) { return lstrip(rstrip(text)); }

std::string strip_char(std::string_view text, char character) {
    std::size_t start = 0;
    std::size_t end = text.size();
    while (start < end && text[start] == character) {
        ++start;
    }
    while (end > start && text[end - 1] == character) {
        --end;
    }
    return std::string{text.substr(start, end - start)};
}

std::vector<std::string> split_ws(std::string_view text) {
    std::vector<std::string> parts;
    std::size_t index = 0;
    while (index < text.size()) {
        while (index < text.size() && is_space(text[index])) {
            ++index;
        }
        const std::size_t start = index;
        while (index < text.size() && !is_space(text[index])) {
            ++index;
        }
        if (index > start) {
            parts.emplace_back(text.substr(start, index - start));
        }
    }
    return parts;
}

/// Splits into at most `limit` pieces on whitespace, the way
/// `str.split(None, limit)` does. Empty pieces are dropped, so the first piece
/// is the command word and the rest is its argument text with the user's case.
std::vector<std::string> split_ws_limit(std::string_view text, std::size_t limit) {
    std::vector<std::string> parts;
    std::size_t index = 0;
    while (index < text.size() && is_space(text[index])) {
        ++index;
    }
    while (index < text.size()) {
        if (parts.size() == limit) {
            parts.emplace_back(rstrip(text.substr(index)));
            return parts;
        }
        const std::size_t start = index;
        while (index < text.size() && !is_space(text[index])) {
            ++index;
        }
        parts.emplace_back(text.substr(start, index - start));
        while (index < text.size() && is_space(text[index])) {
            ++index;
        }
    }
    return parts;
}

/// Python's `splitlines()`: CRLF, a lone CR and LF all end a line, and a
/// trailing newline does not invent an empty last line.
std::vector<std::string> split_lines(std::string_view source) {
    std::vector<std::string> lines;
    std::string current;
    for (std::size_t index = 0; index < source.size(); ++index) {
        const char character = source[index];
        if (character == '\r') {
            lines.push_back(current);
            current.clear();
            if (index + 1 < source.size() && source[index + 1] == '\n') {
                ++index;
            }
        } else if (character == '\n') {
            lines.push_back(current);
            current.clear();
        } else {
            current.push_back(character);
        }
    }
    if (!current.empty()) {
        lines.push_back(current);
    } else if (!source.empty() && source.back() != '\n' && source.back() != '\r') {
        lines.push_back(current);
    }
    return lines;
}

bool starts_with_upper(std::string_view text, std::string_view prefix) {
    return text.size() >= prefix.size() && upper(text.substr(0, prefix.size())) == prefix;
}

// ---------------------------------------------------------------------------
// Numbers, rendered the way Python renders them.
//
// Two different formats are in play and both are compared against the
// reference: `str(float)` (shortest round-trip, always with a decimal point)
// and `f"{value:g}"` (six significant digits). A Logo program tells them apart
// constantly, since PRINT shows one and COUNT measures the other's length.
// ---------------------------------------------------------------------------

std::string repr_real(double value) {
    if (std::isnan(value)) {
        return "nan";
    }
    if (std::isinf(value)) {
        return value < 0.0 ? "-inf" : "inf";
    }
    char buffer[64];
    const std::size_t length =
        timewarp::core::format_double(buffer, buffer + sizeof buffer, value);
    std::string text{buffer, length};
    if (text.find('.') == std::string::npos && text.find('e') == std::string::npos &&
        text.find('n') == std::string::npos && text.find('i') == std::string::npos) {
        text += ".0";
    }
    return text;
}

std::string general_text(double value) {
    if (std::isnan(value)) {
        return "nan";
    }
    if (std::isinf(value)) {
        return value < 0.0 ? "-inf" : "inf";
    }
    char buffer[64];
    const std::size_t length =
        timewarp::core::format_double_general(buffer, buffer + sizeof buffer, value, 6);
    return std::string{buffer, length};
}

/// `str(int(value))` -- Python truncates toward zero.
std::string repr_integer(double value) {
    if (std::isnan(value) || std::isinf(value)) {
        return repr_real(value);
    }
    const double truncated = std::trunc(value);
    char buffer[64];
    const auto result = std::to_chars(buffer, buffer + sizeof buffer,
                                      static_cast<long long>(truncated));
    return std::string{buffer, result.ptr};
}

/// `str(value)` for a value the reference may hold as a float or as a word.
std::string repr_value(double value) { return repr_real(value); }

/// `repr()` of a Python list, which is what `str(list)` prints: strings are
/// quoted with single quotes.
std::string repr_list(const std::vector<std::string>& words) {
    std::string result = "[";
    for (std::size_t index = 0; index < words.size(); ++index) {
        if (index > 0) {
            result += ", ";
        }
        result += "'" + words[index] + "'";
    }
    return result + "]";
}

// ---------------------------------------------------------------------------
// The word lists the reference keeps as module constants.
// ---------------------------------------------------------------------------

bool in_set(const std::set<std::string_view>& words, std::string_view word) {
    return words.find(word) != words.end();
}

/// Every word that can begin a command. A token in this set splits a line.
const std::set<std::string_view>& logo_commands() {
    static const std::set<std::string_view> words{
        "FORWARD",      "FD",          "BACK",       "BK",          "BACKWARD",
        "LEFT",         "LT",          "RIGHT",      "RT",          "PENUP",
        "PU",           "PENDOWN",     "PD",         "HOME",        "CLEARSCREEN",
        "CS",           "CLEAR",       "CLEAN",      "HIDETURTLE",  "HT",
        "SHOWTURTLE",   "ST",          "SETXY",      "SETPOSITION", "SETPOS",
        "SETX",         "SETY",        "SETHEADING", "SETH",        "SETPENCOLOR",
        "SETPC",        "SETCOLOR",    "SETBGCOLOR", "SETBG",       "SETPENWIDTH",
        "SETPW",        "PENWIDTH",    "PENSIZE",    "SETPENSIZE",  "REPEAT",
        "IF",           "IFELSE",      "STOP",       "OUTPUT",      "TO",
        "END",          "MAKE",        "PRINT",      "SHOW",        "TYPE",
        "ARC",          "FILLED",      "PENSTYLE",   "BEZIER",      "LABEL",
        "WAIT",
        "BYE",
        "OP",           "FOREVER",     "REM",        "WHILE",       "UNTIL",
        "FOREACH",      "MAP",         "FILTER",     "REDUCE",      "LOCAL",
        "THING",        "STAMP",       "STAMPCLEAN", "TEXTSIZE",    "SETLABELHEIGHT",
        "DOT",          "PENPAINT",    "PPT",        "PENERASE",    "PE",
        "PENREVERSE",   "PX",          "PENMODE",
    };
    return words;
}

/// Words that take no arguments, so a following procedure name cannot be one.
const std::set<std::string_view>& no_argument_commands() {
    static const std::set<std::string_view> words{
        "PENUP", "PU", "PENDOWN", "PD", "HOME", "CLEARSCREEN", "CS",
        "CLEAN", "CLEAR", "HIDETURTLE", "HT", "SHOWTURTLE", "ST", "STOP", "BYE",
    };
    return words;
}

/// Words that *return* a value. A reporter cannot begin a statement, so the
/// splitter must never start a new command at one: `PRINT THING "X` would
/// otherwise become two commands and print a stray line.
const std::set<std::string_view>& logo_reporters() {
    static const std::set<std::string_view> words{
        "SUM",    "DIFFERENCE", "PRODUCT", "QUOTIENT", "RANDOM", "WORD",
        "LIST",   "SENTENCE",   "FIRST",   "LAST",     "BUTFIRST", "BUTLAST",
        "ITEM",   "THING",      "COUNT",   "REPCOUNT", "POS",    "XCOR",
        "YCOR",   "HEADING",    "ROLL",    "REDUCE",
    };
    return words;
}

/// The reporters whose result can stand in for a colour argument.
const std::set<std::string_view>& colour_reporters() {
    static const std::set<std::string_view> words{
        "SUM",  "DIFFERENCE", "PRODUCT", "QUOTIENT", "RANDOM", "WORD",
        "LIST", "SENTENCE",   "FIRST",   "LAST",     "BUTFIRST", "BUTLAST",
        "ITEM", "THING",
    };
    return words;
}

/// The prefix math functions, which Logo writes as `COS x` and the expression
/// evaluator only understands as `COS(x)`.
const std::set<std::string_view>& math_prefix_functions() {
    static const std::set<std::string_view> words{
        "ACOS", "ASIN",  "ATAN", "CEIL", "CINT",  "COS",  "COSH", "EXP",
        "FIX",  "FLOOR", "INT",  "LOG10", "LOG2", "LOG",  "ROUND", "SGN",
        "SIN",  "SINH",  "SQR",  "SQRT", "TAN",   "TANH",
    };
    return words;
}

/// The functions whose call bracket may be written with a space before it,
/// `ABS (x)`, which the expression evaluator only accepts tight.
const std::set<std::string_view>& spaced_call_functions() {
    static const std::set<std::string_view> words{
        "ABS",  "SIN",  "COS",  "TAN",  "ASIN", "ACOS", "ATAN", "SINH",
        "COSH", "TANH", "SQRT", "SQR",  "FLOOR", "CEIL", "EXP", "LOG",
        "LOG10", "LOG2", "INT",  "ROUND", "SGN",  "MIN",  "MAX",  "POW",
        "LEN",
    };
    return words;
}

const std::map<std::string_view, Rgb>& colour_names() {
    static const std::map<std::string_view, Rgb> table{
        {"BLACK", {0, 0, 0}},       {"WHITE", {255, 255, 255}},
        {"RED", {255, 0, 0}},       {"GREEN", {0, 255, 0}},
        {"BLUE", {0, 0, 255}},      {"YELLOW", {255, 255, 0}},
        {"CYAN", {0, 255, 255}},    {"MAGENTA", {255, 0, 255}},
        {"PINK", {255, 192, 203}},  {"ORANGE", {255, 165, 0}},
        {"PURPLE", {128, 0, 128}},  {"BROWN", {139, 69, 19}},
        {"LIME", {0, 255, 0}},      {"NAVY", {0, 0, 128}},
        {"TEAL", {0, 128, 128}},    {"MAROON", {128, 0, 0}},
        {"OLIVE", {128, 128, 0}},   {"SILVER", {192, 192, 192}},
        {"GOLD", {255, 215, 0}},    {"GRAY", {128, 128, 128}},
        {"GREY", {128, 128, 128}},
    };
    return table;
}

std::optional<Rgb> named_colour(std::string_view name) {
    const auto found = colour_names().find(upper(name));
    if (found == colour_names().end()) {
        return std::nullopt;
    }
    return found->second;
}

int clamp_channel(double value) {
    if (!(value > 0.0)) {
        return 0;
    }
    if (value > 255.0) {
        return 255;
    }
    return static_cast<int>(value);
}

/// `interpreter.variables` holds numbers and words in one map here, so a value
/// the reference would call a string is still a `Value`.
bool looks_like_number(std::string_view text, double& value) {
    const std::string trimmed = strip(text);
    if (trimmed.empty()) {
        return false;
    }
    std::size_t index = 0;
    if (trimmed[index] == '+' || trimmed[index] == '-') {
        ++index;
    }
    bool digit = false;
    bool dot = false;
    for (; index < trimmed.size(); ++index) {
        const char character = trimmed[index];
        if (character >= '0' && character <= '9') {
            digit = true;
        } else if (character == '.' && !dot) {
            dot = true;
        } else if ((character == 'e' || character == 'E') && digit) {
            break;
        } else {
            return false;
        }
    }
    if (!digit) {
        return false;
    }
    try {
        std::size_t used = 0;
        value = std::stod(trimmed, &used);
        return used == trimmed.size();
    } catch (const std::exception&) {
        return false;
    }
}

/// The `]` that closes the `[` at *start*, so a bracketed body can be sliced.
std::size_t matching_bracket(std::string_view text, std::size_t start) {
    int depth = 0;
    for (std::size_t index = start; index < text.size(); ++index) {
        if (text[index] == '[') {
            ++depth;
        } else if (text[index] == ']') {
            --depth;
            if (depth == 0) {
                return index;
            }
        }
    }
    return std::string_view::npos;
}
/// Puts the `[line N]` annotation on each `❌` line, as the reference's
/// `_inject_line_number` does. A Logo statement has no number of its own, so
/// `line` is the position the loader gave it.
std::string annotate(std::string_view text, int line) {
    if (text.empty() || text.find("❌") == std::string_view::npos) {
        return std::string{text};
    }
    const std::string display = std::to_string(line);
    std::string result;
    std::size_t start = 0;
    while (start <= text.size()) {
        const std::size_t end = text.find('\n', start);
        const std::string_view current =
            end == std::string_view::npos ? text.substr(start) : text.substr(start, end - start);
        if (!result.empty()) {
            result.push_back('\n');
        }
        if (current.rfind("❌", 0) == 0 && current.find("[line ") == std::string_view::npos &&
            current.find("at line ") == std::string_view::npos) {
            result += "❌ [line " + display + "] " +
                      lstrip(current.substr(std::string_view{"❌"}.size()));
        } else {
            result += current;
        }
        if (end == std::string_view::npos) {
            break;
        }
        start = end + 1;
    }
    return result;
}

}  // namespace logo_detail
}  // namespace timewarp::lang
