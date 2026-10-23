// SPDX-License-Identifier: MIT
//
// The value helpers, the execution-state accessors, the line helpers and the
// built-in functions: everything the machine and the parser both call.

#include "rexx_internal.hpp"

#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <map>
#include <optional>
#include <regex>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace rexx_detail {

std::string upper(std::string_view text) {
    std::string result{text};
    for (char& character : result) {
        character =
            static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
    }
    return result;
}

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

bool starts_with(std::string_view text, std::string_view prefix) {
    return text.size() >= prefix.size() && text.substr(0, prefix.size()) == prefix;
}

/// The reference's `repr(float)` is the shortest round-trip form.
std::string float_repr(double value) {
    if (std::isfinite(value) && std::trunc(value) == value &&
        std::fabs(value) < 9.0e15) {
        char buffer[32];
        std::snprintf(buffer, sizeof buffer, "%.0f", value);
        return buffer;
    }
    char buffer[64];
    const std::size_t length =
        timewarp::core::format_double(buffer, buffer + sizeof buffer, value);
    if (length != 0) {
        return std::string{buffer, length};
    }
    std::snprintf(buffer, sizeof buffer, "%g", value);
    return buffer;
}

std::string fmt(const RValue& value) {
    return value.text ? value.words : float_repr(value.number);
}

double num(const RValue& value) {
    if (!value.text) {
        return value.number;
    }
    const std::string text = trim(value.words);
    if (text.empty()) {
        return 0.0;
    }
    char* end = nullptr;
    const double parsed = std::strtod(text.c_str(), &end);
    return end == text.c_str() + text.size() ? parsed : 0.0;
}

bool is_numeric(const std::string& text) {
    const std::string trimmed = trim(text);
    if (trimmed.empty()) {
        return false;
    }
    char* end = nullptr;
    std::strtod(trimmed.c_str(), &end);
    return end == trimmed.c_str() + trimmed.size();
}

bool truth(const RValue& value) {
    const std::string text = trim(fmt(value));
    if (text.empty()) {
        return false;
    }
    if (is_numeric(text)) {
        return num(RValue::word(text)) != 0.0;
    }
    return true;
}

std::vector<std::string> split_ws(const std::string& text) {
    std::vector<std::string> words;
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
            words.push_back(text.substr(start, index - start));
        }
    }
    return words;
}


bool is_loop(const std::string& kind) {
    return kind == "while" || kind == "until" || kind == "forever" || kind == "count" ||
           kind == "to" || kind == "over";
}

RValue get_var(Interpreter& interpreter, const std::string& name) {
    RexxState& state = interpreter.executor_state<RexxState>();
    const std::string key = upper(name);
    const auto found = state.vars.find(key);
    if (found != state.vars.end()) {
        return found->second;
    }
    // An uninitialised REXX variable evaluates to its own (upper-cased) name.
    return RValue::word(key);
}

void set_var(Interpreter& interpreter, const std::string& name, const RValue& value) {
    RexxState& state = interpreter.executor_state<RexxState>();
    const std::string key = upper(name);
    state.vars[key] = value;
    if (!value.text) {
        interpreter.set_number(key, value.number);
    }
}


// Line helpers
// ---------------------------------------------------------------------------

std::string strip_comment(const std::string& line) {
    std::string result;
    bool in_string = false;
    char quote = 0;
    std::size_t index = 0;
    while (index < line.size()) {
        if (in_string) {
            result.push_back(line[index]);
            if (line[index] == quote) {
                in_string = false;
            }
            ++index;
            continue;
        }
        if (line[index] == '\'' || line[index] == '"') {
            in_string = true;
            quote = line[index];
            result.push_back(line[index]);
            ++index;
            continue;
        }
        if (line[index] == '/' && index + 1 < line.size() && line[index + 1] == '*') {
            const std::size_t close = line.find("*/", index + 2);
            if (close == std::string::npos) {
                break;  // an unterminated comment runs to the end of the line
            }
            result.push_back(' ');
            index = close + 2;
            continue;
        }
        result.push_back(line[index]);
        ++index;
    }
    const std::string trimmed = trim(result);
    if (starts_with(trimmed, "--")) {
        return {};
    }
    return result;
}

std::vector<std::string> split_statements(const std::string& line) {
    std::vector<std::string> parts;
    int depth = 0;
    bool in_string = false;
    char quote = 0;
    std::string current;
    for (char character : line) {
        if (in_string) {
            current.push_back(character);
            if (character == quote) {
                in_string = false;
            }
            continue;
        }
        if (character == '\'' || character == '"') {
            in_string = true;
            quote = character;
            current.push_back(character);
            continue;
        }
        if (character == '(') {
            ++depth;
        } else if (character == ')') {
            --depth;
        }
        if (character == ';' && depth == 0) {
            parts.push_back(trim(current));
            current.clear();
            continue;
        }
        current.push_back(character);
    }
    const std::string tail = trim(current);
    if (!tail.empty()) {
        parts.push_back(tail);
    }
    std::vector<std::string> result;
    for (const std::string& part : parts) {
        if (!part.empty()) {
            result.push_back(part);
        }
    }
    return result;
}

std::vector<std::string> split_args(const std::string& text) {
    std::vector<std::string> parts;
    if (trim(text).empty()) {
        return parts;
    }
    int depth = 0;
    bool in_string = false;
    char quote = 0;
    std::string current;
    for (char character : text) {
        if (in_string) {
            current.push_back(character);
            if (character == quote) {
                in_string = false;
            }
            continue;
        }
        if (character == '\'' || character == '"') {
            in_string = true;
            quote = character;
            current.push_back(character);
            continue;
        }
        if (character == '(') {
            ++depth;
        } else if (character == ')') {
            --depth;
        }
        if (character == ',' && depth == 0) {
            parts.push_back(trim(current));
            current.clear();
            continue;
        }
        current.push_back(character);
    }
    const std::string tail = trim(current);
    if (!tail.empty()) {
        parts.push_back(tail);
    }
    std::vector<std::string> result;
    for (const std::string& part : parts) {
        if (!part.empty()) {
            result.push_back(part);
        }
    }
    return result;
}

std::vector<std::string> code_words(const std::string& line) {
    std::vector<std::string> words;
    bool in_string = false;
    char quote = 0;
    std::string current;
    for (char character : line) {
        if (in_string) {
            if (character == quote) {
                in_string = false;
            }
            continue;
        }
        if (character == '\'' || character == '"') {
            in_string = true;
            quote = character;
            if (!current.empty()) {
                words.push_back(current);
                current.clear();
            }
            continue;
        }
        if (std::isalnum(static_cast<unsigned char>(character)) != 0 ||
            character == '_') {
            current.push_back(character);
        } else if (!current.empty()) {
            words.push_back(current);
            current.clear();
        }
    }
    if (!current.empty()) {
        words.push_back(current);
    }
    return words;
}

const std::vector<std::string>& block_openers() {
    static const std::vector<std::string> words = {"DO", "SELECT"};
    return words;
}

bool in_words(const std::vector<std::string>& words, std::string_view value) {
    return std::find(words.begin(), words.end(), value) != words.end();
}

std::size_t scan_block_end(Interpreter& interpreter, std::size_t start) {
    int depth = 1;
    std::size_t index = start + 1;
    const auto& lines = interpreter.program_lines();
    while (index < lines.size()) {
        for (const std::string& word : code_words(strip_comment(lines[index].text))) {
            const std::string up = upper(word);
            if (in_words(block_openers(), up)) {
                ++depth;
            } else if (up == "END") {
                --depth;
            }
        }
        if (depth <= 0) {
            return index;
        }
        ++index;
    }
    throw RexxError("missing END for block");
}

const std::regex& label_pattern() {
    static const std::regex pattern{R"(^\s*([A-Za-z_!?][A-Za-z0-9_!?.]*)\s*:\s*(.*)$)",
                                    std::regex::ECMAScript};
    return pattern;
}

std::map<std::string, std::size_t> labels(Interpreter& interpreter) {
    RexxState& state = interpreter.executor_state<RexxState>();
    if (!state.labels.has_value()) {
        std::map<std::string, std::size_t> found;
        const auto& lines = interpreter.program_lines();
        for (std::size_t index = 0; index < lines.size(); ++index) {
            std::smatch match;
            const std::string text = strip_comment(lines[index].text);
            if (std::regex_match(text, match, label_pattern())) {
                found.emplace(upper(match[1].str()), index);
            }
        }
        state.labels = std::move(found);
    }
    return *state.labels;
}

std::string statement_after_label(const std::string& text) {
    std::smatch match;
    if (std::regex_match(text, match, label_pattern())) {
        return trim(match[2].str());
    }
    return trim(text);
}

/// Whether the text at a line starts a DO block.
bool line_starts_do(Interpreter& interpreter, std::size_t line) {
    const auto& lines = interpreter.program_lines();
    if (line >= lines.size()) {
        return false;
    }
    return starts_with(upper(statement_after_label(strip_comment(lines[line].text))), "DO");
}

std::size_t after_else_clause(Interpreter& interpreter, std::size_t else_line);

/// The first line after the statement that begins at `line`.
///
/// One line, unless that line opens something: a DO block runs to its END, and an IF
/// carries its own body and then any ELSE clause of its own. An IF's body is what
/// this has to get right, because the statement after `IF c THEN` may itself be an
/// `IF ... ELSE ...`, and skipping "one line" would leave the inner ELSE looking
/// like an ELSE of the outer IF.
std::size_t after_statement(Interpreter& interpreter, std::size_t line) {
    const auto& lines = interpreter.program_lines();
    if (line >= lines.size()) {
        return line;
    }
    const std::string text = statement_after_label(strip_comment(lines[line].text));
    const std::string shouted = upper(text);
    if (starts_with(shouted, "DO")) {
        return scan_block_end(interpreter, line) + 1;
    }
    static const std::regex then_pattern{R"(^IF\b.*\bTHEN\b(.*)$)",
                                         std::regex::ECMAScript | std::regex::icase};
    std::smatch match;
    if (std::regex_match(text, match, then_pattern)) {
        // `IF c THEN statement` is complete on its line; `IF c THEN` has its body on
        // the next one, and that body is itself a statement.
        const std::string tail = trim(match[1].str());
        std::size_t end = line + 1;
        if (starts_with(upper(tail), "DO")) {
            end = scan_block_end(interpreter, line) + 1;  // `IF c THEN DO`: block here
        } else if (tail.empty() && line + 1 < lines.size()) {
            end = after_statement(interpreter, line + 1);
        }
        if (end < lines.size() &&
            starts_with(upper(statement_after_label(strip_comment(lines[end].text))), "ELSE")) {
            return after_else_clause(interpreter, end);
        }
        return end;
    }
    return line + 1;
}

/// The first line after the ELSE clause that begins at `else_line`.
///
/// An ELSE can be followed by one statement, by a DO block, or by another IF --
/// `ELSE IF ... THEN` is the chain REXX programs are written with, and it is how one
/// says "otherwise, try this instead". Skipping a taken IF's ELSE means skipping all
/// of that, so this walks the chain: each clause ends one line after its body, or
/// after its `END` when the body is a block, and a clause that is itself an IF hands
/// over to the ELSE that may follow its own body.
std::size_t after_else_clause(Interpreter& interpreter, std::size_t else_line) {
    const auto& lines = interpreter.program_lines();
    static const std::regex else_pattern{R"(^ELSE\b)", std::regex::ECMAScript | std::regex::icase};
    static const std::regex then_pattern{R"(^IF\b.*\bTHEN\b(.*)$)",
                                         std::regex::ECMAScript | std::regex::icase};
    const auto text_at = [&](std::size_t index) {
        return index < lines.size() ? statement_after_label(strip_comment(lines[index].text))
                                    : std::string{};
    };
    // One statement, or a whole DO block, beginning at `line`.
    const auto after_body = [&](std::size_t line) {
        return line_starts_do(interpreter, line) ? scan_block_end(interpreter, line) + 1
                                                 : std::min(line + 1, lines.size());
    };

    std::size_t line = else_line;
    for (std::size_t guard = 0; guard < lines.size(); ++guard) {
        const std::string text = text_at(line);
        std::smatch match;
        if (!std::regex_search(text, match, else_pattern)) {
            return line;
        }
        const std::string body = trim(std::regex_replace(text, else_pattern, ""));
        std::size_t end = 0;
        if (body.empty()) {
            end = after_body(line + 1);  // `ELSE` alone: its body is the next line
        } else if (std::smatch inner; std::regex_match(body, inner, then_pattern)) {
            // `ELSE IF ... THEN`: the inner body follows on the next line, or opens a
            // block on this one (`... THEN DO`), or is already complete after THEN.
            const std::string tail = trim(inner[1].str());
            if (tail.empty()) {
                end = after_body(line + 1);
            } else if (starts_with(upper(tail), "DO")) {
                end = scan_block_end(interpreter, line) + 1;
            } else {
                end = line + 1;
            }
        } else if (starts_with(upper(body), "DO")) {
            end = scan_block_end(interpreter, line) + 1;
        } else {
            end = line + 1;  // `ELSE statement` on one line
        }
        if (!starts_with(upper(text_at(end)), "ELSE")) {
            return end;
        }
        line = end;
    }
    return std::min(else_line + 1, lines.size());
}



// ---------------------------------------------------------------------------

RValue call_builtin(Interpreter& interpreter, const std::string& name,
                    const std::vector<RValue>& args) {
    const auto s = [&args](std::size_t index) {
        return index < args.size() ? fmt(args[index]) : std::string{};
    };
    const auto n = [&args](std::size_t index, double fallback = 0.0) {
        return index < args.size() ? num(args[index]) : fallback;
    };

    if (name == "ARG") {
        RexxState& state = interpreter.executor_state<RexxState>();
        const long long index = static_cast<long long>(n(0, 1)) - 1;
        if (index >= 0 && static_cast<std::size_t>(index) < state.args.size()) {
            return state.args[static_cast<std::size_t>(index)];
        }
        return RValue::word("");
    }
    if (name == "LENGTH") {
        return RValue::numeric(static_cast<double>(s(0).size()));
    }
    if (name == "SUBSTR") {
        const std::string text = s(0);
        const long long start = static_cast<long long>(n(1, 1));
        const long long begin = start - 1;
        if (args.size() >= 3) {
            const long long count = static_cast<long long>(n(2));
            if (begin < 0 || static_cast<std::size_t>(begin) >= text.size()) {
                return RValue::word("");
            }
            return RValue::word(text.substr(static_cast<std::size_t>(begin),
                                            static_cast<std::size_t>(std::max(0LL, count))));
        }
        if (begin < 0 || static_cast<std::size_t>(begin) >= text.size()) {
            return RValue::word("");
        }
        return RValue::word(text.substr(static_cast<std::size_t>(begin)));
    }
    // `LEFT(string, length, pad)` and `RIGHT(...)` return exactly `length`
    // characters: they truncate and they *pad*, which is what makes them useful for
    // a column. Both used to ignore the length when the text was short, so a report
    // that lined up on paper came out as one run-together line.
    if (name == "LEFT" || name == "RIGHT") {
        const std::string text = s(0);
        const long long count = static_cast<long long>(n(1));
        if (count <= 0) {
            return RValue::word("");
        }
        const std::string pad = args.size() >= 3 ? s(2) : std::string{" "};
        const char fill = pad.empty() ? ' ' : pad.front();
        const auto wanted = static_cast<std::size_t>(count);
        std::string result;
        if (text.size() >= wanted) {
            result = name == "LEFT" ? text.substr(0, wanted)
                                    : text.substr(text.size() - wanted);
        } else {
            const std::string padding(wanted - text.size(), fill);
            result = name == "LEFT" ? text + padding : padding + text;
        }
        return RValue::word(result);
    }
    if (name == "COPIES") {
        const long long count = std::max(0LL, static_cast<long long>(n(1)));
        std::string result;
        for (long long index = 0; index < count; ++index) {
            result += s(0);
        }
        return RValue::word(result);
    }
    if (name == "REVERSE") {
        std::string text = s(0);
        std::reverse(text.begin(), text.end());
        return RValue::word(text);
    }
    if (name == "UPPER") {
        return RValue::word(upper(s(0)));
    }
    if (name == "LOWER") {
        std::string text = s(0);
        for (char& character : text) {
            character =
                static_cast<char>(std::tolower(static_cast<unsigned char>(character)));
        }
        return RValue::word(text);
    }
    if (name == "STRIP") {
        return RValue::word(trim(s(0)));
    }
    if (name == "SPACE") {
        std::vector<std::string> words;
        std::size_t index = 0;
        const std::string text = s(0);
        while (index < text.size()) {
            while (index < text.size() && is_space(text[index])) {
                ++index;
            }
            const std::size_t start = index;
            while (index < text.size() && !is_space(text[index])) {
                ++index;
            }
            if (index > start) {
                words.push_back(text.substr(start, index - start));
            }
        }
        if (words.empty()) {
            return RValue::word("");
        }
        const long long count = std::max(0LL, static_cast<long long>(n(1, 1)));
        const std::string pad = s(2).empty() ? " " : s(2);
        std::string result;
        for (std::size_t word = 0; word < words.size(); ++word) {
            if (word != 0) {
                for (long long repeat = 0; repeat < count; ++repeat) {
                    result += pad;
                }
            }
            result += words[word];
        }
        return RValue::word(result);
    }
    if (name == "WORD") {
        const std::vector<std::string> words = split_ws(s(0));
        const long long index = static_cast<long long>(n(1, 1)) - 1;
        if (index >= 0 && static_cast<std::size_t>(index) < words.size()) {
            return RValue::word(words[static_cast<std::size_t>(index)]);
        }
        return RValue::word("");
    }
    if (name == "WORDS") {
        return RValue::numeric(static_cast<double>(split_ws(s(0)).size()));
    }
    if (name == "INDEX") {
        const std::size_t found = s(0).find(s(1));
        return RValue::numeric(found == std::string::npos
                                   ? 0.0
                                   : static_cast<double>(found + 1));
    }
    if (name == "ABS") {
        return RValue::numeric(std::fabs(n(0)));
    }
    if (name == "MAX" || name == "MIN") {
        if (args.empty()) {
            return RValue::numeric(0.0);
        }
        double result = num(args.front());
        for (const RValue& argument : args) {
            result = name == "MAX" ? std::max(result, num(argument))
                                   : std::min(result, num(argument));
        }
        return RValue::numeric(result);
    }
    if (name == "TRUNC") {
        return RValue::numeric(std::trunc(n(0)));
    }
    if (name == "SIGN") {
        const double value = n(0);
        return RValue::numeric(value > 0 ? 1 : (value < 0 ? -1 : 0));
    }
    if (name == "FORMAT") {
        return RValue::word(fmt(RValue::numeric(n(0))));
    }
    if (name == "DATATYPE") {
        return RValue::word(is_numeric(s(0)) ? "NUM" : "CHAR");
    }
    throw RexxError("unknown function " + name);
}

}  // namespace rexx_detail
}  // namespace timewarp::lang
