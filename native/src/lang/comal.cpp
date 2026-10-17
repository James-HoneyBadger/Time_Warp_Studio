// SPDX-License-Identifier: MIT
//
// COMAL-80, ported from `time_warp/languages/comal.py`.
//
// A line-by-line language that blends BASIC's immediacy with Pascal's blocks:
// IF/ELIF/ELSE/ENDIF, FOR/NEXT, WHILE/ENDWHILE, REPEAT/UNTIL, LOOP/EXIT/ENDLOOP,
// PROC/ENDPROC, DIM/DATA/READ/RESTORE and GOTO/GOSUB. The reference records loop
// and IF state on a stack; so does this, because `NEXT` must find its own `FOR`
// through any inner blocks that have already closed.

#include "timewarp/lang/comal.hpp"

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstddef>
#include <cstdlib>
#include <functional>
#include <map>
#include <optional>
#include <regex>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace {

class ComalError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

struct ComalFrame {
    ComalFrame() = default;
    ComalFrame(std::string kind_text, std::size_t start_index)
        : kind{std::move(kind_text)}, start{start_index} {}

    std::string kind;
    std::size_t start = 0;
    bool taken = false;
    std::string variable;
    double limit = 0.0;
    double step = 1.0;
    std::string condition;
};

struct ComalState {
    std::vector<ComalFrame> stack;
    std::vector<std::size_t> call_stack;
    std::vector<std::string> data;
    std::size_t data_ptr = 0;
};

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

std::string upper(std::string_view text) {
    std::string result{text};
    for (char& character : result) {
        character = static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
    }
    return result;
}

/// Removes `//` comments outside string literals.
std::string strip_comment(std::string_view line) {
    bool in_string = false;
    for (std::size_t index = 0; index < line.size(); ++index) {
        if (line[index] == '"') {
            in_string = !in_string;
        } else if (!in_string && index + 1 < line.size() && line[index] == '/' &&
                   line[index + 1] == '/') {
            return std::string{line.substr(0, index)};
        }
    }
    return std::string{line};
}

/// Comment-stripped text with string contents blanked, for keyword matching.
std::string code_text(std::string_view line) {
    const std::string stripped = strip_comment(line);
    std::string result;
    bool in_string = false;
    for (char character : stripped) {
        if (character == '"') {
            in_string = !in_string;
            result.append("  ");
        } else if (in_string) {
            result.push_back(' ');
        } else {
            result.push_back(character);
        }
    }
    return trim(result);
}

std::string upper_code(Interpreter& interpreter, std::size_t index) {
    if (index >= interpreter.program_lines().size()) {
        return {};
    }
    return upper(code_text(interpreter.program_lines()[index].text));
}

std::vector<std::string> split_top(std::string_view text, std::string_view separators = ",") {
    std::vector<std::string> parts;
    std::string current;
    int depth = 0;
    bool in_string = false;
    for (char character : text) {
        if (in_string) {
            current.push_back(character);
            if (character == '"') {
                in_string = false;
            }
            continue;
        }
        if (character == '"') {
            in_string = true;
            current.push_back(character);
            continue;
        }
        if (character == '(') {
            ++depth;
        } else if (character == ')') {
            --depth;
        }
        if (depth == 0 && separators.find(character) != std::string_view::npos) {
            parts.push_back(current);
            current.clear();
            continue;
        }
        current.push_back(character);
    }
    parts.push_back(current);
    return parts;
}

bool if_open(std::string_view code) {
    if (!(code.rfind("IF ", 0) == 0 || code == "IF")) {
        return false;
    }
    const std::string text = trim(code);
    return text.size() >= 4 && text.rfind("THEN") == text.size() - 4;
}

bool for_open(std::string_view code) { return code.rfind("FOR ", 0) == 0; }

using Predicate = std::function<bool(std::string_view)>;

std::size_t find_end(Interpreter& interpreter, std::size_t start, Predicate opener,
                     Predicate closer) {
    int depth = 1;
    std::size_t index = start + 1;
    while (index < interpreter.program_lines().size()) {
        const std::string code = upper_code(interpreter, index);
        if (closer(code)) {
            --depth;
        } else if (opener(code)) {
            ++depth;
        }
        if (depth <= 0) {
            return index;
        }
        ++index;
    }
    throw ComalError("unterminated block");
}

bool for_in_range(double value, double limit, double step) {
    return step >= 0.0 ? value <= limit : value >= limit;
}

std::string format_number(double value) {
    if (std::trunc(value) == value && std::abs(value) < 1e15) {
        return std::to_string(static_cast<long long>(value));
    }
    char buffer[64];
    const auto result = std::to_chars(buffer, buffer + sizeof buffer, value);
    return std::string{buffer, result.ptr};
}

class ComalExecutor {
public:
    ComalExecutor(Interpreter& interpreter, TurtleHost& turtle, ComalState& state)
        : interpreter_{interpreter}, turtle_{turtle}, state_{state} {}

    std::string execute(std::string_view source) {
        const std::string raw = strip_comment(source);
        if (trim(raw).empty()) {
            return {};
        }
        if (upper(trim(raw)).rfind("REM", 0) == 0) {
            return {};
        }
        std::vector<std::string> output;
        try {
            statement(trim(raw), output);
        } catch (const ComalError& error) {
            output.push_back("❌ COMAL error: " + std::string{error.what()});
        } catch (const std::exception& error) {
            output.push_back("❌ COMAL error: " + std::string{error.what()});
        }
        if (output.empty()) {
            return {};
        }
        std::string joined;
        for (const std::string& line : output) {
            joined += line;
            joined.push_back('\n');
        }
        return joined;
    }

private:
    // -- values -----------------------------------------------------------

    Value get_variable(const std::string& name) {
        const std::string key = upper(name);
        const auto found = interpreter_.variables().find(key);
        if (found != interpreter_.variables().end()) {
            return found->second;
        }
        return key.size() > 0 && key.back() == '$' ? Value{std::string{}} : Value{0.0};
    }

    void set_variable(const std::string& name, const Value& value) {
        const std::string key = upper(name);
        if (!key.empty() && key.back() == '$') {
            interpreter_.set_text(key, value.is_text() ? value.text() : value.render());
        } else {
            interpreter_.set_number(key, value.is_number() ? value.number() : 0.0);
        }
    }

    double eval_num(std::string_view expression) {
        const std::string text = trim(expression);
        if (text.empty()) {
            return 0.0;
        }
        if (text.size() >= 2 && text.front() == '"' && text.back() == '"') {
            return 0.0;
        }
        try {
            return interpreter_.evaluate_expression(text);
        } catch (const ComalError&) {
            // A FUNC body this executor cannot read has to reach the user as
            // an error; swallowing it here is how it became a plausible 0.
            throw;
        } catch (const std::exception&) {
            return 0.0;
        }
    }

    Value eval_value(std::string_view expression) {
        const std::string text = trim(expression);
        if (text.size() >= 2 && text.front() == '"' && text.back() == '"') {
            return Value{text.substr(1, text.size() - 2)};
        }
        if (!text.empty() && text.back() == '$') {
            return get_variable(text);
        }
        if (text.find('"') != std::string::npos ||
            std::regex_search(text, std::regex{"[A-Za-z_]\\w*\\$"})) {
            return Value{eval_string(text)};
        }
        return Value{eval_num(text)};
    }

    /// String expressions in the reference go through the string evaluator;
    /// here the part COMAL uses is concatenation with `+`.
    std::string eval_string(std::string_view expression) {
        const std::vector<std::string> parts = split_top(expression, "+");
        std::string result;
        for (const std::string& part : parts) {
            const std::string piece = trim(part);
            if (piece.size() >= 2 && piece.front() == '"' && piece.back() == '"') {
                result += piece.substr(1, piece.size() - 2);
            } else if (!piece.empty() && piece.back() == '$') {
                result += get_variable(piece).text();
            } else {
                result += format_number(eval_num(piece));
            }
        }
        return result;
    }

    std::string as_string(std::string_view expression) {
        const std::string text = trim(expression);
        if (text.size() >= 2 && text.front() == '"' && text.back() == '"') {
            return text.substr(1, text.size() - 2);
        }
        if (!text.empty() && text.back() == '$' && std::regex_match(
                                                        text, std::regex{"[A-Za-z_]\\w*\\$"})) {
            return get_variable(text).text();
        }
        const Value value = eval_value(text);
        return value.is_text() ? value.text() : format_number(value.number());
    }

    static bool truthy(const std::string& text) {
        const std::string value = trim(text);
        if (value.empty()) {
            return false;
        }
        char* end = nullptr;
        const double number = std::strtod(value.c_str(), &end);
        return end != value.c_str() && *end == '\0' ? number != 0.0 : true;
    }

    bool eval_condition(std::string_view condition) {
        const std::string text = trim(condition);
        if (text.find('"') != std::string::npos || text.find('$') != std::string::npos) {
            static const std::vector<std::string> kOperators = {"<>", "<=", ">=", "=", "<", ">"};
            for (const std::string& op : kOperators) {
                const std::size_t at = text.find(op);
                if (at == std::string::npos) {
                    continue;
                }
                const std::string left = as_string(std::string_view{text}.substr(0, at));
                const std::string right = as_string(std::string_view{text}.substr(at + op.size()));
                if (op == "=") {
                    return left == right;
                }
                if (op == "<>") {
                    return left != right;
                }
                if (op == "<") {
                    return left < right;
                }
                if (op == ">") {
                    return left > right;
                }
                if (op == "<=") {
                    return left <= right;
                }
                return left >= right;
            }
        }
        try {
            return interpreter_.evaluate_expression(text) != 0.0;
        } catch (const std::exception&) {
            return truthy(as_string(text));
        }
    }

    // -- control ----------------------------------------------------------

    void jump(std::size_t index) { interpreter_.current_line = index; }

    ComalFrame* top(std::string_view kind) {
        if (state_.stack.empty() || state_.stack.back().kind != kind) {
            return nullptr;
        }
        return &state_.stack.back();
    }

    ComalFrame* nearest(std::string_view kind) {
        for (auto it = state_.stack.rbegin(); it != state_.stack.rend(); ++it) {
            if (it->kind == kind) {
                return &*it;
            }
        }
        return nullptr;
    }

    // -- statements -------------------------------------------------------

    void statement(const std::string& input, std::vector<std::string>& output) {
        const std::string text = trim(input);
        const std::string up = upper(text);

        if (up == "ENDIF") {
            if (!state_.stack.empty()) {
                state_.stack.pop_back();
            }
            return;
        }
        if (up.rfind("ELIF ", 0) == 0 || up == "ELIF") {
            handle_elif(text);
            return;
        }
        if (up == "ELSE") {
            handle_else();
            return;
        }
        if (up.rfind("NEXT", 0) == 0) {
            handle_next();
            return;
        }
        if (up == "ENDWHILE") {
            handle_endwhile();
            return;
        }
        if (up.rfind("UNTIL ", 0) == 0) {
            handle_until(text);
            return;
        }
        if (up == "ENDLOOP") {
            handle_endloop();
            return;
        }
        if (up == "ENDPROC" || up == "ENDFUNC") {
            handle_endproc();
            return;
        }
        if (up == "EXIT") {
            handle_exit();
            return;
        }
        if (up.rfind("IF ", 0) == 0) {
            handle_if(text, output);
            return;
        }
        if (up.rfind("FOR ", 0) == 0) {
            handle_for(text);
            return;
        }
        if (up.rfind("WHILE ", 0) == 0) {
            handle_while(text);
            return;
        }
        if (up == "REPEAT") {
            state_.stack.push_back(ComalFrame{"repeat", interpreter_.current_line});
            return;
        }
        if (up == "LOOP") {
            state_.stack.push_back(ComalFrame{"loop", interpreter_.current_line});
            return;
        }
        if (up.rfind("PROC ", 0) == 0 || up.rfind("FUNC ", 0) == 0) {
            handle_proc();
            return;
        }
        if (up.rfind("CALL ", 0) == 0) {
            handle_call(trim(std::string_view{text}.substr(5)));
            return;
        }
        if (up.rfind("RETURN", 0) == 0) {
            handle_return(trim(std::string_view{text}.substr(6)));
            return;
        }
        if (up.rfind("GOTO ", 0) == 0) {
            handle_goto(trim(std::string_view{text}.substr(5)));
            return;
        }
        if (up.rfind("GOSUB ", 0) == 0) {
            handle_gosub(trim(std::string_view{text}.substr(6)));
            return;
        }
        if (up.rfind("DIM ", 0) == 0) {
            handle_dim(trim(std::string_view{text}.substr(4)));
            return;
        }
        if (up.rfind("DATA ", 0) == 0) {
            handle_data(trim(std::string_view{text}.substr(5)));
            return;
        }
        if (up.rfind("READ ", 0) == 0) {
            handle_read(trim(std::string_view{text}.substr(5)));
            return;
        }
        if (up == "RESTORE") {
            state_.data_ptr = 0;
            return;
        }
        if (up.rfind("PRINT", 0) == 0) {
            handle_print(trim(std::string_view{text}.substr(5)), output);
            return;
        }
        if (up.rfind("INPUT", 0) == 0) {
            handle_input(trim(std::string_view{text}.substr(5)));
            return;
        }
        if (up == "STOP" || up == "END") {
            interpreter_.running = false;
            return;
        }
        if (handle_turtle(up, text)) {
            return;
        }

        static const std::regex assignment{
            R"(^([A-Za-z_]\w*\$?)\s*(?:\(\s*([^)]*)\s*\))?\s*:?=\s*(.+)$)",
            std::regex::ECMAScript};
        std::smatch match;
        if (std::regex_match(text, match, assignment)) {
            const std::string name = match[1].str();
            const std::string index = match[2].str();
            const std::string value_expression = trim(match[3].str());
            if (!index.empty()) {
                const int position = static_cast<int>(eval_num(index));
                const auto array = interpreter_.arrays.find(upper(name));
                if (array == interpreter_.arrays.end()) {
                    throw ComalError("array " + upper(name) + " not dimensioned");
                }
                const Value value = eval_value(value_expression);
                std::vector<double>& values = array->second;
                while (static_cast<int>(values.size()) <= position) {
                    values.push_back(0.0);
                }
                if (position >= 0) {
                    values[static_cast<std::size_t>(position)] =
                        value.is_number() ? value.number() : 0.0;
                }
                return;
            }
            set_variable(name, eval_value(value_expression));
            return;
        }
        throw ComalError("unrecognised statement: " + text);
    }

    // -- conditionals -----------------------------------------------------

    void handle_if(const std::string& text, std::vector<std::string>& output) {
        static const std::regex pattern{R"(^IF\s+(.*?)\s+THEN\b(.*)$)",
                                        std::regex::ECMAScript | std::regex::icase};
        std::smatch match;
        if (!std::regex_match(text, match, pattern)) {
            throw ComalError("IF requires THEN");
        }
        const std::string condition = trim(match[1].str());
        const std::string rest = trim(match[2].str());
        if (!rest.empty()) {
            static const std::regex else_pattern{R"(^(.*?)\s+ELSE\s+(.*)$)",
                                                 std::regex::ECMAScript | std::regex::icase};
            std::smatch else_match;
            std::string then_statement = rest;
            std::string else_statement;
            if (std::regex_match(rest, else_match, else_pattern)) {
                then_statement = trim(else_match[1].str());
                else_statement = trim(else_match[2].str());
            }
            if (eval_condition(condition)) {
                statement(then_statement, output);
            } else if (!else_statement.empty()) {
                statement(else_statement, output);
            }
            return;
        }

        const std::size_t start = interpreter_.current_line;
        const std::size_t end =
            find_end(interpreter_, start, if_open, [](std::string_view code) {
                return code.rfind("ENDIF", 0) == 0;
            });
        state_.stack.push_back(ComalFrame{"if", start});
        if (eval_condition(condition)) {
            state_.stack.back().taken = true;
            return;
        }
        next_branch(start, end);
    }

    void next_branch(std::size_t start, std::size_t end) {
        int depth = 0;
        for (std::size_t index = start + 1; index < end; ++index) {
            const std::string code = upper_code(interpreter_, index);
            if (if_open(code)) {
                ++depth;
            } else if (code.rfind("ENDIF", 0) == 0) {
                --depth;
            } else if (depth == 0 &&
                       (code.rfind("ELIF", 0) == 0 || code == "ELSE")) {
                jump(index);
                return;
            }
        }
        if (!state_.stack.empty() && state_.stack.back().kind == "if") {
            state_.stack.pop_back();
        }
        jump(end + 1);
    }

    void handle_elif(const std::string& text) {
        ComalFrame* frame = top("if");
        if (frame == nullptr) {
            throw ComalError("ELIF without IF");
        }
        const std::size_t end = find_end(
            interpreter_, frame->start, if_open,
            [](std::string_view code) { return code.rfind("ENDIF", 0) == 0; });
        if (frame->taken) {
            jump(end + 1);
            return;
        }
        static const std::regex pattern{R"(^ELIF\s+(.*?)\s+THEN\s*$)",
                                        std::regex::ECMAScript | std::regex::icase};
        std::smatch match;
        if (!std::regex_match(text, match, pattern)) {
            throw ComalError("ELIF requires THEN");
        }
        if (eval_condition(trim(match[1].str()))) {
            frame->taken = true;
            return;
        }
        next_branch(interpreter_.current_line, end);
    }

    void handle_else() {
        ComalFrame* frame = top("if");
        if (frame == nullptr) {
            throw ComalError("ELSE without IF");
        }
        if (frame->taken) {
            const std::size_t end = find_end(
                interpreter_, frame->start, if_open,
                [](std::string_view code) { return code.rfind("ENDIF", 0) == 0; });
            jump(end + 1);
            return;
        }
        frame->taken = true;
    }

    // -- loops ------------------------------------------------------------

    void handle_for(const std::string& text) {
        static const std::regex pattern{
            R"(^FOR\s+([A-Za-z_]\w*)\s*:?=\s*(.+?)\s+TO\s+(.+?)(?:\s+STEP\s+(.+?))?\s*(?:DO)?$)",
            std::regex::ECMAScript | std::regex::icase};
        std::smatch match;
        if (!std::regex_match(text, match, pattern)) {
            throw ComalError("malformed FOR statement");
        }
        const std::string name = match[1].str();
        const double start = eval_num(match[2].str());
        const double limit = eval_num(match[3].str());
        const double step = match[4].matched ? eval_num(match[4].str()) : 1.0;
        set_variable(name, Value{start});
        ComalFrame frame{"for", interpreter_.current_line};
        frame.variable = upper(name);
        frame.limit = limit;
        frame.step = step;
        if (!for_in_range(start, limit, step)) {
            const std::size_t end = find_end(
                interpreter_, interpreter_.current_line, for_open,
                [](std::string_view code) { return code.rfind("NEXT", 0) == 0; });
            jump(end + 1);
            return;
        }
        state_.stack.push_back(std::move(frame));
    }

    void handle_next() {
        for (std::size_t index = state_.stack.size(); index-- > 0;) {
            if (state_.stack[index].kind != "for") {
                continue;
            }
            ComalFrame frame = state_.stack[index];
            state_.stack.erase(state_.stack.begin() + static_cast<std::ptrdiff_t>(index + 1),
                               state_.stack.end());
            const double value = get_variable(frame.variable).number() + frame.step;
            set_variable(frame.variable, Value{value});
            if (for_in_range(value, frame.limit, frame.step)) {
                jump(frame.start + 1);
                return;
            }
            state_.stack.erase(state_.stack.begin() + static_cast<std::ptrdiff_t>(index));
            return;
        }
    }

    void handle_while(const std::string& text) {
        const std::string condition = trim(std::string_view{text}.substr(5));
        if (!eval_condition(condition)) {
            const std::size_t end = find_end(
                interpreter_, interpreter_.current_line,
                [](std::string_view code) { return code.rfind("WHILE", 0) == 0; },
                [](std::string_view code) { return code == "ENDWHILE"; });
            jump(end + 1);
            return;
        }
        ComalFrame frame{"while", interpreter_.current_line};
        frame.condition = condition;
        state_.stack.push_back(std::move(frame));
    }

    void handle_endwhile() {
        for (std::size_t index = state_.stack.size(); index-- > 0;) {
            if (state_.stack[index].kind != "while") {
                continue;
            }
            const ComalFrame frame = state_.stack[index];
            state_.stack.erase(state_.stack.begin() + static_cast<std::ptrdiff_t>(index + 1),
                               state_.stack.end());
            if (eval_condition(frame.condition)) {
                jump(frame.start + 1);
                return;
            }
            state_.stack.erase(state_.stack.begin() + static_cast<std::ptrdiff_t>(index));
            return;
        }
        throw ComalError("ENDWHILE without WHILE");
    }

    void handle_until(const std::string& text) {
        const std::string condition = trim(std::string_view{text}.substr(5));
        for (std::size_t index = state_.stack.size(); index-- > 0;) {
            if (state_.stack[index].kind != "repeat") {
                continue;
            }
            const ComalFrame frame = state_.stack[index];
            state_.stack.erase(state_.stack.begin() + static_cast<std::ptrdiff_t>(index + 1),
                               state_.stack.end());
            if (eval_condition(condition)) {
                state_.stack.erase(state_.stack.begin() + static_cast<std::ptrdiff_t>(index));
                return;
            }
            jump(frame.start + 1);
            return;
        }
        throw ComalError("UNTIL without REPEAT");
    }

    void handle_endloop() {
        for (std::size_t index = state_.stack.size(); index-- > 0;) {
            if (state_.stack[index].kind != "loop") {
                continue;
            }
            const ComalFrame frame = state_.stack[index];
            state_.stack.erase(state_.stack.begin() + static_cast<std::ptrdiff_t>(index + 1),
                               state_.stack.end());
            jump(frame.start + 1);
            return;
        }
        throw ComalError("ENDLOOP without LOOP");
    }

    void handle_exit() {
        for (std::size_t index = state_.stack.size(); index-- > 0;) {
            const std::string& kind = state_.stack[index].kind;
            if (kind != "for" && kind != "while" && kind != "repeat" && kind != "loop") {
                continue;
            }
            const ComalFrame frame = state_.stack[index];
            state_.stack.erase(state_.stack.begin() + static_cast<std::ptrdiff_t>(index),
                               state_.stack.end());
            const std::size_t end = loop_end(frame);
            jump(end + 1);
            return;
        }
        interpreter_.running = false;
    }

    std::size_t loop_end(const ComalFrame& frame) {
        if (frame.kind == "for") {
            return find_end(interpreter_, frame.start, for_open,
                            [](std::string_view code) { return code.rfind("NEXT", 0) == 0; });
        }
        if (frame.kind == "while") {
            return find_end(
                interpreter_, frame.start,
                [](std::string_view code) { return code.rfind("WHILE", 0) == 0; },
                [](std::string_view code) { return code == "ENDWHILE"; });
        }
        if (frame.kind == "repeat") {
            return find_end(
                interpreter_, frame.start,
                [](std::string_view code) { return code.rfind("REPEAT", 0) == 0; },
                [](std::string_view code) { return code.rfind("UNTIL", 0) == 0; });
        }
        return find_end(
            interpreter_, frame.start,
            [](std::string_view code) { return code == "LOOP"; },
            [](std::string_view code) { return code == "ENDLOOP"; });
    }

    // -- procedures -------------------------------------------------------

    void handle_proc() {
        const std::string code = upper_code(interpreter_, interpreter_.current_line);
        const bool function = code.rfind("FUNC", 0) == 0;
        const std::size_t end = find_end(
            interpreter_, interpreter_.current_line,
            [function](std::string_view text) { return text.rfind(function ? "FUNC" : "PROC", 0) == 0; },
            [function](std::string_view text) { return text == (function ? "ENDFUNC" : "ENDPROC"); });
        jump(end + 1);
    }

    std::optional<std::size_t> proc_body(const std::string& name) {
        const std::regex pattern{"^(PROC|FUNC)\\s+" + name + "\\b",
                                 std::regex::ECMAScript | std::regex::icase};
        for (std::size_t index = 0; index < interpreter_.program_lines().size(); ++index) {
            if (std::regex_search(code_text(interpreter_.program_lines()[index].text), pattern)) {
                return index;
            }
        }
        return std::nullopt;
    }

    void handle_call(const std::string& call) {
        static const std::regex pattern{R"(^([A-Za-z_]\w*)\s*(?:\((.*)\))?$)",
                                        std::regex::ECMAScript};
        std::smatch match;
        if (!std::regex_match(call, match, pattern)) {
            throw ComalError("malformed CALL");
        }
        const std::string name = match[1].str();
        const std::string arguments_text = match[2].matched ? match[2].str() : std::string{};
        std::vector<Value> arguments;
        for (const std::string& argument : split_top(arguments_text)) {
            if (!trim(argument).empty()) {
                arguments.push_back(eval_value(argument));
            }
        }
        const std::optional<std::size_t> body = proc_body(name);
        if (!body.has_value()) {
            throw ComalError("unknown procedure " + name);
        }
        const std::string header = code_text(interpreter_.program_lines()[*body].text);
        std::vector<std::string> parameters;
        const std::size_t open = header.find('(');
        const std::size_t close = header.rfind(')');
        if (open != std::string::npos && close != std::string::npos && close > open) {
            for (const std::string& parameter :
                 split_top(std::string_view{header}.substr(open + 1, close - open - 1))) {
                if (!trim(parameter).empty()) {
                    parameters.push_back(upper(trim(parameter)));
                }
            }
        }
        for (std::size_t index = 0; index < parameters.size(); ++index) {
            set_variable(parameters[index],
                         index < arguments.size() ? arguments[index] : Value{0.0});
        }
        state_.call_stack.push_back(interpreter_.current_line);
        jump(*body + 1);
    }

    /// A FUNC's value, for a caller that writes it inside an expression.
    ///
    /// The statement form (`CALL`, then `RESULT`) jumps into the body and lets
    /// the main loop run it; an expression cannot do that, so the body's
    /// assignments and RETURN are read here instead. Anything else raises: a
    /// body this cannot read must not quietly answer 0.
    double call_function(const std::string& name,
                         const std::vector<std::string>& parameters,
                         std::span<const double> arguments) {
        const std::optional<std::size_t> body = proc_body(name);
        if (!body.has_value()) {
            throw ComalError("unknown procedure " + name);
        }
        const std::size_t end = find_end(
            interpreter_, *body,
            [](std::string_view text) { return text.rfind("FUNC", 0) == 0; },
            [](std::string_view text) { return text == "ENDFUNC"; });

        struct SavedValue {
            std::string name;
            std::optional<Value> value;
        };
        std::vector<SavedValue> saved;
        for (std::size_t index = 0; index < parameters.size(); ++index) {
            const std::string& parameter = parameters[index];
            const auto existing = interpreter_.variables().find(parameter);
            SavedValue entry{parameter, std::nullopt};
            if (existing != interpreter_.variables().end()) {
                entry.value = existing->second;
            }
            saved.push_back(std::move(entry));
            set_variable(parameter,
                         Value{index < arguments.size() ? arguments[index] : 0.0});
        }
        const auto restore = [this, &saved]() {
            for (const SavedValue& entry : saved) {
                if (entry.value.has_value()) {
                    set_variable(entry.name, *entry.value);
                } else {
                    interpreter_.remove_variable(entry.name);
                }
            }
        };

        try {
            for (std::size_t index = *body + 1; index < end; ++index) {
                const std::string code =
                    code_text(interpreter_.program_lines()[index].text);
                if (code.empty()) {
                    continue;
                }
                if (upper(code).rfind("RETURN", 0) == 0) {
                    const std::string expression =
                        trim(std::string_view{code}.substr(6));
                    const double value =
                        expression.empty() ? 0.0 : eval_num(expression);
                    restore();
                    return value;
                }
                static const std::regex assignment{
                    R"(^([A-Za-z_]\w*)\s*=\s*(.+)$)", std::regex::ECMAScript};
                std::smatch match;
                if (std::regex_match(code, match, assignment)) {
                    set_variable(match[1].str(), Value{eval_num(match[2].str())});
                    continue;
                }
                throw ComalError("FUNC " + name + ": '" + code +
                                 "' cannot be evaluated inside an expression");
            }
            restore();
            return 0.0;
        } catch (...) {
            restore();
            throw;
        }
    }

    // The free function below installs this as the evaluator's resolver, so it
    // is the one member that has to be reachable from outside the class.
public:
    std::optional<expr::UserFunction> user_function(std::string_view upper_name) {
        const std::string name{upper_name};
        const std::optional<std::size_t> body = proc_body(name);
        if (!body.has_value()) {
            return std::nullopt;
        }
        const std::string header =
            code_text(interpreter_.program_lines()[*body].text);
        if (upper(header).rfind("FUNC", 0) != 0) {
            return std::nullopt;  // PROC is not callable in an expression
        }
        std::vector<std::string> parameters;
        const std::size_t open = header.find('(');
        const std::size_t close = header.rfind(')');
        if (open != std::string::npos && close != std::string::npos &&
            close > open) {
            for (const std::string& parameter :
                 split_top(std::string_view{header}.substr(open + 1, close - open - 1))) {
                if (!trim(parameter).empty()) {
                    parameters.push_back(upper(trim(parameter)));
                }
            }
        }
        expr::UserFunction function;
        function.arity = parameters.size();
        function.call = [this, name, parameters](std::span<const double> arguments) {
            return call_function(name, parameters, arguments);
        };
        return function;
    }

private:
    void handle_return(const std::string& expression) {
        if (!expression.empty()) {
            set_variable("RESULT", eval_value(expression));
        }
        if (!state_.call_stack.empty()) {
            const std::size_t return_line = state_.call_stack.back();
            state_.call_stack.pop_back();
            jump(return_line + 1);
            return;
        }
        interpreter_.running = false;
    }

    void handle_endproc() {
        if (!state_.call_stack.empty()) {
            const std::size_t return_line = state_.call_stack.back();
            state_.call_stack.pop_back();
            jump(return_line + 1);
        }
    }

    void handle_goto(const std::string& target) {
        const int number = static_cast<int>(eval_num(target));
        const auto found = interpreter_.line_number_map.find(number);
        if (found == interpreter_.line_number_map.end()) {
            throw ComalError("no such line " + std::to_string(number));
        }
        jump(found->second);
    }

    void handle_gosub(const std::string& target) {
        const int number = static_cast<int>(eval_num(target));
        const auto found = interpreter_.line_number_map.find(number);
        if (found == interpreter_.line_number_map.end()) {
            throw ComalError("no such line " + std::to_string(number));
        }
        state_.call_stack.push_back(interpreter_.current_line);
        jump(found->second);
    }

    // -- data and I/O -----------------------------------------------------

    void handle_dim(const std::string& spec) {
        for (const std::string& entry : split_top(spec)) {
            static const std::regex pattern{R"(^([A-Za-z_]\w*)\s*\(\s*(\d+)\s*\)$)"};
            std::smatch match;
            const std::string text = trim(entry);
            if (!std::regex_match(text, match, pattern)) {
                throw ComalError("malformed DIM: " + text);
            }
            const int size = std::stoi(match[2].str());
            interpreter_.arrays[upper(match[1].str())] = std::vector<double>(
                static_cast<std::size_t>(size) + 1, 0.0);
        }
    }

    void handle_data(const std::string& values) {
        for (const std::string& entry : split_top(values)) {
            const std::string text = trim(entry);
            if (text.empty()) {
                continue;
            }
            if (text.size() >= 2 && text.front() == '"' && text.back() == '"') {
                state_.data.push_back(text.substr(1, text.size() - 2));
            } else {
                state_.data.push_back(text);
            }
        }
    }

    void handle_read(const std::string& names) {
        for (const std::string& entry : split_top(names)) {
            const std::string name = trim(entry);
            if (name.empty()) {
                continue;
            }
            if (state_.data_ptr >= state_.data.size()) {
                throw ComalError("out of DATA");
            }
            const std::string raw = state_.data[state_.data_ptr++];
            if (!name.empty() && name.back() == '$') {
                set_variable(name, Value{raw});
            } else {
                char* end = nullptr;
                const double number = std::strtod(raw.c_str(), &end);
                set_variable(name, Value{end != raw.c_str() && *end == '\0' ? number : 0.0});
            }
        }
    }

    std::vector<std::pair<std::string, char>> split_print_parts(const std::string& text) {
        std::vector<std::pair<std::string, char>> parts;
        int depth = 0;
        bool in_string = false;
        std::string current;
        for (char character : text) {
            if (in_string) {
                current.push_back(character);
                if (character == '"') {
                    in_string = false;
                }
                continue;
            }
            if (character == '"') {
                in_string = true;
                current.push_back(character);
                continue;
            }
            if (character == '(') {
                ++depth;
            } else if (character == ')') {
                --depth;
            }
            if (depth == 0 && (character == ',' || character == ';')) {
                parts.emplace_back(trim(current), character);
                current.clear();
                continue;
            }
            current.push_back(character);
        }
        const std::string tail = trim(current);
        if (!tail.empty() || parts.empty()) {
            parts.emplace_back(tail, '\0');
        }
        return parts;
    }

    void handle_print(const std::string& input, std::vector<std::string>& output) {
        std::string args = input;
        if (upper(args).rfind("USING", 0) == 0) {
            const std::size_t colon = args.find(':');
            if (colon != std::string::npos) {
                args = trim(std::string_view{args}.substr(colon + 1));
            }
        }
        if (trim(args).empty()) {
            output.emplace_back();
            return;
        }
        const std::vector<std::pair<std::string, char>> parts = split_print_parts(args);
        std::string joined;
        for (std::size_t index = 0; index < parts.size(); ++index) {
            if (!parts[index].first.empty()) {
                joined += as_string(parts[index].first);
            }
            if (index + 1 < parts.size() && parts[index].second != ';') {
                joined.push_back(' ');
            }
        }
        output.push_back(joined);
    }

    void handle_input(const std::string& args) {
        static const std::regex pattern{R"(^(?:PROMPT\s+)?(".*?")\s*:?\s*(.*)$)",
                                        std::regex::ECMAScript | std::regex::icase};
        std::string prompt = "? ";
        std::string target = trim(args);
        std::smatch match;
        if (std::regex_match(target, match, pattern)) {
            const std::string quoted = match[1].str();
            prompt = quoted.substr(1, quoted.size() - 2) + " ";
            target = trim(match[2].str());
        }
        while (!target.empty() && target.back() == ':') {
            target.pop_back();
        }
        target = trim(target);
        if (target.empty()) {
            return;
        }
        const std::string variable = target.substr(0, target.find(' '));
        const bool string_variable = !variable.empty() && variable.back() == '$';
        interpreter_.request_input(prompt, upper(variable), !string_variable);
    }

    // -- turtle -----------------------------------------------------------

    bool handle_turtle(const std::string& up, const std::string& text) {
        const std::size_t space = up.find(' ');
        const std::string keyword = space == std::string::npos ? up : up.substr(0, space);
        const std::string rest =
            space == std::string::npos ? "" : trim(std::string_view{text}.substr(space));
        if (keyword == "MOVE") {
            turtle_.forward(eval_num(rest));
            return true;
        }
        if (keyword == "TURN") {
            turtle_.right(eval_num(rest));
            return true;
        }
        if (keyword == "PENUP") {
            turtle_.pen_up();
            return true;
        }
        if (keyword == "PENDOWN") {
            turtle_.pen_down();
            return true;
        }
        if (keyword == "HOME") {
            turtle_.home();
            return true;
        }
        if (keyword == "PLOT") {
            turtle_.draw_point(0, 0, turtle_.pen_colour());
            return true;
        }
        return false;
    }

    Interpreter& interpreter_;
    TurtleHost& turtle_;
    ComalState& state_;
};

}  // namespace

std::string execute_comal(Interpreter& interpreter, std::string_view source, TurtleHost& turtle) {
    ComalState& state = interpreter.executor_state<ComalState>();
    ComalExecutor executor{interpreter, turtle, state};
    // `PRINT DOUBLE(21)` calls a FUNC from inside an expression, so the
    // evaluator needs a resolver while this program runs and no longer after.
    expr::FunctionHook previous = std::move(interpreter.function_hook);
    interpreter.function_hook = [&executor](std::string_view name) {
        return executor.user_function(name);
    };
    try {
        std::string result = executor.execute(source);
        interpreter.function_hook = std::move(previous);
        return result;
    } catch (...) {
        interpreter.function_hook = std::move(previous);
        throw;
    }
}

}  // namespace timewarp::lang
