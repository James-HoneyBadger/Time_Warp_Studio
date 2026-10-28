// SPDX-License-Identifier: MIT
//
// PILOT, ported from `time_warp/languages/pilot.py`.
//
// The unit letter decides everything, and the optional `Y`/`N` suffix is a
// condition on the previous `M:`. Two things are deliberately *not* carried
// over: the reference falls back to the Logo executor for a line it does not
// recognise as a unit, and `G: STAMP` reads the turtle's pose. Neither is used
// by the corpus, and both would need the GUI's turtle mirror. They report
// themselves rather than doing something plausible and wrong.

#include "timewarp/lang/pilot.hpp"

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <map>
#include <memory>
#include <optional>
#include <regex>
#include <stdexcept>
#include <string>
#include <string_view>
#include <thread>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace {

struct PilotState {
    // `shared_ptr`, not `unique_ptr`: `executor_state` stores the state in an
    // `std::any`, which requires the type to be copy constructible.
    std::map<std::string, std::shared_ptr<std::fstream>> files;
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

std::string lower(std::string_view text) {
    std::string result{text};
    for (char& character : result) {
        character = static_cast<char>(std::tolower(static_cast<unsigned char>(character)));
    }
    return result;
}

bool starts_with(std::string_view text, std::string_view prefix) {
    return text.rfind(prefix, 0) == 0;
}

std::optional<std::string> validate_variable_name(const std::string& name, bool allow_suffix) {
    static const std::vector<std::string> kReserved = {
        "FOR",  "NEXT", "WHILE", "WEND", "IF",    "THEN", "ELSE", "ENDIF",
        "SELECT", "CASE", "END",  "SUB",  "FUNCTION", "RETURN", "GOSUB", "GOTO",
        "PRINT", "INPUT", "READ", "DATA", "DIM",   "LET",  "REM",  "LOCATE",
        "CLS",  "STOP"};
    if (name.empty()) {
        return "Variable name cannot be empty";
    }
    if (std::find(kReserved.begin(), kReserved.end(), upper(name)) != kReserved.end()) {
        return "'" + name + "' is a reserved keyword and cannot be used as variable name";
    }
    if (!(std::isalpha(static_cast<unsigned char>(name[0])) != 0 || name[0] == '_')) {
        return "Variable name must start with letter or underscore, got '" + name + "'";
    }
    for (std::size_t index = 0; index < name.size(); ++index) {
        const char character = name[index];
        const bool last = index + 1 == name.size();
        const bool suffix = allow_suffix && last &&
                            (character == '%' || character == '$' || character == '!' ||
                             character == '#');
        if (!(std::isalnum(static_cast<unsigned char>(character)) != 0 || character == '_' ||
              suffix)) {
            return std::string{"Invalid character '"} + character + "' in variable name '" +
                   name + "'";
        }
    }
    return std::nullopt;
}

// ---------------------------------------------------------------------------
// Substitution and string functions
// ---------------------------------------------------------------------------

std::string substitute_variables(Interpreter& interpreter, std::string_view text) {
    std::string result{text};
    const auto lookup = [&interpreter](const std::string& name) -> std::optional<std::string> {
        const auto string_exact = interpreter.variables().find(name + "$");
        if (string_exact != interpreter.variables().end() && string_exact->second.is_text()) {
            return string_exact->second.text();
        }
        const std::string upper_name = upper(name);
        const auto string_upper = interpreter.variables().find(upper_name + "$");
        if (string_upper != interpreter.variables().end() && string_upper->second.is_text()) {
            return string_upper->second.text();
        }
        const auto numeric = interpreter.variables().find(name);
        if (numeric != interpreter.variables().end()) {
            return numeric->second.render();
        }
        const auto numeric_upper = interpreter.variables().find(upper_name);
        if (numeric_upper != interpreter.variables().end()) {
            return numeric_upper->second.render();
        }
        return std::nullopt;
    };
    for (std::size_t index = 0; index < result.size();) {
        if (result[index] != '$' && result[index] != '#') {
            ++index;
            continue;
        }
        std::size_t end = index + 1;
        if (end < result.size() &&
            (std::isalpha(static_cast<unsigned char>(result[end])) != 0 || result[end] == '_')) {
            ++end;
            while (end < result.size() &&
                   (std::isalnum(static_cast<unsigned char>(result[end])) != 0 ||
                    result[end] == '_')) {
                ++end;
            }
            const std::string name = result.substr(index + 1, end - index - 1);
            if (const auto value = lookup(name); value.has_value()) {
                result.replace(index, end - index, *value);
                index += value->size();
                continue;
            }
        }
        ++index;
    }
    return result;
}

std::vector<std::string> split_function_args(std::string_view raw) {
    std::vector<std::string> args;
    std::string current;
    char quote = '\0';
    int depth = 0;
    for (char character : raw) {
        if ((character == '"' || character == '\'') && quote == '\0') {
            quote = character;
            current.push_back(character);
        } else if (character == quote) {
            quote = '\0';
            current.push_back(character);
        } else if (character == '(' && quote == '\0') {
            ++depth;
            current.push_back(character);
        } else if (character == ')' && quote == '\0') {
            --depth;
            current.push_back(character);
        } else if (character == ',' && quote == '\0' && depth == 0) {
            args.push_back(current);
            current.clear();
        } else {
            current.push_back(character);
        }
    }
    if (!current.empty()) {
        args.push_back(current);
    }
    return args;
}

std::string resolve_arg(Interpreter& interpreter, std::string_view raw) {
    const std::string arg = trim(raw);
    if (arg.size() >= 2 && ((arg.front() == '"' && arg.back() == '"') ||
                            (arg.front() == '\'' && arg.back() == '\''))) {
        return arg.substr(1, arg.size() - 2);
    }
    const std::string name = upper(arg);
    const auto as_string = interpreter.variables().find(name + "$");
    if (as_string != interpreter.variables().end() && as_string->second.is_text()) {
        return as_string->second.text();
    }
    const auto plain = interpreter.variables().find(name);
    if (plain != interpreter.variables().end()) {
        return plain->second.render();
    }
    return arg;
}

std::optional<long long> resolve_int(Interpreter& interpreter, std::string_view raw) {
    const std::string arg = trim(raw);
    const auto found = interpreter.variables().find(upper(arg));
    if (found != interpreter.variables().end() && found->second.is_number()) {
        return static_cast<long long>(found->second.number());
    }
    try {
        std::size_t used = 0;
        const long long value = std::stoll(arg, &used);
        if (used != arg.size()) {
            return std::nullopt;
        }
        return value;
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

std::string apply_string_function(Interpreter& interpreter, const std::string& name,
                                  std::string_view raw_args) {
    const std::vector<std::string> args = split_function_args(raw_args);
    if (args.empty()) {
        return {};
    }
    if (name == "LEN") {
        return std::to_string(resolve_arg(interpreter, args[0]).size());
    }
    if (name == "UPPER") {
        return upper(resolve_arg(interpreter, args[0]));
    }
    if (name == "LOWER") {
        return lower(resolve_arg(interpreter, args[0]));
    }
    if (name == "TRIM") {
        return trim(resolve_arg(interpreter, args[0]));
    }
    if (name == "REVERSE") {
        std::string value = resolve_arg(interpreter, args[0]);
        std::reverse(value.begin(), value.end());
        return value;
    }
    if ((name == "LEFT" || name == "RIGHT") && args.size() >= 2) {
        const std::string value = resolve_arg(interpreter, args[0]);
        const std::optional<long long> count = resolve_int(interpreter, args[1]);
        if (!count.has_value()) {
            return {};
        }
        const long long size = static_cast<long long>(value.size());
        if (name == "LEFT") {
            if (*count >= 0) {
                return value.substr(0, static_cast<std::size_t>(std::min(*count, size)));
            }
            return value.substr(0, static_cast<std::size_t>(std::max(0LL, size + *count)));
        }
        if (*count <= 0) {
            return "";
        }
        return value.substr(static_cast<std::size_t>(std::max(0LL, size - *count)));
    }
    if (name == "MID" && args.size() >= 2) {
        const std::string value = resolve_arg(interpreter, args[0]);
        const std::optional<long long> start = resolve_int(interpreter, args[1]);
        std::optional<long long> length;
        if (args.size() > 2) {
            length = resolve_int(interpreter, args[2]);
        }
        if (!start.has_value()) {
            return {};
        }
        const long long size = static_cast<long long>(value.size());
        long long begin = *start;
        if (begin < 0) {
            begin = std::max(0LL, size + begin);
        }
        if (begin >= size) {
            return "";
        }
        const long long count = length.value_or(size);
        if (count < 0) {
            return "";
        }
        return value.substr(static_cast<std::size_t>(begin),
                            static_cast<std::size_t>(std::min(count, size - begin)));
    }
    if (name == "CONCAT" && args.size() >= 2) {
        return resolve_arg(interpreter, args[0]) + resolve_arg(interpreter, args[1]);
    }
    if (name == "REPLACE" && args.size() >= 3) {
        std::string value = resolve_arg(interpreter, args[0]);
        const std::string old = resolve_arg(interpreter, args[1]);
        const std::string replacement = resolve_arg(interpreter, args[2]);
        if (old.empty()) {
            return value;
        }
        std::size_t at = 0;
        while ((at = value.find(old, at)) != std::string::npos) {
            value.replace(at, old.size(), replacement);
            at += replacement.size();
        }
        return value;
    }
    return {};
}

std::string eval_string_functions(Interpreter& interpreter, std::string_view expression) {
    static const std::vector<std::string> kNames = {"LEN",    "UPPER",  "LOWER", "TRIM",
                                                    "REVERSE", "LEFT",  "RIGHT", "MID",
                                                    "CONCAT", "REPLACE"};
    std::string result{expression};
    std::size_t scan = 0;
    while (scan < result.size()) {
        std::size_t best = std::string::npos;
        std::string best_name;
        std::size_t name_end = 0;
        for (const std::string& name : kNames) {
            std::size_t at = scan;
            for (;;) {
                at = result.find(name, at);
                if (at == std::string::npos) {
                    break;
                }
                const bool boundary =
                    at == 0 || !(std::isalnum(static_cast<unsigned char>(result[at - 1])) != 0 ||
                                 result[at - 1] == '_');
                if (boundary) {
                    break;
                }
                at += name.size();
            }
            if (at == std::string::npos) {
                continue;
            }
            std::size_t cursor = at + name.size();
            while (cursor < result.size() && is_space(result[cursor])) {
                ++cursor;
            }
            if (cursor >= result.size() || result[cursor] != '(') {
                continue;
            }
            if (best == std::string::npos || at < best ||
                (at == best && name.size() > best_name.size())) {
                best = at;
                best_name = name;
                name_end = cursor + 1;
            }
        }
        if (best == std::string::npos) {
            break;
        }
        const std::size_t close = result.find(')', name_end);
        if (close == std::string::npos) {
            break;
        }
        const std::string replaced =
            apply_string_function(interpreter, best_name, std::string_view{result}.substr(
                                                               name_end, close - name_end));
        // An empty result is also what an unsupported argument list produces,
        // so only replace when the function actually returned something.
        if (!replaced.empty()) {
            result.replace(best, close - best + 1, replaced);
            scan = best + replaced.size();
        } else {
            scan = close + 1;
        }
    }
    return result;
}

// ---------------------------------------------------------------------------
// Matching
// ---------------------------------------------------------------------------

bool wildcard_match(std::string_view pattern, std::string_view text) {
    std::string regex_text = "^";
    for (char character : pattern) {
        if (character == '*') {
            regex_text += ".*";
        } else if (character == '?') {
            regex_text += ".";
        } else if (std::string_view{R"(\.^$|()[]{}+)"}.find(character) !=
                   std::string_view::npos) {
            regex_text += '\\';
            regex_text += character;
        } else {
            regex_text += character;
        }
    }
    regex_text += "$";
    try {
        return std::regex_match(std::string{text}, std::regex{regex_text,
                                                              std::regex::ECMAScript |
                                                                  std::regex::icase});
    } catch (const std::regex_error&) {
        return false;
    }
}

void set_match_vars(Interpreter& interpreter, const std::string& input, std::size_t position,
                    std::size_t length) {
    interpreter.set_text("LEFT$", input.substr(0, position));
    interpreter.set_text("MATCH$", input.substr(position, length));
    interpreter.set_text("RIGHT$", position + length <= input.size()
                                        ? input.substr(position + length)
                                        : std::string{});
}

// ---------------------------------------------------------------------------
// Graphics
// ---------------------------------------------------------------------------

std::optional<double> eval_argument(Interpreter& interpreter, std::string_view text,
                                    std::string& error, std::string_view message) {
    try {
        return interpreter.evaluate_expression(text);
    } catch (const std::exception&) {
        error = "❌ G: " + std::string{message} + "\n";
        return std::nullopt;
    }
}

std::vector<std::string> split_arguments(std::string_view text) {
    std::vector<std::string> parts;
    std::string current;
    const bool commas = text.find(',') != std::string_view::npos;
    for (char character : text) {
        if ((commas && character == ',') || (!commas && is_space(character))) {
            if (!trim(current).empty()) {
                parts.push_back(trim(current));
                current.clear();
            }
        } else {
            current.push_back(character);
        }
    }
    if (!trim(current).empty()) {
        parts.push_back(trim(current));
    }
    return parts;
}

bool parse_colour(Interpreter& interpreter, const std::vector<std::string>& args, Rgb& colour) {
    if (args.size() < 3) {
        return false;
    }
    try {
        colour.red = static_cast<int>(interpreter.evaluate_expression(args[0]));
        colour.green = static_cast<int>(interpreter.evaluate_expression(args[1]));
        colour.blue = static_cast<int>(interpreter.evaluate_expression(args[2]));
        return true;
    } catch (const std::exception&) {
        return false;
    }
}

std::string graphics_command(Interpreter& interpreter, TurtleHost& turtle, std::string_view text) {
    const std::string command = trim(text);
    const std::size_t space = command.find(' ');
    const std::string head = upper(command.substr(0, space));
    const std::string arguments = space == std::string::npos ? "" : trim(command.substr(space + 1));

    std::string error;
    const auto distance = [&](std::string_view message) -> std::optional<double> {
        return eval_argument(interpreter, arguments, error, message);
    };

    if (head == "FORWARD" || head == "FD") {
        if (arguments.empty()) {
            return "❌ G: FORWARD requires distance\n";
        }
        if (const auto value = distance("Invalid distance"); value.has_value()) {
            turtle.forward(*value);
        }
        return error;
    }
    if (head == "BACK" || head == "BK") {
        if (arguments.empty()) {
            return "❌ G: BACK requires distance\n";
        }
        if (const auto value = distance("Invalid distance"); value.has_value()) {
            turtle.back(*value);
        }
        return error;
    }
    if (head == "LEFT" || head == "LT") {
        if (arguments.empty()) {
            return "❌ G: LEFT requires angle\n";
        }
        if (const auto value = distance("Invalid angle"); value.has_value()) {
            turtle.left(*value);
        }
        return error;
    }
    if (head == "RIGHT" || head == "RT") {
        if (arguments.empty()) {
            return "❌ G: RIGHT requires angle\n";
        }
        if (const auto value = distance("Invalid angle"); value.has_value()) {
            turtle.right(*value);
        }
        return error;
    }
    if (head == "PENUP" || head == "PU") {
        turtle.pen_up();
        return {};
    }
    if (head == "PENDOWN" || head == "PD") {
        turtle.pen_down();
        return {};
    }
    if (head == "HOME") {
        turtle.home();
        return {};
    }
    if (head == "CLEAR" || head == "CS") {
        turtle.clear();
        return {};
    }
    if (head == "SETXY") {
        const std::vector<std::string> parts = split_arguments(arguments);
        if (parts.size() < 2) {
            return "❌ G: SETXY requires x,y coordinates\n";
        }
        try {
            const double x = interpreter.evaluate_expression(parts[0]);
            const double y = interpreter.evaluate_expression(parts[1]);
            turtle.goto_xy(x, y);
        } catch (const std::exception&) {
            return "❌ G: Invalid coordinates\n";
        }
        return {};
    }
    if (head == "CIRCLE") {
        if (arguments.empty()) {
            return "❌ G: CIRCLE requires radius\n";
        }
        if (const auto value = distance("Invalid radius"); value.has_value()) {
            turtle.circle(*value);
        }
        return error;
    }
    if (head == "SETBGCOLOR" || head == "SETPENCOLOR") {
        const std::vector<std::string> parts = split_arguments(arguments);
        const char* name = head == "SETBGCOLOR" ? "SETBGCOLOR" : "SETPENCOLOR";
        if (parts.size() < 3) {
            return std::string{"❌ G: "} + name + " requires r, g, b\n";
        }
        Rgb colour;
        if (!parse_colour(interpreter, parts, colour)) {
            return std::string{"❌ G: Invalid color values\n"};
        }
        if (head == "SETBGCOLOR") {
            turtle.set_background(colour);
        } else {
            turtle.set_color(colour);
        }
        return {};
    }
    if (head == "SETPENWIDTH") {
        if (arguments.empty()) {
            return "❌ G: SETPENWIDTH requires width\n";
        }
        if (const auto value = distance("Invalid width"); value.has_value()) {
            turtle.set_pen_width(*value);
        }
        return error;
    }
    if (head == "SETHEADING" || head == "SETH") {
        if (arguments.empty()) {
            return "❌ G: SETHEADING requires angle\n";
        }
        if (const auto value = distance("Invalid angle"); value.has_value()) {
            turtle.set_heading(*value);
        }
        return error;
    }
    if (head == "HIDETURTLE" || head == "HT") {
        turtle.hide_turtle();
        return {};
    }
    if (head == "SHOWTURTLE" || head == "ST") {
        turtle.show_turtle();
        return {};
    }
    if (head == "ARC") {
        const std::vector<std::string> parts = split_arguments(arguments);
        if (parts.size() < 2) {
            return "❌ G: ARC requires radius and extent\n";
        }
        try {
            const double radius = interpreter.evaluate_expression(parts[0]);
            const double extent = interpreter.evaluate_expression(parts[1]);
            turtle.circle(radius, extent);
        } catch (const std::exception&) {
            return "❌ G: Invalid ARC parameters\n";
        }
        return {};
    }
    if (head == "FILL") {
        turtle.fill_last_shape(turtle.pen_colour());
        return {};
    }
    if (head == "DOT") {
        int radius = 3;
        if (!arguments.empty()) {
            try {
                radius = std::max(1, static_cast<int>(interpreter.evaluate_expression(arguments)));
            } catch (const std::exception&) {
                radius = 3;
            }
        }
        turtle.circle(radius);
        return {};
    }
    if (head == "STAMP") {
        return "❌ G: STAMP is not supported by the native engine\n";
    }
    if (head == "TEXT") {
        if (!arguments.empty()) {
            return "📝 " + substitute_variables(interpreter, arguments) + "\n";
        }
        return {};
    }
    if (head == "SPEED") {
        if (!arguments.empty()) {
            try {
                const double speed = std::clamp(interpreter.evaluate_expression(arguments), 0.0, 10.0);
                interpreter.set_number("_TURTLE_SPEED", std::trunc(speed));
            } catch (const std::exception&) {
                return "❌ G: Invalid speed\n";
            }
        }
        return {};
    }
    return "❌ Unknown graphics command: " + head + "\n";
}

// ---------------------------------------------------------------------------
// File operations
// ---------------------------------------------------------------------------

std::optional<std::string> validate_file_path(const std::string& filename) {
    if (filename.empty()) {
        return "File name cannot be empty";
    }
    if (filename.find("..") != std::string::npos) {
        return "Path traversal not allowed: '" + filename + "'";
    }
    if (filename.front() == '/' || filename.front() == '~') {
        return "Absolute paths not allowed: '" + filename + "'";
    }
    return std::nullopt;
}

std::string file_command(Interpreter& interpreter, PilotState& state, std::string_view text) {
    std::vector<std::string> parts;
    std::string current;
    for (char character : text) {
        if (is_space(character)) {
            if (!current.empty()) {
                parts.push_back(current);
                current.clear();
            }
        } else {
            current.push_back(character);
        }
    }
    if (!current.empty()) {
        parts.push_back(current);
    }
    if (parts.empty()) {
        return "❌ F: requires file operation\n";
    }
    const std::string operation = upper(parts[0]);
    const auto count_error = [&parts](int expected, const std::string& name) -> std::string {
        const int got = static_cast<int>(parts.size()) - 1;
        if (got != expected) {
            return name + " expects " + std::to_string(expected) + " argument(s), got " +
                   std::to_string(got);
        }
        return {};
    };

    if (operation == "OPEN") {
        if (const std::string message = count_error(2, "F: OPEN"); !message.empty()) {
            return "❌ " + message + "\n";
        }
        const std::string filename = parts[1];
        if (const auto invalid = validate_file_path(filename); invalid.has_value()) {
            return "❌ " + *invalid + "\n";
        }
        const std::string mode = upper(parts[2]);
        if (mode != "READ" && mode != "R" && mode != "WRITE" && mode != "W" &&
            mode != "APPEND" && mode != "A") {
            return "❌ F: OPEN requires mode R|W|A, got: " + mode + "\n";
        }
        std::ios::openmode flags = std::ios::in;
        if (mode == "WRITE" || mode == "W") {
            flags = std::ios::out | std::ios::trunc;
        } else if (mode == "APPEND" || mode == "A") {
            flags = std::ios::out | std::ios::app;
        }
        auto existing = state.files.find(filename);
        if (existing != state.files.end()) {
            existing->second->close();
            state.files.erase(existing);
        }
        auto stream = std::make_shared<std::fstream>(filename, flags);
        if (!stream->is_open()) {
            return "❌ F: Error opening file '" + filename + "': cannot open\n";
        }
        state.files[filename] = std::move(stream);
        return {};
    }
    if (operation == "CLOSE") {
        if (const std::string message = count_error(1, "F: CLOSE"); !message.empty()) {
            return "❌ " + message + "\n";
        }
        auto found = state.files.find(parts[1]);
        if (found == state.files.end()) {
            return "❌ F: File not open: " + parts[1] + "\n";
        }
        found->second->close();
        state.files.erase(found);
        return {};
    }
    if (operation == "READ") {
        if (const std::string message = count_error(2, "F: READ"); !message.empty()) {
            return "❌ " + message + "\n";
        }
        const std::string& filename = parts[1];
        const std::string& variable = parts[2];
        if (const auto invalid = validate_variable_name(variable, true); invalid.has_value()) {
            return "❌ " + *invalid + "\n";
        }
        auto found = state.files.find(filename);
        if (found == state.files.end()) {
            return "❌ F: File not open: " + filename + "\n";
        }
        std::string line;
        std::getline(*found->second, line);
        line = trim(line);
        char* end = nullptr;
        const double number = std::strtod(line.c_str(), &end);
        if (end != line.c_str() && *end == '\0') {
            interpreter.set_number(variable, number);
        } else {
            interpreter.set_text(variable, line);
        }
        return {};
    }
    if (operation == "WRITE") {
        if (const std::string message = count_error(2, "F: WRITE"); !message.empty()) {
            return "❌ " + message + "\n";
        }
        const std::string& filename = parts[1];
        auto found = state.files.find(filename);
        if (found == state.files.end()) {
            return "❌ F: File not open: " + filename + "\n";
        }
        std::string data;
        for (std::size_t index = 2; index < parts.size(); ++index) {
            if (!data.empty()) {
                data.push_back(' ');
            }
            data += parts[index];
        }
        *found->second << data << '\n';
        return {};
    }
    return "❌ Unknown file operation: " + operation + "\n";
}

// ---------------------------------------------------------------------------
// The unit dispatcher
// ---------------------------------------------------------------------------

std::string execute_unit(Interpreter& interpreter, TurtleHost& turtle, PilotState& state,
                         std::string_view source);

std::optional<std::string> long_form(Interpreter& interpreter, TurtleHost& turtle,
                                     PilotState& state, const std::string& command) {
    static const std::vector<std::pair<std::string, char>> kLongForms = {
        {"PRINT", 'T'},   {"TYPE", 'T'},   {"ACCEPT", 'A'}, {"COMPUTE", 'C'},
        {"MATCH", 'M'},   {"JUMP", 'J'},   {"USE", 'U'},    {"STOP", 'E'},
        {"END", 'E'},     {"PAUSE", 'P'},  {"GRAPHIC", 'G'}, {"FILE", 'F'},
        {"DELAY", 'D'},   {"HINT", 'H'},   {"NO", 'N'}};
    for (const auto& [keyword, letter] : kLongForms) {
        const bool matches = command == keyword || starts_with(command, keyword + " ") ||
                             starts_with(command, keyword + ":");
        if (!matches) {
            continue;
        }
        std::string rest = command.substr(keyword.size());
        std::size_t start = 0;
        while (start < rest.size() && (rest[start] == ' ' || rest[start] == ':')) {
            ++start;
        }
        rest = trim(std::string_view{rest}.substr(start));
        if (letter == 'C' && rest.find('=') == std::string::npos && rest.find(' ') != std::string::npos) {
            const std::size_t space = rest.find(' ');
            rest = rest.substr(0, space) + " = " + trim(std::string_view{rest}.substr(space));
        }
        if (letter == 'E') {
            interpreter.running = false;
            return std::string{};
        }
        return execute_unit(interpreter, turtle, state,
                            std::string(1, letter) + ":" + rest);
    }
    return std::nullopt;
}

std::string execute_unit(Interpreter& interpreter, TurtleHost& turtle, PilotState& state,
                         std::string_view source) {
    const std::string command = trim(source);
    if (command.size() < 2) {
        return {};
    }
    if (command.front() == '*' || starts_with(upper(command), "L:")) {
        return {};
    }
    const std::string command_upper = upper(command);
    if (starts_with(command_upper, "REMARK") || starts_with(command_upper, "REM ") ||
        command_upper == "REM") {
        return {};
    }
    if (command_upper == "LINK" || command_upper == "LOAD" ||
        starts_with(command_upper, "LINK ") || starts_with(command_upper, "LINK:") ||
        starts_with(command_upper, "LOAD ") || starts_with(command_upper, "LOAD:")) {
        const std::size_t colon = command_upper.find(':');
        const std::string word = trim(std::string_view{command_upper}.substr(
            0, colon == std::string::npos ? command_upper.size() : colon));
        const std::size_t space = word.find(' ');
        return "❌ " + word.substr(0, space) + ": lesson chaining is not implemented\n";
    }
    if (std::optional<std::string> substituted =
            long_form(interpreter, turtle, state, command);
        substituted.has_value()) {
        return *substituted;
    }
    if (starts_with(command_upper, "TU ")) {
        const std::string label = trim(std::string_view{command}.substr(3));
        if (!label.empty()) {
            interpreter.subroutine_stack.push_back(interpreter.current_line + 1);
            interpreter.push_call_frame(label, interpreter.display_line());
            interpreter.jump_to_label(label);
        }
        return {};
    }

    const std::size_t colon = command.find(':');
    if (colon == std::string::npos || colon < 1) {
        return "❌ Invalid PILOT command: " + command + "\n";
    }
    const std::string prefix = upper(std::string_view{command}.substr(0, colon));
    std::string rest = trim(std::string_view{command}.substr(colon + 1));
    std::string type;
    char condition = '\0';
    if (prefix.size() == 1) {
        type = prefix;
    } else if (prefix.size() == 2 && (prefix[1] == 'Y' || prefix[1] == 'N')) {
        type = prefix.substr(0, 1);
        condition = prefix[1];
    } else {
        return "❌ Invalid PILOT command: " + command + "\n";
    }
    if (condition == 'Y' && !interpreter.last_match_succeeded) {
        return {};
    }
    if (condition == 'N' && interpreter.last_match_succeeded) {
        return {};
    }

    if (type == "T") {
        rest = substitute_variables(interpreter, rest);
        std::size_t at = 0;
        while ((at = rest.find("\\t", at)) != std::string::npos) {
            rest.replace(at, 2, "\t");
            at += 1;
        }
        return interpreter.interpolate_text(rest) + "\n";
    }
    if (type == "A") {
        const std::string prompt_text = trim(rest);
        std::string variable = "ANSWER$";
        if (!prompt_text.empty() && prompt_text.front() == '$') {
            variable = upper(trim(std::string_view{prompt_text}.substr(1))) + "$";
        }
        const std::string display =
            (!prompt_text.empty() && prompt_text.front() != '$') ? prompt_text + " " : "? ";
        interpreter.request_input(display, variable, false);
        return {};
    }
    if (type == "M") {
        const std::string pattern = trim(rest);
        if (pattern.empty()) {
            interpreter.last_match_succeeded = false;
            return {};
        }
        std::string input = trim(interpreter.last_input());
        const std::string input_upper = upper(input);
        interpreter.last_match_succeeded = false;
        std::vector<std::string> alternatives;
        std::string current;
        for (char character : pattern) {
            if (character == ',') {
                alternatives.push_back(trim(current));
                current.clear();
            } else {
                current.push_back(character);
            }
        }
        alternatives.push_back(trim(current));
        for (const std::string& alternative : alternatives) {
            const std::string alt_upper = upper(alternative);
            if (alternative.find('*') != std::string::npos ||
                alternative.find('?') != std::string::npos) {
                if (wildcard_match(alt_upper, input_upper)) {
                    interpreter.last_match_succeeded = true;
                    set_match_vars(interpreter, input, 0, input.size());
                    break;
                }
            } else if (input_upper == alt_upper) {
                interpreter.last_match_succeeded = true;
                set_match_vars(interpreter, input, 0, input.size());
                break;
            } else {
                const std::size_t position = input_upper.find(alt_upper);
                if (position != std::string::npos) {
                    interpreter.last_match_succeeded = true;
                    set_match_vars(interpreter, input, position, alternative.size());
                    break;
                }
            }
        }
        return {};
    }
    if (type == "Y") {
        if (interpreter.last_match_succeeded) {
            const std::string label = trim(rest);
            if (!label.empty()) {
                interpreter.jump_to_label(label);
            }
        }
        return {};
    }
    if (type == "C") {
        const std::size_t equals = rest.find('=');
        if (equals == std::string::npos) {
            return "❌ C: requires format: var = expression\n";
        }
        std::string name = trim(std::string_view{rest}.substr(0, equals));
        std::string expression = trim(std::string_view{rest}.substr(equals + 1));
        if (!name.empty() && name.front() == '#') {
            name = trim(std::string_view{name}.substr(1));
        } else if (!name.empty() && name.front() == '$') {
            name = trim(std::string_view{name}.substr(1)) + "$";
        }
        if (name.empty()) {
            return "❌ C: requires variable name\n";
        }
        if (const auto invalid = validate_variable_name(name, true); invalid.has_value()) {
            return "❌ " + *invalid + "\n";
        }
        expression = substitute_variables(interpreter, expression);
        expression = eval_string_functions(interpreter, expression);
        if (upper(name).back() == '$') {
            interpreter.set_text(upper(name), expression);
            return {};
        }
        try {
            interpreter.set_number(upper(name), interpreter.evaluate_expression(expression));
        } catch (const std::exception& error) {
            return "❌ Error in C: " + std::string{error.what()} + "\n";
        }
        return {};
    }
    if (type == "J") {
        const std::string label = trim(rest);
        if (!label.empty()) {
            interpreter.jump_to_label(label);
        }
        return {};
    }
    if (type == "U" && rest.find('=') != std::string::npos) {
        return execute_unit(interpreter, turtle, state, "C:" + rest);
    }
    if (type == "U" || type == "S") {
        const std::string label = trim(rest);
        if (label.empty()) {
            return "❌ " + type + ": requires a label, e.g. " + type + ":*SUB\n";
        }
        constexpr std::size_t kMaxDepth = 128;
        if (interpreter.subroutine_stack.size() >= kMaxDepth) {
            return "❌ " + type + ": Maximum subroutine depth (128) exceeded\n";
        }
        interpreter.subroutine_stack.push_back(interpreter.current_line + 1);
        interpreter.push_call_frame(label, interpreter.display_line());
        interpreter.jump_to_label(label);
        return {};
    }
    if (type == "P") {
        interpreter.request_input("Press Enter to continue...", "_PAUSE", true);
        return {};
    }
    if (type == "B") {
        const std::size_t equals = rest.find('=');
        if (equals == std::string::npos) {
            return "❌ B: requires format: condition=label\n";
        }
        const std::string test = trim(std::string_view{rest}.substr(0, equals));
        const std::string label = trim(std::string_view{rest}.substr(equals + 1));
        if (label.empty()) {
            return "❌ B: requires label\n";
        }
        try {
            if (interpreter.evaluate_expression(test) != 0.0) {
                interpreter.jump_to_label(label);
            }
        } catch (const std::exception& error) {
            return "❌ Error in B: condition: " + std::string{error.what()} + "\n";
        }
        return {};
    }
    if (type == "R") {
        return {};
    }
    if (type == "G") {
        return graphics_command(interpreter, turtle, rest);
    }
    if (type == "F") {
        return file_command(interpreter, state, rest);
    }
    if (type == "D") {
        try {
            const double delay = std::stod(trim(rest));
            const double capped = std::min(std::max(0.0, delay), 10.0);
            const auto deadline =
                std::chrono::steady_clock::now() +
                std::chrono::milliseconds(static_cast<long long>(capped * 1000.0));
            while (std::chrono::steady_clock::now() < deadline) {
                if (!interpreter.check_budget()) {
                    break;
                }
                std::this_thread::sleep_for(std::chrono::milliseconds{10});
            }
        } catch (const std::exception&) {
            return "❌ D: requires numeric delay in seconds\n";
        }
        return {};
    }
    if (type == "H") {
        const std::string hint = trim(rest);
        if (!hint.empty()) {
            return "💡 Hint: " +
                   interpreter.interpolate_text(substitute_variables(interpreter, hint)) + "\n";
        }
        return {};
    }
    if (type == "N") {
        if (!interpreter.last_match_succeeded) {
            return execute_unit(interpreter, turtle, state, "T:" + rest);
        }
        return {};
    }
    if (type == "E") {
        if (!interpreter.subroutine_stack.empty()) {
            interpreter.jump_to(interpreter.subroutine_stack.back());
            interpreter.subroutine_stack.pop_back();
            interpreter.pop_call_frame();
            return {};
        }
        interpreter.running = false;
        return {};
    }
    return "❌ Unknown PILOT command: " + type + ":\n";
}

}  // namespace

std::string execute_pilot(Interpreter& interpreter, std::string_view source, TurtleHost& turtle) {
    PilotState& state = interpreter.executor_state<PilotState>();
    return execute_unit(interpreter, turtle, state, source);
}

}  // namespace timewarp::lang
