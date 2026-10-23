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

std::string Machine::sub_definition(const std::string& args) {
    const std::string header = trim(args);
    if (header.empty()) {
        return "❌ SUB requires a name\n";
    }
    const std::string name = upper(trim(header.substr(0, header.find('('))));
    const auto definition = state_.subs.find(name);
    if (definition != state_.subs.end()) {
        interpreter_.jump_to(definition->second.end);
        return {};
    }
    const auto& lines = interpreter_.program_lines();
    for (std::size_t scan = interpreter_.current_line + 1; scan < lines.size(); ++scan) {
        if (upper(trim(lines[scan].text)) == "END SUB") {
            interpreter_.jump_to(scan);
            break;
        }
    }
    return {};
}

std::string Machine::end_sub() {
    if (state_.calls.empty()) {
        return {};
    }
    CallFrame frame = std::move(state_.calls.back());
    state_.calls.pop_back();
    for (const auto& [name, value] : frame.saved) {
        if (value.has_value()) {
            interpreter_.variables()[name] = *value;
        } else {
            interpreter_.remove_variable(name);
        }
    }
    interpreter_.jump_to(frame.return_line + 1);
    return {};
}

std::string Machine::function_definition(const std::string& args) {
    const std::string header = trim(args);
    if (header.empty()) {
        return "❌ FUNCTION requires a name\n";
    }
    const std::string name = upper(trim(header.substr(0, header.find('('))));
    const auto definition = state_.functions.find(name);
    if (definition != state_.functions.end()) {
        interpreter_.jump_to(definition->second.end);
        return {};
    }
    const auto& lines = interpreter_.program_lines();
    for (std::size_t scan = interpreter_.current_line + 1; scan < lines.size(); ++scan) {
        if (upper(trim(lines[scan].text)) == "END FUNCTION") {
            interpreter_.jump_to(scan);
            break;
        }
    }
    return {};
}

/// The line index of the terminator that closes the block opened at `from`, counting
/// nesting of the same kind. A `FOR` closes at its `NEXT`, a `DO` at its `LOOP` and a
/// `WHILE` at its `WEND`.
std::optional<std::size_t> matching_terminator(Interpreter& interpreter, std::size_t from,
                                               const std::string& opener,
                                               const std::string& closer) {
    const auto& lines = interpreter.program_lines();
    int depth = 1;
    for (std::size_t index = from + 1; index < lines.size(); ++index) {
        const std::string code = upper(trim(lines[index].text));
        if (code.rfind(opener + " ", 0) == 0 || code == opener) {
            ++depth;
        } else if (code.rfind(closer, 0) == 0) {
            --depth;
            if (depth == 0) {
                return index;
            }
        }
    }
    return std::nullopt;
}

std::string Machine::exit_loop(const std::string& kind) {
    const std::string opener = kind;
    const std::string closer = kind == "FOR" ? "NEXT" : (kind == "DO" ? "LOOP" : "WEND");
    if (kind == "FOR") {
        if (state_.fors.empty()) {
            return "❌ EXIT FOR outside a FOR\n";
        }
        state_.fors.pop_back();
    } else if (kind == "DO") {
        if (state_.dos.empty()) {
            return "❌ EXIT DO outside a DO\n";
        }
        state_.dos.pop_back();
    } else {
        if (state_.whiles.empty()) {
            return "❌ EXIT WHILE outside a WHILE\n";
        }
        state_.whiles.pop_back();
    }
    const std::optional<std::size_t> end =
        matching_terminator(interpreter_, interpreter_.current_line, opener, closer);
    if (!end.has_value()) {
        return "❌ " + closer + " is missing for this " + kind + "\n";
    }
    interpreter_.jump_to(*end + 1);
    return {};
}

std::string Machine::end_function() {
    if (state_.calls.empty()) {
        return {};
    }
    CallFrame frame = std::move(state_.calls.back());
    state_.calls.pop_back();
    // The answer is whatever the variable named after the function holds -- text for
    // `FUNCTION GRADE$`, a number otherwise. Coercing it to a number here is what made
    // a `$` function answer 0: the text was read as a double and written back as one.
    Value result{0.0};
    if (!frame.function_name.empty()) {
        if (const auto found = interpreter_.variables().find(frame.function_name);
            found != interpreter_.variables().end()) {
            result = found->second;
        }
    }
    for (const auto& [name, value] : frame.saved) {
        if (value.has_value()) {
            interpreter_.variables()[name] = *value;
        } else {
            interpreter_.remove_variable(name);
        }
    }
    if (!frame.function_name.empty()) {
        interpreter_.variables()[frame.function_name] = result;
    }
    interpreter_.jump_to(frame.return_line + 1);
    return {};
}

std::string Machine::call(const std::string& args) {
    const std::string header = trim(args);
    if (header.empty()) {
        return "❌ CALL requires subroutine name\n";
    }
    std::string name = header;
    std::vector<std::string> arguments;
    const std::size_t open = header.find('(');
    if (open != std::string::npos) {
        name = upper(trim(header.substr(0, open)));
        std::string rest = header.substr(open + 1);
        if (!rest.empty() && rest.back() == ')') {
            rest.pop_back();
        }
        for (const std::string& field : split_plain(rest, ',')) {
            if (!trim(field).empty()) {
                arguments.push_back(trim(field));
            }
        }
    } else {
        name = upper(header);
    }

    bool is_function = false;
    auto definition = state_.subs.find(name);
    if (definition == state_.subs.end()) {
        definition = state_.functions.find(name);
        is_function = true;
    }
    if (definition == state_.functions.end() && definition == state_.subs.end()) {
        return "❌ Subroutine not found: " + name + "\n";
    }

    std::vector<Value> evaluated;
    for (const std::string& argument : arguments) {
        try {
            evaluated.emplace_back(eval(argument));
        } catch (const std::exception&) {
            evaluated.emplace_back(strip_quotes(argument));
        }
    }

    CallFrame frame;
    frame.return_line = interpreter_.current_line;
    frame.function_name = is_function ? name : std::string{};
    for (std::size_t index = 0; index < definition->second.parameters.size(); ++index) {
        const std::string& parameter = definition->second.parameters[index];
        const auto existing = interpreter_.variables().find(parameter);
        frame.saved.emplace_back(parameter, existing == interpreter_.variables().end()
                                                 ? std::nullopt
                                                 : std::optional<Value>{existing->second});
        if (!parameter.empty() && parameter.back() == '$') {
            assign(parameter, index < evaluated.size() ? evaluated[index] : Value{""});
        } else {
            assign(parameter, index < evaluated.size() ? evaluated[index] : Value{0.0});
        }
    }
    state_.calls.push_back(std::move(frame));
    interpreter_.jump_to(definition->second.start);
    return {};
}

}  // namespace timewarp::lang::basic
