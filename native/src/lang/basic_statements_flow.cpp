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

std::string Machine::goto_line(const std::string& args) {
    const std::string target = trim(args);
    if (target.empty()) {
        return "❌ GOTO requires line number\n";
    }
    const std::optional<long> line = parse_integer(target);
    if (!line.has_value()) {
        return "❌ Invalid line number: " + target + "\n";
    }
    const auto found = interpreter_.line_number_map.find(static_cast<int>(*line));
    if (found == interpreter_.line_number_map.end()) {
        return "❌ GOTO " + std::to_string(*line) + " failed: line not found\n";
    }
    interpreter_.jump_to(found->second);
    return {};
}

std::string Machine::gosub(const std::string& args) {
    const std::string target = trim(args);
    if (target.empty()) {
        return "❌ GOSUB requires line number\n";
    }
    double line = 0.0;
    if (!parse_float(target, line)) {
        return "❌ Expected numeric value for line number, got '" + target + "'\n";
    }
    const auto found = interpreter_.line_number_map.find(static_cast<int>(line));
    if (found == interpreter_.line_number_map.end()) {
        throw ExecutionError("Line number " + std::to_string(static_cast<int>(line)) +
                             " not found");
    }
    state_.gosubs.push_back(interpreter_.current_line);
    interpreter_.jump_to(found->second);
    return {};
}

std::string Machine::return_from_sub() {
    if (state_.gosubs.empty()) {
        return "❌ RETURN without GOSUB\n";
    }
    interpreter_.jump_to(state_.gosubs.back() + 1);
    state_.gosubs.pop_back();
    return {};
}

std::string Machine::for_loop(const std::string& args) {
    static const std::regex pattern{
        R"((\w+)\s*=\s*(.+?)\s+TO\s+(.+?)(?:\s+STEP\s+(.+))?$)", std::regex::ECMAScript};
    std::smatch match;
    const std::string text = upper(args);
    if (!std::regex_match(text, match, pattern)) {
        return "❌ FOR requires format: var = start TO end [STEP step]\n";
    }
    const std::string var_name = match[1].str();
    try {
        validate_name(var_name);
    } catch (const BasicError& error) {
        return "❌ " + std::string{error.what()} + "\n";
    }
    try {
        const double start = eval(match[2].str());
        const double limit = eval(match[3].str());
        const double step = match[4].matched ? eval(match[4].str()) : 1.0;
        if (step == 0.0) {
            return "❌ FOR STEP cannot be 0\n";
        }
        assign(var_name, start);
        state_.fors.push_back(ForFrame{var_name, limit, step, interpreter_.current_line});
    } catch (const std::exception& error) {
        return "❌ Error in FOR: " + std::string{error.what()} + "\n";
    }
    return {};
}

std::string Machine::next(const std::string& /*args*/) {
    if (state_.fors.empty()) {
        return "❌ NEXT without FOR\n";
    }
    ForFrame& frame = state_.fors.back();
    const double current = number_of(frame.variable).value_or(0.0);
    const double value = current + frame.step;
    assign(frame.variable, value);
    const bool again =
        frame.step > 0.0 ? value <= frame.limit : value >= frame.limit;
    if (again) {
        // An empty body puts this statement at `body + 1`, so the jump's target
        // is the line already executing -- which is exactly what `jump_to`
        // exists to make visible to the run loop.
        interpreter_.jump_to(frame.body + 1);
    } else {
        state_.fors.pop_back();
    }
    return {};
}

std::string Machine::while_loop(const std::string& args) {
    try {
        if (eval(trim(args)) != 0.0) {
            state_.whiles.push_back(interpreter_.current_line);
            return {};
        }
    } catch (const std::exception& error) {
        return "❌ WHILE syntax error: " + std::string{error.what()} + "\n";
    }
    const auto& lines = interpreter_.program_lines();
    int depth = 0;
    for (std::size_t scan = interpreter_.current_line + 1; scan < lines.size(); ++scan) {
        const std::string code = upper(trim(lines[scan].text));
        if (starts_with(code, "WHILE ")) {
            ++depth;
        } else if (code == "WEND") {
            if (depth == 0) {
                interpreter_.jump_to(scan + 1);
                return {};
            }
            --depth;
        }
    }
    return "❌ WHILE without WEND\n";
}

std::string Machine::wend() {
    if (state_.whiles.empty()) {
        return "❌ WEND without WHILE\n";
    }
    interpreter_.jump_to(state_.whiles.back());
    state_.whiles.pop_back();
    return {};
}

std::string Machine::do_loop(const std::string& args) {
    const std::string condition = upper(trim(args));
    bool should_run = true;
    if (starts_with(condition, "WHILE ")) {
        try {
            should_run = eval(trim(condition.substr(6))) != 0.0;
        } catch (const std::exception&) {
            return "❌ Invalid DO WHILE condition\n";
        }
    } else if (starts_with(condition, "UNTIL ")) {
        try {
            should_run = eval(trim(condition.substr(6))) == 0.0;
        } catch (const std::exception&) {
            return "❌ Invalid DO UNTIL condition\n";
        }
    }
    if (should_run) {
        state_.dos.push_back(interpreter_.current_line);
        return {};
    }
    const auto& lines = interpreter_.program_lines();
    int depth = 0;
    for (std::size_t scan = interpreter_.current_line + 1; scan < lines.size(); ++scan) {
        const std::string code = upper(trim(lines[scan].text));
        if (starts_with(code, "DO")) {
            ++depth;
        } else if (starts_with(code, "LOOP")) {
            if (depth == 0) {
                interpreter_.jump_to(scan + 1);
                return {};
            }
            --depth;
        }
    }
    return "❌ DO without LOOP\n";
}

std::string Machine::loop(const std::string& args) {
    if (state_.dos.empty()) {
        return "❌ LOOP without DO\n";
    }
    const std::size_t start = state_.dos.back();
    state_.dos.pop_back();
    const std::string condition = upper(trim(args));
    bool should_loop = true;
    if (starts_with(condition, "WHILE ")) {
        try {
            should_loop = eval(trim(condition.substr(6))) != 0.0;
        } catch (const std::exception&) {
            return "❌ Invalid LOOP WHILE condition\n";
        }
    } else if (starts_with(condition, "UNTIL ")) {
        try {
            should_loop = eval(trim(condition.substr(6))) == 0.0;
        } catch (const std::exception&) {
            return "❌ Invalid LOOP UNTIL condition\n";
        }
    }
    if (should_loop) {
        interpreter_.jump_to(start);
    }
    return {};
}

std::string Machine::select_case(const std::string& args) {
    state_.select_expression = trim(args);
    state_.in_select = true;
    return {};
}

std::string Machine::case_of(const std::string& args) {
    if (!state_.in_select) {
        return "❌ CASE without SELECT\n";
    }
    try {
        const double selected = eval(state_.select_expression);
        const double value = eval(trim(args));
        return selected == value ? "✅ Case matched\n" : "⏭️ Case skipped\n";
    } catch (const std::exception&) {
        return "❌ Invalid CASE expression\n";
    }
}

}  // namespace timewarp::lang::basic
