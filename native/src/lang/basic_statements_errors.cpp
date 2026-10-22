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

std::string Machine::on_error_goto(const std::string& args) {
    const std::string target = trim(args);
    if (target.empty()) {
        return "❌ ON ERROR GOTO requires a line number\n";
    }
    const std::optional<long> line = parse_integer(target);
    if (!line.has_value()) {
        return "❌ Invalid line number for ON ERROR GOTO: " + target + "\n";
    }
    if (*line == 0) {
        state_.error_handler = 0;
        return {};
    }
    if (interpreter_.line_number_map.find(static_cast<int>(*line)) ==
        interpreter_.line_number_map.end()) {
        return "❌ ON ERROR GOTO " + std::to_string(*line) + ": line not found\n";
    }
    state_.error_handler = static_cast<int>(*line);
    return {};
}

std::string Machine::resume(const std::string& args) {
    const std::string target = trim(args);
    if (!target.empty() && is_all_digits(target)) {
        const auto found = interpreter_.line_number_map.find(static_cast<int>(std::stol(target)));
        if (found == interpreter_.line_number_map.end()) {
            return "❌ RESUME " + target + ": line not found\n";
        }
        interpreter_.jump_to(found->second);
        return {};
    }
    if (state_.error_index >= 0) {
        interpreter_.jump_to(static_cast<std::size_t>(state_.error_index));
    }
    return {};
}

std::string Machine::resume_next() {
    if (state_.error_index >= 0) {
        const auto target = static_cast<std::size_t>(state_.error_index) + 1;
        if (target < interpreter_.program_lines().size()) {
            interpreter_.jump_to(target);
        }
        return {};
    }
    const auto& lines = interpreter_.program_lines();
    if (interpreter_.current_line < lines.size() &&
        lines[interpreter_.current_line].number.has_value()) {
        const auto found =
            interpreter_.line_number_map.find(*lines[interpreter_.current_line].number);
        if (found != interpreter_.line_number_map.end() &&
            found->second + 1 < lines.size()) {
            interpreter_.jump_to(found->second + 1);
        }
    }
    return {};
}

std::string Machine::set_xy(const std::string& args) {
    const std::vector<std::string> parts = split_top(args, " ");
    if (parts.size() != 2) {
        return "❌ SETXY expects 2 argument(s)\n";
    }
    try {
        move_to(eval(parts[0]), eval(parts[1]));
    } catch (const std::exception& error) {
        return "❌ SETXY error: " + std::string{error.what()} + "\n";
    }
    return {};
}

std::string Machine::define_fn(const std::string& args) {
    static const std::regex pattern{
        R"(^DEF\s+FN(\w+)\s*\(([^)]*)\)\s*=\s*(.+)$)",
        std::regex::ECMAScript | std::regex::icase};
    std::smatch match;
    const std::string text = trim(args);
    if (!std::regex_match(text, match, pattern)) {
        return "❌ Invalid DEF FN syntax (expected DEF FNA(X) = expr)\n";
    }
    const std::string name = "FN" + upper(match[1].str());
    std::vector<std::string> parameters;
    for (const std::string& field : split_plain(match[2].str(), ',')) {
        if (!trim(field).empty()) {
            parameters.push_back(upper(trim(field)));
        }
    }
    state_.user_fns[name] = {parameters, trim(match[3].str())};
    return {};
}

}  // namespace timewarp::lang::basic
