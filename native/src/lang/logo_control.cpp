// SPDX-License-Identifier: MIT
//
// Control flow and output: REPEAT, IF, WHILE, FOREVER, PRINT and friends.
#include "logo_internal.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <random>

namespace timewarp::lang {
namespace logo_detail {

// -- control ---------------------------------------------------------------


std::string LogoMachine::do_repeat(std::string_view command) {
    const std::size_t open = command.find('[');
    if (open == std::string_view::npos) {
        return "❌ REPEAT requires [commands]\n";
    }
    const std::size_t close = command.rfind(']');
    if (close == std::string_view::npos || close < open) {
        return "❌ REPEAT requires [commands]\n";
    }
    const std::string header = strip(command.substr(0, open));
    const std::string body = strip(command.substr(open + 1, close - open - 1));
    if (!starts_with_upper(header, "REPEAT")) {
        return "❌ Invalid REPEAT command\n";
    }
    const std::string count_text = strip(std::string_view{header}.substr(6));
    if (count_text.empty()) {
        return "❌ REPEAT requires count\n";
    }
    double count = 0.0;
    try {
        count = std::trunc(eval(count_text));
    } catch (const std::exception&) {
        return "❌ Invalid REPEAT count: " + count_text + "\n";
    }
    std::string output;
    for (long long step = 0; step < static_cast<long long>(count); ++step) {
        tick();
        set_number("REPCOUNT", static_cast<double>(step + 1));
        set_number("REPCOUNTER", static_cast<double>(step + 1));
        output += run_text(body);
    }
    return output;
}

std::string LogoMachine::do_if(std::string_view command) {
    const std::size_t open = command.find('[');
    if (open == std::string_view::npos) {
        return "❌ IF requires [commands]\n";
    }
    const std::size_t close = command.rfind(']');
    if (close == std::string_view::npos || close < open) {
        return "❌ IF requires [commands]\n";
    }
    const std::string header = strip(command.substr(0, open));
    const std::string body = strip(command.substr(open + 1, close - open - 1));
    if (!starts_with_upper(header, "IF")) {
        return "❌ Invalid IF command\n";
    }
    const std::string condition_text = strip(std::string_view{header}.substr(2));
    if (condition_text.empty()) {
        return "❌ IF requires condition\n";
    }
    double condition = 0.0;
    try {
        condition = eval(condition_text);
    } catch (const std::exception&) {
        return "❌ Invalid IF condition: " + condition_text + "\n";
    }
    if (condition != 0.0) {
        return run_text(body);
    }
    return {};
}

std::string LogoMachine::do_ifelse(std::string_view command) {
    const std::size_t first = command.find('[');
    if (first == std::string_view::npos) {
        return "❌ IFELSE requires [true] [false] blocks\n";
    }
    const std::string header = strip(command.substr(0, first));
    const std::vector<std::string> header_words = split_ws(header);
    if (header_words.size() < 2) {
        return "❌ IFELSE requires condition\n";
    }
    std::string condition_text;
    for (std::size_t index = 1; index < header_words.size(); ++index) {
        if (!condition_text.empty()) {
            condition_text.push_back(' ');
        }
        condition_text += header_words[index];
    }
    const std::size_t first_close = matching_bracket(command, first);
    if (first_close == std::string_view::npos) {
        return "❌ IFELSE first block malformed\n";
    }
    const std::string true_block = strip(command.substr(first + 1, first_close - first - 1));
    const std::size_t second = command.find('[', first_close + 1);
    if (second == std::string_view::npos) {
        return "❌ IFELSE requires second [false] block\n";
    }
    const std::size_t second_close = matching_bracket(command, second);
    if (second_close == std::string_view::npos) {
        return "❌ IFELSE second block malformed\n";
    }
    const std::string false_block =
        strip(command.substr(second + 1, second_close - second - 1));

    double condition = 0.0;
    const std::string shouted = upper(strip(condition_text));
    if (shouted == "TRUE") {
        condition = 1.0;
    } else if (shouted == "FALSE") {
        condition = 0.0;
    } else {
        try {
            condition = eval(condition_text);
        } catch (const std::exception&) {
            return "❌ Invalid IFELSE condition: " + condition_text + "\n";
        }
    }
    return condition != 0.0 ? run_text(true_block) : run_text(false_block);
}

std::vector<std::string> LogoMachine::parse_body(std::string_view body) const {
    const std::vector<std::string> lines = split_lines(body);
    std::vector<std::string> commands;
    std::size_t index = 0;
    while (index < lines.size()) {
        const std::string line = strip(lines[index]);
        if (line.empty()) {
            ++index;
            continue;
        }
        int bracket_count = static_cast<int>(std::count(line.begin(), line.end(), '[')) -
                            static_cast<int>(std::count(line.begin(), line.end(), ']'));
        if (bracket_count > 0) {
            std::string joined = line;
            ++index;
            while (index < lines.size() && bracket_count > 0) {
                const std::string next = strip(lines[index]);
                joined += "\n" + next;
                bracket_count += static_cast<int>(std::count(next.begin(), next.end(), '[')) -
                                 static_cast<int>(std::count(next.begin(), next.end(), ']'));
                ++index;
            }
            commands.push_back(joined);
        } else {
            commands.push_back(line);
            ++index;
        }
    }
    return commands;
}

std::string LogoMachine::do_while(std::string_view command, bool until_mode) {
    const char* const which = until_mode ? "UNTIL" : "WHILE";
    std::vector<std::string> blocks;
    std::size_t index = 0;
    while (index < command.size() && blocks.size() < 2) {
        if (command[index] != '[') {
            ++index;
            continue;
        }
        int depth = 0;
        std::size_t cursor = index;
        while (cursor < command.size()) {
            if (command[cursor] == '[') {
                ++depth;
            } else if (command[cursor] == ']') {
                --depth;
            }
            if (depth == 0) {
                blocks.emplace_back(strip(command.substr(index + 1, cursor - index - 1)));
                index = cursor;
                break;
            }
            ++cursor;
        }
        ++index;
    }
    if (blocks.size() < 2) {
        return std::string{"❌ "} + which + " requires [condition] [body]\n";
    }
    std::string output;
    for (int iteration = 0; iteration < 10000; ++iteration) {
        tick();
        if (!interpreter_.running) {
            break;
        }
        double condition = 0.0;
        try {
            condition = eval(blocks[0]);
        } catch (const std::exception&) {
            return std::string{"❌ "} + which + ": invalid condition '" + blocks[0] + "'\n";
        }
        if (until_mode ? condition != 0.0 : condition == 0.0) {
            break;
        }
        output += run_text(blocks[1]);
        if (!interpreter_.running) {
            break;
        }
    }
    return output;
}

std::string LogoMachine::do_forever(std::string_view command) {
    const std::size_t open = command.find('[');
    if (open == std::string_view::npos) {
        return "❌ FOREVER requires [commands]\n";
    }
    const std::size_t start = command.find('[', open) + 1;
    const std::size_t end = command.rfind(']');
    if (start >= end) {
        return "❌ FOREVER commands malformed\n";
    }
    const std::string body = strip(command.substr(start, end - start));
    std::string output;
    for (int iteration = 0; iteration < 10000; ++iteration) {
        tick();
        if (!interpreter_.running) {
            break;
        }
        set_number("REPCOUNT", static_cast<double>(iteration + 1));
        set_number("REPCOUNTER", static_cast<double>(iteration + 1));
        output += run_text(body);
        if (!interpreter_.running) {
            break;
        }
    }
    return output;
}

// -- output ----------------------------------------------------------------

std::string LogoMachine::do_output(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ OUTPUT requires value\n";
    }
    std::string expression;
    for (const std::string& piece : args) {
        if (!expression.empty()) {
            expression.push_back(' ');
        }
        expression += piece;
    }
    try {
        const double value = eval(expression);
        last_output_ = value;
        return repr_value(value) + "STOP_PROCEDURE";
    } catch (const std::exception& error) {
        return std::string{"❌ Invalid OUTPUT value: "} + error.what() + "\n";
    }
}

std::string LogoMachine::do_to(std::string_view command) {
    const std::string shouted = upper(command);
    std::size_t start = shouted.find("TO ");
    if (start == std::string::npos) {
        if (starts_with_upper(shouted, "TO ")) {
            start = 0;
        } else {
            return "❌ Invalid TO command\n";
        }
    }
    const std::size_t end = shouted.find(" END");
    if (end == std::string::npos) {
        return "❌ TO requires END\n";
    }
    const std::vector<std::string> lines = split_lines(command.substr(start, end - start));
    if (lines.empty()) {
        return "❌ Invalid TO command\n";
    }
    const std::vector<std::string> header = split_ws(strip(lines[0]));
    if (header.size() < 2) {
        return "❌ Invalid TO command\n";
    }
    const std::string name = header[1];
    std::vector<std::string> params;
    std::string body;
    if (lines.size() > 1) {
        for (std::size_t index = 2; index < header.size(); ++index) {
            if (header[index].front() == ':' && header[index].size() > 1) {
                params.push_back(header[index].substr(1));
            }
        }
        for (std::size_t index = 1; index < lines.size(); ++index) {
            if (!body.empty()) {
                body.push_back('\n');
            }
            body += lines[index];
        }
    } else {
        bool in_params = true;
        for (std::size_t index = 2; index < header.size(); ++index) {
            const std::string& part = header[index];
            if (in_params && part.front() == ':' && part.size() > 1) {
                params.push_back(part.substr(1));
            } else {
                in_params = false;
                if (!body.empty()) {
                    body.push_back(' ');
                }
                body += part;
            }
        }
    }
    procedure_params_[name] = params;
    procedures_[name] = body;
    return {};
}

std::string LogoMachine::call_procedure(const std::string& name,
                                        const std::vector<std::string>& args) {
    const auto body = procedures_.find(name);
    if (body == procedures_.end()) {
        return "❌ Procedure " + name + " not defined\n";
    }
    const std::vector<std::string> params = procedure_params_[name];
    constexpr int kMaxDepth = 128;

    const double depth = interpreter_.number("__RECURSION_DEPTH__");
    if (depth >= kMaxDepth) {
        return "❌ Procedure " + name + " exceeded maximum recursion depth (" +
               std::to_string(kMaxDepth) + ")\n";
    }
    tick();
    set_number("__RECURSION_DEPTH__", depth + 1.0);

    const std::vector<std::string> used = consume_args(args, params.size());
    if (used.size() < params.size()) {
        set_number("__RECURSION_DEPTH__", depth);
        return "❌ Procedure " + name + " requires " + std::to_string(params.size()) +
               " inputs\n";
    }

    std::vector<Saved> saved;
    saved.reserve(params.size());
    for (std::size_t index = 0; index < params.size(); ++index) {
        const std::string& param = params[index];
        const std::string& argument = used[index];
        Saved entry{param, std::nullopt, std::nullopt};
        const auto existing = interpreter_.variables().find(param);
        if (existing != interpreter_.variables().end()) {
            entry.number = existing->second;
        }
        const auto existing_word = words_.find(param);
        if (existing_word != words_.end()) {
            entry.word = existing_word->second;
        }
        saved.push_back(entry);

        if (!argument.empty() && argument.front() == '"') {
            set_word(param, argument.substr(1));
            continue;
        }
        try {
            set_number(param, eval(argument));
        } catch (const std::exception&) {
            // A bare word is a string, not an error.
            set_word(param, argument);
        }
    }

    std::string output;
    // The parameters and the recursion counter are put back on *every* exit,
    // including the STOP/OUTPUT ones in the middle of the body -- the reference
    // does this in a `finally`, and a `return` that skipped it leaked the
    // counter until a deep program hit the depth limit it should never have
    // reached.
    struct Guard {
        LogoMachine& machine;
        const std::vector<Saved>& saved;
        double depth;
        ~Guard() { machine.restore(saved, depth); }
    } guard{*this, saved, depth};

    for (const std::string& statement : parse_body(body->second)) {
        const std::string trimmed = strip(statement);
        if (trimmed.empty()) {
            continue;
        }
        const std::string result = run_text(trimmed);
        if (result.find("STOP_PROCEDURE") != std::string::npos) {
            std::string cleaned = result;
            std::string::size_type position = 0;
            while ((position = cleaned.find("STOP_PROCEDURE", position)) !=
                   std::string::npos) {
                cleaned.erase(position, std::string{"STOP_PROCEDURE"}.size());
            }
            return output + cleaned;
        }
        if (result.rfind("❌", 0) == 0) {
            // The reference abandons the procedure at the first error and
            // returns what the caller may print.
            return output + result;
        }
        output += result;
    }
    return output;
}

void LogoMachine::restore(const std::vector<Saved>& saved, double depth) {
    set_number("__RECURSION_DEPTH__", depth);
    for (const Saved& entry : saved) {
        if (entry.number.has_value()) {
            interpreter_.set_number(entry.name, entry.number->number());
        } else {
            interpreter_.remove_variable(entry.name);
        }
        if (entry.word.has_value()) {
            words_[entry.name] = *entry.word;
        } else {
            words_.erase(entry.name);
        }
    }
}

}  // namespace logo_detail
}  // namespace timewarp::lang
