// SPDX-License-Identifier: MIT
//
// The machine itself: the loader that turns source into statements, the
// evaluator, and the loop that runs a program.
#include "logo_internal.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <chrono>
#include <random>
#include <set>
#include <thread>
#include <utility>

namespace timewarp::lang {
namespace logo_detail {

void LogoMachine::tick() {
    interpreter_.note_step();
    if (!interpreter_.check_budget()) {
        throw BudgetStop{};
    }
}

// -- loading ---------------------------------------------------------------

void LogoMachine::load(std::string_view source) {
    const std::vector<std::string> lines = split_lines(source);
    std::vector<std::string> pending;
    int bracket_depth = 0;
    std::size_t index = 0;
    // Where the statement being collected begins in the source, 1-based.
    int pending_source_line = 0;

    const auto flush = [this, &pending, &pending_source_line]() {
        if (pending.empty()) {
            return;
        }
        std::string joined;
        for (const std::string& piece : pending) {
            if (!joined.empty()) {
                joined.push_back(' ');
            }
            joined += piece;
        }
        pending.clear();
        // The reference re-reads a joined statement through its line-number
        // parser, so a statement that begins with a number loses it and
        // reports that number in an error annotation.
        const std::vector<std::string> parts = split_ws_limit(joined, 1);
        int reported = static_cast<int>(statements_.size()) + 1;
        if (!parts.empty() && !parts[0].empty() &&
            std::all_of(parts[0].begin(), parts[0].end(), [](char character) {
                return character >= '0' && character <= '9';
            })) {
            reported = std::atoi(parts[0].c_str());
            statements_.push_back(parts.size() > 1 ? strip(parts[1]) : std::string{});
        } else {
            statements_.push_back(rstrip(joined));
        }
        statement_lines_.push_back(reported);
        statement_source_lines_.push_back(pending_source_line);
    };

    while (index < lines.size()) {
        std::string line = strip(lines[index]);
        if (line.empty()) {
            ++index;
            continue;
        }
        const std::size_t comment = line.find(';');
        if (comment != std::string::npos) {
            line = strip(std::string_view{line}.substr(0, comment));
        }
        if (line.empty()) {
            ++index;
            continue;
        }

        if (bracket_depth == 0 && starts_with_upper(line, "TO ")) {
            flush();
            parse_procedure(lines, index);
            // Skip to the END that closes the definition. The reference skips
            // on the raw line, so `END ; done` does not close a block there
            // either.
            while (index < lines.size() && upper(strip(lines[index])) != "END") {
                ++index;
            }
            if (index < lines.size()) {
                ++index;
            }
            continue;
        }

        if (pending.empty()) {
            pending_source_line = static_cast<int>(index) + 1;
        }
        pending.push_back(line);
        bracket_depth += static_cast<int>(std::count(line.begin(), line.end(), '['));
        bracket_depth -= static_cast<int>(std::count(line.begin(), line.end(), ']'));
        if (bracket_depth <= 0) {
            flush();
            bracket_depth = 0;
        }
        ++index;
    }
    flush();
}

void LogoMachine::parse_procedure(const std::vector<std::string>& lines, std::size_t start) {
    const std::vector<std::string> parts = split_ws(upper(strip(lines[start])));
    if (parts.size() < 2 || parts[0] != "TO") {
        return;
    }
    const std::string name = parts[1];
    std::vector<std::string> params;
    for (std::size_t index = 2; index < parts.size(); ++index) {
        if (parts[index].front() == ':' && parts[index].size() > 1) {
            params.push_back(parts[index].substr(1));
        }
    }
    std::size_t end_index = start + 1;
    while (end_index < lines.size() && upper(strip(lines[end_index])) != "END") {
        ++end_index;
    }
    if (end_index >= lines.size()) {
        return;  // No END: not a procedure, and the reference ignores it too.
    }
    std::string body;
    for (std::size_t index = start + 1; index < end_index; ++index) {
        if (!body.empty()) {
            body.push_back('\n');
        }
        body += strip(lines[index]);
    }
    procedures_[name] = body;
    procedure_params_[name] = params;
}

// -- splitting -------------------------------------------------------------

bool LogoMachine::head_has_all_arguments(const std::vector<std::string>& collected) const {
    if (collected.empty()) {
        return false;
    }
    const std::string head = upper(collected[0]);
    if (in_set(no_argument_commands(), head)) {
        return true;
    }
    const auto found = procedure_params_.find(head);
    if (found == procedure_params_.end()) {
        return false;
    }
    return collected.size() - 1 >= found->second.size();
}

std::vector<std::string> LogoMachine::split_commands(std::string_view text) const {
    // `;` starts a comment, and the splitter must not see command names inside
    // one: `; Demonstrates: FORWARD, RIGHT` used to split mid-comment.
    std::string stripped;
    {
        std::size_t start = 0;
        while (true) {
            const std::size_t end = text.find('\n', start);
            std::string line{text.substr(start, end == std::string_view::npos
                                                    ? std::string_view::npos
                                                    : end - start)};
            const std::size_t comment = line.find(';');
            if (comment != std::string::npos) {
                line.resize(comment);
            }
            if (!stripped.empty()) {
                stripped.push_back('\n');
            }
            stripped += line;
            if (end == std::string_view::npos) {
                break;
            }
            start = end + 1;
        }
    }

    // Tokens: a bracket, a quoted word, or a bare word.
    std::vector<std::string> tokens;
    std::vector<bool> starts_line;
    {
        bool seen_newline = true;
        std::size_t index = 0;
        while (index < stripped.size()) {
            const char character = stripped[index];
            if (is_space(character)) {
                if (character == '\n') {
                    seen_newline = true;
                }
                ++index;
                continue;
            }
            std::size_t end = index + 1;
            if (character == '[' || character == ']') {
                // One character, already consumed.
            } else {
                while (end < stripped.size() && !is_space(stripped[end]) &&
                       stripped[end] != '[' && stripped[end] != ']') {
                    ++end;
                }
            }
            tokens.emplace_back(stripped.substr(index, end - index));
            starts_line.push_back(seen_newline);
            seen_newline = false;
            index = end;
        }
    }

    std::vector<std::string> commands;
    std::vector<std::string> current;
    std::set<std::string> local_procedures;
    int bracket_depth = 0;
    bool in_to_block = false;

    const auto flush = [&commands, &current]() {
        if (current.empty()) {
            return;
        }
        std::string joined;
        for (const std::string& piece : current) {
            if (!joined.empty()) {
                joined.push_back(' ');
            }
            joined += piece;
        }
        current.clear();
        commands.push_back(joined);
    };

    for (std::size_t index = 0; index < tokens.size(); ++index) {
        const std::string& token = tokens[index];
        const bool at_line_start = starts_line[index];
        if (token == "[") {
            ++bracket_depth;
            current.push_back(token);
            continue;
        }
        if (token == "]") {
            --bracket_depth;
            current.push_back(token);
            continue;
        }
        if (bracket_depth == 0) {
            const std::string name = upper(token);
            if (name == "TO") {
                if (!current.empty() && !in_to_block) {
                    flush();
                }
                in_to_block = true;
            } else if (name == "END" && in_to_block) {
                current.push_back(token);
                std::string joined;
                for (const std::string& piece : current) {
                    if (!joined.empty()) {
                        joined.push_back(' ');
                    }
                    joined += piece;
                }
                const std::vector<std::string> header = split_ws(joined);
                if (header.size() >= 2) {
                    local_procedures.insert(upper(header[1]));
                }
                flush();
                in_to_block = false;
                continue;
            }
        }
        if (in_to_block) {
            current.push_back(token);
            continue;
        }

        const std::string name = upper(token);
        bool is_command =
            in_set(logo_commands(), name) && !in_set(logo_reporters(), name);
        if (!is_command && bracket_depth == 0) {
            const bool known =
                local_procedures.find(name) != local_procedures.end() ||
                procedures_.find(name) != procedures_.end();
            if (at_line_start && known) {
                is_command = true;
            } else if (procedures_.find(name) != procedures_.end() &&
                       head_has_all_arguments(current)) {
                is_command = true;
            }
        }
        if (bracket_depth == 0 && is_command) {
            flush();
        }
        current.push_back(token);
    }
    flush();
    return commands;
}

std::string LogoMachine::run_text(std::string_view text) {
    std::string output;
    for (const std::string& command : split_commands(text)) {
        const std::string result = run_command(command);
        output += result;
        if (result.find("STOP_PROCEDURE") != std::string::npos) {
            break;
        }
    }
    return output;
}

// -- evaluation ------------------------------------------------------------

std::string LogoMachine::substitute_template_variables(std::string_view expression) const {
    std::string result;
    for (std::size_t index = 0; index < expression.size();) {
        if (expression[index] == ':' && index + 1 < expression.size() &&
            expression[index + 1] == '?') {
            std::size_t end = index + 2;
            if (end < expression.size() && (expression[end] == '1' || expression[end] == '2')) {
                ++end;
            }
            const std::string key = std::string{expression.substr(index + 1, end - index - 1)};
            const auto found = interpreter_.variables().find(key);
            double value = 0.0;
            if (found != interpreter_.variables().end()) {
                if (found->second.is_number()) {
                    value = found->second.number();
                } else if (looks_like_number(found->second.text(), value)) {
                    // Kept as parsed.
                } else {
                    value = 0.0;
                }
            }
            result += std::trunc(value) == value ? repr_integer(value) : repr_real(value);
            index = end;
            continue;
        }
        result.push_back(expression[index]);
        ++index;
    }
    return result;
}

double LogoMachine::eval(std::string_view expression) const {
    std::string text = substitute_template_variables(expression);

    // `:NAME` becomes `NAME`: the evaluator takes bare names, and upper-cases
    // them itself, so a variable written `:x` finds one MAKEd as `X`.
    std::string without_colons;
    for (std::size_t index = 0; index < text.size();) {
        if (text[index] == ':' && index + 1 < text.size() &&
            (std::isalpha(static_cast<unsigned char>(text[index + 1])) != 0 ||
             text[index + 1] == '_')) {
            ++index;  // Drop the colon only.
            continue;
        }
        without_colons.push_back(text[index]);
        ++index;
    }
    text = std::move(without_colons);

    // `RANDOM n` is `FLOOR(RND * n)`: Logo's RANDOM is an expression here, not
    // a command.
    {
        std::string rewritten;
        std::size_t index = 0;
        while (index < text.size()) {
            const bool boundary =
                index == 0 ||
                std::isalnum(static_cast<unsigned char>(text[index - 1])) == 0;
            if (boundary && index + 6 <= text.size() &&
                upper(text.substr(index, 6)) == "RANDOM") {
                std::size_t cursor = index + 6;
                if (cursor < text.size() && is_space(text[cursor])) {
                    while (cursor < text.size() && is_space(text[cursor])) {
                        ++cursor;
                    }
                    const std::size_t start = cursor;
                    while (cursor < text.size() &&
                           (std::isalnum(static_cast<unsigned char>(text[cursor])) != 0 ||
                            text[cursor] == '_' || text[cursor] == '.')) {
                        ++cursor;
                    }
                    if (cursor > start) {
                        rewritten += "FLOOR(RND * ";
                        rewritten += text.substr(start, cursor - start);
                        rewritten += ")";
                        index = cursor;
                        continue;
                    }
                }
            }
            rewritten.push_back(text[index]);
            ++index;
        }
        text = std::move(rewritten);
    }

    // `(* A B C)` is Logo's prefix multiplication; the evaluator wants infix.
    {
        std::string rewritten;
        std::size_t index = 0;
        while (index < text.size()) {
            if (text[index] == '(') {
                std::size_t star = index + 1;
                while (star < text.size() && is_space(text[star])) {
                    ++star;
                }
                if (star < text.size() && text[star] == '*') {
                    std::size_t cursor = star + 1;
                    while (cursor < text.size() && is_space(text[cursor])) {
                        ++cursor;
                    }
                    const std::size_t content_start = cursor;
                    bool nested = false;
                    while (cursor < text.size()) {
                        if (text[cursor] == '(') {
                            nested = true;
                            break;
                        }
                        if (text[cursor] == ')') {
                            break;
                        }
                        ++cursor;
                    }
                    if (cursor < text.size() && text[cursor] == ')' && !nested) {
                        const std::vector<std::string> parts =
                            split_ws(std::string_view{text}.substr(
                                content_start, cursor - content_start));
                        std::string joined;
                        for (const std::string& part : parts) {
                            if (!joined.empty()) {
                                joined += " * ";
                            }
                            joined += part;
                        }
                        rewritten += "(" + joined + ")";
                        index = cursor + 1;
                        continue;
                    }
                }
            }
            rewritten.push_back(text[index]);
            ++index;
        }
        text = std::move(rewritten);
    }

    // `COS x` -> `COS(x)`, and `ABS (x)` -> `ABS(x)`.
    {
        std::string rewritten;
        std::size_t index = 0;
        while (index < text.size()) {
            const bool boundary =
                index == 0 ||
                (std::isalnum(static_cast<unsigned char>(text[index - 1])) == 0 &&
                 text[index - 1] != '_');
            if (boundary && std::isalpha(static_cast<unsigned char>(text[index])) != 0) {
                std::size_t end = index;
                while (end < text.size() &&
                       (std::isalnum(static_cast<unsigned char>(text[end])) != 0 ||
                        text[end] == '_')) {
                    ++end;
                }
                const std::string name = upper(text.substr(index, end - index));
                if (in_set(math_prefix_functions(), name)) {
                    std::size_t argument = end;
                    while (argument < text.size() && is_space(text[argument])) {
                        ++argument;
                    }
                    if (argument < text.size() && text[argument] != '(') {
                        std::size_t argument_end = argument;
                        while (argument_end < text.size() &&
                               (std::isalnum(static_cast<unsigned char>(text[argument_end])) !=
                                    0 ||
                                text[argument_end] == '_' || text[argument_end] == '.')) {
                            ++argument_end;
                        }
                        if (argument_end > argument) {
                            rewritten += name;
                            rewritten += "(";
                            rewritten += text.substr(argument, argument_end - argument);
                            rewritten += ")";
                            index = argument_end;
                            continue;
                        }
                    }
                    // Already parenthesised: drop the space between the two.
                    rewritten += name;
                    index = argument;
                    continue;
                }
            }
            rewritten.push_back(text[index]);
            ++index;
        }
        text = std::move(rewritten);
    }

    // `ABS (x)` -> `ABS(x)`: Logo allows the space, the evaluator does not.
    {
        std::string rewritten;
        std::size_t index = 0;
        while (index < text.size()) {
            const bool boundary =
                index == 0 ||
                (std::isalnum(static_cast<unsigned char>(text[index - 1])) == 0 &&
                 text[index - 1] != '_');
            if (boundary && std::isalpha(static_cast<unsigned char>(text[index])) != 0) {
                std::size_t end = index;
                while (end < text.size() &&
                       (std::isalnum(static_cast<unsigned char>(text[end])) != 0 ||
                        text[end] == '_')) {
                    ++end;
                }
                const std::string name = upper(text.substr(index, end - index));
                if (in_set(spaced_call_functions(), name)) {
                    std::size_t argument = end;
                    while (argument < text.size() && is_space(text[argument])) {
                        ++argument;
                    }
                    if (argument < text.size() && text[argument] == '(') {
                        rewritten += name;
                        index = argument;
                        continue;
                    }
                }
            }
            rewritten.push_back(text[index]);
            ++index;
        }
        text = std::move(rewritten);
    }

    return interpreter_.evaluate_expression(text);
}

double LogoMachine::eval_arg(std::string_view argument) const {
    const std::string text = strip(argument);
    if (!text.empty() && text.front() == ':' && text.find(' ') == std::string::npos) {
        const std::string name = upper(std::string_view{text}.substr(1));
        const auto found = interpreter_.variables().find(name);
        if (found == interpreter_.variables().end()) {
            throw LogoError("Undefined variable: " + text);
        }
        return found->second.number();
    }
    try {
        return eval(text);
    } catch (const std::exception& error) {
        throw LogoError("Cannot evaluate argument '" + text + "': " + error.what());
    }
}

std::optional<double> LogoMachine::eval_number(std::string_view argument) const {
    try {
        return eval_arg(argument);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

std::vector<std::string> LogoMachine::consume_args(const std::vector<std::string>& args,
                                                   std::size_t count) const {
    static const std::set<std::string> operators{"+", "-",  "*", "/",  "%",
                                                 "^", "=",  "<", ">", "<=",
                                                 ">=", "<>"};
    std::vector<std::string> consumed;
    std::size_t index = 0;
    for (std::size_t wanted = 0; wanted < count; ++wanted) {
        if (index >= args.size()) {
            break;
        }
        std::vector<std::string> pieces;
        int balance = 0;
        while (index < args.size()) {
            const std::string& token = args[index];
            pieces.push_back(token);
            ++index;
            balance += static_cast<int>(std::count(token.begin(), token.end(), '('));
            balance -= static_cast<int>(std::count(token.begin(), token.end(), ')'));
            // A list literal is one argument, so brackets count too.
            balance += static_cast<int>(std::count(token.begin(), token.end(), '['));
            balance -= static_cast<int>(std::count(token.begin(), token.end(), ']'));
            if (balance != 0) {
                continue;
            }
            if (operators.find(token) != operators.end()) {
                continue;
            }
            if (index < args.size() && operators.find(args[index]) != operators.end()) {
                continue;
            }
            break;
        }
        std::string joined;
        for (const std::string& piece : pieces) {
            if (!joined.empty()) {
                joined.push_back(' ');
            }
            joined += piece;
        }
        consumed.push_back(joined);
    }
    return consumed;
}

// -- variables -------------------------------------------------------------

void LogoMachine::set_number(const std::string& name, double value) {
    interpreter_.set_number(name, value);
    words_.erase(name);
}

void LogoMachine::set_word(const std::string& name, std::string value) {
    words_[name] = std::move(value);
}

// -- turtle ----------------------------------------------------------------

void LogoMachine::forward(double distance) {
    pose_.move(distance);
    turtle_.forward(distance);
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::back(double distance) {
    pose_.move(-distance);
    turtle_.back(distance);
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::right(double degrees) {
    pose_.turn(degrees);
    turtle_.right(degrees);
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::left(double degrees) {
    pose_.turn(-degrees);
    turtle_.left(degrees);
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::go_to(double x, double y) {
    pose_.go_to(x, y);
    turtle_.goto_xy(x, y);
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::set_heading(double degrees) {
    pose_.set_heading(degrees);
    turtle_.set_heading(degrees);
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::home() {
    pose_.go_to(0.0, 0.0);
    pose_.set_heading(0.0);
    turtle_.home();
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::clear_screen() {
    pose_.home();
    pen_down_ = true;
    pen_width_ = 2.0;
    turtle_.reset();
    if (on_change_) {
        on_change_();
    }
}

// -- helpers shared by several commands ------------------------------------

std::vector<std::string> LogoMachine::group_rgb_arguments(
    const std::vector<std::string>& args) const {
    static const std::set<std::string> operators{"+", "-", "*", "/", "%", "^"};
    std::vector<std::string> groups;
    std::size_t index = 0;
    while (index < args.size() && groups.size() < 3) {
        std::vector<std::string> pieces;
        const std::string& token = args[index];
        pieces.push_back(token);
        ++index;
        if (in_set(math_prefix_functions(), upper(token)) && index < args.size()) {
            pieces.push_back(args[index]);
            ++index;
        }
        while (index + 1 < args.size() &&
               operators.find(args[index]) != operators.end()) {
            pieces.push_back(args[index]);
            pieces.push_back(args[index + 1]);
            index += 2;
        }
        std::string joined;
        for (const std::string& piece : pieces) {
            if (!joined.empty()) {
                joined.push_back(' ');
            }
            joined += piece;
        }
        groups.push_back(joined);
    }
    return groups;
}

std::string LogoMachine::resolve_colour_argument(
    const std::vector<std::string>& args) {
    if (!args.empty() && in_set(colour_reporters(), upper(args[0]))) {
        std::string joined;
        for (const std::string& piece : args) {
            if (!joined.empty()) {
                joined.push_back(' ');
            }
            joined += piece;
        }
        // The reference runs the reporter with no turtle at all. The answer is a
        // value either way, and no reporter is a turtle command.
        const std::string result = run_text(joined);
        const std::string trimmed = strip(result);
        if (trimmed.rfind("❌", 0) == 0) {
            return trimmed;
        }
        return strip_char(trimmed, '"');
    }
    std::string token = strip_char(strip(args.empty() ? std::string{} : args[0]), '"');
    if (!token.empty() && token.front() == ':') {
        const std::string name = upper(std::string_view{token}.substr(1));
        const auto number = interpreter_.variables().find(name);
        if (number != interpreter_.variables().end()) {
            return strip_char(number->second.render(), '"');
        }
        const auto word = words_.find(name);
        if (word != words_.end()) {
            return strip_char(word->second, '"');
        }
        return token;
    }
    return token;
}

// -- the run ---------------------------------------------------------------

void LogoMachine::run(std::string_view source) {
    std::mt19937_64 seeder{std::random_device{}()};
    random_.seed_unsigned(seeder());
    load(source);

    for (std::size_t index = 0; index < statements_.size(); ++index) {
        // The debugger's pause point. A whole statement is the unit: the loader
        // joins bracketed lines into one, and a procedure call is one statement
        // whose body runs commands that have no line of their own.
        if (!interpreter_.pause_if_requested(statement_source_lines_[index])) {
            // Break rather than return: the words this run created are still
            // facts about it, and the Variables panel reads them after a stop.
            interpreter_.running = false;
            break;
        }
        try {
            tick();
        } catch (const BudgetStop&) {
            return;
        }
        std::string output;
        try {
            output = run_text(statements_[index]);
        } catch (const BudgetStop&) {
            return;
        } catch (const std::exception& error) {
            output = std::string{"❌ Error at line "} +
                     std::to_string(index + 1) + ": " + error.what() + "\n";
        }
        interpreter_.log_output(annotate(output, statement_lines_[index]));
        if (!interpreter_.running) {
            break;
        }
    }

    // The reference keeps its words in a second store that the Variables panel
    // and the differential harness both read, so they are folded into the one
    // map at the end of the run, words winning as they do there. Lists are not
    // folded: the reference keeps them in a third store that neither reads.
    for (const auto& [name, text] : words_) {
        interpreter_.set_text(name, text);
    }
}

std::string LogoMachine::run_one(std::string_view command) {
    std::string output;
    try {
        output = run_text(command);
    } catch (const BudgetStop&) {
        return output;
    } catch (const std::exception& error) {
        output = std::string{"❌ "} + error.what() + "\n";
    }
    for (const auto& [name, text] : words_) {
        interpreter_.set_text(name, text);
    }
    return output;
}

}  // namespace logo_detail
}  // namespace timewarp::lang
