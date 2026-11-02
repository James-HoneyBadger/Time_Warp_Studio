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
    interpreter_.push_call_frame("GOSUB", interpreter_.display_line());
    interpreter_.jump_to(found->second);
    return {};
}

std::string Machine::return_from_sub() {
    if (state_.gosubs.empty()) {
        return "❌ RETURN without GOSUB\n";
    }
    interpreter_.jump_to(state_.gosubs.back() + 1);
    state_.gosubs.pop_back();
    interpreter_.pop_call_frame();
    return {};
}

std::string Machine::begin_for(const std::string& args, ForFrame& frame) {
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
        frame.limit = eval(match[3].str());
        frame.step = match[4].matched ? eval(match[4].str()) : 1.0;
        if (frame.step == 0.0) {
            return "❌ FOR STEP cannot be 0\n";
        }
        frame.variable = var_name;
        assign(var_name, start);
    } catch (const std::exception& error) {
        return "❌ Error in FOR: " + std::string{error.what()} + "\n";
    }
    return {};
}

bool Machine::for_runs_again(const ForFrame& frame, double value) const {
    return frame.step > 0.0 ? value <= frame.limit : value >= frame.limit;
}

std::string Machine::for_loop(const std::string& args) {
    ForFrame frame;
    std::string error = begin_for(args, frame);
    if (!error.empty()) {
        return error;
    }
    frame.body = interpreter_.current_line;
    state_.fors.push_back(std::move(frame));
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
    if (for_runs_again(frame, value)) {
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

namespace {

/// One `SELECT CASE` clause: `1`, `1, 3`, `2 TO 5`, `IS > 10` or `ELSE`.
///
/// Kept as data rather than evaluated in place, because the three shapes compare
/// differently and a clause list is a union of them.
struct Clause {
    enum class Kind { Value, Range, Compare, Else };
    Kind kind = Kind::Value;
    std::string first;
    std::string second;
    std::string comparison;
};

std::vector<Clause> parse_clauses(const std::string& text) {
    std::vector<Clause> clauses;
    for (const std::string& part : split_trimmed(text, ',')) {
        const std::string upper_part = upper(part);
        if (upper_part == "ELSE") {
            clauses.push_back(Clause{Clause::Kind::Else, {}, {}, {}});
            continue;
        }
        if (starts_with(upper_part, "IS ")) {
            const std::string rest = trim(part.substr(3));
            // `<op> <expression>`, longest operator first so `<=` is not read as `<`.
            static const char* const kOperators[] = {"<>", "<=", ">=", "=", "<", ">"};
            bool understood = false;
            for (const char* op : kOperators) {
                if (!starts_with(rest, op)) {
                    continue;
                }
                clauses.push_back(Clause{Clause::Kind::Compare,
                                         trim(rest.substr(std::strlen(op))), {}, op});
                understood = true;
                break;
            }
            if (!understood) {
                throw BasicError("Invalid CASE expression");
            }
            continue;
        }
        const std::size_t to = upper_part.find(" TO ");
        if (to != std::string::npos) {
            clauses.push_back(Clause{Clause::Kind::Range, trim(part.substr(0, to)),
                                     trim(part.substr(to + 4)), {}});
            continue;
        }
        clauses.push_back(Clause{Clause::Kind::Value, part, {}, {}});
    }
    if (clauses.empty()) {
        throw BasicError("Invalid CASE expression");
    }
    return clauses;
}

}  // namespace

std::string Machine::select_case(const std::string& args) {
    const std::string expression = trim(args);
    if (expression.empty()) {
        return "❌ SELECT CASE requires an expression\n";
    }
    state_.selects.push_back(State::SelectFrame{expression, false});
    return {};
}

std::string Machine::end_select() {
    if (state_.selects.empty()) {
        return "❌ END SELECT without SELECT\n";
    }
    state_.selects.pop_back();
    return {};
}

std::string Machine::case_of(const std::string& args) {
    if (state_.selects.empty()) {
        return "❌ CASE without SELECT\n";
    }
    if (state_.selects.back().matched) {
        return skip_to_next_clause();
    }
    const std::string clause_text = trim(args);
    if (clause_text.empty()) {
        return "❌ CASE requires a value\n";
    }
    std::vector<Clause> clauses;
    try {
        clauses = parse_clauses(clause_text);
    } catch (const std::exception&) {
        // A clause that cannot be read does not match, and its body is not run:
        // reporting the error and then running it anyway was the defect.
        const std::string skipped = skip_to_next_clause();
        return "❌ Invalid CASE expression: " + clause_text + "\n" + skipped;
    }

    bool matched = false;
    // The selector is read once for the whole block, so every clause compares the
    // same value and a selector that cannot be read fails the same way for each of
    // them.  Read here rather than at `SELECT`, so it sees the variables as they
    // are when the first clause is tested.
    const std::string expression = state_.selects.back().expression;
    try {
        const double selected = eval(expression);
        for (const Clause& clause : clauses) {
            if (clause.kind == Clause::Kind::Else) {
                matched = true;
                break;
            }
            if (clause.kind == Clause::Kind::Value) {
                matched = selected == eval(clause.first);
            } else if (clause.kind == Clause::Kind::Range) {
                matched = selected >= eval(clause.first) &&
                          selected <= eval(clause.second);
            } else {
                const double right = eval(clause.first);
                if (clause.comparison == "=") {
                    matched = selected == right;
                } else if (clause.comparison == "<>") {
                    matched = selected != right;
                } else if (clause.comparison == "<") {
                    matched = selected < right;
                } else if (clause.comparison == ">") {
                    matched = selected > right;
                } else if (clause.comparison == "<=") {
                    matched = selected <= right;
                } else {
                    matched = selected >= right;
                }
            }
            if (matched) {
                break;
            }
        }
    } catch (const std::exception&) {
        const std::string skipped = skip_to_next_clause();
        return "❌ Invalid CASE expression: " + clause_text + "\n" + skipped;
    }

    // A `CASE ELSE` after a clause that already matched is dead code, and reaching
    // one means the block has no clause that applies: the body is skipped either way.
    if (!matched) {
        return skip_to_next_clause();
    }
    state_.selects.back().matched = true;
    return {};
}

std::string Machine::skip_to_next_clause() {
    const auto& lines = interpreter_.program_lines();
    int depth = 0;
    for (std::size_t scan = interpreter_.current_line + 1; scan < lines.size(); ++scan) {
        const std::string code = upper(trim(lines[scan].text));
        if (starts_with(code, "SELECT ")) {
            ++depth;
            continue;
        }
        if (code == "END SELECT") {
            if (depth == 0) {
                interpreter_.jump_to(scan);
                return {};
            }
            --depth;
            continue;
        }
        if (depth == 0 && starts_with(code, "CASE ")) {
            interpreter_.jump_to(scan);
            return {};
        }
    }
    return "❌ SELECT without END SELECT\n";
}

namespace {

/// A colon-separated line's statement, read as `keyword` plus its arguments.
///
/// `matches` is true for `WHILE` and `WHILE X < 3` alike, and `args` is the rest of
/// the statement as the program wrote it -- case matters to the expression reader.
bool sequence_keyword(const std::string& statement, std::string_view keyword,
                      std::string& args) {
    const std::string code = upper(trim(statement));
    if (code.size() < keyword.size() || code.compare(0, keyword.size(), keyword) != 0) {
        return false;
    }
    if (code.size() != keyword.size() && code[keyword.size()] != ' ') {
        return false;
    }
    const std::string written = trim(statement);
    args = trim(written.substr(std::min(keyword.size(), written.size())));
    return true;
}

}  // namespace

std::string Machine::run_sequence(const std::vector<std::string>& statements) {
    // A loop written on one line has no `WEND` line to jump to, so the line runs as
    // a small program of its own: `pc` is the statement, and a frame remembers where
    // its opener was.  Every statement still costs a step, so a runaway one-line
    // loop stops on the same budget as a runaway program.
    std::vector<LineLoop> frames;
    std::string output;
    // Published so a statement run from inside this sequence -- `IF N = 3 THEN
    // EXIT FOR`, which goes through the conditional's own path -- can find the loop
    // it is inside and tell the sequence where to carry on.
    std::vector<LineLoop>* const outer_loops = state_.line_loops;
    state_.line_loops = &frames;
    state_.line_jump.reset();

    const auto find_close = [&statements](std::size_t from, std::string_view opener,
                                          std::string_view closer)
        -> std::optional<std::size_t> {
        int depth = 0;
        std::string ignored;
        for (std::size_t scan = from + 1; scan < statements.size(); ++scan) {
            if (sequence_keyword(statements[scan], opener, ignored)) {
                ++depth;
                continue;
            }
            if (sequence_keyword(statements[scan], closer, ignored)) {
                if (depth == 0) {
                    return scan;
                }
                --depth;
            }
        }
        return std::nullopt;
    };

    const auto innermost = [&frames](char kind) -> std::optional<std::size_t> {
        if (frames.empty() || frames.back().kind != kind) {
            return std::nullopt;
        }
        return frames.size() - 1;
    };

    const std::size_t entry_line = interpreter_.current_line;
    const auto leave = [this, &output, outer_loops]() {
        state_.line_loops = outer_loops;
        state_.line_jump.reset();
        return output;
    };
    std::size_t pc = 0;
    while (pc < statements.size()) {
        if (!interpreter_.running) {
            break;
        }
        interpreter_.note_step();
        if (!interpreter_.check_budget()) {
            break;
        }
        // A GOTO, a GOSUB or a RETURN moved the program somewhere else: the rest of
        // this line is not what runs next.
        if (interpreter_.current_line != entry_line) {
            break;
        }

        const std::string& statement = statements[pc];
        std::string args;
        if (sequence_keyword(statement, "WHILE", args)) {
            const std::optional<std::size_t> close = find_close(pc, "WHILE", "WEND");
            if (!close.has_value()) {
                // No `WEND` on this line, so this is an ordinary block loop whose
                // header shares its line with a statement or two: the header runs as
                // the statement it is, and the `WEND` line closes it.
                output += execute(statement);
                ++pc;
                continue;
            }
            bool carry_on = false;
            try {
                carry_on = eval(args) != 0.0;
            } catch (const std::exception& error) {
                output += "❌ WHILE syntax error: " + std::string{error.what()} + "\n";
                break;
            }
            if (!carry_on) {
                pc = *close + 1;
                continue;
            }
            frames.push_back(LineLoop{'W', pc, pc + 1, *close + 1, {}});
            ++pc;
            continue;
        }
        if (sequence_keyword(statement, "WEND", args)) {
            const std::optional<std::size_t> top = innermost('W');
            if (!top.has_value()) {
                output += "❌ WEND without WHILE\n";
                break;
            }
            const std::size_t opener = frames[*top].opener;
            frames.pop_back();
            pc = opener;
            continue;
        }
        if (sequence_keyword(statement, "FOR", args)) {
            const std::optional<std::size_t> close = find_close(pc, "FOR", "NEXT");
            if (!close.has_value()) {
                // `NEXT` is on a later line, so the header runs as the block form's
                // header does and the statement after it is just the next statement.
                output += execute(statement);
                ++pc;
                continue;
            }
            LineLoop frame{'F', pc, pc + 1, *close + 1, {}};
            const std::string error = begin_for(args, frame.for_frame);
            if (!error.empty()) {
                output += error;
                break;
            }
            // The header does not test the limit: `FOR I = 5 TO 1` runs its body
            // once here, exactly as the same header run as a block does, and
            // `NEXT` is what decides whether there is a second pass.
            frames.push_back(std::move(frame));
            ++pc;
            continue;
        }
        if (sequence_keyword(statement, "NEXT", args)) {
            const std::optional<std::size_t> top = innermost('F');
            if (!top.has_value()) {
                output += "❌ NEXT without FOR\n";
                break;
            }
            ForFrame& frame = frames[*top].for_frame;
            const double value = number_of(frame.variable).value_or(0.0) + frame.step;
            assign(frame.variable, value);
            if (for_runs_again(frame, value)) {
                pc = frames[*top].body;
            } else {
                frames.pop_back();
                ++pc;
            }
            continue;
        }
        if (sequence_keyword(statement, "DO", args)) {
            const std::optional<std::size_t> close = find_close(pc, "DO", "LOOP");
            if (!close.has_value()) {
                // `LOOP` is on a later line: the header and the statements after it
                // run as ordinary statements, and the `LOOP` line closes the block.
                output += execute(statement);
                ++pc;
                continue;
            }
            const std::string condition = upper(trim(args));
            bool carry_on = true;
            if (starts_with(condition, "WHILE ")) {
                try {
                    carry_on = eval(trim(args).substr(6)) != 0.0;
                } catch (const std::exception&) {
                    output += "❌ Invalid DO WHILE condition\n";
                    break;
                }
            } else if (starts_with(condition, "UNTIL ")) {
                try {
                    carry_on = eval(trim(args).substr(6)) == 0.0;
                } catch (const std::exception&) {
                    output += "❌ Invalid DO UNTIL condition\n";
                    break;
                }
            }
            if (!carry_on) {
                pc = *close + 1;
                continue;
            }
            frames.push_back(LineLoop{'D', pc, pc + 1, *close + 1, {}});
            ++pc;
            continue;
        }
        if (sequence_keyword(statement, "LOOP", args)) {
            const std::optional<std::size_t> top = innermost('D');
            if (!top.has_value()) {
                output += "❌ LOOP without DO\n";
                break;
            }
            const std::string condition = upper(trim(args));
            bool again = true;
            if (starts_with(condition, "WHILE ")) {
                try {
                    again = eval(trim(args).substr(6)) != 0.0;
                } catch (const std::exception&) {
                    output += "❌ Invalid LOOP WHILE condition\n";
                    break;
                }
            } else if (starts_with(condition, "UNTIL ")) {
                try {
                    again = eval(trim(args).substr(6)) == 0.0;
                } catch (const std::exception&) {
                    output += "❌ Invalid LOOP UNTIL condition\n";
                    break;
                }
            }
            if (again) {
                const std::size_t opener = frames[*top].opener;
                frames.pop_back();
                pc = opener;
            } else {
                frames.pop_back();
                ++pc;
            }
            continue;
        }
        if (sequence_keyword(statement, "EXIT", args)) {
            const std::string what = upper(trim(args));
            const char kind = what == "FOR" ? 'F' : (what == "DO" ? 'D' : 'W');
            const std::optional<std::size_t> top = innermost(kind);
            if (!top.has_value()) {
                output += "❌ EXIT " + what + " without a matching loop\n";
                break;
            }
            const std::string closer = kind == 'F' ? "NEXT" : (kind == 'D' ? "LOOP" : "WEND");
            const std::string opener = kind == 'F' ? "FOR" : (kind == 'D' ? "DO" : "WHILE");
            const std::size_t opener_index = frames[*top].opener;
            frames.pop_back();
            const std::optional<std::size_t> close = find_close(opener_index, opener, closer);
            pc = close.has_value() ? *close + 1 : statements.size();
            continue;
        }
        output += execute(statement);
        if (state_.line_jump.has_value()) {
            // An `EXIT` ran inside this statement: it named the loop to leave and
            // where leaving it lands.
            pc = *state_.line_jump;
            state_.line_jump.reset();
            continue;
        }
        ++pc;
    }
    return leave();
}

}  // namespace timewarp::lang::basic
