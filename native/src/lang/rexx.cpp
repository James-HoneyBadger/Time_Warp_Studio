// SPDX-License-Identifier: MIT
//
// REXX, ported from `time_warp/languages/rexx.py`.
//
// REXX is typeless: every value is a string that may happen to be a number, so
// the value model here is one string plus one number and a flag saying which
// one the program last wrote. That is what makes `X = 42` compare numerically
// while `X = '4 2'` does not, without a type declaration anywhere.
//
// The parser is recursive descent with REXX's three precedence levels that look
// unusual: there is no operator precedence between AND/OR, `||` is explicit
// concatenation while *abutting* two terms also concatenates, and comparisons
// turn both sides into numbers when both look numeric.

#include "timewarp/lang/rexx.hpp"

#include "rexx_internal.hpp"

#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <cstddef>
#include <map>
#include <optional>
#include <regex>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang {

namespace {

using namespace rexx_detail;

// ---------------------------------------------------------------------------

enum class Flow { None, Jump };

struct RexxMachine {
    Interpreter& interpreter;
    TurtleHost& turtle;
    RexxState& state;
    std::vector<std::string>& output;

    Flow exec_statement(const std::string& statement);
    std::optional<Flow> handle_flow(const std::string& text, const std::string& shouted);
    Flow handle_pull(const std::string& text);
    Flow handle_parse_var(const std::string& body);
    Flow handle_if(const std::string& text);
    Flow after_block(std::size_t block_end);
    Flow handle_do(const std::string& text);
    Flow handle_end();
    Flow skip_else();
    Frame* find_select();
    Flow handle_when(const std::string& text);
    Flow handle_otherwise(const std::string& text);
    Flow handle_call(const std::string& name);
    Flow handle_return(const std::string& text);
    std::optional<std::size_t> nearest_loop();
    Flow handle_leave();
    Flow handle_iterate();
    bool handle_turtle(const std::string& shouted, const std::string& text);
    void sync_pending_input();
    Flow run_statements(const std::string& text);
};

void RexxMachine::sync_pending_input() {
    if (!state.waiting.has_value()) {
        return;
    }
    const std::string name = upper(*state.waiting);
    state.waiting.reset();
    std::string stored;
    const auto found = interpreter.variables().find(name + "$");
    if (found != interpreter.variables().end()) {
        stored = found->second.is_text() ? found->second.text() : found->second.render();
    } else {
        const auto number = interpreter.variables().find(name);
        stored = number != interpreter.variables().end() ? number->second.render() : "";
    }
    set_var(interpreter, name, RValue::word(stored));
}

Flow RexxMachine::run_statements(const std::string& text) {
    const auto skip = std::find(state.skip_else_lines.begin(), state.skip_else_lines.end(),
                                interpreter.current_line);
    if (skip != state.skip_else_lines.end()) {
        state.skip_else_lines.erase(skip);
        interpreter.jump_to(after_else_clause(interpreter, interpreter.current_line));
        return Flow::Jump;
    }
    for (const std::string& statement : split_statements(statement_after_label(text))) {
        Flow flow = Flow::None;
        try {
            flow = exec_statement(statement);
        } catch (const RexxError& error) {
            output.push_back(std::string{"❌ REXX error: "} + error.what());
            break;
        }
        if (flow == Flow::Jump) {
            break;
        }
    }
    return Flow::None;
}

Flow RexxMachine::exec_statement(const std::string& statement) {
    const std::string text = trim(statement);
    if (text.empty()) {
        return Flow::None;
    }
    const std::string shouted = upper(text);

    if (shouted == "NOP") {
        return Flow::None;
    }
    if (shouted == "END") {
        return handle_end();
    }
    // An ELSE line executes itself. The interpreter has already decided that this
    // branch is the one to run -- a taken IF's ELSE is skipped before its line is
    // reached -- and `ELSE` alone leaves the statement on the next line to fall
    // through, while `ELSE IF ... THEN` is simply an IF handled by the case below.
    if (starts_with(shouted, "ELSE")) {
        const std::string body = trim(text.substr(4));
        return body.empty() ? Flow::None : exec_statement(body);
    }

    if (const std::optional<Flow> flow = handle_flow(text, shouted)) {
        return *flow;
    }

    if (starts_with(shouted, "SAY")) {
        output.push_back(fmt(eval_expr(interpreter, trim(text.substr(3)))));
        return Flow::None;
    }

    if (shouted == "EXIT" || shouted == "STOP" || starts_with(shouted, "EXIT ") ||
        starts_with(shouted, "STOP ")) {
        interpreter.running = false;
        return Flow::Jump;
    }

    if (handle_turtle(shouted, text)) {
        return Flow::None;
    }

    static const std::regex assignment{R"(^([A-Za-z_!][A-Za-z0-9_!.]*)\s*=(.*)$)",
                                       std::regex::ECMAScript};
    std::smatch match;
    if (std::regex_match(text, match, assignment)) {
        const std::string rest = match[2].str();
        // `X == 5` is a comparison, not an assignment.
        if (!starts_with(rest, "=")) {
            set_var(interpreter, match[1].str(), eval_expr(interpreter, trim(rest)));
            return Flow::None;
        }
    }

    throw RexxError("unrecognised statement: " + text);
}

std::optional<Flow> RexxMachine::handle_flow(const std::string& text,
                                          const std::string& shouted) {
    if (starts_with(shouted, "PULL") || starts_with(shouted, "PARSE PULL")) {
        return handle_pull(text);
    }
    if (starts_with(shouted, "PARSE VAR")) {
        return handle_parse_var(trim(text.substr(9)));
    }
    if (starts_with(shouted, "IF ")) {
        return handle_if(text);
    }
    if (shouted == "DO" || starts_with(shouted, "DO ")) {
        return handle_do(text);
    }
    if (shouted == "SELECT") {
        Frame frame;
        frame.kind = "select";
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }
    if (starts_with(shouted, "WHEN ")) {
        return handle_when(text);
    }
    if (shouted == "OTHERWISE" || starts_with(shouted, "OTHERWISE ")) {
        return handle_otherwise(text);
    }
    if (starts_with(shouted, "CALL ")) {
        return handle_call(trim(text.substr(5)));
    }
    if (shouted == "RETURN" || starts_with(shouted, "RETURN ")) {
        return handle_return(text);
    }
    if (shouted == "LEAVE") {
        return handle_leave();
    }
    if (shouted == "ITERATE") {
        return handle_iterate();
    }
    return std::nullopt;
}

Flow RexxMachine::handle_pull(const std::string& text) {
    static const std::regex pattern{R"(^\s*(PARSE\s+)?PULL\s*)",
                                    std::regex::ECMAScript | std::regex::icase};
    const std::string body = trim(std::regex_replace(text, pattern, ""));
    if (body.empty()) {
        return Flow::None;
    }
    const std::string name = split_ws(body).front();
    state.waiting = name;
    interpreter.request_input("", upper(name) + "$", false);
    return Flow::Jump;
}

Flow RexxMachine::handle_parse_var(const std::string& body) {
    const std::vector<std::string> parts = split_ws(body);
    if (parts.size() < 2) {
        return Flow::None;
    }
    const std::vector<std::string> words = split_ws(fmt(get_var(interpreter, parts[0])));
    for (std::size_t position = 1; position < parts.size(); ++position) {
        set_var(interpreter, parts[position],
                RValue::word(position - 1 < words.size() ? words[position - 1] : ""));
    }
    return Flow::None;
}

Flow RexxMachine::handle_if(const std::string& text) {
    static const std::regex pattern{R"(^IF\s+(.*?)\s+THEN\b(.*)$)",
                                    std::regex::ECMAScript | std::regex::icase};
    std::smatch match;
    if (!std::regex_match(text, match, pattern)) {
        throw RexxError("IF requires a THEN clause");
    }
    const std::string condition = trim(match[1].str());
    const std::string rest = trim(match[2].str());
    const bool taken = truth(eval_expr(interpreter, condition));

    if (starts_with(upper(rest), "DO") && code_words(rest).size() <= 1) {
        const std::size_t block_end = scan_block_end(interpreter, interpreter.current_line);
        if (taken) {
            Frame frame;
            frame.kind = "if_then";
            state.stack.push_back(std::move(frame));
            return Flow::None;  // fall through into the block
        }
        return after_block(block_end);
    }

    if (rest.empty()) {
        // `IF c THEN` with the statement on the next line, which is the shape most
        // REXX is written in. Falling through ran that statement whether or not the
        // condition held, and the `ELSE` after it was then reached as a bare
        // statement and reported as unknown.
        const std::size_t body = interpreter.current_line + 1;
        const auto& lines = interpreter.program_lines();
        if (body >= lines.size()) {
            return Flow::None;
        }
        const std::size_t body_end = after_statement(interpreter, body);
        if (!taken) {
            // Continues on the ELSE line when there is one, past everything when not.
            interpreter.jump_to(body_end);
            return Flow::Jump;
        }
        // A one-line body has no END to end it, so an ELSE after it is skipped by
        // line number when execution arrives there.
        if (body_end < lines.size() &&
            starts_with(upper(statement_after_label(strip_comment(lines[body_end].text))), "ELSE")) {
            state.skip_else_lines.push_back(body_end);
        }
        interpreter.jump_to(body);
        return Flow::Jump;
    }

    // One line: `IF c THEN statement` or `IF c THEN statement ELSE statement`.
    std::string then_statement = rest;
    std::string else_statement;
    static const std::regex else_pattern{R"(^(.*?)\s+ELSE\s+(.*)$)",
                                         std::regex::ECMAScript | std::regex::icase};
    std::smatch else_match;
    if (std::regex_match(rest, else_match, else_pattern)) {
        then_statement = trim(else_match[1].str());
        else_statement = trim(else_match[2].str());
    }

    if (taken) {
        return then_statement.empty() ? Flow::None : exec_statement(then_statement);
    }
    if (!else_statement.empty()) {
        return exec_statement(else_statement);
    }
    return Flow::None;
}

Flow RexxMachine::after_block(std::size_t block_end) {
    // The branch above this one was not taken, so execution continues on the line
    // after its body. When that line is an `ELSE`, it is executed like any other
    // statement -- it decides what to run and this function does not need to know
    // which of the four shapes of ELSE it is looking at.
    interpreter.jump_to(block_end + 1);
    return Flow::Jump;
}

Flow RexxMachine::handle_do(const std::string& text) {
    const std::size_t do_index = interpreter.current_line;
    static const std::regex prefix{R"(^DO\b)", std::regex::ECMAScript | std::regex::icase};
    const std::string spec = trim(std::regex_replace(text, prefix, ""));
    const std::string spec_upper = upper(spec);

    if (starts_with(spec_upper, "WHILE ")) {
        const std::string condition = trim(spec.substr(6));
        if (!truth(eval_expr(interpreter, condition))) {
            interpreter.jump_to(scan_block_end(interpreter, do_index) + 1);
            return Flow::Jump;
        }
        Frame frame;
        frame.kind = "while";
        frame.do_index = do_index;
        frame.has_do_index = true;
        frame.condition = condition;
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }
    if (starts_with(spec_upper, "UNTIL ")) {
        Frame frame;
        frame.kind = "until";
        frame.do_index = do_index;
        frame.has_do_index = true;
        frame.condition = trim(spec.substr(6));
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }
    if (starts_with(spec_upper, "FOREVER")) {
        Frame frame;
        frame.kind = "forever";
        frame.do_index = do_index;
        frame.has_do_index = true;
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }

    static const std::regex control{
        R"(^([A-Za-z_!][A-Za-z0-9_!.]*)\s*=\s*(.+?)\s+TO\s+(.+?)(?:\s+BY\s+(.+?))?(?:\s+FOR\s+.+)?$)",
        std::regex::ECMAScript | std::regex::icase};
    std::smatch match;
    if (std::regex_match(spec, match, control)) {
        set_var(interpreter, match[1].str(), eval_expr(interpreter, trim(match[2].str())));
        Frame frame;
        frame.kind = "to";
        frame.do_index = do_index;
        frame.has_do_index = true;
        frame.variable = match[1].str();
        frame.limit = eval_expr(interpreter, trim(match[3].str()));
        frame.step = match[4].matched ? eval_expr(interpreter, trim(match[4].str()))
                                      : RValue::numeric(1.0);
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }

    static const std::regex over{R"(^([A-Za-z_!][A-Za-z0-9_!.]*)\s+OVER\s+(.+)$)",
                                 std::regex::ECMAScript | std::regex::icase};
    if (std::regex_match(spec, match, over)) {
        const std::vector<std::string> items =
            split_ws(fmt(eval_expr(interpreter, trim(match[2].str()))));
        if (items.empty()) {
            interpreter.jump_to(scan_block_end(interpreter, do_index) + 1);
            return Flow::Jump;
        }
        set_var(interpreter, match[1].str(), RValue::word(items.front()));
        Frame frame;
        frame.kind = "over";
        frame.do_index = do_index;
        frame.has_do_index = true;
        frame.variable = match[1].str();
        frame.items = items;
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }

    if (!spec.empty()) {
        const long long count = static_cast<long long>(num(eval_expr(interpreter, spec)));
        if (count <= 0) {
            interpreter.jump_to(scan_block_end(interpreter, do_index) + 1);
            return Flow::Jump;
        }
        Frame frame;
        frame.kind = "count";
        frame.do_index = do_index;
        frame.has_do_index = true;
        frame.count = count;
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }

    Frame frame;
    frame.kind = "block";
    frame.do_index = do_index;
    state.stack.push_back(std::move(frame));
    return Flow::None;
}

Flow RexxMachine::handle_end() {
    if (state.stack.empty()) {
        return Flow::None;
    }
    Frame frame = state.stack.back();
    state.stack.pop_back();
    const std::string& kind = frame.kind;
    const std::size_t do_index =
        frame.has_do_index ? frame.do_index : interpreter.current_line;

    if (kind == "if_then") {
        return skip_else();
    }
    if (kind == "block" || kind == "select" || kind == "else_body" ||
        kind == "when_body" || kind == "otherwise_body") {
        return Flow::None;
    }
    if (kind == "while") {
        if (truth(eval_expr(interpreter, frame.condition))) {
            state.stack.push_back(frame);
            interpreter.jump_to(do_index + 1);
            return Flow::Jump;
        }
        return Flow::None;
    }
    if (kind == "until") {
        if (truth(eval_expr(interpreter, frame.condition))) {
            return Flow::None;
        }
        state.stack.push_back(frame);
        interpreter.jump_to(do_index + 1);
        return Flow::Jump;
    }
    if (kind == "forever") {
        state.stack.push_back(frame);
        interpreter.jump_to(do_index + 1);
        return Flow::Jump;
    }
    if (kind == "count") {
        frame.count -= 1;
        if (frame.count > 0) {
            state.stack.push_back(frame);
            interpreter.jump_to(do_index + 1);
            return Flow::Jump;
        }
        return Flow::None;
    }
    if (kind == "to") {
        const double step = num(frame.step);
        const double current = num(get_var(interpreter, frame.variable)) +
                               (step == 0.0 ? 1.0 : step);
        const double limit = num(frame.limit);
        if ((step >= 0.0 && current <= limit) || (step < 0.0 && current >= limit)) {
            set_var(interpreter, frame.variable, RValue::numeric(current));
            state.stack.push_back(frame);
            interpreter.jump_to(do_index + 1);
            return Flow::Jump;
        }
        return Flow::None;
    }
    if (kind == "over") {
        ++frame.index;
        if (frame.index < frame.items.size()) {
            set_var(interpreter, frame.variable, RValue::word(frame.items[frame.index]));
            state.stack.push_back(frame);
            interpreter.jump_to(do_index + 1);
            return Flow::Jump;
        }
        return Flow::None;
    }
    return Flow::None;
}

Flow RexxMachine::skip_else() {
    const auto& lines = interpreter.program_lines();
    const std::size_t next = interpreter.current_line + 1;
    if (next >= lines.size()) {
        return Flow::None;
    }
    const std::string rest = statement_after_label(strip_comment(lines[next].text));
    static const std::regex else_pattern{R"(^ELSE\b)",
                                         std::regex::ECMAScript | std::regex::icase};
    if (!std::regex_search(rest, else_pattern)) {
        return Flow::None;
    }
    // The whole clause, not just its first line: a bare `ELSE` carries its body on
    // the next line and `ELSE IF ... THEN` carries a chain, and both were skipped by
    // one line -- which ran the else body after the then body had already run.
    interpreter.jump_to(after_else_clause(interpreter, next));
    return Flow::Jump;
}

Frame* RexxMachine::find_select() {
    for (auto it = state.stack.rbegin(); it != state.stack.rend(); ++it) {
        if (it->kind == "select") {
            return &*it;
        }
    }
    return nullptr;
}

Flow RexxMachine::handle_when(const std::string& text) {
    Frame* frame = find_select();
    if (frame == nullptr) {
        throw RexxError("WHEN outside of SELECT");
    }
    static const std::regex pattern{R"(^WHEN\s+(.*?)\s+THEN\b(.*)$)",
                                    std::regex::ECMAScript | std::regex::icase};
    std::smatch match;
    if (!std::regex_match(text, match, pattern)) {
        throw RexxError("WHEN requires a THEN clause");
    }
    const std::string condition = trim(match[1].str());
    const std::string body = trim(match[2].str());
    const std::string body_upper = upper(body);

    if (frame->matched || !truth(eval_expr(interpreter, condition))) {
        if (starts_with(body_upper, "DO")) {
            interpreter.jump_to(scan_block_end(interpreter, interpreter.current_line) + 1);
            return Flow::Jump;
        }
        return Flow::None;
    }

    frame->matched = true;
    if (starts_with(body_upper, "DO") && code_words(body).size() <= 1) {
        Frame body_frame;
        body_frame.kind = "when_body";
        state.stack.push_back(std::move(body_frame));
        return Flow::None;
    }
    return body.empty() ? Flow::None : exec_statement(body);
}

Flow RexxMachine::handle_otherwise(const std::string& text) {
    Frame* frame = find_select();
    if (frame == nullptr) {
        throw RexxError("OTHERWISE outside of SELECT");
    }
    static const std::regex prefix{R"(^OTHERWISE\b)",
                                   std::regex::ECMAScript | std::regex::icase};
    const std::string body = trim(std::regex_replace(text, prefix, ""));
    const std::string body_upper = upper(body);

    if (frame->matched) {
        if (starts_with(body_upper, "DO")) {
            interpreter.jump_to(scan_block_end(interpreter, interpreter.current_line) + 1);
            return Flow::Jump;
        }
        return Flow::None;
    }

    frame->matched = true;
    if (starts_with(body_upper, "DO") && code_words(body).size() <= 1) {
        Frame body_frame;
        body_frame.kind = "otherwise_body";
        state.stack.push_back(std::move(body_frame));
        return Flow::None;
    }
    return body.empty() ? Flow::None : exec_statement(body);
}

Flow RexxMachine::handle_call(const std::string& name) {
    static const std::regex pattern{
        R"(^([A-Za-z_!?][A-Za-z0-9_!?.]*)\s*(?:\((.*)\))?\s*(.*)$)",
        std::regex::ECMAScript};
    std::smatch match;
    if (!std::regex_match(name, match, pattern)) {
        throw RexxError("malformed CALL statement");
    }
    const std::string label = upper(match[1].str());
    const std::string argument_text =
        match[2].matched ? match[2].str() : match[3].str();
    std::vector<RValue> arguments;
    for (const std::string& part : split_args(argument_text)) {
        // The reference stores CALL arguments as the strings they will be read
        // back as by ARG(n), so `n = ARG(1)` assigns text, not a number.
        arguments.push_back(RValue::word(fmt(eval_expr(interpreter, part))));
    }
    const std::map<std::string, std::size_t> table = labels(interpreter);
    const auto target = table.find(label);
    if (target == table.end()) {
        throw RexxError("unknown subroutine '" + label + "'");
    }
    state.call_stack.emplace_back(interpreter.current_line, state.args);
    state.args = arguments;
    interpreter.jump_to(target->second + 1);
    return Flow::Jump;
}

Flow RexxMachine::handle_return(const std::string& text) {
    static const std::regex prefix{R"(^RETURN\b)",
                                   std::regex::ECMAScript | std::regex::icase};
    const std::string expression = trim(std::regex_replace(text, prefix, ""));
    if (!expression.empty()) {
        set_var(interpreter, "RESULT", eval_expr(interpreter, expression));
    }
    if (!state.call_stack.empty()) {
        const auto [return_line, previous_args] = state.call_stack.back();
        state.call_stack.pop_back();
        state.args = previous_args;
        interpreter.jump_to(return_line + 1);
        return Flow::Jump;
    }
    interpreter.running = false;
    return Flow::Jump;
}

std::optional<std::size_t> RexxMachine::nearest_loop() {
    for (std::size_t index = state.stack.size(); index-- > 0;) {
        if (is_loop(state.stack[index].kind)) {
            return index;
        }
    }
    return std::nullopt;
}

Flow RexxMachine::handle_leave() {
    const std::optional<std::size_t> found = nearest_loop();
    if (!found.has_value()) {
        throw RexxError("LEAVE outside of a loop");
    }
    const Frame frame = state.stack[*found];
    state.stack.erase(state.stack.begin() + static_cast<std::ptrdiff_t>(*found),
                      state.stack.end());
    interpreter.jump_to(scan_block_end(interpreter, frame.do_index) + 1);
    return Flow::Jump;
}

Flow RexxMachine::handle_iterate() {
    const std::optional<std::size_t> found = nearest_loop();
    if (!found.has_value()) {
        throw RexxError("ITERATE outside of a loop");
    }
    const Frame frame = state.stack[*found];
    interpreter.jump_to(scan_block_end(interpreter, frame.do_index));
    return Flow::Jump;
}

bool RexxMachine::handle_turtle(const std::string& shouted, const std::string& text) {
    const std::vector<std::string> words = split_ws(shouted);
    const std::string keyword = words.empty() ? "" : words.front();
    const std::string argument = trim(text.substr(std::min(text.size(), keyword.size())));
    const auto amount = [this, &argument]() {
        return argument.empty() ? 0.0 : num(eval_expr(interpreter, argument));
    };
    if (keyword == "FORWARD" || keyword == "FD" || keyword == "MOVE") {
        turtle.forward(amount());
        return true;
    }
    if (keyword == "BACK" || keyword == "BK") {
        turtle.back(amount());
        return true;
    }
    if (keyword == "RIGHT" || keyword == "RT" || keyword == "TURN") {
        turtle.right(amount());
        return true;
    }
    if (keyword == "LEFT" || keyword == "LT") {
        turtle.left(amount());
        return true;
    }
    if (keyword == "PENUP") {
        turtle.pen_up();
        return true;
    }
    if (keyword == "PENDOWN") {
        turtle.pen_down();
        return true;
    }
    if (keyword == "HOME") {
        turtle.home();
        return true;
    }
    return false;
}


}  // namespace

std::string execute_rexx(Interpreter& interpreter, std::string_view source,
                         TurtleHost& turtle) {
    RexxState& state = interpreter.executor_state<RexxState>();
    std::vector<std::string> output;
    RexxMachine executor{interpreter, turtle, state, output};
    executor.sync_pending_input();

    const std::string raw = strip_comment(std::string{source});
    if (trim(raw).empty()) {
        return "";
    }

    executor.run_statements(raw);

    if (output.empty()) {
        return "";
    }
    std::string joined;
    for (const std::string& line : output) {
        if (!joined.empty()) {
            joined.push_back('\n');
        }
        joined += line;
    }
    joined.push_back('\n');
    return joined;
}

}  // namespace timewarp::lang
