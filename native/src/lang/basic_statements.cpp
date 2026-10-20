// SPDX-License-Identifier: MIT
//
// The BASIC statement dispatch, ported from `execute_basic` in
// `time_warp/languages/basic.py`.
//
// The order of the tests is behaviour, not style: `PRINT USING` has to be read
// before `PRINT`, `LINE INPUT` before `INPUT`, and the implicit-`LET` test
// before `COLOR`, or a statement silently becomes a different one. It is copied
// as it stands for that reason.

#include "basic_internal.hpp"

#include "timewarp/lang/logo.hpp"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <regex>
#include <string>
#include <utility>

namespace timewarp::lang::basic {

// -- helpers shared with basic.cpp -----------------------------------------

bool is_all_digits(std::string_view text) {
    if (text.empty()) {
        return false;
    }
    for (const char character : text) {
        if (std::isdigit(static_cast<unsigned char>(character)) == 0) {
            return false;
        }
    }
    return true;
}

/// The reference's `is_logo_line`: a statement worth handing to Logo.
bool is_logo_line(std::string_view command) {
    const std::string text = trim(command);
    if (text.empty()) {
        return false;
    }
    const std::size_t space = text.find_first_of(" \t");
    const std::string first = upper(text.substr(0, space));
    if (is_logo_command(first)) {
        return true;
    }
    if (text.rfind(":", 0) == 0 || text.front() == '"' || text.front() == ';') {
        return true;
    }
    // A bracketed body is a Logo block however its head is spelled.
    return text.find('[') != std::string::npos || text.find(']') != std::string::npos;
}

bool division_by_zero(std::string_view message) {
    return message.rfind("Division by zero", 0) == 0;
}

std::string strip_quotes(std::string_view text) {
    std::size_t start = 0;
    std::size_t end = text.size();
    while (start < end && text[start] == '"') {
        ++start;
    }
    while (end > start && text[end - 1] == '"') {
        --end;
    }
    return std::string{text.substr(start, end - start)};
}

bool has_string_concat(std::string_view expression) {
    bool in_string = false;
    char quote = '\0';
    int depth = 0;
    bool has_text = false;
    bool has_plus = false;
    for (const char character : expression) {
        if (in_string) {
            if (character == quote) {
                in_string = false;
            }
        } else if (character == '"' || character == '\'') {
            in_string = true;
            quote = character;
            has_text = true;
        } else if (character == '(') {
            ++depth;
        } else if (character == ')') {
            --depth;
        } else if (character == '+' && depth == 0) {
            has_plus = true;
        }
    }
    if (expression.find('$') != std::string_view::npos) {
        has_text = true;
    }
    return has_text && has_plus;
}

bool mentions_string_function(std::string_view upper_expression) {
    static const char* const heads[] = {
        "LEN(",     "LEFT(",   "RIGHT(",    "MID(",       "INSTR(",    "UPPER(",
        "LOWER(",   "LCASE(",  "LCASE$(",   "UCASE(",     "UCASE$(",   "TRIM(",
        "LTRIM(",   "RTRIM(",  "LPAD(",     "LPAD$(",     "RPAD(",     "RPAD$(",
        "FORMAT(",  "FORMAT$(", "ENVIRON(", "ENVIRON$(",  "REPEAT(",   "REPEAT$(",
        "SPACE(",   "SPACE$(", "STRING$(",  "STR(",       "VAL(",      "CHR(",
        "CHR$("};
    for (const char* head : heads) {
        if (upper_expression.rfind(head, 0) == 0) {
            return true;
        }
        // `MID$(...` is the documented spelling; the reference's pattern allows
        // the `$` on every name, so it is allowed here on every name too.
        const std::string name{head};
        if (name.size() >= 2 && name.compare(name.size() - 2, 2, "$(") != 0) {
            const std::string with_dollar = name.substr(0, name.size() - 1) + "$(";
            if (upper_expression.rfind(with_dollar, 0) == 0) {
                return true;
            }
        }
    }
    return false;
}

std::optional<std::string> string_operand(Machine& machine, std::string_view expression) {
    const std::string expr = trim(expression);
    if (expr.size() >= 2 && expr.front() == '"' && expr.back() == '"') {
        return expr.substr(1, expr.size() - 2);
    }
    if (!expr.empty() && expr.back() == '$') {
        const auto found = machine.interpreter().variables().find(upper(expr));
        if (found != machine.interpreter().variables().end() && found->second.is_text()) {
            return found->second.text();
        }
        return std::string{};
    }
    if (expr.find("$(") != std::string::npos) {
        try {
            Strings strings{machine.interpreter(), machine.state()};
            return strings.evaluate(expr);
        } catch (const BasicError&) {
            return std::nullopt;
        }
    }
    return std::nullopt;
}

int try_string_comparison(Machine& machine, std::string_view condition) {
    const std::string text{condition};
    if (text.find('$') == std::string::npos && text.find("\"\"") == std::string::npos) {
        return -1;
    }
    struct Operator {
        const char* symbol;
        int kind;  // 0 ne, 1 ge, 2 le, 3 eq, 4 lt, 5 gt
    };
    static const Operator operators[] = {
        {"<>", 0}, {">=", 1}, {"<=", 2}, {"=", 3}, {"<", 4}, {">", 5}};
    for (const Operator& op : operators) {
        const std::string symbol{op.symbol};
        const std::size_t at = text.find(symbol);
        if (at == std::string::npos) {
            continue;
        }
        const auto lhs = string_operand(machine, text.substr(0, at));
        const auto rhs = string_operand(machine, text.substr(at + symbol.size()));
        if (!lhs.has_value() || !rhs.has_value()) {
            continue;
        }
        const int comparison = lhs->compare(*rhs);
        switch (op.kind) {
            case 0:
                return comparison != 0 ? 1 : 0;
            case 1:
                return comparison >= 0 ? 1 : 0;
            case 2:
                return comparison <= 0 ? 1 : 0;
            case 3:
                return comparison == 0 ? 1 : 0;
            case 4:
                return comparison < 0 ? 1 : 0;
            default:
                return comparison > 0 ? 1 : 0;
        }
    }
    return -1;
}

std::string substitute_undefined(std::string_view expression, std::string_view name) {
    std::string result;
    const std::string text{expression};
    const auto word = [](char character) {
        return std::isalnum(static_cast<unsigned char>(character)) != 0 || character == '_';
    };
    std::size_t index = 0;
    while (index < text.size()) {
        const bool before = index == 0 || !word(text[index - 1]);
        if (before && text.compare(index, name.size(), name) == 0) {
            const std::size_t after = index + name.size();
            if (after >= text.size() || !word(text[after])) {
                result.push_back('0');
                index = after;
                continue;
            }
        }
        result.push_back(text[index]);
        ++index;
    }
    return result;
}

namespace {

/// `int(text)`: whole-string, optional sign, no fraction.
std::optional<long> parse_integer(std::string_view text) {
    const std::string trimmed = trim(text);
    if (trimmed.empty()) {
        return std::nullopt;
    }
    const char* begin = trimmed.c_str();
    char* end = nullptr;
    errno = 0;
    const long value = std::strtol(begin, &end, 10);
    if (end == begin || *end != '\0' || errno == ERANGE) {
        return std::nullopt;
    }
    return value;
}

std::string join(const std::vector<std::string>& values, std::string_view separator) {
    std::string result;
    for (std::size_t index = 0; index < values.size(); ++index) {
        if (index != 0) {
            result += separator;
        }
        result += values[index];
    }
    return result;
}

/// The first word of a command, for the "Did you mean" hint the reference
/// appends to an unknown statement.
bool starts_with(std::string_view text, std::string_view prefix) {
    return text.rfind(prefix, 0) == 0;
}

/// `text.substr(from)` that answers "" instead of throwing when the command is
/// shorter than the keyword -- `CLOSE` is five characters and its handler slices
/// from six.
std::string tail(std::string_view text, std::size_t from) {
    return from >= text.size() ? std::string{} : std::string{text.substr(from)};
}

}  // namespace

// -- the dispatch ----------------------------------------------------------

std::string Machine::execute(std::string_view command) {
    scan_program();
    install_functions();

    std::string text = trim(command);
    // Classic BASIC lets `?` stand in for PRINT at the start of a statement.
    if (!text.empty() && text.front() == '?') {
        text = "PRINT" + text.substr(1);
    }
    if (text.empty()) {
        return "❌ Unknown BASIC command: " + text + "\n";
    }
    if (text.back() == ':' && text.find(' ') == std::string::npos) {
        return {};  // a label on its own line
    }
    const std::string all_upper = upper(text);
    if (text.find(':') != std::string::npos && !starts_with(all_upper, "REM") &&
        !starts_with(all_upper, "'") && !starts_with(all_upper, "IF ")) {
        const std::vector<std::string> statements = split_statements(text);
        if (statements.size() > 1) {
            std::string output;
            for (const std::string& statement : statements) {
                output += execute(statement);
            }
            return output;
        }
    }

    const std::string cmd = upper(text);

    // -- block IF skip mode ------------------------------------------------
    if (!state_.ifs.empty()) {
        IfFrame& top = state_.ifs.back();
        if (top.skip) {
            const std::string trimmed_right = rstrip(cmd);
            const bool opens_block =
                starts_with(cmd, "IF ") && trimmed_right.size() >= 4 &&
                trimmed_right.compare(trimmed_right.size() - 4, 4, "THEN") == 0;
            if (opens_block) {
                ++top.depth;
                return {};
            }
            const int depth = top.depth;
            if (cmd == "ENDIF" || cmd == "END IF") {
                if (depth > 0) {
                    --top.depth;
                    return {};
                }
                state_.ifs.pop_back();
                return {};
            }
            if (cmd == "ELSE" && depth == 0) {
                top.skip = false;
                return {};
            }
            return {};
        }
        if (cmd == "ENDIF" || cmd == "END IF") {
            state_.ifs.pop_back();
            return {};
        }
        if (cmd == "ELSE") {
            top.skip = true;
            return {};
        }
    } else if (cmd == "ENDIF" || cmd == "END IF" || cmd == "ELSE") {
        return {};
    }

    // -- comments ----------------------------------------------------------
    if (starts_with(cmd, "REM") || starts_with(cmd, "'")) {
        return {};
    }

    // -- output ------------------------------------------------------------
    if (starts_with(cmd, "PRINT USING ")) {
        return print_using(strip_comment(tail(text, 12)));
    }
    if (starts_with(cmd, "PRINT #") || starts_with(cmd, "PRINT  #")) {
        return print_to_file(strip_comment(tail(text, 5)));
    }
    if (starts_with(cmd, "WRITE #") || starts_with(cmd, "WRITE  #")) {
        return print_to_file(strip_comment(tail(text, 5)));
    }
    if (starts_with(cmd, "PRINT ") || cmd == "PRINT") {
        return print(strip_comment(tail(text, 6)));
    }
    if (starts_with(cmd, "LINE INPUT ")) {
        return line_input(strip_comment(tail(text, 11)));
    }
    if (starts_with(cmd, "INPUT #") || starts_with(cmd, "INPUT  #")) {
        return input_file(strip_comment(tail(text, 5)));
    }
    if (starts_with(cmd, "INPUT ")) {
        return input(strip_comment(tail(text, 6)));
    }

    // -- control flow ------------------------------------------------------
    if (starts_with(cmd, "IF ")) {
        return conditional(strip_comment(tail(text, 3)));
    }
    if (starts_with(cmd, "GOTO ")) {
        return goto_line(strip_comment(tail(text, 5)));
    }
    if (starts_with(cmd, "GOSUB ")) {
        return gosub(strip_comment(tail(text, 6)));
    }
    if (cmd == "RETURN") {
        return return_from_sub();
    }
    if (cmd == "END") {
        interpreter_.running = false;
        return {};
    }
    if (starts_with(cmd, "LET ")) {
        return let(strip_comment(tail(text, 4)));
    }
    if (starts_with(cmd, "FOR ")) {
        return for_loop(strip_comment(tail(text, 4)));
    }
    if (is_keyword(cmd, "NEXT")) {
        return next(strip_comment(tail(text, 4)));
    }
    if (starts_with(cmd, "WHILE ")) {
        return while_loop(strip_comment(tail(text, 6)));
    }
    if (cmd == "WEND") {
        return wend();
    }
    if (is_keyword(cmd, "DO")) {
        return do_loop(strip_comment(tail(text, 3)));
    }
    if (is_keyword(cmd, "LOOP")) {
        return loop(strip_comment(tail(text, 5)));
    }
    if (starts_with(cmd, "SELECT ")) {
        return select_case(strip_comment(tail(text, 11)));
    }
    if (starts_with(cmd, "CASE ")) {
        return case_of(strip_comment(tail(text, 5)));
    }
    if (cmd == "END SELECT") {
        state_.in_select = false;
        state_.select_expression.clear();
        return {};
    }
    if (starts_with(cmd, "SUB ")) {
        return sub_definition(strip_comment(tail(text, 4)));
    }
    if (cmd == "END SUB") {
        return end_sub();
    }
    if (starts_with(cmd, "FUNCTION ")) {
        return function_definition(strip_comment(tail(text, 9)));
    }
    if (cmd == "END FUNCTION") {
        return end_function();
    }
    if (starts_with(cmd, "CALL ")) {
        return call(strip_comment(tail(text, 5)));
    }
    if (is_keyword(cmd, "RUN")) {
        return reset_program("🚀 Program reset\n");
    }
    if (is_keyword(cmd, "SYSTEM")) {
        return reset_program("ℹ️ Returned to system prompt\n");
    }

    // -- declarations ------------------------------------------------------
    if (starts_with(cmd, "DIM ")) {
        return dim(strip_comment(tail(text, 4)));
    }
    if (starts_with(cmd, "TYPE ") && !starts_with(cmd, "TYPE$")) {
        return type_definition(strip_comment(tail(text, 5)));
    }
    if (cmd == "END TYPE" || cmd == "ENDTYPE") {
        return {};
    }
    if (all_upper.find('=') != std::string::npos && !starts_with(cmd, "IF ") &&
        !starts_with(cmd, "FOR ") && !starts_with(cmd, "DEF FN") &&
        !starts_with(cmd, "CONST ")) {
        return let(strip_comment(text));
    }

    // -- screen ------------------------------------------------------------
    if (starts_with(cmd, "COLOR ")) {
        return color(strip_comment(tail(text, 6)));
    }
    if (starts_with(cmd, "PALETTE ")) {
        return palette(strip_comment(tail(text, 8)));
    }
    if (starts_with(cmd, "WIDTH ")) {
        return width(strip_comment(tail(text, 6)));
    }

    // -- files -------------------------------------------------------------
    if (starts_with(cmd, "OPEN ")) {
        return open_file(strip_comment(tail(text, 5)));
    }
    if (is_keyword(cmd, "CLOSE")) {
        return close_files(strip_comment(tail(text, 6)));
    }
    if (starts_with(cmd, "GET ")) {
        return get_record(strip_comment(tail(text, 4)));
    }
    if (starts_with(cmd, "PUT ")) {
        return put_record(strip_comment(tail(text, 4)));
    }

    // -- graphics ----------------------------------------------------------
    if (starts_with(cmd, "REM ") || cmd == "REM") {
        return {};
    }
    if (cmd == "CLS") {
        turtle_.clear();
        return "🎨 Screen cleared\n";
    }
    if (starts_with(cmd, "SCREEN ")) {
        return screen(strip_comment(tail(text, 7)));
    }
    if (starts_with(cmd, "LOCATE ")) {
        return locate(strip_comment(tail(text, 7)));
    }
    if (starts_with(cmd, "LINE ")) {
        return draw_line(strip_comment(tail(text, 5)));
    }
    if (starts_with(cmd, "CIRCLE ")) {
        return draw_circle(strip_comment(tail(text, 7)));
    }
    if (starts_with(cmd, "PSET ")) {
        return draw_point(strip_comment(tail(text, 5)), true);
    }
    if (starts_with(cmd, "PRESET ")) {
        return draw_point(strip_comment(tail(text, 7)), false);
    }
    if (starts_with(cmd, "PAINT ")) {
        return paint(strip_comment(tail(text, 6)));
    }

    // -- data --------------------------------------------------------------
    if (starts_with(cmd, "DATA ")) {
        return {};
    }
    if (starts_with(cmd, "READ ")) {
        return read_data(strip_comment(tail(text, 5)));
    }
    if (is_keyword(cmd, "RESTORE")) {
        return restore();
    }
    if (starts_with(cmd, "SWAP ")) {
        return swap(strip_comment(tail(text, 5)));
    }
    if (starts_with(cmd, "ERASE ")) {
        return erase(strip_comment(tail(text, 6)));
    }
    if (starts_with(cmd, "CONST ")) {
        return constant(strip_comment(tail(text, 6)));
    }

    // -- type defaults -----------------------------------------------------
    if (starts_with(cmd, "DEFINT ")) {
        return define_types("int", strip_comment(tail(text, 7)));
    }
    if (starts_with(cmd, "DEFLNG ")) {
        return define_types("long", strip_comment(tail(text, 7)));
    }
    if (starts_with(cmd, "DEFSNG ")) {
        return define_types("single", strip_comment(tail(text, 7)));
    }
    if (starts_with(cmd, "DEFDBL ")) {
        return define_types("double", strip_comment(tail(text, 7)));
    }
    if (starts_with(cmd, "DEFSTR ")) {
        return define_types("string", strip_comment(tail(text, 7)));
    }
    if (starts_with(cmd, "DEF FN") || starts_with(cmd, "DEF  FN")) {
        return define_fn(strip_comment(text));
    }

    // -- game support ------------------------------------------------------
    if (cmd == "BEEP") {
        // The acknowledgement is the reference's; the sound is the native
        // build's, because a host without audio ignores it rather than lying.
        turtle_.beep(120);
        return "🔊 BEEP\n";
    }
    if (starts_with(cmd, "SOUND ")) {
        return sound(strip_comment(tail(text, 6)));
    }
    if (starts_with(cmd, "DRAW ")) {
        return draw_path(strip_comment(tail(text, 5)));
    }
    if (starts_with(cmd, "WAIT ")) {
        return wait(strip_comment(tail(text, 5)));
    }
    if (starts_with(cmd, "SERIAL ")) {
        return serial(strip_comment(tail(text, 7)));
    }
    if (starts_with(cmd, "PARALLEL ")) {
        return parallel(strip_comment(tail(text, 9)));
    }
    if (starts_with(cmd, "SPEED ")) {
        return speed(strip_comment(tail(text, 6)));
    }
    if (starts_with(cmd, "SPRITE ")) {
        return sprite(strip_comment(tail(text, 7)));
    }
    if (starts_with(cmd, "ON TIMER")) {
        return on_timer(strip_comment(tail(text, 8)));
    }
    if (cmd == "TIMER ON") {
        state_.timer_enabled = true;
        return "⏱️ Timer enabled\n";
    }
    if (cmd == "TIMER OFF") {
        state_.timer_enabled = false;
        return "⏱️ Timer disabled\n";
    }

    // -- media -------------------------------------------------------------
    if (starts_with(cmd, "PLAY ")) {
        return play(strip_comment(tail(text, 5)));
    }
    if (starts_with(cmd, "SAY ")) {
        return say(strip_comment(tail(text, 4)));
    }
    if (starts_with(cmd, "SPEAK ")) {
        return say(strip_comment(tail(text, 6)));
    }
    if (starts_with(cmd, "SHAPE ")) {
        return shape(strip_comment(tail(text, 6)));
    }
    if (starts_with(cmd, "PARTICLE ")) {
        return particle(strip_comment(tail(text, 9)));
    }
    if (cmd == "FRACTAL" || starts_with(cmd, "FRACTAL ")) {
        return fractal(trim(strip_comment(tail(text, 7))));
    }
    if (is_keyword(cmd, "JOYINIT")) {
        return "ℹ️ No gamepad backend available (optional: install pygame or inputs)\n";
    }

    // -- simulated hardware ------------------------------------------------
    if (starts_with(cmd, "POKE ")) {
        return poke(strip_comment(tail(text, 5)));
    }
    if (starts_with(cmd, "PEEK ")) {
        return peek(strip_comment(tail(text, 5)));
    }
    if (starts_with(cmd, "OUT ")) {
        return out(strip_comment(tail(text, 4)));
    }
    if (starts_with(cmd, "IN ")) {
        return in_port(strip_comment(tail(text, 3)));
    }
    if (starts_with(cmd, "SHELL ")) {
        return shell(strip_comment(tail(text, 6)));
    }

    // -- error handling ----------------------------------------------------
    if (starts_with(cmd, "ON ERROR GOTO")) {
        return on_error_goto(strip_comment(tail(text, 13)));
    }
    if (cmd == "ON ERROR") {
        state_.error_handler = 0;
        return {};
    }
    if (starts_with(cmd, "RESUME NEXT")) {
        return resume_next();
    }
    if (is_keyword(cmd, "RESUME")) {
        return resume(strip_comment(tail(text, 6)));
    }

    // -- misc --------------------------------------------------------------
    if (is_keyword(cmd, "RANDOMIZE")) {
        // The native evaluator's RND is its own stream; there is no seed to
        // share with the reference, and a program that prints RND is a program
        // the parity gate skips by design.
        return {};
    }
    if (is_keyword(cmd, "SLEEP")) {
        const std::string rest = trim(tail(cmd, 5));
        if (!rest.empty()) {
            try {
                const double seconds = eval(rest);
                char buffer[32];
                std::snprintf(buffer, sizeof buffer, "%0.1f", seconds);
                return "ℹ️ SLEEP " + std::string{buffer} + "s\n";
            } catch (const std::exception&) {
                // fall through to the bare message
            }
        }
        return "ℹ️ SLEEP\n";
    }
    if (starts_with(cmd, "SETXY ")) {
        return set_xy(tail(text, 6));
    }

    // Logo's `MAKE` is the one Logo command the guide documents inside BASIC: a
    // BASIC program that names a variable this way keeps it.
    if (starts_with(cmd, "MAKE ")) {
        return make(strip_comment(tail(text, 5)));
    }

    // -- the Logo blend ------------------------------------------------
    // The reference forwards a statement it does not recognise to the Logo
    // executor when it looks like one, so `FORWARD 15` in a `.bas` file draws
    // with Logo's turtle code. It is not a silent catch-all: only a first word
    // Logo knows (plus a bracketed body, a `;` comment, `:`/`"` words) is
    // forwarded, and a real typo still comes back as an unknown command.
    if (is_logo_line(text)) {
        return execute_logo_command(interpreter_, text, turtle_);
    }

    return "❌ Unknown BASIC command: " + text + "\n";
}

// -- output ----------------------------------------------------------------

std::string Machine::print(const std::string& args_in) {
    if (trim(args_in).empty()) {
        return "\n";
    }
    const std::string args = trim(args_in);
    const bool suppress_newline = args.back() == ';' || args.back() == ',';
    const std::vector<std::string> parts = split_top(args_in, ";,");
    if (parts.empty()) {
        return "\n";
    }
    // The reference's `_print_expression_failure`: an unset numeric variable is
    // 0, and anything else becomes an error line that replaces the whole PRINT.
    const auto numeric = [this](const std::string& item,
                                std::string& error) -> std::optional<double> {
        try {
            return eval(item);
        } catch (const std::exception& thrown) {
            const std::string message = thrown.what();
            if (division_by_zero(message)) {
                throw;
            }
            if (starts_with(message, "Undefined variable: ")) {
                const std::string name = trim(message.substr(20));
                const std::string retried = substitute_undefined(item, name);
                if (retried != item) {
                    try {
                        return eval(retried);
                    } catch (const std::exception& inner) {
                        const std::string inner_message = inner.what();
                        if (division_by_zero(inner_message)) {
                            error = "❌ Division by zero\n";
                            return std::nullopt;
                        }
                        error = "❌ " + inner_message + "\n";
                        return std::nullopt;
                    }
                }
                return 0.0;
            }
            // Anything else is reported rather than turned into a value: the
            // reference hands the evaluator's own words to the learner, which
            // is how a mistyped function shows up as `❌ Unexpected character`.
            error = "❌ " + message + "\n";
            return std::nullopt;
        }
    };

    std::vector<std::string> items;
    for (const std::string& raw : parts) {
        const std::string item = trim(raw);
        const std::string item_upper = upper(item);

        if (has_string_concat(item) || mentions_string_function(item_upper)) {
            try {
                Strings strings{interpreter_, state_};
                items.push_back(strings.evaluate(item));
                continue;
            } catch (const BasicError&) {
                std::string error;
                const std::optional<double> value = numeric(item, error);
                if (!error.empty()) {
                    return error;
                }
                items.push_back(format_numeric(value.value_or(0.0), ""));
                continue;
            }
        }

        if (item.size() >= 2 && item.front() == '"' && item.back() == '"' &&
            item.find('"', 1) == item.size() - 1) {
            items.push_back(item.substr(1, item.size() - 2));
            continue;
        }
        if (item_upper == "INKEY$") {
            items.push_back(key_text());
            continue;
        }
        if (item_upper == "TIME$") {
            items.push_back(clock_time_text());
            continue;
        }
        if (item_upper == "DATE$") {
            items.push_back(clock_date_text());
            continue;
        }
        if (item_upper == "TIMER") {
            items.push_back(std::to_string(static_cast<long long>(clock_timer_seconds())));
            continue;
        }
        if (starts_with(item_upper, "TYPE$(") && item.back() == ')') {
            const std::string name = trim(item.substr(6, item.size() - 7));
            const auto [base, type] = resolve(name);
            (void)base;
            items.push_back(upper(type == "double" ? "DOUBLE" : type));
            continue;
        }
        if (item_upper.back() == '$' ||
            (item_upper.back() == ')' && item_upper.find("$(") != std::string::npos)) {
            const std::size_t open = item.find('(');
            if (open != std::string::npos && item.back() == ')') {
                const std::string array_name = upper(item.substr(0, open));
                const std::string index_expr = item.substr(open + 1, item.size() - open - 2);
                const auto array = state_.string_arrays.find(array_name);
                if (array != state_.string_arrays.end()) {
                    // The reference evaluates the index (`BED$(I)` reads the
                    // loop variable); only a failure to evaluate is an empty
                    // field.
                    long index = 0;
                    bool numeric_index = false;
                    try {
                        index = static_cast<long>(eval(index_expr));
                        numeric_index = true;
                    } catch (const std::exception&) {
                        numeric_index = false;
                    }
                    if (!numeric_index || index < 0 ||
                        static_cast<std::size_t>(index) >= array->second.size()) {
                        items.push_back({});
                    } else {
                        items.push_back(array->second[static_cast<std::size_t>(index)]);
                    }
                } else {
                    items.push_back({});
                }
                continue;
            }
            items.push_back(text_of(item));
            continue;
        }
        if (mentions_string_function(item_upper)) {
            try {
                Strings strings{interpreter_, state_};
                items.push_back(strings.evaluate(item));
            } catch (const BasicError&) {
                std::string error;
                const std::optional<double> value = numeric(item, error);
                if (!error.empty()) {
                    return error;
                }
                items.push_back(format_numeric(value.value_or(0.0), ""));
            }
            continue;
        }

        // Typed numeric variable, then a plain one, then an expression.
        static const std::string suffixes{"%&!#"};
        if (!item_upper.empty() && suffixes.find(item_upper.back()) != std::string::npos) {
            const auto [base, type] = resolve(item_upper);
            const std::optional<double> value = number_of(base);
            if (!value.has_value()) {
                items.push_back({});
            } else {
                items.push_back(format_numeric(*value, type));
            }
            continue;
        }
        const auto direct = number_of(item_upper);
        if (direct.has_value()) {
            const auto [base, type] = resolve(item_upper);
            (void)base;
            items.push_back(format_numeric(*direct, type));
            continue;
        }
        if (resolve(item_upper).second == "string") {
            items.push_back(text_of(resolve(item_upper).first + "$"));
            continue;
        }
        std::string error;
        const std::optional<double> value = numeric(item, error);
        if (!error.empty()) {
            return error;
        }
        items.push_back(format_numeric(value.value_or(0.0), ""));
    }
    const std::string result = join(items, " ");
    return suppress_newline ? result : result + "\n";
}

std::string Machine::print_using(const std::string& args) {
    const std::size_t separator = args.find(';');
    if (separator == std::string::npos) {
        return "❌ PRINT USING: missing ';' separator\n";
    }
    const std::string format_part = trim(args.substr(0, separator));
    const std::string values_part = trim(args.substr(separator + 1));

    std::string format;
    if (format_part.size() >= 2 && format_part.front() == '"' && format_part.back() == '"') {
        format = format_part.substr(1, format_part.size() - 2);
    } else {
        const std::string key = upper(format_part);
        const auto found = interpreter_.variables().find(key);
        if (found != interpreter_.variables().end() && found->second.is_text()) {
            format = found->second.text();
        } else {
            format = interpreter_.interpolate_text(format_part);
        }
    }

    std::vector<std::string> expressions;
    for (const std::string& field : split_plain(values_part, ',')) {
        if (!trim(field).empty()) {
            expressions.push_back(trim(field));
        }
    }

    const auto string_of = [this](const std::string& expr) {
        const std::string trimmed = trim(expr);
        if (trimmed.size() >= 2 && trimmed.front() == '"' && trimmed.back() == '"') {
            return trimmed.substr(1, trimmed.size() - 2);
        }
        const auto found = interpreter_.variables().find(upper(trimmed));
        if (found != interpreter_.variables().end() && found->second.is_text()) {
            return found->second.text();
        }
        return interpreter_.interpolate_text(expr);
    };

    std::string output;
    std::size_t expression_index = 0;
    for (std::size_t index = 0; index < format.size();) {
        const char character = format[index];
        if (expression_index >= expressions.size()) {
            output += format.substr(index);
            break;
        }
        if (std::strchr("#+-$*", character) != nullptr) {
            std::size_t scan = index;
            bool has_decimal = false;
            while (scan < format.size() &&
                   std::strchr("#.,+-$* ", format[scan]) != nullptr) {
                if (format[scan] == '.') {
                    has_decimal = true;
                }
                ++scan;
            }
            const std::string field = format.substr(index, scan - index);
            double value = 0.0;
            try {
                value = eval(expressions[expression_index]);
            } catch (const std::exception&) {
                value = 0.0;
            }
            ++expression_index;
            std::string formatted;
            if (has_decimal) {
                const std::size_t dot = field.find('.');
                const int places = static_cast<int>(field.size() - dot - 1);
                char buffer[128];
                std::snprintf(buffer, sizeof buffer, "%0.*f", places, value);
                formatted = buffer;
            } else {
                char buffer[128];
                std::snprintf(buffer, sizeof buffer, "%0.0f", value);
                formatted = buffer;
            }
            const auto hashes = std::count(field.begin(), field.end(), '#');
            std::size_t sign = 0;
            while (sign < formatted.size() &&
                   (formatted[sign] == '+' || formatted[sign] == '-')) {
                ++sign;
            }
            const auto unsigned_length = static_cast<long>(formatted.size() - sign);
            if (hashes > unsigned_length) {
                const auto target =
                    static_cast<std::size_t>(hashes + (value < 0.0 ? 1 : 0));
                while (formatted.size() < target) {
                    formatted.insert(formatted.begin(), ' ');
                }
            }
            output += formatted;
            index = scan;
            continue;
        }
        if (character == '!') {
            const std::string value = string_of(expressions[expression_index]);
            ++expression_index;
            output += value.empty() ? " " : value.substr(0, 1);
            ++index;
            continue;
        }
        if (character == '&') {
            output += string_of(expressions[expression_index]);
            ++expression_index;
            ++index;
            continue;
        }
        if (character == '\\' && index + 1 < format.size() && format[index + 1] == '\\') {
            std::size_t end = format.find('\\', index + 1);
            if (end == std::string::npos) {
                end = index + 2;
            }
            const auto width = static_cast<long>(end - index + 1);
            std::string value = string_of(expressions[expression_index]);
            ++expression_index;
            if (static_cast<long>(value.size()) > width) {
                value.resize(static_cast<std::size_t>(width));
            }
            while (static_cast<long>(value.size()) < width) {
                value.push_back(' ');
            }
            output += value;
            index = end + 1;
            continue;
        }
        output.push_back(character);
        ++index;
    }
    return output + "\n";
}

// -- assignment ------------------------------------------------------------

std::string Machine::let(const std::string& args) {
    const std::size_t equals = args.find('=');
    if (equals == std::string::npos) {
        return "❌ LET requires format: variable = expression\n";
    }
    const std::string var_name = upper(trim(args.substr(0, equals)));
    const std::string expr = trim(args.substr(equals + 1));
    const std::string expr_upper = upper(expr);
    if (var_name.empty()) {
        return "❌ LET requires variable name\n";
    }

    // Array element: NAME(index) = value
    if (var_name.find('(') != std::string::npos && var_name.back() == ')') {
        const std::size_t open = var_name.find('(');
        const std::string array_name = var_name.substr(0, open);
        const std::string index_expr = var_name.substr(open + 1, var_name.size() - open - 2);
        long index = 0;
        try {
            index = static_cast<long>(eval(index_expr));
        } catch (const std::exception& error) {
            return "❌ Invalid array index or value: " + std::string{error.what()} + "\n";
        }
        if (!array_name.empty() && array_name.back() == '$') {
            const auto array = state_.string_arrays.find(array_name);
            if (array != state_.string_arrays.end()) {
                std::string value;
                try {
                    Strings strings{interpreter_, state_};
                    value = strings.evaluate(expr);
                } catch (const BasicError&) {
                    value = expr;
                }
                if (index < 0 || static_cast<std::size_t>(index) >= array->second.size()) {
                    return "❌ Array index out of bounds: " + std::to_string(index) + "\n";
                }
                array->second[static_cast<std::size_t>(index)] = value;
                return {};
            }
        }
        try {
            const double value = eval(expr);
            const auto array = interpreter_.arrays.find(array_name);
            if (array == interpreter_.arrays.end()) {
                return "❌ Array not defined: " + array_name + "\n";
            }
            if (index < 0 || static_cast<std::size_t>(index) >= array->second.size()) {
                return "❌ Array index out of bounds: " + std::to_string(index) + "\n";
            }
            array->second[static_cast<std::size_t>(index)] = value;
            return {};
        } catch (const std::exception& error) {
            return "❌ Invalid array index or value: " + std::string{error.what()} + "\n";
        }
    }

    // Record field: VAR.FIELD = value
    if (var_name.find('.') != std::string::npos) {
        const std::size_t dot = var_name.find('.');
        const std::string object = var_name.substr(0, dot);
        const std::string field = var_name.substr(dot + 1);
        const auto record = state_.records.find(object);
        if (record == state_.records.end()) {
            return "❌ " + object + " is not a user-defined type instance\n";
        }
        if (!field.empty() && field.back() == '$') {
            record->second[field] = Value{strip_quotes(expr)};
        } else {
            double value = 0.0;
            try {
                value = eval(expr);
            } catch (const std::exception&) {
                value = 0.0;
            }
            record->second[field] = Value{value};
        }
        return {};
    }

    try {
        validate_name(var_name);
    } catch (const BasicError& error) {
        return "❌ " + std::string{error.what()} + "\n";
    }

    if (!var_name.empty() && var_name.back() == '$') {
        const bool simple_literal =
            expr.size() >= 2 && expr.front() == '"' && expr.back() == '"' &&
            expr.find('"', 1) == expr.size() - 1;
        if (simple_literal) {
            assign(var_name, expr.substr(1, expr.size() - 2));
        } else if (expr_upper == "INKEY$") {
            assign(var_name, key_text());
        } else if (expr_upper == "TIME$") {
            assign(var_name, clock_time_text());
        } else if (expr_upper == "DATE$") {
            assign(var_name, clock_date_text());
        } else {
            try {
                Strings strings{interpreter_, state_};
                assign(var_name, strings.evaluate(expr));
            } catch (const BasicError&) {
                assign(var_name, expr);
            }
        }
        return {};
    }

    if (expr_upper == "TIMER") {
        assign(var_name, clock_timer_seconds());
        return {};
    }

    if (!expr.empty() && expr.front() == '"') {
        try {
            Strings strings{interpreter_, state_};
            assign(var_name, strings.evaluate(expr));
        } catch (const BasicError&) {
            assign(var_name, strip_quotes(expr));
        }
        return {};
    }

    try {
        assign(var_name, eval(expr));
    } catch (const std::exception& thrown) {
        const std::string message = thrown.what();
        if (division_by_zero(message)) {
            if (divert_on_error()) {
                return {};
            }
            throw;
        }
        // A string function that returns a number (`LEN`, `INSTR`, `VAL`).
        try {
            Strings strings{interpreter_, state_};
            const std::string text = strings.evaluate(expr);
            double value = 0.0;
            if (parse_float(text, value)) {
                assign(var_name, value);
            } else {
                assign(var_name, text);
            }
        } catch (const BasicError&) {
            return "❌ Error in LET: " + message + " (expr: '" + expr + "')\n";
        }
    }
    return {};
}

std::string Machine::input(const std::string& args) {
    const std::string name = trim(args);
    std::string prompt = "? ";
    std::string var_name = upper(name);
    const std::size_t quote = args.find('"');
    if (quote != std::string::npos) {
        const std::size_t closing = args.find('"', quote + 1);
        if (closing != std::string::npos) {
            std::size_t after = closing + 1;
            if (after < args.size() && (args[after] == ';' || args[after] == ',')) {
                ++after;
            }
            while (after < args.size() && is_space(args[after])) {
                ++after;
            }
            if (after < args.size()) {
                prompt = args.substr(quote + 1, closing - quote - 1) + " ";
                var_name = upper(trim(args.substr(after)));
            }
        }
    }
    if (var_name.empty()) {
        return "❌ INPUT requires variable name\n";
    }
    try {
        validate_name(var_name);
    } catch (const BasicError& error) {
        return "❌ " + std::string{error.what()} + "\n";
    }
    const bool numeric = var_name.back() != '$';
    interpreter_.request_input(prompt, var_name, numeric);
    return {};
}

// -- control flow ----------------------------------------------------------

std::string Machine::conditional(const std::string& args) {
    const std::string args_upper = upper(args);
    std::size_t then_pos = args_upper.find(" THEN ");
    if (then_pos == std::string::npos) {
        const std::string trimmed = rstrip(args_upper);
        if (trimmed.size() >= 4 &&
            trimmed.compare(trimmed.size() - 4, 4, "THEN") == 0) {
            const std::size_t found = trimmed.rfind("THEN");
            then_pos = found == 0 ? std::string::npos : found - 1;
        }
    }
    if (then_pos == std::string::npos) {
        return "❌ IF requires THEN keyword\n";
    }
    const std::string condition = trim(args.substr(0, then_pos + 1));
    const std::string then_part =
        then_pos + 6 < args.size() ? trim(args.substr(then_pos + 6)) : std::string{};

    bool condition_true = false;
    const int verdict = try_string_comparison(*this, condition);
    if (verdict >= 0) {
        condition_true = verdict == 1;
    } else {
        try {
            condition_true = std::fabs(eval(condition)) > 0.0001;
        } catch (const std::exception& error) {
            const std::string message = error.what();
            if (division_by_zero(message)) {
                if (divert_on_error()) {
                    return {};
                }
                throw;
            }
            return "❌ Error in IF condition: " + message + "\n";
        }
    }

    if (then_part.empty()) {
        state_.ifs.push_back(IfFrame{!condition_true, 0});
        return {};
    }

    std::string else_part;
    std::string body = then_part;
    const std::string body_upper = upper(then_part);
    const std::size_t else_pos = body_upper.find(" ELSE ");
    if (else_pos != std::string::npos) {
        else_part = trim(then_part.substr(else_pos + 6));
        body = trim(then_part.substr(0, else_pos));
    }

    if (condition_true && !body.empty()) {
        if (is_all_digits(body)) {
            return goto_line(body);
        }
        return execute(body);
    }
    if (!condition_true && !else_part.empty()) {
        if (is_all_digits(else_part)) {
            return goto_line(else_part);
        }
        return execute(else_part);
    }
    return {};
}

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
    interpreter_.current_line = found->second;
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
    interpreter_.current_line = found->second;
    return {};
}

std::string Machine::return_from_sub() {
    if (state_.gosubs.empty()) {
        return "❌ RETURN without GOSUB\n";
    }
    interpreter_.current_line = state_.gosubs.back() + 1;
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
        interpreter_.current_line = frame.body + 1;
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
                interpreter_.current_line = scan + 1;
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
    interpreter_.current_line = state_.whiles.back();
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
                interpreter_.current_line = scan + 1;
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
        interpreter_.current_line = start;
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

std::string Machine::sub_definition(const std::string& args) {
    const std::string header = trim(args);
    if (header.empty()) {
        return "❌ SUB requires a name\n";
    }
    const std::string name = upper(trim(header.substr(0, header.find('('))));
    const auto definition = state_.subs.find(name);
    if (definition != state_.subs.end()) {
        interpreter_.current_line = definition->second.end;
        return {};
    }
    const auto& lines = interpreter_.program_lines();
    for (std::size_t scan = interpreter_.current_line + 1; scan < lines.size(); ++scan) {
        if (upper(trim(lines[scan].text)) == "END SUB") {
            interpreter_.current_line = scan;
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
    interpreter_.current_line = frame.return_line + 1;
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
        interpreter_.current_line = definition->second.end;
        return {};
    }
    const auto& lines = interpreter_.program_lines();
    for (std::size_t scan = interpreter_.current_line + 1; scan < lines.size(); ++scan) {
        if (upper(trim(lines[scan].text)) == "END FUNCTION") {
            interpreter_.current_line = scan;
            break;
        }
    }
    return {};
}

std::string Machine::end_function() {
    if (state_.calls.empty()) {
        return {};
    }
    CallFrame frame = std::move(state_.calls.back());
    state_.calls.pop_back();
    const double result = number_of(frame.function_name).value_or(0.0);
    for (const auto& [name, value] : frame.saved) {
        if (value.has_value()) {
            interpreter_.variables()[name] = *value;
        } else {
            interpreter_.remove_variable(name);
        }
    }
    interpreter_.set_number(frame.function_name, result);
    interpreter_.current_line = frame.return_line + 1;
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
            evaluated.push_back(Value{eval(argument)});
        } catch (const std::exception&) {
            evaluated.push_back(Value{strip_quotes(argument)});
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
    interpreter_.current_line = definition->second.start;
    return {};
}

std::string Machine::type_definition(const std::string& args) {
    const std::string name = upper(trim(args));
    if (name.empty()) {
        return "❌ TYPE requires a name\n";
    }
    std::map<std::string, Value> fields;
    const auto& lines = interpreter_.program_lines();
    for (std::size_t scan = interpreter_.current_line + 1; scan < lines.size(); ++scan) {
        const std::string text = trim(lines[scan].text);
        const std::string code = upper(text);
        if (code == "END TYPE" || code == "ENDTYPE") {
            interpreter_.current_line = scan;
            break;
        }
        static const std::regex field_pattern{
            R"(^\s*([A-Za-z_][A-Za-z0-9_]*\$?)\s+AS\s+([A-Za-z_][A-Za-z0-9_]*)\s*$)",
            std::regex::ECMAScript};
        std::smatch match;
        if (std::regex_match(text, match, field_pattern)) {
            const std::string field = upper(match[1].str());
            const std::string type = upper(match[2].str());
            fields[field] = type == "STRING" ? Value{std::string{}} : Value{0.0};
        }
    }
    state_.types[name] = std::move(fields);
    return {};
}

std::string Machine::dim(const std::string& args) {
    for (const std::string& raw : split_plain(args, ',')) {
        const std::string part = trim(raw);
        static const std::regex as_pattern{
            R"(^([A-Za-z_][A-Za-z0-9_]*\$?)\s+AS\s+([A-Za-z_][A-Za-z0-9_]*)$)",
            std::regex::ECMAScript | std::regex::icase};
        std::smatch match;
        if (std::regex_match(part, match, as_pattern)) {
            const std::string name = upper(match[1].str());
            const std::string type = upper(match[2].str());
            const auto fields = state_.types.find(type);
            state_.records[name] = fields == state_.types.end()
                                       ? std::map<std::string, Value>{}
                                       : fields->second;
            continue;
        }
        if (part.find('(') == std::string::npos || part.back() != ')') {
            return "❌ Invalid DIM syntax: " + part + "\n";
        }
        const std::string name = upper(trim(part.substr(0, part.find('('))));
        const std::string size_expr =
            part.substr(part.find('(') + 1, part.size() - part.find('(') - 2);
        try {
            validate_name(name);
            double size_value = 0.0;
            if (!parse_float(trim(size_expr), size_value)) {
                return "❌ Expected numeric value for array dimension, got '" +
                       trim(size_expr) + "'\n";
            }
            const auto size = static_cast<long>(std::trunc(eval(trim(size_expr))));
            if (size < 0) {
                return "❌ Array dimension must be non-negative: " + std::to_string(size) +
                       "\n";
            }
            if (size > 65535) {
                return "❌ Array too large: " + std::to_string(size) + " (max 65535)\n";
            }
            const auto length = static_cast<std::size_t>(size) + 1;
            if (!name.empty() && name.back() == '$') {
                state_.string_arrays[name] = std::vector<std::string>(length);
            } else {
                interpreter_.arrays[name] = std::vector<double>(length, 0.0);
            }
        } catch (const BasicError& error) {
            return "❌ " + std::string{error.what()} + "\n";
        } catch (const std::exception& error) {
            return "❌ Error in DIM " + name + ": " + std::string{error.what()} + "\n";
        }
    }
    return {};
}

std::string Machine::read_data(const std::string& args) {
    for (const std::string& raw : split_plain(args, ',')) {
        const std::string name = upper(trim(raw));
        if (state_.data_pointer >= state_.data.size()) {
            return "❌ Out of DATA\n";
        }
        const std::string value = state_.data[state_.data_pointer];
        ++state_.data_pointer;
        double number = 0.0;
        if (parse_float(value, number)) {
            assign(name, number);
        } else {
            assign(name, value);
        }
    }
    return {};
}

std::string Machine::restore() {
    state_.data_pointer = 0;
    return {};
}

std::string Machine::swap(const std::string& args) {
    const std::size_t comma = args.find(',');
    if (comma == std::string::npos) {
        return "❌ SWAP requires two variable names separated by a comma\n";
    }
    const std::string first = upper(trim(args.substr(0, comma)));
    const std::string second = upper(trim(args.substr(comma + 1)));
    const auto numeric = [this](const std::string& name) {
        const auto found = interpreter_.variables().find(name);
        return found != interpreter_.variables().end() && found->second.is_number();
    };
    const auto textual = [this](const std::string& name) {
        const auto found = interpreter_.variables().find(name);
        return found != interpreter_.variables().end() && found->second.is_text();
    };
    if (numeric(first) || numeric(second)) {
        const double a = number_of(first).value_or(0.0);
        const double b = number_of(second).value_or(0.0);
        interpreter_.set_number(first, b);
        interpreter_.set_number(second, a);
        return {};
    }
    if (textual(first) || textual(second)) {
        const std::string a = text_of(first);
        const std::string b = text_of(second);
        interpreter_.set_text(first, b);
        interpreter_.set_text(second, a);
        return {};
    }
    interpreter_.set_number(first, 0.0);
    interpreter_.set_number(second, 0.0);
    return {};
}

std::string Machine::erase(const std::string& args) {
    for (const std::string& raw : split_plain(args, ',')) {
        const std::string name = upper(trim(raw));
        if (name.empty()) {
            continue;
        }
        const auto strings = state_.string_arrays.find(name);
        if (strings != state_.string_arrays.end()) {
            strings->second.assign(strings->second.size(), std::string{});
            continue;
        }
        const auto numbers = interpreter_.arrays.find(name);
        if (numbers != interpreter_.arrays.end()) {
            numbers->second.assign(numbers->second.size(), 0.0);
        }
    }
    return {};
}

std::string Machine::constant(const std::string& args) {
    const std::size_t equals = args.find('=');
    if (equals == std::string::npos) {
        return "❌ CONST syntax error: expected '='\n";
    }
    const std::string name = upper(trim(args.substr(0, equals)));
    const std::string value_text = trim(args.substr(equals + 1));
    if (name.empty()) {
        return "❌ CONST requires a name\n";
    }
    if (!name.empty() && name.back() == '$') {
        if (value_text.size() >= 2 && value_text.front() == '"' && value_text.back() == '"') {
            interpreter_.set_text(name, value_text.substr(1, value_text.size() - 2));
        } else {
            interpreter_.set_text(name, interpreter_.interpolate_text(value_text));
        }
        return {};
    }
    try {
        interpreter_.set_number(name, eval(value_text));
    } catch (const std::exception&) {
        return "❌ CONST: cannot evaluate '" + value_text + "'\n";
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

std::string Machine::define_types(const std::string& type, const std::string& args) {
    for (const std::string& raw : split_plain(args, ',')) {
        std::string part = upper(trim(raw));
        if (part.empty()) {
            continue;
        }
        const std::size_t dash = part.find('-');
        if (dash != std::string::npos) {
            const char first = part.front();
            const char last = part.back();
            for (char letter = first; letter <= last; ++letter) {
                if (letter >= 'A' && letter <= 'Z') {
                    state_.defaults[letter] = type;
                }
            }
        } else if (part.front() >= 'A' && part.front() <= 'Z') {
            state_.defaults[part.front()] = type;
        }
    }
    return {};
}

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
        interpreter_.current_line = found->second;
        return {};
    }
    if (state_.error_index >= 0) {
        interpreter_.current_line = static_cast<std::size_t>(state_.error_index);
    }
    return {};
}

std::string Machine::resume_next() {
    if (state_.error_index >= 0) {
        const auto target = static_cast<std::size_t>(state_.error_index) + 1;
        if (target < interpreter_.program_lines().size()) {
            interpreter_.current_line = target;
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
            interpreter_.current_line = found->second + 1;
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

}  // namespace timewarp::lang::basic
