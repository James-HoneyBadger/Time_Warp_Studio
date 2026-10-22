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

std::string Machine::execute(std::string_view command) {
    // A user FUNCTION runs inside an expression, so anything it prints is collected
    // and handed back with the statement that called it -- ahead of that statement's
    // own text, which is the order the program wrote them in.
    const std::size_t output_mark = state_.function_output.size();
    std::string own = execute_statement(command);
    if (state_.function_output.size() == output_mark) {
        return own;
    }
    std::string printed = state_.function_output.substr(output_mark);
    state_.function_output.erase(output_mark);
    return printed + own;
}

std::string Machine::execute_statement(std::string_view command) {
    scan_program();
    install_functions();
    // `ON TIMER` is a real interrupt: checked at every statement boundary, which is
    // where a machine of the period would have taken it.
    service_timer();

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
    // `EXIT FOR` / `EXIT DO` / `EXIT WHILE` leave the innermost loop of that kind
    // and land on the line after its terminator; `EXIT FUNCTION` and `EXIT SUB` return
    // from the block. Without these a function could not return early, which is the
    // one thing a function is most often wanted for.
    if (starts_with(cmd, "EXIT ")) {
        const std::string what = trim(cmd.substr(5));
        if (what == "FUNCTION") {
            return end_function();
        }
        if (what == "SUB") {
            return end_sub();
        }
        if (what == "FOR" || what == "DO" || what == "WHILE") {
            return exit_loop(what);
        }
        return "❌ EXIT needs FUNCTION, SUB, FOR, DO or WHILE\n";
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
        // `GET #1, A` is the file channel; `GET (x,y)-(x,y), A` captures a
        // sprite off the raster. The `#` is what tells them apart, and it is
        // the spelling the reference already reserves for a channel.
        const std::string rest = trim(strip_comment(tail(text, 4)));
        if (!rest.empty() && rest[0] != '#') {
            return sprite_get(rest);
        }
        return get_record(strip_comment(tail(text, 4)));
    }
    if (starts_with(cmd, "PUT ")) {
        const std::string rest = trim(strip_comment(tail(text, 4)));
        if (!rest.empty() && rest[0] != '#') {
            return sprite_put(rest);
        }
        return put_record(strip_comment(tail(text, 4)));
    }

    // -- graphics ----------------------------------------------------------
    if (starts_with(cmd, "REM ") || cmd == "REM") {
        return {};
    }
    if (cmd == "CLS") {
        turtle_.clear();
        state_.raster.clear(state_.raster.background());
        // On a machine the screen is memory: clearing it is clearing cells, which is
        // what the tube then shows.
        interpreter_.machine_clear_screen();
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
    // The sprite statements that take only a name, before `SPRITE ` above would
    // ever see them -- the dispatch is prefix-based and `SPRITESHOW SHIP` is not
    // a `SPRITE` with a space after it.
    if (starts_with(cmd, "SPRITEMOVE ")) {
        return sprite_move(strip_comment(tail(text, 11)));
    }
    if (starts_with(cmd, "SPRITESHOW ")) {
        return sprite_visible(strip_comment(tail(text, 11)), true);
    }
    if (starts_with(cmd, "SPRITEHIDE ")) {
        return sprite_visible(strip_comment(tail(text, 11)), false);
    }
    if (starts_with(cmd, "ELLIPSEGRADIENT ")) {
        return ellipse_gradient(strip_comment(tail(text, 16)));
    }
    if (starts_with(cmd, "GRADIENT ")) {
        return gradient(strip_comment(tail(text, 9)));
    }
    if (starts_with(cmd, "ON TIMER")) {
        return on_timer(strip_comment(tail(text, 8)));
    }
    if (cmd == "TIMER ON") {
        state_.timer_enabled = true;
        // Due one interval from now, which is what "ON TIMER(n)" means: n seconds
        // between events, not n seconds after the program started.
        state_.timer_next_due = now_seconds() + state_.timer_interval;
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
        auto& pad = timewarp::io::Gamepad::instance();
        if (pad.available()) {
            state_.joystick_initialised = true;
            return std::string{"🎮 Gamepad ready ("} + pad.device() + ")\n";
        }
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
                // It used to print "SLEEP 0.1s" and return, which is the same
                // acknowledged-nothing defect `LOCATE` had. A program that asks to wait
                // waits -- and it can still be stopped while it waits, because the sleep
                // is taken in slices rather than as one uninterruptible block.
                const double clamped = std::clamp(seconds, 0.0, 60.0);
                const auto deadline =
                    std::chrono::steady_clock::now() +
                    std::chrono::duration_cast<std::chrono::steady_clock::duration>(
                        std::chrono::duration<double>(clamped));
                while (std::chrono::steady_clock::now() < deadline) {
                    if (interpreter_.stop_requested()) {
                        break;
                    }
                    std::this_thread::sleep_for(std::chrono::milliseconds(2));
                }
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

}  // namespace timewarp::lang::basic
