// SPDX-License-Identifier: MIT
//
// Forth: the stack, the definitions, the loops and the turtle words.

#include "lang/support.hpp"

namespace tw::lang_tests {

// ---------------------------------------------------------------------------
// Forth
// ---------------------------------------------------------------------------

struct ForthRun {
    std::vector<std::string> output;
    RecordingHost host;
};

ForthRun run_forth(std::string_view source) {
    ForthRun run;
    Interpreter interpreter{Language::Forth, run.host};
    interpreter.load_program(source);
    interpreter.run();
    run.output = interpreter.output();
    return run;
}

std::string forth_text(const ForthRun& run) {
    std::string text;
    for (std::size_t index = 0; index < run.output.size(); ++index) {
        if (index > 0) {
            text += '\n';
        }
        text += run.output[index];
    }
    return text;
}

void test_forth_pushes_and_adds() {
    TW_CHECK_EQ(forth_text(run_forth("2 3 + . CR")), std::string{"5 "});
}

// Forth is postfix: `FD` takes the distance that is already on the stack, so
// `FD 100` is a stack underflow rather than a move.
void test_forth_words_take_their_argument_from_the_stack() {
    const ForthRun run = run_forth("FD 100");
    TW_CHECK(any_error(run.output));
    TW_CHECK(forth_text(run).find("FD: stack underflow") != std::string::npos);
}

void test_forth_defines_and_calls_a_word() {
    TW_CHECK_EQ(forth_text(run_forth(": SQUARE DUP * ; 9 SQUARE . CR")),
                std::string{"81 "});
}

void test_forth_recurses() {
    TW_CHECK_EQ(forth_text(run_forth(": FACT DUP 1 > IF DUP 1- RECURSE * THEN ; 6 FACT . CR")),
                std::string{"720 "});
}

void test_forth_do_loop_with_the_counter() {
    const std::string text = forth_text(run_forth(": T 3 0 DO I . LOOP ; T CR"));
    TW_CHECK_EQ(text, std::string{"0 1 2 "});
}

void test_forth_nested_loops_read_the_outer_counter_with_j() {
    const std::string text =
        forth_text(run_forth(": T 2 0 DO 2 0 DO J . LOOP LOOP ; T CR"));
    TW_CHECK_EQ(text, std::string{"0 0 1 1 "});
}

void test_forth_plus_loop_steps_by_the_stack() {
    TW_CHECK_EQ(forth_text(run_forth(": T 10 0 DO I . 2 +LOOP ; T CR")),
                std::string{"0 2 4 6 8 "});
}

// `LEAVE` is written inside the loop but almost always inside an `IF` as well;
// reading the top of the control stack made every such loop fail to compile.
void test_forth_leave_inside_an_if_leaves_the_loop() {
    TW_CHECK_EQ(forth_text(run_forth(": T 10 0 DO I DUP . 2 = IF LEAVE THEN LOOP ; T CR")),
                std::string{"0 1 2 "});
}

void test_forth_begin_until_and_begin_while_repeat() {
    TW_CHECK_EQ(forth_text(run_forth(": T 0 BEGIN DUP . 1+ DUP 3 = UNTIL DROP ; T CR")),
                std::string{"0 1 2 "});
    TW_CHECK_EQ(forth_text(run_forth(
                    ": T 0 BEGIN DUP 3 < WHILE DUP . 1+ REPEAT DROP ; T CR")),
                std::string{"0 1 2 "});
}

void test_forth_variables_and_memory() {
    TW_CHECK_EQ(forth_text(run_forth("VARIABLE V 7 V ! V @ . CR")), std::string{"7 "});
    TW_CHECK_EQ(forth_text(run_forth("VARIABLE B 65 B C! B C@ . CR")),
                std::string{"65 "});
    TW_CHECK_EQ(forth_text(run_forth("VARIABLE N 10 N ! 5 N +! N @ . CR")),
                std::string{"15 "});
    TW_CHECK_EQ(forth_text(run_forth("99 CONSTANT NINETY NINETY . CR")),
                std::string{"99 "});
}

// `." hello"` prints `hello`: the space after the quote is Forth's delimiter, not
// the first character of the text.
void test_forth_string_output_drops_the_delimiter_space() {
    TW_CHECK_EQ(forth_text(run_forth(".\" hello\" CR")), std::string{"hello"});
}

// `BYE` ends the program where it stands.
void test_forth_bye_stops_the_program() {
    TW_CHECK_EQ(forth_text(run_forth(".\" done\" CR BYE .\" after\" CR")),
                std::string{"done"});
}

void test_forth_undefined_word_names_itself_and_its_line() {
    // The failure is appended to the line the program was on, so the check is on
    // the text rather than on a whole line: `1 .` printed `1 ` first.
    const ForthRun run = run_forth("1 .\nNOSUCHWORD\n2 .");
    const std::string text = forth_text(run);
    TW_CHECK(text.find("❌ Forth, line 2: undefined word: NOSUCHWORD") != std::string::npos);
    // Whatever ran before the failure is still printed.
    TW_CHECK(text.rfind("1 ", 0) == 0);
}

void test_forth_unclosed_definition_is_reported() {
    const ForthRun run = run_forth(": F 1");
    TW_CHECK(any_error(run.output));
    TW_CHECK(forth_text(run).find("never closed") != std::string::npos);
}

// A Forth run can be paused and walked back like a line-loop one. A token is the
// unit, which is what a Forth programmer steps through, and the position, both
// stacks, the dictionary and the text printed so far travel in the checkpoint.
void test_forth_run_can_be_walked_back() {
    std::printf("-- debug: forth rewind\n");
    RecordingHost host;
    Interpreter interpreter{Language::Forth, host};
    timewarp::lang::DebugSession session;
    session.set_breakpoints({1});  // source line 2, the token `3`
    int pauses = 0;
    int first_line = 0;
    int second_line = 0;
    bool second_was_a_rewind = false;
    session.on_paused = [&](int line) {
        ++pauses;
        if (pauses == 1) {
            first_line = line;
            // Back to the first token, so the whole program runs again from there.
            session.request_rewind(3);
            return;
        }
        if (pauses == 2) {
            second_line = line;
            second_was_a_rewind = session.paused_for_rewind();
        }
        session.request_continue();
    };
    interpreter.set_debug_session(&session);
    interpreter.set_history(16);
    interpreter.load_program(
        "1 2 + .\n"
        "3 4 + .\n");
    interpreter.run();

    TW_CHECK_EQ(first_line, 2);
    TW_CHECK_EQ(second_line, 1);
    TW_CHECK(second_was_a_rewind);
    // The breakpoint that stopped the run, the statement the rewind put it back on,
    // and the same breakpoint again when the re-run reaches it.
    TW_CHECK_EQ(pauses, 3);
    // The text printed before the rewind went back with it: without that the
    // statements run again would append to it and the answer would be `3 3 7 `.
    const std::string text = forth_text(ForthRun{interpreter.output(), host});
    TW_CHECK_EQ(text, std::string{"3 7 "});
}

void test_forth_turtle_words_move_and_colour() {
    const ForthRun run = run_forth("1 SETPC 100 FD 90 RT 100 FD");
    TW_CHECK(!any_error(run.output));
    TW_CHECK_EQ(run.host.ops.at(0), std::string{"colour #0000AA"});
    TW_CHECK_EQ(run.host.ops.at(1), std::string{"forward 100"});
    TW_CHECK_EQ(run.host.ops.at(2), std::string{"right 90"});
}

}  // namespace tw::lang_tests
