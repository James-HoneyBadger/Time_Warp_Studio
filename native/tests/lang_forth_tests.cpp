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

void test_forth_turtle_words_move_and_colour() {
    const ForthRun run = run_forth("1 SETPC 100 FD 90 RT 100 FD");
    TW_CHECK(!any_error(run.output));
    TW_CHECK_EQ(run.host.ops.at(0), std::string{"colour #0000AA"});
    TW_CHECK_EQ(run.host.ops.at(1), std::string{"forward 100"});
    TW_CHECK_EQ(run.host.ops.at(2), std::string{"right 90"});
}

}  // namespace tw::lang_tests
