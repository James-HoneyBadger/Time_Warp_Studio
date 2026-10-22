// SPDX-License-Identifier: MIT
//
// APL, REXX and Smalltalk.

#include "lang/support.hpp"

namespace tw::lang_tests {

// ---------------------------------------------------------------------------
// APL
// ---------------------------------------------------------------------------

struct AplRun {
    std::vector<std::string> output;
    RecordingHost host;
};

AplRun run_apl(std::string_view source) {
    AplRun run;
    Interpreter interpreter{Language::Apl, run.host};
    interpreter.set_input_callback([](const std::string&) { return std::string{}; });
    interpreter.load_program(source);
    interpreter.run();
    run.output = interpreter.output();
    return run;
}

void test_apl_iota_and_reshape() {
    const AplRun run = run_apl("IOTA 5\n2 3 RHO IOTA 6\nRHO 2 3 RHO IOTA 6");
    TW_CHECK_EQ(run.output.at(0), std::string{"1 2 3 4 5"});
    // The interpreter's logger splits a multi-line result, so each row of the
    // table is an output entry of its own.
    TW_CHECK_EQ(run.output.at(1), std::string{"1 2 3"});
    TW_CHECK_EQ(run.output.at(2), std::string{"4 5 6"});
    TW_CHECK_EQ(run.output.at(3), std::string{"2 3"});
}

void test_apl_scalar_functions_are_pervasive() {
    const AplRun run = run_apl("1 2 3 + 10\n1 2 3 TIMES 2");
    TW_CHECK_EQ(run.output.at(0), std::string{"11 12 13"});
    TW_CHECK_EQ(run.output.at(1), std::string{"2 4 6"});
}

void test_apl_reduce_and_scan() {
    const AplRun run = run_apl("+REDUCE IOTA 5\n+SCAN IOTA 3");
    TW_CHECK_EQ(run.output.at(0), std::string{"15"});
    TW_CHECK_EQ(run.output.at(1), std::string{"1 3 6"});
}

void test_apl_assignment_is_silent() {
    const AplRun run = run_apl("A ← 2 3 RHO IOTA 6\nRHO A");
    TW_CHECK_EQ(run.output.size(), std::size_t{1});
    TW_CHECK_EQ(run.output.at(0), std::string{"2 3"});
}

void test_apl_defined_function_returns_a_value() {
    const AplRun run = run_apl("∇ R ← DOUBLE X\n  R ← X TIMES 2\n∇\nDOUBLE 21");
    TW_CHECK(contains(run.output, "✅ APL: DOUBLE defined"));
    TW_CHECK_EQ(run.output.back(), std::string{"42"});
}

void test_apl_length_error_is_reported() {
    const AplRun run = run_apl("1 2 + 4 5 6");
    TW_CHECK(contains(run.output, "length error: 2 and 3"));
}

void test_apl_turtle_words_draw_and_stay_silent() {
    const AplRun run = run_apl("FD 100\nRT 90\nFD 50");
    TW_CHECK_EQ(run.host.ops.at(0), std::string{"forward 100"});
    TW_CHECK_EQ(run.host.ops.at(1), std::string{"right 90"});
    TW_CHECK_EQ(run.host.ops.at(2), std::string{"forward 50"});
    TW_CHECK(run.output.empty());
}

void test_apl_turtle_state_is_readable() {
    const AplRun run = run_apl("FD 100\nTX\nTY\nTHETA");
    TW_CHECK_EQ(run.output.at(0), std::string{"0"});
    TW_CHECK_EQ(run.output.at(1), std::string{"100"});
    TW_CHECK_EQ(run.output.at(2), std::string{"0"});
}

// ---------------------------------------------------------------------------
// REXX
// ---------------------------------------------------------------------------

struct RexxRun {
    std::vector<std::string> output;
    RecordingHost host;
};

RexxRun run_rexx(std::string_view source) {
    RexxRun run;
    Interpreter interpreter{Language::Rexx, run.host};
    interpreter.set_input_callback([](const std::string&) { return std::string{}; });
    interpreter.load_program(source);
    interpreter.run();
    run.output = interpreter.output();
    return run;
}

void test_rexx_say_concatenates_abutting_terms() {
    const RexxRun run = run_rexx("SAY 'hello' 'world'");
    TW_CHECK_EQ(run.output.at(0), std::string{"helloworld"});
}

void test_rexx_arithmetic_and_comparison() {
    const RexxRun product = run_rexx("SAY 6 * 7");
    const RexxRun quotient = run_rexx("SAY 7 % 2");
    const RexxRun comparison = run_rexx("SAY 1 = 2");
    TW_CHECK_EQ(product.output.at(0), std::string{"42"});
    TW_CHECK_EQ(quotient.output.at(0), std::string{"3"});
    TW_CHECK_EQ(comparison.output.at(0), std::string{"0"});
}

void test_rexx_do_loop_counts() {
    const RexxRun run = run_rexx("DO i = 1 TO 3\n  SAY i\nEND");
    TW_CHECK_EQ(run.output.size(), std::size_t{3});
    TW_CHECK_EQ(run.output.at(0), std::string{"1"});
    TW_CHECK_EQ(run.output.at(2), std::string{"3"});
}

void test_rexx_if_block_skips_the_else() {
    const RexxRun run = run_rexx(
        "IF 1 = 1 THEN DO\n  SAY 'yes'\nEND\nELSE DO\n  SAY 'no'\nEND");
    TW_CHECK_EQ(run.output.size(), std::size_t{1});
    TW_CHECK_EQ(run.output.at(0), std::string{"yes"});
}

void test_rexx_builtins() {
    const RexxRun substr = run_rexx("SAY SUBSTR('abcdef', 2, 3)");
    const RexxRun space = run_rexx("SAY SPACE('a b c', 0)");
    TW_CHECK_EQ(substr.output.at(0), std::string{"bcd"});
    TW_CHECK_EQ(space.output.at(0), std::string{"abc"});
}

void test_rexx_subroutine_returns_through_result() {
    const RexxRun run =
        run_rexx("CALL double 21\nSAY RESULT\nEXIT\ndouble:\n  RETURN ARG(1) * 2");
    TW_CHECK_EQ(run.output.at(0), std::string{"42"});
}

void test_rexx_turtle_words_draw() {
    const RexxRun run = run_rexx("FD 50\nRT 90");
    TW_CHECK_EQ(run.host.ops.at(0), std::string{"forward 50"});
    TW_CHECK_EQ(run.host.ops.at(1), std::string{"right 90"});
}

// ---------------------------------------------------------------------------
// Smalltalk
// ---------------------------------------------------------------------------

struct SmalltalkRun {
    std::vector<std::string> output;
    RecordingHost host;
};

SmalltalkRun run_smalltalk(std::string_view source) {
    SmalltalkRun run;
    Interpreter interpreter{Language::Smalltalk, run.host};
    interpreter.set_input_callback([](const std::string&) { return std::string{}; });
    interpreter.load_program(source);
    interpreter.run();
    run.output = interpreter.output();
    return run;
}

void test_smalltalk_class_and_instance_variable() {
    const SmalltalkRun run = run_smalltalk(
        "Object subclass: #Counter\n  instanceVariableNames: 'count'\n\n"
        "Counter >> initialize\n  count := 0\n\n"
        "Counter >> bump\n  count := count + 1\n\n"
        "Counter >> value\n  ^count\n\n| c |\nc := Counter new.\nc bump.\nc bump.\nc value");
    TW_CHECK_EQ(run.output.back(), std::string{"2"});
}

void test_smalltalk_block_is_a_closure() {
    const SmalltalkRun run = run_smalltalk(
        "| total |\ntotal := 10.\n[:n | total := total + n] value: 5.\ntotal");
    TW_CHECK_EQ(run.output.back(), std::string{"15"});
}

void test_smalltalk_cascade_sends_to_the_original_receiver() {
    const SmalltalkRun run = run_smalltalk("Transcript show: 'a'; cr");
    TW_CHECK_EQ(run.output.at(0), std::string{"a"});
}

void test_smalltalk_inject_and_range() {
    const SmalltalkRun run = run_smalltalk(
        "| s |\ns := (1 to: 4) inject: 0 into: [:a :b | a + b].\ns");
    TW_CHECK_EQ(run.output.back(), std::string{"10"});
}

void test_smalltalk_non_local_return_leaves_the_block() {
    const SmalltalkRun run = run_smalltalk("| b |\nb := [:x | ^x * 2].\nb value: 21");
    TW_CHECK_EQ(run.output.back(), std::string{"42"});
}

void test_smalltalk_method_answers_its_last_expression() {
    const SmalltalkRun run = run_smalltalk(
        "Object subclass: #Thing\n  instanceVariableNames: ''\n\n"
        "Thing >> greet\n  ^'hi'\n\nThing new greet");
    TW_CHECK_EQ(run.output.back(), std::string{"'hi'"});
}

void test_smalltalk_turtle_messages_draw() {
    const SmalltalkRun run = run_smalltalk(
        "| t |\nt := Turtle new.\nt forward: 100.\nt right: 90");
    TW_CHECK(run.host.ops.size() >= 2);
    TW_CHECK_EQ(run.host.ops.at(0), std::string{"forward 100"});
    TW_CHECK_EQ(run.host.ops.at(1), std::string{"right 90"});
}

}  // namespace tw::lang_tests
