// SPDX-License-Identifier: MIT
//
// FOCAL, PILOT and COMAL.

#include "lang/support.hpp"

namespace tw::lang_tests {

// Read by this file only: it arrived here with the cases that need it.

struct FocalRun {
    std::vector<std::string> output;
    std::vector<std::string> prompts;
    RecordingHost host;
};

FocalRun run_focal(std::string_view source, std::vector<std::string> inputs = {"4"}) {
    FocalRun run;
    Interpreter interpreter{Language::Focal, run.host};
    std::size_t next = 0;
    run.prompts = {};
    interpreter.set_input_callback([&](const std::string& prompt) {
        run.prompts.push_back(prompt);
        if (next < inputs.size()) {
            return inputs[next++];
        }
        return std::string{"4"};
    });
    interpreter.load_program(source);
    interpreter.run();
    run.output = interpreter.output();
    return run;
}

struct PilotRun {
    std::vector<std::string> output;
    std::vector<std::string> prompts;
    RecordingHost host;
};

PilotRun run_pilot(std::string_view source, std::vector<std::string> inputs = {""}) {
    PilotRun run;
    Interpreter interpreter{Language::Pilot, run.host};
    std::size_t next = 0;
    interpreter.set_input_callback([&](const std::string& prompt) {
        run.prompts.push_back(prompt);
        if (next < inputs.size()) {
            return inputs[next++];
        }
        return std::string{};
    });
    interpreter.load_program(source);
    interpreter.run();
    run.output = interpreter.output();
    return run;
}

// ---------------------------------------------------------------------------
// FOCAL
// ---------------------------------------------------------------------------

void test_focal_type_prints_text_and_expressions() {
    const FocalRun run = run_focal(R"(01.10 T "X=",2+3)");
    TW_CHECK(contains(run.output, "X=5.00"));
    TW_CHECK(!any_error(run.output));
}

void test_focal_set_then_read_back() {
    const FocalRun run = run_focal("01.10 S X=2+3\n01.20 T \"X=\",X");
    TW_CHECK(contains(run.output, "X=5.00"));
}

void test_focal_format_directive_sets_decimal_places() {
    const FocalRun run = run_focal(R"(01.10 T %0,"R=",7/2)");
    TW_CHECK(contains(run.output, "R=4"));
}

void test_focal_bang_suppresses_the_newline_within_a_statement() {
    const FocalRun run = run_focal(R"(01.10 T "A"!"B")");
    TW_CHECK(contains(run.output, "AB"));
}

void test_focal_if_branches_on_the_sign() {
    for (const auto& [value, expected] :
         std::vector<std::pair<int, std::string>>{{-3, "NEG"}, {0, "ZERO"}, {5, "POS"}}) {
        const FocalRun run = run_focal("01.10 S X=" + std::to_string(value) +
                                       "\n01.20 IF (X) 1.40,1.30,1.50\n"
                                       "01.30 T \"ZERO\"\n01.40 T \"NEG\"\n01.50 T \"POS\"");
        TW_CHECK(contains(run.output, expected));
    }
}

void test_focal_if_omitted_targets_fall_through() {
    const FocalRun run = run_focal(
        "01.10 S X=5\n01.20 IF (X) ,,1.40\n01.30 T \"SKIPPED\"\n01.40 T \"DONE\"");
    TW_CHECK(contains(run.output, "DONE"));
    TW_CHECK(!contains(run.output, "SKIPPED"));
}

void test_focal_for_runs_the_rest_of_the_line() {
    const FocalRun run = run_focal("01.10 FOR I=1,3;T I");
    TW_CHECK(contains(run.output, "1.00"));
    TW_CHECK(contains(run.output, "2.00"));
    TW_CHECK(contains(run.output, "3.00"));
}

void test_focal_for_takes_a_step_and_a_limit() {
    const FocalRun run = run_focal("01.10 FOR I=0,2,6;T I;");
    TW_CHECK(contains(run.output, "0.00"));
    TW_CHECK(contains(run.output, "6.00"));
    TW_CHECK(!contains(run.output, "8.00"));
}

void test_focal_for_runs_turtle_statements() {
    const FocalRun run = run_focal("01.10 FOR I=1,4;FD 50;RT 90");
    TW_CHECK(!any_error(run.output));
    TW_CHECK_EQ(run.host.ops.size(), std::size_t{8});
    TW_CHECK_EQ(run.host.ops.front(), std::string{"forward 50"});
}

void test_focal_for_keeps_the_last_value_the_body_saw() {
    const FocalRun run = run_focal("01.10 FOR I=1,4;T I");
    TW_CHECK(contains(run.output, "4.00"));
    TW_CHECK(!contains(run.output, "5.00"));
}


// RETURN is this implementation's one addition to DEC's ten commands: a group that
// can hand control back before the end of the program. Without it a DO group is a
// one-way jump, and every group has to be written as if it were the last thing the
// program does.

void test_focal_return_ends_the_group_early() {
    const FocalRun run = run_focal(
        "01.10 T \"START\"\n"
        "01.20 DO 2.10\n"
        "01.30 T \"AFTER\"\n"
        "01.40 Q\n"
        "02.10 T \"GROUP\"\n"
        "02.20 RETURN\n"
        "02.30 T \"NEVER\"");
    TW_CHECK(contains(run.output, "START"));
    TW_CHECK(contains(run.output, "GROUP"));
    TW_CHECK(contains(run.output, "AFTER"));
    TW_CHECK(!contains(run.output, "NEVER"));
    TW_CHECK(!any_error(run.output));
}

void test_focal_return_nests_and_ends_the_line() {
    const FocalRun run = run_focal(
        "01.10 DO 2.10\n"
        "01.20 T \"DONE\"\n"
        "01.30 Q\n"
        "02.10 T \"OUTER\";DO 3.10;T \"BACK\"\n"
        "02.20 RETURN\n"
        "02.30 T \"NEVER-OUTER\"\n"
        "03.10 T \"INNER\";RETURN;T \"NEVER-LINE\"\n"
        "03.20 T \"NEVER-INNER\"");
    // The inner RETURN ends the inner group only; the outer group carries on after
    // its DO, and its own RETURN ends the outer one.
    TW_CHECK(contains(run.output, "OUTER"));
    TW_CHECK(contains(run.output, "INNER"));
    TW_CHECK(contains(run.output, "BACK"));
    TW_CHECK(contains(run.output, "DONE"));
    TW_CHECK(!contains(run.output, "NEVER"));
}

void test_focal_return_ends_a_for_body() {
    const FocalRun run = run_focal(
        "01.10 DO 2.10\n"
        "01.20 T \"AFTER\"\n"
        "01.30 Q\n"
        "02.10 FOR I=1,5;T I;RETURN\n"
        "02.20 T \"NEVER\"");
    // The loop stops on its first pass as well as the group: printing 1 then 2..5
    // would mean the RETURN only ended the group after the body had run to its end.
    TW_CHECK(contains(run.output, "1"));
    TW_CHECK(!contains(run.output, "2"));
    TW_CHECK(contains(run.output, "AFTER"));
    TW_CHECK(!contains(run.output, "NEVER"));
    TW_CHECK(!any_error(run.output));
}

void test_focal_return_without_a_group_is_reported() {
    const FocalRun run = run_focal("01.10 RETURN");
    TW_CHECK(any_error(run.output));
    TW_CHECK(contains(run.output, "RETURN without a DO group"));
}

void test_focal_do_runs_a_group_and_returns() {
    const FocalRun run = run_focal(
        "01.10 T \"START\"\n01.20 DO 2.10\n01.30 T \"END\"\n01.40 Q\n02.10 T \"GROUP\"");
    TW_CHECK(contains(run.output, "START"));
    TW_CHECK(contains(run.output, "GROUP"));
    TW_CHECK(contains(run.output, "END"));
}

void test_focal_do_nesting_is_bounded() {
    const FocalRun run = run_focal("01.10 DO 1.10");
    TW_CHECK(any_error(run.output));
    TW_CHECK(contains(run.output, "DO nested more than"));
}

void test_focal_labels_compare_as_numbers() {
    const FocalRun run = run_focal("01.10 GOTO 2.10\n02.10 T \"THERE\"");
    TW_CHECK(contains(run.output, "THERE"));
}

void test_focal_erase_all_forgets_variables() {
    const FocalRun run = run_focal("01.10 S A=1\n01.20 E ALL\n01.30 T A");
    TW_CHECK(contains(run.output, "0.00"));
}

void test_focal_quit_stops_the_program() {
    const FocalRun run = run_focal("01.10 T \"A\"\n01.20 Q\n01.30 T \"NEVER\"");
    TW_CHECK(contains(run.output, "A"));
    TW_CHECK(!contains(run.output, "NEVER"));
}

void test_focal_turtle_movement_is_silent() {
    const FocalRun run = run_focal("01.10 FOR I=1,4;FD 80;RT 90");
    TW_CHECK(run.output.empty());
    TW_CHECK_EQ(run.host.ops.size(), std::size_t{8});
}

void test_focal_turtle_argument_may_be_a_variable() {
    const FocalRun run = run_focal("01.10 S D=80\n01.20 FD D");
    TW_CHECK(!any_error(run.output));
    TW_CHECK_EQ(run.host.ops.front(), std::string{"forward 80"});
}

void test_focal_turtle_without_a_distance_is_an_error() {
    const FocalRun run = run_focal("01.10 FD");
    TW_CHECK(any_error(run.output));
    TW_CHECK(contains(run.output, "needs a distance"));
}

void test_focal_functions() {
    TW_CHECK(contains(run_focal("01.10 T FSQT(16)").output, "4.00"));
    TW_CHECK(contains(run_focal("01.10 T FPI").output, "3.14"));
    const FocalRun ran = run_focal("01.10 T FRAN");
    TW_CHECK(!any_error(ran.output));
    // FRAN is in [0, 1) and prints to two places, so a draw above 0.995 rounds
    // to "1.00" -- which is not an error, and not a reason for CI to fail once
    // in two hundred runs.
    TW_CHECK(contains(ran.output, "0.") || contains(ran.output, "1.00"));
    TW_CHECK(any_error(run_focal("01.10 T FPI(1)").output));
}

void test_focal_errors_are_reported() {
    TW_CHECK(contains(run_focal("01.10 TIPE X").output, "unknown command"));
    TW_CHECK(contains(run_focal("01.10 T HELLO").output, "quotes"));
    TW_CHECK(contains(run_focal("01.10 GOTO 9.9").output, "no line labelled"));
    TW_CHECK(contains(run_focal("01.10 T 1/0").output, "division by zero"));
    TW_CHECK(any_error(run_focal("01.10 T FSQT(-1)").output));
}

void test_focal_unassigned_single_letter_is_zero() {
    const FocalRun run = run_focal("01.10 T X");
    TW_CHECK(contains(run.output, "0.00"));
    TW_CHECK(!any_error(run.output));
}

void test_focal_ask_reads_the_answer() {
    const FocalRun run =
        run_focal("01.10 ASK \"How many? \" N\n01.20 T \"You said \",N", {"42"});
    TW_CHECK_EQ(run.prompts.size(), std::size_t{1});
    TW_CHECK_EQ(run.prompts[0], std::string{"How many? "});
    TW_CHECK(contains(run.output, "You said 42.00"));
    TW_CHECK(!any_error(run.output));
}

void test_focal_ask_accepts_a_comma_before_the_name() {
    const FocalRun spaced =
        run_focal("01.10 ASK \"How many? \" N\n01.20 T \"You said \",N", {"42"});
    const FocalRun comma =
        run_focal("01.10 ASK \"How many? \", N\n01.20 T \"You said \",N", {"42"});
    TW_CHECK(contains(comma.output, "You said 42.00"));
    TW_CHECK(comma.output == spaced.output);
}

// ---------------------------------------------------------------------------
// PILOT
// ---------------------------------------------------------------------------

void test_pilot_type_accepts_match() {
    const PilotRun run = run_pilot(
        "T: Q\nA:\nM: PARIS\nTY: Correct\nTN: Wrong", {"Paris"});
    TW_CHECK(contains(run.output, "Correct"));
    TW_CHECK(!contains(run.output, "Wrong"));
    TW_CHECK_EQ(run.prompts.size(), std::size_t{1});

    const PilotRun wrong = run_pilot(
        "T: Q\nA:\nM: PARIS\nTY: Correct\nTN: Wrong", {"Rome"});
    TW_CHECK(contains(wrong.output, "Wrong"));
    TW_CHECK(!contains(wrong.output, "Correct"));
}

void test_pilot_substring_and_wildcards_match() {
    const PilotRun substring = run_pilot(
        "A:\nM: PARIS\nTY: HIT\nTN: MISS", {"I like PARIS best"});
    TW_CHECK(contains(substring.output, "HIT"));

    const PilotRun wildcard = run_pilot("A:\nM: P*S\nTY: HIT\nTN: MISS", {"PARIS"});
    TW_CHECK(contains(wildcard.output, "HIT"));
}

void test_pilot_jumps_and_labels() {
    const PilotRun run = run_pilot("J: MARK\nT: NOT REACHED\nL: MARK\nT: LANDED");
    TW_CHECK(contains(run.output, "LANDED"));
    TW_CHECK(!contains(run.output, "NOT REACHED"));
}

void test_pilot_subroutines_return_at_e() {
    const PilotRun run = run_pilot("U: *SUB\nT: BACK\nE:\n*SUB\nT: INSIDE\nE:");
    TW_CHECK(contains(run.output, "INSIDE"));
    TW_CHECK(contains(run.output, "BACK"));
}

void test_pilot_compute_and_branch() {
    const PilotRun run =
        run_pilot("C:X = 5\nB:X=*HIT\nT: MISSED\nE:\n*HIT\nT: BRANCHED");
    TW_CHECK(contains(run.output, "BRANCHED"));
    TW_CHECK(!contains(run.output, "MISSED"));
}

void test_pilot_strings_and_computation() {
    const PilotRun run = run_pilot("C:$GREETING = Hello\nT: Said $GREETING");
    TW_CHECK(contains(run.output, "Said Hello"));

    const PilotRun computed = run_pilot("C:X = 2+3\nT: #X");
    TW_CHECK(contains(computed.output, "5"));
}

void test_pilot_no_and_hint() {
    const PilotRun run = run_pilot("N: NO MATCH\nT: DONE");
    TW_CHECK(contains(run.output, "NO MATCH"));

    const PilotRun hint = run_pilot("H: Try the other one");
    TW_CHECK(contains(hint.output, "💡 Hint: Try the other one"));
}

void test_pilot_graphics_and_text() {
    const PilotRun run = run_pilot(
        "G: FD 50\nG: RT 90\nG: FILL\nG: TEXT A label");
    TW_CHECK_EQ(run.host.ops.size(), std::size_t{3});
    TW_CHECK_EQ(run.host.ops.at(0), std::string{"forward 50"});
    TW_CHECK_EQ(run.host.ops.at(1), std::string{"right 90"});
    TW_CHECK_EQ(run.host.ops.at(2), std::string{"fill #FFFFFF"});
    TW_CHECK(contains(run.output, "📝 A label"));
}

void test_pilot_reports_errors() {
    TW_CHECK(contains(run_pilot("LINK lesson.pilot").output,
                     "lesson chaining is not implemented"));
    TW_CHECK(contains(run_pilot("J: NOWHERE").output, "Label 'NOWHERE' not found"));
    TW_CHECK(contains(run_pilot("F: OPEN notes.txt X").output, "mode R|W|A"));
}

// ---------------------------------------------------------------------------
// COMAL
// ---------------------------------------------------------------------------

struct ComalRun {
    std::vector<std::string> output;
    RecordingHost host;
};

ComalRun run_comal(std::string_view source) {
    ComalRun run;
    Interpreter interpreter{Language::Comal, run.host};
    interpreter.set_input_callback([](const std::string&) { return std::string{}; });
    interpreter.load_program(source);
    interpreter.run();
    run.output = interpreter.output();
    return run;
}

void test_comal_print_and_assignment() {
    const ComalRun run = run_comal("10 X := 2 + 3\n20 PRINT X");
    TW_CHECK(contains(run.output, "5"));
    const ComalRun strings = run_comal("10 A$ := \"hi\"\n20 PRINT A$");
    TW_CHECK(contains(strings.output, "hi"));
}

void test_comal_if_blocks() {
    const ComalRun run = run_comal(
        "10 X := 2\n"
        "20 IF X = 1 THEN\n"
        "30 PRINT \"ONE\"\n"
        "40 ELIF X = 2 THEN\n"
        "50 PRINT \"TWO\"\n"
        "60 ELSE\n"
        "70 PRINT \"OTHER\"\n"
        "80 ENDIF\n");
    TW_CHECK(contains(run.output, "TWO"));
    TW_CHECK(!contains(run.output, "ONE"));

    const ComalRun single = run_comal("10 IF 1 = 1 THEN PRINT \"YES\"");
    TW_CHECK(contains(single.output, "YES"));
}

void test_comal_loops() {
    const ComalRun for_loop = run_comal("10 FOR I := 1 TO 3\n20 PRINT I\n30 NEXT I");
    TW_CHECK(contains(for_loop.output, "1"));
    TW_CHECK(contains(for_loop.output, "3"));

    const ComalRun while_loop =
        run_comal("10 I := 0\n20 WHILE I < 2\n30 I := I + 1\n40 PRINT I\n50 ENDWHILE");
    TW_CHECK(contains(while_loop.output, "2"));

    const ComalRun repeat =
        run_comal("10 I := 0\n20 REPEAT\n30 I := I + 1\n40 UNTIL I = 2\n50 PRINT I");
    TW_CHECK(contains(repeat.output, "2"));

    const ComalRun loop = run_comal(
        "10 I := 0\n20 LOOP\n30 I := I + 1\n40 IF I = 2 THEN EXIT\n50 ENDLOOP\n60 PRINT I");
    TW_CHECK(contains(loop.output, "2"));
}

void test_comal_procedures() {
    const ComalRun run = run_comal(
        "10 CALL DOUBLE(3)\n20 PRINT RESULT\n30 END\n"
        "40 PROC DOUBLE(N)\n50 RESULT := N * 2\n60 ENDPROC");
    TW_CHECK(contains(run.output, "6"));
}

void test_comal_arrays_and_data() {
    const ComalRun array =
        run_comal("10 DIM A(3)\n20 A(2) := 9\n30 PRINT A(2)");
    TW_CHECK(contains(array.output, "9"));

    const ComalRun data =
        run_comal("10 DATA 4, 5\n20 READ X, Y\n30 PRINT X + Y");
    TW_CHECK(contains(data.output, "9"));
}

// A FUNC could do nothing with a variable of its own: every name it touched was the
// caller's. LOCAL gives it one, and gives the caller theirs back on the way out --
// including when the body throws.

void test_comal_function_local_keeps_the_callers_variable() {
    const ComalRun run = run_comal(
        "10 T = 99\n"
        "20 PRINT DOUBLE(21)\n"
        "30 PRINT T\n"
        "40 END\n"
        "100 FUNC DOUBLE(N)\n"
        "110 LOCAL T\n"
        "120 T = N * 2\n"
        "130 RETURN T\n"
        "140 ENDFUNC");
    TW_CHECK(contains(run.output, "42"));
    // The caller's T is 99 again: the function's T was its own.
    TW_CHECK(contains(run.output, "99"));
    TW_CHECK(!any_error(run.output));
}

void test_comal_function_local_is_not_leaked_to_the_caller() {
    // With no T of the caller's, the local one is gone after the call rather than
    // left behind at its last value.
    const ComalRun run = run_comal(
        "10 PRINT DOUBLE(4)\n"
        "20 PRINT T\n"
        "30 END\n"
        "100 FUNC DOUBLE(N)\n"
        "110 LOCAL T\n"
        "120 T = N * 2\n"
        "130 RETURN T\n"
        "140 ENDFUNC");
    TW_CHECK(contains(run.output, "8"));
    TW_CHECK(contains(run.output, "0"));
}

void test_comal_turtle() {
    const ComalRun run = run_comal("10 MOVE 50\n20 TURN 90\n30 PLOT");
    TW_CHECK_EQ(run.host.ops.size(), std::size_t{3});
    TW_CHECK_EQ(run.host.ops.at(0), std::string{"forward 50"});
    TW_CHECK_EQ(run.host.ops.at(1), std::string{"right 90"});
    TW_CHECK(run.host.ops.at(2).rfind("point 0 0 #", 0) == 0);
}

}  // namespace tw::lang_tests
