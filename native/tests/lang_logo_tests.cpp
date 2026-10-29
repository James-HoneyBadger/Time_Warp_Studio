// SPDX-License-Identifier: MIT
//
// Logo, and the seeded generator `RND` and `FRAN` answer with.

#include "lang/support.hpp"

namespace tw::lang_tests {

// ---------------------------------------------------------------------------
// Logo
// ---------------------------------------------------------------------------

struct LogoRun {
    std::vector<std::string> output;
    RecordingHost host;
};

LogoRun run_logo(std::string_view source) {
    LogoRun run;
    Interpreter interpreter{Language::Logo, run.host};
    interpreter.set_input_callback([](const std::string&) { return std::string{}; });
    interpreter.load_program(source);
    interpreter.run();
    run.output = interpreter.output();
    return run;
}

void test_logo_definition_does_not_run_until_it_is_called() {
    const LogoRun defined = run_logo("TO SQUARE :S\nREPEAT 4 [FD :S RT 90]\nEND");
    TW_CHECK(defined.host.ops.empty());
    TW_CHECK(!any_error(defined.output));

    const LogoRun called =
        run_logo("TO SQUARE :S\nREPEAT 4 [FD :S RT 90]\nEND\nSQUARE 20");
    TW_CHECK(!any_error(called.output));
    TW_CHECK_EQ(called.host.ops.size(), std::size_t{8});
    TW_CHECK_EQ(called.host.ops.front(), std::string{"forward 20"});
}

void test_logo_repeat_counts_for_repcount() {
    const LogoRun run = run_logo("REPEAT 2 [SHOW REPCOUNT]");
    TW_CHECK_EQ(run.output.size(), std::size_t{2});
    TW_CHECK_EQ(run.output.at(0), std::string{"1"});
    TW_CHECK_EQ(run.output.at(1), std::string{"2"});
}

void test_logo_recursion_counter_is_restored() {
    // A drifting counter is invisible until a program recurses far enough to
    // trip the depth limit, so it is asserted directly: fifteen nested calls
    // must leave the counter at zero.
    const LogoRun run = run_logo(
        "TO DOWN :N\n"
        "  IF :N = 0 [STOP]\n"
        "  DOWN :N - 1\n"
        "END\n"
        "DOWN 15\n"
        "SHOW THING \"__RECURSION_DEPTH__");
    TW_CHECK(!any_error(run.output));
    TW_CHECK_EQ(run.output.back(), std::string{"0"});
}

void test_logo_stop_ends_only_the_procedure_it_is_in() {
    const LogoRun run = run_logo(
        "TO HALF\n  FD 10\n  STOP\n  FD 10\nEND\nHALF\nFD 5");
    TW_CHECK(!any_error(run.output));
    TW_CHECK_EQ(run.host.ops.size(), std::size_t{2});
    TW_CHECK_EQ(run.host.ops.at(1), std::string{"forward 5"});
}

void test_logo_if_falls_through_when_false() {
    const LogoRun run = run_logo("IF 1 = 2 [FD 10]\nSHOW 42");
    TW_CHECK(run.host.ops.empty());
    TW_CHECK_EQ(run.output.back(), std::string{"42"});
}

void test_logo_print_drops_the_brackets_of_a_list_literal() {
    const LogoRun run = run_logo("PRINT [hello world]");
    TW_CHECK_EQ(run.output.back(), std::string{"hello world"});
}

void test_logo_print_shows_a_reporter_value_not_its_source() {
    const LogoRun run = run_logo("SHOW FIRST [7 8 9]");
    TW_CHECK_EQ(run.output.back(), std::string{"7"});
}

void test_logo_quoted_colour_names_reach_the_host() {
    const LogoRun run = run_logo("SETPENCOLOR \"RED\nFD 20");
    TW_CHECK(!any_error(run.output));
    TW_CHECK_EQ(run.host.ops.front(), std::string{"colour #FF0000"});
}

void test_logo_bracketed_command_may_span_lines() {
    const LogoRun run = run_logo("REPEAT 2 [\n  FD 10\n  RT 90\n]");
    TW_CHECK(!any_error(run.output));
    TW_CHECK_EQ(run.host.ops.size(), std::size_t{4});
    TW_CHECK_EQ(run.host.ops.at(1), std::string{"right 90"});
}

void test_logo_to_without_end_is_not_a_procedure() {
    const LogoRun run = run_logo("TO BROKEN\nFD 10");
    TW_CHECK(run.host.ops.empty());
}

void test_logo_clear_screen_resets_the_turtle() {
    const LogoRun run = run_logo("SETPENCOLOR \"RED\nFD 100\nCS\nFD 10");
    // The reset is the reference's `CS`: pose, pen colour and pen width go back
    // to how a run starts.
    TW_CHECK(run.host.ops.back() == "forward 10");
    TW_CHECK(contains(run.host.ops, "reset"));
}

void test_logo_text_size_reaches_the_text_call() {
    // The size is invisible to the differential harness, which compares the
    // text and its position, so it is asserted here.
    const LogoRun run = run_logo("TEXTSIZE 30\nLABEL \"hi");
    TW_CHECK(!any_error(run.output));
    TW_CHECK_EQ(run.host.ops.back(), std::string{"text 0 0 30 hi"});
}

void test_logo_filled_draws_a_polygon_around_the_path_walked() {
    const LogoRun run = run_logo("FILLED \"RED [FD 20 RT 90 FD 20 RT 90 FD 20]");
    TW_CHECK(!any_error(run.output));
    const std::string& polygon = run.host.ops.back();
    TW_CHECK(polygon.rfind("polygon", 0) == 0);
    TW_CHECK(polygon.find("#FF0000") != std::string::npos);
}

// BEZIER is a native extension: the canvas has drawn a curve since 19.0 and no
// language could ask for one until this word existed.
void test_logo_bezier_draws_a_curve_and_leaves_the_turtle_on_its_end() {
    const LogoRun run =
        run_logo("SETPC \"RED\nFD 5\nBEZIER 0 40 40 40 40 0\nSHOW XCOR\nSHOW YCOR");
    TW_CHECK(!any_error(run.output));
    TW_CHECK_EQ(run.host.ops.at(0), std::string{"colour #FF0000"});
    TW_CHECK_EQ(run.host.ops.at(2),
                std::string{"bezier 0 5 0 40 40 40 40 0 #FF0000 2"});
    // The turtle ends at the curve's end point, so a chained BEZIER starts where
    // the last one finished rather than back at the origin.
    TW_CHECK_EQ(run.output.at(run.output.size() - 2), std::string{"40.0"});
    TW_CHECK_EQ(run.output.back(), std::string{"0.0"});
    // Moving the turtle to the end point must not draw a straight line across the
    // curve that was just drawn, so the pen is lifted for the reposition.
    TW_CHECK_EQ(run.host.ops.at(3), std::string{"penup"});
    TW_CHECK_EQ(run.host.ops.at(4), std::string{"goto 40 0"});
    TW_CHECK_EQ(run.host.ops.at(6), std::string{"pendown"});
    TW_CHECK_EQ(run.host.ops.size(), std::size_t{7});
}

void test_a_chained_bezier_starts_where_the_last_one_finished() {
    const LogoRun run = run_logo("BEZIER 0 40 40 40 40 0\nBEZIER 80 0 80 -40 40 -40");
    TW_CHECK(!any_error(run.output));
    const std::string& second = run.host.ops.at(run.host.ops.size() - 5);
    TW_CHECK_EQ(second, std::string{"bezier 40 0 80 0 80 -40 40 -40 #FFFFFF 2"});
}

// `SETPC 1` is a palette slot, and it used to be accepted and change nothing:
// the pen stayed whatever it was and the picture came out the wrong colour. The
// colour it then got was a host's idea of "slot 1", not the reference's, which is
// why the same statement drew a different picture in Logo and in BASIC.
void test_logo_setpc_with_a_palette_index_sets_the_pen() {
    const LogoRun run = run_logo("SETPC 1\nFD 10");
    TW_CHECK_EQ(run.host.ops.at(0), std::string{"colour #0000AA"});
}

// The sixteen colours a program gets before it sets any of them, from the
// reference's `DEFAULT_PALETTE_16`. A slot past 15 is the grey of its own number.
void test_the_reference_palette_is_the_same_table_everywhere() {
    Rgb slot{};
    TW_CHECK(reference_palette_colour(1, &slot));
    TW_CHECK_EQ(slot, (Rgb{0, 0, 170}));
    TW_CHECK(reference_palette_colour(15, &slot));
    TW_CHECK_EQ(slot, (Rgb{255, 255, 255}));
    TW_CHECK(reference_palette_colour(200, &slot));
    TW_CHECK_EQ(slot, (Rgb{200, 200, 200}));
    TW_CHECK(!reference_palette_colour(300, &slot));
    TW_CHECK(!reference_palette_colour(-1, &slot));

    // The host the lesson checker and the API use answers the same way, so a
    // program judged by the panel and one drawn by the IDE agree about colour.
    timewarp::graphics::RecordingHost host;
    TW_CHECK_EQ(host.resolve_color(1), (Rgb{0, 0, 170}));
    TW_CHECK_EQ(host.resolve_color(4), (Rgb{170, 0, 0}));
}

void test_logo_bezier_rejects_a_short_argument_list() {
    const LogoRun run = run_logo("BEZIER 1 2 3");
    TW_CHECK_EQ(run.output.back(),
                std::string{"❌ [line 1] BEZIER requires cx1 cy1 cx2 cy2 x y"});
}

void test_logo_unknown_command_is_reported_with_the_statement_number() {
    const LogoRun run = run_logo("FD 10\nNOTACOMMAND");
    TW_CHECK_EQ(run.output.back(), std::string{"❌ [line 2] Unknown Logo command: NOTACOMMAND"});
}

void test_logo_heading_starts_north_and_turns_clockwise() {
    // The reference's convention, which `turtle_canvas_tests` also asserts:
    // heading 0 is north, so `FD` moves in +y, and `RT` is clockwise. `HEADING`
    // reports the stored float, so a quarter turn reads `90.0`.
    const LogoRun run = run_logo("FD 10\nRT 90\nFD 10\nSHOW HEADING");
    TW_CHECK_EQ(run.host.ops.at(0), std::string{"forward 10"});
    TW_CHECK_EQ(run.output.back(), std::string{"90.0"});
}

void test_turtle_pose_matches_the_reference_in_three_dimensions() {
    // The numbers are what `TurtleState` in `graphics/turtle_state.py` computes
    // for the same sequence, projection included. The 3-D pose is the one part
    // of Logo the differential harness cannot see: it records the calls, not the
    // projected endpoints the canvas receives.
    using timewarp::lang::TurtlePose;

    TurtlePose turned;
    turned.enable_3d();
    turned.turn(90.0);
    turned.pitch(30.0);
    turned.move(100.0);
    TW_CHECK_NEAR(turned.x(), 86.602540378444, 1e-9);
    TW_CHECK_NEAR(turned.y(), 0.0, 1e-9);
    TW_CHECK_NEAR(turned.z(), 50.0, 1e-9);
    TW_CHECK_NEAR(turned.project().x, 76.980035891950, 1e-9);

    TurtlePose pitched;
    pitched.enable_3d();
    pitched.pitch(45.0);
    pitched.move(100.0);
    TW_CHECK_NEAR(pitched.y(), 70.710678118655, 1e-9);
    TW_CHECK_NEAR(pitched.z(), 70.710678118655, 1e-9);
    TW_CHECK_NEAR(pitched.project().y, 60.088441928934, 1e-9);

    // A roll changes which way a later turn takes the turtle.
    TurtlePose rolled;
    rolled.enable_3d();
    rolled.roll(90.0);
    rolled.move(100.0);
    rolled.turn(90.0);
    rolled.move(100.0);
    TW_CHECK_NEAR(rolled.y(), 100.0, 1e-9);
    TW_CHECK_NEAR(rolled.z(), -100.0, 1e-9);
    TW_CHECK_NEAR(rolled.project().y, 133.333333333333, 1e-9);
}

// A Logo run can be walked back like a line-loop one. The position travels with
// the snapshot because Logo runs its own statement loop: without the checkpoint
// there would be nothing to put back, and the interpreter would say so rather than
// moving a run whose inside it cannot reach.
void test_logo_run_can_be_walked_back() {
    std::printf("-- debug: logo rewind\n");
    RecordingHost host;
    Interpreter interpreter{Language::Logo, host};
    timewarp::lang::DebugSession session;
    session.set_breakpoints({3});  // the fourth statement, `PRINT :A`
    int pauses = 0;
    int second_line = 0;
    std::vector<std::string> output_at_the_second_pause;
    int value_at_the_second_pause = -1;
    session.on_paused = [&](int line) {
        ++pauses;
        if (pauses == 1) {
            // Back to the statement before the one that set A to 2.
            session.request_rewind(2);
            return;
        }
        if (pauses == 2) {
            second_line = line;
            output_at_the_second_pause = interpreter.output();
            const auto found = interpreter.variables().find("A");
            if (found != interpreter.variables().end() && found->second.is_number()) {
                value_at_the_second_pause = static_cast<int>(found->second.number());
            }
        }
        session.request_continue();
    };
    interpreter.set_debug_session(&session);
    interpreter.set_history(8);
    interpreter.load_program(
        "MAKE \"A 1\n"
        "PRINT :A\n"
        "MAKE \"A 2\n"
        "PRINT :A\n");
    interpreter.run();

    // Three: the breakpoint that stopped the run, the statement the rewind put it
    // back on, and the same breakpoint again when the run reaches it a second time.
    TW_CHECK_EQ(pauses, 3);
    // The statement the rewind chose, before it runs: the print that had already
    // happened is gone with the state that produced it.
    TW_CHECK_EQ(second_line, 2);
    TW_CHECK(output_at_the_second_pause.empty());
    TW_CHECK_EQ(value_at_the_second_pause, 1);
    // Running on from there produces the transcript again, in order.
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{2});
    if (interpreter.output().size() == 2) {
        TW_CHECK(interpreter.output().at(0).find('1') != std::string::npos);
        TW_CHECK(interpreter.output().at(1).find('2') != std::string::npos);
    }
}

// ---------------------------------------------------------------------------
// The seeded generator `RND` and `FRAN` answer with
// ---------------------------------------------------------------------------

void test_mersenne_random_matches_the_recorded_sequence() {
    // Seed 1, then three draws, recorded from the reference before it was deleted.
    // The command rows assert only that `RND` and `FRAN` print something, so this
    // is the one place the sequence itself is held: a change here moves the output
    // of every seeded program.
    MersenneRandom random;
    random.seed(1);
    TW_CHECK_NEAR(random.random(), 0.13436424411240122, 1e-15);
    TW_CHECK_NEAR(random.random(), 0.8474337369372327, 1e-15);
    TW_CHECK_NEAR(random.random(), 0.763774618976614, 1e-15);
}
}  // namespace tw::lang_tests
