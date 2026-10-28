// SPDX-License-Identifier: MIT
//
// The raster, and BASIC.

#include "lang/support.hpp"

namespace tw::lang_tests {

// ---------------------------------------------------------------------------
// The raster
// ---------------------------------------------------------------------------

// A fixed-size pixel surface is what makes `POINT`, `GET` and `PUT` possible: a
// display list cannot be read back. Clipping is asserted because GW-BASIC
// clipped too -- a program that plots at (-5, 900) is not making a mistake a
// learner needs a message about.
void test_raster_clips_instead_of_failing() {
    Raster raster;
    TW_CHECK(!raster.active());

    raster.resize(4, 3, Rgb{1, 2, 3});
    TW_CHECK(raster.active());
    TW_CHECK_EQ(raster.width(), 4);
    TW_CHECK_EQ(raster.height(), 3);
    TW_CHECK(raster.at(0, 0) == (Rgb{1, 2, 3}));

    raster.set(0, 0, Rgb{255, 0, 0});
    raster.set(-1, 0, Rgb{0, 255, 0});
    raster.set(4, 0, Rgb{0, 255, 0});
    raster.set(0, 3, Rgb{0, 255, 0});
    TW_CHECK(raster.at(0, 0) == (Rgb{255, 0, 0}));
    TW_CHECK(!raster.contains(4, 0));
    // Outside the surface the answer is the background, not an error.
    TW_CHECK(raster.at(-1, 0) == (Rgb{1, 2, 3}));
    TW_CHECK(raster.at(0, 3) == (Rgb{1, 2, 3}));

    raster.clear(Rgb{9, 9, 9});
    TW_CHECK(raster.at(0, 0) == (Rgb{9, 9, 9}));
    TW_CHECK(raster.background() == (Rgb{9, 9, 9}));
}

void test_raster_line_and_rect_plot_the_pixels_asked_for() {
    Raster raster;
    raster.resize(8, 8);
    raster.line(0, 0, 7, 0, Rgb{255, 0, 0});
    raster.rect(2, 2, 4, 4, Rgb{0, 0, 255}, true);

    for (int x = 0; x < 8; ++x) {
        TW_CHECK(raster.at(x, 0) == (Rgb{255, 0, 0}));
    }
    // A filled rectangle covers its interior, not only its outline.
    TW_CHECK(raster.at(3, 3) == (Rgb{0, 0, 255}));
    TW_CHECK(raster.at(2, 4) == (Rgb{0, 0, 255}));
    TW_CHECK(raster.at(5, 5) == Rgb{});
}

void test_raster_flood_fill_stops_at_a_border() {
    Raster raster;
    raster.resize(5, 5);
    raster.rect(1, 1, 3, 3, Rgb{255, 255, 255}, false);
    raster.flood_fill(0, 0, Rgb{0, 255, 0});

    TW_CHECK(raster.at(0, 0) == (Rgb{0, 255, 0}));
    TW_CHECK(raster.at(4, 4) == (Rgb{0, 255, 0}));
    // Inside the outline is a different region and keeps its colour.
    TW_CHECK(raster.at(2, 2) == Rgb{});
    TW_CHECK(raster.at(1, 1) == (Rgb{255, 255, 255}));
}

void test_raster_round_trips_a_block() {
    Raster raster;
    raster.resize(6, 6);
    raster.set(1, 1, Rgb{10, 20, 30});
    raster.set(2, 1, Rgb{40, 50, 60});

    const std::vector<Rgb> block = raster.block(1, 1, 2, 2);
    TW_CHECK_EQ(block.size(), std::size_t{4});
    TW_CHECK(block.at(0) == (Rgb{10, 20, 30}));
    TW_CHECK(block.at(1) == (Rgb{40, 50, 60}));

    // Placed somewhere else, row-major from the top-left corner of the block.
    raster.put_block(4, 4, 2, 2, block);
    TW_CHECK(raster.at(4, 4) == (Rgb{10, 20, 30}));
    TW_CHECK(raster.at(5, 4) == (Rgb{40, 50, 60}));
    TW_CHECK(raster.at(4, 5) == Rgb{});
}

// ---------------------------------------------------------------------------
// BASIC
// ---------------------------------------------------------------------------

struct BasicRun {
    std::vector<std::string> output;
    /// The prompts `INPUT` asked for, in order, so a test can assert what the
    /// caller is asked as well as what the program does with the answer.
    std::vector<std::string> prompts;
    RecordingHost host;
};

BasicRun run_basic(std::string_view source, std::vector<std::string> inputs = {}) {
    BasicRun run;
    Interpreter interpreter{Language::Basic, run.host};
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

// `SPRITE` used to print "Sprite SHIP at (10, 20) size 30x40" and draw nothing.
// A statement that acknowledges work it did not do is the failure the example
// smoke test exists for, so each form is now asserted on what reached the host.
void test_basic_sprite_statements_capture_place_move_and_hide() {
    const BasicRun run = run_basic(
        "SCREEN 1\n"
        "LINE (10,10)-(40,50), 4, BF\n"
        "SPRITE SHIP, 10, 10, 30, 40\n"
        "SPRITE SHIP, 100, 100\n"
        "SPRITEMOVE SHIP, 5, -5\n"
        "SPRITESHOW SHIP\n"
        "SPRITEHIDE SHIP\n"
        "SPRITE SHIP, OFF");
    TW_CHECK(!any_error(run.output));
    TW_CHECK(contains(run.output, "Sprite SHIP captured 30x40 at (10.0, 10.0)"));
    TW_CHECK(contains(run.output, "Moved sprite SHIP to (100.0, 100.0)"));
    TW_CHECK(contains(run.output, "Moved sprite SHIP by (5.0, -5.0)"));
    TW_CHECK(contains(run.output, "Showing sprite SHIP"));
    TW_CHECK(contains(run.output, "Hiding sprite SHIP"));
    TW_CHECK(contains(run.output, "Removed sprite: SHIP"));
    // The capture carries the pixels: a definition with a size and no bitmap is
    // exactly what the old acknowledgement was.
    TW_CHECK(std::any_of(run.host.ops.begin(), run.host.ops.end(), [](const std::string& op) {
        return op == "sprite SHIP 0 0 30 40 1200";
    }));
    TW_CHECK(std::any_of(run.host.ops.begin(), run.host.ops.end(), [](const std::string& op) {
        return op == "place SHIP 100 100";
    }));
    TW_CHECK(std::any_of(run.host.ops.begin(), run.host.ops.end(), [](const std::string& op) {
        return op == "move SHIP 5 -5";
    }));
    TW_CHECK(std::any_of(run.host.ops.begin(), run.host.ops.end(), [](const std::string& op) {
        return op == "spritehide SHIP";
    }));
}

void test_basic_sprite_statements_refuse_a_name_they_never_captured() {
    const BasicRun run = run_basic("SCREEN 1\nSPRITE GHOST, 10, 10");
    TW_CHECK(contains(run.output, "No sprite named GHOST"));
}

void test_basic_gradients_reach_the_host() {
    const BasicRun run = run_basic(
        "SCREEN 1\nGRADIENT 0, 0, 100, 50, 4, 1, 45\nELLIPSEGRADIENT 0, 0, 60, 40, 2, 3");
    TW_CHECK(!any_error(run.output));
    TW_CHECK(std::any_of(run.host.ops.begin(), run.host.ops.end(), [](const std::string& op) {
        return op.rfind("gradient 0 0 100 50 ", 0) == 0;
    }));
    TW_CHECK(std::any_of(run.host.ops.begin(), run.host.ops.end(), [](const std::string& op) {
        return op.rfind("egradient 0 0 60 40 ", 0) == 0;
    }));
}

// `SLEEP` printed "SLEEP 0.1s" and returned: it acknowledged a wait it never took.
// `ON TIMER`/`TIMER ON` set two flags that nothing ever read. Both were statements
// that said they had done something, which is the failure this suite exists to catch.
void test_sleep_waits_and_can_be_stopped() {
    const auto started = std::chrono::steady_clock::now();
    const BasicRun run = run_basic("SLEEP 0.05\nPRINT \"AFTER\"");
    const double elapsed =
        std::chrono::duration<double>(std::chrono::steady_clock::now() - started).count();
    TW_CHECK(elapsed >= 0.04);
    TW_CHECK(contains(run.output, "AFTER"));

    // A stop request breaks the wait rather than being ignored until it ends.
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.load_program("SLEEP 5\nPRINT \"NEVER\"");
    const auto stopping = std::chrono::steady_clock::now();
    std::thread stopper{[&interpreter] {
        std::this_thread::sleep_for(std::chrono::milliseconds(60));
        interpreter.request_stop();
    }};
    interpreter.run();
    stopper.join();
    const double stop_elapsed =
        std::chrono::duration<double>(std::chrono::steady_clock::now() - stopping).count();
    TW_CHECK(stop_elapsed < 2.0);
    TW_CHECK(!contains(interpreter.output(), "NEVER"));
}

void test_on_timer_fires_the_handler_while_the_program_runs() {
    const BasicRun run = run_basic(
        "10 ON TIMER(0.02) GOSUB 100\n"
        "20 TIMER ON\n"
        "30 FOR J = 1 TO 6\n"
        "40 SLEEP 0.02\n"
        "50 NEXT J\n"
        "60 TIMER OFF\n"
        "70 PRINT \"TICKS:\"; C\n"
        "80 END\n"
        "100 C = C + 1\n"
        "110 RETURN");
    const std::string& last = run.output.back();
    TW_CHECK(last.rfind("TICKS:", 0) == 0);
    // The handler ran, and it ran from an interrupt rather than from a `GOSUB` in the
    // listing: C was never assigned anywhere but line 100.
    const int ticks = std::stoi(last.substr(6));
    TW_CHECK(ticks >= 1);

    // A handler that is not in the program switches the timer off instead of failing
    // every statement.
    const BasicRun broken = run_basic("ON TIMER(0.01) GOSUB 900\nTIMER ON\nSLEEP 0.03");
    TW_CHECK(!any_error(broken.output));
}

// `MID$(` contains `$(`, which an array read used to claim first: every string
// function spelled with a `$` returned an empty field.
void test_basic_dollar_string_functions_are_calls_not_array_reads() {
    const BasicRun run = run_basic("PRINT MID$(\"HELLO\", 1, 3)\nPRINT LEFT$(\"HELLO\", 2)");
    TW_CHECK_EQ(run.output.at(0), std::string{"HEL"});
    TW_CHECK_EQ(run.output.at(1), std::string{"HE"});
}

// The reference evaluates the index, so a string array read through a loop
// variable is the element, not an empty field.
void test_basic_string_array_index_is_an_expression() {
    const BasicRun run =
        run_basic("DIM B$(3)\nB$(1) = \"NORTH\"\nFOR I = 1 TO 1\nPRINT B$(I)\nNEXT I");
    TW_CHECK_EQ(run.output.at(0), std::string{"NORTH"});
}

// A PRINT whose string evaluation fails falls back to the arithmetic evaluator,
// and that evaluator's own message is the answer -- an empty line would hide a
// typo behind a plausible result.
void test_basic_unreadable_print_reports_the_evaluator_message() {
    const BasicRun run = run_basic("PRINT \"X\" + FLUMMOX$(\"a\")");
    TW_CHECK(any_error(run.output));
    TW_CHECK(contains(run.output, "Unexpected character"));
}

// `STR$` of an array element is arithmetic with a string around it. This used to be
// reported as `Unknown function: STR`, because the element looked like a call to a
// function named after the array and the whole item then fell through to the numeric
// evaluator, which has no `STR$`.
void test_basic_str_of_an_array_element_is_printed() {
    const BasicRun run =
        run_basic("DIM N(3)\nN(2) = 7\nPRINT \"X\" + STR$(N(2))\nPRINT STR$(N(2) * 2)");
    TW_CHECK_EQ(run.output.size(), std::size_t{2});
    TW_CHECK_EQ(run.output.at(0), std::string{"X7"});
    TW_CHECK_EQ(run.output.at(1), std::string{"14"});
}

// A `FUNCTION ... END FUNCTION` the program defined is callable from an expression:
// the body's statements are run by the machine, parameters are bound and restored,
// and what the body printed appears before the line that called it.
void test_basic_user_functions_run_their_bodies() {
    const BasicRun run = run_basic(
        "PRINT TWICE(21)\nPRINT TOTAL(4)\nPRINT GRADE$(77)\nEND\n"
        "FUNCTION TWICE(X)\nTWICE = X * 2\nEND FUNCTION\n"
        "FUNCTION TOTAL(N)\nSUM = 0\nFOR K = 1 TO N\nSUM = SUM + K\nNEXT K\n"
        "PRINT \"inside\"\nTOTAL = SUM\nEND FUNCTION\n"
        "FUNCTION GRADE$(M)\nGRADE$ = \"C\"\nEND FUNCTION");
    TW_CHECK_EQ(run.output.size(), std::size_t{4});
    TW_CHECK_EQ(run.output.at(0), std::string{"42"});
    TW_CHECK_EQ(run.output.at(1), std::string{"inside"});
    TW_CHECK_EQ(run.output.at(2), std::string{"10"});
    TW_CHECK_EQ(run.output.at(3), std::string{"C"});
}

// The reference forwards a statement it does not recognise to the Logo
// executor when the first word is a Logo command, so a `.bas` file can use
// turtle words; a real typo is still reported.
void test_basic_logo_words_draw_through_the_fallback() {
    const BasicRun run = run_basic("CLEARSCREEN\nPENUP\nSETPENCOLOR \"YELLOW\"\n"
                                   "PENDOWN\nFORWARD 15\nRIGHT 90");
    TW_CHECK_EQ(run.host.ops.at(0), std::string{"reset"});
    TW_CHECK_EQ(run.host.ops.at(1), std::string{"penup"});
    TW_CHECK_EQ(run.host.ops.at(3), std::string{"pendown"});
    TW_CHECK_EQ(run.host.ops.at(4), std::string{"forward 15"});
    TW_CHECK_EQ(run.host.ops.at(5), std::string{"right 90"});
    TW_CHECK(!any_error(run.output));
}

void test_basic_unknown_word_is_still_an_error() {
    const BasicRun run = run_basic("FLURB 3");
    TW_CHECK(contains(run.output, "Unknown BASIC command: FLURB 3"));
}

// SAY refuses, exactly as the reference does when its optional `pyttsx3` is
// absent. An acknowledgement would claim a sentence was spoken by a build with
// no speech synthesiser in it, and the parity gate compares this line.
void test_basic_say_refuses_without_a_backend() {
    const BasicRun run = run_basic("SAY \"HELLO\"\nA$ = \"WORLD\"\nSAY A$");
    TW_CHECK_EQ(run.output.at(0), std::string{"❌ [line 1] Speech synthesis not available"});
    TW_CHECK_EQ(run.output.at(1), std::string{"❌ [line 3] Speech synthesis not available"});
}

/// An inner loop with an empty body is legal BASIC, and its `NEXT` is the line
/// the loop returns to. The run loop sees a jump by comparing the program
/// counter before and after a statement, so that jump is invisible; before this
/// was handled, the inner frame was left on the stack and the *outer* loop's
/// `NEXT` consumed it, so the outer loop ran once and said nothing.
void test_basic_an_empty_loop_body_does_not_end_its_outer_loop() {
    const BasicRun run = run_basic(
        "10 FOR F = 1 TO 3\n"
        "20 LET C = C + 1\n"
        "30 FOR I = 1 TO 4\n"
        "40 NEXT I\n"
        "50 NEXT F\n"
        "60 PRINT C\n");
    TW_CHECK_EQ(run.output.size(), std::size_t{1});
    if (run.output.empty()) {
        return;
    }
    TW_CHECK_EQ(run.output.at(0), std::string{"3"});
    TW_CHECK(!any_error(run.output));
    // A bare `NEXT I` is enough: the inner loop must also be counted, and the
    // variable it leaves behind is the first value past its limit.
    const BasicRun counted = run_basic(
        "10 FOR I = 1 TO 4\n"
        "20 NEXT I\n"
        "30 PRINT I\n");
    TW_CHECK_EQ(counted.output.size(), std::size_t{1});
    if (!counted.output.empty()) {
        TW_CHECK_EQ(counted.output.at(0), std::string{"5"});
    }
}

/// The shipped game is a gate as much as a sample. It is the one program that
/// uses SCREEN, sprites, POINT and an empty-bodied loop together, and "it ran
/// and drew nothing" is the failure this project treats as the bug.
void test_the_screen_game_example_runs_and_draws() {
#ifdef TIME_WARP_TEST_EXAMPLES
    const std::string path =
        std::string{TIME_WARP_TEST_EXAMPLES} + "/basic/star_patrol.bas";
    std::ifstream file{path};
    TW_CHECK(file.good());
    if (!file) {
        return;
    }
    const std::string source{std::istreambuf_iterator<char>{file},
                             std::istreambuf_iterator<char>{}};
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.load_program(source);
    interpreter.run();

    for (const std::string& line : interpreter.output()) {
        TW_CHECK(line.find("❌") == std::string::npos);
    }

    bool asked_for_a_screen = false;
    bool placed_a_sprite = false;
    bool read_a_pixel = false;
    for (const std::string& op : host.ops) {
        if (op.rfind("screen 320 200", 0) == 0) {
            asked_for_a_screen = true;
        }
        if (op.rfind("blit", 0) == 0) {
            placed_a_sprite = true;
        }
        if (op.rfind("point", 0) == 0) {
            read_a_pixel = true;
        }
    }
    TW_CHECK(asked_for_a_screen);
    TW_CHECK(placed_a_sprite);
    TW_CHECK(read_a_pixel);
#else
    std::printf("-- (example path not configured)\n");
#endif
}

/// A runaway program ends with a sentence rather than a hung window. The budget
/// is the engine's, not the IDE's: a lesson check, a compiled program and the
/// window all get the same answer, and a suite that never mentions it is how a
/// removed cap goes unnoticed.
///
/// It also pins the other half of the jump fix: `10 GOTO 10` used to *end* the
/// program, because a jump to the line already executing is invisible to a
/// before/after comparison of the program counter. It loops until the cap now,
/// which is what makes this test terminate at all.
void test_a_runaway_program_stops_at_the_budget() {
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.load_program("10 GOTO 10\n");
    const auto started = std::chrono::steady_clock::now();
    interpreter.run();
    const double seconds =
        std::chrono::duration<double>(std::chrono::steady_clock::now() - started).count();
    // The iteration cap, not the clock: 100,000 statements take milliseconds, and
    // a test that waited ten seconds for the timeout would not be run often.
    TW_CHECK(interpreter.iteration_capped());
    TW_CHECK(!interpreter.running);
    TW_CHECK(seconds < 5.0);
    const std::vector<std::string> output = interpreter.output();
    TW_CHECK(std::any_of(output.begin(), output.end(), [](const std::string& line) {
        return line.find("Maximum iterations reached") != std::string::npos;
    }));
}

/// Profiling is measurement around the shared line loop: hits per line, time per
/// line, and a report ordered by that time.
void test_profiling_counts_each_line() {
    std::printf("-- profiler: per-line hits and time\n");
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.set_profiling(true);
    interpreter.load_program("10 FOR I = 1 TO 10\n20 PRINT I\n30 NEXT I\n");
    interpreter.run();

    TW_CHECK_EQ(interpreter.output().size(), std::size_t{10});
    const std::vector<timewarp::lang::LineProfile> report = interpreter.line_profiles();
    // Only the lines that ran are in the report; all three of these did.
    TW_CHECK_EQ(report.size(), std::size_t{3});
    const auto body = std::find_if(report.begin(), report.end(),
                                   [](const timewarp::lang::LineProfile& row) {
                                       return row.line == 20;
                                   });
    TW_CHECK(body != report.end());
    if (body != report.end()) {
        TW_CHECK_EQ(body->hits, std::uint64_t{10});
    }
    // Longest first, with the line number breaking ties so two runs that measure
    // the same times print the same order.
    TW_CHECK(std::is_sorted(
        report.begin(), report.end(),
        [](const timewarp::lang::LineProfile& left, const timewarp::lang::LineProfile& right) {
            if (left.elapsed_ms != right.elapsed_ms) {
                return left.elapsed_ms > right.elapsed_ms;
            }
            return left.line < right.line;
        }));
}

void test_profiling_is_off_unless_asked() {
    std::printf("-- profiler: off unless asked\n");
    RecordingHost host;
    Interpreter plain{Language::Basic, host};
    plain.load_program("10 PRINT \"hi\"\n20 PRINT \"there\"\n");
    plain.run();
    TW_CHECK(!plain.profiling());
    TW_CHECK(plain.line_profiles().empty());

    RecordingHost profiled_host;
    Interpreter profiled{Language::Basic, profiled_host};
    profiled.set_profiling(true);
    profiled.load_program("10 PRINT \"hi\"\n20 PRINT \"there\"\n");
    profiled.run();
    // Measurement, not a change of execution: same steps, same output. If a
    // clock read changed the run, the telemetry would stop being comparable
    // between a profiled run and an ordinary one.
    TW_CHECK_EQ(profiled.steps_used(), plain.steps_used());
    TW_CHECK(profiled.output() == plain.output());
    TW_CHECK_EQ(profiled.line_profiles().size(), std::size_t{2});
}

void test_basic_loops_and_subroutines_run() {
    const BasicRun run = run_basic(
        "10 TOTAL = 0\n20 FOR I = 1 TO 3\n30 GOSUB 100\n40 NEXT I\n50 PRINT TOTAL\n"
        "60 END\n100 TOTAL = TOTAL + I\n110 RETURN");
    TW_CHECK_EQ(run.output.at(0), std::string{"6"});
}

// `INPUT` is the one statement whose work happens outside the engine, so both
// halves of the round trip are asserted here: the caller is asked with the
// program's own prompt, and the answer comes back in the variable the statement
// names. A prompt nobody receives and an answer the program never reads look
// identical from the console -- the program simply goes quiet.
void test_basic_input_prompt_and_answer_round_trip() {
    const BasicRun run = run_basic("10 INPUT \"NAME\"; N$\n20 PRINT \"HELLO \"; N$\n",
                                   {"Ada"});
    TW_CHECK_EQ(run.prompts.size(), std::size_t{1});
    if (!run.prompts.empty()) {
        // The reference's prompt keeps the literal's trailing space, which is
        // what separates the question from what the person types.
        TW_CHECK_EQ(run.prompts.at(0), std::string{"NAME "});
    }
    TW_CHECK(contains(run.output, "Ada"));
    TW_CHECK(!any_error(run.output));
}

// A numeric `INPUT` parses what was typed, and an answer nobody gave leaves the
// variable at 0 -- BASIC's value for an unset number -- rather than stopping the
// program at the prompt.
void test_basic_numeric_input_is_coerced() {
    const BasicRun answered = run_basic("10 INPUT \"N\"; N\n20 PRINT N * 2\n", {"21"});
    TW_CHECK_EQ(answered.output.size(), std::size_t{1});
    if (!answered.output.empty()) {
        TW_CHECK_EQ(answered.output.at(0), std::string{"42"});
    }

    const BasicRun unanswered = run_basic("10 INPUT \"N\"; N\n20 PRINT N * 2\n");
    TW_CHECK_EQ(unanswered.output.size(), std::size_t{1});
    if (!unanswered.output.empty()) {
        TW_CHECK_EQ(unanswered.output.at(0), std::string{"0"});
    }
}

// `POINT` answers what is on the screen the program drew on, packed as
// `&H00RRGGBB`. The division is in the program because PRINT's own format is
// six significant digits -- 16711680 would print as 1.67117e+07 -- and the
// pixel is what the test is about, not the number's spelling.
void test_basic_point_reads_back_the_pixel_it_drew() {
    const BasicRun run = run_basic(
        "10 SCREEN 1\n"
        "20 PSET (10, 10), #FF0000\n"
        "30 PRINT POINT(10, 10) / 65536\n"
        "40 PRINT POINT(11, 11)\n");
    TW_CHECK_EQ(run.output.size(), std::size_t{3});
    if (run.output.size() != 3) {
        return;
    }
    TW_CHECK_EQ(run.output.at(0), std::string{"🎨 Graphics mode (800x600)"});
    TW_CHECK_EQ(run.output.at(1), std::string{"255"});
    // A pixel nobody painted is the screen's background.
    TW_CHECK_EQ(run.output.at(2), std::string{"0"});
    TW_CHECK(!any_error(run.output));
}

// `SCREEN n` picks a size, and the size is the one that number meant. The
// surface a program reads back changes with it, which is what makes a game's
// coordinates portable between the modes.
void test_basic_screen_modes_choose_the_screen_size() {
    const BasicRun run = run_basic(
        "10 SCREEN 13\n"
        "20 PSET (319, 199), #FF0000\n"
        "30 PRINT POINT(319, 199) / 65536\n"
        "40 SCREEN 9\n"
        "50 PRINT POINT(319, 199)\n"
        "60 SCREEN 99\n");
    TW_CHECK_EQ(run.output.size(), std::size_t{5});
    if (run.output.size() != 5) {
        return;
    }
    TW_CHECK_EQ(run.output.at(0), std::string{"🎨 Graphics mode (320x200)"});
    TW_CHECK_EQ(run.output.at(1), std::string{"255"});
    // A new mode is a new, blank screen: the pixel the last one had is not on
    // this one, and asking about it answers the background rather than the old
    // picture.
    TW_CHECK_EQ(run.output.at(2), std::string{"🎨 Graphics mode (640x350)"});
    TW_CHECK_EQ(run.output.at(3), std::string{"0"});
    // A number that never was a mode names the ones that were, rather than only
    // saying no.
    TW_CHECK(run.output.at(4).find("not one of 0, 1, 2, 7, 9, 12, 13") !=
            std::string::npos);
}

// -1 is how a program asks whether it has a screen at all, and it is the answer
// in text mode as well as before any `SCREEN`.
void test_basic_point_without_a_screen_answers_minus_one() {
    const BasicRun run = run_basic(
        "10 PRINT POINT(0, 0)\n"
        "20 SCREEN 1\n"
        "30 PSET (10, 10), #FF0000\n"
        "40 SCREEN 0\n"
        "50 PRINT POINT(10, 10)\n");
    TW_CHECK_EQ(run.output.size(), std::size_t{4});
    if (run.output.size() != 4) {
        return;
    }
    TW_CHECK_EQ(run.output.at(0), std::string{"-1"});
    TW_CHECK_EQ(run.output.at(2), std::string{"🎨 Text mode (80x25)"});
    TW_CHECK_EQ(run.output.at(3), std::string{"-1"});
}

// `GET` captures a block into an array that carries its own size, `PUT` restores
// it elsewhere, and both halves are observable through `POINT` -- a capture that
// is remembered but never restored, and a restore nobody can read back, would
// look identical from the console.
void test_basic_get_and_put_round_trip_a_sprite() {
    const BasicRun run = run_basic(
        "10 SCREEN 1\n"
        "20 PSET (5, 5), #00FF00\n"
        "30 GET (5, 5)-(6, 6), A\n"
        "40 PUT (20, 20), A\n"
        "50 PRINT POINT(20, 20) / 256\n"
        "60 PRINT POINT(21, 21)\n");
    TW_CHECK_EQ(run.output.size(), std::size_t{3});
    if (run.output.size() != 3) {
        return;
    }
    TW_CHECK_EQ(run.output.at(1), std::string{"255"});
    TW_CHECK_EQ(run.output.at(2), std::string{"0"});
    // The surface is told as well, at the size the sprite really has.
    TW_CHECK(contains(run.host.ops, "blit 20 20 2 2"));
    TW_CHECK(!any_error(run.output));
}

// `PAINT` fills the screen a program can read back with the colour the surface
// was just given. For `PAINT (x, y), 4` that is the reference's white, because
// its own comma split reads the middle field of the point -- and a raster that
// resolved the colour "correctly" would disagree with the fill a learner can
// see, which is the worse of the two answers. This pins the choice.
void test_basic_paint_fills_with_what_the_surface_was_given() {
    const BasicRun run = run_basic(
        "10 SCREEN 1\n"
        "20 LINE (0, 0)-(4, 4), #0000FF, B\n"
        "30 PAINT (2, 2), 4\n"
        "40 PRINT INT(POINT(2, 2) / 65536)\n");
    TW_CHECK_EQ(run.output.size(), std::size_t{3});
    if (run.output.size() != 3) {
        return;
    }
    // 255 is white; palette index 4 would have answered 0 red, which is the
    // colour the surface was *not* told to fill with.
    TW_CHECK_EQ(run.output.at(2), std::string{"255"});
}

void test_basic_get_needs_a_graphics_screen() {
    const BasicRun run = run_basic("GET (0, 0)-(1, 1), A");
    TW_CHECK(contains(run.output, "GET needs a graphics screen"));
    TW_CHECK(any_error(run.output));
}

// `CLS` clears the screen a program can read back, not only the canvas: a
// program that clears and then asks `POINT` must be told the pixel is gone.
void test_basic_cls_clears_what_point_reads() {
    const BasicRun run = run_basic(
        "10 SCREEN 1\n"
        "20 PSET (10, 10), #FF0000\n"
        "30 CLS\n"
        "40 PRINT POINT(10, 10)\n");
    TW_CHECK_EQ(run.output.size(), std::size_t{3});
    if (run.output.size() != 3) {
        return;
    }
    TW_CHECK_EQ(run.output.at(2), std::string{"0"});
}

// `STICK`/`STRIG` answer 0 until `JOYINIT` has armed them, so a joystick plugged
// into the machine cannot change a program that never asked to read it.
void test_basic_stick_is_neutral_before_joyinit() {
    const BasicRun run = run_basic("PRINT STICK(0)\nPRINT STRIG(0)");
    TW_CHECK_EQ(run.output.size(), std::size_t{2});
    if (run.output.size() == 2) {
        TW_CHECK_EQ(run.output.at(0), std::string{"0"});
        TW_CHECK_EQ(run.output.at(1), std::string{"0"});
    }

    // `JOYINIT` is hardware: it either arms the pad or says there is none. Both
    // are answers, and the second is not an error the learner caused.
    const BasicRun init = run_basic("JOYINIT");
    TW_CHECK_EQ(init.output.size(), std::size_t{1});
    TW_CHECK(!any_error(init.output));
    TW_CHECK(contains(init.output, "amepad"));
}

// The device reports an axis in -32767..32767; `STICK` promises 0-255 with a
// resting stick at 128. Truncating the midpoint would answer 127, so the
// conversion rounds -- and a driver that reports the out-of-range -32768 must
// clamp to 0 rather than wrap to the top.
void test_gamepad_axis_scaling_is_centred() {
    TW_CHECK_EQ(timewarp::io::scale_axis(-32767), 0);
    TW_CHECK_EQ(timewarp::io::scale_axis(-32768), 0);
    TW_CHECK_EQ(timewarp::io::scale_axis(0), 128);
    TW_CHECK_EQ(timewarp::io::scale_axis(32767), 255);
    TW_CHECK_EQ(timewarp::io::scale_axis(32768), 255);
    TW_CHECK_EQ(timewarp::io::scale_axis(-100000), 0);
}


// BASIC is case-insensitive and the expression evaluator was not: the statement
// that stored an array upper-cased the name, and the `Variable` branch upper-cased
// before reading, but a *call* arrived at the lookup spelled exactly as it was
// written. `DIM dat(3)` then `PRINT dat(0)` was "Unknown function: dat" while
// `PRINT DAT(0)` worked -- and `Examples/basic/math_explorer.bas` is written in
// lower case, so the example only ever ran because the smoke harness ran it
// through the reference engine.
void test_a_lowercase_array_name_is_the_same_array() {
    const BasicRun run = run_basic(
        "10 DIM dat(3)\n"
        "20 dat(0) = 5\n"
        "30 PRINT dat(0)\n"
        "40 PRINT dat(0) - 1\n"
        "50 d = dat(0) * 2\n"
        "60 PRINT d\n"
        "70 PRINT DAT(0)\n");
    TW_CHECK_EQ(run.output.size(), std::size_t{4});
    TW_CHECK_EQ(run.output.at(0), std::string{"5"});
    TW_CHECK_EQ(run.output.at(1), std::string{"4"});
    TW_CHECK_EQ(run.output.at(2), std::string{"10"});
    TW_CHECK_EQ(run.output.at(3), std::string{"5"});
}

// A program's own directory is a fence, and a fence has to be made of
// components. The check was a string-prefix test, and `/tmp/run` is a prefix of
// `/tmp/run2` -- so a program confined to the first could reach a sibling whose
// name only started the same way. That matters because a class's programs are
// marked unattended: nobody reads the source before it runs.
void test_file_access_is_confined_to_the_directory() {
    namespace fs = std::filesystem;
    const fs::path root = fs::temp_directory_path() / "timewarp-lang-confinement";
    const fs::path inside = root / "run";
    const fs::path sibling = root / "run2";
    std::error_code ignored;
    fs::remove_all(root, ignored);
    fs::create_directories(inside, ignored);
    fs::create_directories(sibling, ignored);

    const fs::path started_in = fs::current_path();
    std::error_code error;
    fs::current_path(inside, error);
    TW_CHECK(!error);

    const BasicRun escaped = run_basic(
        "10 OPEN \"../run2/escaped.txt\" FOR OUTPUT AS #1\n"
        "20 PRINT #1, \"x\"\n"
        "30 CLOSE #1\n");
    const bool reached_a_sibling = fs::exists(sibling / "escaped.txt");

    // The fence confines rather than refuses: a name inside is still written.
    const BasicRun wrote = run_basic(
        "10 OPEN \"notes.txt\" FOR OUTPUT AS #1\n"
        "20 PRINT #1, \"x\"\n"
        "30 CLOSE #1\n");
    const bool wrote_inside = fs::exists(inside / "notes.txt");

    fs::current_path(started_in, ignored);
    fs::remove_all(root, ignored);

    TW_CHECK(contains(escaped.output, "escapes sandbox"));
    TW_CHECK(!reached_a_sibling);
    TW_CHECK(contains(wrote.output, "Opened file 'notes.txt'"));
    TW_CHECK(wrote_inside);
}

// The Debug panel's stack list is what says *where* a stopped BASIC program is,
// and a BASIC return stack is a list of line indices that nothing published: a
// learner inside a GOSUB saw the paused line and no idea who called it. Each
// statement now mirrors its frame on the interpreter, so a pause reports the
// chain -- outermost first, a waiting frame at its call site and the innermost at
// the line it is standing on.
void test_basic_call_chain_names_the_subroutines() {
    std::printf("-- debug: basic call chain\n");
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    timewarp::lang::DebugSession session;
    session.set_breakpoints({6});  // line 200, inside the inner GOSUB
    std::vector<timewarp::lang::CallFrame> frames;
    int pauses = 0;
    int paused_at = 0;
    session.on_paused = [&](int line) {
        ++pauses;
        paused_at = line;
        frames = interpreter.call_stack();
        session.request_continue();
    };
    interpreter.set_debug_session(&session);
    interpreter.load_program(
        "10 GOSUB 100\n"
        "20 PRINT \"END\"\n"
        "30 END\n"
        "100 LET A = 1\n"
        "110 GOSUB 200\n"
        "120 RETURN\n"
        "200 LET B = 2\n"
        "210 RETURN\n");
    interpreter.run();

    TW_CHECK_EQ(pauses, 1);
    TW_CHECK_EQ(paused_at, 200);
    TW_CHECK_EQ(frames.size(), std::size_t{3});
    if (frames.size() == 3) {
        // The program's own frame waits at the call it is inside, not at the line
        // it started at.
        TW_CHECK_EQ(frames.at(0).procedure, std::string{"PROGRAM"});
        TW_CHECK_EQ(frames.at(0).line, 10);
        TW_CHECK_EQ(frames.at(1).procedure, std::string{"GOSUB"});
        TW_CHECK_EQ(frames.at(1).line, 110);
        // The innermost frame is where the run is.
        TW_CHECK_EQ(frames.at(2).procedure, std::string{"GOSUB"});
        TW_CHECK_EQ(frames.at(2).line, 200);
    }
    // Each RETURN took its frame back, so a finished run has no chain left to
    // report -- and neither does the next pause, which would otherwise name a
    // return stack that has already unwound.
    TW_CHECK(interpreter.call_stack().empty());
}

}  // namespace tw::lang_tests
