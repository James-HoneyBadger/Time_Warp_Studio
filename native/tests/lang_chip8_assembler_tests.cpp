// SPDX-License-Identifier: MIT
//
// CHIP-8 assembly and the debugger that pauses it.

#include "lang/support.hpp"

namespace tw::lang_tests {

// ---------------------------------------------------------------------------
// CHIP-8: assembler
// ---------------------------------------------------------------------------

void test_chip8_program_starts_at_0x200() {
    const Chip8Program program = assemble("start: CLS");
    TW_CHECK_EQ(program.labels.at("start"), 0x200);
    TW_CHECK_EQ(static_cast<int>(program.memory[0x200]), 0x00);
    TW_CHECK_EQ(static_cast<int>(program.memory[0x201]), 0xE0);
}

void test_chip8_labels_resolve_forward_references() {
    const Chip8Program program = assemble("start: JP later\nlater: CLS");
    TW_CHECK_EQ(program.labels.at("later"), 0x202);
    TW_CHECK_EQ(static_cast<int>(program.memory[0x200]), 0x12);
    TW_CHECK_EQ(static_cast<int>(program.memory[0x201]), 0x02);
}

void test_chip8_both_hex_spellings_agree() {
    const Chip8Program prefixed = assemble("start: LD V0, 0x41");
    const Chip8Program suffixed = assemble("start: LD V0, 41h");
    const Chip8Program bare = assemble("start: LD V0, 41");
    TW_CHECK(prefixed.memory[0x200] == suffixed.memory[0x200] &&
             prefixed.memory[0x201] == suffixed.memory[0x201]);
    // A bare number is decimal, which is why the marked forms exist.
    TW_CHECK(bare.memory[0x201] != prefixed.memory[0x201]);
}

void test_chip8_font_is_present_in_memory() {
    const Chip8Program program = assemble("start: HALT");
    TW_CHECK_EQ(static_cast<int>(program.memory[0x50]), 0xF0);
}

void test_chip8_db_and_dw_emit_data() {
    const Chip8Program program = assemble("start: HALT\ndata: DB 0xAB\n      DW 0x1234");
    const std::size_t data = static_cast<std::size_t>(program.labels.at("data"));
    TW_CHECK_EQ(static_cast<int>(program.memory[data]), 0xAB);
    TW_CHECK_EQ(static_cast<int>(program.memory[data + 1]), 0x12);
    TW_CHECK_EQ(static_cast<int>(program.memory[data + 2]), 0x34);
}

void test_the_debug_session_pauses_and_resumes() {
    std::printf("-- debug: breakpoint, step and continue\n");
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.load_program("10 PRINT \"one\"\n20 PRINT \"two\"\n30 PRINT \"three\"\n");

    timewarp::lang::DebugSession session;
    // Zero-based indices, blank lines included: the loader keeps every line, which
    // is what makes an editor line usable as a statement index.
    session.set_breakpoints({1});
    std::mutex mutex;
    std::condition_variable paused;
    int paused_line = -1;
    session.on_paused = [&](int line) {
        {
            const std::lock_guard<std::mutex> lock{mutex};
            paused_line = line;
        }
        paused.notify_all();
    };
    interpreter.set_debug_session(&session);

    std::thread worker{[&] { interpreter.run(); }};
    const auto wait_for_pause = [&](int expected) {
        std::unique_lock lock{mutex};
        paused.wait_for(lock, std::chrono::seconds{5},
                        [&] { return paused_line == expected; });
        return paused_line == expected;
    };

    // Before line 20, not after it: nothing on line 20 has been printed yet.
    TW_CHECK(wait_for_pause(20));
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{1});
    TW_CHECK_EQ(interpreter.output().front(), std::string{"one"});

    // One step runs exactly one statement.
    {
        const std::lock_guard<std::mutex> lock{mutex};
        paused_line = -1;
    }
    session.request_step();
    TW_CHECK(wait_for_pause(30));
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{2});

    // Continue from there runs to the end, with no pause left to take.
    session.request_continue();
    worker.join();
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{3});
    TW_CHECK(!session.paused());
}

void test_stopping_a_paused_run_unwinds_it() {
    std::printf("-- debug: stop while paused\n");
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.load_program("10 PRINT \"one\"\n20 PRINT \"two\"\n");
    timewarp::lang::DebugSession session;
    session.set_breakpoints({0});
    std::mutex mutex;
    std::condition_variable paused;
    bool hit = false;
    session.on_paused = [&](int) {
        {
            const std::lock_guard<std::mutex> lock{mutex};
            hit = true;
        }
        paused.notify_all();
    };
    interpreter.set_debug_session(&session);

    std::thread worker{[&] { interpreter.run(); }};
    {
        std::unique_lock lock{mutex};
        paused.wait_for(lock, std::chrono::seconds{5}, [&] { return hit; });
    }
    TW_CHECK(hit);
    TW_CHECK(session.paused());
    session.request_stop();
    worker.join();
    // The run ended cleanly and reported no error: a stop is not a crash.
    TW_CHECK(interpreter.error().empty());
    TW_CHECK(interpreter.output().empty());
}

/// One pause recorder for the whole-program tests: the engine thread publishes a
/// pause (or the end of the run), the test thread waits for it.
struct PauseRecorder {
    std::mutex mutex;
    std::condition_variable changed;
    int line = -1;
    bool finished = false;

    void attach(timewarp::lang::DebugSession& session) {
        session.on_paused = [this](int paused) {
            {
                const std::lock_guard<std::mutex> lock{mutex};
                line = paused;
            }
            changed.notify_all();
        };
    }

    [[nodiscard]] bool wait_line(int expected) {
        std::unique_lock lock{mutex};
        return changed.wait_for(lock, std::chrono::seconds{5},
                                [&] { return line == expected; });
    }

    [[nodiscard]] bool wait_finished() {
        std::unique_lock lock{mutex};
        return changed.wait_for(lock, std::chrono::seconds{5},
                                [&] { return finished; });
    }

    void finish() {
        {
            const std::lock_guard<std::mutex> lock{mutex};
            finished = true;
        }
        changed.notify_all();
    }
};

void test_logo_stops_at_a_breakpoint_and_steps() {
    std::printf("-- debug: logo statement\n");
    RecordingHost host;
    Interpreter interpreter{Language::Logo, host};
    interpreter.load_program("FD 10\nRT 90\nFD 20\n");

    timewarp::lang::DebugSession session;
    PauseRecorder recorder;
    recorder.attach(session);
    // Zero-based source line 1: the second statement.
    session.set_breakpoints({1});
    interpreter.set_debug_session(&session);

    std::thread worker{[&] {
        interpreter.run();
        recorder.finish();
    }};

    TW_CHECK(recorder.wait_line(2));
    // Before the statement on line 2 runs: only the first turn is on the host.
    TW_CHECK_EQ(host.ops.size(), std::size_t{1});

    // One step is one top-level statement.
    session.request_step();
    TW_CHECK(recorder.wait_line(3));
    TW_CHECK_EQ(host.ops.size(), std::size_t{2});

    session.request_continue();
    TW_CHECK(recorder.wait_finished());
    worker.join();
    TW_CHECK_EQ(host.ops.size(), std::size_t{3});
}

void test_smalltalk_stops_at_a_message_send() {
    std::printf("-- debug: smalltalk send\n");
    RecordingHost host;
    Interpreter interpreter{Language::Smalltalk, host};
    interpreter.load_program("| t |\nt := Turtle new.\nt forward: 100.\nt right: 90.\n");

    timewarp::lang::DebugSession session;
    PauseRecorder recorder;
    recorder.attach(session);
    // Zero-based source line 2, which holds `t forward: 100.`.
    session.set_breakpoints({2});
    interpreter.set_debug_session(&session);

    std::thread worker{[&] {
        interpreter.run();
        recorder.finish();
    }};

    TW_CHECK(recorder.wait_line(3));
    // Paused before the send, so the turtle has not moved.
    TW_CHECK(host.ops.empty());

    // The next send is `t right: 90.` on line 4.
    session.request_step();
    TW_CHECK(recorder.wait_line(4));
    TW_CHECK_EQ(host.ops.size(), std::size_t{1});

    session.request_continue();
    TW_CHECK(recorder.wait_finished());
    worker.join();
    TW_CHECK_EQ(host.ops.size(), std::size_t{2});
}

void test_chip8_stops_before_an_instruction() {
    std::printf("-- debug: chip-8 opcode\n");
    RecordingHost host;
    Interpreter interpreter{Language::Chip8, host};
    interpreter.load_program("start: LD V0, 1\n       LD V1, 2\n       HALT\n");

    timewarp::lang::DebugSession session;
    PauseRecorder recorder;
    recorder.attach(session);
    // Zero-based line 0: the first instruction, whose opcode address the
    // assembler records so a gutter breakpoint names a line rather than an
    // address.
    session.set_breakpoints({0});
    interpreter.set_debug_session(&session);

    std::thread worker{[&] {
        interpreter.run();
        recorder.finish();
    }};

    TW_CHECK(recorder.wait_line(1));
    // One step is one instruction, so the next pause is the listing's line 2.
    session.request_step();
    TW_CHECK(recorder.wait_line(2));

    session.request_continue();
    TW_CHECK(recorder.wait_finished());
    worker.join();
}

void test_chip8_assembler_errors() {
    TW_CHECK(contains(run_chip8("start: FROB V0"), "not an instruction form"));
    TW_CHECK(contains(run_chip8("start: LD V0, 999"), "not a byte"));
    TW_CHECK(contains(run_chip8("dup: CLS\ndup: CLS"), "defined twice"));
    TW_CHECK(contains(run_chip8("start: DRW V0, V1, 20"), "at most 15 rows"));
    TW_CHECK(contains(run_chip8("start: LD VG, 1"), "not a register"));
    TW_CHECK(contains(run_chip8("start: CLS\n       NOPE V0"), "line 2"));
    // Bnnn always adds V0; accepting another register would assemble an
    // instruction that silently ignores it.
    TW_CHECK(contains(run_chip8("start: JP V5, target\ntarget: HALT"), "V0 only"));
}

void test_chip8_sys_is_ignored() {
    // The 0NNN SYS call is an RCA 1802 routine this machine cannot run, and
    // every interpreter since the original ignores it rather than refusing a ROM
    // that contains one.
    const Chip8Machine assembled = run_to_halt(
        "start: SYS 0x123\n       LD V0, 7\n       HALT");
    TW_CHECK_EQ(static_cast<int>(assembled.v[0]), 7);
    const Chip8Machine raw = run_to_halt("start: DW 0x0123\n       LD V0, 9\n       HALT");
    TW_CHECK_EQ(static_cast<int>(raw.v[0]), 9);

    const Chip8Program program = assemble("start: SYS 0x123");
    TW_CHECK_EQ(static_cast<int>(program.memory[0x200]), 0x01);
    TW_CHECK_EQ(static_cast<int>(program.memory[0x201]), 0x23);
}

}  // namespace tw::lang_tests
