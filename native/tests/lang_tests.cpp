// SPDX-License-Identifier: MIT
//
// Tests for the native language engine.
//
// The two ported languages are tested the way the reference tests them: the
// CHIP-8 assembler and machine separately (an assembled wrong opcode and a
// wrongly executed right opcode are different bugs), and FOCAL through the
// interpreter so line numbering, jumps and error decoration are exercised too.
//
// No Qt and no display: `RecordingHost` is the drawing surface, so a test can
// assert what was drawn without a window.

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <condition_variable>
#include <cstdint>
#include <cstdio>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <map>
#include <mutex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <thread>
#include <utility>
#include <vector>

#include "check.hpp"
#include "timewarp/core/charconv.hpp"
#include "timewarp/lang/chip8.hpp"
#include "timewarp/io/gamepad.hpp"
#include "timewarp/lang/debug.hpp"
#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/python_random.hpp"
#include "timewarp/lang/raster.hpp"
#include "timewarp/lang/repl.hpp"
#include "timewarp/lang/turtle_host.hpp"
#include "timewarp/lang/watch.hpp"

namespace {

using timewarp::lang::Chip8Machine;
using timewarp::lang::Chip8Program;
using timewarp::lang::Interpreter;
using timewarp::lang::Language;
using timewarp::lang::PythonRandom;
using timewarp::lang::Raster;
using timewarp::lang::ReplSession;
using timewarp::lang::Rgb;
using timewarp::lang::TurtleHost;

std::string number_text(double value) {
    char buffer[64];
    const std::size_t length =
        timewarp::core::format_double(buffer, buffer + sizeof buffer, value);
    return std::string{buffer, length};
}

std::string colour_text(Rgb colour) {
    char buffer[8];
    std::snprintf(buffer, sizeof buffer, "#%02X%02X%02X", colour.red, colour.green,
                  colour.blue);
    return buffer;
}

/// Records drawing calls as text. The palette starts as the reference's
/// default surface, so a program that never sets a palette still resolves.
class RecordingHost final : public TurtleHost {
public:
    std::vector<std::string> ops;
    std::map<int, Rgb> palette{{0, {255, 255, 255}}, {1, {16, 16, 20}}, {2, {140, 255, 140}}};
    Rgb pen_{255, 255, 255};

    void forward(double distance) override { ops.push_back("forward " + number_text(distance)); }
    void back(double distance) override { ops.push_back("back " + number_text(distance)); }
    void right(double degrees) override { ops.push_back("right " + number_text(degrees)); }
    void left(double degrees) override { ops.push_back("left " + number_text(degrees)); }
    void goto_xy(double x, double y) override {
        ops.push_back("goto " + number_text(x) + " " + number_text(y));
    }
    void set_heading(double degrees) override {
        ops.push_back("heading " + number_text(degrees));
    }
    void home() override { ops.push_back("home"); }
    void clear() override { ops.push_back("clear"); }
    void reset() override { ops.push_back("reset"); }
    void enable_3d() override { ops.push_back("3d on"); }
    void disable_3d() override { ops.push_back("3d off"); }
    void pitch(double degrees) override { ops.push_back("pitch " + number_text(degrees)); }
    void roll(double degrees) override { ops.push_back("roll " + number_text(degrees)); }
    void pen_up() override { ops.push_back("penup"); }
    void pen_down() override { ops.push_back("pendown"); }
    void set_pen_width(double width) override {
        ops.push_back("width " + number_text(width));
    }
    void set_color(Rgb colour) override {
        pen_ = colour;
        ops.push_back("colour " + colour_text(colour));
    }
    void set_background(Rgb colour) override {
        ops.push_back("background " + colour_text(colour));
    }
    void circle(double radius, double /*extent*/) override {
        ops.push_back("circle " + number_text(radius));
    }
    void fill_last_shape(Rgb fill) override { ops.push_back("fill " + colour_text(fill)); }
    [[nodiscard]] Rgb pen_colour() const override { return pen_; }
    void draw_rect(double x0, double y0, double x1, double y1, Rgb fill) override {
        ops.push_back("rect " + number_text(x0) + " " + number_text(y0) + " " +
                      number_text(x1) + " " + number_text(y1) + " " + colour_text(fill));
    }
    void draw_text(double x, double y, const char* text, int font_size = 12) override {
        ops.push_back("text " + number_text(x) + " " + number_text(y) + " " +
                      std::to_string(font_size) + " " + text);
    }
    void draw_point(double x, double y, Rgb colour) override {
        ops.push_back("point " + number_text(x) + " " + number_text(y) + " " +
                      colour_text(colour));
    }
    void draw_polygon(const std::vector<timewarp::lang::Point>& points, Rgb stroke, Rgb fill,
                      double width, bool filled) override {
        std::string op = "polygon";
        for (const timewarp::lang::Point& point : points) {
            op += " " + number_text(point.x) + " " + number_text(point.y);
        }
        op += " " + colour_text(stroke);
        op += filled ? " " + colour_text(fill) : std::string{" -"};
        op += " " + number_text(width);
        ops.push_back(op);
    }
    void bezier(const std::vector<timewarp::lang::Point>& points, Rgb stroke,
                double width) override {
        std::string op = "bezier";
        for (const timewarp::lang::Point& point : points) {
            op += " " + number_text(point.x) + " " + number_text(point.y);
        }
        op += " " + colour_text(stroke) + " " + number_text(width);
        ops.push_back(op);
    }
    void draw_rect_gradient(double x0, double y0, double x1, double y1, double angle, Rgb from,
                            Rgb to) override {
        ops.push_back("gradient " + number_text(x0) + " " + number_text(y0) + " " +
                      number_text(x1) + " " + number_text(y1) + " " + colour_text(from) + " " +
                      colour_text(to) + " " + number_text(angle));
    }
    void draw_ellipse_gradient(double x0, double y0, double x1, double y1, Rgb from,
                               Rgb /*to*/) override {
        ops.push_back("egradient " + number_text(x0) + " " + number_text(y0) + " " +
                      number_text(x1) + " " + number_text(y1) + " " + colour_text(from));
    }
    void define_sprite(const char* name, int hotspot_x, int hotspot_y, int width,
                       int height, const std::vector<Rgb>& pixels) override {
        ops.push_back("sprite " + std::string{name} + " " + std::to_string(hotspot_x) + " " +
                      std::to_string(hotspot_y) + " " + std::to_string(width) + " " +
                      std::to_string(height) + " " + std::to_string(pixels.size()));
    }
    void place_sprite(const char* name, double x, double y) override {
        ops.push_back("place " + std::string{name} + " " + number_text(x) + " " +
                      number_text(y));
    }
    void move_sprite(const char* name, double dx, double dy) override {
        ops.push_back("move " + std::string{name} + " " + number_text(dx) + " " +
                      number_text(dy));
    }
    void set_sprite_visible(const char* name, bool visible) override {
        ops.push_back(std::string{visible ? "spriteshow " : "spritehide "} + name);
    }
    void put_pixels(int x, int y, int width, int height,
                    const std::vector<Rgb>& /*pixels*/) override {
        ops.push_back("blit " + std::to_string(x) + " " + std::to_string(y) + " " +
                      std::to_string(width) + " " + std::to_string(height));
    }
    void show_turtle() override { ops.push_back("show"); }
    void hide_turtle() override { ops.push_back("hide"); }
    void set_svga_mode(int width, int height) override {
        ops.push_back("svga " + std::to_string(width) + " " + std::to_string(height));
    }
    void set_framebuffer(int width, int height) override {
        ops.push_back("screen " + std::to_string(width) + " " + std::to_string(height));
    }
    void clear_framebuffer() override { ops.push_back("noscreen"); }
    void set_palette_color(int index, Rgb colour) override {
        palette[index] = colour;
        ops.push_back("palette " + std::to_string(index) + " " + colour_text(colour));
    }
    [[nodiscard]] Rgb resolve_color(int index) const override {
        const auto found = palette.find(index);
        return found == palette.end() ? Rgb{} : found->second;
    }
};

bool contains(const std::vector<std::string>& output, std::string_view fragment) {
    return std::any_of(output.begin(), output.end(), [fragment](const std::string& line) {
        return line.find(fragment) != std::string::npos;
    });
}

bool any_error(const std::vector<std::string>& output) {
    return std::any_of(output.begin(), output.end(), [](const std::string& line) {
        return line.rfind("❌", 0) == 0;
    });
}

Chip8Program assemble(std::string_view source) {
    return timewarp::lang::assemble_chip8(source);
}

Chip8Machine run_to_halt(std::string_view source, unsigned keys = 0) {
    Chip8Program program = assemble(source);
    Chip8Machine machine{std::move(program.memory), program.entry, keys};
    int guard = 0;
    while (!machine.halted()) {
        machine.step();
        if (++guard > 100'000) {
            throw std::runtime_error("the program did not halt");
        }
    }
    return machine;
}

std::size_t lit_pixels(const Chip8Machine& machine) {
    return static_cast<std::size_t>(
        std::count(machine.framebuffer().begin(), machine.framebuffer().end(), 1));
}

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

std::vector<std::string> run_chip8(std::string_view source) {
    RecordingHost host;
    Interpreter interpreter{Language::Chip8, host};
    interpreter.load_program(source);
    interpreter.run();
    return interpreter.output();
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

// ---------------------------------------------------------------------------
// CHIP-8: machine
// ---------------------------------------------------------------------------

void test_chip8_ld_and_add_with_a_byte() {
    const Chip8Machine machine = run_to_halt("start: LD V0, 5\n       ADD V0, 3\n       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 8);
}

void test_chip8_add_sets_the_carry_flag() {
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 200\n       LD V1, 100\n       ADD V0, V1\n       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 44);
    TW_CHECK_EQ(static_cast<int>(machine.v[0xF]), 1);
}

void test_chip8_subtract_sets_the_borrow_flag() {
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 5\n       LD V1, 9\n       SUB V0, V1\n       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 252);
    TW_CHECK_EQ(static_cast<int>(machine.v[0xF]), 0);
}

void test_chip8_shr_shifts_the_named_register_in_place() {
    const Chip8Machine machine =
        run_to_halt("start: LD V0, 8\n       LD V1, 1\n       SHR V0\n       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 4);
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.v[0xF]), 0);
}

void test_chip8_logic_and_shl() {
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 0x0C\n       LD V1, 0x0A\n"
        "       LD V2, 0x0C\n       OR V2, V1\n"       // 0x0E
        "       LD V3, 0x0C\n       AND V3, V1\n"      // 0x08
        "       LD V4, 0x0C\n       XOR V4, V1\n"      // 0x06
        "       LD V5, 3\n       LD V6, 10\n       SUBN V5, V6\n"  // 7
        "       LD V7, 0x81\n       SHL V7\n"          // 0x02, VF=1
        "       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[2]), 0x0E);
    TW_CHECK_EQ(static_cast<int>(machine.v[3]), 0x08);
    TW_CHECK_EQ(static_cast<int>(machine.v[4]), 0x06);
    TW_CHECK_EQ(static_cast<int>(machine.v[5]), 7);
    TW_CHECK_EQ(static_cast<int>(machine.v[7]), 2);
    TW_CHECK_EQ(static_cast<int>(machine.v[0xF]), 1);
}

void test_chip8_add_byte_wraps_without_carry() {
    const Chip8Machine machine =
        run_to_halt("start: LD V0, 250\n       ADD V0, 10\n       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 4);
    // 7XNN does not touch VF; only the arithmetic group does.
    TW_CHECK_EQ(static_cast<int>(machine.v[0xF]), 0);
}

void test_chip8_index_arithmetic() {
    const Chip8Machine machine = run_to_halt(
        "start: LD I, 0x300\n       LD V0, 5\n       ADD I, V0\n       HALT");
    TW_CHECK_EQ(machine.i, 0x305);
}

void test_chip8_keypad_is_live() {
    const auto run_with_key = [](bool down) {
        const Chip8Program program = assemble(
            "start: LD V0, 3\n       SKP V0\n       LD V1, 1\n       HALT");
        Chip8Machine machine{program.memory, program.entry};
        machine.set_interactive(true);
        machine.set_key(3, true);
        if (!down) {
            machine.set_key(3, false);
        }
        while (!machine.halted()) {
            machine.step();
        }
        return machine;
    };
    TW_CHECK_EQ(static_cast<int>(run_with_key(true).v[1]), 0);   // skipped
    TW_CHECK_EQ(static_cast<int>(run_with_key(false).v[1]), 1);  // not skipped
}

void test_chip8_ld_key_waits_for_a_press() {
    const Chip8Program program =
        assemble("start: LD V0, K\n       LD V1, 9\n       HALT");
    Chip8Machine machine{program.memory, program.entry};
    machine.set_interactive(true);
    machine.step();  // LD V0, K: no key held yet
    TW_CHECK(machine.waiting_for_key());
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 0);
    const int waiting_pc = machine.pc;
    machine.step();
    TW_CHECK(machine.waiting_for_key());
    TW_CHECK_EQ(machine.pc, waiting_pc);
    machine.set_key(7, true);
    machine.step();  // the wait completes, V0 gets the key
    TW_CHECK(!machine.waiting_for_key());
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 7);
    machine.step();
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 9);

    // A whole-program run keeps the reference's one-shot read: no key is 0.
    Chip8Machine legacy{program.memory, program.entry};
    legacy.step();
    TW_CHECK(!legacy.waiting_for_key());
    TW_CHECK_EQ(static_cast<int>(legacy.v[0]), 0);
}

void test_chip8_bcd_conversion_writes_three_digits() {
    const Chip8Program program = assemble(
        "start: LD V0, 205\n       LD I, data\n       LD B, V0\n       HALT\n"
        "data: DB 0\n      DB 0\n      DB 0");
    Chip8Machine machine{program.memory, 0x200};
    while (!machine.halted()) {
        machine.step();
    }
    const std::size_t start = static_cast<std::size_t>(program.labels.at("data"));
    TW_CHECK_EQ(static_cast<int>(machine.memory[start]), 2);
    TW_CHECK_EQ(static_cast<int>(machine.memory[start + 1]), 0);
    TW_CHECK_EQ(static_cast<int>(machine.memory[start + 2]), 5);
}

void test_chip8_store_and_load_through_i() {
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 1\n       LD V1, 2\n       LD I, buf\n       LD [I], V1\n"
        "       LD I, buf\n       LD V3, [I]\n       HALT\n"
        "buf:   DB 0\n       DB 0");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 2);
    TW_CHECK_EQ(static_cast<int>(machine.v[3]), 0);
}

void test_chip8_block_transfer_advances_i() {
    // The 1977 interpreter leaves I one past the block; CHIP-48 and SCHIP leave
    // it where it was.
    const Chip8Program program = assemble(
        "start: LD V0, 1\n       LD V1, 2\n       LD I, buf\n"
        "       LD [I], V1\n       HALT\nbuf: DB 0\n     DB 0");
    Chip8Machine machine{program.memory, program.entry};
    while (!machine.halted()) {
        machine.step();
    }
    TW_CHECK_EQ(machine.i, program.labels.at("buf") + 2);

    const Chip8Program load = assemble(
        "start: LD I, buf\n       LD V1, [I]\n       HALT\nbuf: DB 7\n     DB 9");
    Chip8Machine loaded{load.memory, load.entry};
    while (!loaded.halted()) {
        loaded.step();
    }
    TW_CHECK_EQ(static_cast<int>(loaded.v[0]), 7);
    TW_CHECK_EQ(static_cast<int>(loaded.v[1]), 9);
    TW_CHECK_EQ(loaded.i, load.labels.at("buf") + 2);
}

void test_chip8_skips() {
    TW_CHECK_EQ(
        static_cast<int>(run_to_halt("start: LD V0, 5\n       SE V0, 5\n       LD V1, 1\n       HALT")
                             .v[1]),
        0);
    TW_CHECK_EQ(
        static_cast<int>(run_to_halt("start: LD V0, 5\n       SNE V0, 5\n       LD V1, 1\n       HALT")
                             .v[1]),
        1);
    // 5XY0 and 9XY0 are the register forms; the low nibble is not a value.
    TW_CHECK_EQ(
        static_cast<int>(run_to_halt("start: LD V0, 5\n       LD V1, 5\n"
                                     "       SE V0, V1\n       LD V2, 1\n       HALT")
                             .v[2]),
        0);
    TW_CHECK_EQ(
        static_cast<int>(run_to_halt("start: LD V0, 5\n       LD V1, 5\n"
                                     "       SNE V0, V1\n       LD V2, 1\n       HALT")
                             .v[2]),
        1);
}

void test_chip8_call_and_return() {
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 7\n       CALL sub\n       HALT\nsub:   ADD V0, 1\n       RET");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 8);
    TW_CHECK(machine.stack.empty());
}

void test_chip8_ret_without_call_is_reported() {
    TW_CHECK(contains(run_chip8("start: RET"), "empty call stack"));
}

void test_chip8_draw_lights_the_expected_pixels() {
    // The glyph for 3 is 0xF0,0x10,0xF0,0x10,0xF0: four, one, four, one, four.
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 3\n       LD F, V0\n       LD V0, 0\n       LD V1, 0\n"
        "       DRW V0, V1, 5\n       HALT");
    TW_CHECK_EQ(lit_pixels(machine), std::size_t{14});
}

void test_chip8_drawing_twice_erases_and_reports_the_collision() {
    const Chip8Machine machine = run_to_halt(
        "start: LD I, dot\n       LD V0, 0\n       LD V1, 0\n"
        "       DRW V0, V1, 1\n       DRW V0, V1, 1\n       HALT\n"
        "dot:   DB 0x80");
    TW_CHECK_EQ(static_cast<int>(machine.v[0xF]), 1);
    TW_CHECK_EQ(lit_pixels(machine), std::size_t{0});
}

void test_chip8_pixels_wrap_around_the_screen() {
    const Chip8Machine machine = run_to_halt(
        "start: LD I, pair\n       LD V0, 63\n       LD V1, 0\n"
        "       DRW V0, V1, 1\n       HALT\npair:  DB 0xC0");
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[0]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[63]), 1);
}

void test_chip8_hires_mode_changes_the_grid_and_starts_blank() {
    const Chip8Machine hires = run_to_halt("start: DW 0x00FF\n       HALT");
    TW_CHECK(hires.hi_res());
    TW_CHECK_EQ(hires.screen_width(), timewarp::lang::kChip8HiResWidth);
    TW_CHECK_EQ(hires.screen_height(), timewarp::lang::kChip8HiResHeight);
    TW_CHECK_EQ(hires.framebuffer().size(),
                static_cast<std::size_t>(timewarp::lang::kChip8HiResWidth) *
                    timewarp::lang::kChip8HiResHeight);

    const Chip8Machine lores = run_to_halt(
        "start: LD I, dot\n"
        "       DRW V0, V1, 1\n"
        "       DW 0x00FF\n"
        "       DW 0x00FE\n"
        "       HALT\n"
        "dot:   DB 0xFF");
    TW_CHECK(!lores.hi_res());
    TW_CHECK_EQ(lores.framebuffer().size(),
                static_cast<std::size_t>(timewarp::lang::kChip8ScreenWidth) *
                    timewarp::lang::kChip8ScreenHeight);
    // A mode switch does not keep the old drawing: the grid changed under it.
    TW_CHECK_EQ(lit_pixels(lores), std::size_t{0});
}

void test_chip8_cls_in_hires_keeps_the_hi_res_grid() {
    // CLS is 00E0. In hi-res the grid is 128x64, and clearing has to keep it:
    // a 64x32 buffer here would let the next DRAW write outside the frame and
    // paint nothing below the sixteenth row.
    const Chip8Machine cleared = run_to_halt(
        "start: DW 0x00FF\n"
        "       CLS\n"
        "       HALT");
    TW_CHECK(cleared.hi_res());
    TW_CHECK_EQ(cleared.framebuffer().size(),
                static_cast<std::size_t>(timewarp::lang::kChip8HiResWidth) *
                    timewarp::lang::kChip8HiResHeight);

    const Chip8Machine bottom = run_to_halt(
        "start: DW 0x00FF\n"
        "       CLS\n"
        "       LD V0, 0\n"
        "       LD V1, 63\n"
        "       LD I, dot\n"
        "       DRW V0, V1, 1\n"
        "       HALT\n"
        "dot:   DB 0x80");
    TW_CHECK_EQ(bottom.framebuffer().size(),
                static_cast<std::size_t>(timewarp::lang::kChip8HiResWidth) *
                    timewarp::lang::kChip8HiResHeight);
    TW_CHECK_EQ(static_cast<int>(bottom.framebuffer()[63 * timewarp::lang::kChip8HiResWidth]),
                1);
}

void test_chip8_large_sprite_is_sixteen_by_sixteen_in_hires() {
    std::string rows;
    for (int index = 0; index < 16; ++index) {
        rows += "       DW 0xFFFF\n";
    }
    const Chip8Machine machine = run_to_halt(
        "start: DW 0x00FF\n"
        "       LD I, sprite\n"
        "       DRW V0, V1, 0\n"
        "       HALT\n"
        "sprite:\n" +
        rows);
    TW_CHECK_EQ(lit_pixels(machine), std::size_t{16 * 16});
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[0]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[15]), 1);
    TW_CHECK_EQ(
        static_cast<int>(machine.framebuffer()[15 * timewarp::lang::kChip8HiResWidth + 15]), 1);
}

void test_chip8_large_sprite_doubles_columns_in_lores() {
    std::string rows;
    for (int index = 0; index < 16; ++index) {
        rows += "       DB 0x80\n";
    }
    const Chip8Machine machine = run_to_halt(
        "start: LD I, sprite\n"
        "       DRW V0, V1, 0\n"
        "       HALT\n"
        "sprite:\n" +
        rows);
    // Each row's top bit becomes two pixels: 16 rows of two.
    TW_CHECK_EQ(lit_pixels(machine), std::size_t{32});
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[0]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[1]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[2]), 0);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[timewarp::lang::kChip8ScreenWidth]), 1);
}

void test_chip8_scroll_down_moves_the_drawing_and_clears_the_top() {
    const Chip8Machine machine = run_to_halt(
        "start: LD I, dot\n"
        "       DRW V0, V1, 1\n"
        "       DW 0x00C2\n"
        "       HALT\n"
        "dot:   DB 0x80");
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[2 * timewarp::lang::kChip8ScreenWidth]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[0]), 0);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[timewarp::lang::kChip8ScreenWidth]), 0);
}

void test_chip8_scroll_left_and_right_move_four_pixels() {
    const std::string base =
        "start: LD V0, 8\n"
        "       LD I, dot\n"
        "       DRW V0, V1, 1\n"
        "       DW 0x00FC\n"
        "       HALT\n"
        "dot:   DB 0x80";
    const Chip8Machine left = run_to_halt(base);
    TW_CHECK_EQ(static_cast<int>(left.framebuffer()[4]), 1);
    TW_CHECK_EQ(static_cast<int>(left.framebuffer()[8]), 0);

    std::string right_source = base;
    right_source.replace(right_source.find("0x00FC"), 6, "0x00FB");
    const Chip8Machine right = run_to_halt(right_source);
    TW_CHECK_EQ(static_cast<int>(right.framebuffer()[12]), 1);
    TW_CHECK_EQ(static_cast<int>(right.framebuffer()[8]), 0);
}

void test_chip8_exit_opcode_halts() {
    const Chip8Machine machine = run_to_halt("start: DW 0x00FD\n       LD V0, 9\n       HALT");
    TW_CHECK(machine.halted());
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 0);
}

void test_chip8_fx30_points_at_the_large_font() {
    const Chip8Machine machine = run_to_halt("start: LD V0, 3\n       DW 0xF030\n       HALT");
    TW_CHECK_EQ(machine.i, timewarp::lang::kChip8LargeFontAddress + 30);
}

void test_chip8_large_font_is_in_memory() {
    const Chip8Program program = assemble("start: HALT");
    // The small glyph for 0 is 0xF0, so the doubled first row is 0xFF twice.
    TW_CHECK_EQ(static_cast<int>(program.memory[timewarp::lang::kChip8LargeFontAddress]), 0xFF);
    TW_CHECK_EQ(static_cast<int>(program.memory[timewarp::lang::kChip8LargeFontAddress + 1]),
                0xFF);
}

void test_chip8_rpl_flags_round_trip() {
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 1\n"
        "       LD V1, 2\n"
        "       DW 0xF175\n"
        "       LD V0, 0\n"
        "       LD V1, 0\n"
        "       DW 0xF185\n"
        "       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 2);
}

void test_chip8_hires_display_halves_the_pixel_size() {
    RecordingHost host;
    Interpreter interpreter{Language::Chip8, host};
    interpreter.load_program(
        "start: DW 0x00FF\n       LD I, dot\n       DRW V0, V1, 1\n"
        "       HALT\ndot: DB 0xFF");
    interpreter.run();
    std::string lit;
    for (const std::string& op : host.ops) {
        if (op.rfind("rect ", 0) == 0 && op.find("#8CFF8C") != std::string::npos) {
            lit = op;
            break;
        }
    }
    TW_CHECK(!lit.empty());
    if (!lit.empty()) {
        std::istringstream stream{lit.substr(5)};
        double x0 = 0.0;
        double y0 = 0.0;
        double x1 = 0.0;
        double y1 = 0.0;
        std::string colour;
        stream >> x0 >> y0 >> x1 >> y1 >> colour;
        TW_CHECK(std::abs((x1 - x0) - 4.0) < 1e-9);
    }
}

void test_chip8_keys() {
    const std::string skip = "start: LD V0, 3\n       SKP V0\n       LD V1, 1\n       HALT";
    TW_CHECK_EQ(static_cast<int>(run_to_halt(skip, 1U << 3).v[1]), 0);
    TW_CHECK_EQ(static_cast<int>(run_to_halt(skip).v[1]), 1);

    const std::string skip_not =
        "start: LD V0, 3\n       SKNP V0\n       LD V1, 1\n       HALT";
    TW_CHECK_EQ(static_cast<int>(run_to_halt(skip_not).v[1]), 0);
    TW_CHECK_EQ(static_cast<int>(run_to_halt(skip_not, 1U << 3).v[1]), 1);

    TW_CHECK_EQ(static_cast<int>(run_to_halt("start: LD V0, K\n       HALT", 1U << 5).v[0]), 5);
    TW_CHECK_EQ(static_cast<int>(run_to_halt("start: LD V0, K\n       HALT").v[0]), 0);
}

void test_chip8_jump_with_offset_uses_v0() {
    const auto program = [](int offset) {
        return "start: LD V0, " + std::to_string(offset) +
               "\n       JP V0, target\n"
               "       LD V1, 1\n"
               "target: LD V2, 9\n"
               "       HALT";
    };
    TW_CHECK_EQ(static_cast<int>(run_to_halt(program(0)).v[2]), 9);
    // Two bytes past the label skips `LD V2, 9`, so V2 keeps its zero.
    const Chip8Machine machine = run_to_halt(program(2));
    TW_CHECK_EQ(static_cast<int>(machine.v[2]), 0);
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 0);
}

void test_chip8_timers() {
    const Chip8Program program = assemble(
        "start: LD V0, 2\n       LD DT, V0\n       LD ST, V0\n       LD V1, DT\n       HALT");
    Chip8Machine machine{program.memory, program.entry};
    while (!machine.halted()) {
        machine.step();
    }
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 2);
    TW_CHECK_EQ(machine.delay_timer, 2);
    TW_CHECK_EQ(machine.sound_timer, 2);
    // One tick per frame, never below zero.
    machine.tick_timers();
    TW_CHECK_EQ(machine.delay_timer, 1);
    TW_CHECK_EQ(machine.sound_timer, 1);
    machine.tick_timers();
    machine.tick_timers();
    TW_CHECK_EQ(machine.delay_timer, 0);
    TW_CHECK_EQ(machine.sound_timer, 0);
}

void test_chip8_frame_limit_is_information() {
    TW_CHECK(contains(run_chip8("start: JP start"), "600 frames"));
}

void test_chip8_unknown_opcode_is_reported() {
    // 0NNN is SYS and is ignored; Fx0FF is not an instruction.
    const std::vector<std::string> output = run_chip8("start: DW 0xF0FF");
    TW_CHECK(contains(output, "unknown opcode"));
    TW_CHECK(contains(output, "FxFF"));
}

void test_chip8_binary_rom_loads_at_0x200() {
    // LD V0, 5; LD V1, 7; HALT. The NUL in the HALT opcode is what marks this
    // as a ROM rather than a listing.
    const std::string rom{"\x60\x05\x61\x07\x00\x00", 6};
    const Chip8Machine machine = run_to_halt(rom);
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 5);
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 7);
    // The font is still where Fx29 points.
    TW_CHECK_EQ(static_cast<int>(machine.memory[0x50]), 0xF0);
}

void test_chip8_binary_rom_respects_memory() {
    std::string rom(static_cast<std::size_t>(timewarp::lang::kChip8RomLimit) + 1, '\0');
    rom[0] = '\x60';
    const std::vector<std::string> output = run_chip8(rom);
    TW_CHECK(contains(output, "memory from 0x200 holds 3584"));
}

// ---------------------------------------------------------------------------
// CHIP-8: drawing through the interpreter
// ---------------------------------------------------------------------------

void test_chip8_display_uses_the_palette() {
    RecordingHost host;
    Interpreter interpreter{Language::Chip8, host};
    interpreter.load_program("start: LD I, dot\n       DRW V0, V1, 1\n       HALT\ndot: DB 0xFF");
    interpreter.run();
    const std::size_t lit = static_cast<std::size_t>(
        std::count_if(host.ops.begin(), host.ops.end(), [](const std::string& op) {
            return op.rfind("rect ", 0) == 0 && op.find("#8CFF8C") != std::string::npos;
        }));
    TW_CHECK_EQ(lit, std::size_t{8});
}

void test_chip8_display_is_cleared_before_the_first_frame() {
    RecordingHost host;
    Interpreter interpreter{Language::Chip8, host};
    interpreter.load_program("start: CLS\n       HALT");
    interpreter.run();
    // svga, two palette entries, then the clear rectangle.
    TW_CHECK_EQ(host.ops.size(), std::size_t{4});
    TW_CHECK_EQ(host.ops.at(3), std::string{"rect -256 128 256 -128 #101014"});
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

void test_comal_turtle() {
    const ComalRun run = run_comal("10 MOVE 50\n20 TURN 90\n30 PLOT");
    TW_CHECK_EQ(run.host.ops.size(), std::size_t{3});
    TW_CHECK_EQ(run.host.ops.at(0), std::string{"forward 50"});
    TW_CHECK_EQ(run.host.ops.at(1), std::string{"right 90"});
    TW_CHECK(run.host.ops.at(2).rfind("point 0 0 #", 0) == 0);
}

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
// the pen stayed whatever it was and the picture came out the wrong colour.
void test_logo_setpc_with_a_palette_index_sets_the_pen() {
    const LogoRun run = run_logo("SETPC 1\nFD 10");
    TW_CHECK_EQ(run.host.ops.at(0), std::string{"colour #101014"});
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

// ---------------------------------------------------------------------------
// Python-compatible randomness
// ---------------------------------------------------------------------------

void test_python_random_matches_the_reference_sequence() {
    // CPython: `random.seed(1); [random.random() for _ in range(3)]`
    //   0.13436424411240122, 0.8474337369372327, 0.763774618976614
    PythonRandom random;
    random.seed(1);
    TW_CHECK_NEAR(random.random(), 0.13436424411240122, 1e-15);
    TW_CHECK_NEAR(random.random(), 0.8474337369372327, 1e-15);
    TW_CHECK_NEAR(random.random(), 0.763774618976614, 1e-15);
}

}  // namespace

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
    const BasicRun run = run_basic("DIM N(3)\nPRINT \"X\" + STR$(N(1))");
    TW_CHECK(any_error(run.output));
    TW_CHECK(contains(run.output, "Unexpected character"));
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

// ---------------------------------------------------------------------------
// REPL, watch expressions and data breakpoints
// ---------------------------------------------------------------------------

void test_repl_snippet_keeps_bindings() {
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.load_program("10 PRINT \"program\"\n");
    interpreter.run();
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{1});

    TW_CHECK(interpreter.run_snippet("LET X = 5").empty());
    TW_CHECK(interpreter.has_variable("X"));
    TW_CHECK_EQ(interpreter.number("X"), 5.0);

    // The second submission sees the first one's binding, and the program text
    // that was loaded before the REPL started is not run again.
    const std::string output = interpreter.run_snippet("PRINT X");
    TW_CHECK_EQ(output, std::string{"5\n"});
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{1});
}

void test_basic_question_mark_is_print() {
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.load_program("10 ? 6 * 7\n");
    interpreter.run();
    TW_CHECK(interpreter.error().empty());
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{1});
    TW_CHECK_EQ(interpreter.output().at(0), std::string{"42"});
}

void test_repl_new_clears_basic_bindings() {
    RecordingHost host;
    ReplSession session{Language::Basic, host};
    TW_CHECK(session.execute("LET X = 7").empty());
    TW_CHECK(session.variables().count("X") == 1);

    const std::string cleared = session.execute("  new  ");
    TW_CHECK(cleared.find("cleared") != std::string::npos);
    TW_CHECK(session.variables().empty());
    TW_CHECK(session.error().empty());

    // A different word is still an error, not a second spelling of NEW.
    const std::string typo = session.execute("NEWS");
    TW_CHECK(typo.find("\u274C") != std::string::npos);
}

void test_watch_expressions_use_the_paused_bindings() {
    std::map<std::string, timewarp::lang::Value> variables;
    variables["X"] = timewarp::lang::Value{5.0};
    variables["TITLE"] = timewarp::lang::Value{std::string{"Ada"}};

    const timewarp::lang::WatchResult arithmetic =
        timewarp::lang::evaluate_watch("X * 2 + 1", variables);
    TW_CHECK(arithmetic.ok);
    TW_CHECK_EQ(arithmetic.value, std::string{"11"});

    // Case-insensitive, like the engine's own variable lookup.
    const timewarp::lang::WatchResult lower =
        timewarp::lang::evaluate_watch("x + 1", variables);
    TW_CHECK(lower.ok);
    TW_CHECK_EQ(lower.value, std::string{"6"});

    const timewarp::lang::WatchResult text =
        timewarp::lang::evaluate_watch("TITLE", variables);
    TW_CHECK(text.ok);
    TW_CHECK_EQ(text.value, std::string{"Ada"});

    const timewarp::lang::WatchResult missing =
        timewarp::lang::evaluate_watch("NOPE", variables);
    TW_CHECK(!missing.ok);
    TW_CHECK_EQ(missing.value, std::string{"?"});
    TW_CHECK(!missing.error.empty());

    const timewarp::lang::WatchResult divided =
        timewarp::lang::evaluate_watch("1 / 0", variables);
    TW_CHECK(!divided.ok);
}

void test_sql_statement_reaches_the_database_callback() {
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    std::string seen;
    interpreter.set_sql_callback([&seen](std::string_view sql) {
        seen = std::string{sql};
        return std::string{"TABLE\n"};
    });
    interpreter.load_program("10 SQL SELECT * FROM scores\n");
    interpreter.run();
    TW_CHECK_EQ(seen, std::string{"SELECT * FROM scores"});
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{1});
    TW_CHECK_EQ(interpreter.output().front(), std::string{"TABLE"});

    RecordingHost plain_host;
    Interpreter plain{Language::Focal, plain_host};
    plain.load_program("01.10 SQL SELECT 1\n");
    plain.run();
    TW_CHECK_EQ(plain.output().size(), std::size_t{1});
    TW_CHECK(plain.output().front().find("no database is open") != std::string::npos);

    // Logo reaches the same callback through its own command loop; SQL is the
    // rest of the line, not a reporter split into Logo words.
    RecordingHost logo_host;
    Interpreter logo{Language::Logo, logo_host};
    std::string logo_seen;
    logo.set_sql_callback([&logo_seen](std::string_view sql) {
        logo_seen = std::string{sql};
        return std::string{"LOGO TABLE\n"};
    });
    logo.load_program("SQL SELECT * FROM scores\n");
    logo.run();
    TW_CHECK_EQ(logo_seen, std::string{"SELECT * FROM scores"});
    TW_CHECK_EQ(logo.output().size(), std::size_t{1});
    TW_CHECK_EQ(logo.output().front(), std::string{"LOGO TABLE"});

    // Smalltalk asks through its `SQL` class: `Transcript show: (SQL run: '...')`.
    RecordingHost st_host;
    Interpreter smalltalk{Language::Smalltalk, st_host};
    std::string st_seen;
    smalltalk.set_sql_callback([&st_seen](std::string_view sql) {
        st_seen = std::string{sql};
        return std::string{"ST TABLE"};
    });
    smalltalk.load_program("Transcript show: (SQL run: 'SELECT * FROM scores').\n");
    smalltalk.run();
    TW_CHECK_EQ(st_seen, std::string{"SELECT * FROM scores"});
    TW_CHECK_EQ(smalltalk.output().size(), std::size_t{1});
    TW_CHECK_EQ(smalltalk.output().front(), std::string{"ST TABLE"});
}

void test_data_breakpoint_pauses_on_a_variable_change() {
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.load_program("10 LET X = 1\n20 LET X = 2\n30 PRINT X\n");

    timewarp::lang::DebugSession session;
    session.set_data_breakpoints({"X"});
    std::mutex mutex;
    std::condition_variable changed;
    std::vector<int> pauses;
    std::string changed_name;
    session.on_data_changed = [&](int line, const std::string& name) {
        {
            const std::lock_guard<std::mutex> lock{mutex};
            pauses.push_back(line);
            changed_name = name;
        }
        changed.notify_all();
    };
    interpreter.set_debug_session(&session);

    std::thread worker{[&] { interpreter.run(); }};
    const auto wait_for = [&](std::size_t count) {
        std::unique_lock lock{mutex};
        return changed.wait_for(lock, std::chrono::seconds{5},
                                [&] { return pauses.size() >= count; });
    };

    TW_CHECK(wait_for(1));
    TW_CHECK_EQ(pauses.at(0), 10);
    TW_CHECK_EQ(changed_name, std::string{"X"});
    TW_CHECK_EQ(interpreter.number("X"), 1.0);
    TW_CHECK(interpreter.output().empty());

    session.request_continue();
    TW_CHECK(wait_for(2));
    TW_CHECK_EQ(pauses.at(1), 20);
    TW_CHECK_EQ(interpreter.number("X"), 2.0);

    session.request_continue();
    worker.join();
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{1});
    TW_CHECK_EQ(interpreter.output().front(), std::string{"2"});
    TW_CHECK(!session.paused());
}

void test_data_breakpoint_absent_when_not_watched() {
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.load_program("10 LET X = 1\n20 LET Y = 2\n");
    timewarp::lang::DebugSession session;
    session.set_data_breakpoints({"X"});
    std::mutex mutex;
    std::condition_variable changed;
    int pauses = 0;
    session.on_data_changed = [&](int, const std::string&) {
        {
            const std::lock_guard<std::mutex> lock{mutex};
            ++pauses;
        }
        changed.notify_all();
    };
    interpreter.set_debug_session(&session);
    std::thread worker{[&] { interpreter.run(); }};
    {
        std::unique_lock lock{mutex};
        (void)changed.wait_for(lock, std::chrono::seconds{5}, [&] { return pauses > 0; });
    }
    session.request_continue();
    worker.join();
    // X changed once, at line 10. Y's assignment is not watched and must not
    // produce a second pause.
    TW_CHECK_EQ(pauses, 1);
}


void test_logo_data_breakpoint_pauses_at_a_statement_boundary() {
    RecordingHost host;
    Interpreter interpreter{Language::Logo, host};
    interpreter.load_program("MAKE \"X 1\nMAKE \"X 2\nPRINT :X\n");

    timewarp::lang::DebugSession session;
    session.set_data_breakpoints({"X"});
    std::mutex mutex;
    std::condition_variable changed;
    std::vector<int> pauses;
    session.on_data_changed = [&](int line, const std::string& name) {
        {
            const std::lock_guard<std::mutex> lock{mutex};
            pauses.push_back(line);
        }
        changed.notify_all();
        (void)name;
    };
    interpreter.set_debug_session(&session);

    std::thread worker{[&] { interpreter.run(); }};
    const auto wait_for = [&](std::size_t count) {
        std::unique_lock lock{mutex};
        return changed.wait_for(lock, std::chrono::seconds{5},
                                [&] { return pauses.size() >= count; });
    };

    // The pause point is the boundary *after* the statement, so the first change
    // (X = 1) is reported before line 2 and the second (X = 2) before line 3.
    TW_CHECK(wait_for(1));
    TW_CHECK_EQ(pauses.at(0), 2);
    TW_CHECK_EQ(interpreter.number("X"), 1.0);
    session.request_continue();
    TW_CHECK(wait_for(2));
    TW_CHECK_EQ(pauses.at(1), 3);
    TW_CHECK_EQ(interpreter.number("X"), 2.0);
    session.request_continue();
    worker.join();
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{1});
    TW_CHECK_EQ(interpreter.output().front(), std::string{"2"});
}

int main() {
    test_chip8_program_starts_at_0x200();
    test_chip8_labels_resolve_forward_references();
    test_chip8_both_hex_spellings_agree();
    test_chip8_font_is_present_in_memory();
    test_chip8_db_and_dw_emit_data();
    test_chip8_assembler_errors();
    test_the_debug_session_pauses_and_resumes();
    test_stopping_a_paused_run_unwinds_it();
    test_chip8_sys_is_ignored();
    test_chip8_ld_and_add_with_a_byte();
    test_chip8_add_sets_the_carry_flag();
    test_chip8_subtract_sets_the_borrow_flag();
    test_chip8_add_byte_wraps_without_carry();
    test_chip8_index_arithmetic();
    test_chip8_shr_shifts_the_named_register_in_place();
    test_chip8_logic_and_shl();
    test_chip8_bcd_conversion_writes_three_digits();
    test_chip8_store_and_load_through_i();
    test_chip8_block_transfer_advances_i();
    test_chip8_skips();
    test_chip8_keypad_is_live();
    test_chip8_ld_key_waits_for_a_press();
    test_chip8_call_and_return();
    test_chip8_ret_without_call_is_reported();
    test_chip8_draw_lights_the_expected_pixels();
    test_chip8_drawing_twice_erases_and_reports_the_collision();
    test_chip8_pixels_wrap_around_the_screen();
    test_chip8_hires_mode_changes_the_grid_and_starts_blank();
    test_chip8_cls_in_hires_keeps_the_hi_res_grid();
    test_chip8_large_sprite_is_sixteen_by_sixteen_in_hires();
    test_chip8_large_sprite_doubles_columns_in_lores();
    test_chip8_scroll_down_moves_the_drawing_and_clears_the_top();
    test_chip8_scroll_left_and_right_move_four_pixels();
    test_chip8_exit_opcode_halts();
    test_chip8_fx30_points_at_the_large_font();
    test_chip8_large_font_is_in_memory();
    test_chip8_rpl_flags_round_trip();
    test_chip8_hires_display_halves_the_pixel_size();
    test_chip8_keys();
    test_chip8_jump_with_offset_uses_v0();
    test_chip8_timers();
    test_chip8_frame_limit_is_information();
    test_chip8_unknown_opcode_is_reported();
    test_chip8_binary_rom_loads_at_0x200();
    test_chip8_binary_rom_respects_memory();
    test_chip8_display_uses_the_palette();
    test_chip8_display_is_cleared_before_the_first_frame();
    test_focal_type_prints_text_and_expressions();
    test_focal_set_then_read_back();
    test_focal_format_directive_sets_decimal_places();
    test_focal_bang_suppresses_the_newline_within_a_statement();
    test_focal_if_branches_on_the_sign();
    test_focal_if_omitted_targets_fall_through();
    test_focal_for_runs_the_rest_of_the_line();
    test_focal_for_takes_a_step_and_a_limit();
    test_focal_for_runs_turtle_statements();
    test_focal_for_keeps_the_last_value_the_body_saw();
    test_focal_do_runs_a_group_and_returns();
    test_focal_do_nesting_is_bounded();
    test_focal_labels_compare_as_numbers();
    test_focal_erase_all_forgets_variables();
    test_focal_quit_stops_the_program();
    test_focal_turtle_movement_is_silent();
    test_focal_turtle_argument_may_be_a_variable();
    test_focal_turtle_without_a_distance_is_an_error();
    test_focal_functions();
    test_focal_errors_are_reported();
    test_focal_unassigned_single_letter_is_zero();
    test_focal_ask_reads_the_answer();
    test_focal_ask_accepts_a_comma_before_the_name();
    test_pilot_type_accepts_match();
    test_pilot_substring_and_wildcards_match();
    test_pilot_jumps_and_labels();
    test_pilot_subroutines_return_at_e();
    test_pilot_compute_and_branch();
    test_pilot_strings_and_computation();
    test_pilot_no_and_hint();
    test_pilot_graphics_and_text();
    test_pilot_reports_errors();
    test_comal_print_and_assignment();
    test_comal_if_blocks();
    test_comal_loops();
    test_comal_procedures();
    test_comal_arrays_and_data();
    test_comal_turtle();
    test_logo_definition_does_not_run_until_it_is_called();
    test_logo_repeat_counts_for_repcount();
    test_logo_recursion_counter_is_restored();
    test_logo_stop_ends_only_the_procedure_it_is_in();
    test_logo_if_falls_through_when_false();
    test_logo_print_drops_the_brackets_of_a_list_literal();
    test_logo_print_shows_a_reporter_value_not_its_source();
    test_logo_quoted_colour_names_reach_the_host();
    test_logo_bracketed_command_may_span_lines();
    test_logo_to_without_end_is_not_a_procedure();
    test_logo_clear_screen_resets_the_turtle();
    test_logo_text_size_reaches_the_text_call();
    test_logo_filled_draws_a_polygon_around_the_path_walked();
    test_logo_bezier_draws_a_curve_and_leaves_the_turtle_on_its_end();
    test_a_chained_bezier_starts_where_the_last_one_finished();
    test_logo_bezier_rejects_a_short_argument_list();
    test_logo_setpc_with_a_palette_index_sets_the_pen();
    test_basic_sprite_statements_capture_place_move_and_hide();
    test_basic_gradients_reach_the_host();
    test_logo_unknown_command_is_reported_with_the_statement_number();
    test_logo_heading_starts_north_and_turns_clockwise();
    test_turtle_pose_matches_the_reference_in_three_dimensions();
    test_python_random_matches_the_reference_sequence();
    test_apl_iota_and_reshape();
    test_apl_scalar_functions_are_pervasive();
    test_apl_reduce_and_scan();
    test_apl_assignment_is_silent();
    test_apl_defined_function_returns_a_value();
    test_apl_length_error_is_reported();
    test_apl_turtle_words_draw_and_stay_silent();
    test_apl_turtle_state_is_readable();
    test_rexx_say_concatenates_abutting_terms();
    test_rexx_arithmetic_and_comparison();
    test_rexx_do_loop_counts();
    test_rexx_if_block_skips_the_else();
    test_rexx_builtins();
    test_rexx_subroutine_returns_through_result();
    test_rexx_turtle_words_draw();
    test_smalltalk_class_and_instance_variable();
    test_smalltalk_block_is_a_closure();
    test_smalltalk_cascade_sends_to_the_original_receiver();
    test_smalltalk_inject_and_range();
    test_smalltalk_non_local_return_leaves_the_block();
    test_smalltalk_method_answers_its_last_expression();
    test_smalltalk_turtle_messages_draw();
    test_basic_dollar_string_functions_are_calls_not_array_reads();
    test_basic_string_array_index_is_an_expression();
    test_basic_unreadable_print_reports_the_evaluator_message();
    test_basic_logo_words_draw_through_the_fallback();
    test_basic_unknown_word_is_still_an_error();
    test_basic_say_refuses_without_a_backend();
    test_basic_loops_and_subroutines_run();
    test_basic_input_prompt_and_answer_round_trip();
    test_basic_numeric_input_is_coerced();
    test_raster_clips_instead_of_failing();
    test_raster_line_and_rect_plot_the_pixels_asked_for();
    test_raster_flood_fill_stops_at_a_border();
    test_raster_round_trips_a_block();
    test_basic_point_reads_back_the_pixel_it_drew();
    test_basic_point_without_a_screen_answers_minus_one();
    test_basic_screen_modes_choose_the_screen_size();
    test_basic_get_and_put_round_trip_a_sprite();
    test_basic_paint_fills_with_what_the_surface_was_given();
    test_basic_get_needs_a_graphics_screen();
    test_basic_cls_clears_what_point_reads();
    test_basic_stick_is_neutral_before_joyinit();
    test_gamepad_axis_scaling_is_centred();
    test_a_lowercase_array_name_is_the_same_array();
    test_file_access_is_confined_to_the_directory();
    test_basic_an_empty_loop_body_does_not_end_its_outer_loop();
    test_the_screen_game_example_runs_and_draws();
    test_a_runaway_program_stops_at_the_budget();
    test_profiling_counts_each_line();
    test_profiling_is_off_unless_asked();
    test_logo_stops_at_a_breakpoint_and_steps();
    test_smalltalk_stops_at_a_message_send();
    test_chip8_stops_before_an_instruction();
    test_repl_snippet_keeps_bindings();
    test_repl_new_clears_basic_bindings();
    test_basic_question_mark_is_print();
    test_watch_expressions_use_the_paused_bindings();
    test_sql_statement_reaches_the_database_callback();
    test_data_breakpoint_pauses_on_a_variable_change();
    test_data_breakpoint_absent_when_not_watched();
    test_logo_data_breakpoint_pauses_at_a_statement_boundary();
    return tw::test::summary("lang_tests");
}
