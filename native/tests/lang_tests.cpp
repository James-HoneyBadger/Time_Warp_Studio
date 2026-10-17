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
#include <charconv>
#include <cstdint>
#include <cstdio>
#include <map>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "check.hpp"
#include "timewarp/lang/chip8.hpp"
#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/python_random.hpp"
#include "timewarp/lang/turtle_host.hpp"

namespace {

using timewarp::lang::Chip8Machine;
using timewarp::lang::Chip8Program;
using timewarp::lang::Interpreter;
using timewarp::lang::Language;
using timewarp::lang::PythonRandom;
using timewarp::lang::Rgb;
using timewarp::lang::TurtleHost;

std::string number_text(double value) {
    char buffer[64];
    const auto result = std::to_chars(buffer, buffer + sizeof buffer, value);
    return std::string{buffer, result.ptr};
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
    void show_turtle() override { ops.push_back("show"); }
    void hide_turtle() override { ops.push_back("hide"); }
    void set_svga_mode(int width, int height) override {
        ops.push_back("svga " + std::to_string(width) + " " + std::to_string(height));
    }
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

void test_chip8_assembler_errors() {
    TW_CHECK(contains(run_chip8("start: FROB V0"), "not an instruction form"));
    TW_CHECK(contains(run_chip8("start: LD V0, 999"), "not a byte"));
    TW_CHECK(contains(run_chip8("dup: CLS\ndup: CLS"), "defined twice"));
    TW_CHECK(contains(run_chip8("start: DRW V0, V1, 20"), "at most 15 rows"));
    TW_CHECK(contains(run_chip8("start: LD VG, 1"), "not a register"));
    TW_CHECK(contains(run_chip8("start: CLS\n       NOPE V0"), "line 2"));
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
        "       LD V0, 0\n       LD V1, 0\n       LD V2, [I]\n       HALT\n"
        "buf:   DB 0\n       DB 0");
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 2);
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
    const Chip8Machine machine =
        run_to_halt("start: LD V0, 30\n       LD DT, V0\n       LD V1, DT\n       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 30);
}

void test_chip8_frame_limit_is_information() {
    TW_CHECK(contains(run_chip8("start: JP start"), "600 frames"));
}

void test_chip8_unknown_opcode_names_its_address() {
    const std::vector<std::string> output = run_chip8("start: DW 0x0FFF");
    TW_CHECK(contains(output, "unknown opcode"));
    TW_CHECK(contains(output, "0x200"));
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
    TW_CHECK(contains(ran.output, "0."));
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

int main() {
    test_chip8_program_starts_at_0x200();
    test_chip8_labels_resolve_forward_references();
    test_chip8_both_hex_spellings_agree();
    test_chip8_font_is_present_in_memory();
    test_chip8_db_and_dw_emit_data();
    test_chip8_assembler_errors();
    test_chip8_ld_and_add_with_a_byte();
    test_chip8_add_sets_the_carry_flag();
    test_chip8_subtract_sets_the_borrow_flag();
    test_chip8_shr_shifts_the_named_register_in_place();
    test_chip8_bcd_conversion_writes_three_digits();
    test_chip8_store_and_load_through_i();
    test_chip8_skips();
    test_chip8_call_and_return();
    test_chip8_ret_without_call_is_reported();
    test_chip8_draw_lights_the_expected_pixels();
    test_chip8_drawing_twice_erases_and_reports_the_collision();
    test_chip8_pixels_wrap_around_the_screen();
    test_chip8_keys();
    test_chip8_jump_with_offset_uses_v0();
    test_chip8_timers();
    test_chip8_frame_limit_is_information();
    test_chip8_unknown_opcode_names_its_address();
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
    test_logo_unknown_command_is_reported_with_the_statement_number();
    test_logo_heading_starts_north_and_turns_clockwise();
    test_turtle_pose_matches_the_reference_in_three_dimensions();
    test_python_random_matches_the_reference_sequence();
    return tw::test::summary("lang_tests");
}
