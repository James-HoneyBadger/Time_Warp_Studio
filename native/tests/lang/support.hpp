// SPDX-License-Identifier: MIT
//
// What the language suite's files share.
//
// `lang_tests.cpp` was 3,150 lines and every executor's cases lived in it. The
// cases are now one file per machine or language, in `lang/`; this header is the
// recording turtle host they all draw into, the CHIP-8 assembly helpers, and a
// `run_*` per language so a case reads as a program and an assertion.
//
// No Qt and no display: `RecordingHost` is the drawing surface, so a case can
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
#include "timewarp/lang/mersenne_random.hpp"
#include "timewarp/lang/raster.hpp"
#include "timewarp/lang/repl.hpp"
#include "timewarp/lang/turtle_host.hpp"
#include "timewarp/graphics/recording_host.hpp"
#include "timewarp/lang/watch.hpp"
// The suite is one file's worth of cases split across `lang/*.cpp`, one file per
// machine or language; this header is what they share: the recording turtle host,
// the assembling helper and a `run_*` per language.

namespace tw::lang_tests {

using timewarp::lang::Chip8Machine;
using timewarp::lang::Chip8Program;
using timewarp::lang::Interpreter;
using timewarp::lang::Language;
using timewarp::lang::MersenneRandom;
using timewarp::lang::Raster;
using timewarp::lang::ReplSession;
using timewarp::lang::Rgb;
using timewarp::lang::TurtleHost;

inline std::string number_text(double value) {
    char buffer[64];
    const std::size_t length =
        timewarp::core::format_double(buffer, buffer + sizeof buffer, value);
    return std::string{buffer, length};
}

inline std::string colour_text(Rgb colour) {
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
    std::map<int, Rgb> palette;
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
        if (found != palette.end()) {
            return found->second;
        }
        Rgb slot{};
        return reference_palette_colour(index, &slot) ? slot : pen_;
    }
};

inline bool contains(const std::vector<std::string>& output, std::string_view fragment) {
    return std::any_of(output.begin(), output.end(), [fragment](const std::string& line) {
        return line.find(fragment) != std::string::npos;
    });
}

inline bool any_error(const std::vector<std::string>& output) {
    return std::any_of(output.begin(), output.end(), [](const std::string& line) {
        return line.rfind("❌", 0) == 0;
    });
}

inline Chip8Program assemble(std::string_view source) {
    return timewarp::lang::assemble_chip8(source);
}

inline Chip8Machine run_to_halt(std::string_view source, unsigned keys = 0) {
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

inline std::size_t lit_pixels(const Chip8Machine& machine) {
    return static_cast<std::size_t>(
        std::count(machine.framebuffer().begin(), machine.framebuffer().end(), 1));
}

inline std::vector<std::string> run_chip8(std::string_view source) {
    RecordingHost host;
    Interpreter interpreter{Language::Chip8, host};
    interpreter.load_program(source);
    interpreter.run();
    return interpreter.output();
}

}  // namespace tw::lang_tests
