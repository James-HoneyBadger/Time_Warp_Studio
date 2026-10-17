// SPDX-License-Identifier: MIT
//
// A `TurtleHost` that writes each call as one line of text.
//
// This is the format `tools/lang_differential_check.py` compares against a
// recording host in the Python reference. It is a tool rather than part of the
// library because nothing in the product draws to stdout.

#pragma once

#include "timewarp/lang/turtle_host.hpp"

#include <charconv>
#include <cstdio>
#include <map>
#include <string>
#include <type_traits>
#include <utility>

namespace timewarp::lang {

class TraceHost final : public TurtleHost {
public:
    void forward(double distance) override { line("forward", distance); }
    void back(double distance) override { line("back", distance); }
    void right(double degrees) override { line("right", degrees); }
    void left(double degrees) override { line("left", degrees); }
    void goto_xy(double x, double y) override {
        trace("goto", x, y);
    }
    void set_heading(double degrees) override { line("heading", degrees); }
    void home() override { trace("home"); }
    void clear() override { trace("clear"); }
    void reset() override { trace("reset"); }
    void enable_3d() override { trace("3d", std::string{"on"}); }
    void disable_3d() override { trace("3d", std::string{"off"}); }
    void pitch(double degrees) override { line("pitch", degrees); }
    void roll(double degrees) override { line("roll", degrees); }
    void pen_up() override { trace("penup"); }
    void pen_down() override { trace("pendown"); }
    void set_pen_width(double width) override { line("width", width); }
    void set_color(Rgb colour) override {
        pen_ = colour;
        trace("colour", colour);
    }
    void set_background(Rgb colour) override { trace("background", colour); }
    void circle(double radius, double /*extent*/) override { line("circle", radius); }
    void fill_last_shape(Rgb fill) override { trace("fill", fill); }
    [[nodiscard]] Rgb pen_colour() const override { return pen_; }
    void draw_rect(double x0, double y0, double x1, double y1, Rgb fill) override {
        trace("rect", x0, y0, x1, y1, fill);
    }
    void draw_text(double x, double y, const char* text, int font_size = 12) override {
        trace("text", x, y, font_size, std::string{text});
    }
    void draw_point(double x, double y, Rgb colour) override {
        trace("point", x, y, colour);
    }
    void draw_polygon(const std::vector<Point>& points, Rgb stroke, Rgb fill,
                      double width, bool filled) override {
        std::printf("DRAW\tpolygon");
        for (const Point& point : points) {
            print_argument(point.x);
            print_argument(point.y);
        }
        print_argument(stroke);
        if (filled) {
            print_argument(fill);
        } else {
            std::printf(" -");
        }
        print_argument(width);
        std::printf("\n");
    }
    void show_turtle() override { trace("show"); }
    void hide_turtle() override { trace("hide"); }
    void set_svga_mode(int width, int height) override { trace("svga", width, height); }
    void set_palette_color(int index, Rgb colour) override {
        palette_[index] = colour;
        trace("palette", index, colour);
    }
    [[nodiscard]] Rgb resolve_color(int index) const override {
        const auto found = palette_.find(index);
        return found == palette_.end() ? Rgb{} : found->second;
    }

private:
    template <typename... Args>
    void trace(const char* name, const Args&... arguments) {
        std::printf("DRAW\t%s", name);
        (print_argument(arguments), ...);
        std::printf("\n");
    }

    template <typename T>
    void print_argument(const T& value) {
        using Raw = std::remove_cvref_t<T>;
        if constexpr (std::is_same_v<Raw, Rgb>) {
            std::printf(" #%02X%02X%02X", value.red, value.green, value.blue);
        } else if constexpr (std::is_same_v<Raw, std::string>) {
            std::printf(" %s", value.c_str());
        } else if constexpr (std::is_floating_point_v<Raw>) {
            char buffer[64];
            // A whole number is written as an integer, which is what the
            // Python side's `str(int(v)) if v.is_integer()` produces -- and it
            // is what makes `-0.0` come out as `0` on both sides.
            if (value == static_cast<double>(static_cast<long long>(value)) &&
                std::abs(value) < 1e15) {
                std::printf(" %lld", static_cast<long long>(value));
                return;
            }
            const auto result = std::to_chars(buffer, buffer + sizeof buffer,
                                              static_cast<double>(value));
            std::printf(" %.*s", static_cast<int>(result.ptr - buffer), buffer);
        } else {
            std::printf(" %lld", static_cast<long long>(value));
        }
    }

    void line(const char* name, double value) {
        char buffer[64];
        if (value == static_cast<double>(static_cast<long long>(value)) &&
            std::abs(value) < 1e15) {
            std::printf("DRAW\t%s %lld\n", name, static_cast<long long>(value));
            return;
        }
        const auto result = std::to_chars(buffer, buffer + sizeof buffer, value);
        std::printf("DRAW\t%s %.*s\n", name, static_cast<int>(result.ptr - buffer), buffer);
    }

    std::map<int, Rgb> palette_{{0, {255, 255, 255}}, {1, {16, 16, 20}}, {2, {140, 255, 140}}};
    Rgb pen_{255, 255, 255};
};

}  // namespace timewarp::lang
