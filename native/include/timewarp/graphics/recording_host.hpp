// SPDX-License-Identifier: MIT
//
// The drawing surface a compiled Studio program carries with it.
//
// A generated executable is not the IDE: there is no window, no canvas and no
// Qt. What it still has is the turtle, and a program that draws must not exit
// silently with nothing to show -- that is the failure the project keeps
// guarding against. This host records the drawing and writes it as SVG, either
// to `--svg FILE` or to `<program>.svg` beside the executable when the program
// drew something and said nothing about where it should go.
//
// It implements both `lang::TurtleHost` (the line-by-line and whole-program
// languages) and `scheme::Host` (Scheme's own machine). One implementation, so
// Scheme's `(forward 10)` and Logo's `FD 10` produce the same line.

#pragma once

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <fstream>
#include <functional>
#include <map>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "timewarp/lang/turtle_host.hpp"
#include "timewarp/scheme/host.hpp"

namespace timewarp::graphics {

class RecordingHost final : public timewarp::lang::TurtleHost,
                            public timewarp::scheme::Host {
public:
    struct Segment {
        double x1 = 0.0;
        double y1 = 0.0;
        double x2 = 0.0;
        double y2 = 0.0;
        timewarp::lang::Rgb colour;
        double width = 2.0;
    };

    struct Sprite {
        int hotspot_x = 0;
        int hotspot_y = 0;
        int width = 0;
        int height = 0;
        std::vector<timewarp::lang::Rgb> pixels;
    };

    struct Shape {
        enum class Kind { Circle, Rect, Polygon, Point, Text } kind = Kind::Circle;
        double x = 0.0;
        double y = 0.0;
        double w = 0.0;
        double h = 0.0;
        std::vector<timewarp::lang::Point> points;
        std::string text;
        int font_size = 12;
        timewarp::lang::Rgb stroke;
        timewarp::lang::Rgb fill;
        double width = 2.0;
        bool filled = false;
    };

    // -- lang::TurtleHost -------------------------------------------------
    void forward(double distance) override { ++actions_; move(distance); }
    void back(double distance) override { ++actions_; move(-distance); }
    void right(double degrees) override { ++actions_; heading_ += degrees; }
    void left(double degrees) override { ++actions_; heading_ -= degrees; }
    void goto_xy(double x, double y) override { ++actions_; travel(x, y); }
    void set_heading(double degrees) override { ++actions_; heading_ = degrees; }
    void home() override { ++actions_; travel(0.0, 0.0); }
    void clear() override { ++actions_; resetDrawing(); }
    void reset() override {
        ++actions_;
        resetDrawing();
        x_ = 0.0;
        y_ = 0.0;
        heading_ = 0.0;
    }
    void enable_3d() override { ++actions_; }
    void disable_3d() override { ++actions_; }
    void pitch(double) override { ++actions_; }
    void roll(double) override { ++actions_; }
    void pen_up() override { ++actions_; pen_down_ = false; }
    void pen_down() override { ++actions_; pen_down_ = true; }
    void set_pen_width(double width) override { ++actions_; pen_width_ = width; }
    void set_color(timewarp::lang::Rgb colour) override { ++actions_; pen_ = colour; }
    void set_background(timewarp::lang::Rgb colour) override {
        ++actions_;
        background_ = colour;
    }

    void circle(double radius) override { circle(radius, 360.0); }
    void circle(double radius, double /*extent*/) override {
        ++actions_;
        Shape shape;
        shape.kind = Shape::Kind::Circle;
        shape.x = x_;
        shape.y = y_;
        shape.w = radius;
        shape.stroke = pen_;
        shape.width = pen_width_;
        shapes_.push_back(std::move(shape));
        ++drawing_calls_;
    }
    void draw_rect(double x0, double y0, double x1, double y1,
                   timewarp::lang::Rgb fill) override {
        ++actions_;
        Shape shape;
        shape.kind = Shape::Kind::Rect;
        shape.x = std::min(x0, x1);
        shape.y = std::min(y0, y1);
        shape.w = std::fabs(x1 - x0);
        shape.h = std::fabs(y1 - y0);
        shape.fill = fill;
        shape.filled = true;
        shapes_.push_back(std::move(shape));
        ++drawing_calls_;
    }
    void draw_text(double x, double y, const char* text, int font_size) override {
        ++actions_;
        Shape shape;
        shape.kind = Shape::Kind::Text;
        shape.x = x;
        shape.y = y;
        shape.text = text != nullptr ? text : "";
        shape.font_size = font_size;
        shape.fill = pen_;
        shapes_.push_back(std::move(shape));
        ++drawing_calls_;
    }
    void draw_point(double x, double y, timewarp::lang::Rgb colour) override {
        ++actions_;
        Shape shape;
        shape.kind = Shape::Kind::Point;
        shape.x = x;
        shape.y = y;
        shape.fill = colour;
        shape.filled = true;
        shapes_.push_back(std::move(shape));
        ++drawing_calls_;
    }
    /// A captured sprite, restored by `PUT`. One rect per pixel: the recorded
    /// drawing has no bitmap shape, and a sprite is small enough that the SVG
    /// stays readable.
    void put_pixels(int x, int y, int width, int height,
                    const std::vector<timewarp::lang::Rgb>& pixels) override {
        if (width <= 0 || height <= 0) {
            return;
        }
        ++actions_;
        const auto columns = static_cast<std::size_t>(width);
        const std::size_t expected = columns * static_cast<std::size_t>(height);
        for (std::size_t index = 0; index < pixels.size() && index < expected; ++index) {
            Shape shape;
            shape.kind = Shape::Kind::Rect;
            shape.x = x + static_cast<int>(index % columns);
            // The raster counts rows down from its top edge; every other shape
            // here counts up from the origin.
            shape.y = y - static_cast<int>(index / columns);
            shape.w = 1.0;
            shape.h = 1.0;
            shape.fill = pixels[index];
            shape.filled = true;
            shapes_.push_back(std::move(shape));
        }
        ++drawing_calls_;
    }
    void fill_last_shape(timewarp::lang::Rgb) override { ++actions_; }
    /// A style change draws nothing and still *is* something the program did: a
    /// gate that asked only about ink would call `PENSTYLE 1` silent and then be
    /// surprised when the next line came out dashed.
    void set_pen_style(int, double) override { ++actions_; }
    void cycle_palette(int, int, int) override { ++actions_; }

    /// A bezier is recorded as the polyline through its control points: the SVG
    /// this host writes has no curves, and a gate that cannot see the command at
    /// all would call a `BEZIER` program silent.
    void bezier(const std::vector<timewarp::lang::Point>& points,
                timewarp::lang::Rgb stroke, double width) override {
        ++actions_;
        Shape shape;
        shape.kind = Shape::Kind::Polygon;
        shape.points = points;
        shape.stroke = stroke;
        shape.width = width;
        shapes_.push_back(std::move(shape));
        ++drawing_calls_;
    }
    /// A gradient is a filled shape with two colours; the recording keeps the
    /// first, because a recorder that is asked "did this program draw" is not
    /// asked "how did it shade".
    void draw_rect_gradient(double x0, double y0, double x1, double y1, double,
                            timewarp::lang::Rgb from, timewarp::lang::Rgb) override {
        draw_rect(x0, y0, x1, y1, from);
    }
    void draw_ellipse_gradient(double x0, double y0, double x1, double y1,
                               timewarp::lang::Rgb from, timewarp::lang::Rgb) override {
        ++actions_;
        Shape shape;
        shape.kind = Shape::Kind::Circle;
        shape.x = std::min(x0, x1);
        shape.y = std::min(y0, y1);
        shape.w = std::fabs(x1 - x0);
        shape.h = std::fabs(y1 - y0);
        shape.fill = from;
        shape.filled = true;
        shapes_.push_back(std::move(shape));
        ++drawing_calls_;
    }
    void define_sprite(const char* name, int hotspot_x, int hotspot_y, int width,
                       int height,
                       const std::vector<timewarp::lang::Rgb>& pixels) override {
        ++actions_;
        Sprite sprite;
        sprite.hotspot_x = hotspot_x;
        sprite.hotspot_y = hotspot_y;
        sprite.width = width;
        sprite.height = height;
        sprite.pixels.assign(pixels.begin(), pixels.end());
        sprites_[name != nullptr ? name : ""] = std::move(sprite);
    }
    void place_sprite(const char* name, double x, double y) override {
        ++actions_;
        placed_sprite_ = name != nullptr ? name : "";
        placed_at_ = {x, y};
        sprite_visible_ = true;
        ++drawing_calls_;
    }
    void move_sprite(const char* name, double dx, double dy) override {
        ++actions_;
        placed_sprite_ = name != nullptr ? name : "";
        placed_at_ = {placed_at_.first + dx, placed_at_.second + dy};
        ++drawing_calls_;
    }
    void set_sprite_visible(const char*, bool visible) override {
        ++actions_;
        sprite_visible_ = visible;
    }
    /// What the last sprite call recorded: a gate can assert that a definition
    /// carried pixels and a placement carried a position, which is the difference
    /// between a sprite and an acknowledgement.
    [[nodiscard]] const std::string& placed_sprite() const noexcept { return placed_sprite_; }
    [[nodiscard]] std::pair<double, double> placed_at() const noexcept { return placed_at_; }
    [[nodiscard]] bool sprite_visible() const noexcept { return sprite_visible_; }
    /// The pixels of a defined sprite, empty for a name nobody defined.
    [[nodiscard]] std::size_t defined_sprite_pixels(std::string_view name) const {
        const auto found = sprites_.find(std::string{name});
        return found == sprites_.end() ? 0 : found->second.pixels.size();
    }
    void draw_polygon(const std::vector<timewarp::lang::Point>& points,
                      timewarp::lang::Rgb stroke, timewarp::lang::Rgb fill,
                      double width, bool filled) override {
        ++actions_;
        Shape shape;
        shape.kind = Shape::Kind::Polygon;
        shape.points = points;
        shape.stroke = stroke;
        shape.fill = fill;
        shape.width = width;
        shape.filled = filled;
        shapes_.push_back(std::move(shape));
        ++drawing_calls_;
    }
    [[nodiscard]] timewarp::lang::Rgb pen_colour() const override { return pen_; }
    void show_turtle() override { ++actions_; }
    void hide_turtle() override { ++actions_; }
    void set_svga_mode(int width, int height) override {
        ++actions_;
        screen_width_ = width;
        screen_height_ = height;
    }
    /// A program that named a screen asked for it to be shown at its own size.
    /// Recorded rather than drawn: the recording is what a gate reads, and a
    /// gate that cannot tell "asked for a 320x200 screen" from "asked for none"
    /// cannot check the one thing the screen-mode work was for.
    void set_framebuffer(int width, int height) override {
        ++actions_;
        screen_width_ = width;
        screen_height_ = height;
    }
    void clear_framebuffer() override {
        ++actions_;
        screen_width_ = 0;
        screen_height_ = 0;
    }
    void set_palette_color(int index, timewarp::lang::Rgb colour) override {
        ++actions_;
        palette_[index] = colour;
    }
    [[nodiscard]] timewarp::lang::Rgb resolve_color(int index) const override {
        const auto found = palette_.find(index);
        if (found != palette_.end()) {
            return found->second;
        }
        timewarp::lang::Rgb slot{};
        return timewarp::lang::reference_palette_colour(index, &slot)
                   ? slot
                   : timewarp::lang::Rgb{255, 255, 255};
    }

    // -- scheme::Host -----------------------------------------------------
    bool tick(std::uint64_t) override { return true; }
    void set_color(int red, int green, int blue) override {
        ++actions_;
        pen_ = timewarp::lang::Rgb{red, green, blue};
    }
    std::string sql(std::string_view statement) override {
        if (!sql_runner_) {
            return "\u274C SQL: no database is open.\n";
        }
        return sql_runner_(statement);
    }

    /// The database a program's `SQL` statement reaches. A compiled executable
    /// supplies one backed by SQLite; the IDE's API supplies its open connection.
    /// Unset means the statement explains that no database is open.
    using SqlRunner = std::function<std::string(std::string_view)>;
    void set_sql_runner(SqlRunner runner) { sql_runner_ = std::move(runner); }

    [[nodiscard]] bool drew() const noexcept { return drawing_calls_ > 0; }
    [[nodiscard]] std::size_t drawingCalls() const noexcept { return drawing_calls_; }
    [[nodiscard]] std::uint64_t drawing_calls() const override {
        return static_cast<std::uint64_t>(drawing_calls_);
    }
    /// Turtle operations seen, including turns and pen changes that leave no ink.
    [[nodiscard]] std::size_t actionCount() const noexcept { return actions_; }

    // -- what a lesson check reads ----------------------------------------
    //
    // A curriculum checkpoint counts pen segments and painted shapes, and asks
    // whether the turtle finished where it started. Those are the three facts
    // here rather than a general view of the recording: a lesson is judged by
    // what was drawn, not by how.
    [[nodiscard]] std::size_t segmentCount() const noexcept { return segments_.size(); }
    [[nodiscard]] std::size_t shapeCount() const noexcept { return shapes_.size(); }
    /// What was recorded, for a caller that needs it broken down by kind rather
    /// than counted. `time_warp_check` reports per-kind totals so the example
    /// and command harnesses can keep measuring ink without a bridge.
    [[nodiscard]] const std::vector<Segment>& segments() const noexcept { return segments_; }
    [[nodiscard]] const std::vector<Shape>& shapes() const noexcept { return shapes_; }
    /// The screen a program asked for, or 0x0 for a drawing.
    [[nodiscard]] int screenWidth() const noexcept { return screen_width_; }
    [[nodiscard]] int screenHeight() const noexcept { return screen_height_; }
    [[nodiscard]] double turtleX() const noexcept { return x_; }
    [[nodiscard]] double turtleY() const noexcept { return y_; }

    /// The drawing as an SVG document. Empty before the program drew anything.
    [[nodiscard]] std::string svg() const {
        std::ostringstream stream;
        write_svg(stream);
        return stream.str();
    }

    /// Writes an SVG of everything recorded. Returns false when the file cannot
    /// be opened; the caller prints that rather than pretending it saved.
    bool write_svg(const std::string& path) const {
        std::ofstream file{path};
        if (!file) {
            return false;
        }
        write_svg(file);
        return static_cast<bool>(file);
    }

    void write_svg(std::ostream& out) const {
        Bounds bounds;
        for (const Segment& segment : segments_) {
            bounds.add(segment.x1, segment.y1);
            bounds.add(segment.x2, segment.y2);
        }
        for (const Shape& shape : shapes_) {
            if (shape.kind == Shape::Kind::Polygon) {
                for (const auto& point : shape.points) {
                    bounds.add(point.x, point.y);
                }
            } else if (shape.kind == Shape::Kind::Circle) {
                bounds.add(shape.x - shape.w, shape.y - shape.w);
                bounds.add(shape.x + shape.w, shape.y + shape.w);
            } else if (shape.kind != Shape::Kind::Text) {
                bounds.add(shape.x, shape.y);
                bounds.add(shape.x + shape.w, shape.y + shape.h);
            } else {
                bounds.add(shape.x, shape.y);
            }
        }
        bounds.add(0.0, 0.0);
        const double margin = 12.0;
        const double width = std::max(1.0, bounds.max_x - bounds.min_x + 2 * margin);
        const double height = std::max(1.0, bounds.max_y - bounds.min_y + 2 * margin);
        // SVG y grows downwards; the turtle's grows upwards. Every emitted
        // coordinate is negated, which is also why text stays upright.
        const double view_x = bounds.min_x - margin;
        const double view_y = -(bounds.max_y + margin);

        std::ostream& file = out;
        file << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
        file << "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" << width
             << "\" height=\"" << height << "\" viewBox=\"" << view_x << ' ' << view_y
             << ' ' << width << ' ' << height << "\">\n";
        file << "<rect x=\"" << view_x << "\" y=\"" << view_y << "\" width=\"" << width
             << "\" height=\"" << height << "\" fill=\"" << rgb(background_) << "\"/>\n";
        for (const Segment& segment : segments_) {
            file << "<line x1=\"" << segment.x1 << "\" y1=\"" << -segment.y1 << "\" x2=\""
                 << segment.x2 << "\" y2=\"" << -segment.y2 << "\" stroke=\""
                 << rgb(segment.colour) << "\" stroke-width=\"" << segment.width
                 << "\" stroke-linecap=\"round\"/>\n";
        }
        for (const Shape& shape : shapes_) {
            switch (shape.kind) {
                case Shape::Kind::Circle:
                    file << "<circle cx=\"" << shape.x << "\" cy=\"" << -shape.y
                         << "\" r=\"" << std::fabs(shape.w) << "\" fill=\"none\" stroke=\""
                         << rgb(shape.stroke) << "\" stroke-width=\"" << shape.width
                         << "\"/>\n";
                    break;
                case Shape::Kind::Rect:
                    file << "<rect x=\"" << shape.x << "\" y=\"" << -(shape.y + shape.h)
                         << "\" width=\"" << shape.w << "\" height=\"" << shape.h
                         << "\" fill=\"" << rgb(shape.fill) << "\"/>\n";
                    break;
                case Shape::Kind::Point:
                    file << "<circle cx=\"" << shape.x << "\" cy=\"" << -shape.y
                         << "\" r=\"1.5\" fill=\"" << rgb(shape.fill) << "\"/>\n";
                    break;
                case Shape::Kind::Text:
                    file << "<text x=\"" << shape.x << "\" y=\"" << -shape.y
                         << "\" font-size=\"" << shape.font_size << "\" fill=\""
                         << rgb(shape.fill) << "\" font-family=\"sans-serif\">"
                         << escape(shape.text) << "</text>\n";
                    break;
                case Shape::Kind::Polygon: {
                    file << "<polygon points=\"";
                    for (std::size_t index = 0; index < shape.points.size(); ++index) {
                        if (index > 0) {
                            file << ' ';
                        }
                        file << shape.points[index].x << ',' << -shape.points[index].y;
                    }
                    file << "\" fill=\"" << (shape.filled ? rgb(shape.fill) : "none")
                         << "\" stroke=\"" << rgb(shape.stroke) << "\" stroke-width=\""
                         << shape.width << "\"/>\n";
                    break;
                }
            }
        }
        file << "</svg>\n";
    }

private:
    struct Bounds {
        double min_x = 0.0;
        double max_x = 0.0;
        double min_y = 0.0;
        double max_y = 0.0;
        bool any = false;

        void add(double x, double y) {
            if (!any) {
                min_x = max_x = x;
                min_y = max_y = y;
                any = true;
                return;
            }
            min_x = std::min(min_x, x);
            max_x = std::max(max_x, x);
            min_y = std::min(min_y, y);
            max_y = std::max(max_y, y);
        }
    };

    static std::string rgb(timewarp::lang::Rgb colour) {
        char buffer[32];
        std::snprintf(buffer, sizeof buffer, "rgb(%d,%d,%d)", colour.red, colour.green,
                      colour.blue);
        return buffer;
    }

    static std::string escape(std::string_view text) {
        std::string out;
        for (const char character : text) {
            switch (character) {
                case '&': out += "&amp;"; break;
                case '<': out += "&lt;"; break;
                case '>': out += "&gt;"; break;
                case '"': out += "&quot;"; break;
                default: out.push_back(character); break;
            }
        }
        return out;
    }

    void move(double distance) {
        constexpr double kPi = 3.14159265358979323846;
        const double radians = heading_ * kPi / 180.0;
        travel(x_ + distance * std::sin(radians), y_ + distance * std::cos(radians));
    }

    void travel(double new_x, double new_y) {
        if (pen_down_) {
            Segment segment;
            segment.x1 = x_;
            segment.y1 = y_;
            segment.x2 = new_x;
            segment.y2 = new_y;
            segment.colour = pen_;
            segment.width = pen_width_;
            segments_.push_back(segment);
            ++drawing_calls_;
        }
        x_ = new_x;
        y_ = new_y;
    }

    void resetDrawing() {
        segments_.clear();
        shapes_.clear();
        drawing_calls_ = 0;
    }

    std::vector<Segment> segments_;
    std::vector<Shape> shapes_;
    std::map<std::string, Sprite> sprites_;
    std::string placed_sprite_;
    std::pair<double, double> placed_at_{0.0, 0.0};
    bool sprite_visible_ = false;
    /// Slots a program has set. An unset one is answered by
    /// `reference_palette_colour`, so this host and the IDE agree about what
    /// `SETPC 1` means without either of them naming the sixteen colours.
    std::map<int, timewarp::lang::Rgb> palette_;
    timewarp::lang::Rgb pen_{255, 255, 255};
    timewarp::lang::Rgb background_{10, 10, 20};
    double pen_width_ = 2.0;
    double x_ = 0.0;
    double y_ = 0.0;
    double heading_ = 0.0;
    bool pen_down_ = true;
    std::size_t drawing_calls_ = 0;
    /// The screen a program asked for; 0x0 while it is drawing.
    int screen_width_ = 0;
    int screen_height_ = 0;
    std::size_t actions_ = 0;
    SqlRunner sql_runner_;
};

}  // namespace timewarp::graphics
