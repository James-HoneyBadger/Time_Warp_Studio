// SPDX-License-Identifier: MIT
//
// Drawing: lines, circles, points, fills, paths and the two gradients.

#include "basic_internal.hpp"
#include "basic_statements_internal.hpp"

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstddef>
#include <filesystem>
#include <optional>
#include <regex>
#include <string>
#include <string_view>
#include <thread>
#include <utility>
#include <vector>

#include "timewarp/audio/music.hpp"
#include "timewarp/lang/screen_mode.hpp"

namespace timewarp::lang::basic {

std::string Machine::draw_line(const std::string& args) {
    static const std::regex qbasic{
        R"(\(\s*([^,]+)\s*,\s*([^,]+)\s*\)\s*-\s*\(\s*([^,]+)\s*,\s*([^,]+)\s*\)(?:\s*,\s*([^,]+?))?(?:\s*,\s*(B|BF|b|bf))?\s*$)",
        std::regex::ECMAScript};
    std::smatch match;
    std::string x1_text;
    std::string y1_text;
    std::string x2_text;
    std::string y2_text;
    std::string colour_text;
    std::string mode;
    if (std::regex_match(args, match, qbasic)) {
        x1_text = trim(match[1].str());
        y1_text = trim(match[2].str());
        x2_text = trim(match[3].str());
        y2_text = trim(match[4].str());
        colour_text = match[5].matched ? trim(match[5].str()) : std::string{};
        mode = match[6].matched ? upper(match[6].str()) : std::string{};
    } else {
        const std::vector<std::string> parts = split_trimmed(args, ',');
        if (parts.size() < 4) {
            return "❌ LINE requires (x1,y1)-(x2,y2)\n";
        }
        x1_text = parts[0];
        y1_text = parts[1];
        x2_text = parts[2];
        y2_text = parts[3];
        colour_text = parts.size() > 4 ? parts[4] : std::string{};
        mode = parts.size() > 5 ? upper(parts[5]) : std::string{};
    }
    try {
        const double x1 = eval(x1_text);
        const double y1 = eval(y1_text);
        const double x2 = eval(x2_text);
        const double y2 = eval(y2_text);
        const Rgb colour = colour_text.empty() ? turtle_.pen_colour()
                                               : resolve_colour(colour_text,
                                                                turtle_.pen_colour());
        const auto line = [this, colour](double ax, double ay, double bx, double by) {
            turtle_.draw_polygon({Point{ax, ay}, Point{bx, by}}, colour, Rgb{}, pen_width_,
                                 false);
            state_.raster.line(pixel(ax), pixel(ay), pixel(bx), pixel(by), colour);
        };
        if (mode == "B") {
            line(x1, y1, x2, y1);
            line(x2, y1, x2, y2);
            line(x2, y2, x1, y2);
            line(x1, y2, x1, y1);
        } else if (mode == "BF") {
            turtle_.draw_rect(x1, y1, x2, y2, colour);
            state_.raster.rect(pixel(x1), pixel(y1), pixel(x2), pixel(y2), colour, true);
        } else {
            line(x1, y1, x2, y2);
        }
    } catch (const std::exception& error) {
        return "❌ LINE error: " + std::string{error.what()} + "\n";
    }
    return {};
}

std::string Machine::draw_circle(const std::string& args) {
    static const std::regex qbasic{
        R"(\(\s*([^,]+)\s*,\s*([^,]+)\s*\)\s*,\s*([^,)]+)(?:\s*,\s*([^,)]+))?(?:\s*,\s*([^,)]+))?(?:\s*,\s*([^,)]+))?)",
        std::regex::ECMAScript};
    std::smatch match;
    std::string x_text;
    std::string y_text;
    std::string radius_text;
    std::string colour_text;
    std::string start_text;
    std::string end_text;
    if (std::regex_search(args, match, qbasic)) {
        x_text = trim(match[1].str());
        y_text = trim(match[2].str());
        radius_text = trim(match[3].str());
        colour_text = match[4].matched ? trim(match[4].str()) : std::string{};
        start_text = match[5].matched ? trim(match[5].str()) : std::string{};
        end_text = match[6].matched ? trim(match[6].str()) : std::string{};
    } else {
        const std::vector<std::string> parts = split_trimmed(args, ',');
        if (parts.size() < 3) {
            return "❌ CIRCLE requires (x,y),radius\n";
        }
        x_text = parts[0];
        y_text = parts[1];
        radius_text = parts[2];
        colour_text = parts.size() > 3 ? parts[3] : std::string{};
        start_text = parts.size() > 4 ? parts[4] : std::string{};
        end_text = parts.size() > 5 ? parts[5] : std::string{};
    }
    try {
        const double centre_x = eval(x_text);
        const double centre_y = eval(y_text);
        const double radius = eval(radius_text);
        if (radius <= 0.0) {
            return "❌ CIRCLE radius must be positive\n";
        }
        const Rgb colour = colour_text.empty() ? turtle_.pen_colour()
                                               : resolve_colour(colour_text,
                                                                turtle_.pen_colour());
        double start_angle = 0.0;
        double end_angle = 360.0;
        if (!start_text.empty()) {
            start_angle = eval(start_text) * 180.0 / 3.14159265358979323846;
        }
        if (!end_text.empty()) {
            end_angle = eval(end_text) * 180.0 / 3.14159265358979323846;
        }
        double extent = end_angle - start_angle;
        if (extent == 0.0) {
            extent = 360.0;
        }
        const bool was_down = pen_down_;
        pen_up();
        move_to(centre_x + radius, centre_y);
        pen_down();
        const Rgb previous = turtle_.pen_colour();
        turtle_.set_color(colour);
        const auto segments = std::max(12, static_cast<int>(std::fabs(extent) / 10.0));
        const double step = extent / static_cast<double>(segments);
        for (int index = 0; index <= segments; ++index) {
            const double angle = start_angle + index * step;
            const double radians = angle * 3.14159265358979323846 / 180.0;
            move_to(centre_x + radius * std::cos(radians),
                    centre_y + radius * std::sin(radians));
        }
        if (extent >= 360.0) {
            move_to(centre_x + radius, centre_y);
        }
        state_.raster.arc(centre_x, centre_y, radius, start_angle, extent, colour);
        turtle_.set_color(previous);
        if (!was_down) {
            pen_up();
        }
    } catch (const std::exception& error) {
        return "❌ CIRCLE error: " + std::string{error.what()} + "\n";
    }
    return {};
}


std::string Machine::draw_point(const std::string& args, bool foreground) {
    static const std::regex pattern{
        R"(\(\s*([^,]+)\s*,\s*([^,]+)\s*\)(?:\s*,\s*(.+))?)", std::regex::ECMAScript};
    std::smatch match;
    if (!std::regex_search(args, match, pattern)) {
        return foreground ? "❌ PSET requires (x,y)\n" : "❌ PRESET requires (x,y)\n";
    }
    try {
        const double x = eval(trim(match[1].str()));
        const double y = eval(trim(match[2].str()));
        const Rgb fallback = foreground ? turtle_.pen_colour() : Rgb{10, 10, 20};
        const Rgb colour = match[3].matched ? resolve_colour(trim(match[3].str()), fallback)
                                            : fallback;
        turtle_.draw_point(x, y, colour);
        state_.raster.set(pixel(x), pixel(y), colour);
    } catch (const std::exception& error) {
        return (foreground ? "❌ PSET error: " : "❌ PRESET error: ") +
               std::string{error.what()} + "\n";
    }
    return {};
}


std::string Machine::paint(const std::string& args) {
    const std::vector<std::string> parts = split_trimmed(args, ',');
    if (parts.empty()) {
        return "❌ PAINT requires coordinates\n";
    }
    Rgb colour{255, 255, 255};
    if (parts.size() >= 2) {
        colour = resolve_colour(parts[1], colour);
    }
    turtle_.fill_last_shape(colour);

    // The raster is filled as well, in its own coordinates -- with the colour the
    // surface was just given, not with the one the statement names. The two can
    // differ: the reference resolves `PAINT (50, 30), 4` from a plain comma
    // split, which makes the fill white, and a screen a program can read back
    // that disagreed with the fill a learner can see would be the worse answer.
    // `split_top` is used for the point because a point carries a comma of its
    // own, not to re-read the colour. The flood happens whether or not the
    // surface had a "last shape" to fill: the reference's own answer is a stub,
    // and flooding the region that contains the point is what PAINT means.
    const std::vector<std::string> fields = split_top(args, ",");
    if (!fields.empty()) {
        const std::string point_text = trim(fields[0]);
        if (point_text.size() >= 2 && point_text.front() == '(' &&
            point_text.back() == ')') {
            const std::vector<std::string> point = split_trimmed(
                point_text.substr(1, point_text.size() - 2), ',');
            if (point.size() >= 2) {
                try {
                    state_.raster.flood_fill(pixel(eval(point[0])), pixel(eval(point[1])),
                                             colour);
                } catch (const std::exception&) {
                    // The host's own answer is the one a learner reads; a point
                    // the evaluator cannot read has already been reported by the
                    // caller, and a second message would not help.
                }
            }
        }
    }
    // The reference fills the most recent *closed* shape; a line drawn with LINE
    // is not one, and a program that paints before drawing anything gets told.
    return fillable_shapes_ > 0 ? std::string{} : std::string{"ℹ️ No shape to fill\n"};
}


std::string Machine::draw_path(const std::string& args) {
    std::string path = trim(args);
    if (path.size() >= 2 && path.front() == '"' && path.back() == '"') {
        path = path.substr(1, path.size() - 2);
    }

    // GW-BASIC's `DRAW` string: one command letter and one or two numbers, with
    // spaces and semicolons between. U/D are up and down the screen, which is
    // forward and back in turtle terms -- the turtle starts facing north.
    int executed = 0;
    std::size_t index = 0;
    while (index < path.size()) {
        const char character = path[index];
        if (character == ' ' || character == ';' || character == ',') {
            ++index;
            continue;
        }
        const char letter = static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
        ++index;
        auto read_value = [&path, &index]() -> std::optional<double> {
            const std::size_t start = index;
            while (index < path.size() &&
                   (std::isdigit(static_cast<unsigned char>(path[index])) ||
                    path[index] == '-' || path[index] == '+' || path[index] == '.')) {
                ++index;
            }
            if (start == index) {
                return std::nullopt;
            }
            return std::strtod(path.substr(start, index - start).c_str(), nullptr);
        };

        switch (letter) {
            case 'U':
            case 'F': {
                const std::optional<double> distance = read_value();
                step_forward(distance.value_or(1.0));
                ++executed;
                break;
            }
            case 'D':
            case 'G': {
                const std::optional<double> distance = read_value();
                step_forward(-distance.value_or(1.0));
                ++executed;
                break;
            }
            case 'R': {
                turn_right(read_value().value_or(0.0));
                ++executed;
                break;
            }
            case 'L': {
                turn_left(read_value().value_or(0.0));
                ++executed;
                break;
            }
            case 'E': {
                turn_right(read_value().value_or(0.0));
                ++executed;
                break;
            }
            case 'H': {
                turn_left(read_value().value_or(0.0));
                ++executed;
                break;
            }
            case 'B':
                pen_up();
                ++executed;
                break;
            case 'N':
                pen_down();
                ++executed;
                break;
            case 'M': {
                const std::optional<double> x = read_value();
                if (index < path.size() && path[index] == ',') {
                    ++index;
                }
                const std::optional<double> y = read_value();
                if (x.has_value() && y.has_value()) {
                    move_to(*x, *y);
                    ++executed;
                }
                break;
            }
            case 'C': {
                const std::optional<double> value = read_value();
                if (value.has_value()) {
                    turtle_.set_color(palette_colour(static_cast<int>(*value)));
                }
                break;
            }
            default:
                return "❌ DRAW: `" + std::string(1, character) +
                       "` is not a DRAW command (U D L R E H F G B N M C)\n";
        }
    }
    return "🐢 DRAW " + std::to_string(executed) + " steps\n";
}


std::string Machine::gradient(const std::string& args) {
    const std::vector<std::string> parts = split_trimmed(args, ',');
    if (parts.size() < 6) {
        return "❌ GRADIENT requires x1, y1, x2, y2, first, second [, angle]\n";
    }
    if (!state_.raster.active()) {
        return "❌ GRADIENT needs a graphics screen (SCREEN 1)\n";
    }
    try {
        const double x1 = eval(parts[0]);
        const double y1 = eval(parts[1]);
        const double x2 = eval(parts[2]);
        const double y2 = eval(parts[3]);
        const Rgb from = resolve_colour(parts[4], turtle_.pen_colour());
        const Rgb to = resolve_colour(parts[5], Rgb{0, 0, 0});
        const double angle = parts.size() > 6 ? eval(parts[6]) : 0.0;
        state_.raster.rect(static_cast<int>(std::min(x1, x2)),
                           static_cast<int>(std::min(y1, y2)),
                           static_cast<int>(std::max(x1, x2)),
                           static_cast<int>(std::max(y1, y2)), from, true);
        turtle_.draw_rect_gradient(x1, y1, x2, y2, angle, from, to);
    } catch (const std::exception& error) {
        return "❌ GRADIENT error: " + std::string{error.what()} + "\n";
    }
    return {};
}

std::string Machine::ellipse_gradient(const std::string& args) {
    const std::vector<std::string> parts = split_trimmed(args, ',');
    if (parts.size() < 6) {
        return "❌ ELLIPSEGRADIENT requires x1, y1, x2, y2, first, second\n";
    }
    if (!state_.raster.active()) {
        return "❌ ELLIPSEGRADIENT needs a graphics screen (SCREEN 1)\n";
    }
    try {
        const double x1 = eval(parts[0]);
        const double y1 = eval(parts[1]);
        const double x2 = eval(parts[2]);
        const double y2 = eval(parts[3]);
        const Rgb from = resolve_colour(parts[4], turtle_.pen_colour());
        const Rgb to = resolve_colour(parts[5], Rgb{0, 0, 0});
        turtle_.draw_ellipse_gradient(x1, y1, x2, y2, from, to);
    } catch (const std::exception& error) {
        return "❌ ELLIPSEGRADIENT error: " + std::string{error.what()} + "\n";
    }
    return {};
}

}  // namespace timewarp::lang::basic
