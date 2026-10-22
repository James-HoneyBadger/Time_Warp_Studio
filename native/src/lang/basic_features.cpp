// SPDX-License-Identifier: MIT
//
// BASIC's screen, file, media and simulated-hardware statements, plus the
// entry point the interpreter's line loop calls.
//
// These are separated from `basic_statements.cpp` because none of them is about
// the language: they are the surface BASIC reaches outward through, and each
// one answers in the reference's own words (an emoji and a sentence) rather
// than failing.

#include "basic_internal.hpp"

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <regex>
#include <string>
#include <thread>
#include <utility>
#include <vector>

#include "timewarp/audio/music.hpp"
#include "timewarp/lang/screen_mode.hpp"

namespace timewarp::lang::basic {
namespace {

std::string join(const std::vector<std::string>& values, std::string_view separator) {
    std::string result;
    for (std::size_t index = 0; index < values.size(); ++index) {
        if (index != 0) {
            result += separator;
        }
        result += values[index];
    }
    return result;
}

bool starts_with(std::string_view text, std::string_view prefix) {
    return text.rfind(prefix, 0) == 0;
}

/// `int(text)` for a whole string, the form `SOURCE`'s arguments take.
std::optional<long> parse_integer(std::string_view text) {
    const std::string trimmed = trim(text);
    if (trimmed.empty()) {
        return std::nullopt;
    }
    const char* begin = trimmed.c_str();
    char* end = nullptr;
    const long value = std::strtol(begin, &end, 10);
    if (end == begin || *end != '\0') {
        return std::nullopt;
    }
    return value;
}

std::optional<long> evaluate_integer(Machine& machine, std::string_view text) {
    double value = 0.0;
    if (!parse_float(trim(text), value)) {
        return std::nullopt;
    }
    return static_cast<long>(machine.eval(trim(text)));
}

std::vector<std::string> split_trimmed(std::string_view text, char separator) {
    std::vector<std::string> parts;
    for (const std::string& field : split_plain(text, separator)) {
        parts.push_back(trim(field));
    }
    return parts;
}

/// A coordinate on the raster. The statements carry doubles; a pixel is a place.
int pixel(double value) {
    return static_cast<int>(std::lround(value));
}

}  // namespace

int pack_colour(Rgb colour) {
    return (std::clamp(colour.red, 0, 255) << 16) | (std::clamp(colour.green, 0, 255) << 8) |
           std::clamp(colour.blue, 0, 255);
}

Rgb unpack_colour(long value) {
    const long clamped = value < 0 ? 0 : (value > 0xFFFFFF ? 0xFFFFFF : value);
    return Rgb{static_cast<int>((clamped >> 16) & 0xFF),
               static_cast<int>((clamped >> 8) & 0xFF),
               static_cast<int>(clamped & 0xFF)};
}

// -- screen ----------------------------------------------------------------

std::string Machine::screen(const std::string& args) {
    const std::vector<std::string> parts = split_plain(args, ',');
    if (parts.empty()) {
        return "❌ SCREEN requires mode parameter\n";
    }
    double mode_value = 0.0;
    if (!parse_float(trim(parts[0]), mode_value)) {
        return "❌ Invalid SCREEN parameters: Expected numeric value for screen mode, got '" +
               trim(parts[0]) + "'\n";
    }
    const auto mode = static_cast<long>(mode_value);
    const ScreenMode* chosen = find_screen_mode(static_cast<int>(mode));
    if (chosen == nullptr) {
        return "❌ Value " + std::to_string(mode) + " for screen mode is not one of " +
               supported_screen_modes() + "\n";
    }
    if (chosen->colours == 0) {
        state_.screen_mode = 0;
        // Text mode takes the screen away, so a later POINT has an honest
        // answer -- the error that says there is no screen -- rather than the
        // pixels of a mode the program has left.
        state_.raster = Raster{};
        turtle_.clear_framebuffer();
        if (parts.size() >= 3) {
            const std::optional<long> columns = evaluate_integer(*this, parts[1]);
            const std::optional<long> rows = evaluate_integer(*this, parts[2]);
            if (columns.has_value() && (*columns < 40 || *columns > 120)) {
                return "❌ Value " + std::to_string(*columns) + " for columns is outside 40-120\n";
            }
            if (rows.has_value() && (*rows < 20 || *rows > 50)) {
                return "❌ Value " + std::to_string(*rows) + " for rows is outside 20-50\n";
            }
            if (columns.has_value()) {
                state_.cols = static_cast<int>(*columns);
            }
            if (rows.has_value()) {
                state_.rows = static_cast<int>(*rows);
            }
        }
        return "🎨 Text mode (" + std::to_string(state_.cols) + "x" +
               std::to_string(state_.rows) + ")\n";
    }
    state_.screen_mode = chosen->number;
    state_.screen_width = chosen->width;
    state_.screen_height = chosen->height;
    // Only mode 1 takes a size: it is the reference's graphics mode, whose
    // 800x600 is a default rather than a standard. The numbered modes are the
    // sizes their numbers meant, so an extra argument is ignored there rather
    // than quietly making `SCREEN 13` something no machine called 13.
    if (chosen->number == 1 && parts.size() >= 3) {
        const std::optional<long> width = evaluate_integer(*this, parts[1]);
        const std::optional<long> height = evaluate_integer(*this, parts[2]);
        if (width.has_value() && (*width < 320 || *width > 1920)) {
            return "❌ Value " + std::to_string(*width) + " for width is outside 320-1920\n";
        }
        if (height.has_value() && (*height < 200 || *height > 1200)) {
            return "❌ Value " + std::to_string(*height) + " for height is outside 200-1200\n";
        }
        if (width.has_value()) {
            state_.screen_width = static_cast<int>(*width);
        }
        if (height.has_value()) {
            state_.screen_height = static_cast<int>(*height);
        }
    }
    // The screen the program can read back. The canvas is told as well, because
    // a program that asked for a screen wants it *shown* at the size it asked
    // for: this is what switches the drawing surface from fit-to-content to a
    // fixed-coordinate framebuffer.
    state_.raster.resize(state_.screen_width, state_.screen_height, palette_colour(0));
    turtle_.set_framebuffer(state_.screen_width, state_.screen_height);
    return "🎨 Graphics mode (" + std::to_string(state_.screen_width) + "x" +
           std::to_string(state_.screen_height) + ")\n";
}

std::string Machine::locate(const std::string& args) {
    const std::vector<std::string> parts = split_plain(args, ',');
    if (parts.size() < 2) {
        return "❌ LOCATE requires row, col\n";
    }
    try {
        const auto row = static_cast<long>(eval(trim(parts[0])));
        const auto column = static_cast<long>(eval(trim(parts[1])));
        if (row < 1 || row > 25) {
            return "❌ Value " + std::to_string(row) + " for row is outside 1-25\n";
        }
        if (column < 1 || column > 80) {
            return "❌ Value " + std::to_string(column) + " for column is outside 1-80\n";
        }
        // Until 20.1 this validated the range and returned: the statement said it
        // had positioned the cursor and had not. With a machine attached it moves
        // the cursor register, and the next PRINT lands there.
        if (interpreter_.machine_attached()) {
            interpreter_.machine_locate(static_cast<int>(row) - 1,
                                        static_cast<int>(column) - 1);
        }
    } catch (const std::exception& error) {
        return "❌ LOCATE error: " + std::string{error.what()} + "\n";
    }
    return {};
}

namespace {

/// A bare palette number in a `COLOR` argument. Absent for a colour name or a hex
/// value, which are the turtle's business rather than the tube's.
[[nodiscard]] std::optional<int> palette_index(const std::vector<std::string>& parts,
                                               std::size_t index) {
    if (index >= parts.size()) {
        return std::nullopt;
    }
    const std::string text = trim(parts[index]);
    if (text.empty()) {
        return std::nullopt;
    }
    for (const char character : text) {
        if (!std::isdigit(static_cast<unsigned char>(character))) {
            return std::nullopt;
        }
    }
    const int value = std::stoi(text);
    if (value < 0 || value > 255) {
        return std::nullopt;
    }
    return value;
}

}  // namespace

std::string Machine::color(const std::string& args) {
    if (trim(args).empty()) {
        return {};
    }
    const std::vector<std::string> parts = split_plain(args, ',');
    if (!parts.empty() && !trim(parts[0]).empty()) {
        const Rgb colour = resolve_colour(trim(parts[0]), turtle_.pen_colour());
        turtle_.set_color(colour);
    }
    // A machine's `COLOR` is two *palette numbers*, because a screen of the period
    // stored a nibble per cell and not a colour. The turtle colour above is the
    // drawing surface; these are the tube.
    if (interpreter_.machine_attached()) {
        if (const std::optional<int> ink = palette_index(parts, 0); ink.has_value()) {
            interpreter_.machine_ink(*ink);
        }
        if (const std::optional<int> paper = palette_index(parts, 1); paper.has_value()) {
            interpreter_.machine_paper(*paper);
        }
    }
    if (parts.size() >= 2 && !trim(parts[1]).empty()) {
        const Rgb background = resolve_colour(trim(parts[1]), Rgb{10, 10, 20});
        turtle_.set_background(background);
        return "🎨 Colors set\n";
    }
    return "🎨 Pen color set\n";
}

std::string Machine::palette(const std::string& args) {
    const std::string raw = trim(args);
    if (starts_with(upper(raw), "CYCLE")) {
        const std::string rest = trim(raw.substr(5));
        const auto cycle = [this](int from, int to, int step) {
            const int low = std::max(0, std::min(255, from));
            const int high = std::max(low, std::min(255, to));
            std::vector<int> indices;
            for (int index = low; index <= high; ++index) {
                indices.push_back(index);
            }
            if (indices.size() <= 1) {
                return;
            }
            std::vector<Rgb> colours;
            colours.reserve(indices.size());
            for (const int index : indices) {
                colours.push_back(palette_colour(index));
            }
            const auto size = static_cast<int>(colours.size());
            int shift = step % size;
            if (shift < 0) {
                shift += size;
            }
            std::vector<Rgb> rotated;
            rotated.reserve(colours.size());
            for (int index = 0; index < size; ++index) {
                rotated.push_back(colours[static_cast<std::size_t>((index - shift + size) % size)]);
            }
            for (std::size_t index = 0; index < indices.size(); ++index) {
                state_.palette[indices[index]] = rotated[index];
            }
        };
        if (rest.empty()) {
            cycle(1, 15, 1);
            // The engine's own raster palette and the canvas's display list are
            // two views of one palette; a cycle that moved only the first left the
            // picture unchanged, which looks exactly like a statement that does
            // nothing.
            turtle_.cycle_palette(1, 15, 1);
            return "🎨 Palette cycled (1..15)\n";
        }
        std::vector<std::string> fields;
        for (const std::string& field : split_trimmed(rest, ',')) {
            if (!field.empty()) {
                fields.push_back(field);
            }
        }
        try {
            const int from = fields.size() >= 1 ? static_cast<int>(eval(fields[0])) : 1;
            const int to = fields.size() >= 2 ? static_cast<int>(eval(fields[1])) : 15;
            const int step = fields.size() >= 3 ? static_cast<int>(eval(fields[2])) : 1;
            cycle(from, to, step);
            turtle_.cycle_palette(from, to, step);
            return "🎨 Palette cycled (" + std::to_string(from) + ".." + std::to_string(to) +
                   " step " + std::to_string(step) + ")\n";
        } catch (const std::exception& error) {
            return "❌ PALETTE CYCLE error: " + std::string{error.what()} + "\n";
        }
    }
    std::vector<std::string> fields;
    for (const std::string& field : split_trimmed(raw, ',')) {
        if (!field.empty()) {
            fields.push_back(field);
        }
    }
    if (fields.size() < 2) {
        return "❌ PALETTE requires index, r, g, b (or index, hex_rgb)\n";
    }
    try {
        const int index = static_cast<int>(eval(fields[0]));
        int red = 0;
        int green = 0;
        int blue = 0;
        if (fields.size() >= 4) {
            red = static_cast<int>(eval(fields[1]));
            green = static_cast<int>(eval(fields[2]));
            blue = static_cast<int>(eval(fields[3]));
        } else {
            const Rgb colour = resolve_colour(fields[1], Rgb{255, 255, 255});
            red = colour.red;
            green = colour.green;
            blue = colour.blue;
        }
        state_.palette[index] = Rgb{red, green, blue};
        turtle_.set_palette_color(index, Rgb{red, green, blue});
        return "🎨 Palette[" + std::to_string(index) + "] = (" + std::to_string(red) + ", " +
               std::to_string(green) + ", " + std::to_string(blue) + ")\n";
    } catch (const std::exception& error) {
        return "❌ PALETTE error: " + std::string{error.what()} + "\n";
    }
}

std::string Machine::width(const std::string& args) {
    const std::optional<long> columns = parse_integer(args);
    if (!columns.has_value()) {
        return "❌ WIDTH requires numeric value\n";
    }
    return "📐 Set width to " + std::to_string(*columns) + " columns\n";
}

// -- drawing ---------------------------------------------------------------

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

std::string Machine::sprite_get(const std::string& args) {
    static const std::regex pattern{
        R"(\(\s*([^,]+)\s*,\s*([^,]+)\s*\)\s*-\s*\(\s*([^,]+)\s*,\s*([^,]+)\s*\)\s*,\s*([A-Za-z_][A-Za-z0-9_]*\$?)\s*$)",
        std::regex::ECMAScript};
    std::smatch match;
    if (!std::regex_match(args, match, pattern)) {
        return "❌ GET requires (x1,y1)-(x2,y2), array\n";
    }
    if (!state_.raster.active()) {
        return "❌ GET needs a graphics screen (SCREEN 1)\n";
    }
    const std::string name = upper(trim(match[5].str()));
    if (!name.empty() && name.back() == '$') {
        return "❌ GET requires a numeric array, not " + name + "\n";
    }
    try {
        const double x1 = eval(trim(match[1].str()));
        const double y1 = eval(trim(match[2].str()));
        const double x2 = eval(trim(match[3].str()));
        const double y2 = eval(trim(match[4].str()));
        const int left = pixel(std::min(x1, x2));
        const int right = pixel(std::max(x1, x2));
        const int top = pixel(std::min(y1, y2));
        const int bottom = pixel(std::max(y1, y2));
        const int width = right - left + 1;
        const int height = bottom - top + 1;
        const std::vector<Rgb> captured = state_.raster.block(left, top, width, height);
        // The array is sized here rather than demanded of DIM: a sprite has a
        // size only once the second corner is known, and making a learner
        // compute `2 + w*h` before capturing would be a trap, not a lesson.
        std::vector<double>& array =
            interpreter_.arrays[name];
        array.assign(captured.size() + 2, 0.0);
        array[0] = static_cast<double>(width);
        array[1] = static_cast<double>(height);
        for (std::size_t index = 0; index < captured.size(); ++index) {
            array[index + 2] = static_cast<double>(pack_colour(captured[index]));
        }
    } catch (const std::exception& error) {
        return "❌ GET error: " + std::string{error.what()} + "\n";
    }
    return {};
}

std::string Machine::sprite_put(const std::string& args) {
    static const std::regex pattern{
        R"(\(\s*([^,]+)\s*,\s*([^,]+)\s*\)\s*,\s*([A-Za-z_][A-Za-z0-9_]*\$?)\s*$)",
        std::regex::ECMAScript};
    std::smatch match;
    if (!std::regex_match(args, match, pattern)) {
        return "❌ PUT requires (x,y), array\n";
    }
    if (!state_.raster.active()) {
        return "❌ PUT needs a graphics screen (SCREEN 1)\n";
    }
    const std::string name = upper(trim(match[3].str()));
    const auto found = interpreter_.arrays.find(name);
    if (found == interpreter_.arrays.end() || found->second.size() < 2) {
        return "❌ No sprite captured in " + name +
               " (capture one with GET first)\n";
    }
    const std::vector<double>& array = found->second;
    const int width = static_cast<int>(array[0]);
    const int height = static_cast<int>(array[1]);
    if (width <= 0 || height <= 0) {
        return "❌ Sprite array " + name + " is empty\n";
    }
    if (array.size() < static_cast<std::size_t>(width) * static_cast<std::size_t>(height) + 2) {
        return "❌ Sprite array " + name + " is too small for " +
               std::to_string(width) + "x" + std::to_string(height) + "\n";
    }
    try {
        const int x = pixel(eval(trim(match[1].str())));
        const int y = pixel(eval(trim(match[2].str())));
        std::vector<Rgb> pixels;
        pixels.reserve(static_cast<std::size_t>(width) * static_cast<std::size_t>(height));
        for (std::size_t index = 2;
             index < static_cast<std::size_t>(width) * static_cast<std::size_t>(height) + 2;
             ++index) {
            pixels.push_back(unpack_colour(static_cast<long>(array[index])));
        }
        state_.raster.put_block(x, y, width, height, pixels);
        // The surface draws it too, so a sprite is visible and not merely
        // remembered -- the feature exists to be seen.
        turtle_.put_pixels(x, y, width, height, pixels);
    } catch (const std::exception& error) {
        return "❌ PUT error: " + std::string{error.what()} + "\n";
    }
    return {};
}

// -- files -----------------------------------------------------------------

std::string resolve_confined_path(std::string_view filename, std::string_view base_dir) {
    // The base is the run's own directory -- a project's root, or the folder the
    // program was opened from -- and only falls back to the process's working
    // directory when no run has named one.
    std::filesystem::path base = base_dir.empty()
                                     ? std::filesystem::current_path()
                                     : std::filesystem::path{std::string{base_dir}};
    std::error_code error;
    const std::filesystem::path resolved_base = std::filesystem::weakly_canonical(base, error);
    if (!error) {
        base = resolved_base;
    }
    const std::string raw = strip_quotes(trim(filename));
    std::filesystem::path candidate = base / raw;
    std::error_code canonical_error;
    const std::filesystem::path canonical =
        std::filesystem::weakly_canonical(candidate, canonical_error);
    if (!canonical_error) {
        candidate = canonical;
    }
    // A prefix test is not a containment test. `/tmp/run` is a prefix of
    // `/tmp/run2`, so `OPEN "../run2/notes.txt"` from `/tmp/run` passed the old
    // check and wrote outside the directory the program was confined to. Compare
    // whole components: the candidate is inside when it *is* the base, or when
    // what follows the base is a separator.
    const std::string base_text = base.generic_string();
    const std::string candidate_text = candidate.generic_string();
    const bool at_base = candidate_text == base_text;
    const bool below_base =
        candidate_text.size() > base_text.size() &&
        candidate_text.compare(0, base_text.size(), base_text) == 0 &&
        (base_text.back() == '/' || candidate_text[base_text.size()] == '/');
    if (!at_base && !below_base) {
        throw BasicError("Path '" + raw + "' escapes sandbox directory");
    }
    return candidate_text;
}

std::string Machine::open_file(const std::string& args) {
    static const std::regex long_form{
        R"(^["']?([^"']+?)["']?\s+FOR\s+(INPUT|OUTPUT|APPEND|RANDOM)\s+AS\s+#?(\d+)\s*$)",
        std::regex::ECMAScript | std::regex::icase};
    static const std::regex short_form{R"(^["']?([IinOoAaRr])["']?\s*,\s*#?(\d+)\s*,\s*["']?([^"']+)["']?\s*$)",
                                       std::regex::ECMAScript};
    const std::string raw = trim(args);
    std::string filename;
    std::string mode;
    long channel = 0;
    std::smatch match;
    if (std::regex_match(raw, match, long_form)) {
        filename = trim(match[1].str());
        mode = upper(match[2].str());
        channel = std::stol(match[3].str());
    } else if (std::regex_match(raw, match, short_form)) {
        const std::string letter = upper(match[1].str());
        channel = std::stol(match[2].str());
        filename = trim(match[3].str());
        mode = letter == "I" ? "INPUT" : (letter == "A" ? "APPEND" : "OUTPUT");
    } else {
        return "❌ OPEN syntax error: expected OPEN \"file\" FOR INPUT|OUTPUT|APPEND AS #n\n";
    }

    const char* file_mode = "r";
    if (mode == "OUTPUT") {
        file_mode = "w";
    } else if (mode == "APPEND") {
        file_mode = "a";
    } else if (mode == "RANDOM") {
        file_mode = "r+";
    }
    try {
        const std::string path = resolve_confined_path(filename, interpreter_.file_base());
        if (mode == "RANDOM" && !std::filesystem::exists(path)) {
            std::FILE* created = std::fopen(path.c_str(), "w");
            if (created != nullptr) {
                std::fclose(created);
            }
        }
        std::FILE* handle = std::fopen(path.c_str(), file_mode);
        if (handle == nullptr) {
            return "❌ OPEN error: cannot open '" + filename + "'\n";
        }
        auto entry = std::make_shared<FileHandle>();
        entry->handle = handle;
        state_.files[static_cast<int>(channel)] = std::move(entry);
        return "📂 Opened file '" + filename + "' (" + mode + ") as #" +
               std::to_string(channel) + "\n";
    } catch (const std::exception& error) {
        return "❌ OPEN error: " + std::string{error.what()} + "\n";
    }
}

std::string Machine::close_files(const std::string& args) {
    const std::string raw = trim(args);
    if (raw.empty()) {
        state_.files.clear();
        return "📂 Closed all files\n";
    }
    std::vector<std::string> closed;
    for (const std::string& field : split_plain(raw, ',')) {
        std::string token = trim(field);
        if (!token.empty() && token.front() == '#') {
            token = trim(token.substr(1));
        }
        if (token.empty()) {
            continue;
        }
        const std::optional<long> channel = parse_integer(token);
        if (!channel.has_value()) {
            return "❌ CLOSE invalid channel: " + trim(field) + "\n";
        }
        state_.files.erase(static_cast<int>(*channel));
        closed.push_back("#" + std::to_string(*channel));
    }
    return "📂 Closed file " + join(closed, ", ") + "\n";
}

std::string Machine::print_to_file(const std::string& args) {
    static const std::regex pattern{R"(^#?(\d+)\s*,\s*(.*)$)", std::regex::ECMAScript};
    std::smatch match;
    const std::string text = trim(args);
    if (!std::regex_match(text, match, pattern)) {
        return "❌ PRINT # requires #channel, expression\n";
    }
    const auto channel = static_cast<int>(std::stol(match[1].str()));
    const auto found = state_.files.find(channel);
    if (found == state_.files.end() || found->second->handle == nullptr) {
        return "❌ File #" + std::to_string(channel) + " is not open\n";
    }
    const std::string rendered = print(match[2].str());
    if (starts_with(rendered, "❌")) {
        return rendered;
    }
    std::fwrite(rendered.data(), 1, rendered.size(), found->second->handle);
    std::fflush(found->second->handle);
    return {};
}

std::string Machine::input_file(const std::string& args) {
    static const std::regex pattern{R"(^#?(\d+)\s*,\s*(.+)$)", std::regex::ECMAScript};
    std::smatch match;
    const std::string text = trim(args);
    if (!std::regex_match(text, match, pattern)) {
        return "❌ INPUT # requires #channel, variable\n";
    }
    const auto channel = static_cast<int>(std::stol(match[1].str()));
    const auto found = state_.files.find(channel);
    if (found == state_.files.end() || found->second->handle == nullptr) {
        return "❌ File #" + std::to_string(channel) + " is not open\n";
    }
    for (const std::string& raw : split_plain(match[2].str(), ',')) {
        const std::string name = upper(trim(raw));
        if (name.empty()) {
            continue;
        }
        std::string line;
        char buffer[4096];
        bool read_any = false;
        while (std::fgets(buffer, sizeof buffer, found->second->handle) != nullptr) {
            line += buffer;
            read_any = true;
            if (!line.empty() && line.back() == '\n') {
                break;
            }
        }
        if (!read_any) {
            return "❌ Input past end of file on #" + std::to_string(channel) + "\n";
        }
        while (!line.empty() && (line.back() == '\n' || line.back() == '\r')) {
            line.pop_back();
        }
        assign(name, line);
    }
    return {};
}

std::string Machine::line_input(const std::string& args) {
    const std::string stripped = trim(args);
    if (!stripped.empty() && stripped.front() == '#') {
        return input_file(stripped);
    }
    return input(stripped);
}

std::string Machine::get_record(const std::string& args) {
    std::vector<std::string> parts;
    for (const std::string& field : split_trimmed(args, ',')) {
        if (!field.empty()) {
            parts.push_back(field);
        }
    }
    if (parts.empty()) {
        return "❌ GET requires #channel\n";
    }
    std::string channel_text = parts[0];
    if (!channel_text.empty() && channel_text.front() == '#') {
        channel_text = channel_text.substr(1);
    }
    const std::optional<long> channel = parse_integer(channel_text);
    if (!channel.has_value()) {
        return "❌ GET requires numeric #channel\n";
    }
    const auto found = state_.files.find(static_cast<int>(*channel));
    if (found == state_.files.end() || found->second->handle == nullptr) {
        return "❌ File #" + std::to_string(*channel) + " is not open\n";
    }
    std::string line;
    char buffer[4096];
    if (std::fgets(buffer, sizeof buffer, found->second->handle) != nullptr) {
        line = buffer;
    }
    while (!line.empty() && (line.back() == '\n' || line.back() == '\r')) {
        line.pop_back();
    }
    if (parts.size() >= 3) {
        assign(upper(parts[2]), line);
    } else {
        interpreter_.set_text("RECORD$", line);
    }
    return "📖 Read record from #" + std::to_string(*channel) + "\n";
}

std::string Machine::put_record(const std::string& args) {
    std::vector<std::string> parts;
    for (const std::string& field : split_trimmed(args, ',')) {
        if (!field.empty()) {
            parts.push_back(field);
        }
    }
    if (parts.empty()) {
        return "❌ PUT requires #channel\n";
    }
    std::string channel_text = parts[0];
    if (!channel_text.empty() && channel_text.front() == '#') {
        channel_text = channel_text.substr(1);
    }
    const std::optional<long> channel = parse_integer(channel_text);
    if (!channel.has_value()) {
        return "❌ PUT requires numeric #channel\n";
    }
    const auto found = state_.files.find(static_cast<int>(*channel));
    if (found == state_.files.end() || found->second->handle == nullptr) {
        return "❌ File #" + std::to_string(*channel) + " is not open\n";
    }
    std::string payload;
    if (parts.size() >= 3) {
        payload = print(parts[2]);
        while (!payload.empty() && payload.back() == '\n') {
            payload.pop_back();
        }
    } else {
        payload = text_of("RECORD$");
    }
    payload += "\n";
    std::fwrite(payload.data(), 1, payload.size(), found->second->handle);
    std::fflush(found->second->handle);
    return "📝 Wrote record to #" + std::to_string(*channel) + "\n";
}

// -- media and hardware ----------------------------------------------------

std::string Machine::sound(const std::string& args) {
    if (upper(trim(args)) == "OFF") {
        turtle_.stop_sound();
        return "🔇 SOUND OFF\n";
    }
    const std::vector<std::string> parts = split_trimmed(args, ',');
    if (parts.size() < 2) {
        return "❌ SOUND requires frequency, duration\n";
    }
    try {
        const auto frequency = static_cast<long>(eval(parts[0]));
        const auto duration = static_cast<long>(eval(parts[1]));
        const auto milliseconds = static_cast<long>(duration * timewarp::audio::kTickMilliseconds);
        // The text is the reference's, so the parity gate still compares equal.
        // The tone is the native build's, and a host without audio drops it.
        turtle_.tone(static_cast<double>(frequency), static_cast<int>(milliseconds),
                     "square");
        return "🔊 SOUND " + std::to_string(frequency) + "Hz for " +
               std::to_string(milliseconds) + "ms\n";
    } catch (const std::exception& error) {
        return "❌ SOUND error: " + std::string{error.what()} + "\n";
    }
}

std::string Machine::key_text() {
    // `INKEY$` is one key: the rest of what was typed stays queued for the next
    // call, which is what a game loop polling every frame expects.
    key_buffer_ = turtle_.take_keystrokes();
    if (key_buffer_.empty()) {
        return {};
    }
    const std::string key(1, key_buffer_.front());
    key_buffer_.erase(0, 1);
    return key;
}

std::string Machine::wait(const std::string& args) {
    try {
        const double milliseconds = eval(args);
        const auto clamped = static_cast<long>(std::clamp(milliseconds, 0.0, 60000.0));
        std::this_thread::sleep_for(std::chrono::milliseconds(clamped));
        return "ℹ️ WAIT " + std::to_string(clamped) + "ms\n";
    } catch (const std::exception& error) {
        return "❌ WAIT error: " + std::string{error.what()} + "\n";
    }
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

namespace {

/// The text of one statement argument: `"text"`, a string variable, or an
/// expression -- evaluated with the string evaluator, exactly as `PUT #` does.
[[nodiscard]] std::string argument_text(Interpreter& interpreter, const State& state,
                                        const std::string& field) {
    const std::string trimmed = trim(field);
    if (trimmed.size() >= 2 && trimmed.front() == '"' && trimmed.back() == '"') {
        return trimmed.substr(1, trimmed.size() - 2);
    }
    try {
        return Strings{interpreter, state}.evaluate(trimmed);
    } catch (const BasicError&) {
        return trimmed;
    }
}

/// The `#channel` of a port statement, or -1.
[[nodiscard]] int channel_of(const std::string& field) {
    std::string text = trim(field);
    if (!text.empty() && text.front() == '#') {
        text.erase(0, 1);
    }
    const std::optional<long> value = parse_integer(text);
    return value.has_value() ? static_cast<int>(*value) : -1;
}

}  // namespace

std::string Machine::serial(const std::string& args) {
    const std::vector<std::string> parts = split_trimmed(args, ',');
    if (parts.empty()) {
        return "❌ SERIAL needs OPEN, READ, WRITE or CLOSE\n";
    }
    const std::string first = trim(parts[0]);
    const std::string action = upper(first.substr(0, first.find(' ')));
    const std::string first_rest = first.size() > action.size() ? trim(first.substr(action.size()))
                                                                 : std::string{};

    if (action == "OPEN") {
        const int channel = channel_of(first_rest);
        if (channel < 0 || parts.size() < 3) {
            return "❌ SERIAL OPEN needs #channel, \"path\", baud\n";
        }
        const std::string path = argument_text(interpreter_, state_, parts[1]);
        const std::optional<long> baud = parse_integer(parts[2]);
        if (!baud.has_value()) {
            return "❌ SERIAL OPEN needs a baud rate\n";
        }
        std::string error;
        auto& port = state_.serial_ports[channel];
        if (!port) {
            port = std::make_shared<timewarp::io::SerialPort>();
        }
        if (!port->open(path, static_cast<int>(*baud), &error)) {
            state_.serial_ports.erase(channel);
            return "❌ " + error + "\n";
        }
        return "📡 SERIAL #" + std::to_string(channel) + " open: " + path + " at " +
               std::to_string(*baud) + " baud\n";
    }

    if (action == "CLOSE") {
        const int channel = channel_of(first_rest);
        const auto found = state_.serial_ports.find(channel);
        if (found == state_.serial_ports.end() || !found->second->is_open()) {
            return "❌ SERIAL #" + std::to_string(channel) + " is not open\n";
        }
        found->second->close();
        state_.serial_ports.erase(found);
        return "📡 SERIAL #" + std::to_string(channel) + " closed\n";
    }

    const int channel = action == "READ" || action == "WRITE" ? channel_of(first_rest) : -1;
    if (channel < 0) {
        return "❌ SERIAL " + action + " needs #channel\n";
    }
    const auto found = state_.serial_ports.find(channel);
    if (found == state_.serial_ports.end() || !found->second->is_open()) {
        return "❌ SERIAL #" + std::to_string(channel) + " is not open\n";
    }

    if (action == "WRITE") {
        std::string payload;
        for (std::size_t index = 1; index < parts.size(); ++index) {
            payload += argument_text(interpreter_, state_, parts[index]);
        }
        payload += "\n";
        std::string error;
        if (!found->second->write(payload, &error)) {
            return "❌ " + error + "\n";
        }
        return "📝 SERIAL #" + std::to_string(channel) + " wrote " +
               std::to_string(payload.size()) + " bytes\n";
    }

    // READ: a variable to put the line in, and an optional timeout in ms.
    if (parts.size() < 2) {
        return "❌ SERIAL READ needs #channel, variable$\n";
    }
    int timeout = 1000;
    if (parts.size() >= 3) {
        if (const std::optional<long> ms = parse_integer(parts[2]); ms.has_value()) {
            timeout = static_cast<int>(std::clamp<long>(*ms, 0, 60000));
        }
    }
    std::string error;
    const std::string line = found->second->read_line(timeout, &error);
    if (!error.empty()) {
        return "❌ " + error + "\n";
    }
    std::string target = trim(parts[1]);
    if (!target.empty() && target.back() != '$') {
        target += '$';
    }
    assign(target, line);
    return "📡 SERIAL #" + std::to_string(channel) + " read " +
           std::to_string(line.size()) + " chars\n";
}

std::string Machine::parallel(const std::string& args) {
    const std::vector<std::string> parts = split_trimmed(args, ',');
    if (parts.empty()) {
        return "❌ PARALLEL needs OPEN, WRITE, BYTE or CLOSE\n";
    }
    const std::string first = trim(parts[0]);
    const std::string action = upper(first.substr(0, first.find(' ')));
    const std::string first_rest =
        first.size() > action.size() ? trim(first.substr(action.size())) : std::string{};

    if (action == "OPEN") {
        const int channel = channel_of(first_rest);
        if (channel < 0 || parts.size() < 2) {
            return "❌ PARALLEL OPEN needs #channel, \"path\"\n";
        }
        const std::string path = argument_text(interpreter_, state_, parts[1]);
        std::string error;
        auto& port = state_.parallel_ports[channel];
        if (!port) {
            port = std::make_shared<timewarp::io::ParallelPort>();
        }
        if (!port->open(path, &error)) {
            state_.parallel_ports.erase(channel);
            return "❌ " + error + "\n";
        }
        return "🖨️ PARALLEL #" + std::to_string(channel) + " open: " + path + "\n";
    }

    if (action == "CLOSE") {
        const int channel = channel_of(first_rest);
        const auto found = state_.parallel_ports.find(channel);
        if (found == state_.parallel_ports.end() || !found->second->is_open()) {
            return "❌ PARALLEL #" + std::to_string(channel) + " is not open\n";
        }
        found->second->close();
        state_.parallel_ports.erase(found);
        return "🖨️ PARALLEL #" + std::to_string(channel) + " closed\n";
    }

    const int channel = channel_of(first_rest);
    const auto found = state_.parallel_ports.find(channel);
    if (found == state_.parallel_ports.end() || !found->second->is_open()) {
        return "❌ PARALLEL #" + std::to_string(channel) + " is not open\n";
    }

    if (action == "WRITE") {
        std::string payload;
        for (std::size_t index = 1; index < parts.size(); ++index) {
            payload += argument_text(interpreter_, state_, parts[index]);
        }
        std::string error;
        if (!found->second->write(payload, &error)) {
            return "❌ " + error + "\n";
        }
        return "🖨️ PARALLEL #" + std::to_string(channel) + " wrote " +
               std::to_string(payload.size()) + " bytes\n";
    }

    if (action == "BYTE") {
        if (parts.size() < 2) {
            return "❌ PARALLEL BYTE needs #channel, value\n";
        }
        try {
            const auto value = static_cast<long>(eval(parts[1])) & 0xFF;
            std::string error;
            if (!found->second->write_byte(static_cast<unsigned char>(value), &error)) {
                return "❌ " + error + "\n";
            }
            return "🖨️ PARALLEL #" + std::to_string(channel) + " <- " +
                   std::to_string(value) + "\n";
        } catch (const std::exception& error) {
            return "❌ PARALLEL BYTE: " + std::string{error.what()} + "\n";
        }
    }

    return "❌ PARALLEL " + action + " is not a command (OPEN, WRITE, BYTE, CLOSE)\n";
}

std::string Machine::speed(const std::string& args) {
    const std::optional<long> value = parse_integer(args);
    if (!value.has_value()) {
        return "❌ SPEED requires numeric value (0-10)\n";
    }
    const long clamped = std::max(0L, std::min(10L, *value));
    return "🐢 Turtle speed set to " + std::to_string(clamped) + "\n";
}

std::string Machine::sprite(const std::string& args) {
    const std::vector<std::string> parts = split_trimmed(args, ',');
    if (parts.size() < 2) {
        return "❌ SPRITE requires name and coordinates\n";
    }
    const std::string name = upper(parts[0]);
    if (parts.size() == 2 && upper(parts[1]) == "OFF") {
        // Forget the bitmap as well as hiding it: a name a program has released
        // must not come back when it is used again, or a redefinition would be
        // impossible to tell from the first one.
        state_.sprites.erase(name);
        turtle_.set_sprite_visible(name.c_str(), false);
        return "🎮 Removed sprite: " + name + "\n";
    }
    if (parts.size() >= 5) {
        // `SPRITE name, x, y, w, h` captures the block already drawn at (x, y),
        // which is how a sprite is made on every machine that had them: draw it,
        // take a copy, then move the copy around without redrawing.
        if (!state_.raster.active()) {
            return "❌ SPRITE needs a graphics screen (SCREEN 1)\n";
        }
        try {
            const int x = pixel(eval(parts[1]));
            const int y = pixel(eval(parts[2]));
            const int width = pixel(eval(parts[3]));
            const int height = pixel(eval(parts[4]));
            if (width <= 0 || height <= 0) {
                return "❌ SPRITE " + name + " needs a positive size, got " +
                       std::to_string(width) + "x" + std::to_string(height) + "\n";
            }
            CapturedSprite sprite;
            sprite.width = width;
            sprite.height = height;
            sprite.pixels = state_.raster.block(x, y, width, height);
            // The hotspot is the top-left corner, the same corner `PUT` places a
            // captured block by, so a captured sprite goes back exactly where it
            // came from.
            turtle_.define_sprite(name.c_str(), 0, 0, width, height, sprite.pixels);
            const std::string size = std::to_string(width) + "x" + std::to_string(height);
            sprite.x = x;
            sprite.y = y;
            sprite.placed = true;
            state_.sprites[name] = std::move(sprite);
            return "🎮 Sprite " + name + " captured " + size + " at (" +
                   printed_number(x) + ", " + printed_number(y) + ")\n";
        } catch (const std::exception& error) {
            return "❌ SPRITE error: " + std::string{error.what()} + "\n";
        }
    }
    try {
        const double x = eval(parts[1]);
        const double y = parts.size() > 2 ? eval(parts[2]) : 0.0;
        const auto found = state_.sprites.find(name);
        if (found == state_.sprites.end()) {
            return "❌ No sprite named " + name + " (capture one with SPRITE " + name +
                   ", x, y, w, h)\n";
        }
        if (state_.raster.active()) {
            // The engine's own raster follows, so POINT and GET read what the
            // screen shows rather than what it showed before the sprite moved.
            state_.raster.put_block(static_cast<int>(x), static_cast<int>(y),
                                    found->second.width, found->second.height,
                                    found->second.pixels);
        }
        found->second.x = x;
        found->second.y = y;
        found->second.placed = true;
        turtle_.place_sprite(name.c_str(), x, y);
        return "🎮 Moved sprite " + name + " to (" + printed_number(x) + ", " +
               printed_number(y) + ")\n";
    } catch (const std::exception& error) {
        return "❌ SPRITE error: " + std::string{error.what()} + "\n";
    }
}

std::string Machine::sprite_visible(const std::string& args, bool visible) {
    const std::string name = upper(trim(args));
    if (name.empty()) {
        return std::string{"❌ "} + (visible ? "SPRITESHOW" : "SPRITEHIDE") +
               " requires a sprite name\n";
    }
    if (state_.sprites.find(name) == state_.sprites.end()) {
        return "❌ No sprite named " + name + " (capture one with SPRITE " + name +
               ", x, y, w, h)\n";
    }
    turtle_.set_sprite_visible(name.c_str(), visible);
    return std::string{"🎮 "} + (visible ? "Showing" : "Hiding") + " sprite " + name + "\n";
}

std::string Machine::sprite_move(const std::string& args) {
    const std::vector<std::string> parts = split_trimmed(args, ',');
    if (parts.size() != 3) {
        return "❌ SPRITEMOVE requires name, dx, dy\n";
    }
    const std::string name = upper(parts[0]);
    const auto found = state_.sprites.find(name);
    if (found == state_.sprites.end()) {
        return "❌ No sprite named " + name + " (capture one with SPRITE " + name +
               ", x, y, w, h)\n";
    }
    if (!found->second.placed) {
        return "❌ Sprite " + name + " has not been placed yet (SPRITE " + name +
               ", x, y)\n";
    }
    try {
        const double dx = eval(parts[1]);
        const double dy = eval(parts[2]);
        // A relative move needs the last position, which the canvas already keeps;
        // the engine keeps it too so a raster read stays truthful.
        if (state_.raster.active()) {
            state_.raster.put_block(static_cast<int>(found->second.x + dx),
                                    static_cast<int>(found->second.y + dy),
                                    found->second.width, found->second.height,
                                    found->second.pixels);
        }
        found->second.x += dx;
        found->second.y += dy;
        turtle_.move_sprite(name.c_str(), dx, dy);
        return "🎮 Moved sprite " + name + " by (" + printed_number(dx) + ", " +
               printed_number(dy) + ")\n";
    } catch (const std::exception& error) {
        return "❌ SPRITEMOVE error: " + std::string{error.what()} + "\n";
    }
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

std::string Machine::on_timer(const std::string& args) {
    static const std::regex pattern{R"(\((\d+(?:\.\d+)?)\)\s*GOSUB\s*(\d+))",
                                    std::regex::ECMAScript | std::regex::icase};
    std::smatch match;
    const std::string text = trim(args);
    if (!std::regex_search(text, match, pattern)) {
        return "❌ ON TIMER requires format: ON TIMER(n) GOSUB line\n";
    }
    const double interval = std::stod(match[1].str());
    const long target = std::stol(match[2].str());
    if (interval <= 0.0) {
        return "❌ ON TIMER needs a positive interval, got " + printed_number(interval) +
               "\n";
    }
    state_.timer_defined = true;
    state_.timer_interval = interval;
    state_.timer_target = target;
    return "⏱️ Timer set: every " + printed_number(interval) + "s GOSUB " +
           std::to_string(target) + "\n";
}

double Machine::now_seconds() {
    using clock = std::chrono::steady_clock;
    static const clock::time_point origin = clock::now();
    return std::chrono::duration<double>(clock::now() - origin).count();
}

void Machine::service_timer() {
    if (!state_.timer_enabled || !state_.timer_defined) {
        return;
    }
    // A handler that took longer than the interval must not be entered again from
    // inside itself: the machine would be recursively interrupting itself, which no
    // BASIC ever did. The handler is over when its `GOSUB` frame has been returned.
    if (timer_running) {
        if (state_.gosubs.size() >= timer_depth) {
            return;
        }
        timer_running = false;
    }
    const double now = now_seconds();
    if (now < state_.timer_next_due) {
        return;
    }
    const auto found =
        interpreter_.line_number_map.find(static_cast<int>(state_.timer_target));
    if (found == interpreter_.line_number_map.end()) {
        // A target that is not in the program cannot be a handler; switching the timer
        // off says so once instead of failing every statement.
        state_.timer_enabled = false;
        return;
    }
    // The next event is counted from now, not from the missed one: a program that
    // paused in `INPUT` should not be hit by a burst of catch-up interrupts.
    state_.timer_next_due = now + state_.timer_interval;
    state_.gosubs.push_back(interpreter_.current_line);
    timer_depth = state_.gosubs.size();
    timer_running = true;
    interpreter_.jump_to(found->second);
}

std::string Machine::play(const std::string& args) { return play_music(*this, args); }

std::string Machine::say(const std::string& args) { return speak(*this, args); }

std::string Machine::shape(const std::string& args) { return draw_shape(*this, args); }

std::string Machine::particle(const std::string& args) {
    return create_particle(*this, args);
}

std::string Machine::fractal(const std::string& args) { return draw_fractal(*this, args); }

std::string Machine::poke(const std::string& args) {
    if (args.find(',') == std::string::npos) {
        return "❌ POKE requires: POKE address, value\n";
    }
    const std::size_t comma = args.find(',');
    const std::string address_text = trim(args.substr(0, comma));
    const std::string value_text = trim(args.substr(comma + 1));
    // The arguments are *expressions*, as they are on every machine that had a POKE:
    // `POKE 32768 + 4 * 80 + 5, 65` is how a program writes a cell it computed. They
    // were bare literals until 20.1, which made a computed address an error.
    try {
        const auto address = static_cast<long>(eval(address_text));
        const auto value = static_cast<long>(eval(value_text));
        if (address < 0 || address > 65535) {
            return "❌ POKE address must be 0-65535\n";
        }
        if (value < 0 || value > 255) {
            return "❌ POKE value must be 0-255\n";
        }
        if (interpreter_.machine_attached()) {
            // A machine has real memory, and a POKE into a cell draws. Refusing a
            // write into ROM is the one case a program has to be told about: the
            // byte cannot land and pretending it did would be the silent failure
            // this project treats as the bug.
            if (!interpreter_.machine_poke(static_cast<int>(address),
                                           static_cast<std::uint8_t>(value))) {
                return "❌ POKE " + std::to_string(address) + " is ROM: it cannot be written\n";
            }
            return {};
        }
        state_.memory[static_cast<int>(address)] = static_cast<int>(value);
    } catch (const std::exception& error) {
        return "❌ POKE error: " + std::string{error.what()} + "\n";
    }
    return {};
}

std::string Machine::peek(const std::string& args) {
    // An expression, for the same reason `POKE`'s address is one: a program reads the
    // cell it computed.
    try {
        const auto address = static_cast<long>(eval(trim(args)));
        if (address < 0 || address > 65535) {
            return "❌ PEEK address must be 0-65535: got " + std::to_string(address) + "\n";
        }
        const int value = interpreter_.machine_attached()
                              ? static_cast<int>(
                                    interpreter_.machine_peek(static_cast<int>(address)))
                              : [this, address] {
                                    const auto found =
                                        state_.memory.find(static_cast<int>(address));
                                    return found == state_.memory.end() ? 0 : found->second;
                                }();
        return "ℹ️ PEEK(" + std::to_string(address) + ") = " + std::to_string(value) + "\n";
    } catch (const std::exception& error) {
        return "❌ PEEK error: " + std::string{error.what()} + "\n";
    }
}

std::string Machine::out(const std::string& args) {
    if (args.find(',') == std::string::npos) {
        return "❌ OUT requires: OUT port, value\n";
    }
    const std::size_t comma = args.find(',');
    const std::string port_text = trim(args.substr(0, comma));
    const std::string value_text = trim(args.substr(comma + 1));
    double port_number = 0.0;
    double value_number = 0.0;
    if (!parse_float(port_text, port_number)) {
        return "❌ Expected numeric value for port number, got '" + port_text + "'\n";
    }
    if (!parse_float(value_text, value_number)) {
        return "❌ Expected numeric value for port value, got '" + value_text + "'\n";
    }
    try {
        const auto port = static_cast<long>(eval(port_text));
        const auto value = static_cast<long>(eval(value_text));
        if (port < 0 || port > 65535) {
            return "❌ OUT port must be 0-65535\n";
        }
        if (value < 0 || value > 255) {
            return "❌ OUT value must be 0-255\n";
        }
        state_.ports[static_cast<int>(port)] = static_cast<int>(value);
    } catch (const std::exception& error) {
        return "❌ OUT error: " + std::string{error.what()} + "\n";
    }
    return {};
}

std::string Machine::in_port(const std::string& args) {
    double port_number = 0.0;
    if (!parse_float(trim(args), port_number)) {
        return "❌ Expected numeric value for port number, got '" + trim(args) + "'\n";
    }
    try {
        const auto port = static_cast<long>(eval(trim(args)));
        if (port < 0 || port > 65535) {
            return "❌ IN port must be 0-65535: got " + std::to_string(port) + "\n";
        }
        const auto found = state_.ports.find(static_cast<int>(port));
        const int value = found == state_.ports.end() ? 0 : found->second;
        return "ℹ️ IN(" + std::to_string(port) + ") = " + std::to_string(value) + "\n";
    } catch (const std::exception& error) {
        return "❌ IN error: " + std::string{error.what()} + "\n";
    }
}

std::string Machine::shell(const std::string& args) {
    std::string command = trim(args);
    if (command.empty()) {
        return "❌ SHELL requires command string\n";
    }
    if (command.size() >= 2 && command.front() == '"' && command.back() == '"') {
        command = command.substr(1, command.size() - 2);
    }
    if (command.empty()) {
        return "❌ SHELL requires a command string\n";
    }
    // The reference logs the attempt and refuses to run it; the port refuses
    // without a log sink, and says the same thing.
    return "ℹ️ SHELL commands are disabled in IDE for security. Command: " + command + "\n";
}

std::string Machine::make(const std::string& args) {
    const std::string text = trim(args);
    if (text.empty() || text.front() != '"') {
        return "❌ MAKE expects MAKE \"name value\n";
    }
    const std::size_t closing = text.find('"', 1);
    if (closing == std::string::npos) {
        return "❌ MAKE expects MAKE \"name value\n";
    }
    const std::string name = upper(text.substr(1, closing - 1));
    const std::string value_text = trim(text.substr(closing + 1));
    try {
        assign(name, eval(value_text));
    } catch (const std::exception&) {
        assign(name, strip_quotes(value_text));
    }
    return {};
}

std::string Machine::reset_program(std::string_view message) {
    interpreter_.variables().clear();
    interpreter_.arrays.clear();
    interpreter_.function_hook = nullptr;
    state_.defaults.clear();
    state_.fors.clear();
    state_.gosubs.clear();
    state_.ifs.clear();
    state_.whiles.clear();
    state_.dos.clear();
    state_.calls.clear();
    state_.string_arrays.clear();
    state_.types.clear();
    state_.records.clear();
    state_.subs.clear();
    state_.functions.clear();
    state_.user_fns.clear();
    state_.data.clear();
    state_.data_pointer = 0;
    state_.scanned = false;
    state_.select_expression.clear();
    state_.in_select = false;
    state_.memory.clear();
    state_.ports.clear();
    state_.palette.clear();
    state_.files.clear();
    state_.error_handler = 0;
    state_.error_index = -1;
    state_.timer_defined = false;
    state_.timer_enabled = false;
    state_.screen_mode = 0;
    state_.cols = 80;
    state_.rows = 25;
    state_.screen_width = 800;
    state_.screen_height = 600;
    // `RUN` is a new run, so the screen it could read back goes with the rest of
    // the state: a program that starts with `POINT` must not see the picture its
    // previous run left, and `STICK` must answer 0 until the new run's own
    // `JOYINIT` -- the two facts `SCREEN 0` and the missing `JOYINIT` mean.
    state_.raster = Raster{};
    state_.joystick_initialised = false;
    functions_installed_ = false;
    // The reference clears the program itself, so the run ends here; moving the
    // counter past the last line is the same observable end.
    interpreter_.jump_to(interpreter_.program_lines().size());
    return std::string{message};
}

}  // namespace timewarp::lang::basic

namespace timewarp::lang {

std::string execute_basic(Interpreter& interpreter, std::string_view command,
                          TurtleHost& turtle) {
    basic::State& state = interpreter.executor_state<basic::State>();
    basic::Machine machine{interpreter, turtle, state};
    try {
        return machine.execute(command);
    } catch (const basic::BasicError& error) {
        return "❌ " + std::string{error.what()} + "\n";
    }
}

}  // namespace timewarp::lang
