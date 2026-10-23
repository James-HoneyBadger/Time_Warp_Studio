// SPDX-License-Identifier: MIT
//
// The tube: `SCREEN`, `LOCATE`, `COLOR`, `PALETTE` and `WIDTH` -- the statements that
// describe the surface before anything is drawn on it.

#include "basic_internal.hpp"
#include "basic_statements_internal.hpp"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstddef>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "timewarp/audio/music.hpp"
#include "timewarp/lang/screen_mode.hpp"

namespace timewarp::lang::basic {

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

}  // namespace timewarp::lang::basic
