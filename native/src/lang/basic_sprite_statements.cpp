// SPDX-License-Identifier: MIT
//
// Sprites and pixels: `GET`/`PUT`, the sprite statements and their visibility and
// movement.

#include "basic_internal.hpp"
#include "basic_statements_internal.hpp"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstddef>
#include <filesystem>
#include <regex>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "timewarp/audio/music.hpp"
#include "timewarp/lang/screen_mode.hpp"

namespace timewarp::lang::basic {

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

}  // namespace timewarp::lang::basic
