// SPDX-License-Identifier: MIT
//
// The raster's operations. See `include/timewarp/lang/raster.hpp` for what it
// is for and for its coordinate convention.

#include "timewarp/lang/raster.hpp"

#include <algorithm>
#include <cmath>
#include <utility>

namespace timewarp::lang {
namespace {

constexpr double kPi = 3.14159265358979323846;

[[nodiscard]] int rounded(double value) {
    return static_cast<int>(std::lround(value));
}

}  // namespace

void Raster::resize(int width, int height, Rgb background) {
    width_ = std::max(0, width);
    height_ = std::max(0, height);
    background_ = background;
    pixels_.assign(static_cast<std::size_t>(width_) * static_cast<std::size_t>(height_),
                   background);
}

void Raster::clear(Rgb background) {
    background_ = background;
    std::fill(pixels_.begin(), pixels_.end(), background);
}

bool Raster::contains(int x, int y) const noexcept {
    return x >= 0 && y >= 0 && x < width_ && y < height_;
}

std::size_t Raster::offset(int x, int y) const noexcept {
    return static_cast<std::size_t>(y) * static_cast<std::size_t>(width_) +
           static_cast<std::size_t>(x);
}

void Raster::set(int x, int y, Rgb colour) {
    if (contains(x, y)) {
        pixels_[offset(x, y)] = colour;
    }
}

Rgb Raster::at(int x, int y) const {
    return contains(x, y) ? pixels_[offset(x, y)] : background_;
}

void Raster::line(int x0, int y0, int x1, int y1, Rgb colour) {
    // Bresenham in integers, so two runs plot the same pixels and a captured
    // sprite is the sprite that was drawn.
    int dx = std::abs(x1 - x0);
    int dy = -std::abs(y1 - y0);
    const int step_x = x0 < x1 ? 1 : -1;
    const int step_y = y0 < y1 ? 1 : -1;
    int error = dx + dy;
    for (;;) {
        set(x0, y0, colour);
        if (x0 == x1 && y0 == y1) {
            break;
        }
        const int twice = 2 * error;
        if (twice >= dy) {
            error += dy;
            x0 += step_x;
        }
        if (twice <= dx) {
            error += dx;
            y0 += step_y;
        }
    }
}

void Raster::rect(int x0, int y0, int x1, int y1, Rgb colour, bool filled) {
    const int left = std::min(x0, x1);
    const int right = std::max(x0, x1);
    const int top = std::min(y0, y1);
    const int bottom = std::max(y0, y1);
    if (filled) {
        for (int y = top; y <= bottom; ++y) {
            for (int x = left; x <= right; ++x) {
                set(x, y, colour);
            }
        }
        return;
    }
    line(left, top, right, top, colour);
    line(right, top, right, bottom, colour);
    line(right, bottom, left, bottom, colour);
    line(left, bottom, left, top, colour);
}

void Raster::arc(double centre_x, double centre_y, double radius, double start_degrees,
                 double extent, Rgb colour) {
    if (radius <= 0.0) {
        return;
    }
    if (extent == 0.0) {
        extent = 360.0;
    }
    // The same sampling `CIRCLE` hands the host, so the pixels and the polyline
    // the canvas draws are two renderings of one shape rather than two shapes.
    const int segments = std::max(12, static_cast<int>(std::fabs(extent) / 10.0));
    const double step = extent / static_cast<double>(segments);
    const auto point = [centre_x, centre_y, radius](double degrees) {
        const double radians = degrees * kPi / 180.0;
        return std::pair<int, int>{rounded(centre_x + radius * std::cos(radians)),
                                   rounded(centre_y + radius * std::sin(radians))};
    };
    auto [x, y] = point(start_degrees);
    for (int index = 1; index <= segments; ++index) {
        const auto [next_x, next_y] = point(start_degrees + index * step);
        line(x, y, next_x, next_y, colour);
        x = next_x;
        y = next_y;
    }
}

void Raster::flood_fill(int x, int y, Rgb colour) {
    if (!contains(x, y)) {
        return;
    }
    const Rgb target = pixels_[offset(x, y)];
    if (target == colour) {
        return;
    }
    std::vector<std::pair<int, int>> pending;
    pending.emplace_back(x, y);
    while (!pending.empty()) {
        const auto [current_x, current_y] = pending.back();
        pending.pop_back();
        if (!contains(current_x, current_y)) {
            continue;
        }
        if (!(pixels_[offset(current_x, current_y)] == target)) {
            continue;
        }
        pixels_[offset(current_x, current_y)] = colour;
        pending.emplace_back(current_x + 1, current_y);
        pending.emplace_back(current_x - 1, current_y);
        pending.emplace_back(current_x, current_y + 1);
        pending.emplace_back(current_x, current_y - 1);
    }
}

std::vector<Rgb> Raster::block(int x, int y, int width, int height) const {
    std::vector<Rgb> result;
    if (width <= 0 || height <= 0) {
        return result;
    }
    result.reserve(static_cast<std::size_t>(width) * static_cast<std::size_t>(height));
    for (int row = 0; row < height; ++row) {
        for (int column = 0; column < width; ++column) {
            result.push_back(at(x + column, y + row));
        }
    }
    return result;
}

void Raster::put_block(int x, int y, int width, int height,
                       const std::vector<Rgb>& pixels) {
    if (width <= 0 || height <= 0) {
        return;
    }
    if (pixels.size() < static_cast<std::size_t>(width) * static_cast<std::size_t>(height)) {
        return;
    }
    for (int row = 0; row < height; ++row) {
        for (int column = 0; column < width; ++column) {
            set(x + column, y + row, pixels[static_cast<std::size_t>(row) *
                                               static_cast<std::size_t>(width) +
                                           static_cast<std::size_t>(column)]);
        }
    }
}

}  // namespace timewarp::lang
