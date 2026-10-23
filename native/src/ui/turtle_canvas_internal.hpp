// SPDX-License-Identifier: MIT
//
// What the canvas's translation units share.
//
// `turtle_canvas.cpp` was 1,847 lines: the command handlers, the projection, the
// events, the screen mode and the painter in one file, over a preamble of constants
// and two angle helpers that every one of those parts uses. The preamble is here, in
// a named `canvas_detail` namespace, because four translation units read it now.
//
// Internal to the canvas: nothing outside `src/ui/turtle_canvas*.cpp` includes it.

#pragma once

#include "timewarp/ui/turtle_canvas.hpp"

#include <cmath>
#include <numbers>

namespace timewarp::ui::canvas_detail {

/// The world window shown when nothing has been drawn yet. Chosen to match the
/// Python canvas's default extent closely enough that a program's first
/// `forward 100` looks the same size in both.
constexpr double kDefaultWorldHalfExtent = 250.0;

/// Breathing room left around the ink when fitting, as a fraction of its size.
constexpr double kFitMargin = 0.08;

/// Smallest world window the fit will produce, so a program that draws a single
/// point does not get magnified to a meaningless scale.
constexpr double kMinimumFitExtent = 10.0;

constexpr double kMinimumZoom = 0.05;
constexpr double kMaximumZoom = 50.0;

/// Target on-screen spacing between grid lines. Below this the grid stops being
/// a reference and becomes noise, so the step moves up a decade instead.
constexpr double kMinimumGridSpacingPixels = 14.0;

/// Grid steps a person reads without counting: 1, 2, 5, 10, 20, 50, ...
constexpr double kGridSteps[] = {0.1,  0.2,  0.5,  1.0,   2.0,   5.0,
                                 10.0, 20.0, 50.0, 100.0, 200.0, 500.0,
                                 1000.0, 2000.0, 5000.0, 10000.0};

/// The turtle marker's size in *screen* pixels. Deliberately not world units: a
/// turtle that grows when you zoom out stops being a position cue.
constexpr double kTurtlePixels = 9.0;

[[nodiscard]] inline double degreesToRadians(double degrees) noexcept {
    return degrees * std::numbers::pi / 180.0;
}

/// Brings an angle into `[0, 360)`, the way Python's `%` does.
///
/// `std::fmod` alone is not enough: it keeps the sign of its argument, so
/// `right(720)` and `left(360)` would leave the heading negative here while the
/// reference leaves it positive, and an identical program would then disagree
/// about the heading from its first left turn onwards.
[[nodiscard]] inline double normaliseHeading(double degrees) noexcept {
    double result = std::fmod(degrees, 360.0);
    if (result < 0.0) {
        result += 360.0;
    }
    return result;
}

}  // namespace timewarp::ui::canvas_detail
