// SPDX-License-Identifier: MIT
//
// What the canvas's translation units share.
//
// `turtle_canvas.cpp` was 1,847 lines over a preamble of constants and two angle
// helpers. When it was split by section, most of that preamble turned out to belong to
// one part each -- the fit's constants are the view's, the grid's are the painter's --
// so they went with their reader and only `degreesToRadians`, which three parts call,
// stayed. One fact, one home: a constant in a shared header is a constant nobody owns.
//
// Internal to the canvas: nothing outside `src/ui/turtle_canvas*.cpp` includes it.

#pragma once

#include <cmath>
#include <numbers>

namespace timewarp::ui::canvas_detail {

[[nodiscard]] inline double degreesToRadians(double degrees) noexcept {
    return degrees * std::numbers::pi / 180.0;
}

}  // namespace timewarp::ui::canvas_detail
