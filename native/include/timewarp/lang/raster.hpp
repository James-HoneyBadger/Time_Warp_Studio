// SPDX-License-Identifier: MIT
//
// A fixed-size pixel surface for programs that want a screen, not a drawing.
//
// The turtle canvas is a display list: it scales to fit, it has no origin a
// program can rely on, and nothing in it can be read back. A BASIC program that
// does `SCREEN 1`, plots pixels and then captures a sprite has a different
// model in mind -- a screen of known width and height where (0, 0) is a corner
// and `POINT(x, y)` answers what is there.
//
// This is that screen. It is deliberately *not* a host: the executors keep
// drawing through `TurtleHost` exactly as before, so the picture the canvas
// shows and the parity gate's command stream are unchanged, and this buffer is
// what makes `GET`, `PUT` and `POINT` possible. When a raster-aware canvas
// arrives it renders `pixels()`; until then the raster is the program's own
// memory of what it drew.
//
// Coordinates are screen coordinates: x to the right, y **down**, origin at the
// top left. The primitive stream uses world coordinates with y up; the two are
// related by a flip, which is the adapter's job and not this class's.

#pragma once

#include <cstddef>
#include <vector>

#include "timewarp/lang/turtle_host.hpp"

namespace timewarp::lang {

/// A width x height buffer of `Rgb`, with the raster operations a program needs.
///
/// Every operation clips. A program that plots at (-5, 900) on a 320x200 screen
/// is not making an error a learner wants a message about -- GW-BASIC clipped
/// too -- so nothing here throws.
class Raster {
public:
    Raster() = default;

    /// Sizes the surface and fills it with *background*. Re-sizing discards the
    /// picture, which is what `SCREEN` means.
    void resize(int width, int height, Rgb background = Rgb{});
    /// Keeps the size and fills it.
    void clear(Rgb background);

    /// True once a size has been set. A program that plots before `SCREEN` has
    /// no raster, and the statements that need one say so.
    [[nodiscard]] bool active() const noexcept { return width_ > 0 && height_ > 0; }
    [[nodiscard]] int width() const noexcept { return width_; }
    [[nodiscard]] int height() const noexcept { return height_; }
    [[nodiscard]] Rgb background() const noexcept { return background_; }

    [[nodiscard]] bool contains(int x, int y) const noexcept;

    void set(int x, int y, Rgb colour);
    /// The colour at a point. Outside the surface it is the background, so a
    /// program that asks about a pixel that is not there gets an answer rather
    /// than an error.
    [[nodiscard]] Rgb at(int x, int y) const;

    void line(int x0, int y0, int x1, int y1, Rgb colour);
    void rect(int x0, int y0, int x1, int y1, Rgb colour, bool filled);
    /// A circular arc. `start_degrees` is measured clockwise from +x and
    /// `extent` is the sweep; a full circle is `extent == 360`.
    void arc(double centre_x, double centre_y, double radius, double start_degrees,
             double extent, Rgb colour);
    /// Paints the connected region of one colour that contains (x, y).
    void flood_fill(int x, int y, Rgb colour);

    /// A rectangular block, row-major, top row first. Clipped to the surface.
    [[nodiscard]] std::vector<Rgb> block(int x, int y, int width, int height) const;
    void put_block(int x, int y, int width, int height, const std::vector<Rgb>& pixels);

    /// The whole surface, row-major, top row first.
    [[nodiscard]] const std::vector<Rgb>& pixels() const noexcept { return pixels_; }

private:
    [[nodiscard]] std::size_t offset(int x, int y) const noexcept;

    int width_ = 0;
    int height_ = 0;
    Rgb background_{};
    std::vector<Rgb> pixels_;
};

}  // namespace timewarp::lang
