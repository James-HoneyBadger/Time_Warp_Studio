// SPDX-License-Identifier: MIT
//
// The screen a program can ask for, as data.
//
// `SCREEN n` is the one statement whose whole job is to pick a display mode, and
// the modes those machines had are a fixed, small set. Keeping them in a table
// rather than in a chain of `if`s means the raster, the canvas and the error
// message that lists the choices cannot disagree about what `SCREEN 13` is.
//
// Mode 1 is the reference's own graphics mode: it has a size (800x600) but also
// accepts an explicit width and height, which is what the parity gate compares.
// The numbered modes above it are the historical ones, and are a native
// extension -- the reference never implemented them.

#pragma once

#include <span>

namespace timewarp::lang {

/// One display mode a program may select.
struct ScreenMode {
    /// The number `SCREEN` takes.
    int number;
    /// Pixels across, or columns in text mode.
    int width;
    /// Pixels down, or rows in text mode.
    int height;
    /// Colours the mode could show; 0 means text.
    int colours;
    /// What the mode is called in the error message and the guide.
    const char* name;
};

/// Every mode, in ascending order of number.
[[nodiscard]] std::span<const ScreenMode> screen_modes() noexcept;

/// The mode with this number, or nullptr when there is no such mode.
[[nodiscard]] const ScreenMode* find_screen_mode(int number) noexcept;

/// True for the text mode, which has no pixel surface.
[[nodiscard]] bool is_text_mode(int number) noexcept;

/// `0, 1, 2, 7, 9, 12, 13` -- the choices, for an error message that tells a
/// learner what they could have said instead of only what they may not.
[[nodiscard]] const char* supported_screen_modes() noexcept;

}  // namespace timewarp::lang
