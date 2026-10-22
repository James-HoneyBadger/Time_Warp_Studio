// SPDX-License-Identifier: MIT
//
// The screen-mode table. See `screen_mode.hpp` for why it is data.

#include "timewarp/lang/screen_mode.hpp"

#include <algorithm>
#include <array>

namespace timewarp::lang {
namespace {

// The historical numbers, and what each one was. `SCREEN 1` keeps the
// reference's 800x600 because that is the size the parity gate has always
// compared; the rest are the modes the machines of the era actually had, and a
// program that names one gets the size its author expected.
constexpr std::array<ScreenMode, 7> kModes{{
    {0, 80, 25, 0, "text"},
    {1, 800, 600, 16, "graphics"},
    {2, 640, 200, 16, "CGA graphics"},
    {7, 320, 200, 16, "EGA 320x200"},
    {9, 640, 350, 16, "EGA 640x350"},
    {12, 640, 480, 16, "VGA 640x480"},
    {13, 320, 200, 256, "VGA 320x200, 256 colours"},
}};

}  // namespace

std::span<const ScreenMode> screen_modes() noexcept { return kModes; }

const ScreenMode* find_screen_mode(int number) noexcept {
    const auto found = std::find_if(kModes.begin(), kModes.end(),
                                    [number](const ScreenMode& mode) {
                                        return mode.number == number;
                                    });
    return found == kModes.end() ? nullptr : &*found;
}

bool is_text_mode(int number) noexcept {
    const ScreenMode* mode = find_screen_mode(number);
    return mode != nullptr && mode->colours == 0;
}

const char* supported_screen_modes() noexcept { return "0, 1, 2, 7, 9, 12, 13"; }

}  // namespace timewarp::lang
