// SPDX-License-Identifier: MIT
//
// A joystick, read without a window system or a game library.
//
// BASIC's `STICK`/`STRIG` and the `JOYINIT` that arms them were answered with 0
// because the reference's backend (pygame, or `inputs`) was an optional import
// it never shipped. The port has no such dependency to want: Linux already
// exposes every joystick as `/dev/input/js*`, and reading those events needs
// nothing but `read`.
//
// The device is opened on first use and never blocks: a program that does not
// poll a joystick pays for nothing, and a machine with none reports
// `available() == false` rather than failing the program that asked.

#pragma once

#include <array>
#include <string>

namespace timewarp::io {

/// A raw joystick axis value in -32767..32767 scaled to 0-255, centred at 128.
///
/// Rounded rather than truncated: the midpoint of the device's range lands on
/// exactly half of 255, and truncating it would report a resting stick as 127
/// while `STICK` documents 128. Out-of-range raw values clamp, so a driver that
/// reports -32768 cannot wrap to 255.
[[nodiscard]] constexpr int scale_axis(long raw) noexcept {
    const long clamped = raw < -32767L ? -32767L : (raw > 32767L ? 32767L : raw);
    return static_cast<int>(((clamped + 32767L) * 255L + 32767L) / 65534L);
}

/// The first usable joystick on the machine.
///
/// One instance, because there is one joystick a BASIC program can name: the
/// language's `STICK(0..3)` has no device argument to spend on a second.
class Gamepad {
public:
    static Gamepad& instance();

    /// True when a device opened. The first call opens it; later calls report
    /// the state of that one attempt.
    [[nodiscard]] bool available();

    /// An axis in 0-255, centred at 128. Out-of-range indices read 128, so a
    /// program asking for axis 9 gets a neutral answer rather than an error.
    [[nodiscard]] int axis(int index);

    /// A button: 1 while it is held, 0 otherwise.
    [[nodiscard]] bool button(int index);

    /// The device path in use, or an empty string. For the status line and for
    /// a test that wants to know which device was picked.
    [[nodiscard]] const char* device() const noexcept;

    /// Re-reads the pending events. Each `axis`/`button` call polls, so a
    /// program does not have to; this is for a caller that wants one read of
    /// several values at the same time.
    void poll();

private:
    Gamepad() = default;
    ~Gamepad();
    Gamepad(const Gamepad&) = delete;
    Gamepad& operator=(const Gamepad&) = delete;

    void open_device();

    int descriptor_ = -1;
    bool tried_ = false;
    std::string device_path_;
    std::array<int, 8> axes_{{128, 128, 128, 128, 128, 128, 128, 128}};
    std::array<bool, 32> buttons_{};
};

}  // namespace timewarp::io
