// SPDX-License-Identifier: MIT

#include "timewarp/hb2/peripherals.hpp"

#include <algorithm>

namespace timewarp::hb2 {

// ---------------------------------------------------------------------------
// The pointer and the joysticks
// ---------------------------------------------------------------------------

void Input::movePointer(int x, int y) {
    // The delta is what the pointer's owner reads without losing events: it is the
    // distance moved since the last read, not since the last move.
    const int previous_x = pointerX();
    const int previous_y = pointerY();
    registers_[kPointerXLow] = static_cast<std::uint8_t>(x & 0xFF);
    registers_[kPointerXHigh] = static_cast<std::uint8_t>((x >> 8) & 0xFF);
    registers_[kPointerYLow] = static_cast<std::uint8_t>(y & 0xFF);
    registers_[kPointerYHigh] = static_cast<std::uint8_t>((y >> 8) & 0xFF);
    delta_x_ = static_cast<std::int16_t>(delta_x_ + (x - previous_x));
    delta_y_ = static_cast<std::int16_t>(delta_y_ + (y - previous_y));
}

void Input::setPointerButtons(std::uint8_t buttons) { registers_[kPointerButtons] = buttons; }

int Input::pointerX() const {
    return static_cast<int>(registers_[kPointerXLow]) |
           (static_cast<int>(registers_[kPointerXHigh]) << 8);
}

int Input::pointerY() const {
    return static_cast<int>(registers_[kPointerYLow]) |
           (static_cast<int>(registers_[kPointerYHigh]) << 8);
}

void Input::setJoystick(int index, std::int8_t x, std::int8_t y, std::uint8_t buttons) {
    if (index < 0 || index > 1) {
        return;
    }
    const std::uint32_t base = kJoystickBase + static_cast<std::uint32_t>(index) * kJoystickStride;
    registers_[base] = static_cast<std::uint8_t>(x);
    registers_[base + 1] = static_cast<std::uint8_t>(y);
    registers_[base + 2] = buttons;
}

std::uint8_t Input::read(std::uint32_t offset) const {
    if (offset == kPointerDeltaX) {
        const auto value = static_cast<std::uint8_t>(delta_x_ & 0xFF);
        return value;
    }
    if (offset == kPointerDeltaX + 1) {
        return static_cast<std::uint8_t>((delta_x_ >> 8) & 0xFF);
    }
    if (offset == kPointerDeltaY) {
        return static_cast<std::uint8_t>(delta_y_ & 0xFF);
    }
    if (offset == kPointerDeltaY + 1) {
        return static_cast<std::uint8_t>((delta_y_ >> 8) & 0xFF);
    }
    return offset < sizeof(registers_) ? registers_[offset] : std::uint8_t{0};
}

void Input::write(std::uint32_t offset, std::uint8_t value) {
    if (offset >= sizeof(registers_)) {
        return;
    }
    // Reading a delta is what clears it, so the high byte clears the pair once it
    // has been read: a program that asks for both halves gets one movement.
    if (offset == kPointerDeltaY + 1) {
        delta_x_ = 0;
        delta_y_ = 0;
    }
    registers_[offset] = value;
}

void Input::reset() {
    std::fill(std::begin(registers_), std::end(registers_), std::uint8_t{0});
    delta_x_ = 0;
    delta_y_ = 0;
}

// ---------------------------------------------------------------------------
// The clock
// ---------------------------------------------------------------------------

void Rtc::setTime(int year, int month, int day, int hour, int minute, int second) {
    registers_[kYear] = static_cast<std::uint8_t>(std::clamp(year, 0, 255));
    registers_[kMonth] = static_cast<std::uint8_t>(std::clamp(month, 1, 12));
    registers_[kDay] = static_cast<std::uint8_t>(std::clamp(day, 1, 31));
    registers_[kHours] = static_cast<std::uint8_t>(std::clamp(hour, 0, 23));
    registers_[kMinutes] = static_cast<std::uint8_t>(std::clamp(minute, 0, 59));
    registers_[kSeconds] = static_cast<std::uint8_t>(std::clamp(second, 0, 59));
    sub_second_ = 0;
}

std::uint8_t Rtc::read(std::uint32_t offset) const {
    return offset < sizeof(registers_) ? registers_[offset] : std::uint8_t{0};
}

void Rtc::write(std::uint32_t offset, std::uint8_t value) {
    if (offset < sizeof(registers_)) {
        registers_[offset] = value;
    }
}

void Rtc::tick() {
    if (++sub_second_ < 60) {
        return;
    }
    // Sixty frame ticks is a second, which is the machine's own time: no host clock
    // is consulted, so a replay of N ticks reads the same clock every time.
    sub_second_ = 0;
    ++registers_[kTicks];
    if (++registers_[kSeconds] < 60) {
        return;
    }
    registers_[kSeconds] = 0;
    if (++registers_[kMinutes] < 60) {
        return;
    }
    registers_[kMinutes] = 0;
    if (++registers_[kHours] < 24) {
        return;
    }
    registers_[kHours] = 0;
    if (++registers_[kDay] <= 28) {
        return;
    }
    // The machine is kind where a real 1992 clock was cruel: a month is 28 days and
    // the manual says so, rather than a leap-year table nobody would read.
    registers_[kDay] = 1;
    if (++registers_[kMonth] <= 12) {
        return;
    }
    registers_[kMonth] = 1;
    ++registers_[kYear];
}

void Rtc::reset() {
    std::fill(std::begin(registers_), std::end(registers_), std::uint8_t{0});
    setTime(92, 1, 1, 0, 0, 0);
}

// ---------------------------------------------------------------------------
// The serial port
// ---------------------------------------------------------------------------

void Uart::receive(const std::string& bytes) { received_ += bytes; }

std::uint8_t Uart::read(std::uint32_t offset) const {
    if (offset == kData) {
        if (read_at_ < received_.size()) {
            return static_cast<std::uint8_t>(received_[read_at_++]);
        }
        return 0;
    }
    if (offset == kStatus) {
        return read_at_ < received_.size() ? 0x01 : 0x02;
    }
    return 0;
}

void Uart::write(std::uint32_t offset, std::uint8_t value) {
    if (offset == kData) {
        transmitted_.push_back(static_cast<char>(value));
    }
}

void Uart::reset() {
    received_.clear();
    read_at_ = 0;
    transmitted_.clear();
}

}  // namespace timewarp::hb2
