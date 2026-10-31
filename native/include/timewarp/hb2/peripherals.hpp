// SPDX-License-Identifier: MIT
//
// The three small devices a 1992 machine plugged in: a pointer and two joysticks,
// a real-time clock, and a serial port.
//
// Each is one page of registers, because that is what each was: a mouse is a
// position and two buttons, a clock is six numbers, a UART is a byte in and a byte
// out. None of them knows about a widget, and each can be driven from a test as
// easily as from a program -- which is how the pointer at a known cell is asserted
// rather than clicked.
//
// The clock is the machine's own, never the host's: `setTime` is how a test injects
// a reading, and it advances one second per sixty frame ticks. A test that waited for
// a real second would be a test that fails on a loaded machine.

#pragma once

#include <cstdint>
#include <string>

#include "timewarp/hb2/device.hpp"

namespace timewarp::hb2 {

/// The pointer and two joysticks, on one page.
class Input final : public Device {
public:
    enum Register : std::uint32_t {
        kPointerXLow = 0x00,
        kPointerXHigh = 0x01,
        kPointerYLow = 0x02,
        kPointerYHigh = 0x03,
        /// Bit 0 is the left button, bit 1 the right.
        kPointerButtons = 0x04,
        /// Movement since the last read; reading clears it, so a program can keep up
        /// without losing events.
        kPointerDeltaX = 0x06,
        kPointerDeltaY = 0x08,
        kJoystickBase = 0x10,
        kJoystickStride = 0x04,
    };

    /// Moves the pointer and records the delta the pointer's owner will read.
    void movePointer(int x, int y);
    void setPointerButtons(std::uint8_t buttons);
    [[nodiscard]] int pointerX() const;
    [[nodiscard]] int pointerY() const;
    void setJoystick(int index, std::int8_t x, std::int8_t y, std::uint8_t buttons);

    [[nodiscard]] std::uint8_t read(std::uint32_t offset) const override;
    void write(std::uint32_t offset, std::uint8_t value) override;
    void reset() override;

private:
    std::uint8_t registers_[0x100]{};
    mutable std::int16_t delta_x_ = 0;
    mutable std::int16_t delta_y_ = 0;
};

/// A real-time clock. Six numbers, advanced by the machine's frame tick.
class Rtc final : public Device {
public:
    enum Register : std::uint32_t {
        kSeconds = 0x00,
        kMinutes = 0x01,
        kHours = 0x02,
        kDay = 0x03,
        kMonth = 0x04,
        kYear = 0x05,
        kTicks = 0x06,
    };

    /// Injects a reading, which is what a test does instead of waiting.
    void setTime(int year, int month, int day, int hour, int minute, int second);
    [[nodiscard]] int seconds() const { return registers_[kSeconds]; }
    [[nodiscard]] int minutes() const { return registers_[kMinutes]; }
    [[nodiscard]] int hours() const { return registers_[kHours]; }

    [[nodiscard]] std::uint8_t read(std::uint32_t offset) const override;
    void write(std::uint32_t offset, std::uint8_t value) override;
    void tick() override;
    void reset() override;

private:
    std::uint8_t registers_[0x20]{};
    int sub_second_ = 0;
};

/// A serial port with a byte of FIFO each way. The transmitted bytes are kept, so a
/// test reads the line rather than watching one.
class Uart final : public Device {
public:
    enum Register : std::uint32_t {
        kData = 0x00,
        /// Bit 0: a byte is waiting. Bit 1: the transmitter is clear.
        kStatus = 0x01,
    };

    void receive(const std::string& bytes);
    [[nodiscard]] const std::string& transmitted() const { return transmitted_; }
    void clearTransmitted() { transmitted_.clear(); }

    [[nodiscard]] std::uint8_t read(std::uint32_t offset) const override;
    void write(std::uint32_t offset, std::uint8_t value) override;
    void reset() override;

private:
    std::string received_;
    /// Mutable because reading the byte is what consumes it: a UART's receive
    /// register is not a const thing.
    mutable std::size_t read_at_ = 0;
    std::string transmitted_;
};

}  // namespace timewarp::hb2
