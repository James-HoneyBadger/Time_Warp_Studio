// SPDX-License-Identifier: MIT
//
// What a device on the HB-2's bus is: six plain calls and no widgets.
//
// The rule the machine is built by is that a device is a value type -- `HbDos` was
// the model: plain calls, one image, and the window owns the pixels. A device here
// answers on a range of its own addresses, and the bus does the arithmetic that maps
// a program's 24-bit address onto it. Nothing inherits a `QWidget`, and no device
// knows what a screen is.

#pragma once

#include <cstdint>
#include <vector>

namespace timewarp::hb2 {

class Device {
public:
    virtual ~Device() = default;

    /// A byte of the device's own address space. Out-of-range reads answer zero,
    /// because a machine built down to a price has holes in it and a hole that
    /// answers zero is easier to reason about than one that answers an error.
    [[nodiscard]] virtual std::uint8_t read(std::uint32_t offset) const = 0;
    /// A byte written. A device may refuse -- a ROM does -- and refusing is not an
    /// error the program sees, exactly like the real thing.
    virtual void write(std::uint32_t offset, std::uint8_t value) = 0;
    /// One 60 Hz frame of the device's own time.
    virtual void tick() {}
    virtual void reset() {}
};

/// The 64 KiB of ROM the machine boots from. Reads answer its bytes; writes are
/// refused. Filled with `$FF` rather than zero, so that jumping into an empty page
/// is an invalid instruction the processor reports instead of a field of `NOP`s that
/// runs forever -- which is what "SYS into ROM reports rather than executing zeros"
/// asks for.
class Rom final : public Device {
public:
    static constexpr std::uint32_t kSize = 0x10000;

    Rom();
    [[nodiscard]] std::uint8_t read(std::uint32_t offset) const override;
    void write(std::uint32_t offset, std::uint8_t value) override;
    void reset() override;

    /// Copies `bytes` in at `offset`, for a module or a test. A program cannot do
    /// this; only the host can.
    void load(std::uint32_t offset, const std::vector<std::uint8_t>& bytes);

    [[nodiscard]] std::uint32_t refusedWrites() const noexcept { return refused_; }

private:
    std::vector<std::uint8_t> bytes_;
    std::uint32_t refused_ = 0;
};

}  // namespace timewarp::hb2
