// SPDX-License-Identifier: MIT
//
// The HB-2's bus: one 24-bit address space, routed to devices by the table in
// `devices.inc`.
//
// A program sees one address space, and everything it can see is somewhere in it.
// That is the machine's first rule, and it is what makes `POKE` a way to program a
// device rather than a way to write into a map nothing reads. The arithmetic that
// turns an address into a device and an offset is here, once, instead of in the
// seven files it used to be spread across.

#pragma once

#include <array>
#include <cstdint>
#include <vector>

#include "timewarp/hb2/device.hpp"
#include "timewarp/hb2/map.hpp"

namespace timewarp::hb2 {

class Bus {
public:
    static constexpr std::uint32_t kRamSize = 0x100000;

    Bus();

    /// The device that answers for `kind`. Null detaches it, which reads as holes.
    void attach(RegionKind kind, Device* device);
    [[nodiscard]] Device* device(RegionKind kind) const;

    /// MODE 0 turns the 1982 map on: its screen, attributes, I/O page and ROM
    /// overlay RAM, so a 1982 program's `POKE 32768` paints the text cells. MODE 1
    /// and 2 use the 1992 map only.
    void setCompat(bool compat) noexcept { compat_ = compat; }
    [[nodiscard]] bool compat() const noexcept { return compat_; }

    [[nodiscard]] std::uint8_t read(std::uint32_t address) const;
    void write(std::uint32_t address, std::uint8_t value);
    [[nodiscard]] std::uint16_t read16(std::uint32_t address) const;
    void write16(std::uint32_t address, std::uint16_t value);
    /// One 60 Hz frame: every attached device takes one tick.
    void tick();
    void reset();

    [[nodiscard]] std::vector<std::uint8_t>& ram() noexcept { return ram_; }
    [[nodiscard]] const std::vector<std::uint8_t>& ram() const noexcept { return ram_; }

    /// Writes to an address no region claims, or to a region that refuses them.
    /// Counted rather than thrown: a stray `POKE` is a program's mistake, and the
    /// harness reports the count.
    [[nodiscard]] std::uint32_t refusedWrites() const noexcept { return refused_; }

private:
    static constexpr std::size_t kKindCount = 7;

    std::array<Device*, kKindCount> devices_{};
    std::vector<std::uint8_t> ram_;
    bool compat_ = false;
    std::uint32_t refused_ = 0;
};

}  // namespace timewarp::hb2
