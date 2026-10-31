// SPDX-License-Identifier: MIT
//
// The HB-2's memory map, read from `devices.inc`.
//
// The page a device answers on is not an implementation detail: a program POKEs it,
// the manual prints it and the bus routes with it. So the rows live in one include
// and this header is the compiled form of them, the way `theme_table.inc` and the
// language registry are the compiled form of theirs.

#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <string_view>

namespace timewarp::hb2 {

/// Which device owns a region of the map. `Ram` is the bus's own.
enum class RegionKind { Ram, Video, Audio, Input, Rtc, Uart, Rom };

struct Region {
    std::string_view name;
    std::uint32_t base;
    std::uint32_t size;
    RegionKind kind;
    /// Where in the owning device's own address space this window starts.
    std::uint32_t device_offset;
    /// True for a row of the 1982 map, which exists only in MODE 0.
    bool compat_only;

    [[nodiscard]] std::uint32_t end() const noexcept { return base + size; }
    [[nodiscard]] bool holds(std::uint32_t address) const noexcept {
        return address >= base && address < end();
    }
};

#define HB2_REGION(name, base, size, kind, offset, compat) \
    Region { name, base, size, RegionKind::kind, offset, compat },

inline constexpr std::array<Region, 12> kRegions{{
#include "timewarp/hb2/devices.inc"
}};

#undef HB2_REGION

/// The region that claims `address`, in the order the bus asks: the 1982 rows first
/// when the machine is in MODE 0, because they overlay RAM. Null for an address no
/// device answers for, which reads zero and refuses a write.
[[nodiscard]] inline const Region* regionFor(std::uint32_t address, bool compat) {
    if (compat) {
        for (const Region& region : kRegions) {
            if (region.compat_only && region.holds(address)) {
                return &region;
            }
        }
    }
    for (const Region& region : kRegions) {
        if (!region.compat_only && region.holds(address)) {
            return &region;
        }
    }
    return nullptr;
}

}  // namespace timewarp::hb2
