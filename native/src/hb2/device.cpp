// SPDX-License-Identifier: MIT

#include "timewarp/hb2/device.hpp"

#include <algorithm>

namespace timewarp::hb2 {

Rom::Rom() : bytes_(kSize, 0xFF) {}

std::uint8_t Rom::read(std::uint32_t offset) const {
    return offset < bytes_.size() ? bytes_[offset] : std::uint8_t{0};
}

void Rom::write(std::uint32_t /*offset*/, std::uint8_t /*value*/) {
    // A ROM is a ROM. The count is here so a test can say "the write was refused"
    // rather than inferring it from the byte that did not change.
    ++refused_;
}

void Rom::reset() {
    // The bytes stay: a ROM is what it is until the host loads another one.
}

void Rom::load(std::uint32_t offset, const std::vector<std::uint8_t>& bytes) {
    for (std::size_t index = 0; index < bytes.size(); ++index) {
        const std::uint64_t at = static_cast<std::uint64_t>(offset) + index;
        if (at < bytes_.size()) {
            bytes_[static_cast<std::size_t>(at)] = bytes[index];
        }
    }
}

}  // namespace timewarp::hb2
