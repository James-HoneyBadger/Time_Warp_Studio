// SPDX-License-Identifier: MIT

#include "timewarp/hb2/bus.hpp"

#include <cstddef>

namespace timewarp::hb2 {

namespace {

constexpr std::size_t indexOf(RegionKind kind) { return static_cast<std::size_t>(kind); }

}  // namespace

Bus::Bus() : ram_(kRamSize, 0) {}

void Bus::attach(RegionKind kind, Device* device) { devices_[indexOf(kind)] = device; }

Device* Bus::device(RegionKind kind) const { return devices_[indexOf(kind)]; }

std::uint8_t Bus::read(std::uint32_t address) const {
    const Region* region = regionFor(address, compat_);
    if (region == nullptr) {
        // Unclaimed: a hole. Zero rather than an exception, because that is what a
        // hole in a real map answers and because a `PEEK` must not stop a program.
        return 0;
    }
    const std::uint32_t offset = address - region->base + region->device_offset;
    switch (region->kind) {
    case RegionKind::Ram:
        return offset < ram_.size() ? ram_[offset] : std::uint8_t{0};
    default:
        break;
    }
    Device* device = this->device(region->kind);
    return device == nullptr ? std::uint8_t{0} : device->read(offset);
}

void Bus::write(std::uint32_t address, std::uint8_t value) {
    const Region* region = regionFor(address, compat_);
    if (region == nullptr) {
        ++refused_;
        return;
    }
    const std::uint32_t offset = address - region->base + region->device_offset;
    if (region->kind == RegionKind::Ram) {
        if (offset < ram_.size()) {
            ram_[offset] = value;
        } else {
            ++refused_;
        }
        return;
    }
    Device* device = this->device(region->kind);
    if (device == nullptr) {
        ++refused_;
        return;
    }
    device->write(offset, value);
}

std::uint16_t Bus::read16(std::uint32_t address) const {
    // Little-endian, which is the processor's order and therefore the order a
    // program writing a table expects.
    const std::uint16_t low = read(address);
    const std::uint16_t high = read(address + 1);
    return static_cast<std::uint16_t>(low | (high << 8));
}

void Bus::write16(std::uint32_t address, std::uint16_t value) {
    write(address, static_cast<std::uint8_t>(value & 0xFF));
    write(address + 1, static_cast<std::uint8_t>(value >> 8));
}

void Bus::tick() {
    for (Device* device : devices_) {
        if (device != nullptr) {
            device->tick();
        }
    }
}

void Bus::reset() {
    std::fill(ram_.begin(), ram_.end(), std::uint8_t{0});
    for (Device* device : devices_) {
        if (device != nullptr) {
            device->reset();
        }
    }
    compat_ = false;
    refused_ = 0;
}

}  // namespace timewarp::hb2
