// SPDX-License-Identifier: MIT
//
// Python's `random` module, reproduced exactly, for parity.
//
// Several languages expose randomness (`FRAN` in FOCAL, `RND` in BASIC and
// CHIP-8). A port that used `std::mt19937` directly would be an implementation
// of something the reference does not do: the same seed would produce a
// different sequence, so the differential harness could not tell a real defect
// from a different generator. This is CPython's Mersenne Twister seeded the way
// `random.seed(int)` seeds it, with `random()` and `randint()` built the way
// `_randommodule.c` builds them.
//
// Deliberately small: only the operations the executors use. Adding one means
// copying CPython's definition of it, not inventing an equivalent.

#pragma once

#include <cstdint>

namespace timewarp::lang {

class PythonRandom {
public:
    PythonRandom() { seed(0); }

    /// `random.seed(n)` for an integer: the absolute value, little-endian
    /// 32-bit words, through MT19937's `init_by_array`.
    void seed(std::int64_t value);
    void seed_unsigned(std::uint64_t value);

    /// `random.random()`: 53 random bits, scaled to [0, 1).
    [[nodiscard]] double random();

    /// `random.randint(low, high)`, inclusive.
    [[nodiscard]] int randint(int low, int high);

private:
    void init_genrand(std::uint32_t seed);
    void init_by_array(const std::uint32_t* key, std::size_t length);
    [[nodiscard]] std::uint32_t next_uint32();
    [[nodiscard]] std::uint32_t getrandbits(unsigned bits);

    static constexpr std::size_t kStateSize = 624;
    static constexpr std::size_t kPeriod = 397;
    static constexpr std::uint32_t kMatrixA = 0x9908b0dfU;
    static constexpr std::uint32_t kUpperMask = 0x80000000U;
    static constexpr std::uint32_t kLowerMask = 0x7fffffffU;

    std::uint32_t state_[kStateSize]{};
    std::size_t index_ = kStateSize + 1;
};

}  // namespace timewarp::lang
