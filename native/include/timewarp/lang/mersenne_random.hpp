// SPDX-License-Identifier: MIT
//
// The seeded generator `RND` and `FRAN` answer with.
//
// Several languages expose randomness (`FRAN` in FOCAL, `RND` in BASIC, CHIP-8 and
// Logo). A port that used `std::mt19937` directly would implement something else:
// the same seed would produce a different sequence, so a program that seeds itself
// would print different numbers after the port than before it. So this is MT19937
// with the seeding and the sampling the reference used -- an integer seed split
// little-endian into 32-bit words through `init_by_array`, `random()` taking 53
// bits and scaling to [0, 1), and `randint(low, high)` inclusive by rejection
// sampling.
//
// The command rows assert only that these commands *produce output*, so the
// sequence itself is held in one place: the recorded draws asserted in `lang_tests`.
// A change here is a change to a program's output, which is why that test exists.
//
// Deliberately small: only the operations the executors use. Adding one means
// reproducing the reference's definition of it, not inventing an equivalent.

#pragma once

#include <cstddef>
#include <cstdint>

namespace timewarp::lang {

class MersenneRandom {
public:
    MersenneRandom() { seed(0); }

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
