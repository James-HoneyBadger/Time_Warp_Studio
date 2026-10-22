// SPDX-License-Identifier: MIT

#include "timewarp/lang/mersenne_random.hpp"

#include <cstdlib>

namespace timewarp::lang {

void MersenneRandom::seed(std::int64_t value) {
    seed_unsigned(value < 0 ? static_cast<std::uint64_t>(-(value + 1)) + 1U
                            : static_cast<std::uint64_t>(value));
}

void MersenneRandom::seed_unsigned(std::uint64_t value) {
    // The recorded seeding takes the absolute value, splits it into
    // little-endian 32-bit words and passes them to init_by_array. A zero seed is
    // a one-word `[0]`, not an empty array.
    std::uint32_t key[2]{};
    std::size_t length = 1;
    key[0] = static_cast<std::uint32_t>(value & 0xFFFFFFFFU);
    if (value > 0xFFFFFFFFULL) {
        key[1] = static_cast<std::uint32_t>(value >> 32);
        length = 2;
    }
    init_by_array(key, length);
}

void MersenneRandom::init_genrand(std::uint32_t seed) {
    state_[0] = seed;
    for (std::size_t index = 1; index < kStateSize; ++index) {
        state_[index] =
            static_cast<std::uint32_t>(1812433253U * (state_[index - 1] ^ (state_[index - 1] >> 30)) +
                                       index);
    }
    index_ = kStateSize;
}

void MersenneRandom::init_by_array(const std::uint32_t* key, std::size_t length) {
    init_genrand(19650218U);
    std::size_t i = 1;
    std::size_t j = 0;
    std::size_t count = kStateSize > length ? kStateSize : length;
    for (; count != 0; --count) {
        state_[i] = (state_[i] ^ ((state_[i - 1] ^ (state_[i - 1] >> 30)) * 1664525U)) + key[j] +
                    static_cast<std::uint32_t>(j);
        ++i;
        ++j;
        if (i >= kStateSize) {
            state_[0] = state_[kStateSize - 1];
            i = 1;
        }
        if (j >= length) {
            j = 0;
        }
    }
    for (count = kStateSize - 1; count != 0; --count) {
        state_[i] =
            (state_[i] ^ ((state_[i - 1] ^ (state_[i - 1] >> 30)) * 1566083941U)) -
            static_cast<std::uint32_t>(i);
        ++i;
        if (i >= kStateSize) {
            state_[0] = state_[kStateSize - 1];
            i = 1;
        }
    }
    state_[0] = 0x80000000U;
    index_ = kStateSize;
}

std::uint32_t MersenneRandom::next_uint32() {
    if (index_ >= kStateSize) {
        for (std::size_t i = 0; i < kStateSize; ++i) {
            const std::uint32_t value =
                (state_[i] & kUpperMask) | (state_[(i + 1) % kStateSize] & kLowerMask);
            state_[i] = state_[(i + kPeriod) % kStateSize] ^ (value >> 1);
            if ((value & 1U) != 0U) {
                state_[i] ^= kMatrixA;
            }
        }
        index_ = 0;
    }
    std::uint32_t value = state_[index_++];
    value ^= value >> 11;
    value ^= (value << 7) & 0x9d2c5680U;
    value ^= (value << 15) & 0xefc60000U;
    value ^= value >> 18;
    return value;
}

std::uint32_t MersenneRandom::getrandbits(unsigned bits) {
    if (bits == 0) {
        return 0;
    }
    std::uint32_t value = next_uint32();
    if (bits < 32) {
        value >>= (32U - bits);
    }
    return value;
}

double MersenneRandom::random() {
    const std::uint32_t high = next_uint32() >> 5;
    const std::uint32_t low = next_uint32() >> 6;
    return (static_cast<double>(high) * 67108864.0 + static_cast<double>(low)) *
           (1.0 / 9007199254740992.0);
}

int MersenneRandom::randint(int low, int high) {
    const auto width = static_cast<std::uint32_t>(high - low) + 1U;
    if (width == 0) {
        return low;
    }
    unsigned bits = 0;
    for (std::uint32_t value = width; value != 0; value >>= 1) {
        ++bits;
    }
    std::uint32_t result = getrandbits(bits);
    while (result >= width) {
        result = getrandbits(bits);
    }
    return low + static_cast<int>(result);
}

}  // namespace timewarp::lang
