// SPDX-License-Identifier: MIT

#include "timewarp/hb2/audio.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>

namespace timewarp::hb2 {

namespace {

/// The shapes the machine ships with. Slot 0 is the square a 1982 latch produced,
/// so the compat path sounds like the machine it came from.
std::vector<std::uint8_t> builtinWaveform(int slot) {
    std::vector<std::uint8_t> samples(Audio::kWaveformLength, 128);
    const auto at = [](int index) { return static_cast<std::size_t>(index); };
    switch (slot % 5) {
    case 0:  // square
        for (int index = 0; index < Audio::kWaveformLength; ++index) {
            samples[at(index)] = index < Audio::kWaveformLength / 2 ? 192 : 64;
        }
        break;
    case 1:  // sine
        for (int index = 0; index < Audio::kWaveformLength; ++index) {
            const double angle = (2.0 * 3.14159265358979323846 * index) / Audio::kWaveformLength;
            samples[at(index)] = static_cast<std::uint8_t>(128.0 + 63.0 * std::sin(angle));
        }
        break;
    case 2:  // saw
        for (int index = 0; index < Audio::kWaveformLength; ++index) {
            samples[at(index)] = static_cast<std::uint8_t>(64 + (index * 4));
        }
        break;
    case 3:  // triangle
        for (int index = 0; index < Audio::kWaveformLength; ++index) {
            const int half = index < Audio::kWaveformLength / 2 ? index : Audio::kWaveformLength - index;
            samples[at(index)] = static_cast<std::uint8_t>(128 + (half - 8) * 8);
        }
        break;
    default:  // noise, from a fixed seed so two runs agree
        {
            std::uint32_t state = 0x1234ABCDU;
            for (int index = 0; index < Audio::kWaveformLength; ++index) {
                state = state * 1664525U + 1013904223U;
                samples[at(index)] = static_cast<std::uint8_t>((state >> 16) & 0xFF);
            }
        }
        break;
    }
    return samples;
}

}  // namespace

Audio::Audio() { reset(); }

void Audio::reset() {
    registers_.fill(0);
    for (int slot = 0; slot < kWaveformCount; ++slot) {
        waves_[static_cast<std::size_t>(slot)] = builtinWaveform(slot);
    }
    phase_.fill(0);
    elapsed_.fill(0);
    registers_[kMasterVolume] = 0xFF;
    frames_ = 0;
}

int Audio::samplesPerFrame(std::uint64_t frame) {
    // 22 050 = 60 * 367 + 30, so half the frames take one extra sample and the
    // long-run average is exact.
    return 367 + ((frame % 2 == 0) ? 1 : 0);
}

std::uint8_t Audio::envelope(int channel) const {
    const std::uint32_t base = static_cast<std::uint32_t>(channel) * kChannelStride;
    const std::uint8_t volume = registers_[base + kVolume];
    const std::uint64_t attack = registers_[base + kAttack];
    const std::uint64_t decay = registers_[base + kDecay];
    const std::uint8_t sustain = registers_[base + kSustain];
    const std::uint64_t age = elapsed_[static_cast<std::size_t>(channel)];
    std::uint32_t level = volume;
    if (attack > 0 && age < attack) {
        level = static_cast<std::uint32_t>((volume * age) / attack);
    } else if (decay > 0 && age < attack + decay) {
        const std::uint64_t into = age - attack;
        const std::uint32_t floor = static_cast<std::uint32_t>(sustain) * volume / 255;
        level = static_cast<std::uint32_t>(volume - ((volume - floor) * into) / decay);
    } else if (decay > 0) {
        level = static_cast<std::uint32_t>(sustain) * volume / 255;
    }
    return static_cast<std::uint8_t>(std::min<std::uint32_t>(level, 255));
}

std::uint8_t Audio::sampleFor(int channel) const {
    const std::uint32_t base = static_cast<std::uint32_t>(channel) * kChannelStride;
    const int slot = registers_[base + kWaveform] % kWaveformCount;
    const std::vector<std::uint8_t>& wave = waves_[static_cast<std::size_t>(slot)];
    if (wave.empty()) {
        return 128;
    }
    // A period of zero means "hold one sample", which is a DC level rather than a
    // division by zero; the chip answers that way rather than refusing the write.
    const std::uint32_t period = static_cast<std::uint32_t>(registers_[base + kPeriodLow]) |
                                 (static_cast<std::uint32_t>(registers_[base + kPeriodHigh]) << 8);
    const std::uint32_t step = period == 0 ? 0 : ((phase_[static_cast<std::size_t>(channel)] / period) %
                                                 static_cast<std::uint32_t>(wave.size()));
    return wave[step];
}

std::vector<std::uint8_t> Audio::mix(int frames) {
    std::vector<std::uint8_t> buffer;
    if (frames <= 0) {
        return buffer;
    }
    buffer.reserve(static_cast<std::size_t>(frames));
    const std::uint32_t master = registers_[kMasterVolume];
    for (int frame = 0; frame < frames; ++frame) {
        std::int32_t sum = 0;
        for (int channel = 0; channel < kChannels; ++channel) {
            const std::uint32_t base = static_cast<std::uint32_t>(channel) * kChannelStride;
            if ((registers_[base + kEnable] & 0x01U) == 0) {
                ++phase_[static_cast<std::size_t>(channel)];
                ++elapsed_[static_cast<std::size_t>(channel)];
                continue;
            }
            const std::int32_t sample = static_cast<std::int32_t>(sampleFor(channel)) - 128;
            const std::int32_t level = envelope(channel);
            sum += (sample * level) / 255;
            ++phase_[static_cast<std::size_t>(channel)];
            ++elapsed_[static_cast<std::size_t>(channel)];
        }
        sum = (sum * static_cast<std::int32_t>(master)) / 255;
        // Four channels at full scale would be four times the range, so the sum is
        // divided before it is clamped: a chord is loud, not distorted.
        sum = std::clamp<std::int32_t>(sum / kChannels, -128, 127);
        buffer.push_back(static_cast<std::uint8_t>(sum + 128));
    }
    return buffer;
}

void Audio::loadWaveform(int slot, const std::vector<std::uint8_t>& samples) {
    if (slot < 0 || slot >= kWaveformCount) {
        return;
    }
    std::vector<std::uint8_t>& wave = waves_[static_cast<std::size_t>(slot)];
    wave.assign(kWaveformLength, 128);
    for (std::size_t index = 0; index < samples.size() && index < wave.size(); ++index) {
        wave[index] = samples[index];
    }
}

std::vector<std::uint8_t> Audio::waveform(int slot) const {
    if (slot < 0 || slot >= kWaveformCount) {
        return {};
    }
    return waves_[static_cast<std::size_t>(slot)];
}

void Audio::trigger(int channel, int waveform, int period, std::uint8_t volume) {
    if (channel < 0 || channel >= kChannels) {
        return;
    }
    const std::uint32_t base = static_cast<std::uint32_t>(channel) * kChannelStride;
    registers_[base + kEnable] = 0x01;
    registers_[base + kWaveform] = static_cast<std::uint8_t>(waveform % kWaveformCount);
    registers_[base + kPeriodLow] = static_cast<std::uint8_t>(period & 0xFF);
    registers_[base + kPeriodHigh] = static_cast<std::uint8_t>((period >> 8) & 0xFF);
    registers_[base + kVolume] = volume;
    registers_[base + kSustain] = volume;
    phase_[static_cast<std::size_t>(channel)] = 0;
    elapsed_[static_cast<std::size_t>(channel)] = 0;
}

std::uint8_t Audio::takeLatch() {
    const std::uint8_t note = registers_[kLatch];
    registers_[kLatch] = 0;
    return note;
}

std::vector<std::uint8_t> Audio::state() const {
    std::vector<std::uint8_t> bytes(0x1000, 0);
    for (std::uint32_t offset = 0; offset < bytes.size(); ++offset) {
        bytes[offset] = read(offset);
    }
    return bytes;
}

void Audio::setState(const std::vector<std::uint8_t>& bytes) {
    for (std::uint32_t offset = 0; offset < bytes.size() && offset < 0x1000; ++offset) {
        write(offset, bytes[offset]);
    }
}

std::uint8_t Audio::read(std::uint32_t offset) const {
    if (offset >= kWaveformBase &&
        offset < kWaveformBase + static_cast<std::uint32_t>(kWaveformCount * kWaveformLength)) {
        const std::uint32_t at = offset - kWaveformBase;
        const int slot = static_cast<int>(at / kWaveformLength);
        const int index = static_cast<int>(at % kWaveformLength);
        return waves_[static_cast<std::size_t>(slot)][static_cast<std::size_t>(index)];
    }
    return offset < registers_.size() ? registers_[offset] : std::uint8_t{0};
}

void Audio::write(std::uint32_t offset, std::uint8_t value) {
    // The bank is checked first: it sits above the channel registers in the same
    // page, and a `POKE` into it rewrites the instrument rather than a register.
    if (offset >= kWaveformBase &&
        offset < kWaveformBase + static_cast<std::uint32_t>(kWaveformCount * kWaveformLength)) {
        const std::uint32_t at = offset - kWaveformBase;
        const int slot = static_cast<int>(at / kWaveformLength);
        const int index = static_cast<int>(at % kWaveformLength);
        waves_[static_cast<std::size_t>(slot)][static_cast<std::size_t>(index)] = value;
        return;
    }
    if (offset >= registers_.size()) {
        return;
    }
    if (offset == kLatch && value != 0) {
        // The 1982 latch, as channel zero: every existing `SOUND` still sounds.
        const int period = 256 / std::max<int>(1, value);
        trigger(0, 0, std::max(1, period), 0xC0);
        registers_[kLatch] = value;
        return;
    }
    registers_[offset] = value;
    if (offset % kChannelStride == kEnable && (value & 0x01U) != 0) {
        const int channel = static_cast<int>(offset / kChannelStride);
        if (channel >= 0 && channel < kChannels) {
            phase_[static_cast<std::size_t>(channel)] = 0;
            elapsed_[static_cast<std::size_t>(channel)] = 0;
        }
    }
}

void Audio::tick() {
    // With no sink attached the mixer still advances, so an envelope that should
    // have decayed has: a test that mixes later gets the same bytes a machine with
    // a speaker would have played.
    const std::vector<std::uint8_t> discarded = mix(samplesPerFrame(frames_));
    static_cast<void>(discarded);
    ++frames_;
}

}  // namespace timewarp::hb2
