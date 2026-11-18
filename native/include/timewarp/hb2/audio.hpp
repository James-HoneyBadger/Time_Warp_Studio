// SPDX-License-Identifier: MIT
//
// The HB-2's sound: four channels of 8-bit PCM at 22 050 Hz, and the 1982 latch as
// channel zero.
//
// The 1982 machine had one latch and five noises the hardware made on its own; a
// program could ask for a sound but could not make one. This chip is the other way
// round. Four channels, each with a waveform, a period, a volume and a three-stage
// envelope, mixed on the frame tick. A 32-slot waveform bank lives in the chip's own
// memory, so a program that `POKE`s it rewrites the instrument -- which is what a
// digi-drum was.
//
// The five machine sounds stay outside the mixer. They belong to the hardware, not
// to a program: the boot chime is the machine saying something, and no channel can
// silence it. The old latch is channel zero's trigger, so every existing `SOUND`
// still sounds.
//
// Mixing is deterministic: same registers, same number of frames, same bytes. There
// is no host clock in here, which is what lets a test assert a chord sample by
// sample (rule 3 of the machine's design rules).

#pragma once

#include <array>
#include <cstdint>
#include <vector>

#include "timewarp/hb2/device.hpp"

namespace timewarp::hb2 {

class Audio final : public Device {
public:
    static constexpr int kSampleRate = 22050;
    static constexpr int kChannels = 4;
    static constexpr int kWaveformCount = 32;
    static constexpr int kWaveformLength = 32;

    /// Registers, as offsets from the channel's own base.
    enum ChannelRegister : std::uint32_t {
        kEnable = 0x00,
        kWaveform = 0x01,
        kPeriodLow = 0x02,
        kPeriodHigh = 0x03,
        kVolume = 0x04,
        kAttack = 0x05,
        kDecay = 0x06,
        kSustain = 0x07,
        kChannelStride = 0x08,
    };

    static constexpr std::uint32_t kMasterVolume = 0x40;
    static constexpr std::uint32_t kLatch = 0x41;
    static constexpr std::uint32_t kWaveformBase = 0x100;

    Audio();

    // -- Device ------------------------------------------------------------
    [[nodiscard]] std::uint8_t read(std::uint32_t offset) const override;
    void write(std::uint32_t offset, std::uint8_t value) override;
    void tick() override;
    void reset() override;

    // -- The mixer ---------------------------------------------------------
    /// `frames` samples per channel, mixed and returned as bytes. Silence is 128,
    /// which is what an unsigned 8-bit sample centres on.
    [[nodiscard]] std::vector<std::uint8_t> mix(int frames);
    /// One frame of samples: 22 050 / 60 is 367.5, so odd frames take 368 samples
    /// and even ones 367 and a long run does not drift.
    [[nodiscard]] static int samplesPerFrame(std::uint64_t frame);

    /// Replaces a waveform slot, which is what a `POKE` into the bank does.
    void loadWaveform(int slot, const std::vector<std::uint8_t>& samples);
    [[nodiscard]] std::vector<std::uint8_t> waveform(int slot) const;

    /// Triggers a channel from the host or from the 1982 latch: envelope restarts,
    /// phase resets.
    void trigger(int channel, int waveform, int period, std::uint8_t volume);

    /// Reads the 1982 sound latch, clearing it, as the tube used to.
    [[nodiscard]] std::uint8_t takeLatch();

    /// Registers and the waveform bank, for the machine's snapshot.
    [[nodiscard]] std::vector<std::uint8_t> state() const;
    void setState(const std::vector<std::uint8_t>& bytes);

private:
    [[nodiscard]] std::uint8_t envelope(int channel) const;
    [[nodiscard]] std::uint8_t sampleFor(int channel) const;

    std::array<std::uint8_t, 0x1000> registers_{};
    std::array<std::vector<std::uint8_t>, kWaveformCount> waves_;
    std::array<std::uint32_t, kChannels> phase_{};
    std::array<std::uint64_t, kChannels> elapsed_{};
    std::uint64_t frames_ = 0;
};

}  // namespace timewarp::hb2
