// SPDX-License-Identifier: MIT
//
// The mixer. See `mixer.hpp` for why the mix is a pure function.

#include "timewarp/audio/mixer.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>

namespace timewarp::audio {
namespace {

constexpr double kPi = 3.14159265358979323846;

/// Deterministic noise, the same generator the sequential renderer uses: the
/// same mix has to produce the same bytes or the test that compares two runs is
/// measuring nothing.
[[nodiscard]] double next_noise(std::uint32_t* state) {
    *state ^= *state << 13U;
    *state ^= *state >> 17U;
    *state ^= *state << 5U;
    return static_cast<double>(*state & 0xFFFFu) / 32767.5 - 1.0;
}

/// The envelope a voice is shaped by at `position` of `total`.
///
/// A few milliseconds of attack and release: without them every note starts and
/// stops with a click, which on a square wave is louder than the note.
[[nodiscard]] double envelope_at(int position, int total, int ramp) {
    if (position < ramp) {
        return static_cast<double>(position) / static_cast<double>(ramp);
    }
    if (total - position <= ramp) {
        return static_cast<double>(total - position) / static_cast<double>(ramp);
    }
    return 1.0;
}

}  // namespace

bool Mixer::add(const Tone& tone) {
    if (tone.frequency <= 0.0 || tone.milliseconds <= 0) {
        return true;  // a rest: accepted, and it sounds like nothing
    }
    if (active_voices() >= kMaxVoices) {
        return false;
    }
    Voice voice;
    voice.waveform = tone.waveform;
    voice.frequency = tone.frequency;
    voice.amplitude = std::clamp(tone.volume, 0.0, 1.0);
    voice.total_samples = std::max(1, tone.milliseconds * sample_rate() / 1000);
    // Noise is deterministic per voice, seeded apart so two noise voices are not
    // the same noise twice.
    voice.noise_state = 0x1234567u + static_cast<std::uint32_t>(voices_.size()) * 7919u;
    voices_.push_back(voice);
    return true;
}

bool Mixer::note(double frequency, int milliseconds, Waveform waveform) {
    Tone tone;
    tone.waveform = waveform;
    tone.frequency = frequency;
    tone.milliseconds = milliseconds;
    return add(tone);
}

void Mixer::render(std::int16_t* out, int frames) {
    if (out == nullptr || frames <= 0) {
        return;
    }
    for (int frame = 0; frame < frames; ++frame) {
        double mixed = 0.0;
        for (Voice& voice : voices_) {
            if (!voice.active()) {
                continue;
            }
            const int ramp = std::min(voice.total_samples / 4,
                                      std::max(1, sample_rate() * 3 / 1000));
            const double step = 2.0 * kPi * voice.frequency /
                                static_cast<double>(sample_rate());
            const double phase = static_cast<double>(voice.position) * step;

            double value = 0.0;
            switch (voice.waveform) {
                case Waveform::Square:
                    value = std::sin(phase) >= 0.0 ? 1.0 : -1.0;
                    break;
                case Waveform::Triangle:
                    value = (2.0 / kPi) * std::asin(std::sin(phase));
                    break;
                case Waveform::Sine:
                    value = std::sin(phase);
                    break;
                case Waveform::Noise:
                    value = next_noise(&voice.noise_state);
                    break;
            }
            mixed += value * envelope_at(voice.position, voice.total_samples, ramp) *
                     voice.amplitude;
            ++voice.position;
        }
        // Sum first, clip once: two voices at half volume are louder than one,
        // but not as loud as the sum of their peaks.
        const double scaled = mixed * 32767.0;
        out[frame] = static_cast<std::int16_t>(std::clamp(scaled, -32768.0, 32767.0));
    }
    // Finished voices are dropped in one pass rather than during the loop, so
    // the iteration above does not invalidate itself.
    voices_.erase(std::remove_if(voices_.begin(), voices_.end(),
                                 [](const Voice& voice) { return !voice.active(); }),
                  voices_.end());
}

std::vector<std::int16_t> Mixer::render_all(int limit_ms) {
    if (active_voices() == 0) {
        return {};
    }
    const int limit_frames = std::max(0, limit_ms) * sample_rate() / 1000;
    int longest = 0;
    for (const Voice& voice : voices_) {
        longest = std::max(longest, voice.total_samples);
    }
    longest = std::min(longest, limit_frames);
    std::vector<std::int16_t> pcm(static_cast<std::size_t>(longest), 0);
    render(pcm.data(), longest);
    // A moment of silence at the end, so a player that stops on the last sample
    // does not clip the release.
    pcm.insert(pcm.end(), static_cast<std::size_t>(sample_rate()) / 40, 0);
    return pcm;
}

int Mixer::active_voices() const noexcept {
    return static_cast<int>(std::count_if(voices_.begin(), voices_.end(),
                                          [](const Voice& voice) { return voice.active(); }));
}

}  // namespace timewarp::audio
