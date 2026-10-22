// SPDX-License-Identifier: MIT
//
// Several voices at once.
//
// `render()` plays a note list one note after another, which is what `PLAY` is:
// a tune. A game needs the other thing -- a sound effect over a background
// tone -- and that is a mixer: voices that start when they are asked to and are
// summed, sample by sample, until each runs out.
//
// #### Why the mix is a pure function
//
// The engine has no device, and a test machine has no speaker. `render()` turns
// mixer state into samples and nothing else, so "two voices are audible
// together" is an assertion about a buffer rather than something a person has to
// listen for. The device stays where it was: `ui/sound_player.hpp` hands the
// buffer to an external player.
//
// #### What it deliberately is not
//
// No filters, no pan, no per-voice sample rate, no mixing of programs -- one run
// owns the mixer. A voice is a waveform, a frequency and an envelope, which is
// the whole vocabulary these machines had.

#pragma once

#include <cstdint>
#include <vector>

#include "timewarp/audio/music.hpp"

namespace timewarp::audio {

/// A handful of voices: a chord, a laser over a beat, and room to spare. Eight
/// is more than the square-wave hardware this emulates could do, and small
/// enough that summing them is cheaper than any bookkeeping that would avoid it.
inline constexpr int kMaxVoices = 8;

/// One sounding voice. Public so a test can read what the mixer is doing without
/// the mixer having to describe itself in prose.
struct Voice {
    Waveform waveform = Waveform::Square;
    double frequency = 440.0;
    double amplitude = 0.25;
    int total_samples = 0;
    int position = 0;
    std::uint32_t noise_state = 0x1234567u;

    [[nodiscard]] bool active() const noexcept { return position < total_samples; }
    [[nodiscard]] int remaining() const noexcept { return total_samples - position; }
};

/// Adds voices and renders them together.
class Mixer {
public:
    /// Starts `tone` as a voice. A tone that has already finished, or a rest, is
    /// accepted and adds nothing -- a program that asks for silence gets it.
    /// Returns false when every voice is busy; the caller may treat that as
    /// "the sound was dropped", which is what the hardware would have done.
    bool add(const Tone& tone);
    /// Starts a note at `frequency` with a default envelope.
    bool note(double frequency, int milliseconds, Waveform waveform = Waveform::Square);

    /// Sums the active voices into `out`, `frames` samples. Voices that finish
    /// part-way through the block are silent for the rest of it, which is what
    /// makes a short effect under a long tone work.
    void render(std::int16_t* out, int frames);

    /// Everything that is still sounding, to the end of the longest voice, with
    /// a moment of silence at the end. `limit_ms` bounds it so a caller cannot
    /// ask for a ten-minute file by accident.
    [[nodiscard]] std::vector<std::int16_t> render_all(int limit_ms = 10000);

    [[nodiscard]] int active_voices() const noexcept;
    [[nodiscard]] int voice_limit() const noexcept { return kMaxVoices; }
    /// Drops every voice. `SOUND OFF`.
    void clear() noexcept { voices_.clear(); }

    [[nodiscard]] const std::vector<Voice>& voices() const noexcept { return voices_; }

    /// The sample rate the mixer renders at, which is the synth's own.
    [[nodiscard]] static constexpr int sample_rate() noexcept { return kDefaultSampleRate; }

private:
    std::vector<Voice> voices_;
};

}  // namespace timewarp::audio
