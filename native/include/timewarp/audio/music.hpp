// SPDX-License-Identifier: MIT
//
// Sound: a small synthesiser, `PLAY`'s notation, and a WAV file.
//
// The removed implementation's media commands were acknowledge-only: they
// printed "🔊 SOUND 440Hz for 55ms" and played nothing, because the Python build
// made audio optional behind `pygame`. That is honest but useless -- a game with
// no sound is a game with half its feedback -- so the native build makes the
// sound itself.
//
// #### Why synthesise rather than ship samples
//
// These are eight-bit-era machines. `SOUND 440, 18` on a BBC Micro or a PC
// speaker is a **square wave**, not a recording, and `PLAY "CDEFGAB"` is a note
// list, not an audio file. Synthesising is what the original hardware did, it
// needs no assets, and it keeps the whole feature in about three hundred lines
// of arithmetic that a test can compare byte for byte.
//
// #### Layering
//
// This file is Qt-free and runs headless, like the rest of the engine. Turning
// PCM into noise out of a speaker is a platform job: `ui/sound_player.hpp` runs
// an external player (`paplay`, `aplay`, `ffplay` or `play`), and a machine with
// none of them keeps the note parsing and the acknowledgement without the sound.
//
// #### What the notation supports
//
// The `PLAY` mini-language as BASIC defines it: note names with `#`/`b`,
// octaves (`O3`, `>`, `<`), lengths (`L4`, `C4.`), tempo (`T120`), rests (`P`,
// `R`) and the numeric form `N60`. Articulation words (`MN`, `ML`, `MS`) are
// accepted and treated as normal: they change how notes join, which this
// synthesiser does not model.

#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace timewarp::audio {

/// The synth's rate. 22.05kHz is the period-correct choice and a quarter of the
/// memory of 44.1kHz for the same tune.
inline constexpr int kDefaultSampleRate = 22050;

/// How long one `SOUND` tick is, in milliseconds: BASIC's duration unit is
/// 1/18.2 of a second, which is the IBM PC's timer rate.
inline constexpr double kTickMilliseconds = 1000.0 / 18.2;

/// The shapes this synth can produce. Square is the period-correct one; the
/// others exist because a triangle is far kinder over a laptop speaker.
enum class Waveform {
    Square,
    Triangle,
    Sine,
    Noise,
};

/// One note or rest.
struct Tone {
    Waveform waveform = Waveform::Square;
    /// Hertz. Zero is a rest.
    double frequency = 440.0;
    int milliseconds = 200;
    /// 0..1. Clamped when rendering.
    double volume = 0.25;
};

/// The outcome of parsing, in the shape the executors already use for errors:
/// a bool and a sentence.
struct ParseResult {
    bool ok = true;
    std::string message;
};

/// `SOUND frequency, duration` as a square wave. `frequency` of 0 is a rest, and
/// a duration of 0 is treated as one tick rather than as an instant click.
[[nodiscard]] Tone tone_from_sound(long frequency, int milliseconds);

/// `PLAY "T120 O4 L8 CDEFGAB"` to a list of tones.
///
/// On a malformed string the tones parsed so far are returned and `error`
/// describes the problem: a learner who mistypes one note should hear the rest of
/// the tune rather than nothing.
[[nodiscard]] std::vector<Tone> parse_play(const std::string& notation,
                                           ParseResult* error);

/// 16-bit mono PCM. Tones play in sequence, as `PLAY`'s do.
[[nodiscard]] std::vector<std::int16_t> render(const std::vector<Tone>& tones,
                                               int sample_rate = kDefaultSampleRate);

/// A complete RIFF/WAVE file containing `pcm`.
[[nodiscard]] std::string wav(const std::vector<std::int16_t>& pcm, int sample_rate);

}  // namespace timewarp::audio
