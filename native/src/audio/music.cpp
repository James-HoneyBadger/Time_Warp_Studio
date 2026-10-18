// SPDX-License-Identifier: MIT

#include "timewarp/audio/music.hpp"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstddef>
#include <cstdlib>

namespace timewarp::audio {

namespace {

constexpr double kPi = 3.14159265358979323846;

/// The sample rate the default arguments use, declared before the functions that
/// need it as a default.
constexpr int kDefaultRate = kDefaultSampleRate;

[[nodiscard]] char upper(char character) {
    if (character >= 'a' && character <= 'z') {
        return static_cast<char>(character - 'a' + 'A');
    }
    return character;
}

[[nodiscard]] bool is_digit(char character) {
    return character >= '0' && character <= '9';
}

/// Reads an unsigned number starting at `index`, advancing it. Returns -1 when
/// there are no digits, which is how the caller tells "absent" from "zero".
[[nodiscard]] long read_number(const std::string& text, std::size_t* index) {
    if (*index >= text.size() || !is_digit(text[*index])) {
        return -1;
    }
    long value = 0;
    while (*index < text.size() && is_digit(text[*index])) {
        value = value * 10 + (text[*index] - '0');
        ++*index;
    }
    return value;
}

/// Semitones above C for a note letter, or -1.
[[nodiscard]] int semitone_of(char letter) {
    switch (upper(letter)) {
        case 'C':
            return 0;
        case 'D':
            return 2;
        case 'E':
            return 4;
        case 'F':
            return 5;
        case 'G':
            return 7;
        case 'A':
            return 9;
        case 'B':
            return 11;
        default:
            return -1;
    }
}

/// Equal temperament, A4 = 440Hz at MIDI 69.
[[nodiscard]] double frequency_of(int midi) {
    return 440.0 * std::pow(2.0, (static_cast<double>(midi) - 69.0) / 12.0);
}

/// One note's length in milliseconds: `60000 / tempo` is a quarter note, and
/// `length` is the denominator of the note's fraction, as `L8` is an eighth.
[[nodiscard]] int length_in_ms(long tempo, int length, bool dotted) {
    const double quarter = 60000.0 / static_cast<double>(std::max(1L, tempo));
    double milliseconds = quarter * 4.0 / static_cast<double>(std::max(1, length));
    if (dotted) {
        milliseconds *= 1.5;
    }
    return std::max(1, static_cast<int>(std::lround(milliseconds)));
}

/// Deterministic noise: the same tune renders to the same bytes, which is what
/// makes the WAV comparable in a test.
[[nodiscard]] double next_noise(std::uint32_t* state) {
    *state ^= *state << 13;
    *state ^= *state >> 17;
    *state ^= *state << 5;
    return (static_cast<double>(*state) / 2147483648.0) - 1.0;
}

void append_u32(std::string* out, std::uint32_t value) {
    out->push_back(static_cast<char>(value & 0xFF));
    out->push_back(static_cast<char>((value >> 8) & 0xFF));
    out->push_back(static_cast<char>((value >> 16) & 0xFF));
    out->push_back(static_cast<char>((value >> 24) & 0xFF));
}

void append_u16(std::string* out, std::uint16_t value) {
    out->push_back(static_cast<char>(value & 0xFF));
    out->push_back(static_cast<char>((value >> 8) & 0xFF));
}

}  // namespace

Tone tone_from_sound(long frequency, int milliseconds) {
    Tone tone;
    tone.waveform = Waveform::Square;
    tone.frequency = frequency > 0 ? static_cast<double>(frequency) : 0.0;
    // One tick minimum: `SOUND 440, 0` is a click the speaker cannot render, and
    // a zero-length tone is a division by zero in the mixer.
    tone.milliseconds = std::max(1, milliseconds);
    tone.volume = 0.22;
    return tone;
}

std::vector<Tone> parse_play(const std::string& notation, ParseResult* error) {
    ParseResult local;
    std::vector<Tone> tones;

    long tempo = 120;      // quarter notes per minute
    int length = 4;        // L4: a quarter note by default
    int octave = 4;        // O4: middle C is C4
    Waveform waveform = Waveform::Square;

    std::size_t index = 0;
    const auto fail = [&local](std::string message) {
        if (local.ok) {
            local.ok = false;
            local.message = std::move(message);
        }
    };

    while (index < notation.size()) {
        const char character = notation[index];
        if (character == ' ' || character == '\t' || character == ',' ||
            character == '\n' || character == '\r') {
            ++index;
            continue;
        }
        const char letter = upper(character);
        ++index;

        if (letter == 'T') {
            const long value = read_number(notation, &index);
            if (value <= 0 || value > 255) {
                fail("T needs a tempo between 1 and 255");
                continue;
            }
            tempo = value;
            continue;
        }
        if (letter == 'L') {
            const long value = read_number(notation, &index);
            if (value < 1 || value > 64) {
                fail("L needs a length of 1 to 64 (L4 is a quarter note)");
                continue;
            }
            length = static_cast<int>(value);
            continue;
        }
        if (letter == 'O') {
            const long value = read_number(notation, &index);
            if (value < 0 || value > 6) {
                fail("O needs an octave from 0 to 6");
                continue;
            }
            octave = static_cast<int>(value);
            continue;
        }
        if (letter == 'M') {
            // Articulation: MN normal, ML legato, MS staccato, MB/MF background
            // or foreground. Accepted and ignored -- see the header.
            if (index < notation.size() && std::isalpha(static_cast<unsigned char>(notation[index]))) {
                ++index;
            }
            continue;
        }
        if (letter == '>') {
            octave = std::min(6, octave + 1);
            continue;
        }
        if (letter == '<') {
            octave = std::max(0, octave - 1);
            continue;
        }
        if (letter == 'N') {
            const long value = read_number(notation, &index);
            if (value < 0 || value > 84) {
                fail("N needs a note number from 0 (rest) to 84");
                continue;
            }
            Tone tone;
            tone.waveform = waveform;
            // MBASIC's N counts from C0. Its 1 is C0, so the MIDI note is value+11.
            tone.frequency = value == 0 ? 0.0 : frequency_of(static_cast<int>(value) + 11);
            tone.milliseconds = length_in_ms(tempo, length, false);
            tones.push_back(tone);
            continue;
        }
        if (letter == 'P' || letter == 'R') {
            // A rest: same length rules, no pitch.
            bool dotted = false;
            if (index < notation.size() && notation[index] == '.') {
                dotted = true;
                ++index;
            }
            Tone rest;
            rest.waveform = waveform;
            rest.frequency = 0.0;
            rest.milliseconds = length_in_ms(tempo, length, dotted);
            tones.push_back(rest);
            continue;
        }
        if (const int semitone = semitone_of(letter); semitone >= 0) {
            int accidental = 0;
            if (index < notation.size()) {
                if (notation[index] == '#' || notation[index] == '+') {
                    accidental = 1;
                    ++index;
                } else if (notation[index] == '-') {
                    accidental = -1;
                    ++index;
                } else if (notation[index] == 'b') {
                    // A lowercase `b` is a flat, which is how every musician
                    // writes one. It cannot be confused with the note B, whose
                    // letter is uppercase by the time it is read.
                    accidental = -1;
                    ++index;
                }
            }
            int note_octave = octave;
            if (index < notation.size() && is_digit(notation[index])) {
                note_octave = notation[index] - '0';
                ++index;
            }
            int note_length = length;
            if (index < notation.size() && is_digit(notation[index])) {
                const long value = read_number(notation, &index);
                if (value >= 1 && value <= 64) {
                    note_length = static_cast<int>(value);
                }
            }
            bool dotted = false;
            if (index < notation.size() && notation[index] == '.') {
                dotted = true;
                ++index;
            }
            const int midi = (note_octave + 1) * 12 + semitone + accidental;
            Tone tone;
            tone.waveform = waveform;
            tone.frequency = midi <= 0 ? 0.0 : frequency_of(midi);
            tone.milliseconds = length_in_ms(tempo, note_length, dotted);
            tones.push_back(tone);
            continue;
        }

        fail(std::string{"unknown character '"} + character + "' in PLAY");
    }

    if (local.ok && tones.empty()) {
        local.ok = false;
        local.message = "PLAY needs at least one note";
    }
    if (error != nullptr) {
        *error = local;
    }
    return tones;
}

std::vector<std::int16_t> render(const std::vector<Tone>& tones, int sample_rate) {
    const int rate = sample_rate > 0 ? sample_rate : kDefaultRate;
    std::vector<std::int16_t> pcm;
    std::uint32_t noise_state = 0x1234567u;

    for (const Tone& tone : tones) {
        const int samples = std::max(1, tone.milliseconds * rate / 1000);
        const double volume = std::clamp(tone.volume, 0.0, 1.0);
        // A few milliseconds of attack and release. Without them every note
        // starts and stops with a click, which on a square wave is louder than
        // the note.
        const int ramp = std::min(samples / 4, std::max(1, rate * 3 / 1000));
        const double step = tone.frequency > 0.0
                                ? 2.0 * kPi * tone.frequency / static_cast<double>(rate)
                                : 0.0;
        double phase = 0.0;

        for (int sample = 0; sample < samples; ++sample) {
            double value = 0.0;
            if (tone.frequency > 0.0) {
                switch (tone.waveform) {
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
                        value = next_noise(&noise_state);
                        break;
                }
                phase += step;
                if (phase > 2.0 * kPi) {
                    phase -= 2.0 * kPi;
                }
            }
            double envelope = 1.0;
            if (sample < ramp) {
                envelope = static_cast<double>(sample) / static_cast<double>(ramp);
            } else if (samples - sample <= ramp) {
                envelope = static_cast<double>(samples - sample) / static_cast<double>(ramp);
            }
            const double scaled = value * envelope * volume * 32767.0;
            pcm.push_back(static_cast<std::int16_t>(std::clamp(scaled, -32768.0, 32767.0)));
        }
    }

    // A moment of silence at the end, so a player that stops on the last sample
    // does not clip the release.
    pcm.insert(pcm.end(), static_cast<std::size_t>(rate) / 40, 0);
    return pcm;
}

std::string wav(const std::vector<std::int16_t>& pcm, int sample_rate) {
    const int rate = sample_rate > 0 ? sample_rate : kDefaultRate;
    const std::uint32_t data_bytes =
        static_cast<std::uint32_t>(pcm.size() * sizeof(std::int16_t));

    std::string file;
    file.reserve(44 + data_bytes);
    file += "RIFF";
    append_u32(&file, 36 + data_bytes);
    file += "WAVE";
    file += "fmt ";
    append_u32(&file, 16);                 // PCM header size
    append_u16(&file, 1);                  // PCM
    append_u16(&file, 1);                  // mono
    append_u32(&file, static_cast<std::uint32_t>(rate));
    append_u32(&file, static_cast<std::uint32_t>(rate * 2));  // byte rate
    append_u16(&file, 2);                  // block align
    append_u16(&file, 16);                 // bits per sample
    file += "data";
    append_u32(&file, data_bytes);
    for (const std::int16_t sample : pcm) {
        append_u16(&file, static_cast<std::uint16_t>(sample));
    }
    return file;
}

}  // namespace timewarp::audio
