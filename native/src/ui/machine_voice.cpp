// SPDX-License-Identifier: MIT

#include "timewarp/ui/machine_voice.hpp"

#include <algorithm>
#include <cstddef>

#include "timewarp/ui/sound_player.hpp"

namespace timewarp::ui {

namespace {

using timewarp::audio::Tone;
using timewarp::audio::Waveform;

Tone note(double frequency, int milliseconds, double volume, Waveform waveform) {
    Tone tone;
    tone.frequency = frequency;
    tone.milliseconds = milliseconds;
    tone.volume = volume;
    tone.waveform = waveform;
    return tone;
}

}  // namespace

MachineVoice::MachineVoice(QObject* parent) : QObject(parent) {}

void MachineVoice::setVolume(int percent) { volume_ = std::clamp(percent, 0, 100); }

std::vector<Tone> MachineVoice::tonesFor(Event event) {
    switch (event) {
        case Event::Boot:
            // A rising two-note chirp: the sound a machine makes when the tube warms
            // up, which is a sound nobody who owned one has forgotten.
            return {note(523.25, 70, 0.22, Waveform::Square),
                    note(1046.5, 110, 0.18, Waveform::Square)};
        case Event::Key:
            // One short click. A whole keystroke is too long to sit under typing.
            return {note(1600.0, 12, 0.10, Waveform::Square)};
        case Event::Disk:
            // A head step: two short noise bursts, which is a rattle rather than a
            // note, and a note would sound like the machine was talking.
            return {note(0.0, 18, 0.0, Waveform::Noise),
                    note(220.0, 26, 0.12, Waveform::Noise),
                    note(180.0, 30, 0.10, Waveform::Noise)};
        case Event::Error:
            // The buzz, and it has to be unpleasant: a low square held long enough to
            // read the message over.
            return {note(110.0, 180, 0.20, Waveform::Square)};
        case Event::Tape:
            // The warble of a tape motor: a rising then falling tone over a hiss.
            return {note(480.0, 60, 0.10, Waveform::Noise),
                    note(720.0, 90, 0.12, Waveform::Triangle),
                    note(540.0, 90, 0.10, Waveform::Triangle)};
    }
    return {};
}

Tone MachineVoice::toneForCode(int code) {
    // The sound latch is one byte, so its meaning has to be one line of arithmetic:
    // 100 Hz for a zero, eight more for each step, and 80 ms. It is enough for a
    // program to play a scale by poking fifteen values, which is what a machine with
    // a one-register beeper could actually do.
    const int value = std::clamp(code, 0, 255);
    return note(100.0 + value * 8.0, 80, 0.20, Waveform::Square);
}

void MachineVoice::play(Event event) {
    ++requested_;
    ++counts_[static_cast<std::size_t>(event)];
    if (muted_) {
        return;
    }
    emit_tones(tonesFor(event));
}

void MachineVoice::tone(int code) {
    ++requested_;
    if (muted_ || code == 0) {
        return;
    }
    emit_tones({toneForCode(code)});
}

int MachineVoice::count(Event event) const {
    return counts_[static_cast<std::size_t>(event)];
}

void MachineVoice::resetCounts() {
    counts_.fill(0);
    sounded_ = 0;
    requested_ = 0;
}

void MachineVoice::emit_tones(const std::vector<Tone>& tones) {
    if (tones.empty()) {
        return;
    }
    if (player_ == nullptr || volume_ == 0) {
        // Counted as requested, not as sounded: nothing was emitted, and a reader of
        // `sounded()` should be able to tell that from a mute.
        return;
    }
    std::vector<Tone> scaled = tones;
    const double gain = static_cast<double>(volume_) / 100.0;
    for (Tone& tone : scaled) {
        tone.volume = tone.volume * gain;
    }
    player_->play(scaled);
    ++sounded_;
}

}  // namespace timewarp::ui
