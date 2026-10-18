// SPDX-License-Identifier: MIT
//
// Playing the synth's output through whatever the machine has.
//
// The engine synthesises PCM (`timewarp/audio/music.hpp`) and knows nothing about
// speakers. This turns PCM into sound with no library dependency: it writes a WAV
// to a temporary file and hands it to the first player it finds -- `paplay`
// (PulseAudio), `aplay` (ALSA), `ffplay` (ffmpeg) or `play` (SoX). Every desktop
// Linux distribution has at least one of those, they all accept a file path, and
// none of them has to be linked into the build.
//
// #### Why a process rather than QAudioSink
//
// `QAudioSink` needs Qt Multimedia, which is a whole component this build does
// not otherwise ask for, and a device-open failure there is silent and
// platform-specific. A missing player here is *known*: `available()` says so and
// the language reports it, which is the honest version of "the machine has no
// sound".
//
// #### One sound at a time
//
// A starting sound stops the previous one. `SOUND` in BASIC is synchronous on the
// original hardware and note lists are the musical case; mixing two programs'
// tones would need a mixer thread for no educational gain, and a second tone
// while the first still plays is almost always a mistake in the program.

#pragma once

#include <QByteArray>
#include <QObject>
#include <QString>
#include <QStringList>

#include <vector>

#include "timewarp/audio/music.hpp"

class QProcess;
class QTemporaryDir;

namespace timewarp::ui {

class SoundPlayer final : public QObject {
    Q_OBJECT

public:
    explicit SoundPlayer(QObject* parent = nullptr);
    ~SoundPlayer() override;

    /// Whether a player program was found. False means tones are accepted and
    /// dropped, which is what `audio_available()` reports to the language.
    [[nodiscard]] bool available() const noexcept { return !program_.isEmpty(); }
    /// The player's name, for the status line and the About box.
    [[nodiscard]] QString backend() const { return program_; }

    /// Renders `tones` and plays them, stopping whatever was playing.
    void play(const std::vector<timewarp::audio::Tone>& tones);
    /// A single note, which is what `SOUND` and `BEEP` are.
    void note(double frequency, int milliseconds,
              timewarp::audio::Waveform waveform = timewarp::audio::Waveform::Square);
    /// Silence. Safe when nothing is playing.
    void stop();

    /// The WAV bytes for `tones`, exposed so a test can check the file without a
    /// speaker.
    [[nodiscard]] static QByteArray wavFor(const std::vector<timewarp::audio::Tone>& tones);

private:
    void start(const QByteArray& wav);

    QString program_;
    QStringList arguments_;
    QProcess* process_ = nullptr;
    /// The file the current process is playing. Kept alive until the process
    /// exits: `aplay` reads it lazily, so deleting it on return plays silence.
    QString current_file_;
};

}  // namespace timewarp::ui
