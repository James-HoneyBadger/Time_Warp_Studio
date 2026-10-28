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
// #### Mixing rather than replacing
//
// `SOUND` joins the mix instead of stopping it: a game wants its laser over its
// engine, and a second tone while the first still plays is exactly what a game
// does. `timewarp::audio::Mixer` sums the voices and this class renders the mix
// to one file, so there is still one player process -- the thing that changed is
// that the file now holds everything that is sounding. A `PLAY` note list is
// still a tune: it stops the mix and plays its notes in order, because that is
// what a note list means.

#pragma once

#include <QByteArray>
#include <QObject>
#include <QString>
#include <QStringList>

#include <vector>

#include "timewarp/audio/mixer.hpp"
#include "timewarp/audio/music.hpp"

class QProcess;
class QTemporaryDir;

namespace timewarp::ui {

/// How a player is told which file to play. Public because the member that
/// records it has to be typed, and a bare int for two values is the kind of
/// thing that becomes a third by accident.
enum class AudioSink {
    PathArgument,  ///< the path is appended as the last argument
    PowerShell,    ///< the path goes inside a script (Windows)
};

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

    /// What the current run has sounded, in the order it asked for it. Recorded
    /// whether or not a player was found, because the export is a file rather than a
    /// speaker: a machine with no audio can still keep what its program played.
    [[nodiscard]] const std::vector<timewarp::audio::Tone>& recorded() const noexcept {
        return recorded_;
    }
    /// Forgets what has sounded so far, which is what the window does when a run
    /// starts: an export is this run's audio, not every note since launch.
    void beginRecording() noexcept { recorded_.clear(); }
    /// Everything recorded so far as one WAV file, for File > Export Audio.
    [[nodiscard]] QByteArray recordedWav() const;

    /// The WAV bytes for `tones`, exposed so a test can check the file without a
    /// speaker.
    [[nodiscard]] static QByteArray wavFor(const std::vector<timewarp::audio::Tone>& tones);
    /// The WAV bytes for samples already mixed. Public for the same reason.
    [[nodiscard]] static QByteArray wavBytes(const std::vector<std::int16_t>& pcm);
    /// The script `powershell.exe -Command` plays `path` with. Public because
    /// the quoting is the part that breaks on a path with an apostrophe in it
    /// (`C:\Users\O'Brien\...`), and a test on Linux can still check that.
    [[nodiscard]] static QString powerShellCommand(const QString& path);

private:
    void start(const QByteArray& wav);

    QString program_;
    QStringList arguments_;
    AudioSink sink_ = AudioSink::PathArgument;
    QProcess* process_ = nullptr;
    /// What is sounding, which is more than the file being played: a new note
    /// is summed with the voices already in it.
    timewarp::audio::Mixer mixer_;
    /// Every tone this run asked for, so it can be written out as a WAV.
    std::vector<timewarp::audio::Tone> recorded_;
    /// The file the current process is playing. Kept alive until the process
    /// exits: `aplay` reads it lazily, so deleting it on return plays silence.
    QString current_file_;
};

}  // namespace timewarp::ui
