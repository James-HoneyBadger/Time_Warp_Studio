// SPDX-License-Identifier: MIT

#include "timewarp/ui/sound_player.hpp"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QDateTime>
#include <QProcess>
#include <QStandardPaths>
#include <QTemporaryDir>

namespace timewarp::ui {

namespace {

/// The players to try, with the arguments that make them quiet and exit when the
/// file ends. Order matters: the first one installed by the desktop is the one
/// with the working default device.
struct Candidate {
    const char* program;
    const char* arguments;
};

constexpr Candidate kCandidates[] = {
    // PipeWire first: it is what a current desktop runs, and `pw-play` talks to it
    // directly. `paplay` and `aplay` follow because both still exist, and a
    // distribution may have one of them and not the others.
    {"pw-play", ""},
    {"paplay", ""},
    {"aplay", "-q"},
    {"ffplay", "-nodisp -autoexit -loglevel quiet"},
    {"play", "-q"},
};

/// The first player on the PATH, or an empty string.
[[nodiscard]] QString findPlayer(QString* arguments) {
    for (const Candidate& candidate : kCandidates) {
        const QString path = QStandardPaths::findExecutable(
            QString::fromLatin1(candidate.program));
        if (path.isEmpty()) {
            continue;
        }
        if (arguments != nullptr) {
            *arguments = QString::fromLatin1(candidate.arguments);
        }
        return path;
    }
    return {};
}

}  // namespace

SoundPlayer::SoundPlayer(QObject* parent) : QObject(parent) {
    QString arguments;
    program_ = findPlayer(&arguments);
    arguments_ = arguments.split(QLatin1Char(' '), Qt::SkipEmptyParts);
    process_ = new QProcess{this};
    // The player exits on its own when the file ends. Its output is not the
    // program's output, so it goes nowhere rather than into the console.
    process_->setProcessChannelMode(QProcess::MergedChannels);
}

SoundPlayer::~SoundPlayer() { stop(); }

QByteArray SoundPlayer::wavFor(const std::vector<timewarp::audio::Tone>& tones) {
    return wavBytes(timewarp::audio::render(tones));
}

QByteArray SoundPlayer::recordedWav() const { return wavFor(recorded_); }

QByteArray SoundPlayer::wavBytes(const std::vector<std::int16_t>& pcm) {
    const std::string bytes = timewarp::audio::wav(pcm, timewarp::audio::kDefaultSampleRate);
    return QByteArray{bytes.data(), static_cast<int>(bytes.size())};
}

void SoundPlayer::note(double frequency, int milliseconds,
                       timewarp::audio::Waveform waveform) {
    // Recorded before the player check: the tick a program asked for is part of the
    // run whether or not this machine can hear it, and the export says so.
    if (frequency > 0.0) {
        recorded_.push_back(timewarp::audio::Tone{
            waveform, frequency, milliseconds > 0 ? milliseconds : 1});
    }
    if (!available() || frequency <= 0.0) {
        return;
    }
    // The note joins the mix. The file is re-rendered so that everything still
    // sounding is in it -- one process, several voices -- which is what makes a
    // short effect audible over a long tone.
    mixer_.note(frequency, milliseconds > 0 ? milliseconds : 1, waveform);
    start(wavBytes(mixer_.render_all()));
}

void SoundPlayer::play(const std::vector<timewarp::audio::Tone>& tones) {
    recorded_.insert(recorded_.end(), tones.begin(), tones.end());
    stop();
    if (!available() || tones.empty()) {
        return;
    }
    start(wavFor(tones));
}

void SoundPlayer::start(const QByteArray& wav) {
    // A directory rather than one fixed path, so two windows (or a test and the
    // IDE) cannot overwrite each other's tune mid-playback.
    static QTemporaryDir directory;
    if (!directory.isValid()) {
        return;
    }
    current_file_ = directory.filePath(QStringLiteral("tone-%1.wav")
                                           .arg(QDateTime::currentMSecsSinceEpoch()));
    QFile file{current_file_};
    if (!file.open(QIODevice::WriteOnly)) {
        current_file_.clear();
        return;
    }
    file.write(wav);
    file.close();

    QStringList arguments = arguments_;
    arguments << current_file_;
    process_->start(program_, arguments);
}

void SoundPlayer::stop() {
    // The mix is part of what is playing, so silencing the machine clears it:
    // the next note must not arrive on top of a voice nobody can hear.
    mixer_.clear();
    if (process_ != nullptr && process_->state() != QProcess::NotRunning) {
        // `kill` rather than `terminate`: a player told to stop politely may
        // finish the buffer, which is exactly the sound being asked to go away.
        process_->kill();
        process_->waitForFinished(200);
    }
    if (!current_file_.isEmpty()) {
        QFile::remove(current_file_);
        current_file_.clear();
    }
}

}  // namespace timewarp::ui
