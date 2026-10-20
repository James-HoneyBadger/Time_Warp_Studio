// SPDX-License-Identifier: MIT

#include "timewarp/core/qt_compat.hpp"
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
    {"paplay", ""},
    {"aplay", "-q"},
    {"afplay", ""},  // macOS's own player; the three above are the Linux ones
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
    const std::vector<std::int16_t> pcm = timewarp::audio::render(tones);
    const std::string bytes = timewarp::audio::wav(pcm, timewarp::audio::kDefaultSampleRate);
    return QByteArray{bytes.data(), timewarp::core::intLength(bytes.size())};
}

void SoundPlayer::note(double frequency, int milliseconds,
                       timewarp::audio::Waveform waveform) {
    timewarp::audio::Tone tone;
    tone.frequency = frequency > 0.0 ? frequency : 0.0;
    tone.milliseconds = milliseconds > 0 ? milliseconds : 1;
    tone.waveform = waveform;
    play({tone});
}

void SoundPlayer::play(const std::vector<timewarp::audio::Tone>& tones) {
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
