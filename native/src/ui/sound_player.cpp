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
    AudioSink sink;
};

constexpr Candidate kCandidates[] = {
    {"paplay", "", AudioSink::PathArgument},
    {"aplay", "-q", AudioSink::PathArgument},
    {"afplay", "", AudioSink::PathArgument},  // macOS's own player
    {"ffplay", "-nodisp -autoexit -loglevel quiet", AudioSink::PathArgument},
    {"play", "-q", AudioSink::PathArgument},
    // Windows has no equivalent on PATH, but it has PowerShell and a .NET class
    // that plays a WAV. `PlaySync` blocks, which is what the other players do
    // too, and killing the process is what stops it.
    {"powershell.exe", "-NoProfile -NonInteractive -WindowStyle Hidden -Command",
     AudioSink::PowerShell},
    {"powershell", "-NoProfile -NonInteractive -WindowStyle Hidden -Command",
     AudioSink::PowerShell},
};

/// The first player on the PATH, or an empty string.
[[nodiscard]] QString findPlayer(QString* arguments, AudioSink* sink) {
    for (const Candidate& candidate : kCandidates) {
        const QString path = QStandardPaths::findExecutable(
            QString::fromLatin1(candidate.program));
        if (path.isEmpty()) {
            continue;
        }
        if (arguments != nullptr) {
            *arguments = QString::fromLatin1(candidate.arguments);
        }
        if (sink != nullptr) {
            *sink = candidate.sink;
        }
        return path;
    }
    return {};
}

}  // namespace

SoundPlayer::SoundPlayer(QObject* parent) : QObject(parent) {
    QString arguments;
    program_ = findPlayer(&arguments, &sink_);
    arguments_ = arguments.split(QLatin1Char(' '), Qt::SkipEmptyParts);
    process_ = new QProcess{this};
    // The player exits on its own when the file ends. Its output is not the
    // program's output, so it goes nowhere rather than into the console.
    process_->setProcessChannelMode(QProcess::MergedChannels);
}

SoundPlayer::~SoundPlayer() { stop(); }

QString SoundPlayer::powerShellCommand(const QString& path) {
    // PowerShell's single-quoted string is literal apart from a doubled quote,
    // so `O'Brien` becomes `O''Brien` and the whole path stays one token.
    QString escaped = path;
    escaped.replace(QLatin1Char{'\''}, QStringLiteral("''"));
    return QStringLiteral("(New-Object Media.SoundPlayer '%1').PlaySync()").arg(escaped);
}

QByteArray SoundPlayer::wavFor(const std::vector<timewarp::audio::Tone>& tones) {
    return wavBytes(timewarp::audio::render(tones));
}

QByteArray SoundPlayer::wavBytes(const std::vector<std::int16_t>& pcm) {
    const std::string bytes = timewarp::audio::wav(pcm, timewarp::audio::kDefaultSampleRate);
    return QByteArray{bytes.data(), timewarp::core::intLength(bytes.size())};
}

void SoundPlayer::note(double frequency, int milliseconds,
                       timewarp::audio::Waveform waveform) {
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
    if (sink_ == AudioSink::PowerShell) {
        arguments << powerShellCommand(current_file_);
    } else {
        arguments << current_file_;
    }
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
