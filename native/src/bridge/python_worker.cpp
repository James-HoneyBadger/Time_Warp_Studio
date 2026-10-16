// SPDX-License-Identifier: MIT

#include "timewarp/bridge/python_worker.hpp"

#include <algorithm>
#include <array>
#include <optional>
#include <vector>

#include <QCoreApplication>
#include <QDir>
#include <QElapsedTimer>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonValue>
#include <QProcess>
#include <QProcessEnvironment>
#include <QStandardPaths>
#include <QTimer>

#include "timewarp/canvas/command.hpp"
#include "timewarp/canvas/command_queue.hpp"

namespace timewarp::bridge {

using timewarp::canvas::CanvasCommand;
using timewarp::canvas::CanvasCommandKind;
using timewarp::canvas::make_command;
using timewarp::canvas::make_color;
using timewarp::canvas::make_rgb;

namespace {

/// How long the worker gets to exit after a cooperative stop before it is killed.
///
/// A statement that never returns -- `REPEAT FOREVER [...]`, a runaway recursion
/// -- cannot be interrupted from the outside at all, so this is not a fallback for
/// a rare case; it is the only stop that works for some programs.
constexpr int kStopGraceMilliseconds = 1500;

/// How long the `ready` handshake may take. A Python interpreter has to start and
/// import the package, which is not instant on a cold cache but is never minutes.
constexpr int kStartHandshakeMilliseconds = 20000;

/// Environment override for the interpreter, e.g. a virtual environment.
constexpr auto kPythonEnvironmentVariable = "TIME_WARP_PYTHON";
/// Environment override for the worker script.
constexpr auto kWorkerEnvironmentVariable = "TIME_WARP_WORKER";
/// The script's name, used for every candidate path.
constexpr auto kWorkerFileName = "worker.py";

[[nodiscard]] QString fromEnvironment(const char* name) {
    return QProcessEnvironment::systemEnvironment().value(QString::fromLatin1(name));
}

/// Directories worth looking in for the worker script, in order.
[[nodiscard]] QStringList scriptCandidates() {
    QStringList directories;
    const QString application = QCoreApplication::applicationDirPath();
    if (!application.isEmpty()) {
        directories << application;                                  // installed beside the app
        directories << application + QStringLiteral("/..");           // one level up
        directories << application + QStringLiteral("/../..");        // native/build -> repo
        directories << application + QStringLiteral("/../bridge");    // native/build -> native/bridge
        directories << application + QStringLiteral("/../../native/bridge");
        directories << application + QStringLiteral("/../share/time_warp");
    }
    // The working directory, which is how a test or a launcher finds the tree
    // without knowing where the binary is.
    const QString cwd = QDir::currentPath();
    directories << cwd;
    directories << cwd + QStringLiteral("/native/bridge");
    directories << cwd + QStringLiteral("/../native/bridge");

    QStringList candidates;
    candidates.reserve(directories.size());
    for (const QString& directory : directories) {
        candidates << QDir::cleanPath(directory + QLatin1Char('/') + QString::fromLatin1(kWorkerFileName));
    }
    return candidates;
}

/// Reasonable interpreters to try, in order, given the script's own directory.
///
/// The repository's `.venv` comes first because that is where this project's
/// dependencies are installed; falling back to `python3` from `PATH` keeps a
/// system-installed Qt build usable without a virtual environment.
[[nodiscard]] QStringList interpreterCandidates(const QString& script) {
    QStringList candidates;
    if (!script.isEmpty()) {
        const QDir script_directory{QFileInfo{script}.absolutePath()};
        // <repo>/native/bridge/worker.py -> <repo>/.venv/bin/python3
        const QString repository = QDir::cleanPath(script_directory.absoluteFilePath(QStringLiteral("../..")));
        candidates << repository + QStringLiteral("/.venv/bin/python3");
        candidates << repository + QStringLiteral("/.venv/bin/python");
    }
    for (const char* name : {"python3", "python"}) {
        const QString found = QStandardPaths::findExecutable(QString::fromLatin1(name));
        if (!found.isEmpty()) {
            candidates << found;
        }
    }
    return candidates;
}

[[nodiscard]] QString firstExisting(const QStringList& candidates) {
    for (const QString& candidate : candidates) {
        const QFileInfo info{candidate};
        if (info.exists() && info.isFile()) {
            return info.absoluteFilePath();
        }
    }
    return {};
}

/// `#rrggbb`-free colour from the protocol's `[r, g, b]` array.
[[nodiscard]] std::uint32_t colourOf(const QJsonObject& command) {
    const QJsonValue value = command.value(QStringLiteral("c"));
    if (!value.isArray()) {
        return 0;
    }
    const QJsonArray channels = value.toArray();
    if (channels.size() < 3) {
        return 0;
    }
    return make_rgb(channels.at(0).toInt(), channels.at(1).toInt(), channels.at(2).toInt());
}

/// The optional fill colour of a shape, or `std::nullopt` when it has none.
///
/// Carried in the command's `b` field, which the header already documents as
/// "extent / secondary angle / second colour" -- the same slot gradients use for
/// their second colour. Inventing a new field would have changed a struct whose
/// layout is asserted by a static_assert and relied on by the ring buffer.
[[nodiscard]] std::optional<double> fillOf(const QJsonObject& command) {
    const QJsonValue value = command.value(QStringLiteral("fill"));
    if (!value.isArray()) {
        return std::nullopt;
    }
    const QJsonArray channels = value.toArray();
    if (channels.size() < 3) {
        return std::nullopt;
    }
    const std::uint32_t packed =
        make_rgb(channels.at(0).toInt(), channels.at(1).toInt(), channels.at(2).toInt());
    return static_cast<double>(packed);
}

[[nodiscard]] double number(const QJsonObject& object, const char* key, double fallback = 0.0) {
    const QJsonValue value = object.value(QString::fromLatin1(key));
    return value.isDouble() ? value.toDouble() : fallback;
}

}  // namespace

WorkerPaths locateWorker(QString* reason) {
    const auto set_reason = [reason](const QString& text) {
        if (reason != nullptr) {
            *reason = text;
        }
    };

    WorkerPaths paths;
    const QString override_script = fromEnvironment(kWorkerEnvironmentVariable);
    if (!override_script.isEmpty()) {
        if (QFileInfo{override_script}.isFile()) {
            paths.script = QFileInfo{override_script}.absoluteFilePath();
        } else {
            set_reason(QStringLiteral("%1 points at %2, which is not a file.")
                           .arg(QString::fromLatin1(kWorkerEnvironmentVariable), override_script));
            return {};
        }
    } else {
        paths.script = firstExisting(scriptCandidates());
    }

    if (paths.script.isEmpty()) {
        set_reason(QStringLiteral("The language worker script (%1) was not found. Searched: %2")
                       .arg(QString::fromLatin1(kWorkerFileName),
                            scriptCandidates().join(QStringLiteral(", "))));
        return {};
    }

    const QString override_python = fromEnvironment(kPythonEnvironmentVariable);
    if (!override_python.isEmpty()) {
        if (QFileInfo{override_python}.isExecutable()) {
            paths.interpreter = override_python;
        } else {
            set_reason(QStringLiteral("%1 points at %2, which is not executable.")
                           .arg(QString::fromLatin1(kPythonEnvironmentVariable), override_python));
            return {};
        }
    } else {
        paths.interpreter = firstExisting(interpreterCandidates(paths.script));
    }

    if (paths.interpreter.isEmpty()) {
        set_reason(QStringLiteral(
            "No Python interpreter was found. Install python3, or set %1 to one.")
                       .arg(QString::fromLatin1(kPythonEnvironmentVariable)));
        return {};
    }
    return paths;
}

// ---------------------------------------------------------------------------
// PythonWorker
// ---------------------------------------------------------------------------

PythonWorker::PythonWorker(timewarp::canvas::CanvasCommandQueue& queue, QObject* parent)
    : QObject(parent), queue_{queue} {
    // The lesson structures cross a signal, so they are registered rather than
    // relied on being free: a queued connection to an unregistered type fails at
    // run time, in the one place nobody is watching for it.
    qRegisterMetaType<timewarp::bridge::LessonSummary>();
    qRegisterMetaType<timewarp::bridge::LessonCheckpoint>();
    qRegisterMetaType<timewarp::bridge::LessonProgress>();
    qRegisterMetaType<QList<timewarp::bridge::LessonSummary>>();
    qRegisterMetaType<QList<timewarp::bridge::LessonProgress>>();
    paths_ = locateWorker();
}

PythonWorker::~PythonWorker() {
    // The destructor must not leave a child process behind: a worker whose parent
    // has gone would keep running the program nobody is watching.
    if (process_ != nullptr) {
        process_->disconnect(this);
        if (process_->state() != QProcess::NotRunning) {
            process_->kill();
            process_->waitForFinished(2000);
        }
    }
}

bool PythonWorker::start(QString* error) {
    if (isRunning()) {
        return true;
    }
    if (!paths_.isValid()) {
        // Re-searched rather than trusted from the constructor: a caller may set
        // the environment overrides after constructing, and the first attempt's
        // failure is not the last word. Adopting the result is the point -- failing
        // with a good reason while holding a usable path would be perverse.
        QString reason;
        const WorkerPaths found = locateWorker(&reason);
        if (!found.isValid()) {
            if (error != nullptr) {
                *error = reason;
            }
            return false;
        }
        paths_ = found;
    }

    saw_ready_ = false;
    // A finished process object is kept only so `isRunning` can answer honestly;
    // it is disposed of here, because a new run must not inherit the old one's
    // state, queued signals or stderr buffer.
    if (process_ != nullptr) {
        process_->disconnect(this);
        process_->deleteLater();
        process_ = nullptr;
    }
    standard_error_.clear();
    pending_stdout_.clear();
    saw_done_ = false;
    running_program_ = false;
    stop_requested_ = false;

    process_ = new QProcess{this};
    // Keep the protocol on stdout and diagnostics on stderr, separate: a line that
    // is not JSON must never reach the parser, and a warning from a library must
    // not look like a program error.
    process_->setProcessChannelMode(QProcess::SeparateChannels);
    process_->setProcessEnvironment(QProcessEnvironment::systemEnvironment());
    connect(process_, &QProcess::readyReadStandardOutput, this,
            &PythonWorker::onReadyReadStandardOutput);
    connect(process_, &QProcess::readyReadStandardError, this,
            &PythonWorker::onReadyReadStandardError);
    connect(process_, &QProcess::finished, this, &PythonWorker::onProcessFinished);
    // Qt 6 still emits this alongside `finished`; wiring it means a process that
    // cannot start at all is reported at the moment it happens rather than at the
    // next read.
    connect(process_, &QProcess::errorOccurred, this, [this](QProcess::ProcessError process_error) {
        if (process_error != QProcess::FailedToStart) {
            return;
        }
        emit failed(tr("The language worker could not be started: %1 %2\n%3")
                        .arg(paths_.interpreter, paths_.script, process_->errorString()));
    });

    process_->start(paths_.interpreter, {paths_.script}, QIODevice::ReadWrite);
    if (!process_->waitForStarted(kStartHandshakeMilliseconds)) {
        if (error != nullptr) {
            *error = tr("The language worker did not start: %1")
                         .arg(process_->errorString());
        }
        return false;
    }

    // Wait for `ready` so a worker that dies during start-up -- a missing
    // dependency, a syntax error in the script, a bad interpreter -- is reported
    // by `start` with its stderr, instead of the IDE handing it a program and
    // waiting for a reply that can never come.
    //
    // A loop rather than one wait, because the handshake is a *line* and a pipe
    // delivers bytes: a worker that flushes in pieces -- which is what any
    // buffered writer does above a certain size -- would otherwise be judged
    // "never ready" on the strength of having sent half a message. The deadline
    // is the same either way, and a process that dies still ends the loop at once.
    QElapsedTimer handshake;
    handshake.start();
    while (!saw_ready_ && process_->state() != QProcess::NotRunning &&
           handshake.elapsed() < kStartHandshakeMilliseconds) {
        // Drained explicitly as well as by signal: a worker that writes its
        // handshake immediately can have it sitting in QProcess's buffer before
        // this call, and `waitForReadyRead` then returns at once *without*
        // emitting `readyReadStandardOutput`.
        process_->waitForReadyRead(100);
        onReadyReadStandardOutput();
    }
    if (!saw_ready_) {
        const QString detail = standard_error_.trimmed();
        if (error != nullptr) {
            *error = detail.isEmpty()
                         ? tr("The language worker stopped before it was ready (%1).")
                               .arg(process_->errorString())
                         : tr("The language worker stopped before it was ready:\n%1").arg(detail);
        }
        return false;
    }
    return true;
}

bool PythonWorker::run(const QString& language, const QString& source, const QString& name) {
    if (!isRunning() || !saw_ready_) {
        emit failed(tr("The language worker is not running."));
        return false;
    }
    saw_done_ = false;
    stop_requested_ = false;
    running_program_ = true;
    standard_error_.clear();
    commands_pushed_ = 0;
    batches_ = 0;

    QJsonObject message;
    message[QStringLiteral("t")] = QStringLiteral("run");
    message[QStringLiteral("language")] = language;
    message[QStringLiteral("source")] = source;
    message[QStringLiteral("name")] = name;

    const QByteArray payload = QJsonDocument{message}.toJson(QJsonDocument::Compact) + '\n';
    if (process_->write(payload) != payload.size()) {
        emit failed(tr("The program could not be sent to the language worker."));
        return false;
    }
    process_->waitForBytesWritten(2000);
    return true;
}

void PythonWorker::stop() {
    if (!isRunning()) {
        return;
    }
    // Cooperative first: the worker sets the interpreter's cancel flag, which lets
    // it finish the statement it is in and flush what it has drawn. Then, because a
    // single statement can be an unbounded loop, a hard kill.
    stop_requested_ = true;
    const QByteArray request = QByteArrayLiteral("{\"t\":\"stop\"}\n");
    process_->write(request);
    process_->waitForBytesWritten(200);
    if (!process_->waitForFinished(kStopGraceMilliseconds)) {
        process_->kill();
        // Do not report a failure here: the user asked for the program to stop, so
        // a killed process is the requested outcome, not an error.
        process_->waitForFinished(2000);
    }
}

bool PythonWorker::isRunning() const {
    return process_ != nullptr && process_->state() != QProcess::NotRunning;
}

void PythonWorker::answerInput(const QString& text) {
    if (!isRunning()) {
        return;
    }
    QJsonObject message;
    message[QStringLiteral("t")] = QStringLiteral("in-reply");
    message[QStringLiteral("text")] = text;
    process_->write(QJsonDocument{message}.toJson(QJsonDocument::Compact) + '\n');
}

bool PythonWorker::sendRequest(const QJsonObject& message) {
    if (!isRunning() || !saw_ready_) {
        emit failed(tr("The language worker is not running."));
        return false;
    }
    const QByteArray payload =
        QJsonDocument{message}.toJson(QJsonDocument::Compact) + '\n';
    if (process_->write(payload) != payload.size()) {
        return false;
    }
    process_->waitForBytesWritten(2000);
    return true;
}

bool PythonWorker::requestLessons() {
    if (waiting_for_lesson_) {
        return false;
    }
    QJsonObject message;
    message[QStringLiteral("t")] = QStringLiteral("lessons");
    if (!sendRequest(message)) {
        return false;
    }
    waiting_for_lesson_ = true;
    return true;
}

bool PythonWorker::requestLessonProgress() {
    if (waiting_for_lesson_) {
        return false;
    }
    QJsonObject message;
    message[QStringLiteral("t")] = QStringLiteral("lesson-progress");
    if (!sendRequest(message)) {
        return false;
    }
    waiting_for_lesson_ = true;
    return true;
}

bool PythonWorker::requestHint(const QString& lesson, int checkpoint) {
    if (waiting_for_lesson_) {
        return false;
    }
    QJsonObject message;
    message[QStringLiteral("t")] = QStringLiteral("lesson-hint");
    message[QStringLiteral("lesson")] = lesson;
    message[QStringLiteral("checkpoint")] = checkpoint;
    if (!sendRequest(message)) {
        return false;
    }
    waiting_for_lesson_ = true;
    return true;
}

bool PythonWorker::requestSolution(const QString& lesson, int checkpoint) {
    if (waiting_for_lesson_) {
        return false;
    }
    QJsonObject message;
    message[QStringLiteral("t")] = QStringLiteral("lesson-solution");
    message[QStringLiteral("lesson")] = lesson;
    message[QStringLiteral("checkpoint")] = checkpoint;
    if (!sendRequest(message)) {
        return false;
    }
    waiting_for_lesson_ = true;
    return true;
}

bool PythonWorker::checkLesson(const QString& lesson, int checkpoint, const QString& source) {
    if (waiting_for_lesson_) {
        return false;
    }
    // A check runs an interpreter, and so does a program. The worker refuses the
    // race as well; refusing here too means the panel is told at once rather than
    // after a round trip that ends in the same answer.
    if (isRunningProgram()) {
        emit lessonFailed(QStringLiteral("lesson-check"),
                          tr("a program is running. Stop it before checking your work."));
        return false;
    }
    QJsonObject message;
    message[QStringLiteral("t")] = QStringLiteral("lesson-check");
    message[QStringLiteral("lesson")] = lesson;
    message[QStringLiteral("checkpoint")] = checkpoint;
    message[QStringLiteral("source")] = source;
    if (!sendRequest(message)) {
        return false;
    }
    waiting_for_lesson_ = true;
    return true;
}

bool PythonWorker::resetLesson(const QString& lesson) {
    if (waiting_for_lesson_) {
        return false;
    }
    QJsonObject message;
    message[QStringLiteral("t")] = QStringLiteral("lesson-reset");
    message[QStringLiteral("lesson")] = lesson;
    if (!sendRequest(message)) {
        return false;
    }
    waiting_for_lesson_ = true;
    return true;
}

void PythonWorker::onReadyReadStandardOutput() {
    pending_stdout_ += process_->readAllStandardOutput();
    // A message can be split anywhere, including between the braces of a JSON
    // object, so anything after the last newline stays buffered for next time.
    int newline = pending_stdout_.indexOf('\n');
    while (newline >= 0) {
        const QByteArray line = pending_stdout_.left(newline);
        pending_stdout_.remove(0, newline + 1);
        if (!line.trimmed().isEmpty()) {
            handleMessage(line);
        }
        newline = pending_stdout_.indexOf('\n');
    }
}

void PythonWorker::onReadyReadStandardError() {
    noteStandardError(QString::fromUtf8(process_->readAllStandardError()));
}

void PythonWorker::noteStandardError(const QString& text) {
    // Bounded: a program that prints to stderr in a loop would otherwise grow this
    // without limit while the IDE holds it for a crash report.
    constexpr int kMaximumDiagnostics = 4000;
    if (standard_error_.size() >= kMaximumDiagnostics) {
        return;
    }
    standard_error_ += text;
    if (standard_error_.size() > kMaximumDiagnostics) {
        standard_error_.truncate(kMaximumDiagnostics);
        standard_error_ += QStringLiteral("\n[...]");
    }
}

void PythonWorker::onProcessFinished(int exitCode, int status) {
    // Everything the process had to say is collected before it is judged. A
    // process that dies quickly -- a syntax error in the script, a missing import
    // -- can exit with its last stderr still unread, and that text is the *only*
    // thing that says why. Losing it turns "the worker did not start" into a
    // message with no cause.
    noteStandardError(QString::fromUtf8(process_->readAllStandardError()));

    // Anything still buffered is a truncated message: the process ended mid-line.
    const QByteArray leftover = pending_stdout_;
    pending_stdout_.clear();
    if (!leftover.trimmed().isEmpty()) {
        handleMessage(leftover);
    }

    const bool crashed = !saw_done_;
    const bool was_running = running_program_;
    const bool was_requested = stop_requested_;
    running_program_ = false;
    stop_requested_ = false;

    if (!crashed) {
        // `done` already reported the outcome; the exit is just the exit.
    } else if (was_requested) {
        // The user asked for this. A killed process is the requested outcome, so
        // reporting a failure would put an error in front of them for pressing
        // Stop -- but a run still has to end with exactly one terminal event, so
        // the stop is reported as an unfinished run.
        emit finished(false, QString{}, QStringList{});
    } else if (was_running) {
        QString detail = standard_error_.trimmed();
        if (detail.isEmpty()) {
            detail = tr("no diagnostics were produced");
        }
        emit failed(tr("The language worker exited unexpectedly (code %1, %2).\n%3")
                        .arg(exitCode)
                        .arg(status == QProcess::CrashExit ? tr("crash") : tr("normal exit"))
                        .arg(detail));
    }
    // The process object is kept until the next `start`, so `isRunning`'s answer
    // stays honest and a caller can inspect the exit for itself.
}

namespace {

/// A JSON array of strings as a QStringList, ignoring anything else in it.
[[nodiscard]] QStringList stringList(const QJsonValue& value) {
    QStringList list;
    const QJsonArray array = value.toArray();
    for (const QJsonValue& item : array) {
        list.append(item.toString());
    }
    return list;
}

[[nodiscard]] LessonCheckpoint checkpointFrom(const QJsonObject& object) {
    LessonCheckpoint checkpoint;
    checkpoint.title = object.value(QStringLiteral("title")).toString();
    checkpoint.description = object.value(QStringLiteral("description")).toString();
    checkpoint.starter = object.value(QStringLiteral("starter")).toString();
    checkpoint.expectation = object.value(QStringLiteral("expectation")).toString();
    checkpoint.hints = stringList(object.value(QStringLiteral("hints")));
    return checkpoint;
}

[[nodiscard]] LessonSummary lessonFrom(const QJsonObject& object) {
    LessonSummary lesson;
    lesson.id = object.value(QStringLiteral("id")).toString();
    lesson.title = object.value(QStringLiteral("title")).toString();
    lesson.language = object.value(QStringLiteral("language")).toString();
    lesson.languageKey = object.value(QStringLiteral("languageKey")).toString();
    lesson.difficulty = object.value(QStringLiteral("difficulty")).toString();
    lesson.description = object.value(QStringLiteral("description")).toString();
    lesson.objectives = stringList(object.value(QStringLiteral("objectives")));
    lesson.prerequisites = stringList(object.value(QStringLiteral("prerequisites")));
    const QJsonArray checkpoints = object.value(QStringLiteral("checkpoints")).toArray();
    for (const QJsonValue& item : checkpoints) {
        lesson.checkpoints.append(checkpointFrom(item.toObject()));
    }
    return lesson;
}

[[nodiscard]] LessonProgress progressFrom(const QJsonObject& object) {
    LessonProgress row;
    row.id = object.value(QStringLiteral("id")).toString();
    row.title = object.value(QStringLiteral("title")).toString();
    row.language = object.value(QStringLiteral("language")).toString();
    row.difficulty = object.value(QStringLiteral("difficulty")).toString();
    row.status = object.value(QStringLiteral("status")).toString();
    row.checkpoint = object.value(QStringLiteral("checkpoint")).toInt();
    row.checkpoints = object.value(QStringLiteral("checkpoints")).toInt();
    row.hintsUsed = object.value(QStringLiteral("hints_used")).toInt();
    return row;
}

}  // namespace

void PythonWorker::handleMessage(const QByteArray& line) {
    QJsonParseError parse_error{};
    const QJsonDocument document = QJsonDocument::fromJson(line, &parse_error);
    if (parse_error.error != QJsonParseError::NoError || !document.isObject()) {
        // Reported rather than ignored: a line the IDE cannot read is a protocol
        // bug, and silently skipping it would show up as a drawing with a hole in
        // it and no explanation.
        emit errorReceived(tr("The language worker sent a message this build cannot read: %1")
                               .arg(QString::fromUtf8(line.left(200))));
        return;
    }
    const QJsonObject message = document.object();
    const QString kind = message.value(QStringLiteral("t")).toString();

    if (kind == QLatin1String("ready")) {
        saw_ready_ = true;
        emit ready();
        return;
    }
    if (kind == QLatin1String("out")) {
        emit outputReceived(message.value(QStringLiteral("text")).toString());
        return;
    }
    if (kind == QLatin1String("err")) {
        emit errorReceived(message.value(QStringLiteral("text")).toString());
        return;
    }
    if (kind == QLatin1String("in")) {
        emit inputRequested(message.value(QStringLiteral("prompt")).toString());
        return;
    }
    if (kind == QLatin1String("pose")) {
        emit poseReceived(number(message, "x"), number(message, "y"), number(message, "h"),
                          message.value(QStringLiteral("show")).toBool(true));
        return;
    }
    if (kind == QLatin1String("canvas")) {
        const QJsonArray commands = message.value(QStringLiteral("cmds")).toArray();
        for (const QJsonValue& value : commands) {
            if (value.isObject()) {
                applyCommand(value.toObject());
            }
        }
        // A batch is a delivery unit, so everything in it has to become visible.
        // State-only commands -- a background colour, a pen colour, a heading --
        // are *coalesced* by the queue and held in its staging slot until a
        // drawing command displaces them or somebody flushes. Without this, a
        // program whose last command is `setbgcolor` leaves the canvas on the old
        // background for ever, and the same applies to any pen change that nothing
        // subsequently draws with.
        queue_.flush();
        ++batches_;
        // Emitted once per batch, not once per command: the consumer drains the
        // queue in one pass, and a signal per command would be the per-command Qt
        // delivery cost the queue exists to avoid.
        emit commandsQueued();
        return;
    }
    // ---- lessons -----------------------------------------------------------
    //
    // Every reply clears `waiting_for_lesson_` first, whatever it says: a failed
    // request that left the flag set would lock the panel out of every request
    // that followed it.
    if (kind == QLatin1String("lessons")) {
        waiting_for_lesson_ = false;
        if (!message.value(QStringLiteral("ok")).toBool(true)) {
            emit lessonFailed(QStringLiteral("lessons"),
                              message.value(QStringLiteral("error")).toString());
            return;
        }
        QList<LessonSummary> lessons;
        const QJsonArray items = message.value(QStringLiteral("items")).toArray();
        for (const QJsonValue& item : items) {
            lessons.append(lessonFrom(item.toObject()));
        }
        // A payload from an older worker has no presentation block; the panel's
        // own default is then used, which is what the `0` means.
        int presentation_font = 0;
        const QJsonValue presentation = message.value(QStringLiteral("presentation"));
        if (presentation.isObject()) {
            presentation_font =
                presentation.toObject().value(QStringLiteral("font_size")).toInt();
        }
        emit lessonsReceived(lessons, presentation_font);
        return;
    }
    if (kind == QLatin1String("lesson-progress")) {
        waiting_for_lesson_ = false;
        if (!message.value(QStringLiteral("ok")).toBool(true)) {
            emit lessonFailed(QStringLiteral("lesson-progress"),
                              message.value(QStringLiteral("error")).toString());
            return;
        }
        QList<LessonProgress> rows;
        const QJsonArray items = message.value(QStringLiteral("items")).toArray();
        for (const QJsonValue& item : items) {
            rows.append(progressFrom(item.toObject()));
        }
        emit lessonProgressReceived(rows, message.value(QStringLiteral("completed")).toInt(),
                                    message.value(QStringLiteral("total")).toInt());
        return;
    }
    if (kind == QLatin1String("lesson-hint")) {
        waiting_for_lesson_ = false;
        if (!message.value(QStringLiteral("ok")).toBool(true)) {
            emit lessonFailed(QStringLiteral("lesson-hint"),
                              message.value(QStringLiteral("error")).toString());
            return;
        }
        emit lessonHintReceived(message.value(QStringLiteral("text")).toString(),
                                message.value(QStringLiteral("n")).toInt(),
                                message.value(QStringLiteral("total")).toInt());
        return;
    }
    if (kind == QLatin1String("lesson-solution")) {
        waiting_for_lesson_ = false;
        if (!message.value(QStringLiteral("ok")).toBool(true)) {
            emit lessonFailed(QStringLiteral("lesson-solution"),
                              message.value(QStringLiteral("error")).toString());
            return;
        }
        emit lessonSolutionReceived(message.value(QStringLiteral("text")).toString());
        return;
    }
    if (kind == QLatin1String("lesson-check")) {
        waiting_for_lesson_ = false;
        if (!message.value(QStringLiteral("ok")).toBool(true)) {
            emit lessonFailed(QStringLiteral("lesson-check"),
                              message.value(QStringLiteral("error")).toString());
            return;
        }
        emit lessonChecked(
            message.value(QStringLiteral("lesson")).toString(),
            message.value(QStringLiteral("checkpoint")).toInt(),
            message.value(QStringLiteral("passed")).toBool(),
            message.value(QStringLiteral("message")).toString(),
            message.value(QStringLiteral("completed")).toBool(),
            message.value(QStringLiteral("finished")).toInt(),
            message.value(QStringLiteral("total")).toInt());
        return;
    }
    if (kind == QLatin1String("lesson-reset")) {
        waiting_for_lesson_ = false;
        if (!message.value(QStringLiteral("ok")).toBool(true)) {
            emit lessonFailed(QStringLiteral("lesson-reset"),
                              message.value(QStringLiteral("error")).toString());
            return;
        }
        emit lessonResetDone(message.value(QStringLiteral("lesson")).toString());
        return;
    }

    if (kind == QLatin1String("done")) {
        saw_done_ = true;
        running_program_ = false;
        QStringList unsupported;
        for (const QJsonValue& value : message.value(QStringLiteral("unsupported")).toArray()) {
            unsupported << value.toString();
        }
        emit finished(message.value(QStringLiteral("ok")).toBool(true),
                      message.value(QStringLiteral("error")).toString(), unsupported);
        return;
    }
    emit errorReceived(tr("The language worker sent an unknown message kind: %1").arg(kind));
}

void PythonWorker::applyCommand(const QJsonObject& command) {
    const QString kind = command.value(QStringLiteral("k")).toString();
    const std::uint32_t colour = colourOf(command);

    if (kind == QLatin1String("clear")) {
        queue_.push(make_command(CanvasCommandKind::Clear));
        ++commands_pushed_;
        return;
    }
    if (kind == QLatin1String("bg")) {
        queue_.push(make_color(CanvasCommandKind::SetBackgroundColor, colour));
        ++commands_pushed_;
        return;
    }
    if (kind == QLatin1String("line")) {
        // A stroked line is a two-point polyline. There is no separate "line"
        // command kind, and adding one would mean a new record layout for a shape
        // the polyline already covers exactly.
        const std::array<double, 4> points{number(command, "x1"), number(command, "y1"),
                                           number(command, "x2"), number(command, "y2")};
        CanvasCommand stroke = make_color(CanvasCommandKind::DrawPolyline, colour);
        stroke.x = number(command, "w", 1.0);
        queue_.push(stroke, {}, points);
        ++commands_pushed_;
        return;
    }
    if (kind == QLatin1String("poly")) {
        const QJsonArray values = command.value(QStringLiteral("pts")).toArray();
        std::vector<double> points;
        points.reserve(static_cast<std::size_t>(values.size()));
        for (const QJsonValue& value : values) {
            points.push_back(value.toDouble());
        }
        CanvasCommand stroke = make_color(CanvasCommandKind::DrawPolyline, colour);
        stroke.x = number(command, "w", 1.0);
        if (command.value(QStringLiteral("closed")).toBool(false)) {
            stroke.flags |= timewarp::canvas::kCanvasFlagClosed;
        }
        if (const std::optional<double> fill = fillOf(command)) {
            stroke.flags |= timewarp::canvas::kCanvasFlagFilled;
            stroke.b = *fill;
        }
        queue_.push(stroke, {}, points);
        ++commands_pushed_;
        return;
    }
    if (kind == QLatin1String("rect") || kind == QLatin1String("ellipse")) {
        CanvasCommand shape = make_color(kind == QLatin1String("rect")
                                             ? CanvasCommandKind::DrawRect
                                             : CanvasCommandKind::DrawEllipse,
                                         colour);
        shape.x = number(command, "x");
        shape.y = number(command, "y");
        shape.w = number(command, "w");
        shape.h = number(command, "h");
        if (const std::optional<double> fill = fillOf(command)) {
            shape.flags |= timewarp::canvas::kCanvasFlagFilled;
            shape.b = *fill;
        }
        queue_.push(shape);
        ++commands_pushed_;
        return;
    }
    if (kind == QLatin1String("point")) {
        CanvasCommand point = make_color(CanvasCommandKind::DrawPoint, colour);
        point.x = number(command, "x");
        point.y = number(command, "y");
        queue_.push(point);
        ++commands_pushed_;
        return;
    }
    if (kind == QLatin1String("text")) {
        CanvasCommand text = make_color(CanvasCommandKind::DrawText, colour);
        text.x = number(command, "x");
        text.y = number(command, "y");
        text.w = number(command, "size", 12.0);
        const QByteArray payload = command.value(QStringLiteral("s")).toString().toUtf8();
        queue_.push(text, std::string_view{payload.constData(),
                                           static_cast<std::size_t>(payload.size())});
        ++commands_pushed_;
        return;
    }

    // An unknown kind is reported rather than dropped: it means the worker is
    // sending something this build does not know how to draw, and the difference
    // between "the program drew nothing" and "the IDE cannot draw it" matters.
    emit errorReceived(tr("The language worker sent an unknown drawing command: %1").arg(kind));
}

}  // namespace timewarp::bridge
