// SPDX-License-Identifier: MIT
//
// The IDE's end of the Python bridge: it starts `time_warp_worker.py`, hands it a
// program, and turns the stream of JSON lines it answers with into canvas
// commands and Qt signals.
//
// #### Why a separate process
//
// Five of the six languages have no C++ interpreter yet. The alternative to this
// is reimplementing them, and the Python implementations are complete and tested;
// the alternative *inside* one process is embedding CPython, which turns a
// runaway program into a runaway IDE. Out of process, the worst case is a process
// to kill.
//
// #### What this class is careful about
//
//  * **A worker that dies is an error, not a hang.** If the process exits without
//    a `done` message, that is reported as a failure with the stderr text rather
//    than leaving a run that never finishes.
//  * **A message can be split across reads.** `QProcess::readyRead` says nothing
//    about message boundaries, so partial lines are buffered until their newline
//    arrives.
//  * **A stray `print` cannot corrupt the protocol.** The worker redirects its own
//    stdout for exactly that reason; stderr is captured separately and only ever
//    shown as diagnostics.

#pragma once

#include <cstdint>

#include <QByteArray>
#include <QObject>
#include <QString>
#include <QStringList>

class QJsonObject;
class QProcess;

namespace timewarp::canvas {
class CanvasCommandQueue;
}

namespace timewarp::bridge {

/// Where the worker and its interpreter were found.
struct WorkerPaths {
    QString interpreter;
    QString script;

    /// True when both were located and exist.
    [[nodiscard]] bool isValid() const { return !interpreter.isEmpty() && !script.isEmpty(); }
};

/// Locates the Python interpreter and the worker script.
///
/// The search order is explicit rather than clever, because "it works on my
/// machine" for a cross-process launcher means "the paths happen to line up
/// here":
///
///   1. `TIME_WARP_PYTHON` and `TIME_WARP_WORKER` override everything.
///   2. The script beside the executable, then two levels up (the build tree
///      layout: `native/build/time_warp_ide` next to `native/bridge/worker.py`),
///      then the working directory.
///   3. The interpreter from the repository's `.venv` next to the script, then
///      `python3` and `python` from `PATH`.
///
/// `reason` is set to a sentence naming what was searched for, for the error
/// panel: a missing bridge has to say which half is missing.
[[nodiscard]] WorkerPaths locateWorker(QString* reason = nullptr);

/// Runs one program at a time in a Python worker process.
///
/// Every method must be called from the thread that owns the object, which is the
/// GUI thread in the application and the test thread in the tests. The
/// interpreter is not consulted for anything: it is a pipe and a parser.
class PythonWorker final : public QObject {
    Q_OBJECT

public:
    /// `queue` receives the drawing. It is pushed to directly rather than through
    /// a signal so a batch costs one call instead of one queued invocation per
    /// command.
    explicit PythonWorker(timewarp::canvas::CanvasCommandQueue& queue,
                          QObject* parent = nullptr);
    ~PythonWorker() override;

    /// Starts the process if it is not already running, and waits for the worker's
    /// `ready` handshake. Returns false and sets `error` when the interpreter or
    /// the script cannot be found, or the process will not start.
    bool start(QString* error);

    /// Sends a program to run. The worker must be started. The source is carried
    /// as a JSON string, so any program text is safe.
    bool run(const QString& language, const QString& source, const QString& name);

    /// Asks the worker to stop, then kills it if it has not exited within a short
    /// grace period. Safe when nothing is running.
    void stop();

    /// True while the process is alive.
    [[nodiscard]] bool isRunning() const;

    /// True while a program has been sent and its result has not arrived.
    ///
    /// Distinct from `isRunning`, which stays true while the worker sits idle
    /// between runs -- deliberately, because a Python start costs about 150ms and
    /// reusing the process across runs is what keeps Run feeling instant. Callers
    /// that mean "a program is executing" have to ask this one, or they will claim
    /// to stop something that is not running and needlessly restart the worker.
    [[nodiscard]] bool isRunningProgram() const noexcept { return running_program_; }

    /// Answers a pending input request.
    void answerInput(const QString& text);

    /// The paths this instance was started with, for diagnostics.
    [[nodiscard]] const WorkerPaths& paths() const noexcept { return paths_; }

signals:
    /// The worker's handshake. A run before this is accepted is a protocol error.
    void ready();
    /// One line of program output, newline included.
    void outputReceived(const QString& text);
    /// A batch of drawing commands has been pushed into the queue. The consumer
    /// drains on this rather than on a timer.
    void commandsQueued();
    /// The turtle moved. Carried as a signal rather than as a canvas command
    /// because painting the *marker* is not something the command stream expresses:
    /// its commands say what to draw, not where the turtle ended up.
    void poseReceived(double x, double y, double heading, bool visible);
    /// The program is waiting for a line of input.
    void inputRequested(const QString& prompt);
    /// A failure the worker reported, for the error panel.
    void errorReceived(const QString& text);
    /// The run ended. `ok` is false for a program error, a stop, or a crash.
    void finished(bool ok, const QString& error, const QStringList& unsupported);
    /// The process could not start, or died mid-run. Distinct from `finished`
    /// because there is no program result to report.
    void failed(const QString& text);

private slots:
    void onReadyReadStandardOutput();
    void onReadyReadStandardError();
    void onProcessFinished(int exitCode, int status);

private:
    /// Handles one complete protocol line.
    void handleMessage(const QByteArray& line);
    /// Applies one entry of a `canvas` batch to the queue.
    void applyCommand(const QJsonObject& command);
    /// Appends to the error stream text, for diagnostics on a crash.
    void noteStandardError(const QString& text);

    timewarp::canvas::CanvasCommandQueue& queue_;
    WorkerPaths paths_;
    QProcess* process_ = nullptr;
    QByteArray pending_stdout_;
    QString standard_error_;
    /// Set when `done` arrives, so a later process exit is not reported as a
    /// crash.
    bool saw_done_ = false;
    /// Set by the `ready` handshake, and checked by `start`: a worker that dies
    /// during start-up must fail there rather than swallow the first program.
    bool saw_ready_ = false;
    /// Set when the user asked for a stop, so the process exit that follows is
    /// reported as a stop rather than as a crash. Cleared when a run begins.
    bool stop_requested_ = false;
    bool running_program_ = false;
    std::uint64_t commands_pushed_ = 0;
    std::uint64_t batches_ = 0;
};

}  // namespace timewarp::bridge
