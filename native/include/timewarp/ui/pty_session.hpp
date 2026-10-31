// SPDX-License-Identifier: MIT
//
// A real pseudo-terminal for the Terminal tab.
//
// A pipe is not a terminal. A shell on a pipe does not print a prompt, colours
// nothing, has no job control, and a program that asks how wide the terminal is
// gets an error instead of an answer. This gives the shell a device node of its own:
// `posix_openpt`, a `fork`, and the slave as the child's controlling terminal.
//
// It is written by hand rather than taken from a library because the parts are four
// calls -- `posix_openpt`, `grantpt`, `unlockpt`, `ptsname` -- and a terminal widget
// library would bring a whole KDE-descended dependency into a build that has three.

#pragma once

#include <QByteArray>
#include <QString>

namespace timewarp::ui {

/// A shell running on a pseudo-terminal of its own.
///
/// The child is a session leader with the slave as its controlling terminal, which
/// is what makes `isatty()` true inside it and puts its process group where `stop()`
/// can reach every process it started.
class PtySession {
public:
    PtySession() = default;
    ~PtySession();

    PtySession(const PtySession&) = delete;
    PtySession& operator=(const PtySession&) = delete;

    /// Starts `program` on a new pty, in `directory` when it is not empty, with the
    /// view's size already set so the first prompt wraps where the window does.
    /// Returns false and fills `error` when no pty or no child could be had.
    bool start(const QString& program, const QString& directory, int columns, int rows,
               QString* error);

    /// Whether the shell is still there. A shell that has closed its terminal reads
    /// as gone, and its exit status is what `exitCode()` then answers.
    [[nodiscard]] bool isRunning();

    /// The master side, for a `QSocketNotifier` to watch.
    [[nodiscard]] int descriptor() const { return master_; }

    /// Everything the shell has written that has not been read yet. Empty when the
    /// shell is gone, which is how a reader learns it is gone.
    [[nodiscard]] QByteArray read();

    /// Types `bytes` at the shell, queueing whatever the terminal will not take yet.
    ///
    /// It never blocks: a pty's input buffer is small, and the caller is a widget on
    /// the GUI thread, so a shell that has stopped reading -- a foreground job that
    /// ignores its input, a process that was stopped -- must not stop the window. What
    /// does not fit waits in `pending_` for `flush()`, which the caller drives from a
    /// writable notifier.
    ///
    /// False when the input was refused: the pty has gone, or the queue is full
    /// because the shell has not read anything for a long time. A caller that ignores
    /// the answer loses keystrokes silently, so it should say so.
    bool write(const QByteArray& bytes);

    /// Sends what is queued, as far as the terminal will take it, and returns how
    /// many bytes are still waiting.
    int flush();

    /// Whether anything is still queued, which is what enables the writable notifier.
    [[nodiscard]] bool hasPending() const noexcept { return !pending_.isEmpty(); }

    /// Tells the shell how big the view is, which also sends it `SIGWINCH` so a
    /// program that cares redraws.
    void setWindowSize(int columns, int rows);

    /// Ends the shell: `SIGHUP`, then `SIGTERM`, then `SIGKILL`, reaping whatever it
    /// started. Nothing a program ran outlives the window that started it.
    void stop();

    /// How the shell ended, or -1 while it runs and when it was killed by a signal.
    [[nodiscard]] int exitCode();

private:
    /// `waitpid` without blocking; true when the child is gone and `status_` holds
    /// its status.
    bool reap();

    int master_ = -1;
    int pid_ = -1;
    int status_ = 0;
    bool exited_ = false;
    bool eof_ = false;
    /// Typed at the widget and not yet accepted by the terminal. Bounded, because a
    /// queue nobody drains is a memory leak that looks like a keyboard.
    QByteArray pending_;
    static constexpr int kMaxPending = 256 * 1024;
};

}  // namespace timewarp::ui
