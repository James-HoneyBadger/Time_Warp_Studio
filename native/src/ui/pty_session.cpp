// SPDX-License-Identifier: MIT

#include "timewarp/ui/pty_session.hpp"

#include <cerrno>
#include <csignal>
#include <cstdlib>
#include <cstring>

#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

namespace timewarp::ui {

namespace {

/// Sets the window size the shell sees. Failing here is not fatal: a shell with no
/// size still runs, it just wraps where it decides to.
void applyWindowSize(int descriptor, int columns, int rows) {
    if (descriptor < 0) {
        return;
    }
    struct winsize size {};
    size.ws_col = static_cast<unsigned short>(columns > 0 ? columns : 80);
    size.ws_row = static_cast<unsigned short>(rows > 0 ? rows : 24);
    static_cast<void>(::ioctl(descriptor, TIOCSWINSZ, &size));
}

/// Waits for the shell to go, and takes it by force when it will not.
bool waitOrKill(int pid) {
    for (int attempt = 0; attempt < 50; ++attempt) {
        int status = 0;
        const pid_t finished = ::waitpid(pid, &status, WNOHANG);
        if (finished == pid || (finished < 0 && errno != EINTR)) {
            return true;
        }
        ::usleep(10000);
    }
    // The whole process group, not just the shell: a `sleep 100 &` typed at the
    // prompt is the shell's child, and leaving it running is leaving a process
    // nobody can find.
    static_cast<void>(::kill(-pid, SIGKILL));
    static_cast<void>(::kill(pid, SIGKILL));
    for (int attempt = 0; attempt < 50; ++attempt) {
        int status = 0;
        const pid_t finished = ::waitpid(pid, &status, WNOHANG);
        if (finished == pid || (finished < 0 && errno != EINTR)) {
            return true;
        }
        ::usleep(10000);
    }
    return false;
}

}  // namespace

PtySession::~PtySession() {
    stop();
}

bool PtySession::start(const QString& program, const QString& directory, int columns,
                       int rows, QString* error) {
    stop();

    const int master = ::posix_openpt(O_RDWR | O_NOCTTY | O_CLOEXEC);
    if (master < 0) {
        if (error != nullptr) {
            *error = QStringLiteral("no pseudo-terminal available: %1")
                         .arg(QString::fromLocal8Bit(std::strerror(errno)));
        }
        return false;
    }
    if (::grantpt(master) != 0 || ::unlockpt(master) != 0) {
        if (error != nullptr) {
            *error = QStringLiteral("the pseudo-terminal was refused: %1")
                         .arg(QString::fromLocal8Bit(std::strerror(errno)));
        }
        ::close(master);
        return false;
    }
    // Copied before the fork: `ptsname` returns a static buffer, and the child
    // shares it until it execs.
    const char* slave_name = ::ptsname(master);
    if (slave_name == nullptr) {
        if (error != nullptr) {
            *error = QStringLiteral("the pseudo-terminal has no slave device");
        }
        ::close(master);
        return false;
    }
    const QByteArray slave_path{slave_name};

    const QByteArray executable = program.toLocal8Bit();
    const QByteArray working = directory.toLocal8Bit();
    applyWindowSize(master, columns, rows);

    // Everything the child needs before it execs, prepared here rather than after
    // the fork: between `fork` and `exec` only async-signal-safe calls are safe,
    // and `QByteArray` is not.
    const char* const env_dir = working.isEmpty() ? nullptr : working.constData();

    const pid_t pid = ::fork();
    if (pid < 0) {
        if (error != nullptr) {
            *error = QStringLiteral("could not start a shell: %1")
                         .arg(QString::fromLocal8Bit(std::strerror(errno)));
        }
        ::close(master);
        return false;
    }

    if (pid == 0) {
        // The child. `setsid` first, so it is a session leader with no controlling
        // terminal; opening the slave then makes that its terminal, which is what
        // gives it a foreground process group and job control.
        static_cast<void>(::setsid());
        const int slave = ::open(slave_path.constData(), O_RDWR);
        if (slave < 0) {
            ::_exit(127);
        }
        // Some of these carry `warn_unused_result` in glibc -- `-Werror` and
        // `_FORTIFY_SOURCE` turn a discarded one into a failed build, and
        // `static_cast<void>` does not silence it. The results are still not
        // actionable between `fork` and `exec`: there is nobody to tell but the
        // parent, and the parent learns from the exit status.
        [[maybe_unused]] const int controlling = ::ioctl(slave, TIOCSCTTY, 0);
        [[maybe_unused]] const int dup_out = ::dup2(slave, STDOUT_FILENO);
        [[maybe_unused]] const int dup_err = ::dup2(slave, STDERR_FILENO);
        if (::dup2(slave, STDIN_FILENO) < 0) {
            ::_exit(127);
        }
        if (slave > STDERR_FILENO) {
            ::close(slave);
        }
        ::close(master);

        // The parent is a Qt application, which ignores `SIGPIPE` and may have
        // blocked signals; a shell must start from the defaults or a `| head` would
        // not end, and Ctrl+C would do nothing.
        for (const int signal : {SIGINT, SIGQUIT, SIGTERM, SIGPIPE, SIGCHLD, SIGTSTP,
                                 SIGTTIN, SIGTTOU}) {
            ::signal(signal, SIG_DFL);
        }
        sigset_t none;
        ::sigemptyset(&none);
        ::sigprocmask(SIG_SETMASK, &none, nullptr);

        // A folder that has gone is not a reason to refuse to start a shell: it
        // opens where it is, which is what a person would get from a prompt.
        [[maybe_unused]] const int moved =
            env_dir != nullptr ? ::chdir(env_dir) : 0;
        // The terminal the shell believes it is on. Naming a real terminal type is
        // what makes a prompt colour itself; the view strips the escapes.
        [[maybe_unused]] const int termed = ::setenv("TERM", "xterm-256color", 1);
        static_cast<void>(::execl(executable.constData(), executable.constData(),
                                  static_cast<char*>(nullptr)));
        ::_exit(127);
    }

    master_ = master;
    pid_ = static_cast<int>(pid);
    status_ = 0;
    exited_ = false;
    eof_ = false;

    // Non-blocking, because a read on a quiet shell must not stop the window.
    const int flags = ::fcntl(master_, F_GETFL, 0);
    if (flags >= 0) {
        static_cast<void>(::fcntl(master_, F_SETFL, flags | O_NONBLOCK));
    }
    return true;
}

bool PtySession::isRunning() {
    if (pid_ <= 0) {
        return false;
    }
    if (exited_) {
        return false;
    }
    if (reap()) {
        return false;
    }
    return !eof_;
}

QByteArray PtySession::read() {
    if (master_ < 0) {
        return {};
    }
    QByteArray collected;
    char buffer[4096];
    for (;;) {
        const ssize_t count = ::read(master_, buffer, sizeof buffer);
        if (count > 0) {
            collected.append(buffer, static_cast<int>(count));
            continue;
        }
        if (count < 0 && errno == EINTR) {
            continue;
        }
        // `EAGAIN` and `EWOULDBLOCK` are the same value on Linux, so naming both
        // trips -Wlogical-op.
        if (count < 0 && errno == EAGAIN) {
            break;
        }
        // Zero, or `EIO`: the shell closed its side, which is how a pty says the
        // program has finished rather than "try again later".
        eof_ = true;
        break;
    }
    return collected;
}

bool PtySession::write(const QByteArray& bytes) {
    if (master_ < 0) {
        return false;
    }
    // Typed input keeps its order: whatever is still queued goes before this.
    if (pending_.size() + bytes.size() > kMaxPending) {
        // The shell has not read anything for a long time. Refusing the keystroke is
        // what makes that visible; growing the queue instead would turn a stopped
        // shell into an allocation nobody notices.
        static_cast<void>(flush());
        return false;
    }
    pending_.append(bytes);
    static_cast<void>(flush());
    return true;
}

int PtySession::flush() {
    if (master_ < 0) {
        pending_.clear();
        return 0;
    }
    while (!pending_.isEmpty()) {
        const ssize_t count = ::write(master_, pending_.constData(), pending_.size());
        if (count > 0) {
            pending_.remove(0, static_cast<int>(count));
            continue;
        }
        if (count < 0 && errno == EINTR) {
            continue;
        }
        if (count < 0 && errno == EAGAIN) {
            // The terminal is full. The caller's writable notifier comes back here
            // rather than this loop waiting on the GUI thread.
            break;
        }
        // The far side is gone, and with it the input.
        eof_ = true;
        pending_.clear();
        return 0;
    }
    return static_cast<int>(pending_.size());
}

void PtySession::setWindowSize(int columns, int rows) {
    applyWindowSize(master_, columns, rows);
}

void PtySession::stop() {
    pending_.clear();
    if (master_ >= 0) {
        ::close(master_);
        master_ = -1;
    }
    if (pid_ > 0) {
        if (!exited_) {
            // `SIGHUP` first, as a closed terminal would: a well behaved shell exits.
            if (::kill(-pid_, SIGHUP) != 0) {
                static_cast<void>(::kill(pid_, SIGHUP));
            }
            static_cast<void>(waitOrKill(pid_));
        }
        pid_ = -1;
    }
    exited_ = false;
    eof_ = false;
    status_ = 0;
}

int PtySession::exitCode() {
    if (!exited_ && pid_ > 0) {
        // The shell has closed the terminal; it is a zombie now, so the wait does
        // not block for long. A child that somehow outlives its terminal is left
        // alone rather than hanging the window.
        for (int attempt = 0; attempt < 20; ++attempt) {
            if (reap()) {
                break;
            }
            ::usleep(5000);
        }
    }
    if (!exited_) {
        return -1;
    }
    if (WIFEXITED(status_)) {
        return WEXITSTATUS(status_);
    }
    return -1;
}

bool PtySession::reap() {
    if (pid_ <= 0) {
        return false;
    }
    const pid_t finished = ::waitpid(pid_, &status_, WNOHANG);
    if (finished == pid_) {
        exited_ = true;
        return true;
    }
    return false;
}

}  // namespace timewarp::ui
