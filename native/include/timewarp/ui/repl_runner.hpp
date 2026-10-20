// SPDX-License-Identifier: MIT
//
// The REPL bar's engine: one background thread, one persistent session.
//
// A REPL command is still a program statement, so it runs where every other
// statement runs -- off the GUI thread -- and its drawing goes through the same
// canvas queue. What differs is that the interpreter is not thrown away between
// commands: `X = 5` followed by `PRINT X` prints 5.
//
// Scheme has its own machine and therefore its own path through this class, but
// the same rule applies: the machine is kept, and the value of the last expression
// is echoed the way a Scheme REPL does.

#pragma once

#include <QString>
#include <QThread>

#include <condition_variable>
#include <cstddef>
#include <deque>
#include <functional>
#include <memory>
#include <mutex>
#include <string>
#include <string_view>

#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/lang/language.hpp"
#include "timewarp/lang/turtle_host.hpp"

namespace timewarp::scheme {
class Arena;
class Machine;
}  // namespace timewarp::scheme

namespace timewarp::canvas {
class QueuedTurtleHost;
}

namespace timewarp::lang {
class ReplSession;
}

namespace timewarp::ui {

class NativeCanvasHost;

class ReplRunner final : public QThread {
    Q_OBJECT

public:
    ReplRunner(timewarp::lang::Language language,
               timewarp::canvas::CanvasCommandQueue& queue, QObject* parent = nullptr);
    ~ReplRunner() override;

    /// Queues one submission. The thread starts on the first one and stays alive
    /// until the runner is stopped, so a second command cannot inherit a first
    /// command's half-finished state.
    void submit(const QString& command);
    /// Drops every binding. Queued, so it cannot overtake a command in flight.
    void resetSession();
    /// Cooperative stop of the command in flight, then the thread.
    void requestStop();

    void setSqlCallback(std::function<std::string(std::string_view)> callback);
    /// Wakes the GUI so it drains the canvas queue. The window supplies
    /// `CanvasBridge::wake`; a headless test can leave it unset.
    void setWakeup(std::function<void()> wakeup);
    /// The theme's drawing surface for a language that starts drawing without
    /// naming a background.
    void setSurfaceColor(timewarp::lang::Rgb colour);

    /// The ink a REPL run starts with, for the same reason `setSurfaceColor`
    /// exists: a theme with light paper needs dark ink.
    void setInkColor(timewarp::lang::Rgb colour);

    [[nodiscard]] timewarp::lang::Language language() const noexcept { return language_; }

signals:
    /// Text the command produced, newline-terminated as appropriate. Empty when
    /// the command was silent (an assignment, for example).
    void outputAvailable(const QString& text);
    /// One command finished. `ok` is false when the engine reported an error;
    /// `error` is its message, empty on success.
    void commandFinished(bool ok, const QString& error);

protected:
    void run() override;

private:
    void executeLang(const QString& command);
    void executeScheme(const QString& command);
    void resetNow();

    timewarp::lang::Language language_;
    std::unique_ptr<NativeCanvasHost> lang_host_;
    std::unique_ptr<timewarp::lang::ReplSession> session_;
    std::unique_ptr<timewarp::canvas::QueuedTurtleHost> scheme_host_;
    std::unique_ptr<timewarp::scheme::Arena> scheme_arena_;
    std::unique_ptr<timewarp::scheme::Machine> scheme_machine_;
    /// Bytes of the Scheme machine's output already delivered. The machine's
    /// output buffer accumulates for the life of the session.
    std::size_t scheme_emitted_ = 0;
    bool surface_begun_ = false;

    std::mutex mutex_;
    std::condition_variable wake_;
    std::deque<QString> commands_;
    bool reset_requested_ = false;
    bool quit_ = false;
    std::function<std::string(std::string_view)> sql_callback_;
    std::function<void()> wakeup_;
};

}  // namespace timewarp::ui
