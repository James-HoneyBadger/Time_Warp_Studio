// SPDX-License-Identifier: MIT

#include "timewarp/api/debug.hpp"

#include <QString>
#include <QStringList>
#include <QVector>

#include <chrono>
#include <condition_variable>
#include <mutex>
#include <thread>

#include "timewarp/debugger/debugger_runner.hpp"
#include "timewarp/graphics/recording_host.hpp"
#include "timewarp/lang/debug.hpp"
#include "timewarp/lang/interpreter.hpp"

namespace timewarp::api::debug {

namespace {

constexpr auto kWait = std::chrono::seconds{15};

std::string joinLines(const std::vector<std::string>& lines) {
    std::string text;
    for (const std::string& line : lines) {
        text += line;
        text.push_back('\n');
    }
    return text;
}

class NativeSession final : public Session {
public:
    NativeSession(timewarp::lang::Language language, std::string source,
                  const std::set<int>& breakpoints,
                  const std::set<std::string>& data_breakpoints,
                  std::function<std::string(std::string_view)> sql)
        : interpreter_{std::make_unique<timewarp::lang::Interpreter>(language, host_)} {
        std::set<std::size_t> indices;
        for (const int line : breakpoints) {
            if (line > 0) {
                indices.insert(static_cast<std::size_t>(line - 1));
            }
        }
        debug_.set_breakpoints(std::move(indices));
        debug_.set_data_breakpoints(data_breakpoints);
        debug_.on_paused = [this](int line) {
            publish(line, debug_.paused_for_step() ? "step" : "breakpoint", {});
        };
        debug_.on_data_changed = [this](int line, const std::string& name) {
            publish(line, "data", name);
        };
        interpreter_->set_debug_session(&debug_);
        interpreter_->set_output_callback([this](const std::string& line) {
            const std::lock_guard<std::mutex> lock{mutex_};
            output_.push_back(line);
        });
        if (sql) {
            interpreter_->set_sql_callback(std::move(sql));
        }
        interpreter_->load_program(source);
        thread_ = std::thread([this] {
            interpreter_->run();
            finish();
        });
        (void)waitFor(0);
    }

    ~NativeSession() override {
        if (thread_.joinable()) {
            if (!finished()) {
                debug_.request_stop();
            }
            thread_.join();
        }
    }

    Snapshot snapshot() const override {
        const std::lock_guard<std::mutex> lock{mutex_};
        return state_;
    }

    Snapshot resume() override {
        if (finished()) {
            return snapshot();
        }
        const std::uint64_t seen = event();
        debug_.request_continue();
        (void)waitFor(seen);
        return snapshot();
    }

    Snapshot step(int mode) override {
        if (finished()) {
            return snapshot();
        }
        const std::uint64_t seen = event();
        // The line loop has one step unit. Into, over and out are the same
        // request there, which is the same contract the Debug panel has.
        (void)mode;
        debug_.request_step();
        (void)waitFor(seen);
        return snapshot();
    }

    Snapshot stop() override {
        if (!finished()) {
            debug_.request_stop();
        }
        if (thread_.joinable()) {
            thread_.join();
        }
        return snapshot();
    }

private:
    [[nodiscard]] bool finished() const {
        const std::lock_guard<std::mutex> lock{mutex_};
        return finished_;
    }

    [[nodiscard]] std::uint64_t event() const {
        const std::lock_guard<std::mutex> lock{mutex_};
        return event_;
    }

    [[nodiscard]] bool waitFor(std::uint64_t seen) {
        std::unique_lock lock{mutex_};
        return changed_.wait_for(lock, kWait,
                                 [this, seen] { return event_ != seen; });
    }

    void publish(int line, const char* reason, const std::string& changed) {
        Snapshot snapshot;
        snapshot.paused = true;
        snapshot.line = line;
        snapshot.reason = reason;
        snapshot.changed_variable = changed;
        for (const auto& [name, value] : interpreter_->variables()) {
            snapshot.variables.push_back(
                {name, std::string{value.type_name()}, value.render()});
        }
        // Read here and not in `snapshot()`: the probe is only meaningful while
        // the interpreter's thread is parked in the pause, which is this call.
        for (const timewarp::lang::CallFrame& frame : interpreter_->call_stack()) {
            snapshot.stack.push_back({frame.procedure, frame.line});
        }
        {
            const std::lock_guard<std::mutex> lock{mutex_};
            snapshot.output = joinLines(output_);
        }
        snapshot.svg = host_.svg();
        snapshot.drawing_commands = host_.drawingCalls();
        {
            const std::lock_guard<std::mutex> lock{mutex_};
            state_ = std::move(snapshot);
            ++event_;
        }
        changed_.notify_all();
    }

    void finish() {
        Snapshot snapshot;
        snapshot.finished = true;
        snapshot.reason = interpreter_->error().empty() ? "finished" : "error";
        snapshot.error = interpreter_->error();
        for (const auto& [name, value] : interpreter_->variables()) {
            snapshot.variables.push_back(
                {name, std::string{value.type_name()}, value.render()});
        }
        {
            const std::lock_guard<std::mutex> lock{mutex_};
            snapshot.output = joinLines(output_);
            state_ = std::move(snapshot);
            finished_ = true;
            ++event_;
        }
        changed_.notify_all();
    }

    graphics::RecordingHost host_;
    std::unique_ptr<timewarp::lang::Interpreter> interpreter_;
    timewarp::lang::DebugSession debug_;
    std::thread thread_;

    mutable std::mutex mutex_;
    std::condition_variable changed_;
    std::uint64_t event_ = 0;
    bool finished_ = false;
    Snapshot state_;
    std::vector<std::string> output_;
};

class SchemeSession final : public Session {
public:
    SchemeSession(std::string source, const std::set<int>& breakpoints,
                  const std::set<std::string>& data_breakpoints,
                  std::function<std::string(std::string_view)> sql) {
        QObject::connect(
            &runner_, &timewarp::debugger::DebuggerRunner::paused, &runner_,
            [this](int, int, const QString&) {
                {
                    const std::lock_guard<std::mutex> lock{mutex_};
                    ++event_;
                }
                changed_.notify_all();
            },
            Qt::DirectConnection);
        QObject::connect(
            &runner_, &timewarp::debugger::DebuggerRunner::run_finished, &runner_,
            [this](bool, const QString&) {
                {
                    const std::lock_guard<std::mutex> lock{mutex_};
                    finished_ = true;
                    ++event_;
                }
                changed_.notify_all();
            },
            Qt::DirectConnection);

        QVector<int> lines;
        for (const int line : breakpoints) {
            if (line > 0) {
                lines.push_back(line);
            }
        }
        QStringList names;
        for (const std::string& name : data_breakpoints) {
            names.append(QString::fromStdString(name));
        }
        runner_.set_host(&host_);
        if (sql) {
            host_.set_sql_runner(std::move(sql));
        }
        runner_.set_breakpoints(lines);
        runner_.set_data_breakpoints(names);
        runner_.load_program(QString::fromStdString(source));
        runner_.start();
        (void)waitFor(0);
    }

    ~SchemeSession() override { stop(); }

    Snapshot snapshot() const override {
        const timewarp::debugger::DebugSnapshot debug = runner_.snapshot();
        Snapshot snapshot;
        snapshot.line = debug.line;
        snapshot.depth = debug.depth;
        snapshot.procedure = debug.procedure.toStdString();
        snapshot.reason = reasonOf(debug.cause);
        snapshot.changed_variable = debug.changed_variable.toStdString();
        snapshot.error = debug.error.toStdString();
        for (const timewarp::debugger::StackFrameInfo& frame : debug.stack) {
            snapshot.stack.push_back({frame.procedure.toStdString(), frame.line});
        }
        for (const timewarp::debugger::DebugVariable& variable : debug.variables) {
            snapshot.variables.push_back({variable.name.toStdString(),
                                          variable.kind.toStdString(),
                                          variable.rendered.toStdString()});
        }
        snapshot.output = runner_.output().toStdString();
        {
            const std::lock_guard<std::mutex> lock{mutex_};
            snapshot.finished = finished_;
        }
        snapshot.paused = runner_.is_paused();
        // The recording host is written by the runner thread. Reading it is safe
        // exactly when the runner is parked (paused) or finished; a timeout state
        // leaves it alone rather than racing the interpreter.
        if (snapshot.paused || snapshot.finished) {
            snapshot.svg = host_.svg();
            snapshot.drawing_commands = host_.drawingCalls();
        }
        if (snapshot.finished) {
            // The runner keeps the last pause's line and cause as the most
            // precise state it observed. A caller that asked for the end of the
            // run needs to know it ended, not that it is still at line 2.
            snapshot.reason = snapshot.error.empty() ? "finished" : "error";
        }
        return snapshot;
    }

    Snapshot resume() override {
        if (isFinished()) {
            return snapshot();
        }
        const std::uint64_t seen = event();
        runner_.request_continue();
        (void)waitFor(seen);
        return snapshot();
    }

    Snapshot step(int mode) override {
        if (isFinished()) {
            return snapshot();
        }
        const std::uint64_t seen = event();
        if (mode == 0) {
            runner_.request_step_into();
        } else if (mode == 1) {
            runner_.request_step_over();
        } else {
            runner_.request_step_out();
        }
        (void)waitFor(seen);
        return snapshot();
    }

    Snapshot stop() override {
        if (!isFinished()) {
            runner_.request_stop();
            runner_.wait(5000);
        }
        {
            const std::lock_guard<std::mutex> lock{mutex_};
            finished_ = true;
        }
        return snapshot();
    }

private:
    static std::string reasonOf(timewarp::debugger::PauseCause cause) {
        switch (cause) {
            case timewarp::debugger::PauseCause::Breakpoint:
                return "breakpoint";
            case timewarp::debugger::PauseCause::Step:
                return "step";
            case timewarp::debugger::PauseCause::PauseRequest:
                return "pause";
            case timewarp::debugger::PauseCause::DataBreakpoint:
                return "data";
            case timewarp::debugger::PauseCause::Error:
                return "error";
            case timewarp::debugger::PauseCause::Finished:
                return "finished";
        }
        return "unknown";
    }

    [[nodiscard]] bool isFinished() const {
        const std::lock_guard<std::mutex> lock{mutex_};
        return finished_;
    }

    [[nodiscard]] std::uint64_t event() const {
        const std::lock_guard<std::mutex> lock{mutex_};
        return event_;
    }

    [[nodiscard]] bool waitFor(std::uint64_t seen) {
        std::unique_lock lock{mutex_};
        return changed_.wait_for(lock, kWait,
                                 [this, seen] { return event_ != seen; });
    }

    graphics::RecordingHost host_;
    timewarp::debugger::DebuggerRunner runner_;

    mutable std::mutex mutex_;
    std::condition_variable changed_;
    std::uint64_t event_ = 0;
    bool finished_ = false;
};

}  // namespace

std::unique_ptr<Session> Session::start(
    timewarp::lang::Language language, std::string source,
    const std::set<int>& breakpoints, const std::set<std::string>& data_breakpoints,
    std::function<std::string(std::string_view)> sql) {
    if (language == timewarp::lang::Language::Scheme) {
        return std::make_unique<SchemeSession>(std::move(source), breakpoints,
                                               data_breakpoints, std::move(sql));
    }
    return std::make_unique<NativeSession>(language, std::move(source), breakpoints,
                                           data_breakpoints, std::move(sql));
}

}  // namespace timewarp::api::debug
