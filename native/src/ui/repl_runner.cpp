// SPDX-License-Identifier: MIT

#include "timewarp/ui/repl_runner.hpp"

#include "timewarp/canvas/host_adapter.hpp"
#include "timewarp/lang/repl.hpp"
#include "timewarp/scheme/machine.hpp"
#include "timewarp/scheme/printer.hpp"
#include "timewarp/ui/native_runner.hpp"

#include <exception>

namespace timewarp::ui {

namespace {

QString fromUtf8(const std::string& text) {
    return QString::fromUtf8(text.data(), static_cast<int>(text.size()));
}

/// Adds the newline a console row needs without doubling one the engine wrote.
void ensureNewline(std::string& text) {
    if (!text.empty() && text.back() != '\n') {
        text.push_back('\n');
    }
}

}  // namespace

ReplRunner::ReplRunner(timewarp::lang::Language language,
                       timewarp::canvas::CanvasCommandQueue& queue, QObject* parent)
    : QThread(parent), language_(language) {
    lang_host_ = std::make_unique<NativeCanvasHost>(queue);
    session_ = std::make_unique<timewarp::lang::ReplSession>(language, *lang_host_);

    if (language == timewarp::lang::Language::Scheme) {
        scheme_host_ = std::make_unique<timewarp::canvas::QueuedTurtleHost>(queue);
        scheme_arena_ = std::make_unique<timewarp::scheme::Arena>();
        scheme_machine_ = std::make_unique<timewarp::scheme::Machine>(*scheme_arena_,
                                                                      *scheme_host_);
    }
    setObjectName(QStringLiteral("replRunner"));
}

void ReplRunner::setMachineBus(timewarp::lang::Interpreter::MachineBus bus) {
    if (session_ != nullptr) {
        session_->set_machine_bus(std::move(bus));
    }
}

ReplRunner::~ReplRunner() {
    requestStop();
    wait();
}

void ReplRunner::submit(const QString& command) {
    {
        const std::lock_guard<std::mutex> lock{mutex_};
        if (quit_) {
            return;
        }
        commands_.push_back(command);
    }
    wake_.notify_all();
    if (!isRunning()) {
        start();
    }
}

void ReplRunner::resetSession() {
    {
        const std::lock_guard<std::mutex> lock{mutex_};
        if (quit_) {
            return;
        }
        reset_requested_ = true;
    }
    wake_.notify_all();
    if (!isRunning()) {
        start();
    }
}

void ReplRunner::requestStop() {
    {
        const std::lock_guard<std::mutex> lock{mutex_};
        quit_ = true;
        commands_.clear();
    }
    // The command in flight checks this between statements, which is the same
    // contract a document run has.
    if (session_ != nullptr) {
        session_->request_stop();
    }
    wake_.notify_all();
}

void ReplRunner::setWakeup(std::function<void()> wakeup) {
    wakeup_ = std::move(wakeup);
    if (lang_host_ != nullptr) {
        lang_host_->set_wakeup(wakeup_);
    }
    if (scheme_host_ != nullptr) {
        scheme_host_->set_wakeup(wakeup_);
    }
}

void ReplRunner::setSqlCallback(std::function<std::string(std::string_view)> callback) {
    sql_callback_ = std::move(callback);
    if (session_ != nullptr) {
        session_->set_sql_callback(sql_callback_);
    }
    if (scheme_host_ != nullptr) {
        if (sql_callback_) {
            scheme_host_->set_sql_runner(sql_callback_);
        }
    }
}

void ReplRunner::setSurfaceColor(timewarp::lang::Rgb colour) {
    if (lang_host_ != nullptr) {
        lang_host_->set_default_background(colour);
    }
}

void ReplRunner::setInkColor(timewarp::lang::Rgb colour) {
    if (lang_host_ != nullptr) {
        lang_host_->set_default_pen(colour);
    }
}

void ReplRunner::run() {
    for (;;) {
        QString command;
        {
            std::unique_lock lock{mutex_};
            wake_.wait(lock, [this] {
                return quit_ || reset_requested_ || !commands_.empty();
            });
            if (quit_) {
                return;
            }
            if (reset_requested_) {
                reset_requested_ = false;
                lock.unlock();
                resetNow();
                continue;
            }
            if (commands_.empty()) {
                continue;
            }
            command = commands_.front();
            commands_.pop_front();
        }

        if (!surface_begun_ && lang_host_ != nullptr) {
            lang_host_->begin_run();
            surface_begun_ = true;
        }
        if (language_ == timewarp::lang::Language::Scheme) {
            executeScheme(command);
        } else {
            executeLang(command);
        }
        if (lang_host_ != nullptr) {
            lang_host_->flush();
        }
    }
}

void ReplRunner::resetNow() {
    if (language_ == timewarp::lang::Language::Scheme) {
        // The machine's arena owns every value it has ever allocated, so a reset
        // is a whole new arena rather than a cleared environment.
        scheme_arena_ = std::make_unique<timewarp::scheme::Arena>();
        scheme_machine_ = std::make_unique<timewarp::scheme::Machine>(*scheme_arena_,
                                                                      *scheme_host_);
        scheme_emitted_ = 0;
    } else if (session_ != nullptr) {
        session_->reset();
    }
    emit commandFinished(true, QString{});
}

void ReplRunner::executeLang(const QString& command) {
    std::string text;
    try {
        text = session_->execute(command.toStdString());
    } catch (const std::exception& thrown) {
        emit commandFinished(false, QString::fromUtf8(thrown.what()));
        return;
    }
    if (!text.empty()) {
        emit outputAvailable(fromUtf8(text));
    }
    const std::string error = session_->error();
    emit commandFinished(error.empty(), fromUtf8(error));
}

void ReplRunner::executeScheme(const QString& command) {
    std::string delta;
    try {
        const std::string text = scheme_machine_->execute(command.toStdString());
        const std::string& out = scheme_machine_->output();
        if (out.size() >= scheme_emitted_) {
            delta = out.substr(scheme_emitted_);
            scheme_emitted_ = out.size();
        }
        if (delta.empty() && text.rfind("\u274C", 0) == 0) {
            // A reader or evaluator error never reaches the output buffer; it is
            // the returned text. Without this a mistyped form would look like a
            // silent success.
            delta = text;
        }
        const bool failed = text.rfind("\u274C", 0) == 0;
        const timewarp::scheme::Value value = scheme_machine_->last_value();
        if (!failed && value.type != timewarp::scheme::ValueType::Unspecified) {
            ensureNewline(delta);
            delta += timewarp::scheme::write_to_string(value);
            delta.push_back('\n');
        }
        if (!delta.empty()) {
            ensureNewline(delta);
        }
    } catch (const std::exception& thrown) {
        emit commandFinished(false, QString::fromUtf8(thrown.what()));
        return;
    }
    if (!delta.empty()) {
        emit outputAvailable(fromUtf8(delta));
    }
    const bool ok = delta.rfind("\u274C", 0) != 0;
    emit commandFinished(ok, ok ? QString{} : fromUtf8(delta));
}

}  // namespace timewarp::ui
