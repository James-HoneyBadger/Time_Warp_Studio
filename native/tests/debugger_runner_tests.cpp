// SPDX-License-Identifier: MIT
//
// Integration tests for the background debugger runner.
//
// These need a real Qt event loop and a real second thread, because the whole
// point of the runner is the handshake between them -- a single-threaded test
// would prove nothing.  Built only when Qt6 is found; see native/CMakeLists.txt.

#include <algorithm>
#include <chrono>
#include <cstddef>
#include <cstdio>
#include <functional>
#include <string>
#include <thread>

#include <QCoreApplication>
#include <QObject>
#include <QPair>
#include <QString>
#include <QVector>

#include "check.hpp"
#include "timewarp/debugger/debugger_runner.hpp"

namespace {

using timewarp::debugger::DebugSnapshot;
using timewarp::debugger::DebuggerRunner;
using timewarp::debugger::PauseCause;

/// `(fact 5)`, with the recursive call on line 4.  Five breakpoint hits: n = 5
/// down to 1.
constexpr const char* kFact =
    "(define (fact n)\n"
    "  (if (= n 0)\n"
    "      1\n"
    "      (* n (fact (- n 1)))))\n"
    "(display (fact 5))\n";

/// Prints, then loops long enough that a control request cannot race past it.
constexpr const char* kSpin =
    "(define (spin n) (if (= n 0) 0 (spin (- n 1))))\n"
    "(display \"started\")\n"
    "(spin 2000000)\n";

/// `tick` keeps its body on line 2, so a breakpoint there is unambiguous: the
/// top-level define *opens* on line 1, and breakpoints are line-based, so the
/// definition itself cannot match.  The body then loops a hundred thousand
/// times, which is what makes a mid-run breakpoint edit meaningful.
constexpr const char* kTick =
    "(define (tick n)\n"
    "  (if (= n 0) 0 (tick (- n 1))))\n"
    "(display \"go\")\n"
    "(tick 100000)\n";

/// `inner` is called from the `let` initialiser, so it nests; `(+ v 1)` runs at
/// depth 1 after it returns.  That is the step-out target.
constexpr const char* kNested =
    "(define (inner x) (* x 2))\n"
    "(define (outer n)\n"
    "  (let ((v (inner n)))\n"
    "    (+ v 1)))\n"
    "(display (outer 5))\n";

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

/// Everything the GUI thread observed, in arrival order.
///
/// A `QObject`, and used as the connection *context* rather than as a plain
/// struct, because that is what makes the connections safe to leave behind.
///
/// A queued connection outlives the emit: the event sits in the receiver's queue
/// until the next `processEvents()`. If the context were the application -- which
/// lives for the whole run -- those events would still be delivered after the
/// frame that owns this recorder has returned, and the lambda would read and
/// write a dead stack frame. AddressSanitizer reports that as
/// `stack-use-after-return`; without ASan it is silent corruption, which in
/// practice showed up as a segfault in an unrelated part of the process two
/// tests later. With the recorder as the context, `~QObject` discards its
/// pending events and the functor can never outlive what it captured.
struct Recorder : QObject {
    int started = 0;
    int resumed = 0;
    int finished = 0;
    bool completed = false;
    QString error;
    QString output;
    QVector<int> pause_lines;
    QVector<int> pause_depths;
    QVector<QString> pause_procedures;
    QVector<PauseCause> pause_causes;
};

void attach(DebuggerRunner& runner, Recorder& recorder) {
    QObject::connect(&runner, &DebuggerRunner::run_started, &recorder,
                     [&recorder] { ++recorder.started; });
    QObject::connect(&runner, &DebuggerRunner::output_available, &recorder,
                     [&recorder](const QString& text) { recorder.output += text; });
    QObject::connect(
        &runner, &DebuggerRunner::paused, &recorder,
        [&recorder](int line, int depth, const QString& procedure) {
            recorder.pause_lines.push_back(line);
            recorder.pause_depths.push_back(depth);
            recorder.pause_procedures.push_back(procedure);
        });
    QObject::connect(&runner, &DebuggerRunner::resumed, &recorder,
                     [&recorder] { ++recorder.resumed; });
    QObject::connect(&runner, &DebuggerRunner::run_finished, &recorder,
                     [&recorder](bool completed, const QString& error) {
                         ++recorder.finished;
                         recorder.completed = completed;
                         recorder.error = error;
                     });
}

/// Pumps the Qt event loop until `done()`, or the timeout expires.
[[nodiscard]] bool spin_until(
    QCoreApplication& app, const std::function<bool()>& done,
    std::chrono::milliseconds timeout = std::chrono::milliseconds{10000}) {
    const auto deadline = std::chrono::steady_clock::now() + timeout;
    for (;;) {
        app.processEvents();
        if (done()) {
            return true;
        }
        if (std::chrono::steady_clock::now() >= deadline) {
            return false;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds{1});
    }
}

/// Waits until the pause count exceeds `before`, reporting whether it did.
[[nodiscard]] bool wait_for_pause(QCoreApplication& app, Recorder& recorder,
                                  int before) {
    return spin_until(app, [&recorder, before] {
        return static_cast<int>(recorder.pause_lines.size()) > before;
    });
}

[[nodiscard]] bool has_variable(const DebugSnapshot& snapshot,
                                const QString& name, const QString& rendered) {
    for (const QPair<QString, QString>& entry : snapshot.variables) {
        if (entry.first == name && entry.second == rendered) {
            return true;
        }
    }
    return false;
}

/// Names only: some bindings are procedures, whose rendering is not the point.
[[nodiscard]] bool has_variable_named(const DebugSnapshot& snapshot,
                                      const QString& name) {
    for (const QPair<QString, QString>& entry : snapshot.variables) {
        if (entry.first == name) {
            return true;
        }
    }
    return false;
}

// A timed-out wait would leave these empty, and Qt's `last()` on an empty
// vector is undefined -- so the tests read through safe accessors and the
// assertion fails loudly instead of the process dying.
[[nodiscard]] int last_line(const Recorder& recorder) {
    return recorder.pause_lines.isEmpty() ? -1 : recorder.pause_lines.last();
}

[[nodiscard]] int last_depth(const Recorder& recorder) {
    return recorder.pause_depths.isEmpty() ? -1 : recorder.pause_depths.last();
}

// ---------------------------------------------------------------------------
// Plain execution
// ---------------------------------------------------------------------------

void test_runs_to_completion() {
    announce("run to completion");
    QCoreApplication& app = *QCoreApplication::instance();
    DebuggerRunner runner;
    Recorder recorder;
    attach(runner, recorder);

    runner.load_program(QString::fromUtf8(kFact));
    runner.start();

    TW_CHECK(spin_until(app, [&recorder] { return recorder.finished > 0; }));
    TW_CHECK(runner.wait(5000));

    TW_CHECK_EQ(recorder.started, 1);
    TW_CHECK_EQ(recorder.finished, 1);
    TW_CHECK(recorder.completed);
    TW_CHECK_EQ(recorder.error.toStdString(), std::string{});
    TW_CHECK_EQ(recorder.output.toStdString(), std::string{"120"});
    TW_CHECK_EQ(recorder.pause_lines.size(), 0);
    TW_CHECK_EQ(recorder.resumed, 0);
    TW_CHECK(!runner.is_paused());
}

void test_syntax_error_finishes_with_a_message() {
    announce("reader error reported");
    QCoreApplication& app = *QCoreApplication::instance();
    DebuggerRunner runner;
    Recorder recorder;
    attach(runner, recorder);

    runner.load_program(QString::fromUtf8("(display (+ 1"));
    runner.start();

    TW_CHECK(spin_until(app, [&recorder] { return recorder.finished > 0; }));
    TW_CHECK(runner.wait(5000));
    TW_CHECK(!recorder.completed);
    TW_CHECK(!recorder.error.isEmpty());
    TW_CHECK_EQ(recorder.error.contains("closing"), true);
}

void test_runtime_error_pauses_when_asked() {
    announce("break on error");
    QCoreApplication& app = *QCoreApplication::instance();
    DebuggerRunner runner;
    Recorder recorder;
    attach(runner, recorder);

    // Calling a number is a *runtime* error, raised from inside the machine with
    // the frame stack populated -- a different path from a reader error, which
    // happens before anything runs.
    runner.set_break_on_error(true);
    runner.load_program(QString::fromUtf8("(define (f n) (n))\n(f 1)\n"));
    runner.start();

    TW_CHECK(spin_until(app, [&recorder] {
        return !recorder.pause_lines.isEmpty();
    }));
    TW_CHECK(runner.is_paused());
    // Held open rather than finished: the point is to inspect the failure.
    TW_CHECK_EQ(recorder.finished, 0);

    const DebugSnapshot snapshot = runner.snapshot();
    TW_CHECK(snapshot.cause == PauseCause::Error);
    TW_CHECK(!snapshot.error.isEmpty());
    TW_CHECK(!snapshot.procedure.isEmpty());
    // Note the ordering: the pause arrives *before* the completion signal, so the
    // recorder has no error to compare against yet.  That is the intended
    // sequence -- the failure is inspectable before the session is torn down --
    // and the cross-check below happens once the run has actually finished.
    TW_CHECK_EQ(recorder.error.toStdString(), std::string{});

    runner.request_continue();
    TW_CHECK(spin_until(app, [&recorder] { return recorder.finished > 0; }));
    TW_CHECK(runner.wait(5000));
    TW_CHECK(!recorder.completed);
    TW_CHECK(!recorder.error.isEmpty());
    // The snapshot and the completion signal must agree on what went wrong.
    TW_CHECK_EQ(snapshot.error.toStdString(), recorder.error.toStdString());
    TW_CHECK(!runner.is_paused());
    TW_CHECK_EQ(recorder.resumed, 1);
}

// ---------------------------------------------------------------------------
// Breakpoints
// ---------------------------------------------------------------------------

void test_breakpoint_pauses_and_the_stack_is_named() {
    announce("breakpoint + snapshot");
    QCoreApplication& app = *QCoreApplication::instance();
    DebuggerRunner runner;
    Recorder recorder;
    attach(runner, recorder);

    runner.set_breakpoints(QVector<int>{4});
    runner.load_program(QString::fromUtf8(kFact));
    runner.start();

    TW_CHECK(spin_until(app, [&recorder] {
        return !recorder.pause_lines.isEmpty();
    }));
    TW_CHECK(runner.is_paused());

    const DebugSnapshot snapshot = runner.snapshot();
    TW_CHECK_EQ(snapshot.line, 4);
    TW_CHECK_EQ(snapshot.last_line, 4);
    TW_CHECK_EQ(snapshot.depth, 1);
    TW_CHECK(snapshot.cause == PauseCause::Breakpoint);
    TW_CHECK_EQ(snapshot.procedure.toStdString(), std::string{"fact"});

    // <program> plus the live activation.
    TW_CHECK_EQ(snapshot.stack.size(), 2);
    TW_CHECK_EQ(snapshot.stack[0].procedure.toStdString(), std::string{"<program>"});
    TW_CHECK_EQ(snapshot.stack[0].line, 5);
    TW_CHECK_EQ(snapshot.stack[1].procedure.toStdString(), std::string{"fact"});
    // The frame points at the procedure's body, which opens on line 2 -- not at
    // the `define` on line 1 that the body list technically spans.
    TW_CHECK_EQ(snapshot.stack[1].line, 2);

    TW_CHECK(snapshot.error.isEmpty());
    TW_CHECK(has_variable(snapshot, QStringLiteral("n"), QStringLiteral("5")));
    TW_CHECK(has_variable_named(snapshot, QStringLiteral("fact")));

    // Line 4 runs once per recursion level, for n = 5 down to 1.
    std::size_t guard = 0;
    while (recorder.finished == 0 && guard < 30) {
        const int before = static_cast<int>(recorder.pause_lines.size());
        runner.request_continue();
        TW_CHECK(spin_until(app, [&recorder, before] {
            return recorder.finished > 0 ||
                   static_cast<int>(recorder.pause_lines.size()) > before;
        }));
        ++guard;
    }
    TW_CHECK(spin_until(app, [&recorder] { return recorder.finished > 0; }));
    TW_CHECK(runner.wait(5000));

    // Three forms open on line 4 -- `(* n (fact (- n 1)))`, `(fact (- n 1))`
    // and `(- n 1)` -- and the recursion goes five deep, so the breakpoint fires
    // fifteen times: three stops per level, each on a genuinely different form.
    // The alternative is to stop only on the first form of the line, which needs
    // a reliable "execution returned to this line" signal the evaluator cannot
    // always provide.
    TW_CHECK_EQ(recorder.pause_lines.size(), 15);
    TW_CHECK_EQ(recorder.pause_depths.size(), 15);
    for (int index = 0; index < recorder.pause_depths.size(); ++index) {
        TW_CHECK_EQ(recorder.pause_lines[index], 4);
        TW_CHECK_EQ(recorder.pause_depths[index], index / 3 + 1);
        TW_CHECK_EQ(recorder.pause_procedures[index].toStdString(),
                    std::string{"fact"});
    }
    TW_CHECK_EQ(recorder.resumed, 15);
    TW_CHECK(recorder.completed);
    TW_CHECK_EQ(recorder.output.toStdString(), std::string{"120"});
}

void test_breakpoints_can_be_added_and_removed_mid_run() {
    announce("breakpoints change mid-run");
    QCoreApplication& app = *QCoreApplication::instance();
    DebuggerRunner runner;
    Recorder recorder;
    attach(runner, recorder);

    // No breakpoints at first: the program starts and gets going.
    runner.load_program(QString::fromUtf8(kTick));
    runner.start();

    TW_CHECK(spin_until(app, [&recorder] {
        return recorder.output.contains(QStringLiteral("go"));
    }));
    TW_CHECK_EQ(recorder.pause_lines.size(), 0);

    // Adding one while it is running has to take effect, which is what the
    // generation counter is for.
    runner.set_breakpoints(QVector<int>{2});
    TW_CHECK(spin_until(app, [&recorder] {
        return !recorder.pause_lines.isEmpty();
    }));
    TW_CHECK_EQ(last_line(recorder), 2);
    TW_CHECK_EQ(last_depth(recorder), 1);

    // And removing it again lets the program run to the end, with no further
    // stops -- which is the half of this that a stale cache would break.
    const int pauses_before_removal =
        static_cast<int>(recorder.pause_lines.size());
    runner.set_breakpoints(QVector<int>{});
    runner.request_continue();
    TW_CHECK(spin_until(app, [&recorder] { return recorder.finished > 0; },
                        std::chrono::milliseconds{30000}));
    TW_CHECK(runner.wait(5000));
    TW_CHECK_EQ(static_cast<int>(recorder.pause_lines.size()),
                pauses_before_removal);
    TW_CHECK(recorder.completed);
}

// ---------------------------------------------------------------------------
// Stepping
// ---------------------------------------------------------------------------

void test_step_into_descends_into_calls() {
    announce("step into");
    QCoreApplication& app = *QCoreApplication::instance();
    DebuggerRunner runner;
    Recorder recorder;
    attach(runner, recorder);

    runner.set_breakpoints(QVector<int>{4});
    runner.load_program(QString::fromUtf8(kFact));
    runner.start();
    TW_CHECK(wait_for_pause(app, recorder, 0));
    TW_CHECK_EQ(last_depth(recorder), 1);

    // Stepping into the recursive call must show deeper activations; that is the
    // only reason to have a step-into at all.
    int deepest = last_depth(recorder);
    for (int step = 0; step < 40 && deepest < 3 && recorder.finished == 0; ++step) {
        const int before = static_cast<int>(recorder.pause_lines.size());
        runner.request_step_into();
        TW_CHECK(spin_until(app, [&recorder, before] {
            return recorder.finished > 0 ||
                   static_cast<int>(recorder.pause_lines.size()) > before;
        }));
        if (recorder.finished == 0) {
            deepest = std::max(deepest, last_depth(recorder));
            // The step test runs before the breakpoint test, so while stepping
            // it is always the step that wins.  A breakpoint set on this line is
            // still in place and must not be what stopped us.
            TW_CHECK(runner.snapshot().cause == PauseCause::Step);
        }
    }
    TW_CHECK(deepest >= 3);

    runner.request_stop();
    TW_CHECK(spin_until(app, [&recorder] { return recorder.finished > 0; }));
    TW_CHECK(runner.wait(5000));
}

void test_step_over_never_descends() {
    announce("step over");
    QCoreApplication& app = *QCoreApplication::instance();
    DebuggerRunner runner;
    Recorder recorder;
    attach(runner, recorder);

    runner.set_breakpoints(QVector<int>{4});
    runner.load_program(QString::fromUtf8(kFact));
    runner.start();
    TW_CHECK(wait_for_pause(app, recorder, 0));
    TW_CHECK_EQ(last_depth(recorder), 1);

    // Over-stepping stays at the same depth by construction, so the recursion is
    // never entered.  Stepping over the whole program also has to produce the
    // right answer -- a debugger that changes the answer is worse than none.
    int deepest = last_depth(recorder);
    for (int step = 0; step < 40 && recorder.finished == 0; ++step) {
        const int before = static_cast<int>(recorder.pause_lines.size());
        runner.request_step_over();
        if (!spin_until(app, [&recorder, before] {
                return recorder.finished > 0 ||
                       static_cast<int>(recorder.pause_lines.size()) > before;
            })) {
            break;
        }
        if (recorder.finished == 0) {
            deepest = std::max(deepest, last_depth(recorder));
            TW_CHECK(runner.snapshot().cause == PauseCause::Step);
        }
    }
    TW_CHECK(spin_until(app, [&recorder] { return recorder.finished > 0; }));
    TW_CHECK(runner.wait(5000));

    TW_CHECK_EQ(deepest, 1);
    TW_CHECK(recorder.completed);
    TW_CHECK_EQ(recorder.output.toStdString(), std::string{"120"});
}

void test_step_out_returns_to_the_caller() {
    announce("step out");
    QCoreApplication& app = *QCoreApplication::instance();
    DebuggerRunner runner;
    Recorder recorder;
    attach(runner, recorder);

    runner.set_breakpoints(QVector<int>{1});
    runner.load_program(QString::fromUtf8(kNested));
    runner.start();

    // Line 1 is `inner`'s body, but it is also where the top-level
    // `(define (inner x) ...)` opens, so the first arrival is that definition at
    // depth 0.  Continue until the breakpoint lands inside the call.
    TW_CHECK(wait_for_pause(app, recorder, 0));
    TW_CHECK_EQ(last_depth(recorder), 0);
    int guard = 0;
    while (guard < 10 && last_depth(recorder) != 2) {
        ++guard;
        const int before = static_cast<int>(recorder.pause_lines.size());
        runner.request_continue();
        TW_CHECK(spin_until(app, [&recorder, before] {
            return recorder.finished > 0 ||
                   static_cast<int>(recorder.pause_lines.size()) > before;
        }));
        if (recorder.finished > 0) {
            break;
        }
    }
    TW_CHECK_EQ(last_depth(recorder), 2);

    const DebugSnapshot inside = runner.snapshot();
    TW_CHECK_EQ(inside.procedure.toStdString(), std::string{"inner"});
    TW_CHECK_EQ(inside.stack.size(), 3);

    const int before = static_cast<int>(recorder.pause_lines.size());
    runner.request_step_out();
    TW_CHECK(spin_until(app, [&recorder, before] {
        return recorder.finished > 0 ||
               static_cast<int>(recorder.pause_lines.size()) > before;
    }));
    TW_CHECK_EQ(recorder.finished, 0);

    const DebugSnapshot after = runner.snapshot();
    TW_CHECK_EQ(after.depth, 1);
    TW_CHECK_EQ(after.line, 4);
    TW_CHECK_EQ(after.procedure.toStdString(), std::string{"outer"});

    runner.request_continue();
    TW_CHECK(spin_until(app, [&recorder] { return recorder.finished > 0; }));
    TW_CHECK(runner.wait(5000));
    TW_CHECK(recorder.completed);
    TW_CHECK_EQ(recorder.output.toStdString(), std::string{"11"});
}

// ---------------------------------------------------------------------------
// Pause / stop
// ---------------------------------------------------------------------------

void test_pause_while_running_then_stop() {
    announce("pause then stop");
    QCoreApplication& app = *QCoreApplication::instance();
    DebuggerRunner runner;
    Recorder recorder;
    attach(runner, recorder);

    runner.load_program(QString::fromUtf8(kSpin));
    runner.start();

    // Wait for proof that it is genuinely mid-program before asking it to stop.
    TW_CHECK(spin_until(app, [&recorder] {
        return recorder.output.contains(QStringLiteral("started"));
    }));

    runner.request_pause();
    TW_CHECK(spin_until(app, [&recorder] {
        return !recorder.pause_lines.isEmpty();
    }));
    TW_CHECK(runner.is_paused());
    TW_CHECK(runner.snapshot().cause == PauseCause::PauseRequest);
    TW_CHECK_EQ(recorder.finished, 0);

    runner.request_stop();
    TW_CHECK(spin_until(app, [&recorder] { return recorder.finished > 0; }));
    TW_CHECK(runner.wait(5000));
    TW_CHECK(!recorder.completed);
    TW_CHECK(!runner.is_paused());
}

void test_pause_while_paused_does_not_unpause() {
    announce("pause while paused");
    QCoreApplication& app = *QCoreApplication::instance();
    DebuggerRunner runner;
    Recorder recorder;
    attach(runner, recorder);

    runner.load_program(QString::fromUtf8(kSpin));
    runner.start();
    TW_CHECK(spin_until(app, [&recorder] {
        return recorder.output.contains(QStringLiteral("started"));
    }));
    runner.request_pause();
    TW_CHECK(spin_until(app, [&recorder] {
        return !recorder.pause_lines.isEmpty();
    }));

    const int resumptions = recorder.resumed;
    for (int index = 0; index < 5; ++index) {
        runner.request_pause();
    }
    // A pause while already paused asks for nothing.  Returning from the wait
    // would silently un-pause the program, which is exactly the bug this guards.
    (void)spin_until(app, [] { return false; }, std::chrono::milliseconds{80});
    TW_CHECK(runner.is_paused());
    TW_CHECK_EQ(recorder.resumed, resumptions);
    TW_CHECK_EQ(recorder.finished, 0);

    runner.request_stop();
    TW_CHECK(spin_until(app, [&recorder] { return recorder.finished > 0; }));
    TW_CHECK(runner.wait(5000));
}

void test_stop_requested_before_the_run_starts_is_not_lost() {
    announce("early stop is not lost");
    QCoreApplication& app = *QCoreApplication::instance();
    DebuggerRunner runner;
    Recorder recorder;
    attach(runner, recorder);

    runner.load_program(QString::fromUtf8(kSpin));
    // Posted before the thread exists: the command must still be honoured rather
    // than dropped on the floor, which a naive "check a flag" design would do.
    runner.request_stop();
    runner.start();

    TW_CHECK(spin_until(app, [&recorder] { return recorder.finished > 0; }));
    TW_CHECK(runner.wait(5000));
    TW_CHECK(!recorder.completed);
    TW_CHECK_EQ(recorder.error.toStdString(), std::string{});
}

void test_destroying_a_running_runner_joins() {
    announce("destructor joins a live run");
    QCoreApplication& app = *QCoreApplication::instance();
    Recorder recorder;
    auto* runner = new DebuggerRunner{};
    attach(*runner, recorder);
    runner->load_program(QString::fromUtf8(kSpin));
    runner->start();

    TW_CHECK(spin_until(app, [&recorder] {
        return recorder.output.contains(QStringLiteral("started"));
    }));

    // The destructor has to stop the program and join.  If it merely set a flag,
    // or if the wakeup were lost, this would hang, and Qt would abort on a
    // QThread destroyed while still running.
    delete runner;
}

// ---------------------------------------------------------------------------
// Concurrency
// ---------------------------------------------------------------------------

void test_command_storm_from_another_thread() {
    announce("command storm");
    QCoreApplication& app = *QCoreApplication::instance();
    DebuggerRunner runner;
    Recorder recorder;
    attach(runner, recorder);

    runner.set_breakpoints(QVector<int>{2});
    runner.load_program(QString::fromUtf8(kTick));
    runner.start();
    TW_CHECK(spin_until(app, [&recorder] {
        return !recorder.pause_lines.isEmpty();
    }));

    // Hammer the channel from a thread that is not the runner and not the GUI.
    // The only requirement is that nothing is lost, nothing deadlocks, and the
    // program can still be stopped afterwards.
    std::thread spammer{[&runner] {
        for (int index = 0; index < 3000; ++index) {
            runner.request_pause();
            runner.request_continue();
            runner.request_step_over();
            runner.request_continue();
        }
    }};
    spammer.join();

    TW_CHECK(spin_until(app, [&recorder] {
        return !recorder.pause_lines.isEmpty();
    }));

    runner.request_stop();
    TW_CHECK(spin_until(app, [&recorder] { return recorder.finished > 0; },
                        std::chrono::milliseconds{30000}));
    TW_CHECK(runner.wait(5000));
    TW_CHECK(!runner.is_paused());
}

}  // namespace

int main(int argc, char** argv) {
    QCoreApplication app{argc, argv};

    test_runs_to_completion();
    test_syntax_error_finishes_with_a_message();
    test_runtime_error_pauses_when_asked();

    test_breakpoint_pauses_and_the_stack_is_named();
    test_breakpoints_can_be_added_and_removed_mid_run();

    test_step_into_descends_into_calls();
    test_step_over_never_descends();
    test_step_out_returns_to_the_caller();

    test_pause_while_running_then_stop();
    test_pause_while_paused_does_not_unpause();
    test_stop_requested_before_the_run_starts_is_not_lost();
    test_destroying_a_running_runner_joins();

    test_command_storm_from_another_thread();
    return tw::test::summary("debugger_runner");
}
