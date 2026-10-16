// SPDX-License-Identifier: MIT
//
// Integration tests for the Qt bridge.  These need a real Qt event loop and a
// real second thread, because the whole point of the bridge is the handshake
// between them -- a single-threaded test would prove nothing.
//
// Built only when Qt6 is found; see native/CMakeLists.txt.

#include <atomic>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <functional>
#include <string>
#include <thread>
#include <vector>

#include <QCoreApplication>
#include <QObject>

#include "check.hpp"
#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/canvas/host_adapter.hpp"
#include "timewarp/canvas/qt_bridge.hpp"

namespace {

using timewarp::canvas::accepted;
using timewarp::canvas::CanvasCommandKind;
using timewarp::canvas::CanvasCommandQueue;
using timewarp::canvas::CanvasCommandView;
using timewarp::canvas::make_motion;
using timewarp::canvas::qt::CanvasBridge;

/// Pumps the Qt event loop until `done()` or the timeout.
bool spin_until(QCoreApplication& app, const std::function<bool()>& done,
                std::chrono::milliseconds timeout) {
    const auto deadline = std::chrono::steady_clock::now() + timeout;
    while (!done()) {
        if (std::chrono::steady_clock::now() >= deadline) {
            return false;
        }
        app.processEvents();
        std::this_thread::sleep_for(std::chrono::milliseconds{1});
    }
    return true;
}

// ---------------------------------------------------------------------------

void test_wakeup_crosses_threads_and_preserves_order() {
    QCoreApplication& app = *QCoreApplication::instance();
    CanvasCommandQueue queue;
    CanvasBridge bridge{queue};

    const std::thread::id gui_thread = std::this_thread::get_id();
    std::vector<double> seen;
    std::atomic<bool> sink_stayed_on_gui_thread{true};

    bridge.set_sink([&](CanvasCommandView view) {
        if (std::this_thread::get_id() != gui_thread) {
            sink_stayed_on_gui_thread.store(false, std::memory_order_release);
        }
        seen.push_back(view.command.x);
    });

    constexpr std::size_t kCount = 2000;
    std::thread producer{[&] {
        for (std::size_t index = 0; index < kCount; ++index) {
            const auto result = queue.push(make_motion(
                CanvasCommandKind::Forward, static_cast<double>(index)));
            if (!accepted(result)) {
                break;
            }
            bridge.wake();
        }
        queue.flush();
    }};

    const bool arrived = spin_until(app, [&] { return seen.size() >= kCount; },
                                    std::chrono::milliseconds{10000});
    producer.join();

    TW_CHECK(arrived);
    TW_CHECK(sink_stayed_on_gui_thread.load(std::memory_order_acquire));
    TW_CHECK_EQ(seen.size(), kCount);
    bool ordered = true;
    for (std::size_t index = 0; index < seen.size(); ++index) {
        if (seen[index] != static_cast<double>(index)) {
            ordered = false;
            break;
        }
    }
    TW_CHECK(ordered);
}

void test_a_wake_storm_costs_one_event_post() {
    QCoreApplication& app = *QCoreApplication::instance();
    CanvasCommandQueue queue;
    CanvasBridge bridge{queue};

    int batches = 0;
    std::size_t applied = 0;
    QObject::connect(&bridge, &CanvasBridge::batch_applied,
                     [&batches](int) { ++batches; });
    bridge.set_sink([&applied](CanvasCommandView) { ++applied; });

    // The GUI thread deliberately does not pump while the producer runs, so the
    // gate can hand out a single post for the whole storm.  That constrains the
    // storm: with a blocking producer and no consumer, pushing more than the ring
    // holds would deadlock *by design*.  Half the capacity keeps this robust if
    // the default ever changes.
    const std::size_t kCount =
        queue.capacity() / sizeof(timewarp::canvas::CanvasCommand) / 2;
    std::thread producer{[&] {
        for (std::size_t index = 0; index < kCount; ++index) {
            queue.push(make_motion(CanvasCommandKind::Forward,
                                   static_cast<double>(index)));
            bridge.wake();
        }
        queue.flush();
    }};
    producer.join();

    app.processEvents();

    TW_CHECK_EQ(applied, kCount);
    // This is the entire motivation for the class: 5,000 commands, one wakeup.
    TW_CHECK(batches <= 2);
    TW_CHECK(bridge.drain_pending() == false);
}

void test_no_wakeup_is_lost_when_producer_and_consumer_race() {
    // The interesting case for the handshake: the consumer keeps draining while
    // the producer keeps pushing and waking, so the flag is repeatedly cleared
    // and re-set underneath the producer.
    QCoreApplication& app = *QCoreApplication::instance();
    CanvasCommandQueue queue;
    CanvasBridge bridge{queue};

    std::vector<double> seen;
    bridge.set_sink([&seen](CanvasCommandView view) {
        seen.push_back(view.command.x);
    });

    constexpr std::size_t kCount = 20000;
    std::atomic<bool> producer_done{false};
    std::thread producer{[&] {
        for (std::size_t index = 0; index < kCount; ++index) {
            if (!accepted(queue.push(make_motion(
                    CanvasCommandKind::Forward, static_cast<double>(index))))) {
                break;
            }
            bridge.wake();
        }
        queue.flush();
        producer_done.store(true, std::memory_order_release);
    }};

    // Pump continuously, so every drain races the producer.
    const bool arrived = spin_until(
        app,
        [&] {
            return seen.size() >= kCount ||
                   (producer_done.load(std::memory_order_acquire) &&
                    seen.size() >= queue.applied_count());
        },
        std::chrono::milliseconds{20000});
    producer.join();
    // One last pump to pick up anything still queued.
    spin_until(app, [&] { return queue.empty(); },
               std::chrono::milliseconds{5000});

    TW_CHECK(arrived);
    TW_CHECK_EQ(seen.size(), kCount);
    bool ordered = true;
    for (std::size_t index = 0; index < seen.size(); ++index) {
        if (seen[index] != static_cast<double>(index)) {
            ordered = false;
            break;
        }
    }
    TW_CHECK(ordered);
}

void test_poll_interval_drains_without_a_wake() {
    QCoreApplication& app = *QCoreApplication::instance();
    CanvasCommandQueue queue;
    CanvasBridge bridge{queue};

    std::size_t applied = 0;
    bridge.set_sink([&applied](CanvasCommandView) { ++applied; });
    bridge.set_poll_interval(std::chrono::milliseconds{5});

    // Deliberately no wake(): the timer is the safety net.
    queue.push(make_motion(CanvasCommandKind::Forward, 7.0));

    const bool arrived = spin_until(app, [&] { return applied >= 1; },
                                    std::chrono::milliseconds{5000});
    TW_CHECK(arrived);
    bridge.set_poll_interval(std::chrono::milliseconds{0});
}

void test_close_signals_completion() {
    QCoreApplication& app = *QCoreApplication::instance();
    CanvasCommandQueue queue;
    CanvasBridge bridge{queue};

    int completions = 0;
    std::size_t applied = 0;
    QObject::connect(&bridge, &CanvasBridge::drained_and_closed,
                     [&completions] { ++completions; });
    bridge.set_sink([&applied](CanvasCommandView) { ++applied; });

    queue.push(make_motion(CanvasCommandKind::Forward, 1.0));
    queue.push(make_motion(CanvasCommandKind::Forward, 2.0));
    queue.close();
    bridge.wake();

    const bool finished = spin_until(app, [&] { return completions >= 1; },
                                     std::chrono::milliseconds{5000});
    TW_CHECK(finished);
    // Commands queued before the close are still applied: the final frame draws.
    TW_CHECK_EQ(applied, std::size_t{2});
}

void test_interpreter_host_bridges_turtle_calls() {
    // End to end: scheme::Host turtle calls become queued commands, and the
    // bridge applies them on the GUI thread.
    QCoreApplication& app = *QCoreApplication::instance();
    CanvasCommandQueue queue;
    CanvasBridge bridge{queue};

    std::vector<CanvasCommandKind> kinds;
    bridge.set_sink([&kinds](CanvasCommandView view) {
        kinds.push_back(view.command.kind);
    });

    timewarp::canvas::QueuedTurtleHost host{queue};
    host.set_wakeup([&bridge] { bridge.wake(); });

    std::thread interpreter{[&] {
        host.forward(10.0);
        host.right(90.0);
        host.set_pen_width(3.0);
        host.set_color(255, 0, 0);
        host.forward(5.0);
        host.pen_up();
        host.home();
        host.flush();  // publish the trailing coalesced state
    }};

    const bool arrived = spin_until(app, [&] { return kinds.size() >= 7; },
                                    std::chrono::milliseconds{5000});
    interpreter.join();

    TW_CHECK(arrived);
    TW_CHECK_EQ(host.rejected(), std::uint64_t{0});
    TW_CHECK_EQ(kinds.size(), std::size_t{7});
    TW_CHECK_EQ(kinds[0], CanvasCommandKind::Forward);
    TW_CHECK_EQ(kinds[1], CanvasCommandKind::Right);
    TW_CHECK_EQ(kinds[2], CanvasCommandKind::SetPenWidth);
    TW_CHECK_EQ(kinds[3], CanvasCommandKind::SetPenColor);
    TW_CHECK_EQ(kinds[4], CanvasCommandKind::Forward);
    TW_CHECK_EQ(kinds[5], CanvasCommandKind::PenUp);
    TW_CHECK_EQ(kinds[6], CanvasCommandKind::Home);
}

void test_host_reports_rejected_commands_after_close() {
    CanvasCommandQueue queue;
    queue.close();
    timewarp::canvas::QueuedTurtleHost host{queue};
    host.forward(1.0);
    host.forward(1.0);
    TW_CHECK_EQ(host.submitted(), std::uint64_t{2});
    TW_CHECK_EQ(host.rejected(), std::uint64_t{2});
    TW_CHECK(host.queue_closed());
}

}  // namespace

/// Names each test and flushes first, so a hang points at its cause.
#define RUN(fn)                                       \
    do {                                              \
        std::printf("  %-58s", #fn);                  \
        std::fflush(stdout);                          \
        fn();                                         \
        std::printf("ok\n");                          \
    } while (false)

int main(int argc, char** argv) {
    QCoreApplication app{argc, argv};

    RUN(test_wakeup_crosses_threads_and_preserves_order);
    RUN(test_a_wake_storm_costs_one_event_post);
    RUN(test_no_wakeup_is_lost_when_producer_and_consumer_race);
    RUN(test_poll_interval_drains_without_a_wake);
    RUN(test_close_signals_completion);
    RUN(test_interpreter_host_bridges_turtle_calls);
    RUN(test_host_reports_rejected_commands_after_close);

    return tw::test::summary("canvas_bridge");
}
