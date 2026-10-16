// SPDX-License-Identifier: MIT
//
// Measures the canvas command queue: raw throughput, and the two behaviours
// that motivate it over "one Qt signal per turtle call".
//
//   ./native/build/canvas_queue_bench

#include <atomic>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <thread>
#include <vector>

#include "timewarp/canvas/command.hpp"
#include "timewarp/canvas/command_queue.hpp"

namespace {

using Clock = std::chrono::steady_clock;
using timewarp::canvas::CanvasCommandKind;
using timewarp::canvas::CanvasCommandQueue;
using timewarp::canvas::CanvasCommandView;
using timewarp::canvas::make_motion;

[[nodiscard]] double millis_since(Clock::time_point start) {
    return std::chrono::duration<double, std::milli>(Clock::now() - start)
        .count();
}

[[nodiscard]] double per_command_ns(double milliseconds,
                                    std::size_t count) {
    return milliseconds * 1e6 / static_cast<double>(count);
}

/// Single thread, so this isolates the queue's own cost from scheduling.
void report_single_threaded(std::size_t count) {
    CanvasCommandQueue queue;
    std::size_t applied = 0;
    const auto sink = [&applied](CanvasCommandView) { ++applied; };

    const Clock::time_point start = Clock::now();
    for (std::size_t index = 0; index < count; ++index) {
        queue.push(make_motion(CanvasCommandKind::Forward,
                               static_cast<double>(index)));
        if (index % 512 == 511) {
            queue.drain(sink);
        }
    }
    queue.drain(sink);
    const double elapsed = millis_since(start);

    std::printf("%-34s %8.2f M/s %9.2f ns/cmd  (applied %zu)\n",
                "single-threaded push + drain",
                static_cast<double>(count) / elapsed / 1000.0,
                per_command_ns(elapsed, count), applied);
}

/// The real configuration: one producer thread, one consumer thread, a ring far
/// too small to hold the whole run, so the producer genuinely blocks and wakes.
void report_spsc(std::size_t count) {
    CanvasCommandQueue queue;
    std::atomic<bool> done{false};

    const Clock::time_point start = Clock::now();
    std::thread producer{[&] {
        for (std::size_t index = 0; index < count; ++index) {
            queue.push(make_motion(CanvasCommandKind::Forward,
                                   static_cast<double>(index)));
        }
        queue.flush();
        done.store(true, std::memory_order_release);
    }};

    std::size_t applied = 0;
    const auto sink = [&applied](CanvasCommandView) { ++applied; };
    while (applied < count) {
        if (queue.drain(sink) != 0) {
            continue;
        }
        if (done.load(std::memory_order_acquire) && queue.empty()) {
            break;
        }
        queue.wait_for_data(std::chrono::milliseconds{50});
    }
    producer.join();
    const double elapsed = millis_since(start);

    std::printf("%-34s %8.2f M/s %9.2f ns/cmd  (applied %zu, blocked %llu)\n",
                "SPSC through a small ring",
                static_cast<double>(applied) / elapsed / 1000.0,
                per_command_ns(elapsed, applied), applied,
                static_cast<unsigned long long>(queue.blocked_count()));
}

/// How many commands arrive per *published* record.  The Qt bridge turns each
/// published record into at most one event-loop post, so this ratio is directly
/// the reduction in marshalling cost.
void report_coalescing(std::size_t count) {
    CanvasCommandQueue queue;
    std::size_t delivered = 0;

    const Clock::time_point start = Clock::now();
    for (std::size_t index = 0; index < count; ++index) {
        // A turtle heading that changes every step but draws nothing: the case
        // coalescing exists for.
        queue.push(make_motion(CanvasCommandKind::SetHeading,
                               static_cast<double>(index)));
    }
    queue.flush();
    queue.drain([&delivered](CanvasCommandView) { ++delivered; });
    const double elapsed = millis_since(start);

    std::printf("%-34s %zu pushes -> %zu record(s), %llu coalesced\n",
                "coalescing (state only)", count, delivered,
                static_cast<unsigned long long>(queue.coalesced_count()));
    std::printf("%-34s %8.2f M/s %9.2f ns/push\n", "",
                static_cast<double>(count) / elapsed / 1000.0,
                per_command_ns(elapsed, count));
}

/// Geometry must never collapse, so this is the cost when nothing can be
/// coalesced -- the worst case.
///
/// The consumer drains as it goes, because that is what a live GUI does.  Left
/// undrained this would fill the ring and park the producer, which is the
/// documented consequence of "never drop" rather than a defect.
void report_no_coalescing(std::size_t count) {
    CanvasCommandQueue queue;
    std::size_t delivered = 0;
    const auto sink = [&delivered](CanvasCommandView) { ++delivered; };

    const Clock::time_point start = Clock::now();
    for (std::size_t index = 0; index < count; ++index) {
        queue.push(make_motion(CanvasCommandKind::Forward,
                               static_cast<double>(index)));
        if (index % 1024 == 1023) {
            queue.drain(sink);
        }
    }
    queue.drain(sink);
    const double elapsed = millis_since(start);

    std::printf("%-34s %zu pushes -> %zu record(s), %llu coalesced\n",
                "no coalescing (geometry)", count, delivered,
                static_cast<unsigned long long>(queue.coalesced_count()));
    std::printf("%-34s %8.2f M/s %9.2f ns/push\n", "",
                static_cast<double>(count) / elapsed / 1000.0,
                per_command_ns(elapsed, count));
}

}  // namespace

int main() {
    std::printf("Canvas command queue (lock-free SPSC ring, %zu-byte records)\n\n",
                sizeof(timewarp::canvas::CanvasCommand));

    report_single_threaded(2000000);
    report_spsc(2000000);
    std::printf("\n");
    report_coalescing(1000000);
    report_no_coalescing(1000000);

    std::printf("\n");
    return 0;
}
