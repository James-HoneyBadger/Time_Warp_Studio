// SPDX-License-Identifier: MIT
//
// Tests for the canvas command queue.  No Qt, no display: the core is
// deliberately testable without either.
//
// The interesting cases are the ones the design exists to get right:
//   * nothing is ever lost, including under a full ring;
//   * coalescing cannot reorder or drop geometry;
//   * a packed queue cannot deadlock a shutdown.

#include <atomic>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <string>
#include <thread>
#include <type_traits>
#include <vector>

#include "check.hpp"
#include "timewarp/canvas/command.hpp"
#include "timewarp/canvas/command_queue.hpp"

namespace {

using timewarp::canvas::accepted;
using timewarp::canvas::CanvasCommand;
using timewarp::canvas::CanvasCommandKind;
using timewarp::canvas::CanvasCommandQueue;
using timewarp::canvas::CanvasCommandView;
using timewarp::canvas::make_command;
using timewarp::canvas::make_draw_color;
using timewarp::canvas::make_motion;
using timewarp::canvas::make_polyline;
using timewarp::canvas::make_rgb;
using timewarp::canvas::PushResult;

/// Records what the consumer observed, so order and payload can be checked.
struct Recorder {
    struct Item {
        CanvasCommandKind kind;
        double x;
        std::string text;
    };
    std::vector<Item> items;

    void operator()(CanvasCommandView view) {
        items.push_back(Item{view.command.kind, view.command.x,
                             std::string{view.text}});
    }
};

std::size_t drain_all(CanvasCommandQueue& queue, Recorder& recorder) {
    std::size_t total = 0;
    for (;;) {
        const std::size_t applied = queue.drain(recorder);
        total += applied;
        if (applied == 0) {
            break;
        }
    }
    return total;
}

/// Pushes commands until the ring reports it is full.  Returns how many fitted.
///
/// Filling happens on the calling thread and finishes before any spawned
/// producer starts, so the single-producer contract holds even though two
/// different threads push over the test's lifetime.
std::size_t fill_queue(CanvasCommandQueue& queue) {
    std::size_t pushed = 0;
    while (true) {
        const std::size_t before = queue.bytes_buffered();
        const PushResult result =
            queue.push_for(make_motion(CanvasCommandKind::Forward, 1.0),
                           std::chrono::milliseconds{1});
        if (result == PushResult::TimedOut) {
            TW_CHECK(queue.bytes_buffered() >= before);
            return pushed;
        }
        TW_CHECK(accepted(result));
        ++pushed;
        if (pushed > 1000000) {
            return pushed;  // safety net; the ring is bounded so this is unreachable
        }
    }
}

// ---------------------------------------------------------------------------
// Value type
// ---------------------------------------------------------------------------

void test_command_layout() {
    TW_CHECK_EQ(sizeof(CanvasCommand), std::size_t{72});
    TW_CHECK(std::is_trivially_copyable_v<CanvasCommand>);
    TW_CHECK_EQ(alignof(CanvasCommand), std::size_t{8});
}

void test_colour_packing() {
    const CanvasCommand command =
        make_draw_color(CanvasCommandKind::SetPenColor, 0x12, 0x34, 0x56);
    TW_CHECK_EQ(command.color, 0x123456u);
    TW_CHECK_EQ(command.red(), 0x12);
    TW_CHECK_EQ(command.green(), 0x34);
    TW_CHECK_EQ(command.blue(), 0x56);
    // Channels are masked, so an out-of-range channel cannot bleed into another.
    TW_CHECK_EQ(make_rgb(0x1FF, 0, 0), 0xFF0000u);
    TW_CHECK_EQ(make_rgb(-1, 0, 0), 0xFF0000u);
}

void test_coalescible_predicate() {
    TW_CHECK(timewarp::canvas::is_coalescible(CanvasCommandKind::SetHeading));
    TW_CHECK(timewarp::canvas::is_coalescible(CanvasCommandKind::SetPenWidth));
    // Geometry is never coalescible.
    TW_CHECK(!timewarp::canvas::is_coalescible(CanvasCommandKind::Forward));
    TW_CHECK(!timewarp::canvas::is_coalescible(CanvasCommandKind::DrawPolyline));
    TW_CHECK(!timewarp::canvas::is_coalescible(CanvasCommandKind::Circle));
    TW_CHECK(!timewarp::canvas::is_coalescible(CanvasCommandKind::Clear));
    // The pen pair is a toggle, not two assignments to one value.
    TW_CHECK(!timewarp::canvas::is_coalescible(CanvasCommandKind::PenUp));
    TW_CHECK(!timewarp::canvas::is_coalescible(CanvasCommandKind::PenDown));
}

// ---------------------------------------------------------------------------
// Round trips
// ---------------------------------------------------------------------------

void test_round_trip_preserves_order() {
    CanvasCommandQueue queue;
    Recorder recorder;

    for (std::size_t index = 0; index < 500; ++index) {
        const PushResult result = queue.push(make_motion(
            CanvasCommandKind::Forward, static_cast<double>(index)));
        TW_CHECK(accepted(result));
    }
    TW_CHECK_EQ(drain_all(queue, recorder), std::size_t{500});
    TW_CHECK_EQ(recorder.items.size(), std::size_t{500});

    bool ordered = true;
    for (std::size_t index = 0; index < recorder.items.size(); ++index) {
        if (recorder.items[index].kind != CanvasCommandKind::Forward ||
            recorder.items[index].x != static_cast<double>(index)) {
            ordered = false;
            break;
        }
    }
    TW_CHECK(ordered);
    TW_CHECK(queue.empty());
    TW_CHECK_EQ(queue.applied_count(), std::uint64_t{500});
}

void test_variable_length_payloads_survive_wraparound() {
    // A 16 KiB ring with a 176-byte record wraps many times over 600 rounds.
    CanvasCommandQueue queue{CanvasCommandQueue::kMinCapacityBytes};
    constexpr std::size_t kRounds = 600;

    std::size_t received = 0;
    for (std::size_t index = 0; index < kRounds; ++index) {
        std::vector<double> points(8);
        for (std::size_t k = 0; k < points.size(); ++k) {
            points[k] = static_cast<double>(k);
        }
        const std::string text = "record-" + std::to_string(index % 10);
        const CanvasCommand command = make_polyline(
            CanvasCommandKind::DrawPolyline, make_rgb(1, 2, 3),
            static_cast<double>(index), /*closed=*/true);

        const PushResult result = queue.push(command, text, points);
        TW_CHECK(accepted(result));

        queue.drain([&](CanvasCommandView view) {
            TW_CHECK_EQ(view.command.kind, CanvasCommandKind::DrawPolyline);
            TW_CHECK_EQ(view.point_count(), std::size_t{4});
            TW_CHECK_EQ(view.points.front(), 0.0);
            TW_CHECK_EQ(view.points.back(), 7.0);
            TW_CHECK_EQ(std::string{view.text}, text);
            TW_CHECK(view.command.is_closed());
            TW_CHECK_EQ(view.command.x, static_cast<double>(index));
            ++received;
        });
    }
    TW_CHECK_EQ(received, kRounds);
    TW_CHECK(queue.empty());
}

void test_text_longer_than_a_naive_inline_buffer() {
    CanvasCommandQueue queue;
    // 8 KiB of text: far past any fixed inline buffer, so it only works if the
    // record genuinely carries a variable-length payload.
    const std::string text(8192, 'x');
    const PushResult result = queue.push(
        make_command(CanvasCommandKind::DrawText), text);
    TW_CHECK(accepted(result));

    std::size_t seen = 0;
    queue.drain([&](CanvasCommandView view) {
        TW_CHECK_EQ(view.text.size(), text.size());
        TW_CHECK_EQ(std::string{view.text}, text);
        ++seen;
    });
    TW_CHECK_EQ(seen, std::size_t{1});
}

void test_record_larger_than_the_ring_is_rejected() {
    CanvasCommandQueue queue{CanvasCommandQueue::kMinCapacityBytes};
    std::vector<double> points((queue.capacity() / sizeof(double)) + 16, 1.0);
    const PushResult result = queue.push(
        make_polyline(CanvasCommandKind::DrawPolyline, 0, 1.0), {}, points);
    TW_CHECK_EQ(result, PushResult::TooLarge);
    TW_CHECK(!accepted(result));
    TW_CHECK(queue.empty());
}

// ---------------------------------------------------------------------------
// Coalescing
// ---------------------------------------------------------------------------

void test_adjacent_state_collapses_to_the_last_value() {
    CanvasCommandQueue queue;
    Recorder recorder;

    TW_CHECK_EQ(queue.push(make_motion(CanvasCommandKind::SetHeading, 90)),
                PushResult::Staged);
    TW_CHECK_EQ(queue.push(make_motion(CanvasCommandKind::SetHeading, 45)),
                PushResult::Coalesced);
    TW_CHECK_EQ(queue.push(make_motion(CanvasCommandKind::SetHeading, 30)),
                PushResult::Coalesced);
    TW_CHECK_EQ(queue.flush(), PushResult::Appended);

    TW_CHECK_EQ(queue.coalesced_count(), std::uint64_t{2});
    TW_CHECK_EQ(drain_all(queue, recorder), std::size_t{1});
    TW_CHECK_EQ(recorder.items.size(), std::size_t{1});
    TW_CHECK_EQ(recorder.items[0].kind, CanvasCommandKind::SetHeading);
    TW_CHECK_EQ(recorder.items[0].x, 30.0);
}

void test_coalescing_never_reorders_or_drops_geometry() {
    CanvasCommandQueue queue;
    Recorder recorder;

    queue.push(make_motion(CanvasCommandKind::SetHeading, 90));  // staged
    queue.push(make_motion(CanvasCommandKind::Forward, 10));     // publishes 90
    queue.push(make_motion(CanvasCommandKind::SetHeading, 45));  // staged
    queue.push(make_motion(CanvasCommandKind::SetHeading, 30));  // coalesced
    queue.push(make_motion(CanvasCommandKind::Forward, 20));     // publishes 30
    queue.push(make_motion(CanvasCommandKind::SetHeading, 1));   // staged
    queue.flush();

    TW_CHECK_EQ(drain_all(queue, recorder), std::size_t{5});
    TW_CHECK_EQ(recorder.items.size(), std::size_t{5});

    // Both `forward`s survive, and the first `set_heading` is *not* collapsed
    // across the intervening move -- collapsing it would change where the pen
    // travelled.
    const CanvasCommandKind expected_kinds[5] = {
        CanvasCommandKind::SetHeading, CanvasCommandKind::Forward,
        CanvasCommandKind::SetHeading, CanvasCommandKind::Forward,
        CanvasCommandKind::SetHeading};
    const double expected_values[5] = {90.0, 10.0, 30.0, 20.0, 1.0};
    bool matched = true;
    for (std::size_t index = 0; index < 5; ++index) {
        if (recorder.items[index].kind != expected_kinds[index] ||
            recorder.items[index].x != expected_values[index]) {
            matched = false;
            break;
        }
    }
    TW_CHECK(matched);
}

void test_drawing_commands_never_coalesce() {
    CanvasCommandQueue queue;
    Recorder recorder;
    for (std::size_t index = 0; index < 200; ++index) {
        // Identical arguments on purpose: even then, all 200 must be delivered,
        // because each one draws a separate segment.
        queue.push(make_motion(CanvasCommandKind::Forward, 1.0));
    }
    queue.flush();
    TW_CHECK_EQ(queue.coalesced_count(), std::uint64_t{0});
    TW_CHECK_EQ(drain_all(queue, recorder), std::size_t{200});
}

void test_staging_is_bounded_so_the_consumer_cannot_be_starved() {
    CanvasCommandQueue queue;
    queue.set_coalesce_limit(4);
    Recorder recorder;

    // 100 consecutive state changes; with a limit of 4 the consumer must see
    // intermediate values rather than only the final one.
    for (std::size_t index = 0; index < 100; ++index) {
        queue.push(make_motion(CanvasCommandKind::SetHeading,
                               static_cast<double>(index)));
    }
    queue.flush();

    const std::size_t count = drain_all(queue, recorder);
    TW_CHECK(count >= std::size_t{25});  // 100 / 4
    TW_CHECK(count <= std::size_t{100});
    // The final value is always the one that wins.
    TW_CHECK_EQ(recorder.items.back().x, 99.0);
}

void test_flush_with_nothing_staged_is_harmless() {
    CanvasCommandQueue queue;
    Recorder recorder;
    TW_CHECK_EQ(queue.flush(), PushResult::Appended);
    TW_CHECK_EQ(drain_all(queue, recorder), std::size_t{0});
    TW_CHECK(!queue.has_staged());
}

// ---------------------------------------------------------------------------
// Blocking and shutdown
// ---------------------------------------------------------------------------

void test_producer_blocks_until_space_frees() {
    CanvasCommandQueue queue{CanvasCommandQueue::kMinCapacityBytes};
    const std::size_t fitted = fill_queue(queue);
    TW_CHECK(fitted > 0);
    TW_CHECK(queue.bytes_buffered() > 0);

    std::atomic<bool> completed{false};
    std::atomic<bool> accepted_result{false};
    std::thread producer{[&] {
        const PushResult result = queue.push(
            make_motion(CanvasCommandKind::Forward, 42.0));
        accepted_result.store(accepted(result), std::memory_order_release);
        completed.store(true, std::memory_order_release);
    }};

    // The ring is full, so the producer must be parked, not spinning to success.
    std::this_thread::sleep_for(std::chrono::milliseconds{50});
    TW_CHECK(!completed.load(std::memory_order_acquire));
    TW_CHECK(queue.blocked_count() > 0);

    // Draining frees space and must let it through.
    Recorder recorder;
    drain_all(queue, recorder);
    producer.join();

    TW_CHECK(completed.load(std::memory_order_acquire));
    TW_CHECK(accepted_result.load(std::memory_order_acquire));
    drain_all(queue, recorder);
    TW_CHECK_EQ(recorder.items.size(), fitted + 1);
    TW_CHECK_EQ(recorder.items.back().x, 42.0);
}

void test_push_for_reports_a_timeout_instead_of_hanging() {
    CanvasCommandQueue queue{CanvasCommandQueue::kMinCapacityBytes};
    const std::size_t fitted = fill_queue(queue);
    TW_CHECK(fitted > 0);

    const PushResult result = queue.push_for(
        make_command(CanvasCommandKind::Home),
        std::chrono::milliseconds{20});
    TW_CHECK_EQ(result, PushResult::TimedOut);
    TW_CHECK(!accepted(result));

    // Nothing was consumed and nothing was lost.
    Recorder recorder;
    TW_CHECK_EQ(drain_all(queue, recorder), fitted);
}

void test_close_unblocks_a_parked_producer() {
    // This is the anti-deadlock guarantee: a consumer that stops draining
    // (window closed, application shutting down) must not hang the interpreter
    // thread forever.
    CanvasCommandQueue queue{CanvasCommandQueue::kMinCapacityBytes};
    const std::size_t fitted = fill_queue(queue);
    TW_CHECK(fitted > 0);

    std::atomic<PushResult> outcome{PushResult::Appended};
    std::thread producer{[&] {
        outcome.store(queue.push(make_command(CanvasCommandKind::Home)),
                      std::memory_order_release);
    }};

    std::this_thread::sleep_for(std::chrono::milliseconds{50});
    TW_CHECK(queue.blocked_count() > 0);
    // Verified by inspecting the ring rather than by pushing from this thread:
    // a second concurrent producer would violate the SPSC contract this class
    // is built on, and the test would be exercising undefined behaviour.
    TW_CHECK(queue.bytes_buffered() + sizeof(CanvasCommand) > queue.capacity());

    queue.close();
    producer.join();
    TW_CHECK_EQ(outcome.load(std::memory_order_acquire), PushResult::Closed);

    // Closing does not discard what was already queued: the final frame still
    // draws.
    Recorder recorder;
    TW_CHECK_EQ(drain_all(queue, recorder), fitted);
    TW_CHECK(queue.closed());
}

void test_close_is_idempotent_and_visible_to_pushers() {
    CanvasCommandQueue queue;
    queue.close();
    queue.close();
    TW_CHECK(queue.closed());
    TW_CHECK_EQ(queue.push(make_command(CanvasCommandKind::Home)),
                PushResult::Closed);
    TW_CHECK(queue.empty());
}

void test_wait_for_data_wakes_on_push() {
    CanvasCommandQueue queue;
    std::atomic<bool> saw_data{false};
    std::thread producer{[&] {
        std::this_thread::sleep_for(std::chrono::milliseconds{20});
        queue.push(make_motion(CanvasCommandKind::Forward, 1.0));
    }};

    saw_data.store(queue.wait_for_data(std::chrono::milliseconds{2000}),
                   std::memory_order_release);
    producer.join();
    TW_CHECK(saw_data.load(std::memory_order_acquire));
    TW_CHECK(!queue.empty());
}

void test_wait_for_data_gives_up_when_closed() {
    CanvasCommandQueue queue;
    std::thread closer{[&] {
        std::this_thread::sleep_for(std::chrono::milliseconds{20});
        queue.close();
    }};
    // No data will ever arrive, so this must return false rather than block
    // until the timeout.
    const bool available =
        queue.wait_for_data(std::chrono::milliseconds{5000});
    closer.join();
    TW_CHECK(!available);
}

// ---------------------------------------------------------------------------
// Concurrency
// ---------------------------------------------------------------------------

void test_concurrent_producer_and_consumer_lose_nothing() {
    CanvasCommandQueue queue{64u * 1024u};
    constexpr std::size_t kCount = 200000;

    std::atomic<bool> producer_done{false};
    std::thread producer{[&] {
        for (std::size_t index = 0; index < kCount; ++index) {
            const PushResult result = queue.push(make_motion(
                CanvasCommandKind::Forward, static_cast<double>(index)));
            if (!accepted(result)) {
                break;
            }
        }
        queue.flush();
        producer_done.store(true, std::memory_order_release);
    }};

    std::vector<double> seen;
    seen.reserve(kCount);
    const auto sink = [&seen](CanvasCommandView view) {
        seen.push_back(view.command.x);
    };

    // The loop terminates either when everything has arrived, or when the
    // producer has stopped and the queue is provably dry -- so a producer-side
    // failure cannot turn this into a hang.
    while (seen.size() < kCount) {
        if (queue.drain(sink) != 0) {
            continue;
        }
        if (producer_done.load(std::memory_order_acquire) && queue.empty()) {
            break;
        }
        queue.wait_for_data(std::chrono::milliseconds{100});
    }
    producer.join();

    TW_CHECK_EQ(seen.size(), kCount);
    bool ordered = true;
    for (std::size_t index = 0; index < seen.size(); ++index) {
        if (seen[index] != static_cast<double>(index)) {
            ordered = false;
            break;
        }
    }
    TW_CHECK(ordered);
    TW_CHECK_EQ(queue.pushed_count(), std::uint64_t{200000});
    // Deliberately no assertion on `blocked_count()`: whether the producer ever
    // finds the ring full depends on how the scheduler interleaves the two
    // threads, and a consumer that keeps up means it legitimately never blocks.
    // Blocking behaviour is asserted deterministically in
    // `test_producer_blocks_until_space_frees` and
    // `test_close_unblocks_a_parked_producer`, where nothing drains.
}

void test_concurrent_shutdown_with_a_full_queue() {
    // Close while a producer is mid-storm: every push must resolve, none may
    // hang, and everything accepted before the close must still be drainable.
    CanvasCommandQueue queue{CanvasCommandQueue::kMinCapacityBytes};
    std::atomic<bool> producer_done{false};
    std::atomic<std::uint64_t> rejected{0};

    std::thread producer{[&] {
        for (std::size_t index = 0; index < 20000; ++index) {
            const PushResult result = queue.push(
                make_motion(CanvasCommandKind::Forward,
                            static_cast<double>(index)));
            if (result == PushResult::Closed) {
                rejected.fetch_add(1, std::memory_order_relaxed);
                break;
            }
        }
        producer_done.store(true, std::memory_order_release);
    }};

    std::this_thread::sleep_for(std::chrono::milliseconds{10});
    queue.close();
    producer.join();

    TW_CHECK(producer_done.load(std::memory_order_acquire));
    TW_CHECK(queue.closed());

    std::size_t drained = 0;
    Recorder recorder;
    drain_all(queue, recorder);
    drained = recorder.items.size();
    TW_CHECK(drained > 0);
    // Order is still monotonic, i.e. the close did not corrupt anything.
    bool monotonic = true;
    for (std::size_t index = 0; index < recorder.items.size(); ++index) {
        if (recorder.items[index].x != static_cast<double>(index)) {
            monotonic = false;
            break;
        }
    }
    TW_CHECK(monotonic);
}

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

void test_capacity_is_rounded_up_to_a_power_of_two() {
    CanvasCommandQueue tiny{1};
    TW_CHECK_EQ(tiny.capacity(), CanvasCommandQueue::kMinCapacityBytes);
    TW_CHECK_EQ(tiny.capacity() & (tiny.capacity() - 1), std::size_t{0});

    CanvasCommandQueue odd{100000};
    TW_CHECK(odd.capacity() >= 100000);
    TW_CHECK_EQ(odd.capacity() & (odd.capacity() - 1), std::size_t{0});
}

void test_counters_track_the_work() {
    CanvasCommandQueue queue;
    queue.push(make_motion(CanvasCommandKind::SetHeading, 1));
    queue.push(make_motion(CanvasCommandKind::SetHeading, 2));  // coalesced
    queue.push(make_motion(CanvasCommandKind::Forward, 1));
    queue.flush();

    Recorder recorder;
    TW_CHECK_EQ(drain_all(queue, recorder), std::size_t{2});
    TW_CHECK_EQ(queue.pushed_count(), std::uint64_t{2});
    TW_CHECK_EQ(queue.coalesced_count(), std::uint64_t{1});
    TW_CHECK_EQ(queue.applied_count(), std::uint64_t{2});
    TW_CHECK_EQ(queue.blocked_count(), std::uint64_t{0});
}

void test_a_sink_returning_false_stops_the_drain() {
    CanvasCommandQueue queue;
    for (std::size_t index = 0; index < 10; ++index) {
        queue.push(make_motion(CanvasCommandKind::Forward,
                               static_cast<double>(index)));
    }
    std::size_t seen = 0;
    const std::size_t applied = queue.drain([&seen](CanvasCommandView) {
        ++seen;
        return seen < 3;  // stop after the third
    });
    TW_CHECK_EQ(applied, std::size_t{3});
    TW_CHECK_EQ(seen, std::size_t{3});
    TW_CHECK_EQ(queue.bytes_buffered(),
                std::size_t{7} * sizeof(CanvasCommand));
}

}  // namespace

/// Runs one test, naming it first and flushing, so a hang points at its cause
/// instead of leaving a blank terminal.
#define RUN(fn)                                       \
    do {                                              \
        std::printf("  %-58s", #fn);                  \
        std::fflush(stdout);                          \
        fn();                                         \
        std::printf("ok\n");                          \
    } while (false)

int main() {
    RUN(test_command_layout);
    RUN(test_colour_packing);
    RUN(test_coalescible_predicate);

    RUN(test_round_trip_preserves_order);
    RUN(test_variable_length_payloads_survive_wraparound);
    RUN(test_text_longer_than_a_naive_inline_buffer);
    RUN(test_record_larger_than_the_ring_is_rejected);

    RUN(test_adjacent_state_collapses_to_the_last_value);
    RUN(test_coalescing_never_reorders_or_drops_geometry);
    RUN(test_drawing_commands_never_coalesce);
    RUN(test_staging_is_bounded_so_the_consumer_cannot_be_starved);
    RUN(test_flush_with_nothing_staged_is_harmless);

    RUN(test_producer_blocks_until_space_frees);
    RUN(test_push_for_reports_a_timeout_instead_of_hanging);
    RUN(test_close_unblocks_a_parked_producer);
    RUN(test_close_is_idempotent_and_visible_to_pushers);
    RUN(test_wait_for_data_wakes_on_push);
    RUN(test_wait_for_data_gives_up_when_closed);

    RUN(test_concurrent_producer_and_consumer_lose_nothing);
    RUN(test_concurrent_shutdown_with_a_full_queue);

    RUN(test_capacity_is_rounded_up_to_a_power_of_two);
    RUN(test_counters_track_the_work);
    RUN(test_a_sink_returning_false_stops_the_drain);

    return tw::test::summary("canvas_queue");
}
