// SPDX-License-Identifier: MIT
//
// A bounded, lock-free, single-producer/single-consumer canvas command queue.
//
// ═══════════════════════════════════════════════════════════════════════════
// The problem
// ═══════════════════════════════════════════════════════════════════════════
//
// An interpreter runs on its own thread and wants to draw.  Qt widgets may only
// be touched from the thread that owns them.  The reference Python application
// bridges the two by emitting a Qt signal per turtle call, which means one
// queued-connection post -- a heap allocation and an event-loop wakeup -- for
// every `forward(...)`.  A turtle program in a tight loop can emit those
// hundreds of thousands of times per second, so the marshalling cost dwarfs the
// drawing.
//
// This queue replaces "one signal per command" with "one buffer for all of
// them": the producer appends to a preallocated ring with no allocation and no
// syscall, and the consumer drains a whole batch under a single wakeup.
//
// ═══════════════════════════════════════════════════════════════════════════
// The design
// ═══════════════════════════════════════════════════════════════════════════
//
//   * Bounded byte ring with free-running counters.  `head` and `tail` are
//     monotonic byte offsets, never wrapped, so "empty" and "full" are both
//     representable and the buffer index is just `offset & mask_`.  Capacity is
//     a power of two, so the mask replaces a modulo.
//
//   * Release/acquire pairing.  The producer writes a whole record *then*
//     publishes `head` with a release store; the consumer acquires `head`, so
//     seeing `tail < head` guarantees the entire record is readable.  `tail` is
//     published the same way in the other direction.  Each counter has exactly
//     one writer, which is what makes this lock-free without a mutex.
//
//   * Variable-length records.  A record is a fixed `CanvasCommand` header
//     followed by an optional point list and optional UTF-8 text.  The length is
//     in the header, so no arbitrary limit is imposed on either.
//
//   * Blocking without a mutex.  When the ring is full the producer blocks in
//     `std::atomic::wait`; no `std::mutex`, no `condition_variable`, no kernel
//     object until the wait actually spins down.
//
//     The waiter parks on a *dedicated* wakeup word, `space_released_`, which
//     every notifier **increments**.  That is not incidental.  `atomic::wait`
//     re-checks the value after each notification and goes back to sleep if it is
//     unchanged, so parking on `tail_` and calling `tail_.notify_all()` without
//     modifying `tail_` leaves the waiter parked forever.  Measured against
//     libstdc++: notify-only does not wake a waiter, bump-then-notify does.  A
//     plain counter cannot be bumped by `close()` either -- it has a single
//     writer -- which is why the wakeup word is separate from `tail_`.
//
//     `wait` is safe here from the classic "value changed and changed back"
//     hazard too, because `space_released_` only ever increases.
//
//     The wait also follows the required ordering: the wakeup word is read
//     *before* the conditions are tested, so a release that happens after that
//     read necessarily changes the value and cannot be missed.
//
//     One C++20 limitation applies.  `std::atomic::wait` has no *timed*
//     counterpart in C++20 -- `wait_for` / `wait_until` on atomics are C++23 -- so
//     the explicitly bounded entry points (`push_for`, `wait_for_data`) poll with
//     a short sleep instead of parking.  The unbounded paths, `push` and
//     `wait_until_closed`, park properly, and those are the ones the hot path
//     uses.
//
//   * No lost wakeups.  `atomic::wait(old)` returns immediately when the value
//     already differs from `old`, so a notification that races with the wait
//     cannot be missed.  Every waiter re-loads and re-tests in a loop.
//
//   * Coalescing happens in a *producer-private* staging buffer.  This matters
//     and is easy to get wrong: rewriting a slot that has already been published
//     would let the consumer observe a half-written record, and a 72-byte store
//     is not atomic.  By holding the pending coalescible command back instead,
//     nothing published is ever mutated, so the race cannot arise at all.
//
//   * Closing is the anti-deadlock valve.  Because the producer blocks when
//     full, a consumer that stops draining (window minimised, application
//     shutting down) would hang the interpreter thread forever.  `close()`
//     unblocks both sides and makes further pushes fail fast with `Closed`, so
//     a blocked producer always terminates.
//
// ═══════════════════════════════════════════════════════════════════════════
// Coalescing semantics
// ═══════════════════════════════════════════════════════════════════════════
//
// Only `is_coalescible()` kinds collapse, and only into an *identical, adjacent*
// predecessor.  That restriction is what makes it safe:
//
//     set_heading(90); forward(10); set_heading(45)
//
// keeps both `set_heading` commands, because the second is not adjacent to the
// first.  Collapsing across the `forward` would silently change the geometry.
//
// Because a staged command is not yet visible, the consumer may lag by up to one
// coalescible command.  `flush()` publishes it, and `set_coalesce_limit()`
// bounds how long it can be held, so a live read-out (the position strip) cannot
// be starved.  A paused interpreter must call `flush()` to publish trailing
// state; the Qt bridge does this automatically when a run finishes.
//
// ═══════════════════════════════════════════════════════════════════════════
// Threading contract
// ═══════════════════════════════════════════════════════════════════════════
//
//   * `push`, `push_for`, `flush`  -- producer thread only.
//   * `drain`, `wait_for_data`     -- consumer thread only.
//   * `close`, and every query     -- any thread, and must be safe from the
//                                     GUI thread during shutdown.
//
// The queue must outlive every thread using it.  `close()` unblocks waiters so
// they can be joined, but it cannot make concurrent destruction safe.

#pragma once

#include <atomic>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

#include "timewarp/canvas/command.hpp"

namespace timewarp::canvas {

/// Outcome of a producer-side push.
///
/// `Closed`, `TooLarge` and `TimedOut` are the only outcomes where the command
/// was *not* accepted.  `Staged` and `Coalesced` mean accepted-but-not-yet-
/// published, which is not a failure: nothing is ever dropped.
enum class PushResult : std::uint8_t {
    Appended,  ///< published to the ring; the consumer can see it now
    Staged,    ///< accepted, held producer-side for possible coalescing
    Coalesced, ///< accepted, replacing a previously staged command
    Closed,    ///< the queue is closed; the producer should stop
    TooLarge,  ///< the record exceeds the whole ring; cannot ever fit
    TimedOut,  ///< no space became available before the deadline
};

/// True when the command was accepted (whether or not it is visible yet).
[[nodiscard]] constexpr bool accepted(PushResult result) noexcept {
    return result == PushResult::Appended || result == PushResult::Staged ||
           result == PushResult::Coalesced;
}

[[nodiscard]] std::string_view to_string(PushResult result) noexcept;

/// The single-producer/single-consumer canvas command queue.
class CanvasCommandQueue {
public:
    /// Smallest ring worth having; smaller requests are rounded up.
    static constexpr std::size_t kMinCapacityBytes = 16u * 1024u;
    /// 256 KiB holds roughly 3,600 maximum-size records, or ~16,000 polygon
    /// points in one record.  Plenty for a frame's worth of turtle output.
    static constexpr std::size_t kDefaultCapacityBytes = 256u * 1024u;
    /// How many times a staged command may be replaced before it is published
    /// anyway, so a long run of state changes cannot starve the consumer.
    static constexpr std::size_t kDefaultCoalesceLimit = 1024;

    explicit CanvasCommandQueue(
        std::size_t capacity_bytes = kDefaultCapacityBytes);
    ~CanvasCommandQueue();

    CanvasCommandQueue(const CanvasCommandQueue&) = delete;
    CanvasCommandQueue& operator=(const CanvasCommandQueue&) = delete;
    CanvasCommandQueue(CanvasCommandQueue&&) = delete;
    CanvasCommandQueue& operator=(CanvasCommandQueue&&) = delete;

    // -- producer side ---------------------------------------------------

    /// Enqueues a command, blocking until there is room.
    ///
    /// `points` is consecutive (x, y) pairs for the kinds that take a point
    /// list; `text` is UTF-8 for the kinds that take a string.
    PushResult push(const CanvasCommand& command, std::string_view text = {},
                    std::span<const double> points = {});

    /// As `push`, but gives up after `timeout` and returns `TimedOut` rather
    /// than blocking indefinitely.
    PushResult push_for(const CanvasCommand& command,
                        std::chrono::milliseconds timeout,
                        std::string_view text = {},
                        std::span<const double> points = {});

    /// Publishes a staged coalescible command, if any.  Returns `Appended` when
    /// there was nothing to publish.
    ///
    /// Call this whenever the interpreter pauses -- end of a run, a debugger
    /// break, before deciding the frame is complete -- so trailing state changes
    /// become visible.
    PushResult flush();

    /// Unblocks every waiter and makes subsequent pushes return `Closed`.
    /// Already-queued commands remain drainable so the final frame still draws.
    /// Safe to call from any thread, more than once.
    void close() noexcept;

    [[nodiscard]] bool closed() const noexcept {
        return closed_.load(std::memory_order_acquire);
    }

    // -- consumer side (GUI thread) --------------------------------------

    /// Applies every currently available command, in order.
    ///
    /// The sink is invoked as `sink(CanvasCommandView)`; the view, and the
    /// memory it points at, are only valid for the duration of the call.  If the
    /// sink returns `bool`, returning `false` stops the drain early.
    ///
    /// Returns the number of commands applied.
    template <typename Sink>
    std::size_t drain(Sink&& sink) {
        std::uint64_t tail = tail_.load(std::memory_order_relaxed);
        const std::uint64_t head = head_.load(std::memory_order_acquire);
        std::size_t applied = 0;

        while (tail < head) {
            CanvasCommand command{};
            copy_out(tail, reinterpret_cast<std::byte*>(&command),
                     sizeof command);

            const std::size_t points_bytes = command.point_bytes;
            const std::size_t text_bytes = command.text_bytes;
            const std::size_t total = sizeof command + points_bytes + text_bytes;

            // A published record is always complete: the producer stores `head`
            // only after writing every byte of it.
            if (points_bytes != 0) {
                points_.resize(points_bytes / sizeof(double));
                copy_out(tail + sizeof command,
                         reinterpret_cast<std::byte*>(points_.data()),
                         points_bytes);
            } else {
                points_.clear();
            }
            if (text_bytes != 0) {
                text_.resize(text_bytes);
                copy_out(tail + sizeof command + points_bytes,
                         reinterpret_cast<std::byte*>(text_.data()), text_bytes);
            } else {
                text_.clear();
            }

            ++applied;
            tail += total;

            const CanvasCommandView view{command, points_, text_};
            if constexpr (std::is_same_v<
                              std::invoke_result_t<Sink&, CanvasCommandView>,
                              bool>) {
                if (!sink(view)) {
                    break;
                }
            } else {
                sink(view);
            }
        }

        if (applied != 0) {
            tail_.store(tail, std::memory_order_release);
            // Bump the wakeup word *after* publishing the new tail, so a producer
            // released by it is guaranteed to observe the freed space.  Only a
            // value change releases a parked waiter; a bare notify would not.
            space_released_.fetch_add(1, std::memory_order_release);
            space_released_.notify_all();
            applied_.fetch_add(applied, std::memory_order_relaxed);
        }
        return applied;
    }

    /// Blocks until at least one command is available, the queue closes, or the
    /// timeout expires.  Returns true when there is something to drain.
    ///
    /// Bounded, so it polls with a short sleep rather than parking; see the note
    /// on C++20 atomic waits at the top of this header.  The Qt bridge does not
    /// use this path -- it drives the queue from the event loop via `wake()`.
    bool wait_for_data(std::chrono::milliseconds timeout);

    /// Blocks until `close()` is called.  For a consumer thread that has nothing
    /// else to do.
    void wait_until_closed();

    // -- queries (any thread) --------------------------------------------

    [[nodiscard]] bool empty() const noexcept {
        return tail_.load(std::memory_order_acquire) >=
               head_.load(std::memory_order_acquire);
    }
    [[nodiscard]] std::size_t capacity() const noexcept { return capacity_; }
    /// Bytes currently in the ring.  A `uint64_t` because the ring's counters
    /// are monotonic byte offsets, not indices.
    [[nodiscard]] std::uint64_t bytes_buffered() const noexcept {
        const std::uint64_t head = head_.load(std::memory_order_acquire);
        const std::uint64_t tail = tail_.load(std::memory_order_acquire);
        return head - tail;
    }
    /// Largest record that can ever be enqueued; bigger ones get `TooLarge`.
    [[nodiscard]] std::size_t max_record_bytes() const noexcept {
        return capacity_;
    }
    /// True when a coalescible command is currently held producer-side.
    [[nodiscard]] bool has_staged() const noexcept { return staged_; }

    [[nodiscard]] std::uint64_t pushed_count() const noexcept {
        return pushed_.load(std::memory_order_relaxed);
    }
    [[nodiscard]] std::uint64_t coalesced_count() const noexcept {
        return coalesced_.load(std::memory_order_relaxed);
    }
    [[nodiscard]] std::uint64_t applied_count() const noexcept {
        return applied_.load(std::memory_order_relaxed);
    }
    /// How many times a producer found the ring full and had to wait.
    [[nodiscard]] std::uint64_t blocked_count() const noexcept {
        return blocked_.load(std::memory_order_relaxed);
    }

    /// Upper bound on how many times a staged command is replaced before being
    /// published anyway.  Lower values reduce consumer latency.
    void set_coalesce_limit(std::size_t limit) noexcept {
        coalesce_limit_ = limit == 0 ? 1 : limit;
    }
    [[nodiscard]] std::size_t coalesce_limit() const noexcept {
        return coalesce_limit_;
    }

private:
    using Clock = std::chrono::steady_clock;

    static std::size_t round_up_to_power_of_two(std::size_t value);
    static std::size_t record_bytes(std::size_t text_length,
                                    std::size_t point_count) noexcept;

    PushResult enqueue(const CanvasCommand& command, std::string_view text,
                       std::span<const double> points,
                       Clock::time_point deadline);
    void build_staged(const CanvasCommand& command, std::string_view text,
                      std::span<const double> points);
    /// Writes the built record to the ring and counts it.
    PushResult emit_staged(Clock::time_point deadline);
    /// Publishes the held coalescible command, clearing the staged flag.
    PushResult publish_held(Clock::time_point deadline);

    PushResult write_bytes(const std::byte* data, std::size_t length,
                           Clock::time_point deadline);
    void copy_in(std::uint64_t offset, const std::byte* source,
                 std::size_t length) noexcept;
    void copy_out(std::uint64_t offset, std::byte* destination,
                  std::size_t length) const noexcept;

    std::unique_ptr<std::byte[]> buffer_;
    std::size_t capacity_ = 0;
    std::size_t mask_ = 0;

    /// Producer-private staging.  Also the scratch buffer every record is built
    /// in, so a direct push allocates nothing either.
    std::vector<std::byte> staging_;
    std::size_t staged_bytes_ = 0;
    bool staged_ = false;
    CanvasCommandKind staged_kind_ = CanvasCommandKind::None;
    std::size_t staged_replaces_ = 0;
    std::size_t coalesce_limit_ = kDefaultCoalesceLimit;

    /// Consumer-private scratch for the trailing payloads of one record.
    std::vector<double> points_;
    std::string text_;

    // The counters sit on separate cache lines: the producer spins on `tail_`
    // while the consumer spins on `head_`, and sharing a line would make each
    // side invalidate the other's cache entry on every access.  64 is the
    // common line size; the ABI-unstable std::hardware_* constant is avoided
    // because -Winterference-size makes it a portability hazard.
    alignas(64) std::atomic<std::uint64_t> head_{0};
    alignas(64) std::atomic<std::uint64_t> tail_{0};
    alignas(64) std::atomic<bool> closed_{false};

    /// Dedicated wakeup word for a producer parked waiting for room.
    ///
    /// Incremented -- never merely notified -- by whoever frees space (the
    /// consumer, or `close()`).  See the note on blocking at the top of this
    /// header for why a wakeup word has to be separate from `tail_`.
    alignas(64) std::atomic<std::uint64_t> space_released_{0};

    std::atomic<std::uint64_t> pushed_{0};
    std::atomic<std::uint64_t> coalesced_{0};
    std::atomic<std::uint64_t> applied_{0};
    std::atomic<std::uint64_t> blocked_{0};
};

}  // namespace timewarp::canvas
