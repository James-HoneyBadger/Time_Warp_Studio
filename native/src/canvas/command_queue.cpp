// SPDX-License-Identifier: MIT
//
// See command_queue.hpp for the design rationale; this file is the mechanics.
//
// The two rules that keep it correct:
//
//   1. Each counter has exactly one writer.  `head_` is written only by the
//      producer, `tail_` only by the consumer.  That is what makes the
//      read-test-publish sequences safe without a lock.
//   2. A record is fully written before `head_` is published with a release
//      store, and the consumer acquires `head_`.  Therefore `tail < head`
//      implies every byte of the record at `tail` is readable.

#include "timewarp/canvas/command_queue.hpp"

#include <algorithm>
#include <cstring>
#include <thread>

namespace timewarp::canvas {
namespace {

/// Iterations to spin (yielding) before parking in the kernel.  The common case
/// is that the consumer is mid-drain and about to free space, which resolves in
/// far less time than a futex round trip.
constexpr int kSpinBeforeWait = 128;

/// Poll interval for the bounded waits.  C++20 has no timed atomic wait, so
/// `push_for` and `wait_for_data` poll; 200 microseconds keeps the added latency
/// well below a frame while costing almost no CPU.
constexpr std::chrono::microseconds kBoundedWaitPoll{200};

/// Ceiling for capacity rounding, so a nonsense request cannot overflow the
/// shift loop.
constexpr std::size_t kCapacityCeiling = std::size_t{1} << 40;

}  // namespace

std::string_view to_string(PushResult result) noexcept {
    switch (result) {
        case PushResult::Appended:
            return "Appended";
        case PushResult::Staged:
            return "Staged";
        case PushResult::Coalesced:
            return "Coalesced";
        case PushResult::Closed:
            return "Closed";
        case PushResult::TooLarge:
            return "TooLarge";
        case PushResult::TimedOut:
            return "TimedOut";
    }
    return "Unknown";
}

std::size_t CanvasCommandQueue::round_up_to_power_of_two(std::size_t value) {
    std::size_t result = 64;
    while (result < value && result < kCapacityCeiling) {
        result <<= 1;
    }
    return result;
}

std::size_t CanvasCommandQueue::record_bytes(std::size_t text_length,
                                            std::size_t point_count) noexcept {
    return sizeof(CanvasCommand) + point_count * sizeof(double) + text_length;
}

CanvasCommandQueue::CanvasCommandQueue(std::size_t capacity_bytes)
    : capacity_{
          round_up_to_power_of_two(std::max(capacity_bytes, kMinCapacityBytes))},
      mask_{capacity_ - 1} {
    buffer_ = std::make_unique<std::byte[]>(capacity_);
}

CanvasCommandQueue::~CanvasCommandQueue() {
    // Releases anyone parked in a wait so the owner can join them.  It cannot
    // make concurrent destruction safe; see the contract in the header.
    close();
}

// ---------------------------------------------------------------------------
// Buffer addressing
// ---------------------------------------------------------------------------

void CanvasCommandQueue::copy_in(std::uint64_t offset, const std::byte* source,
                                 std::size_t length) noexcept {
    const std::size_t position = offset & mask_;
    const std::size_t first = std::min(length, capacity_ - position);
    std::memcpy(buffer_.get() + position, source, first);
    if (length > first) {
        // The record straddled the end of the ring; the remainder goes at the
        // start.  Packing records this way keeps the ring dense, at the cost of
        // at most two memcpy per record.
        std::memcpy(buffer_.get(), source + first, length - first);
    }
}

void CanvasCommandQueue::copy_out(std::uint64_t offset, std::byte* destination,
                                  std::size_t length) const noexcept {
    const std::size_t position = offset & mask_;
    const std::size_t first = std::min(length, capacity_ - position);
    std::memcpy(destination, buffer_.get() + position, first);
    if (length > first) {
        std::memcpy(destination + first, buffer_.get(), length - first);
    }
}

// ---------------------------------------------------------------------------
// Producer
// ---------------------------------------------------------------------------

PushResult CanvasCommandQueue::write_bytes(const std::byte* data,
                                           std::size_t length,
                                           Clock::time_point deadline) {
    // Safe to hoist: `head_` is written only by this thread.
    const std::uint64_t head = head_.load(std::memory_order_relaxed);
    int spins = 0;

    for (;;) {
        // The wakeup word is read *before* the conditions are tested.  A release
        // that lands after this read therefore changes the value, so the `wait`
        // below cannot miss it; and because the state store is ordered before the
        // bump, the fresh load immediately below sees the state change too.
        const std::uint64_t observed =
            space_released_.load(std::memory_order_acquire);

        const std::uint64_t tail = tail_.load(std::memory_order_acquire);
        if (head - tail + length <= capacity_) {
            copy_in(head, data, length);
            // Release: the bytes above must be visible before the consumer can
            // observe that `head` moved past them.
            head_.store(head + length, std::memory_order_release);
            return PushResult::Appended;
        }

        if (closed_.load(std::memory_order_acquire)) {
            return PushResult::Closed;
        }

        if (spins < kSpinBeforeWait) {
            ++spins;
            std::this_thread::yield();
            continue;
        }
        if (spins == kSpinBeforeWait) {
            ++spins;
            blocked_.fetch_add(1, std::memory_order_relaxed);
        }

        if (deadline == Clock::time_point::max()) {
            space_released_.wait(observed, std::memory_order_acquire);
            continue;
        }
        if (Clock::now() >= deadline) {
            return PushResult::TimedOut;
        }
        // No timed atomic wait in C++20, so a bounded wait polls.
        std::this_thread::sleep_for(kBoundedWaitPoll);
    }
}

void CanvasCommandQueue::build_staged(const CanvasCommand& command,
                                      std::string_view text,
                                      std::span<const double> points) {
    CanvasCommand header = command;
    header.point_bytes = static_cast<std::uint32_t>(points.size() * sizeof(double));
    header.text_bytes = static_cast<std::uint32_t>(text.size());

    staged_bytes_ = record_bytes(header.text_bytes, points.size());
    if (staging_.size() < staged_bytes_) {
        staging_.resize(staged_bytes_);
    }

    std::byte* cursor = staging_.data();
    std::memcpy(cursor, &header, sizeof header);
    cursor += sizeof header;
    if (header.point_bytes != 0) {
        std::memcpy(cursor, points.data(), header.point_bytes);
        cursor += header.point_bytes;
    }
    if (header.text_bytes != 0) {
        std::memcpy(cursor, text.data(), header.text_bytes);
    }
}

PushResult CanvasCommandQueue::emit_staged(Clock::time_point deadline) {
    const PushResult result =
        write_bytes(staging_.data(), staged_bytes_, deadline);
    if (result == PushResult::Appended) {
        pushed_.fetch_add(1, std::memory_order_relaxed);
    }
    return result;
}

PushResult CanvasCommandQueue::publish_held(Clock::time_point deadline) {
    if (!staged_) {
        return PushResult::Appended;
    }
    const PushResult result = emit_staged(deadline);
    if (result == PushResult::Appended) {
        staged_ = false;
        staged_kind_ = CanvasCommandKind::None;
        staged_bytes_ = 0;
        staged_replaces_ = 0;
    }
    // On failure the staging is kept intact, so the command can be retried and
    // is never lost.
    return result;
}

PushResult CanvasCommandQueue::enqueue(const CanvasCommand& command,
                                       std::string_view text,
                                       std::span<const double> points,
                                       Clock::time_point deadline) {
    if (closed_.load(std::memory_order_acquire)) {
        return PushResult::Closed;
    }

    const std::size_t length = record_bytes(text.size(), points.size());
    if (length > capacity_) {
        // Larger than the entire ring, so waiting cannot help.
        return PushResult::TooLarge;
    }

    const bool plain = text.empty() && points.empty();
    const bool matches_held = staged_ && plain &&
                              is_coalescible(command.kind) &&
                              command.kind == staged_kind_ &&
                              length == staged_bytes_;

    if (matches_held && staged_replaces_ + 1 < coalesce_limit_) {
        // Same kind and same record size, so only the header changes and the
        // rewrite is confined to the producer's own staging buffer.  Nothing
        // published is ever mutated, which is what makes this race-free.
        CanvasCommand header = command;
        header.point_bytes = 0;
        header.text_bytes = 0;
        std::memcpy(staging_.data(), &header, sizeof header);
        coalesced_.fetch_add(1, std::memory_order_relaxed);
        ++staged_replaces_;
        return PushResult::Coalesced;
    }

    // Everything else must not overtake a held state change, so publish it
    // first.  This is where a full ring blocks the producer.
    if (staged_) {
        const PushResult published = publish_held(deadline);
        if (published != PushResult::Appended) {
            return published;  // nothing has been mutated
        }
    }

    build_staged(command, text, points);

    if (!is_coalescible(command.kind)) {
        const PushResult result = emit_staged(deadline);
        if (result == PushResult::Appended) {
            staged_bytes_ = 0;
        }
        return result;
    }

    // A run of coalescible commands has to end somewhere, or a caller that only
    // ever sets state would never publish anything.
    if (staged_replaces_ + 1 >= coalesce_limit_) {
        const PushResult result = emit_staged(deadline);
        if (result == PushResult::Appended) {
            staged_bytes_ = 0;
        }
        return result;
    }

    staged_ = true;
    staged_kind_ = command.kind;
    staged_replaces_ = 0;
    return PushResult::Staged;
}

PushResult CanvasCommandQueue::push(const CanvasCommand& command,
                                    std::string_view text,
                                    std::span<const double> points) {
    return enqueue(command, text, points, Clock::time_point::max());
}

PushResult CanvasCommandQueue::push_for(const CanvasCommand& command,
                                        std::chrono::milliseconds timeout,
                                        std::string_view text,
                                        std::span<const double> points) {
    return enqueue(command, text, points, Clock::now() + timeout);
}

PushResult CanvasCommandQueue::flush() {
    return publish_held(Clock::time_point::max());
}

// ---------------------------------------------------------------------------
// Lifecycle
// ---------------------------------------------------------------------------

void CanvasCommandQueue::close() noexcept {
    closed_.store(true, std::memory_order_release);
    // `closed_` is what `wait_until_closed` parks on, and it just changed, so a
    // notify is enough there.
    closed_.notify_all();
    // A producer parked for room is waiting on this word, and `atomic::wait` only
    // returns once the value differs -- so it must be *bumped*, not merely
    // notified.  Getting this wrong deadlocks shutdown, which is exactly the case
    // the valve exists to prevent.
    space_released_.fetch_add(1, std::memory_order_release);
    space_released_.notify_all();
}

// ---------------------------------------------------------------------------
// Consumer
// ---------------------------------------------------------------------------

bool CanvasCommandQueue::wait_for_data(std::chrono::milliseconds timeout) {
    const Clock::time_point deadline = Clock::now() + timeout;
    int spins = 0;

    // `have_data` is re-read from the atomics at every step, so the return value
    // after a timeout still reflects anything that arrived meanwhile.
    const auto have_data = [this] {
        return tail_.load(std::memory_order_relaxed) <
               head_.load(std::memory_order_acquire);
    };

    for (;;) {
        if (have_data()) {
            return true;
        }
        if (closed_.load(std::memory_order_acquire)) {
            // Commands already queued remain drainable; there just will not be
            // any more.
            return have_data();
        }
        if (spins < kSpinBeforeWait) {
            ++spins;
            std::this_thread::yield();
            continue;
        }
        if (Clock::now() >= deadline) {
            return have_data();
        }
        // Bounded: polls, because C++20 has no timed atomic wait.
        std::this_thread::sleep_for(kBoundedWaitPoll);
    }
}

void CanvasCommandQueue::wait_until_closed() {
    while (!closed_.load(std::memory_order_acquire)) {
        closed_.wait(false, std::memory_order_acquire);
    }
}

}  // namespace timewarp::canvas
