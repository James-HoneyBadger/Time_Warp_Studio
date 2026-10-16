// SPDX-License-Identifier: MIT
//
// Qt glue for CanvasCommandQueue.  This is the only Qt-facing file in native/;
// the queue itself has no Qt dependency, which keeps the core unit-testable
// without a Qt toolchain or a display.
//
// ═══════════════════════════════════════════════════════════════════════════
// How the wakeup works
// ═══════════════════════════════════════════════════════════════════════════
//
// Qt forbids touching a widget from another thread, but `QMetaObject::invoke-
// Method` with `Qt::QueuedConnection` is the documented, supported way to hand
// work to an object's own thread.  It is the mechanism used here, with one
// refinement: the producer may call `wake()` for every single command and still
// cost at most one event-loop post, because a `compare_exchange` gate collapses
// a storm of pushes into a single pending invocation.
//
// The gate is a two-phase handshake, not a simple latch, because a plain latch
// strands commands.  Clearing it *after* a drain lets a push that lands
// mid-drain see the flag still set, skip its post, and then wait for a wakeup
// that has already been consumed.  Clearing it *before* a drain has the
// mirror-image problem when the push completes just after the drain's last read
// of the head counter.
//
// So `drainNow()` clears the flag, drains, and then loops while the flag was set
// again.  The two sides agree through an atomic read-modify-write on the same
// word: if the producer's exchange observes `false`, it takes responsibility for
// posting; if the consumer's observes `true`, it takes responsibility for
// looping.  Those two RMWs are totally ordered, so exactly one of them happens.
// There is no interleaving in which a command is neither posted for nor looped
// over -- the worst case is a redundant drain that applies nothing.
//
// ═══════════════════════════════════════════════════════════════════════════
// Signals are per *batch*, never per command
// ═══════════════════════════════════════════════════════════════════════════
//
// The whole point of the queue is to stop paying a Qt signal per turtle
// operation.  The bridge therefore drains into a plain callback and emits one
// signal per batch, carrying only a count.  A drawing sink that wants to react
// per command implements it as an ordinary function call, not a connection.

#pragma once

#include <atomic>
#include <chrono>
#include <cstddef>
#include <functional>
#include <utility>

#include <QObject>
#include <QString>
#include <QTimer>

#include "timewarp/canvas/command_queue.hpp"

namespace timewarp::canvas::qt {

/// Drives a `CanvasCommandQueue` from a Qt event loop.
///
/// Create it on the GUI thread.  The interpreter thread pushes to the queue and
/// calls `wake()`; the bridge drains on the GUI thread and hands each command to
/// the installed sink.
class CanvasBridge : public QObject {
    Q_OBJECT

public:
    /// Applies one command.  Called on the bridge's thread while draining; the
    /// view and the memory it references are valid only for the call.
    using Sink = std::function<void(CanvasCommandView)>;

    explicit CanvasBridge(CanvasCommandQueue& queue, QObject* parent = nullptr);
    ~CanvasBridge() override;

    /// Registers the drawing sink.  Set it before the first drain.
    void set_sink(Sink sink) { sink_ = std::move(sink); }

    /// Wakeup posted from the producer thread.  Cheap and safe to call per
    /// command; at most one queued invocation is ever outstanding.
    void wake() noexcept;

    /// Periodic safety-net drain.  `0` disables it (the default).
    ///
    /// A wakeup cannot be lost, so this exists only to bound worst-case latency
    /// if the event loop is stalled by something else, and to pick up trailing
    /// state after a producer goes quiet without calling `CanvasCommandQueue::
    /// flush()`.
    void set_poll_interval(std::chrono::milliseconds interval);

    [[nodiscard]] CanvasCommandQueue& queue() noexcept { return queue_; }
    [[nodiscard]] bool drain_pending() const noexcept {
        return pending_.load(std::memory_order_acquire);
    }

public slots:
    /// Drains everything currently queued, invoking the sink per command.
    /// Returns the number applied.
    int drainNow();

signals:
    /// Emitted once per non-empty batch, on the bridge's thread.
    void batch_applied(int count);
    /// Emitted when the queue is closed and has been drained dry, so the caller
    /// can tear down an animation or stop a timer.
    void drained_and_closed();

private slots:
    void onPoll();

private:
    /// One pass over the queue.  `drainNow` may call this more than once.
    std::size_t drain_once();

    CanvasCommandQueue& queue_;
    Sink sink_;
    std::atomic<bool> pending_{false};
    QTimer* poll_timer_ = nullptr;
};

}  // namespace timewarp::canvas::qt
