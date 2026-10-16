// SPDX-License-Identifier: MIT

#include "timewarp/canvas/qt_bridge.hpp"

#include <QMetaObject>

namespace timewarp::canvas::qt {

CanvasBridge::CanvasBridge(CanvasCommandQueue& queue, QObject* parent)
    : QObject{parent}, queue_{queue} {}

CanvasBridge::~CanvasBridge() = default;

void CanvasBridge::wake() noexcept {
    // Only the false -> true transition posts.  This is half of the handshake
    // described in the header: observing `false` means the consumer has already
    // released responsibility, so this side must post.  Observing `true` means
    // an invocation is outstanding and `drainNow` will loop until it can see
    // this command.
    if (!pending_.exchange(true, std::memory_order_acq_rel)) {
        // Qt's supported cross-thread hand-off: the lambda runs on this
        // object's thread, which is the GUI thread.
        QMetaObject::invokeMethod(
            this, [this] { drainNow(); }, Qt::QueuedConnection);
    }
}

void CanvasBridge::set_poll_interval(std::chrono::milliseconds interval) {
    if (interval.count() <= 0) {
        if (poll_timer_ != nullptr) {
            poll_timer_->stop();
            poll_timer_->deleteLater();
            poll_timer_ = nullptr;
        }
        return;
    }
    if (poll_timer_ == nullptr) {
        poll_timer_ = new QTimer{this};
        connect(poll_timer_, &QTimer::timeout, this, &CanvasBridge::onPoll);
    }
    poll_timer_->start(static_cast<int>(interval.count()));
}

void CanvasBridge::onPoll() {
    // A poll is just a self-directed wakeup, so it goes through the same gate
    // and cannot produce a redundant drain while one is already pending.
    wake();
}

int CanvasBridge::drainNow() {
    std::size_t applied = 0;
    do {
        // Released *before* draining, so a producer pushing from here on sees
        // `false` and posts its own wakeup rather than trusting this one.
        pending_.store(false, std::memory_order_release);
        applied += drain_once();
        // If a producer set the flag while we were draining, it is relying on
        // this drain to come back for its data.  The exchange settles it: the
        // second of the two RMWs takes responsibility.
    } while (pending_.exchange(false, std::memory_order_acq_rel));

    if (applied != 0) {
        emit batch_applied(static_cast<int>(applied));
    }
    if (queue_.closed() && queue_.empty()) {
        emit drained_and_closed();
    }
    return static_cast<int>(applied);
}

std::size_t CanvasBridge::drain_once() {
    return queue_.drain([this](CanvasCommandView view) {
        if (sink_) {
            sink_(view);
        }
    });
}

}  // namespace timewarp::canvas::qt
