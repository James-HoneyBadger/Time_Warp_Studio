// SPDX-License-Identifier: MIT
//
// The machine's clock: sixty frames a second.
//
// A computer of 1982 had one clock a program could feel, and everything that moved
// moved on it: the cursor blinked, the sound latch was serviced, a frame counter
// ticked, and `WAIT` waited for the next one rather than sleeping. This is that
// clock, and nothing else.
//
// It is separate from the widget tree on purpose: a test advances it by hand
// (`advance()`), so a frame is a deterministic event rather than something a test
// waits 16.6 milliseconds for. The window starts it with `start()`, which is a
// `QTimer` at the same rate.

#pragma once

#include <cstdint>

#include <QObject>

class QTimer;

namespace timewarp::ui {

class HbClock final : public QObject {
    Q_OBJECT

public:
    /// Frames a second. The period's machines counted in these, and so does the
    /// frame register a program reads.
    static constexpr int kFramesPerSecond = 60;

    explicit HbClock(QObject* parent = nullptr);

    /// Runs the clock at 60 Hz. Idempotent.
    void start();
    void stop();
    [[nodiscard]] bool running() const noexcept { return running_; }

    /// One frame: counts it and emits `tick`. Called by the timer while running, and
    /// directly by a test that wants a frame now.
    void advance();

    [[nodiscard]] std::uint64_t frames() const noexcept { return frames_; }

signals:
    /// One frame has begun. Everything the machine does per frame hangs off this.
    void tick();

private:
    QTimer* timer_ = nullptr;
    std::uint64_t frames_ = 0;
    bool running_ = false;
};

}  // namespace timewarp::ui
