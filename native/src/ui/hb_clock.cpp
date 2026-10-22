// SPDX-License-Identifier: MIT

#include "timewarp/ui/hb_clock.hpp"

#include <QTimer>

namespace timewarp::ui {

HbClock::HbClock(QObject* parent) : QObject(parent) {
    timer_ = new QTimer{this};
    timer_->setTimerType(Qt::PreciseTimer);
    timer_->setInterval(1000 / kFramesPerSecond);
    connect(timer_, &QTimer::timeout, this, [this] { advance(); });
}

void HbClock::start() {
    if (running_) {
        return;
    }
    running_ = true;
    timer_->start();
}

void HbClock::stop() {
    running_ = false;
    timer_->stop();
}

void HbClock::advance() {
    ++frames_;
    emit tick();
}

}  // namespace timewarp::ui
