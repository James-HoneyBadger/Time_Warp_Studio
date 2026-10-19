// SPDX-License-Identifier: MIT

#include "timewarp/ui/scrubber.hpp"

#include <QHBoxLayout>
#include <QLabel>
#include <QSignalBlocker>
#include <QSlider>
#include <QTimer>
#include <QToolButton>

#include <algorithm>

namespace timewarp::ui {

namespace {

constexpr int kTickMilliseconds = 16;
constexpr int kPlaybackSeconds = 10;
constexpr int kTicksPerSecond = 60;

}  // namespace

Scrubber::Scrubber(QWidget* parent) : QWidget(parent) {
    setObjectName(QStringLiteral("scrubber"));
    auto* layout = new QHBoxLayout{this};
    layout->setContentsMargins(6, 2, 6, 2);
    layout->setSpacing(6);

    play_ = new QToolButton{this};
    play_->setObjectName(QStringLiteral("scrubberPlay"));
    play_->setText(tr("Play"));
    play_->setToolTip(tr("Replay the drawing"));
    layout->addWidget(play_);

    slider_ = new QSlider{Qt::Horizontal, this};
    slider_->setObjectName(QStringLiteral("scrubberSlider"));
    slider_->setRange(0, 0);
    slider_->setSingleStep(1);
    layout->addWidget(slider_, 1);

    label_ = new QLabel{this};
    label_->setObjectName(QStringLiteral("scrubberLabel"));
    label_->setMinimumWidth(72);
    label_->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    layout->addWidget(label_);

    timer_ = new QTimer{this};
    timer_->setInterval(kTickMilliseconds);
    timer_->setTimerType(Qt::PreciseTimer);
    connect(timer_, &QTimer::timeout, this, &Scrubber::advance);
    connect(play_, &QToolButton::clicked, this, &Scrubber::togglePlay);
    connect(slider_, &QSlider::valueChanged, this, [this](int value) {
        if (value == visible_) {
            return;
        }
        visible_ = value;
        updateLabel();
        emit frameRequested(value);
    });

    setFrames(0);
}

void Scrubber::setFrames(int frames) {
    // A finished drawing should show finished rather than snapped back to blank,
    // so growing the count from the live end keeps the view at the new end.
    const bool was_at_end = visible_ >= frames_;
    frames_ = std::max(0, frames);
    visible_ = was_at_end ? frames_ : std::min(visible_, frames_);
    const QSignalBlocker blocker{slider_};
    slider_->setRange(0, frames_);
    slider_->setValue(visible_);
    setEnabled(frames_ > 0);
    updateLabel();
    if (frames_ == 0) {
        stop();
    }
}

void Scrubber::setVisibleFrame(int frame) {
    visible_ = std::clamp(frame, 0, frames_);
    const QSignalBlocker blocker{slider_};
    slider_->setValue(visible_);
    updateLabel();
}

int Scrubber::stepForFrames(int frames) {
    const int total_ticks = kPlaybackSeconds * kTicksPerSecond;
    return std::max(1, frames / std::max(1, total_ticks));
}

void Scrubber::togglePlay() {
    if (frames_ == 0) {
        return;
    }
    playing_ = !playing_;
    if (playing_) {
        if (visible_ >= frames_) {
            visible_ = 0;
            {
                const QSignalBlocker blocker{slider_};
                slider_->setValue(0);
            }
            emit frameRequested(0);
        }
        play_->setText(tr("Pause"));
        timer_->start();
    } else {
        play_->setText(tr("Play"));
        timer_->stop();
    }
}

void Scrubber::stop() {
    playing_ = false;
    if (play_ != nullptr) {
        play_->setText(tr("Play"));
    }
    if (timer_ != nullptr) {
        timer_->stop();
    }
}

void Scrubber::advance() {
    if (!playing_) {
        return;
    }
    const int next = std::min(frames_, visible_ + stepForFrames(frames_));
    if (next == visible_) {
        stop();
        return;
    }
    visible_ = next;
    {
        const QSignalBlocker blocker{slider_};
        slider_->setValue(visible_);
    }
    updateLabel();
    emit frameRequested(visible_);
    if (visible_ >= frames_) {
        stop();
    }
}

void Scrubber::updateLabel() {
    if (label_ == nullptr) {
        return;
    }
    label_->setText(frames_ == 0 ? tr("\u2014")
                                 : tr("%1 / %2").arg(visible_).arg(frames_));
}

}  // namespace timewarp::ui
