// SPDX-License-Identifier: MIT
//
// The playback strip under the canvas: a play/pause button and a frame slider.
//
// It controls a *view*. The canvas keeps the whole drawing, and this only says
// how much of it to show, which is why export, fit and the ink counters ignore
// the position of the slider.

#pragma once

#include <QWidget>

class QLabel;
class QSlider;
class QTimer;
class QToolButton;

namespace timewarp::ui {

class Scrubber final : public QWidget {
    Q_OBJECT

public:
    explicit Scrubber(QWidget* parent = nullptr);

    void setFrames(int frames);
    /// Moves the view without emitting `frameRequested`: the caller is already
    /// the one changing the canvas.
    void setVisibleFrame(int frame);

    [[nodiscard]] int frames() const noexcept { return frames_; }
    [[nodiscard]] int visibleFrame() const noexcept { return visible_; }
    [[nodiscard]] bool playing() const noexcept { return playing_; }
    /// How many frames one 60 Hz tick advances, chosen so a whole drawing plays
    /// in about ten seconds rather than in hours.
    [[nodiscard]] static int stepForFrames(int frames);

public slots:
    void togglePlay();
    void stop();
    /// One playback tick. Public for the tests, which cannot wait ten seconds
    /// for a slider.
    void advance();

signals:
    void frameRequested(int frame);

private:
    void updateLabel();

    QToolButton* play_ = nullptr;
    QSlider* slider_ = nullptr;
    QLabel* label_ = nullptr;
    QTimer* timer_ = nullptr;
    int frames_ = 0;
    int visible_ = 0;
    bool playing_ = false;
};

}  // namespace timewarp::ui
