// SPDX-License-Identifier: MIT
//
// The two widgets a CHIP-8 program actually talks to: the 64x32 screen and the
// sixteen-key pad.

#pragma once

#include <QColor>
#include <QImage>
#include <QWidget>

#include <array>
#include <cstdint>
#include <vector>

class QPushButton;

namespace timewarp::ui {

/// The framebuffer, scaled with nearest-neighbour so a lit pixel stays a square.
///
/// It also owns the PC-keyboard mapping. The pad is what the program reads, and
/// the screen is where a learner's hands already are, so keys work without
/// making them click first.
class Chip8Screen final : public QWidget {
    Q_OBJECT

public:
    explicit Chip8Screen(QWidget* parent = nullptr);

    void setPixels(const std::vector<std::uint8_t>& pixels);
    void setColors(const QColor& on, const QColor& off);

    [[nodiscard]] QSize sizeHint() const override;
    [[nodiscard]] QSize minimumSizeHint() const override;

signals:
    void keyChanged(int key, bool down);

protected:
    void paintEvent(QPaintEvent* event) override;
    void keyPressEvent(QKeyEvent* event) override;
    void keyReleaseEvent(QKeyEvent* event) override;
    void focusOutEvent(QFocusEvent* event) override;
    void mousePressEvent(QMouseEvent* event) override;

private:
    /// A focus change must not leave a key held: the machine would otherwise
    /// think a key is down for the rest of the run.
    void releaseHeldKeys();

    QImage image_;
    QColor on_{224, 248, 208};
    QColor off_{10, 10, 20};
    std::array<bool, 16> held_{};
};

/// The 4x4 pad: clickable, and a mirror of keys pressed on the keyboard.
class Chip8Keypad final : public QWidget {
    Q_OBJECT

public:
    explicit Chip8Keypad(QWidget* parent = nullptr);

    void setKeyDown(int key, bool down);

signals:
    void keyChanged(int key, bool down);

private:
    std::array<QPushButton*, 16> buttons_{};
};

}  // namespace timewarp::ui
