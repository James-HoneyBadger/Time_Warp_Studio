// SPDX-License-Identifier: MIT

#include "timewarp/ui/chip8_screen.hpp"

#include "timewarp/lang/chip8.hpp"

#include <QFocusEvent>
#include <QGridLayout>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QPainter>
#include <QPushButton>

#include <algorithm>
#include <array>

namespace timewarp::ui {

namespace {

/// The CHIP-8 key for each PC key, in key order 0-F. The letters follow the
/// original keypad's layout, so the finger positions the books show still work.
constexpr std::array<int, 16> kQtKeys{
    Qt::Key_1, Qt::Key_2, Qt::Key_3, Qt::Key_4, Qt::Key_Q, Qt::Key_W,
    Qt::Key_E, Qt::Key_R, Qt::Key_A, Qt::Key_S, Qt::Key_D, Qt::Key_F,
    Qt::Key_Z, Qt::Key_X, Qt::Key_C, Qt::Key_V,
};

constexpr std::array<const char*, 16> kKeyLabels{
    "1", "2", "3", "C", "4", "5", "6", "D",
    "7", "8", "9", "E", "A", "0", "B", "F",
};

[[nodiscard]] int keyFor(int qt_key) {
    for (int index = 0; index < static_cast<int>(kQtKeys.size()); ++index) {
        if (kQtKeys[static_cast<std::size_t>(index)] == qt_key) {
            return index;
        }
    }
    return -1;
}

}  // namespace

Chip8Screen::Chip8Screen(QWidget* parent)
    : QWidget(parent),
      image_(timewarp::lang::kChip8ScreenWidth, timewarp::lang::kChip8ScreenHeight,
             QImage::Format_RGB32) {
    setFocusPolicy(Qt::StrongFocus);
    setAttribute(Qt::WA_OpaquePaintEvent);
    image_.fill(off_);
    setMinimumSize(minimumSizeHint());
}

QSize Chip8Screen::sizeHint() const {
    return {timewarp::lang::kChip8ScreenWidth * timewarp::lang::kChip8PixelScale,
            timewarp::lang::kChip8ScreenHeight * timewarp::lang::kChip8PixelScale};
}

QSize Chip8Screen::minimumSizeHint() const { return {128, 64}; }

void Chip8Screen::setPixels(const std::vector<std::uint8_t>& pixels) {
    if (pixels.size() !=
        static_cast<std::size_t>(image_.width()) * static_cast<std::size_t>(image_.height())) {
        return;
    }
    image_.fill(off_);
    for (int y = 0; y < image_.height(); ++y) {
        for (int x = 0; x < image_.width(); ++x) {
            if (pixels[static_cast<std::size_t>(y) * static_cast<std::size_t>(image_.width()) +
                       static_cast<std::size_t>(x)] != 0) {
                image_.setPixelColor(x, y, on_);
            }
        }
    }
    update();
}

void Chip8Screen::setColors(const QColor& on, const QColor& off) {
    on_ = on;
    off_ = off;
}

void Chip8Screen::paintEvent(QPaintEvent* event) {
    Q_UNUSED(event);
    QPainter painter{this};
    painter.fillRect(rect(), off_);
    // An integer scale: a fractional one turns a one-pixel sprite into a blurry
    // smear, and CHIP-8 sprites are one pixel wide.
    const int scale = std::max(1, std::min(width() / image_.width(),
                                           height() / image_.height()));
    const QSize target{image_.width() * scale, image_.height() * scale};
    const QPoint origin{(width() - target.width()) / 2, (height() - target.height()) / 2};
    painter.setRenderHint(QPainter::SmoothPixmapTransform, false);
    painter.drawImage(QRect{origin, target}, image_);
    painter.setPen(QColor{60, 60, 72});
    painter.drawRect(QRect{origin, target}.adjusted(-1, -1, 0, 0));
}

void Chip8Screen::keyPressEvent(QKeyEvent* event) {
    if (event->isAutoRepeat()) {
        event->accept();
        return;
    }
    const int key = keyFor(event->key());
    if (key < 0) {
        QWidget::keyPressEvent(event);
        return;
    }
    held_[static_cast<std::size_t>(key)] = true;
    emit keyChanged(key, true);
    event->accept();
}

void Chip8Screen::keyReleaseEvent(QKeyEvent* event) {
    if (event->isAutoRepeat()) {
        event->accept();
        return;
    }
    const int key = keyFor(event->key());
    if (key < 0) {
        QWidget::keyReleaseEvent(event);
        return;
    }
    held_[static_cast<std::size_t>(key)] = false;
    emit keyChanged(key, false);
    event->accept();
}

void Chip8Screen::focusOutEvent(QFocusEvent* event) {
    releaseHeldKeys();
    QWidget::focusOutEvent(event);
}

void Chip8Screen::mousePressEvent(QMouseEvent* event) {
    setFocus(Qt::MouseFocusReason);
    QWidget::mousePressEvent(event);
}

void Chip8Screen::releaseHeldKeys() {
    for (int key = 0; key < static_cast<int>(held_.size()); ++key) {
        if (held_[static_cast<std::size_t>(key)]) {
            held_[static_cast<std::size_t>(key)] = false;
            emit keyChanged(key, false);
        }
    }
}

Chip8Keypad::Chip8Keypad(QWidget* parent) : QWidget(parent) {
    auto* grid = new QGridLayout{this};
    grid->setContentsMargins(0, 0, 0, 0);
    grid->setSpacing(4);
    for (int index = 0; index < static_cast<int>(kKeyLabels.size()); ++index) {
        auto* button = new QPushButton{QString::fromLatin1(kKeyLabels[index]), this};
        // The screen owns the keyboard: a button that took focus would swallow
        // the next key press.
        button->setFocusPolicy(Qt::NoFocus);
        button->setAutoDefault(false);
        button->setMinimumSize(40, 30);
        button->setToolTip(QStringLiteral("CHIP-8 key %1").arg(kKeyLabels[index]));
        connect(button, &QPushButton::pressed, this,
                [this, index] { emit keyChanged(index, true); });
        connect(button, &QPushButton::released, this,
                [this, index] { emit keyChanged(index, false); });
        buttons_[static_cast<std::size_t>(index)] = button;
        grid->addWidget(button, index / 4, index % 4);
    }
}

void Chip8Keypad::setKeyDown(int key, bool down) {
    if (key < 0 || key >= static_cast<int>(buttons_.size())) {
        return;
    }
    buttons_[static_cast<std::size_t>(key)]->setDown(down);
}

}  // namespace timewarp::ui
