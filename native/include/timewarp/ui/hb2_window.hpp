// SPDX-License-Identifier: MIT
//
// The HB-2 on a desktop: a screen, a keyboard, a prompt and a monitor.
//
// The machine itself is `timewarp::hb2::Machine`, and it is Qt-free on purpose -- the
// harness and the suite run it with no display. This is the glass: it paints the
// framebuffer the chip composed, hands the keyboard to the machine's own key device,
// plays the four-channel mixer through the Studio's speaker, and puts the prompt and
// the monitor on pages beside the screen.
//
// It is a machine window rather than a mode of the editor for the same reason the
// HB-1's is: a computer has one screen and one keyboard, and an IDE's docks are
// neither. The listing is on the PROGRAM page, which is the machine's own editor, and
// `Ctrl+Return` assembles and runs it.

#pragma once

#include <QByteArray>
#include <QImage>
#include <QMainWindow>
#include <QStringList>

#include <cstdint>
#include <vector>

#include "timewarp/hb2/dos.hpp"
#include "timewarp/hb2/machine.hpp"
#include "timewarp/hb2/modules.hpp"
#include "timewarp/hb2/monitor.hpp"
#include "timewarp/hb2/shell.hpp"

class QLabel;
class QLineEdit;
class QPlainTextEdit;
class QSpinBox;
class QTemporaryDir;
class QTimer;

namespace timewarp::ui {

class SoundPlayer;

/// The glass: the machine's picture, scaled to the window, and the keyboard.
class Hb2Screen final : public QWidget {
    Q_OBJECT

public:
    explicit Hb2Screen(timewarp::hb2::Machine& machine, QWidget* parent = nullptr);

    /// Whether this screen has the keyboard, which is what clicking it means.
    [[nodiscard]] bool hasKeyboard() const noexcept { return keyboard_; }
    void setKeyboard(bool on);
    /// Rebuilds the picture from the machine and repaints. Called on the frame tick.
    void refresh();

signals:
    /// A key the machine did not take, for the window to handle (Escape, F-keys).
    void keyForWindow(int qt_key);

protected:
    void paintEvent(QPaintEvent* event) override;
    void keyPressEvent(QKeyEvent* event) override;
    void keyReleaseEvent(QKeyEvent* event) override;
    void mousePressEvent(QMouseEvent* event) override;
    void mouseMoveEvent(QMouseEvent* event) override;
    void focusInEvent(QFocusEvent* event) override;
    void focusOutEvent(QFocusEvent* event) override;

private:
    void rebuildImage();

    timewarp::hb2::Machine& machine_;
    QImage image_;
    bool keyboard_ = false;
    int last_x_ = -1;
    int last_y_ = -1;
};

class Hb2Window final : public QMainWindow {
    Q_OBJECT

public:
    explicit Hb2Window(QWidget* parent = nullptr);
    ~Hb2Window() override;

    [[nodiscard]] timewarp::hb2::Machine& machine() noexcept { return machine_; }
    [[nodiscard]] timewarp::hb2::Shell& shell() noexcept { return shell_; }
    /// The screen widget, for a test that wants to look at what is being shown.
    [[nodiscard]] Hb2Screen* screen() const noexcept { return screen_; }

    /// Writes the listing into the machine's editor page.
    void setListing(const QString& text);
    /// Runs one prompt line and shows the answer.
    void runCommand(const QString& line);
    /// Whether the frame timer is ticking the machine.
    [[nodiscard]] bool running() const noexcept;
    void start();
    void stop();
    /// Advances the machine one frame and repaints, which is what the timer does.
    void tickOnce();

private:
    void buildUi();
    void refreshStatus();
    void refreshMonitor();
    void mixAudio();

    timewarp::hb2::Machine machine_;
    timewarp::hb2::Monitor monitor_;
    timewarp::hb2::Dos dos_;
    timewarp::hb2::Modules modules_;
    timewarp::hb2::Shell shell_;

    Hb2Screen* screen_ = nullptr;
    QPlainTextEdit* transcript_ = nullptr;
    QLineEdit* entry_ = nullptr;
    QPlainTextEdit* listing_ = nullptr;
    QPlainTextEdit* monitor_view_ = nullptr;
    QLabel* status_ = nullptr;
    QLabel* mode_label_ = nullptr;
    QTimer* timer_ = nullptr;
    SoundPlayer* sound_ = nullptr;
    QTemporaryDir* bay_dir_ = nullptr;
    QTimer* autosave_ = nullptr;
    std::vector<std::uint8_t> pcm_;
};

}  // namespace timewarp::ui
