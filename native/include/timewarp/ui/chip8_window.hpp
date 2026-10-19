// SPDX-License-Identifier: MIT
//
// The interactive CHIP-8 window: screen, keypad, controls and inspector.
//
// It is a separate window rather than a mode of the main editor because a game
// and a lesson want different things from the same document: the editor keeps
// its whole-program Run, and this window runs the same bytes at sixty frames a
// second with a live keypad.

#pragma once

#include "timewarp/ui/chip8_emulator.hpp"

#include <QMainWindow>

class QAction;
class QCheckBox;
class QLabel;
class QPlainTextEdit;
class QSpinBox;
class QTableWidget;
class QVBoxLayout;

namespace timewarp::ui {

class Chip8Keypad;
class Chip8Screen;
class SoundPlayer;

class Chip8Window final : public QMainWindow {
    Q_OBJECT

public:
    explicit Chip8Window(QWidget* parent = nullptr);
    ~Chip8Window() override;

    /// Loads a `.ch8` file: a listing is assembled, a ROM is copied byte for
    /// byte. Reports a load failure itself and returns false.
    bool openFile(const QString& path);
    /// Loads the current editor document. A binary document goes through the
    /// editor's byte-preserving codec.
    bool loadSource(const QString& source);

    [[nodiscard]] Chip8Emulator* emulator() const;
    [[nodiscard]] Chip8Screen* screen() const;

public slots:
    void openRomDialog();

protected:
    void dragEnterEvent(QDragEnterEvent* event) override;
    void dropEvent(QDropEvent* event) override;
    void closeEvent(QCloseEvent* event) override;

private:
    void buildUi();
    void buildToolbar();
    void buildInspector(QVBoxLayout* column);
    void refresh();
    void updateControls();
    void updateInspector(const Chip8Snapshot& snapshot);
    void updateSound(const Chip8Snapshot& snapshot);
    [[nodiscard]] static bool isRomFile(const QString& path);

    Chip8Emulator* emulator_ = nullptr;
    Chip8Screen* screen_ = nullptr;
    Chip8Keypad* keypad_ = nullptr;
    SoundPlayer* sound_ = nullptr;
    QAction* run_action_ = nullptr;
    QAction* step_action_ = nullptr;
    QAction* reset_action_ = nullptr;
    QSpinBox* speed_ = nullptr;
    QCheckBox* mute_ = nullptr;
    QTableWidget* registers_ = nullptr;
    QPlainTextEdit* memory_ = nullptr;
    QLabel* state_label_ = nullptr;
    QLabel* detail_label_ = nullptr;
    bool buzzer_on_ = false;
    int last_sound_timer_ = 0;
    int inspector_countdown_ = 0;
};

}  // namespace timewarp::ui
