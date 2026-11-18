// SPDX-License-Identifier: MIT
//
// The interactive CHIP-8 window: screen, keypad, controls and inspector.
//
// The manual is `docs/chip8/README.md`.
//
// It is a separate window rather than a mode of the main editor because a game
// and a lesson want different things from the same document: the editor keeps
// its whole-program Run, and this window runs the same bytes at sixty frames a
// second with a live keypad.

#pragma once

#include "timewarp/ui/chip8_emulator.hpp"

#include <QMainWindow>
#include <QPointer>

#ifdef TIME_WARP_HAVE_AI
#include "timewarp/ui/assistant_dialog.hpp"
#endif

class QAction;
class QCheckBox;
class QSlider;
class QLabel;
class QPlainTextEdit;
class QSpinBox;
class QTableWidget;
class QTabWidget;
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
    /// Loads the bundled paint program (or another CHIP-8 example) when one is
    /// installed. False when the installation ships no CHIP-8 example, which
    /// leaves the window with an empty listing editor rather than an error.
    bool loadBundledProgram();

    [[nodiscard]] Chip8Emulator* emulator() const;
    [[nodiscard]] Chip8Screen* screen() const;

public slots:
    void openRomDialog();
#ifdef TIME_WARP_HAVE_AI
    /// The assistant, about the listing in this window. A dialog for the same reason
    /// the HB-1 has one: this window is a machine's own, not the IDE's.
    void openAssistant();
    [[nodiscard]] AssistantDialog* assistantDialog() const noexcept {
        return assistant_.data();
    }
#endif
    /// Browses the folder the user pointed the ROM library at, and remembers the
    /// folder after a successful open.
    void openRomLibraryDialog();
    /// Assembles the text in the Listing tab and runs it. A listing that does not
    /// assemble leaves the running program alone and prints the error in the tab.
    void assembleListing();

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
    /// Points the rewind strip at the emulator's history.
    void updateRewind();
    void updateInspector(const Chip8Snapshot& snapshot);
    void updateSound(const Chip8Snapshot& snapshot);
    void setListing(const QString& text, const QString& status);
    [[nodiscard]] static bool isRomFile(const QString& path);

    Chip8Emulator* emulator_ = nullptr;
    Chip8Screen* screen_ = nullptr;
    Chip8Keypad* keypad_ = nullptr;
    SoundPlayer* sound_ = nullptr;
    QAction* run_action_ = nullptr;
    QAction* step_action_ = nullptr;
    QAction* reset_action_ = nullptr;
    QAction* assemble_action_ = nullptr;
    QSpinBox* speed_ = nullptr;
    QCheckBox* mute_ = nullptr;
    QTableWidget* registers_ = nullptr;
    QPlainTextEdit* memory_ = nullptr;
    QPlainTextEdit* listing_ = nullptr;
#ifdef TIME_WARP_HAVE_AI
    QPointer<AssistantDialog> assistant_;
#endif
    QLabel* listing_status_ = nullptr;
    QTabWidget* inspector_tabs_ = nullptr;
    QWidget* listing_page_ = nullptr;
    QLabel* state_label_ = nullptr;
    QLabel* detail_label_ = nullptr;
    /// The rewind strip: a slider over the moments the emulator kept, and the label
    /// saying how far back it is.
    QSlider* rewind_ = nullptr;
    QLabel* rewind_label_ = nullptr;
    bool buzzer_on_ = false;
    int last_sound_timer_ = 0;
    int inspector_countdown_ = 0;
};

}  // namespace timewarp::ui
