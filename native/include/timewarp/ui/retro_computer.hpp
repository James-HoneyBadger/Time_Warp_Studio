// SPDX-License-Identifier: MIT
//
// The HB-1: a self-contained home computer with a selectable ROM.
//
// It is one of the Studio's two machines, and it has its own books:
// `docs/MACHINES.md` for why it exists, `docs/hb1/README.md` for the
// machine, and `docs/hb1/inside.md` for the code below.
//
// The rest of the IDE is a modern tool -- tabs, docks, inspectors -- and this is
// the machine a 1980s learner would have sat in front of. One screen holds the
// text and the graphics, an intro sits above it, and a prompt waits below. The
// language selector is the ROM socket: changing it cold-boots the machine into
// that language, with its own prompt, its own boot message and its own stored
// program.
//
// It is a window rather than a mode of the editor for the same reason the CHIP-8
// machine is: a computer has one screen and one keyboard, and pretending the
// editor's docks are part of the machine would make neither of them convincing.
// The editor stays the place to write a large program; this is the place to type
// `10 PRINT "HELLO"` and see it run.

#pragma once

#include <QHash>
#include <QMainWindow>
#include <QStringList>

#include <memory>

#include "timewarp/ui/hb_dos.hpp"
#include "timewarp/ui/language.hpp"
#include "timewarp/ui/retro_settings.hpp"

class QComboBox;
class QLabel;
class QLineEdit;
class QPlainTextEdit;
class QPushButton;
class QSplitter;
class QTabWidget;
class QVBoxLayout;

namespace timewarp::canvas {
class CanvasCommandQueue;
namespace qt {
class CanvasBridge;
}
}  // namespace timewarp::canvas

namespace timewarp::ui {

class CodeEditor;
class CrtOverlay;
class NativeRunner;
class ReplRunner;
class SoundPlayer;
class TurtleCanvas;

class RetroComputerWindow final : public QMainWindow {
    Q_OBJECT

public:
    /// What the one screen is showing. `Split` is the machine's natural state:
    /// a listing on the left and the drawing on the right.
    enum class ScreenMode {
        Text,
        Graphics,
        Split,
    };

    explicit RetroComputerWindow(QWidget* parent = nullptr);
    ~RetroComputerWindow() override;

    // -- the ROM socket ---------------------------------------------------

    [[nodiscard]] Language language() const noexcept { return language_; }
    /// Cold-boots into `language`. The current ROM's program is kept in its own
    /// memory and restored when the user selects it again.
    void setLanguage(Language language);
    /// The current ROM's program.
    [[nodiscard]] QString program() const;
    /// Replaces the current ROM's program without booting.
    void loadProgram(const QString& source);
    /// Raises the PROGRAM page, the place to read or edit the program. The
    /// machine boots on OUTPUT so the banner is the first thing seen.
    void showProgramPage();

    // -- configuration ----------------------------------------------------

    /// What the machine is set up to do. Session state (the ROM and screen it was
    /// left in) is not here; that is persisted by the window itself.
    [[nodiscard]] const RetroComputerSettings& settings() const noexcept { return settings_; }
    /// Applies `settings`, remembers them, and repaints. Boot ROM and boot screen
    /// are used at the *next* power-on rather than switching the machine now:
    /// a settings dialog must not throw away the program being typed.
    void setSettings(const RetroComputerSettings& settings);
    /// Opens the configuration screen with a live preview. Returns it so the
    /// caller (and a test) can reach its controls; cancelling puts the old look
    /// back and saves nothing.
    RetroComputerSettingsDialog* openSettings();

    // -- the screen -------------------------------------------------------

    [[nodiscard]] ScreenMode screenMode() const noexcept { return mode_; }
    void setScreenMode(ScreenMode mode);

    // -- controls and surfaces, for the window's owner and its tests -------

    [[nodiscard]] QComboBox* languageSelector() const noexcept { return language_selector_; }
    [[nodiscard]] QComboBox* modeSelector() const noexcept { return mode_selector_; }
    [[nodiscard]] QLineEdit* promptInput() const noexcept { return input_; }
    [[nodiscard]] QPushButton* configButton() const noexcept { return config_button_; }
    [[nodiscard]] QLabel* promptLabel() const noexcept { return prompt_label_; }
    [[nodiscard]] QPlainTextEdit* consoleView() const noexcept { return console_; }
    [[nodiscard]] CodeEditor* programEditor() const noexcept { return program_; }
    [[nodiscard]] TurtleCanvas* graphicsCanvas() const noexcept { return canvas_; }
    [[nodiscard]] QTabWidget* textTabs() const noexcept { return text_tabs_; }

    /// Feeds one line to the prompt exactly as Return would. Public so a test can
    /// drive the machine without synthesising key events.
    void submitCommand(const QString& command);
    /// The disk this machine is using. Public so a test can assert what a
    /// catalogue should have shown without reading the console back.
    [[nodiscard]] const HbDos& disk() const noexcept { return dos_; }
    [[nodiscard]] QLabel* statusLabel() const noexcept { return status_label_; }
    /// Runs the program in the editor through the selected language's engine.
    void runProgram();
    /// Stops a program that is running. The prompt session is left alone.
    void stopProgram();
    /// Cold-boots the current ROM: clears the screen and the canvas, resets the
    /// prompt session, and prints the boot message again.
    void reboot();

    [[nodiscard]] bool isRunning() const noexcept { return runner_ != nullptr; }

signals:
    /// One prompt line finished. Immediate commands finish synchronously; `RUN`
    /// emits `runFinished` instead, once the program has ended.
    void commandFinished();
    void runFinished();

protected:
    bool eventFilter(QObject* watched, QEvent* event) override;
    void closeEvent(QCloseEvent* event) override;

private:
    void buildUi();
    void buildHeader(QVBoxLayout* column);
    void buildScreen(QVBoxLayout* column);
    void buildPrompt(QVBoxLayout* column);
    void buildStatus(QVBoxLayout* column);

    void appendConsole(const QString& text);
    void appendBootBanner();
    void updatePrompt();
    void updateStatus();
    void updateModeControls();

    void handlePromptLine();
    /// Handles a numbered BASIC line. Returns false when `command` is not one.
    bool handleBasicLine(const QString& command);
    /// Handles the machine's own words (`RUN`, `LIST`, `NEW`, `CLS`, `EDIT`,
    /// `TEXT`, `GRAPHICS`, `SPLIT`, `HELP`, `ROMS`). Returns false when the line
    /// belongs to the language's REPL.
    bool handleMachineCommand(const QString& command);

    void ensureRepl();
    void stopRepl();
    void stopRunner();
    void clearScreenAndCanvas();

    // -- HB DOS -----------------------------------------------------------
    //
    // Each of these is one machine word. They report through the console, in the
    // same voice as the rest of the prompt (`?` for a refusal), and write the
    // volume through `HbDos`, which is what makes them testable without a disk.
    void listDisk();
    void showDiskStatus();
    void saveToDisk(const QString& name);
    void loadFromDisk(const QString& name);
    void runFromDisk(const QString& name);
    void eraseFromDisk(const QString& name);
    void renameOnDisk(const QString& from, const QString& to);
    void lockOnDisk(const QString& name, bool locked);
    void verifyOnDisk(const QString& name);
    void typeFromDisk(const QString& name);
    void formatDisk();

    /// The visual half of a settings change: palette, text sizes, canvas surface
    /// and the glass over the screen.
    void applyMachineLook();
    /// The behavioural half: the prompt history budget.
    void applyHistoryDepth();
    void applySettings(const RetroComputerSettings& settings, bool remember);

    void saveCurrentProgram();
    void saveSettings();
    void restoreSettings();

    Language language_ = Language::Basic;
    ScreenMode mode_ = ScreenMode::Split;
    RetroComputerSettings settings_;
    /// What the machine looked like before the settings dialog opened, so Cancel
    /// can put it back after a live preview.
    RetroComputerSettings settings_before_dialog_;

    QComboBox* language_selector_ = nullptr;
    QComboBox* mode_selector_ = nullptr;
    QPushButton* config_button_ = nullptr;
    QLabel* machine_label_ = nullptr;
    QLabel* tagline_label_ = nullptr;
    QLabel* lamp_label_ = nullptr;
    QTabWidget* text_tabs_ = nullptr;
    CodeEditor* program_ = nullptr;
    QPlainTextEdit* console_ = nullptr;
    TurtleCanvas* canvas_ = nullptr;
    QSplitter* screen_splitter_ = nullptr;
    CrtOverlay* overlay_ = nullptr;
    QLabel* prompt_label_ = nullptr;
    QLineEdit* input_ = nullptr;
    QPushButton* run_button_ = nullptr;
    QPushButton* stop_button_ = nullptr;
    QLabel* status_label_ = nullptr;

    HbDos dos_;
    /// `INIT` erases a disk, so it is the one word that has to be said twice.
    bool init_pending_ = false;

    std::unique_ptr<timewarp::canvas::CanvasCommandQueue> queue_;
    std::unique_ptr<timewarp::canvas::qt::CanvasBridge> bridge_;
    NativeRunner* runner_ = nullptr;
    ReplRunner* repl_ = nullptr;
    SoundPlayer* sound_ = nullptr;

    /// The program each ROM is holding, keyed by `Language` as an int so the
    /// machine can save one ROM's memory while another is selected.
    QHash<int, QString> programs_;
    QStringList history_;
    int history_index_ = 0;
    bool booted_ = false;
    bool prepared_ = false;
    bool settings_restored_ = false;
    /// Graphics that arrive while the screen is in text mode switch it to Split
    /// once. A program that draws should not draw where the user cannot see it.
    bool auto_shown_graphics_ = false;
};

}  // namespace timewarp::ui
