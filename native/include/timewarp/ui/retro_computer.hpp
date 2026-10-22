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
#include <QList>
#include <QMainWindow>
#include <QStringList>

#include <memory>

#include "timewarp/lang/interpreter.hpp"

#include "timewarp/ui/hb_dos.hpp"
#include "timewarp/ui/hb_memory.hpp"
#include "timewarp/ui/hb_tube.hpp"
#include "timewarp/ui/language.hpp"
#include "timewarp/ui/machine_image.hpp"
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
    /// True while `AUTO` is numbering lines.
    [[nodiscard]] bool autoNumbering() const noexcept { return auto_active_; }
    /// True while `TRON` is on: each statement's line is printed as it runs.
    [[nodiscard]] bool tracing() const noexcept { return trace_on_; }
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
    /// The machine's screen: the tube, painted from `memory()`.
    [[nodiscard]] HbTube* consoleView() const noexcept { return tube_; }
    /// The machine's 64K. `POKE` writes it, the tube paints it, and the tests read it.
    [[nodiscard]] const HbMemory& memory() const noexcept { return memory_; }
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

    // -- the machine as a file --------------------------------------------
    //
    // A machine is the ROM it is left in, the screen, every ROM's program, the
    // phosphor and configuration, and the disk. `MachineImage` is that as one
    // versioned document, `SAVEMACHINE`/`LOADMACHINE` are the words, and the
    // player's `--machine` boots one with no IDE around it.

    /// Everything this machine is, right now.
    [[nodiscard]] MachineImage machineImage() const;
    /// Makes this machine that one: the ROM, the screen, the programs, the
    /// configuration, the palette and the volume. The caller still owns the
    /// window's geometry and the settings file, which are not a machine.
    void applyMachineImage(const MachineImage& image, bool boot);
    /// Writes one. A bare name is `<data dir>/machines/<NAME>.hb1`, which is the
    /// path `SAVEMACHINE` uses. False with a reason in `error`.
    [[nodiscard]] bool saveMachine(const QString& path, QString* error = nullptr) const;
    /// Reads one and applies it. A refusal leaves the machine exactly as it was.
    bool loadMachine(const QString& path, QString* error = nullptr);

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

    // -- the BASIC ROM's line editor --------------------------------------
    //
    // A 1982 machine's BASIC is judged by its editor, and this one had only
    // numbered lines. `AUTO` is almost pure prompt behaviour, `RENUM` renumbers
    // *and rewrites every jump*, and the range forms of `LIST` and `DELETE` are
    // what a listing of more than one screen needs.

    /// The words below, which only make sense with line numbers. Called before
    /// `handleMachineCommand`, because `DELETE 20-30` must not reach HB DOS's
    /// `DELETE "NAME"`. False for anything it does not own.
    bool handleBasicEditor(const QString& command);

    /// One line of the listing, as the machine parsed it.
    struct BasicLine {
        int number = 0;
        QString body;
    };
    /// The listing, in the order it is stored.
    [[nodiscard]] QList<BasicLine> basicLines() const;
    void setBasicLines(const QList<BasicLine>& lines);

    /// Renumbers from `start` by `step`, rewriting the targets in every line.
    /// `dangling` counts targets that named a line the program does not have;
    /// they are left alone, because changing a number a program cannot resolve
    /// would turn a runtime error into a different runtime error.
    bool renumberBasic(int start, int step, int* dangling);

    /// `AUTO`: the prompt offers the next line number, and the line that is typed
    /// carries it. An empty line ends it, which is what the period's machines did.
    void startAuto(int start, int step);
    void endAuto();
    void advanceAuto(int stored);

    /// The one-deep backup `NEW` leaves behind. `UNDO` puts it back.
    void rememberForUndo();
    void restoreUndo();
    /// Handles the machine's own words (`RUN`, `LIST`, `NEW`, `CLS`, `EDIT`,
    /// `TEXT`, `GRAPHICS`, `SPLIT`, `HELP`, `ROMS`). Returns false when the line
    /// belongs to the language's REPL.
    bool handleMachineCommand(const QString& command);

    /// The bus a language reaches the machine through. One definition, so the prompt
    /// session and a whole-program run cannot disagree about what a POKE means.
    [[nodiscard]] timewarp::lang::Interpreter::MachineBus machineBus();

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

    /// `SAVE MACHINE` and `LOAD MACHINE`: the words, which report into the console the
    /// way every other machine word does. The window API above is what a test and the
    /// player use.
    void saveMachineTo(const QString& name);
    void loadMachineFrom(const QString& name);

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
    HbTube* tube_ = nullptr;
    TurtleCanvas* canvas_ = nullptr;
    QSplitter* screen_splitter_ = nullptr;
    CrtOverlay* overlay_ = nullptr;
    QLabel* prompt_label_ = nullptr;
    QLineEdit* input_ = nullptr;
    QPushButton* run_button_ = nullptr;
    QPushButton* stop_button_ = nullptr;
    QLabel* status_label_ = nullptr;

    HbDos dos_;
    /// The machine's address space. One 64K, cleared by a cold boot: a machine of
    /// 1982 had one bank of RAM, and the per-ROM program buffer is a convenience of
    /// the PROGRAM page rather than a second memory.
    HbMemory memory_;
    /// `AUTO`'s next line number and step. `auto_active_` is what the prompt asks
    /// before it decides whether to end.
    bool auto_active_ = false;
    int auto_next_ = 10;
    int auto_step_ = 10;
    /// `TRON`/`TROFF`, passed to the runner of the next run.
    bool trace_on_ = false;
    /// What `NEW` erased, once. A 1982 machine had no undo and every owner of one
    /// learned to regret it; this machine keeps one listing and says so.
    QString undo_text_;
    bool undo_available_ = false;
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
