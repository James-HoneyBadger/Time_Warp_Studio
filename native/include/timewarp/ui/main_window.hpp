// SPDX-License-Identifier: MIT
//
// The IDE main window: editor column on the left, output and tooling on the
// right, function-key bar underneath.
//
// Translated from the removed PySide6 implementation's main_window.py.
// (`MainWindow.setup_ui`, `create_toolbar`, `create_statusbar`,
// `_setup_document_outline_dock`, `save_state` / `restore_state`).
//
// #### What is preserved exactly
//
// The widget tree, the nesting, and every sizing decision that changes how the
// window behaves when resized: splitter orientations and handle widths,
// `childrenCollapsible(false)` where the Python sets it, the stretch factors, and
// the initial `setSizes()` values. Those numbers are load-bearing -- they are what
// stops the REPL bar being dragged to nothing and what gives the editor 60% of the
// width -- so they are transcribed rather than re-derived, and asserted by
// `tests/ui_layout_tests.cpp`.
//
// #### What changed, and why
//
//  * **Widgets are named.** Every child gets an `objectName`. Qt needs one for
//    `saveState()`/`restoreState()` to persist dock and toolbar geometry at all,
//    the stylesheet scopes by it, and the layout tests find nodes with it.
//  * **Splitter positions are persisted.** The Python saves window geometry and
//    dock state but *not* splitter positions, so a user's carefully dragged
//    editor/output divider is forgotten on every restart. Fixed here.
//  * **The CRT overlay tracks its target itself** instead of being resized by
//    `MainWindow::resizeEvent`, so reparenting the overlay cannot silently break
//    it. `MainWindow::resizeEvent` is gone as a result.
//  * **Menus delegate.** The Python builds ~600 lines of menu in one method; the
//    action catalogue belongs with the feature that owns it, so this lays out the
//    menus and their structure and leaves the entries to the owning module.
//
// #### Ownership
//
// Every widget is parented, so Qt destroys them: containers take ownership of
// anything passed to `addWidget()`, and the menu/toolbar actions are parented to
// the window. The only raw pointers kept are non-owning views, and each is
// documented as such.

#pragma once

#include <memory>

#include <QByteArray>
#include <QElapsedTimer>
#include <QMainWindow>
#include <QPointer>
#include <QString>
#include <QStringList>

#include <array>

#include "timewarp/ui/reports.hpp"
#include "timewarp/lessons/classroom.hpp"
#include "timewarp/lessons/progress.hpp"
#include "timewarp/debugger/debugger_runner.hpp"
#include "timewarp/ai/config.hpp"
#include "timewarp/ai/prompt.hpp"
#include "timewarp/lang/language.hpp"
#include "timewarp/ui/autosave.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/gif_encoder.hpp"
#include "timewarp/ui/language.hpp"
// The CRT overlay's `Settings` is a nested type, so a forward declaration will not
// do here; `panels.hpp` is where the overlay lives.
#include "timewarp/project/project.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/snippets.hpp"

class QAction;
class QCloseEvent;
class QComboBox;
class QDockWidget;
class QLabel;
class QMenu;
class QPlainTextEdit;
class QShortcut;
class QSplitter;
class QTabWidget;
class QTimer;
class QToolBar;
class QToolButton;
class QVBoxLayout;

namespace timewarp::canvas {
class CanvasCommandQueue;
class QueuedTurtleHost;
namespace qt {
class CanvasBridge;
}
}  // namespace timewarp::canvas

namespace timewarp::ui {
class ClassroomMarker;
}  // namespace timewarp::ui

namespace timewarp::sql {
class Database;
}

namespace timewarp::ui {

class TerminalWidget;

/// The four Monitor styles the removed implementation had. Expressed as a theme
/// plus CRT values rather than as a parallel display system: there is exactly one
/// mechanism, and a preset is a named point in it.
enum class ScreenPreset { Normal, Amber, Green, White };

class AiPanel;
class ClassroomPanel;
class CommandReferencePanel;
class CodeEditor;
class CommandPalette;
class Chip8Window;
class FindInFilesDialog;
class CrtOverlay;
class DebugPanel;
class FindBar;
class FunctionKeyBar;
class ImmediateModePanel;
class LessonPanel;
class NativeRunner;
class PlaceholderPanel;
class ProjectPanel;
class ReplRunner;
class ResizableTabWidget;
class RetroComputerWindow;
class Scrubber;
class SoundPlayer;
class SqlPanel;
class TelemetryPanel;
class TourGuide;
class TurtleCanvas;
class VariablesPanel;

/// The application's top-level window.
///
/// Construct, then `restoreLayout()` to bring back the previous session, then
/// `show()`.
class MainWindow final : public QMainWindow {
    Q_OBJECT

public:
    explicit MainWindow(QWidget* parent = nullptr);
    /// Defined in the .cpp: the members below own engine objects through
    /// `unique_ptr`, and an implicit destructor here would need their complete
    /// types at every translation unit that includes this header.
    ~MainWindow() override;

    // -- layout accessors -------------------------------------------------
    //
    // Non-owning. Exposed because the rest of the application needs to put
    // editors and panels where this class laid out the containers.

    [[nodiscard]] ResizableTabWidget* editorTabs() const noexcept { return editor_tabs_; }
    [[nodiscard]] ResizableTabWidget* secondaryEditorTabs() const noexcept {
        return editor_tabs_secondary_;
    }
    [[nodiscard]] ResizableTabWidget* rightTabs() const noexcept { return right_tabs_; }
    [[nodiscard]] QSplitter* mainSplitter() const noexcept { return main_splitter_; }
    [[nodiscard]] QSplitter* editorColumnSplitter() const noexcept {
        return editor_column_splitter_;
    }
    [[nodiscard]] QSplitter* editorPaneSplitter() const noexcept {
        return editor_pane_splitter_;
    }
    [[nodiscard]] QSplitter* outputCanvasSplitter() const noexcept {
        return output_canvas_splitter_;
    }
    [[nodiscard]] QTabWidget* outputSubTabs() const noexcept { return output_sub_tabs_; }
    /// Brings a right-column panel to the front by name: `output`, `variables`,
    /// `telemetry`, `debug` or `lessons`. False when the name is not one of them.
    ///
    /// Named rather than numbered because the caller is a command line and a
    /// screenshot script: "show the lessons" survives a panel being inserted
    /// above it, and "show tab 4" does not.
    /// Raises a page of the right column by its key ("output", "variables",
    /// "telemetry", "debug", "lessons"). `false` when the key is not a page,
    /// which is what lets a caller holding a name it read from data tell the
    /// difference between "shown" and "nothing happened".
    [[nodiscard]] bool showPanel(const QString& name);

    /// The keys `showPanel` accepts, in tab order.
    ///
    /// Exposed for the tests: a page added to `buildRightColumn` without a key
    /// would still be visible and could not be raised from the palette, the
    /// function keys or a menu, and nothing else in the build would notice.
    [[nodiscard]] static QStringList panelKeys();

    /// The lesson panel, so a test can drive it and a screenshot can show it.
    [[nodiscard]] LessonPanel* lessonPanel() const noexcept { return lesson_panel_; }

    /// The class list panel: the right column's teacher-facing page.
    [[nodiscard]] ClassroomPanel* classroomPanel() const noexcept {
        return classroom_panel_;
    }

    /// The find and replace bar, so a test can drive it.
    [[nodiscard]] FindBar* findBar() const noexcept { return find_bar_; }

    // -- the tooling panels -----------------------------------------------
    //
    // Exposed for the same reason the lesson panel is: a test drives them, and a
    // screenshot shows them.

    [[nodiscard]] VariablesPanel* variablesPanel() const noexcept {
        return variables_panel_;
    }
    [[nodiscard]] TelemetryPanel* telemetryPanel() const noexcept {
        return telemetry_panel_;
    }
    [[nodiscard]] DebugPanel* debugPanel() const noexcept { return debug_panel_; }
    /// The project explorer, so a test can drive it and a screenshot can show it.
    [[nodiscard]] ProjectPanel* projectPanel() const noexcept { return project_panel_; }
    /// The database manager page. Null only when Qt itself is unavailable, which
    /// is the case in which there is no window either.
    [[nodiscard]] SqlPanel* sqlPanel() const noexcept { return sql_panel_; }
    /// The command reference page, so a test can drive it.
    [[nodiscard]] CommandReferencePanel* commandReferencePanel() const noexcept {
        return reference_panel_;
    }
    /// The assistant page, so a test can drive it and a screenshot can show it.
    /// Null when this build has no Qt Network, which is the one case in which the
    /// assistant does not exist at all.
    [[nodiscard]] AiPanel* aiPanel() const noexcept { return ai_panel_; }
    /// The assistant's configuration as this window holds it -- the settings file
    /// at start-up, and whatever the settings dialog last accepted.
    [[nodiscard]] const timewarp::ai::Config& aiConfig() const noexcept {
        return ai_config_;
    }
    /// Raises the assistant's page and puts the cursor in its question box. Opens
    /// its settings instead when there is nothing configured to ask.
    void showAssistant();
    /// Opens the assistant's settings dialog: the endpoint, the model and the key.
    void showAiSettings();
    /// Runs one request about the document in view, for the menu entries that are
    /// a single click rather than a typed question.
    void askAssistant(timewarp::ai::Task task);
    /// The open project's file path, or empty when none is open.
    [[nodiscard]] QString activeProjectPath() const;
    /// Shows the find bar, with the replacement row when `withReplacement`.
    void showFindBar(bool withReplacement);
    /// Finds the next match for the bar's term. From the menu, so it does not
    /// depend on the term's field having focus.
    void findNextMatch(bool backwards);
    /// Asks for a line number and goes there.
    void goToLinePrompt();
    /// Re-indents the current document. Reports in the status bar, because a
    /// format with no visible effect and no message is indistinguishable from a
    /// key that did nothing.
    void formatCurrentDocument();
    /// Opens Find in Files on the current document's folder, pre-filled with the
    /// selection when there is one.
    void showFindInFiles();
    /// Runs only the selected text, in the current document's language, without
    /// writing it to a file. Reports in the status bar when nothing is selected
    /// rather than running the whole program.
    void runSelection();
    /// Clears the canvas and the output and stops anything running, keeping the
    /// document. What "start again from the top" means.
    void resetProgram();
    /// The recent runs, newest first, as `path<TAB>language<TAB>epoch<TAB>title`.
    /// Exposed the way `recentFiles()` is, so a test can check what was recorded.
    [[nodiscard]] QStringList runHistory() const;

    /// Writes a snapshot of every document with unsaved changes, now.
    ///
    /// Public because the timer is not the only caller worth having: a test that
    /// waited two minutes for the interval would not be run.
    void autosaveNow();
    /// One status-bar line when a previous session left copies newer than their
    /// files. Never a dialog: nothing modal runs at startup here.
    void reportRecoveredWork();
    /// Opens the version history for the current document.
    void showVersionHistory();
    /// Opens the documentation browser, on `topic` (a path relative to the
    /// docs root) when one is named, else on the index.
    void showDocumentation(const QString& topic = {});
    /// Opens the current language's tutorial at the word under the cursor. F1.
    void showContextHelp();
    /// The documentation topic F1 would open, relative to the docs root. Empty
    /// for an unknown language, and exposed so a test can check the mapping
    /// without a modal dialog.
    [[nodiscard]] QString contextHelpTopic() const;
    /// Opens the language's keywords with the guide's descriptions. Ctrl+F1.
    void showKeywordInspector();
    /// Rebuilds the outline dock from the current document. Public because the
    /// debounce is not its only caller: switching tabs refreshes immediately, and
    /// a test should not have to wait 350 ms to see the list.
    void refreshOutline();
    /// Opens `text` in a new, unnamed tab marked modified, and returns it. The
    /// second half of `showVersionHistory`, separated because that dialog is
    /// modal: the part worth testing is what a chosen version turns into.
    CodeEditor* openRestoredCopy(const QString& text);
    /// Opens the snippet picker for the current document's language and inserts
    /// the chosen body at the cursor.
    void insertSnippet();
    /// Opens the interactive CHIP-8 window, loading the current document when it
    /// is a CHIP-8 program. One window is reused, so a second invocation raises
    /// the running emulator rather than starting a second one.
    void openChip8Emulator();
    /// Opens the HB-1: one screen shared by text and graphics, a
    /// selectable language ROM, and a prompt below the boot message. The first
    /// invocation seeds it with the current document and language; later ones
    /// raise the machine as the user left it.
    void openRetroComputer();
    /// Shows the guided tour if this installation has not seen it. Called once
    /// from the application's startup path, never from a test's construction.
    void showGuidedTourIfUnseen();
    /// Shows the tour now, even when it has been seen. Help > Guided Tour.
    void showGuidedTour();
    /// Forgets that the tour and its hints were seen. Settings > Help.
    void rearmGuidedTour();
    /// Inserts `snippet` at the cursor: the selection replaces `$SELECTION$`, the
    /// caret lands on `$CURSOR$`, and the whole insertion is one undo step.
    ///
    /// Public and separate from `insertSnippet` because the picker is modal: a
    /// test that had to go through it could not run, and the part worth testing
    /// is the insertion.
    void applySnippet(const Snippet& snippet);
    /// Puts the cursor on `line` (1-based) of the current document, scrolled into
    /// view. Out-of-range values clamp rather than failing: every caller reaches
    /// here from a number it read somewhere.
    void goToLine(int line);
    /// Asks for a path and writes the drawing to it.
    void exportDrawingDialog();
    /// Asks for a path and writes the current program as a PDF.
    void exportCodePdfDialog();
    /// Asks for a path and writes the program, its output and its drawing as one
    /// self-contained HTML page.
    void exportSessionHtmlDialog();
    /// Asks for a printer and sends the current program to it. Present only in a
    /// build with Qt6::PrintSupport; the menu entry is absent otherwise.
    void printCode();
    /// Asks for a printer and sends the drawing to it.
    void printDrawing();
    /// Starts or stops sampling the canvas. Stopping with frames asks for a path
    /// and writes them; stopping with none says so and writes nothing.
    void setGifRecording(bool on);
    /// One sample of the canvas. Skips a blank canvas and merges a frame that did
    /// not change into the delay of the one before it.
    void captureGifFrame();
    /// Asks for a path and writes the current lesson as a Markdown report: the
    /// lesson, its checkpoints with this session's verdicts, the program and the
    /// console.
    void exportLessonSessionMarkdownDialog();
    /// The same report as a PDF.
    void exportLessonSessionPdfDialog();
    /// The current lesson plus the document and console it is being worked in.
    [[nodiscard]] timewarp::ui::exporter::LessonSession currentLessonSession() const;
    /// Puts the drawing on the clipboard as an image.
    void copyDrawingToClipboard();

    [[nodiscard]] ImmediateModePanel* immediateMode() const noexcept {
        return immediate_mode_;
    }
    [[nodiscard]] FunctionKeyBar* functionKeyBar() const noexcept {
        return function_key_bar_;
    }
    [[nodiscard]] CrtOverlay* crtOverlay() const noexcept { return crt_overlay_; }
    [[nodiscard]] QDockWidget* outlineDock() const noexcept { return outline_dock_; }
    [[nodiscard]] QToolBar* mainToolBar() const noexcept { return tool_bar_; }
    /// The second toolbar row. Split from the first because a single row of the
    /// combined controls does not fit a 1400px window without overflowing into a
    /// chevron, which hides the overflow rather than making it reachable.
    [[nodiscard]] QToolBar* secondaryToolBar() const noexcept { return tool_bar_secondary_; }
    [[nodiscard]] TurtleCanvas* turtleCanvas() const noexcept { return turtle_canvas_; }
    /// The strip under the canvas: pose, pen, the screen mode a program asked
    /// for, and the command count. Public because a test reads what a user reads.
    [[nodiscard]] TurtleInspector* turtleInspector() const noexcept {
        return turtle_inspector_;
    }

    /// Applies one command to everything that shows the drawing: the canvas
    /// itself, the command log and the inspector's counters. The bridge's sink is
    /// this function, so a command pushed by a program takes exactly this path.
    void applyCanvasCommand(const timewarp::canvas::CanvasCommandView& view);

    // -- appearance -------------------------------------------------------
    //
    // A theme is one of the 33 colour records in `themes.cpp`. Applying one sets
    // the application palette (which is what most of `ui_style.cpp` reads), the
    // window's theme rules, every open editor's syntax colours and the canvas
    // surface. It is persisted as `appearance/theme`, so the choice survives a
    // restart -- which is the difference between a setting and a demonstration.

    /// Applies `name` and records it. A name this build does not have falls back
    /// to the default rather than leaving the window half themed.
    void applyTheme(const QString& name);
    /// The theme currently applied.
    [[nodiscard]] QString currentThemeName() const { return theme_name_; }
    /// The next theme in the table, wrapping. What Shift+F10 does.
    void cycleTheme();

    // -- accessibility -----------------------------------------------------

    /// Turns the maximum-contrast variant of the current theme on or off. It is
    /// not a theme of its own: every theme has one, so the user keeps the colours
    /// they chose and gains legibility rather than being moved somewhere else.
    void setHighContrast(bool on);
    [[nodiscard]] bool highContrast() const noexcept { return high_contrast_; }

    /// The text-scale ladder. Three steps rather than a slider: a scale that can
    /// be any number invites a learner to hunt for one, and every step has to look
    /// right on all three surfaces.
    static constexpr std::array<int, 3> kFontScales{100, 125, 150};
    /// Scales the editor, the console and the panels together, and persists it.
    /// Anything off the ladder snaps to the nearest step below it (the top step
    /// for anything larger), so a hand-edited settings file cannot produce a
    /// fourth size nothing was designed for.
    void setFontScale(int percent);
    [[nodiscard]] int fontScale() const noexcept {
        return editor_settings_.font_scale_percent;
    }
    /// The next step, wrapping. What the A+ button and Ctrl+Alt+A do.
    void cycleFontScale();

    /// Hides the chrome -- both toolbars, the function-key bar, the status bar and
    /// every dock -- so the editor and the canvas get the window. Esc leaves it.
    ///
    /// Session state, not a setting: a window that opened in focus mode would hide
    /// its own way out, which is a state a user cannot be expected to reason about
    /// at launch.
    void setFocusMode(bool on);
    [[nodiscard]] bool focusMode() const noexcept { return focus_mode_; }

    /// Teaching from the front: the chrome goes away as it does in focus mode,
    /// and the lesson panel and the console grow to the size the class is
    /// projected at. It is a mode rather than a setting for the same reason focus
    /// mode is; Esc leaves it.
    ///
    /// Distinct from focus mode in what it is for: focus mode is for the person at
    /// the keyboard, so it leaves the fonts alone. This one is for a room.
    void setPresentationMode(bool on);
    [[nodiscard]] bool presentationMode() const noexcept { return presentation_mode_; }
    /// The size and window style the class is taught at, as `teach.py present`
    /// wrote them. Kept even while the mode is off, so entering it needs no round
    /// trip; a size of zero means the worker has not answered yet, and the fonts
    /// are then left as the teacher configured them.
    void setPresentationSettings(int fontPoints, bool fullscreen);

    /// The CRT overlay's five effects. Applying them shows the overlay, because a
    /// setting that is on but invisible is a setting the user cannot judge.
    void setCrtSettings(const CrtOverlay::Settings& settings);
    [[nodiscard]] const CrtOverlay::Settings& crtSettings() const noexcept {
        return crt_settings_;
    }
    /// Applies a Monitor style: the theme it is built on plus its CRT values, in
    /// one action. Both are persisted, because a preset the user has to reapply
    /// after every restart is a demonstration rather than a setting.
    void applyScreenPreset(ScreenPreset preset);
    /// Opens the settings dialog with a live preview; Cancel puts the old values
    /// back, and OK persists them.
    void showCrtSettings();
    /// The overlay's effects as stored, with everything off for a fresh profile.
    [[nodiscard]] CrtOverlay::Settings loadCrtSettings() const;
    void saveCrtSettings() const;    // -- editor settings ---------------------------------------------------
    //
    // One settings record, applied to every tab: the editor's font, tab width,
    // gutter, current-line band, auto-indent, checking and completion. Persisted
    // as `editor/*`, and edited in the Preferences dialog (Ctrl+,).

    /// Applies `settings` to every open tab and to every tab opened later.
    /// Persistence is separate, so Cancel can restore what was there.
    void applyEditorSettings(const EditorSettings& settings);
    [[nodiscard]] const EditorSettings& editorSettings() const noexcept {
        return editor_settings_;
    }
    /// The Preferences dialog (Ctrl+,).
    void showPreferences();
    /// Turns the embedded terminal on or off, remembers the choice, and adds or
    /// removes its tab. Public because the setting is also a test seam: a
    /// second window must agree with the first about whether it exists.
    void setTerminalEnabled(bool on);
    [[nodiscard]] bool terminalEnabled() const noexcept { return terminal_enabled_; }

    // -- keys, the palette and actions ------------------------------------
    //
    // One implementation per action, reached by the function-key bar's buttons,
    // by the matching key sequence, by the command palette and by the menus.
    // Before this, the bar's buttons emitted a signal nothing listened to; the
    // fix is not ten new handlers but one dispatcher the bar can name.

    /// Performs an action by its stable id: `"save"`, `"cycle_theme"`, `"run"`.
    ///
    /// An id with no implementation is reported in the status bar rather than
    /// ignored, because a silent no-op is indistinguishable from a broken button.
    void performAction(const QString& action);
    /// Applies a workspace deck: `code_focus`, `split_lab`, `graphics_studio` or
    /// `debugger_deck`.
    void selectDeck(const QString& deckId);
    /// The command palette (F10, Ctrl+Shift+P).
    void showCommandPalette();
    /// The keyboard reference (F1, Shift+F1).
    void showShortcutHelp();

    // -- standalone programs ----------------------------------------------
    //
    // A program compiled into a copy of this executable (`standalone.hpp`) is
    // opened as a *program*: read-only, language taken from the payload rather
    // than the file name, authoring UI hidden, and running before the window is
    // shown. The same window, in a different mode.

    /// Opens `source` in player mode and runs it. Returns false when the language
    /// key is not one this build has.
    bool openStandaloneProgram(const QString& languageKey, const QString& source,
                               const QString& title);
    [[nodiscard]] bool isPlayerMode() const noexcept { return player_mode_; }

    // -- execution --------------------------------------------------------
    //
    // The window owns the pipeline because it is the only object that lives as
    // long as one: the interpreter thread pushes into the queue, the bridge
    // drains it on the GUI thread, and the canvas consumes the result.

    /// Where turtle commands are pushed. Exposed rather than hidden so a runner
    /// can be wired to this canvas without the window knowing about it.
    [[nodiscard]] timewarp::canvas::CanvasCommandQueue& canvasQueue() noexcept;

    // -- documents --------------------------------------------------------
    //
    // A document's identity lives on its widget: the file path is stored as a
    // dynamic property, and the language is derived from that path. Keeping a
    // parallel table of paths keyed by tab index would have to be repaired on
    // every tab close, move and reorder, and the one time it was not would load
    // the wrong file's text into the wrong tab.

    /// Creates an empty tab with a real editor and returns it.
    ///
    /// Not a slot: a slot's return type has to be known to moc, and this returns a
    /// pointer to a class that is only forward-declared here.
    CodeEditor* newFile();

    /// How a failed `openFile` reports the failure.
    enum class OpenFailure {
        /// A dialog. Right for a file the user just picked.
        ReportToUser,
        /// Nothing but the return value. Required for files named on the command
        /// line: a modal dialog at startup blocks a script or a CI run forever,
        /// which is exactly what a mistyped path must not do.
        Silent,
    };

    /// Opens `path`. If it is already open its tab is raised instead of a second
    /// copy being loaded, so the same file cannot be edited in two places and
    /// saved over itself. Returns false, having changed nothing, if the file
    /// cannot be read.
    bool openFile(const QString& path, OpenFailure failure = OpenFailure::ReportToUser);

    /// The editor in the current tab, or `nullptr` when there are no tabs.
    [[nodiscard]] CodeEditor* currentEditor() const;

    /// Paths of every open document that has been saved to disk, in tab order.
    [[nodiscard]] QStringList openFilePaths() const;

    // -- split editor ------------------------------------------------------
    //
    // The second pane exists from construction and is hidden. Splitting moves a
    // document into it rather than creating a second view: two views of one
    // buffer would be two places to edit the same file, and the document's own
    // dirty state would be the only thing keeping them in step.

    /// File > Split Editor Right: moves the current document into the second pane
    /// and shows it. Leaves a fresh document behind, so both halves are usable.
    void splitEditor();
    /// Moves every document back into the primary pane and hides the split.
    void closeSplit();
    /// Moves the current document to the other pane, creating the split first if
    /// it is not open yet.
    void moveTabToOtherPane();
    [[nodiscard]] bool editorSplit() const noexcept { return editor_split_; }
    /// Paths open in the second pane. Stored separately from `openFilePaths`, so a
    /// restart puts each document back on the side it was on.
    [[nodiscard]] QStringList secondaryFilePaths() const;

    // -- comparison --------------------------------------------------------

    /// Run > Run Both Panes: runs both documents headless, one after the other, and
    /// puts a table of output, time and ink in the console. There is deliberately
    /// no shared variable bus: see `benchmark.hpp`.
    void runBothPanes();

public slots:
    /// Writes the current document, prompting for a name the first time. Returns
    /// false if the user cancelled a dialog or the write failed.
    bool saveCurrentFile();
    /// Asks for a name, then writes. Returns false if cancelled or unwritable.
    bool saveCurrentFileAs();
    /// Prompts for a file and opens it. Returns false if cancelled or unreadable.
    bool openFileDialog();
    /// Closes the current tab, prompting for unsaved changes first.
    bool closeCurrentTab();
    /// Closes the tab at `index`, prompting for unsaved changes first.
    bool closeTab(int index);

public:
    /// Runs the current document.
    ///
    /// Scheme goes through its own machine on a background thread and the other
    /// ten through `NativeRunner`, which is `Interpreter` plus the canvas queue. A
    /// project with an entry point runs that file (see `runProject`).
    void runCurrentDocument();
    /// Opens `path` as the project file. The dialog and the session restore both
    /// come through here, and so does a test: the model is the thing under test,
    /// and a modal file dialog is not.
    [[nodiscard]] bool openProject(const QString& path, bool silent = false);
    /// Makes `path` the project's entry point (or clears it), saves, and refreshes
    /// the dock so the play mark moves.
    void setProjectEntry(const QString& path);
    void clearProjectEntry();
    /// Forgets the open project. Its files stay where they are: closing a project
    /// is not deleting anything.
    void closeProject();
    /// Runs the open project's entry point, with the project's root as the
    /// directory its relative file names are confined to. Reports in the status bar
    /// when there is no project or no entry point, rather than running something
    /// the user did not ask for.
    void runProject();
    /// Stops the running program, if any. Safe when nothing is running.
    void stopRunningDocument();
    /// Empties the output console.
    void clearOutput();
    /// Empties the graphics canvas.
    void clearCanvas();
    /// Refreshes the turtle strip from the canvas: the canvas owns the pose, and
    /// the strip is a view of it.
    void refreshTurtleInspector();
    /// Empties the command log and the strip, so the next run's log describes the
    /// next run rather than the last one plus this one.
    void clearTurtleLog();

public:
    // -- window state -----------------------------------------------------

    /// Writes geometry, dock/toolbar state and every splitter position.
    void saveLayout() const;
    /// Reads what `saveLayout()` wrote. Missing or malformed values leave the
    /// layout at its defaults rather than producing a wedged window.
    void restoreLayout();

    // -- sessions ---------------------------------------------------------
    //
    // `saveLayout` brings back the window; these bring back the *documents*, which
    // is the half a user notices. Kept apart because they fail for unrelated
    // reasons and the layout can be reset (`--reset-layout`) without losing the
    // list of files somebody was working on.

    /// Records the open documents so the next launch can reopen them.
    void saveSession() const;
    /// Reopens what the last session had open, skipping files that have since
    /// been deleted or moved. Returns how many were reopened.
    int restoreSession();
    /// The paths File > Open Recent lists, most recent first.
    [[nodiscard]] QStringList recentFiles() const;
    /// Forgets them. File > Open Recent > Clear Menu.
    void clearRecentFiles();

    /// Sets the persistent execution badge and the status message. The badge is a
    /// permanent widget, so it stays visible while the message is transient.
    void setExecutionState(const QString& state, const QString& message);

signals:
    /// A function key was pressed, carrying the action name (e.g. `"run"`).
    void functionKeyTriggered(const QString& action);
    /// A workspace deck was chosen, carrying its stable id.
    void workspaceDeckSelected(const QString& deckId);

protected:
    void closeEvent(QCloseEvent* event) override;

private:
    void buildUi();
    void buildCentralColumn();
    void buildEditorColumn(QSplitter* parent);
    void buildRightColumn(QSplitter* parent);
    void buildOutputColumn();
    void buildFunctionKeyRow(QVBoxLayout* column);
    void buildMenuBar();
    /// Files the 30 function-key actions into the window as shortcuts, taking the
    /// table from `FunctionKeyBar::bindingsFor` so a key and its button cannot
    /// disagree. A key the application already binds is skipped, not duplicated:
    /// two actions on one sequence is an ambiguous shortcut, and Qt then fires
    /// whichever it prefers.
    void buildFunctionKeyShortcuts();
    void buildToolBar();
    void buildStatusBar();
    void buildDocks();

    /// The View menu's theme submenu: one checkable entry per theme.
    void buildThemeMenu(QMenu* view);
    /// Re-reads the current theme's colours into everything that is not a palette
    /// role: syntax highlighting, the canvas surface, the lesson verdict colours.
    void applyThemeToWidgets();

    /// Reads the editor settings out of QSettings, defaulting anything absent.
    void loadEditorSettings();
    /// Writes them back. Called when the Preferences dialog is accepted, not on
    /// every Apply: a live preview that was cancelled should not survive a restart.
    void saveEditorSettings() const;

    /// Names the splitters and asks Qt for child-relative width/height. Kept in
    /// one place because the same three calls are correct for every one of them.
    static void configureSplitter(QSplitter* splitter);

    /// Adds `editor` as a tab, wires its cursor and modification signals, and
    /// refreshes the title. The single place a tab is created, so nothing can
    /// forget to connect the signals.
    void adoptEditor(CodeEditor* editor);

    /// Moves `editor` from whichever pane holds it to `target`, keeping its
    /// caption. Used by the split actions and by session restore, so a document
    /// returns to the side it was on.
    void moveEditorToPane(CodeEditor* editor, QTabWidget* target);
    /// The pane the focused widget is in, or the primary one when focus is
    /// somewhere else (a menu, the console, nothing). `currentEditor` reads it, so
    /// "the current document" means the one the user is working in.
    [[nodiscard]] QTabWidget* activeEditorTabs() const;

    /// Offers to save a modified document. Returns false when the user cancels,
    /// which the caller must treat as "abandon the operation".
    [[nodiscard]] bool maybeSave(CodeEditor* editor);

    /// Rewrites one tab's caption from its path and modification state.
    void updateTabLabel(CodeEditor* editor);

    /// Writes `editor` to `path` and adopts that path as the document's own.
    /// Reports the failure through a dialog and returns false, so a read-only
    /// directory is an error the user sees rather than a silent no-op.
    [[nodiscard]] bool writeDocument(CodeEditor* editor, const QString& path);

    /// Saves `editor`, asking for a path first if it has never been saved.
    [[nodiscard]] bool saveDocument(CodeEditor* editor);
    /// Asks for a path, then saves `editor` there.
    [[nodiscard]] bool saveDocumentAs(CodeEditor* editor);

    /// Called when the current tab or its modification state changes: refreshes
    /// the language badge, the position badge, the breadcrumb and the title.
    void refreshDocumentState();

    /// The text widget an Edit-menu item should act on: whatever holds keyboard
    /// focus when that is a text widget, so Copy inside the error log copies from
    /// the error log, and otherwise the current document's editor.
    [[nodiscard]] QPlainTextEdit* editTarget() const;

    /// Adds a path to the recent list: most recent first, no duplicates, capped.
    void noteRecentFile(const QString& path);
    /// Records a run in the history, newest first and capped.
    void noteRun(const QString& source);
    /// Rebuilds the Run popup from the history, every time it is opened.
    void rebuildRunMenu();

    /// The shared body of Run and Run Selection: `source` is what the engine
    /// sees, which is the document for one and the selection for the other.
    void runSource(CodeEditor* editor, const QString& source);
    /// Rebuilds the recent submenu from the list, every time it is opened.
    void rebuildRecentMenu();
    /// Rebuilds the examples submenu from the programs this installation
    /// shipped, every time it is opened: where they are depends on how the
    /// application was installed, and a menu built once would be a menu of the
    /// directories that happened to exist when the window was created.
    void rebuildExamplesMenu();
    /// Opens an example program as a document.
    void openExample(const QString& path);

    /// Connects the debug panel's buttons to the Scheme runner. Called once, at
    /// construction; the panel's signals look the runner up when they fire,
    /// because a runner exists only for the length of a run.
    void connectDebugPanel();

    /// Runs one line from the REPL bar in the current document's language,
    /// creating or resetting the session when the language changes.
    void submitReplCommand(const QString& command);
    /// Handles the BASIC direct-mode commands that edit or run the document:
    /// numbered lines, LIST, RUN and NEW's document-clearing half. Returns true
    /// when the command was consumed.
    bool handleBasicDirectCommand(const QString& command, CodeEditor* editor);

    // -- projects ---------------------------------------------------------
    //
    // One path through `project::Project`: every panel signal calls one of these,
    // and each one saves the file before telling the panel to redraw. A project
    // edit that is only in a widget is a project edit the next launch loses.

    void newProjectDialog();
    void openProjectDialog();
    [[nodiscard]] bool saveProject();
    void addFilesToProject(const QString& group);
    void addFolderToProject();
    void addGroupToProject();
    void addNewGroupForFile(const QString& path);
    void renameProjectGroup(const QString& group);
    void removeProjectGroup(const QString& group);
    void moveProjectFile(const QString& path, const QString& group);
    void renameProject();
    void revealProjectFolder();
    /// The directory this run's relative file names are confined to: the project's
    /// root when one is open, else the document's own folder, else empty (the
    /// process's working directory).
    [[nodiscard]] QString runFileBase(const CodeEditor* editor) const;
    /// Rebuilds the panel from the model and keeps the dock's title current.
    void refreshProjectPanel();
    void restoreProject();
    void noteRecentProject(const QString& path);

    // -- SQL --------------------------------------------------------------
    void buildSqlPanel();
    void openSqlDatabaseDialog();
    void newSqlDatabaseDialog();
    /// Opens (creating when absent) `path`. Reports a failure unless `silent`.
    [[nodiscard]] bool openSqlDatabase(const QString& path, bool silent = false);
    void restoreSqlDatabase();
    void runSqlQuery(const QString& sql);
    void refreshSqlTables();
    void showSqlSchema(const QString& table);
    void noteRecentDatabase(const QString& path);
    /// The rows only the IDE can vouch for: wall clock, and how much ink actually
    /// reached the canvas. Appended to whatever the engine reported.
    [[nodiscard]] QList<timewarp::ui::TelemetryRow> ideTelemetryRows() const;
    /// Shows the Scheme runner's stack and variables in the variables and debug
    /// panels. Harmless when nothing is paused: it shows the last known state.
    void showRunnerSnapshot();
    /// Re-evaluates every watch against the last bindings published. Called at
    /// each pause, at the end of a run, and when the watch list changes.
    void refreshWatches();
    /// Empties three panels that would otherwise show the previous run's state as
    /// if it were this one's.
    void clearRunState();
    /// Starts the wall clock the HUD reports. Called once per run, before the
    /// engine is asked to move.
    void startRunClock();
    /// Stops it and records the elapsed time as the last run's.
    void stopRunClock();

    // -- the command reference and the assistant ---------------------------
    void buildCommandReference();
    void buildAiPanel();
    /// What the assistant is told about the document in view: the language, the
    /// name, the text, the selection and what the last run printed.
    [[nodiscard]] timewarp::ai::Context aiContext() const;
    /// Puts an answer's code into the document in view, replacing a selection or
    /// landing at the cursor, in one undo step.
    void insertAssistantCode(const QString& code);
    /// Adds or removes the terminal tab to match `terminal_enabled_`, the mode
    /// that is active, and whether this window may run commands at all.
    void refreshTerminalTab();
    /// The folder the terminal should be in: the current document's, or home
    /// when the document has no path yet.
    [[nodiscard]] QString terminalDirectory() const;

    /// `path` for an editor whose document has been saved, empty for a new one.
    [[nodiscard]] static QString pathOf(const CodeEditor* editor);
    /// The language shown for an editor: from its path, or the default when the
    /// document has no name yet.
    [[nodiscard]] static Language languageOf(const CodeEditor* editor);
    /// The name shown for an editor: the file name, or its untitled label.
    [[nodiscard]] static QString nameOf(const CodeEditor* editor);

    // -- layout nodes (all owned by Qt's parent-child tree) ----------------
    ResizableTabWidget* editor_tabs_ = nullptr;
    ResizableTabWidget* editor_tabs_secondary_ = nullptr;
    ResizableTabWidget* right_tabs_ = nullptr;
    QSplitter* main_splitter_ = nullptr;
    QSplitter* editor_column_splitter_ = nullptr;
    QSplitter* editor_pane_splitter_ = nullptr;
    QSplitter* output_canvas_splitter_ = nullptr;
    QTabWidget* output_sub_tabs_ = nullptr;
    /// Created the first time the terminal is enabled, and kept while it is off
    /// so its scrollback survives a presentation or a trip into Settings. The
    /// shell itself is stopped when the tab is not offered.
    TerminalWidget* terminal_widget_ = nullptr;
    bool terminal_enabled_ = false;
    ImmediateModePanel* immediate_mode_ = nullptr;
    /// The REPL bar's engine. One per language session, discarded when the user
    /// runs a document or switches language: a REPL that kept BASIC's variables
    /// after the editor became Logo would be lying about what it can see.
    ReplRunner* repl_runner_ = nullptr;
    timewarp::lang::Language repl_language_ = timewarp::lang::Language::Basic;
    FindBar* find_bar_ = nullptr;
    VariablesPanel* variables_panel_ = nullptr;
    TelemetryPanel* telemetry_panel_ = nullptr;
    DebugPanel* debug_panel_ = nullptr;
    /// The CHIP-8 window is a top-level window parented to this one, so closing
    /// the IDE closes it too; `WA_DeleteOnClose` makes the pointer go null when
    /// the learner closes it alone.
    QPointer<Chip8Window> chip8_window_;
    /// The HB-1 window. Same lifetime rule as the CHIP-8 machine: it is
    /// parented to the IDE and deletes itself when its own close box is used.
    QPointer<RetroComputerWindow> retro_computer_window_;
    /// The tour is a child widget through its overlay; the guide itself persists
    /// "seen" in QSettings.
    TourGuide* tour_ = nullptr;
    /// The run in flight, for the HUD's wall-clock row. Invalid between runs.
    QElapsedTimer run_clock_;
    /// How long the last run took, in milliseconds; negative before the first.
    qint64 last_run_ms_ = -1;
    /// What the engine reported for the run in flight. Held rather than shown
    /// because the wall clock is only known once the run has stopped, and the two
    /// are one table.
    QList<timewarp::ui::TelemetryRow> engine_telemetry_;
    /// The bindings last shown in the Variables tab. Watch expressions are
    /// evaluated against these rather than against a live engine, so a watch
    /// cannot race a runner thread that is already executing again.
    QList<timewarp::ui::VariableRow> last_variables_;
    FunctionKeyBar* function_key_bar_ = nullptr;
    CrtOverlay* crt_overlay_ = nullptr;
    QDockWidget* outline_dock_ = nullptr;
    /// The outline's view. Rebuilt from the current document on a debounce, so
    /// typing does not re-parse the program on every keystroke.
    OutlinePanel* outline_panel_ = nullptr;
    QTimer* outline_timer_ = nullptr;
    /// The Projects dock and its view.
    QDockWidget* project_dock_ = nullptr;
    ProjectPanel* project_panel_ = nullptr;
    /// The open project. Invalid when none is; the panel shows the empty state.
    timewarp::project::Project project_;
    /// Rebuilt on every open, like the recent-files menu.
    QMenu* recent_projects_menu_ = nullptr;
    /// The database manager page, and the one connection it manages. A shared
    /// pointer because the interpreter's SQL callback runs on a runner thread and
    /// must keep the connection alive even if the panel's tab is rebuilt.
    SqlPanel* sql_panel_ = nullptr;
    /// The assistant page and the configuration it holds. The configuration is a
    /// setting (the settings file, edited in its own dialog); the *conversation*
    /// lives in the panel, because a conversation is a session rather than a
    /// preference that should survive a restart.
    AiPanel* ai_panel_ = nullptr;
    timewarp::ai::Config ai_config_;
    /// The command reference: a view over the table the command gate drives.
    CommandReferencePanel* reference_panel_ = nullptr;
    std::shared_ptr<timewarp::sql::Database> sql_database_;
    QToolBar* tool_bar_ = nullptr;
    QToolBar* tool_bar_secondary_ = nullptr;
    /// Rebuilt on every open: a recent list that is only built once is a list of
    /// the files that were open when the window was created.
    QMenu* recent_menu_ = nullptr;
    /// Likewise. The point of this one is that a learner who has just installed
    /// the application should find programs to open without knowing where the
    /// package put them.
    QMenu* examples_menu_ = nullptr;
    /// The Run button's popup, rebuilt on every open from the run history.
    QMenu* run_menu_ = nullptr;
    /// Set while a selection run is in flight, so the history can say which kind
    /// of run it was.
    bool running_selection_ = false;
    /// Snapshots of unsaved work, and the timer that takes them.
    AutosaveStore autosave_;
    QTimer* autosave_timer_ = nullptr;
    /// Created on first use: a dialog nobody opens costs nothing, and the window
    /// that a test builds should not construct a search dialog it never shows.
    FindInFilesDialog* find_in_files_ = nullptr;

    /// The theme in force, and the menu that records it. The actions live in the
    /// View menu and are `QActionGroup`-exclusive, so the tick is the state.
    QString theme_name_;
    QMenu* theme_menu_ = nullptr;
    /// Whether the second editor pane is showing. Kept in step with the pane's own
    /// visibility by the split actions, and persisted with the layout.
    bool editor_split_ = false;
    /// Created on first use: a palette nobody opens costs nothing.
    CommandPalette* command_palette_ = nullptr;
    /// The View menu's checkable CRT entry, so the function key can drive the
    /// menu's state instead of a second flag that could disagree with it.
    QAction* crt_action_ = nullptr;
    /// The View menu's High Contrast entry, kept so `setHighContrast` can move the
    /// tick when something other than the menu asks for the change.
    QAction* high_contrast_action_ = nullptr;
    /// View > Profile Run. Deliberately not persisted: profiling costs a clock
    /// read per statement, so it is opt-in per session rather than a setting that
    /// follows the user into the next launch without them remembering it.
    QAction* profile_action_ = nullptr;
    /// Session state, not a setting: the old implementation's toggle reset on
    /// restart, and a window that opens in high contrast without the user asking
    /// again is a window that forgot why.
    bool high_contrast_ = false;
    /// Focus mode: the action, the Esc shortcut and exactly the widgets this mode
    /// hid, so leaving restores what was there rather than what a default layout
    /// would show.
    QAction* focus_action_ = nullptr;
    QShortcut* focus_shortcut_ = nullptr;
    bool focus_mode_ = false;
    QList<QPointer<QWidget>> focus_hidden_;
    /// Presentation mode: the same recorded-chrome idea, plus the class's font
    /// size and the window style `teach.py present` asked for. The geometry is
    /// restored only when this mode changed it, so a window the user had already
    /// made full screen stays that way.
    QAction* presentation_action_ = nullptr;
    QShortcut* presentation_shortcut_ = nullptr;
    bool presentation_mode_ = false;
    QList<QPointer<QWidget>> presentation_hidden_;
    int presentation_font_ = 0;
    bool presentation_fullscreen_ = true;
    bool presentation_was_maximized_ = false;
    bool presentation_restore_geometry_ = false;
    /// Puts the settings on screen if the mode is on. Called on entering and when
    /// an answer from the worker brings new values, so the size a teacher set is
    /// what the mode uses whether it was read before or after the mode began.
    void applyPresentationSettings();
    /// The CRT effect values in force, and the dialog that edits them.
    CrtOverlay::Settings crt_settings_;
    /// What was in force when the dialog opened. The preview overwrites
    /// `crt_settings_` on every slider move, so Cancel needs a copy of its own.
    CrtOverlay::Settings crt_before_dialog_;
    CrtSettingsDialog* crt_dialog_ = nullptr;
    /// The editor's customisations, as loaded at startup and edited in Preferences.
    EditorSettings editor_settings_;
    /// The one sound player, shared by every run: a `SOUND` from the next program
    /// should stop the previous one anyway, so two players would be one too many.
    SoundPlayer* sound_ = nullptr;
    /// Set by `openStandaloneProgram`. Hides the authoring UI and keeps the
    /// document read-only; never cleared, because there is nothing to return to.
    bool player_mode_ = false;

    /// Serial number for naming untitled documents, so two of them are not both
    /// called "untitled". Never decreases: reusing a name the user just closed
    /// reads as the old document having come back.
    int untitled_count_ = 0;

    // -- status bar -------------------------------------------------------
    QLabel* language_badge_ = nullptr;
    QLabel* position_badge_ = nullptr;
    QLabel* execution_badge_ = nullptr;
    QLabel* breadcrumb_ = nullptr;

    // -- toolbar ----------------------------------------------------------
    QComboBox* language_combo_ = nullptr;

    // -- execution --------------------------------------------------------
    TurtleCanvas* turtle_canvas_ = nullptr;
    /// The pose readout under the canvas, and the command log beside it. Both are
    /// views over what the canvas was told; neither keeps state of its own.
    TurtleInspector* turtle_inspector_ = nullptr;
    TurtleCommandLog* turtle_log_ = nullptr;
    /// The playback strip under the canvas. Hidden until there is ink to
    /// scrub, so an empty window keeps its full canvas height.
    Scrubber* scrubber_ = nullptr;
    /// GIF recording: a 10 fps sample of the canvas. A frame that is identical to
    /// the one before it is merged into it as extra delay, which is what keeps a
    /// recording of a mostly-idle canvas small. `gif_size_` is fixed at the first
    /// kept frame so a window resized mid-recording cannot make frames disagree.
    QTimer* gif_timer_ = nullptr;
    QAction* record_gif_action_ = nullptr;
    QVector<timewarp::ui::gif::Frame> gif_frames_;
    QSize gif_size_;
    /// Program output and errors, both read-only views. Named apart from the
    /// Loads the curriculum and the learner's place in it, once. The lesson
    /// panel is a view over this, not a client of the bridge: a check runs
    /// in-process through `timewarp_lessons`, which is the same function the
    /// teacher's command asks.
    [[nodiscard]] bool ensureLessons();
    /// Loads the class: the roster, the assignment and what has been marked.
    [[nodiscard]] bool ensureClassroom();
    /// Opens a checkpoint's starting program in a new document.
    void openLessonCode(const QString& languageKey, const QString& source,
                        const QString& name);
    /// Sends the lesson requests the panel asks for, once the curriculum is loaded.
    void requestLessonCurriculum();
    void requestLessonProgress();
    void requestLessonCheck(const QString& lesson, int checkpoint);
    void requestLessonHint(const QString& lesson, int checkpoint);
    void requestLessonSolution(const QString& lesson, int checkpoint);
    void requestLessonReset(const QString& lesson);
    /// Asks for the class list; the marks follow when the answer arrives. They
    /// are separate messages because the roster is small and cheap and the
    /// marking is a directory walk, so the panel can show the class list while
    /// the marks are still being worked out.
    void requestClassroom();
    /// Asks for a path and has the engine write the class to it. The archive is
    /// the engine's format, so `teach.py unbundle` can read what this writes.
    void exportClassroomBundle();
    /// Asks for an archive and unpacks it, after asking whether files that are
    /// already there should be replaced.
    void importClassroomBundle();

    /// `outputSubTabs_` container that holds them.
    QPlainTextEdit* console_view_ = nullptr;
    QPlainTextEdit* errors_view_ = nullptr;

    std::unique_ptr<timewarp::canvas::CanvasCommandQueue> canvas_queue_;
    std::unique_ptr<timewarp::canvas::qt::CanvasBridge> canvas_bridge_;
    /// The interpreter-side adapter. One per run, so it is created in
    /// `runCurrentDocument` rather than here; the queue outlives it.
    std::unique_ptr<timewarp::canvas::QueuedTurtleHost> turtle_host_;
    /// `QPointer` because the runner is deleted when its run ends, and the
    /// destructor of a running `QThread` blocks: the window must be able to tell
    /// "gone" from "still running" without a dangling pointer.
    QPointer<timewarp::debugger::DebuggerRunner> runner_;
    /// The in-process engine for a ported language. One per run, like `runner_`,
    /// because a finished run's variables are read from it before the next start.
    QPointer<NativeRunner> native_runner_;
    /// The child process that runs the languages with no C++ interpreter.
    /// Created lazily, because most sessions never run one and starting an
    /// interpreter on launch would cost every user for the sake of some.
    LessonPanel* lesson_panel_ = nullptr;
    /// Set once the curriculum has been asked for, so showing the Lessons tab a
    /// second time does not ask again. The panel keeps its own copy afterwards.
    bool curriculum_requested_ = false;

    /// The teacher's page: the class list and a row of marks per student.
    ClassroomPanel* classroom_panel_ = nullptr;
    /// Set once the class list has been asked for, for the same reason
    /// `curriculum_requested_` exists.
    bool classroom_requested_ = false;

    /// The curriculum and the learner's progress. Loaded on first use, because a
    /// learner who never opens the Lessons tab should not pay for it at start-up.
    std::unique_ptr<timewarp::lessons::ProgressStore> lessons_;
    /// The class the Classroom page shows. Loaded when the page is first raised,
    /// for the same reason the curriculum is: a learner who never opens it should
    /// not pay for it.
    std::unique_ptr<timewarp::lessons::Classroom> classroom_;
    /// Marks the class on its own thread, because a mark runs a learner's program
    /// and a runaway handout takes its whole budget.
    ClassroomMarker* classroom_marker_ = nullptr;

    /// Appends a line to the console, scrolling only if the user was already at
    /// the bottom.
    void appendOutput(const QString& text);
    /// Reports a failure in the error panel and the status bar.
    void reportError(const QString& message);
    /// Stops whichever engine is running, and waits for it to be gone. One place,
    /// because three callers need it and a missed one means two programs drawing
    /// into the same canvas.
    void stopEngines();
    /// Runs `editor` on the native engine for a language that has been ported.
    void runNatively(CodeEditor* editor, timewarp::lang::Language language,
                     const QString& source);
    /// Tears down the current run and waits for its thread, then empties the
    /// queue so no command from the old run can land on the fresh canvas.
    void finishRun(const QString& error, bool completed);
};

}  // namespace timewarp::ui
