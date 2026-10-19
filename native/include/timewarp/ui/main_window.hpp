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

#include "timewarp/bridge/python_worker.hpp"
#include "timewarp/debugger/debugger_runner.hpp"
#include "timewarp/lang/language.hpp"
#include "timewarp/ui/autosave.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/language.hpp"
#include "timewarp/ui/snippets.hpp"

class QAction;
class QCloseEvent;
class QComboBox;
class QDockWidget;
class QLabel;
class QMenu;
class QPlainTextEdit;
class QSplitter;
class QTabWidget;
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
class ResizableTabWidget;
class SoundPlayer;
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

    // -- editor settings ---------------------------------------------------
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
    /// Scheme goes through the native interpreter on a background thread. The
    /// other five go through the Python bridge, because reimplementing them is
    /// the work the bridge exists to postpone -- and a Run button that refuses to
    /// run five sixths of the advertised languages is not an IDE.
    void runCurrentDocument();
    /// Stops the running program, if any. Safe when nothing is running.
    void stopRunningDocument();
    /// Empties the output console.
    void clearOutput();
    /// Empties the graphics canvas.
    void clearCanvas();

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
    /// The rows only the IDE can vouch for: wall clock, and how much ink actually
    /// reached the canvas. Appended to whatever the engine reported.
    [[nodiscard]] QList<timewarp::bridge::TelemetryRow> ideTelemetryRows() const;
    /// Shows the Scheme runner's stack and variables in the variables and debug
    /// panels. Harmless when nothing is paused: it shows the last known state.
    void showRunnerSnapshot();
    /// Empties three panels that would otherwise show the previous run's state as
    /// if it were this one's.
    void clearRunState();
    /// Starts the wall clock the HUD reports. Called once per run, before the
    /// engine is asked to move.
    void startRunClock();
    /// Stops it and records the elapsed time as the last run's.
    void stopRunClock();

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
    ImmediateModePanel* immediate_mode_ = nullptr;
    FindBar* find_bar_ = nullptr;
    VariablesPanel* variables_panel_ = nullptr;
    TelemetryPanel* telemetry_panel_ = nullptr;
    DebugPanel* debug_panel_ = nullptr;
    /// The CHIP-8 window is a top-level window parented to this one, so closing
    /// the IDE closes it too; `WA_DeleteOnClose` makes the pointer go null when
    /// the learner closes it alone.
    QPointer<Chip8Window> chip8_window_;
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
    QList<timewarp::bridge::TelemetryRow> engine_telemetry_;
    FunctionKeyBar* function_key_bar_ = nullptr;
    CrtOverlay* crt_overlay_ = nullptr;
    QDockWidget* outline_dock_ = nullptr;
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
    /// Created on first use: a palette nobody opens costs nothing.
    CommandPalette* command_palette_ = nullptr;
    /// The View menu's checkable CRT entry, so the function key can drive the
    /// menu's state instead of a second flag that could disagree with it.
    QAction* crt_action_ = nullptr;
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
    /// Program output and errors, both read-only views. Named apart from the
    /// Starts the bridge if it is not running. Distinct from `runInBridge`,
    /// which also clears the console and canvas: a lesson check must not wipe the
    /// drawing the learner is looking at while checking it.
    [[nodiscard]] bool ensureBridge();
    /// Opens a checkpoint's starting program in a new document.
    void openLessonCode(const QString& languageKey, const QString& source,
                        const QString& name);
    /// Sends the lesson requests the panel asks for, once the bridge is up.
    void requestLessonCurriculum();
    void requestLessonProgress();
    void requestLessonCheck(const QString& lesson, int checkpoint);
    void requestLessonHint(const QString& lesson, int checkpoint);
    void requestLessonSolution(const QString& lesson, int checkpoint);
    void requestLessonReset(const QString& lesson);

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

    std::unique_ptr<timewarp::bridge::PythonWorker> python_worker_;

    /// Appends a line to the console, scrolling only if the user was already at
    /// the bottom.
    void appendOutput(const QString& text);
    /// Reports a failure in the error panel and the status bar.
    void reportError(const QString& message);
    /// Stops whichever engine is running, and waits for it to be gone. One place,
    /// because three callers need it and a missed one means two programs drawing
    /// into the same canvas.
    void stopEngines();
    /// Runs `editor` in the Python bridge, starting the worker if needed.
    void runInBridge(CodeEditor* editor, const QString& source);
    /// Runs `editor` on the native engine for a language that has been ported.
    void runNatively(CodeEditor* editor, timewarp::lang::Language language,
                     const QString& source);
    /// Wires a freshly created bridge worker to this window.
    void adoptBridgeWorker();
    /// Tears down the current run and waits for its thread, then empties the
    /// queue so no command from the old run can land on the fresh canvas.
    void finishRun(const QString& error, bool completed);
};

}  // namespace timewarp::ui
