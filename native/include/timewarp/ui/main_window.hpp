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
#include <QMainWindow>
#include <QPointer>
#include <QString>
#include <QStringList>

#include "timewarp/bridge/python_worker.hpp"
#include "timewarp/debugger/debugger_runner.hpp"
#include "timewarp/ui/language.hpp"

class QCloseEvent;
class QComboBox;
class QDockWidget;
class QLabel;
class QPlainTextEdit;
class QSlider;
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
class CrtOverlay;
class FunctionKeyBar;
class ImmediateModePanel;
class LessonPanel;
class PlaceholderPanel;
class ResizableTabWidget;
class TurtleCanvas;

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
    [[nodiscard]] bool showPanel(const QString& name);

    /// The lesson panel, so a test can drive it and a screenshot can show it.
    [[nodiscard]] LessonPanel* lessonPanel() const noexcept { return lesson_panel_; }

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
    void buildToolBar();
    void buildStatusBar();
    void buildDocks();

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
    FunctionKeyBar* function_key_bar_ = nullptr;
    CrtOverlay* crt_overlay_ = nullptr;
    QDockWidget* outline_dock_ = nullptr;
    QToolBar* tool_bar_ = nullptr;
    QToolBar* tool_bar_secondary_ = nullptr;

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
    QSlider* speed_slider_ = nullptr;
    QLabel* speed_value_ = nullptr;

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
    /// The child process that runs the five languages with no C++ interpreter.
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
    void runInBridge(CodeEditor* editor);
    /// Wires a freshly created bridge worker to this window.
    void adoptBridgeWorker();
    /// Tears down the current run and waits for its thread, then empties the
    /// queue so no command from the old run can land on the fresh canvas.
    void finishRun(const QString& error, bool completed);
};

}  // namespace timewarp::ui
