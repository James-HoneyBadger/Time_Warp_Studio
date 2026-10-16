// SPDX-License-Identifier: MIT

#include "timewarp/ui/main_window.hpp"

#include <QAction>
#include <QApplication>
#include <QCloseEvent>
#include <QComboBox>
#include <QDir>
#include <QDockWidget>
#include <QFile>
#include <QFileDialog>
#include <QFileInfo>
#include <QFont>
#include <QHBoxLayout>
#include <QInputDialog>
#include <QKeySequence>
#include <QLabel>
#include <QLineEdit>
#include <QMenu>
#include <QMenuBar>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QSaveFile>
#include <QSettings>
#include <QSignalBlocker>
#include <QSlider>
#include <QSplitter>
#include <QStatusBar>
#include <QTabWidget>
#include <QTextDocument>
#include <QToolBar>
#include <QToolButton>
#include <QVBoxLayout>

#include <functional>
#include <memory>

#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/canvas/host_adapter.hpp"
#include "timewarp/canvas/qt_bridge.hpp"
#include "timewarp/ui/app.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/resizable_tab_widget.hpp"
#include "timewarp/ui/turtle_canvas.hpp"
#include "timewarp/ui/ui_style.hpp"

// Supplied by CMake from the repository's VERSION file, so the window title and
// the packaged version cannot drift apart. The fallback keeps a standalone
// compile of this file working.
#ifndef TIME_WARP_VERSION
#define TIME_WARP_VERSION "0.0.0"
#endif

namespace timewarp::ui {

namespace {

/// Keys under which the layout is persisted. Grouped in one place so a rename
/// cannot leave half the settings orphaned.
constexpr auto kSettingGeometry = "layout/geometry";
constexpr auto kSettingWindowState = "layout/windowState";
constexpr auto kSettingSplitters = "layout/splitters";

constexpr int kMinimumWindowWidth = 900;
constexpr int kMinimumWindowHeight = 600;

// Splitter geometry, transcribed from the Python. These are the numbers that
// decide which pane "wins" a resize, so they are named rather than inlined at
// the call site.
constexpr int kSplitterHandleWidth = 6;
constexpr int kEditorPaneHandleWidth = 5;
constexpr int kMainSplitterStretchLeft = 3;
constexpr int kMainSplitterStretchRight = 2;
constexpr int kMainSplitterEditorWidth = 720;
constexpr int kMainSplitterRightWidth = 480;
constexpr int kEditorColumnHeight = 550;
constexpr int kImmediateModeHeight = 50;
constexpr int kOutputHeight = 200;
constexpr int kCanvasHeight = 300;
constexpr int kToolBarMinimumHeight = 44;

/// The turtle speed slider's range, from the Python toolbar.
constexpr int kSpeedMinimum = 0;
constexpr int kSpeedMaximum = 500;

/// How long a status message stays up before the bar returns to showing the
/// document path. Long enough to read a sentence, short enough that the path --
/// which is the resting state of that corner -- comes back on its own.
constexpr int kStatusMessageMilliseconds = 5000;

// -- document state ---------------------------------------------------------
//
// A document's identity is stored on its editor widget as dynamic properties
// rather than in a table beside the tabs. A table keyed by tab index has to be
// repaired on every close, move and reorder; the one time it is not, the wrong
// file's text ends up under the wrong name.

/// The file a document was loaded from. Absent for a document never saved.
constexpr auto kFilePathProperty = "filePath";
/// The caption for a document with no path yet, e.g. "untitled 2".
constexpr auto kUntitledLabelProperty = "untitledLabel";
/// A language the user picked by hand, as a settings key. Absent means "work it
/// out from the file name", which is the case that has to keep working.
constexpr auto kLanguageOverrideProperty = "languageOverride";

}  // namespace

// ---------------------------------------------------------------------------
// Construction
// ---------------------------------------------------------------------------

MainWindow::MainWindow(QWidget* parent) : QMainWindow(parent) {
    setObjectName(QStringLiteral("mainWindow"));
    setWindowTitle(tr("\U0001F3A8 Time Warp Studio v%1").arg(TIME_WARP_VERSION));
    setMinimumSize(kMinimumWindowWidth, kMinimumWindowHeight);
    // Added last so the overlay sits above the central layout, so it has to be
    // created after the tree it covers exists.
    buildUi();
    setStyleSheet(style::window());
}

// ---------------------------------------------------------------------------
// Layout
// ---------------------------------------------------------------------------

void MainWindow::configureSplitter(QSplitter* splitter) {
    // A collapsible child is a child that can be dragged to zero width, and a
    // zero-width panel has no way back -- there is no handle left to grab. Every
    // splitter in this window holds something the user needs, so none of them
    // are collapsible.
    splitter->setChildrenCollapsible(false);
    // A 1px handle is Qt's default and is nearly impossible to hit with a mouse.
    splitter->setHandleWidth(kSplitterHandleWidth);
}

void MainWindow::buildUi() {
    // Menu first for the two bars' own sake, but the *menu bar* comes after the
    // central column: View's entries read the canvas (the grid tick reports it),
    // and a menu built before the canvas exists can only guess. Building it in the
    // wrong order is invisible until somebody opens View and sees a tick that
    // disagrees with what they are looking at.
    buildToolBar();
    buildStatusBar();
    buildCentralColumn();
    buildDocks();
    buildMenuBar();
}

void MainWindow::buildCentralColumn() {
    auto* central = new QWidget{this};
    central->setObjectName(QStringLiteral("centralWidget"));
    setCentralWidget(central);

    auto* column = new QVBoxLayout{central};
    column->setContentsMargins(5, 5, 5, 5);

    main_splitter_ = new QSplitter{Qt::Horizontal, central};
    main_splitter_->setObjectName(QStringLiteral("mainSplitter"));
    configureSplitter(main_splitter_);

    buildEditorColumn(main_splitter_);
    buildRightColumn(main_splitter_);

    // On resize the editor column grows three times as fast as the output column.
    // Explicit because a QSplitter otherwise distributes the delta by widget size
    // hint, which would let the canvas outgrow the code.
    main_splitter_->setStretchFactor(0, kMainSplitterStretchLeft);
    main_splitter_->setStretchFactor(1, kMainSplitterStretchRight);
    main_splitter_->setSizes({kMainSplitterEditorWidth, kMainSplitterRightWidth});

    column->addWidget(main_splitter_, 1);

    buildFunctionKeyRow(column);

    // Created last, parented to the central widget, sized by the overlay itself.
    crt_overlay_ = new CrtOverlay{central};
}

void MainWindow::buildEditorColumn(QSplitter* parent) {
    // -- editor pane ------------------------------------------------------
    editor_pane_splitter_ = new QSplitter{Qt::Horizontal, parent};
    editor_pane_splitter_->setObjectName(QStringLiteral("editorPaneSplitter"));
    editor_pane_splitter_->setChildrenCollapsible(false);
    editor_pane_splitter_->setHandleWidth(kEditorPaneHandleWidth);

    editor_tabs_ = new ResizableTabWidget{editor_pane_splitter_};
    editor_tabs_->setObjectName(QStringLiteral("editorTabs"));
    editor_tabs_->setTabsClosable(true);
    editor_tabs_->setAccessibleName(tr("Editor Tabs"));
    editor_tabs_->setAccessibleDescription(tr("Code editor tab group"));
    editor_tabs_->enableEnhancedTabBar();
    editor_pane_splitter_->addWidget(editor_tabs_);

    // The Python calls `new_file()` at this point so a fresh window has a document
    // to edit rather than an empty box, and so does this.
    newFile();
    // Closing is the tab widget's signal and the window's decision: whether the
    // last tab may be closed, and whether a new one appears in its place, is
    // application policy, not a property of a tab bar.
    connect(editor_tabs_, &ResizableTabWidget::closeRequested, this,
            [this](int index) { closeTab(index); });
    connect(editor_tabs_, &QTabWidget::currentChanged, this,
            [this](int /*index*/) { refreshDocumentState(); });

    // The secondary pane exists from the start and is hidden, rather than being
    // created on demand: splitter sizes are saved by child index, so a pane that
    // appears later would renumber everything the user had already arranged.
    editor_tabs_secondary_ = new ResizableTabWidget{editor_pane_splitter_};
    editor_tabs_secondary_->setObjectName(QStringLiteral("editorTabsSecondary"));
    editor_tabs_secondary_->setTabsClosable(true);
    editor_tabs_secondary_->setAccessibleName(tr("Secondary Editor Tabs"));
    editor_tabs_secondary_->enableEnhancedTabBar();
    editor_tabs_secondary_->hide();
    editor_pane_splitter_->addWidget(editor_tabs_secondary_);
    connect(editor_tabs_secondary_, &ResizableTabWidget::closeRequested, this,
            [this](int index) { editor_tabs_secondary_->removeTab(index); });

    editor_pane_splitter_->setStretchFactor(0, 1);
    editor_pane_splitter_->setStretchFactor(1, 1);

    // -- column: editor above, REPL bar below ------------------------------
    editor_column_splitter_ = new QSplitter{Qt::Vertical, parent};
    editor_column_splitter_->setObjectName(QStringLiteral("editorColumnSplitter"));
    configureSplitter(editor_column_splitter_);

    editor_column_splitter_->addWidget(editor_pane_splitter_);
    editor_column_splitter_->setStretchFactor(0, 1);

    immediate_mode_ = new ImmediateModePanel{editor_column_splitter_};
    editor_column_splitter_->addWidget(immediate_mode_);
    // Zero stretch: the REPL bar keeps its 44-50px and all spare height goes to
    // the editor. With a non-zero stretch it would creep open on every resize.
    editor_column_splitter_->setStretchFactor(1, 0);
    editor_column_splitter_->setSizes({kEditorColumnHeight, kImmediateModeHeight});

    parent->addWidget(editor_column_splitter_);
}

void MainWindow::buildRightColumn(QSplitter* parent) {
    right_tabs_ = new ResizableTabWidget{parent};
    right_tabs_->setObjectName(QStringLiteral("rightTabs"));
    right_tabs_->setAccessibleName(tr("Output and Tooling"));
    // Deliberately *not* `enableEnhancedTabBar()` and not closable, matching the
    // Python: these are fixed panes, not documents. Movable tabs here would let a
    // user reorder Output behind Debug and reorder them back wrong; a close menu
    // would offer to remove the canvas entirely.
    // The ResizableTabWidget base still matters, because these pages are large
    // and their size hints must not pin the window's minimum.

    buildOutputColumn();
    right_tabs_->addTab(output_canvas_splitter_,
                        tr("\U0001F4DD Output  \U0001F3A8 Graphics"));

    auto* variables = new PlaceholderPanel{
        tr("\U0001F50D Variables"), tr("ui/variable_inspector.py \u2014 not yet ported"),
        QSize{160, 100}, right_tabs_};
    right_tabs_->addTab(variables, tr("\U0001F50D Variables"));

    auto* telemetry = new PlaceholderPanel{
        tr("\u26A1 Telemetry HUD"), tr("ui/telemetry_hud.py \u2014 not yet ported"),
        QSize{160, 100}, right_tabs_};
    right_tabs_->addTab(telemetry, tr("\u26A1 Telemetry HUD"));

    auto* debug = new PlaceholderPanel{
        tr("\U0001F41B Debug"), tr("ui/debug_panel.py \u2014 not yet ported"),
        QSize{180, 120}, right_tabs_};
    right_tabs_->addTab(debug, tr("\U0001F41B Debug"));

    // The lesson panel is the one page of the right column that is *not* a
    // placeholder: the Python implementation had a working one, and a learner of
    // eleven is not going to open a terminal instead.
    lesson_panel_ = new LessonPanel{right_tabs_};
    right_tabs_->addTab(lesson_panel_, tr("\U0001F393 Lessons"));
    connect(lesson_panel_, &LessonPanel::curriculumNeeded, this,
            &MainWindow::requestLessonCurriculum);
    connect(lesson_panel_, &LessonPanel::progressNeeded, this,
            &MainWindow::requestLessonProgress);
    connect(lesson_panel_, &LessonPanel::openCodeRequested, this,
            &MainWindow::openLessonCode);
    connect(lesson_panel_, &LessonPanel::checkRequested, this,
            &MainWindow::requestLessonCheck);
    connect(lesson_panel_, &LessonPanel::hintRequested, this,
            &MainWindow::requestLessonHint);
    connect(lesson_panel_, &LessonPanel::solutionRequested, this,
            &MainWindow::requestLessonSolution);
    connect(lesson_panel_, &LessonPanel::resetRequested, this,
            &MainWindow::requestLessonReset);
    // The curriculum is loaded when the tab is first looked at, not when the
    // window is built. Starting a Python process for a user who never opens the
    // lesson panel would be a second of start-up spent on a feature they are not
    // using, and this is the only place that decides to pay it.
    connect(right_tabs_, &QTabWidget::currentChanged, this, [this](int index) {
        if (curriculum_requested_ || lesson_panel_ == nullptr) {
            return;
        }
        if (right_tabs_->widget(index) == lesson_panel_) {
            curriculum_requested_ = true;
            emit lesson_panel_->curriculumNeeded();
        }
    });

    parent->addWidget(right_tabs_);
}

void MainWindow::buildOutputColumn() {
    // Output above graphics in a *persistent* vertical split, not tabs: a program
    // that prints and then draws would otherwise flip the user away from the
    // output they are reading.
    output_canvas_splitter_ = new QSplitter{Qt::Vertical, right_tabs_};
    output_canvas_splitter_->setObjectName(QStringLiteral("outputCanvasSplitter"));
    output_canvas_splitter_->setChildrenCollapsible(false);
    output_canvas_splitter_->setHandleWidth(kEditorPaneHandleWidth);

    output_sub_tabs_ = new QTabWidget{output_canvas_splitter_};
    output_sub_tabs_->setObjectName(QStringLiteral("outputSubTabs"));
    // South, so the sub-tab strip sits against the divider and the two panes read
    // as one contiguous surface.
    output_sub_tabs_->setTabPosition(QTabWidget::South);
    output_sub_tabs_->setDocumentMode(true);

    // The console is a real read-only view rather than a placeholder, because
    // program output has to have somewhere to go the moment anything can run.
    console_view_ = new QPlainTextEdit{output_sub_tabs_};
    console_view_->setObjectName(QStringLiteral("consoleOutput"));
    console_view_->setReadOnly(true);
    console_view_->setFont(QFont{QStringLiteral("Courier New"), 10});
    console_view_->setAccessibleName(tr("Program Output"));
    console_view_->setPlaceholderText(tr("Program output appears here\u2026"));
    // Wrapping is off: a program that prints a table should keep its columns,
    // and a horizontal scroll bar is a better answer than quietly reflowing it.
    console_view_->setLineWrapMode(QPlainTextEdit::NoWrap);
    output_sub_tabs_->addTab(console_view_, tr("\U0001F4DD Console"));

    errors_view_ = new QPlainTextEdit{output_sub_tabs_};
    errors_view_->setObjectName(QStringLiteral("errorsLog"));
    errors_view_->setReadOnly(true);
    errors_view_->setFont(QFont{QStringLiteral("Courier New"), 10});
    errors_view_->setAccessibleName(tr("Error Log"));
    errors_view_->setPlaceholderText(tr("Errors and warnings appear here\u2026"));
    output_sub_tabs_->addTab(errors_view_, tr("\u274C Errors"));

    auto* turtle_log = new QPlainTextEdit{output_sub_tabs_};
    turtle_log->setObjectName(QStringLiteral("turtleLog"));
    turtle_log->setReadOnly(true);
    turtle_log->setFont(QFont{QStringLiteral("Courier New"), 10});
    turtle_log->setAccessibleName(tr("Turtle Command Log"));
    turtle_log->setPlaceholderText(tr("Turtle \U0001F422 commands appear here\u2026"));
    output_sub_tabs_->addTab(turtle_log, tr("\U0001F422 Turtle Log"));

    auto* terminal = new PlaceholderPanel{
        tr("\U0001F4BB Terminal"), tr("ui/terminal_widget.py \u2014 not yet ported"),
        QSize{200, 80}, output_sub_tabs_};
    output_sub_tabs_->addTab(terminal, tr("\U0001F4BB Terminal"));

    output_canvas_splitter_->addWidget(output_sub_tabs_);

    // The real canvas, not a stand-in. It consumes the command queue through
    // the bridge below, so a turtle program's output has somewhere to land.
    turtle_canvas_ = new TurtleCanvas{output_canvas_splitter_};
    output_canvas_splitter_->addWidget(turtle_canvas_);
    output_canvas_splitter_->setSizes({kOutputHeight, kCanvasHeight});

    // The queue takes no ownership and does not start a thread: the producer
    // pushes from whichever thread runs the interpreter, and the bridge drains
    // on this one. That split is why a turtle command costs one lock-free push
    // instead of a queued signal delivery.
    canvas_queue_ = std::make_unique<timewarp::canvas::CanvasCommandQueue>();
    canvas_bridge_ = std::make_unique<timewarp::canvas::qt::CanvasBridge>(*canvas_queue_, this);
    canvas_bridge_->set_sink([this](timewarp::canvas::CanvasCommandView view) {
        turtle_canvas_->apply(view);
    });
}

void MainWindow::buildFunctionKeyRow(QVBoxLayout* column) {
    function_key_bar_ = new FunctionKeyBar{this};
    connect(function_key_bar_, &FunctionKeyBar::actionTriggered, this,
            &MainWindow::functionKeyTriggered);
    connect(function_key_bar_, &FunctionKeyBar::deckSelected, this,
            &MainWindow::workspaceDeckSelected);

    // No stretch, so it keeps its fixed 30px and never absorbs leftover height.
    column->addWidget(function_key_bar_);
}

// ---------------------------------------------------------------------------
// Menu bar
// ---------------------------------------------------------------------------

void MainWindow::buildMenuBar() {
    // Every entry here does something.
    //
    // These menus used to be a skeleton: the seven titles were created and every
    // one of them was left empty, on the theory that the feature owning an entry
    // would append to it. Nothing ever did, so File and Edit opened onto nothing
    // at all -- the menu bar looked like navigation and behaved like decoration.
    // The rule now is that an entry is added when it works, which is why there
    // is no Debug or Tools menu: they have nothing to put in them yet, and an
    // empty menu is worse than an absent one (`ui_layout_tests` asserts no menu
    // is empty, and that every entry is connected to a receiver).
    QMenuBar* bar = menuBar();
    bar->setObjectName(QStringLiteral("mainMenuBar"));

    // One helper, so an entry cannot be created without a receiver: `connect`
    // being part of the call is the point of it.
    const auto add = [this](QMenu* menu, const QString& text, const QKeySequence& keys,
                            auto&& slot) {
        QAction* action = menu->addAction(text);
        if (!keys.isEmpty()) {
            action->setShortcut(keys);
        }
        connect(action, &QAction::triggered, this, std::forward<decltype(slot)>(slot));
        return action;
    };

    // -- File -------------------------------------------------------------
    QMenu* file = bar->addMenu(tr("&File"));
    file->setObjectName(QStringLiteral("fileMenu"));
    add(file, tr("&New"), QKeySequence::New, [this] { newFile(); });
    add(file, tr("&Open..."), QKeySequence::Open, [this] { openFileDialog(); });
    file->addSeparator();
    add(file, tr("&Save"), QKeySequence::Save, [this] { saveCurrentFile(); });
    add(file, tr("Save &As..."), QKeySequence::SaveAs, [this] { saveCurrentFileAs(); });
    file->addSeparator();
    add(file, tr("&Close Tab"), QKeySequence::Close, [this] { closeCurrentTab(); });
    // Through close() rather than qApp->quit(), so a modified document still gets
    // its prompt: closeEvent is the one place that asks.
    add(file, tr("&Quit"), QKeySequence::Quit, [this] { close(); });

    // -- Edit -------------------------------------------------------------
    QMenu* edit = bar->addMenu(tr("&Edit"));
    edit->setObjectName(QStringLiteral("editMenu"));
    // Routed through editTarget() rather than bound to one widget, because these
    // must follow focus: the console and the error log are text views too, and an
    // Edit menu that always acted on the code editor would copy the wrong text
    // whenever the user's cursor was somewhere else.
    add(edit, tr("&Undo"), QKeySequence::Undo,
        [this] { if (QPlainTextEdit* target = editTarget()) { target->undo(); } });
    add(edit, tr("&Redo"), QKeySequence::Redo,
        [this] { if (QPlainTextEdit* target = editTarget()) { target->redo(); } });
    edit->addSeparator();
    add(edit, tr("Cu&t"), QKeySequence::Cut,
        [this] { if (QPlainTextEdit* target = editTarget()) { target->cut(); } });
    add(edit, tr("&Copy"), QKeySequence::Copy,
        [this] { if (QPlainTextEdit* target = editTarget()) { target->copy(); } });
    add(edit, tr("&Paste"), QKeySequence::Paste,
        [this] { if (QPlainTextEdit* target = editTarget()) { target->paste(); } });
    edit->addSeparator();
    add(edit, tr("Select &All"), QKeySequence::SelectAll,
        [this] { if (QPlainTextEdit* target = editTarget()) { target->selectAll(); } });

    // -- Run --------------------------------------------------------------
    QMenu* run = bar->addMenu(tr("&Run"));
    run->setObjectName(QStringLiteral("runMenu"));
    // F9 lives here and nowhere else. It used to be on the toolbar button, which
    // would now be a *second* binding for the same key: Qt resolves that by
    // warning about an ambiguous shortcut and firing whichever it prefers, so one
    // owner is not tidiness, it is the difference between working and not.
    add(run, tr("&Run"), QKeySequence{Qt::Key_F9}, [this] { runCurrentDocument(); });
    add(run, tr("&Stop"), QKeySequence{Qt::SHIFT | Qt::Key_F9},
        [this] { stopRunningDocument(); });
    run->addSeparator();
    add(run, tr("Clear &Output"), {}, [this] { clearOutput(); });
    add(run, tr("Clear &Canvas"), {}, [this] { clearCanvas(); });

    // -- View -------------------------------------------------------------
    QMenu* view = bar->addMenu(tr("&View"));
    view->setObjectName(QStringLiteral("viewMenu"));
    // Matched to the wheel step, so the menu and the wheel feel like the same
    // control used two ways.
    constexpr double kZoomStep = 1.25;
    add(view, tr("Zoom &In"), QKeySequence::ZoomIn, [this] {
        if (turtle_canvas_ != nullptr) {
            turtle_canvas_->setZoom(turtle_canvas_->zoom() * kZoomStep);
        }
    });
    add(view, tr("Zoom &Out"), QKeySequence::ZoomOut, [this] {
        if (turtle_canvas_ != nullptr) {
            turtle_canvas_->setZoom(turtle_canvas_->zoom() / kZoomStep);
        }
    });
    add(view, tr("&Fit Drawing"), QKeySequence{Qt::CTRL | Qt::Key_0}, [this] {
        if (turtle_canvas_ != nullptr) {
            turtle_canvas_->fitToContent();
        }
    });
    view->addSeparator();
    // Hand-built rather than through the helper: a checkable entry has to drive
    // its state from the canvas rather than toggle a flag of its own, or the tick
    // and the grid disagree the first time anything else changes the grid.
    QAction* grid = view->addAction(tr("Show &Grid"));
    grid->setCheckable(true);
    grid->setChecked(turtle_canvas_ != nullptr && turtle_canvas_->gridVisible());
    grid->setShortcut(QKeySequence{Qt::CTRL | Qt::Key_G});
    connect(grid, &QAction::toggled, this, [this](bool visible) {
        if (turtle_canvas_ != nullptr) {
            turtle_canvas_->setGridVisible(visible);
        }
    });
    // Re-read the canvas every time the menu opens, so the tick is a statement
    // about the canvas rather than a memory of the last click. Without this the
    // two drift apart the moment anything else changes the grid.
    connect(view, &QMenu::aboutToShow, this, [this, grid] {
        if (turtle_canvas_ != nullptr) {
            QSignalBlocker blocker{grid};
            grid->setChecked(turtle_canvas_->gridVisible());
        }
    });

    // -- Help -------------------------------------------------------------
    QMenu* help = bar->addMenu(tr("&Help"));
    help->setObjectName(QStringLiteral("helpMenu"));
    add(help, tr("&About Time Warp Studio"), {}, [this] {
        QMessageBox::about(
            this, tr("About Time Warp Studio"),
            tr("<b>Time Warp Studio</b> %1"
               "<p>An educational IDE for BASIC, PILOT, Logo, COMAL, REXX and "
               "Scheme, with an integrated turtle graphics canvas."
               "<p>Built with C++20 and Qt6. Scheme and the expression evaluator "
               "run natively; the other five languages run through the bundled "
               "engine in <code>share/time_warp</code>.")
                .arg(timewarp::ui::version()));
    });
    add(help, tr("About &Qt"), {}, [this] { QMessageBox::aboutQt(this); });
}

QPlainTextEdit* MainWindow::editTarget() const {
    // Focus first. Falling back to the current document keeps the menu useful
    // when focus is somewhere that is not a text widget at all -- the toolbar,
    // the canvas, the menu itself.
    if (auto* focused = qobject_cast<QPlainTextEdit*>(QApplication::focusWidget())) {
        return focused;
    }
    return currentEditor();
}

// ---------------------------------------------------------------------------
// Tool bar
// ---------------------------------------------------------------------------

void MainWindow::buildToolBar() {
    // Two rows. The Python build put all thirteen controls on one toolbar, which
    // needs roughly 1900px to fit; below that Qt collapses the overflow behind a
    // chevron, so the display controls at the right-hand end are simply not
    // reachable at any ordinary window size. Splitting by frequency -- actions
    // first, display settings second -- keeps every control on screen at 1000px.
    auto makeBar = [this](const QString& objectName, const QString& title) {
        auto* bar = new QToolBar{title, this};
        bar->setObjectName(objectName);
        // Not movable: the toolbar is the primary control surface, and a floating
        // toolbar can be hidden behind the window by an accidental drag with no
        // obvious way to bring it back.
        bar->setMovable(false);
        bar->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
        bar->setMinimumHeight(kToolBarMinimumHeight);
        return bar;
    };

    tool_bar_ = makeBar(QStringLiteral("mainToolBar"), tr("Main Toolbar"));
    tool_bar_secondary_ = makeBar(QStringLiteral("secondaryToolBar"), tr("Display Toolbar"));

    addToolBar(Qt::TopToolBarArea, tool_bar_);
    // Without the break the two bars share one row and the split buys nothing.
    addToolBarBreak(Qt::TopToolBarArea);
    addToolBar(Qt::TopToolBarArea, tool_bar_secondary_);

    // -- file group -------------------------------------------------------
    // Standard keys rather than literal "Ctrl+N": Qt translates them per
    // platform, so the same code produces Cmd+N on macOS. The tooltip keeps the
    // Linux spelling the user is reading here.
    //
    // The F-key alternates are the Python's: F2 saves and F3 opens. They are part
    // of the interface the tooltips advertise, so an action whose tooltip says
    // "F3" has to answer to F3.
    auto addFileAction = [this](const QString& text, QKeySequence::StandardKey key,
                                const Qt::Key alternate, const QString& tip,
                                const QString& statusTip,
                                const std::function<void()>& slot) {
        QAction* action = tool_bar_->addAction(text);
        // The standard key first, then the F-key alternate the tooltip promises.
        // A toolbar action whose tooltip says "F3" has to answer to F3.
        QList<QKeySequence> shortcuts{QKeySequence{key}};
        if (alternate != Qt::Key_unknown) {
            shortcuts << QKeySequence{alternate};
        }
        action->setShortcuts(shortcuts);
        action->setToolTip(tip);
        action->setStatusTip(statusTip);
        connect(action, &QAction::triggered, this, slot);
        return action;
    };
    addFileAction(tr("\U0001F4C4 New"), QKeySequence::New, Qt::Key_unknown,
                  tr("Create a new file (Ctrl+N)"),
                  tr("Create a new editor tab"), [this] { newFile(); });
    addFileAction(tr("\U0001F4C2 Open"), QKeySequence::Open, Qt::Key_F3,
                  tr("Open an existing file (F3 / Ctrl+O)"),
                  tr("Open a file from disk"), [this] { openFileDialog(); });
    addFileAction(tr("\U0001F4BE Save"), QKeySequence::Save, Qt::Key_F2,
                  tr("Save the current file (F2 / Ctrl+S)"),
                  tr("Save changes to disk"), [this] { saveCurrentFile(); });


    tool_bar_->addSeparator();

    // -- language selector -------------------------------------------------
    // The label and the combo are handed to the toolbar rather than parented to
    // it here: `addWidget` transfers ownership, and a parent that disagrees with
    // the toolbar's layout would be a second owner of the same widget.
    tool_bar_->addWidget(new QLabel{tr(" Language: ")});
    language_combo_ = new QComboBox;
    language_combo_->setObjectName(QStringLiteral("languageSelector"));
    language_combo_->setToolTip(tr("Programming language for the current tab"));
    language_combo_->setMinimumWidth(120);
    for (const Language language : allLanguages()) {
        // The settings key is the item's data, not its text: persisted state and
        // the runner protocol both carry "logo", and "Logo" would have to be
        // parsed back out of a display string to recover it.
        language_combo_->addItem(displayName(language), settingsKey(language));
    }
    // A deliberate choice is recorded as an override on the document, so that
    // selecting REXX for a file called `thing.txt` is not undone the next time
    // the badges refresh and derive the language from the name again.
    connect(language_combo_, &QComboBox::currentIndexChanged, this,
            [this](int index) {
                if (index < 0) {
                    return;
                }
                if (CodeEditor* editor = currentEditor(); editor != nullptr) {
                    editor->setProperty(kLanguageOverrideProperty,
                                        language_combo_->itemData(index));
                    // The badge, the title and the tab label are all derived from
                    // the language, so the choice is only visible once they are
                    // re-derived. Without this the selector changes and nothing
                    // else does, which reads as a control that does not work.
                    refreshDocumentState();
                }
            });
    tool_bar_->addWidget(language_combo_);

    tool_bar_->addSeparator();

    // -- run group ---------------------------------------------------------
    // A menu button because Run has variants (run selection, run in a fresh
    // session) that the default action must not consume; `MenuButtonPopup` keeps
    // a plain click on the main action.
    auto* run_button = new QToolButton{tool_bar_};
    run_button->setObjectName(QStringLiteral("runButton"));
    run_button->setText(tr("\U0001F680 Run"));
    run_button->setToolTip(tr("Execute the current program (F9)"));
    run_button->setStatusTip(tr("Execute the code in the current editor"));
    run_button->setPopupMode(QToolButton::MenuButtonPopup);
    // The chevron opens the run controls. It used to open an *empty* menu: the
    // variants it had been reserved for (run the selection, run in a fresh
    // interpreter) each need a source-parameterised run path that does not exist
    // yet, and a popup that opens onto nothing is the same fault as a dead menu
    // item. What is in it now is what works.
    auto* run_menu = new QMenu{run_button};
    run_menu->setObjectName(QStringLiteral("runButtonMenu"));
    QAction* popup_run = run_menu->addAction(tr("\U0001F680 Run"));
    // No shortcut here: F9 belongs to the Run menu's action, and two actions
    // claiming it is an ambiguous shortcut rather than a convenience.
    connect(popup_run, &QAction::triggered, this, [this] { runCurrentDocument(); });
    QAction* popup_stop = run_menu->addAction(tr("\u23F9\uFE0F Stop"));
    connect(popup_stop, &QAction::triggered, this, [this] { stopRunningDocument(); });
    run_button->setMenu(run_menu);
    connect(run_button, &QToolButton::clicked, this, [this] { runCurrentDocument(); });
    tool_bar_->addWidget(run_button);

    auto addRunAction = [this](const QString& text, const QString& tip) {
        QAction* action = tool_bar_->addAction(text);
        action->setToolTip(tip);
        return action;
    };
    // Actions whose feature has not landed are **disabled**, with the reason in
    // the tooltip. A control that looks live and does nothing is the bug users
    // report; a greyed one with a reason says what is left to build. Stop is
    // wired, because a program that can be started and not stopped is worse than
    // one that cannot be started at all.
    const auto notPorted = [](QAction* action, const QString& reason) {
        action->setEnabled(false);
        action->setToolTip(QStringLiteral("%1 - %2").arg(action->toolTip(), reason));
        return action;
    };
    notPorted(addRunAction(tr("\U0001F41B Debug"), tr("Start debugging")),
              tr("needs the debugger panel, which is not ported yet"));
    notPorted(addRunAction(tr("\u25B6\uFE0F Continue"), tr("Continue to the next breakpoint")),
              tr("needs the debugger panel, which is not ported yet"));
    notPorted(addRunAction(tr("\u2193 Trace"), tr("Step into the next statement")),
              tr("needs the debugger panel, which is not ported yet"));
    QAction* stop_action =
        addRunAction(tr("\u23F9\uFE0F Stop"), tr("Stop the running program"));
    connect(stop_action, &QAction::triggered, this, [this] { stopRunningDocument(); });
    notPorted(addRunAction(tr("\U0001F4CB Snippets"), tr("Insert a code snippet")),
              tr("needs the snippet library, which is not ported yet"));

    QAction* clear_output = tool_bar_secondary_->addAction(tr("\U0001F9F9 Clear Output"));
    clear_output->setToolTip(tr("Clear the output panel"));
    connect(clear_output, &QAction::triggered, this, [this] { clearOutput(); });
    QAction* clear_canvas = tool_bar_secondary_->addAction(tr("\U0001F5BC Clear Canvas"));
    clear_canvas->setToolTip(tr("Clear the graphics canvas"));
    connect(clear_canvas, &QAction::triggered, this, [this] { clearCanvas(); });

    tool_bar_secondary_->addSeparator();
    notPorted(tool_bar_secondary_->addAction(tr("\u267F A+")),
              tr("needs the accessibility presets, which are not ported yet"))
        ->setToolTip(tr("Apply an accessibility preset - needs the accessibility "
                        "presets, which are not ported yet"));

    tool_bar_secondary_->addSeparator();

    // -- turtle speed ------------------------------------------------------
    // `addWidget` gives the label and slider to the toolbar, which is why no
    // parent is passed here: the toolbar parents them and lays them out, and a
    // parent that disagrees with the toolbar's layout would be a second owner.
    auto* speed_label = new QLabel{tr(" \U0001F422 Speed: ")};
    tool_bar_secondary_->addWidget(speed_label);
    speed_slider_ = new QSlider{Qt::Horizontal};
    speed_slider_->setObjectName(QStringLiteral("turtleSpeedSlider"));
    speed_slider_->setRange(kSpeedMinimum, kSpeedMaximum);
    speed_slider_->setMinimumWidth(120);
    speed_slider_->setToolTip(tr("Delay between turtle steps - needs drawing "
                                 "animation, which is not ported yet"));
    tool_bar_secondary_->addWidget(speed_slider_);

    speed_value_ = new QLabel{tr("0 ms")};
    speed_value_->setObjectName(QStringLiteral("turtleSpeedValue"));
    // A fixed width, so the row does not reflow as the number changes width.
    speed_value_->setMinimumWidth(48);
    // A slider that changes the program's timing needs to show the value it
    // actually selected, not just its position on the track.
    connect(speed_slider_, &QSlider::valueChanged, this, [this](int value) {
        speed_value_->setText(tr("%1 ms").arg(value));
    });
    tool_bar_secondary_->addWidget(speed_value_);
    // Disabled until there is animation to control. The drawing currently appears
    // as fast as the program produces it, so a slider that changes nothing is a
    // control that lies about what it does; the readout stays because it is the
    // first half of the feature rather than a promise.
    speed_slider_->setEnabled(false);
}

// ---------------------------------------------------------------------------
// Status bar
// ---------------------------------------------------------------------------

void MainWindow::buildStatusBar() {
    auto* bar = new QStatusBar{this};
    bar->setObjectName(QStringLiteral("mainStatusBar"));
    setStatusBar(bar);

    // Permanent widgets are laid out from the right edge and are not covered by
    // transient messages -- which is the point: the language and cursor position
    // stay visible while a status message is showing.
    language_badge_ = new QLabel{tr("\U0001F522 BASIC"), bar};
    language_badge_->setObjectName(QStringLiteral("languageBadge"));
    bar->addPermanentWidget(language_badge_);

    position_badge_ = new QLabel{tr("Ln 1, Col 1"), bar};
    position_badge_->setObjectName(QStringLiteral("positionBadge"));
    bar->addPermanentWidget(position_badge_);

    execution_badge_ = new QLabel{bar};
    execution_badge_->setObjectName(QStringLiteral("executionBadge"));
    execution_badge_->setToolTip(tr("Current program execution state"));
    bar->addPermanentWidget(execution_badge_);

    // The breadcrumb is the *first* permanent widget, so it sits to the left of
    // the three badges and the message area is left free for messages.
    //
    // It used to be a message-area widget, and that was wrong in two ways: Qt paints
    // a transient message over the message area and hides whatever is in it, so
    // every status message hid the path -- and because the messages had no timeout,
    // the path was never visible at all. A screenshot is what showed it, as messages
    // printed on top of the path.
    breadcrumb_ = new QLabel{QString{}, bar};
    breadcrumb_->setObjectName(QStringLiteral("breadcrumb"));
    // The path is elided when the bar is narrow, so the full path is on the tooltip.
    breadcrumb_->setToolTip(tr("Current document"));
    bar->addPermanentWidget(breadcrumb_);

    setExecutionState(tr("ready"), tr("Ready"));
}

// ---------------------------------------------------------------------------
// Docks
// ---------------------------------------------------------------------------

void MainWindow::buildDocks() {
    auto* outline = new PlaceholderPanel{
        tr("\U0001F4CB Document Outline"),
        tr("ui/document_outline.py \u2014 not yet ported"), QSize{200, 160}, this};

    outline_dock_ = new QDockWidget{tr("\U0001F4CB Document Outline"), this};
    outline_dock_->setObjectName(QStringLiteral("documentOutlineDock"));
    outline_dock_->setWidget(outline);
    // Deliberately not `setAllowedAreas(AllDockWidgetAreas)`: a dock that can be
    // dropped on any edge can be dropped somewhere the user cannot find again.
    outline_dock_->setAllowedAreas(Qt::RightDockWidgetArea | Qt::LeftDockWidgetArea);
    // Hidden at startup; `restoreState()` re-shows it if it was open last session.
    outline_dock_->setVisible(false);
    addDockWidget(Qt::RightDockWidgetArea, outline_dock_);
}

// ---------------------------------------------------------------------------
// Persistence
// ---------------------------------------------------------------------------

void MainWindow::saveLayout() const {
    QSettings settings;
    settings.setValue(kSettingGeometry, saveGeometry());
    // Docks and toolbars have no useful geometry to save without an
    // `objectName` on each of them, which is why naming the widgets is a
    // precondition for this working at all.
    settings.setValue(kSettingWindowState, saveState());

    // Qt saves dock and toolbar arrangement but knows nothing about splitters, so
    // the editor/output divider survives a restart only because of this block.
    settings.beginWriteArray(kSettingSplitters, 4);
    const QSplitter* const splitters[] = {main_splitter_, editor_column_splitter_,
                                          editor_pane_splitter_,
                                          output_canvas_splitter_};
    for (int index = 0; index < 4; ++index) {
        settings.setArrayIndex(index);
        // The index is positional. Reordering the array above without bumping
        // the count or the product version silently restores the wrong geometry,
        // so the worst case is only ever a reset to defaults.
        settings.setValue(QStringLiteral("state"), splitters[index]->saveState());
    }
    settings.endArray();
}

void MainWindow::restoreLayout() {
    QSettings settings;
    if (const QVariant geometry = settings.value(kSettingGeometry);
        geometry.isValid()) {
        restoreGeometry(geometry.toByteArray());
    }
    if (const QVariant state = settings.value(kSettingWindowState);
        state.isValid()) {
        restoreState(state.toByteArray());
    }

    QSplitter* const splitters[] = {main_splitter_, editor_column_splitter_,
                                    editor_pane_splitter_, output_canvas_splitter_};
    const int stored = settings.beginReadArray(kSettingSplitters);
    for (int index = 0; index < 4; ++index) {
        settings.setArrayIndex(index);
        const QByteArray state =
            settings.value(QStringLiteral("state")).toByteArray();
        // Only restore what was actually written. A settings file from an older
        // build, or a truncated one, then leaves that splitter at its default
        // instead of producing a window with a zero-width pane and no way back.
        if (index < stored && !state.isEmpty()) {
            splitters[index]->restoreState(state);
        }
    }
    settings.endArray();
}

void MainWindow::closeEvent(QCloseEvent* event) {
    // Anything running is stopped first. `~QThread` joins and the worker holds a
    // child process, so closing the window during a runaway program would
    // otherwise appear to hang -- and the stop flag is checked per form, so the
    // join is bounded by one form rather than by the program.
    stopEngines();

    // Every open document gets its say before the window goes, and any Cancel
    // aborts the whole close. The Python window has no such prompt: it discards
    // unsaved edits without asking, which loses a lesson's worth of typing with
    // no way to get it back.
    for (int index = 0; index < editor_tabs_->count(); ++index) {
        if (!maybeSave(qobject_cast<CodeEditor*>(editor_tabs_->widget(index)))) {
            event->ignore();
            return;
        }
    }
    saveLayout();
    event->accept();
}

void MainWindow::setExecutionState(const QString& state, const QString& message) {
    execution_badge_->setText(state);
    statusBar()->showMessage(message, kStatusMessageMilliseconds);
}

// ---------------------------------------------------------------------------
// Documents
// ---------------------------------------------------------------------------

QString MainWindow::pathOf(const CodeEditor* editor) {
    return editor == nullptr ? QString{} : editor->property(kFilePathProperty).toString();
}

QString MainWindow::nameOf(const CodeEditor* editor) {
    if (editor == nullptr) {
        return {};
    }
    const QString path = pathOf(editor);
    if (!path.isEmpty()) {
        return QFileInfo{path}.fileName();
    }
    return editor->property(kUntitledLabelProperty).toString();
}

Language MainWindow::languageOf(const CodeEditor* editor) {
    if (editor == nullptr) {
        return defaultLanguage();
    }
    const QVariant chosen = editor->property(kLanguageOverrideProperty);
    if (chosen.isValid()) {
        const Language explicitChoice = languageFromKey(chosen.toString());
        if (explicitChoice != Language::Unknown) {
            return explicitChoice;
        }
    }
    const Language fromPath = languageForFile(pathOf(editor));
    // An unknown extension is BASIC, which is what the Python registry's
    // `language_name_for_extension` falls back to. Doing it here rather than in
    // `languageForFile` keeps "this file's extension means nothing to me" and
    // "this document runs BASIC" as separate facts.
    return fromPath == Language::Unknown ? defaultLanguage() : fromPath;
}

CodeEditor* MainWindow::currentEditor() const {
    return qobject_cast<CodeEditor*>(editor_tabs_->currentWidget());
}

QStringList MainWindow::openFilePaths() const {
    QStringList paths;
    for (int index = 0; index < editor_tabs_->count(); ++index) {
        const QString path = pathOf(qobject_cast<CodeEditor*>(editor_tabs_->widget(index)));
        if (!path.isEmpty()) {
            paths << path;
        }
    }
    return paths;
}

void MainWindow::adoptEditor(CodeEditor* editor) {
    editor_tabs_->addTab(editor, nameOf(editor));
    editor_tabs_->setCurrentIndex(editor_tabs_->indexOf(editor));

    connect(editor, &CodeEditor::cursorMoved, this,
            [this, editor](int line, int column) {
                // Only the visible editor drives the status bar. A programmatic
                // move in a background tab would otherwise rewrite the position
                // the user is watching, for a document they cannot see.
                if (editor == currentEditor()) {
                    position_badge_->setText(
                        tr("Ln %1, Col %2").arg(line).arg(column));
                }
            });
    connect(editor->document(), &QTextDocument::modificationChanged, this,
            [this, editor](bool) {
                updateTabLabel(editor);
                // The window title carries the same marker, so a document that
                // becomes dirty while it is the visible one has to re-title the
                // window. Guarded on the current editor so that an edit in a
                // background tab cannot rename the window the user is looking at.
                if (editor == currentEditor()) {
                    refreshDocumentState();
                }
            });

    updateTabLabel(editor);
    refreshDocumentState();
}

void MainWindow::updateTabLabel(CodeEditor* editor) {
    const int index = editor_tabs_->indexOf(editor);
    if (index < 0) {
        return;
    }
    // A dot rather than the conventional asterisk: the tab bar already uses `*`
    // nowhere, but a leading `*` next to a file name reads as a wildcard, and this
    // is a marker for "unsaved", not "matches anything".
    const QString marker = editor->document()->isModified() ? tr(" \u25CF") : QString{};
    editor_tabs_->setTabText(index, nameOf(editor) + marker);
    editor_tabs_->setTabToolTip(index, pathOf(editor));
}

void MainWindow::refreshDocumentState() {
    const QString base = tr("\U0001F3A8 Time Warp Studio v%1").arg(TIME_WARP_VERSION);
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        setWindowTitle(base);
        language_badge_->setText(QStringLiteral("\U0001F522 \u2014"));
        position_badge_->setText(QString{});
        breadcrumb_->setText(QString{});
        return;
    }

    const QString path = pathOf(editor);
    setWindowTitle(editor->document()->isModified()
                       ? tr("%1 \u2014 %2*").arg(base, nameOf(editor))
                       : tr("%1 \u2014 %2").arg(base, nameOf(editor)));
    language_badge_->setText(
        tr("\U0001F522 %1").arg(displayName(languageOf(editor))));
    position_badge_->setText(
        tr("Ln %1, Col %2").arg(editor->currentLine()).arg(editor->currentColumn()));
    breadcrumb_->setText(path.isEmpty() ? nameOf(editor) : QDir::toNativeSeparators(path));

    // Syncing the combo emits `currentIndexChanged`, which would write the
    // language of the tab we just left onto the one we just entered. Blocking the
    // signal during the sync is what makes this a display of the document rather
    // than an edit of it.
    const QSignalBlocker blocker{language_combo_};
    const int index = language_combo_->findData(settingsKey(languageOf(editor)));
    if (index >= 0) {
        language_combo_->setCurrentIndex(index);
    }
}

CodeEditor* MainWindow::newFile() {
    auto* editor = new CodeEditor{editor_tabs_};
    ++untitled_count_;
    editor->setProperty(kUntitledLabelProperty,
                        untitled_count_ == 1
                            ? tr("untitled")
                            : tr("untitled %1").arg(untitled_count_));
    adoptEditor(editor);
    editor->setFocus();
    setExecutionState(tr("ready"), tr("New file"));
    return editor;
}

bool MainWindow::openFile(const QString& path, OpenFailure failure) {
    if (path.isEmpty()) {
        return false;
    }
    const QString absolute = QFileInfo{path}.absoluteFilePath();

    // Raise the existing tab rather than loading a second copy. Two copies of one
    // file means the last save wins and the other window's edits vanish, which is
    // the kind of data loss that has no undo.
    for (int index = 0; index < editor_tabs_->count(); ++index) {
        auto* existing = qobject_cast<CodeEditor*>(editor_tabs_->widget(index));
        if (existing != nullptr && pathOf(existing) == absolute) {
            editor_tabs_->setCurrentIndex(index);
            existing->setFocus();
            return true;
        }
    }

    QFile file{absolute};
    if (!file.open(QIODevice::ReadOnly)) {
        const QString reason = file.errorString();
        if (failure == OpenFailure::ReportToUser) {
            QMessageBox::warning(this, tr("Open failed"),
                                 tr("Cannot read %1.\n\n%2")
                                     .arg(QDir::toNativeSeparators(absolute), reason));
        }
        setExecutionState(tr("ready"),
                          tr("Could not open %1: %2")
                              .arg(QFileInfo{absolute}.fileName(), reason));
        return false;
    }
    const QByteArray bytes = file.readAll();
    file.close();

    QString text = QString::fromUtf8(bytes);
    // A byte-order mark is not part of the program. Left in place it is an
    // invisible first character, so the first token of the first line fails to
    // parse with an error pointing at a column that looks empty.
    if (text.startsWith(QChar{0xFEFF})) {
        text.remove(0, 1);
    }

    auto* editor = new CodeEditor{editor_tabs_};
    editor->setPlainText(text);
    editor->setProperty(kFilePathProperty, absolute);
    adoptEditor(editor);
    // `setPlainText` already clears the modified flag; being explicit means a
    // future change to how the text is loaded cannot make a just-opened file look
    // dirty and prompt for a save it does not need.
    editor->document()->setModified(false);
    updateTabLabel(editor);
    editor->setFocus();

    setExecutionState(tr("ready"), tr("Opened %1").arg(QFileInfo{absolute}.fileName()));
    return true;
}

bool MainWindow::openFileDialog() {
    const QString path = QFileDialog::getOpenFileName(this, tr("Open file"), QString{},
                                                      openFileFilter());
    return path.isEmpty() ? false : openFile(path);
}

bool MainWindow::writeDocument(CodeEditor* editor, const QString& path) {
    // QSaveFile writes a temporary and renames it over the original, so a failed
    // or interrupted save leaves the previous contents intact. A plain QFile would
    // have truncated the user's program to zero bytes before discovering it could
    // not write the rest.
    QSaveFile file{path};
    if (!file.open(QIODevice::WriteOnly)) {
        QMessageBox::warning(this, tr("Save failed"),
                             tr("Cannot write %1.\n\n%2")
                                 .arg(QDir::toNativeSeparators(path),
                                      file.errorString()));
        return false;
    }
    const QByteArray bytes = editor->toPlainText().toUtf8();
    if (file.write(bytes) != bytes.size() || !file.commit()) {
        QMessageBox::warning(this, tr("Save failed"),
                             tr("Could not write %1.").arg(QDir::toNativeSeparators(path)));
        return false;
    }

    editor->setProperty(kFilePathProperty, path);
    editor->document()->setModified(false);
    // The new path may name a different language -- saving `untitled` as `draw.logo`
    // has to turn it into a Logo document -- so the label, the badges and the
    // language selector are all refreshed from the path that just changed.
    updateTabLabel(editor);
    refreshDocumentState();
    setExecutionState(tr("ready"), tr("Saved %1").arg(QFileInfo{path}.fileName()));
    return true;
}

bool MainWindow::saveDocument(CodeEditor* editor) {
    if (editor == nullptr) {
        return false;
    }
    const QString path = pathOf(editor);
    return path.isEmpty() ? saveDocumentAs(editor) : writeDocument(editor, path);
}

bool MainWindow::saveDocumentAs(CodeEditor* editor) {
    if (editor == nullptr) {
        return false;
    }
    QString suggested = pathOf(editor);
    if (suggested.isEmpty()) {
        // Suggest the default language's own extension, so accepting the default
        // name produces a file the IDE recognises instead of one it calls BASIC
        // because it cannot tell.
        suggested = QStringLiteral("%1.%2")
                        .arg(nameOf(editor).replace(QLatin1Char(' '), QLatin1Char('_')),
                             extensions(defaultLanguage()).first());
    }
    const QString path = QFileDialog::getSaveFileName(this, tr("Save as"), suggested,
                                                       openFileFilter());
    return path.isEmpty() ? false : writeDocument(editor, path);
}

bool MainWindow::saveCurrentFile() { return saveDocument(currentEditor()); }

bool MainWindow::saveCurrentFileAs() { return saveDocumentAs(currentEditor()); }

bool MainWindow::maybeSave(CodeEditor* editor) {
    if (editor == nullptr || !editor->document()->isModified()) {
        return true;
    }
    const QMessageBox::StandardButton choice = QMessageBox::warning(
        this, tr("Unsaved changes"),
        tr("%1 has unsaved changes.\n\nSave before closing it?").arg(nameOf(editor)),
        QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, QMessageBox::Save);

    if (choice == QMessageBox::Cancel) {
        return false;
    }
    if (choice == QMessageBox::Discard) {
        return true;
    }
    return saveDocument(editor);
}

bool MainWindow::closeTab(int index) {
    // An out-of-range index is refused explicitly. `removeTab` treats one as a
    // no-op rather than an error, so without this check the call would report
    // success for a tab that was never there.
    if (index < 0 || index >= editor_tabs_->count()) {
        return false;
    }
    auto* editor = qobject_cast<CodeEditor*>(editor_tabs_->widget(index));
    if (!maybeSave(editor)) {
        return false;
    }

    editor_tabs_->removeTab(index);
    if (editor != nullptr) {
        // `removeTab` detaches the page but does not delete it, so without this the
        // document and its undo history stay alive until the window closes.
        editor->deleteLater();
    }

    // An editor area of empty grey reads as a window that has broken rather than
    // as a working IDE, so closing the last document opens a new one.
    if (editor_tabs_->count() == 0) {
        newFile();
    }
    refreshDocumentState();
    return true;
}

bool MainWindow::closeCurrentTab() {
    return closeTab(editor_tabs_->currentIndex());
}

// ---------------------------------------------------------------------------
// Execution
// ---------------------------------------------------------------------------

MainWindow::~MainWindow() = default;

timewarp::canvas::CanvasCommandQueue& MainWindow::canvasQueue() noexcept {
    return *canvas_queue_;
}

void MainWindow::appendOutput(const QString& text) {
    if (text.isEmpty() || console_view_ == nullptr) {
        return;
    }
    // `moveCursor` + `insertPlainText` rather than `appendPlainText`, which adds
    // its own paragraph break: the interpreter decides where its lines end, and
    // a view that second-guesses it turns "a" and "b" printed separately into
    // two lines in one build and one line in the other.
    QTextCursor cursor = console_view_->textCursor();
    cursor.movePosition(QTextCursor::End);
    cursor.insertText(text);
    console_view_->setTextCursor(cursor);
}

void MainWindow::reportError(const QString& message) {
    if (message.isEmpty()) {
        return;
    }
    if (errors_view_ != nullptr) {
        QTextCursor cursor = errors_view_->textCursor();
        cursor.movePosition(QTextCursor::End);
        cursor.insertText(message.endsWith(QLatin1Char('\n')) ? message
                                                             : message + QLatin1Char('\n'));
        errors_view_->setTextCursor(cursor);
        // Raised, not merely written. An error the user has to go looking for is
        // how a program that failed looks exactly like a program that drew nothing
        // -- which was a real report about this IDE, and this is the fix for the
        // half of it that is presentation rather than function.
        if (output_sub_tabs_ != nullptr) {
            output_sub_tabs_->setCurrentWidget(errors_view_);
        }
    }
    setExecutionState(tr("error"), message.simplified());
}

void MainWindow::clearOutput() {
    if (console_view_ != nullptr) {
        console_view_->clear();
    }
    if (errors_view_ != nullptr) {
        errors_view_->clear();
    }
    setExecutionState(tr("ready"), tr("Output cleared"));
}

void MainWindow::clearCanvas() {
    if (turtle_canvas_ != nullptr) {
        turtle_canvas_->reset();
    }
    setExecutionState(tr("ready"), tr("Canvas cleared"));
}

void MainWindow::runCurrentDocument() {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        return;
    }

    // Whichever engine ran last is stopped first, whichever engine is about to
    // run. Two programs drawing into one canvas is not something a user can
    // interpret, and a Scheme run left alive while a Logo one starts would be
    // exactly that.
    stopEngines();

    if (languageOf(editor) != Language::Scheme) {
        runInBridge(editor);
        return;
    }

    // A clean surface. The previous run's tail is drained *before* the canvas is
    // cleared, so a command still in the queue cannot land on the new picture.
    canvas_bridge_->drainNow();
    turtle_canvas_->reset();
    if (console_view_ != nullptr) {
        console_view_->clear();
    }
    if (errors_view_ != nullptr) {
        errors_view_->clear();
    }

    auto* runner = new timewarp::debugger::DebuggerRunner{this};
    runner_ = runner;

    // One host per run: it holds the counters for that run, and the queue it
    // writes into outlives it.
    turtle_host_ = std::make_unique<timewarp::canvas::QueuedTurtleHost>(*canvas_queue_);
    turtle_host_->set_wakeup([this] { canvas_bridge_->wake(); });
    runner->set_host(turtle_host_.get());

    connect(runner, &timewarp::debugger::DebuggerRunner::output_available, this,
            [this, runner](const QString& text) {
                // A superseded run must not write into the new run's console. The
                // old runner is deleted before the new one starts, which drops its
                // queued deliveries -- this makes the intent explicit rather than
                // resting on that, because losing it would be silent.
                if (runner_ != runner) {
                    return;
                }
                appendOutput(text);
            });
    connect(runner, &timewarp::debugger::DebuggerRunner::run_finished, this,
            [this, runner](bool completed, const QString& error) {
                if (runner_ != runner) {
                    return;
                }
                finishRun(error, completed);
            });

    setExecutionState(tr("running"), tr("Running %1…").arg(nameOf(editor)));
    runner->load_program(editor->toPlainText(), nameOf(editor));
    runner->start();
}

void MainWindow::stopEngines() {
    if (runner_ != nullptr) {
        // `wait` rather than a delay: the stop flag is checked per form, so this
        // returns as soon as the interpreter reaches the next one.
        runner_->request_stop();
        runner_->wait();
        delete runner_.data();
    }
    // Only when a program is actually in flight. The worker process stays alive
    // between runs on purpose, and cycling it here would pay a Python start every
    // time for nothing.
    if (python_worker_ != nullptr && python_worker_->isRunningProgram()) {
        // Cooperative stop, then a kill if the program is inside a statement that
        // will never return. Both are needed: the first lets a program that is
        // merely slow finish tidily, the second is the only thing that stops a
        // loop with nothing to check a flag between.
        python_worker_->stop();
    }
}

void MainWindow::stopRunningDocument() {
    const bool scheme_running = runner_ != nullptr && runner_->isRunning();
    const bool bridge_running =
        python_worker_ != nullptr && python_worker_->isRunningProgram();
    if (!scheme_running && !bridge_running) {
        setExecutionState(tr("ready"), tr("Nothing is running"));
        return;
    }
    setExecutionState(tr("stopping"), tr("Stopping…"));
    stopEngines();
    setExecutionState(tr("stopped"), tr("Stopped"));
}

void MainWindow::runInBridge(CodeEditor* editor) {
    if (!ensureBridge()) {
        return;
    }

    // The caller has already stopped any program in flight. The worker *process*
    // is left alone: it stays alive between runs on purpose, so Run is a message
    // rather than a Python start.
    canvas_bridge_->drainNow();
    turtle_canvas_->reset();
    if (console_view_ != nullptr) {
        console_view_->clear();
    }
    if (errors_view_ != nullptr) {
        errors_view_->clear();
    }

    setExecutionState(tr("running"), tr("Running %1…").arg(nameOf(editor)));
    if (!python_worker_->run(settingsKey(languageOf(editor)), editor->toPlainText(),
                             nameOf(editor))) {
        reportError(tr("The program could not be started."));
    }
}

bool MainWindow::showPanel(const QString& name) {
    struct Entry {
        const char* key;
        int index;
    };
    // The order matches `buildRightColumn`. A panel inserted in the middle has to
    // be added here too, which is why the mapping is one visible table rather
    // than five `if`s scattered through the file.
    static constexpr Entry kPanels[] = {
        {"output", 0}, {"variables", 1}, {"telemetry", 2}, {"debug", 3}, {"lessons", 4}};
    const QString wanted = name.trimmed().toLower();
    for (const Entry& entry : kPanels) {
        if (wanted == QLatin1String(entry.key)) {
            if (right_tabs_ != nullptr) {
                right_tabs_->setCurrentIndex(entry.index);
            }
            return true;
        }
    }
    return false;
}

bool MainWindow::ensureBridge() {
    if (python_worker_ == nullptr) {
        python_worker_ = std::make_unique<timewarp::bridge::PythonWorker>(*canvas_queue_);
        adoptBridgeWorker();
    }
    QString problem;
    if (!python_worker_->start(&problem)) {
        reportError(problem);
        if (lesson_panel_ != nullptr) {
            lesson_panel_->setFailure(tr("the language engine"), problem);
        }
        return false;
    }
    return true;
}

void MainWindow::openLessonCode(const QString& languageKey, const QString& source,
                                const QString& name) {
    if (source.isEmpty()) {
        return;
    }
    CodeEditor* editor = newFile();
    editor->setPlainText(source);
    // The language is recorded as an *override* rather than guessed from the name:
    // a lesson document is called something like `basic_first_steps-1`, which has
    // no extension for the language table to read.
    editor->setProperty(kLanguageOverrideProperty, languageKey);
    editor->setProperty(kUntitledLabelProperty,
                        name.isEmpty() ? tr("lesson") : name);
    updateTabLabel(editor);
}

void MainWindow::requestLessonCurriculum() {
    if (ensureBridge()) {
        python_worker_->requestLessons();
    }
}

void MainWindow::requestLessonProgress() {
    if (python_worker_ != nullptr && python_worker_->isRunning()) {
        python_worker_->requestLessonProgress();
    }
}

void MainWindow::requestLessonCheck(const QString& lesson, int checkpoint) {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        if (lesson_panel_ != nullptr) {
            lesson_panel_->setFailure(tr("the check"),
                                      tr("open a document with your program in it first"));
        }
        return;
    }
    if (!ensureBridge()) {
        return;
    }
    // The panel has already been told why by `checkLesson` when a program is in
    // flight; nothing here needs to repeat it.
    python_worker_->checkLesson(lesson, checkpoint, editor->toPlainText());
}

void MainWindow::requestLessonHint(const QString& lesson, int checkpoint) {
    if (ensureBridge()) {
        python_worker_->requestHint(lesson, checkpoint);
    }
}

void MainWindow::requestLessonSolution(const QString& lesson, int checkpoint) {
    if (ensureBridge()) {
        python_worker_->requestSolution(lesson, checkpoint);
    }
}

void MainWindow::requestLessonReset(const QString& lesson) {
    if (ensureBridge()) {
        python_worker_->resetLesson(lesson);
    }
}

void MainWindow::adoptBridgeWorker() {
    // Everything the worker produces arrives on this thread, so the canvas is
    // touched only here. There is no lock in this path because there is no second
    // thread to lock against: the process's pipes are delivered by the event loop.
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::commandsQueued, this,
            [this] { canvas_bridge_->drainNow(); });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::outputReceived, this,
            [this](const QString& text) { appendOutput(text); });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::errorReceived, this,
            [this](const QString& text) { reportError(text); });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::poseReceived, this,
            [this](double x, double y, double heading, bool visible) {
                turtle_canvas_->setTurtlePose(QPointF{x, y}, heading, visible);
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::inputRequested, this,
            [this](const QString& prompt) {
                // A modal prompt is right *here*: a program that asked a question
                // and stopped is waiting for an answer, and the answer is not
                // something the IDE can invent.
                bool accepted = false;
                const QString answer = QInputDialog::getText(
                    this, tr("The program is waiting for input"), prompt, QLineEdit::Normal,
                    QString{}, &accepted);
                python_worker_->answerInput(accepted ? answer : QString{});
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::finished, this,
            [this](bool ok, const QString& error, const QStringList& unsupported) {
                canvas_bridge_->drainNow();
                turtle_canvas_->fitToContent();
                if (!error.isEmpty()) {
                    reportError(error);
                    return;
                }
                // A drawing the canvas could not fully render says so. Silence
                // would leave a missing shape to be discovered by looking.
                const QString gaps = unsupported.isEmpty()
                                         ? QString{}
                                         : tr(" — %1 not drawn").arg(unsupported.join(tr(", ")));
                setExecutionState(ok ? tr("ready") : tr("stopped"),
                                  (ok ? tr("Finished") : tr("Stopped")) + gaps);
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::lessonsReceived, this,
            [this](const QList<timewarp::bridge::LessonSummary>& lessons,
                   int presentation_font) {
                if (lesson_panel_ != nullptr) {
                    lesson_panel_->setLessons(lessons);
                    if (presentation_font > 0) {
                        lesson_panel_->setPresentationFont(presentation_font);
                    }
                }
                requestLessonProgress();
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::lessonProgressReceived,
            this,
            [this](const QList<timewarp::bridge::LessonProgress>& rows, int completed,
                   int total) {
                if (lesson_panel_ != nullptr) {
                    lesson_panel_->setProgress(rows, completed, total);
                }
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::lessonHintReceived, this,
            [this](const QString& text, int number, int total) {
                if (lesson_panel_ != nullptr) {
                    lesson_panel_->setHint(text, number, total);
                }
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::lessonSolutionReceived,
            this, [this](const QString& text) {
                if (lesson_panel_ != nullptr) {
                    lesson_panel_->setSolution(text);
                }
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::lessonChecked, this,
            [this](const QString& lesson, int checkpoint, bool passed, const QString& message,
                   bool completed, int finished, int total) {
                if (lesson_panel_ != nullptr) {
                    lesson_panel_->setChecked(lesson, checkpoint, passed, message, completed,
                                              finished, total);
                }
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::lessonResetDone, this,
            [this](const QString& lesson) {
                Q_UNUSED(lesson);
                // The panel shows where the learner is, so the only thing to do
                // after a reset is ask again.
                requestLessonProgress();
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::lessonFailed, this,
            [this](const QString& request, const QString& error) {
                if (lesson_panel_ != nullptr) {
                    lesson_panel_->setFailure(request, error);
                }
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::failed, this,
            [this](const QString& text) {
                canvas_bridge_->drainNow();
                turtle_canvas_->fitToContent();
                reportError(text);
            });
}

void MainWindow::finishRun(const QString& error, bool completed) {
    // Publish the tail of the drawing: the host stages coalesced state changes,
    // so the last pen colour or width can still be sitting in the queue.
    if (turtle_host_ != nullptr) {
        turtle_host_->flush();
    }
    canvas_bridge_->drainNow();

    // Fit the view only now. Refitting on every command would shrink the picture
    // continuously while it is being drawn, which reads as the canvas fighting
    // back.
    turtle_canvas_->fitToContent();

    if (!error.isEmpty()) {
        reportError(error);
        return;
    }
    setExecutionState(completed ? tr("ready") : tr("stopped"),
                      completed ? tr("Finished") : tr("Stopped"));
}

}  // namespace timewarp::ui
