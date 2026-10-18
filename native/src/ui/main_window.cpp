// SPDX-License-Identifier: MIT

#include "timewarp/ui/main_window.hpp"

#include "timewarp/lang/executor.hpp"
#include "timewarp/ui/native_runner.hpp"

#include <QAction>
#include <QActionGroup>
#include <QApplication>
#include <QCloseEvent>
#include <QClipboard>
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
#include "timewarp/ui/command_palette.hpp"
#include "timewarp/ui/find_bar.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/preferences.hpp"
#include "timewarp/ui/resizable_tab_widget.hpp"
#include "timewarp/ui/sound_player.hpp"
#include "timewarp/ui/syntax_highlighter.hpp"
#include "timewarp/ui/themes.hpp"
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
/// The documents that were open, so the next launch can reopen them.
constexpr auto kSettingOpenFiles = "session/openFiles";
/// File > Open Recent. Capped, because a menu of forty paths is a menu nobody
/// reads.
constexpr auto kSettingRecentFiles = "session/recentFiles";
constexpr int kRecentFileLimit = 10;
/// The chosen theme's name. A settings value rather than a pointer, so a theme
/// removed from a later build cannot leave a dangling index behind.
constexpr auto kSettingTheme = "appearance/theme";

// The editor's customisations. One key each rather than a serialised blob: a
// settings file a person can read is a settings file a person can fix, and a new
// field in a later build defaults rather than invalidating the rest.
constexpr auto kSettingEditorFont = "editor/fontFamily";
constexpr auto kSettingEditorFontSize = "editor/fontSize";
constexpr auto kSettingEditorTabWidth = "editor/tabWidth";
constexpr auto kSettingEditorLineNumbers = "editor/showLineNumbers";
constexpr auto kSettingEditorCurrentLine = "editor/highlightCurrentLine";
constexpr auto kSettingEditorWordWrap = "editor/wordWrap";
constexpr auto kSettingEditorAutoIndent = "editor/autoIndent";
constexpr auto kSettingEditorCompletion = "editor/completion";
constexpr auto kSettingEditorLint = "editor/lint";

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

/// The size a window is forced to for `--screenshot` when none was asked for.
constexpr int kDefaultScreenshotWidth = 1400;
constexpr int kDefaultScreenshotHeight = 900;
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
    // Before the widget tree: the first document is created while the UI is being
    // built, and it has to be born with the user's font and tab width rather than
    // corrected afterwards.
    loadEditorSettings();
    // Added last so the overlay sits above the central layout, so it has to be
    // created after the tree it covers exists.
    buildUi();
    // After the widget tree and before `restoreLayout()`/`show()`: a palette
    // applied to a visible window repaints it, which shows as a flash of the
    // default colours before the user's theme arrives.
    applyTheme(QSettings{}.value(kSettingTheme, themes::defaultName()).toString());
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
    // Last, because it reads the menus: a key the menus already answer must not
    // get a second action, and the check is against the actions that exist.
    buildFunctionKeyShortcuts();
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

    // The tabs and the find bar share a container so the bar can sit directly
    // under them. Directly, and not under the whole editor column: the column
    // also holds the REPL, and a find bar below *that* would be nowhere near the
    // text it searches.
    auto* editor_pane = new QWidget{editor_pane_splitter_};
    editor_pane->setObjectName(QStringLiteral("editorPane"));
    auto* editor_pane_layout = new QVBoxLayout{editor_pane};
    editor_pane_layout->setContentsMargins(0, 0, 0, 0);
    editor_pane_layout->setSpacing(0);

    editor_tabs_ = new ResizableTabWidget{editor_pane};
    editor_tabs_->setObjectName(QStringLiteral("editorTabs"));
    editor_tabs_->setTabsClosable(true);
    editor_tabs_->setAccessibleName(tr("Editor Tabs"));
    editor_tabs_->setAccessibleDescription(tr("Code editor tab group"));
    editor_tabs_->enableEnhancedTabBar();
    editor_pane_layout->addWidget(editor_tabs_, 1);

    // Hidden until Edit > Find asks for it. It is part of the layout rather than
    // a floating overlay so that it pushes the editor up instead of covering the
    // line the user is reading.
    find_bar_ = new FindBar{editor_pane};
    editor_pane_layout->addWidget(find_bar_);

    editor_pane_splitter_->addWidget(editor_pane);

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

    variables_panel_ = new VariablesPanel{right_tabs_};
    right_tabs_->addTab(variables_panel_, tr("\U0001F50D Variables"));

    telemetry_panel_ = new TelemetryPanel{right_tabs_};
    right_tabs_->addTab(telemetry_panel_, tr("\u26A1 Telemetry HUD"));

    debug_panel_ = new DebugPanel{right_tabs_};
    right_tabs_->addTab(debug_panel_, tr("\U0001F41B Debug"));
    connectDebugPanel();
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

    // Keys typed at the canvas are what `INKEY$` reads. Routed through the
    // runner rather than read here, because the program polls them from its own
    // thread; a window with no run in flight drops them, which is what a learner
    // pressing a key while nothing runs should get.
    connect(turtle_canvas_, &TurtleCanvas::keyTyped, this, [this](QChar key) {
        if (native_runner_ != nullptr && !key.isNull()) {
            native_runner_->push_key(key.toLatin1());
        }
    });
}

void MainWindow::buildFunctionKeyRow(QVBoxLayout* column) {
    function_key_bar_ = new FunctionKeyBar{this};
    // The bar's buttons land in the same dispatcher as the shortcuts built from
    // its table (`buildFunctionKeyShortcuts`) and the command palette. Before
    // this the two connections went to *signals* -- `functionKeyTriggered` and
    // `workspaceDeckSelected` are declared under `signals:`, so connecting them
    // re-emitted them to nobody and every button was inert.
    connect(function_key_bar_, &FunctionKeyBar::actionTriggered, this,
            &MainWindow::performAction);
    connect(function_key_bar_, &FunctionKeyBar::deckSelected, this,
            &MainWindow::selectDeck);
    // Emitted as well as acted on: a test (or a plugin) listening for the bar's
    // signal must still see it.
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
    // Rebuilt every time it is opened, from settings: a list built once would be
    // the files that were recent when the window was created.
    recent_menu_ = file->addMenu(tr("Open &Recent"));
    recent_menu_->setObjectName(QStringLiteral("recentMenu"));
    connect(recent_menu_, &QMenu::aboutToShow, this, [this] { rebuildRecentMenu(); });
    // A fresh install opens onto an empty window, and the programs that make it
    // worth having are on disk already: this is the shortest path from "I
    // installed it" to "something moved on the canvas".
    examples_menu_ = file->addMenu(tr("Open E&xample"));
    examples_menu_->setObjectName(QStringLiteral("examplesMenu"));
    connect(examples_menu_, &QMenu::aboutToShow, this, [this] { rebuildExamplesMenu(); });
    file->addSeparator();
    add(file, tr("&Save"), QKeySequence::Save, [this] { saveCurrentFile(); });
    add(file, tr("Save &As..."), QKeySequence::SaveAs, [this] { saveCurrentFileAs(); });
    file->addSeparator();
    add(file, tr("&Export Drawing..."), QKeySequence{Qt::CTRL | Qt::Key_E},
        [this] { exportDrawingDialog(); });
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
    edit->addSeparator();
    // The drawing is not text, so it has its own copy rather than borrowing
    // Ctrl+C from the editor -- which is why it is Ctrl+Shift+C.
    add(edit, tr("&Copy Drawing"), QKeySequence{Qt::CTRL | Qt::SHIFT | Qt::Key_C},
        [this] { copyDrawingToClipboard(); });
    edit->addSeparator();
    add(edit, tr("&Find..."), QKeySequence::Find, [this] { showFindBar(false); });
    add(edit, tr("Find and &Replace..."), QKeySequence::Replace,
        [this] { showFindBar(true); });
    // F3 rather than Ctrl+F, so finding again works with the bar closed: Ctrl+F
    // would only re-open and re-focus the field.
    add(edit, tr("Find &Next"), QKeySequence{Qt::Key_F3},
        [this] { findNextMatch(false); });
    add(edit, tr("Find Pre&vious"), QKeySequence{Qt::SHIFT | Qt::Key_F3},
        [this] { findNextMatch(true); });
    edit->addSeparator();
    // Ctrl+L, not Ctrl+G: the grid on the canvas already owns Ctrl+G, and two
    // actions on one key is an ambiguous shortcut that Qt resolves by guessing.
    add(edit, tr("&Go to Line..."), QKeySequence{Qt::CTRL | Qt::Key_L},
        [this] { goToLinePrompt(); });
    edit->addSeparator();
    add(edit, tr("&Preferences..."), QKeySequence{Qt::CTRL | Qt::Key_Comma},
        [this] { showPreferences(); })
        ->setObjectName(QStringLiteral("preferencesMenuAction"));

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

    // The CRT overlay has existed since the port and had no way to be switched
    // on: it was constructed, hidden, and never mentioned again. A checkable
    // entry is the whole feature, because the overlay itself already paints.
    crt_action_ = view->addAction(tr("CRT &Scanlines"));
    crt_action_->setObjectName(QStringLiteral("crtAction"));
    crt_action_->setCheckable(true);
    crt_action_->setChecked(crt_overlay_ != nullptr && crt_overlay_->isVisible());
    connect(crt_action_, &QAction::toggled, this, [this](bool on) {
        if (crt_overlay_ != nullptr) {
            crt_overlay_->setVisible(on);
        }
    });

    view->addSeparator();
    // F10 is on the function-key bar; Ctrl+Shift+P is the convention everybody
    // already has in their fingers.
    add(view, tr("Command &Palette..."),
        QKeySequence{Qt::CTRL | Qt::SHIFT | Qt::Key_P},
        [this] { showCommandPalette(); })
        ->setObjectName(QStringLiteral("commandPaletteAction"));
    buildThemeMenu(view);

    // -- Help -------------------------------------------------------------
    QMenu* help = bar->addMenu(tr("&Help"));
    help->setObjectName(QStringLiteral("helpMenu"));
    add(help, tr("&Keyboard Shortcuts"), QKeySequence{Qt::Key_F1},
        [this] { showShortcutHelp(); })
        ->setObjectName(QStringLiteral("shortcutsAction"));
    add(help, tr("&About Time Warp Studio"), {}, [this] {
        QMessageBox::about(
            this, tr("About Time Warp Studio"),
            tr("<b>Time Warp Studio</b> %1"
               "<p>An educational IDE for ten languages: BASIC, APL, Logo, PILOT, "
               "FOCAL, Scheme, CHIP-8, COMAL, REXX and Smalltalk, with an "
               "integrated turtle graphics canvas."
               "<p>Built with C++20 and Qt6. Every language runs natively; the "
               "bundled Python engine in <code>share/time_warp</code> is the "
               "reference the lesson check uses.")
                .arg(timewarp::ui::version()));
    });
    add(help, tr("About &Qt"), {}, [this] { QMessageBox::aboutQt(this); });
}

// ---------------------------------------------------------------------------
// Appearance
// ---------------------------------------------------------------------------

void MainWindow::buildThemeMenu(QMenu* view) {
    theme_menu_ = view->addMenu(tr("&Theme"));
    theme_menu_->setObjectName(QStringLiteral("themeMenu"));

    auto* group = new QActionGroup{theme_menu_};
    group->setExclusive(true);
    for (const Theme& theme : themes::all()) {
        const QString name = QString::fromUtf8(theme.name);
        QAction* action = theme_menu_->addAction(name);
        action->setCheckable(true);
        // The name travels as data rather than being parsed back out of the text:
        // a translation would break a lookup keyed on the visible string.
        action->setData(name);
        action->setChecked(name == theme_name_);
        group->addAction(action);
        connect(action, &QAction::triggered, this, [this, name] { applyTheme(name); });
    }
    theme_menu_->addSeparator();
    QAction* next = theme_menu_->addAction(tr("&Next Theme"));
    next->setObjectName(QStringLiteral("nextThemeAction"));
    next->setShortcut(QKeySequence{Qt::SHIFT | Qt::Key_F10});
    connect(next, &QAction::triggered, this, [this] { cycleTheme(); });
}

void MainWindow::applyTheme(const QString& name) {
    const Theme& theme = themes::byName(name);
    theme_name_ = QString::fromUtf8(theme.name);

    // The application palette, not this window's: dialogs and message boxes are
    // separate top-level widgets and would otherwise stay on the default system
    // colours, which reads as a theme that half applied.
    if (auto* app = qobject_cast<QApplication*>(QCoreApplication::instance())) {
        app->setPalette(themes::paletteFor(theme));
    }
    // The base sheet reads `palette(...)`, so it follows the palette above; the
    // theme's own block adds what a role cannot express.
    setStyleSheet(style::window() + themes::stylesheetFor(theme));

    if (theme_menu_ != nullptr) {
        for (QAction* action : theme_menu_->actions()) {
            if (action->isCheckable()) {
                QSignalBlocker blocker{action};
                action->setChecked(action->data().toString() == theme_name_);
            }
        }
    }

    QSettings{}.setValue(kSettingTheme, theme_name_);
    applyThemeToWidgets();
}

void MainWindow::cycleTheme() {
    applyTheme(QString::fromUtf8(themes::next(theme_name_).name));
}

void MainWindow::applyThemeToWidgets() {
    const Theme& theme = themes::byName(theme_name_);

    // Syntax colours. The highlighter's five roles against the theme's six: the
    // theme's `function` is the closest thing it has to a "directive" (PILOT's
    // unit letters, Scheme's labels), which is what that slot means here.
    SyntaxColors colours;
    colours.keyword = themes::colour(theme.keyword);
    colours.comment = themes::colour(theme.comment);
    colours.string = themes::colour(theme.string);
    colours.number = themes::colour(theme.number);
    colours.directive = themes::colour(theme.function);

    const auto recolour = [&colours](QTabWidget* tabs) {
        if (tabs == nullptr) {
            return;
        }
        for (int index = 0; index < tabs->count(); ++index) {
            auto* editor = qobject_cast<CodeEditor*>(tabs->widget(index));
            if (editor != nullptr && editor->highlighter() != nullptr) {
                editor->highlighter()->setColors(colours);
            }
        }
    };
    recolour(editor_tabs_);
    recolour(editor_tabs_secondary_);

    // The canvas surface, and the surface the next run starts on. A program that
    // names its own background still wins for that run; this is the default.
    const QColor surface = themes::colour(theme.canvas_bg);
    if (turtle_canvas_ != nullptr) {
        turtle_canvas_->setSurfaceColor(surface);
    }

    // The two verdict colours, which the theme carries for exactly this reason:
    // the panel's shipped green and red are unreadable on the phosphor themes.
    if (lesson_panel_ != nullptr) {
        lesson_panel_->setVerdictColors(themes::colour(theme.success_color),
                                        themes::colour(theme.error_color));
    }

    // The problem markers. A red squiggle on the red-phosphor or amber themes is
    // exactly the colour of everything else, so the marker colours come from the
    // theme too -- the error colour it carries for output, and its warning colour.
    const auto recolour_markers = [&theme](QTabWidget* tabs) {
        if (tabs == nullptr) {
            return;
        }
        for (int index = 0; index < tabs->count(); ++index) {
            if (auto* editor = qobject_cast<CodeEditor*>(tabs->widget(index));
                editor != nullptr) {
                editor->setDiagnosticColors(themes::colour(theme.error_color),
                                            themes::colour(theme.warning_color));
            }
        }
    };
    recolour_markers(editor_tabs_);
    recolour_markers(editor_tabs_secondary_);
}

// ---------------------------------------------------------------------------
// Editor settings
// ---------------------------------------------------------------------------

void MainWindow::loadEditorSettings() {
    QSettings settings;
    EditorSettings loaded;
    loaded.font_family = settings.value(kSettingEditorFont, QString{}).toString();
    loaded.font_point_size = settings.value(kSettingEditorFontSize, 11).toInt();
    loaded.tab_width = settings.value(kSettingEditorTabWidth, 4).toInt();
    loaded.show_line_numbers = settings.value(kSettingEditorLineNumbers, true).toBool();
    loaded.highlight_current_line =
        settings.value(kSettingEditorCurrentLine, true).toBool();
    loaded.word_wrap = settings.value(kSettingEditorWordWrap, false).toBool();
    loaded.auto_indent = settings.value(kSettingEditorAutoIndent, true).toBool();
    loaded.completion = settings.value(kSettingEditorCompletion, false).toBool();
    loaded.lint = settings.value(kSettingEditorLint, true).toBool();
    editor_settings_ = loaded;
}

void MainWindow::saveEditorSettings() const {
    QSettings settings;
    settings.setValue(kSettingEditorFont, editor_settings_.font_family);
    settings.setValue(kSettingEditorFontSize, editor_settings_.font_point_size);
    settings.setValue(kSettingEditorTabWidth, editor_settings_.tab_width);
    settings.setValue(kSettingEditorLineNumbers, editor_settings_.show_line_numbers);
    settings.setValue(kSettingEditorCurrentLine, editor_settings_.highlight_current_line);
    settings.setValue(kSettingEditorWordWrap, editor_settings_.word_wrap);
    settings.setValue(kSettingEditorAutoIndent, editor_settings_.auto_indent);
    settings.setValue(kSettingEditorCompletion, editor_settings_.completion);
    settings.setValue(kSettingEditorLint, editor_settings_.lint);
}

void MainWindow::applyEditorSettings(const EditorSettings& settings) {
    editor_settings_ = settings;
    const Theme& theme = themes::byName(theme_name_);
    const auto apply_to = [this, &theme](QTabWidget* tabs) {
        if (tabs == nullptr) {
            return;
        }
        for (int index = 0; index < tabs->count(); ++index) {
            if (auto* editor = qobject_cast<CodeEditor*>(tabs->widget(index));
                editor != nullptr) {
                editor->setDiagnosticColors(themes::colour(theme.error_color),
                                            themes::colour(theme.warning_color));
                editor->applySettings(editor_settings_);
            }
        }
    };
    apply_to(editor_tabs_);
    apply_to(editor_tabs_secondary_);
}

void MainWindow::showPreferences() {
    // A stack copy: the dialog edits its own record, and Cancel simply means the
    // window never hears about it.
    auto* dialog = new PreferencesDialog{editor_settings_, this};
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    connect(dialog, &PreferencesDialog::applied, this,
            [this](const EditorSettings& settings) { applyEditorSettings(settings); });
    connect(dialog, &PreferencesDialog::accepted, this,
            [this](const EditorSettings& settings) {
                applyEditorSettings(settings);
                saveEditorSettings();
                setExecutionState(tr("ready"), tr("Editor settings saved"));
            });
    dialog->open();
}

// ---------------------------------------------------------------------------
// Actions: one dispatcher behind the key bar, the keys, the palette and the menu
// ---------------------------------------------------------------------------

void MainWindow::performAction(const QString& action) {
    const QString id = action.trimmed().toLower();

    // `showPanel` is [[nodiscard]] because its caller may be holding a name it
    // read from data and has to see "no such panel". Every name below is a
    // literal naming a panel that exists, so the answer is known and is
    // discarded on purpose.
    const auto raisePanel = [this](const char* name) {
        static_cast<void>(showPanel(QString::fromLatin1(name)));
    };

    // -- run ---------------------------------------------------------------
    if (id == QLatin1String("run")) {
        runCurrentDocument();
        return;
    }
    if (id == QLatin1String("stop")) {
        stopRunningDocument();
        return;
    }
    if (id == QLatin1String("debug")) {
        // There is no separate "start debugging" in this build: a Scheme run
        // pauses where its breakpoints are, so arming them and running *is*
        // debugging. The panel is raised so the pause is visible when it happens.
        raisePanel("debug");
        runCurrentDocument();
        return;
    }
    if (id == QLatin1String("pause")) {
        if (debug_panel_ != nullptr && debug_panel_->isAvailable()) {
            debug_panel_->requestPause();
        } else {
            setExecutionState(tr("ready"),
                              tr("Pause needs a language with a debugger \u2014 "
                                 "Scheme only for now"));
        }
        return;
    }
    if (id == QLatin1String("step_into") || id == QLatin1String("step_over") ||
        id == QLatin1String("step_out")) {
        if (debug_panel_ == nullptr || !debug_panel_->isAvailable()) {
            setExecutionState(tr("ready"),
                              tr("Stepping needs a paused Scheme program"));
            return;
        }
        if (id == QLatin1String("step_into")) {
            debug_panel_->requestStepInto();
        } else if (id == QLatin1String("step_over")) {
            debug_panel_->requestStepOver();
        } else {
            debug_panel_->requestStepOut();
        }
        return;
    }
    if (id == QLatin1String("breakpoint")) {
        CodeEditor* editor = currentEditor();
        if (editor != nullptr && debug_panel_ != nullptr) {
            debug_panel_->toggleBreakpointAt(editor->currentLine());
            raisePanel("debug");
        }
        return;
    }

    // -- files -------------------------------------------------------------
    if (id == QLatin1String("save")) {
        saveCurrentFile();
        return;
    }
    if (id == QLatin1String("save_as")) {
        saveCurrentFileAs();
        return;
    }
    if (id == QLatin1String("open")) {
        openFileDialog();
        return;
    }
    if (id == QLatin1String("export")) {
        exportDrawingDialog();
        return;
    }

    // -- editing and view ---------------------------------------------------
    if (id == QLatin1String("find")) {
        findNextMatch(false);
        return;
    }
    if (id == QLatin1String("find_previous")) {
        findNextMatch(true);
        return;
    }
    if (id == QLatin1String("fit") || id == QLatin1String("zoom")) {
        if (turtle_canvas_ != nullptr) {
            turtle_canvas_->fitToContent();
        }
        return;
    }
    if (id == QLatin1String("outline")) {
        if (outline_dock_ != nullptr) {
            outline_dock_->setVisible(!outline_dock_->isVisible());
            // Raised as well as shown: a dock that appears behind the right-hand
            // tabs is a keystroke that looks like it did nothing.
            if (outline_dock_->isVisible()) {
                outline_dock_->raise();
            }
        }
        return;
    }
    if (id == QLatin1String("next_pane")) {
        QWidget* panes[] = {currentEditor(), console_view_, turtle_canvas_, right_tabs_};
        QWidget* focused = QApplication::focusWidget();
        int current = -1;
        for (int index = 0; index < 4; ++index) {
            if (panes[index] != nullptr &&
                (panes[index] == focused ||
                 (focused != nullptr && panes[index]->isAncestorOf(focused)))) {
                current = index;
                break;
            }
        }
        for (int step = 1; step <= 4; ++step) {
            QWidget* next_pane = panes[(current + step + 4) % 4];
            if (next_pane != nullptr) {
                next_pane->setFocus(Qt::ShortcutFocusReason);
                return;
            }
        }
        return;
    }
    if (id == QLatin1String("toggle_hud")) {
        raisePanel("telemetry");
        return;
    }
    if (id == QLatin1String("toggle_crt")) {
        if (crt_action_ != nullptr) {
            crt_action_->setChecked(!crt_action_->isChecked());
        }
        return;
    }
    if (id == QLatin1String("inspect")) {
        raisePanel("variables");
        return;
    }
    if (id == QLatin1String("lessons")) {
        raisePanel("lessons");
        return;
    }
    if (id == QLatin1String("filter_errors")) {
        raisePanel("output");
        if (output_sub_tabs_ != nullptr) {
            output_sub_tabs_->setCurrentWidget(errors_view_);
        }
        return;
    }
    if (id == QLatin1String("clear_output")) {
        clearOutput();
        return;
    }
    if (id == QLatin1String("clear_canvas")) {
        clearCanvas();
        return;
    }

    // -- the palette, the keys, the themes ---------------------------------
    if (id == QLatin1String("palette")) {
        showCommandPalette();
        return;
    }
    if (id == QLatin1String("help") || id == QLatin1String("shortcuts")) {
        showShortcutHelp();
        return;
    }
    if (id == QLatin1String("cycle_theme")) {
        cycleTheme();
        return;
    }

    // -- named, and not built ----------------------------------------------
    //
    // These keep their places on the bar because a learner reading the old
    // documentation should find the key where it says it is, but the key says so
    // out loud rather than doing nothing. A dead key is the fault this replaced.
    if (id == QLatin1String("split_editor")) {
        setExecutionState(tr("ready"),
                          tr("The split editor is not built yet \u2014 see "
                             "docs/ROADMAP.md"));
        return;
    }
    if (id == QLatin1String("find_in_files")) {
        setExecutionState(tr("ready"),
                          tr("Find in files is not built yet \u2014 use Find, or "
                             "see docs/ROADMAP.md"));
        return;
    }

    // Any other id is a bug in the table rather than something a user can
    // trigger, and it is reported so the next reader of the table sees it.
    setExecutionState(tr("ready"),
                      tr("No action is bound to \u201C%1\u201D").arg(action));
}

void MainWindow::selectDeck(const QString& deckId) {
    const QString id = deckId.trimmed().toLower();
    if (id == QLatin1String("code_focus")) {
        // The code, and only the code: focus, with nothing rearranged, because
        // the layout the user chose is theirs.
        if (CodeEditor* editor = currentEditor()) {
            editor->setFocus(Qt::ShortcutFocusReason);
        }
        setExecutionState(tr("ready"), tr("Deck: code"));
        return;
    }
    if (id == QLatin1String("graphics_studio")) {
        if (turtle_canvas_ != nullptr) {
            turtle_canvas_->setFocus(Qt::ShortcutFocusReason);
            turtle_canvas_->fitToContent();
        }
        setExecutionState(tr("ready"), tr("Deck: graphics"));
        return;
    }
    if (id == QLatin1String("debugger_deck")) {
        static_cast<void>(showPanel(QStringLiteral("debug")));
        setExecutionState(tr("ready"), tr("Deck: debugger"));
        return;
    }
    if (id == QLatin1String("split_lab")) {
        // Honest rather than silent: the second editor pane exists in the layout
        // (`editor_tabs_secondary_`) but nothing creates a tab in it yet.
        setExecutionState(tr("ready"),
                          tr("The split workspace is not built yet \u2014 see "
                             "docs/ROADMAP.md"));
        return;
    }
    setExecutionState(tr("ready"), tr("Unknown deck \u201C%1\u201D").arg(deckId));
}

void MainWindow::buildFunctionKeyShortcuts() {
    if (function_key_bar_ == nullptr) {
        return;
    }
    const auto alreadyClaimed = [this](const QKeySequence& sequence) {
        for (QAction* action : findChildren<QAction*>()) {
            const QList<QKeySequence> shortcuts = action->shortcuts();
            if (shortcuts.contains(sequence) ||
                action->shortcut() == sequence) {
                return true;
            }
        }
        return false;
    };
    for (const FunctionKeyBar::Layer layer : {FunctionKeyBar::Layer::Normal,
                                              FunctionKeyBar::Layer::Shift,
                                              FunctionKeyBar::Layer::Control}) {
        for (const FunctionKeyBar::Binding& binding :
             FunctionKeyBar::bindingsFor(layer)) {
            const QKeySequence sequence{binding.keys};
            if (sequence.isEmpty()) {
                continue;
            }
            // A key the menus already answer (F1 Help, F2 Save, F3 Find, F9 Run,
            // Shift+F9 Stop) is left alone: two actions on one sequence is an
            // ambiguous shortcut, and Qt resolves that by picking one and warning.
            if (alreadyClaimed(sequence)) {
                continue;
            }
            auto* action = new QAction{this};
            action->setObjectName(QStringLiteral("fkey_%1").arg(binding.action));
            action->setShortcut(sequence);
            action->setShortcutContext(Qt::WindowShortcut);
            const QString id = binding.action;
            connect(action, &QAction::triggered, this,
                    [this, id] { performAction(id); });
            addAction(action);
        }
    }
}

void MainWindow::showCommandPalette() {
    if (command_palette_ == nullptr) {
        command_palette_ = new CommandPalette{this};
    }
    // Populated per open: the menus change (Open Recent is rebuilt, actions are
    // enabled and disabled with the run state), and a palette built once would
    // offer yesterday's commands.
    command_palette_->populate(this);
    command_palette_->exec();
}

void MainWindow::showShortcutHelp() {
    // Built from the same table the bar draws, so the help cannot list a key the
    // bar does not have. Three columns rather than a paragraph: this is a
    // reference, and it is scanned rather than read.
    const auto rows = [](FunctionKeyBar::Layer layer) {
        QString text;
        for (const FunctionKeyBar::Binding& binding :
             FunctionKeyBar::bindingsFor(layer)) {
            text += QStringLiteral("<tr><td><tt>%1</tt></td><td>%2</td></tr>")
                        .arg(binding.keys.toHtmlEscaped(),
                             binding.caption.toHtmlEscaped());
        }
        return text;
    };
    const auto column = [&rows](FunctionKeyBar::Layer layer, const QString& title) {
        return QStringLiteral("<b>%1</b><table>%2</table>").arg(title, rows(layer));
    };

    auto* box = new QMessageBox{this};
    box->setObjectName(QStringLiteral("shortcutHelp"));
    box->setAttribute(Qt::WA_DeleteOnClose);
    box->setWindowTitle(tr("Keyboard Shortcuts"));
    box->setIcon(QMessageBox::Information);
    box->setTextFormat(Qt::RichText);
    box->setText(tr("<p><b>Time Warp Studio</b> keys. The bar along the bottom of "
                    "the window is this table's source, and its badge cycles the "
                    "three layers.</p>"
                    "<table><tr><td valign=\"top\">%1</td>"
                    "<td valign=\"top\">%2</td>"
                    "<td valign=\"top\">%3</td></tr></table>"
                    "<p>F10 or Ctrl+Shift+P opens the command palette, where every "
                    "menu entry can be found by typing part of its name.</p>")
                       .arg(column(FunctionKeyBar::Layer::Normal, tr("Normal")),
                            column(FunctionKeyBar::Layer::Shift, tr("Shift")),
                            column(FunctionKeyBar::Layer::Control, tr("Ctrl"))));
    box->open();
}

bool MainWindow::openStandaloneProgram(const QString& languageKey,
                                       const QString& source,
                                       const QString& title) {
    const Language language = languageFromKey(languageKey);
    if (language == Language::Unknown) {
        return false;
    }

    CodeEditor* editor = newFile();
    if (editor == nullptr) {
        return false;
    }
    editor->setPlainText(source);
    // The payload's language is the truth here: the artifact's file name is
    // whatever the author called it, and may have no extension at all.
    editor->setProperty(kLanguageOverrideProperty, settingsKey(language));
    editor->setReadOnly(true);
    refreshDocumentState();

    player_mode_ = true;
    // Everything that exists to *write* a program is hidden, because there is
    // nothing to author: this window is the program. What remains is the canvas,
    // the output console and the status bar.
    if (QMenuBar* bar = menuBar()) {
        bar->setVisible(false);
    }
    if (tool_bar_ != nullptr) {
        tool_bar_->setVisible(false);
    }
    if (tool_bar_secondary_ != nullptr) {
        tool_bar_secondary_->setVisible(false);
    }
    if (function_key_bar_ != nullptr) {
        function_key_bar_->setVisible(false);
    }
    if (outline_dock_ != nullptr) {
        outline_dock_->setVisible(false);
    }
    if (right_tabs_ != nullptr) {
        right_tabs_->setVisible(false);
    }
    if (find_bar_ != nullptr) {
        find_bar_->setVisible(false);
    }
    if (immediate_mode_ != nullptr) {
        immediate_mode_->setVisible(false);
    }
    // The editor column, rather than the editor alone: the tab strip and the
    // find bar live inside it, and a hidden editor with a visible column leaves a
    // blank panel where the code used to be.
    if (main_splitter_ != nullptr && main_splitter_->widget(0) != nullptr) {
        main_splitter_->widget(0)->setVisible(false);
    }

    setWindowTitle(title);
    setExecutionState(tr("ready"), tr("Running %1").arg(title));
    return true;
}

void MainWindow::showFindBar(bool withReplacement) {
    if (find_bar_ == nullptr) {
        return;
    }
    find_bar_->setEditor(currentEditor());
    find_bar_->open(withReplacement);
}

void MainWindow::findNextMatch(bool backwards) {
    if (find_bar_ == nullptr) {
        return;
    }
    if (find_bar_->term().isEmpty()) {
        // Nothing to search for yet. Opening the bar is more useful than a
        // silent no-op, and it is where the user was going anyway.
        showFindBar(false);
        return;
    }
    find_bar_->setEditor(currentEditor());
    find_bar_->findAgain(backwards);
}

void MainWindow::goToLinePrompt() {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        return;
    }
    bool accepted = false;
    const int line = QInputDialog::getInt(this, tr("Go to Line"), tr("Line number:"),
                                          editor->currentLine(), 1,
                                          editor->document()->blockCount(), 1, &accepted);
    if (accepted) {
        // `goToLine` clamps, so a number past the end is a move to the last line
        // rather than a refusal.
        editor->goToLine(line);
    }
}

void MainWindow::exportDrawingDialog() {
    if (turtle_canvas_ == nullptr || turtle_canvas_->strokeCount() == 0) {
        // Nothing drawn yet. Said rather than done silently: a save dialog that
        // produces an empty picture is worse than being told there is nothing.
        setExecutionState(tr("ready"),
                          tr("Nothing to export yet \u2014 run a program that draws"));
        return;
    }
    const QString path = QFileDialog::getSaveFileName(
        this, tr("Export drawing"), QString{},
        tr("PNG image (*.png);;JPEG image (*.jpg *.jpeg);;Windows bitmap (*.bmp)"));
    if (path.isEmpty()) {
        return;
    }
    QString error;
    if (!turtle_canvas_->exportDrawing(path, &error)) {
        QMessageBox::warning(this, tr("Export failed"),
                             tr("Could not write %1.\n\n%2")
                                 .arg(QDir::toNativeSeparators(path), error));
        return;
    }
    setExecutionState(tr("ready"), tr("Exported %1").arg(QFileInfo{path}.fileName()));
}

void MainWindow::copyDrawingToClipboard() {
    if (turtle_canvas_ == nullptr || turtle_canvas_->strokeCount() == 0) {
        setExecutionState(tr("ready"),
                          tr("Nothing to copy yet \u2014 run a program that draws"));
        return;
    }
    QGuiApplication::clipboard()->setImage(turtle_canvas_->renderedDrawing());
    setExecutionState(tr("ready"), tr("Copied the drawing"));
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
              tr("needs the toolbar's link to the debug panel, which is not ported yet"));
    notPorted(addRunAction(tr("\u25B6\uFE0F Continue"), tr("Continue to the next breakpoint")),
              tr("needs the toolbar's link to the debug panel, which is not ported yet"));
    notPorted(addRunAction(tr("\u2193 Trace"), tr("Step into the next statement")),
              tr("needs the toolbar's link to the debug panel, which is not ported yet"));
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
    // Where the turtle-speed slider used to be. The slider was removed rather
    // than finished: nothing animates the display list, so it controlled an
    // ability the application does not have, and a disabled control that never
    // becomes enabled is clutter in a row a learner reads as the feature list.
    QAction* preferences = tool_bar_secondary_->addAction(tr("\u2699\uFE0F Settings"));
    preferences->setObjectName(QStringLiteral("preferencesAction"));
    preferences->setToolTip(tr("Editor and application settings (Ctrl+,)"));
    connect(preferences, &QAction::triggered, this, [this] { showPreferences(); });
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
    saveSession();
    event->accept();
}

void MainWindow::saveSession() const {
    // Only documents with a path: an untitled buffer has no name to reopen, and a
    // tab restored as "untitled 3" would be a document the user never had.
    QSettings{}.setValue(kSettingOpenFiles, openFilePaths());
}

int MainWindow::restoreSession() {
    const QStringList files = QSettings{}.value(kSettingOpenFiles).toStringList();
    int reopened = 0;
    for (const QString& path : files) {
        // Silently: a file that has been moved or deleted since the last session
        // is a missing file, not an error the user has to dismiss at startup --
        // which is also what makes this safe to call before `show()`.
        if (openFile(path, OpenFailure::Silent)) {
            ++reopened;
        }
    }
    return reopened;
}

QStringList MainWindow::recentFiles() const {
    return QSettings{}.value(kSettingRecentFiles).toStringList();
}

void MainWindow::noteRecentFile(const QString& path) {
    if (path.isEmpty()) {
        return;
    }
    // Absolute, so the same file reached by two different relative paths is one
    // entry rather than two.
    const QString absolute = QFileInfo{path}.absoluteFilePath();
    QStringList recent = recentFiles();
    recent.removeAll(absolute);
    recent.prepend(absolute);
    while (recent.size() > kRecentFileLimit) {
        recent.removeLast();
    }
    QSettings{}.setValue(kSettingRecentFiles, recent);
}

void MainWindow::rebuildRecentMenu() {
    if (recent_menu_ == nullptr) {
        return;
    }
    recent_menu_->clear();
    const QStringList recent = recentFiles();
    if (recent.isEmpty()) {
        QAction* none = recent_menu_->addAction(tr("(nothing yet)"));
        none->setEnabled(false);
        return;
    }
    for (const QString& path : recent) {
        recent_menu_->addAction(QDir::toNativeSeparators(path), this,
                                [this, path] { openFile(path); });
    }
    recent_menu_->addSeparator();
    recent_menu_->addAction(tr("Clear Menu"), this, [this] { clearRecentFiles(); });
}

void MainWindow::clearRecentFiles() {
    // The session's document list is *not* touched: clearing the menu of paths a
    // user has visited is not the same as saying they were never working on what
    // is open now.
    QSettings{}.remove(kSettingRecentFiles);
}

namespace {

/// Where this build's example programs live, or empty when it has none.
///
/// Searched in the order `locateWorker()` uses, for the same reason: the
/// installed layout first, because that is what a package ships and where it puts
/// things, then the checkout, which is what a build tree has. The environment
/// variable wins over both, so a teacher can point the menu at a folder of their
/// own programs.
QString exampleRoot() {
    const QString from_environment = qEnvironmentVariable("TIME_WARP_EXAMPLES");
    if (!from_environment.isEmpty() && QFileInfo{from_environment}.isDir()) {
        return QFileInfo{from_environment}.absoluteFilePath();
    }
    const QString app_dir = QApplication::applicationDirPath();
    // `bin/time_warp_ide` -> `share/time-warp-studio/examples`, which is the
    // destination CMakeLists.txt installs them to.
    const QString installed =
        app_dir + QStringLiteral("/../share/time-warp-studio/examples");
    if (QFileInfo{installed}.isDir()) {
        return QFileInfo{installed}.absoluteFilePath();
    }
    // A build tree: the binary is `native/build/time_warp_ide` and the programs
    // are beside `native/`. The second path covers a binary in `native/build/bin`.
    for (const QString& upward :
         {QStringLiteral("/../Examples"), QStringLiteral("/../../Examples")}) {
        const QString candidate = app_dir + upward;
        if (QFileInfo{candidate}.isDir()) {
            return QFileInfo{candidate}.absoluteFilePath();
        }
    }
    // Last, the working directory: someone who unpacked the tarball and ran the
    // binary from inside it has the programs right there.
    if (QFileInfo{QStringLiteral("Examples")}.isDir()) {
        return QFileInfo{QStringLiteral("Examples")}.absoluteFilePath();
    }
    return {};
}

}  // namespace

void MainWindow::rebuildExamplesMenu() {
    if (examples_menu_ == nullptr) {
        return;
    }
    examples_menu_->clear();
    const QDir root{exampleRoot()};
    if (root.path().isEmpty() || !root.exists()) {
        // Not an error: a build made without the checkout has nothing to list,
        // and an empty menu with no explanation is the thing a user cannot
        // interpret.
        QAction* none = examples_menu_->addAction(tr("(no examples installed)"));
        none->setEnabled(false);
        return;
    }
    const QStringList folders =
        root.entryList(QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name);
    int listed = 0;
    for (const QString& folder : folders) {
        const QDir directory{root.filePath(folder)};
        const QStringList files = directory.entryList(QDir::Files, QDir::Name);
        if (files.isEmpty()) {
            continue;
        }
        // The folder name is the settings key ("chip8"), so the display name
        // ("CHIP-8") comes from the same table the rest of the UI uses rather
        // than from the directory.
        const Language language = languageFromKey(folder);
        QMenu* submenu = examples_menu_->addMenu(
            language == Language::Unknown ? folder : displayName(language));
        submenu->setObjectName(QStringLiteral("examplesMenu.") + folder);
        for (const QString& file : files) {
            const QString path = directory.filePath(file);
            submenu->addAction(file, this, [this, path] { openExample(path); });
            ++listed;
        }
    }
    if (listed == 0) {
        QAction* none = examples_menu_->addAction(tr("(no examples installed)"));
        none->setEnabled(false);
    }
}

void MainWindow::openExample(const QString& path) {
    // Through openFile, so an example behaves like any other document: it lands
    // in the recent list, its language comes from the extension, and a second
    // click raises the tab it already has.
    openFile(path);
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

    // A tab created after a theme was applied starts in that theme. Without
    // this, every new document after the first would be syntax-coloured for the
    // light theme while the window around it is dark.
    if (editor->highlighter() != nullptr) {
        const Theme& theme = themes::byName(theme_name_);
        SyntaxColors colours;
        colours.keyword = themes::colour(theme.keyword);
        colours.comment = themes::colour(theme.comment);
        colours.string = themes::colour(theme.string);
        colours.number = themes::colour(theme.number);
        colours.directive = themes::colour(theme.function);
        editor->highlighter()->setColors(colours);
        editor->setDiagnosticColors(themes::colour(theme.error_color),
                                    themes::colour(theme.warning_color));
    }
    // And the editor's own customisations, for the same reason: a tab opened
    // after Preferences was used has to look like the tabs that were already
    // open.
    editor->applySettings(editor_settings_);

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
    // The find bar's match count is a statement about the document, so it has to
    // be stale-proof: a count that only refreshed when the term changed would
    // still say "4 matches" after two of them were deleted.
    connect(editor, &QPlainTextEdit::textChanged, this, [this, editor] {
        if (find_bar_ != nullptr && editor == currentEditor()) {
            find_bar_->refresh();
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
        if (find_bar_ != nullptr) {
            find_bar_->setEditor(nullptr);
        }
        if (debug_panel_ != nullptr) {
            debug_panel_->setAvailable(false, tr("Open a document to debug it."));
        }
        return;
    }

    const QString path = pathOf(editor);
    setWindowTitle(editor->document()->isModified()
                       ? tr("%1 \u2014 %2*").arg(base, nameOf(editor))
                       : tr("%1 \u2014 %2").arg(base, nameOf(editor)));
    language_badge_->setText(
        tr("\U0001F522 %1").arg(displayName(languageOf(editor))));
    // The highlighter follows the document's language. Done here, in the one
    // place that already answers "what language is this document?", so a rename
    // that changes the language cannot recolour every tab but this one.
    editor->setLanguage(languageOf(editor));
    // Same reasoning for the find bar: it searches the document the user is
    // looking at, and switching tabs has to move it.
    if (find_bar_ != nullptr) {
        find_bar_->setEditor(editor);
    }
    if (debug_panel_ != nullptr) {
        // Stepping needs the pause protocol, which only the in-process Scheme
        // machine has. The panel says so instead of offering six dead buttons.
        debug_panel_->setAvailable(
            languageOf(editor) == Language::Scheme,
            tr("Stepping is Scheme only \u2014 every other language runs to "
               "completion with no pause protocol."));
        debug_panel_->setLineNumber(editor->currentLine());
    }
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
            // Raising a tab is using the file, so it belongs at the top of the
            // recent list too. Otherwise re-opening a file from the list would
            // leave the list in the order it already had.
            noteRecentFile(absolute);
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
    noteRecentFile(absolute);

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
    // A file that has just been saved under a name is a file the user will want
    // back, which is what the recent list is for.
    noteRecentFile(path);
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

MainWindow::~MainWindow() {
    // Before any member is destroyed: a runner thread holds a reference to the
    // canvas queue and to its own host, so letting Qt tear those down first is a
    // write into freed memory from a thread that is still drawing.
    stopEngines();
}

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

    // A panel showing the previous run's numbers while this one is still starting
    // is a lie that is easy to read as this run's. Emptying them is the honest
    // state, and the HUD has a row that says so.
    clearRunState();
    startRunClock();

    // A ported language runs in this process. The bridge is only for the ones
    // that have not been ported, and `executor_for` is the question that decides
    // it -- so adding an executor is all it takes for the IDE to use it.
    if (languageOf(editor) != Language::Scheme) {
        const std::optional<timewarp::lang::Language> native = nativeLanguage(languageOf(editor));
        if (native.has_value() && timewarp::lang::executor_for(*native) != nullptr) {
            runNatively(editor, *native);
        } else {
            runInBridge(editor);
        }
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

    // The panel's buttons were already hooked up to "whatever runner exists";
    // these two are the other direction -- the runner telling the panel it has
    // stopped somewhere.
    connect(runner, &timewarp::debugger::DebuggerRunner::paused, this,
            [this, runner](int line, int, const QString& procedure) {
                if (runner_ != runner) {
                    return;
                }
                if (debug_panel_ != nullptr) {
                    debug_panel_->setPaused(true, line, procedure);
                }
                showRunnerSnapshot();
            });
    connect(runner, &timewarp::debugger::DebuggerRunner::resumed, this,
            [this, runner] {
                if (runner_ != runner) {
                    return;
                }
                if (debug_panel_ != nullptr) {
                    debug_panel_->setPaused(false, 0, QString{});
                }
            });

    // The breakpoints already in the panel are the ones this run honours, which
    // is why the set is applied here rather than only when it changes.
    if (debug_panel_ != nullptr) {
        runner->set_breakpoints(debug_panel_->breakpoints());
    }

    setExecutionState(tr("running"), tr("Running %1…").arg(nameOf(editor)));
    runner->load_program(editor->toPlainText(), nameOf(editor));
    runner->start();
}

void MainWindow::stopEngines() {
    if (native_runner_ != nullptr) {
        // Cooperative, then wait: an executor checks the flag between statements.
        native_runner_->request_stop();
        native_runner_->wait();
        delete native_runner_.data();
        native_runner_ = nullptr;
    }
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
    const bool native_running = native_runner_ != nullptr && native_runner_->isRunning();
    const bool bridge_running =
        python_worker_ != nullptr && python_worker_->isRunningProgram();
    if (!scheme_running && !native_running && !bridge_running) {
        setExecutionState(tr("ready"), tr("Nothing is running"));
        return;
    }
    setExecutionState(tr("stopping"), tr("Stopping…"));
    stopEngines();
    setExecutionState(tr("stopped"), tr("Stopped"));
}

void MainWindow::runNatively(CodeEditor* editor, timewarp::lang::Language language) {
    // The same clean surface the other engines get, in the same order.
    canvas_bridge_->drainNow();
    turtle_canvas_->reset();
    if (console_view_ != nullptr) {
        console_view_->clear();
    }
    if (errors_view_ != nullptr) {
        errors_view_->clear();
    }

    auto* runner = new NativeRunner{language, *canvas_queue_, this};
    native_runner_ = runner;
    // The queue has no consumer of its own: a run that fills it waits until the
    // GUI drains it, so every command has to say "look at the queue now".
    runner->set_wakeup([this] { canvas_bridge_->wake(); });
    // Sound: created once, on the first run that might make one. A machine with
    // no player still runs the program -- the language's acknowledgement is the
    // reference's, and `audio_available()` is what tells a program the truth.
    if (sound_ == nullptr) {
        sound_ = new SoundPlayer{this};
    }
    runner->set_sound_player(sound_);
    // The theme's surface, so a program that draws before naming a colour draws
    // on the paper the user's theme chose rather than on the reference's.
    if (const QColor surface =
            turtle_canvas_ != nullptr ? turtle_canvas_->surfaceColor() : QColor{};
        surface.isValid()) {
        runner->set_surface_color(
            timewarp::lang::Rgb{static_cast<std::uint8_t>(surface.red()),
                                static_cast<std::uint8_t>(surface.green()),
                                static_cast<std::uint8_t>(surface.blue())});
    }
    runner->load_program(editor->toPlainText());

    connect(runner, &NativeRunner::outputAvailable, this,
            [this, runner](const QString& text) {
                if (native_runner_ != runner) {
                    return;
                }
                // The runner reports lines without terminators, like the engine's
                // own buffer; the console needs the newline the logger added.
                appendOutput(text + QLatin1Char('\n'));
            });
    connect(runner, &NativeRunner::inputRequested, this,
            [this, runner](const QString& prompt) {
                if (native_runner_ != runner) {
                    return;
                }
                // A program that asked a question and stopped is waiting for an
                // answer the IDE cannot invent.
                bool accepted = false;
                const QString answer = QInputDialog::getText(
                    this, tr("The program is waiting for input"), prompt, QLineEdit::Normal,
                    QString{}, &accepted);
                runner->answerInput(accepted ? answer : QString{});
            });
    connect(runner, &NativeRunner::run_finished, this,
            [this, runner, language](bool completed, const QString& error) {
                if (native_runner_ != runner) {
                    return;
                }
                stopRunClock();
                canvas_bridge_->drainNow();
                turtle_canvas_->fitToContent();

                QList<timewarp::bridge::VariableRow> rows;
                for (const NativeRunner::VariableRow& row : runner->variables()) {
                    rows.append({row.name, row.type, row.value});
                }
                if (variables_panel_ != nullptr) {
                    variables_panel_->setRows(rows);
                }
                if (telemetry_panel_ != nullptr) {
                    const std::string_view name = timewarp::lang::display_name(language);
                    QList<timewarp::bridge::TelemetryRow> hud;
                    hud.append({tr("Language"),
                                QString::fromLatin1(name.data(), static_cast<int>(name.size()))});
                    hud.append({tr("Outcome"), completed ? tr("finished") : tr("stopped")});
                    hud.append({tr("Steps"), QString::number(runner->steps())});
                    hud.append({tr("Engine time"),
                                tr("%1 ms").arg(runner->elapsedMs(), 0, 'f', 1)});
                    hud.append({tr("Drawing commands"),
                                QString::number(runner->drawingCommands())});
                    telemetry_panel_->setRows(hud + ideTelemetryRows());
                }
                if (!error.isEmpty()) {
                    reportError(error);
                }
                setExecutionState(completed ? tr("ready") : tr("stopped"),
                                  completed ? tr("Finished") : tr("Stopped"));
            });

    setExecutionState(tr("running"), tr("Running %1…").arg(nameOf(editor)));
    runner->start();
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

namespace {

/// The right column's fixed pages: the key `showPanel` accepts and the index of
/// the tab `buildRightColumn` adds. One table, because the failure it prevents is
/// a page that exists and cannot be raised -- from the palette, a function key or
/// a menu -- and that looks exactly like a working page until somebody tries.
struct PanelEntry {
    const char* key;
    int index;
};

constexpr PanelEntry kPanelTable[] = {
    {"output", 0}, {"variables", 1}, {"telemetry", 2}, {"debug", 3}, {"lessons", 4}};

}  // namespace

QStringList MainWindow::panelKeys() {
    QStringList keys;
    for (const PanelEntry& entry : kPanelTable) {
        keys << QString::fromLatin1(entry.key);
    }
    return keys;
}

bool MainWindow::showPanel(const QString& name) {
    const QString wanted = name.trimmed().toLower();
    for (const PanelEntry& entry : kPanelTable) {
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
                stopRunClock();
                canvas_bridge_->drainNow();
                turtle_canvas_->fitToContent();
                if (telemetry_panel_ != nullptr) {
                    telemetry_panel_->setRows(engine_telemetry_ + ideTelemetryRows());
                }
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
    // The run's own state, which arrives just before `finished`.
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::variablesReported,
            this,
            [this](const QList<timewarp::bridge::VariableRow>& rows) {
                if (variables_panel_ != nullptr) {
                    variables_panel_->setRows(rows);
                }
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::telemetryReported,
            this,
            [this](const QList<timewarp::bridge::TelemetryRow>& rows) {
                // Held, not shown: the wall clock is not known until `finished`,
                // and both belong to the same table.
                engine_telemetry_ = rows;
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

void MainWindow::connectDebugPanel() {
    if (debug_panel_ == nullptr) {
        return;
    }
    // These do not connect *to* a runner: one exists only for the length of a
    // run, and is deleted when the run ends. They look it up when a button is
    // pressed, so a press between runs is a no-op rather than a dangling call.
    const auto forward = [this](void (timewarp::debugger::DebuggerRunner::*command)()) {
        return [this, command] {
            if (runner_ != nullptr) {
                (runner_.data()->*command)();
            }
        };
    };
    connect(debug_panel_, &DebugPanel::continueRequested, this,
            forward(&timewarp::debugger::DebuggerRunner::request_continue));
    connect(debug_panel_, &DebugPanel::pauseRequested, this,
            forward(&timewarp::debugger::DebuggerRunner::request_pause));
    connect(debug_panel_, &DebugPanel::stepIntoRequested, this,
            forward(&timewarp::debugger::DebuggerRunner::request_step_into));
    connect(debug_panel_, &DebugPanel::stepOverRequested, this,
            forward(&timewarp::debugger::DebuggerRunner::request_step_over));
    connect(debug_panel_, &DebugPanel::stepOutRequested, this,
            forward(&timewarp::debugger::DebuggerRunner::request_step_out));
    connect(debug_panel_, &DebugPanel::stopRequested, this,
            forward(&timewarp::debugger::DebuggerRunner::request_stop));
    connect(debug_panel_, &DebugPanel::breakpointsChanged, this,
            [this](const QList<int>& lines) {
                if (runner_ != nullptr) {
                    runner_->set_breakpoints(lines);
                }
            });
}

QList<timewarp::bridge::TelemetryRow> MainWindow::ideTelemetryRows() const {
    QList<timewarp::bridge::TelemetryRow> rows;
    if (last_run_ms_ >= 0) {
        rows.append({tr("Wall clock"), tr("%1 ms").arg(last_run_ms_)});
    }
    if (turtle_canvas_ != nullptr) {
        rows.append(
            {tr("Canvas ink"), tr("%1 segments, %2 shapes")
                                   .arg(turtle_canvas_->strokeCount())
                                   .arg(turtle_canvas_->shapeCount())});
        rows.append({tr("Ink length"),
                     tr("%1 units").arg(turtle_canvas_->totalStrokeLength(), 0, 'f', 1)});
    }
    return rows;
}

void MainWindow::showRunnerSnapshot() {
    if (runner_ == nullptr) {
        return;
    }
    const timewarp::debugger::DebugSnapshot snapshot = runner_->snapshot();
    // One vocabulary for both sources: the Scheme machine says "string" where the
    // bridge's inspector says "text", and a column whose word changed with the
    // language would read as two different types.
    const auto kind_word = [](const QString& kind) {
        return kind == QLatin1String("string") ? QStringLiteral("text") : kind;
    };
    QList<timewarp::bridge::VariableRow> rows;
    rows.reserve(snapshot.variables.size());
    for (const timewarp::debugger::DebugVariable& entry : snapshot.variables) {
        rows.append({entry.name, kind_word(entry.kind), entry.rendered});
    }
    if (variables_panel_ != nullptr) {
        variables_panel_->setRows(rows);
    }
    if (debug_panel_ != nullptr) {
        QList<DebugPanel::Frame> frames;
        frames.reserve(snapshot.stack.size());
        for (const timewarp::debugger::StackFrameInfo& frame : snapshot.stack) {
            frames.append({frame.procedure, frame.line});
        }
        debug_panel_->setStack(frames);
        debug_panel_->setVariables(rows);
    }
}

void MainWindow::clearRunState() {
    engine_telemetry_.clear();
    if (variables_panel_ != nullptr) {
        variables_panel_->setRows({});
    }
    if (telemetry_panel_ != nullptr) {
        telemetry_panel_->setRows({});
    }
    if (debug_panel_ != nullptr) {
        debug_panel_->setPaused(false, 0, QString{});
        debug_panel_->setStack({});
        debug_panel_->setVariables({});
    }
}

void MainWindow::startRunClock() {
    last_run_ms_ = -1;
    run_clock_.start();
}

void MainWindow::stopRunClock() {
    if (!run_clock_.isValid()) {
        return;
    }
    last_run_ms_ = run_clock_.elapsed();
    run_clock_.invalidate();
}

void MainWindow::finishRun(const QString& error, bool completed) {
    stopRunClock();
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

    // A Scheme run reports nothing over the bridge, so its two tables come from
    // the runner and the IDE. Publishing them here rather than leaving the HUD
    // showing the last bridged run is the difference between a stale number and
    // an absent one.
    if (debug_panel_ != nullptr) {
        debug_panel_->setPaused(false, 0, QString{});
    }
    showRunnerSnapshot();
    if (telemetry_panel_ != nullptr) {
        QList<timewarp::bridge::TelemetryRow> rows;
        rows.append({tr("Language"), tr("Scheme")});
        rows.append({tr("Outcome"),
                     error.isEmpty() ? (completed ? tr("finished") : tr("stopped"))
                                     : tr("stopped with an error")});
        telemetry_panel_->setRows(rows + ideTelemetryRows());
    }

    if (!error.isEmpty()) {
        reportError(error);
        return;
    }
    setExecutionState(completed ? tr("ready") : tr("stopped"),
                      completed ? tr("Finished") : tr("Stopped"));
}

}  // namespace timewarp::ui
