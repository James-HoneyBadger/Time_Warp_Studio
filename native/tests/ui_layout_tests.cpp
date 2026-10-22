// SPDX-License-Identifier: MIT
//
// Structural tests for the ported main-window layout.
//
// A layout translation is easy to get "almost right" and impossible to notice:
// swap two children of a splitter, drop a `setStretchFactor`, forget
// `setChildrenCollapsible(false)`, and the window still looks correct until
// somebody resizes it and a pane they cannot recover. These tests assert the
// structure and the *sizing behaviour* rather than the pixels, so a regression
// fails here instead of in front of a user.
//
// Runs offscreen, so it needs no display and belongs in CI alongside the rest.

#include <atomic>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <memory>
#include <string>
#include <vector>

#include <QAction>
#include <QElapsedTimer>
#include <QThread>
#include <QTimer>
#include <QApplication>
#include <QCheckBox>
#include <QComboBox>
#include <QDir>
#include <QDockWidget>
#include <QFile>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QProgressBar>
#include <QSet>
#include <QTextBrowser>
#include <QKeySequence>
#include <QMargins>
#include <QMenu>
#include <QMenuBar>
#include <QMetaMethod>
#include <QPainter>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QScrollBar>
#include <QSettings>
#include <QShortcut>
#include <QSlider>
#include <QSpinBox>
#include <QSplitter>
#include <QStatusBar>
#include <QTabBar>
#include <QTabWidget>
#include <QTableWidget>
#include <QTextCursor>
#include <QTemporaryDir>
#include <QToolBar>
#include <QToolButton>
#include <QTreeWidget>
#include <QVBoxLayout>
#include <QWidgetAction>

#include "timewarp/core/qt_gui_compat.hpp"
#include "check.hpp"
#include "temp_dir.hpp"
#include "timewarp/canvas/command.hpp"
#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/ui/autosave.hpp"
#include "timewarp/ui/chip8_window.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/command_palette.hpp"
#include "timewarp/ui/find_bar.hpp"
#include "timewarp/ui/main_window.hpp"
#include "timewarp/ui/native_runner.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/preferences.hpp"
#include "timewarp/project/project.hpp"
#include "timewarp/sql/database.hpp"
#include "timewarp/ui/profile_report.hpp"
#include "timewarp/ui/project_panel.hpp"
#include "timewarp/ui/sql_panel.hpp"
#include "timewarp/ui/scrubber.hpp"
#include "timewarp/ui/sound_player.hpp"

#include "timewarp/ui/snippet_dialog.hpp"
#include "timewarp/ui/snippets.hpp"
#include "timewarp/ui/standalone.hpp"
#include "timewarp/ui/terminal_widget.hpp"
#include "timewarp/ui/themes.hpp"
#include "timewarp/ui/tour.hpp"
#include "timewarp/ui/turtle_canvas.hpp"
#include "timewarp/ui/version_history.hpp"
#include "timewarp/ui/resizable_tab_widget.hpp"


/// Pumps the event loop until `done()`, so a signal from a runner thread has
/// somewhere to arrive.
template <typename Predicate>
bool spin_until(Predicate done, int timeout_ms = 10000) {
    QElapsedTimer timer;
    timer.start();
    while (!done()) {
        QApplication::processEvents();
        if (timer.elapsed() > timeout_ms) {
            return false;
        }
        QThread::msleep(2);
    }
    return true;
}

namespace {

using timewarp::ui::CrtOverlay;
using timewarp::ui::CodeEditor;
using timewarp::ui::FunctionKeyBar;
using timewarp::ui::ImmediateModePanel;
using timewarp::ui::DebugPanel;
using timewarp::ui::LessonPanel;
using timewarp::ui::ProjectPanel;
using timewarp::ui::SqlPanel;
using timewarp::ui::Language;
using timewarp::ui::MainWindow;
using timewarp::ui::ResizableTabWidget;
using timewarp::ui::TelemetryPanel;
using timewarp::ui::VariablesPanel;

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

/// A page that asks for a lot of room, to prove size hints follow it.
class BulkyPage final : public QWidget {
public:
    explicit BulkyPage(QWidget* parent = nullptr) : QWidget(parent) {}
    [[nodiscard]] QSize sizeHint() const override { return {1400, 900}; }
    [[nodiscard]] QSize minimumSizeHint() const override { return {1400, 900}; }
};

/// A page with a valid but small hint, which still has to clear the floor.
class SmallPage final : public QWidget {
public:
    explicit SmallPage(QWidget* parent = nullptr) : QWidget(parent) {}
    [[nodiscard]] QSize sizeHint() const override { return {60, 40}; }
    [[nodiscard]] QSize minimumSizeHint() const override { return {60, 40}; }
};

[[nodiscard]] QString tabTitles(const QTabWidget* tabs) {
    QStringList titles;
    for (int index = 0; index < tabs->count(); ++index) {
        titles << tabs->tabText(index);
    }
    return titles.join(QLatin1Char('|'));
}

/// `QSplitter` has no stretch-factor getter -- only `setStretchFactor` -- and Qt
/// implements that by writing the child's size policy. Reading the policy back is
/// therefore the only way to assert a stretch factor directly; the independent
/// check is the resize-ratio test at the end of this file. If a future Qt stops
/// keeping the two in step, these will disagree, which is the signal to look.
[[nodiscard]] int horizontalStretch(const QSplitter* splitter, int index) {
    return splitter->widget(index)->sizePolicy().horizontalStretch();
}

[[nodiscard]] int verticalStretch(const QSplitter* splitter, int index) {
    return splitter->widget(index)->sizePolicy().verticalStretch();
}

/// A *shown* window.
///
/// Splitter geometry only means anything after Qt has laid the window out: an
/// unshown QSplitter still holds degenerate sizes, and the `setSizes()` in the
/// constructor is only a hint until the first layout pass. Asserting 720/480 on a
/// window that has never been shown tests nothing but Qt's placeholder values.
[[nodiscard]] std::unique_ptr<MainWindow> shownWindow(int width = 1200,
                                                      int height = 800) {
    auto window = std::make_unique<MainWindow>();
    window->resize(width, height);
    window->show();
    QApplication::processEvents();
    QApplication::processEvents();
    return window;
}

/// The share of the splitter's extent taken by one child, so the assertion holds
/// at whatever size the window happens to be laid out at.
[[nodiscard]] double fraction(const QSplitter* splitter, int index) {
    const QList<int> sizes = splitter->sizes();
    if (sizes.size() < 2) {
        return 0.0;
    }
    const int total = sizes.at(0) + sizes.at(1);
    return total > 0 ? static_cast<double>(sizes.at(index)) / total : 0.0;
}

/// A window that has restored its saved layout, in the order the header
/// documents: construct, resize, restore, *then* show.
///
/// The order matters. `restoreGeometry()` applied to an already-visible window
/// lets Qt re-fit it to the screen instead of honouring the stored size -- under
/// the offscreen platform a 1200x800 window came back as 900x774, which is its
/// minimum width. Restoring first avoids the whole question.
[[nodiscard]] std::unique_ptr<MainWindow> restoredWindow(int width = 1200,
                                                        int height = 800) {
    auto window = std::make_unique<MainWindow>();
    window->resize(width, height);
    window->restoreLayout();
    window->show();
    QApplication::processEvents();
    QApplication::processEvents();
    return window;
}

// ---------------------------------------------------------------------------
// The tab widget: the reason two classes exist rather than one
// ---------------------------------------------------------------------------

void test_editor_tabs_refuse_to_be_sized_by_their_largest_page() {
    announce("ResizableTabWidget size hints");
    ResizableTabWidget tabs;
    tabs.addTab(new BulkyPage, QStringLiteral("big"));
    tabs.addTab(new SmallPage, QStringLiteral("small"));
    tabs.addTab(new QWidget, QStringLiteral("bare"));

    // QTabWidget would report 1400x900 here -- the maximum over every page --
    // and pin the whole window to that. That is the bug this class exists for.
    TW_CHECK_EQ(tabs.minimumSizeHint(), QSize(ResizableTabWidget::kMinimumSide,
                                              ResizableTabWidget::kMinimumSide));

    const int bar = tabs.tabBar()->sizeHint().height() +
                    ResizableTabWidget::kTabBarMargin;

    tabs.setCurrentIndex(0);
    TW_CHECK_EQ(tabs.sizeHint().width(), 1400);
    TW_CHECK_EQ(tabs.sizeHint().height(), 900 + bar);

    // A valid but tiny hint is raised to the floor, not passed through.
    tabs.setCurrentIndex(1);
    TW_CHECK_EQ(tabs.sizeHint().width(), ResizableTabWidget::kMinimumSide);
    TW_CHECK_EQ(tabs.sizeHint().height(), ResizableTabWidget::kMinimumSide + bar);

    // A page that reports no hint at all (-1,-1, which is what a bare QWidget
    // does) falls back to the default instead of propagating the negative size to
    // the window.
    tabs.setCurrentIndex(2);
    TW_CHECK_EQ(tabs.sizeHint(),
                QSize(ResizableTabWidget::kFallbackSize.width(),
                      ResizableTabWidget::kFallbackSize.height() + bar));

    // And a tab widget with no pages still has to answer sensibly.
    auto* empty = new ResizableTabWidget;
    TW_CHECK_EQ(empty->sizeHint(), ResizableTabWidget::kFallbackSize);
    delete empty;
}

void test_enhanced_tab_bar_is_idempotent() {
    announce("enableEnhancedTabBar once");
    ResizableTabWidget tabs;
    tabs.addTab(new QWidget, QStringLiteral("a"));
    tabs.addTab(new QWidget, QStringLiteral("b"));

    TW_CHECK(!tabs.tabBar()->isMovable());
    tabs.enableEnhancedTabBar();
    TW_CHECK(tabs.tabBar()->isMovable());

    int moves = 0;
    QObject::connect(&tabs, &ResizableTabWidget::tabReordered,
                     [&moves](int, int) { ++moves; });

    // Calling it twice must not connect the reorder signal twice, or one drag
    // would be reported as two moves and the owner's per-tab bookkeeping would
    // double-count it -- corrupting exactly the state the signal exists to keep
    // in step.
    tabs.enableEnhancedTabBar();
    tabs.tabBar()->moveTab(0, 1);
    TW_CHECK_EQ(moves, 1);
}

// ---------------------------------------------------------------------------
// The window's layout
// ---------------------------------------------------------------------------

void test_window_frame_properties() {
    announce("window frame");
    MainWindow window;

    TW_CHECK_EQ(window.minimumSize(), QSize(900, 600));
    TW_CHECK(window.windowTitle().startsWith(QStringLiteral("\U0001F3A8 Time Warp Studio v")));
    TW_CHECK_EQ(window.objectName(), QStringLiteral("mainWindow"));
    // One stylesheet, scoped by object name, so it must be present.
    TW_CHECK(!window.styleSheet().isEmpty());
}

void test_central_column_nesting() {
    announce("central column");
    MainWindow window;

    QWidget* central = window.centralWidget();
    TW_CHECK(central != nullptr);
    TW_CHECK_EQ(central->objectName(), QStringLiteral("centralWidget"));

    auto* column = qobject_cast<QVBoxLayout*>(central->layout());
    TW_CHECK(column != nullptr);
    if (column == nullptr) {
        return;
    }
    // The 5px gutter the Python sets, matching the splitters' own margins.
    TW_CHECK_EQ(column->contentsMargins(), QMargins(5, 5, 5, 5));
    TW_CHECK_EQ(column->count(), 2);
    TW_CHECK_EQ(column->itemAt(0)->widget(), window.mainSplitter());
    TW_CHECK_EQ(column->itemAt(1)->widget(), window.functionKeyBar());
    // The splitter takes the slack; the key bar keeps its fixed height.
    TW_CHECK_EQ(column->stretch(0), 1);
    TW_CHECK_EQ(column->stretch(1), 0);
}

void test_main_splitter_geometry_and_behaviour() {
    announce("main splitter");
    const std::unique_ptr<MainWindow> window = shownWindow();
    QSplitter* splitter = window->mainSplitter();

    TW_CHECK_EQ(splitter->objectName(), QStringLiteral("mainSplitter"));
    TW_CHECK_EQ(splitter->orientation(), Qt::Horizontal);
    TW_CHECK_EQ(splitter->handleWidth(), 6);
    // Collapsible children can be dragged to zero width, and a zero-width pane
    // leaves no handle to drag back.
    TW_CHECK(!splitter->childrenCollapsible());
    TW_CHECK_EQ(splitter->count(), 2);
    TW_CHECK_EQ(horizontalStretch(splitter, 0), 3);
    TW_CHECK_EQ(horizontalStretch(splitter, 1), 2);
    // 720:480 of the Python's example geometry -- the editor gets three fifths.
    TW_CHECK(std::abs(fraction(splitter, 0) - 0.6) < 0.05);
    TW_CHECK_EQ(splitter->widget(0), window->editorColumnSplitter());
    TW_CHECK_EQ(splitter->widget(1), window->rightTabs());
}

void test_editor_column_is_editor_over_repl() {
    announce("editor column");
    const std::unique_ptr<MainWindow> window = shownWindow();
    QSplitter* column = window->editorColumnSplitter();

    TW_CHECK_EQ(column->objectName(), QStringLiteral("editorColumnSplitter"));
    TW_CHECK_EQ(column->orientation(), Qt::Vertical);
    TW_CHECK_EQ(column->handleWidth(), 6);
    TW_CHECK(!column->childrenCollapsible());
    TW_CHECK_EQ(column->count(), 2);
    TW_CHECK_EQ(column->widget(0), window->editorPaneSplitter());
    TW_CHECK_EQ(column->widget(1), window->immediateMode());

    // The REPL bar must not absorb spare height, and must not be draggable below
    // the height at which its entry line becomes unusable.
    TW_CHECK_EQ(verticalStretch(column, 0), 1);
    TW_CHECK_EQ(verticalStretch(column, 1), 0);
    // 550:50 -- the editor keeps the overwhelming share of the column.
    TW_CHECK(fraction(column, 0) > 0.7);
    TW_CHECK_EQ(window->immediateMode()->minimumHeight(),
                ImmediateModePanel::kMinimumHeight);
}

void test_editor_pane_splitter_holds_two_tab_groups() {
    announce("editor panes");
    MainWindow window;
    QSplitter* panes = window.editorPaneSplitter();

    TW_CHECK_EQ(panes->objectName(), QStringLiteral("editorPaneSplitter"));
    TW_CHECK_EQ(panes->orientation(), Qt::Horizontal);
    TW_CHECK_EQ(panes->handleWidth(), 5);
    TW_CHECK(!panes->childrenCollapsible());
    TW_CHECK_EQ(panes->count(), 2);
    // The primary pane is a container holding the tabs *and* the find bar, so the
    // bar sits directly under the text it searches rather than under the whole
    // editor column (which also holds the REPL).
    QWidget* primary = panes->widget(0);
    TW_CHECK_EQ(primary->objectName(), QStringLiteral("editorPane"));
    TW_CHECK_EQ(window.editorTabs()->parentWidget(), primary);
    TW_CHECK_EQ(window.findBar()->parentWidget(), primary);
    auto* primary_layout = qobject_cast<QVBoxLayout*>(primary->layout());
    TW_CHECK(primary_layout != nullptr);
    TW_CHECK_EQ(primary_layout->indexOf(window.editorTabs()), 0);
    TW_CHECK_EQ(primary_layout->indexOf(window.findBar()), 1);
    // Out of the way until Edit > Find asks for it.
    TW_CHECK(window.findBar()->isHidden());
    TW_CHECK_EQ(panes->widget(1), window.secondaryEditorTabs());
    // An even split: the split editor is meant to give both panes equal room.
    TW_CHECK_EQ(horizontalStretch(panes, 0), 1);
    TW_CHECK_EQ(horizontalStretch(panes, 1), 1);

    TW_CHECK(window.editorTabs()->tabsClosable());
    TW_CHECK_EQ(window.editorTabs()->objectName(), QStringLiteral("editorTabs"));
    // The secondary pane exists but is out of the way until a split is asked for.
    TW_CHECK(window.secondaryEditorTabs()->isHidden());
}

// The lesson panel is the only page in the right column that is not a
// placeholder, so it is the only one that gets a contract test: the placeholders
// are asserted by their size hints, and this one has behaviour.
void test_lesson_panel_contract() {
    announce("lesson panel");
    const std::unique_ptr<MainWindow> window = shownWindow();
    LessonPanel* panel = window->lessonPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }

    TW_CHECK_EQ(panel->objectName(), QStringLiteral("lessonPanel"));

    // The parts a learner touches, by object name: these are the names the
    // stylesheet and any screenshot tool will use, so they are a contract rather
    // than an implementation detail.
    TW_CHECK(panel->findChild<QComboBox*>(QStringLiteral("lessonChooser")) != nullptr);
    TW_CHECK(panel->findChild<QListWidget*>(QStringLiteral("lessonCheckpoints")) != nullptr);
    TW_CHECK(panel->findChild<QLabel*>(QStringLiteral("lessonVerdict")) != nullptr);
    TW_CHECK(panel->findChild<QProgressBar*>(QStringLiteral("lessonProgress")) != nullptr);
    TW_CHECK(panel->findChild<QTextBrowser*>(QStringLiteral("lessonDetail")) != nullptr);
    for (const char* name : {"lessonOpenButton", "lessonCheckButton", "lessonHintButton",
                             "lessonSolutionButton", "lessonResetButton"}) {
        TW_CHECK(panel->findChild<QPushButton*>(QString::fromLatin1(name)) != nullptr);
    }

    // Empty until the curriculum arrives. The panel does *not* ask for it itself:
    // loading it at start-up would make every user pay for a Python process, and
    // the window asks when the tab is first shown.
    TW_CHECK_EQ(panel->lessonCount(), 0);
    TW_CHECK(panel->currentLesson().isEmpty());
    TW_CHECK_EQ(panel->currentCheckpoint(), -1);
    TW_CHECK(panel->currentStarter().isEmpty());
    TW_CHECK(panel->verdictText().isEmpty());

    // It must survive being squeezed, or a splitter can reduce it to nothing and
    // there is no way back.
    TW_CHECK(panel->minimumSizeHint().width() >= LessonPanel::kMinimumWidth);

    TW_CHECK(window->rightTabs()->widget(4) == panel);
    TW_CHECK_EQ(window->rightTabs()->tabText(4), QString::fromUtf8("\U0001F393 Lessons"));
}

// Opening the Lessons tab loads the curriculum *in process*. This is the test
// that proves the panel is no longer a client of the Python worker: no bridge is
// started here, and the twenty lessons still arrive.
void test_the_lesson_panel_loads_the_curriculum_without_a_bridge() {
    announce("lessons load without the bridge");
    tw::test::TempDir directory;
    TW_CHECK(directory.valid());
    if (!directory.valid()) {
        return;
    }
    directory.useAsDataDir();
    {
        const std::unique_ptr<MainWindow> window = shownWindow();
        LessonPanel* panel = window->lessonPanel();
        TW_CHECK(panel != nullptr);
        if (panel != nullptr) {
            // Raising the tab is what makes the window ask: the panel itself does
            // not load anything at start-up.
            window->rightTabs()->setCurrentIndex(4);
            TW_CHECK_EQ(panel->lessonCount(), 20);
            TW_CHECK(!panel->currentLesson().isEmpty());
            // The first lesson's starter is a document the learner can open.
            TW_CHECK(!panel->currentStarter().isEmpty());
        }
    }
    directory.restoreDataDir();
}

// The panel keeps every verdict this session, not only the one on screen:
// session report lists what was tried, and the panel is the one place a verdict
// arrives. It has no bridge here -- these are the setters the worker's answers
// call.
void test_lesson_panel_records_the_session() {
    announce("lesson session record");
    const std::unique_ptr<MainWindow> window = shownWindow();
    LessonPanel* panel = window->lessonPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }

    timewarp::ui::LessonSummary lesson;
    lesson.id = QStringLiteral("basic_first_steps");
    lesson.title = QStringLiteral("First Steps in BASIC");
    lesson.language = QStringLiteral("BASIC");
    lesson.description = QStringLiteral("Print something.");
    lesson.objectives = QStringList{QStringLiteral("Use PRINT")};
    for (int index = 0; index < 3; ++index) {
        timewarp::ui::LessonCheckpoint checkpoint;
        checkpoint.title = QStringLiteral("Checkpoint %1").arg(index + 1);
        checkpoint.description = QStringLiteral("Do the thing.");
        lesson.checkpoints.append(checkpoint);
    }
    timewarp::ui::LessonProgress row;
    row.id = lesson.id;
    row.status = QStringLiteral("in_progress");
    row.checkpoint = 0;
    row.checkpoints = 3;
    panel->setLessons({lesson});
    panel->setProgress({row}, 0, 1);

    const auto before = panel->session();
    TW_CHECK_EQ(before.lessonTitle, lesson.title);
    TW_CHECK_EQ(before.language, lesson.language);
    TW_CHECK_EQ(before.status, QStringLiteral("in_progress"));
    TW_CHECK_EQ(before.objectives, lesson.objectives);
    TW_CHECK_EQ(before.checkpoints.size(), 3);
    for (const auto& checkpoint : before.checkpoints) {
        TW_CHECK(!checkpoint.checked);
    }

    // A pass advances the panel to the next checkpoint. The record of that pass
    // survives the move, and a verdict for a checkpoint that is no longer on
    // screen is still recorded -- it is a verdict, not a display.
    panel->setChecked(lesson.id, 0, true, QStringLiteral("Well done."), false, 0, 1);
    panel->setChecked(lesson.id, 2, false, QStringLiteral("Try again."), false, 0, 1);
    const auto recorded = panel->session();
    TW_CHECK_EQ(recorded.current, 1);
    TW_CHECK(recorded.checkpoints.at(0).checked);
    TW_CHECK(recorded.checkpoints.at(0).passed);
    TW_CHECK_EQ(recorded.checkpoints.at(0).message, QStringLiteral("Well done."));
    TW_CHECK(!recorded.checkpoints.at(1).checked);
    TW_CHECK(recorded.checkpoints.at(2).checked);
    TW_CHECK(!recorded.checkpoints.at(2).passed);
    TW_CHECK_EQ(recorded.checkpoints.at(2).message, QStringLiteral("Try again."));
}

// The classroom page: a class list and a row of marks per learner. It has no
// bridge here -- these are the panel's own setters, which is what the worker's
// answers end up calling.
void test_classroom_panel_contract() {
    announce("classroom panel");
    const std::unique_ptr<MainWindow> window = shownWindow();
    timewarp::ui::ClassroomPanel* panel = window->classroomPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }

    TW_CHECK_EQ(panel->objectName(), QStringLiteral("classroomPanel"));
    TW_CHECK(window->rightTabs()->widget(5) == panel);
    TW_CHECK_EQ(window->rightTabs()->tabText(5), QString::fromUtf8("\U0001F3EB Classroom"));

    // No class list is the state a fresh installation is in, and it has to say so
    // rather than show an empty table that looks like a class of nobody.
    TW_CHECK_EQ(panel->studentCount(), 0);
    TW_CHECK_EQ(panel->markRowCount(), 0);
    TW_CHECK(panel->emptyStateVisible());
    TW_CHECK(panel->findChild<QTableWidget*>(QStringLiteral("classroomMarks")) != nullptr);
    TW_CHECK(panel->findChild<QLabel*>(QStringLiteral("classroomEmptyState")) != nullptr);
    for (const char* name : {"classroomOpenFolder", "classroomRefresh", "classroomSummary",
                             "classroomBundle", "classroomUnbundle"}) {
        TW_CHECK(panel->findChild<QWidget*>(QString::fromLatin1(name)) != nullptr);
    }
    // Nothing to carry until there is a class; an archive to unpack is exactly how
    // a class arrives, so that button is always there.
    auto* save_bundle = panel->findChild<QPushButton*>(QStringLiteral("classroomBundle"));
    auto* load_bundle = panel->findChild<QPushButton*>(QStringLiteral("classroomUnbundle"));
    TW_CHECK(save_bundle != nullptr);
    TW_CHECK(load_bundle != nullptr);
    if (save_bundle != nullptr) {
        TW_CHECK(!save_bundle->isEnabled());
    }
    if (load_bundle != nullptr) {
        TW_CHECK(load_bundle->isEnabled());
    }

    QList<timewarp::ui::ClassroomStudent> students;
    students.append(timewarp::ui::ClassroomStudent{
        QStringLiteral("ada-lovelace"), QStringLiteral("Ada Lovelace"),
        QStringLiteral("/tmp/class/students/ada-lovelace")});
    students.append(timewarp::ui::ClassroomStudent{
        QStringLiteral("alan-turing"), QStringLiteral("Alan Turing"),
        QStringLiteral("/tmp/class/students/alan-turing")});
    panel->setRoster(students, {QStringLiteral("basic_first_steps")},
                     QStringLiteral("Chapter one"));
    TW_CHECK_EQ(panel->studentCount(), 2);
    TW_CHECK(!panel->emptyStateVisible());
    if (save_bundle != nullptr) {
        TW_CHECK(save_bundle->isEnabled());
    }
    // With nothing marked yet, Open Folder still has somewhere to go: the first
    // learner's directory is the useful default for a teacher looking at an
    // unmarked class.
    TW_CHECK_EQ(panel->directoryForRow(-1),
                QStringLiteral("/tmp/class/students/ada-lovelace"));

    QList<timewarp::ui::ClassroomMark> rows;
    rows.append(timewarp::ui::ClassroomMark{
        QStringLiteral("alan-turing"), QStringLiteral("basic_first_steps"),
        QStringLiteral("First steps"), 3, 3, 2, false});
    panel->setMarks(rows);
    TW_CHECK_EQ(panel->markRowCount(), 1);
    // The folder follows the *row*, not the class list order: a teacher clicking
    // Alan's mark must not be shown Ada's work.
    TW_CHECK_EQ(panel->directoryForRow(0),
                QStringLiteral("/tmp/class/students/alan-turing"));
    // A row is four readable cells, and "2/3" is what a teacher reads rather than
    // two numbers in two columns to be added up mentally.
    auto* table = panel->findChild<QTableWidget*>(QStringLiteral("classroomMarks"));
    TW_CHECK(table != nullptr);
    if (table != nullptr) {
        // The name, not the id: the mark rows carry ids and the roster carries
        // names, and a teacher reads the mark sheet.
        TW_CHECK_EQ(table->item(0, 0)->text(), QStringLiteral("Alan Turing"));
        TW_CHECK_EQ(table->item(0, 1)->text(), QStringLiteral("First steps"));
        TW_CHECK_EQ(table->item(0, 2)->text(), QStringLiteral("3/3"));
        TW_CHECK_EQ(table->item(0, 3)->text(), QStringLiteral("2/3"));
    }

    // A refresh that brings a new name updates the row rather than leaving the
    // old one: the mark row and the roster are two answers to "who is this?".
    students[1].name = QStringLiteral("Alan M. Turing");
    panel->setRoster(students, {QStringLiteral("basic_first_steps")},
                     QStringLiteral("Chapter one"));
    if (table != nullptr) {
        TW_CHECK_EQ(table->item(0, 0)->text(), QStringLiteral("Alan M. Turing"));
    }

    // A failure is said in the summary, and the last answer stays on screen: a
    // refresh the engine could not answer must not look like an empty class.
    panel->setFailure(QStringLiteral("classroom-marks"),
                      QStringLiteral("something went wrong"));
    auto* summary = panel->findChild<QLabel*>(QStringLiteral("classroomSummary"));
    TW_CHECK(summary != nullptr);
    if (summary != nullptr) {
        TW_CHECK(summary->text().contains(QStringLiteral("classroom-marks")));
        TW_CHECK(summary->text().contains(QStringLiteral("something went wrong")));
    }
    TW_CHECK_EQ(panel->studentCount(), 2);
    TW_CHECK(!panel->emptyStateVisible());

    // And a roster with nobody on it goes back to the empty state, because that
    // is what the engine reports for an empty class.
    panel->setRoster({}, {}, {});
    TW_CHECK_EQ(panel->studentCount(), 0);
    TW_CHECK(panel->emptyStateVisible());
    if (save_bundle != nullptr) {
        TW_CHECK(!save_bundle->isEnabled());
    }
}

void test_variables_panel_contract() {
    announce("variables panel");
    const std::unique_ptr<MainWindow> window = shownWindow();
    VariablesPanel* panel = window->variablesPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    TW_CHECK_EQ(panel->objectName(), QStringLiteral("variablesPanel"));
    TW_CHECK(window->rightTabs()->widget(1) == panel);

    // The parts a user touches, by object name: the filter, the table and the
    // sentence that explains an empty panel.
    auto* filter = panel->findChild<QLineEdit*>(QStringLiteral("variablesFilter"));
    auto* table = panel->findChild<QTableWidget*>(QStringLiteral("variablesTable"));
    auto* status = panel->findChild<QLabel*>(QStringLiteral("variablesStatus"));
    TW_CHECK(filter != nullptr);
    TW_CHECK(table != nullptr);
    TW_CHECK(status != nullptr);

    // Before a run there is nothing to show, and the panel says so rather than
    // presenting an empty table that reads as "this program has no variables".
    TW_CHECK_EQ(panel->rowCount(), 0);
    TW_CHECK_EQ(panel->visibleRowCount(), 0);
    TW_CHECK(status != nullptr && !status->text().isEmpty());

    const QList<timewarp::ui::VariableRow> rows{
        {QStringLiteral("name"), QStringLiteral("text"), QStringLiteral("ada")},
        {QStringLiteral("repcount"), QStringLiteral("number"), QStringLiteral("4")},
        {QStringLiteral("total"), QStringLiteral("number"), QStringLiteral("12.5")}};
    panel->setRows(rows);
    TW_CHECK_EQ(panel->rowCount(), 3);
    TW_CHECK_EQ(panel->visibleRowCount(), 3);
    TW_CHECK(table != nullptr && table->rowCount() == 3);
    if (table != nullptr) {
        TW_CHECK_EQ(table->item(0, 0)->text(), QStringLiteral("name"));
        TW_CHECK_EQ(table->item(0, 2)->text(), QStringLiteral("ada"));
    }

    // A filter narrows what is *shown* and keeps the set, so clearing it puts the
    // rows back. Hiding rows for good would make the filter destructive.
    panel->setFilter(QStringLiteral("COUNT"));
    TW_CHECK_EQ(panel->visibleRowCount(), 1);
    TW_CHECK_EQ(panel->rowCount(), 3);
    panel->setFilter(QString{});
    TW_CHECK_EQ(panel->visibleRowCount(), 3);

    // A filter that survives a new set of names would show an empty table with no
    // explanation, so setting rows re-applies it rather than keeping a stale one.
    panel->setFilter(QStringLiteral("nothing matches this"));
    panel->setRows(rows);
    TW_CHECK_EQ(panel->visibleRowCount(), 0);
    TW_CHECK_EQ(panel->rowCount(), 3);
    TW_CHECK(status != nullptr && !status->text().isEmpty());
}

void test_telemetry_panel_contract() {
    announce("telemetry panel");
    const std::unique_ptr<MainWindow> window = shownWindow();
    TelemetryPanel* panel = window->telemetryPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    TW_CHECK_EQ(panel->objectName(), QStringLiteral("telemetryPanel"));
    TW_CHECK(window->rightTabs()->widget(2) == panel);
    TW_CHECK(panel->findChild<QTableWidget*>(QStringLiteral("telemetryTable")) != nullptr);

    // A HUD over a run that has not happened has no counters, and no invented
    // ones either.
    TW_CHECK_EQ(panel->rowCount(), 0);
    TW_CHECK(panel->valueOf(QStringLiteral("Steps")).isEmpty());

    panel->setRows({{QStringLiteral("Steps"), QStringLiteral("1,180 of 250,000")},
                    {QStringLiteral("Canvas ink"), QStringLiteral("4 segments, 0 shapes")}});
    TW_CHECK_EQ(panel->rowCount(), 2);
    TW_CHECK_EQ(panel->valueOf(QStringLiteral("Steps")),
                QStringLiteral("1,180 of 250,000"));
    TW_CHECK_EQ(panel->valueOf(QStringLiteral("Canvas ink")),
                QStringLiteral("4 segments, 0 shapes"));
    TW_CHECK(panel->valueOf(QStringLiteral("no such label")).isEmpty());
}

void test_debug_panel_controls() {
    announce("debug panel");
    const std::unique_ptr<MainWindow> window = shownWindow();
    DebugPanel* panel = window->debugPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    TW_CHECK_EQ(panel->objectName(), QStringLiteral("debugPanel"));
    TW_CHECK(window->rightTabs()->widget(3) == panel);

    // The controls a debugger needs, by object name.
    for (const char* name : {"debugContinue", "debugPause", "debugStepInto",
                             "debugStepOver", "debugStepOut", "debugStop",
                             "debugAddBreakpoint", "debugRemoveBreakpoint"}) {
        TW_CHECK(panel->findChild<QPushButton*>(QString::fromLatin1(name)) != nullptr);
    }
    for (const char* name : {"debugLine", "debugBreakpoints", "debugStack",
                             "debugVariables", "debugStatus", "debugReason"}) {
        TW_CHECK(panel->findChild<QWidget*>(QString::fromLatin1(name)) != nullptr);
    }

    // Breakpoints are a set of lines, and the panel reports the whole set rather
    // than a delta, so nothing downstream has to keep its own copy in step.
    auto* line = panel->findChild<QSpinBox*>(QStringLiteral("debugLine"));
    TW_CHECK(line != nullptr);
    if (line != nullptr) {
        panel->setLineNumber(12);
        TW_CHECK_EQ(panel->lineNumber(), 12);
        TW_CHECK_EQ(line->value(), 12);
    }

    QList<int> announced;
    QObject::connect(panel, &DebugPanel::breakpointsChanged,
                     [&announced](const QList<int>& lines) { announced = lines; });
    auto* add = panel->findChild<QPushButton*>(QStringLiteral("debugAddBreakpoint"));
    auto* remove = panel->findChild<QPushButton*>(QStringLiteral("debugRemoveBreakpoint"));
    TW_CHECK(add != nullptr && remove != nullptr);

    // Every language can stop now -- at a statement, a message send or an
    // instruction -- so the controls stay live whichever document is open. The
    // call stack remains the Scheme machine's own, which is a display difference
    // rather than a disabled button.
    auto* combo = window->findChild<QComboBox*>(QStringLiteral("languageSelector"));
    TW_CHECK(combo != nullptr);
    if (combo != nullptr) {
        const int scheme = combo->findData(QStringLiteral("scheme"));
        const int basic = combo->findData(QStringLiteral("basic"));
        TW_CHECK(scheme >= 0);
        TW_CHECK(basic >= 0);
        if (scheme >= 0 && basic >= 0) {
            combo->setCurrentIndex(basic);
            TW_CHECK(panel->isAvailable());
            TW_CHECK(add != nullptr && add->isEnabled());
            combo->setCurrentIndex(scheme);
            TW_CHECK(panel->isAvailable());
            TW_CHECK(add != nullptr && add->isEnabled());
        }
    }

    if (add != nullptr && remove != nullptr) {
        panel->setLineNumber(7);
        add->click();
        TW_CHECK_EQ(announced, QList<int>{7});
        TW_CHECK_EQ(panel->breakpoints(), QList<int>{7});
        // Adding the same line twice is a set, not two entries.
        add->click();
        TW_CHECK_EQ(panel->breakpoints().size(), 1);
        remove->click();
        TW_CHECK(panel->breakpoints().isEmpty());
        TW_CHECK(announced.isEmpty());
    }

    // A paused run shows where, and an idle one does not claim a line.
    auto* status = panel->findChild<QLabel*>(QStringLiteral("debugStatus"));
    TW_CHECK(status != nullptr);
    panel->setPaused(true, 9, QStringLiteral("area"));
    TW_CHECK(status != nullptr && status->text().contains(QStringLiteral("9")));
    TW_CHECK(status != nullptr && status->text().contains(QStringLiteral("area")));
    panel->setPaused(false, 0, QString{});
    // "not paused" rather than "running": the panel cannot tell a run in
    // progress from one that has finished, so it says only what it knows.
    TW_CHECK(status != nullptr && !status->text().contains(QStringLiteral("area")));
    TW_CHECK(status != nullptr && status->text() != QStringLiteral("running"));

    // The stack and the frame's variables.
    panel->setStack({DebugPanel::Frame{QStringLiteral("<program>"), 1},
                     DebugPanel::Frame{QStringLiteral("area"), 9}});
    auto* stack = panel->findChild<QListWidget*>(QStringLiteral("debugStack"));
    TW_CHECK(stack != nullptr && stack->count() == 2);
    if (stack != nullptr && stack->count() == 2) {
        TW_CHECK(stack->item(0)->text().contains(QStringLiteral("<program>")));
        TW_CHECK(stack->item(1)->text().contains(QStringLiteral("area")));
    }
    panel->setVariables(
        {{QStringLiteral("w"), QStringLiteral("number"), QStringLiteral("3")}});
    auto* variables = panel->findChild<QTableWidget*>(QStringLiteral("debugVariables"));
    TW_CHECK(variables != nullptr && variables->rowCount() == 1);
    if (variables != nullptr && variables->rowCount() == 1) {
        TW_CHECK_EQ(variables->item(0, 0)->text(), QStringLiteral("w"));
        TW_CHECK_EQ(variables->item(0, 1)->text(), QStringLiteral("3"));
    }
}

void test_right_column_tabs_and_order() {
    announce("right column tabs");
    const std::unique_ptr<MainWindow> window = shownWindow();
    ResizableTabWidget* tabs = window->rightTabs();

    TW_CHECK_EQ(tabs->objectName(), QStringLiteral("rightTabs"));
    TW_CHECK_EQ(tabs->count(), 7);
    // Order is part of the interface: users reach for Output first, and the
    // classroom page is last because only a teacher opens it.
    TW_CHECK_EQ(tabTitles(tabs).toStdString(),
                std::string{"\U0001F4DD Output  \U0001F3A8 Graphics|"
                            "\U0001F50D Variables|\u26A1 Telemetry HUD|"
                            "\U0001F41B Debug|\U0001F393 Lessons|"
                            "\U0001F3EB Classroom|\U0001F5C4 Database"});
    TW_CHECK_EQ(tabs->widget(0), window->outputCanvasSplitter());

    // These are fixed panes, not documents: no close buttons, no dragging them
    // into a different order. The editor tabs next door are the opposite, which
    // is the distinction the Python draws by calling `_setup_tab_bar` on one and
    // not the other.
    TW_CHECK(!tabs->tabsClosable());
    TW_CHECK(!tabs->tabBar()->isMovable());
    TW_CHECK(window->editorTabs()->tabsClosable());
    TW_CHECK(window->editorTabs()->tabBar()->isMovable());
}

void test_output_and_graphics_share_a_persistent_split() {
    announce("output/canvas split");
    const std::unique_ptr<MainWindow> window = shownWindow();
    QSplitter* pane = window->outputCanvasSplitter();

    TW_CHECK_EQ(pane->objectName(), QStringLiteral("outputCanvasSplitter"));
    // Vertical and *not* tabbed: a program that prints and then draws must not
    // switch the user away from the output they are reading.
    TW_CHECK_EQ(pane->orientation(), Qt::Vertical);
    TW_CHECK(!pane->childrenCollapsible());
    TW_CHECK_EQ(pane->handleWidth(), 5);
    TW_CHECK_EQ(pane->count(), 2);
    TW_CHECK_EQ(pane->widget(0), window->outputSubTabs());
    // Canvas and playback strip travel together in one pane.
    TW_CHECK_EQ(pane->widget(1)->objectName(), QStringLiteral("canvasPane"));
    TW_CHECK(pane->widget(1)->findChild<timewarp::ui::TurtleCanvas*>() != nullptr);
    // 200:300 -- the canvas gets the larger share of the right column, which is
    // the point of showing both at once rather than flipping between them.
    TW_CHECK(std::abs(fraction(pane, 1) - 0.6) < 0.08);
}

void test_run_both_panes_compares_them() {
    announce("run both panes");
    const std::unique_ptr<MainWindow> window = shownWindow();
    // Without a split there is nothing to compare, and the window says so instead
    // of running the same program twice.
    window->runBothPanes();
    TW_CHECK(!window->findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"))
                  ->toPlainText()
                  .contains(QStringLiteral("Both panes")));

    window->splitEditor();
    auto* left = qobject_cast<CodeEditor*>(window->editorTabs()->currentWidget());
    auto* right = qobject_cast<CodeEditor*>(
        window->secondaryEditorTabs()->currentWidget());
    TW_CHECK(left != nullptr);
    TW_CHECK(right != nullptr);
    if (left == nullptr || right == nullptr) {
        return;
    }
    // The left program fails on its second statement; the right one must still run,
    // which is the property a comparison table is worthless without.
    left->setPlainText(QStringLiteral("10 PRINT \"LEFT\"\n20 GOTO 999\n"));
    right->setPlainText(QStringLiteral("10 PRINT \"RIGHT\"\n20 PRINT 2 + 2\n"));

    window->runBothPanes();
    auto* console = window->findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    TW_CHECK(console != nullptr);
    if (console == nullptr) {
        return;
    }
    const QString text = console->toPlainText();
    TW_CHECK(text.contains(QStringLiteral("Both panes")));
    // Both programs are named, so the numbers can be attributed.
    TW_CHECK(text.contains(QStringLiteral("LEFT")));
    TW_CHECK(text.contains(QStringLiteral("RIGHT")));
    // One finished and one did not, and the table says which is which.
    TW_CHECK(text.contains(QStringLiteral("finished")));
    TW_CHECK(text.contains(QStringLiteral("stopped")));
    // The right program's own output arrived even though the left one failed.
    TW_CHECK(text.contains(QStringLiteral("4")));
    // And real timings, not a placeholder: every row of the table has "ms" in it.
    TW_CHECK(text.count(QStringLiteral(" ms")) >= 2);
}

void test_the_split_editor_moves_documents_between_panes() {
    announce("split editor");
    const std::unique_ptr<MainWindow> window = shownWindow();
    ResizableTabWidget* primary = window->editorTabs();
    ResizableTabWidget* secondary = window->secondaryEditorTabs();
    TW_CHECK(primary != nullptr);
    TW_CHECK(secondary != nullptr);
    if (primary == nullptr || secondary == nullptr) {
        return;
    }
    // The pane exists from construction and is hidden, so splitter sizes stay put.
    TW_CHECK(!secondary->isVisible());
    TW_CHECK(!window->editorSplit());
    const int primary_before = primary->count();

    window->splitEditor();
    TW_CHECK(window->editorSplit());
    TW_CHECK(secondary->isVisible());
    TW_CHECK_EQ(secondary->count(), 1);
    // Splitting moves the document rather than showing a second view of it, and
    // leaves a fresh one behind so both halves are usable.
    TW_CHECK_EQ(primary->count(), primary_before);
    TW_CHECK(secondary->widget(0) != primary->currentWidget());

    // Moving the only document out of the split closes it: a blank second pane is
    // not a state a user can do anything with.
    auto* moved = qobject_cast<CodeEditor*>(secondary->widget(0));
    TW_CHECK(moved != nullptr);
    if (moved != nullptr) {
        moved->setFocus();
        QApplication::processEvents();
        window->moveTabToOtherPane();
    }
    TW_CHECK(!window->editorSplit());
    TW_CHECK(!secondary->isVisible());
    TW_CHECK_EQ(secondary->count(), 0);
    TW_CHECK_EQ(primary->count(), primary_before + 1);

    // And Close Split merges whatever is on the right back into the left.
    window->splitEditor();
    TW_CHECK(secondary->isVisible());
    const int both = primary->count() + secondary->count();
    window->closeSplit();
    TW_CHECK(!secondary->isVisible());
    TW_CHECK_EQ(primary->count(), both);
    TW_CHECK_EQ(secondary->count(), 0);
    // The document that was moved is still open, not closed and quietly lost.
    TW_CHECK(primary->indexOf(moved) >= 0 || moved == nullptr);
}

void test_the_outline_lists_and_jumps() {
    announce("outline panel");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* panel = window->findChild<timewarp::ui::OutlinePanel*>(
        QStringLiteral("outlinePanel"));
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    // The dock holds the real panel now, not the placeholder it started as.
    TW_CHECK_EQ(window->outlineDock()->widget(), panel);

    CodeEditor* editor = window->currentEditor();
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        return;
    }
    // Text first, then the language: setting the text marks the document
    // modified, and the window re-derives the language from the path (a scratch
    // buffer is BASIC) when that happens.
    editor->setPlainText(QStringLiteral("TO SQUARE\n  REPEAT 4 [ FORWARD 50 RIGHT 90 ]\nEND\n"
                                        "TO TRI\n  REPEAT 3 [ FORWARD 50 RIGHT 120 ]\nEND\n"));
    editor->setLanguage(Language::Logo);
    // The edit restarted the debounce, which is the wiring an edit's own test
    // cannot see; `refreshOutline` is what the timer would have called.
    auto* timer = window->findChild<QTimer*>(QStringLiteral("outlineTimer"));
    TW_CHECK(timer != nullptr);
    if (timer != nullptr) {
        TW_CHECK(timer->isActive());
    }
    window->refreshOutline();
    // The ordering above is load-bearing: the edit re-derives the language, so the
    // switch comes after it.
    TW_CHECK_EQ(static_cast<int>(editor->language()), static_cast<int>(Language::Logo));
    TW_CHECK_EQ(panel->entryCount(), 2);
    TW_CHECK_EQ(panel->lineAt(0), 1);
    TW_CHECK_EQ(panel->lineAt(1), 4);
    TW_CHECK(!panel->emptyStateVisible());

    // Activating an entry is what a click does: it puts the cursor on that line.
    auto* list = panel->findChild<QListWidget*>(QStringLiteral("outlineList"));
    TW_CHECK(list != nullptr);
    if (list != nullptr) {
        QListWidgetItem* item = list->item(1);
        TW_CHECK(item != nullptr);
        if (item != nullptr) {
            TW_CHECK_EQ(item->text(), QStringLiteral("TRI"));
            QMetaObject::invokeMethod(list, "itemActivated", Q_ARG(QListWidgetItem*, item));
        }
    }
    TW_CHECK_EQ(editor->currentLine(), 4);

    // A language with no named places shows the empty state rather than a blank
    // box, and the empty state is a sentence rather than a disabled widget.
    editor->setLanguage(Language::Focal);
    editor->setPlainText(QStringLiteral("1.10 T \"HI\"\n"));
    window->refreshOutline();
    TW_CHECK_EQ(panel->entryCount(), 0);
    TW_CHECK(panel->emptyStateVisible());
}

void test_the_scrubber_appears_only_when_there_is_ink() {
    announce("canvas scrubber");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* scrubber = window->findChild<timewarp::ui::Scrubber*>(QStringLiteral("scrubber"));
    TW_CHECK(scrubber != nullptr);
    if (scrubber == nullptr) {
        return;
    }
    // An empty canvas has nothing to replay, so the strip keeps out of the way
    // and gives the drawing its height.
    TW_CHECK(!scrubber->isVisible());
    TW_CHECK_EQ(scrubber->frames(), 0);

    auto forward = timewarp::canvas::make_motion(
        timewarp::canvas::CanvasCommandKind::Forward, 20.0);
    window->turtleCanvas()->apply(timewarp::canvas::CanvasCommandView{forward, {}, {}});
    TW_CHECK_EQ(scrubber->frames(), 1);
    TW_CHECK(scrubber->isVisible());
    TW_CHECK_EQ(scrubber->visibleFrame(), 1);

    // The slider is what a user drags, and it drives the canvas's view without
    // deleting the ink behind it.
    auto* slider = scrubber->findChild<QSlider*>(QStringLiteral("scrubberSlider"));
    TW_CHECK(slider != nullptr);
    if (slider != nullptr) {
        slider->setValue(0);
        TW_CHECK_EQ(window->turtleCanvas()->visibleFrameCount(), 0);
        TW_CHECK_EQ(window->turtleCanvas()->strokeCount(), 1);
        slider->setValue(1);
        TW_CHECK_EQ(window->turtleCanvas()->visibleFrameCount(), 1);
    }

    // `clear` empties the history, which hides the strip again.
    auto clear = timewarp::canvas::make_command(timewarp::canvas::CanvasCommandKind::Clear);
    window->turtleCanvas()->apply(timewarp::canvas::CanvasCommandView{clear, {}, {}});
    TW_CHECK_EQ(scrubber->frames(), 0);
    TW_CHECK(!scrubber->isVisible());
}

// The command log is how a picture can be explained after the fact: a program
// emits hundreds of commands, and "why does it look like that" is answered by the
// sequence. The spelling of each kind is what a person reads, so it is asserted
// directly rather than through the widget.
void test_turtle_command_log_spells_each_command() {
    announce("turtle command log");
    using timewarp::canvas::CanvasCommand;
    using timewarp::canvas::CanvasCommandKind;
    using timewarp::canvas::CanvasCommandView;
    using timewarp::ui::TurtleCommandLog;

    const auto line = [](CanvasCommand command, std::vector<double> points = {},
                         std::string text = {}) {
        return TurtleCommandLog::describe(CanvasCommandView{command, points, text});
    };

    TW_CHECK_EQ(line(timewarp::canvas::make_motion(CanvasCommandKind::Forward, 100.0)),
                QStringLiteral("FORWARD 100"));
    // A half degree survives: a heading rounded to a whole number is a drawing
    // that quietly disagrees with the program.
    TW_CHECK_EQ(line(timewarp::canvas::make_motion(CanvasCommandKind::Right, 90.5)),
                QStringLiteral("RIGHT 90.5"));

    CanvasCommand colour = timewarp::canvas::make_command(CanvasCommandKind::SetPenColor);
    colour.color = timewarp::canvas::make_rgb(255, 136, 0);
    TW_CHECK_EQ(line(colour), QStringLiteral("SETPENCOLOR #FF8800"));

    // A full circle has no meaningful extent, so it is not printed.
    CanvasCommand circle = timewarp::canvas::make_command(CanvasCommandKind::Circle);
    circle.x = 30.0;
    circle.y = 360.0;
    TW_CHECK_EQ(line(circle), QStringLiteral("CIRCLE 30"));

    // A point list is summarised: the coordinates of a 200-point polygon would
    // bury every command around it.
    CanvasCommand polygon = timewarp::canvas::make_command(CanvasCommandKind::DrawPolyline);
    polygon.color = timewarp::canvas::make_rgb(0, 255, 0);
    TW_CHECK_EQ(line(polygon, {0.0, 0.0, 10.0, 10.0, 20.0, 0.0}),
                QStringLiteral("DRAWPOLYLINE 3 points #00FF00"));

    CanvasCommand text = timewarp::canvas::make_command(CanvasCommandKind::DrawText);
    text.x = 5.0;
    text.y = 6.0;
    text.w = 12.0;
    TW_CHECK_EQ(line(text, {}, "hi"), QStringLiteral("DRAWTEXT \"hi\" at 5 6 (size 12)"));

    // A name is a payload too, and it arrives as trailing bytes.
    CanvasCommand sprite = timewarp::canvas::make_command(CanvasCommandKind::ShowSprite);
    TW_CHECK_EQ(line(sprite, {}, "ship"), QStringLiteral("SHOWSPRITE \"ship\""));

    TW_CHECK_EQ(line(timewarp::canvas::make_command(CanvasCommandKind::PenUp)),
                QStringLiteral("PENUP"));
}

// The strip and the log are views over the canvas, so they are asserted through
// the window: the command that reaches the drawing has to reach the record of it
// in the same call, or the two disagree about what was drawn.
void test_turtle_log_and_inspector_follow_the_canvas() {
    announce("turtle inspector");
    using timewarp::canvas::CanvasCommandKind;
    using timewarp::canvas::CanvasCommandView;

    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* inspector =
        window->findChild<timewarp::ui::TurtleInspector*>(QStringLiteral("turtleInspector"));
    auto* log = window->findChild<timewarp::ui::TurtleCommandLog*>(
        QStringLiteral("turtleLog"));
    TW_CHECK(inspector != nullptr);
    TW_CHECK(log != nullptr);
    if (inspector == nullptr || log == nullptr) {
        return;
    }

    // An untouched window has drawn nothing and says so, rather than showing a
    // pose from whatever was drawn last.
    TW_CHECK_EQ(log->commandCount(), 0);
    TW_CHECK_EQ(inspector->commandCount(), 0);

    // Through the window's own apply, which is the sink the bridge drains into.
    auto forward = timewarp::canvas::make_motion(CanvasCommandKind::Forward, 40.0);
    window->applyCanvasCommand(CanvasCommandView{forward, {}, {}});
    TW_CHECK_EQ(log->commandCount(), 1);
    TW_CHECK_EQ(inspector->commandCount(), 1);
    TW_CHECK(log->toPlainText().endsWith(QStringLiteral("FORWARD 40")));

    // Heading 0 is north, so a forward move grows Y: the strip reads the canvas's
    // own pose rather than keeping a copy that could drift from it.
    TW_CHECK(inspector->poseText().contains(QStringLiteral("Y 40")));
    TW_CHECK(inspector->penText().contains(QStringLiteral("DOWN")));

    auto pen_up = timewarp::canvas::make_command(CanvasCommandKind::PenUp);
    window->applyCanvasCommand(CanvasCommandView{pen_up, {}, {}});
    TW_CHECK(inspector->penText().contains(QStringLiteral("UP")));
    TW_CHECK_EQ(log->commandCount(), 2);

    auto right = timewarp::canvas::make_motion(CanvasCommandKind::Right, 90.0);
    window->applyCanvasCommand(CanvasCommandView{right, {}, {}});
    TW_CHECK(inspector->poseText().contains(QStringLiteral("Heading 90")));

    // Clearing the canvas clears the record of how it was made.
    window->clearCanvas();
    TW_CHECK_EQ(log->commandCount(), 0);
    TW_CHECK(log->toPlainText().isEmpty());
    TW_CHECK_EQ(inspector->commandCount(), 0);
    TW_CHECK(inspector->penText().contains(QStringLiteral("DOWN")));
}

void test_output_sub_tabs() {
    announce("output sub-tabs");
    MainWindow window;
    QTabWidget* tabs = window.outputSubTabs();

    TW_CHECK_EQ(tabs->objectName(), QStringLiteral("outputSubTabs"));
    // South because the strip sits against the divider, keeping the console and
    // the canvas visually contiguous.
    TW_CHECK_EQ(tabs->tabPosition(), QTabWidget::South);
    TW_CHECK(tabs->documentMode());
    TW_CHECK_EQ(tabs->count(), 3);
    TW_CHECK_EQ(tabTitles(tabs).toStdString(),
                std::string{"\U0001F4DD Console|\u274C Errors|\U0001F422 Turtle Log"});

    // The error and turtle logs are plain text edits with the fixed-width font
    // the Python sets, so log columns line up.
    auto* errors = tabs->findChild<QPlainTextEdit*>(QStringLiteral("errorsLog"));
    TW_CHECK(errors != nullptr);
    if (errors != nullptr) {
        TW_CHECK(errors->isReadOnly());
        // Bound to a named local: `font()` returns a QFont by value, so
        // `errors->font().family()` would hand back a reference into a temporary
        // that dies at the end of the statement.
        const QFont logFont = errors->font();
        TW_CHECK_EQ(logFont.family(), QStringLiteral("Courier New"));
    }
    TW_CHECK(tabs->findChild<QPlainTextEdit*>(QStringLiteral("turtleLog")) != nullptr);

    // The terminal is not part of a default window: it runs commands, so it is
    // opted into in Settings. Turning it on adds one tab, last; off removes it.
    TW_CHECK(window.findChild<timewarp::ui::TerminalWidget*>() == nullptr);
    window.setTerminalEnabled(true);
    TW_CHECK_EQ(tabs->count(), 4);
    TW_CHECK_EQ(tabTitles(tabs).toStdString(),
                std::string{"\U0001F4DD Console|\u274C Errors|\U0001F422 Turtle Log|"
                            "\U0001F4BB Terminal"});
    // Presentation mode is a room, not a workspace: the tab goes and comes back.
    window.setPresentationMode(true);
    TW_CHECK_EQ(tabs->count(), 3);
    window.setPresentationMode(false);
    TW_CHECK_EQ(tabs->count(), 4);

    window.setTerminalEnabled(false);
    TW_CHECK_EQ(tabs->count(), 3);
    // Leave the persisted setting as the default for the tests that follow.
    QSettings{}.remove(QStringLiteral("terminal/enabled"));
}

void test_function_key_bar_is_fixed_height() {    announce("function key bar");
    MainWindow window;
    FunctionKeyBar* bar = window.functionKeyBar();

    TW_CHECK_EQ(bar->objectName(), QStringLiteral("functionKeyBar"));
    // Both bounds, because a bar that only sets a minimum would still eat slack.
    TW_CHECK_EQ(bar->minimumHeight(), FunctionKeyBar::kFixedHeight);
    TW_CHECK_EQ(bar->maximumHeight(), FunctionKeyBar::kFixedHeight);
    TW_CHECK_EQ(bar->height(), FunctionKeyBar::kFixedHeight);

    // Ten keys plus the layer badge. The badge is a button too, which is why the
    // count is eleven rather than ten.
    const int buttons = static_cast<int>(bar->findChildren<QPushButton*>().size());
    TW_CHECK_EQ(buttons, 11);
    TW_CHECK(bar->findChild<QComboBox*>(QStringLiteral("deckSelector")) != nullptr);
    TW_CHECK(bar->layerBadge() != nullptr);
    TW_CHECK_EQ(static_cast<int>(bar->keyButtons().size()), 10);
}

void test_function_key_layers() {
    announce("function key layers");
    FunctionKeyBar bar;

    // Three layers of ten keys each, with actions and captions every time: a
    // slot with no action is the dead key this bar used to be made of.
    for (const FunctionKeyBar::Layer layer : {FunctionKeyBar::Layer::Normal,
                                              FunctionKeyBar::Layer::Shift,
                                              FunctionKeyBar::Layer::Control}) {
        const QList<FunctionKeyBar::Binding> bindings =
            FunctionKeyBar::bindingsFor(layer);
        TW_CHECK_EQ(bindings.size(), 10);
        QStringList ids;
        for (const FunctionKeyBar::Binding& binding : bindings) {
            TW_CHECK(!binding.keys.isEmpty());
            TW_CHECK(!binding.action.isEmpty());
            TW_CHECK(!binding.caption.isEmpty());
            ids << binding.action;
        }
        // Two keys on one layer doing the same thing would be a table mistake;
        // they are told apart by their id.
        TW_CHECK_EQ(ids.size(), static_cast<int>(QSet<QString>(ids.begin(), ids.end()).size()));
    }

    TW_CHECK(bar.layer() == FunctionKeyBar::Layer::Normal);
    const QString first_normal = bar.keyButtons().value(0) != nullptr
                                     ? bar.keyButtons().at(0)->text()
                                     : QString{};

    bar.cycleLayer();
    TW_CHECK(bar.layer() == FunctionKeyBar::Layer::Shift);
    TW_CHECK(bar.layerBadge()->text().contains(QStringLiteral("SHIFT")));
    // The keys are rebuilt, not merely relabelled: the actions differ per layer.
    TW_CHECK(bar.keyButtons().at(0)->text() != first_normal);
    TW_CHECK(bar.keyButtons().at(0)->property("action").toString() ==
             QStringLiteral("shortcuts"));

    bar.cycleLayer();
    TW_CHECK(bar.layer() == FunctionKeyBar::Layer::Control);
    bar.cycleLayer();
    TW_CHECK(bar.layer() == FunctionKeyBar::Layer::Normal);
    TW_CHECK_EQ(bar.keyButtons().at(0)->text(), first_normal);
}

void test_function_keys_are_wired() {
    announce("function keys dispatch");
    MainWindow window;
    FunctionKeyBar* bar = window.functionKeyBar();

    // Every key emits its action, and the window has a handler for each: the
    // dispatcher must not answer "No action is bound" for anything on the bar.
    // Only the keys that cannot open a dialog are actually clicked -- the point
    // of the test is the wiring, not the save prompt.
    QStringList seen;
    QObject::connect(bar, &FunctionKeyBar::actionTriggered, &window,
                     [&seen](const QString& action) { seen << action; });

    bar->setLayer(FunctionKeyBar::Layer::Control);
    // Not `keywords`: Ctrl+F1 is the menu's now, and opening the inspector is a
    // modal dialog this test must not enter.
    const QStringList safe{QStringLiteral("lessons"),
                           QStringLiteral("clear_output"),
                           QStringLiteral("filter_errors"),
                           QStringLiteral("clear_canvas"),
                           QStringLiteral("pause"), QStringLiteral("breakpoint")};
    for (QPushButton* button : bar->keyButtons()) {
        if (safe.contains(button->property("action").toString())) {
            button->click();
        }
    }
    TW_CHECK_EQ(seen.size(), static_cast<int>(safe.size()));

    // A key with no handler says so rather than doing nothing at all.
    window.performAction(QStringLiteral("definitely_not_a_real_action"));
    const QString message = window.statusBar()->currentMessage();
    TW_CHECK(message.contains(QStringLiteral("No action is bound")));

    // The free function keys really are bound to their actions, and the ones the
    // menus already own are not bound twice -- two actions on one sequence is an
    // ambiguous shortcut that Qt resolves by guessing.
    auto* fit = window.findChild<QAction*>(QStringLiteral("fkey_fit"));
    TW_CHECK(fit != nullptr);
    if (fit != nullptr) {
        TW_CHECK_EQ(fit->shortcut(), QKeySequence{QStringLiteral("F5")});
    }
    TW_CHECK(window.findChild<QAction*>(QStringLiteral("fkey_outline")) != nullptr);
    TW_CHECK(window.findChild<QAction*>(QStringLiteral("fkey_save")) == nullptr);
    TW_CHECK(window.findChild<QAction*>(QStringLiteral("fkey_run")) == nullptr);
    // F1 and Ctrl+F1 belong to the Help menu now; a second action on either
    // sequence would be the ambiguous shortcut the skip exists to avoid.
    TW_CHECK(window.findChild<QAction*>(QStringLiteral("fkey_help")) == nullptr);
    TW_CHECK(window.findChild<QAction*>(QStringLiteral("fkey_keywords")) == nullptr);

    // Decks are wired too: the debugger deck raises the Debug panel.
    window.selectDeck(QStringLiteral("debugger_deck"));
    TW_CHECK_EQ(window.rightTabs()->currentIndex(), 3);
    // The split deck toggles the second pane, the same state the File menu owns.
    window.selectDeck(QStringLiteral("split_lab"));
    TW_CHECK(window.editorSplit());
    window.selectDeck(QStringLiteral("split_lab"));
    TW_CHECK(!window.editorSplit());
}

void test_themes() {
    announce("themes");
    using timewarp::ui::themes::all;
    using timewarp::ui::themes::byName;
    using timewarp::ui::themes::defaultName;
    using timewarp::ui::themes::next;

    // The table ships 35 named schemes -- the Python's 33 plus the two Windows
    // schemes, 2000 and 7 -- so the count is a fact worth pinning: a truncated
    // table would silently drop the retro sets.
    TW_CHECK_EQ(all().size(), std::size_t{35});
    QSet<QString> names;
    for (const auto& theme : all()) {
        const QString name = QString::fromUtf8(theme.name);
        TW_CHECK(!name.isEmpty());
        names.insert(name);
        // Every colour is a valid #rrggbb, so a typo in the table cannot reach a
        // stylesheet as an empty string.
        for (const char* hex : {theme.background, theme.foreground, theme.editor_bg,
                                theme.editor_fg, theme.keyword, theme.comment,
                                theme.canvas_bg, theme.selection_bg}) {
            TW_CHECK(timewarp::ui::themes::colour(hex).isValid());
        }
    }
    TW_CHECK_EQ(names.size(), 35);

    // An unknown name is a stale setting, not a crash.
    TW_CHECK_EQ(QString::fromUtf8(byName(QStringLiteral("no such theme")).name),
                defaultName());
    // Cycling wraps.
    TW_CHECK_EQ(QString::fromUtf8(next(QString::fromUtf8(all().back().name)).name),
                defaultName());

    // These tests share a settings identity between runs, and the theme is a
    // persisted setting: start from "nothing chosen" so "a fresh window is the
    // default" is a statement about the window rather than about the last run.
    QSettings{}.remove(QStringLiteral("appearance/theme"));
    MainWindow window;
    const QString original = window.currentThemeName();
    // A fresh window starts on the default -- or on whatever the settings say,
    // which is the next assertion's job.
    TW_CHECK_EQ(original, defaultName());

    // Every theme applies: the window palette follows it, the theme's own rules
    // reach the stylesheet, and the canvas paper changes with it.
    for (const auto& theme : all()) {
        const QString name = QString::fromUtf8(theme.name);
        window.applyTheme(name);
        TW_CHECK_EQ(window.currentThemeName(), name);
        // Bound to named locals: `palette().color()` and `surfaceColor()` return
        // QColor by value, and the check macro binds a reference into the
        // temporary (GCC 16's `-Wdangling-reference` catches exactly this).
        const QColor window_colour = QApplication::palette().color(QPalette::Window);
        const QColor expected_window{QString::fromUtf8(theme.background)};
        TW_CHECK_EQ(window_colour, expected_window);
        TW_CHECK(window.styleSheet().contains(QString::fromUtf8(theme.selection_bg)));
        const QColor paper = window.turtleCanvas()->surfaceColor();
        const QColor expected_paper{QString::fromUtf8(theme.canvas_bg)};
        TW_CHECK_EQ(paper, expected_paper);
    }

    // The choice is remembered, which is what makes it a setting.
    window.applyTheme(QStringLiteral("Dracula"));
    {
        MainWindow second;
        TW_CHECK_EQ(second.currentThemeName(), QStringLiteral("Dracula"));
    }
    // Shift+F10's action cycles rather than repeating.
    window.cycleTheme();
    TW_CHECK(window.currentThemeName() != QStringLiteral("Dracula"));

    // Leave the developer's settings as they were for the tests after this one.
    window.applyTheme(original);
    QSettings{}.remove(QStringLiteral("appearance/theme"));
}

void test_high_contrast_clears_a_seven_to_one_floor() {
    announce("high contrast: the contrast floor");
    using timewarp::ui::themes::contrastRatio;
    using timewarp::ui::themes::resolve;

    // A dark theme, a light one and a monochrome retro set: the three cases where
    // an accent that happens to suit one surface would fail on another.
    for (const char* name : {"Dracula", "GitHub Light", "VGA Amber Lab"}) {
        // Copied, not bound by reference: `byName` takes a temporary and returns a
        // reference to the table, which GCC's dangling-reference analysis cannot
        // tell apart from a reference into the argument.
        const timewarp::ui::Theme theme =
            timewarp::ui::themes::byName(QString::fromLatin1(name));
        const timewarp::ui::themes::Colours colours = resolve(theme, true);
        const QColor surface = colours.editor_bg;

        // 7:1 is WCAG AAA for body text; the plan uses it as the floor.
        TW_CHECK(contrastRatio(colours.foreground, colours.background) >= 7.0);
        TW_CHECK(contrastRatio(colours.editor_fg, surface) >= 7.0);
        TW_CHECK(contrastRatio(colours.line_number_fg, colours.line_number_bg) >= 7.0);
        for (const QColor& accent : {colours.keyword, colours.comment, colours.string,
                                     colours.number, colours.function,
                                     colours.error_color, colours.warning_color,
                                     colours.success_color}) {
            TW_CHECK(contrastRatio(accent, surface) >= 7.0);
        }

        // The theme's direction survives: asking for more contrast on a dark theme
        // must not hand back a light one.
        const bool theme_is_dark = timewarp::ui::themes::colour(theme.background).lightness() < 128;
        TW_CHECK_EQ(colours.background.lightness() < 128, theme_is_dark);
        const QColor expected_surface = theme_is_dark ? QColor{Qt::black} : QColor{Qt::white};
        TW_CHECK_EQ(colours.background, expected_surface);

        // A selection has to stay visible against a pure surface.
        TW_CHECK(contrastRatio(colours.selection_fg, colours.selection_bg) >= 7.0);
    }

    // Without the flag the theme's own colours come through untouched, which is
    // what makes this a variant rather than a replacement.
    const timewarp::ui::Theme dracula = timewarp::ui::themes::byName(QStringLiteral("Dracula"));
    const timewarp::ui::themes::Colours plain = resolve(dracula, false);
    const QColor dracula_background = timewarp::ui::themes::colour(dracula.background);
    const QColor dracula_keyword = timewarp::ui::themes::colour(dracula.keyword);
    TW_CHECK_EQ(plain.background, dracula_background);
    TW_CHECK_EQ(plain.keyword, dracula_keyword);
}

void test_high_contrast_toggles_and_restores_the_palette() {
    announce("high contrast: toggle and restore");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* action = window->findChild<QAction*>(QStringLiteral("highContrastAction"));
    TW_CHECK(action != nullptr);
    if (action == nullptr) {
        return;
    }
    TW_CHECK(action->isCheckable());
    TW_CHECK_EQ(action->shortcut(), timewarp::core::keySequence(Qt::CTRL | Qt::ALT, Qt::Key_H));
    TW_CHECK(!window->highContrast());

    // This test changes the theme, and the theme is a persisted setting.
    const QString original = window->currentThemeName();

    // A dark theme first.
    window->applyTheme(QStringLiteral("Dracula"));
    const QPalette dark_before = QApplication::palette();
    action->trigger();
    TW_CHECK(window->highContrast());
    const QColor dark_window = QApplication::palette().color(QPalette::Window);
    TW_CHECK_EQ(dark_window, QColor{Qt::black});
    TW_CHECK(QApplication::palette() != dark_before);
    action->trigger();
    TW_CHECK(!window->highContrast());
    // Exactly: a toggle that leaves a role behind is a toggle a user has to fix by
    // changing theme and back.
    TW_CHECK(QApplication::palette() == dark_before);

    // And a light one, where the variant is white rather than black.
    window->applyTheme(QStringLiteral("GitHub Light"));
    const QPalette light_before = QApplication::palette();
    window->setHighContrast(true);
    const QColor light_window = QApplication::palette().color(QPalette::Window);
    TW_CHECK_EQ(light_window, QColor{Qt::white});
    // The menu's tick follows a change made through the API rather than the menu.
    if (auto* checked = window->findChild<QAction*>(QStringLiteral("highContrastAction"));
        checked != nullptr) {
        TW_CHECK(checked->isChecked());
    }
    window->setHighContrast(false);
    TW_CHECK(QApplication::palette() == light_before);
    window->applyTheme(original);
    QSettings{}.remove(QStringLiteral("appearance/theme"));
}

void test_font_scale_ladder() {
    announce("text scale");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* action = window->findChild<QAction*>(QStringLiteral("fontScaleAction"));
    TW_CHECK(action != nullptr);
    if (action == nullptr) {
        return;
    }
    // Start from the bottom step whatever an earlier test left behind.
    window->setFontScale(100);
    TW_CHECK_EQ(window->fontScale(), 100);
    TW_CHECK_EQ(action->shortcut(), timewarp::core::keySequence(Qt::CTRL | Qt::ALT, Qt::Key_A));

    QWidget* console = window->outputSubTabs()->findChild<QPlainTextEdit*>(
        QStringLiteral("consoleOutput"));
    TW_CHECK(console != nullptr);
    auto* console_view = qobject_cast<QPlainTextEdit*>(console);
    if (console_view == nullptr) {
        return;
    }
    const double base_editor = window->currentEditor()->font().pointSizeF();
    const int base_console = console_view->font().pointSize();
    const double base_panel = QApplication::font().pointSizeF();

    action->trigger();  // 125%
    TW_CHECK_EQ(window->fontScale(), 125);
    // The editor scales from the size the user chose, not by replacing it.
    TW_CHECK(std::abs(window->currentEditor()->font().pointSizeF() - base_editor * 1.25) < 0.01);
    // Ten points of console becomes 12.5, which rounds to 13: a log font smaller
    // than the editor's at every step would defeat the purpose.
    TW_CHECK_EQ(console_view->font().pointSize(), 13);
    TW_CHECK(QApplication::font().pointSizeF() > base_panel);

    // The ladder wraps rather than stopping at the top.
    window->cycleFontScale();
    TW_CHECK_EQ(window->fontScale(), 150);
    window->cycleFontScale();
    TW_CHECK_EQ(window->fontScale(), 100);
    TW_CHECK(std::abs(window->currentEditor()->font().pointSizeF() - base_editor) < 0.01);
    TW_CHECK_EQ(console_view->font().pointSize(), base_console);
    TW_CHECK(std::abs(QApplication::font().pointSizeF() - base_panel) < 0.01);

    // Off-ladder values snap, so a hand-edited settings file cannot produce a size
    // nothing was laid out for.
    window->setFontScale(140);
    TW_CHECK_EQ(window->fontScale(), 125);
    window->setFontScale(1000);
    TW_CHECK_EQ(window->fontScale(), 150);

    // Persisted: a second window opens at the scale the first one chose.
    {
        MainWindow second;
        TW_CHECK_EQ(second.fontScale(), 150);
    }
    window->setFontScale(100);
}

void test_focus_mode_hides_and_restores_the_chrome() {
    announce("focus mode");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* action = window->findChild<QAction*>(QStringLiteral("focusModeAction"));
    TW_CHECK(action != nullptr);
    if (action == nullptr) {
        return;
    }
    TW_CHECK(action->isCheckable());
    TW_CHECK_EQ(action->shortcut(), QKeySequence{Qt::Key_F11});
    TW_CHECK(!window->focusMode());

    auto* escape = window->findChild<QShortcut*>(QStringLiteral("focusModeEscape"));
    TW_CHECK(escape != nullptr);
    if (escape != nullptr) {
        // Escape has no business being bound while the mode is off.
        TW_CHECK(!escape->isEnabled());
    }

    // What is visible before entering is what has to come back.
    const bool bar_before = window->mainToolBar()->isVisible();
    const bool second_before = window->secondaryToolBar()->isVisible();
    const bool keys_before = window->functionKeyBar()->isVisible();
    const bool status_before = window->statusBar()->isVisible();
    TW_CHECK(bar_before);
    TW_CHECK(keys_before);

    action->trigger();
    TW_CHECK(window->focusMode());
    TW_CHECK(!window->mainToolBar()->isVisible());
    TW_CHECK(!window->secondaryToolBar()->isVisible());
    TW_CHECK(!window->functionKeyBar()->isVisible());
    TW_CHECK(!window->statusBar()->isVisible());
    if (escape != nullptr) {
        TW_CHECK(escape->isEnabled());
    }

    action->trigger();
    TW_CHECK(!window->focusMode());
    TW_CHECK_EQ(window->mainToolBar()->isVisible(), bar_before);
    TW_CHECK_EQ(window->secondaryToolBar()->isVisible(), second_before);
    TW_CHECK_EQ(window->functionKeyBar()->isVisible(), keys_before);
    TW_CHECK_EQ(window->statusBar()->isVisible(), status_before);
    if (escape != nullptr) {
        TW_CHECK(!escape->isEnabled());
    }

    // Esc is the way out, and it un-binds itself on the way out.
    window->setFocusMode(true);
    TW_CHECK(window->focusMode());
    if (escape != nullptr) {
        QMetaObject::invokeMethod(escape, "activated");
    }
    TW_CHECK(!window->focusMode());
    TW_CHECK(window->mainToolBar()->isVisible());
    if (escape != nullptr) {
        TW_CHECK(!escape->isEnabled());
    }
}

void test_presentation_mode_is_focus_mode_for_a_room() {
    announce("presentation mode");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* action = window->findChild<QAction*>(QStringLiteral("presentationModeAction"));
    TW_CHECK(action != nullptr);
    if (action == nullptr) {
        return;
    }
    TW_CHECK(action->isCheckable());
    // Next to F11, which already means "give the editor the window": this one is
    // the same idea with an audience.
    TW_CHECK_EQ(action->shortcut(), timewarp::core::keySequence(Qt::SHIFT, Qt::Key_F11));
    TW_CHECK(!window->presentationMode());

    auto* escape = window->findChild<QShortcut*>(QStringLiteral("presentationModeEscape"));
    TW_CHECK(escape != nullptr);
    if (escape != nullptr) {
        TW_CHECK(!escape->isEnabled());
    }

    // A windowed class, so this test says nothing about the machine's screen.
    window->setPresentationSettings(28, false);

    const bool bar_before = window->mainToolBar()->isVisible();
    const bool second_before = window->secondaryToolBar()->isVisible();
    const bool keys_before = window->functionKeyBar()->isVisible();
    const bool status_before = window->statusBar()->isVisible();
    const bool menu_before = window->menuBar()->isVisible();
    TW_CHECK(bar_before);
    TW_CHECK(keys_before);

    auto* console = window->findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    auto* description = window->findChild<QLabel*>(QStringLiteral("lessonDescription"));
    TW_CHECK(console != nullptr);
    TW_CHECK(description != nullptr);
    const int console_before = console == nullptr ? 0 : console->font().pointSize();

    action->trigger();
    TW_CHECK(window->presentationMode());
    // The menu bar goes too: focus mode keeps it for the person at the keyboard,
    // but a room does not need File and Edit above the lesson.
    TW_CHECK(!window->menuBar()->isVisible());
    TW_CHECK(!window->mainToolBar()->isVisible());
    TW_CHECK(!window->secondaryToolBar()->isVisible());
    TW_CHECK(!window->functionKeyBar()->isVisible());
    TW_CHECK(!window->statusBar()->isVisible());
    TW_CHECK(!window->isFullScreen());
    if (escape != nullptr) {
        TW_CHECK(escape->isEnabled());
    }
    // The two surfaces that have to be readable from the back of the room.
    if (console != nullptr) {
        TW_CHECK_EQ(console->font().pointSize(), 28);
    }
    if (description != nullptr) {
        TW_CHECK_EQ(description->font().pointSize(), 28);
    }

    // A size set while presenting lands without leaving the mode, which is what
    // Refresh after `teach.py present` does.
    window->setPresentationSettings(32, false);
    if (console != nullptr) {
        TW_CHECK_EQ(console->font().pointSize(), 32);
    }

    action->trigger();
    TW_CHECK(!window->presentationMode());
    TW_CHECK_EQ(window->menuBar()->isVisible(), menu_before);
    TW_CHECK_EQ(window->mainToolBar()->isVisible(), bar_before);
    TW_CHECK_EQ(window->secondaryToolBar()->isVisible(), second_before);
    TW_CHECK_EQ(window->functionKeyBar()->isVisible(), keys_before);
    TW_CHECK_EQ(window->statusBar()->isVisible(), status_before);
    if (escape != nullptr) {
        TW_CHECK(!escape->isEnabled());
    }
    // The console goes back through the settings that own it, not a guessed size.
    if (console != nullptr) {
        TW_CHECK_EQ(console->font().pointSize(), console_before);
    }

    // Esc is the way out, and it un-binds itself on the way out.
    window->setPresentationMode(true);
    TW_CHECK(window->presentationMode());
    if (escape != nullptr) {
        QMetaObject::invokeMethod(escape, "activated");
    }
    TW_CHECK(!window->presentationMode());
    TW_CHECK(window->mainToolBar()->isVisible());

    // Both modes own Escape, so asking for one while the other is on hands over
    // rather than leaving two Escape shortcuts competing for the key.
    window->setPresentationMode(true);
    window->setFocusMode(true);
    TW_CHECK(!window->presentationMode());
    TW_CHECK(window->focusMode());
    window->setFocusMode(false);

    // A class that asks for the whole screen gets it, and gets its window back.
    window->setPresentationSettings(24, true);
    window->setPresentationMode(true);
    TW_CHECK(window->presentationMode());
    TW_CHECK(window->isFullScreen());
    window->setPresentationMode(false);
    TW_CHECK(!window->presentationMode());
    TW_CHECK(!window->isFullScreen());
    TW_CHECK(window->mainToolBar()->isVisible());
}

// Recording is a state, not a dialog: the menu entry toggles it and the timer
// samples the canvas. An empty canvas records nothing, so stopping asks for no
// path -- which is why this test can stop it without a modal dialog.
void test_gif_recording_toggles_and_samples_the_canvas() {
    announce("record canvas as GIF");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* action = window->findChild<QAction*>(QStringLiteral("recordGifAction"));
    TW_CHECK(action != nullptr);
    if (action == nullptr) {
        return;
    }
    TW_CHECK(action->isCheckable());
    TW_CHECK(!action->isChecked());

    auto* timer = window->findChild<QTimer*>(QStringLiteral("gifRecordTimer"));
    TW_CHECK(timer != nullptr);
    if (timer != nullptr) {
        TW_CHECK(!timer->isActive());
    }

    action->trigger();
    TW_CHECK(action->isChecked());
    if (timer != nullptr) {
        TW_CHECK(timer->isActive());
        // Ten frames a second: the removed implementation's rate, and enough for
        // a turtle drawing without producing a file nobody can open.
        TW_CHECK_EQ(timer->interval(), 100);
    }

    action->trigger();
    TW_CHECK(!action->isChecked());
    if (timer != nullptr) {
        TW_CHECK(!timer->isActive());
    }
}

void test_command_palette() {
    announce("command palette");
    MainWindow window;
    timewarp::ui::CommandPalette palette{&window};
    palette.populate(&window);

    // The palette is the menu bar, so it cannot be shorter than the File menu.
    TW_CHECK(palette.commandCount() > 15);
    palette.setFilter(QStringLiteral("save"));
    QList<QString> rows = palette.visibleCommands();
    TW_CHECK(!rows.isEmpty());
    for (const QString& row : rows) {
        TW_CHECK(row.contains(QStringLiteral("ave"), Qt::CaseInsensitive));
    }

    // A filter that matches nothing offers nothing, rather than silently
    // offering everything.
    palette.setFilter(QStringLiteral("zzzz-no-such-command"));
    TW_CHECK_EQ(palette.commandCount(), 0);

    // Choosing a row really runs the command -- and closes the palette first, so
    // an action that opens a dialog is not left underneath it.
    window.clearOutput();
    auto* console = window.findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    TW_CHECK(console != nullptr);
    if (console != nullptr) {
        console->setPlainText(QStringLiteral("something"));
    }
    palette.setFilter(QStringLiteral("Clear Output"));
    TW_CHECK(palette.commandCount() >= 1);
    TW_CHECK(palette.activate(0));
    if (console != nullptr) {
        TW_CHECK(console->toPlainText().isEmpty());
    }
}

void test_standalone_player_layout() {
    announce("standalone player layout");
    MainWindow window;
    TW_CHECK(window.openStandaloneProgram(QStringLiteral("basic"),
                                          QStringLiteral("10 PRINT 1\n"),
                                          QStringLiteral("Player")));
    TW_CHECK(window.isPlayerMode());
    const auto* tabs = window.findChild<QTabWidget*>(QStringLiteral("rightTabs"));
    TW_CHECK(tabs != nullptr);
    if (tabs != nullptr) {
        // The program's output and drawing remain; the inspector panels and their
        // tab strip do not.
        TW_CHECK_EQ(tabs->count(), 1);
        TW_CHECK(tabs->widget(0) != nullptr &&
                 tabs->widget(0)->objectName() == QStringLiteral("outputCanvasSplitter"));
        TW_CHECK(!tabs->isHidden());
        TW_CHECK(tabs->tabBar()->isHidden());
    }
    const auto* project = window.findChild<QDockWidget*>(QStringLiteral("projectDock"));
    TW_CHECK(project != nullptr && project->isHidden());
}

void test_standalone_payload() {
    announce("standalone payload");
    using timewarp::ui::standalone::Embedded;

    // The format round-trips through the encoder and the decoder, which is the
    // half of "compile" that can be tested without a second process.
    const QByteArray bytes =
        timewarp::ui::standalone::encode(QStringLiteral("basic"),
                                         QStringLiteral("PRINT \"HI\"\n"));
    Embedded decoded;
    QString error;
    TW_CHECK(timewarp::ui::standalone::decode(bytes, &decoded, &error));
    TW_CHECK(decoded.present);
    TW_CHECK_EQ(decoded.language, QStringLiteral("basic"));
    TW_CHECK_EQ(decoded.source, QStringLiteral("PRINT \"HI\"\n"));

    // Truncation and foreign files are refused with a reason, not decoded into a
    // plausible-looking empty program.
    TW_CHECK(!timewarp::ui::standalone::decode(bytes.left(bytes.size() - 3), &decoded,
                                               &error));
    TW_CHECK(!error.isEmpty());
    TW_CHECK(!timewarp::ui::standalone::decode(QByteArray{"not a program"}, &decoded,
                                               &error));

    // End to end: build an artifact from the *test* binary (which is an
    // executable like any other) and read the program back out of it.
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    const QString source_path = dir.filePath(QStringLiteral("hello.bas"));
    {
        QFile source{source_path};
        TW_CHECK(source.open(QIODevice::WriteOnly));
        source.write("PRINT \"HELLO\"\n");
    }
    const QString artifact = dir.filePath(QStringLiteral("hello"));
    TW_CHECK(timewarp::ui::standalone::compileFile(
        QCoreApplication::applicationFilePath(), source_path, artifact, false, &error));

    const Embedded carried = timewarp::ui::standalone::embeddedIn(artifact);
    TW_CHECK(carried.present);
    TW_CHECK_EQ(carried.language, QStringLiteral("basic"));
    TW_CHECK_EQ(carried.source, QStringLiteral("PRINT \"HELLO\"\n"));

    // The artifact is executable, or the thing a learner is told to double-click
    // cannot be. Windows has no execute bit -- a file runs because of its
    // extension, and setPermissions there can only toggle read-only -- so the
    // promise is checkable on the POSIX builds, which is where it is made.
#ifndef Q_OS_WIN
    QFile::Permissions permissions = QFile::permissions(artifact);
    TW_CHECK(permissions.testFlag(QFile::ExeOwner));
#endif

    // Overwriting needs to be asked for, and a file that is not a program is
    // refused before anything is written.
    TW_CHECK(!timewarp::ui::standalone::compileFile(
        QCoreApplication::applicationFilePath(), source_path, artifact, false, &error));
    const QString text_path = dir.filePath(QStringLiteral("notes.txt"));
    {
        QFile text{text_path};
        TW_CHECK(text.open(QIODevice::WriteOnly));
        text.write("hello");
    }
    TW_CHECK(!timewarp::ui::standalone::compileFile(
        QCoreApplication::applicationFilePath(), text_path,
        dir.filePath(QStringLiteral("notes")), true, &error));

    // A plain executable carries nothing, which is the IDE's own case.
    TW_CHECK(!timewarp::ui::standalone::embeddedIn(
                  QCoreApplication::applicationFilePath())
                  .present);
}

void test_tool_bar_configuration() {
    announce("tool bar");
    MainWindow window;
    QToolBar* bar = window.mainToolBar();
    QToolBar* display = window.secondaryToolBar();

    // A non-empty objectName is required for saveState() to persist the toolbar
    // at all: Qt skips unnamed tool bars silently.
    TW_CHECK_EQ(bar->objectName(), QStringLiteral("mainToolBar"));
    TW_CHECK(!bar->isMovable());
    TW_CHECK_EQ(bar->toolButtonStyle(), Qt::ToolButtonTextBesideIcon);
#ifdef Q_OS_MACOS
    // The native macOS style sizes a tool bar itself and drops a widget-level
    // minimum on the floor -- it reports 0 -- so the number is pinned where it is
    // the thing that decides the layout, and macOS asserts the row exists.
    TW_CHECK(bar->sizeHint().height() > 0);
#else
    TW_CHECK_EQ(bar->minimumHeight(), 44);
#endif

    // Two rows, because one row of thirteen controls does not fit a 1400px window
    // and Qt hides the overflow behind a chevron rather than making it reachable.
    TW_CHECK_EQ(display->objectName(), QStringLiteral("secondaryToolBar"));
    TW_CHECK(!display->isMovable());
#ifdef Q_OS_MACOS
    TW_CHECK(display->sizeHint().height() > 0);
#else
    TW_CHECK_EQ(display->minimumHeight(), 44);
#endif

    // Run carries a menu of variants; the others are plain actions.
    auto* run = bar->findChild<QToolButton*>(QStringLiteral("runButton"));
    TW_CHECK(run != nullptr);
    if (run != nullptr) {
        TW_CHECK(run->menu() != nullptr);
        TW_CHECK_EQ(run->popupMode(), QToolButton::MenuButtonPopup);
    }
    TW_CHECK(bar->findChild<QComboBox*>(QStringLiteral("languageSelector")) != nullptr);
    // The display controls belong to the second row, and only to the second row.
    // The turtle-speed slider was removed rather than finished: nothing animates
    // the display list, so it controlled an ability the application does not
    // have. The Settings button that took its place is on the second row.
    TW_CHECK(display->findChild<QSlider*>(QStringLiteral("turtleSpeedSlider")) == nullptr);
    TW_CHECK(bar->findChild<QSlider*>(QStringLiteral("turtleSpeedSlider")) == nullptr);
    TW_CHECK(display->findChild<QAction*>(QStringLiteral("preferencesAction")) != nullptr);

    // The language selector has to actually offer the languages; an empty combo is
    // a control that promises a choice and gives none.
    const auto* selector = bar->findChild<QComboBox*>(QStringLiteral("languageSelector"));
    // Compared against the table rather than a literal: this checks the wiring,
    // and the count itself is pinned in ui_document_tests.
    TW_CHECK_EQ(selector->count(), timewarp::ui::allLanguages().size());
    TW_CHECK_EQ(selector->itemText(0), QStringLiteral("BASIC"));
    TW_CHECK_EQ(selector->itemData(0).toString(), QStringLiteral("basic"));

    // Every toolbar *button* on both rows has to say something when hovered, or
    // the toolbar gives no clue what a button does. Widgets added with
    // `addWidget()` (the language selector, the speed slider) arrive as
    // QWidgetActions with no tooltip of their own -- their control carries it.
    auto countButtons = [](const QToolBar* source) {
        int count = 0;
        for (const QAction* action : source->actions()) {
            if (action->isSeparator() ||
                qobject_cast<const QWidgetAction*>(action) != nullptr) {
                continue;
            }
            ++count;
            TW_CHECK(!action->toolTip().isEmpty());
        }
        return count;
    };
    // The file, language and run groups on the first row.
    TW_CHECK(countButtons(bar) >= 8);
    // Clear Output, Clear Canvas and A+ on the second.
    TW_CHECK(countButtons(display) >= 3);
}

void test_status_bar_badge_placement() {
    announce("status bar");
    MainWindow window;
    QStatusBar* bar = window.statusBar();

    TW_CHECK_EQ(bar->objectName(), QStringLiteral("mainStatusBar"));

    auto names = [bar] {
        QStringList found;
        for (const QLabel* label : bar->findChildren<QLabel*>()) {
            found << label->objectName();
        }
        return found;
    }();
    TW_CHECK(names.contains(QStringLiteral("languageBadge")));
    TW_CHECK(names.contains(QStringLiteral("positionBadge")));
    TW_CHECK(names.contains(QStringLiteral("executionBadge")));
    TW_CHECK(names.contains(QStringLiteral("breadcrumb")));

    auto* language = bar->findChild<QLabel*>(QStringLiteral("languageBadge"));
    auto* position = bar->findChild<QLabel*>(QStringLiteral("positionBadge"));
    auto* execution = bar->findChild<QLabel*>(QStringLiteral("executionBadge"));
    auto* breadcrumb = bar->findChild<QLabel*>(QStringLiteral("breadcrumb"));
    TW_CHECK(language != nullptr && position != nullptr && execution != nullptr &&
             breadcrumb != nullptr);

    // All four are permanent, so a transient status message cannot hide the path
    // or the badges. This is not cosmetic: the breadcrumb used to sit in the
    // message area, where Qt hides it for the duration of every message -- and
    // since the messages had no timeout, the path was never visible at all. What
    // caught it was a screenshot, with the message printed on top of the path.
    TW_CHECK_EQ(language->parentWidget(), bar);
    TW_CHECK_EQ(breadcrumb->parentWidget(), bar);

    window.setExecutionState(QStringLiteral("running"), QStringLiteral("Executing\u2026"));
    TW_CHECK_EQ(execution->text(), QStringLiteral("running"));
    TW_CHECK_EQ(bar->currentMessage(), QStringLiteral("Executing\u2026"));

    // Shown, because a widget cannot be visible in a window that is not.
    const std::unique_ptr<MainWindow> shown = shownWindow(1000, 700);
    QStatusBar* shownBar = shown->statusBar();
    auto* shownBreadcrumb = shownBar->findChild<QLabel*>(QStringLiteral("breadcrumb"));
    TW_CHECK(shownBreadcrumb != nullptr);
    if (shownBreadcrumb != nullptr) {
        // A fresh window has one unnamed document, and its name is the path.
        TW_CHECK_EQ(shownBreadcrumb->text(), QStringLiteral("untitled"));
        shown->setExecutionState(QStringLiteral("running"), QStringLiteral("Executing\u2026"));
        QApplication::processEvents();
        // Still on screen while a message is up, and positioned in the permanent
        // group rather than at the left-hand edge where messages appear. Both are
        // what stop the two from being painted over each other.
        TW_CHECK(shownBreadcrumb->isVisible());
        TW_CHECK(shownBreadcrumb->x() > 0);
    }
}

void test_outline_dock_starts_hidden() {
    announce("outline dock");
    MainWindow window;
    QDockWidget* dock = window.outlineDock();

    TW_CHECK_EQ(dock->objectName(), QStringLiteral("documentOutlineDock"));
    TW_CHECK(dock->widget() != nullptr);
    TW_CHECK_EQ(dock->allowedAreas(),
                Qt::DockWidgetAreas(Qt::RightDockWidgetArea |
                                    Qt::LeftDockWidgetArea));
    TW_CHECK(!dock->isVisible());
    TW_CHECK_EQ(window.dockWidgetArea(dock), Qt::RightDockWidgetArea);
}

void test_menu_structure() {
    announce("menu bar");
    MainWindow window;
    QList<QAction*> menus = window.menuBar()->actions();

    QStringList titles;
    for (const QAction* action : menus) {
        titles << action->text();
    }
    // Debug is absent on purpose: a menu with nothing to put in it is a promise
    // the window cannot keep. Tools carries the CHIP-8 emulator.
    TW_CHECK_EQ(titles.join(QLatin1Char('|')).toStdString(),
                std::string{"&File|&Project|&Edit|&Run|&Tools|&View|&Help"});

    // That the titles exist is not the interesting part. This test used to check
    // only the titles, which is how seven menus with no entries in them passed
    // for as long as they did: the window had a menu bar that opened onto
    // nothing, and every click on File did nothing at all.
    for (QAction* action : menus) {
        QMenu* menu = action->menu();
        TW_CHECK(menu != nullptr);
        if (menu == nullptr) {
            continue;
        }
        int entries = 0;
        for (const QAction* entry : menu->actions()) {
            if (!entry->isSeparator()) {
                ++entries;
            }
        }
        TW_CHECK(entries >= 1);
    }
}

/// Printing needs Qt6::PrintSupport and is optional, so File offers the two
/// entries exactly when the build has it. A dead entry is worse than an absent
/// one, which is why the absent branch is asserted too.
void test_print_entries_follow_the_component() {
    announce("File menu printing follows PrintSupport");
    MainWindow window;
    QAction* code = window.findChild<QAction*>(QStringLiteral("printCodeAction"));
    QAction* drawing = window.findChild<QAction*>(QStringLiteral("printDrawingAction"));
#ifdef TIME_WARP_HAVE_PRINT
    TW_CHECK(code != nullptr);
    TW_CHECK(drawing != nullptr);
    if (code != nullptr) {
        TW_CHECK(code->isEnabled());
        TW_CHECK(code->shortcut() == QKeySequence{QKeySequence::Print});
    }
    if (drawing != nullptr) {
        TW_CHECK(drawing->isEnabled());
    }
    // Owned by the File menu, not merely present in the window: an orphaned
    // action is invisible and therefore useless.
    for (QAction* action : {code, drawing}) {
        if (action == nullptr) {
            continue;
        }
        auto* menu = qobject_cast<QMenu*>(action->parent());
        TW_CHECK(menu != nullptr);
        if (menu != nullptr) {
            TW_CHECK_EQ(menu->objectName(), QStringLiteral("fileMenu"));
        }
    }
#else
    TW_CHECK(code == nullptr);
    TW_CHECK(drawing == nullptr);
#endif
}

/// Help carries the documentation browser and the student guide. F1 still opens
/// the shortcut reference, so both entries have to be reachable from the menu.
void test_help_menu_opens_the_documentation() {
    announce("help menu: documentation");
    MainWindow window;
    auto* documentation = window.findChild<QAction*>(QStringLiteral("documentationAction"));
    auto* getting_started = window.findChild<QAction*>(QStringLiteral("gettingStartedAction"));
    auto* context_help = window.findChild<QAction*>(QStringLiteral("contextHelpAction"));
    auto* inspector = window.findChild<QAction*>(QStringLiteral("keywordInspectorAction"));
    auto* shortcuts = window.findChild<QAction*>(QStringLiteral("shortcutsAction"));
    TW_CHECK(documentation != nullptr);
    TW_CHECK(getting_started != nullptr);
    TW_CHECK(context_help != nullptr);
    TW_CHECK(inspector != nullptr);
    TW_CHECK(shortcuts != nullptr);
    if (documentation != nullptr && getting_started != nullptr) {
        TW_CHECK(documentation->text().contains(QStringLiteral("Documentation")));
        TW_CHECK(getting_started->text().contains(QStringLiteral("Getting Started")));
        TW_CHECK(documentation->isEnabled());
        TW_CHECK(getting_started->isEnabled());
    }
    // F1 is context help and Ctrl+F1 the inspector. The shortcut table has no
    // sequence of its own: Shift+F1 on the function-key bar is its key, and a
    // menu sequence as well would be two actions on one shortcut.
    if (context_help != nullptr) {
        TW_CHECK_EQ(context_help->shortcut().toString().toStdString(), std::string{"F1"});
    }
    if (inspector != nullptr) {
        TW_CHECK_EQ(inspector->shortcut().toString().toStdString(), std::string{"Ctrl+F1"});
    }
    if (shortcuts != nullptr) {
        TW_CHECK(shortcuts->shortcut().isEmpty());
    }
}

/// The minimap reserves its space, maps a click to a scroll position and leaves
/// the text area alone when it is switched off. Whitespace marks are a document
/// option, so the check is that the option carries the flag.
void test_editor_minimap_and_whitespace() {
    announce("editor: minimap and whitespace");
    MainWindow window;
    QWidget* editor_widget = window.currentEditor();
    auto* editor = qobject_cast<timewarp::ui::CodeEditor*>(editor_widget);
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        return;
    }

    // A long document, so the strip has something to map and the scrollbar can
    // actually move.
    QStringList lines;
    for (int index = 1; index <= 200; ++index) {
        lines << QStringLiteral("PRINT \"line %1\"").arg(index);
    }
    editor->setPlainText(lines.join(QLatin1Char('\n')));
    QApplication::processEvents();

    editor->setMinimapVisible(true);
    TW_CHECK(editor->minimapVisible());
    TW_CHECK(editor->minimapWidth() > 0);
    QWidget* strip = editor->findChild<QWidget*>(QStringLiteral("minimapArea"));
    TW_CHECK(strip != nullptr);
    if (strip != nullptr) {
        // `isHidden()`, not `isVisible()`: the window is never shown in this
        // suite, so a child of it is not *visible* whatever its own flag says.
        // `isHidden()` answers the question the toggle controls.
        TW_CHECK(!strip->isHidden());
        TW_CHECK_EQ(strip->width(), editor->minimapWidth());
        // The strip sits in the margin, not over the text: its right edge is the
        // editor's content right edge.
        TW_CHECK(strip->geometry().right() <= editor->contentsRect().right());
    }

    // Clicking near the bottom maps near the end of the scrollbar, and near the
    // top maps to the start. This is the mapping the mouse handler uses.
    const int height = strip == nullptr ? 100 : strip->height();
    TW_CHECK(editor->scrollValueForMinimapY(0) <= editor->verticalScrollBar()->minimum());
    TW_CHECK(editor->scrollValueForMinimapY(height) >=
             editor->verticalScrollBar()->maximum());
    TW_CHECK(editor->scrollValueForMinimapY(height / 2) >
             editor->scrollValueForMinimapY(0));

    // Off: the margin is given back, so a narrow window has the text it had
    // before.
    editor->setMinimapVisible(false);
    TW_CHECK(!editor->minimapVisible());
    TW_CHECK_EQ(editor->minimapWidth(), 0);
    if (strip != nullptr) {
        TW_CHECK(strip->isHidden());
    }

    // Whitespace marks are a `QTextOption` flag on the document.
    timewarp::ui::EditorSettings settings = editor->settings();
    settings.show_whitespace = true;
    editor->applySettings(settings);
    TW_CHECK((editor->document()->defaultTextOption().flags() &
              QTextOption::ShowTabsAndSpaces) != 0);
    settings.show_whitespace = false;
    editor->applySettings(settings);
    TW_CHECK((editor->document()->defaultTextOption().flags() &
              QTextOption::ShowTabsAndSpaces) == 0);

    // And the strip reaches the paint code without a crash: `grab()` runs the
    // whole paint path, which is where an empty document or a zero-height strip
    // would divide by zero.
    editor->setMinimapVisible(true);
    editor->setPlainText(QString{});
    QApplication::processEvents();
    TW_CHECK(!editor->grab().isNull());
}

/// Version History lists the store's snapshots for one document, newest first,
/// and previews the text. Modal in use, so the dialog is constructed here rather
/// than exec()'d.
void test_version_history_lists_snapshots() {
    announce("version history");
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    if (!dir.isValid()) {
        return;
    }
    timewarp::ui::AutosaveStore store{dir.filePath(QStringLiteral("autosave"))};
    const QString document = dir.filePath(QStringLiteral("program.bas"));
    static_cast<void>(store.save(document, QStringLiteral("10 PRINT 1\n"), 1000));
    static_cast<void>(store.save(document, QStringLiteral("10 PRINT 2\n"), 2000));
    // A snapshot of another document must not appear in this one's history.
    static_cast<void>(store.save(dir.filePath(QStringLiteral("other.bas")),
                                 QStringLiteral("20 END\n"), 3000));

    timewarp::ui::VersionHistoryDialog dialog{store, document};
    TW_CHECK_EQ(dialog.versionCount(), 2);
    // Nothing is chosen until a version is: cancelling must not hand the window
    // text to open.
    TW_CHECK(dialog.chosenText().isEmpty());

    auto* list = dialog.findChild<QListWidget*>(QStringLiteral("versionHistoryList"));
    auto* preview = dialog.findChild<QPlainTextEdit*>(QStringLiteral("versionHistoryPreview"));
    auto* restore = dialog.findChild<QPushButton*>(QStringLiteral("versionHistoryRestore"));
    TW_CHECK(list != nullptr);
    TW_CHECK(preview != nullptr);
    TW_CHECK(restore != nullptr);
    if (list != nullptr && preview != nullptr) {
        TW_CHECK_EQ(list->count(), 2);
        // The newest is selected, and the preview is its text.
        TW_CHECK_EQ(list->currentRow(), 0);
        TW_CHECK(preview->toPlainText().contains(QStringLiteral("10 PRINT 2")));
    }
    // Choosing a version accepts the dialog and hands back exactly the text that
    // was previewed; the window, not the dialog, decides where it goes.
    if (restore != nullptr) {
        restore->click();
        TW_CHECK_EQ(dialog.result(), int(QDialog::Accepted));
        TW_CHECK_EQ(dialog.chosenText().toStdString(), std::string{"10 PRINT 2\n"});
    }
}

/// The picker shows the catalogue, filtered by language, and reports a choice.
void test_snippet_picker_filters_by_language() {
    announce("snippet picker");
    QString error;
    const QList<timewarp::ui::Snippet> catalogue = timewarp::ui::loadSnippets(&error);
    TW_CHECK(error.isEmpty());
    TW_CHECK(!catalogue.isEmpty());
    if (catalogue.isEmpty()) {
        return;
    }

    timewarp::ui::SnippetDialog dialog;
    dialog.setSnippets(catalogue);
    dialog.setLanguage(timewarp::ui::Language::Logo);
    const timewarp::ui::Snippet first = dialog.chosen();
    TW_CHECK(first.language == timewarp::ui::Language::Logo);
    TW_CHECK(!first.body.isEmpty());

    // And a language with no entries shows everything rather than nothing: an
    // empty list tells a user their language has no snippets, which is not the
    // same as "the file could not be read".
    dialog.setLanguage(timewarp::ui::Language::Unknown);
    TW_CHECK(!dialog.chosen().body.isEmpty());
}

/// Find in Files is in the Edit menu, on Ctrl+Shift+F, wired to something. The
/// reserved action id and the menu entry have to agree, or the palette lists a
/// command the key does not reach.
void test_edit_menu_has_find_in_files() {
    announce("Edit > Find in Files");
    MainWindow window;
    QAction* action = window.findChild<QAction*>(QStringLiteral("findInFilesAction"));
    TW_CHECK(action != nullptr);
    if (action == nullptr) {
        return;
    }
    TW_CHECK_EQ(action->shortcut().toString().toStdString(), std::string{"Ctrl+Shift+F"});
    TW_CHECK(window.findChild<QAction*>(QStringLiteral("fkey_find_in_files")) == nullptr ||
             action->shortcut() != window.findChild<QAction*>(QStringLiteral("fkey_find_in_files"))->shortcut());
}

/// Every page of the right column has a key that raises it, and every key names
/// a page. A page added to `buildRightColumn` without one is visible and
/// unreachable from the palette, the function keys and every menu at once, which
/// is the fault this table exists to prevent.
void test_every_right_column_page_has_a_key() {
    announce("right column panel keys");
    MainWindow window;
    QTabWidget* tabs = window.rightTabs();
    TW_CHECK(tabs != nullptr);
    if (tabs == nullptr) {
        return;
    }
    const QStringList keys = MainWindow::panelKeys();
    TW_CHECK_EQ(keys.size(), tabs->count());

    for (int index = 0; index < keys.size() && index < tabs->count(); ++index) {
        // The key must raise the tab it is listed against: the order is the
        // contract, not just membership, because the table stores indices.
        tabs->setCurrentIndex(0);
        TW_CHECK(window.showPanel(keys.at(index)));
        TW_CHECK_EQ(tabs->currentIndex(), index);
    }

    // And an unknown key is refused rather than silently raising page one, which
    // is what a caller holding a name from data needs to be able to trust.
    TW_CHECK(!window.showPanel(QStringLiteral("no-such-panel")));
}

// ---------------------------------------------------------------------------
// Menu behaviour
// ---------------------------------------------------------------------------

/// `text` without its keyboard mnemonics, so "Zoom &In" matches "Zoom In".
QString withoutMnemonic(const QString& text) {
    return QString{text}.remove(QLatin1Char('&'));
}

/// The menu bar entry whose text is `title` (mnemonics included).
QMenu* menuNamed(MainWindow& window, const QString& title) {
    for (QAction* action : window.menuBar()->actions()) {
        if (withoutMnemonic(action->text()) == withoutMnemonic(title)) {
            return action->menu();
        }
    }
    return nullptr;
}

/// The first entry in `menu` whose text contains `fragment`, ignoring mnemonics.
QAction* entryContaining(QMenu* menu, const QString& fragment) {
    if (menu == nullptr) {
        return nullptr;
    }
    for (QAction* action : menu->actions()) {
        if (withoutMnemonic(action->text()).contains(fragment, Qt::CaseInsensitive)) {
            return action;
        }
    }
    return nullptr;
}

void test_file_menu_new_opens_a_document() {
    announce("File > New");
    MainWindow window;
    const int before = window.editorTabs()->count();

    QAction* item = entryContaining(menuNamed(window, QStringLiteral("&File")),
                                    QStringLiteral("New"));
    TW_CHECK(item != nullptr);
    if (item != nullptr) {
        item->trigger();
        TW_CHECK_EQ(window.editorTabs()->count(), before + 1);
    }
}

void test_file_menu_close_tab_closes_one() {
    announce("File > Close Tab");
    MainWindow window;
    window.newFile();  // so closing leaves a document behind
    const int before = window.editorTabs()->count();
    TW_CHECK(before >= 2);

    QAction* item = entryContaining(menuNamed(window, QStringLiteral("&File")),
                                    QStringLiteral("Close Tab"));
    TW_CHECK(item != nullptr);
    if (item != nullptr) {
        item->trigger();
        TW_CHECK_EQ(window.editorTabs()->count(), before - 1);
    }
}

void test_tools_menu_opens_one_emulator() {
    announce("Tools > CHIP-8 Emulator");
    MainWindow window;
    QAction* item = entryContaining(menuNamed(window, QStringLiteral("&Tools")),
                                    QStringLiteral("CHIP-8"));
    TW_CHECK(item != nullptr);
    if (item == nullptr) {
        return;
    }
    TW_CHECK(item->isEnabled());
    item->trigger();
    TW_CHECK_EQ(window.findChildren<timewarp::ui::Chip8Window*>().size(), 1);
    // A second invocation raises the running emulator rather than starting a
    // second machine on the same program.
    item->trigger();
    TW_CHECK_EQ(window.findChildren<timewarp::ui::Chip8Window*>().size(), 1);
}

void test_the_guided_tour_points_at_real_widgets() {
    announce("guided tour: every anchor resolves");
    MainWindow window;
    const QList<timewarp::ui::TourStep> steps = timewarp::ui::TourGuide::steps();
    TW_CHECK_EQ(steps.size(), 4);
    for (const timewarp::ui::TourStep& step : steps) {
        QWidget* anchor = window.findChild<QWidget*>(step.objectName);
        TW_CHECK(anchor != nullptr);
    }
}

void test_the_guided_tour_is_seen_once_and_rearmable() {
    announce("guided tour: seen once, re-armable");
    QSettings settings;
    settings.remove(QStringLiteral("tour/seen"));

    MainWindow window;
    timewarp::ui::TourGuide guide{&window};
    TW_CHECK(!guide.seen());
    guide.start();
    TW_CHECK(guide.active());
    TW_CHECK_EQ(guide.overlay()->step(), 0);
    guide.overlay()->dismiss();
    TW_CHECK(!guide.active());
    TW_CHECK(guide.seen());

    // A second window in the same installation does not show it again.
    timewarp::ui::TourGuide second{&window};
    second.start();
    TW_CHECK(!second.active());

    guide.rearm();
    TW_CHECK(!guide.seen());
    guide.start();
    TW_CHECK(guide.active());
    guide.overlay()->dismiss();
}

void test_the_guided_tour_walks_its_steps() {
    announce("guided tour: next, back and done");
    QSettings{}.remove(QStringLiteral("tour/seen"));
    MainWindow window;
    timewarp::ui::TourGuide guide{&window};
    guide.show();
    timewarp::ui::TourOverlay* overlay = guide.overlay();
    TW_CHECK(overlay != nullptr);
    if (overlay == nullptr) {
        return;
    }
    TW_CHECK_EQ(overlay->step(), 0);
    overlay->next();
    TW_CHECK_EQ(overlay->step(), 1);
    overlay->back();
    TW_CHECK_EQ(overlay->step(), 0);
    for (int index = 1; index < overlay->stepCount(); ++index) {
        overlay->next();
    }
    TW_CHECK(overlay->active());
    overlay->next();  // Done on the last step
    TW_CHECK(!overlay->active());
    TW_CHECK(guide.seen());
}

void test_coach_marks_appear_once() {
    announce("coach marks: once and dismissible");
    QSettings{}.remove(QStringLiteral("tour/coach/run-history"));
    MainWindow window;
    timewarp::ui::TourGuide guide{&window};
    TW_CHECK(!guide.coachSeen(QStringLiteral("run-history")));
    TW_CHECK(guide.hint(QStringLiteral("run-history")));
    TW_CHECK(guide.active());
    guide.overlay()->dismiss();
    TW_CHECK(!guide.active());
    TW_CHECK(guide.coachSeen(QStringLiteral("run-history")));
    TW_CHECK(!guide.hint(QStringLiteral("run-history")));
    // An unknown key is not a hint and does not open an empty card.
    TW_CHECK(!guide.hint(QStringLiteral("no-such-hint")));
}

void test_help_menu_opens_the_tour() {
    announce("Help > Guided Tour");
    MainWindow window;
    QAction* item = entryContaining(menuNamed(window, QStringLiteral("&Help")),
                                    QStringLiteral("Guided Tour"));
    TW_CHECK(item != nullptr);
    if (item != nullptr) {
        item->trigger();
        auto* overlay = window.findChild<timewarp::ui::TourOverlay*>(
            QStringLiteral("tourOverlay"));
        TW_CHECK(overlay != nullptr);
        TW_CHECK(overlay != nullptr && overlay->active());
    }
}

void test_settings_rearms_the_tour() {
    announce("Settings: re-arm the guided tour");
    timewarp::ui::PreferencesDialog dialog{timewarp::ui::EditorSettings{}};
    bool requested = false;
    QObject::connect(&dialog, &timewarp::ui::PreferencesDialog::tourRequested, &dialog,
                     [&requested] { requested = true; });
    auto* button = dialog.findChild<QPushButton*>(QStringLiteral("tourButton"));
    TW_CHECK(button != nullptr);
    if (button != nullptr) {
        button->click();
    }
    TW_CHECK(requested);
}

/// The terminal is a workspace preference, not an editor setting, and its
/// checkbox drives the tab immediately rather than waiting for OK.
void test_settings_toggle_the_terminal() {
    announce("Settings: terminal toggle");
    timewarp::ui::PreferencesDialog dialog{timewarp::ui::EditorSettings{}, false};
    bool enabled = false;
    QObject::connect(&dialog, &timewarp::ui::PreferencesDialog::terminalToggled, &dialog,
                     [&enabled](bool on) { enabled = on; });
    auto* box = dialog.findChild<QCheckBox*>(QStringLiteral("terminalEnabledBox"));
    TW_CHECK(box != nullptr);
    if (box != nullptr) {
        TW_CHECK(!box->isChecked());
        box->setChecked(true);
    }
    TW_CHECK(enabled);
    TW_CHECK(dialog.terminalEnabled());
}

void test_tab_close_button_closes_the_tab() {
    announce("the tab's own X button");
    MainWindow window;
    window.newFile();  // so closing one leaves a document behind
    QTabWidget* tabs = window.editorTabs();
    const int before = tabs->count();
    TW_CHECK(before >= 2);

    // The button Qt paints for `setTabsClosable(true)`, found the way a user
    // finds it: on the tab bar. Clicking it is the bug report -- the X was
    // drawn and inert, because `QTabWidget::tabCloseRequested` was never
    // connected to anything.
    QTabBar* bar = tabs->tabBar();
    QWidget* button = bar->tabButton(0, QTabBar::RightSide);
    if (button == nullptr) {
        button = bar->tabButton(0, QTabBar::LeftSide);
    }
    TW_CHECK(button != nullptr);
    if (button == nullptr) {
        return;
    }
    auto* close = qobject_cast<QAbstractButton*>(button);
    TW_CHECK(close != nullptr);
    if (close == nullptr) {
        return;
    }
    close->click();
    TW_CHECK_EQ(tabs->count(), before - 1);
}

void test_file_menu_lists_the_installed_examples() {
    announce("File > Open Example");
    // Its own directory rather than the checkout's Examples: this is about the
    // menu reading whatever this installation shipped, and a test that asserted
    // the corpus would fail every time an example was added.
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    if (!dir.isValid()) {
        return;
    }
    const QDir root{dir.path()};
    TW_CHECK(root.mkpath(QStringLiteral("logo")));
    TW_CHECK(root.mkpath(QStringLiteral("basic")));
    // An empty language folder is what the menu must skip rather than render as
    // a submenu with nothing in it.
    TW_CHECK(root.mkpath(QStringLiteral("apl")));
    for (const QString& file : {QStringLiteral("logo/tri.logo"), QStringLiteral("basic/hello.bas")}) {
        QFile handle{root.filePath(file)};
        TW_CHECK(handle.open(QIODevice::WriteOnly));
        handle.write("FORWARD 50\n");
    }

    // The variable is how the menu is pointed at a folder, so setting it is also
    // the assertion that the override works at all -- a packaged install has no
    // `Examples/` beside the binary to fall back on.
    qputenv("TIME_WARP_EXAMPLES", dir.path().toUtf8());
    MainWindow window;

    QMenu* file = menuNamed(window, QStringLiteral("&File"));
    QAction* item = entryContaining(file, QStringLiteral("Example"));
    TW_CHECK(item != nullptr);
    if (item == nullptr || item->menu() == nullptr) {
        qunsetenv("TIME_WARP_EXAMPLES");
        return;
    }
    QMenu* examples = item->menu();

    // The submenu is built when it opens, so nothing is listed until then: that
    // is what keeps it honest about a directory that changed while the window
    // was open.
    QMetaObject::invokeMethod(examples, "aboutToShow", Qt::DirectConnection);
    QStringList titles;
    for (QAction* entry : examples->actions()) {
        titles << withoutMnemonic(entry->text());
    }
    // Sorted by folder name and labelled with the same display names the rest of
    // the UI uses -- "CHIP-8", not "chip8".
    TW_CHECK_EQ(titles.join(QLatin1Char('|')).toStdString(),
                std::string{"BASIC|Logo"});

    QAction* logo_menu_action = examples->actions().value(1);
    QMenu* logo = logo_menu_action == nullptr ? nullptr : logo_menu_action->menu();
    TW_CHECK(logo != nullptr);
    if (logo == nullptr) {
        qunsetenv("TIME_WARP_EXAMPLES");
        return;
    }
    QAction* tri = entryContaining(logo, QStringLiteral("tri"));
    TW_CHECK(tri != nullptr);
    if (tri != nullptr) {
        const int before = window.editorTabs()->count();
        tri->trigger();
        TW_CHECK_EQ(window.editorTabs()->count(), before + 1);
        timewarp::ui::CodeEditor* editor = window.currentEditor();
        TW_CHECK(editor != nullptr);
        if (editor != nullptr) {
            // Opened as a document, not as text in the current one: the menu's
            // whole job is to get a learner to a file the editor knows the
            // language of.
            TW_CHECK_EQ(editor->toPlainText().toStdString(), std::string{"FORWARD 50\n"});
            TW_CHECK_EQ(editor->property("filePath").toString().toStdString(),
                        QFileInfo{root.filePath(QStringLiteral("logo/tri.logo"))}
                            .absoluteFilePath()
                            .toStdString());
        }
    }
    qunsetenv("TIME_WARP_EXAMPLES");
}

void test_view_menu_grid_drives_the_canvas() {
    announce("View > Show Grid");
    MainWindow window;
    timewarp::ui::TurtleCanvas* canvas = window.turtleCanvas();
    TW_CHECK(canvas != nullptr);
    if (canvas == nullptr) {
        return;
    }

    QAction* item = entryContaining(menuNamed(window, QStringLiteral("&View")),
                                    QStringLiteral("Grid"));
    TW_CHECK(item != nullptr);
    if (item == nullptr) {
        return;
    }

    // The tick and the canvas have to agree both ways round, which is why the
    // entry is checkable and reports the canvas's state rather than keeping one
    // of its own.
    TW_CHECK_EQ(item->isChecked(), canvas->gridVisible());
    const bool before = canvas->gridVisible();
    item->trigger();
    TW_CHECK_EQ(canvas->gridVisible(), !before);
    TW_CHECK_EQ(item->isChecked(), !before);
}

void test_view_menu_zoom_scales_the_canvas() {
    announce("View > Zoom In");
    MainWindow window;
    timewarp::ui::TurtleCanvas* canvas = window.turtleCanvas();
    TW_CHECK(canvas != nullptr);
    if (canvas == nullptr) {
        return;
    }

    QAction* in = entryContaining(menuNamed(window, QStringLiteral("&View")),
                                  QStringLiteral("Zoom In"));
    QAction* out = entryContaining(menuNamed(window, QStringLiteral("&View")),
                                   QStringLiteral("Zoom Out"));
    QAction* fit = entryContaining(menuNamed(window, QStringLiteral("&View")),
                                   QStringLiteral("Fit"));
    TW_CHECK(in != nullptr && out != nullptr && fit != nullptr);
    if (in == nullptr || out == nullptr || fit == nullptr) {
        return;
    }

    const double start = canvas->zoom();
    in->trigger();
    TW_CHECK(canvas->zoom() > start);
    out->trigger();
    out->trigger();
    TW_CHECK(canvas->zoom() < start);
    // Fit is the way back to 1:1, so it has to reset the zoom as well as the pan.
    fit->trigger();
    TW_CHECK_EQ(canvas->zoom(), 1.0);
}

/// View > Profile Run is per-session and off at launch: timing every statement
/// is work, and a remembered "on" would make every later run pay for a report
/// nobody asked to see.
void test_profile_run_is_opt_in() {
    announce("View menu: Profile Run is opt-in");
    MainWindow window;
    QAction* profile = entryContaining(menuNamed(window, QStringLiteral("&View")),
                                       QStringLiteral("Profile Run"));
    TW_CHECK(profile != nullptr);
    if (profile == nullptr) {
        return;
    }
    TW_CHECK(profile->isCheckable());
    TW_CHECK(!profile->isChecked());
    profile->setChecked(true);
    TW_CHECK(profile->isChecked());
}

// The window's prompt path, without the dialog: the runner asks, the answer the
// dialog would have collected goes back, and the program reads it. The dialog
// itself is modal and cannot be clicked from a test -- which is why this seam,
// the one the dialog calls, is the half worth covering, and why the round trip
// went unverified for so long.
void test_input_round_trip_reaches_the_runner() {
    announce("input round trip");

    timewarp::canvas::CanvasCommandQueue queue;
    timewarp::ui::NativeRunner runner{timewarp::lang::Language::Basic, queue};

    QStringList prompts;
    QString output;
    std::atomic<bool> finished{false};
    bool completed = false;
    QObject::connect(&runner, &timewarp::ui::NativeRunner::inputRequested, &runner,
                     [&](const QString& prompt) {
                         prompts << prompt;
                         runner.answerInput(QStringLiteral("21"));
                     });
    QObject::connect(&runner, &timewarp::ui::NativeRunner::outputAvailable, &runner,
                     [&](const QString& text) { output += text; });
    QObject::connect(&runner, &timewarp::ui::NativeRunner::run_finished, &runner,
                     [&](bool ok, const QString&) {
                         completed = ok;
                         finished = true;
                     });

    runner.load_program(QStringLiteral("10 INPUT \"N\"; N\n20 PRINT N * 2\n"));
    runner.start();
    TW_CHECK(spin_until([&finished] { return finished.load(); }));
    runner.wait(5000);

    TW_CHECK(completed);
    TW_CHECK_EQ(prompts.size(), 1);
    if (!prompts.isEmpty()) {
        TW_CHECK_EQ(prompts.first(), QStringLiteral("N "));
    }
    TW_CHECK(output.contains(QStringLiteral("42")));

    // The answer landed in the engine, not just in the runner: the Variables panel
    // reads the same snapshot, so a value the program used has to be there.
    bool saw_n = false;
    for (const auto& row : runner.variables()) {
        if (row.name == QStringLiteral("N")) {
            saw_n = true;
        }
    }
    TW_CHECK(saw_n);
}

// `SCREEN` is a request that has to survive the whole chain: the language asks
// its host, the host pushes a command, and the canvas changes what it draws.
// Each half has a test of its own; this is the seam between them, which is the
// one that could silently do nothing.
void test_a_program_screen_reaches_the_canvas() {
    announce("a screen reaches the canvas");

    timewarp::canvas::CanvasCommandQueue queue;
    timewarp::ui::NativeRunner runner{timewarp::lang::Language::Basic, queue};

    std::atomic<bool> finished{false};
    QObject::connect(&runner, &timewarp::ui::NativeRunner::run_finished, &runner,
                     [&](bool, const QString&) { finished = true; });

    runner.load_program(QStringLiteral("10 SCREEN 13\n20 PSET (5, 7), #FF0000\n"));
    runner.start();
    TW_CHECK(spin_until([&finished] { return finished.load(); }));
    runner.wait(5000);

    timewarp::ui::TurtleCanvas canvas;
    canvas.resize(800, 600);
    while (queue.drain([&canvas](const timewarp::canvas::CanvasCommandView& view) {
        canvas.apply(view);
    }) > 0) {
    }

    TW_CHECK(canvas.framebufferActive());
    TW_CHECK_EQ(canvas.framebufferSize().width(), 320);
    TW_CHECK_EQ(canvas.framebufferSize().height(), 200);
    TW_CHECK_EQ(canvas.framebufferPixel(5, 7), QColor(255, 0, 0));

    // And the window says which mode is on: on a screen X and Y are pixels from
    // the top-left, not the turtle's world, and a learner reading "X 156 Y 180"
    // while looking at a 320x200 game has the wrong explanation for it.
    const std::unique_ptr<MainWindow> window = shownWindow();
    timewarp::ui::TurtleCanvas* shown = window->turtleCanvas();
    TW_CHECK(shown != nullptr);
    if (shown != nullptr) {
        shown->apply(timewarp::canvas::CanvasCommandView{
            timewarp::canvas::make_framebuffer(320.0, 200.0), {}, {}});
        TW_CHECK_EQ(window->turtleInspector()->screenText(), QStringLiteral("SCREEN 320x200"));
        shown->apply(timewarp::canvas::CanvasCommandView{
            timewarp::canvas::make_clear_framebuffer(), {}, {}});
        TW_CHECK(window->turtleInspector()->screenText().isEmpty());
    }
}

/// The report shows what the engine measured, in the order the engine sorted it:
/// the dialog must not re-sort or drop rows.
void test_profile_report_lists_the_measurements() {
    announce("profile report rows");
    const QList<timewarp::ui::ProfileEntry> entries{
        {20, QStringLiteral("  PRINT I"), 10, 12.5},
        {10, QStringLiteral("FOR I = 1 TO 10"), 1, 0.5},
    };
    timewarp::ui::ProfileReportDialog dialog{QStringLiteral("loop.bas"), entries, 13.0};
    TW_CHECK_EQ(dialog.rowCount(), 2);
    TW_CHECK_EQ(dialog.lineAt(0), 20);
    TW_CHECK_EQ(dialog.lineAt(1), 10);
    TW_CHECK_EQ(dialog.hitsAt(0), std::uint64_t{10});
}

void test_edit_menu_acts_on_whatever_has_focus() {
    announce("Edit > Undo follows focus");
    MainWindow window;

    timewarp::ui::CodeEditor* editor = window.currentEditor();
    auto* console = window.findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    TW_CHECK(editor != nullptr);
    TW_CHECK(console != nullptr);
    if (editor == nullptr || console == nullptr) {
        return;
    }

    QAction* undo = entryContaining(menuNamed(window, QStringLiteral("&Edit")),
                                    QStringLiteral("Undo"));
    TW_CHECK(undo != nullptr);
    if (undo == nullptr) {
        return;
    }

    // A `setPlainText` empties the undo stack, so the edit that gets undone has to
    // be one the user could have typed. It also leaves the cursor at position 0,
    // which is why each insert below moves to the end first: inserting at the
    // start would quietly test a different edit than the one asserted.
    editor->setPlainText(QStringLiteral("kept"));
    console->setPlainText(QStringLiteral("log"));

    // Undo in the editor, with the editor focused.
    editor->setFocus();
    editor->moveCursor(QTextCursor::End);
    editor->insertPlainText(QStringLiteral(" typed"));
    TW_CHECK_EQ(editor->toPlainText().toStdString(), std::string{"kept typed"});
    undo->trigger();
    TW_CHECK_EQ(editor->toPlainText().toStdString(), std::string{"kept"});

    // And in the console, with the console focused. This is the part an Edit menu
    // bound to one widget gets wrong: it would edit the code editor while the
    // cursor the user is looking at is somewhere else.
    //
    // Focus routing needs a window that can hold focus. A hidden window has no
    // focus widget at all, and editTarget() then falls back to the editor by
    // design -- which is what the first half of this test just proved. So the
    // window is shown, and the routing is only asserted once focus was really
    // taken, because otherwise this would be testing the environment.
    window.show();
    QApplication::processEvents();
    console->setFocus(Qt::OtherFocusReason);
    QApplication::processEvents();
    TW_CHECK(QApplication::focusWidget() == console);
    if (QApplication::focusWidget() != console) {
        return;
    }

    console->moveCursor(QTextCursor::End);
    console->insertPlainText(QStringLiteral(" line"));
    TW_CHECK_EQ(console->toPlainText().toStdString(), std::string{"log line"});
    undo->trigger();
    TW_CHECK_EQ(console->toPlainText().toStdString(), std::string{"log"});
    TW_CHECK_EQ(editor->toPlainText().toStdString(), std::string{"kept"});
}

void test_run_menu_clear_output_empties_the_console() {
    announce("Run > Clear Output");
    MainWindow window;
    auto* console = window.findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    TW_CHECK(console != nullptr);
    if (console == nullptr) {
        return;
    }

    console->setPlainText(QStringLiteral("noise"));
    TW_CHECK(!console->toPlainText().isEmpty());

    QAction* item = entryContaining(menuNamed(window, QStringLiteral("&Run")),
                                    QStringLiteral("Clear Output"));
    TW_CHECK(item != nullptr);
    if (item != nullptr) {
        item->trigger();
        TW_CHECK(console->toPlainText().isEmpty());
    }
}

void test_run_button_popup_is_not_empty() {
    announce("run button popup");
    MainWindow window;
    auto* run = window.mainToolBar()->findChild<QToolButton*>(QStringLiteral("runButton"));
    TW_CHECK(run != nullptr);
    if (run == nullptr) {
        return;
    }
    QMenu* popup = run->menu();
    TW_CHECK(popup != nullptr);
    if (popup == nullptr) {
        return;
    }
    // It was an empty menu: the chevron opened onto nothing, which is the same
    // fault as a menu bar full of empty menus, one control further in.
    int entries = 0;
    for (const QAction* entry : popup->actions()) {
        if (!entry->isSeparator()) {
            ++entries;
        }
    }
    TW_CHECK(entries >= 2);
    TW_CHECK(popup->actions().first()->isEnabled());
}

/// True when nothing would happen if the user activated `action`.
///
/// Qt's own "is this signal connected?" (`QObject::isSignalConnected`) is
/// protected, so it cannot be asked from here. This test therefore guards the
/// *shape* of the fault instead: an entry must either have a submenu, or be an
/// enabled placeholder-free entry. The placeholders are named and checked by
/// `test_unported_controls_say_so_instead_of_doing_nothing`, which fails if one is
/// enabled before its feature exists -- that is what stops a dead control being
/// introduced by accident.
void test_no_enabled_control_is_inert() {
    announce("no empty menu, no live-looking placeholder");
    MainWindow window;

    // Every menu that exists has entries, and none of those entries is disabled
    // inside an otherwise usable menu without saying why.
    int menus = 0;
    for (QAction* menu_action : window.menuBar()->actions()) {
        QMenu* menu = menu_action->menu();
        if (menu == nullptr) {
            continue;
        }
        ++menus;
        int enabled = 0;
        for (const QAction* entry : menu->actions()) {
            if (!entry->isSeparator() && entry->isEnabled()) {
                ++enabled;
            }
        }
        TW_CHECK(enabled >= 1);
    }
    TW_CHECK(menus >= 5);

    // Every action the tool bars show and enable must have been given a tooltip,
    // because that is the one place the wiring is visible from the outside: the
    // controls that are not wired are disabled and explain themselves there.
    const QList<QToolBar*> bars{window.mainToolBar(), window.secondaryToolBar()};
    for (QToolBar* bar : bars) {
        for (const QAction* action : bar->actions()) {
            if (action->isSeparator() || !action->isEnabled() ||
                qobject_cast<const QWidgetAction*>(action) != nullptr) {
                continue;
            }
            TW_CHECK(!action->toolTip().isEmpty());
        }
    }
}

void test_unported_controls_say_so_instead_of_doing_nothing() {
    announce("no control is a placeholder");
    MainWindow window;

    // Nothing on either toolbar is a placeholder any more: Debug, Continue and
    // Trace now drive a run, so like Snippets below they are asserted the other
    // way round -- enabled, and describing what they do rather than excusing
    // themselves.
    const QList<QToolBar*> bars{window.mainToolBar(), window.secondaryToolBar()};
    for (const char* name : {"debugAction", "continueAction", "traceAction"}) {
        auto* action = window.findChild<QAction*>(QString::fromLatin1(name));
        TW_CHECK(action != nullptr);
        if (action != nullptr) {
            TW_CHECK(action->isEnabled());
            TW_CHECK(!action->toolTip().contains(QStringLiteral("not ported")));
        }
    }

    // Snippets used to be in that list. It is wired now, so it is asserted the
    // other way round: enabled, with a tooltip that describes what it does rather
    // than excusing it.
    QAction* snippets = nullptr;
    for (QToolBar* bar : bars) {
        for (QAction* action : bar->actions()) {
            if (action->text().contains(QStringLiteral("Snippets"))) {
                snippets = action;
                break;
            }
        }
    }
    TW_CHECK(snippets != nullptr);
    if (snippets != nullptr) {
        TW_CHECK(snippets->isEnabled());
        TW_CHECK(!snippets->toolTip().contains(QStringLiteral("not ported")));
    }

    // A+ was on that list too. It is the text-size ladder now, so it is enabled
    // and reachable by name; `test_font_scale_ladder` asserts what it does.
    auto* font_scale = window.findChild<QAction*>(QStringLiteral("fontScaleAction"));
    TW_CHECK(font_scale != nullptr);
    if (font_scale != nullptr) {
        TW_CHECK(font_scale->isEnabled());
        TW_CHECK(!font_scale->toolTip().contains(QStringLiteral("not ported")));
    }

    // The speed slider is gone rather than disabled: a control that delays nothing
    // until drawings animate was the one entry in this row that could never be
    // enabled, and a permanently disabled control reads as a broken feature.
    TW_CHECK(window.findChild<QSlider*>(QStringLiteral("turtleSpeedSlider")) == nullptr);
}

void test_crt_overlay_tracks_its_target() {
    announce("CRT overlay tracking");
    MainWindow window;
    CrtOverlay* overlay = window.crtOverlay();

    TW_CHECK(overlay != nullptr);
    if (overlay == nullptr) {
        return;
    }
    // A full-window overlay that accepted clicks would make every control
    // underneath dead.
    TW_CHECK(overlay->testAttribute(Qt::WA_TransparentForMouseEvents));
    TW_CHECK_EQ(overlay->target(), window.centralWidget());
    TW_CHECK(overlay->isHidden());  // the Python starts with it disabled

    window.resize(1100, 700);
    window.show();
    QApplication::processEvents();

    // The whole reason the overlay installs an event filter instead of being
    // resized by the window: it keeps tracking after a resize.
    const QRect before = overlay->geometry();
    window.resize(1400, 900);
    QApplication::processEvents();
    TW_CHECK(overlay->geometry() != before);
    TW_CHECK_EQ(overlay->geometry(), window.centralWidget()->rect());

    overlay->setScanlineIntensity(40);
    TW_CHECK_EQ(overlay->scanlineIntensity(), 40);
    // Clamped, not wrapped: a slider or a theme cannot drive it out of range.
    overlay->setScanlineIntensity(1000);
    TW_CHECK_EQ(overlay->scanlineIntensity(), 100);
    overlay->setScanlineIntensity(-5);
    TW_CHECK_EQ(overlay->scanlineIntensity(), 0);
}

/// Renders the overlay into an image and counts the pixels it touched, which is
/// the only way to say "paints nothing" without looking at a screen.
[[nodiscard]] int overlayInk(CrtOverlay& overlay) {
    overlay.resize(80, 60);
    QImage image{80, 60, QImage::Format_ARGB32_Premultiplied};
    image.fill(Qt::transparent);
    QPainter painter{&image};
    overlay.render(&painter);
    painter.end();
    int inked = 0;
    for (int y = 0; y < image.height(); ++y) {
        for (int x = 0; x < image.width(); ++x) {
            if (image.pixelColor(x, y).alpha() > 0) {
                ++inked;
            }
        }
    }
    return inked;
}

void test_crt_settings_reach_the_overlay() {
    announce("CRT settings");
    const std::unique_ptr<MainWindow> window = shownWindow();
    CrtOverlay* overlay = window->crtOverlay();
    TW_CHECK(overlay != nullptr);
    if (overlay == nullptr) {
        return;
    }

    CrtOverlay::Settings off;
    off.scanlines = 0;
    off.vignette = 0;
    off.curvature = 0;
    off.flicker = 0;
    off.phosphor = 0;
    window->setCrtSettings(off);
    // Zero everywhere is exactly zero: not a nearly-invisible layer that dulls the
    // text and leaves a user wondering what changed.
    TW_CHECK_EQ(overlayInk(*overlay), 0);
    TW_CHECK(!overlay->isVisible());

    // Each effect on its own reaches the overlay and paints something.
    for (int CrtOverlay::Settings::*field :
         {&CrtOverlay::Settings::scanlines, &CrtOverlay::Settings::vignette,
          &CrtOverlay::Settings::curvature, &CrtOverlay::Settings::phosphor}) {
        CrtOverlay::Settings one = off;
        one.*(field) = 100;
        window->setCrtSettings(one);
        TW_CHECK(overlayInk(*overlay) > 0);
        TW_CHECK(overlay->isVisible());
    }

    // The dialog drives the real overlay: a preview in a swatch can be right while
    // the window is wrong.
    window->setCrtSettings(off);
    window->showCrtSettings();
    auto* dialog = window->findChild<timewarp::ui::CrtSettingsDialog*>(
        QStringLiteral("crtSettingsDialog"));
    TW_CHECK(dialog != nullptr);
    if (dialog == nullptr) {
        return;
    }
    auto* scanlines = dialog->findChild<QSlider*>(QStringLiteral("crtScanlines"));
    auto* phosphor = dialog->findChild<QSlider*>(QStringLiteral("crtPhosphor"));
    TW_CHECK(scanlines != nullptr);
    TW_CHECK(phosphor != nullptr);
    if (scanlines != nullptr) {
        scanlines->setValue(45);
        TW_CHECK_EQ(window->crtSettings().scanlines, 45);
        TW_CHECK_EQ(overlay->settings().scanlines, 45);
        TW_CHECK(overlay->isVisible());
    }
    if (phosphor != nullptr) {
        phosphor->setValue(15);
        TW_CHECK_EQ(overlay->settings().phosphor, 15);
    }

    // Cancel puts back what was in force before the dialog opened, including the
    // overlay's visibility.
    dialog->reject();
    QApplication::processEvents();
    TW_CHECK_EQ(window->crtSettings().scanlines, 0);
    TW_CHECK_EQ(window->crtSettings().phosphor, 0);
    TW_CHECK(!overlay->isVisible());

    // OK persists, and a second window opens on the same values.
    window->showCrtSettings();
    dialog = window->findChild<timewarp::ui::CrtSettingsDialog*>(
        QStringLiteral("crtSettingsDialog"));
    TW_CHECK(dialog != nullptr);
    if (dialog != nullptr) {
        if (auto* value = dialog->findChild<QSlider*>(QStringLiteral("crtScanlines"));
            value != nullptr) {
            value->setValue(35);
        }
        if (auto* value = dialog->findChild<QSlider*>(QStringLiteral("crtPhosphor"));
            value != nullptr) {
            value->setValue(25);
        }
        dialog->accept();
        QApplication::processEvents();
    }
    TW_CHECK_EQ(window->crtSettings().scanlines, 35);
    {
        MainWindow second;
        TW_CHECK_EQ(second.crtSettings().scanlines, 35);
        TW_CHECK_EQ(second.crtSettings().phosphor, 25);
    }

    // Leave the developer's settings as they were: a saved CRT effect would make
    // the next test's fresh window start with the overlay showing.
    window->setCrtSettings(off);
    QSettings settings;
    for (const char* key : {"display/crt/scanlines", "display/crt/vignette",
                            "display/crt/curvature", "display/crt/flicker",
                            "display/crt/phosphor"}) {
        settings.remove(QString::fromLatin1(key));
    }
}

void test_screen_presets_pair_a_theme_with_its_crt_values() {
    announce("screen presets");
    const std::unique_ptr<MainWindow> window = shownWindow();
    CrtOverlay* overlay = window->crtOverlay();
    TW_CHECK(overlay != nullptr);
    if (overlay == nullptr) {
        return;
    }
    const QString original = window->currentThemeName();

    auto* amber = window->findChild<QAction*>(QStringLiteral("screenPresetAmber"));
    auto* green = window->findChild<QAction*>(QStringLiteral("screenPresetGreen"));
    auto* white = window->findChild<QAction*>(QStringLiteral("screenPresetWhite"));
    auto* normal = window->findChild<QAction*>(QStringLiteral("screenPresetNormal"));
    TW_CHECK(amber != nullptr);
    TW_CHECK(green != nullptr);
    TW_CHECK(white != nullptr);
    TW_CHECK(normal != nullptr);
    if (amber == nullptr || green == nullptr || white == nullptr || normal == nullptr) {
        return;
    }

    // A preset is a theme *and* the effect values that suit it: the theme alone
    // would be a colour scheme, and the CRT values alone would be a wash over
    // whatever was on screen.
    amber->trigger();
    TW_CHECK_EQ(window->currentThemeName(), QStringLiteral("Amber Monochrome"));
    TW_CHECK_EQ(window->crtSettings().phosphor_colour, QColor(255, 176, 0));
    TW_CHECK(window->crtSettings().scanlines > 0);
    TW_CHECK_EQ(overlay->settings().scanlines, window->crtSettings().scanlines);
    TW_CHECK(overlay->isVisible());

    green->trigger();
    TW_CHECK_EQ(window->currentThemeName(), QStringLiteral("Green Monochrome"));
    TW_CHECK_EQ(window->crtSettings().phosphor_colour, QColor(0, 255, 0));

    white->trigger();
    TW_CHECK_EQ(window->currentThemeName(), QStringLiteral("High Contrast Dark"));
    TW_CHECK(window->crtSettings().phosphor_colour.red() > 200);
    TW_CHECK(window->crtSettings().curvature > 0);

    // Normal is the way back: a normal theme and no effects at all.
    normal->trigger();
    TW_CHECK_EQ(window->currentThemeName(), QStringLiteral("VS Code Dark"));
    TW_CHECK_EQ(window->crtSettings().scanlines, 0);
    TW_CHECK_EQ(window->crtSettings().vignette, 0);
    TW_CHECK_EQ(window->crtSettings().curvature, 0);
    TW_CHECK_EQ(window->crtSettings().phosphor, 0);
    TW_CHECK(!overlay->isVisible());

    // Leave no saved effect behind for the tests that build a window next.
    window->applyTheme(original);
    QSettings{}.remove(QStringLiteral("appearance/theme"));
    QSettings settings;
    for (const char* key : {"display/crt/scanlines", "display/crt/vignette",
                            "display/crt/curvature", "display/crt/flicker",
                            "display/crt/phosphor"}) {
        settings.remove(QString::fromLatin1(key));
    }
}

void test_immediate_mode_panel_contract() {
    announce("immediate mode panel");
    MainWindow window;
    ImmediateModePanel* panel = window.immediateMode();

    TW_CHECK_EQ(panel->objectName(), QStringLiteral("immediateMode"));
    TW_CHECK(panel->minimumHeight() >= ImmediateModePanel::kMinimumHeight);

    // The panel reports the command rather than clearing its own input: only the
    // interpreter can say whether the command was accepted.
    QString received;
    QObject::connect(panel, &ImmediateModePanel::commandSubmitted,
                     [&received](const QString& text) { received = text; });

    auto* input = panel->findChild<QLineEdit*>();
    TW_CHECK(input != nullptr);
    if (input != nullptr) {
        input->setText(QStringLiteral("PRINT 2 + 2"));
        emit input->returnPressed();
        TW_CHECK_EQ(received, QStringLiteral("PRINT 2 + 2"));
        // The panel itself does not clear; the window does once it has handed the
        // command to the REPL. Both behaviours are asserted here because both are
        // part of who owns the input.
        TW_CHECK(panel->command().isEmpty());

        // An empty command is not a command.
        received.clear();
        input->setText(QStringLiteral("   "));
        emit input->returnPressed();
        TW_CHECK(received.isEmpty());
    }
}

void test_windows_sound_command_line() {
    announce("windows sound command line");
    // Windows has no `aplay` on PATH, so the player is PowerShell and a .NET
    // class, and the file path goes inside a script rather than after an
    // argument. A path is not a safe thing to paste into a script -- a user
    // called O'Brien has an apostrophe in it -- so the quoting is the part
    // worth pinning, and it can be checked from Linux.
    const QString plain =
        timewarp::ui::SoundPlayer::powerShellCommand(QStringLiteral("C:/Temp/tone.wav"));
    TW_CHECK(plain.contains(QStringLiteral("'C:/Temp/tone.wav'")));
    TW_CHECK(plain.startsWith(QStringLiteral("(New-Object Media.SoundPlayer ")));
    TW_CHECK(plain.endsWith(QStringLiteral(".PlaySync()")));

    // The doubled quote is PowerShell's escape, and it must not close the string.
    const QString quoted = timewarp::ui::SoundPlayer::powerShellCommand(
        QStringLiteral("C:/Users/O'Brien/AppData/Local/Temp/tone.wav"));
    TW_CHECK(quoted.contains(QStringLiteral("'C:/Users/O''Brien/")));
    TW_CHECK_EQ(quoted.count(QLatin1Char{'\''}), 4);  // open, doubled pair, close
}

// ---------------------------------------------------------------------------
// Persistence: the part a user notices only when it is missing
// ---------------------------------------------------------------------------

void test_splitter_positions_survive_a_restart() {
    announce("splitter persistence");
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    QSettings::setPath(QSettings::IniFormat, QSettings::UserScope, dir.path());

    QList<int> savedMain;
    QList<int> savedColumn;
    QList<int> defaultMain;

    {
        const std::unique_ptr<MainWindow> first = shownWindow();
        defaultMain = first->mainSplitter()->sizes();

        // Move the dividers the way a user would. Both windows below are the same
        // size, so a saved pixel layout is comparable rather than merely similar.
        first->mainSplitter()->setSizes({400, 800});
        first->editorColumnSplitter()->setSizes({300, 120});
        QApplication::processEvents();

        savedMain = first->mainSplitter()->sizes();
        savedColumn = first->editorColumnSplitter()->sizes();
        // The move has to be a real change, or the round trip below would pass
        // with the defaults left in place.
        TW_CHECK(savedMain.at(0) != defaultMain.at(0));
        first->saveLayout();
    }

    {
        const std::unique_ptr<MainWindow> second = restoredWindow();
        const QList<int> main = second->mainSplitter()->sizes();
        const QList<int> column = second->editorColumnSplitter()->sizes();

        // The Python persists window geometry and dock state but not split
        // positions, so a dragged divider is forgotten on every restart. These
        // assertions are what fail if that block is removed from
        // `saveLayout()` / `restoreLayout()`.
        //
        // Compared as proportions, not pixels. Splitter state is *defined* as
        // proportions, and the window is not guaranteed to come back the same
        // size: `restoreGeometry()` fits a stored geometry to the available
        // screen, and the offscreen platform's virtual screen is 800x800, so a
        // 1200x800 window legitimately returns as 900x774. Asserting the pixels
        // would be asserting that the test environment has a large display.
        TW_CHECK(std::abs(fraction(second->mainSplitter(), 0) -
                          static_cast<double>(savedMain.at(0)) /
                              (savedMain.at(0) + savedMain.at(1))) < 0.01);
        TW_CHECK(std::abs(fraction(second->editorColumnSplitter(), 0) -
                          static_cast<double>(savedColumn.at(0)) /
                              (savedColumn.at(0) + savedColumn.at(1))) < 0.02);
        // And the REPL bar, being small and height-driven, survives verbatim.
        TW_CHECK(std::abs(column.at(1) - savedColumn.at(1)) <= 8);
    }

    // A settings file with no splitter state -- a first run, or one written by an
    // older build -- must leave the layout alone rather than wedge a pane to
    // zero, which is the failure mode a blind `restoreState()` would produce.
    QSettings().clear();
    {
        const std::unique_ptr<MainWindow> fresh = restoredWindow();
        // With nothing stored, `restoreLayout()` must leave the defaults exactly
        // as they are rather than wedge a pane to zero -- which is what a blind
        // `restoreState()` on an empty array would produce.
        TW_CHECK(std::abs(fraction(fresh->mainSplitter(), 0) - 0.6) < 0.05);
        TW_CHECK(fresh->immediateMode()->height() >= 20);
    }
}

void test_left_column_gets_the_larger_share_on_growth() {
    announce("resize behaviour");
    const std::unique_ptr<MainWindow> window = shownWindow(1000, 700);

    const QList<int> before = window->mainSplitter()->sizes();
    // The default split is exactly 3:2 -- 720:480 -- which is what the stretch
    // factors encode. Growth must preserve that ratio: with equal stretch the
    // canvas would gain as fast as the code and the ratio would drift to 1:1.
    TW_CHECK(std::abs(static_cast<double>(before.at(0)) / before.at(1) - 1.5) < 0.05);

    window->resize(1800, 700);
    QApplication::processEvents();
    QApplication::processEvents();
    const QList<int> after = window->mainSplitter()->sizes();

    // Whether the offscreen platform actually delivers the resize is not
    // something this test should depend on, so the growth itself is a soft
    // assertion and the ratio is the real one.
    TW_CHECK(after.at(0) >= before.at(0));
    if (after.at(0) > before.at(0)) {
        TW_CHECK(std::abs(static_cast<double>(after.at(0)) / after.at(1) - 1.5) < 0.2);
    }

    // And the REPL bar must not have crept open: it holds its height while the
    // editor takes the vertical slack.
    TW_CHECK(window->immediateMode()->height() <= 80);
}

}  // namespace


void test_project_panel_shows_groups_and_files() {
    announce("project panel");
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    MainWindow window;
    ProjectPanel* panel = window.projectPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    TW_CHECK_EQ(panel->objectName(), QStringLiteral("projectPanel"));
    QDockWidget* dock = window.findChild<QDockWidget*>(QStringLiteral("projectDock"));
    TW_CHECK(dock != nullptr);
    if (dock != nullptr) {
        TW_CHECK(dock->widget() == panel);
        TW_CHECK_EQ(window.dockWidgetArea(dock), Qt::LeftDockWidgetArea);
    }

    // The model is the source of truth; the panel is its view.
    const QString root = dir.path();
    const QString first = QDir{root}.filePath(QStringLiteral("one.bas"));
    const QString second = QDir{root}.filePath(QStringLiteral("two.logo"));
    {
        QFile file{first};
        static_cast<void>(file.open(QIODevice::WriteOnly));
    }
    {
        QFile file{second};
        static_cast<void>(file.open(QIODevice::WriteOnly));
    }
    timewarp::project::Project project =
        timewarp::project::Project::create(QStringLiteral("Class"), root);
    project.addGroup(QStringLiteral("Art"));
    project.addFile(first, QStringLiteral("Art"));
    project.addFile(second);
    panel->setProject(project);

    TW_CHECK_EQ(panel->projectName(), QStringLiteral("Class"));
    TW_CHECK_EQ(panel->fileCount(), 2);
    TW_CHECK_EQ(panel->groupNames(), QStringList({QStringLiteral("Art")}));
    TW_CHECK(panel->hasFile(first));
    TW_CHECK_EQ(panel->groupOf(first), QStringLiteral("Art"));
    TW_CHECK_EQ(panel->groupOf(second), QString{});
    TW_CHECK(panel->tree()->topLevelItemCount() >= 2);
    panel->selectFile(first);
    TW_CHECK(panel->tree()->currentItem() != nullptr);

    panel->clearProject();
    TW_CHECK_EQ(panel->fileCount(), 0);
    TW_CHECK_EQ(panel->projectName(), QString{});
}

void test_watch_and_data_breakpoint_controls() {
    announce("watch and data breakpoint controls");
    // The panel restores the session-wide lists; a value written by an earlier
    // test would otherwise make "the list I just created" wrong.
    QSettings{}.remove(QStringLiteral("debug/watches"));
    QSettings{}.remove(QStringLiteral("debug/dataBreakpoints"));
    MainWindow window;
    DebugPanel* panel = window.debugPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    for (const char* name : {"debugWatchInput", "debugAddWatch", "debugRemoveWatch",
                             "debugWatches", "debugDataBreakpointInput",
                             "debugAddDataBreakpoint", "debugRemoveDataBreakpoint",
                             "debugDataBreakpoints", "debugPauseReason"}) {
        TW_CHECK(panel->findChild<QWidget*>(QString::fromLatin1(name)) != nullptr);
    }

    QStringList announced;
    QObject::connect(panel, &DebugPanel::watchesChanged,
                     [&announced](const QStringList& expressions) {
                         announced = expressions;
                     });
    auto* watch_input = panel->findChild<QLineEdit*>(QStringLiteral("debugWatchInput"));
    auto* watch_add = panel->findChild<QPushButton*>(QStringLiteral("debugAddWatch"));
    TW_CHECK(watch_input != nullptr);
    TW_CHECK(watch_add != nullptr);
    if (watch_input != nullptr && watch_add != nullptr) {
        watch_input->setText(QStringLiteral("X * 2"));
        watch_add->click();
        TW_CHECK_EQ(panel->watches(), QStringList({QStringLiteral("X * 2")}));
        TW_CHECK_EQ(announced, QStringList({QStringLiteral("X * 2")}));
    }
    panel->setWatchValues({DebugPanel::WatchRow{QStringLiteral("X * 2"),
                                                QStringLiteral("10"), true, QString{}}});
    auto* table = panel->findChild<QTableWidget*>(QStringLiteral("debugWatches"));
    TW_CHECK(table != nullptr);
    if (table != nullptr) {
        TW_CHECK_EQ(table->rowCount(), 1);
        TW_CHECK_EQ(table->item(0, 1)->text(), QStringLiteral("10"));
    }

    QStringList watched;
    QObject::connect(panel, &DebugPanel::dataBreakpointsChanged,
                     [&watched](const QStringList& names) { watched = names; });
    auto* data_input =
        panel->findChild<QLineEdit*>(QStringLiteral("debugDataBreakpointInput"));
    auto* data_add =
        panel->findChild<QPushButton*>(QStringLiteral("debugAddDataBreakpoint"));
    TW_CHECK(data_input != nullptr);
    TW_CHECK(data_add != nullptr);
    if (data_input != nullptr && data_add != nullptr) {
        data_input->setText(QStringLiteral("TOTAL"));
        data_add->click();
        TW_CHECK_EQ(panel->dataBreakpoints(), QStringList({QStringLiteral("TOTAL")}));
        TW_CHECK_EQ(watched, QStringList({QStringLiteral("TOTAL")}));
    }
    panel->setDataBreakpoints({QStringLiteral("X")});
    TW_CHECK_EQ(panel->dataBreakpoints(), QStringList({QStringLiteral("X")}));
    panel->setPauseReason(QStringLiteral("X changed"));
    auto* reason = panel->findChild<QLabel*>(QStringLiteral("debugPauseReason"));
    TW_CHECK(reason != nullptr);
    if (reason != nullptr) {
        TW_CHECK_EQ(reason->text(), QStringLiteral("X changed"));
    }
}

void test_sql_panel_runs_and_shows_results() {
    announce("SQL panel");
    MainWindow window;
    SqlPanel* panel = window.sqlPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    TW_CHECK_EQ(panel->objectName(), QStringLiteral("sqlPanel"));
    TW_CHECK(window.showPanel(QStringLiteral("sql")));
    TW_CHECK(window.rightTabs()->widget(6) == panel);

    panel->setDatabase(QStringLiteral(":memory:"), {QStringLiteral("scores")});
    TW_CHECK_EQ(panel->tableCount(), 1);
    panel->setQuery(QStringLiteral("SELECT * FROM scores;"));
    TW_CHECK_EQ(panel->query(), QStringLiteral("SELECT * FROM scores;"));

    QString submitted;
    QObject::connect(panel, &SqlPanel::runRequested,
                     [&submitted](const QString& sql) { submitted = sql; });
    auto* run = panel->findChild<QPushButton*>(QStringLiteral("sqlRun"));
    TW_CHECK(run != nullptr);
    if (run != nullptr) {
        run->click();
        TW_CHECK_EQ(submitted, QStringLiteral("SELECT * FROM scores;"));
    }

    timewarp::sql::QueryResult result;
    result.columns = QStringList{QStringLiteral("name"), QStringLiteral("score")};
    result.rows = {{QStringLiteral("Ada"), QStringLiteral("42")}};
    panel->setResult(result);
    TW_CHECK_EQ(panel->resultRowCount(), 1);
    TW_CHECK_EQ(panel->resultColumnCount(), 2);
    TW_CHECK(panel->statusText().contains(QStringLiteral("1 row")));

    timewarp::sql::QueryResult failure;
    failure.error = QStringLiteral("no such table: nope");
    panel->setResult(failure);
    TW_CHECK(panel->statusText().contains(QStringLiteral("no such table")));
}

void test_repl_bar_runs_and_keeps_bindings() {
    announce("REPL bar");
    const std::unique_ptr<MainWindow> window = shownWindow();
    ImmediateModePanel* panel = window->immediateMode();
    auto* console = window->findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    TW_CHECK(panel != nullptr);
    TW_CHECK(console != nullptr);
    if (panel == nullptr || console == nullptr) {
        return;
    }

    emit panel->commandSubmitted(QStringLiteral("PRINT 2 + 2"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("4"));
    }));
    TW_CHECK(console->toPlainText().contains(QStringLiteral("REPL")));
    // The bar is cleared as soon as the command is handed over, so the next
    // command starts from an empty field rather than appending to the last one.
    TW_CHECK(panel->command().isEmpty());

    emit panel->commandSubmitted(QStringLiteral("LET X = 7"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("REPL \u203A LET X = 7"));
    }));
    emit panel->commandSubmitted(QStringLiteral("PRINT X"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().count(QStringLiteral("7")) >= 2;
    }));

    // BASIC's NEW clears the workspace rather than being an unknown command.
    emit panel->commandSubmitted(QStringLiteral("new"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("workspace cleared"));
    }));
    const QString before = console->toPlainText();
    emit panel->commandSubmitted(QStringLiteral("PRINT X"));
    TW_CHECK(spin_until([console, before] {
        return console->toPlainText().mid(before.size()).contains(QStringLiteral("0"));
    }));
}

void test_repl_basic_direct_mode() {
    announce("REPL bar: BASIC direct mode");
    const std::unique_ptr<MainWindow> window = shownWindow();
    ImmediateModePanel* panel = window->immediateMode();
    auto* console = window->findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    CodeEditor* editor = window->currentEditor();
    TW_CHECK(panel != nullptr && console != nullptr && editor != nullptr);
    if (panel == nullptr || console == nullptr || editor == nullptr) {
        return;
    }
    editor->setPlainText(QString{});
    editor->setLanguage(Language::Basic);

    emit panel->commandSubmitted(QStringLiteral("20 PRINT \"TWO\""));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("Line 20 stored"));
    }));
    emit panel->commandSubmitted(QStringLiteral("10 PRINT \"ONE\""));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("Line 10 stored"));
    }));
    emit panel->commandSubmitted(QStringLiteral("15 ? 6 * 7"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("Line 15 stored"));
    }));
    TW_CHECK_EQ(editor->toPlainText(),
                QStringLiteral("10 PRINT \"ONE\"\n15 PRINT 6 * 7\n20 PRINT \"TWO\"\n"));

    // The same number replaces the line instead of duplicating it.
    emit panel->commandSubmitted(QStringLiteral("10 PRINT \"REPLACED\""));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("Line 10 stored"));
    }));
    TW_CHECK_EQ(editor->toPlainText(),
                QStringLiteral("10 PRINT \"REPLACED\"\n15 PRINT 6 * 7\n20 PRINT \"TWO\"\n"));

    // `?` is PRINT in immediate mode too.
    emit panel->commandSubmitted(QStringLiteral("? 6 * 7"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("42"));
    }));

    const QString before_list = console->toPlainText();
    emit panel->commandSubmitted(QStringLiteral("LIST"));
    TW_CHECK(spin_until([console, before_list] {
        return console->toPlainText().mid(before_list.size()).contains(
            QStringLiteral("10 PRINT \"REPLACED\""));
    }));

    // RUN executes the program that the numbered entries built.
    emit panel->commandSubmitted(QStringLiteral("RUN"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("REPLACED")) &&
               console->toPlainText().contains(QStringLiteral("TWO"));
    }));

    // NEW clears both the document and the REPL bindings.
    emit panel->commandSubmitted(QStringLiteral("NEW"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("workspace cleared"));
    }));
    TW_CHECK(editor->toPlainText().isEmpty());
}

void test_repl_bar_scheme_errors_and_language_switch() {
    announce("REPL bar: Scheme, errors and language switch");
    const std::unique_ptr<MainWindow> window = shownWindow();
    ImmediateModePanel* panel = window->immediateMode();
    auto* console = window->findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    CodeEditor* editor = window->currentEditor();
    auto* selector = window->findChild<QComboBox*>(QStringLiteral("languageSelector"));
    TW_CHECK(panel != nullptr && console != nullptr && editor != nullptr &&
             selector != nullptr);
    if (panel == nullptr || console == nullptr || editor == nullptr ||
        selector == nullptr) {
        return;
    }
    const auto choose = [selector](const QString& key) {
        const int index = selector->findData(key);
        TW_CHECK(index >= 0);
        selector->setCurrentIndex(index);
    };

    choose(QStringLiteral("scheme"));
    emit panel->commandSubmitted(QStringLiteral("(define y 4)"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("REPL \u203A (define y 4)"));
    }));
    emit panel->commandSubmitted(QStringLiteral("(* y 5)"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("20"));
    }));

    // A reader error is shown, not swallowed.
    emit panel->commandSubmitted(QStringLiteral("("));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("\u274C"));
    }));

    // Switching the document's language swaps the REPL engine rather than
    // sending Logo a Scheme form.
    choose(QStringLiteral("logo"));
    const QString before = console->toPlainText();
    emit panel->commandSubmitted(QStringLiteral("PRINT 2 + 2"));
    TW_CHECK(spin_until([console, before] {
        return console->toPlainText().mid(before.size()).contains(QStringLiteral("4"));
    }));
    TW_CHECK(console->toPlainText().mid(before.size()).contains(
        QStringLiteral("REPL \u203A PRINT 2 + 2")));
}

int main(int argc, char** argv) {
    // Set before QApplication exists, or Qt tries to open a real display.
    qputenv("QT_QPA_PLATFORM", "offscreen");
    QApplication app{argc, argv};
    // Isolate QSettings from the developer's real configuration.
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpTests"));
    QCoreApplication::setApplicationName(QStringLiteral("UiLayoutTests"));
    QSettings::setDefaultFormat(QSettings::IniFormat);

    // The snippet catalogue is read through the resolver, which walks up from
    // the executable: that reaches the checkout from `native/build` and from
    // nowhere else. Where the walk finds nothing, the path CMake recorded is
    // supplied through the resolver's own override.
    if (timewarp::ui::snippetRoot().isEmpty()) {
        qputenv("TIME_WARP_SNIPPETS", QStringLiteral(TIME_WARP_TEST_SNIPPETS).toUtf8());
    }

    test_editor_tabs_refuse_to_be_sized_by_their_largest_page();
    test_enhanced_tab_bar_is_idempotent();

    test_window_frame_properties();
    test_central_column_nesting();
    test_main_splitter_geometry_and_behaviour();
    test_editor_column_is_editor_over_repl();
    test_editor_pane_splitter_holds_two_tab_groups();
    test_right_column_tabs_and_order();
    test_lesson_panel_contract();
    test_the_lesson_panel_loads_the_curriculum_without_a_bridge();
    test_lesson_panel_records_the_session();
    test_classroom_panel_contract();
    test_variables_panel_contract();
    test_telemetry_panel_contract();
    test_debug_panel_controls();
    test_output_and_graphics_share_a_persistent_split();
    test_run_both_panes_compares_them();
    test_the_split_editor_moves_documents_between_panes();
    test_project_panel_shows_groups_and_files();
    test_watch_and_data_breakpoint_controls();
    test_sql_panel_runs_and_shows_results();
    test_repl_bar_runs_and_keeps_bindings();
    test_repl_bar_scheme_errors_and_language_switch();
    test_repl_basic_direct_mode();
    test_the_outline_lists_and_jumps();
    test_the_scrubber_appears_only_when_there_is_ink();
    test_turtle_command_log_spells_each_command();
    test_turtle_log_and_inspector_follow_the_canvas();
    test_output_sub_tabs();
    test_function_key_bar_is_fixed_height();
    test_function_key_layers();
    test_function_keys_are_wired();
    test_themes();
    test_high_contrast_clears_a_seven_to_one_floor();
    test_high_contrast_toggles_and_restores_the_palette();
    test_font_scale_ladder();
    test_focus_mode_hides_and_restores_the_chrome();
    test_presentation_mode_is_focus_mode_for_a_room();
    test_gif_recording_toggles_and_samples_the_canvas();
    test_command_palette();
    test_standalone_payload();
    test_standalone_player_layout();
    test_tool_bar_configuration();
    test_status_bar_badge_placement();
    test_outline_dock_starts_hidden();
    test_menu_structure();
    test_print_entries_follow_the_component();
    test_help_menu_opens_the_documentation();
    test_edit_menu_has_find_in_files();
    test_snippet_picker_filters_by_language();
    test_version_history_lists_snapshots();
    test_editor_minimap_and_whitespace();
    test_every_right_column_page_has_a_key();
    test_file_menu_new_opens_a_document();
    test_tools_menu_opens_one_emulator();
    test_the_guided_tour_points_at_real_widgets();
    test_the_guided_tour_is_seen_once_and_rearmable();
    test_the_guided_tour_walks_its_steps();
    test_coach_marks_appear_once();
    test_help_menu_opens_the_tour();
    test_settings_rearms_the_tour();
    test_settings_toggle_the_terminal();
    test_file_menu_close_tab_closes_one();
    test_file_menu_lists_the_installed_examples();
    test_tab_close_button_closes_the_tab();
    test_view_menu_grid_drives_the_canvas();
    test_view_menu_zoom_scales_the_canvas();
    test_profile_run_is_opt_in();
    test_input_round_trip_reaches_the_runner();
    test_a_program_screen_reaches_the_canvas();
    test_profile_report_lists_the_measurements();
    test_run_menu_clear_output_empties_the_console();
    test_edit_menu_acts_on_whatever_has_focus();
    test_run_button_popup_is_not_empty();
    test_no_enabled_control_is_inert();
    test_unported_controls_say_so_instead_of_doing_nothing();
    test_crt_overlay_tracks_its_target();
    test_crt_settings_reach_the_overlay();
    test_screen_presets_pair_a_theme_with_its_crt_values();
    test_immediate_mode_panel_contract();

    test_windows_sound_command_line();
    test_splitter_positions_survive_a_restart();
    test_left_column_gets_the_larger_share_on_growth();
    return tw::test::summary("ui_layout");
}
