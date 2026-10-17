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

#include <cmath>
#include <cstddef>
#include <cstdio>
#include <memory>
#include <string>

#include <QAction>
#include <QApplication>
#include <QComboBox>
#include <QDockWidget>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QProgressBar>
#include <QTextBrowser>
#include <QMargins>
#include <QMenu>
#include <QMenuBar>
#include <QMetaMethod>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QSettings>
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
#include <QVBoxLayout>
#include <QVBoxLayout>
#include <QWidgetAction>

#include "check.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/find_bar.hpp"
#include "timewarp/ui/main_window.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/turtle_canvas.hpp"
#include "timewarp/ui/resizable_tab_widget.hpp"

namespace {

using timewarp::ui::CrtOverlay;
using timewarp::ui::FunctionKeyBar;
using timewarp::ui::ImmediateModePanel;
using timewarp::ui::DebugPanel;
using timewarp::ui::LessonPanel;
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

    const QList<timewarp::bridge::VariableRow> rows{
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

void test_debug_panel_is_scheme_only() {
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

    // Whether it can step is a property of the language, and the window turns it
    // on for Scheme and off -- with a reason -- for everything else. The controls
    // are disabled with it, so a breakpoint cannot be set for a language that
    // will never stop at one.
    auto* combo = window->findChild<QComboBox*>(QStringLiteral("languageSelector"));
    TW_CHECK(combo != nullptr);
    if (combo != nullptr) {
        const int scheme = combo->findData(QStringLiteral("scheme"));
        const int basic = combo->findData(QStringLiteral("basic"));
        TW_CHECK(scheme >= 0);
        TW_CHECK(basic >= 0);
        if (scheme >= 0 && basic >= 0) {
            combo->setCurrentIndex(basic);
            TW_CHECK(!panel->isAvailable());
            TW_CHECK(add != nullptr && !add->isEnabled());
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
    TW_CHECK_EQ(tabs->count(), 5);
    // Order is part of the interface: users reach for Output first.
    TW_CHECK_EQ(tabTitles(tabs).toStdString(),
                std::string{"\U0001F4DD Output  \U0001F3A8 Graphics|"
                            "\U0001F50D Variables|\u26A1 Telemetry HUD|"
                            "\U0001F41B Debug|\U0001F393 Lessons"});
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
    TW_CHECK_EQ(pane->widget(1)->objectName(), QStringLiteral("turtleCanvas"));
    // 200:300 -- the canvas gets the larger share of the right column, which is
    // the point of showing both at once rather than flipping between them.
    TW_CHECK(std::abs(fraction(pane, 1) - 0.6) < 0.08);
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
    TW_CHECK_EQ(tabs->count(), 4);
    TW_CHECK_EQ(tabTitles(tabs).toStdString(),
                std::string{"\U0001F4DD Console|\u274C Errors|\U0001F422 Turtle Log|"
                            "\U0001F4BB Terminal"});

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
}

void test_function_key_bar_is_fixed_height() {
    announce("function key bar");
    MainWindow window;
    FunctionKeyBar* bar = window.functionKeyBar();

    TW_CHECK_EQ(bar->objectName(), QStringLiteral("functionKeyBar"));
    // Both bounds, because a bar that only sets a minimum would still eat slack.
    TW_CHECK_EQ(bar->minimumHeight(), FunctionKeyBar::kFixedHeight);
    TW_CHECK_EQ(bar->maximumHeight(), FunctionKeyBar::kFixedHeight);
    TW_CHECK_EQ(bar->height(), FunctionKeyBar::kFixedHeight);

    // F1..F10 plus the deck selector.
    const int buttons = static_cast<int>(bar->findChildren<QPushButton*>().size());
    TW_CHECK_EQ(buttons, 10);
    TW_CHECK(bar->findChild<QComboBox*>(QStringLiteral("deckSelector")) != nullptr);
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
    TW_CHECK_EQ(bar->minimumHeight(), 44);

    // Two rows, because one row of thirteen controls does not fit a 1400px window
    // and Qt hides the overflow behind a chevron rather than making it reachable.
    TW_CHECK_EQ(display->objectName(), QStringLiteral("secondaryToolBar"));
    TW_CHECK(!display->isMovable());
    TW_CHECK_EQ(display->minimumHeight(), 44);

    // Run carries a menu of variants; the others are plain actions.
    auto* run = bar->findChild<QToolButton*>(QStringLiteral("runButton"));
    TW_CHECK(run != nullptr);
    if (run != nullptr) {
        TW_CHECK(run->menu() != nullptr);
        TW_CHECK_EQ(run->popupMode(), QToolButton::MenuButtonPopup);
    }
    TW_CHECK(bar->findChild<QComboBox*>(QStringLiteral("languageSelector")) != nullptr);
    // The display controls belong to the second row, and only to the second row:
    // a slider found on the first row would mean the split had not happened.
    TW_CHECK(bar->findChild<QSlider*>(QStringLiteral("turtleSpeedSlider")) == nullptr);
    TW_CHECK(display->findChild<QSlider*>(QStringLiteral("turtleSpeedSlider")) != nullptr);
    TW_CHECK(display->findChild<QLabel*>(QStringLiteral("turtleSpeedValue")) != nullptr);

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
    // Debug and Tools are absent on purpose: a menu with nothing to put in it is
    // a promise the window cannot keep. They come back with the debugger panel
    // and the snippet library.
    TW_CHECK_EQ(titles.join(QLatin1Char('|')).toStdString(),
                std::string{"&File|&Edit|&Run|&View|&Help"});

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
    announce("unported controls are disabled");
    MainWindow window;

    // Each of these is laid out where the finished IDE will have it. Until its
    // feature lands it must be visibly unavailable and say why, because a button
    // that silently does nothing is what the user reports as broken.
    const QStringList placeholders{QStringLiteral("Debug"), QStringLiteral("Continue"),
                                   QStringLiteral("Trace"), QStringLiteral("Snippets"),
                                   QStringLiteral("A+")};
    const QList<QToolBar*> bars{window.mainToolBar(), window.secondaryToolBar()};

    for (const QString& name : placeholders) {
        QAction* found = nullptr;
        for (QToolBar* bar : bars) {
            for (QAction* action : bar->actions()) {
                if (action->text().contains(name)) {
                    found = action;
                    break;
                }
            }
            if (found != nullptr) {
                break;
            }
        }
        TW_CHECK(found != nullptr);
        if (found != nullptr) {
            TW_CHECK(!found->isEnabled());
            TW_CHECK(found->toolTip().contains(QStringLiteral("not ported")));
        }
    }

    // The speed slider is a widget rather than an action, and it delays nothing
    // until drawings animate.
    auto* speed = window.findChild<QSlider*>(QStringLiteral("turtleSpeedSlider"));
    TW_CHECK(speed != nullptr);
    if (speed != nullptr) {
        TW_CHECK(!speed->isEnabled());
        TW_CHECK(speed->toolTip().contains(QStringLiteral("not ported")));
    }
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
        TW_CHECK_EQ(panel->command(), QStringLiteral("PRINT 2 + 2"));

        // An empty command is not a command.
        received.clear();
        input->setText(QStringLiteral("   "));
        emit input->returnPressed();
        TW_CHECK(received.isEmpty());
    }
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

int main(int argc, char** argv) {
    // Set before QApplication exists, or Qt tries to open a real display.
    qputenv("QT_QPA_PLATFORM", "offscreen");
    QApplication app{argc, argv};
    // Isolate QSettings from the developer's real configuration.
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpTests"));
    QCoreApplication::setApplicationName(QStringLiteral("UiLayoutTests"));
    QSettings::setDefaultFormat(QSettings::IniFormat);

    test_editor_tabs_refuse_to_be_sized_by_their_largest_page();
    test_enhanced_tab_bar_is_idempotent();

    test_window_frame_properties();
    test_central_column_nesting();
    test_main_splitter_geometry_and_behaviour();
    test_editor_column_is_editor_over_repl();
    test_editor_pane_splitter_holds_two_tab_groups();
    test_right_column_tabs_and_order();
    test_lesson_panel_contract();
    test_variables_panel_contract();
    test_telemetry_panel_contract();
    test_debug_panel_is_scheme_only();
    test_output_and_graphics_share_a_persistent_split();
    test_output_sub_tabs();
    test_function_key_bar_is_fixed_height();
    test_tool_bar_configuration();
    test_status_bar_badge_placement();
    test_outline_dock_starts_hidden();
    test_menu_structure();
    test_file_menu_new_opens_a_document();
    test_file_menu_close_tab_closes_one();
    test_view_menu_grid_drives_the_canvas();
    test_view_menu_zoom_scales_the_canvas();
    test_run_menu_clear_output_empties_the_console();
    test_edit_menu_acts_on_whatever_has_focus();
    test_run_button_popup_is_not_empty();
    test_no_enabled_control_is_inert();
    test_unported_controls_say_so_instead_of_doing_nothing();
    test_crt_overlay_tracks_its_target();
    test_immediate_mode_panel_contract();

    test_splitter_positions_survive_a_restart();
    test_left_column_gets_the_larger_share_on_growth();
    return tw::test::summary("ui_layout");
}
