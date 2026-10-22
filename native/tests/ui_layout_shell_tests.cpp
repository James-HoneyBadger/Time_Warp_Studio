// SPDX-License-Identifier: MIT
//
// The shell: the tab widget, the window's frame, its splitters and its tabs.

#include "ui_layout/support.hpp"

namespace tw::ui_layout {

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

}  // namespace tw::ui_layout
