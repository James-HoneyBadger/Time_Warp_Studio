// SPDX-License-Identifier: MIT
//
// The menus, the tool bar, the status bar, the tour, and the controls that must
// not be inert.

#include "ui_layout/support.hpp"

namespace tw::ui_layout {

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

}  // namespace tw::ui_layout
