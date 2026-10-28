// SPDX-License-Identifier: MIT

#include "main_window_internal.hpp"

#include "timewarp/ui/command_reference.hpp"
#include "timewarp/ui/main_window.hpp"

#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/watch.hpp"
#include "timewarp/ui/native_runner.hpp"

#include <QDateTime>
#include <QDesktopServices>
#include <QDir>
#include <QFileInfo>
#include <QFont>
#include <QPlainTextEdit>
#ifdef TIME_WARP_HAVE_PRINT
#endif
#include <QSet>
#include <QSettings>
#include <QSplitter>
#include <QStatusBar>
#include <QTabWidget>
#include <QTimer>
#include <QUrl>
#include <QVBoxLayout>

#include <memory>
#include <string>
#include <vector>

#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/canvas/host_adapter.hpp"
#include "timewarp/canvas/qt_bridge.hpp"
#include "timewarp/ui/app.hpp"
#include "timewarp/ui/app_paths.hpp"
#include "timewarp/ui/benchmark.hpp"
#include "timewarp/ui/breakpoints.hpp"
#include "timewarp/ui/chip8_window.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/classroom_marker.hpp"
#include "timewarp/ui/command_palette.hpp"
#include "timewarp/ui/export.hpp"
#include "timewarp/ui/find_bar.hpp"
#include "timewarp/ui/find_in_files.hpp"
#include "timewarp/ui/formatter.hpp"
#include "timewarp/ui/help_browser.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/preferences.hpp"
#include "timewarp/ui/profile_report.hpp"
#include "timewarp/ui/project_panel.hpp"
#include "timewarp/ui/retro_computer.hpp"
#include "timewarp/ui/repl_runner.hpp"
#include "timewarp/ui/sql_panel.hpp"
#include "timewarp/sql/database.hpp"
#include "timewarp/ui/resizable_tab_widget.hpp"
#include "timewarp/ui/scrubber.hpp"
#include "timewarp/ui/snippet_dialog.hpp"
#include "timewarp/ui/snippets.hpp"
#include "timewarp/ui/sound_player.hpp"
#include "timewarp/ui/tour.hpp"
#include "timewarp/ui/syntax_highlighter.hpp"
#include "timewarp/ui/terminal_widget.hpp"
#include "timewarp/ui/themes.hpp"
#include "timewarp/ui/turtle_canvas.hpp"
#include "timewarp/ui/version_history.hpp"
#include "timewarp/ui/ui_style.hpp"

// Supplied by CMake from the repository's VERSION file, so the window title and
// the packaged version cannot drift apart. The fallback keeps a standalone
// compile of this file working.
#ifndef TIME_WARP_VERSION
#define TIME_WARP_VERSION "0.0.0"
#endif

namespace timewarp::ui {

// The settings keys and the small helpers the sections share; they were an
// anonymous namespace in the one-file version, and a `using` is what keeps the call
// sites reading the same.
using namespace detail;

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
    // Before `buildUi`, because the first document is adopted while the tree is
    // built and its edits have to reach a timer that already exists.
    outline_timer_ = new QTimer{this};
    outline_timer_->setObjectName(QStringLiteral("outlineTimer"));
    outline_timer_->setSingleShot(true);
    // The linter's delay: long enough that typing does not re-parse the program on
    // every keystroke, short enough that the outline is not visibly stale.
    outline_timer_->setInterval(350);
    connect(outline_timer_, &QTimer::timeout, this, &MainWindow::refreshOutline);
    // Added last so the overlay sits above the central layout, so it has to be
    // created after the tree it covers exists.
    buildUi();
    // The tour points at widgets built above; the overlay is a child of this
    // window and is created after the tree it dims. Step 2 is the lessons panel,
    // which has to be raised before the overlay can measure a visible anchor.
    tour_ = new TourGuide{this, this};
    connect(tour_, &TourGuide::stepShown, this, [this](int index) {
        if (index == 2) {
            static_cast<void>(showPanel(QStringLiteral("lessons")));
        }
    });
    // After the widget tree and before `restoreLayout()`/`show()`: a palette
    // applied to a visible window repaints it, which shows as a flash of the
    // default colours before the user's theme arrives.
    applyTheme(QSettings{}.value(kSettingTheme, themes::defaultName()).toString());

    // Autosave: a timer the settings control, and one line in the status bar if a
    // previous session left unsaved work behind. Deliberately not a dialog --
    // nothing modal runs on a startup path in this application, and the recovery
    // itself lives in File > Version History where the user can find it when they
    // want it.
    autosave_timer_ = new QTimer{this};
    autosave_timer_->setObjectName(QStringLiteral("autosaveTimer"));
    connect(autosave_timer_, &QTimer::timeout, this, &MainWindow::autosaveNow);

    // The canvas recorder. A fixed sample rate rather than a signal per segment:
    // a fast turtle can draw thousands of segments a second, and a GIF with a
    // frame per segment is a file nobody can open. Ten frames a second is the
    // rate the removed implementation used and is enough for a drawing.
    gif_timer_ = new QTimer{this};
    gif_timer_->setObjectName(QStringLiteral("gifRecordTimer"));
    gif_timer_->setInterval(100);
    connect(gif_timer_, &QTimer::timeout, this, &MainWindow::captureGifFrame);
    applyEditorSettings(editor_settings_);
    // Before any document is opened, so the settings file cannot keep growing a
    // dead path per deleted program. The count is not reported: who used to have a
    // breakpoint in a file they have since deleted is not something a learner
    // needs a dialog about.
    QSettings breakpoint_settings;
    static_cast<void>(breakpoints::prune(breakpoint_settings));
    reportRecoveredWork();

    // Last: the tab is added to the output column built above, and only when a
    // previous session asked for it.
    terminal_enabled_ = QSettings{}.value(kSettingTerminalEnabled, false).toBool();
    refreshTerminalTab();
}

void MainWindow::autosaveNow() {
    if (!editor_settings_.autosave || editor_tabs_ == nullptr) {
        return;
    }
    const qint64 now = QDateTime::currentMSecsSinceEpoch();
    for (int index = 0; index < editor_tabs_->count(); ++index) {
        auto* editor = qobject_cast<CodeEditor*>(editor_tabs_->widget(index));
        // Only documents with unsaved changes. Snapshotting everything would push
        // the useful copies out of the five kept, which is the opposite of the
        // point.
        if (editor == nullptr || !editor->document()->isModified()) {
            continue;
        }
        QString document = pathOf(editor);
        if (document.isEmpty()) {
            // An untitled buffer is exactly the case this exists for: there is no
            // file to lose, so there is nothing to recover from except this.
            document = QStringLiteral("untitled:") + nameOf(editor);
        }
        // A failed snapshot means the data directory is not writable. Saying so on
        // every interval would bury the message it is trying to deliver; the next
        // tick tries again, and Version History shows what did get written.
        static_cast<void>(autosave_.save(document, editor->toPlainText(), now));
    }
}

void MainWindow::reportRecoveredWork() {
    // One message per document, not per snapshot: three copies of one buffer is
    // still one thing to recover. `all()` is newest first, so the first entry
    // seen for a document is the one that decides whether the file has it.
    QSet<QString> seen;
    int count = 0;
    for (const Snapshot& snapshot : autosave_.all()) {
        if (seen.contains(snapshot.document)) {
            continue;
        }
        seen.insert(snapshot.document);
        bool newer_than_file = true;
        if (!snapshot.document.startsWith(QLatin1String("untitled:"))) {
            // A named document whose file is gone counts too: `on_disk` is then
            // zero, and the snapshot is the only copy left.
            const QFileInfo file{snapshot.document};
            const qint64 on_disk = file.exists() ? file.lastModified().toMSecsSinceEpoch() : 0;
            newer_than_file = snapshot.timestamp > on_disk;
        }
        if (newer_than_file) {
            ++count;
        }
    }
    if (count == 0) {
        return;
    }
    const QString message =
        count == 1
            ? tr("An unsaved copy from a previous session is available \u2014 "
                 "File > Version History")
            : tr("%1 unsaved copies from a previous session are available \u2014 "
                 "File > Version History")
                  .arg(count);
    // No timeout: this is the one startup message whose whole purpose is to be
    // read later, and five seconds is not long enough to notice a status bar.
    statusBar()->showMessage(message);
}

CodeEditor* MainWindow::openRestoredCopy(const QString& text) {
    CodeEditor* restored = newFile();
    if (restored == nullptr) {
        return nullptr;
    }
    restored->setPlainText(text);
    // Modified, so the new tab is visibly unsaved and closing it asks first: the
    // text came from the store, not from a file, and saying otherwise would let a
    // close discard the only copy.
    restored->document()->setModified(true);
    updateTabLabel(restored);
    return restored;
}

QString MainWindow::contextHelpTopic() const {
    const CodeEditor* editor = currentEditor();
    return editor == nullptr ? QString{} : guideTopic(editor->language());
}

void MainWindow::showContextHelp() {
    HelpBrowserDialog dialog{this};
    const QString topic = contextHelpTopic();
    if (!topic.isEmpty()) {
        static_cast<void>(dialog.openTopic(QDir{docsRoot()}.filePath(topic)));
    }
    // The word under the cursor is the command being asked about. A word the
    // guide does not contain leaves the page at the top rather than saying
    // nothing happened: the guide itself is the answer in that case.
    if (const CodeEditor* editor = currentEditor(); editor != nullptr) {
        static_cast<void>(dialog.findInPage(editor->wordAtCursor()));
    }
    dialog.exec();
}

void MainWindow::showKeywordInspector() {
    const CodeEditor* editor = currentEditor();
    const Language language =
        editor == nullptr ? defaultLanguage() : editor->language();
    KeywordInspectorDialog dialog{language == Language::Unknown ? defaultLanguage() : language,
                                  this};
    dialog.exec();
}

void MainWindow::refreshOutline() {
    if (outline_panel_ == nullptr) {
        return;
    }
    const CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        outline_panel_->setEntries({});
        return;
    }
    outline_panel_->setEntries(outlineFor(editor->language(), editor->toPlainText()));
}

void MainWindow::showDocumentation(const QString& topic) {
    HelpBrowserDialog dialog{this};
    // A named topic is opened after the index, so the contents tree still has a
    // selection and Home works. A missing file leaves the index on screen rather
    // than showing an empty pane.
    if (!topic.isEmpty()) {
        static_cast<void>(dialog.openTopic(QDir{docsRoot()}.filePath(topic)));
    }
    dialog.exec();
}

void MainWindow::showVersionHistory() {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        return;
    }
    QString document = pathOf(editor);
    if (document.isEmpty()) {
        document = QStringLiteral("untitled:") + nameOf(editor);
    }
    // The editor's text is what a copy is compared with: "restore this" is a
    // decision about the difference between the two.
    VersionHistoryDialog dialog{autosave_, document, editor->toPlainText(), this};
    if (dialog.exec() != QDialog::Accepted) {
        return;
    }
    const QString text = dialog.chosenText();
    if (text.isEmpty()) {
        return;
    }
    // A new, unnamed tab: writing the old text back over the file would make
    // "look at an older version" a destructive action.
    if (openRestoredCopy(text) == nullptr) {
        return;
    }
    setExecutionState(tr("ready"),
                      tr("Opened a saved copy \u2014 save it under a new name to keep it"));
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
    // The REPL bar is laid out with the editor but its engine only exists once
    // somebody types into it.
    if (immediate_mode_ != nullptr) {
        connect(immediate_mode_, &ImmediateModePanel::commandSubmitted, this,
                &MainWindow::submitReplCommand);
    }
    // The first document is created while the right column is still being built,
    // so its first refresh could not reach the debug panel. One refresh after
    // the tree exists puts every badge and panel in step with it.
    refreshDocumentState();
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
    // Loaded here rather than in `buildUi`: the overlay exists now, and a stale
    // settings file must not leave the effect at a value the dialog cannot show.
    setCrtSettings(loadCrtSettings());
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
    // The outline describes one document at a time, so a tab change is an
    // immediate refresh -- no debounce, because nothing was typed.
    connect(editor_tabs_, &QTabWidget::currentChanged, this,
            [this](int) { refreshOutline(); });

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
    connect(lesson_panel_, &LessonPanel::handInRequested, this,
            &MainWindow::handInLesson);
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

    // The teacher's page. It is empty in a student's copy of the application and
    // says so, because a class list is a file a teacher makes, not a setting the
    // program guesses at.
    classroom_panel_ = new ClassroomPanel{right_tabs_};
    right_tabs_->addTab(classroom_panel_, tr("\U0001F3EB Classroom"));
    buildSqlPanel();
    buildCommandReference();
#ifdef TIME_WARP_HAVE_AI
    buildAiPanel();
#endif
    // The reference opens on the document's language, every time it is raised: a
    // page that remembered the language somebody browsed last week would be a page
    // that answers a question they are not asking.
    connect(right_tabs_, &QTabWidget::currentChanged, this, [this](int index) {
        if (reference_panel_ == nullptr ||
            right_tabs_->widget(index) != static_cast<QWidget*>(reference_panel_)) {
            return;
        }
        if (const CodeEditor* editor = currentEditor()) {
            reference_panel_->setLanguage(languageOf(editor));
        } else {
            reference_panel_->ensureLoaded();
        }
    });
    connect(classroom_panel_, &ClassroomPanel::refreshRequested, this,
            &MainWindow::requestClassroom);
    connect(classroom_panel_, &ClassroomPanel::bundleRequested, this,
            &MainWindow::exportClassroomBundle);
    connect(classroom_panel_, &ClassroomPanel::unbundleRequested, this,
            &MainWindow::importClassroomBundle);
    connect(classroom_panel_, &ClassroomPanel::openFolderRequested, this,
            [this](const QString& path) {
                // A folder opened in the desktop's file manager is how a teacher
                // hands back a marked file. The program does not copy or rename
                // anything: the folder is the teacher's, and moving files in it
                // behind their back would be worse than doing nothing.
                if (!QDesktopServices::openUrl(QUrl::fromLocalFile(path))) {
                    statusBar()->showMessage(
                        tr("Could not open %1 — the folder is at that path.").arg(path), 8000);
                }
            });
    // Asked for on the first look, like the curriculum, and for the same reason.
    connect(right_tabs_, &QTabWidget::currentChanged, this, [this](int index) {
        if (classroom_requested_ || classroom_panel_ == nullptr) {
            return;
        }
        if (right_tabs_->widget(index) == classroom_panel_) {
            classroom_requested_ = true;
            requestClassroom();
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

    auto* turtle_log = new TurtleCommandLog{output_sub_tabs_};
    turtle_log_ = turtle_log;
    output_sub_tabs_->addTab(turtle_log, tr("\U0001F422 Turtle Log"));

    // The terminal tab is added by `refreshTerminalTab()`, which the constructor
    // and the Preferences checkbox drive: a build where nobody asked for a shell
    // should not have a tab that invites one.

    output_canvas_splitter_->addWidget(output_sub_tabs_);

    // The real canvas, not a stand-in. It consumes the command queue through
    // the bridge below, so a turtle program's output has somewhere to land.
    //
    // Canvas and scrubber share a pane so the playback strip is part of the
    // drawing surface: collapsing the canvas takes them together, and the
    // splitter still moves one thing.
    auto* canvas_pane = new QWidget{output_canvas_splitter_};
    canvas_pane->setObjectName(QStringLiteral("canvasPane"));
    auto* canvas_column = new QVBoxLayout{canvas_pane};
    canvas_column->setContentsMargins(0, 0, 0, 0);
    canvas_column->setSpacing(0);

    turtle_canvas_ = new TurtleCanvas{canvas_pane};
    canvas_column->addWidget(turtle_canvas_, 1);

    // The pose readout goes under the drawing rather than in a dock: an inspector
    // whose value changes while the turtle turns is only useful if it can be read
    // without looking away from the turtle.
    turtle_inspector_ = new TurtleInspector{canvas_pane};
    canvas_column->addWidget(turtle_inspector_);
    connect(turtle_canvas_, &TurtleCanvas::turtleStateChanged, this,
            [this] { refreshTurtleInspector(); });

    scrubber_ = new Scrubber{canvas_pane};
    // Hidden until there is ink: an empty window should not offer to replay a
    // drawing that does not exist, and the canvas gets the height instead.
    scrubber_->setVisible(false);
    canvas_column->addWidget(scrubber_);

    output_canvas_splitter_->addWidget(canvas_pane);
    output_canvas_splitter_->setSizes({kOutputHeight, kCanvasHeight});

    // A program that names a screen is showing something the turtle strip cannot
    // describe: its coordinates are pixels from the top-left, and a click is a
    // question about one of them. The badge says which mode is on, the click is
    // answered in the status bar, and the scrubber -- which rewinds a display
    // list a screen does not show -- says why it is off.
    connect(turtle_canvas_, &TurtleCanvas::framebufferChanged, this, [this] {
        if (turtle_canvas_ == nullptr) {
            return;
        }
        const bool screen = turtle_canvas_->framebufferActive();
        if (turtle_inspector_ != nullptr) {
            const QSize size = turtle_canvas_->framebufferSize();
            turtle_inspector_->setScreenMode(screen ? size.width() : 0,
                                             screen ? size.height() : 0);
        }
        if (scrubber_ != nullptr) {
            scrubber_->setEnabled(!screen);
            scrubber_->setToolTip(
                screen ? tr("A screen shows its last frame; scrubbing works on drawings")
                       : QString{});
        }
    });
    connect(turtle_canvas_, &TurtleCanvas::pixelPicked, this,
            [this](int x, int y, const QColor& colour) {
                statusBar()->showMessage(
                    tr("pixel (%1, %2) = %3").arg(x).arg(y).arg(colour.name().toUpper()),
                    6000);
            });

    connect(scrubber_, &Scrubber::frameRequested, this, [this](int frame) {
        if (turtle_canvas_ != nullptr) {
            turtle_canvas_->setVisibleFrame(frame);
        }
    });
    connect(turtle_canvas_, &TurtleCanvas::contentChanged, this, [this] {
        if (turtle_canvas_ == nullptr || scrubber_ == nullptr) {
            return;
        }
        const int frames = turtle_canvas_->frameCount();
        if (frames != scrubber_->frames()) {
            scrubber_->setFrames(frames);
            scrubber_->setVisibleFrame(turtle_canvas_->visibleFrameCount());
        }
        // A `clear` empties the history, so this also hides the strip again.
        scrubber_->setVisible(frames > 0);
    });

    // The queue takes no ownership and does not start a thread: the producer
    // pushes from whichever thread runs the interpreter, and the bridge drains
    // on this one. That split is why a turtle command costs one lock-free push
    // instead of a queued signal delivery.
    canvas_queue_ = std::make_unique<timewarp::canvas::CanvasCommandQueue>();
    canvas_bridge_ = std::make_unique<timewarp::canvas::qt::CanvasBridge>(*canvas_queue_, this);
    canvas_bridge_->set_sink([this](timewarp::canvas::CanvasCommandView view) {
        applyCanvasCommand(view);
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

}  // namespace timewarp::ui
