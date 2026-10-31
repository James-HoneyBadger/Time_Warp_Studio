// SPDX-License-Identifier: MIT

#include "main_window_internal.hpp"

#include "timewarp/core/qt_names.hpp"
#include "timewarp/ui/main_window.hpp"

#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/watch.hpp"
#include "timewarp/ui/native_runner.hpp"

#include <QAction>
#include <QComboBox>
#include <QDockWidget>
#include <QKeySequence>
#include <QLabel>
#include <QMenu>
#ifdef TIME_WARP_HAVE_PRINT
#endif
#include <QStatusBar>
#include <QToolBar>
#include <QToolButton>

#include <functional>
#include <memory>
#include <string>
#include <vector>

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
    connect(language_combo_, QOverload<int>::of(&QComboBox::currentIndexChanged), this,
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
    run_menu_ = new QMenu{run_button};
    run_menu_->setObjectName(QStringLiteral("runButtonMenu"));
    // Rebuilt on every open: a history menu built once is the history as it was
    // when the window started.
    connect(run_menu_, &QMenu::aboutToShow, this, [this] { rebuildRunMenu(); });
    QAction* popup_run = run_menu_->addAction(tr("\U0001F680 Run"));
    // No shortcut here: F9 belongs to the Run menu's action, and two actions
    // claiming it is an ambiguous shortcut rather than a convenience.
    connect(popup_run, &QAction::triggered, this, [this] { runCurrentDocument(); });
    QAction* popup_stop = run_menu_->addAction(tr("\u23F9\uFE0F Stop"));
    connect(popup_stop, &QAction::triggered, this, [this] { stopRunningDocument(); });
    run_button->setMenu(run_menu_);
    connect(run_button, &QToolButton::clicked, this, [this] { runCurrentDocument(); });
    tool_bar_->addWidget(run_button);

    auto addRunAction = [this](const QString& text, const QString& tip) {
        QAction* action = tool_bar_->addAction(text);
        action->setToolTip(tip);
        return action;
    };
    // The debugging controls, live for every language. The line-loop languages
    // stop at a statement; Logo at a top-level statement, Smalltalk at a message
    // send and CHIP-8 at an instruction; Scheme through its own runner. Continue
    // resumes and Trace stops at the next boundary.
    QAction* debug_action =
        addRunAction(tr("\U0001F41B Debug"), tr("Run, stopping before any breakpoint"));
    debug_action->setObjectName(QStringLiteral("debugAction"));
    connect(debug_action, &QAction::triggered, this, [this] { runCurrentDocument(); });
    QAction* continue_action = addRunAction(
        tr("\u25B6\uFE0F Continue"), tr("Resume from a pause to the next breakpoint"));
    continue_action->setObjectName(QStringLiteral("continueAction"));
    connect(continue_action, &QAction::triggered, this, [this] {
        if (native_runner_ != nullptr && native_runner_->isPaused()) {
            native_runner_->request_continue();
            return;
        }
        setExecutionState(tr("ready"), tr("Nothing is paused"));
    });
    QAction* trace_action = addRunAction(
        tr("\u2193 Trace"),
        tr("Resume and stop at the next statement, message send or instruction"));
    trace_action->setObjectName(QStringLiteral("traceAction"));
    connect(trace_action, &QAction::triggered, this, [this] {
        if (native_runner_ != nullptr && native_runner_->isPaused()) {
            native_runner_->request_step();
            return;
        }
        setExecutionState(tr("ready"), tr("Nothing is paused"));
    });
    QAction* stop_action =
        addRunAction(tr("\u23F9\uFE0F Stop"), tr("Stop the running program"));
    connect(stop_action, &QAction::triggered, this, [this] { stopRunningDocument(); });
    QAction* snippets_action =
        addRunAction(tr("\U0001F4CB Snippets"), tr("Insert a code snippet (Ctrl+Shift+I)"));
    connect(snippets_action, &QAction::triggered, this, [this] { insertSnippet(); });

    QAction* clear_output = tool_bar_secondary_->addAction(tr("\U0001F9F9 Clear Output"));
    clear_output->setToolTip(tr("Clear the output panel"));
    connect(clear_output, &QAction::triggered, this, [this] { clearOutput(); });
    QAction* clear_canvas = tool_bar_secondary_->addAction(tr("\U0001F5BC Clear Canvas"));
    clear_canvas->setToolTip(tr("Clear the graphics canvas"));
    connect(clear_canvas, &QAction::triggered, this, [this] { clearCanvas(); });

    tool_bar_secondary_->addSeparator();
    // The accessibility ladder: 100, 125, 150%. It scales the editor, the console
    // and the panels together -- a learner who needs bigger text needs all of it,
    // not one pane at a time -- and the chosen step is a setting.
    QAction* font_scale = tool_bar_secondary_->addAction(tr("\u267F A+"));
    font_scale->setObjectName(QStringLiteral("fontScaleAction"));
    font_scale->setToolTip(tr("Bigger text: 100%, 125%, 150% (Ctrl+Alt+A)"));
    font_scale->setShortcut(timewarp::core::keySequence(Qt::CTRL | Qt::ALT, Qt::Key_A));
    connect(font_scale, &QAction::triggered, this, [this] { cycleFontScale(); });

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
    project_panel_ = new ProjectPanel{this};
    connect(project_panel_, &ProjectPanel::newProjectRequested, this,
            &MainWindow::newProjectDialog);
    connect(project_panel_, &ProjectPanel::openProjectRequested, this,
            &MainWindow::openProjectDialog);
    connect(project_panel_, &ProjectPanel::addFilesRequested, this,
            &MainWindow::addFilesToProject);
    connect(project_panel_, &ProjectPanel::addFolderRequested, this,
            &MainWindow::addFolderToProject);
    connect(project_panel_, &ProjectPanel::addGroupRequested, this,
            &MainWindow::addGroupToProject);
    connect(project_panel_, &ProjectPanel::fileToNewGroupRequested, this,
            &MainWindow::addNewGroupForFile);
    connect(project_panel_, &ProjectPanel::renameGroupRequested, this,
            &MainWindow::renameProjectGroup);
    connect(project_panel_, &ProjectPanel::removeGroupRequested, this,
            &MainWindow::removeProjectGroup);
    connect(project_panel_, &ProjectPanel::renameProjectRequested, this,
            &MainWindow::renameProject);
    connect(project_panel_, &ProjectPanel::closeProjectRequested, this,
            &MainWindow::closeProject);
    connect(project_panel_, &ProjectPanel::revealRequested, this,
            &MainWindow::revealProjectFolder);
    connect(project_panel_, &ProjectPanel::runProjectRequested, this,
            &MainWindow::runProject);
    connect(project_panel_, &ProjectPanel::setEntryRequested, this,
            &MainWindow::setProjectEntry);
    connect(project_panel_, &ProjectPanel::clearEntryRequested, this,
            &MainWindow::clearProjectEntry);
    connect(project_panel_, &ProjectPanel::removeFileRequested, this,
            [this](const QString& path) {
                if (project_.removeFile(path) && saveProject()) {
                    refreshProjectPanel();
                }
            });
    connect(project_panel_, &ProjectPanel::fileActivated, this, [this](const QString& path) {
        if (openFile(path)) {
            project_panel_->selectFile(path);
        }
    });
    connect(project_panel_, &ProjectPanel::fileMovedToGroup, this,
            &MainWindow::moveProjectFile);
    connect(project_panel_, &ProjectPanel::filesDropped, this,
            [this](const QStringList& paths, const QString& group) {
                if (!project_.isValid() || paths.isEmpty()) {
                    return;
                }
                // Dropped files are programs the user chose; nothing else in the
                // folder is dragged in, so a folder full of build output does not
                // become a project entry.
                project_.addFiles(paths, group);
                if (saveProject()) {
                    refreshProjectPanel();
                }
            });

    project_dock_ = new QDockWidget{tr("\U0001F5C2 Projects"), this};
    project_dock_->setObjectName(QStringLiteral("projectDock"));
    project_dock_->setWidget(project_panel_);
    project_dock_->setAllowedAreas(Qt::LeftDockWidgetArea | Qt::RightDockWidgetArea);
    // Visible from the start: a project explorer nobody can see is a feature
    // nobody can find, and this one explains itself with an empty state.
    project_dock_->setVisible(true);
    addDockWidget(Qt::LeftDockWidgetArea, project_dock_);

    outline_panel_ = new OutlinePanel{this};
    // A click in the outline is a jump, through the same method Go to Line uses,
    // so the cursor, the scroll position and the status bar all agree.
    connect(outline_panel_, &OutlinePanel::entryActivated, this, [this](int line) {
        if (CodeEditor* editor = currentEditor(); editor != nullptr) {
            editor->goToLine(line);
            editor->setFocus();
        }
    });

    outline_dock_ = new QDockWidget{tr("\U0001F4CB Document Outline"), this};
    outline_dock_->setObjectName(QStringLiteral("documentOutlineDock"));
    outline_dock_->setWidget(outline_panel_);
    // Deliberately not `setAllowedAreas(AllDockWidgetAreas)`: a dock that can be
    // dropped on any edge can be dropped somewhere the user cannot find again.
    outline_dock_->setAllowedAreas(Qt::RightDockWidgetArea | Qt::LeftDockWidgetArea);
    // Hidden at startup; `restoreState()` re-shows it if it was open last session.
    outline_dock_->setVisible(false);
    addDockWidget(Qt::RightDockWidgetArea, outline_dock_);
}

}  // namespace timewarp::ui
