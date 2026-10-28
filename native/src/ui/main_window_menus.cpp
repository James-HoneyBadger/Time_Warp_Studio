// SPDX-License-Identifier: MIT

#include "main_window_internal.hpp"

#include "timewarp/core/qt_gui_compat.hpp"
#include "timewarp/ui/main_window.hpp"

#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/watch.hpp"
#include "timewarp/ui/native_runner.hpp"

#include <QAction>
#include <QDir>
#include <QDockWidget>
#include <QFileInfo>
#include <QKeySequence>
#include <QMenu>
#include <QMenuBar>
#include <QMessageBox>
#include <QPlainTextEdit>
#ifdef TIME_WARP_HAVE_PRINT
#endif
#include <QSettings>
#include <QShortcut>
#include <QSignalBlocker>

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
    // Ctrl+Alt+V, the shortcut the removed implementation used, and one no other
    // action here claims.
    add(file, tr("&Version History..."), timewarp::core::keySequence(Qt::CTRL | Qt::ALT, Qt::Key_V),
        [this] { showVersionHistory(); })
        ->setObjectName(QStringLiteral("versionHistoryAction"));
    file->addSeparator();
    add(file, tr("&Export Drawing..."), timewarp::core::keySequence(Qt::CTRL, Qt::Key_E),
        [this] { exportDrawingDialog(); });
    add(file, tr("Export &Audio as WAV..."), QKeySequence{},
        [this] { exportAudioDialog(); })
        ->setObjectName(QStringLiteral("exportAudioAction"));
    add(file, tr("Examples &Gallery..."), QKeySequence{},
        [this] { showExamplesGallery(); })
        ->setObjectName(QStringLiteral("examplesGalleryAction"));
    add(file, tr("&New Notebook..."), QKeySequence{},
        [this] { newNotebook(); })
        ->setObjectName(QStringLiteral("newNotebookAction"));
    add(file, tr("&Open Notebook..."), QKeySequence{},
        [this] { openNotebook(); })
        ->setObjectName(QStringLiteral("openNotebookAction"));
    // Qt Gui carries QPdfWriter, so the PDF needs no component beyond the ones
    // this application already requires.
    add(file, tr("Export Code as &PDF..."), QKeySequence{},
        [this] { exportCodePdfDialog(); })
        ->setObjectName(QStringLiteral("exportPdfAction"));
    add(file, tr("Save &Output As..."), QKeySequence{},
        [this] { saveOutputAs(); })
        ->setObjectName(QStringLiteral("saveOutputAction"));
    add(file, tr("Export &Session as HTML..."), QKeySequence{},
        [this] { exportSessionHtmlDialog(); })
        ->setObjectName(QStringLiteral("exportSessionAction"));
    // Printing is the one entry here that needs a component of its own, so a
    // build without Qt6::PrintSupport does not show a dead entry.
#ifdef TIME_WARP_HAVE_PRINT
    add(file, tr("&Print Code..."), QKeySequence::Print, [this] { printCode(); })
        ->setObjectName(QStringLiteral("printCodeAction"));
    add(file, tr("Print &Drawing..."), QKeySequence{},
        [this] { printDrawing(); })
        ->setObjectName(QStringLiteral("printDrawingAction"));
#endif
    // A toggle rather than a dialog: recording is a state the canvas is in, and a
    // menu that has to be opened again to stop is not one a demonstration can
    // use. Qt reads GIF but does not write it, so `gif_encoder` is in tree.
    record_gif_action_ = file->addAction(tr("Record Canvas as &GIF..."));
    record_gif_action_->setObjectName(QStringLiteral("recordGifAction"));
    record_gif_action_->setCheckable(true);
    connect(record_gif_action_, &QAction::toggled, this,
            [this](bool on) { setGifRecording(on); });
    add(file, tr("Export Lesson Session as &Markdown..."), QKeySequence{},
        [this] { exportLessonSessionMarkdownDialog(); })
        ->setObjectName(QStringLiteral("exportLessonMarkdownAction"));
    add(file, tr("Export Lesson Session as P&DF..."), QKeySequence{},
        [this] { exportLessonSessionPdfDialog(); })
        ->setObjectName(QStringLiteral("exportLessonPdfAction"));
    file->addSeparator();
    // The split: one document each side, rather than two views of one buffer.
    add(file, tr("Split Editor &Right"), timewarp::core::keySequence(Qt::CTRL, Qt::Key_Backslash),
        [this] { splitEditor(); })
        ->setObjectName(QStringLiteral("splitEditorAction"));
    add(file, tr("Close S&plit"), timewarp::core::keySequence(Qt::CTRL | Qt::SHIFT, Qt::Key_Backslash),
        [this] { closeSplit(); })
        ->setObjectName(QStringLiteral("closeSplitAction"));
    add(file, tr("&Move Tab to Other Pane"), timewarp::core::keySequence(Qt::CTRL | Qt::ALT, Qt::Key_M),
        [this] { moveTabToOtherPane(); })
        ->setObjectName(QStringLiteral("moveTabAction"));
    file->addSeparator();
    add(file, tr("&Close Tab"), QKeySequence::Close, [this] { closeCurrentTab(); });
    // Through close() rather than qApp->quit(), so a modified document still gets
    // its prompt: closeEvent is the one place that asks.
    add(file, tr("&Quit"), QKeySequence::Quit, [this] { close(); });

    // -- Project ----------------------------------------------------------
    //
    // Its own menu rather than more entries under File: a project is a workspace
    // with its own open/close lifecycle, and burying Close Project between Export
    // and Quit is how a user ends up with two projects in one window.
    QMenu* project_menu = bar->addMenu(tr("&Project"));
    project_menu->setObjectName(QStringLiteral("projectMenu"));
    add(project_menu, tr("&New Project..."),
        timewarp::core::keySequence(Qt::CTRL | Qt::SHIFT, Qt::Key_N), [this] { newProjectDialog(); })
        ->setObjectName(QStringLiteral("newProjectAction"));
    add(project_menu, tr("&Open Project..."),
        timewarp::core::keySequence(Qt::CTRL | Qt::SHIFT, Qt::Key_O), [this] { openProjectDialog(); })
        ->setObjectName(QStringLiteral("openProjectAction"));
    recent_projects_menu_ = project_menu->addMenu(tr("Open &Recent Project"));
    recent_projects_menu_->setObjectName(QStringLiteral("recentProjectsMenu"));
    connect(recent_projects_menu_, &QMenu::aboutToShow, this, [this] {
        recent_projects_menu_->clear();
        const QStringList paths =
            QSettings{}.value(QLatin1String(kSettingRecentProjects)).toStringList();
        for (const QString& path : paths) {
            QAction* action = recent_projects_menu_->addAction(
                QFileInfo{path}.fileName() + QStringLiteral("  \u2014  ") +
                QDir::toNativeSeparators(QFileInfo{path}.absolutePath()));
            connect(action, &QAction::triggered, this,
                    [this, path] { static_cast<void>(openProject(path)); });
        }
        if (paths.isEmpty()) {
            recent_projects_menu_->addAction(tr("(none)"))->setEnabled(false);
        }
    });
    project_menu->addSeparator();
    add(project_menu, tr("Add Current File to Project"), QKeySequence{},
        [this] {
            CodeEditor* editor = currentEditor();
            const QString path = editor != nullptr ? pathOf(editor) : QString{};
            if (!project_.isValid()) {
                setExecutionState(tr("ready"), tr("Open or create a project first."));
                return;
            }
            if (path.isEmpty()) {
                setExecutionState(tr("ready"), tr("Save the document before adding it."));
                return;
            }
            if (project_.contains(path)) {
                setExecutionState(tr("ready"), tr("That file is already in the project."));
                return;
            }
            project_.addFile(path);
            if (saveProject()) {
                refreshProjectPanel();
                project_panel_->selectFile(path);
                setExecutionState(tr("ready"), tr("Added %1 to %2").arg(nameOf(editor),
                                                                         project_.name()));
            }
        })
        ->setObjectName(QStringLiteral("addCurrentFileToProjectAction"));
    add(project_menu, tr("Add &Files to Project..."), QKeySequence{},
        [this] { addFilesToProject(QString{}); })
        ->setObjectName(QStringLiteral("addFilesToProjectAction"));
    add(project_menu, tr("Add F&older to Project..."), QKeySequence{},
        [this] { addFolderToProject(); })
        ->setObjectName(QStringLiteral("addFolderToProjectAction"));
    project_menu->addSeparator();
    add(project_menu, tr("New &Group..."), timewarp::core::keySequence(Qt::CTRL | Qt::SHIFT, Qt::Key_G),
        [this] { addGroupToProject(); })
        ->setObjectName(QStringLiteral("newProjectGroupAction"));
    add(project_menu, tr("Rename Project..."), QKeySequence{},
        [this] { renameProject(); })
        ->setObjectName(QStringLiteral("renameProjectAction"));
    add(project_menu, tr("Set Current File as &Entry Point"), QKeySequence{},
        [this] {
            CodeEditor* editor = currentEditor();
            const QString path = editor != nullptr ? pathOf(editor) : QString{};
            if (path.isEmpty()) {
                setExecutionState(tr("ready"), tr("Save the document first: an entry "
                                                  "point is a file on disk."));
                return;
            }
            setProjectEntry(path);
        })
        ->setObjectName(QStringLiteral("setProjectEntryAction"));
    add(project_menu, tr("Clear Entry Point"), QKeySequence{},
        [this] { clearProjectEntry(); })
        ->setObjectName(QStringLiteral("clearProjectEntryAction"));
    add(project_menu, tr("Reveal Project &Folder"), QKeySequence{},
        [this] { revealProjectFolder(); })
        ->setObjectName(QStringLiteral("revealProjectAction"));
    add(project_menu, tr("&Close Project"), QKeySequence{},
        [this] { closeProject(); })
        ->setObjectName(QStringLiteral("closeProjectAction"));

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
    add(edit, tr("&Copy Drawing"), timewarp::core::keySequence(Qt::CTRL | Qt::SHIFT, Qt::Key_C),
        [this] { copyDrawingToClipboard(); });
    edit->addSeparator();
    add(edit, tr("&Find..."), QKeySequence::Find, [this] { showFindBar(false); });
    add(edit, tr("Find and &Replace..."), QKeySequence::Replace,
        [this] { showFindBar(true); });
    // F3 rather than Ctrl+F, so finding again works with the bar closed: Ctrl+F
    // would only re-open and re-focus the field.
    add(edit, tr("Find &Next"), QKeySequence{Qt::Key_F3},
        [this] { findNextMatch(false); });
    add(edit, tr("Find Pre&vious"), timewarp::core::keySequence(Qt::SHIFT, Qt::Key_F3),
        [this] { findNextMatch(true); });
    // The repository-wide search the removed implementation had and this one did
    // not. Ctrl+Shift+F, which no other action uses.
    add(edit, tr("Find in F&iles..."), timewarp::core::keySequence(Qt::CTRL | Qt::SHIFT, Qt::Key_F),
        [this] { showFindInFiles(); })
        ->setObjectName(QStringLiteral("findInFilesAction"));
    // Ctrl+Shift+I, which no other action claims.
    add(edit, tr("Insert &Snippet..."), timewarp::core::keySequence(Qt::CTRL | Qt::SHIFT, Qt::Key_I),
        [this] { insertSnippet(); })
        ->setObjectName(QStringLiteral("insertSnippetAction"));
    edit->addSeparator();
    // Ctrl+L, not Ctrl+G: the grid on the canvas already owns Ctrl+G, and two
    // actions on one key is an ambiguous shortcut that Qt resolves by guessing.
    add(edit, tr("&Go to Line..."), timewarp::core::keySequence(Qt::CTRL, Qt::Key_L),
        [this] { goToLinePrompt(); });
    // Ctrl+I, which no other action claims. There was no Format Code at all
    // before this; the removed implementation had one under Edit.
    add(edit, tr("&Format Code"), timewarp::core::keySequence(Qt::CTRL, Qt::Key_I),
        [this] { formatCurrentDocument(); })
        ->setObjectName(QStringLiteral("formatCodeAction"));
    edit->addSeparator();
    add(edit, tr("&Preferences..."), timewarp::core::keySequence(Qt::CTRL, Qt::Key_Comma),
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
    add(run, tr("Run &Selection"), timewarp::core::keySequence(Qt::CTRL, Qt::Key_F9),
        [this] { runSelection(); })
        ->setObjectName(QStringLiteral("runSelectionAction"));
    add(run, tr("&Stop"), timewarp::core::keySequence(Qt::SHIFT, Qt::Key_F9),
        [this] { stopRunningDocument(); });
    // A comparison, not a race: both programs run headless, one after the other,
    // and the table attributes every number to the program that produced it.
    add(run, tr("Run &Both Panes"), timewarp::core::keySequence(Qt::CTRL | Qt::ALT, Qt::Key_B),
        [this] { runBothPanes(); })
        ->setObjectName(QStringLiteral("runBothPanesAction"));
    // A project is a set of programs with one of them the way in, so running the
    // set is running that one -- with the project's folder as the run's directory.
    add(run, tr("Run Pro&ject"), timewarp::core::keySequence(Qt::CTRL | Qt::ALT, Qt::Key_R),
        [this] { runProject(); })
        ->setObjectName(QStringLiteral("runProjectAction"));
    run->addSeparator();
    // Reset is not Stop: Stop ends a program that is still running, Reset puts
    // the screen back to how it looked before a program that has already ended
    // -- which is the thing a learner presses after a drawing they want to redo.
    add(run, tr("&Reset Program"), timewarp::core::keySequence(Qt::CTRL | Qt::SHIFT, Qt::Key_F9),
        [this] { resetProgram(); })
        ->setObjectName(QStringLiteral("resetProgramAction"));
    run->addSeparator();
    add(run, tr("Clear &Output"), {}, [this] { clearOutput(); });
    add(run, tr("Clear &Canvas"), {}, [this] { clearCanvas(); });

    // -- Tools ------------------------------------------------------------
    // A CHIP-8 program is the one document that wants a live machine rather
    // than a run-to-completion executor, and a window is where its keypad and
    // inspector fit. Ctrl+Shift+M is unclaimed.
    QMenu* tools = bar->addMenu(tr("&Tools"));
    tools->setObjectName(QStringLiteral("toolsMenu"));
    add(tools, tr("CHIP-8 &Emulator..."),
        timewarp::core::keySequence(Qt::CTRL | Qt::SHIFT, Qt::Key_M), [this] { openChip8Emulator(); })
        ->setObjectName(QStringLiteral("chip8EmulatorAction"));
    // The machine the languages share. Ctrl+Alt+W is unclaimed and is the
    // chord the machine has always been documented with, so renaming it to the
    // HB-1 does not move it out from under anyone's fingers.
    add(tools, tr("&HB-1 Computer..."),
        timewarp::core::keySequence(Qt::CTRL | Qt::ALT, Qt::Key_W), [this] { openRetroComputer(); })
        ->setObjectName(QStringLiteral("retroComputerAction"));
#ifdef TIME_WARP_HAVE_AI
    tools->addSeparator();
    // The assistant sits beside the machines rather than in a menu of its own: one
    // heading for what this IDE can do *to a program* is a menu a person can find,
    // and a top-level menu with three entries is a heading that exists to hold its
    // own name.
    add(tools, tr("\U0001F916 &Assistant"),
        timewarp::core::keySequence(Qt::CTRL | Qt::SHIFT, Qt::Key_A),
        [this] { showAssistant(); })
        ->setObjectName(QStringLiteral("aiAssistantAction"));
    // The two questions a learner asks most often, one click from the program they
    // are looking at. No shortcut: a key pressed by accident spends a request.
    add(tools, tr("&Explain This Program"), QKeySequence{},
        [this] { askAssistant(timewarp::ai::Task::Explain); })
        ->setObjectName(QStringLiteral("aiExplainAction"));
    add(tools, tr("Fix This &Program"), QKeySequence{},
        [this] { askAssistant(timewarp::ai::Task::Fix); })
        ->setObjectName(QStringLiteral("aiFixAction"));
    // The one question that starts from what the run said rather than from the
    // program: the Console already holds the error, so the action names it.
    add(tools, tr("Ask About the Last &Error"), QKeySequence{},
        [this] { askAssistantAboutOutput(); })
        ->setObjectName(QStringLiteral("aiDiagnoseAction"));
    add(tools, tr("AI &Settings..."), QKeySequence{}, [this] { showAiSettings(); })
        ->setObjectName(QStringLiteral("aiSettingsAction"));
#endif

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
    add(view, tr("&Fit Drawing"), timewarp::core::keySequence(Qt::CTRL, Qt::Key_0), [this] {
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
    grid->setShortcut(timewarp::core::keySequence(Qt::CTRL, Qt::Key_G));
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
    // The values behind the toggle. Until this existed the overlay had one
    // hard-coded intensity, which made "CRT scanlines" a switch rather than a
    // display setting.
    add(view, tr("CRT Se&ttings..."), QKeySequence{}, [this] { showCrtSettings(); })
        ->setObjectName(QStringLiteral("crtSettingsAction"));

    // Off every launch on purpose: timing every statement is work, and a setting
    // that remembered "on" would make every later run pay for a report nobody
    // asked to see.
    profile_action_ = view->addAction(tr("&Profile Run"));
    profile_action_->setObjectName(QStringLiteral("profileRunAction"));
    profile_action_->setCheckable(true);
    profile_action_->setChecked(false);
    connect(profile_action_, &QAction::toggled, this, [this](bool on) {
        setExecutionState(tr("ready"),
                          on ? tr("Profiling the next run") : tr("Profiling off"));
    });

    // The old Monitor styles, as one action each: a theme and the CRT values that
    // belong with it, so "Amber monitor" is one click rather than a hunt through
    // two different menus.
    QMenu* monitors = view->addMenu(tr("&Screen Mode"));
    monitors->setObjectName(QStringLiteral("screenModeMenu"));
    add(monitors, tr("&Normal Display"), QKeySequence{}, [this] {
        applyScreenPreset(ScreenPreset::Normal);
    })->setObjectName(QStringLiteral("screenPresetNormal"));
    add(monitors, tr("&Amber Monitor"), QKeySequence{}, [this] {
        applyScreenPreset(ScreenPreset::Amber);
    })->setObjectName(QStringLiteral("screenPresetAmber"));
    add(monitors, tr("&Green Monitor"), QKeySequence{}, [this] {
        applyScreenPreset(ScreenPreset::Green);
    })->setObjectName(QStringLiteral("screenPresetGreen"));
    add(monitors, tr("&White Phosphor"), QKeySequence{}, [this] {
        applyScreenPreset(ScreenPreset::White);
    })->setObjectName(QStringLiteral("screenPresetWhite"));

    // Not a theme of its own: every theme gets a maximum-contrast variant, so a
    // learner who prefers the amber phosphor keeps it and can still read it. The
    // tick is driven from the window's state through the action's own toggle.
    high_contrast_action_ = view->addAction(tr("High &Contrast"));
    high_contrast_action_->setObjectName(QStringLiteral("highContrastAction"));
    high_contrast_action_->setCheckable(true);
    high_contrast_action_->setShortcut(timewarp::core::keySequence(Qt::CTRL | Qt::ALT, Qt::Key_H));
    high_contrast_action_->setChecked(high_contrast_);
    connect(high_contrast_action_, &QAction::toggled, this,
            [this](bool on) { setHighContrast(on); });

    // Focus mode keeps the editor and the canvas and takes the rest away: the one
    // thing every "distraction free" mode has to get right is the way out, which
    // is why Esc is bound while the mode is on and unbound the moment it ends.
    focus_action_ = view->addAction(tr("&Focus Mode"));
    focus_action_->setObjectName(QStringLiteral("focusModeAction"));
    focus_action_->setCheckable(true);
    focus_action_->setShortcut(QKeySequence{Qt::Key_F11});
    connect(focus_action_, &QAction::toggled, this, [this](bool on) { setFocusMode(on); });
    focus_shortcut_ = new QShortcut{QKeySequence{Qt::Key_Escape}, this};
    focus_shortcut_->setObjectName(QStringLiteral("focusModeEscape"));
    // Window-scoped, and disabled until the mode is entered: Escape has to keep
    // whatever meaning the focused widget gives it at all other times.
    focus_shortcut_->setContext(Qt::WindowShortcut);
    focus_shortcut_->setEnabled(false);
    connect(focus_shortcut_, &QShortcut::activated, this, [this] { setFocusMode(false); });

    // The classroom's mode: focus mode for an audience. The size and whether it
    // takes the whole screen belong to the class, because `teach.py present`
    // writes them, so the menu entry is only the switch.
    presentation_action_ = view->addAction(tr("&Presentation Mode"));
    presentation_action_->setObjectName(QStringLiteral("presentationModeAction"));
    presentation_action_->setCheckable(true);
    presentation_action_->setShortcut(timewarp::core::keySequence(Qt::SHIFT, Qt::Key_F11));
    connect(presentation_action_, &QAction::toggled, this,
            [this](bool on) { setPresentationMode(on); });
    presentation_shortcut_ = new QShortcut{QKeySequence{Qt::Key_Escape}, this};
    presentation_shortcut_->setObjectName(QStringLiteral("presentationModeEscape"));
    presentation_shortcut_->setContext(Qt::WindowShortcut);
    presentation_shortcut_->setEnabled(false);
    connect(presentation_shortcut_, &QShortcut::activated, this,
            [this] { setPresentationMode(false); });

    view->addSeparator();
    // Ctrl+Shift+E, the key every editor's file explorer answers to. The dock
    // starts visible, so this is the way back after it is closed.
    add(view, tr("&Project Panel"),
        timewarp::core::keySequence(Qt::CTRL | Qt::SHIFT, Qt::Key_E),
        [this] {
            if (project_dock_ == nullptr) {
                return;
            }
            project_dock_->setVisible(!project_dock_->isVisible());
            if (project_dock_->isVisible()) {
                project_dock_->raise();
            }
        })
        ->setObjectName(QStringLiteral("projectPanelAction"));
    // F10 is on the function-key bar; Ctrl+Shift+P is the convention everybody
    // already has in their fingers.
    add(view, tr("Command &Palette..."),
        timewarp::core::keySequence(Qt::CTRL | Qt::SHIFT, Qt::Key_P),
        [this] { showCommandPalette(); })
        ->setObjectName(QStringLiteral("commandPaletteAction"));
    buildThemeMenu(view);

    // -- Help -------------------------------------------------------------
    QMenu* help = bar->addMenu(tr("&Help"));
    help->setObjectName(QStringLiteral("helpMenu"));
    add(help, tr("&Context Help"), QKeySequence{Qt::Key_F1},
        [this] { showContextHelp(); })
        ->setObjectName(QStringLiteral("contextHelpAction"));
    add(help, tr("&Keyword Inspector..."), timewarp::core::keySequence(Qt::CTRL, Qt::Key_F1),
        [this] { showKeywordInspector(); })
        ->setObjectName(QStringLiteral("keywordInspectorAction"));
    help->addSeparator();
    add(help, tr("&Documentation"), {}, [this] { showDocumentation(); })
        ->setObjectName(QStringLiteral("documentationAction"));
    add(help, tr("&Getting Started"), {},
        [this] { showDocumentation(QStringLiteral("lessons/student-guide.md")); })
        ->setObjectName(QStringLiteral("gettingStartedAction"));
    add(help, tr("&Guided Tour"), QKeySequence{Qt::Key_F12},
        [this] { showGuidedTour(); })
        ->setObjectName(QStringLiteral("guidedTourAction"));
    help->addSeparator();
    // Shift+F1 is the bar's "Keys" binding; the menu entry has no sequence of
    // its own so the two cannot disagree about which key opens the table.
    add(help, tr("&Keyboard Shortcuts"), {}, [this] { showShortcutHelp(); })
        ->setObjectName(QStringLiteral("shortcutsAction"));
    add(help, tr("&About Time Warp Studio"), {}, [this] {
        QMessageBox::about(
            this, tr("About Time Warp Studio"),
            tr("<b>Time Warp Studio</b> %1"
               "<p>An old-school programming environment for eleven languages: BASIC, "
               "APL, Logo, PILOT, FOCAL, Scheme, CHIP-8, COMAL, REXX, Smalltalk and "
               "Forth, with an integrated turtle graphics canvas — and two complete "
               "machines inside it: the HB-1 home computer and a live CHIP-8."
               "<p>Everything runs on this machine: no account, no network call and "
               "no telemetry.")
                .arg(timewarp::ui::version()));
    });
    add(help, tr("About &Qt"), {}, [this] { QMessageBox::aboutQt(this); });
}

}  // namespace timewarp::ui
