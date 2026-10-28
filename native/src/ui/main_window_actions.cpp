// SPDX-License-Identifier: MIT

#include "main_window_internal.hpp"

#include "timewarp/core/qt_gui_compat.hpp"
#include "timewarp/ui/main_window.hpp"

#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/watch.hpp"
#include "timewarp/ui/native_runner.hpp"

#include <QAction>
#include <QActionGroup>
#include <QApplication>
#include <QCloseEvent>
#include <QClipboard>
#include <QComboBox>
#include <QDateTime>
#include <QDesktopServices>
#include <QDir>
#include <QDirIterator>
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
#include <QPageLayout>
#include <QPlainTextEdit>
#ifdef TIME_WARP_HAVE_PRINT
#include <QPrintDialog>
#include <QPrinter>
#endif
#include <QPushButton>
#include <QSaveFile>
#include <QSet>
#include <QSettings>
#include <QShortcut>
#include <QSignalBlocker>
#include <QSlider>
#include <QSplitter>
#include <QStatusBar>
#include <QTabWidget>
#include <QTimer>
#include <QTextDocument>
#include <QToolBar>
#include <QToolButton>
#include <QUrl>
#include <QVBoxLayout>

#include <algorithm>
#include <functional>
#include <map>
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
    if (id == QLatin1String("run_both")) {
        runBothPanes();
        return;
    }
    if (id == QLatin1String("run_selection")) {
        runSelection();
        return;
    }
    if (id == QLatin1String("reset")) {
        resetProgram();
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
    if (id == QLatin1String("export_pdf")) {
        exportCodePdfDialog();
        return;
    }
    if (id == QLatin1String("export_session")) {
        exportSessionHtmlDialog();
        return;
    }

    // -- editing and view ---------------------------------------------------
    if (id == QLatin1String("find")) {
        findNextMatch(false);
        return;
    }
    if (id == QLatin1String("format")) {
        formatCurrentDocument();
        return;
    }
    if (id == QLatin1String("snippet")) {
        insertSnippet();
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
    if (id == QLatin1String("keywords")) {
        showKeywordInspector();
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
    if (id == QLatin1String("help")) {
        showContextHelp();
        return;
    }
    if (id == QLatin1String("shortcuts")) {
        showShortcutHelp();
        return;
    }
    if (id == QLatin1String("cycle_theme")) {
        cycleTheme();
        return;
    }

    // -- workspace keys ----------------------------------------------------
    //
    // Ids the function-key table names. Every one does something now: the rule is
    // that a key is bound when its action exists, and a key with no action is
    // absent rather than inert.
    if (id == QLatin1String("split_editor")) {
        if (editorSplit()) {
            closeSplit();
        } else {
            splitEditor();
        }
        return;
    }
    if (id == QLatin1String("find_in_files")) {
        showFindInFiles();
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
        // The deck is the split's own toggle, so the key and File > Split Editor
        // Right cannot disagree about whether the second pane is showing.
        if (editor_split_) {
            closeSplit();
        } else {
            splitEditor();
        }
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
    // A player window belongs to the program, not its reader: no prompt that
    // runs commands.
    refreshTerminalTab();
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
    if (project_dock_ != nullptr) {
        project_dock_->setVisible(false);
    }
    if (right_tabs_ != nullptr) {
        // The output/canvas column is the program, so it stays. The panels that
        // exist to inspect or author one do not, and neither does the tab strip
        // for a column with one page.
        QWidget* const output = output_canvas_splitter_;
        for (int index = right_tabs_->count() - 1; index >= 0; --index) {
            if (right_tabs_->widget(index) != output) {
                right_tabs_->removeTab(index);
            }
        }
        right_tabs_->setCurrentWidget(output);
        if (QTabBar* tabs = right_tabs_->tabBar()) {
            tabs->setVisible(false);
        }
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

void MainWindow::showFindInFiles() {
    if (find_in_files_ == nullptr) {
        find_in_files_ = new FindInFilesDialog{this};
        // A result opens the file at the line, through the same path the Open
        // dialog uses, so a hit is a document with its language, tab and recent
        // entry -- not a read-only preview.
        connect(find_in_files_, &FindInFilesDialog::hitActivated, this,
                [this](const QString& path, int line) {
                    if (openFile(path)) {
                        goToLine(line);
                    }
                });
        // Replace All writes to disk, so every open document it changed is reloaded
        // here: a buffer still holding the old text would write it back over the
        // replacement on the next save. Documents with unsaved changes are named as
        // off limits before the write, so there is nothing to reconcile for them.
        connect(find_in_files_, &FindInFilesDialog::filesReplaced, this,
                [this](const QStringList& paths) {
                    int reloaded = 0;
                    const QList<CodeEditor*> editors = findChildren<CodeEditor*>();
                    for (CodeEditor* editor : editors) {
                        const QString path = pathOf(editor);
                        if (path.isEmpty() ||
                            !paths.contains(QFileInfo{path}.absoluteFilePath())) {
                            continue;
                        }
                        QFile file{path};
                        if (!file.open(QIODevice::ReadOnly)) {
                            continue;
                        }
                        const QString text = QString::fromUtf8(file.readAll());
                        editor->setPlainText(text);
                        editor->document()->setModified(false);
                        ++reloaded;
                    }
                    setExecutionState(tr("ready"),
                                      reloaded > 0
                                          ? tr("Replaced in %1 file(s); %2 open document(s) "
                                               "reloaded")
                                                .arg(paths.size())
                                                .arg(reloaded)
                                          : tr("Replaced in %1 file(s)").arg(paths.size()));
                });
    }
    // Documents holding unsaved work are the ones a bulk edit must not write behind:
    // the disk copy is older than the buffer, and saving it back would be silent data
    // loss. Named before every use, because the set changes as documents do.
    QSet<QString> off_limits;
    for (CodeEditor* editor : findChildren<CodeEditor*>()) {
        if (!editor->document()->isModified()) {
            continue;
        }
        const QString path = pathOf(editor);
        if (!path.isEmpty()) {
            off_limits.insert(QFileInfo{path}.absoluteFilePath());
        }
    }
    find_in_files_->setOffLimits(off_limits);
    // The folder of the document in front, because that is what "in files" means
    // to whoever pressed the key; a selection becomes the term, because searching
    // for what you just highlighted is the common case.
    QString root;
    if (CodeEditor* editor = currentEditor()) {
        const QString path = pathOf(editor);
        if (!path.isEmpty()) {
            root = QFileInfo{path}.absolutePath();
        }
        const QString selected = editor->textCursor().selectedText();
        if (!selected.isEmpty() && !selected.contains(QLatin1Char('\n'))) {
            find_in_files_->setTerm(selected);
        }
    }
    if (root.isEmpty()) {
        root = QDir::homePath();
    }
    find_in_files_->setRoot(root);
    find_in_files_->show();
    find_in_files_->raise();
    find_in_files_->activateWindow();
}

void MainWindow::insertSnippet() {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        return;
    }
    QString error;
    const QList<Snippet> snippets = loadSnippets(&error);
    if (snippets.isEmpty()) {
        setExecutionState(tr("ready"),
                          error.isEmpty() ? tr("No snippets are installed.") : error);
        return;
    }
    SnippetDialog dialog{this};
    dialog.setSnippets(snippets);
    dialog.setLanguage(languageOf(editor));
    if (dialog.exec() != QDialog::Accepted) {
        return;
    }
    applySnippet(dialog.chosen());
}

void MainWindow::applySnippet(const Snippet& snippet) {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr || snippet.body.isEmpty()) {
        return;
    }
    // A selection becomes the insertion's starting text: `$SELECTION$` is how a
    // snippet wraps what the user already wrote.
    QString selection = editor->textCursor().selectedText();
    // Qt separates paragraphs with U+2029 in `selectedText()`; a snippet expects
    // real newlines.
    selection.replace(QChar::ParagraphSeparator, QLatin1Char('\n'));
    const SnippetInsertion insertion = resolveSnippet(snippet.body, selection);

    const QTextCursor cursor = editor->textCursor();
    const int start = cursor.selectionStart();
    QTextCursor edit = cursor;
    edit.beginEditBlock();
    edit.insertText(insertion.text);
    edit.endEditBlock();

    QTextCursor placed = editor->textCursor();
    placed.setPosition(start + insertion.caret);
    editor->setTextCursor(placed);
    editor->setFocus(Qt::ShortcutFocusReason);
    setExecutionState(tr("ready"), tr("Inserted \u201C%1\u201D").arg(snippet.name));
}

void MainWindow::showGuidedTourIfUnseen() {
    if (tour_ != nullptr) {
        tour_->start();
    }
}

void MainWindow::showGuidedTour() {
    if (tour_ != nullptr) {
        tour_->show();
    }
}

void MainWindow::rearmGuidedTour() {
    if (tour_ != nullptr) {
        tour_->rearm();
    }
}

void MainWindow::openChip8Emulator() {
    const bool created = chip8_window_ == nullptr;
    if (created) {
        auto* window = new Chip8Window{this};
        // Closing the emulator alone must end it; the QPointer then goes null,
        // so the next invocation builds a fresh one rather than using a dead
        // window.
        window->setAttribute(Qt::WA_DeleteOnClose);
        chip8_window_ = window;
    }
    chip8_window_->show();
    chip8_window_->raise();
    chip8_window_->activateWindow();

    // An emulator that already has a program is not overwritten: the learner may
    // have typed into the listing tab or run the game to a state worth keeping.
    // Opening the tool the first time loads the current document, or the bundled
    // paint program so the window is ready to run rather than blank.
    if (!created && chip8_window_->emulator()->loaded()) {
        return;
    }
    CodeEditor* editor = currentEditor();
    if (editor != nullptr && languageOf(editor) == Language::Chip8) {
        const QString path = pathOf(editor);
        if (path.isEmpty()) {
            chip8_window_->loadSource(editor->toPlainText());
        } else {
            chip8_window_->openFile(path);
        }
        return;
    }
    chip8_window_->loadBundledProgram();
}

void MainWindow::openRetroComputer() {
    const bool created = retro_computer_window_ == nullptr;
    if (created) {
        auto* window = new RetroComputerWindow{this};
        // Closing the machine alone must end it; the QPointer then goes null, so
        // the next invocation builds a fresh one rather than raising a dead
        // window. A live machine is never rebuilt, because its RAM is the point.
        window->setAttribute(Qt::WA_DeleteOnClose);
        retro_computer_window_ = window;

        CodeEditor* editor = currentEditor();
        if (editor != nullptr) {
            const Language language = languageOf(editor);
            if (language != Language::Unknown) {
                retro_computer_window_->setLanguage(language);
            }
            if (!editor->toPlainText().isEmpty()) {
                retro_computer_window_->loadProgram(editor->toPlainText());
                retro_computer_window_->showProgramPage();
            }
        }
    }
    retro_computer_window_->show();
    retro_computer_window_->raise();
    retro_computer_window_->activateWindow();
}

void MainWindow::goToLine(int line) {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        return;
    }
    QTextCursor cursor = editor->textCursor();
    cursor.movePosition(QTextCursor::Start);
    // Zero-based blocks, one-based lines: `line - 1` steps down from the start,
    // and Qt clamps at the last block for a number past the end.
    cursor.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor, std::max(0, line - 1));
    editor->setTextCursor(cursor);
    editor->centerCursor();
    editor->setFocus(Qt::ShortcutFocusReason);
}

void MainWindow::formatCurrentDocument() {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        return;
    }
    const FormatOutcome outcome =
        formatSource(editor->toPlainText(), languageOf(editor), editorSettings().tab_width);
    if (outcome.changed()) {
        // One edit block, so a single Ctrl+Z puts the whole reformat back: the
        // alternative is one undo step per line, which is worse than no undo.
        const int line = editor->textCursor().blockNumber();
        const int column = editor->textCursor().positionInBlock();
        QTextCursor edit = editor->textCursor();
        edit.beginEditBlock();
        edit.select(QTextCursor::Document);
        edit.insertText(outcome.text);
        edit.endEditBlock();

        // Put the cursor back by *line and column within the line*. An absolute
        // offset would land somewhere else as soon as an indent changed, which
        // is what the removed implementation did: it clamped to the document
        // length and dropped the caret at the end of the file.
        QTextCursor restored = editor->textCursor();
        restored.movePosition(QTextCursor::Start);
        restored.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor, line);
        const int reachable = std::min(column, restored.block().length() - 1);
        restored.movePosition(QTextCursor::StartOfBlock);
        restored.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, reachable);
        editor->setTextCursor(restored);
    }
    setExecutionState(tr("ready"), outcome.message);
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
    // `frameCount`, not `strokeCount`: CHIP-8 draws rectangles, so a screen full of
    // pixels has no strokes at all and would otherwise be "nothing to export".
    if (turtle_canvas_ == nullptr || turtle_canvas_->frameCount() == 0) {
        // Nothing drawn yet. Said rather than done silently: a save dialog that
        // produces an empty picture is worse than being told there is nothing.
        setExecutionState(tr("ready"),
                          tr("Nothing to export yet \u2014 run a program that draws"));
        return;
    }
    // SVG is listed only in a build that can write it, so the dialog never
    // offers a format whose export would fail.
    QString filters = tr("PNG image (*.png);;JPEG image (*.jpg *.jpeg);;Windows bitmap (*.bmp)");
#ifdef TIME_WARP_HAVE_SVG
    filters += tr(";;SVG image (*.svg)");
#endif
    const QString path =
        QFileDialog::getSaveFileName(this, tr("Export drawing"), QString{}, filters);
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

void MainWindow::exportCodePdfDialog() {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        setExecutionState(tr("ready"), tr("Open a document before exporting it"));
        return;
    }
    const QString title = nameOf(editor);
    const QString path = QFileDialog::getSaveFileName(
        this, tr("Export code as PDF"), suggestedExportPath(title, QStringLiteral("pdf")),
        tr("PDF document (*.pdf)"));
    if (path.isEmpty()) {
        return;
    }
    QString error;
    if (!timewarp::ui::exporter::writeCodePdf(path, title, displayName(languageOf(editor)),
                                              editor->toPlainText(), &error)) {
        QMessageBox::warning(this, tr("Export failed"),
                             tr("Could not write %1.\n\n%2")
                                 .arg(QDir::toNativeSeparators(path), error));
        return;
    }
    setExecutionState(tr("ready"), tr("Exported %1").arg(QFileInfo{path}.fileName()));
}

void MainWindow::exportSessionHtmlDialog() {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        setExecutionState(tr("ready"), tr("Open a document before exporting it"));
        return;
    }
    const QString title = nameOf(editor);
    const QString path = QFileDialog::getSaveFileName(
        this, tr("Export session as HTML"), suggestedExportPath(title, QStringLiteral("html")),
        tr("HTML page (*.html *.htm)"));
    if (path.isEmpty()) {
        return;
    }
    // The drawing at export size, or nothing at all: a blank canvas should say
    // so in the page rather than shipping an empty rectangle.
    const QImage drawing =
        (turtle_canvas_ != nullptr && turtle_canvas_->frameCount() > 0)
            ? turtle_canvas_->renderedDrawing()
            : QImage{};
    QString error;
    if (!timewarp::ui::exporter::writeSessionHtml(
            path, title, displayName(languageOf(editor)), editor->toPlainText(),
            console_view_ != nullptr ? console_view_->toPlainText() : QString{}, drawing,
            &error)) {
        QMessageBox::warning(this, tr("Export failed"),
                             tr("Could not write %1.\n\n%2")
                                 .arg(QDir::toNativeSeparators(path), error));
        return;
    }
    setExecutionState(tr("ready"), tr("Exported %1").arg(QFileInfo{path}.fileName()));
}

void MainWindow::printCode() {
#ifdef TIME_WARP_HAVE_PRINT
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        setExecutionState(tr("ready"), tr("Open a document before printing it"));
        return;
    }
    QPrinter printer{QPrinter::HighResolution};
    printer.setDocName(nameOf(editor));
    QPrintDialog dialog{&printer, this};
    dialog.setWindowTitle(tr("Print code"));
    if (dialog.exec() != QDialog::Accepted) {
        return;
    }
    timewarp::ui::exporter::printCode(printer, nameOf(editor),
                                      displayName(languageOf(editor)),
                                      editor->toPlainText());
    if (printer.outputFormat() == QPrinter::PdfFormat) {
        setExecutionState(tr("ready"), tr("Wrote %1")
                                            .arg(QFileInfo{printer.outputFileName()}.fileName()));
    } else {
        setExecutionState(tr("ready"), tr("Printed %1").arg(nameOf(editor)));
    }
#endif
}

void MainWindow::printDrawing() {
#ifdef TIME_WARP_HAVE_PRINT
    if (turtle_canvas_ == nullptr || turtle_canvas_->frameCount() == 0) {
        setExecutionState(tr("ready"),
                          tr("Nothing to print yet \u2014 run a program that draws"));
        return;
    }
    QPrinter printer{QPrinter::HighResolution};
    printer.setDocName(tr("Time Warp Studio drawing"));
    QPrintDialog dialog{&printer, this};
    dialog.setWindowTitle(tr("Print drawing"));
    if (dialog.exec() != QDialog::Accepted) {
        return;
    }
    // Rendered for the paper rather than for the screen: a screen-sized raster
    // scaled up to a page is soft, so ask the canvas for enough pixels to fill
    // the printable area. The cap keeps a very large printer from asking for an
    // image no one needs.
    const QRect area = printer.pageLayout().paintRectPixels(printer.resolution());
    const double factor =
        std::clamp(area.width() / static_cast<double>(std::max(1, turtle_canvas_->width())),
                   1.0, 8.0);
    QString error;
    if (!timewarp::ui::exporter::printDrawing(printer,
                                              turtle_canvas_->renderedDrawing(factor), &error)) {
        QMessageBox::warning(this, tr("Print failed"),
                             tr("Could not print the drawing.\n\n%1").arg(error));
        return;
    }
    if (printer.outputFormat() == QPrinter::PdfFormat) {
        setExecutionState(tr("ready"), tr("Wrote %1")
                                            .arg(QFileInfo{printer.outputFileName()}.fileName()));
    } else {
        setExecutionState(tr("ready"), tr("Printed the drawing"));
    }
#endif
}

void MainWindow::setGifRecording(bool on) {
    if (on) {
        gif_frames_.clear();
        gif_size_ = turtle_canvas_ != nullptr ? turtle_canvas_->size() : QSize{};
        gif_timer_->start();
        setExecutionState(tr("ready"),
                          tr("Recording the canvas \u2014 run a program, then stop to save"));
    } else {
        gif_timer_->stop();
        if (gif_frames_.isEmpty()) {
            setExecutionState(tr("ready"), tr("Nothing was drawn while recording"));
        } else {
            QString target = QFileDialog::getSaveFileName(
                this, tr("Save animated GIF"),
                QDir{QDir::homePath()}.filePath(QStringLiteral("turtle_recording.gif")),
                tr("Animated GIF (*.gif)"));
            if (target.isEmpty()) {
                setExecutionState(tr("ready"), tr("Recording discarded"));
            } else {
                // The dialog's filter is a suggestion on some platforms, and a GIF
                // without the suffix is a file the desktop will not preview.
                if (!target.endsWith(QStringLiteral(".gif"), Qt::CaseInsensitive)) {
                    target += QStringLiteral(".gif");
                }
                QString error;
                if (!timewarp::ui::gif::writeGif(target, gif_frames_, &error)) {
                    QMessageBox::warning(this, tr("Export failed"),
                                         tr("Could not write %1.\n\n%2")
                                             .arg(QDir::toNativeSeparators(target), error));
                } else {
                    setExecutionState(tr("ready"),
                                      tr("Wrote %1").arg(QFileInfo{target}.fileName()));
                }
            }
        }
        gif_frames_.clear();
    }
    if (record_gif_action_ != nullptr && record_gif_action_->isChecked() != on) {
        QSignalBlocker blocker{record_gif_action_};
        record_gif_action_->setChecked(on);
    }
}

void MainWindow::captureGifFrame() {
    if (turtle_canvas_ == nullptr) {
        return;
    }
    // A blank canvas is not a frame. Recording usually starts before Run, and a
    // stretch of empty paper at the front of the GIF is not what was recorded.
    if (gif_frames_.isEmpty() && turtle_canvas_->frameCount() <= 0) {
        return;
    }
    QImage frame = turtle_canvas_->renderedDrawing(1.0);
    if (gif_size_.isEmpty()) {
        gif_size_ = frame.size();
    } else if (frame.size() != gif_size_) {
        // A window resized mid-recording: scale rather than write frames whose
        // sizes disagree, which is a GIF no reader will play.
        frame = frame.scaled(gif_size_, Qt::IgnoreAspectRatio, Qt::SmoothTransformation);
    }
    // `appendFrame` merges a picture that did not change into the frame before
    // it: nothing moved, but the time still passed.
    timewarp::ui::gif::appendFrame(gif_frames_, frame, gif_timer_->interval());
}

timewarp::ui::exporter::LessonSession MainWindow::currentLessonSession() const {
    timewarp::ui::exporter::LessonSession session;
    if (lesson_panel_ != nullptr) {
        session = lesson_panel_->session();
    }
    // The program and the console are the window's, not the lesson's: the lesson
    // knows what was asked, and only the document knows what was written.
    if (const CodeEditor* editor = currentEditor(); editor != nullptr) {
        session.source = editor->toPlainText();
    }
    if (console_view_ != nullptr) {
        session.consoleOutput = console_view_->toPlainText();
    }
    return session;
}

void MainWindow::exportLessonSessionMarkdownDialog() {
    const timewarp::ui::exporter::LessonSession session = currentLessonSession();
    if (session.lessonTitle.isEmpty()) {
        setExecutionState(tr("ready"), tr("Open a lesson before exporting a session"));
        return;
    }
    const QString path = QFileDialog::getSaveFileName(
        this, tr("Export lesson session as Markdown"),
        suggestedExportPath(session.lessonTitle, QStringLiteral("md")),
        tr("Markdown file (*.md)"));
    if (path.isEmpty()) {
        return;
    }
    QString error;
    if (!timewarp::ui::exporter::writeLessonSessionMarkdown(path, session, &error)) {
        QMessageBox::warning(this, tr("Export failed"),
                             tr("Could not write %1.\n\n%2")
                                 .arg(QDir::toNativeSeparators(path), error));
        return;
    }
    setExecutionState(tr("ready"), tr("Exported %1").arg(QFileInfo{path}.fileName()));
}

void MainWindow::exportLessonSessionPdfDialog() {
    const timewarp::ui::exporter::LessonSession session = currentLessonSession();
    if (session.lessonTitle.isEmpty()) {
        setExecutionState(tr("ready"), tr("Open a lesson before exporting a session"));
        return;
    }
    const QString path = QFileDialog::getSaveFileName(
        this, tr("Export lesson session as PDF"),
        suggestedExportPath(session.lessonTitle, QStringLiteral("pdf")),
        tr("PDF document (*.pdf)"));
    if (path.isEmpty()) {
        return;
    }
    QString error;
    if (!timewarp::ui::exporter::writeLessonSessionPdf(path, session, &error)) {
        QMessageBox::warning(this, tr("Export failed"),
                             tr("Could not write %1.\n\n%2")
                                 .arg(QDir::toNativeSeparators(path), error));
        return;
    }
    setExecutionState(tr("ready"), tr("Exported %1").arg(QFileInfo{path}.fileName()));
}

void MainWindow::copyDrawingToClipboard() {
    if (turtle_canvas_ == nullptr || turtle_canvas_->frameCount() == 0) {
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

}  // namespace timewarp::ui
