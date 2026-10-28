// SPDX-License-Identifier: MIT

#include "main_window_internal.hpp"

#include "timewarp/ui/main_window.hpp"

#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/watch.hpp"
#include "timewarp/ui/native_runner.hpp"

#include <QAction>
#include <QApplication>
#include <QCloseEvent>
#include <QComboBox>
#include <QDateTime>
#include <QDir>
#include <QFile>
#include <QFileDialog>
#include <QFileInfo>
#include <QLabel>
#include <QMenu>
#include <QMessageBox>
#include <QPlainTextEdit>
#ifdef TIME_WARP_HAVE_PRINT
#endif
#include <QSaveFile>
#include <QSettings>
#include <QSignalBlocker>
#include <QSplitter>
#include <QStatusBar>
#include <QTabWidget>
#include <QTextDocument>

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
#include "timewarp/ui/gallery.hpp"
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
    // A child process is not a QThread: without this the shell outlives the
    // window that started it. After the save prompts, so cancelling a close
    // leaves a running shell running.
    if (terminal_widget_ != nullptr) {
        terminal_widget_->stop();
    }
    saveLayout();
    saveSession();
    event->accept();
}

void MainWindow::saveSession() const {
    // Only documents with a path: an untitled buffer has no name to reopen, and a
    // tab restored as "untitled 3" would be a document the user never had.
    QSettings settings;
    settings.setValue(kSettingOpenFiles, openFilePaths());
    settings.setValue(kSettingSecondaryFiles, secondaryFilePaths());
    settings.setValue(kSettingEditorSplit, editor_split_);
}

int MainWindow::restoreSession() {
    QSettings settings;
    const QStringList files = settings.value(kSettingOpenFiles).toStringList();
    int reopened = 0;
    for (const QString& path : files) {
        // Silently: a file that has been moved or deleted since the last session
        // is a missing file, not an error the user has to dismiss at startup --
        // which is also what makes this safe to call before `show()`.
        if (openFile(path, OpenFailure::Silent)) {
            ++reopened;
        }
    }

    // The other pane's documents, moved across after opening. Opening into the
    // primary and moving is deliberate: `openFile` is the one path that knows how
    // to load a document, and teaching it a target pane would give it a second
    // reason to change.
    const QStringList secondary = settings.value(kSettingSecondaryFiles).toStringList();
    editor_split_ = settings.value(kSettingEditorSplit, false).toBool();
    for (const QString& path : secondary) {
        if (!openFile(path, OpenFailure::Silent)) {
            continue;
        }
        if (CodeEditor* editor = currentEditor(); editor != nullptr) {
            moveEditorToPane(editor, editor_tabs_secondary_);
        }
        ++reopened;
    }
    // A saved split with nothing on the right is a blank pane, which reads as a
    // broken window. Show it only when there is a document in it.
    if (editor_split_ && editor_tabs_secondary_ != nullptr &&
        editor_tabs_secondary_->count() > 0) {
        editor_tabs_secondary_->show();
    } else {
        editor_split_ = false;
        if (editor_tabs_secondary_ != nullptr) {
            editor_tabs_secondary_->hide();
        }
    }
    // The project and the database are session state: the files and tables a
    // learner was working with come back with their documents.
    restoreProject();
    restoreSqlDatabase();
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

QStringList MainWindow::runHistory() const {
    return QSettings{}.value(kSettingRunHistory).toStringList();
}

void MainWindow::noteRun(const QString& source) {
    // One line per run: the path to reopen, the language to explain it, when it
    // ran, and enough of the first line to recognise it. The source itself is not
    // stored -- a settings file is not a document store, and a learner's settings
    // growing by a program per Run is not something to discover later.
    QString title;
    if (source.contains(QChar{0})) {
        // A binary ROM has no lines to quote, and half an opcode in the history
        // list is not a title.
        title = tr("binary ROM");
    } else {
        for (const QString& line : source.split(QLatin1Char('\n'))) {
            const QString trimmed = line.trimmed();
            if (!trimmed.isEmpty()) {
                title = trimmed.left(48);
                break;
            }
        }
    }
    if (title.isEmpty()) {
        return;  // an empty program is not a run worth remembering
    }
    const CodeEditor* editor = currentEditor();
    const QString path = editor != nullptr ? pathOf(editor) : QString{};
    const QString entry =
        QStringList{path.isEmpty() ? QStringLiteral("-") : path,
                    editor != nullptr ? settingsKey(languageOf(editor))
                                      : QStringLiteral("basic"),
                    QString::number(QDateTime::currentSecsSinceEpoch()),
                    (running_selection_ ? tr("selection: ") : QString{}) + title}
            .join(QLatin1Char('\t'));

    QStringList history = runHistory();
    history.removeAll(entry);
    history.prepend(entry);
    while (history.size() > kRunHistoryLimit) {
        history.removeLast();
    }
    QSettings{}.setValue(kSettingRunHistory, history);
    // The arrow on the Run button is easy to miss and there is no other place
    // that says the list exists. Once per installation is enough.
    if (tour_ != nullptr) {
        tour_->hint(QStringLiteral("run-history"));
    }
}

void MainWindow::rebuildRunMenu() {
    if (run_menu_ == nullptr) {
        return;
    }
    // Keep the three entries that are not history: Run, Stop, and the separator
    // that follows them. Rebuilding the whole menu would mean rebuilding those
    // too, and their shortcuts are already claimed.
    while (run_menu_->actions().size() > 2) {
        QAction* extra = run_menu_->actions().last();
        run_menu_->removeAction(extra);
        extra->deleteLater();
    }
    const QStringList history = runHistory();
    run_menu_->addSeparator();
    if (history.isEmpty()) {
        QAction* none = run_menu_->addAction(tr("(no runs yet)"));
        none->setEnabled(false);
        return;
    }
    for (const QString& entry : history) {
        const QStringList parts = entry.split(QLatin1Char('\t'));
        if (parts.size() < 4) {
            continue;
        }
        const QString path = parts.at(0);
        const QString title = parts.at(3);
        QAction* action = run_menu_->addAction(title);
        if (path != QLatin1String("-") && !QFileInfo{path}.exists()) {
            // Kept, but not runnable: the file moved. Saying so is better than
            // offering a click that opens a dialog about a missing file.
            action->setEnabled(false);
            action->setToolTip(tr("%1 is no longer there").arg(path));
            continue;
        }
        connect(action, &QAction::triggered, this, [this, path] {
            if (path != QLatin1String("-") && openFile(path)) {
                runCurrentDocument();
            }
        });
    }
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

/// The examples resolver used to live here. It is in `app_paths.cpp` now because
/// the CHIP-8 emulator starts on a bundled program and must find it the same way
/// File > Open Example does.

}  // namespace

void MainWindow::showExamplesGallery() {
    GalleryDialog gallery{this};
    connect(&gallery, &GalleryDialog::programChosen, this,
            [this](const QString& path) { openFile(path); });
    gallery.exec();
}

void MainWindow::rebuildExamplesMenu() {
    if (examples_menu_ == nullptr) {
        return;
    }
    examples_menu_->clear();
    const QDir root{examplesDir()};
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

QTabWidget* MainWindow::activeEditorTabs() const {
    // Focus, not a stored "active pane": the user's idea of the current document
    // is the one their cursor is in, and clicking a tab moves focus with it.
    if (editor_tabs_secondary_ != nullptr && editor_tabs_secondary_->isVisible()) {
        if (QWidget* focus = QApplication::focusWidget(); focus != nullptr &&
                                                          editor_tabs_secondary_->isAncestorOf(focus)) {
            return editor_tabs_secondary_;
        }
    }
    return editor_tabs_;
}

CodeEditor* MainWindow::currentEditor() const {
    return qobject_cast<CodeEditor*>(activeEditorTabs()->currentWidget());
}

void MainWindow::moveEditorToPane(CodeEditor* editor, QTabWidget* target) {
    if (editor == nullptr || target == nullptr) {
        return;
    }
    QTabWidget* source = target == editor_tabs_ ? editor_tabs_secondary_ : editor_tabs_;
    if (source == nullptr) {
        return;
    }
    const int index = source->indexOf(editor);
    if (index < 0) {
        // Not open at all: put it in the target rather than silently doing nothing.
        adoptEditor(editor);
        return;
    }
    const QString caption = source->tabText(index);
    source->removeTab(index);
    target->addTab(editor, caption);
    target->setCurrentWidget(editor);
}

void MainWindow::splitEditor() {
    if (editor_tabs_ == nullptr || editor_tabs_secondary_ == nullptr) {
        return;
    }
    if (CodeEditor* editor = currentEditor(); editor != nullptr) {
        moveEditorToPane(editor, editor_tabs_secondary_);
    }
    editor_tabs_secondary_->show();
    editor_split_ = true;
    // A split with an empty left half is a document and a blank rectangle. A fresh
    // document is what makes the split usable in the next keystroke.
    if (editor_tabs_->count() == 0) {
        newFile();
    }
    refreshDocumentState();
    setExecutionState(tr("ready"), tr("Editor split"));
}

void MainWindow::closeSplit() {
    if (editor_tabs_secondary_ == nullptr) {
        return;
    }
    // Documents first, then the pane: hiding it with tabs inside would leave them
    // open where nothing can reach them.
    while (editor_tabs_secondary_->count() > 0) {
        auto* editor = qobject_cast<CodeEditor*>(editor_tabs_secondary_->widget(0));
        const QString caption = editor_tabs_secondary_->tabText(0);
        editor_tabs_secondary_->removeTab(0);
        if (editor != nullptr) {
            editor_tabs_->addTab(editor, caption);
            editor_tabs_->setCurrentWidget(editor);
        }
    }
    editor_tabs_secondary_->hide();
    editor_split_ = false;
    refreshDocumentState();
    setExecutionState(tr("ready"), tr("Split closed"));
}

void MainWindow::moveTabToOtherPane() {
    if (editor_tabs_secondary_ == nullptr) {
        return;
    }
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        return;
    }
    if (editor_tabs_secondary_->isVisible() && editor_tabs_secondary_->count() == 1 &&
        editor_tabs_secondary_->indexOf(editor) >= 0) {
        // Moving the only document out of the split is closing it, which is what
        // the user meant whether or not they said so.
        closeSplit();
        return;
    }
    if (!editor_tabs_secondary_->isVisible()) {
        editor_tabs_secondary_->show();
        editor_split_ = true;
    }
    const bool in_secondary = editor_tabs_secondary_->indexOf(editor) >= 0;
    moveEditorToPane(editor, in_secondary ? static_cast<QTabWidget*>(editor_tabs_)
                                          : static_cast<QTabWidget*>(editor_tabs_secondary_));
    if (editor_tabs_secondary_->count() == 0) {
        editor_tabs_secondary_->hide();
        editor_split_ = false;
    }
    refreshDocumentState();
}

QStringList MainWindow::secondaryFilePaths() const {
    QStringList paths;
    if (editor_tabs_secondary_ == nullptr) {
        return paths;
    }
    for (int index = 0; index < editor_tabs_secondary_->count(); ++index) {
        const QString path = pathOf(qobject_cast<CodeEditor*>(editor_tabs_secondary_->widget(index)));
        if (!path.isEmpty()) {
            paths << path;
        }
    }
    return paths;
}

void MainWindow::runBothPanes() {
    auto* left = qobject_cast<CodeEditor*>(editor_tabs_->currentWidget());
    auto* right = editor_tabs_secondary_ != nullptr
                      ? qobject_cast<CodeEditor*>(editor_tabs_secondary_->currentWidget())
                      : nullptr;
    if (left == nullptr || right == nullptr || !editor_tabs_secondary_->isVisible()) {
        setExecutionState(tr("ready"),
                          tr("Open two documents side by side first \u2014 File > Split Editor Right"));
        return;
    }

    // Synchronous, and the wait cursor says so: the interpreter's own budget bounds
    // a runaway program, so the worst case is a wait rather than a hang, and two
    // live runners on one canvas could not attribute the ink to either program.
    QApplication::setOverrideCursor(Qt::WaitCursor);
    QList<BenchResult> results;
    results.append(runHeadless(languageOf(left), nameOf(left), left->toPlainText()));
    results.append(runHeadless(languageOf(right), nameOf(right), right->toPlainText()));
    QApplication::restoreOverrideCursor();

    clearOutput();
    appendOutput(comparisonTable(results));
    // Each program's own output, attributed: the table says *how much*, this says
    // *what*, and a benchmark that only gave numbers would make the two programs
    // indistinguishable the moment they printed anything.
    for (const BenchResult& result : results) {
        appendOutput(tr("\n%1 (%2):\n").arg(result.name, result.language));
        appendOutput(result.output.isEmpty() ? tr("  (no output)\n") : result.output + QLatin1Char('\n'));
    }
    setExecutionState(tr("ready"), tr("Compared both panes"));
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
        const themes::Colours resolved =
            themes::resolve(themes::byName(theme_name_), high_contrast_);
        SyntaxColors colours;
        colours.keyword = resolved.keyword;
        colours.comment = resolved.comment;
        colours.string = resolved.string;
        colours.number = resolved.number;
        colours.directive = resolved.function;
        editor->highlighter()->setColors(colours);
        editor->setDiagnosticColors(resolved.error_color, resolved.warning_color);
    }
    // And the editor's own customisations, for the same reason: a tab opened
    // after Preferences was used has to look like the tabs that were already
    // open.
    editor->applySettings(editor_settings_);

    // A document's breakpoints belong to its file, not to the tab: reopening a
    // program brings back the points set in an earlier session. An untitled
    // buffer keeps its set in memory only -- there is no path to hang it on.
    const QString document_path = pathOf(editor);
    if (!document_path.isEmpty()) {
        QSettings breakpoint_settings;
        editor->setBreakpoints(breakpoints::load(breakpoint_settings, document_path));
    }
    connect(editor, &CodeEditor::breakpointsChanged, this, [editor] {
        QSettings breakpoint_settings;
        breakpoints::save(breakpoint_settings, pathOf(editor), editor->breakpoints());
    });
    // The outline follows the text, on the same debounce as the checker.
    connect(editor->document(), &QTextDocument::contentsChanged, this, [this, editor] {
        if (editor == currentEditor() && outline_timer_ != nullptr) {
            outline_timer_->start();
        }
    });

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
        if (terminal_widget_ != nullptr) {
            terminal_widget_->setWorkingDirectory(QDir::homePath());
        }
        return;
    }

    const QString path = pathOf(editor);
    // The terminal follows the document, the way a build command should: the
    // shell is already there when the user switches to its tab.
    if (terminal_widget_ != nullptr) {
        terminal_widget_->setWorkingDirectory(path.isEmpty() ? QDir::homePath()
                                                             : QFileInfo{path}.absolutePath());
    }
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
        // Every language can stop now: the line-loop languages at a statement,
        // Logo at a top-level statement, Smalltalk at a message send and CHIP-8
        // at an instruction. The call stack remains the Scheme machine's own.
        debug_panel_->setAvailable(true, QString{});
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
    QByteArray bytes = file.readAll();
    file.close();

    const bool chip8 = languageForFile(absolute) == Language::Chip8;
    const bool binary_rom = chip8 && bytes.contains('\0');
    QString text;
    if (binary_rom) {
        // A .ch8 file may be a binary ROM. A document normalises '\r' to '\n',
        // so the bytes go through the codec in `language.hpp` rather than
        // through `fromLatin1`: that is what keeps 0x0D a 0x0D when the file is
        // run or saved back.
        text = romBytesToText(bytes);
    } else {
        if (chip8 && bytes.startsWith("\xEF\xBB\xBF")) {
            bytes.remove(0, 3);
        }
        text = chip8 ? QString::fromLatin1(bytes) : QString::fromUtf8(bytes);
    }
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
    // A ROM is bytecode, not editable source. Showing it in the editor is useful
    // (it is the thing Run executes), but a keystroke in it would be a corrupted
    // program waiting to be saved.
    editor->setReadOnly(binary_rom);
    updateTabLabel(editor);
    editor->setFocus();
    noteRecentFile(absolute);

    setExecutionState(tr("ready"),
                      binary_rom
                          ? tr("Opened %1 as a binary ROM (read-only)")
                                .arg(QFileInfo{absolute}.fileName())
                          : tr("Opened %1").arg(QFileInfo{absolute}.fileName()));
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
    // A binary ROM was decoded through the codec that preserves every byte;
    // writing it as UTF-8 would turn every high byte into two and the program
    // would no longer be the one loaded.
    const QString text = editor->toPlainText();
    const QByteArray bytes = languageOf(editor) == Language::Chip8 &&
                                     text.contains(QChar{0})
                                 ? romTextToBytes(text)
                                 : text.toUtf8();
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
    // Breakpoints follow the document to its new name: they were set against this
    // program, and Save As is the same program.
    QSettings breakpoint_settings;
    breakpoints::save(breakpoint_settings, path, editor->breakpoints());
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

}  // namespace timewarp::ui
