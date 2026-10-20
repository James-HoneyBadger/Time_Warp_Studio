// SPDX-License-Identifier: MIT

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

namespace {

/// Keys under which the layout is persisted. Grouped in one place so a rename
/// cannot leave half the settings orphaned.
constexpr auto kSettingGeometry = "layout/geometry";
constexpr auto kSettingWindowState = "layout/windowState";
constexpr auto kSettingSplitters = "layout/splitters";
/// The documents that were open, so the next launch can reopen them.
constexpr auto kSettingOpenFiles = "session/openFiles";
/// The second pane's documents, kept apart so a restart puts each one back on the
/// side it was on.
constexpr auto kSettingSecondaryFiles = "session/secondaryFiles";
constexpr auto kSettingEditorSplit = "session/editorSplit";
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
constexpr auto kSettingFontScale = "display/fontScale";
// The CRT effects. One key each, like the editor's settings, so a value can be
// read and corrected by hand.
constexpr auto kSettingCrtScanlines = "display/crt/scanlines";
constexpr auto kSettingCrtVignette = "display/crt/vignette";
constexpr auto kSettingCrtCurvature = "display/crt/curvature";
constexpr auto kSettingCrtFlicker = "display/crt/flicker";
constexpr auto kSettingCrtPhosphor = "display/crt/phosphor";
constexpr auto kSettingEditorTabWidth = "editor/tabWidth";
constexpr auto kSettingEditorLineNumbers = "editor/showLineNumbers";
constexpr auto kSettingEditorCurrentLine = "editor/highlightCurrentLine";
constexpr auto kSettingEditorWordWrap = "editor/wordWrap";
constexpr auto kSettingEditorAutoIndent = "editor/autoIndent";
constexpr auto kSettingEditorCompletion = "editor/completion";
constexpr auto kSettingEditorLint = "editor/lint";
constexpr auto kSettingRunHistory = "run/history";
constexpr auto kRunHistoryLimit = 10;
constexpr auto kSettingEditorWhitespace = "editor/showWhitespace";
constexpr auto kSettingEditorAutosave = "editor/autosave";
constexpr auto kSettingEditorAutosaveSeconds = "editor/autosaveSeconds";
constexpr auto kSettingEditorMinimap = "editor/minimap";
/// Off unless the user asked for it: a shell in a teaching IDE runs whatever is
/// typed in it, with the user's permissions.
constexpr auto kSettingTerminalEnabled = "terminal/enabled";
/// The project that was open last session, so a class's work comes back as one
/// thing rather than as a pile of tabs. The recent list is separate: the active
/// project must survive a Clear Menu.
constexpr auto kSettingActiveProject = "project/active";
constexpr auto kSettingRecentProjects = "project/recentProjects";
constexpr int kRecentProjectLimit = 10;
/// The database the SQL page has open, and the databases it has opened before.
constexpr auto kSettingSqlDatabase = "sql/database";
constexpr auto kSettingRecentDatabases = "sql/recentDatabases";
/// The debugger's watch expressions and data breakpoints. Session-wide rather
/// than per document: "watch X" is a question about a run, and the same question
/// is usually asked of the next run too.
constexpr auto kSettingWatches = "debug/watches";
constexpr auto kSettingDataBreakpoints = "debug/dataBreakpoints";
constexpr int kRecentDatabaseLimit = 8;

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

/// A save-dialog suggestion built from a document's name: the extension is
/// replaced rather than appended, so exporting `hello.bas` as a PDF offers
/// `hello.pdf` and not `hello.bas.pdf`.
[[nodiscard]] QString suggestedExportPath(const QString& documentName, const QString& suffix) {
    QString name = documentName;
    const int dot = name.lastIndexOf(QLatin1Char('.'));
    if (dot > 0) {
        name.truncate(dot);
    }
    if (name.isEmpty()) {
        name = QStringLiteral("program");
    }
    return name + QLatin1Char('.') + suffix;
}

/// The number and body of a BASIC source line, for direct-mode entry.
struct BasicProgramLine {
    int number = 0;
    QString body;
};

/// Reads `10 PRINT "HI"` or `10` (a deletion). Returns nullopt when the text is
/// not a numbered BASIC line: the body must begin with a letter, `?`, or a
/// comment mark, so `2 + 2` stays an immediate expression rather than becoming
/// line 2.
[[nodiscard]] std::optional<BasicProgramLine> parseBasicProgramLine(const QString& text) {
    int index = 0;
    while (index < text.size() && text.at(index).isSpace()) {
        ++index;
    }
    const int digits_begin = index;
    while (index < text.size() && text.at(index).isDigit()) {
        ++index;
    }
    if (index == digits_begin) {
        return std::nullopt;
    }
    bool ok = false;
    const int number = text.mid(digits_begin, index - digits_begin).toInt(&ok);
    if (!ok || number < 0) {
        return std::nullopt;
    }
    while (index < text.size() && text.at(index).isSpace()) {
        ++index;
    }
    const QString body = text.mid(index);
    const QString first = body.trimmed();
    if (!first.isEmpty() && !first.front().isLetter() && first.front() != QLatin1Char('?') &&
        first.front() != QLatin1Char('\'')) {
        return std::nullopt;
    }
    return BasicProgramLine{number, body};
}

/// The BASIC shorthand `?` means PRINT, including after a line number.
[[nodiscard]] QString withPrintShorthand(QString body) {
    const QString trimmed = body.trimmed();
    if (!trimmed.startsWith(QLatin1Char('?'))) {
        return body;
    }
    QString rest = trimmed.mid(1).trimmed();
    return rest.isEmpty() ? QStringLiteral("PRINT")
                          : QStringLiteral("PRINT ") + rest;
}

/// Splits a document into source lines without the trailing empty one.
[[nodiscard]] QStringList programLines(const QString& text) {
    QStringList lines = text.split(QLatin1Char('\n'));
    if (!lines.isEmpty() && lines.last().isEmpty()) {
        lines.removeLast();
    }
    return lines;
}

/// The document text with `body` stored at `number`, replacing an existing line
/// or inserting in numeric order.
[[nodiscard]] QString withBasicProgramLine(const QString& text, int number,
                                           const QString& body) {
    QStringList lines = programLines(text);
    int insert_at = lines.size();
    for (int index = 0; index < lines.size(); ++index) {
        const std::optional<BasicProgramLine> existing = parseBasicProgramLine(lines.at(index));
        if (!existing.has_value()) {
            continue;
        }
        if (existing->number == number) {
            lines[index] = QStringLiteral("%1 %2").arg(number).arg(body.trimmed());
            return lines.join(QLatin1Char('\n')) + QLatin1Char('\n');
        }
        if (existing->number > number) {
            insert_at = index;
            break;
        }
    }
    lines.insert(insert_at, QStringLiteral("%1 %2").arg(number).arg(body.trimmed()));
    return lines.join(QLatin1Char('\n')) + QLatin1Char('\n');
}

/// The document text with `number` removed. No change when the line is absent.
[[nodiscard]] QString withoutBasicProgramLine(const QString& text, int number) {
    QStringList lines = programLines(text);
    for (int index = 0; index < lines.size(); ++index) {
        const std::optional<BasicProgramLine> existing = parseBasicProgramLine(lines.at(index));
        if (existing.has_value() && existing->number == number) {
            lines.removeAt(index);
            break;
        }
    }
    return lines.isEmpty() ? QString{} : lines.join(QLatin1Char('\n')) + QLatin1Char('\n');
}

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
    VersionHistoryDialog dialog{autosave_, document, this};
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

    auto* turtle_log = new QPlainTextEdit{output_sub_tabs_};
    turtle_log->setObjectName(QStringLiteral("turtleLog"));
    turtle_log->setReadOnly(true);
    turtle_log->setFont(QFont{QStringLiteral("Courier New"), 10});
    turtle_log->setAccessibleName(tr("Turtle Command Log"));
    turtle_log->setPlaceholderText(tr("Turtle \U0001F422 commands appear here\u2026"));
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

    scrubber_ = new Scrubber{canvas_pane};
    // Hidden until there is ink: an empty window should not offer to replay a
    // drawing that does not exist, and the canvas gets the height instead.
    scrubber_->setVisible(false);
    canvas_column->addWidget(scrubber_);

    output_canvas_splitter_->addWidget(canvas_pane);
    output_canvas_splitter_->setSizes({kOutputHeight, kCanvasHeight});

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
    // Ctrl+Alt+V, the shortcut the removed implementation used, and one no other
    // action here claims.
    add(file, tr("&Version History..."), QKeySequence{Qt::CTRL | Qt::ALT | Qt::Key_V},
        [this] { showVersionHistory(); })
        ->setObjectName(QStringLiteral("versionHistoryAction"));
    file->addSeparator();
    add(file, tr("&Export Drawing..."), QKeySequence{Qt::CTRL | Qt::Key_E},
        [this] { exportDrawingDialog(); });
    // Qt Gui carries QPdfWriter, so the PDF needs no component beyond the ones
    // this application already requires.
    add(file, tr("Export Code as &PDF..."), QKeySequence{},
        [this] { exportCodePdfDialog(); })
        ->setObjectName(QStringLiteral("exportPdfAction"));
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
    add(file, tr("Split Editor &Right"), QKeySequence{Qt::CTRL | Qt::Key_Backslash},
        [this] { splitEditor(); })
        ->setObjectName(QStringLiteral("splitEditorAction"));
    add(file, tr("Close S&plit"), QKeySequence{Qt::CTRL | Qt::SHIFT | Qt::Key_Backslash},
        [this] { closeSplit(); })
        ->setObjectName(QStringLiteral("closeSplitAction"));
    add(file, tr("&Move Tab to Other Pane"), QKeySequence{Qt::CTRL | Qt::ALT | Qt::Key_M},
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
        QKeySequence{Qt::CTRL | Qt::SHIFT | Qt::Key_N}, [this] { newProjectDialog(); })
        ->setObjectName(QStringLiteral("newProjectAction"));
    add(project_menu, tr("&Open Project..."),
        QKeySequence{Qt::CTRL | Qt::SHIFT | Qt::Key_O}, [this] { openProjectDialog(); })
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
    add(project_menu, tr("New &Group..."), QKeySequence{Qt::CTRL | Qt::SHIFT | Qt::Key_G},
        [this] { addGroupToProject(); })
        ->setObjectName(QStringLiteral("newProjectGroupAction"));
    add(project_menu, tr("Rename Project..."), QKeySequence{},
        [this] { renameProject(); })
        ->setObjectName(QStringLiteral("renameProjectAction"));
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
    // The repository-wide search the removed implementation had and this one did
    // not. Ctrl+Shift+F, which no other action uses.
    add(edit, tr("Find in F&iles..."), QKeySequence{Qt::CTRL | Qt::SHIFT | Qt::Key_F},
        [this] { showFindInFiles(); })
        ->setObjectName(QStringLiteral("findInFilesAction"));
    // Ctrl+Shift+I, which no other action claims.
    add(edit, tr("Insert &Snippet..."), QKeySequence{Qt::CTRL | Qt::SHIFT | Qt::Key_I},
        [this] { insertSnippet(); })
        ->setObjectName(QStringLiteral("insertSnippetAction"));
    edit->addSeparator();
    // Ctrl+L, not Ctrl+G: the grid on the canvas already owns Ctrl+G, and two
    // actions on one key is an ambiguous shortcut that Qt resolves by guessing.
    add(edit, tr("&Go to Line..."), QKeySequence{Qt::CTRL | Qt::Key_L},
        [this] { goToLinePrompt(); });
    // Ctrl+I, which no other action claims. There was no Format Code at all
    // before this; the removed implementation had one under Edit.
    add(edit, tr("&Format Code"), QKeySequence{Qt::CTRL | Qt::Key_I},
        [this] { formatCurrentDocument(); })
        ->setObjectName(QStringLiteral("formatCodeAction"));
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
    add(run, tr("Run &Selection"), QKeySequence{Qt::CTRL | Qt::Key_F9},
        [this] { runSelection(); })
        ->setObjectName(QStringLiteral("runSelectionAction"));
    add(run, tr("&Stop"), QKeySequence{Qt::SHIFT | Qt::Key_F9},
        [this] { stopRunningDocument(); });
    // A comparison, not a race: both programs run headless, one after the other,
    // and the table attributes every number to the program that produced it.
    add(run, tr("Run &Both Panes"), QKeySequence{Qt::CTRL | Qt::ALT | Qt::Key_B},
        [this] { runBothPanes(); })
        ->setObjectName(QStringLiteral("runBothPanesAction"));
    run->addSeparator();
    // Reset is not Stop: Stop ends a program that is still running, Reset puts
    // the screen back to how it looked before a program that has already ended
    // -- which is the thing a learner presses after a drawing they want to redo.
    add(run, tr("&Reset Program"), QKeySequence{Qt::CTRL | Qt::SHIFT | Qt::Key_F9},
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
        QKeySequence{Qt::CTRL | Qt::SHIFT | Qt::Key_M}, [this] { openChip8Emulator(); })
        ->setObjectName(QStringLiteral("chip8EmulatorAction"));
    // The machine the languages share. Ctrl+Alt+W is unclaimed and is the
    // chord the machine has always been documented with, so renaming it to the
    // HB-1 does not move it out from under anyone's fingers.
    add(tools, tr("&HB-1 Computer..."),
        QKeySequence{Qt::CTRL | Qt::ALT | Qt::Key_W}, [this] { openRetroComputer(); })
        ->setObjectName(QStringLiteral("retroComputerAction"));

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
    high_contrast_action_->setShortcut(QKeySequence{Qt::CTRL | Qt::ALT | Qt::Key_H});
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
    presentation_action_->setShortcut(QKeySequence{Qt::SHIFT | Qt::Key_F11});
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
        QKeySequence{Qt::CTRL | Qt::SHIFT | Qt::Key_E},
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
        QKeySequence{Qt::CTRL | Qt::SHIFT | Qt::Key_P},
        [this] { showCommandPalette(); })
        ->setObjectName(QStringLiteral("commandPaletteAction"));
    buildThemeMenu(view);

    // -- Help -------------------------------------------------------------
    QMenu* help = bar->addMenu(tr("&Help"));
    help->setObjectName(QStringLiteral("helpMenu"));
    add(help, tr("&Context Help"), QKeySequence{Qt::Key_F1},
        [this] { showContextHelp(); })
        ->setObjectName(QStringLiteral("contextHelpAction"));
    add(help, tr("&Keyword Inspector..."), QKeySequence{Qt::CTRL | Qt::Key_F1},
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
               "<p>An old-school programming environment for ten languages: BASIC, "
               "APL, Logo, PILOT, FOCAL, Scheme, CHIP-8, COMAL, REXX and Smalltalk, "
               "with an integrated turtle graphics canvas — and two complete "
               "machines inside it: the HB-1 home computer and a live CHIP-8."
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
        app->setPalette(themes::paletteFor(theme, high_contrast_));
    }
    // The base sheet reads `palette(...)`, so it follows the palette above; the
    // theme's own block adds what a role cannot express.
    setStyleSheet(style::window() + themes::stylesheetFor(theme, high_contrast_));

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

void MainWindow::setFontScale(int percent) {
    // Snap to the ladder rather than trusting the caller: the three steps are the
    // ones that were laid out and looked at, and a fourth size is a settings value
    // nobody designed.
    int snapped = kFontScales.front();
    for (const int step : kFontScales) {
        if (percent >= step) {
            snapped = step;
        }
    }
    if (editor_settings_.font_scale_percent == snapped) {
        return;
    }
    editor_settings_.font_scale_percent = snapped;
    saveEditorSettings();
    // One call applies all three surfaces: the editors through their own settings,
    // the console and logs through the same number, and the panels through the
    // application font that everything without an explicit font inherits.
    applyEditorSettings(editor_settings_);
    setExecutionState(tr("ready"), tr("Text size %1%").arg(snapped));
}

void MainWindow::cycleFontScale() {
    const int current = editor_settings_.font_scale_percent;
    const auto at = std::find(kFontScales.begin(), kFontScales.end(), current);
    if (at == kFontScales.end() || std::next(at) == kFontScales.end()) {
        setFontScale(kFontScales.front());
        return;
    }
    setFontScale(*std::next(at));
}

CrtOverlay::Settings MainWindow::loadCrtSettings() const {
    QSettings settings;
    // Off unless the user turned an effect on: the overlay used to start hidden and
    // a fresh install must keep looking like a modern IDE rather than a 1983
    // terminal -- the retro look is a choice, which is what the settings are for.
    CrtOverlay::Settings loaded;
    loaded.scanlines = settings.value(kSettingCrtScanlines, 0).toInt();
    loaded.vignette = settings.value(kSettingCrtVignette, 0).toInt();
    loaded.curvature = settings.value(kSettingCrtCurvature, 0).toInt();
    loaded.flicker = settings.value(kSettingCrtFlicker, 0).toInt();
    loaded.phosphor = settings.value(kSettingCrtPhosphor, 0).toInt();
    return loaded;
}

void MainWindow::saveCrtSettings() const {
    QSettings settings;
    settings.setValue(kSettingCrtScanlines, crt_settings_.scanlines);
    settings.setValue(kSettingCrtVignette, crt_settings_.vignette);
    settings.setValue(kSettingCrtCurvature, crt_settings_.curvature);
    settings.setValue(kSettingCrtFlicker, crt_settings_.flicker);
    settings.setValue(kSettingCrtPhosphor, crt_settings_.phosphor);
}

void MainWindow::setCrtSettings(const CrtOverlay::Settings& settings) {
    crt_settings_ = settings;
    if (crt_overlay_ != nullptr) {
        crt_overlay_->setSettings(crt_settings_);
    }
    // Showing the overlay is part of applying settings: a preview the user cannot
    // see is not a preview, and the menu's tick has to agree with the screen.
    const bool on = crt_settings_.scanlines > 0 || crt_settings_.vignette > 0 ||
                    crt_settings_.curvature > 0 || crt_settings_.flicker > 0 ||
                    crt_settings_.phosphor > 0;
    if (crt_overlay_ != nullptr) {
        crt_overlay_->setVisible(on);
    }
    if (crt_action_ != nullptr && crt_action_->isChecked() != on) {
        QSignalBlocker blocker{crt_action_};
        crt_action_->setChecked(on);
    }
}

void MainWindow::showCrtSettings() {
    if (crt_dialog_ != nullptr) {
        crt_dialog_->raise();
        crt_dialog_->activateWindow();
        return;
    }
    auto* dialog = new CrtSettingsDialog{crt_settings_, this};
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    crt_dialog_ = dialog;
    crt_before_dialog_ = crt_settings_;
    // The preview is the real overlay, not a swatch in the dialog: a settings
    // window that shows its own mock-up can be right while the window is wrong.
    connect(dialog, &CrtSettingsDialog::previewRequested, this,
            [this](const CrtOverlay::Settings& preview) { setCrtSettings(preview); });
    connect(dialog, &QDialog::accepted, this, [this, dialog] {
        setCrtSettings(dialog->settings());
        saveCrtSettings();
        setExecutionState(tr("ready"), tr("CRT settings saved"));
    });
    connect(dialog, &QDialog::rejected, this, [this] {
        // Back to what was in force before the dialog opened, including the
        // overlay's visibility.
        setCrtSettings(crt_before_dialog_);
    });
    connect(dialog, &QObject::destroyed, this, [this] { crt_dialog_ = nullptr; });
    dialog->open();
}

void MainWindow::applyScreenPreset(ScreenPreset preset) {
    // One table, so a preset cannot half-apply and a new one is a row rather than
    // a code path. The themes are the ones the monochrome tube sets were built
    // from, and the CRT values are what make them read as a screen rather than as
    // a colour scheme.
    CrtOverlay::Settings crt;
    crt.scanlines = 0;
    crt.vignette = 0;
    crt.curvature = 0;
    crt.flicker = 0;
    crt.phosphor = 0;
    QString theme = QStringLiteral("VS Code Dark");
    QString label = tr("Normal display");

    switch (preset) {
        case ScreenPreset::Normal:
            break;
        case ScreenPreset::Amber:
            theme = QStringLiteral("Amber Monochrome");
            crt.scanlines = 25;
            crt.vignette = 30;
            crt.phosphor = 30;
            crt.phosphor_colour = QColor{255, 176, 0};
            label = tr("Amber monitor");
            break;
        case ScreenPreset::Green:
            theme = QStringLiteral("Green Monochrome");
            crt.scanlines = 25;
            crt.vignette = 30;
            crt.phosphor = 30;
            crt.phosphor_colour = QColor{0, 255, 0};
            label = tr("Green monitor");
            break;
        case ScreenPreset::White:
            theme = QStringLiteral("High Contrast Dark");
            crt.scanlines = 20;
            crt.curvature = 35;
            crt.phosphor = 20;
            crt.phosphor_colour = QColor{235, 235, 235};
            label = tr("White phosphor monitor");
            break;
    }

    applyTheme(theme);
    setCrtSettings(crt);
    saveCrtSettings();
    setExecutionState(tr("ready"), label);
}

void MainWindow::setFocusMode(bool on) {
    if (focus_mode_ == on) {
        return;
    }
    // Both modes own Escape, so the last one asked for wins rather than both
    // being on and the key being ambiguous. This is the other half of the same
    // rule in `setPresentationMode`.
    if (on && presentation_mode_) {
        setPresentationMode(false);
    }
    focus_mode_ = on;

    if (on) {
        // Only what was actually visible is recorded, so leaving does not pop up a
        // panel the user had closed before entering the mode.
        QList<QWidget*> chrome{tool_bar_, tool_bar_secondary_, function_key_bar_,
                               statusBar()};
        // Docks are the panels, and their titles are part of the chrome.
        for (QDockWidget* dock : findChildren<QDockWidget*>()) {
            chrome.append(dock);
        }
        focus_hidden_.clear();
        for (QWidget* widget : chrome) {
            if (widget != nullptr && widget->isVisible()) {
                focus_hidden_.append(QPointer<QWidget>{widget});
                widget->setVisible(false);
            }
        }
    } else {
        for (const QPointer<QWidget>& guarded : focus_hidden_) {
            // Through a raw pointer: GCC's `-Wnull-dereference` reads
            // `QPointer::operator->` as possibly null whatever the guard.
            QWidget* widget = guarded.data();
            if (widget != nullptr) {
                widget->setVisible(true);
            }
        }
        focus_hidden_.clear();
    }

    if (focus_shortcut_ != nullptr) {
        focus_shortcut_->setEnabled(on);
    }
    if (focus_action_ != nullptr && focus_action_->isChecked() != on) {
        QSignalBlocker blocker{focus_action_};
        focus_action_->setChecked(on);
    }
    setExecutionState(tr("ready"), on ? tr("Focus mode \u2014 press Esc to leave")
                                      : tr("Focus mode off"));
}

void MainWindow::setPresentationSettings(int fontPoints, bool fullscreen) {
    presentation_font_ = fontPoints;
    presentation_fullscreen_ = fullscreen;
    applyPresentationSettings();
}

void MainWindow::applyPresentationSettings() {
    if (!presentation_mode_) {
        return;
    }
    if (presentation_font_ > 0) {
        if (lesson_panel_ != nullptr) {
            lesson_panel_->setPresentationFont(presentation_font_);
        }
        // The console and the two logs, at the projection size rather than the
        // learner's ladder. Leaving the mode puts them back through
        // `applyEditorSettings`, which is what owns them.
        for (const QString& name : {QStringLiteral("consoleOutput"),
                                    QStringLiteral("errorsLog"),
                                    QStringLiteral("turtleLog")}) {
            const QList<QPlainTextEdit*> logs = findChildren<QPlainTextEdit*>(name);
            for (QPlainTextEdit* log : logs) {
                QFont font = log->font();
                font.setPointSize(presentation_font_);
                log->setFont(font);
            }
        }
    }
    if (presentation_fullscreen_) {
        if (!isFullScreen()) {
            showFullScreen();
            // Only a change this mode made is undone by leaving it; a window the
            // user had already made full screen is their choice.
            presentation_restore_geometry_ = true;
        }
    } else if (presentation_restore_geometry_ && isFullScreen()) {
        // The class says windowed and this mode made it full screen: put it back
        // but stay in the mode, which is what `teach.py present --windowed` means.
        presentation_restore_geometry_ = false;
        if (presentation_was_maximized_) {
            showMaximized();
        } else {
            showNormal();
        }
    }
}

void MainWindow::setPresentationMode(bool on) {
    if (presentation_mode_ == on) {
        return;
    }
    // Both modes own Escape, and two owners make it ambiguous. Presentation is the
    // stronger of the two -- it is the one with an audience -- so entering it ends
    // focus mode; leaving it returns to the plain window.
    if (on && focus_mode_) {
        setFocusMode(false);
    }
    presentation_mode_ = on;
    if (on) {
        // The menu bar goes too: focus mode keeps it for the person at the
        // keyboard, but a room does not need File and Edit above the lesson.
        QList<QWidget*> chrome{menuBar(),         tool_bar_, tool_bar_secondary_,
                               function_key_bar_, statusBar()};
        for (QDockWidget* dock : findChildren<QDockWidget*>()) {
            chrome.append(dock);
        }
        presentation_hidden_.clear();
        for (QWidget* widget : chrome) {
            if (widget != nullptr && widget->isVisible()) {
                presentation_hidden_.append(QPointer<QWidget>{widget});
                widget->setVisible(false);
            }
        }
        presentation_was_maximized_ = isMaximized();
        presentation_restore_geometry_ = false;
        // The class's size may not have been read yet, and presentation mode is
        // reachable without opening a panel first. One request settles it; the
        // answer applies to the mode already on screen.
        if (presentation_font_ <= 0 && ensureBridge()) {
            static_cast<void>(python_worker_->requestClassroomPresentation());
        }
        applyPresentationSettings();
    } else {
        for (const QPointer<QWidget>& guarded : presentation_hidden_) {
            QWidget* widget = guarded.data();
            if (widget != nullptr) {
                widget->setVisible(true);
            }
        }
        presentation_hidden_.clear();
        if (presentation_restore_geometry_) {
            presentation_restore_geometry_ = false;
            if (presentation_was_maximized_) {
                showMaximized();
            } else {
                showNormal();
            }
        }
        // The console goes back through the settings that own it, so the learner's
        // scale is what returns rather than a guessed default.
        applyEditorSettings(editor_settings_);
    }
    if (presentation_shortcut_ != nullptr) {
        presentation_shortcut_->setEnabled(on);
    }
    if (presentation_action_ != nullptr && presentation_action_->isChecked() != on) {
        QSignalBlocker blocker{presentation_action_};
        presentation_action_->setChecked(on);
    }
    // No shell in front of a room: the tab leaves the window, and a shell that
    // was running is stopped rather than left as an invisible process.
    refreshTerminalTab();
    setExecutionState(tr("ready"), on ? tr("Presentation mode \u2014 press Esc to leave")
                                      : tr("Presentation mode off"));
}

void MainWindow::setHighContrast(bool on) {
    if (high_contrast_ == on) {
        // Still re-applied: a caller that asks for the state the window is already
        // in wants the state, not a no-op, and a second `applyTheme` is cheap.
        applyTheme(theme_name_);
        return;
    }
    high_contrast_ = on;
    // Re-applying the same theme with the flag set is what makes this a *variant*
    // rather than a theme: the colours were chosen by the user and are not
    // replaced, only pushed until they can be read.
    applyTheme(theme_name_);
    if (high_contrast_action_ != nullptr && high_contrast_action_->isChecked() != on) {
        QSignalBlocker blocker{high_contrast_action_};
        high_contrast_action_->setChecked(on);
    }
    setExecutionState(tr("ready"), on ? tr("High contrast on") : tr("High contrast off"));
}

void MainWindow::applyThemeToWidgets() {
    const Theme& theme = themes::byName(theme_name_);
    const themes::Colours resolved = themes::resolve(theme, high_contrast_);

    // Syntax colours. The highlighter's five roles against the theme's six: the
    // theme's `function` is the closest thing it has to a "directive" (PILOT's
    // unit letters, Scheme's labels), which is what that slot means here.
    SyntaxColors colours;
    colours.keyword = resolved.keyword;
    colours.comment = resolved.comment;
    colours.string = resolved.string;
    colours.number = resolved.number;
    colours.directive = resolved.function;

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
    const QColor surface = resolved.canvas_bg;
    if (turtle_canvas_ != nullptr) {
        turtle_canvas_->setSurfaceColor(surface);
    }

    // The two verdict colours, which the theme carries for exactly this reason:
    // the panel's shipped green and red are unreadable on the phosphor themes.
    if (lesson_panel_ != nullptr) {
        lesson_panel_->setVerdictColors(resolved.success_color, resolved.error_color);
    }

    // The problem markers. A red squiggle on the red-phosphor or amber themes is
    // exactly the colour of everything else, so the marker colours come from the
    // theme too -- the error colour it carries for output, and its warning colour.
    const auto recolour_markers = [&resolved](QTabWidget* tabs) {
        if (tabs == nullptr) {
            return;
        }
        for (int index = 0; index < tabs->count(); ++index) {
            if (auto* editor = qobject_cast<CodeEditor*>(tabs->widget(index));
                editor != nullptr) {
                editor->setDiagnosticColors(resolved.error_color, resolved.warning_color);
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
    loaded.font_scale_percent = settings.value(kSettingFontScale, 100).toInt();
    loaded.tab_width = settings.value(kSettingEditorTabWidth, 4).toInt();
    loaded.show_line_numbers = settings.value(kSettingEditorLineNumbers, true).toBool();
    loaded.highlight_current_line =
        settings.value(kSettingEditorCurrentLine, true).toBool();
    loaded.word_wrap = settings.value(kSettingEditorWordWrap, false).toBool();
    loaded.auto_indent = settings.value(kSettingEditorAutoIndent, true).toBool();
    loaded.completion = settings.value(kSettingEditorCompletion, false).toBool();
    loaded.lint = settings.value(kSettingEditorLint, true).toBool();
    loaded.show_whitespace = settings.value(kSettingEditorWhitespace, false).toBool();
    loaded.minimap = settings.value(kSettingEditorMinimap, true).toBool();
    loaded.autosave = settings.value(kSettingEditorAutosave, true).toBool();
    loaded.autosave_seconds = settings.value(kSettingEditorAutosaveSeconds, 120).toInt();
    editor_settings_ = loaded;
}

void MainWindow::saveEditorSettings() const {
    QSettings settings;
    settings.setValue(kSettingEditorFont, editor_settings_.font_family);
    settings.setValue(kSettingEditorFontSize, editor_settings_.font_point_size);
    settings.setValue(kSettingFontScale, editor_settings_.font_scale_percent);
    settings.setValue(kSettingEditorTabWidth, editor_settings_.tab_width);
    settings.setValue(kSettingEditorLineNumbers, editor_settings_.show_line_numbers);
    settings.setValue(kSettingEditorCurrentLine, editor_settings_.highlight_current_line);
    settings.setValue(kSettingEditorWordWrap, editor_settings_.word_wrap);
    settings.setValue(kSettingEditorAutoIndent, editor_settings_.auto_indent);
    settings.setValue(kSettingEditorCompletion, editor_settings_.completion);
    settings.setValue(kSettingEditorLint, editor_settings_.lint);
    settings.setValue(kSettingEditorWhitespace, editor_settings_.show_whitespace);
    settings.setValue(kSettingEditorMinimap, editor_settings_.minimap);
    settings.setValue(kSettingEditorAutosave, editor_settings_.autosave);
    settings.setValue(kSettingEditorAutosaveSeconds, editor_settings_.autosave_seconds);
}

void MainWindow::applyEditorSettings(const EditorSettings& settings) {
    editor_settings_ = settings;
    const themes::Colours resolved =
        themes::resolve(themes::byName(theme_name_), high_contrast_);
    const auto apply_to = [this, &resolved](QTabWidget* tabs) {
        if (tabs == nullptr) {
            return;
        }
        for (int index = 0; index < tabs->count(); ++index) {
            if (auto* editor = qobject_cast<CodeEditor*>(tabs->widget(index));
                editor != nullptr) {
                editor->setDiagnosticColors(resolved.error_color, resolved.warning_color);
                editor->applySettings(editor_settings_);
            }
        }
    };
    apply_to(editor_tabs_);
    apply_to(editor_tabs_secondary_);

    // The console and the two logs are separate text views with a fixed font, so
    // they scale with the same ladder rather than staying at ten points while the
    // editor grows around them.
    const int log_points =
        std::max(6, static_cast<int>(std::lround(10.0 * editor_settings_.font_scale_percent / 100.0)));
    for (const QString& name : {QStringLiteral("consoleOutput"), QStringLiteral("errorsLog"),
                                QStringLiteral("turtleLog")}) {
        const QList<QPlainTextEdit*> logs = findChildren<QPlainTextEdit*>(name);
        for (QPlainTextEdit* log : logs) {
            QFont log_font = log->font();
            log_font.setPointSize(log_points);
            log->setFont(log_font);
        }
    }

    // Everything without a font of its own -- panels, docks, dialogs, menus --
    // inherits the application font. Scaling from the font captured at first use
    // rather than from the current one keeps 150% from becoming 225% the next time
    // Preferences is applied.
    static const QFont base_ui_font = QApplication::font();
    QFont ui_font = base_ui_font;
    ui_font.setPointSizeF(std::max(6.0, base_ui_font.pointSizeF() *
                                            (std::max(1, editor_settings_.font_scale_percent) /
                                             100.0)));
    if (auto* app = qobject_cast<QApplication*>(QCoreApplication::instance())) {
        app->setFont(ui_font);
    }

    // The interval is a setting, so the timer follows it. Ten seconds is the
    // floor: anything shorter is a disk-write loop wearing a feature's clothes.
    if (autosave_timer_ != nullptr) {
        autosave_timer_->setInterval(std::max(10, editor_settings_.autosave_seconds) * 1000);
        if (editor_settings_.autosave) {
            autosave_timer_->start();
        } else {
            autosave_timer_->stop();
        }
    }
}

void MainWindow::setTerminalEnabled(bool on) {
    terminal_enabled_ = on;
    QSettings{}.setValue(kSettingTerminalEnabled, on);
    refreshTerminalTab();
}

void MainWindow::refreshTerminalTab() {
    if (output_sub_tabs_ == nullptr) {
        return;
    }
    // Presentation mode is a room watching a program, and a player window is a
    // program its reader did not start: neither is a place for a prompt that
    // runs arbitrary commands. Both remove the tab rather than merely hiding it.
    const bool allowed = terminal_enabled_ && !presentation_mode_ && !player_mode_;
    if (!allowed) {
        if (terminal_widget_ != nullptr) {
            const int index = output_sub_tabs_->indexOf(terminal_widget_);
            if (index >= 0) {
                output_sub_tabs_->removeTab(index);
            }
            // A shell nobody can see is a process nobody can stop.
            terminal_widget_->stop();
        }
        return;
    }
    if (terminal_widget_ == nullptr) {
        terminal_widget_ = new TerminalWidget{terminalDirectory(), output_sub_tabs_};
    }
    if (output_sub_tabs_->indexOf(terminal_widget_) < 0) {
        const int index =
            output_sub_tabs_->addTab(terminal_widget_, tr("\U0001F4BB Terminal"));
        output_sub_tabs_->setTabToolTip(
            index, tr("Commands run through your shell in this document's folder. "
                      "Nothing here is sandboxed."));
    }
    terminal_widget_->setWorkingDirectory(terminalDirectory());
}

QString MainWindow::terminalDirectory() const {
    const CodeEditor* editor = currentEditor();
    const QString path = editor != nullptr ? pathOf(editor) : QString{};
    if (!path.isEmpty()) {
        return QFileInfo{path}.absolutePath();
    }
    return QDir::homePath();
}

void MainWindow::showPreferences() {
    // A stack copy: the dialog edits its own record, and Cancel simply means the
    // window never hears about it.
    auto* dialog = new PreferencesDialog{editor_settings_, terminal_enabled_, this};
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    connect(dialog, &PreferencesDialog::applied, this,
            [this](const EditorSettings& settings) { applyEditorSettings(settings); });
    connect(dialog, &PreferencesDialog::accepted, this,
            [this](const EditorSettings& settings) {
                applyEditorSettings(settings);
                saveEditorSettings();
                setExecutionState(tr("ready"), tr("Editor settings saved"));
            });
    connect(dialog, &PreferencesDialog::tourRequested, this, [this] {
        rearmGuidedTour();
        setExecutionState(tr("ready"),
                          tr("The guided tour will show the next time the window opens."));
    });
    // Immediate, like the tour: the tab is either on screen or not, and Apply is
    // not required for a control whose whole effect is visible behind the dialog.
    connect(dialog, &PreferencesDialog::terminalToggled, this, [this](bool on) {
        setTerminalEnabled(on);
        setExecutionState(tr("ready"),
                          on ? tr("The terminal is on")
                             : tr("The terminal is off"));
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
    }
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
    font_scale->setShortcut(QKeySequence{Qt::CTRL | Qt::ALT | Qt::Key_A});
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

// ---------------------------------------------------------------------------
// SQL
// ---------------------------------------------------------------------------

namespace {

constexpr auto kSqlFileFilter =
    "SQLite databases (*.db *.sqlite *.sqlite3);;All files (*)";

}  // namespace

void MainWindow::buildSqlPanel() {
    if (sql_database_ == nullptr) {
        sql_database_ = std::make_shared<timewarp::sql::Database>();
    }
    sql_panel_ = new SqlPanel{right_tabs_};
    right_tabs_->addTab(sql_panel_, tr("\U0001F5C4 Database"));
    connect(sql_panel_, &SqlPanel::openDatabaseRequested, this,
            &MainWindow::openSqlDatabaseDialog);
    connect(sql_panel_, &SqlPanel::newDatabaseRequested, this,
            &MainWindow::newSqlDatabaseDialog);
    connect(sql_panel_, &SqlPanel::runRequested, this, &MainWindow::runSqlQuery);
    connect(sql_panel_, &SqlPanel::refreshRequested, this, &MainWindow::refreshSqlTables);
    connect(sql_panel_, &SqlPanel::schemaRequested, this, &MainWindow::showSqlSchema);
    connect(sql_panel_, &SqlPanel::recentDatabaseRequested, this, [this](const QString& path) {
        static_cast<void>(openSqlDatabase(path));
    });
    sql_panel_->setRecentDatabases(
        QSettings{}.value(QLatin1String(kSettingRecentDatabases)).toStringList());
}

void MainWindow::noteRecentDatabase(const QString& path) {
    if (path.isEmpty()) {
        return;
    }
    QSettings settings;
    QStringList paths =
        settings.value(QLatin1String(kSettingRecentDatabases)).toStringList();
    paths.removeAll(path);
    paths.prepend(path);
    while (paths.size() > kRecentDatabaseLimit) {
        paths.removeLast();
    }
    settings.setValue(QLatin1String(kSettingRecentDatabases), paths);
    settings.setValue(QLatin1String(kSettingSqlDatabase), path);
    if (sql_panel_ != nullptr) {
        sql_panel_->setRecentDatabases(paths);
    }
}

bool MainWindow::openSqlDatabase(const QString& path, bool silent) {
    if (path.trimmed().isEmpty()) {
        return false;
    }
    if (sql_database_ == nullptr) {
        sql_database_ = std::make_shared<timewarp::sql::Database>();
    }
    QString error;
    if (!sql_database_->open(path, &error)) {
        if (!silent) {
            reportError(tr("The database could not be opened: %1").arg(error));
            if (sql_panel_ != nullptr) {
                sql_panel_->setStatus(error, true);
            }
        }
        return false;
    }
    noteRecentDatabase(path);
    const QString displayed = path == QLatin1String(":memory:")
                                  ? tr("(in memory)")
                                  : QDir::toNativeSeparators(path);
    if (sql_panel_ != nullptr) {
        sql_panel_->setDatabase(displayed, sql_database_->tables());
    }
    if (repl_runner_ != nullptr) {
        const std::shared_ptr<timewarp::sql::Database> database = sql_database_;
        repl_runner_->setSqlCallback([database](std::string_view sql) {
            return database->executeForOutput(sql);
        });
    }
    setExecutionState(tr("ready"), tr("Database %1 open").arg(displayed));
    return true;
}

void MainWindow::openSqlDatabaseDialog() {
    const QString start =
        QSettings{}.value(QLatin1String(kSettingSqlDatabase)).toString();
    const QString path = QFileDialog::getOpenFileName(
        this, tr("Open SQLite Database"),
        start.isEmpty() ? QDir::homePath() : QFileInfo{start}.absolutePath(),
        QLatin1String(kSqlFileFilter));
    if (!path.isEmpty()) {
        static_cast<void>(openSqlDatabase(path));
    }
}

void MainWindow::newSqlDatabaseDialog() {
    const QString suggestion =
        QDir{QDir::homePath()}.filePath(QStringLiteral("studio.db"));
    QString path = QFileDialog::getSaveFileName(this, tr("New SQLite Database"),
                                                suggestion,
                                                QLatin1String(kSqlFileFilter));
    if (path.isEmpty()) {
        return;
    }
    if (QFileInfo{path}.suffix().isEmpty()) {
        path += QStringLiteral(".db");
    }
    if (QFileInfo::exists(path)) {
        const auto answer = QMessageBox::question(
            this, tr("Database already exists"),
            tr("%1 already exists. Open it instead?")
                .arg(QDir::toNativeSeparators(path)));
        if (answer == QMessageBox::Yes) {
            static_cast<void>(openSqlDatabase(path));
        }
        return;
    }
    static_cast<void>(openSqlDatabase(path));
}

void MainWindow::restoreSqlDatabase() {
    const QString path =
        QSettings{}.value(QLatin1String(kSettingSqlDatabase)).toString();
    if (path.isEmpty()) {
        return;
    }
    if (path != QLatin1String(":memory:") && !QFileInfo::exists(path)) {
        QSettings{}.remove(QLatin1String(kSettingSqlDatabase));
        return;
    }
    if (!openSqlDatabase(path, true)) {
        QSettings{}.remove(QLatin1String(kSettingSqlDatabase));
    }
}

void MainWindow::runSqlQuery(const QString& sql) {
    if (sql_database_ == nullptr || !sql_database_->isOpen()) {
        if (sql_panel_ != nullptr) {
            sql_panel_->setStatus(tr("Open or create a database first."), true);
        }
        return;
    }
    const timewarp::sql::QueryResult result = sql_database_->execute(sql);
    if (sql_panel_ != nullptr) {
        sql_panel_->setResult(result);
    }
    // A script may have created or dropped a table; the list is cheap to rebuild
    // and a stale one is a table the user can see but cannot open.
    refreshSqlTables();
    setExecutionState(result.ok() ? tr("ready") : tr("error"),
                      result.ok() ? tr("SQL: done") : tr("SQL: %1").arg(result.error));
}

void MainWindow::refreshSqlTables() {
    if (sql_panel_ == nullptr) {
        return;
    }
    sql_panel_->setTables(sql_database_ != nullptr && sql_database_->isOpen()
                              ? sql_database_->tables()
                              : QStringList{});
}

void MainWindow::showSqlSchema(const QString& table) {
    if (sql_database_ == nullptr || !sql_database_->isOpen() || sql_panel_ == nullptr) {
        return;
    }
    const QString schema = sql_database_->schema(table);
    sql_panel_->setQuery(schema + QLatin1Char('\n'));
    sql_panel_->focusQuery();
    sql_panel_->setStatus(schema.startsWith(QChar{0x274C})
                              ? QStringLiteral("\u274C Table not found")
                              : QStringLiteral("\u2139\uFE0F Schema shown in the editor"));
}

// ---------------------------------------------------------------------------
// Projects
// ---------------------------------------------------------------------------

QString MainWindow::activeProjectPath() const {
    return project_.isValid() ? project_.filePath() : QString{};
}

void MainWindow::refreshProjectPanel() {
    if (project_panel_ == nullptr) {
        return;
    }
    project_panel_->setProject(project_);
    if (project_dock_ != nullptr) {
        project_dock_->setWindowTitle(
            project_.isValid()
                ? tr("\U0001F5C2 %1").arg(project_.name())
                : tr("\U0001F5C2 Projects"));
    }
}

bool MainWindow::saveProject() {
    if (!project_.isValid()) {
        return false;
    }
    QString error;
    if (!project_.save(&error)) {
        reportError(tr("The project could not be saved: %1").arg(error));
        QMessageBox::warning(this, tr("Project not saved"), error);
        return false;
    }
    // The active path lives in settings only once the file it names exists.
    QSettings{}.setValue(QLatin1String(kSettingActiveProject), project_.filePath());
    return true;
}

void MainWindow::noteRecentProject(const QString& path) {
    if (path.isEmpty()) {
        return;
    }
    QSettings settings;
    QStringList paths = settings.value(QLatin1String(kSettingRecentProjects)).toStringList();
    paths.removeAll(path);
    paths.prepend(path);
    while (paths.size() > kRecentProjectLimit) {
        paths.removeLast();
    }
    settings.setValue(QLatin1String(kSettingRecentProjects), paths);
    settings.setValue(QLatin1String(kSettingActiveProject), path);
}

void MainWindow::newProjectDialog() {
    const QString start =
        project_.isValid() ? project_.root() : QDir::homePath();
    const QString folder = QFileDialog::getExistingDirectory(
        this, tr("Choose the folder for the new project"), start);
    if (folder.isEmpty()) {
        return;
    }
    bool accepted = false;
    const QString suggested =
        QFileInfo{folder}.fileName().isEmpty() ? tr("New Project")
                                              : QFileInfo{folder}.fileName();
    const QString name = QInputDialog::getText(
        this, tr("New Project"), tr("Project name:"), QLineEdit::Normal, suggested,
        &accepted);
    if (!accepted || name.trimmed().isEmpty()) {
        return;
    }
    timewarp::project::Project created =
        timewarp::project::Project::create(name.trimmed(), folder);
    if (QFileInfo::exists(created.filePath())) {
        const auto answer = QMessageBox::question(
            this, tr("Project already exists"),
            tr("%1 already exists. Open that project instead?")
                .arg(QDir::toNativeSeparators(created.filePath())));
        if (answer == QMessageBox::Yes) {
            static_cast<void>(openProject(created.filePath()));
        }
        return;
    }
    project_ = created;
    if (!saveProject()) {
        project_ = timewarp::project::Project{};
        refreshProjectPanel();
        return;
    }
    noteRecentProject(project_.filePath());
    refreshProjectPanel();
    setExecutionState(tr("ready"), tr("Project %1 created").arg(project_.name()));
}

void MainWindow::openProjectDialog() {
    const QString start = project_.isValid() ? project_.root() : QDir::homePath();
    const QString path = QFileDialog::getOpenFileName(
        this, tr("Open Project"), start, timewarp::project::openFileFilter());
    if (!path.isEmpty()) {
        static_cast<void>(openProject(path));
    }
}

bool MainWindow::openProject(const QString& path, bool silent) {
    timewarp::project::Project loaded;
    QString error;
    if (!loaded.load(path, &error)) {
        if (!silent) {
            reportError(tr("The project could not be opened: %1").arg(error));
            QMessageBox::warning(this, tr("Project not opened"), error);
        }
        return false;
    }
    project_ = loaded;
    noteRecentProject(project_.filePath());
    refreshProjectPanel();
    setExecutionState(tr("ready"), tr("Project %1 opened").arg(project_.name()));
    return true;
}

void MainWindow::closeProject() {
    if (!project_.isValid()) {
        return;
    }
    const QString name = project_.name();
    project_ = timewarp::project::Project{};
    QSettings settings;
    settings.remove(QLatin1String(kSettingActiveProject));
    refreshProjectPanel();
    setExecutionState(tr("ready"), tr("Project %1 closed").arg(name));
}

void MainWindow::restoreProject() {
    const QString path =
        QSettings{}.value(QLatin1String(kSettingActiveProject)).toString();
    if (path.isEmpty()) {
        return;
    }
    if (!QFileInfo::exists(path)) {
        // A project that has been moved or deleted is not a startup error. It is
        // also not silently reopened as an empty project: the recent menu still
        // names it, so the user can find it again.
        QSettings{}.remove(QLatin1String(kSettingActiveProject));
        return;
    }
    if (!openProject(path, true)) {
        QSettings{}.remove(QLatin1String(kSettingActiveProject));
    }
}

void MainWindow::addFilesToProject(const QString& group) {
    if (!project_.isValid()) {
        setExecutionState(tr("ready"), tr("Create or open a project first."));
        return;
    }
    QString start = project_.root();
    if (CodeEditor* editor = currentEditor(); editor != nullptr) {
        const QString path = pathOf(editor);
        if (!path.isEmpty()) {
            start = QFileInfo{path}.absolutePath();
        }
    }
    const QStringList paths = QFileDialog::getOpenFileNames(
        this, group.isEmpty() ? tr("Add programs to %1").arg(project_.name())
                              : tr("Add programs to %1").arg(group),
        start, openFileFilter());
    if (paths.isEmpty()) {
        return;
    }
    const int added = project_.addFiles(paths, group);
    if (added > 0 && saveProject()) {
        refreshProjectPanel();
        setExecutionState(tr("ready"),
                          added == 1 ? tr("Added 1 program to the project")
                                     : tr("Added %1 programs to the project").arg(added));
    }
}

void MainWindow::addFolderToProject() {
    if (!project_.isValid()) {
        setExecutionState(tr("ready"), tr("Create or open a project first."));
        return;
    }
    const QString folder = QFileDialog::getExistingDirectory(
        this, tr("Add every program under a folder"), project_.root());
    if (folder.isEmpty()) {
        return;
    }

    const QString base = QDir::cleanPath(folder);
    const QString folder_name = QFileInfo{base}.fileName();
    QStringList added;
    QDirIterator iterator{base, QDir::Files, QDirIterator::Subdirectories};
    while (iterator.hasNext()) {
        const QString path = iterator.next();
        if (languageForFile(path) == Language::Unknown) {
            continue;
        }
        const QString relative = QDir{base}.relativeFilePath(path);
        QString group = QFileInfo{relative}.path();
        if (group == QLatin1String(".")) {
            group.clear();
        }
        if (base != QDir::cleanPath(project_.root()) && !folder_name.isEmpty()) {
            group = group.isEmpty() ? folder_name : folder_name + QLatin1Char('/') + group;
        }
        if (project_.addFile(path, group)) {
            added.append(path);
        }
    }
    if (added.isEmpty()) {
        setExecutionState(tr("ready"), tr("No programs were found in that folder."));
        return;
    }
    if (saveProject()) {
        refreshProjectPanel();
        setExecutionState(tr("ready"),
                          added.size() == 1
                              ? tr("Added 1 program from the folder")
                              : tr("Added %1 programs from the folder").arg(added.size()));
    }
}

void MainWindow::addGroupToProject() {
    if (!project_.isValid()) {
        setExecutionState(tr("ready"), tr("Create or open a project first."));
        return;
    }
    bool accepted = false;
    const QString name = QInputDialog::getText(
        this, tr("New Group"), tr("Group name:"), QLineEdit::Normal, QString{}, &accepted);
    if (!accepted || !project_.addGroup(name)) {
        return;
    }
    if (saveProject()) {
        refreshProjectPanel();
    }
}

void MainWindow::addNewGroupForFile(const QString& path) {
    if (!project_.isValid()) {
        return;
    }
    bool accepted = false;
    const QString name = QInputDialog::getText(
        this, tr("New Group"), tr("Group name:"), QLineEdit::Normal, QString{}, &accepted);
    if (!accepted || !project_.addGroup(name)) {
        return;
    }
    project_.setGroup(path, name);
    if (saveProject()) {
        refreshProjectPanel();
        project_panel_->selectFile(path);
    }
}

void MainWindow::renameProjectGroup(const QString& group) {
    if (!project_.isValid()) {
        return;
    }
    bool accepted = false;
    const QString name = QInputDialog::getText(
        this, tr("Rename Group"), tr("New name:"), QLineEdit::Normal, group, &accepted);
    if (!accepted || !project_.renameGroup(group, name)) {
        return;
    }
    if (saveProject()) {
        refreshProjectPanel();
    }
}

void MainWindow::removeProjectGroup(const QString& group) {
    if (!project_.isValid()) {
        return;
    }
    const auto answer = QMessageBox::question(
        this, tr("Remove Group"),
        tr("Remove the group \"%1\"?\n\nIts programs stay in the project, ungrouped.")
            .arg(group));
    if (answer != QMessageBox::Yes || !project_.removeGroup(group)) {
        return;
    }
    if (saveProject()) {
        refreshProjectPanel();
    }
}

void MainWindow::moveProjectFile(const QString& path, const QString& group) {
    if (!project_.isValid()) {
        return;
    }
    if (!project_.setGroup(path, group)) {
        return;
    }
    if (saveProject()) {
        refreshProjectPanel();
        project_panel_->selectFile(path);
    }
}

void MainWindow::renameProject() {
    if (!project_.isValid()) {
        return;
    }
    bool accepted = false;
    const QString name = QInputDialog::getText(
        this, tr("Rename Project"), tr("Project name:"), QLineEdit::Normal,
        project_.name(), &accepted);
    if (!accepted || name.trimmed().isEmpty()) {
        return;
    }
    project_.setName(name);
    if (saveProject()) {
        refreshProjectPanel();
    }
}

void MainWindow::revealProjectFolder() {
    if (!project_.isValid()) {
        return;
    }
    QDesktopServices::openUrl(QUrl::fromLocalFile(project_.root()));
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
    connect(editor, &CodeEditor::breakpointsChanged, this, [this, editor] {
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

bool MainWindow::handleBasicDirectCommand(const QString& command, CodeEditor* editor) {
    if (editor == nullptr) {
        return false;
    }
    const QString trimmed = command.trimmed();
    if (const std::optional<BasicProgramLine> line = parseBasicProgramLine(trimmed);
        line.has_value()) {
        appendOutput(tr("REPL \u203A %1\n").arg(trimmed));
        const QString before = editor->toPlainText();
        QString after;
        if (line->body.trimmed().isEmpty()) {
            after = withoutBasicProgramLine(before, line->number);
            appendOutput(after == before
                             ? tr("\u2139\uFE0F No line %1 to delete\n").arg(line->number)
                             : tr("\u2139\uFE0F Deleted line %1\n").arg(line->number));
        } else {
            after = withBasicProgramLine(before, line->number,
                                         withPrintShorthand(line->body));
            appendOutput(tr("\u2139\uFE0F Line %1 stored in the program\n")
                             .arg(line->number));
        }
        if (after != before) {
            QTextCursor edit{editor->document()};
            edit.beginEditBlock();
            edit.select(QTextCursor::Document);
            edit.insertText(after);
            edit.endEditBlock();
            editor->document()->setModified(true);
            refreshDocumentState();
        }
        setExecutionState(tr("ready"), tr("Line %1 stored").arg(line->number));
        return true;
    }

    if (trimmed.compare(QStringLiteral("LIST"), Qt::CaseInsensitive) == 0) {
        appendOutput(tr("REPL \u203A %1\n").arg(trimmed));
        const QString program = editor->toPlainText();
        if (program.trimmed().isEmpty()) {
            appendOutput(tr("\u2139\uFE0F The program is empty.\n"));
        } else {
            appendOutput(program.endsWith(QLatin1Char('\n')) ? program : program + QLatin1Char('\n'));
        }
        setExecutionState(tr("ready"), tr("Program listed"));
        return true;
    }

    if (trimmed.compare(QStringLiteral("RUN"), Qt::CaseInsensitive) == 0) {
        appendOutput(tr("REPL \u203A %1\n").arg(trimmed));
        if (editor->toPlainText().trimmed().isEmpty()) {
            appendOutput(tr("\u2139\uFE0F The program is empty.\n"));
            setExecutionState(tr("ready"), tr("Program is empty"));
            return true;
        }
        runCurrentDocument();
        return true;
    }

    // NEW clears the program as well as the REPL bindings. The bindings are reset
    // by ReplSession when the command reaches it, so only the document is handled
    // here.
    if (trimmed.compare(QStringLiteral("NEW"), Qt::CaseInsensitive) == 0) {
        stopEngines();
        QTextCursor edit{editor->document()};
        edit.beginEditBlock();
        edit.select(QTextCursor::Document);
        edit.removeSelectedText();
        edit.endEditBlock();
        editor->document()->setModified(true);
        refreshDocumentState();
    }
    return false;
}

void MainWindow::submitReplCommand(const QString& command) {
    const QString trimmed = command.trimmed();
    if (trimmed.isEmpty()) {
        return;
    }
    if (immediate_mode_ == nullptr || canvas_queue_ == nullptr ||
        canvas_bridge_ == nullptr) {
        return;
    }

    CodeEditor* editor = currentEditor();
    const Language ui_language = editor != nullptr ? languageOf(editor) : defaultLanguage();
    const std::optional<timewarp::lang::Language> native = nativeLanguage(ui_language);
    if (!native.has_value()) {
        setExecutionState(tr("ready"), tr("This language has no REPL."));
        return;
    }

    if (ui_language == Language::Basic && handleBasicDirectCommand(trimmed, editor)) {
        return;
    }

    if (repl_runner_ == nullptr || repl_language_ != *native) {
        if (repl_runner_ != nullptr) {
            repl_runner_->requestStop();
            repl_runner_->wait();
            delete repl_runner_;
            repl_runner_ = nullptr;
        }
        repl_runner_ = new ReplRunner{*native, *canvas_queue_, this};
        repl_language_ = *native;
        repl_runner_->setWakeup([this] { canvas_bridge_->wake(); });
        if (const QColor surface = turtle_canvas_ != nullptr ? turtle_canvas_->surfaceColor()
                                                             : QColor{};
            surface.isValid()) {
            repl_runner_->setSurfaceColor(
                timewarp::lang::Rgb{static_cast<std::uint8_t>(surface.red()),
                                    static_cast<std::uint8_t>(surface.green()),
                                    static_cast<std::uint8_t>(surface.blue())});
        }
        if (sql_database_ != nullptr) {
            const std::shared_ptr<timewarp::sql::Database> database = sql_database_;
            repl_runner_->setSqlCallback([database](std::string_view sql) {
                return database->executeForOutput(sql);
            });
        }
        connect(repl_runner_, &ReplRunner::outputAvailable, this,
                [this](const QString& text) { appendOutput(text); });
        connect(repl_runner_, &ReplRunner::commandFinished, this,
                [this](bool ok, const QString& error) {
                    if (canvas_bridge_ != nullptr) {
                        canvas_bridge_->drainNow();
                    }
                    if (!error.isEmpty()) {
                        reportError(error);
                    }
                    setExecutionState(ok ? tr("ready") : tr("error"),
                                      ok ? tr("REPL ready") : tr("REPL error"));
                });
    }

    appendOutput(tr("REPL \u203A %1\n").arg(trimmed));
    immediate_mode_->setCommand(QString{});
    immediate_mode_->focusInput();
    setExecutionState(tr("running"), tr("REPL: running"));
    repl_runner_->submit(trimmed);
}

void MainWindow::runCurrentDocument() {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        return;
    }
    const QString source = editor->toPlainText();
    running_selection_ = false;
    noteRun(source);
    runSource(editor, source);
}

void MainWindow::runSelection() {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        return;
    }
    // The selection as the engine sees it. Qt stores paragraph breaks as U+2029
    // in `selectedText()`, and a program that receives those does not parse.
    QString selection = editor->textCursor().selectedText();
    selection.replace(QChar::ParagraphSeparator, QLatin1Char('\n'));
    if (selection.trimmed().isEmpty()) {
        // Refused rather than falling back to the document: running the whole
        // program when the user asked for three lines is a worse surprise than a
        // message saying nothing is selected.
        setExecutionState(tr("ready"), tr("Select the lines to run first."));
        return;
    }
    running_selection_ = true;
    noteRun(selection);
    runSource(editor, selection);
}

void MainWindow::resetProgram() {
    // Stop first: clearing a canvas something is still drawing into is a race the
    // user loses.
    stopEngines();
    canvas_bridge_->drainNow();
    if (turtle_canvas_ != nullptr) {
        turtle_canvas_->reset();
    }
    clearOutput();
    clearRunState();
    setExecutionState(tr("ready"), tr("Program reset"));
}

void MainWindow::runSource(CodeEditor* editor, const QString& source) {
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
            runNatively(editor, *native, source);
        } else {
            runInBridge(editor, source);
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
    if (sql_database_ != nullptr && sql_database_->isOpen()) {
        const std::shared_ptr<timewarp::sql::Database> database = sql_database_;
        turtle_host_->set_sql_runner([database](std::string_view sql) {
            return database->executeForOutput(sql);
        });
    }
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
        runner->set_data_breakpoints(debug_panel_->dataBreakpoints());
    }

    setExecutionState(tr("running"), tr("Running %1…").arg(nameOf(editor)));
    runner->load_program(source, nameOf(editor));
    runner->start();
}

void MainWindow::stopEngines() {
    if (repl_runner_ != nullptr) {
        // A REPL command is a small run, and running a document while one is in
        // flight would put two engines on one canvas.
        repl_runner_->requestStop();
        repl_runner_->wait();
        delete repl_runner_;
        repl_runner_ = nullptr;
    }
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

void MainWindow::runNatively(CodeEditor* editor, timewarp::lang::Language language,
                             const QString& source) {
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
    if (sql_database_ != nullptr && sql_database_->isOpen()) {
        const std::shared_ptr<timewarp::sql::Database> database = sql_database_;
        runner->setSqlCallback([database](std::string_view sql) {
            return database->executeForOutput(sql);
        });
    }
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
    // Profiling is a per-run choice: the flag travels with this runner, and the
    // report is built from this run's own numbers.
    const bool profiling = profile_action_ != nullptr && profile_action_->isChecked();
    runner->set_profiling(profiling);
    const QString profile_title = nameOf(editor);
    QStringList profile_source;
    if (profiling) {
        // The engine's statement indices are source-line indices (the loader
        // keeps blanks), so the report can quote the line the index names.
        profile_source = editor->toPlainText().split(QLatin1Char('\n'));
    }
    runner->load_program(source);
    // The editor's breakpoints are zero-based lines. The line-loop languages use
    // them as statement indices, which is why `parse_lines` keeps every line,
    // blanks included; the whole-program executors match them against the source
    // line they are about to run.
    std::set<std::size_t> breakpoints;
    for (const int line : editor->breakpoints()) {
        breakpoints.insert(static_cast<std::size_t>(line));
    }
    runner->set_breakpoints(breakpoints);
    if (debug_panel_ != nullptr) {
        // The panel's set applies to every run, whichever engine drives it. The
        // editor's gutter holds line breakpoints; a data breakpoint has no gutter.
        runner->setDataBreakpoints(debug_panel_->dataBreakpoints());
    }

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
            [this, runner, language, profiling, profile_title,
             profile_source](bool completed, const QString& error) {
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
                last_variables_ = rows;
                if (variables_panel_ != nullptr) {
                    variables_panel_->setRows(rows);
                }
                refreshWatches();
                // A run stopped while paused ends here rather than through
                // `resumed`, so the panel is told the pause is over.
                if (debug_panel_ != nullptr) {
                    debug_panel_->setPaused(false, 0, QString{});
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

                if (profiling) {
                    const auto profile_rows = runner->profile();
                    if (profile_rows.empty()) {
                        // A whole-program language has no statement loop to time;
                        // saying so is better than a report with no rows.
                        const std::string_view name = timewarp::lang::display_name(language);
                        appendOutput(tr("No per-line profile: %1 runs as a whole program.\n")
                                         .arg(QString::fromLatin1(
                                             name.data(), static_cast<int>(name.size()))));
                    } else {
                        QList<ProfileEntry> entries;
                        entries.reserve(static_cast<int>(profile_rows.size()));
                        for (const timewarp::lang::LineProfile& row : profile_rows) {
                            const auto index = static_cast<int>(row.index);
                            entries.append(
                                {row.line,
                                 index >= 0 && index < profile_source.size()
                                     ? profile_source.at(index)
                                     : QString{},
                                 row.hits, row.elapsed_ms});
                        }
                        ProfileReportDialog dialog{profile_title, entries, runner->elapsedMs(),
                                                   this};
                        dialog.exec();
                    }
                }
            });

    connect(runner, &NativeRunner::paused, this, [this, runner](int line) {
        if (native_runner_ != runner) {
            return;
        }
        // The console says where the run is, because the canvas cannot: a learner
        // needs to know which statement has not happened yet, not that something
        // happened somewhere. Line 0 means the program has no source lines (a
        // binary CHIP-8 ROM), so the instruction is named rather than a line.
        setExecutionState(tr("paused"),
                          line > 0 ? tr("Paused before line %1").arg(line)
                                   : tr("Paused before the next instruction"));
        // The Debug tab and the Variables tab follow the pause, so a stopped
        // program is inspectable wherever the user looks.
        if (debug_panel_ != nullptr) {
            debug_panel_->setPaused(true, line, QString{});
        }
        QList<timewarp::bridge::VariableRow> rows;
        for (const NativeRunner::VariableRow& row : runner->variables()) {
            rows.append({row.name, row.type, row.value});
        }
        last_variables_ = rows;
        if (variables_panel_ != nullptr) {
            variables_panel_->setRows(rows);
        }
        if (debug_panel_ != nullptr) {
            // The panel's own variables table is the frame's bindings for
            // Scheme; for a native engine the run's bindings are the best
            // answer, and it is better than an empty table while stopped.
            debug_panel_->setVariables(rows);
        }
        refreshWatches();
        static_cast<void>(showPanel(QStringLiteral("debug")));
    });
    connect(runner, &NativeRunner::dataBreakpointHit, this,
            [this, runner](const QString& name, int line) {
                if (native_runner_ != runner) {
                    return;
                }
                setExecutionState(tr("paused"), tr("Paused: %1 changed").arg(name));
                if (debug_panel_ != nullptr) {
                    debug_panel_->setPaused(true, line, QString{});
                    debug_panel_->setPauseReason(tr("%1 changed").arg(name));
                }
                QList<timewarp::bridge::VariableRow> rows;
                for (const NativeRunner::VariableRow& row : runner->variables()) {
                    rows.append({row.name, row.type, row.value});
                }
                last_variables_ = rows;
                if (variables_panel_ != nullptr) {
                    variables_panel_->setRows(rows);
                }
                if (debug_panel_ != nullptr) {
                    debug_panel_->setVariables(rows);
                }
                refreshWatches();
                static_cast<void>(showPanel(QStringLiteral("debug")));
            });
    connect(runner, &NativeRunner::resumed, this, [this, runner] {
        if (native_runner_ != runner) {
            return;
        }
        if (debug_panel_ != nullptr) {
            debug_panel_->setPaused(false, 0, QString{});
        }
        setExecutionState(tr("running"), tr("Running…"));
    });

    setExecutionState(tr("running"), tr("Running %1…").arg(nameOf(editor)));
    runner->start();
}

void MainWindow::runInBridge(CodeEditor* editor, const QString& source) {
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
    if (!python_worker_->run(settingsKey(languageOf(editor)), source, nameOf(editor))) {
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
    {"output", 0},   {"variables", 1}, {"telemetry", 2},
    {"debug", 3},    {"lessons", 4},   {"classroom", 5},
    {"sql", 6}};

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
            // A one-time hint the first time the lessons page is raised. It
            // refuses while the guided tour is running, which is the one case
            // where two overlays would compete for the same panel.
            if (wanted == QLatin1String("lessons") && tour_ != nullptr) {
                tour_->hint(QStringLiteral("lessons"));
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

void MainWindow::requestClassroom() {
    if (!ensureBridge()) {
        if (classroom_panel_ != nullptr) {
            classroom_panel_->setFailure(QStringLiteral("classroom-roster"),
                                         tr("the language engine did not start"));
        }
        return;
    }
    // The roster first, and the marks when its answer arrives: the worker takes
    // one request at a time, so sending both at once would have the second
    // refused and the table would stay empty forever. They are separate requests
    // because the class list is a file read and the marking is a directory walk,
    // so the panel can show who is in the class while the marks are worked out.
    if (python_worker_->requestClassroomRoster() || classroom_panel_ == nullptr) {
        return;
    }
    // Refused, which means another request is in flight or the worker has gone:
    // either way the panel must say so rather than sit there looking like a
    // school with no pupils.
    classroom_panel_->setFailure(
        QStringLiteral("classroom-roster"),
        python_worker_->isRunning()
            ? tr("another request is still being answered; try Refresh")
            : tr("the language engine is not running"));
}

void MainWindow::exportClassroomBundle() {
    // The dialog comes first: a teacher who cancels should not have started a
    // Python process to do it.
    const QString path = QFileDialog::getSaveFileName(
        this, tr("Save class bundle"),
        QDir{QDir::homePath()}.filePath(QStringLiteral("classroom.zip")),
        tr("Class bundle (*.zip)"));
    if (path.isEmpty() || !ensureBridge()) {
        return;
    }
    if (!python_worker_->requestClassroomBundle(path)) {
        statusBar()->showMessage(tr("Another classroom request is still being answered"), 8000);
    }
}

void MainWindow::importClassroomBundle() {
    const QString path = QFileDialog::getOpenFileName(this, tr("Open class bundle"),
                                                      QDir::homePath(),
                                                      tr("Class bundle (*.zip)"));
    if (path.isEmpty()) {
        return;
    }
    // The one question with two genuinely different answers: setting up a new
    // class wants existing files left alone, and refreshing one wants the archive
    // to win. Overwriting a learner's work is the answer that cannot be undone,
    // so it is the one that has to be asked for.
    const QMessageBox::StandardButton answer = QMessageBox::question(
        this, tr("Unpack class bundle"),
        tr("Replace files that are already in the classroom?\n\n"
           "No leaves them alone, which is what setting up a new class wants."),
        QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::No);
    if (answer == QMessageBox::Cancel || !ensureBridge()) {
        return;
    }
    if (!python_worker_->requestClassroomUnbundle(path, answer == QMessageBox::Yes)) {
        statusBar()->showMessage(tr("Another classroom request is still being answered"), 8000);
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
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::classroomRosterReceived,
            this,
            [this](const QList<timewarp::bridge::ClassroomStudent>& students,
                   const QStringList& lessons, const QString& note) {
                if (classroom_panel_ != nullptr) {
                    classroom_panel_->setRoster(students, lessons, note);
                }
                // The roster's answer is what lets the marks be asked for; see
                // `requestClassroom`. An empty class has no marks, so it is not
                // worth a second round trip.
                if (!students.isEmpty()) {
                    static_cast<void>(python_worker_->requestClassroomMarks());
                }
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::classroomMarksReceived,
            this, [this](const QList<timewarp::bridge::ClassroomMark>& rows) {
                if (classroom_panel_ != nullptr) {
                    classroom_panel_->setMarks(rows);
                }
            });
    connect(python_worker_.get(),
            &timewarp::bridge::PythonWorker::classroomPresentationReceived, this,
            [this](const timewarp::bridge::ClassroomPresentation& settings) {
                setPresentationSettings(settings.fontSize, settings.fullscreen);
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::classroomBundleWritten,
            this, [this](const QString& path) {
                setExecutionState(tr("ready"),
                                  tr("Wrote %1").arg(QFileInfo{path}.fileName()));
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::classroomUnbundled, this,
            [this](int students) {
                setExecutionState(tr("ready"),
                                  tr("Imported %1 student(s)").arg(students));
                // The class on screen is now a different class.
                requestClassroom();
            });
    connect(python_worker_.get(), &timewarp::bridge::PythonWorker::classroomFailed, this,
            [this](const QString& request, const QString& error) {
                // Said in three places on purpose: an empty class and a broken
                // class look the same in a table, so the summary row and the
                // status bar have to say which one this is.
                reportError(error);
                statusBar()->showMessage(tr("%1 could not be answered: %2").arg(request, error),
                                         8000);
                if (classroom_panel_ != nullptr) {
                    classroom_panel_->setFailure(request, error);
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
    //
    // A native run is driven here too. The panel is the same panel whichever
    // engine is paused; only the commands differ, because the native runner has
    // one step unit (a statement, a send, a top-level statement or an opcode)
    // rather than Scheme's in/over/out.
    const auto forward = [this](void (timewarp::debugger::DebuggerRunner::*command)()) {
        return [this, command] {
            if (native_runner_ != nullptr && native_runner_->isRunning()) {
                using timewarp::debugger::DebuggerRunner;
                if (command == &DebuggerRunner::request_continue) {
                    native_runner_->request_continue();
                    return;
                }
                if (command == &DebuggerRunner::request_stop) {
                    stopRunningDocument();
                    return;
                }
                // Pause and the three step variants are all "stop at the next
                // boundary" for an engine without a frame stack.
                native_runner_->request_step();
                return;
            }
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
                if (native_runner_ != nullptr) {
                    // The panel holds 1-based lines; the editor's gutter set,
                    // which the native runner is given at run start, is
                    // zero-based.
                    std::set<std::size_t> zero_based;
                    for (const int line : lines) {
                        if (line > 0) {
                            zero_based.insert(static_cast<std::size_t>(line - 1));
                        }
                    }
                    native_runner_->set_breakpoints(zero_based);
                }
            });

    // Watches are a session-wide question, so they come back with the next
    // launch. The values do not: a binding from last night's run would be shown
    // as if it were this run's.
    const QSettings settings;
    debug_panel_->setWatches(settings.value(QLatin1String(kSettingWatches)).toStringList());
    debug_panel_->setDataBreakpoints(
        settings.value(QLatin1String(kSettingDataBreakpoints)).toStringList());
    connect(debug_panel_, &DebugPanel::watchesChanged, this,
            [this](const QStringList& expressions) {
                QSettings{}.setValue(QLatin1String(kSettingWatches), expressions);
                refreshWatches();
            });
    connect(debug_panel_, &DebugPanel::dataBreakpointsChanged, this,
            [this](const QStringList& names) {
                QSettings{}.setValue(QLatin1String(kSettingDataBreakpoints), names);
                if (runner_ != nullptr) {
                    runner_->set_data_breakpoints(names);
                }
                if (native_runner_ != nullptr) {
                    native_runner_->setDataBreakpoints(names);
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

void MainWindow::refreshWatches() {
    if (debug_panel_ == nullptr) {
        return;
    }
    const QStringList expressions = debug_panel_->watches();
    if (expressions.isEmpty()) {
        debug_panel_->setWatchValues({});
        return;
    }

    std::map<std::string, timewarp::lang::Value> bindings;
    for (const timewarp::bridge::VariableRow& row : last_variables_) {
        if (row.type == QLatin1String("text")) {
            bindings[row.name.toStdString()] = timewarp::lang::Value{row.value.toStdString()};
        } else if (row.value == QLatin1String("#t") || row.value == QLatin1String("#f")) {
            // Scheme booleans, which the machine renders this way. A watch of
            // `FLAG AND 1` should mean the same thing as in the program.
            bindings[row.name.toStdString()] =
                timewarp::lang::Value{row.value == QLatin1String("#t") ? 1.0 : 0.0};
        } else {
            bool ok = false;
            const double number = row.value.toDouble(&ok);
            bindings[row.name.toStdString()] = timewarp::lang::Value{ok ? number : 0.0};
        }
    }

    std::vector<std::string> wanted;
    wanted.reserve(static_cast<std::size_t>(expressions.size()));
    for (const QString& expression : expressions) {
        wanted.push_back(expression.toStdString());
    }
    const std::vector<timewarp::lang::WatchResult> results =
        timewarp::lang::evaluate_watches(wanted, bindings);

    QList<DebugPanel::WatchRow> rows;
    rows.reserve(static_cast<int>(results.size()));
    for (const timewarp::lang::WatchResult& result : results) {
        rows.append({QString::fromStdString(result.expression),
                     QString::fromStdString(result.value), result.ok,
                     QString::fromStdString(result.error)});
    }
    debug_panel_->setWatchValues(rows);
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
    last_variables_ = rows;
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
        if (snapshot.cause == timewarp::debugger::PauseCause::DataBreakpoint) {
            debug_panel_->setPauseReason(
                tr("%1 changed").arg(snapshot.changed_variable));
        }
    }
    refreshWatches();
}

void MainWindow::clearRunState() {
    engine_telemetry_.clear();
    last_variables_.clear();
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
    refreshWatches();
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
