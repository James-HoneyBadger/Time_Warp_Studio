// SPDX-License-Identifier: MIT
//
// What the window's sections share: the settings keys and the small helpers.
//
// `main_window.cpp` was 6,245 lines -- construction, layout, menus, appearance,
// actions, the docks, SQL, projects, persistence, documents and execution in one
// translation unit. The sections are separate files now, and this is the part they
// have in common: the QSettings keys, the splitter geometry, and the handful of
// free functions that turn a theme or a path into what a widget wants.
//
// Internal to the UI implementation: nothing outside `src/ui/main_window*.cpp`
// includes it, which is why it lives beside them rather than under `include/`.

#pragma once

#include "timewarp/lang/turtle_host.hpp"

#include <QApplication>
#include <QFileInfo>
#include <QIcon>
#include <QPalette>
#include <QString>
#include <QStringList>

#include <optional>

#include <cstdint>

namespace timewarp::ui::detail {

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
constexpr auto kSettingHistory = "debug/history";
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

/// The ink a program draws with before it names a colour.
///
/// The reference's pen is white: right on its own dark surface, and invisible on a
/// light theme's paper -- Windows 2000 and High Contrast Light both drew white on
/// white and reported strokes that showed nothing. The paper decides: white ink on
/// a dark surface, the theme's text colour on a light one. A program that names a
/// colour still wins for that run; so does one that names a background, which is
/// why this is only the starting pen rather than a rule about every stroke.
[[nodiscard]] inline timewarp::lang::Rgb default_ink_for(const QColor& surface) {
    if (!surface.isValid() || surface.lightness() < 128) {
        return timewarp::lang::Rgb{255, 255, 255};
    }
    const QColor ink = QApplication::palette().color(QPalette::Text);
    return timewarp::lang::Rgb{static_cast<std::uint8_t>(ink.red()),
                               static_cast<std::uint8_t>(ink.green()),
                               static_cast<std::uint8_t>(ink.blue())};
}

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
[[nodiscard]] inline QString suggestedExportPath(const QString& documentName, const QString& suffix) {
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
[[nodiscard]] inline std::optional<BasicProgramLine> parseBasicProgramLine(const QString& text) {
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
[[nodiscard]] inline QString withPrintShorthand(QString body) {
    const QString trimmed = body.trimmed();
    if (!trimmed.startsWith(QLatin1Char('?'))) {
        return body;
    }
    QString rest = trimmed.mid(1).trimmed();
    return rest.isEmpty() ? QStringLiteral("PRINT")
                          : QStringLiteral("PRINT ") + rest;
}

/// Splits a document into source lines without the trailing empty one.
[[nodiscard]] inline QStringList programLines(const QString& text) {
    QStringList lines = text.split(QLatin1Char('\n'));
    if (!lines.isEmpty() && lines.last().isEmpty()) {
        lines.removeLast();
    }
    return lines;
}

/// The document text with `body` stored at `number`, replacing an existing line
/// or inserting in numeric order.
[[nodiscard]] inline QString withBasicProgramLine(const QString& text, int number,
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

/// The 1-based text line a *source* line number names.
///
/// A console link says what the program says -- `❌ [line 30]` in a BASIC program is
/// statement 30, not the thirtieth line of text -- so the number is looked up among
/// the numbered lines first. A language that does not number its lines numbers them by
/// position, which is why the fallback is the number itself: `goToLine` clamps it.
[[nodiscard]] inline int textLineForSourceLine(const QString& text, int number) {
    const QStringList lines = programLines(text);
    for (int index = 0; index < lines.size(); ++index) {
        const std::optional<BasicProgramLine> existing = parseBasicProgramLine(lines.at(index));
        if (existing.has_value() && existing->number == number) {
            return index + 1;
        }
    }
    return number;
}

/// The last line of `text` that holds anything, trimmed. Bounded by the reader:
/// a console holds every run of the session and the newest line is the one a
/// question about output means.
[[nodiscard]] inline QString lastNonEmptyLine(const QString& text) {
    const QStringList lines = text.split(QLatin1Char('\n'));
    for (auto line = lines.crbegin(); line != lines.crend(); ++line) {
        const QString trimmed = line->trimmed();
        if (!trimmed.isEmpty()) {
            return trimmed;
        }
    }
    return QString{};
}

/// The document text with `number` removed. No change when the line is absent.
[[nodiscard]] inline QString withoutBasicProgramLine(const QString& text, int number) {
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

}  // namespace timewarp::ui::detail

