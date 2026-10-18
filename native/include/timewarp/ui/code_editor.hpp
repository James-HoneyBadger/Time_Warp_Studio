// SPDX-License-Identifier: MIT
//
// The code editor.
//
// A `QPlainTextEdit` grown into the editor a teaching IDE needs: a line-number
// gutter that also carries the problem markers, a current-line band, syntax
// colouring, language-specific checking as you type, optional completion,
// auto-indent, and the settings that decide all of it.
//
// #### What is deliberately *not* here
//
// The find bar drives `CodeEditor`'s search methods, so searching is here and the
// bar is the view over it (`find_bar.hpp`). The outline, the minimap and a
// formatter stay separate: each needs a parse of its own, and mixing them into one
// 2,000-line widget is how the Python implementation got there.
//
// #### Where the checking runs
//
// `lint.hpp` is a pure function of (language, text), and this class calls it on a
// debounce. The purity is the point: the rules are tested against strings rather
// than against a widget, and the editor is tested for *where* a finding is
// painted, not for whether it is correct.

#pragma once

#include <QColor>
#include <QList>
#include <QPlainTextEdit>
#include <QString>
#include <QStringList>

#include "timewarp/ui/language.hpp"
#include "timewarp/ui/lint.hpp"

class QCompleter;
class QKeyEvent;
class QResizeEvent;
class QTimer;
class QWidget;

namespace timewarp::ui {

class CodeEditor;
class SyntaxHighlighter;

/// Everything about the editor a user can change.
///
/// One struct rather than a dozen setters because the preferences dialog edits it
/// as a whole and the window applies it to every open tab: a half-applied setting
/// -- a new font but the old tab width -- is not a state anybody asked for.
struct EditorSettings {
    /// Empty means "the platform's fixed-width font", which is the right answer
    /// on a machine whose fonts are unknown.
    QString font_family;
    int font_point_size = 11;
    /// Spaces a tab advances to.
    int tab_width = 4;
    bool show_line_numbers = true;
    bool highlight_current_line = true;
    bool word_wrap = false;
    /// Copy the previous line's indentation, and add one level after a block
    /// opener (`FOR`, `IF ... THEN`, `SUB`, `[`).
    bool auto_indent = true;
    /// Offer the language's words as you type. Off by default: a popup that
    /// appears unasked over a beginner's first `PRINT` is an interruption.
    bool completion = false;
    /// Check the text as it changes and mark what is wrong.
    bool lint = true;
    /// Draw a middle dot for each space and an arrow for each tab, so the
    /// difference between indenting with one and the other is visible.
    bool show_whitespace = false;
    /// The strip at the right edge showing the whole document at a glance.
    bool minimap = true;
    /// Keep a copy of a document that has unsaved changes, so a crash costs the
    /// last few minutes rather than the whole buffer.
    bool autosave = true;
    /// How often, in seconds. The floor is ten: a snapshot per keystroke is a
    /// disk-write loop, not a safety net.
    int autosave_seconds = 120;
};

/// Paints the line numbers beside a `CodeEditor`.
///
/// A child widget rather than a margin, because Qt's text-edit margins cannot be
/// painted into and a child can be given an exact width from the editor's own
/// layout maths.
class LineNumberArea final : public QWidget {
    Q_OBJECT

public:
    explicit LineNumberArea(CodeEditor* editor);

    [[nodiscard]] QSize sizeHint() const override;

protected:
    void paintEvent(QPaintEvent* event) override;

private:
    CodeEditor* editor_;
};

/// Paints the strip at the right edge: the whole document, one row per sampled
/// line, with the viewport marked.
///
/// A child widget in the right margin, like the gutter, because that is how the
/// text area is kept out of its way: a minimap painted *over* the text would
/// cover the end of every long line.
class MinimapArea final : public QWidget {
    Q_OBJECT

public:
    explicit MinimapArea(CodeEditor* editor);

protected:
    void paintEvent(QPaintEvent* event) override;
    void mousePressEvent(QMouseEvent* event) override;
    void mouseMoveEvent(QMouseEvent* event) override;

private:
    CodeEditor* editor_;
};

/// A code editor with line numbers, a current-line highlight, language-specific
/// checking and optional completion.
class CodeEditor final : public QPlainTextEdit {
    Q_OBJECT

public:
    explicit CodeEditor(QWidget* parent = nullptr);

    // -- settings ---------------------------------------------------------

    /// Applies `settings` and re-renders: font, tab stops, wrapping, gutter,
    /// current-line band, and whether checking and completion are on. Idempotent,
    /// and called on every tab the window adopts.
    void applySettings(const EditorSettings& settings);
    [[nodiscard]] const EditorSettings& settings() const noexcept { return settings_; }

    // -- checking ---------------------------------------------------------

    /// Replaces the findings and repaints the markers. Called by `revalidate`,
    /// and directly by a test that wants a marker without a typing delay.
    void setDiagnostics(const QList<Diagnostic>& diagnostics);
    [[nodiscard]] const QList<Diagnostic>& diagnostics() const noexcept {
        return diagnostics_;
    }
    /// The findings on a 1-based line, in column order.
    [[nodiscard]] QList<Diagnostic> diagnosticsForLine(int line) const;
    [[nodiscard]] int errorCount() const;
    [[nodiscard]] int warningCount() const;

    /// Runs the language's checks now and applies the result. What the debounce
    /// ends in, and what opening a file does immediately.
    void revalidate();

    /// The colours the problem markers are drawn in. Set from the theme, so a
    /// red squiggle on a red-phosphor theme is still a visible one.
    void setDiagnosticColors(const QColor& error, const QColor& warning);

    // -- completion -------------------------------------------------------

    /// What would be completed at the cursor: the word being typed, or the empty
    /// string.
    [[nodiscard]] QString completionPrefix() const;
    /// Opens the completion popup for the word at the cursor, if there is one.
    /// Ctrl+Space, and what typing a word character does when completion is on.
    void showCompletion();
    [[nodiscard]] bool completionVisible() const;

    // -- indentation ------------------------------------------------------

    /// Indents the selected lines by one level, or removes one level when
    /// `outdent`. With no selection, Tab inserts a level and Shift+Tab outdents
    /// the current line.
    void indentSelection(bool outdent);

    // -- minimap ----------------------------------------------------------

    /// The strip's width in pixels: 0 when it is switched off, which is also how
    /// the text area reclaims the space.
    [[nodiscard]] int minimapWidth() const;
    [[nodiscard]] bool minimapVisible() const { return settings_.minimap; }
    /// Turns it on or off. Part of the settings, so it persists with the rest.
    void setMinimapVisible(bool visible);
    /// Where a click at `y` inside the strip scrolls to, in scrollbar units.
    /// Separate from the mouse handler so the mapping can be tested without
    /// synthesising an event.
    [[nodiscard]] int scrollValueForMinimapY(int y) const;
    /// Paints the strip. Called by `MinimapArea`.
    void paintMinimap(QPaintEvent* event);

    /// The width the gutter needs for the document's current line count. Exposed
    /// because `LineNumberArea` asks for it while painting.
    [[nodiscard]] int lineNumberAreaWidth() const;

    /// Paints the gutter. Called by `LineNumberArea`.
    void paintLineNumbers(QPaintEvent* event);

    /// Moves the cursor to a 1-based line and centres it, clamped to the document
    /// rather than refused -- "go to line 9999" in a 10-line file should land on
    /// the last line, not silently do nothing.
    void goToLine(int line);

    /// 1-based cursor line and column, for the status bar.
    [[nodiscard]] int currentLine() const;
    [[nodiscard]] int currentColumn() const;

    // -- search -----------------------------------------------------------
    //
    // Here rather than in the find bar because the document is here, and because
    // a search is the kind of thing that has to be testable without a window
    // around it. The bar is a view over these four calls.

    /// Selects the next occurrence of `text`, wrapping at the end of the
    /// document. Returns false, having changed nothing, when there is no match
    /// -- an empty search term included.
    bool findNext(const QString& text, bool caseSensitive, bool backwards = false);

    /// How many occurrences of `text` the document holds.
    [[nodiscard]] int matchCount(const QString& text, bool caseSensitive) const;

    /// Replaces every occurrence, as one undo step, and returns how many were
    /// replaced. A replacement that contains the search term does not loop:
    /// each search resumes after the text just inserted.
    int replaceAll(const QString& text, const QString& replacement, bool caseSensitive);

    /// Replaces the occurrence at the cursor when the cursor is sitting on one,
    /// then moves to the next. Returns false when the cursor was not on a match,
    /// so the caller can search first rather than replace something arbitrary.
    bool replaceCurrent(const QString& text, const QString& replacement,
                        bool caseSensitive);

    /// Colours the document for `language`.
    ///
    /// Called by the window whenever a document's language changes, which is not
    /// only when it is created: saving a scratch buffer as `mandala.logo` turns
    /// BASIC source into Logo and has to recolour it.
    void setLanguage(Language language);
    /// The language the document is currently coloured as.
    [[nodiscard]] Language language() const noexcept;
    /// The highlighter itself, so a test can assert what rules it installed.
    [[nodiscard]] SyntaxHighlighter* highlighter() const noexcept { return highlighter_; }

signals:
    /// The cursor moved, either by typing, by click, or programmatically.
    ///
    /// Named `cursorMoved` rather than `cursorPositionChanged` on purpose:
    /// QPlainTextEdit already emits a signal with that name and no arguments, and
    /// a derived signal of the same name would hide it -- so a later
    /// `connect(editor, &QPlainTextEdit::cursorPositionChanged, ...)` and
    /// `&CodeEditor::cursorPositionChanged` would mean two different things.
    void cursorMoved(int line, int column);

protected:
    void resizeEvent(QResizeEvent* event) override;
    /// Enter auto-indents, Tab indents a selection, and typing a word opens the
    /// completion popup when it is enabled.
    void keyPressEvent(QKeyEvent* event) override;
    /// The tooltip over a marked word: the finding's message.
    bool event(QEvent* event) override;

private:
    void updateGutterWidth(int blockCount);
    /// Places the strip in the right margin after a resize or a toggle.
    void updateMinimapGeometry();
    void highlightCurrentLine();
    /// Rebuilds the extra selections: the current-line band and the problem
    /// underlines, which share one list because a `QPlainTextEdit` has one.
    void refreshExtraSelections();
    void scheduleLint();
    void rebuildCompletionModel();
    void insertCompletion(const QString& word);
    /// The diagnostic under a viewport position, or null.
    [[nodiscard]] const Diagnostic* diagnosticAt(const QPoint& position) const;
    /// The current line's text, without its newline.
    [[nodiscard]] QString currentLineText() const;
    /// Whether the line ends in something that opens a block, so the next line
    /// should be indented one more level.
    [[nodiscard]] bool lineOpensBlock(const QString& line) const;
    /// One level of indentation, as spaces.
    [[nodiscard]] QString indentUnit() const;

    LineNumberArea* gutter_ = nullptr;
    MinimapArea* minimap_ = nullptr;
    /// Owned by the document, which takes it as a child: `QSyntaxHighlighter`'s
    /// parent is the `QTextDocument`, so the document destroys it.
    SyntaxHighlighter* highlighter_ = nullptr;

    EditorSettings settings_;
    QList<Diagnostic> diagnostics_;
    /// Debounce for the checker: typing a word must not run the rules on every
    /// keystroke, and a red squiggle that appears mid-word is noise.
    QTimer* lint_timer_ = nullptr;
    QCompleter* completer_ = nullptr;
    QStringList completion_words_;
    QColor error_colour_{0xD6, 0x45, 0x45};
    QColor warning_colour_{0xD9, 0xA1, 0x3A};
};

}  // namespace timewarp::ui
