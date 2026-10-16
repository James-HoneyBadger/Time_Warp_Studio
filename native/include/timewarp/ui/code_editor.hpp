// SPDX-License-Identifier: MIT
//
// The code editor.
//
// A `QPlainTextEdit` with a line-number gutter, a highlighted current line and
// cursor tracking, because those are the three things that make a text box feel
// like an editor rather than a scratchpad. Syntax highlighting, the minimap,
// find/replace, the outline and the formatter layer on top of this later; the
// Python editor is 2,254 lines because of exactly those extras, and they are
// worth adding one at a time against a working base.

#pragma once

#include <QPlainTextEdit>
#include <QString>

class QResizeEvent;
class QWidget;

namespace timewarp::ui {

class CodeEditor;

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

/// A code editor with line numbers and a current-line highlight.
class CodeEditor final : public QPlainTextEdit {
    Q_OBJECT

public:
    explicit CodeEditor(QWidget* parent = nullptr);

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

private:
    void updateGutterWidth(int blockCount);
    void highlightCurrentLine();

    LineNumberArea* gutter_ = nullptr;
};

}  // namespace timewarp::ui
