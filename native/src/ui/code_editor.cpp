// SPDX-License-Identifier: MIT

#include "timewarp/ui/code_editor.hpp"

#include <algorithm>

#include <QCompleter>
#include <QAbstractItemView>
#include <QEvent>
#include <QFont>
#include <QFontDatabase>
#include <QFontMetrics>
#include <QHelpEvent>
#include <QKeyEvent>
#include <QPainter>
#include <QPaintEvent>
#include <QPolygon>
#include <QRegularExpression>
#include <QResizeEvent>
#include <QScrollBar>
#include <QSet>
#include <QStringListModel>
#include <QTextBlock>
#include <QTextCharFormat>
#include <QTextCursor>
#include <QTextDocument>
#include <QTimer>
#include <QToolTip>

#include "timewarp/ui/syntax_highlighter.hpp"

namespace timewarp::ui {

namespace {

/// Space either side of the digits in the gutter.
constexpr int kGutterPadding = 6;

/// The gutter never shrinks below this, so the numbers do not jiggle while typing
/// on line 9 of a new file.
constexpr int kMinimumGutterWidth = 24;

/// Room at the right of the gutter for a problem marker.
constexpr int kMarkerWidth = 8;

/// Number of spaces a tab inserts. Matching the Python editor's four matters more
/// than any argument for a different number: a file re-saved by either editor
/// should not reflow.
constexpr int kTabWidthInSpaces = 4;

/// How long typing has to pause before the document is checked again. Long
/// enough that a word is finished, short enough that the marker appears while the
/// mistake is still the thing being looked at.
constexpr int kLintDelayMilliseconds = 350;

/// The editor font. The Python editor asks for these in order and takes whatever
/// the system has; so does Qt, which is why the same stack is used.
[[nodiscard]] QFont editorFont(const EditorSettings& settings) {
    QFont font;
    if (settings.font_family.isEmpty()) {
        font = QFontDatabase::systemFont(QFontDatabase::FixedFont);
        font.setStyleHint(QFont::Monospace);
        font.setFixedPitch(true);
    } else {
        font = QFont{settings.font_family};
        font.setStyleHint(QFont::Monospace);
    }
    font.setPointSize(std::max(6, settings.font_point_size));
    return font;
}

/// Characters that make up a word, for completion.
[[nodiscard]] bool isWordCharacter(QChar character) {
    return character.isLetterOrNumber() || character == QLatin1Char('_') ||
           character == QLatin1Char('-') || character == QLatin1Char('?') ||
           character == QLatin1Char('!');
}

/// The leading spaces and tabs of `text`.
[[nodiscard]] QString leadingWhitespace(const QString& text) {
    int index = 0;
    while (index < text.size() && (text.at(index) == QLatin1Char(' ') ||
                                   text.at(index) == QLatin1Char('\t'))) {
        ++index;
    }
    return text.left(index);
}

}  // namespace

LineNumberArea::LineNumberArea(CodeEditor* editor)
    : QWidget(editor), editor_(editor) {
    setObjectName(QStringLiteral("lineNumberArea"));
}

QSize LineNumberArea::sizeHint() const {
    return QSize(editor_->lineNumberAreaWidth(), 0);
}

void LineNumberArea::paintEvent(QPaintEvent* event) { editor_->paintLineNumbers(event); }

CodeEditor::CodeEditor(QWidget* parent) : QPlainTextEdit(parent) {
    setObjectName(QStringLiteral("codeEditor"));
    setFont(editorFont(settings_));
    setTabStopDistance(QFontMetricsF{font()}.horizontalAdvance(QLatin1Char(' ')) *
                       kTabWidthInSpaces);
    setLineWrapMode(QPlainTextEdit::NoWrap);
    setFrameShape(QFrame::NoFrame);

    gutter_ = new LineNumberArea(this);

    // Colouring belongs to the document, not the widget, so it survives a tab
    // being dragged to the other editor pane. The window sets the real language
    // as soon as the document is adopted; this is the default for a scratch
    // buffer that has no name yet.
    highlighter_ = new SyntaxHighlighter(document(), defaultLanguage());

    lint_timer_ = new QTimer{this};
    lint_timer_->setSingleShot(true);
    lint_timer_->setInterval(kLintDelayMilliseconds);
    connect(lint_timer_, &QTimer::timeout, this, &CodeEditor::revalidate);

    completer_ = new QCompleter{this};
    completer_->setWidget(this);
    completer_->setCompletionMode(QCompleter::PopupCompletion);
    completer_->setCaseSensitivity(Qt::CaseInsensitive);
    completer_->setModel(new QStringListModel{completer_});
    completer_->popup()->setObjectName(QStringLiteral("completionPopup"));
    connect(completer_, QOverload<const QString&>::of(&QCompleter::activated), this,
            &CodeEditor::insertCompletion);

    connect(this, &QPlainTextEdit::blockCountChanged, this, &CodeEditor::updateGutterWidth);
    connect(this, &QPlainTextEdit::updateRequest, this,
            [this](const QRect& rect, int dy) {
                if (gutter_ == nullptr) {
                    return;
                }
                if (dy != 0) {
                    gutter_->scroll(0, dy);
                } else {
                    gutter_->update(0, rect.y(), gutter_->width(), rect.height());
                }
            });
    // QPlainTextEdit's own signal has no arguments and a different name is not an
    // option for it, so the position is read here and re-emitted with coordinates.
    connect(this, &QPlainTextEdit::cursorPositionChanged, this, [this] {
        highlightCurrentLine();
        emit cursorMoved(currentLine(), currentColumn());
    });
    connect(this, &QPlainTextEdit::textChanged, this, [this] { scheduleLint(); });

    updateGutterWidth(blockCount());
    highlightCurrentLine();
}

// ---------------------------------------------------------------------------
// Settings
// ---------------------------------------------------------------------------

void CodeEditor::applySettings(const EditorSettings& settings) {
    settings_ = settings;
    setFont(editorFont(settings_));
    setTabStopDistance(QFontMetricsF{font()}.horizontalAdvance(QLatin1Char(' ')) *
                       std::max(1, settings_.tab_width));
    setLineWrapMode(settings_.word_wrap ? QPlainTextEdit::WidgetWidth
                                        : QPlainTextEdit::NoWrap);
    updateGutterWidth(blockCount());
    highlightCurrentLine();
    if (!settings_.lint) {
        // Turning checking off has to clear what it already drew, or the marks
        // stay on screen with nothing left to remove them.
        setDiagnostics({});
    } else {
        scheduleLint();
    }
    if (!settings_.completion && completer_ != nullptr) {
        completer_->popup()->hide();
    }
}

QString CodeEditor::indentUnit() const {
    return QString(std::max(1, settings_.tab_width), QLatin1Char(' '));
}

// ---------------------------------------------------------------------------
// Checking
// ---------------------------------------------------------------------------

void CodeEditor::scheduleLint() {
    if (lint_timer_ != nullptr && settings_.lint) {
        lint_timer_->start();
    }
}

void CodeEditor::revalidate() {
    if (!settings_.lint) {
        setDiagnostics({});
        return;
    }
    setDiagnostics(lintText(language(), toPlainText()));
}

void CodeEditor::setDiagnostics(const QList<Diagnostic>& diagnostics) {
    diagnostics_ = diagnostics;
    refreshExtraSelections();
    if (gutter_ != nullptr) {
        gutter_->update();
    }
}

QList<Diagnostic> CodeEditor::diagnosticsForLine(int line) const {
    QList<Diagnostic> found;
    for (const Diagnostic& diagnostic : diagnostics_) {
        if (diagnostic.line == line) {
            found.append(diagnostic);
        }
    }
    return found;
}

int CodeEditor::errorCount() const {
    return countAtLeast(diagnostics_, Diagnostic::Severity::Error);
}

int CodeEditor::warningCount() const {
    return countAtLeast(diagnostics_, Diagnostic::Severity::Warning) - errorCount();
}

void CodeEditor::setDiagnosticColors(const QColor& error, const QColor& warning) {
    error_colour_ = error;
    warning_colour_ = warning;
    refreshExtraSelections();
    if (gutter_ != nullptr) {
        gutter_->update();
    }
}

const Diagnostic* CodeEditor::diagnosticAt(const QPoint& position) const {
    const QTextCursor cursor = cursorForPosition(position);
    const int line_number = cursor.blockNumber() + 1;
    const int column = cursor.positionInBlock() + 1;
    for (const Diagnostic& diagnostic : diagnostics_) {
        if (diagnostic.line != line_number) {
            continue;
        }
        // A one-character finding still has to be hoverable, so the test is
        // "on or just after", not a strict interval.
        if (column >= diagnostic.column &&
            column <= diagnostic.column + std::max(1, diagnostic.length)) {
            return &diagnostic;
        }
    }
    return nullptr;
}

// ---------------------------------------------------------------------------
// Completion
// ---------------------------------------------------------------------------

QString CodeEditor::completionPrefix() const {
    const QTextCursor cursor = textCursor();
    const QString line = cursor.block().text();
    int start = cursor.positionInBlock();
    while (start > 0 && isWordCharacter(line.at(start - 1))) {
        --start;
    }
    return line.mid(start, cursor.positionInBlock() - start);
}

void CodeEditor::rebuildCompletionModel() {
    if (completer_ == nullptr) {
        return;
    }
    QStringList words = keywordsOf(language());
    // Identifiers already in the document, so a learner's own procedure names
    // complete too. The whole text is scanned rather than one line: names are
    // defined before they are used.
    static const QRegularExpression identifier{QStringLiteral(R"([A-Za-z_][A-Za-z0-9_]*)")};
    QRegularExpressionMatchIterator matches = identifier.globalMatch(toPlainText());
    QSet<QString> seen;
    while (matches.hasNext()) {
        const QString word = matches.next().captured(0);
        if (word.size() > 1 && !seen.contains(word)) {
            seen.insert(word);
            words.append(word);
        }
    }
    words.removeDuplicates();
    words.sort(Qt::CaseInsensitive);
    completion_words_ = words;

    auto* model = qobject_cast<QStringListModel*>(completer_->model());
    if (model != nullptr) {
        model->setStringList(completion_words_);
    }
}

void CodeEditor::showCompletion() {
    if (completer_ == nullptr || !settings_.completion) {
        return;
    }
    const QString prefix = completionPrefix();
    if (prefix.isEmpty()) {
        return;
    }
    rebuildCompletionModel();
    completer_->setCompletionPrefix(prefix);
    if (completer_->completionCount() == 0) {
        completer_->popup()->hide();
        return;
    }
    // One candidate that is exactly what has been typed is not a choice.
    if (completer_->completionCount() == 1 &&
        completer_->currentCompletion().compare(prefix, Qt::CaseInsensitive) == 0) {
        completer_->popup()->hide();
        return;
    }
    completer_->popup()->setCurrentIndex(completer_->completionModel()->index(0, 0));
    QRect rectangle = cursorRect();
    rectangle.setWidth(completer_->popup()->sizeHintForColumn(0) +
                       completer_->popup()->verticalScrollBar()->sizeHint().width() + 8);
    completer_->complete(rectangle);
}

bool CodeEditor::completionVisible() const {
    return completer_ != nullptr && completer_->popup()->isVisible();
}

void CodeEditor::insertCompletion(const QString& word) {
    if (completer_ == nullptr) {
        return;
    }
    const QString prefix = completer_->completionPrefix();
    if (prefix.isEmpty()) {
        return;
    }
    QTextCursor cursor = textCursor();
    // Replace the prefix rather than appending: the popup's word already
    // contains it.
    cursor.movePosition(QTextCursor::Left, QTextCursor::KeepAnchor, prefix.size());
    cursor.insertText(word);
    setTextCursor(cursor);
}

void CodeEditor::setLanguage(Language language) {
    if (highlighter_ != nullptr) {
        highlighter_->setLanguage(language);
    }
    // A different language means different rules, so what was flagged under the
    // old one is stale. Re-checked at once rather than on the next keystroke: a
    // project saved as `.logo` should not keep a BASIC complaint until it is
    // typed in.
    revalidate();
}

Language CodeEditor::language() const noexcept {
    return highlighter_ == nullptr ? Language::Unknown : highlighter_->language();
}

namespace {

/// The flags `QTextDocument::find` takes for a plain-text search.
[[nodiscard]] QTextDocument::FindFlags findFlags(bool caseSensitive, bool backwards) {
    QTextDocument::FindFlags flags;
    if (caseSensitive) {
        flags |= QTextDocument::FindCaseSensitively;
    }
    if (backwards) {
        flags |= QTextDocument::FindBackward;
    }
    return flags;
}

}  // namespace

bool CodeEditor::findNext(const QString& text, bool caseSensitive, bool backwards) {
    if (text.isEmpty()) {
        return false;
    }
    const QTextDocument::FindFlags flags = findFlags(caseSensitive, backwards);

    QTextCursor found = document()->find(text, textCursor(), flags);
    if (found.isNull()) {
        // Wrap. A search that stops at the end of the file is a search the user
        // has to scroll to continue, and the Python IDE wrapped as well.
        found = backwards ? document()->find(text, document()->characterCount(), flags)
                          : document()->find(text, 0, flags);
    }
    if (found.isNull()) {
        return false;
    }
    setTextCursor(found);
    ensureCursorVisible();
    return true;
}

int CodeEditor::matchCount(const QString& text, bool caseSensitive) const {
    if (text.isEmpty()) {
        return 0;
    }
    const QTextDocument::FindFlags flags = findFlags(caseSensitive, false);
    int count = 0;
    QTextCursor found = document()->find(text, 0, flags);
    while (!found.isNull()) {
        ++count;
        found = document()->find(text, found, flags);
    }
    return count;
}

int CodeEditor::replaceAll(const QString& text, const QString& replacement,
                           bool caseSensitive) {
    if (text.isEmpty()) {
        return 0;
    }
    const QTextDocument::FindFlags flags = findFlags(caseSensitive, false);

    QTextCursor editor{document()};
    editor.beginEditBlock();
    int count = 0;
    QTextCursor found = document()->find(text, 0, flags);
    while (!found.isNull()) {
        found.insertText(replacement);
        ++count;
        // Resume *after* what was just inserted, so `a` -> `aa` terminates.
        found = document()->find(text, found, flags);
    }
    editor.endEditBlock();
    return count;
}

bool CodeEditor::replaceCurrent(const QString& text, const QString& replacement,
                                bool caseSensitive) {
    if (text.isEmpty()) {
        return false;
    }
    QTextCursor cursor = textCursor();
    const QString selected = cursor.selectedText();
    const Qt::CaseSensitivity sensitivity =
        caseSensitive ? Qt::CaseSensitive : Qt::CaseInsensitive;
    if (selected.compare(text, sensitivity) != 0) {
        return false;
    }
    QTextCursor editor{document()};
    editor.beginEditBlock();
    cursor.insertText(replacement);
    editor.endEditBlock();
    return true;
}

int CodeEditor::lineNumberAreaWidth() const {
    if (!settings_.show_line_numbers) {
        // The gutter stays, because the problem markers live in it. Only the
        // digits go, and with them the space they were measuring for.
        return kMarkerWidth + 2 * kGutterPadding;
    }
    const int digits = QString::number(std::max(1, blockCount())).size();
    return std::max(kMinimumGutterWidth,
                    kGutterPadding + QFontMetrics{font()}.horizontalAdvance(QLatin1Char('9')) *
                                        digits +
                        kGutterPadding + kMarkerWidth);
}

void CodeEditor::paintLineNumbers(QPaintEvent* event) {
    QPainter painter(gutter_);
    painter.fillRect(event->rect(), palette().color(QPalette::AlternateBase));

    QTextBlock block = firstVisibleBlock();
    int blockNumber = block.blockNumber();
    int top = qRound(blockBoundingGeometry(block).translated(contentOffset()).top());
    int bottom = top + qRound(blockBoundingRect(block).height());
    const int currentLineNumber = textCursor().blockNumber();
    const int textWidth = gutter_->width() - kGutterPadding - kMarkerWidth;

    while (block.isValid() && top <= event->rect().bottom()) {
        if (block.isVisible() && bottom >= event->rect().top()) {
            const int line_number = blockNumber + 1;
            if (settings_.show_line_numbers) {
                const QString number = QString::number(line_number);
                const bool isCurrent = blockNumber == currentLineNumber;
                painter.setPen(isCurrent ? palette().color(QPalette::Text)
                                         : palette().color(QPalette::Mid));
                painter.drawText(0, top, textWidth, QFontMetrics{font()}.height(),
                                 Qt::AlignRight, number);
            }

            // The problem marker: a triangle for an error, a smaller one for a
            // warning, drawn against the right edge of the gutter so it lines up
            // in a column -- a learner scans the markers, not the numbers.
            bool has_error = false;
            bool has_warning = false;
            for (const Diagnostic& diagnostic : diagnostics_) {
                if (diagnostic.line != line_number) {
                    continue;
                }
                if (diagnostic.severity == Diagnostic::Severity::Error) {
                    has_error = true;
                } else {
                    has_warning = true;
                }
            }
            if (has_error || has_warning) {
                const int height = QFontMetrics{font()}.height();
                const int size = has_error ? std::max(6, height / 2) : std::max(4, height / 3);
                const int left = gutter_->width() - kGutterPadding - size;
                const int centre = top + height / 2;
                QPolygon triangle;
                triangle << QPoint{left, centre - size / 2}
                         << QPoint{left + size, centre - size / 2}
                         << QPoint{left + size / 2, centre + size / 2};
                painter.setRenderHint(QPainter::Antialiasing, true);
                painter.setPen(Qt::NoPen);
                painter.setBrush(has_error ? error_colour_ : warning_colour_);
                painter.drawPolygon(triangle);
                painter.setBrush(Qt::NoBrush);
            }
        }
        block = block.next();
        top = bottom;
        bottom = top + qRound(blockBoundingRect(block).height());
        ++blockNumber;
    }
}

void CodeEditor::goToLine(int line) {
    const int target = std::clamp(line, 1, std::max(1, blockCount()));
    const QTextBlock block = document()->findBlockByNumber(target - 1);
    if (!block.isValid()) {
        return;
    }
    QTextCursor cursor{block};
    cursor.movePosition(QTextCursor::StartOfBlock);
    setTextCursor(cursor);
    centerCursor();
}

int CodeEditor::currentLine() const { return textCursor().blockNumber() + 1; }

int CodeEditor::currentColumn() const {
    return textCursor().positionInBlock() + 1;
}

void CodeEditor::resizeEvent(QResizeEvent* event) {
    QPlainTextEdit::resizeEvent(event);
    const QRect contents = contentsRect();
    gutter_->setGeometry(QRect{contents.left(), contents.top(), lineNumberAreaWidth(),
                               contents.height()});
}

void CodeEditor::updateGutterWidth(int /*blockCount*/) {
    setViewportMargins(lineNumberAreaWidth(), 0, 0, 0);
}

void CodeEditor::highlightCurrentLine() { refreshExtraSelections(); }

void CodeEditor::refreshExtraSelections() {
    QList<QTextEdit::ExtraSelection> selections;

    // The current-line band first, so a problem underline is drawn over it rather
    // than being painted out by it.
    if (settings_.highlight_current_line && !isReadOnly()) {
        QTextEdit::ExtraSelection band;
        band.format.setBackground(palette().color(QPalette::AlternateBase));
        band.format.setProperty(QTextFormat::FullWidthSelection, true);
        band.cursor = textCursor();
        band.cursor.clearSelection();
        selections.append(band);
    }

    // The underlines. A squiggle per finding, clamped to its own line: a length
    // that ran past the end of the block would be invisible, and an unclamped one
    // would underline the next line's first characters.
    for (const Diagnostic& diagnostic : diagnostics_) {
        const QTextBlock block = document()->findBlockByNumber(diagnostic.line - 1);
        if (!block.isValid()) {
            continue;
        }
        const int start = block.position() + std::min(diagnostic.column - 1,
                                                      block.length() - 1);
        const int available = std::max(1, block.length() - 1 - (start - block.position()));
        const int length = std::clamp(diagnostic.length, 1, available);

        QTextEdit::ExtraSelection selection;
        selection.format.setUnderlineStyle(QTextCharFormat::WaveUnderline);
        selection.format.setUnderlineColor(diagnostic.severity == Diagnostic::Severity::Error
                                               ? error_colour_
                                               : warning_colour_);
        selection.cursor = QTextCursor{document()};
        selection.cursor.setPosition(start);
        selection.cursor.setPosition(start + length, QTextCursor::KeepAnchor);
        selections.append(selection);
    }

    setExtraSelections(selections);
}

// ---------------------------------------------------------------------------
// Keys
// ---------------------------------------------------------------------------

QString CodeEditor::currentLineText() const {
    return textCursor().block().text();
}

bool CodeEditor::lineOpensBlock(const QString& line) const {
    const QString trimmed = line.trimmed();
    if (trimmed.isEmpty()) {
        return false;
    }
    // The words that begin a block in any of the ten languages, plus the bracket
    // that begins one in six of them. Checked case-insensitively because most of
    // the languages are, and a false positive only costs one indent level.
    static const QStringList openers{QStringLiteral("FOR"),   QStringLiteral("WHILE"),
                                     QStringLiteral("DO"),    QStringLiteral("REPEAT"),
                                     QStringLiteral("SUB"),   QStringLiteral("PROC"),
                                     QStringLiteral("FUNC"),  QStringLiteral("FUNCTION"),
                                     QStringLiteral("IF"),    QStringLiteral("SELECT"),
                                     QStringLiteral("CASE"),  QStringLiteral("TYPE"),
                                     QStringLiteral("DEFINE"), QStringLiteral("LET"),
                                     QStringLiteral("LAMBDA"), QStringLiteral("BEGIN"),
                                     QStringLiteral("COND"),  QStringLiteral("WHEN"),
                                     QStringLiteral("ELSE"),  QStringLiteral("ELSEIF")};
    const QString first = trimmed.section(QLatin1Char(' '), 0, 0).toUpper();
    const QString without_comment = first.section(QLatin1Char(';'), 0, 0);
    if (openers.contains(without_comment)) {
        // `IF x THEN PRINT 1` is one line and opens nothing.
        if (without_comment == QLatin1String("IF")) {
            const int then_at = trimmed.indexOf(QLatin1String("THEN"), 0, Qt::CaseInsensitive);
            if (then_at >= 0 && !trimmed.mid(then_at + 4).trimmed().isEmpty()) {
                return false;
            }
        }
        return true;
    }
    // A line ending in an opening bracket opens a block in Logo, Scheme, APL and
    // Smalltalk.
    return trimmed.endsWith(QLatin1Char('[')) || trimmed.endsWith(QLatin1Char('('));
}

void CodeEditor::keyPressEvent(QKeyEvent* event) {
    if (isReadOnly()) {
        QPlainTextEdit::keyPressEvent(event);
        return;
    }

    const bool plain = !(event->modifiers() & (Qt::ControlModifier | Qt::AltModifier));

    if (plain && (event->key() == Qt::Key_Return || event->key() == Qt::Key_Enter) &&
        settings_.auto_indent) {
        // Handled here rather than in a `textChanged` hook so the indent lands in
        // the same undo step as the newline.
        const QString line = currentLineText();
        QString indent = leadingWhitespace(line);
        if (lineOpensBlock(line)) {
            indent += indentUnit();
        }
        QPlainTextEdit::keyPressEvent(event);
        if (!indent.isEmpty()) {
            QTextCursor cursor = textCursor();
            cursor.insertText(indent);
            setTextCursor(cursor);
        }
        return;
    }

    if (event->key() == Qt::Key_Tab || event->key() == Qt::Key_Backtab) {
        indentSelection(event->key() == Qt::Key_Backtab ||
                        ((event->modifiers() & Qt::ShiftModifier) != 0));
        return;
    }

    // Ctrl+Space asks for completion even when the setting has it off, which is
    // what a learner will reach for before finding the preference.
    if (event->key() == Qt::Key_Space && (event->modifiers() & Qt::ControlModifier)) {
        if (!settings_.completion) {
            EditorSettings enabled = settings_;
            enabled.completion = true;
            applySettings(enabled);
        }
        showCompletion();
        return;
    }

    QPlainTextEdit::keyPressEvent(event);

    // Typing a word character re-opens the list at the new prefix. The popup does
    // not appear for one character: `showCompletion` decides that from the count.
    if (settings_.completion && !event->text().isEmpty() &&
        isWordCharacter(event->text().at(0))) {
        showCompletion();
    }
}

void CodeEditor::indentSelection(bool outdent) {
    if (isReadOnly()) {
        return;
    }
    QTextCursor cursor = textCursor();

    if (!cursor.hasSelection()) {
        QTextCursor line = cursor;
        line.movePosition(QTextCursor::StartOfBlock);
        if (outdent) {
            int removed = 0;
            while (removed < static_cast<int>(indentUnit().size()) &&
                   line.position() < cursor.position() &&
                   (document()->characterAt(line.position()) == QLatin1Char(' ') ||
                    document()->characterAt(line.position()) == QLatin1Char('\t'))) {
                line.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor, 1);
                ++removed;
            }
            if (removed > 0) {
                line.removeSelectedText();
            }
        } else {
            cursor.insertText(indentUnit());
            setTextCursor(cursor);
        }
        return;
    }

    const int first = document()->findBlock(cursor.selectionStart()).blockNumber();
    const int last = document()->findBlock(cursor.selectionEnd()).blockNumber();

    cursor.beginEditBlock();
    // Bottom-up, so the block numbers of the lines still to visit do not move.
    for (int line = last; line >= first; --line) {
        const QTextBlock block = document()->findBlockByNumber(line);
        if (!block.isValid()) {
            continue;
        }
        QTextCursor edit{block};
        edit.movePosition(QTextCursor::StartOfBlock);
        if (outdent) {
            int removed = 0;
            while (removed < static_cast<int>(indentUnit().size()) &&
                   (document()->characterAt(edit.position()) == QLatin1Char(' ') ||
                    document()->characterAt(edit.position()) == QLatin1Char('\t'))) {
                edit.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor, 1);
                ++removed;
            }
            if (removed > 0) {
                edit.removeSelectedText();
            }
        } else {
            edit.insertText(indentUnit());
        }
    }
    cursor.endEditBlock();

    // The selection is put back over the same lines, so Tab can be held down.
    const QTextBlock first_block = document()->findBlockByNumber(first);
    const QTextBlock last_block = document()->findBlockByNumber(last);
    if (first_block.isValid() && last_block.isValid()) {
        QTextCursor restored{document()};
        restored.setPosition(first_block.position());
        restored.setPosition(last_block.position() + std::max(0, last_block.length() - 1),
                             QTextCursor::KeepAnchor);
        setTextCursor(restored);
    }
}

bool CodeEditor::event(QEvent* event) {
    if (event->type() == QEvent::ToolTip) {
        auto* help = static_cast<QHelpEvent*>(event);
        if (const Diagnostic* diagnostic = diagnosticAt(help->pos()); diagnostic != nullptr) {
            QToolTip::showText(help->globalPos(), diagnostic->message, this);
            return true;
        }
    }
    return QPlainTextEdit::event(event);
}

}  // namespace timewarp::ui
