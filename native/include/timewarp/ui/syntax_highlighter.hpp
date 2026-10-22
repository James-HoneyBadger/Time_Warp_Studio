// SPDX-License-Identifier: MIT
//
// Syntax highlighting, one rule table per language.
//
// The ten languages are not conventionally coloured text, so a single shared
// approximation with a keyword list bolted on gets several of them wrong:
//
//   * FOCAL has no comment syntax at all -- an unrecognised command is an error,
//     which is a deliberate part of the dialect rather than an omission.
//   * REXX has both `--` and `/* ... */`, and the block form is what a listing
//     actually uses.
//   * Smalltalk's comment is a **double**-quoted string and its string is a
//     single-quoted one, which is the reverse of everything else here.
//   * PILOT's units are a single letter and a colon at the start of a line
//     (`T:`, `A:`, `M:`), which is a shape, not a word.
//   * CHIP-8 is a bytecode listing: opcodes, hex literals and labels.
//
// So the rules are data per language. The keyword lists come from the engine: the
// names `native/tools/gates/inventory.cpp` extracts from the executors, which is
// the same inventory the command-coverage gate uses. That is not tidiness for its
// own sake -- a highlighter that colours a word the engine would reject teaches
// the wrong vocabulary, and the vocabulary is the whole point of a teaching IDE.
//
// #### What is not here
//
// Colour choice. The formats are built from `SyntaxColors`, a plain struct, so
// the themes in `docs/ROADMAP.md` can set them without touching any rule.

#pragma once

#include <QColor>
#include <QRegularExpression>
#include <QString>
#include <QStringList>
#include <QSyntaxHighlighter>
#include <QTextCharFormat>
#include <QVector>

#include "timewarp/ui/language.hpp"

class QTextDocument;

namespace timewarp::ui {

/// The colours a document is painted in.
///
/// Five roles, because that is how many distinctions the ten languages need
/// between them.
struct SyntaxColors {
    QColor keyword;
    QColor comment;
    QColor string;
    QColor number;
    /// PILOT's unit letters, CHIP-8's labels, Scheme's `#t`/`#f`.
    QColor directive;

    /// Colours that read on the palette the application actually applies.
    /// `ui_style.cpp` styles from `palette(...)` rather than from fixed colours,
    /// so these are chosen to survive both a light and a dark base.
    [[nodiscard]] static SyntaxColors readableOnAnyBase();
};

/// Colours source for one language.
///
/// Attach to a document and set the language; each `setLanguage` re-highlights.
/// Nothing here reads a file or a widget, which is what lets the test suite drive
/// it against a bare `QTextDocument` with no window.
class SyntaxHighlighter final : public QSyntaxHighlighter {
    Q_OBJECT

public:
    explicit SyntaxHighlighter(QTextDocument* document,
                               Language language = Language::Unknown);

    /// Switches the rule table and re-highlights the document.
    ///
    /// Not a construction-time decision: a document's language changes the moment
    /// it is saved under a name, and a `.bas` renamed to `.logo` has to recolour.
    void setLanguage(Language language);
    [[nodiscard]] Language language() const noexcept { return language_; }

    /// How many word rules the current language installed. Zero for `Unknown`,
    /// which is what makes "nothing is coloured" an assertion a test can make
    /// rather than an absence it cannot.
    [[nodiscard]] int ruleCount() const noexcept {
        return static_cast<int>(rules_.size());
    }

    /// Whether this language matches keywords without regard to case. Six of the
    /// ten do, and the engine uppercases before every lookup.
    [[nodiscard]] bool keywordsAreCaseInsensitive() const noexcept {
        return case_insensitive_;
    }

    /// Replaces the colours. Highlighting is redone, because a format already
    /// applied to a block keeps the colour it was built with.
    void setColors(const SyntaxColors& colors);

private:
    struct Rule {
        QRegularExpression pattern;
        QTextCharFormat format;
    };

    /// The word rules for a language: keywords, numbers, directives.
    QVector<Rule> rules_;
    /// The patterns that delimit a string, e.g. `"` with backslash escapes.
    QVector<QRegularExpression> string_patterns_;
    /// Literal prefixes that comment out the rest of a line: `--`, `//`, `;`.
    QStringList line_comments_;
    /// Words that do the same, matched at a word boundary: BASIC's `REM`.
    QVector<QRegularExpression> comment_words_;
    /// `/*` and `*/`, or Smalltalk's `"` twice. Empty when the language has none.
    QString block_open_;
    QString block_close_;

    QTextCharFormat keyword_format_;
    QTextCharFormat comment_format_;
    QTextCharFormat string_format_;
    QTextCharFormat number_format_;
    QTextCharFormat directive_format_;

    Language language_ = Language::Unknown;
    bool case_insensitive_ = false;

protected:
    void highlightBlock(const QString& text) override;
};

/// The words a language colours, for a test to check the table against the
/// engine's inventory without going through a document.
[[nodiscard]] QStringList keywordsOf(Language language);

/// Where code stops and prose starts, for a tool that has to read the *code*.
///
/// The lint pass (`lint.hpp`) skips comments and string bodies, so it has to
/// agree with the highlighter about where they begin -- and the two disagreeing
/// is how a checker reports a misspelling inside a comment. Exposed rather than
/// copied so there is one table.
struct LanguageSyntax {
    /// Prefixes that comment out the rest of a line: `'`, `;`, `--`, `//`.
    QStringList line_comments;
    /// Words that do the same, matched at a word boundary: BASIC's `REM`.
    QStringList comment_words;
    /// A comment that spans lines (`/*` … `*/`, or Smalltalk's `"` twice).
    /// Empty when the language has none.
    QString block_open;
    QString block_close;
    /// String delimiters.
    QStringList strings;
    /// Whether the language matches keywords without regard to case.
    bool case_insensitive = false;
};

/// The syntax of one language, from the highlighter's own table.
[[nodiscard]] LanguageSyntax syntaxOf(Language language);

}  // namespace timewarp::ui
