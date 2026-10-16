// SPDX-License-Identifier: MIT
//
// Tests for the syntax highlighter.
//
// A highlighter is easy to get *nearly* right and impossible to check by looking
// at a screenshot: coloured text that is one token out of place still looks like
// a working editor, and the mistake only shows up when it teaches a learner the
// wrong thing. So each test names a position and the colour that position must
// have, and the interesting cases are the collisions between rules:
//
//   * a keyword inside a string is not a keyword
//   * a comment marker inside a string is not a comment
//   * Smalltalk's comment is a double-quoted "string" and its string is a
//     single-quoted one, which is the reverse of every other language here
//   * REXX's block comment outlives the line it starts on
//
// Runs offscreen, like the other UI suites, so it needs no display.

#include <cstdio>

#include <QApplication>
#include <QCoreApplication>
#include <QString>
#include <QTextBlock>
#include <QTextDocument>
#include <QTextLayout>

#include "check.hpp"
#include "timewarp/ui/language.hpp"
#include "timewarp/ui/syntax_highlighter.hpp"

namespace {

using timewarp::ui::Language;
using timewarp::ui::SyntaxColors;
using timewarp::ui::SyntaxHighlighter;
using timewarp::ui::keywordsOf;

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

/// The colour applied at `position` of `blockNumber`, or an invalid colour when
/// nothing was applied there.
[[nodiscard]] QColor colorAt(const QTextDocument& document, int blockNumber,
                             int position) {
    const QTextBlock block = document.findBlockByNumber(blockNumber);
    if (!block.isValid() || block.layout() == nullptr) {
        return QColor{};
    }
    const QVector<QTextLayout::FormatRange> formats = block.layout()->formats();
    for (const QTextLayout::FormatRange& range : formats) {
        if (position >= range.start && position < range.start + range.length) {
            return range.format.foreground().color();
        }
    }
    return QColor{};
}

/// A document with `source` in it, coloured as `language`.
struct Coloured {
    QTextDocument document;
    SyntaxHighlighter highlighter;

    Coloured(const QString& source, Language language)
        : highlighter{&document, language} {
        document.setPlainText(source);
        highlighter.rehighlight();
    }
};

[[nodiscard]] const SyntaxColors& colors() {
    static const SyntaxColors value = SyntaxColors::readableOnAnyBase();
    return value;
}

// ---------------------------------------------------------------------------
// The collisions between rules
// ---------------------------------------------------------------------------

void test_a_keyword_inside_a_string_is_not_a_keyword() {
    announce("a string beats a keyword");
    const Coloured colour{QStringLiteral("PRINT \"IF THEN ELSE\""), Language::Basic};

    // `PRINT` is a command, `IF` is a word the engine would reject there.
    TW_CHECK_EQ(colorAt(colour.document, 0, 0), colors().keyword);
    TW_CHECK_EQ(colorAt(colour.document, 0, 7), colors().string);
    TW_CHECK_EQ(colorAt(colour.document, 0, 8), colors().string);
}

void test_a_comment_marker_inside_a_string_is_not_a_comment() {
    announce("a string beats a comment");
    const Coloured colour{QStringLiteral("PRINT \"a ; b\" ; note"), Language::Logo};

    // The first `;` is inside the string, so only the second one comments.
    const int semicolon_in_string = 9;
    TW_CHECK_EQ(colorAt(colour.document, 0, semicolon_in_string), colors().string);
    const int semicolon_outside = 15;
    TW_CHECK_EQ(colorAt(colour.document, 0, semicolon_outside), colors().comment);
}

void test_smalltalk_quotes_are_the_other_way_round() {
    announce("Smalltalk: \"...\" comments, '...' strings");
    const Coloured colour{
        QStringLiteral("\"a comment\" Transcript show: 'text'"), Language::Smalltalk};

    TW_CHECK_EQ(colorAt(colour.document, 0, 1), colors().comment);
    // `Transcript` is a class name, so it is a keyword; the `'text'` after it is
    // a string. They are adjacent, which is the case that catches a delimiter
    // table where the two are swapped.
    TW_CHECK_EQ(colorAt(colour.document, 0, 13), colors().keyword);
    TW_CHECK_EQ(colorAt(colour.document, 0, 30), colors().string);
}

void test_a_block_comment_outlives_its_line() {
    announce("REXX: /* ... */ spans lines");
    const Coloured colour{QStringLiteral("SAY 'first'\n/* note\nstill note\n*/\nSAY 'last'"),
                          Language::Rexx};

    TW_CHECK_EQ(colorAt(colour.document, 0, 0), colors().keyword);
    TW_CHECK_EQ(colorAt(colour.document, 1, 1), colors().comment);
    // The middle line has no marker of its own: it is coloured only because the
    // previous line left the block comment open.
    TW_CHECK_EQ(colorAt(colour.document, 2, 0), colors().comment);
    // And the line after the close is code again.
    TW_CHECK_EQ(colorAt(colour.document, 4, 0), colors().keyword);
}

// ---------------------------------------------------------------------------
// The shapes that are not words
// ---------------------------------------------------------------------------

void test_pilot_units_are_a_shape_not_a_keyword() {
    announce("PILOT: T:, A:, M:");
    const Coloured colour{QStringLiteral("T: Hello there\nA: answer"), Language::Pilot};

    TW_CHECK_EQ(colorAt(colour.document, 0, 0), colors().directive);
    TW_CHECK_EQ(colorAt(colour.document, 1, 0), colors().directive);
    // The text after a unit is not coloured: it is the operand.
    TW_CHECK(colorAt(colour.document, 0, 3) != colors().directive);
}

void test_chip8_hex_labels_and_comments() {
    announce("CHIP-8: opcodes, labels, hex, `;`");
    const Coloured colour{QStringLiteral("start: LD V0, 0x1F ; load"), Language::Chip8};

    TW_CHECK_EQ(colorAt(colour.document, 0, 0), colors().directive);
    TW_CHECK_EQ(colorAt(colour.document, 0, 7), colors().keyword);
    TW_CHECK_EQ(colorAt(colour.document, 0, 14), colors().number);
    TW_CHECK_EQ(colorAt(colour.document, 0, 21), colors().comment);
}

void test_apl_comments_are_its_own_symbol() {
    announce("APL: the comment symbol, and a string");
    const Coloured colour{
        QStringLiteral("'text' \u235D a comment"), Language::Apl};

    TW_CHECK_EQ(colorAt(colour.document, 0, 1), colors().string);
    TW_CHECK_EQ(colorAt(colour.document, 0, 9), colors().comment);
}

// ---------------------------------------------------------------------------
// Case, and the switching of rulesets
// ---------------------------------------------------------------------------

void test_case_sensitivity_follows_the_engine() {
    announce("case: BASIC ignores it, Scheme does not");
    const Coloured basic{QStringLiteral("print 1"), Language::Basic};
    TW_CHECK(basic.highlighter.keywordsAreCaseInsensitive());
    TW_CHECK_EQ(colorAt(basic.document, 0, 0), colors().keyword);

    const Coloured scheme{QStringLiteral("DISPLAY 1"), Language::Scheme};
    TW_CHECK(!scheme.highlighter.keywordsAreCaseInsensitive());
    // Scheme's `display` is a builtin; `DISPLAY` is a different symbol, and a
    // highlighter that coloured it would hide a real mistake.
    TW_CHECK(colorAt(scheme.document, 0, 0) != colors().keyword);
    const Coloured lower{QStringLiteral("display 1"), Language::Scheme};
    TW_CHECK_EQ(colorAt(lower.document, 0, 0), colors().keyword);
}

void test_switching_the_language_switches_the_rules() {
    announce("a document can change language");
    QTextDocument document;
    SyntaxHighlighter highlighter{&document, Language::Basic};
    document.setPlainText(QStringLiteral("WEND"));
    // A bare document with no widget never asks for a block to be laid out, so
    // the highlighter's on-demand formatting has not run yet.
    highlighter.rehighlight();

    TW_CHECK_EQ(colorAt(document, 0, 0), colors().keyword);

    highlighter.setLanguage(Language::Scheme);
    TW_CHECK_EQ(highlighter.language(), Language::Scheme);
    highlighter.rehighlight();
    // `WEND` is not a Scheme symbol, so the same text must lose its colour --
    // which is the whole content of "the ruleset changed".
    TW_CHECK(colorAt(document, 0, 0) != colors().keyword);
}

void test_an_unknown_language_colours_nothing() {
    announce("an unknown language is not guessed at");
    const Coloured colour{QStringLiteral("PRINT 1 ; anything"), Language::Unknown};

    TW_CHECK_EQ(colour.highlighter.ruleCount(), 0);
    TW_CHECK_EQ(colorAt(colour.document, 0, 0), QColor{});
}

// ---------------------------------------------------------------------------
// The keyword tables
// ---------------------------------------------------------------------------

void test_the_keyword_tables_hold_the_engines_names() {
    announce("keyword tables");
    TW_CHECK(keywordsOf(Language::Basic).contains(QStringLiteral("PRINT")));
    TW_CHECK(keywordsOf(Language::Basic).contains(QStringLiteral("WEND")));
    TW_CHECK(keywordsOf(Language::Logo).contains(QStringLiteral("FORWARD")));
    TW_CHECK(keywordsOf(Language::Logo).contains(QStringLiteral("SETPC")));
    TW_CHECK(keywordsOf(Language::Rexx).contains(QStringLiteral("SAY")));
    TW_CHECK(keywordsOf(Language::Chip8).contains(QStringLiteral("DRW")));
    TW_CHECK(keywordsOf(Language::Scheme).contains(QStringLiteral("set!")));
    TW_CHECK(keywordsOf(Language::Smalltalk).contains(QStringLiteral("forward:")));

    // Every language the IDE offers has a table, so none of them is silently
    // uncoloured -- `Unknown` is the only empty one.
    for (const Language language : timewarp::ui::allLanguages()) {
        TW_CHECK(!keywordsOf(language).isEmpty());
    }
}

}  // namespace

int main(int argc, char** argv) {
    qputenv("QT_QPA_PLATFORM", "offscreen");
    QApplication app{argc, argv};
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpTests"));
    QCoreApplication::setApplicationName(QStringLiteral("SyntaxHighlighterTests"));

    test_a_keyword_inside_a_string_is_not_a_keyword();
    test_a_comment_marker_inside_a_string_is_not_a_comment();
    test_smalltalk_quotes_are_the_other_way_round();
    test_a_block_comment_outlives_its_line();

    test_pilot_units_are_a_shape_not_a_keyword();
    test_chip8_hex_labels_and_comments();
    test_apl_comments_are_its_own_symbol();

    test_case_sensitivity_follows_the_engine();
    test_switching_the_language_switches_the_rules();
    test_an_unknown_language_colours_nothing();

    test_the_keyword_tables_hold_the_engines_names();

    return tw::test::summary("syntax_highlighter");
}
