// SPDX-License-Identifier: MIT
//
// The IDE's text services: the things that rewrite or read a document without
// running it.
//
// This is a plain `main` with no window, because none of these need one --
// formatting is a function from text to text, and a test that opens a window to
// check a string is a test that fails for unrelated reasons.
//
// The formatter's three invariants are asserted for every sample, because they
// are what makes re-indenting safe on somebody's program:
//
//  1. **Text is preserved.** Strip both versions line by line and they are
//     equal: only the left margin moved.
//  2. **It is idempotent.** Formatting twice is formatting once; otherwise a
//     save loop drifts the file.
//  3. **It says so.** `changed_lines` is 0 exactly when the text is unchanged.

#include <cstdio>
#include <string>

#include <QCoreApplication>
#include <QHash>
#include <QSet>
#include <QStringList>

#include "check.hpp"
#include "temp_dir.hpp"
#include "timewarp/ui/find_in_files.hpp"
#include "timewarp/ui/formatter.hpp"
#include "timewarp/ui/snippets.hpp"
#include "timewarp/ui/language.hpp"

namespace {

using timewarp::ui::formatSource;
using timewarp::ui::FormatOutcome;
using timewarp::ui::Language;

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

/// Checks the three invariants every format must hold, whatever it produced.
void check_invariants(const QString& input, const QString& output, const FormatOutcome& outcome) {
    const QStringList before = input.split(QLatin1Char('\n'));
    const QStringList after = output.split(QLatin1Char('\n'));
    TW_CHECK_EQ(before.size(), after.size());
    for (int index = 0; index < before.size() && index < after.size(); ++index) {
        // 1. Only leading whitespace changed.
        //
        // `simplified()`, not `trimmed()`: BASIC's indentation sits *after* the
        // line number, so it is internal whitespace and trimming would keep it.
        // Collapsing every run compares the words and punctuation, which is the
        // part that must not move.
        TW_CHECK_EQ(before.at(index).simplified().toStdString(),
                    after.at(index).simplified().toStdString());
        // A blank line stays blank rather than collecting indentation.
        if (before.at(index).trimmed().isEmpty()) {
            TW_CHECK(after.at(index).trimmed().isEmpty());
        }
    }
    // 3. `changed_lines` agrees with the text.
    TW_CHECK_EQ(outcome.changed(), output != input);
}

void expect(const QString& input, Language language, int width, const QString& expected) {
    const FormatOutcome outcome = formatSource(input, language, width);
    TW_CHECK_EQ(outcome.text.toStdString(), expected.toStdString());
    check_invariants(input, outcome.text, outcome);
    // 2. Idempotent.
    const FormatOutcome again = formatSource(outcome.text, language, width);
    TW_CHECK_EQ(again.text.toStdString(), outcome.text.toStdString());
    TW_CHECK_EQ(again.changed_lines, 0);
    TW_CHECK_EQ(again.message.toStdString(), std::string{"Already formatted."});
}

void test_basic_indents_a_loop_body() {
    announce("format: BASIC loop body");
    expect(QStringLiteral("10 FOR I = 1 TO 3\n"
                          "20 PRINT I\n"
                          "30 NEXT I\n"),
           Language::Basic, 4,
           QStringLiteral("10 FOR I = 1 TO 3\n"
                          "20     PRINT I\n"
                          "30 NEXT I\n"));
}

void test_basic_else_body_is_indented() {
    announce("format: BASIC ELSE body");
    // The removed implementation dedented at `ELSE` and never re-indented, so
    // the else-branch sat one level left of the if-branch. This is the assertion
    // that keeps that from coming back.
    expect(QStringLiteral("10 IF X > 1 THEN\n"
                          "20 PRINT \"big\"\n"
                          "30 ELSE\n"
                          "40 PRINT \"small\"\n"
                          "50 END IF\n"),
           Language::Basic, 4,
           QStringLiteral("10 IF X > 1 THEN\n"
                          "20     PRINT \"big\"\n"
                          "30 ELSE\n"
                          "40     PRINT \"small\"\n"
                          "50 END IF\n"));
}

void test_basic_single_line_if_opens_nothing() {
    announce("format: BASIC single-line IF");
    expect(QStringLiteral("10 IF X > 1 THEN PRINT \"yes\"\n"
                          "20 PRINT \"after\"\n"),
           Language::Basic, 4,
           QStringLiteral("10 IF X > 1 THEN PRINT \"yes\"\n"
                          "20 PRINT \"after\"\n"));
}

void test_basic_comment_does_not_open_a_block() {
    announce("format: BASIC REM is not a block");
    expect(QStringLiteral("10 REM FOR EVER AND EVER\n"
                          "20 PRINT \"flat\"\n"),
           Language::Basic, 4,
           QStringLiteral("10 REM FOR EVER AND EVER\n"
                          "20 PRINT \"flat\"\n"));
}

void test_indent_width_is_honoured() {
    announce("format: indent width");
    expect(QStringLiteral("10 FOR I = 1 TO 3\n"
                          "20 PRINT I\n"
                          "30 NEXT I\n"),
           Language::Basic, 2,
           QStringLiteral("10 FOR I = 1 TO 3\n"
                          "20   PRINT I\n"
                          "30 NEXT I\n"));
}

void test_logo_procedure_and_bracket_blocks() {
    announce("format: Logo procedure and brackets");
    expect(QStringLiteral("TO TRI\n"
                          "REPEAT 3 [\n"
                          "FORWARD 50\n"
                          "RIGHT 120\n"
                          "]\n"
                          "END\n"),
           Language::Logo, 2,
           QStringLiteral("TO TRI\n"
                          "  REPEAT 3 [\n"
                          "    FORWARD 50\n"
                          "    RIGHT 120\n"
                          "  ]\n"
                          "END\n"));
}

void test_logo_balanced_brackets_open_nothing() {
    announce("format: balanced Logo brackets");
    // A one-line block must not indent the line after it: the brackets are the
    // block, and they are already closed.
    expect(QStringLiteral("TO SQ\n"
                          "REPEAT 4 [ FORWARD 50 RIGHT 90 ]\n"
                          "END\n"),
           Language::Logo, 2,
           QStringLiteral("TO SQ\n"
                          "  REPEAT 4 [ FORWARD 50 RIGHT 90 ]\n"
                          "END\n"));
}

void test_comal_procedure() {
    announce("format: COMAL proc");
    expect(QStringLiteral("PROC hello\n"
                          "PRINT \"hi\"\n"
                          "ENDPROC\n"),
           Language::Comal, 4,
           QStringLiteral("PROC hello\n"
                          "    PRINT \"hi\"\n"
                          "ENDPROC\n"));
}

void test_rexx_do_block() {
    announce("format: REXX DO block");
    expect(QStringLiteral("DO i = 1 TO 3\n"
                          "SAY i\n"
                          "END\n"),
           Language::Rexx, 4,
           QStringLiteral("DO i = 1 TO 3\n"
                          "    SAY i\n"
                          "END\n"));
}

void test_rexx_single_line_block() {
    announce("format: REXX one-line DO");
    // `DO i=1 TO 3; SAY i; END` opens and closes on its own line, so the line
    // after it is not part of a block.
    expect(QStringLiteral("DO i = 1 TO 3; SAY i; END\n"
                          "SAY \"after\"\n"),
           Language::Rexx, 4,
           QStringLiteral("DO i = 1 TO 3; SAY i; END\n"
                          "SAY \"after\"\n"));
}

void test_a_language_without_blocks_says_so() {
    announce("format: FOCAL refuses honestly");
    const QString source = QStringLiteral("1.10 T \"HELLO\"\n"
                                          "1.20 T !\n");
    const FormatOutcome outcome = formatSource(source, Language::Focal, 4);
    TW_CHECK_EQ(outcome.text.toStdString(), source.toStdString());
    TW_CHECK_EQ(outcome.changed_lines, 0);
    TW_CHECK(!outcome.changed());
    TW_CHECK(outcome.message.contains(QStringLiteral("no block structure")));
    TW_CHECK(!timewarp::ui::canFormat(Language::Focal));
    TW_CHECK(timewarp::ui::canFormat(Language::Basic));
}

void test_line_endings_survive() {
    announce("format: CRLF and a missing final newline");
    // The re-indent must not rewrite every line ending: a document saved on
    // Windows that comes back with LF-only endings is a whole-file diff.
    const FormatOutcome outcome = formatSource(
        QStringLiteral("10 FOR I = 1 TO 3\r\n20 PRINT I\r\n30 NEXT I"),
        Language::Basic, 4);
    TW_CHECK_EQ(outcome.text.toStdString(),
                std::string{"10 FOR I = 1 TO 3\r\n20     PRINT I\r\n30 NEXT I"});
    TW_CHECK(!outcome.text.endsWith(QLatin1Char('\n')));
    TW_CHECK_EQ(outcome.changed_lines, 1);
    TW_CHECK_EQ(outcome.message.toStdString(), std::string{"Formatted 1 line."});
}

void test_blank_lines_stay_blank() {
    announce("format: blank lines");
    expect(QStringLiteral("10 FOR I = 1 TO 3\n"
                          "\n"
                          "20 PRINT I\n"
                          "30 NEXT I\n"),
           Language::Basic, 4,
           QStringLiteral("10 FOR I = 1 TO 3\n"
                          "\n"
                          "20     PRINT I\n"
                          "30 NEXT I\n"));
}


// ---------------------------------------------------------------------------
// Find in Files
// ---------------------------------------------------------------------------

/// Three files and a binary, so the interesting questions are answerable: which
/// files are searched, which lines match, and what a bad pattern does.
void make_search_tree(const tw::test::TempDir& scratch) {
    scratch.write(QStringLiteral("a.bas"),
                  "10 PRINT \"needle\"\n20 REM nothing\n30 PRINT \"needle\"\n");
    scratch.write(QStringLiteral("nested/b.logo"),
                  "TO GO\nPRINT \"NEEDLE\"\nEND\n");
    scratch.write(QStringLiteral("nested/deep/c.txt"), "needle at the end\n");
    // A NUL byte in the first block: the search must skip it rather than treat a
    // binary as a one-line file.
    scratch.write(QStringLiteral("blob.bin"),
                  QByteArray("needle\0\0\0binary", 17));
    // A file in a directory the search prunes.
    scratch.write(QStringLiteral("build/generated.bas"), "10 PRINT \"needle\"\n");
}

void test_find_in_files_matches_every_file() {
    announce("find in files: matches, skipping and pruning");
    tw::test::TempDir scratch;
    TW_CHECK(scratch.valid());
    if (!scratch.valid()) {
        return;
    }
    make_search_tree(scratch);

    timewarp::ui::FindQuery query;
    query.term = QStringLiteral("needle");
    QString error;
    const QList<timewarp::ui::FindHit> hits =
        timewarp::ui::searchInFiles(scratch.path(), query, &error);
    TW_CHECK(error.isEmpty());

    QStringList files;
    for (const timewarp::ui::FindHit& hit : hits) {
        files << QFileInfo{hit.path}.fileName();
    }
    files.removeDuplicates();
    files.sort();
    // `a.bas` has two hits, `b.logo` one (case-insensitively), `c.txt` one. The
    // binary is skipped and `build/` is pruned.
    TW_CHECK_EQ(hits.size(), 4);
    TW_CHECK_EQ(files.join(QLatin1Char('|')).toStdString(),
                std::string{"a.bas|b.logo|c.txt"});

    // Line numbers are 1-based and point at the matching line.
    for (const timewarp::ui::FindHit& hit : hits) {
        if (QFileInfo{hit.path}.fileName() == QStringLiteral("c.txt")) {
            TW_CHECK_EQ(hit.line, 1);
            TW_CHECK_EQ(hit.text.toStdString(), std::string{"needle at the end"});
        }
    }
}

void test_find_in_files_options() {
    announce("find in files: case, regex, bad pattern, cap");
    tw::test::TempDir scratch;
    TW_CHECK(scratch.valid());
    if (!scratch.valid()) {
        return;
    }
    make_search_tree(scratch);

    timewarp::ui::FindQuery case_sensitive;
    case_sensitive.term = QStringLiteral("NEEDLE");
    case_sensitive.case_sensitive = true;
    QString error;
    const QList<timewarp::ui::FindHit> upper =
        timewarp::ui::searchInFiles(scratch.path(), case_sensitive, &error);
    // Only the Logo file spells it in capitals.
    TW_CHECK_EQ(upper.size(), 1);
    TW_CHECK(QFileInfo{upper.isEmpty() ? QString{} : upper.first().path}.fileName() ==
             QStringLiteral("b.logo"));

    timewarp::ui::FindQuery regex;
    regex.term = QStringLiteral("^10 PRINT");
    regex.regex = true;
    const QList<timewarp::ui::FindHit> anchored =
        timewarp::ui::searchInFiles(scratch.path(), regex, &error);
    // One: only `a.bas` opens with `10 PRINT` on a line of its own. The second
    // `PRINT` in that file is numbered 30, and the copy under `build/` is pruned.
    TW_CHECK_EQ(anchored.size(), 1);
    if (!anchored.isEmpty()) {
        TW_CHECK_EQ(anchored.first().line, 1);
    }

    timewarp::ui::FindQuery broken;
    broken.term = QStringLiteral("[unclosed");
    broken.regex = true;
    error.clear();
    const QList<timewarp::ui::FindHit> none =
        timewarp::ui::searchInFiles(scratch.path(), broken, &error);
    // A broken pattern is an error, not "no matches": the two are different
    // answers and a learner needs the one that says what is wrong.
    TW_CHECK(none.isEmpty());
    TW_CHECK(!error.isEmpty());
    TW_CHECK(error.contains(QStringLiteral("regular expression")));

    timewarp::ui::FindQuery capped;
    capped.term = QStringLiteral("needle");
    capped.max_hits = 2;
    TW_CHECK_EQ(timewarp::ui::searchInFiles(scratch.path(), capped, &error).size(), 2);

    // And an empty term is refused rather than matching every line.
    timewarp::ui::FindQuery empty;
    error.clear();
    TW_CHECK(timewarp::ui::searchInFiles(scratch.path(), empty, &error).isEmpty());
    TW_CHECK(!error.isEmpty());
}

void test_find_in_files_stops_when_the_callback_says_so() {
    announce("find in files: streaming callback");
    tw::test::TempDir scratch;
    TW_CHECK(scratch.valid());
    if (!scratch.valid()) {
        return;
    }
    make_search_tree(scratch);

    timewarp::ui::FindQuery query;
    query.term = QStringLiteral("needle");
    QString error;
    int seen = 0;
    const QList<timewarp::ui::FindHit> hits = timewarp::ui::searchInFiles(
        scratch.path(), query, &error, [&seen](const timewarp::ui::FindHit&) {
            ++seen;
            return seen < 2;  // stop after the second hit
        });
    TW_CHECK_EQ(seen, 2);
    TW_CHECK_EQ(hits.size(), 2);
}


// ---------------------------------------------------------------------------
// Snippets
// ---------------------------------------------------------------------------

/// The shipped catalogue, as a user gets it. This is the test that keeps the file
/// honest: a snippet in a language this build does not know, a body that would
/// glue onto the next line, two entries with one id, or a second `$CURSOR$` are
/// all failures rather than surprises at insertion time.
void test_the_shipped_snippet_catalogue_is_sound() {
    announce("snippets: the shipped catalogue");
    TW_CHECK(!timewarp::ui::snippetRoot().isEmpty());
    QString error;
    const QList<timewarp::ui::Snippet> snippets = timewarp::ui::loadSnippets(&error);
    TW_CHECK(error.isEmpty());
    TW_CHECK(!snippets.isEmpty());

    QSet<QString> ids;
    QHash<int, int> per_language;
    for (const timewarp::ui::Snippet& snippet : snippets) {
        TW_CHECK(!snippet.name.isEmpty());
        TW_CHECK(!snippet.description.isEmpty());
        TW_CHECK(snippet.language != Language::Unknown);
        TW_CHECK(snippet.body.endsWith(QLatin1Char('\n')));
        TW_CHECK_EQ(snippet.body.count(QStringLiteral("$CURSOR$")), 1);
        TW_CHECK(!ids.contains(snippet.id()));
        ids.insert(snippet.id());
        ++per_language[static_cast<int>(snippet.language)];
    }
    // Every language has at least two, so the picker is never empty for a
    // document the IDE can open.
    for (const Language language : timewarp::ui::allLanguages()) {
        TW_CHECK(per_language.value(static_cast<int>(language)) >= 2);
    }
}

void test_snippet_insertion_resolves_placeholders() {
    announce("snippets: placeholders and the caret");
    // The caret lands where $CURSOR$ was, and the marker is gone from the text.
    const timewarp::ui::SnippetInsertion plain =
        timewarp::ui::resolveSnippet(QStringLiteral("10 PRINT 1\n20 $CURSOR$END\n"), QString{});
    TW_CHECK_EQ(plain.text.toStdString(), std::string{"10 PRINT 1\n20 END\n"});
    TW_CHECK_EQ(plain.caret, 14);

    // A selection replaces $SELECTION$.
    const timewarp::ui::SnippetInsertion wrapped =
        timewarp::ui::resolveSnippet(QStringLiteral("PRINT [$SELECTION$]\n$CURSOR$"),
                                     QStringLiteral("hello"));
    TW_CHECK_EQ(wrapped.text.toStdString(), std::string{"PRINT [hello]\n"});
    TW_CHECK_EQ(wrapped.caret, 14);

    // No marker at all: the caret goes to the end of what was inserted.
    const timewarp::ui::SnippetInsertion bare =
        timewarp::ui::resolveSnippet(QStringLiteral("SAY 'hi'\n"), QString{});
    TW_CHECK_EQ(bare.caret, int(bare.text.size()));

    // A placeholder this build does not know is left exactly as written: silently
    // dropping text somebody put in the catalogue is the one thing a substitution
    // must not do.
    const timewarp::ui::SnippetInsertion unknown =
        timewarp::ui::resolveSnippet(QStringLiteral("x $DATE$ y\n"), QString{});
    TW_CHECK_EQ(unknown.text.toStdString(), std::string{"x $DATE$ y\n"});
}

}  // namespace

int main(int argc, char** argv) {
    // Needed for `QCoreApplication::applicationDirPath()`, which is where the
    // snippet and example resolvers start looking. No widget is created.
    QCoreApplication app{argc, argv};
    test_basic_indents_a_loop_body();
    test_basic_else_body_is_indented();
    test_basic_single_line_if_opens_nothing();
    test_basic_comment_does_not_open_a_block();
    test_indent_width_is_honoured();
    test_logo_procedure_and_bracket_blocks();
    test_logo_balanced_brackets_open_nothing();
    test_comal_procedure();
    test_rexx_do_block();
    test_rexx_single_line_block();
    test_a_language_without_blocks_says_so();
    test_line_endings_survive();
    test_blank_lines_stay_blank();
    test_find_in_files_matches_every_file();
    test_find_in_files_options();
    test_find_in_files_stops_when_the_callback_says_so();
    test_the_shipped_snippet_catalogue_is_sound();
    test_snippet_insertion_resolves_placeholders();
    return tw::test::summary("editor_services");
}
