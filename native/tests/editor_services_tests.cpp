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

#include <QStringList>

#include "check.hpp"
#include "timewarp/ui/formatter.hpp"
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

}  // namespace

int main() {
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
    return tw::test::summary("editor_services");
}
