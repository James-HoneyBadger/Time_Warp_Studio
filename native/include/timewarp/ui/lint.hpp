// SPDX-License-Identifier: MIT
//
// Language-specific checking, on the text, as you type.
//
// The IDE teaches eleven languages whose grammars disagree about nearly everything,
// so "does this look right?" has to be answered per language rather than by one
// generic linter. What the eleven *do* share is that a beginner's mistakes are
// structural and local: a bracket left open, a string with no closing quote, a
// `FOR` with no `NEXT`, a keyword typed one letter wrong. Those are what this
// checks, and it checks them from the text alone.
//
// #### Why not the engine
//
// The real parsers are in `native/src/lang/`, and asking them would be exact.
// It is also the wrong tool here: a program is a *line at a time* in most of
// these languages, the compiler-grade answer arrives only when the whole program
// is well-formed enough to parse, and a half-typed `FOR i = 1 TO` has to produce
// something useful rather than "syntax error". The engine's verdict is what
// **Run** gives; this is the earlier, cheaper opinion.
//
// #### What it will not do
//
// It does not claim a program is correct. A clean lint means "nothing here is
// definitely broken", which is why every finding is either an error the language
// itself refuses (an unclosed `[`, an unterminated comment) or a warning that
// names the rule it applied.
//
// #### Where the vocabulary comes from
//
// `keywordsOf()` -- the same table the syntax highlighter paints from, which is
// the engine's own command inventory. A checker that knows a word the interpreter
// does not, or misses one it does, teaches the wrong vocabulary: the whole point
// of a teaching IDE is that the words on screen are the words that run.

#pragma once

#include <QList>
#include <QString>

#include "timewarp/ui/language.hpp"

namespace timewarp::ui {

/// One finding: where it is, how bad it is, and what to do about it.
struct Diagnostic {
    enum class Severity {
        /// The language will refuse this. An unclosed bracket, a `NEXT` with no
        /// `FOR`, a string that never ends.
        Error,
        /// Legal, but almost certainly not what was meant: a keyword one letter
        /// away from a real one.
        Warning,
    };

    /// 1-based, matching what the gutter and `CodeEditor::goToLine` use.
    int line = 1;
    /// 1-based column where the finding starts.
    int column = 1;
    /// How many characters on `line` to underline. At least 1.
    int length = 1;
    Severity severity = Severity::Error;
    QString message;
    /// Stable id, e.g. `"basic.for-without-next"`. Used by tests, and by anybody
    /// who wants to waive one rule.
    QString rule;
};

/// Checks `text` as `language`.
///
/// Pure: no widget, no document, no clock. The editor calls it on a debounce and
/// the tests call it directly, which is what keeps "the squiggle is in the wrong
/// place" a failing assertion rather than a screenshot.
[[nodiscard]] QList<Diagnostic> lintText(Language language, const QString& text);

/// The severities in `diagnostics` of this level or worse.
[[nodiscard]] int countAtLeast(const QList<Diagnostic>& diagnostics,
                               Diagnostic::Severity severity);

}  // namespace timewarp::ui
