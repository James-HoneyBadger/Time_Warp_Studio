// SPDX-License-Identifier: MIT
//
// Re-indenting a document, for the languages whose layout has blocks.
//
// Ported from the removed implementation's `utils/code_formatter.py`, which
// walked a line list with an indent level and a table of keywords that open,
// close, or open-and-close a block. The port keeps the shape and fixes three
// things the original got wrong:
//
//  * **`ELSE` re-indents.** The original listed `ELSE`/`ELSEIF` as dedent-only,
//    so every else-branch body sat one level too far left. The list of
//    "dedent then indent" keywords existed and was never used.
//  * **Nothing but leading whitespace changes.** The original also upper-cased
//    BASIC keywords, which alters string literals. `formatSource` cannot change
//    a line's text: strip the indentation from its output and it is the input,
//    line for line. A test asserts exactly that.
//  * **A language without blocks says so** instead of returning the source and
//    claiming it was formatted.
//
// It is deliberately not a parser. A formatter that guesses wrong reformats a
// learner's program into one that means something else; one that only moves the
// left margin of lines it recognises cannot.

#pragma once

#include <QString>

#include "timewarp/ui/language.hpp"

namespace timewarp::ui {

/// The result of asking for a format.
struct FormatOutcome {
    /// The source, re-indented. Equal to the input when `changed_lines` is 0.
    QString text;
    /// What to show the user: how many lines moved, or why nothing did.
    QString message;
    /// How many lines' indentation changed. 0 means the document was untouched.
    int changed_lines = 0;

    [[nodiscard]] bool changed() const { return changed_lines > 0; }
};

/// Re-indents `source` for `language`, using `indent_width` spaces per level.
///
/// Line endings are preserved as they were (`\n`, `\r\n`), the final line keeps
/// its absence of a terminator if it had none, and a blank line stays blank
/// rather than collecting whitespace.
[[nodiscard]] FormatOutcome formatSource(const QString& source, Language language,
                                         int indent_width = 4);

/// Whether this build can re-indent `language` at all.
///
/// Exposed so the action can be enabled, disabled or hidden rather than offering
/// a button that only ever apologises.
[[nodiscard]] bool canFormat(Language language);

}  // namespace timewarp::ui
