// SPDX-License-Identifier: MIT
//
// The snippet library: a catalogue of small, runnable programs to insert at the
// cursor.
//
// Ported from `ui/snippets.py` (641 lines) and `features/snippets.json`, with the
// catalogue itself rewritten: the old file held BASIC, Logo, Pascal, Prolog,
// Forth and C, and three of those languages no longer exist here. This one has
// the ten, and every construct in it appears in a program under `Examples/` or in
// `docs/LANGUAGE_GUIDE.md` -- a snippet that teaches syntax the engine rejects is
// worse than an empty library.
//
// The data is `native/share/snippets.json`, installed next to the application, so
// a package and a build tree read the same file. `snippetRoot()` is the resolver,
// shaped like `exampleRoot()`; `TIME_WARP_SNIPPETS` overrides it, which is what
// the tests use.

#pragma once

#include <QList>
#include <QString>

#include "timewarp/ui/language.hpp"

namespace timewarp::ui {

/// One entry of the catalogue.
struct Snippet {
    QString name;
    /// One line, shown in the list and the preview.
    QString description;
    Language language = Language::Unknown;
    /// The text to insert. `$CURSOR$` marks where the caret goes; `$SELECTION$`
    /// is replaced by the selection, if there is one.
    QString body;
    /// For the tests and for a stored selection: `"basic/hello-world"`.
    [[nodiscard]] QString id() const;
};

/// Where the catalogue lives: `TIME_WARP_SNIPPETS` when it names a file, else
/// `share/time-warp-studio/snippets.json` beside the binary, else the checkout's
/// `native/share/snippets.json`. Empty when none of those exist.
[[nodiscard]] QString snippetRoot();

/// Loads the catalogue from `snippetRoot()`.
///
/// Entries whose language this build does not know, or whose body is empty, are
/// dropped; `error` says why when nothing could be read at all. A catalogue that
/// half-parses is not reported as a failure, because the entries that did parse
/// are usable and a learner does not need to know a file has a typo in one
/// record -- the test asserts the shipped file has none.
[[nodiscard]] QList<Snippet> loadSnippets(QString* error = nullptr);

/// The result of inserting one: the text to put in, and where the caret belongs
/// counted from the start of `text`.
struct SnippetInsertion {
    QString text;
    int caret = 0;
};

/// Substitutes `$SELECTION$` and `$CURSOR$` in `body`.
///
/// A placeholder this build does not know is left exactly as written -- silently
/// dropping text someone typed into the catalogue is the one thing a substitution
/// must not do.
[[nodiscard]] SnippetInsertion resolveSnippet(const QString& body, const QString& selection);

}  // namespace timewarp::ui
