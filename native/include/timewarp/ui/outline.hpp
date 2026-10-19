// SPDX-License-Identifier: MIT
//
// The document outline: the named places in a program, in source order.
//
// A model, not a widget. The dock is a view over this (see `OutlinePanel`), and
// the parsing is a pure function of (language, text) for the same reason the
// linter is: the interesting cases -- a Logo procedure, a BASIC label, a REXX
// label at column zero -- are assertions about strings, not about a window.
//
// #### What counts as a name
//
// Only things a reader would call a place: procedures, functions, subroutines,
// labels, and Scheme's `define`s. Not every numbered BASIC line: a 400-line
// program has 400 of those, and an outline that lists all of them is the file
// with extra steps.

#pragma once

#include <QList>
#include <QString>

#include "timewarp/ui/language.hpp"

namespace timewarp::ui {

/// What kind of named place an entry is, so the panel can say so.
enum class OutlineKind { Procedure, Function, Subroutine, Label, FunctionDefinition };

/// One line a reader can jump to.
struct OutlineEntry {
    /// The name as written, without the keyword that introduced it.
    QString name;
    /// 1-based, so it can go straight to `CodeEditor::goToLine`.
    int line = 0;
    OutlineKind kind = OutlineKind::Label;
};

/// A short lowercase word for the kind, for the panel's second column.
[[nodiscard]] QString outlineKindName(OutlineKind kind);

/// Every entry in `source`, in source order.
///
/// `language` decides which rules apply; a language with no outline rules
/// (FOCAL, CHIP-8, APL beyond `∇`) returns an empty list rather than a guess,
/// which the panel shows as an empty state.
[[nodiscard]] QList<OutlineEntry> outlineFor(Language language, const QString& source);

}  // namespace timewarp::ui
