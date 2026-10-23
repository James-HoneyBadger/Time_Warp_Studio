// SPDX-License-Identifier: MIT
//
// The rules, one declaration each, so `lint` can call them and a reader can see the
// whole set in one place. The numbers are the house rules' own names -- MD018 is
// "no space after the hashes" -- and they are kept because a document that breaks one
// is fixed by reading that rule.
//
// Internal to the `md` command: nothing outside `src/docs/md_*.cpp` includes it.

#pragma once

#include "md_internal.hpp"

namespace docs::md {

// `Document` and `Issue` live in two other headers: the model in `md_internal.hpp`
// and the report in `md.hpp`.
using detail::Document;

/// MD001: `##` under a `#` is fine; `####` under a `#` skips two levels and
/// breaks any outline a reader builds from the headings.
void md001(const Document& document, std::vector<Issue>* issues);
/// MD003: one heading style per document. A document that closes some headings
/// with `##` and not others is a document written by two people.
void md003(const Document& document, std::vector<Issue>* issues);
/// MD004 / MD007 / MD030: list marker style, indent step and spacing after the
/// marker.
void md004(const Document& document, std::vector<Issue>* issues);
/// MD005: items at the same level line up. A nested item is not a violation --
/// it is a level -- which is why this walks a stack of indents rather than
/// comparing every item with the one above it.
void md005(const Document& document, std::vector<Issue>* /*issues*/);
void md007(const Document& document, std::vector<Issue>* issues);
/// MD009: trailing spaces. Exactly two is a hard line break and is how the
/// `**Project:** ...` header in the agent instructions gets its lines to end
/// together, so that many is allowed and anything else is not.
void md009(const Document& document, std::vector<Issue>* issues);
/// MD010: a hard tab in prose. Inside a fence it is content.
void md010(const Document& document, std::vector<Issue>* issues);
/// MD012: more than one blank line in a row.
void md012(const Document& document, std::vector<Issue>* issues);
// ---------------------------------------------------------------------------
// The rules
// ---------------------------------------------------------------------------

/// MD018: `#Heading` is not a heading at all, which is why it is worth a rule:
/// the document says "heading" and the renderer shows a word with a hash.
void md018(const Document& document, std::vector<Issue>* issues);
/// MD019: `##  Heading` with more than one space still renders, but the extra
/// spaces are invisible in the source and nothing else in the tree has them.
void md019(const Document& document, std::vector<Issue>* issues);
/// MD022: a heading with no blank line above or below is a heading glued to its
/// neighbours, and a table or a list directly under one renders differently in
/// some viewers.
void md022(const Document& document, std::vector<Issue>* issues);
/// MD023: an indented heading is code in some renderers and a heading in none
/// this project targets.
void md023(const Document& document, std::vector<Issue>* issues);
/// MD025: one top-level heading. Two `#`s is two documents stacked.
void md025(const Document& document, std::vector<Issue>* issues);
/// MD026: a heading is not a sentence, and the full stop is the tell.
void md026(const Document& document, std::vector<Issue>* issues);
/// MD028: a blank line between two block quotes splits one quote into two, which
/// is a paragraph break in the quote or a mistake -- never what was meant.
void md028(const Document& document, std::vector<Issue>* issues);
/// MD029: an ordered list either numbers from one in order, or is all `1.`.
/// Only the first item of a contiguous ordered run is checked, because a run
/// restarts after a blank line or a change of level.
void md029(const Document& document, std::vector<Issue>* issues);
void md030(const Document& document, std::vector<Issue>* issues);
/// MD031: a fence needs a blank line before and after, or the paragraph beside it
/// is read as part of the code.
void md031(const Document& document, std::vector<Issue>* issues);
/// MD032: a list glued to the paragraph above it is a paragraph ending in a
/// bullet in more than one renderer, and a list glued to the paragraph below it
/// is a last item that swallowed a sentence.
void md032(const Document& document, std::vector<Issue>* issues);
/// MD033: HTML in Markdown. The renderer that matters -- GitHub, and the help
/// browser -- both pass it through, which is why it is tempting and why a
/// document that has it looks different in the two places it is read.
void md033(const Document& document, std::vector<Issue>* issues);
/// MD034: a URL that is not a link is a URL a reader has to copy. `<...>` is an
/// autolink and `](...)` is a link target; anything else is bare.
void md034(const Document& document, std::vector<Issue>* issues);
/// MD040: a fence with no language. `language_only` also refuses an info string
/// with anything after the language, which is how a fence ends up looking like a
/// command line.
void md040(const Document& document, std::vector<Issue>* issues);
/// MD041: the first line is the document's title. A leading HTML comment is
/// allowed because a template's instructions belong above the title they
/// describe, and so is front matter, which is not content at all.
void md041(const Document& document, std::vector<Issue>* issues);
/// MD046: one code style per document. Indented blocks are invisible in a diff
/// and there are none here, so fenced is the style and an indented block is a
/// paragraph that lost its fence.
void md046(const Document& document, std::vector<Issue>* issues);
/// MD047: the file ends with exactly one newline, which is what every tool that
/// reads a patch expects.
void md047(std::string_view text, const Document& document, std::vector<Issue>* issues);
/// MD048: one fence character per document. `~~~` renders identically to ``` and
/// a document that uses both was written by two people.
void md048(const Document& document, std::vector<Issue>* issues);
/// MD049 / MD050: one emphasis and one strong delimiter per document.
void md049(const Document& document, std::vector<Issue>* issues);
/// MD055 / MD056: one table pipe style, and a row that has as many cells as the
/// row above it -- a missing pipe silently drops a column.
void md055(const Document& document, std::vector<Issue>* issues);

}  // namespace docs::md
