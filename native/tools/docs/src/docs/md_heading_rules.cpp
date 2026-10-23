// SPDX-License-Identifier: MIT
//
// The heading rules: one top-level heading, no skipped levels, one style, space after
// the hashes, blank lines around it, and a title on the first line.

#include "md_internal.hpp"
#include "md_rules.hpp"

#include <algorithm>
#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

namespace docs::md {

using namespace detail;

/// MD001: `##` under a `#` is fine; `####` under a `#` skips two levels and
/// breaks any outline a reader builds from the headings.
void md001(const Document& document, std::vector<Issue>* issues) {
    int previous = 0;
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind != Kind::Heading) {
            continue;
        }
        if (previous != 0 && line.level > previous + 1) {
            issues->push_back(Issue{static_cast<int>(index) + 1,
                                    "MD001/heading-increment",
                                    "Heading levels should only increment by one level "
                                    "at a time [Expected: h" +
                                        std::to_string(previous + 1) + "; Actual: h" +
                                        std::to_string(line.level) + "]"});
        }
        previous = line.level;
    }
}

/// MD003: one heading style per document. A document that closes some headings
/// with `##` and not others is a document written by two people.
void md003(const Document& document, std::vector<Issue>* issues) {
    std::optional<bool> closed_style;
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind != Kind::Heading) {
            continue;
        }
        if (!closed_style.has_value()) {
            closed_style = line.closed;
            continue;
        }
        if (*closed_style != line.closed) {
            issues->push_back(Issue{static_cast<int>(index) + 1, "MD003/heading-style",
                                    "Heading style should be consistent "
                                    "[Expected: " +
                                        std::string{*closed_style ? "atx_closed" : "atx"} +
                                        "; Actual: " +
                                        std::string{line.closed ? "atx_closed" : "atx"} + "]"});
        }
    }
}

// ---------------------------------------------------------------------------
// The rules
// ---------------------------------------------------------------------------

/// MD018: `#Heading` is not a heading at all, which is why it is worth a rule:
/// the document says "heading" and the renderer shows a word with a hash.
void md018(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        // A heading with no space is not a heading, so the parser calls it text --
        // which is exactly the line this rule is about. Only code is skipped: a
        // `#` in a listing is the language's, not the document's.
        if (line.kind == Kind::Code || line.kind == Kind::Blank ||
            line.kind == Kind::FrontMatter) {
            continue;
        }
        const std::size_t indent = leading_spaces(line.raw);
        if (indent > 3 || indent >= line.raw.size() || line.raw[indent] != '#') {
            continue;
        }
        std::size_t hashes = 0;
        while (indent + hashes < line.raw.size() && line.raw[indent + hashes] == '#') {
            ++hashes;
        }
        if (hashes > 6) {
            continue;
        }
        // Only a hash run that is followed by text, not by a closing fence of
        // hashes and not by a space.
        if (indent + hashes < line.raw.size() && line.raw[indent + hashes] != ' ') {
            issues->push_back(Issue{static_cast<int>(index) + 1,
                                    "MD018/no-missing-space-atx",
                                    "No space after hash on atx style heading"});
        }
    }
}

/// MD019: `##  Heading` with more than one space still renders, but the extra
/// spaces are invisible in the source and nothing else in the tree has them.
void md019(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind != Kind::Heading) {
            continue;
        }
        std::size_t hashes = line.indent;
        while (hashes < line.raw.size() && line.raw[hashes] == '#') {
            ++hashes;
        }
        std::size_t spaces = 0;
        while (hashes + spaces < line.raw.size() && line.raw[hashes + spaces] == ' ') {
            ++spaces;
        }
        if (spaces > 1) {
            issues->push_back(Issue{static_cast<int>(index) + 1,
                                    "MD019/no-multiple-space-atx",
                                    "Multiple spaces after hash on atx style heading"});
        }
    }
}

/// MD022: a heading with no blank line above or below is a heading glued to its
/// neighbours, and a table or a list directly under one renders differently in
/// some viewers.
void md022(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind != Kind::Heading) {
            continue;
        }
        const bool first = index == document.content_start;
        if (!first && !document.lines[index - 1].is_blank() &&
            document.lines[index - 1].kind != Kind::FrontMatter) {
            issues->push_back(Issue{static_cast<int>(index) + 1,
                                    "MD022/blanks-around-headings",
                                    "Headings should be surrounded by blank lines "
                                    "[Expected: 1; Actual: 0; Above]"});
        }
        if (index + 1 < document.lines.size() && !document.lines[index + 1].is_blank()) {
            issues->push_back(Issue{static_cast<int>(index) + 1,
                                    "MD022/blanks-around-headings",
                                    "Headings should be surrounded by blank lines "
                                    "[Expected: 1; Actual: 0; Below]"});
        }
    }
}

/// MD023: an indented heading is code in some renderers and a heading in none
/// this project targets.
void md023(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind == Kind::Heading && line.indent > 0) {
            issues->push_back(Issue{static_cast<int>(index) + 1,
                                    "MD023/heading-start-left",
                                    "Headings must start at the beginning of the line"});
        }
    }
}

/// MD025: one top-level heading. Two `#`s is two documents stacked.
void md025(const Document& document, std::vector<Issue>* issues) {
    bool seen = false;
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind != Kind::Heading || line.level != 1) {
            continue;
        }
        if (seen) {
            issues->push_back(Issue{static_cast<int>(index) + 1, "MD025/single-title",
                                    "Multiple top-level headings in the same document"});
        }
        seen = true;
    }
}

/// MD026: a heading is not a sentence, and the full stop is the tell.
void md026(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind != Kind::Heading || line.text.empty()) {
            continue;
        }
        const char last = line.text.back();
        // markdownlint's default set, minus the ideographic ones: a `?` is a
        // question, not a sentence that lost its place, and the FAQ headings are
        // questions.
        if (last == '.' || last == ',' || last == ';' || last == ':' || last == '!') {
            issues->push_back(Issue{static_cast<int>(index) + 1,
                                    "MD026/no-trailing-punctuation",
                                    std::string{"Trailing punctuation in heading [Punctuation: '"} +
                                        last + "']"});
        }
    }
}

/// MD041: the first line is the document's title. A leading HTML comment is
/// allowed because a template's instructions belong above the title they
/// describe, and so is front matter, which is not content at all.
void md041(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = document.content_start; index < document.lines.size();
         ++index) {
        const Line& line = document.lines[index];
        if (line.is_blank()) {
            continue;
        }
        if (line.kind == Kind::HtmlComment) {
            // Skip the comment block, then look at what follows.
            while (index < document.lines.size() &&
                   document.lines[index].kind == Kind::HtmlComment) {
                ++index;
            }
            continue;
        }
        if (line.kind != Kind::Heading || line.level != 1) {
            issues->push_back(Issue{static_cast<int>(index) + 1,
                                    "MD041/first-line-heading",
                                    "First line in a file should be a top-level heading"});
        }
        return;
    }
}

}  // namespace docs::md
