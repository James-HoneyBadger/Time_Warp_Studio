// SPDX-License-Identifier: MIT
//
// Whitespace and document shape: trailing spaces, tabs in prose, doubled blank lines,
// a split block quote, and the one newline a file ends with.

#include "md_internal.hpp"
#include "md_rules.hpp"

#include <algorithm>
#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

namespace docs::md {

using namespace detail;

/// MD009: trailing spaces. Exactly two is a hard line break and is how the
/// `**Project:** ...` header in the agent instructions gets its lines to end
/// together, so that many is allowed and anything else is not.
void md009(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind == Kind::Code || line.kind == Kind::FrontMatter) {
            continue;
        }
        std::size_t trailing = 0;
        while (trailing < line.raw.size() &&
               (line.raw[line.raw.size() - 1 - trailing] == ' ' ||
                line.raw[line.raw.size() - 1 - trailing] == '\t')) {
            ++trailing;
        }
        if (trailing == 0 || trailing == 2) {
            continue;
        }
        issues->push_back(Issue{static_cast<int>(index) + 1, "MD009/no-trailing-spaces",
                                trailing == 1 ? "Trailing space"
                                              : "Trailing spaces [" +
                                                    std::to_string(trailing) + "]"});
    }
}

/// MD010: a hard tab in prose. Inside a fence it is content.
void md010(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind == Kind::Code || line.kind == Kind::FrontMatter) {
            continue;
        }
        if (line.raw.find('\t') != std::string_view::npos) {
            issues->push_back(Issue{static_cast<int>(index) + 1, "MD010/no-hard-tabs",
                                    "Hard tab"});
        }
    }
}

/// MD012: more than one blank line in a row.
void md012(const Document& document, std::vector<Issue>* issues) {
    int blanks = 0;
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        if (!document.lines[index].is_blank()) {
            blanks = 0;
            continue;
        }
        ++blanks;
        if (blanks > 1) {
            issues->push_back(Issue{static_cast<int>(index) + 1,
                                    "MD012/no-multiple-blanks",
                                    "Multiple consecutive blank lines"});
        }
    }
}

/// MD028: a blank line between two block quotes splits one quote into two, which
/// is a paragraph break in the quote or a mistake -- never what was meant.
void md028(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = 1; index + 1 < document.lines.size(); ++index) {
        if (!document.lines[index].is_blank()) {
            continue;
        }
        if (document.lines[index - 1].kind == Kind::BlockQuote &&
            document.lines[index + 1].kind == Kind::BlockQuote) {
            issues->push_back(Issue{static_cast<int>(index) + 1,
                                    "MD028/no-blanks-blockquote",
                                    "Blank line inside blockquote"});
        }
    }
}

/// MD047: the file ends with exactly one newline, which is what every tool that
/// reads a patch expects.
void md047(std::string_view text, const Document& document, std::vector<Issue>* issues) {
    if (text.empty()) {
        return;
    }
    if (text.back() != '\n') {
        issues->push_back(Issue{static_cast<int>(document.lines.size()),
                                "MD047/single-trailing-newline",
                                "Files should end with a single newline character"});
        return;
    }
    if (text.size() >= 2 && text[text.size() - 2] == '\n') {
        issues->push_back(Issue{static_cast<int>(document.lines.size()),
                                "MD047/single-trailing-newline",
                                "Files should end with a single newline character"});
    }
}

}  // namespace docs::md
