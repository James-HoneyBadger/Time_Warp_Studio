// SPDX-License-Identifier: MIT
//
// The code rules: blank lines around a fence, a language on every fence, one fence
// character and one code style per document.

#include "md_internal.hpp"
#include "md_rules.hpp"

#include <algorithm>
#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

namespace docs::md {

using namespace detail;

/// MD031: a fence needs a blank line before and after, or the paragraph beside it
/// is read as part of the code.
void md031(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind != Kind::FenceOpen && line.kind != Kind::FenceClose) {
            continue;
        }
        const char* side = line.kind == Kind::FenceOpen ? "Above" : "Below";
        const std::size_t neighbour =
            line.kind == Kind::FenceOpen ? (index == 0 ? index : index - 1) : index + 1;
        const bool at_edge = line.kind == Kind::FenceOpen
                                 ? index == 0
                                 : index + 1 >= document.lines.size();
        if (at_edge) {
            continue;
        }
        if (!document.lines[neighbour].is_blank()) {
            issues->push_back(Issue{static_cast<int>(index) + 1,
                                    "MD031/blanks-around-fences",
                                    std::string{"Fenced code blocks should be surrounded by "
                                                "blank lines [Expected: 1; Actual: 0; "} +
                                        side + "]"});
        }
    }
}

/// MD040: a fence with no language. `language_only` also refuses an info string
/// with anything after the language, which is how a fence ends up looking like a
/// command line.
void md040(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind != Kind::FenceOpen) {
            continue;
        }
        if (line.fence_info.empty()) {
            issues->push_back(Issue{static_cast<int>(index) + 1,
                                    "MD040/fenced-code-language",
                                    "Fenced code blocks should have a language specified"});
            continue;
        }
        if (line.fence_info.find(' ') != std::string_view::npos) {
            issues->push_back(Issue{static_cast<int>(index) + 1,
                                    "MD040/fenced-code-language",
                                    "Fenced code blocks should have a language specified "
                                    "[Expected: only the language; Actual: '" +
                                        std::string{line.fence_info} + "']"});
        }
    }
}

/// MD046: one code style per document. Indented blocks are invisible in a diff
/// and there are none here, so fenced is the style and an indented block is a
/// paragraph that lost its fence.
void md046(const Document& document, std::vector<Issue>* issues) {
    // Which lines belong to a list, whose continuation lines are indented four
    // spaces and are not code blocks.
    std::vector<bool> in_list(document.lines.size(), false);
    for (const ListBlock& block : list_blocks(document)) {
        for (std::size_t index = block.first; index <= block.last; ++index) {
            in_list[index] = true;
        }
    }
    bool saw_fence = false;
    for (const Line& line : document.lines) {
        if (line.kind == Kind::FenceOpen) {
            saw_fence = true;
            break;
        }
    }
    if (!saw_fence) {
        return;
    }
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind != Kind::Text || in_list[index]) {
            continue;
        }
        const std::size_t indent = leading_spaces(line.raw);
        if (indent < 4 || indent >= line.raw.size()) {
            continue;
        }
        // An indented line directly after a list item is that item's continuation,
        // which is not a code block.
        const bool after_list =
            index > 0 && document.lines[index - 1].is_list() &&
            document.lines[index - 1].indent + 2 <= indent;
        const bool after_blank_list =
            index > 1 && document.lines[index - 1].is_blank() &&
            document.lines[index - 2].is_list() &&
            document.lines[index - 2].indent + 2 <= indent;
        if (after_list || after_blank_list) {
            continue;
        }
        const bool previous_is_text = index > 0 && document.lines[index - 1].kind == Kind::Text &&
                                      leading_spaces(document.lines[index - 1].raw) >= 4;
        if (previous_is_text || (index > 0 && document.lines[index - 1].is_blank())) {
            issues->push_back(Issue{static_cast<int>(index) + 1, "MD046/code-block-style",
                                    "Code block style should be consistent [Expected: fenced]"});
        }
    }
}

/// MD048: one fence character per document. `~~~` renders identically to ``` and
/// a document that uses both was written by two people.
void md048(const Document& document, std::vector<Issue>* issues) {
    std::optional<char> style;
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind != Kind::FenceOpen) {
            continue;
        }
        if (!style.has_value()) {
            style = line.fence_char;
            continue;
        }
        if (*style != line.fence_char) {
            issues->push_back(Issue{static_cast<int>(index) + 1, "MD048/code-fence-style",
                                    std::string{"Code fence style should be consistent "
                                                "[Expected: "} +
                                        (*style == '`' ? "backtick" : "tilde") +
                                        "; Actual: " +
                                        (line.fence_char == '`' ? "backtick" : "tilde") + "]"});
        }
    }
}

}  // namespace docs::md
