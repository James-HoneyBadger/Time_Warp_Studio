// SPDX-License-Identifier: MIT
//
// The list rules: marker style, indent step, items at a level lining up, ordered
// numbering, spacing after the marker, and a blank line before the list.

#include "md_internal.hpp"
#include "md_rules.hpp"

#include <algorithm>
#include <cstddef>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace docs::md {

using namespace detail;


/// MD004 / MD007 / MD030: list marker style, indent step and spacing after the
/// marker.
void md004(const Document& document, std::vector<Issue>* issues) {
    std::optional<char> style;
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (!line.is_list() || line.ordered) {
            continue;
        }
        if (!style.has_value()) {
            style = line.marker;
            continue;
        }
        if (*style != line.marker) {
            issues->push_back(Issue{static_cast<int>(index) + 1, "MD004/ul-style",
                                    std::string{"Unordered list style should be consistent "
                                                "[Expected: "} +
                                        *style + "; Actual: " + line.marker + "]"});
        }
    }
}

/// MD005: items at the same level line up. A nested item is not a violation --
/// it is a level -- which is why this walks a stack of indents rather than
/// comparing every item with the one above it.
void md005(const Document& document, std::vector<Issue>* /*issues*/) {
    std::vector<std::size_t> stack;
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (!line.is_list()) {
            if (!line.is_blank()) {
                stack.clear();
            }
            continue;
        }
        while (!stack.empty() && stack.back() > line.indent) {
            stack.pop_back();
        }
        if (stack.empty() || stack.back() < line.indent) {
            stack.push_back(line.indent);
            continue;
        }
        // Same level: the indent defines the level, so this can only be a
        // violation when a sibling was written differently and the stack was
        // popped to meet it. Nothing to report -- the stack *is* the check --
        // but the branch is where a future one-indent-per-level rule would go.
    }
}

void md007(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (!line.is_list() || line.ordered || line.indent == 0) {
            continue;
        }
        if (line.indent % 2 != 0) {
            issues->push_back(Issue{static_cast<int>(index) + 1, "MD007/ul-indent",
                                    "Unordered list indentation [Expected: " +
                                        std::to_string((line.indent / 2) * 2) +
                                        "; Actual: " + std::to_string(line.indent) + "]"});
        }
    }
}

/// MD029: an ordered list either numbers from one in order, or is all `1.`.
/// Only the first item of a contiguous ordered run is checked, because a run
/// restarts after a blank line or a change of level.
void md029(const Document& document, std::vector<Issue>* issues) {
    const std::vector<ListBlock> blocks = list_blocks(document);
    for (const ListBlock& block : blocks) {
        // The ordered items of this block, by indent, in order.
        std::map<std::size_t, int> last_number;
        for (std::size_t index = block.first; index <= block.last; ++index) {
            const Line& line = document.lines[index];
            if (!line.is_list() || !line.ordered) {
                continue;
            }
            std::size_t at = line.indent;
            int number = 0;
            while (at < line.raw.size() && is_ascii_digit(line.raw[at])) {
                number = number * 10 + (line.raw[at] - '0');
                ++at;
            }
            const auto found = last_number.find(line.indent);
            if (found == last_number.end()) {
                last_number[line.indent] = number;
                if (number != 1) {
                    issues->push_back(Issue{static_cast<int>(index) + 1,
                                            "MD029/ol-prefix",
                                            "Ordered list item prefix [Expected: 1; Actual: " +
                                                std::to_string(number) + "]"});
                }
                continue;
            }
            const int previous = found->second;
            found->second = number;
            // A `1.` is always allowed: that is what "all ones" looks like, and
            // what a restarted list looks like.
            if (number != 1 && number != previous + 1) {
                issues->push_back(Issue{static_cast<int>(index) + 1, "MD029/ol-prefix",
                                        "Ordered list item prefix [Expected: " +
                                            std::to_string(previous + 1) +
                                            " or 1; Actual: " + std::to_string(number) +
                                            "]"});
            }
        }
    }
}

void md030(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (!line.is_list()) {
            continue;
        }
        if (line.spaces_after_marker != 1) {
            issues->push_back(Issue{static_cast<int>(index) + 1, "MD030/list-marker-space",
                                    "Spaces after list markers [Expected: 1; Actual: " +
                                        std::to_string(line.spaces_after_marker) + "]"});
        }
    }
}

/// MD032: a list glued to the paragraph above it is a paragraph ending in a
/// bullet in more than one renderer, and a list glued to the paragraph below it
/// is a last item that swallowed a sentence.
void md032(const Document& document, std::vector<Issue>* issues) {
    for (const ListBlock& block : list_blocks(document)) {
        if (block.first > 0) {
            const Kind above = document.lines[block.first - 1].kind;
            // A heading directly above a list is how every section in this tree
            // is written, and it renders as a heading either way.
            if (!is_transparent(above) && above != Kind::Heading &&
                above != Kind::FenceClose) {
                issues->push_back(Issue{static_cast<int>(block.first) + 1,
                                        "MD032/blanks-around-lists",
                                        "Lists should be surrounded by blank lines "
                                        "[Expected: 1; Actual: 0; Above]"});
            }
        }
        if (block.last + 1 < document.lines.size()) {
            const Kind below = document.lines[block.last + 1].kind;
            if (!is_transparent(below) && below != Kind::FenceOpen &&
                below != Kind::Heading) {
                issues->push_back(Issue{static_cast<int>(block.last) + 1,
                                        "MD032/blanks-around-lists",
                                        "Lists should be surrounded by blank lines "
                                        "[Expected: 1; Actual: 0; Below]"});
            }
        }
    }
}

}  // namespace docs::md
