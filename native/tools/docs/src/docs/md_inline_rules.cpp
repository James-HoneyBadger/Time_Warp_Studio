// SPDX-License-Identifier: MIT
//
// Inline content: raw HTML, a bare URL, one emphasis delimiter, and one table pipe
// style with rows as wide as the header.

#include "md_internal.hpp"
#include "md_rules.hpp"

#include "docs/text.hpp"

#include <algorithm>
#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

namespace docs::md {

using namespace detail;

/// MD033: HTML in Markdown. The renderer that matters -- GitHub, and the help
/// browser -- both pass it through, which is why it is tempting and why a
/// document that has it looks different in the two places it is read.
void md033(const Document& document, std::vector<Issue>* issues) {
    // The element names worth recognising. A `<` followed by a word that is not
    // one of these is a placeholder -- `<file>`, `<target>` -- which the prose
    // here uses deliberately and which no renderer treats as markup.
    static const char* const kElements[] = {
        "a",      "abbr",  "b",       "blockquote", "br",     "code",   "dd",     "del",
        "details", "div",  "dl",      "dt",         "em",     "h1",     "h2",     "h3",
        "h4",     "h5",    "h6",      "hr",         "i",      "img",    "input",  "kbd",
        "li",     "ol",    "p",       "pre",        "s",      "samp",   "span",   "strong",
        "sub",    "summary", "sup",   "table",      "tbody",  "td",     "th",     "thead",
        "tr",     "u",     "ul",      "var",        "video",  "source", "picture", "figure",
    };
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind == Kind::Code || line.kind == Kind::HtmlComment ||
            line.kind == Kind::FrontMatter) {
            continue;
        }
        const std::string stripped = without_inline_code(line.raw);
        for (std::size_t at = 0; at < stripped.size(); ++at) {
            if (stripped[at] != '<') {
                continue;
            }
            std::size_t cursor = at + 1;
            bool closing = false;
            if (cursor < stripped.size() && stripped[cursor] == '/') {
                closing = true;
                ++cursor;
            }
            std::string name;
            while (cursor < stripped.size() &&
                   ((stripped[cursor] >= 'a' && stripped[cursor] <= 'z') ||
                    (stripped[cursor] >= 'A' && stripped[cursor] <= 'Z') ||
                    is_ascii_digit(stripped[cursor]))) {
                name.push_back(static_cast<char>(std::tolower(stripped[cursor])));
                ++cursor;
            }
            if (name.empty() || cursor >= stripped.size() || stripped[cursor] != '>') {
                continue;
            }
            const bool known =
                std::any_of(std::begin(kElements), std::end(kElements),
                            [&name](const char* element) { return name == element; });
            if (!known) {
                continue;
            }
            issues->push_back(Issue{static_cast<int>(index) + 1, "MD033/no-inline-html",
                                    std::string{"Inline HTML [Element: "} +
                                        (closing ? "/" : "") + name + "]"});
            break;
        }
    }
}

/// MD034: a URL that is not a link is a URL a reader has to copy. `<...>` is an
/// autolink and `](...)` is a link target; anything else is bare.
void md034(const Document& document, std::vector<Issue>* issues) {
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind == Kind::Code || line.kind == Kind::FrontMatter ||
            line.kind == Kind::HtmlComment) {
            continue;
        }
        const std::string stripped = without_inline_code(line.raw);
        for (std::size_t at = 0; at + 7 < stripped.size(); ++at) {
            const bool scheme = stripped.compare(at, 7, "http://") == 0 ||
                                stripped.compare(at, 8, "https://") == 0;
            if (!scheme) {
                continue;
            }
            // The character before decides: `(`, `<`, `"` open a target or an
            // autolink; anything else means the URL stands alone.
            const char before = at == 0 ? ' ' : stripped[at - 1];
            if (before == '(' || before == '<' || before == '"' || before == '\'') {
                continue;
            }
            issues->push_back(Issue{static_cast<int>(index) + 1, "MD034/no-bare-urls",
                                    "Bare URL used"});
            break;
        }
    }
}

/// MD049 / MD050: one emphasis and one strong delimiter per document.
void md049(const Document& document, std::vector<Issue>* issues) {
    std::optional<char> emphasis_style;
    std::optional<char> strong_style;
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind == Kind::Code || line.kind == Kind::FrontMatter ||
            line.kind == Kind::HtmlComment) {
            continue;
        }
        const std::string stripped = without_inline_code(line.raw);
        // A delimiter is a run of `*` or `_` with text after it on the same line.
        // `*.bas` and `2 * 3` are not emphasis: the run must be followed by a
        // non-space and preceded by a space or the line start.
        for (std::size_t at = 0; at < stripped.size(); ++at) {
            const char character = stripped[at];
            if (character != '*' && character != '_') {
                continue;
            }
            std::size_t run = 0;
            while (at + run < stripped.size() && stripped[at + run] == character) {
                ++run;
            }
            if (at + run >= stripped.size() || stripped[at + run] == ' ') {
                at += run - 1;
                continue;
            }
            if (at > 0 && stripped[at - 1] != ' ' && stripped[at - 1] != '(' &&
                stripped[at - 1] != '"' && stripped[at - 1] != '\'') {
                at += run - 1;
                continue;
            }
            // Find the closing run of the same length.
            std::size_t close = at + run;
            bool found = false;
            while (close < stripped.size()) {
                if (stripped[close] == character) {
                    std::size_t closing_run = 0;
                    while (close + closing_run < stripped.size() &&
                           stripped[close + closing_run] == character) {
                        ++closing_run;
                    }
                    if (closing_run == run && close > at + run &&
                        stripped[close - 1] != ' ') {
                        found = true;
                        break;
                    }
                    close += closing_run;
                    continue;
                }
                ++close;
            }
            if (!found) {
                at += run - 1;
                continue;
            }
            const bool is_strong = run == 2;
            if (run != 1 && !is_strong) {
                at += run - 1;
                continue;
            }
            std::optional<char>& style = is_strong ? strong_style : emphasis_style;
            if (!style.has_value()) {
                style = character;
            } else if (*style != character) {
                issues->push_back(
                    Issue{static_cast<int>(index) + 1,
                          is_strong ? "MD050/strong-style" : "MD049/emphasis-style",
                          std::string{(is_strong ? "Strong" : "Emphasis")} +
                              " style should be consistent [Expected: " + *style +
                              "; Actual: " + character + "]"});
            }
            at = close + run - 1;
        }
    }
}

/// MD055 / MD056: one table pipe style, and a row that has as many cells as the
/// row above it -- a missing pipe silently drops a column.
void md055(const Document& document, std::vector<Issue>* issues) {
    enum class Style { Unknown, Both, Leading, Trailing, None };
    Style style = Style::Unknown;
    std::size_t expected_cells = 0;
    bool in_table = false;

    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        if (line.kind != Kind::TableRow && line.kind != Kind::TableDelimiter) {
            in_table = false;
            expected_cells = 0;
            continue;
        }
        const std::string_view body = trim(line.raw);
        const bool leading = !body.empty() && body.front() == '|';
        const bool trailing = !body.empty() && body.back() == '|';
        const Style current = leading && trailing ? Style::Both
                              : leading           ? Style::Leading
                              : trailing          ? Style::Trailing
                                                  : Style::None;
        if (style == Style::Unknown) {
            style = current;
        } else if (style != current) {
            issues->push_back(Issue{static_cast<int>(index) + 1, "MD055/table-pipe-style",
                                    "Table pipe style should be consistent"});
        }
        if (line.kind == Kind::TableDelimiter) {
            in_table = true;
            continue;
        }
        if (line.kind == Kind::TableRow) {
            const std::size_t cells = table_cells(line.raw).size();
            if (!in_table || expected_cells == 0) {
                expected_cells = cells;
                in_table = true;
            } else if (cells != expected_cells) {
                issues->push_back(Issue{static_cast<int>(index) + 1,
                                        "MD056/table-column-count",
                                        "Table column count [Expected: " +
                                            std::to_string(expected_cells) +
                                            "; Actual: " + std::to_string(cells) + "]"});
            }
        }
    }
}

}  // namespace docs::md
