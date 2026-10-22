// SPDX-License-Identifier: MIT
//
// See `md.hpp`. The line model first, then one function per rule.

#include "docs/md.hpp"

#include "docs/pipes.hpp"
#include "docs/text.hpp"

#include <algorithm>
#include <cstdio>
#include <fstream>
#include <map>
#include <optional>

namespace docs::md {

namespace {

// ---------------------------------------------------------------------------
// The line model
// ---------------------------------------------------------------------------

enum class Kind {
    Blank,
    FrontMatter,
    FenceOpen,
    FenceClose,
    Code,
    Heading,
    ThematicBreak,
    BlockQuote,
    ListItem,
    TableRow,
    TableDelimiter,
    HtmlComment,
    Text,
};

struct Line {
    std::string_view raw;
    Kind kind = Kind::Text;

    // Heading.
    int level = 0;
    std::size_t indent = 0;
    bool closed = false;
    std::string_view text;  // the heading's text, without the hashes

    // Fence.
    char fence_char = '\0';
    std::size_t fence_length = 0;
    std::string_view fence_info;

    // List item.
    bool ordered = false;
    char marker = '\0';
    std::size_t spaces_after_marker = 0;

    [[nodiscard]] bool is_blank() const { return kind == Kind::Blank; }
    [[nodiscard]] bool is_list() const { return kind == Kind::ListItem; }
    [[nodiscard]] bool is_fence_marker() const {
        return kind == Kind::FenceOpen || kind == Kind::FenceClose;
    }
};

[[nodiscard]] std::string_view rtrim(std::string_view text) {
    std::size_t end = text.size();
    while (end > 0 && (text[end - 1] == ' ' || text[end - 1] == '\t')) {
        --end;
    }
    return text.substr(0, end);
}

[[nodiscard]] std::size_t leading_spaces(std::string_view line) {
    std::size_t count = 0;
    while (count < line.size() && (line[count] == ' ' || line[count] == '\t')) {
        ++count;
    }
    return count;
}

/// Whether a line is invisible to a rule that asks what surrounds a block. An
/// HTML comment renders as nothing, and this tree uses one to bracket the
/// history sections, so a list that runs across one has not been interrupted.
[[nodiscard]] bool is_transparent(Kind kind) {
    return kind == Kind::Blank || kind == Kind::FrontMatter || kind == Kind::HtmlComment;
}

[[nodiscard]] bool is_ascii_digit(char character) {
    return character >= '0' && character <= '9';
}

/// `^\s{0,3}(`{3,}|~{3,})(info)$`, with the runs reported.
struct FenceMatch {
    char character = '\0';
    std::size_t length = 0;
    std::string_view info;
    std::size_t indent = 0;
};

/// A fence marker. A line indented four spaces or more is code, not a fence,
/// which is the one CommonMark rule that changes how the rest of a file is read.
[[nodiscard]] std::optional<FenceMatch> fence_of(std::string_view line) {
    const std::size_t indent = leading_spaces(line);
    if (indent > 3 || indent >= line.size()) {
        return std::nullopt;
    }
    const char character = line[indent];
    if (character != '`' && character != '~') {
        return std::nullopt;
    }
    std::size_t length = 0;
    while (indent + length < line.size() && line[indent + length] == character) {
        ++length;
    }
    if (length < 3) {
        return std::nullopt;
    }
    FenceMatch match;
    match.character = character;
    match.length = length;
    match.indent = indent;
    match.info = trim(line.substr(indent + length));
    return match;
}

/// `^\s{0,3}#{1,6}(\s|$)`.
struct HeadingMatch {
    int level = 0;
    std::size_t indent = 0;
    std::size_t spaces_after_hashes = 0;
    bool closed = false;
    std::string_view text;
};

[[nodiscard]] std::optional<HeadingMatch> heading_of(std::string_view line) {
    const std::size_t indent = leading_spaces(line);
    if (indent > 3 || indent >= line.size() || line[indent] != '#') {
        return std::nullopt;
    }
    std::size_t hashes = 0;
    while (indent + hashes < line.size() && line[indent + hashes] == '#') {
        ++hashes;
    }
    if (hashes > 6) {
        return std::nullopt;
    }
    if (indent + hashes < line.size() && line[indent + hashes] != ' ') {
        // `#Heading` -- a heading with no space. Reported by MD018, but it is not
        // a heading for the purposes of anything else, which is why this returns
        // nullopt rather than a match with zero spaces.
        return std::nullopt;
    }
    HeadingMatch match;
    match.level = static_cast<int>(hashes);
    match.indent = indent;
    std::size_t at = indent + hashes;
    while (at < line.size() && line[at] == ' ') {
        ++at;
    }
    match.spaces_after_hashes = at - (indent + hashes);
    std::string_view rest = rtrim(line.substr(at));
    // A closed heading: trailing `#` run preceded by a space.
    if (!rest.empty() && rest.back() == '#') {
        std::size_t end = rest.size();
        while (end > 0 && rest[end - 1] == '#') {
            --end;
        }
        if (end == 0) {
            match.closed = true;
            rest = {};
        } else if (rest[end - 1] == ' ') {
            match.closed = true;
            rest = rtrim(rest.substr(0, end));
        }
    }
    match.text = rest;
    return match;
}

[[nodiscard]] bool is_thematic_break(std::string_view line) {
    const std::string_view body = trim(line);
    if (body.size() < 3) {
        return false;
    }
    const char character = body[0];
    if (character != '-' && character != '*' && character != '_') {
        return false;
    }
    std::size_t count = 0;
    for (const char value : body) {
        if (value == character) {
            ++count;
        } else if (value != ' ') {
            return false;
        }
    }
    return count >= 3;
}

struct ListMatch {
    bool ordered = false;
    char marker = '\0';
    std::size_t indent = 0;
    std::size_t spaces_after = 0;
};

[[nodiscard]] std::optional<ListMatch> list_of(std::string_view line) {
    const std::size_t indent = leading_spaces(line);
    if (indent >= line.size()) {
        return std::nullopt;
    }
    ListMatch match;
    match.indent = indent;
    const char first = line[indent];
    std::size_t at = indent;
    if (first == '-' || first == '*' || first == '+') {
        match.marker = first;
        at = indent + 1;
    } else if (is_ascii_digit(first)) {
        while (at < line.size() && is_ascii_digit(line[at])) {
            ++at;
        }
        if (at >= line.size() || (line[at] != '.' && line[at] != ')')) {
            return std::nullopt;
        }
        match.ordered = true;
        match.marker = line[at];
        ++at;
    } else {
        return std::nullopt;
    }
    if (at >= line.size() || line[at] != ' ') {
        return std::nullopt;
    }
    // How wide the marker itself is: `10.` is three characters, `-` is one, and
    // counting the digits rather than assuming one is the difference between
    // `10. item` looking like two spaces and it being one.
    const std::size_t marker_width = at - indent;
    while (at < line.size() && line[at] == ' ') {
        ++at;
    }
    match.spaces_after = at - indent - marker_width;
    return match;
}

/// The columns of a table row, split on unescaped `|`.
[[nodiscard]] std::vector<std::string_view> table_cells(std::string_view line) {
    std::vector<std::string_view> cells;
    const std::string_view body = trim(line);
    std::size_t start = 0;
    std::size_t index = 0;
    bool escaped = false;
    for (; index < body.size(); ++index) {
        const char character = body[index];
        if (escaped) {
            escaped = false;
            continue;
        }
        if (character == '\\') {
            escaped = true;
            continue;
        }
        if (character == '|') {
            cells.push_back(body.substr(start, index - start));
            start = index + 1;
        }
    }
    cells.push_back(body.substr(start));
    // A leading and a trailing pipe produce an empty first and last cell; they
    // are the style MD055 is about, not columns of their own.
    if (!cells.empty() && trim(cells.front()).empty() && !body.empty() && body.front() == '|') {
        cells.erase(cells.begin());
    }
    if (!cells.empty() && trim(cells.back()).empty() && !body.empty() && body.back() == '|') {
        cells.pop_back();
    }
    return cells;
}

[[nodiscard]] bool is_table_delimiter(std::string_view line) {
    const std::string_view body = trim(line);
    if (body.empty() || body.find('|') == std::string_view::npos) {
        return false;
    }
    for (const char character : body) {
        if (character != '|' && character != '-' && character != ':' && character != ' ') {
            return false;
        }
    }
    return body.find('-') != std::string_view::npos;
}

/// Whether `line` opens or continues an HTML comment block.
[[nodiscard]] bool opens_html_comment(std::string_view line, bool* closes_same_line) {
    const std::string_view body = trim(line);
    if (body.rfind("<!--", 0) != 0) {
        return false;
    }
    *closes_same_line = body.find("-->", 4) != std::string_view::npos;
    return true;
}

struct Document {
    std::vector<Line> lines;
    /// The index of the first line after front matter, or 0.
    std::size_t content_start = 0;
};

[[nodiscard]] Document parse(std::string_view text) {
    Document document;
    const std::vector<std::string_view> raw_lines = split_lines(text);
    document.lines.reserve(raw_lines.size());

    // Front matter: a `---` first line and a closing `---` or `...`.
    std::size_t first = 0;
    if (!raw_lines.empty() && trim(raw_lines.front()) == "---") {
        for (std::size_t index = 1; index < raw_lines.size(); ++index) {
            const std::string_view body = trim(raw_lines[index]);
            if (body == "---" || body == "...") {
                first = index + 1;
                break;
            }
        }
    }
    document.content_start = first;

    bool in_fence = false;
    char fence_char = '\0';
    std::size_t fence_length = 0;
    bool in_comment = false;

    for (std::size_t index = 0; index < raw_lines.size(); ++index) {
        Line line;
        line.raw = raw_lines[index];

        if (index < first) {
            line.kind = Kind::FrontMatter;
            document.lines.push_back(line);
            continue;
        }

        if (in_comment) {
            line.kind = Kind::HtmlComment;
            if (line.raw.find("-->") != std::string_view::npos) {
                in_comment = false;
            }
            document.lines.push_back(line);
            continue;
        }

        const std::optional<FenceMatch> fence = fence_of(line.raw);
        if (in_fence) {
            // A closing fence: same character, at least as long, nothing after it.
            if (fence.has_value() && fence->character == fence_char &&
                fence->length >= fence_length && fence->info.empty()) {
                line.kind = Kind::FenceClose;
                line.fence_char = fence->character;
                line.fence_length = fence->length;
                in_fence = false;
            } else {
                line.kind = Kind::Code;
            }
            document.lines.push_back(line);
            continue;
        }

        if (trim(line.raw).empty()) {
            line.kind = Kind::Blank;
            document.lines.push_back(line);
            continue;
        }

        bool closes_same_line = false;
        if (opens_html_comment(line.raw, &closes_same_line)) {
            line.kind = Kind::HtmlComment;
            in_comment = !closes_same_line;
            document.lines.push_back(line);
            continue;
        }

        if (fence.has_value()) {
            line.kind = Kind::FenceOpen;
            line.fence_char = fence->character;
            line.fence_length = fence->length;
            line.fence_info = fence->info;
            in_fence = true;
            fence_char = fence->character;
            fence_length = fence->length;
            document.lines.push_back(line);
            continue;
        }

        if (const std::optional<HeadingMatch> heading = heading_of(line.raw);
            heading.has_value()) {
            line.kind = Kind::Heading;
            line.level = heading->level;
            line.indent = heading->indent;
            line.closed = heading->closed;
            line.text = heading->text;
            document.lines.push_back(line);
            continue;
        }

        if (is_thematic_break(line.raw)) {
            line.kind = Kind::ThematicBreak;
            document.lines.push_back(line);
            continue;
        }

        if (is_table_delimiter(line.raw)) {
            line.kind = Kind::TableDelimiter;
            document.lines.push_back(line);
            continue;
        }

        if (const std::optional<ListMatch> list = list_of(line.raw); list.has_value()) {
            line.kind = Kind::ListItem;
            line.ordered = list->ordered;
            line.marker = list->marker;
            line.indent = list->indent;
            line.spaces_after_marker = list->spaces_after;
            document.lines.push_back(line);
            continue;
        }

        if (const std::string_view body = trim(line.raw); !body.empty() && body.front() == '>') {
            line.kind = Kind::BlockQuote;
            document.lines.push_back(line);
            continue;
        }

        // A row of a table that has already started, or a line that looks like the
        // head of one because the next line delimits it.
        const bool next_is_delimiter =
            index + 1 < raw_lines.size() && is_table_delimiter(raw_lines[index + 1]);
        const bool previous_was_table =
            !document.lines.empty() && (document.lines.back().kind == Kind::TableRow ||
                                        document.lines.back().kind == Kind::TableDelimiter);
        if (line.raw.find('|') != std::string_view::npos &&
            (next_is_delimiter || previous_was_table)) {
            line.kind = Kind::TableRow;
            document.lines.push_back(line);
            continue;
        }

        line.kind = Kind::Text;
        document.lines.push_back(line);
    }
    return document;
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

/// A list block: the run of lines that belong to one list, including the
/// continuation lines of its items and the blank lines between them.
///
/// Both the indentation rule and the numbering rule need this, because a list is
/// not a line: a bullet with a wrapped paragraph under it is two lines of one
/// item, and a list with a blank line in the middle is still one list.
struct ListBlock {
    std::size_t first = 0;  ///< index of the first line
    std::size_t last = 0;   ///< index of the last line
};

[[nodiscard]] std::vector<ListBlock> list_blocks(const Document& document) {
    std::vector<ListBlock> blocks;
    std::optional<ListBlock> current;
    bool previous_belongs = false;
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& line = document.lines[index];
        bool belongs = line.is_list();
        if (!belongs && line.kind == Kind::Text && previous_belongs) {
            // A continuation of an item: indented at least as far as the text
            // after that item's marker.
            const std::size_t indent = leading_spaces(line.raw);
            const Line& previous = document.lines[index - 1];
            const std::size_t needed =
                previous.is_list() ? previous.indent + 2 : 2;
            belongs = indent >= needed;
        }
        if (!belongs && is_transparent(line.kind) && current.has_value()) {
            // A blank line or an HTML comment inside or after a list keeps the
            // block open until something that is not a list follows it.
            continue;
        }
        if (line.kind == Kind::FenceOpen && current.has_value()) {
            // A fenced block indented to the item's content is part of that item,
            // which is how a list of tips with an example under each one is
            // written. An unindented fence ends the list.
            const std::size_t fence_indent = leading_spaces(line.raw);
            if (fence_indent >= 2) {
                current->last = index;
                ++index;
                while (index < document.lines.size() &&
                       document.lines[index].kind != Kind::FenceClose) {
                    current->last = index;
                    ++index;
                }
                if (index < document.lines.size()) {
                    current->last = index;
                }
                previous_belongs = true;
                continue;
            }
        }
        if (!belongs) {
            if (current.has_value()) {
                blocks.push_back(*current);
                current.reset();
            }
            previous_belongs = false;
            continue;
        }
        if (!current.has_value()) {
            current = ListBlock{index, index};
        }
        current->last = index;
        previous_belongs = true;
    }
    if (current.has_value()) {
        blocks.push_back(*current);
    }
    return blocks;
}

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

}  // namespace

std::vector<Issue> lint(std::string_view text) {
    const Document document = parse(text);
    std::vector<Issue> issues;
    md001(document, &issues);
    md003(document, &issues);
    md004(document, &issues);
    md005(document, &issues);
    md007(document, &issues);
    md009(document, &issues);
    md010(document, &issues);
    md012(document, &issues);
    md018(document, &issues);
    md019(document, &issues);
    md022(document, &issues);
    md023(document, &issues);
    md025(document, &issues);
    md026(document, &issues);
    md028(document, &issues);
    md029(document, &issues);
    md030(document, &issues);
    md031(document, &issues);
    md032(document, &issues);
    md033(document, &issues);
    md034(document, &issues);
    md040(document, &issues);
    md041(document, &issues);
    md046(document, &issues);
    md047(text, document, &issues);
    md048(document, &issues);
    md049(document, &issues);
    md055(document, &issues);

    std::stable_sort(issues.begin(), issues.end(),
                     [](const Issue& left, const Issue& right) {
                         return left.line < right.line;
                     });
    return issues;
}

std::string describe(std::string_view line) {
    const Document document = parse(line);
    std::string text;
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& entry = document.lines[index];
        const char* kind = "text";
        switch (entry.kind) {
            case Kind::Blank: kind = "blank"; break;
            case Kind::FrontMatter: kind = "front-matter"; break;
            case Kind::FenceOpen: kind = "fence-open"; break;
            case Kind::FenceClose: kind = "fence-close"; break;
            case Kind::Code: kind = "code"; break;
            case Kind::Heading: kind = "heading"; break;
            case Kind::ThematicBreak: kind = "thematic-break"; break;
            case Kind::BlockQuote: kind = "quote"; break;
            case Kind::ListItem: kind = "list"; break;
            case Kind::TableRow: kind = "table-row"; break;
            case Kind::TableDelimiter: kind = "table-delim"; break;
            case Kind::HtmlComment: kind = "comment"; break;
            case Kind::Text: kind = "text"; break;
        }
        text += std::to_string(index + 1) + "\t" + kind;
        if (entry.kind == Kind::Heading) {
            text += " h" + std::to_string(entry.level);
        }
        if (entry.kind == Kind::ListItem) {
            text += std::string{" "} + (entry.ordered ? "ordered" : "bullet") + " indent=" +
                    std::to_string(entry.indent) + " after=" +
                    std::to_string(entry.spaces_after_marker);
        }
        text += "\t" + std::string{rtrim(entry.raw).substr(0, 40)};
        text += "\n";
    }
    return text;
}

int run(const std::vector<std::string>& files, bool quiet) {
    int total = 0;
    int files_with_issues = 0;
    for (const std::string& path : files) {
        const std::optional<std::string> text = read_text(path);
        if (!text.has_value()) {
            std::fprintf(stderr, "❌ %s: cannot be read as UTF-8\n", path.c_str());
            ++total;
            ++files_with_issues;
            continue;
        }
        const std::vector<Issue> issues = lint(*text);
        if (issues.empty()) {
            continue;
        }
        ++files_with_issues;
        total += static_cast<int>(issues.size());
        for (const Issue& issue : issues) {
            std::printf("%s:%d: %s: %s\n", path.c_str(), issue.line, issue.rule.c_str(),
                        issue.message.c_str());
        }
    }
    if (total == 0) {
        if (!quiet) {
            std::printf("markdown: %d file(s), no issues\n", static_cast<int>(files.size()));
        }
        return 0;
    }
    std::fprintf(stderr, "\n%d issue(s) in %d file(s)\n", total, files_with_issues);
    return 1;
}

int run_md_command(int argc, char** argv) {
    std::string root;
    bool quiet = false;
    bool explain = false;
    std::vector<std::string> files;
    for (int index = 2; index < argc; ++index) {
        const std::string_view argument = argv[index];
        if (argument == "--quiet") {
            quiet = true;
        } else if (argument == "--explain") {
            explain = true;
        } else if (argument == "--root" && index + 1 < argc) {
            root = argv[++index];
        } else if (argument.rfind("--root=", 0) == 0) {
            root = std::string{argument.substr(7)};
        } else if (argument == "--help" || argument == "-h") {
            std::printf("usage: time_warp_docs md [--root DIR] [--quiet] [FILE ...]\n");
            return 0;
        } else if (argument.rfind("--", 0) == 0) {
            std::fprintf(stderr, "md: unknown option %s\n", std::string{argument}.c_str());
            return 2;
        } else {
            files.emplace_back(argument);
        }
    }

    if (files.empty()) {
        // The tracked files, so a build tree or a vendored directory stays out of
        // the run without a second ignore list -- the same reason the claims gate
        // asks git.
        std::string command = "git ls-files '*.md'";
        if (!root.empty()) {
            command = "cd '" + root + "' && " + command;
        }
        const CommandResult listing = run_command(command);
        if (!listing.started || listing.status != 0) {
            std::fprintf(stderr,
                         "❌ md: `git ls-files` failed; pass the files on the command "
                         "line instead\n");
            return 2;
        }
        std::size_t start = 0;
        while (start < listing.output.size()) {
            const std::size_t end = listing.output.find('\n', start);
            const std::string line =
                listing.output.substr(start, end == std::string::npos ? std::string::npos
                                                                     : end - start);
            if (!line.empty()) {
                files.push_back(root.empty() ? line : root + "/" + line);
            }
            if (end == std::string::npos) {
                break;
            }
            start = end + 1;
        }
        if (files.empty()) {
            std::printf("markdown: no tracked Markdown files\n");
            return 0;
        }
    }
    if (explain) {
        for (const std::string& path : files) {
            const std::optional<std::string> text = read_text(path);
            if (!text.has_value()) {
                std::fprintf(stderr, "❌ %s: cannot be read\n", path.c_str());
                return 2;
            }
            std::fputs(describe(*text).c_str(), stdout);
        }
        return 0;
    }
    return run(files, quiet);
}

}  // namespace docs::md
