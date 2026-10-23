// SPDX-License-Identifier: MIT
//
// The line model: one pass over the text, one `Line` per source line, with what the
// rules need to know about it already decided -- is it a fence, a heading, a table
// row, a list item -- so that no rule re-parses the document and two rules cannot
// disagree about where a heading ends. The types are in `md_internal.hpp`.

#include "md_internal.hpp"

#include "docs/text.hpp"

#include <algorithm>
#include <cstddef>
#include <optional>
#include <string_view>
#include <vector>

namespace docs::md::detail {

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

}  // namespace docs::md::detail
