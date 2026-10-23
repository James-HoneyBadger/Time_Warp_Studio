// SPDX-License-Identifier: MIT
//
// The Markdown linter's line model, shared by the files that hold its rules.
//
// `md.cpp` was 1,494 lines: a line model, twenty-eight rules and the command that
// runs them. The rules were already one function each, so the split is the model
// into a header and the rules into a file per subject. The model lives in
// `docs::md::detail` rather than an anonymous namespace, because several translation
// units parse documents now and a type they all pass around needs one definition.
//
// Internal to the `md` command: nothing outside `src/docs/md_*.cpp` includes it.

#pragma once

#include "docs/md.hpp"

#include <cstddef>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace docs::md::detail {

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

/// `^\s{0,3}(`{3,}|~{3,})(info)$`, with the runs reported.
struct FenceMatch {
    char character = '\0';
    std::size_t length = 0;
    std::string_view info;
    std::size_t indent = 0;
};

/// `^\s{0,3}#{1,6}(\s|$)`.
struct HeadingMatch {
    int level = 0;
    std::size_t indent = 0;
    std::size_t spaces_after_hashes = 0;
    bool closed = false;
    std::string_view text;
};

struct ListMatch {
    bool ordered = false;
    char marker = '\0';
    std::size_t indent = 0;
    std::size_t spaces_after = 0;
};

struct Document {
    std::vector<Line> lines;
    /// The index of the first line after front matter, or 0.
    std::size_t content_start = 0;
};

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

[[nodiscard]] std::string_view rtrim(std::string_view text);

[[nodiscard]] std::size_t leading_spaces(std::string_view line);

/// Whether a line is invisible to a rule that asks what surrounds a block. An
/// HTML comment renders as nothing, and this tree uses one to bracket the
/// history sections, so a list that runs across one has not been interrupted.
[[nodiscard]] bool is_transparent(Kind kind);

[[nodiscard]] bool is_ascii_digit(char character);

/// A fence marker. A line indented four spaces or more is code, not a fence,
/// which is the one CommonMark rule that changes how the rest of a file is read.
[[nodiscard]] std::optional<FenceMatch> fence_of(std::string_view line);

[[nodiscard]] std::optional<HeadingMatch> heading_of(std::string_view line);

[[nodiscard]] bool is_thematic_break(std::string_view line);

[[nodiscard]] std::optional<ListMatch> list_of(std::string_view line);

/// The columns of a table row, split on unescaped `|`.
[[nodiscard]] std::vector<std::string_view> table_cells(std::string_view line);

[[nodiscard]] bool is_table_delimiter(std::string_view line);

/// Whether `line` opens or continues an HTML comment block.
[[nodiscard]] bool opens_html_comment(std::string_view line, bool* closes_same_line);

[[nodiscard]] Document parse(std::string_view text);

[[nodiscard]] std::vector<ListBlock> list_blocks(const Document& document);

}  // namespace docs::md::detail
