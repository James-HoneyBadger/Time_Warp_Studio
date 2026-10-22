// SPDX-License-Identifier: MIT
//
// The text vocabulary the documentation gates share.
//
// Hand-written rather than <regex>, and deliberately so. The Python these
// functions replace speaks `re`, whose leftmost-match and backtracking rules are
// not ECMAScript's; a scanner that disagreed by one character would make the port
// report a failure the reference does not, which is the one thing a port must not
// do. The shapes needed here -- a fenced line, a backticked token, an inline link,
// an ATX heading -- are small enough to read in a dozen lines each.

#pragma once

#include <cstddef>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace docs {

/// The file's text, or nullopt when it cannot be read or is not valid UTF-8 --
/// the two things Python's `read_text(encoding="utf-8")` refuses.
[[nodiscard]] std::optional<std::string> read_text(const std::filesystem::path& path);

/// Writes `text` to `path` as UTF-8. False when the write failed.
bool write_text(const std::filesystem::path& path, std::string_view text);

/// `str.splitlines()`: the lines, each without its terminator, splitting on every
/// terminator Python recognises.
[[nodiscard]] std::vector<std::string_view> split_lines(std::string_view text);

/// Whether a whitespace character starts at `position`, in the sense `\s` has in a
/// Python `str` pattern (which is Unicode-aware, unlike ECMAScript's).
[[nodiscard]] bool is_space_at(std::string_view text, std::size_t position);
[[nodiscard]] std::size_t skip_space(std::string_view text, std::size_t position);

/// `^\s*(```|~~~)`, or only backticks when `tildes` is false -- which is the
/// narrower fence rule `check_doc_claims.py` uses, kept as it is.
[[nodiscard]] bool is_fence(std::string_view line, bool tildes);

/// Every `` `token` `` on a line: never empty, never spanning a backtick.
[[nodiscard]] std::vector<std::string_view> backtick_tokens(std::string_view line);

/// The line with every `inline code span` removed, as `re.sub` does for links.
[[nodiscard]] std::string without_inline_code(std::string_view line);

/// Every `[text](target)` and `![alt](target)` on a line, in order.
[[nodiscard]] std::vector<std::string_view> link_targets(std::string_view line);

/// The text of an ATX heading: `## Title ##` gives `Title`, a line without the
/// `#{1,6}` and a following space gives nullopt.
[[nodiscard]] std::optional<std::string_view> heading_text(std::string_view line);

/// GitHub's heading anchor: backticks and HTML tags removed, lower case, spaces to
/// `-`, punctuation dropped.
[[nodiscard]] std::string slug(std::string_view text);

/// `str.strip()`: both ends trimmed of the whitespace `\s` names.
[[nodiscard]] std::string_view trim(std::string_view text);

/// Words split on whitespace, as `str.split()` does.
[[nodiscard]] std::vector<std::string_view> split_words(std::string_view text);

/// ASCII lower case, for the case-insensitive vocabulary the claims gate reads.
[[nodiscard]] std::string lower_ascii(std::string_view text);

}  // namespace docs
