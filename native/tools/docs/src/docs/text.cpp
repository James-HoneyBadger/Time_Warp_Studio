// SPDX-License-Identifier: MIT
//
// Reading, splitting and scanning the text the gates read. See text.hpp for why
// these are scanners rather than regular expressions.

#include "docs/text.hpp"

#include <fstream>
#include <iterator>

namespace docs {
namespace {

struct CodePoint {
    char32_t value = 0;
    std::size_t length = 1;
};

CodePoint decode_at(std::string_view text, std::size_t position) {
    const auto lead = static_cast<unsigned char>(text[position]);
    if (lead < 0x80) {
        return {lead, 1};
    }
    std::size_t length = 0;
    char32_t value = 0;
    if ((lead & 0xE0) == 0xC0) {
        length = 2;
        value = lead & 0x1F;
    } else if ((lead & 0xF0) == 0xE0) {
        length = 3;
        value = lead & 0x0F;
    } else if ((lead & 0xF8) == 0xF0) {
        length = 4;
        value = lead & 0x07;
    } else {
        return {lead, 1};
    }
    if (position + length > text.size()) {
        return {lead, 1};
    }
    for (std::size_t index = 1; index < length; ++index) {
        const auto continuation = static_cast<unsigned char>(text[position + index]);
        if ((continuation & 0xC0) != 0x80) {
            return {lead, 1};
        }
        value = static_cast<char32_t>((value << 6) | (continuation & 0x3F));
    }
    return {value, length};
}

bool is_valid_utf8(std::string_view text) {
    std::size_t position = 0;
    while (position < text.size()) {
        const auto lead = static_cast<unsigned char>(text[position]);
        if (lead < 0x80) {
            ++position;
            continue;
        }
        std::size_t length = 0;
        char32_t value = 0;
        char32_t smallest = 0;
        if ((lead & 0xE0) == 0xC0) {
            length = 2;
            value = lead & 0x1F;
            smallest = 0x80;
        } else if ((lead & 0xF0) == 0xE0) {
            length = 3;
            value = lead & 0x0F;
            smallest = 0x800;
        } else if ((lead & 0xF8) == 0xF0) {
            length = 4;
            value = lead & 0x07;
            smallest = 0x10000;
        } else {
            return false;
        }
        if (position + length > text.size()) {
            return false;
        }
        for (std::size_t index = 1; index < length; ++index) {
            const auto continuation = static_cast<unsigned char>(text[position + index]);
            if ((continuation & 0xC0) != 0x80) {
                return false;
            }
            value = static_cast<char32_t>((value << 6) | (continuation & 0x3F));
        }
        if (value < smallest || value > 0x10FFFF) {
            return false;
        }
        if (value >= 0xD800 && value <= 0xDFFF) {
            return false;
        }
        position += length;
    }
    return true;
}

void append_utf8(std::string& out, char32_t value) {
    if (value < 0x80) {
        out += static_cast<char>(value);
    } else if (value < 0x800) {
        out += static_cast<char>(0xC0 | (value >> 6));
        out += static_cast<char>(0x80 | (value & 0x3F));
    } else if (value < 0x10000) {
        out += static_cast<char>(0xE0 | (value >> 12));
        out += static_cast<char>(0x80 | ((value >> 6) & 0x3F));
        out += static_cast<char>(0x80 | (value & 0x3F));
    } else {
        out += static_cast<char>(0xF0 | (value >> 18));
        out += static_cast<char>(0x80 | ((value >> 12) & 0x3F));
        out += static_cast<char>(0x80 | ((value >> 6) & 0x3F));
        out += static_cast<char>(0x80 | (value & 0x3F));
    }
}

bool is_python_space(char32_t value) {
    switch (value) {
        case 0x09:  // tab
        case 0x0A:  // newline
        case 0x0B:  // vertical tab
        case 0x0C:  // form feed
        case 0x0D:  // carriage return
        case 0x1C:
        case 0x1D:
        case 0x1E:
        case 0x1F:
        case 0x20:  // space
        case 0x85:  // next line
        case 0xA0:  // no-break space
        case 0x1680:
        case 0x2028:
        case 0x2029:
        case 0x202F:
        case 0x205F:
        case 0x3000:
            return true;
        default:
            return value >= 0x2000 && value <= 0x200A;
    }
}

/// The bytes a line terminator occupies at `position`, or 0 when there is none.
/// `str.splitlines()` also breaks on the C0 separators and the Unicode ones, and
/// a file copied out of a Windows editor is the ordinary case for `\r\n`.
std::size_t terminator_length(std::string_view text, std::size_t position) {
    const char character = text[position];
    if (character == '\n' || character == '\v' || character == '\f' || character == '\x1C' ||
        character == '\x1D' || character == '\x1E') {
        return 1;
    }
    if (character == '\r') {
        return (position + 1 < text.size() && text[position + 1] == '\n') ? 2 : 1;
    }
    if (static_cast<unsigned char>(character) >= 0x80) {
        const CodePoint point = decode_at(text, position);
        if (point.value == 0x85 || point.value == 0x2028 || point.value == 0x2029) {
            return point.length;
        }
    }
    return 0;
}

/// `str.isalnum()` for the scripts a heading is written in.
///
/// Python asks Unicode; without ICU the answer has to be written down. The set
/// below is the *non*-alphanumeric blocks -- punctuation, symbols, marks, spaces,
/// emoji -- so that an accented, Greek, Cyrillic or CJK heading keeps its letters,
/// which is the case that matters, and a dash, a quote or a tick does not, which is
/// the case that is everywhere. Nothing in this repository's headings is affected;
/// the approximation is here so the next language's is not silently mangled.
bool is_unicode_alnum(char32_t value) {
    struct Range {
        char32_t first;
        char32_t last;
    };
    static constexpr Range kNotAlnum[] = {
        {0x0080, 0x00A0},  // C1 controls and no-break space
        {0x00A1, 0x00BF},  // Latin-1 punctuation and signs (c, R, 2/3, inverted marks)
        {0x00D7, 0x00D7},  // multiplication sign
        {0x00F7, 0x00F7},  // division sign
        {0x02B0, 0x02FF},  // spacing modifier letters and combining marks
        {0x0300, 0x036F},  // combining diacritical marks
        {0x0483, 0x0489},  // Cyrillic combining marks
        {0x1D00, 0x1DBF},  // phonetic extensions
        {0x2000, 0x206F},  // general punctuation: dashes, quotes, ellipsis, superscripts
        {0x2070, 0x2BFF},  // currency, letterlike, arrows, maths, dingbats (the tick)
        {0x2E00, 0x2E7F},  // supplemental punctuation
        {0x3000, 0x303F},  // CJK symbols and punctuation
        {0xFE00, 0xFE0F},  // variation selectors
        {0xFE30, 0xFE6F},  // compatibility forms, small forms
        {0xFF00, 0xFF20},  // fullwidth punctuation; fullwidth letters and digits stay
        {0xFF3B, 0xFF40},
        {0xFF5B, 0xFF65},
        {0xFFE0, 0xFFEF},
        {0x1F000, 0x1FAFF},  // emoji and pictographs
        {0xE0000, 0xE007F},  // tag characters
    };
    for (const Range& range : kNotAlnum) {
        if (value >= range.first && value <= range.last) {
            return false;
        }
    }
    return true;
}

/// `str.lower()` for the Latin, Greek and Cyrillic ranges a heading may use. Full
/// Unicode case folding would need ICU; the ranges here are the ones a document in
/// this repository can hold, and an unmapped character keeps its case rather than
/// becoming something else.
char32_t lower_code_point(char32_t value) {
    if (value < 0x80) {
        return (value >= U'A' && value <= U'Z') ? value + 0x20 : value;
    }
    if (value >= 0x00C0 && value <= 0x00DE && value != 0x00D7) {
        return value + 0x20;
    }
    if (value >= 0x0100 && value <= 0x017F && value % 2 == 0) {
        return value + 1;
    }
    if (value >= 0x0391 && value <= 0x03A9 && value != 0x03A2) {
        return value + 0x20;
    }
    if (value >= 0x0410 && value <= 0x042F) {
        return value + 0x20;
    }
    if (value >= 0x0400 && value <= 0x040F) {
        return value + 0x50;
    }
    return value;
}

/// Whether `text` is entirely `\s*#*\s*`, the tail `HEADING` allows after its
/// title: closing hashes and the whitespace around them.
bool is_heading_tail(std::string_view text) {
    std::size_t at = skip_space(text, 0);
    while (at < text.size() && text[at] == '#') {
        ++at;
    }
    return skip_space(text, at) == text.size();
}

}  // namespace

std::optional<std::string> read_text(const std::filesystem::path& path) {
    std::ifstream stream(path, std::ios::binary);
    if (!stream) {
        return std::nullopt;
    }
    std::string content((std::istreambuf_iterator<char>(stream)), std::istreambuf_iterator<char>());
    if (stream.bad() || !is_valid_utf8(content)) {
        return std::nullopt;
    }
    return content;
}

bool write_text(const std::filesystem::path& path, std::string_view text) {
    std::ofstream stream(path, std::ios::binary | std::ios::trunc);
    if (!stream) {
        return false;
    }
    stream.write(text.data(), static_cast<std::streamsize>(text.size()));
    return stream.good();
}

std::vector<std::string_view> split_lines(std::string_view text) {
    std::vector<std::string_view> lines;
    std::size_t start = 0;
    std::size_t position = 0;
    while (position < text.size()) {
        const std::size_t terminator = terminator_length(text, position);
        if (terminator == 0) {
            ++position;
            continue;
        }
        lines.push_back(text.substr(start, position - start));
        position += terminator;
        start = position;
    }
    if (start < text.size()) {
        lines.push_back(text.substr(start));
    }
    return lines;
}

bool is_space_at(std::string_view text, std::size_t position) {
    if (position >= text.size()) {
        return false;
    }
    return is_python_space(decode_at(text, position).value);
}

std::size_t skip_space(std::string_view text, std::size_t position) {
    while (position < text.size()) {
        const CodePoint point = decode_at(text, position);
        if (!is_python_space(point.value)) {
            break;
        }
        position += point.length;
    }
    return position;
}

bool is_fence(std::string_view line, bool tildes) {
    const std::string_view rest = line.substr(skip_space(line, 0));
    if (rest.starts_with("```")) {
        return true;
    }
    return tildes && rest.starts_with("~~~");
}

std::vector<std::string_view> backtick_tokens(std::string_view line) {
    std::vector<std::string_view> tokens;
    std::size_t position = 0;
    while (position < line.size()) {
        if (line[position] != '`') {
            ++position;
            continue;
        }
        const std::size_t close = line.find('`', position + 1);
        if (close == std::string_view::npos || close == position + 1) {
            // No closing backtick, or nothing between the pair: `[^`\n]+` needs at
            // least one character, so the regex engine starts again one byte later.
            ++position;
            continue;
        }
        tokens.push_back(line.substr(position + 1, close - position - 1));
        position = close + 1;
    }
    return tokens;
}

std::string without_inline_code(std::string_view line) {
    std::string result;
    result.reserve(line.size());
    std::size_t position = 0;
    while (position < line.size()) {
        if (line[position] == '`') {
            const std::size_t close = line.find('`', position + 1);
            if (close != std::string_view::npos) {
                position = close + 1;
                continue;
            }
        }
        result += line[position];
        ++position;
    }
    return result;
}

std::vector<std::string_view> link_targets(std::string_view line) {
    std::vector<std::string_view> targets;
    std::size_t position = 0;
    while (position < line.size()) {
        const bool bang = line[position] == '!';
        const bool bracket = line[position] == '[' ||
                             (bang && position + 1 < line.size() && line[position + 1] == '[');
        if (!bracket) {
            ++position;
            continue;
        }
        const std::size_t open = bang ? position + 1 : position;
        const std::size_t close = line.find(']', open + 1);
        if (close == std::string_view::npos || close + 1 >= line.size() ||
            line[close + 1] != '(') {
            ++position;
            continue;
        }

        std::size_t target_end = close + 2;
        while (target_end < line.size() && line[target_end] != ')' &&
               !is_space_at(line, target_end)) {
            ++target_end;
        }
        if (target_end == close + 2) {
            ++position;
            continue;
        }

        // `(?:\s+"[^"]*")?` -- a link title, which is not part of the target.
        std::size_t after = target_end;
        if (after < line.size() && is_space_at(line, after)) {
            const std::size_t title = skip_space(line, after);
            if (title < line.size() && line[title] == '"') {
                const std::size_t quote = line.find('"', title + 1);
                if (quote != std::string_view::npos) {
                    after = quote + 1;
                }
            }
        }

        if (after < line.size() && line[after] == ')') {
            targets.push_back(line.substr(close + 2, target_end - close - 2));
            position = after + 1;
            continue;
        }
        ++position;
    }
    return targets;
}

std::optional<std::string_view> heading_text(std::string_view line) {
    std::size_t at = 0;
    while (at < line.size() && at < 6 && line[at] == '#') {
        ++at;
    }
    if (at == 0) {
        return std::nullopt;
    }
    const std::size_t after = skip_space(line, at);
    if (after == at) {
        return std::nullopt;
    }
    const std::string_view rest = line.substr(after);
    // `(.*?)` is lazy, so the title is the shortest prefix whose remainder is the
    // heading's tail; the empty remainder always is, which is why this ends.
    for (std::size_t split = 0; split <= rest.size(); ++split) {
        if (is_heading_tail(rest.substr(split))) {
            return rest.substr(0, split);
        }
    }
    return std::nullopt;
}

std::string slug(std::string_view text) {
    std::string without_ticks;
    without_ticks.reserve(text.size());
    for (const char character : text) {
        if (character != '`') {
            without_ticks += character;
        }
    }

    std::string without_tags;
    for (std::size_t at = 0; at < without_ticks.size();) {
        if (without_ticks[at] == '<') {
            // `<[^>]+>` needs something between the brackets.
            const std::size_t close = without_ticks.find('>', at + 2);
            if (close != std::string::npos) {
                at = close + 1;
                continue;
            }
        }
        without_tags += without_ticks[at];
        ++at;
    }

    const std::string_view trimmed = trim(without_tags);
    std::string result;
    result.reserve(trimmed.size());
    std::size_t at = 0;
    while (at < trimmed.size()) {
        const CodePoint point = decode_at(trimmed, at);
        at += point.length;
        const char32_t lowered = lower_code_point(point.value);
        if (lowered == U' ') {
            result += '-';
            continue;
        }
        if (lowered == U'-' || lowered == U'_') {
            result += static_cast<char>(lowered);
            continue;
        }
        if (lowered < 0x80) {
            if ((lowered >= U'0' && lowered <= U'9') || (lowered >= U'a' && lowered <= U'z')) {
                result += static_cast<char>(lowered);
            }
            continue;
        }
        if (is_unicode_alnum(lowered)) {
            append_utf8(result, lowered);
        }
    }
    return result;
}

std::string_view trim(std::string_view text) {
    std::size_t begin = 0;
    while (begin < text.size() && is_space_at(text, begin)) {
        begin += decode_at(text, begin).length;
    }
    std::size_t end = text.size();
    std::size_t scan = begin;
    while (scan < text.size()) {
        const CodePoint point = decode_at(text, scan);
        if (!is_python_space(point.value)) {
            end = scan + point.length;
        }
        scan += point.length;
    }
    if (end < begin) {
        end = begin;
    }
    return text.substr(begin, end - begin);
}

std::vector<std::string_view> split_words(std::string_view text) {
    std::vector<std::string_view> words;
    std::size_t position = 0;
    while (position < text.size()) {
        const std::size_t start = skip_space(text, position);
        if (start >= text.size()) {
            break;
        }
        std::size_t end = start;
        while (end < text.size() && !is_space_at(text, end)) {
            ++end;
        }
        words.push_back(text.substr(start, end - start));
        position = end;
    }
    return words;
}

std::string lower_ascii(std::string_view text) {
    std::string result;
    result.reserve(text.size());
    for (const char character : text) {
        result += (character >= 'A' && character <= 'Z')
                      ? static_cast<char>(character - 'A' + 'a')
                      : character;
    }
    return result;
}

}  // namespace docs
