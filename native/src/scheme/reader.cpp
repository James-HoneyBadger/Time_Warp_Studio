// SPDX-License-Identifier: MIT

#include "timewarp/scheme/reader.hpp"

#include <charconv>
#include <cstdint>
#include <limits>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

namespace timewarp::scheme {
namespace {

/// The reference's `_DELIMITERS` set, as a lookup string.
constexpr std::string_view kDelimiters = "()'\"`; \t\r\n,";

[[nodiscard]] bool is_space(char ch) {
    return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' || ch == '\f' ||
           ch == '\v';
}

/// Mirrors Python's `str.strip()` for the whitespace a token can contain.
[[nodiscard]] std::string_view trim(std::string_view text) {
    while (!text.empty() && is_space(text.front())) {
        text.remove_prefix(1);
    }
    while (!text.empty() && is_space(text.back())) {
        text.remove_suffix(1);
    }
    return text;
}

/// Removes `_` separators, which Python accepts between digits.
[[nodiscard]] std::string strip_underscores(std::string_view text) {
    std::string cleaned;
    cleaned.reserve(text.size());
    for (const char ch : text) {
        if (ch != '_') {
            cleaned.push_back(ch);
        }
    }
    return cleaned;
}

/// The reference's escape table: `{"n", "t", "r", "\\", '"'}`, else identity.
[[nodiscard]] char unescape(char escape) {
    switch (escape) {
        case 'n':
            return '\n';
        case 't':
            return '\t';
        case 'r':
            return '\r';
        case '\\':
            return '\\';
        case '"':
            return '"';
        default:
            return escape;
    }
}

[[nodiscard]] std::string lower_ascii(std::string_view text) {
    std::string lowered;
    lowered.reserve(text.size());
    for (const char ch : text) {
        lowered.push_back(ch >= 'A' && ch <= 'Z'
                              ? static_cast<char>(ch - 'A' + 'a')
                              : ch);
    }
    return lowered;
}

/// A partially built form: either a list awaiting its closing paren, or a quote
/// sugar awaiting the single expression it wraps.
struct Frame {
    enum class Kind : std::uint8_t { List, Quote };
    Kind kind = Kind::List;
    std::vector<Value> items;
    Symbol* tag = nullptr;
    /// Line the form opened on, so its span can be recorded when it closes.
    std::uint32_t start_line = 1;
};

[[nodiscard]] bool parse_real_impl(std::string_view text, double& out) {
    if (text.empty()) {
        return false;
    }
    double value = 0.0;
    auto result = std::from_chars(text.data(), text.data() + text.size(), value,
                                  std::chars_format::general);
    if (result.ec == std::errc{} && result.ptr == text.data() + text.size()) {
        out = value;
        return true;
    }
    return false;
}

}  // namespace

std::vector<SourceToken> tokenize_with_lines(std::string_view source) {
    std::vector<SourceToken> tokens;
    std::size_t index = 0;
    const std::size_t size = source.size();
    std::uint32_t line = 1;

    while (index < size) {
        const char ch = source[index];

        if (is_space(ch)) {
            if (ch == '\n') {
                ++line;
            }
            ++index;
            continue;
        }
        if (ch == ';') {
            // Stops *at* the newline, which the whitespace branch above then
            // consumes and counts.
            while (index < size && source[index] != '\n') {
                ++index;
            }
            continue;
        }
        if (ch == '(' || ch == ')' || ch == '\'' || ch == '`' || ch == ',') {
            if (ch == ',' && index + 1 < size && source[index + 1] == '@') {
                tokens.push_back(SourceToken{",@", line});
                index += 2;
                continue;
            }
            tokens.push_back(SourceToken{std::string(1, ch), line});
            ++index;
            continue;
        }
        if (ch == '"') {
            // The token is attributed to the line the literal *opens* on, even
            // when it spans several.
            const std::uint32_t start_line = line;
            std::size_t cursor = index + 1;
            std::string buffer;
            while (cursor < size && source[cursor] != '"') {
                if (source[cursor] == '\\' && cursor + 1 < size) {
                    buffer.push_back(unescape(source[cursor + 1]));
                    cursor += 2;
                    continue;
                }
                if (source[cursor] == '\n') {
                    ++line;
                }
                buffer.push_back(source[cursor]);
                ++cursor;
            }
            if (cursor >= size) {
                throw ReaderError{"unterminated string literal"};
            }
            // The reference re-quotes the decoded body so `atom()` can still
            // recognise it as a string.
            std::string token;
            token.reserve(buffer.size() + 2);
            token.push_back('"');
            token += buffer;
            token.push_back('"');
            tokens.push_back(SourceToken{std::move(token), start_line});
            index = cursor + 1;
            continue;
        }
        if (ch == '|') {
            const std::uint32_t start_line = line;
            const std::size_t closing = source.find('|', index + 1);
            if (closing == std::string_view::npos) {
                throw ReaderError{"unterminated |symbol|"};
            }
            tokens.push_back(SourceToken{
                std::string{source.substr(index + 1, closing - index - 1)},
                start_line});
            for (std::size_t scan = index; scan < closing; ++scan) {
                if (source[scan] == '\n') {
                    ++line;
                }
            }
            index = closing + 1;
            continue;
        }

        std::size_t cursor = index;
        while (cursor < size && kDelimiters.find(source[cursor]) ==
                                    std::string_view::npos) {
            ++cursor;
        }
        tokens.push_back(
            SourceToken{std::string{source.substr(index, cursor - index)}, line});
        index = cursor;
    }
    return tokens;
}

std::vector<std::string> tokenize(std::string_view source) {
    const std::vector<SourceToken> with_lines = tokenize_with_lines(source);
    std::vector<std::string> tokens;
    tokens.reserve(with_lines.size());
    for (const SourceToken& token : with_lines) {
        tokens.push_back(token.text);
    }
    return tokens;
}

bool parse_integer_token(std::string_view text, std::int64_t& out) {
    text = trim(text);
    if (text.empty()) {
        return false;
    }

    bool negative = false;
    if (text.front() == '+' || text.front() == '-') {
        negative = text.front() == '-';
        text.remove_prefix(1);
    }
    if (text.empty()) {
        return false;
    }

    const std::string digits = strip_underscores(text);
    if (digits.empty()) {
        return false;
    }

    std::uint64_t magnitude = 0;
    const auto result = std::from_chars(digits.data(),
                                        digits.data() + digits.size(), magnitude);
    if (result.ec != std::errc{} || result.ptr != digits.data() + digits.size()) {
        return false;
    }

    // Values that do not fit are reported as "not an integer" so the caller
    // falls through to the real parser.  Python would widen to a bignum here;
    // see the divergence list in native/README.md.
    constexpr std::uint64_t kPositiveLimit = 9223372036854775807ULL;
    constexpr std::uint64_t kNegativeLimit = 9223372036854775808ULL;
    const std::uint64_t limit = negative ? kNegativeLimit : kPositiveLimit;
    if (magnitude > limit) {
        return false;
    }

    if (negative) {
        out = magnitude == kNegativeLimit
                  ? std::numeric_limits<std::int64_t>::min()
                  : -static_cast<std::int64_t>(magnitude);
    } else {
        out = static_cast<std::int64_t>(magnitude);
    }
    return true;
}

bool parse_real_token(std::string_view text, double& out) {
    text = trim(text);
    if (text.empty()) {
        return false;
    }
    const std::string cleaned = strip_underscores(text);
    if (cleaned.empty()) {
        return false;
    }
    if (parse_real_impl(cleaned, out)) {
        return true;
    }

    // Python requires at least one digit (aside from the inf/nan spellings the
    // first attempt already accepted).  This matters: the reference relies on
    // `float(".")` failing so that `.` in a parameter list stays a symbol.
    if (cleaned.find_first_of("0123456789") == std::string::npos) {
        return false;
    }

    // Python accepts a bare leading or trailing dot (".5", "5."), which the
    // from_chars grammar rejects.  Patch in the missing zero and retry.
    if (cleaned.find('.') != std::string::npos) {
        std::string patched = cleaned;
        if (patched.front() == '.') {
            patched.insert(0, "0");
        } else if (patched.size() > 1 && (patched.front() == '+' ||
                                          patched.front() == '-') &&
                   patched[1] == '.') {
            patched.insert(1, "0");
        }
        if (patched.back() == '.') {
            patched.push_back('0');
        }
        if (patched != cleaned && parse_real_impl(patched, out)) {
            return true;
        }
    }
    return false;
}

Value atom(Arena& arena, std::string_view token) {
    if (token == "#t" || token == "#true") {
        return boolean_value(true);
    }
    if (token == "#f" || token == "#false") {
        return boolean_value(false);
    }
    if (token.size() > 1 && token.front() == '"' && token.back() == '"') {
        return string_value(
            arena.make_string(token.substr(1, token.size() - 2)));
    }
    if (token.size() > 1 && token[0] == '#' && token[1] == '\\') {
        // Characters are plain strings in the reference, not a distinct type.
        const std::string_view body = token.substr(2);
        if (const std::string lowered = lower_ascii(body); lowered == "space") {
            return string_value(arena.make_string(" "));
        } else if (lowered == "newline") {
            return string_value(arena.make_string("\n"));
        } else if (lowered == "tab") {
            return string_value(arena.make_string("\t"));
        }
        return string_value(arena.make_string(body));
    }

    if (std::int64_t number = 0; parse_integer_token(token, number)) {
        return integer_value(number);
    }
    if (double real = 0.0; parse_real_token(token, real)) {
        return real_value(real);
    }
    return symbol_value(arena.intern(token));
}

std::vector<Value> parse_program(Arena& arena, std::string_view source,
                                 SourceMap* source_map) {
    const std::vector<SourceToken> tokens = tokenize_with_lines(source);

    std::vector<Frame> stack;
    std::vector<Value> forms;
    Value parts[2];

    const auto record = [source_map](const Value& form, LineSpan span) {
        if (source_map == nullptr || form.type != ValueType::Pair) {
            return;
        }
        // Every pair on the spine, not just the head.  A list is a chain of
        // pairs, and a `cdr`-derived sublist is one of those nodes: a
        // procedure's body is exactly that, so recording only the head would
        // leave a perfectly ordinary form with no position at all.
        for (const Pair* node = form.pair; node != nullptr;) {
            source_map->record(node, span);
            const Value& rest = node->cdr;
            node = rest.type == ValueType::Pair ? rest.pair : nullptr;
        }
    };

    // Completes a form, then closes any quote frames stacked above it.  Written
    // as a loop rather than a recursive call so `'''x` cannot grow the stack.
    const auto deliver = [&](Value value, std::uint32_t start_line,
                             std::uint32_t end_line) {
        record(value, LineSpan{start_line, end_line});
        while (true) {
            if (stack.empty()) {
                forms.push_back(value);
                return;
            }
            Frame& top = stack.back();
            if (top.kind == Frame::Kind::List) {
                top.items.push_back(value);
                return;
            }
            // A quote frame completes: `'x` becomes `(quote x)`, spanning from
            // the quote mark to the end of what it wraps.
            const std::uint32_t tag_line = top.start_line;
            Symbol* tag = top.tag;
            stack.pop_back();
            parts[0] = symbol_value(tag);
            parts[1] = value;
            value = pair_value(arena.list(parts, nil_value()));
            record(value, LineSpan{tag_line, end_line});
        }
    };

    for (const SourceToken& token : tokens) {
        const std::string& text = token.text;
        if (text == "(") {
            stack.push_back(
                Frame{Frame::Kind::List, {}, nullptr, token.line});
            continue;
        }
        if (text == ")") {
            if (stack.empty() || stack.back().kind != Frame::Kind::List) {
                throw ReaderError{"unexpected )"};
            }
            Frame frame = std::move(stack.back());
            stack.pop_back();
            if (frame.items.empty()) {
                // `()` is not a pair, so it has no span to record.
                deliver(nil_value(), token.line, token.line);
            } else {
                deliver(pair_value(arena.list(frame.items, nil_value())),
                        frame.start_line, token.line);
            }
            continue;
        }
        if (text == "'" || text == "`" || text == "," || text == ",@") {
            const std::string_view tag = text == "'"    ? "quote"
                                        : text == "`"   ? "quasiquote"
                                        : text == ","   ? "unquote"
                                                        : "unquote-splicing";
            stack.push_back(
                Frame{Frame::Kind::Quote, {}, arena.intern(tag), token.line});
            continue;
        }
        deliver(atom(arena, text), token.line, token.line);
    }

    if (!stack.empty()) {
        // The reference reports an unterminated list from inside `parse_expr`,
        // and a dangling quote from its entry guard.
        if (stack.back().kind == Frame::Kind::List) {
            throw ReaderError{"missing closing )"};
        }
        throw ReaderError{"unexpected end of input"};
    }
    return forms;
}

std::vector<Value> parse_program(Arena& arena, std::string_view source) {
    return parse_program(arena, source, nullptr);
}

Value parse_one(Arena& arena, std::string_view source) {
    std::vector<Value> forms = parse_program(arena, source);
    if (forms.size() != 1) {
        throw ReaderError{"expected exactly one expression"};
    }
    return forms.front();
}

}  // namespace timewarp::scheme
