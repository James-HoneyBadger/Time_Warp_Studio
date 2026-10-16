// SPDX-License-Identifier: MIT
//
// Source positions for the debugging surface.
//
// The Scheme port is a faithful reproduction of an interpreter that carried no
// locations at all, and that was fine while everything ran to completion.  A
// debugger cannot work that way: `pause_at_line`, breakpoints and "where am I"
// all need to know which source text is in flight.
//
// ---------------------------------------------------------------------------
// Why a side table keyed by pointer
// ---------------------------------------------------------------------------
//
// `Value` is a 16-byte tagged union chosen for copying cheaply, and a `Pair` is
// two `Value`s with nothing spare.  Widening either to carry a span would tax
// every value in every program to serve a feature almost no run uses.
//
// Instead the reader records spans in a table keyed by the `Pair*`.  That key is
// stable for the life of the program because every pair is arena-allocated and
// the arena only frees wholesale.  A run with no debugger attached never
// consults the table, so the cost is confined to the phase where it is useful.
//
// Only compound forms are recorded.  A bare atom in code position has no
// sub-structure to step into, and it is allocated as a plain `Value` rather than
// a `Pair`, so it could not be keyed even if it were worth recording.

#pragma once

#include <cstddef>
#include <cstdint>
#include <unordered_map>

#include "timewarp/scheme/value.hpp"

namespace timewarp::scheme {

/// A 1-based, inclusive range of source lines.
///
/// `first_line == 0` means "unknown", which is what a synthetically constructed
/// form reports.
struct LineSpan {
    std::uint32_t first_line = 0;
    std::uint32_t last_line = 0;

    [[nodiscard]] constexpr bool known() const noexcept {
        return first_line != 0;
    }
    /// True when `line` falls inside the span.  An unknown span matches nothing.
    [[nodiscard]] constexpr bool contains(std::uint32_t line) const noexcept {
        return known() && line >= first_line && line <= last_line;
    }

    friend constexpr bool operator==(LineSpan, LineSpan) noexcept = default;
};

/// Maps reader-produced forms to the source they came from.
class SourceMap {
public:
    /// Records the span of a compound form.  Later records win, which matters
    /// only if a caller re-parses over an existing map.
    void record(const Pair* form, LineSpan span) { spans_[form] = span; }

    /// The span of a form, or an unknown span when it was not recorded.
    [[nodiscard]] LineSpan lookup(const Pair* form) const noexcept {
        if (form == nullptr) {
            return {};
        }
        const auto found = spans_.find(form);
        return found == spans_.end() ? LineSpan{} : found->second;
    }

    /// Convenience overload; a non-pair value has no recorded span.
    [[nodiscard]] LineSpan lookup(const Value& form) const noexcept {
        return form.type == ValueType::Pair ? lookup(form.pair) : LineSpan{};
    }

    [[nodiscard]] std::size_t size() const noexcept { return spans_.size(); }
    void clear() { spans_.clear(); }

private:
    std::unordered_map<const Pair*, LineSpan> spans_;
};

}  // namespace timewarp::scheme
