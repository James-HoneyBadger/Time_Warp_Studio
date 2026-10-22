// SPDX-License-Identifier: MIT
//
// The eleven languages, in one place.
//
// This table is the only place a language's display name, protocol key or file
// extension is written down. It used to be two tables -- one in
// `src/ui/language.cpp` with a display name, a key and two extensions, and one
// implied by the switches in `src/lang/language.cpp` -- and CHIP-8's `.ch8`/`.c8`
// were spelled out in four files besides. Two copies of one fact drift, and the
// copies that drift are the ones a reader trusts.
//
// Header-only and Qt-free on purpose: the language executors, the Qt shell, the
// command-line tools and the gates all ask the same table, and none of them should
// gain a link dependency to do it.
//
// `Language` (in `language.hpp`) is the identity every other part of the program
// uses. The UI's own enum in `ui/language.hpp` carries one extra member, `Unknown`,
// so that "this file is not one of ours" has somewhere to go; it maps by position,
// which the `static_assert` below holds to the table's size.

#pragma once

#include "timewarp/lang/language.hpp"

#include <array>
#include <cstddef>
#include <optional>
#include <string_view>

namespace timewarp::lang {

/// One row: everything the program knows about a language that is not behaviour.
struct LanguageRow {
    /// The identity, matching the `Language` enumerator.
    Language language;
    /// The name a person reads: "BASIC", "CHIP-8", "Smalltalk".
    std::string_view display;
    /// The protocol and settings key: lowercase, no punctuation.
    std::string_view key;
    /// The extensions that select it, most common first. Unused slots are empty.
    std::array<std::string_view, 3> extensions;
    /// A language whose programs are run whole rather than line by line.
    bool whole_program;
};

/// The table, in `Language` order, so a row's index is its enumerator's value.
inline constexpr std::array<LanguageRow, 11> kLanguageTable{{
    {Language::Basic, "BASIC", "basic", {"bas", "bsc"}, false},
    {Language::Pilot, "PILOT", "pilot", {"pilot", "plt"}, false},
    {Language::Logo, "Logo", "logo", {"logo", "lg"}, true},
    {Language::Comal, "COMAL", "comal", {"comal", "cml"}, false},
    {Language::Rexx, "REXX", "rexx", {"rexx", "rex"}, false},
    {Language::Scheme, "Scheme", "scheme", {"scm", "ss", "scheme"}, true},
    {Language::Focal, "FOCAL", "focal", {"foc", "focal"}, false},
    {Language::Chip8, "CHIP-8", "chip8", {"ch8", "c8"}, true},
    {Language::Apl, "APL", "apl", {"apl", "dyalog"}, false},
    {Language::Smalltalk, "Smalltalk", "smalltalk", {"st", "smalltalk"}, true},
    {Language::Forth, "Forth", "forth", {"fs", "fth"}, true},
}};

static_assert(kLanguageTable.size() == 11,
              "the table has one row per Language, in enumerator order");
static_assert(kLanguageTable.front().language == Language::Basic &&
                  kLanguageTable.back().language == Language::Forth,
              "the table is in Language order, so a row can be found by index");

/// The row for *language*, or null for a value outside the table.
[[nodiscard]] inline const LanguageRow* language_row(Language language) noexcept {
    const auto index = static_cast<std::size_t>(language);
    if (index >= kLanguageTable.size() || kLanguageTable[index].language != language) {
        return nullptr;
    }
    return &kLanguageTable[index];
}

/// The row whose key is *name* (case-sensitive: keys are lowercase).
[[nodiscard]] inline const LanguageRow* language_row_for_key(std::string_view name) noexcept {
    for (const LanguageRow& row : kLanguageTable) {
        if (row.key == name) {
            return &row;
        }
    }
    return nullptr;
}

/// The row whose extension list holds *extension*, given without a dot and in
/// lower case.
[[nodiscard]] inline const LanguageRow* language_row_for_extension(
    std::string_view extension) noexcept {
    for (const LanguageRow& row : kLanguageTable) {
        for (const std::string_view candidate : row.extensions) {
            if (!candidate.empty() && candidate == extension) {
                return &row;
            }
        }
    }
    return nullptr;
}

}  // namespace timewarp::lang
