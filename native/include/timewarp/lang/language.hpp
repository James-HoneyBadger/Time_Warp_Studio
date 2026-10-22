// SPDX-License-Identifier: MIT
//
// The language set, and which execution shape each one has.

#pragma once

#include <optional>
#include <span>
#include <string_view>
#include <vector>

namespace timewarp::lang {

enum class Language {
    Basic,
    Pilot,
    Logo,
    Comal,
    Rexx,
    Scheme,
    Focal,
    Chip8,
    Apl,
    Smalltalk,
    Forth,
};

/// The display name, spelled exactly as the IDE and the lessons spell it.
/// `Chip8` is the identifier; `CHIP-8` is the name a person reads, and using one
/// where the other belongs puts a wrong name in the one place it is written.
[[nodiscard]] std::string_view display_name(Language language) noexcept;

/// The protocol/settings key: lowercase, no punctuation ("chip8").
[[nodiscard]] std::string_view key(Language language) noexcept;

/// The file extensions that select this language, most common first, without dots
/// and in lower case. Empty for a value outside the table.
///
/// This is the registry's list, not a second one: `registry.hpp` holds the row and
/// this reads it, so `ui::extensions` and the Qt shell cannot disagree with the
/// executors about what a `.ch8` file is.
[[nodiscard]] std::span<const std::string_view> extensions(Language language) noexcept;

/// The inverse of `key`, case-insensitively. Empty when the key is unknown.
[[nodiscard]] std::optional<Language> language_from_key(std::string_view key) noexcept;

/// Every language the enum defines, in display order.
///
/// The parity harness asks the built CLI which of these have a native
/// executor, so a port is registered in exactly one place (`executor_for`)
/// rather than in a list on each side.
[[nodiscard]] std::vector<Language> all_languages();

/// A whole-program language receives the complete source and runs it to
/// completion inside one executor call. A line-by-line language is called once
/// per statement by the interpreter's main loop.
///
/// Scheme is whole-program too, but its implementation is the separate native
/// `timewarp_scheme` machine rather than a `timewarp_lang` executor.
[[nodiscard]] bool is_whole_program(Language language) noexcept;

}  // namespace timewarp::lang
