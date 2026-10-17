// SPDX-License-Identifier: MIT

#include "timewarp/lang/language.hpp"

#include "timewarp/lang/chip8.hpp"
#include "timewarp/lang/comal.hpp"
#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/focal.hpp"
#include "timewarp/lang/logo.hpp"
#include "timewarp/lang/pilot.hpp"
#include "timewarp/lang/apl.hpp"

#include <cctype>
#include <string>

namespace timewarp::lang {

std::string_view display_name(Language language) noexcept {
    switch (language) {
        case Language::Basic:
            return "BASIC";
        case Language::Pilot:
            return "PILOT";
        case Language::Logo:
            return "Logo";
        case Language::Comal:
            return "COMAL";
        case Language::Rexx:
            return "REXX";
        case Language::Scheme:
            return "Scheme";
        case Language::Focal:
            return "FOCAL";
        case Language::Chip8:
            return "CHIP-8";
        case Language::Apl:
            return "APL";
        case Language::Smalltalk:
            return "Smalltalk";
    }
    return "?";
}

std::string_view key(Language language) noexcept {
    switch (language) {
        case Language::Basic:
            return "basic";
        case Language::Pilot:
            return "pilot";
        case Language::Logo:
            return "logo";
        case Language::Comal:
            return "comal";
        case Language::Rexx:
            return "rexx";
        case Language::Scheme:
            return "scheme";
        case Language::Focal:
            return "focal";
        case Language::Chip8:
            return "chip8";
        case Language::Apl:
            return "apl";
        case Language::Smalltalk:
            return "smalltalk";
    }
    return "?";
}

std::optional<Language> language_from_key(std::string_view name) noexcept {
    std::string lower{name};
    for (char& character : lower) {
        character = static_cast<char>(std::tolower(static_cast<unsigned char>(character)));
    }
    for (const Language language :
         {Language::Basic, Language::Pilot, Language::Logo, Language::Comal,
          Language::Rexx, Language::Scheme, Language::Focal, Language::Chip8,
          Language::Apl, Language::Smalltalk}) {
        if (lower == key(language)) {
            return language;
        }
    }
    return std::nullopt;
}

bool is_whole_program(Language language) noexcept {
    switch (language) {
        case Language::Scheme:
        case Language::Chip8:
        case Language::Logo:
        case Language::Smalltalk:
            return true;
        case Language::Basic:
        case Language::Pilot:
        case Language::Comal:
        case Language::Rexx:
        case Language::Focal:
        case Language::Apl:
            return false;
    }
    return false;
}

Executor executor_for(Language language) noexcept {
    switch (language) {
        // Whole-program executors.
        case Language::Chip8:
            return &execute_chip8;
        case Language::Logo:
            return &execute_logo;
        // Line-by-line executors.
        case Language::Focal:
            return &execute_focal;
        case Language::Pilot:
            return &execute_pilot;
        case Language::Comal:
            return &execute_comal;
        case Language::Apl:
            return &execute_apl;

        // Still driven by the Python bridge. Returning null is how the caller
        // asks "is this one ported yet?" without an exception.
        case Language::Basic:
        case Language::Rexx:
        case Language::Smalltalk:
        case Language::Scheme:
            return nullptr;
    }
    return nullptr;
}

}  // namespace timewarp::lang
