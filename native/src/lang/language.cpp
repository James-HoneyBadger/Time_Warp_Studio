// SPDX-License-Identifier: MIT

#include "timewarp/lang/language.hpp"

#include "timewarp/lang/basic.hpp"
#include "timewarp/lang/chip8.hpp"
#include "timewarp/lang/comal.hpp"
#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/focal.hpp"
#include "timewarp/lang/forth.hpp"
#include "timewarp/lang/logo.hpp"
#include "timewarp/lang/pilot.hpp"
#include "timewarp/lang/apl.hpp"
#include "timewarp/lang/rexx.hpp"
#include "timewarp/lang/smalltalk.hpp"

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
        case Language::Forth:
            return "Forth";
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
        case Language::Forth:
            return "forth";
    }
    return "?";
}

std::vector<Language> all_languages() {
    return {Language::Basic, Language::Pilot,  Language::Logo,  Language::Comal,
            Language::Rexx,  Language::Scheme, Language::Focal, Language::Chip8,
            Language::Apl,   Language::Smalltalk, Language::Forth};
}

std::optional<Language> language_from_key(std::string_view name) noexcept {
    std::string lower{name};
    for (char& character : lower) {
        character = static_cast<char>(std::tolower(static_cast<unsigned char>(character)));
    }
    for (const Language language : all_languages()) {
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
        case Language::Forth:
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
        case Language::Smalltalk:
            return &execute_smalltalk;
        case Language::Forth:
            return &execute_forth;
        // Line-by-line executors.
        case Language::Basic:
            return &execute_basic;
        case Language::Focal:
            return &execute_focal;
        case Language::Pilot:
            return &execute_pilot;
        case Language::Comal:
            return &execute_comal;
        case Language::Apl:
            return &execute_apl;
        case Language::Rexx:
            return &execute_rexx;

        // Scheme is not an executor in this table: it is the separate explicit-stack
        // machine in `timewarp_scheme`, which the IDE drives through its debugger
        // runner, `time_warp_check` runs directly, and the API's REPL holds an
        // instance of. A null here is how a caller tells the two apart.
        case Language::Scheme:
            return nullptr;
    }
    return nullptr;
}

}  // namespace timewarp::lang
