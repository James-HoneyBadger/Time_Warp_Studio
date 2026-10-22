// SPDX-License-Identifier: MIT

#include "timewarp/lang/language.hpp"

#include "timewarp/lang/registry.hpp"

#include <span>

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
    const LanguageRow* row = language_row(language);
    return row != nullptr ? row->display : std::string_view{};
}

std::string_view key(Language language) noexcept {
    const LanguageRow* row = language_row(language);
    return row != nullptr ? row->key : std::string_view{};
}

std::optional<Language> language_from_key(std::string_view name) noexcept {
    // Keys are lowercase in the table; the caller's spelling is not always, because
    // a protocol payload may say "BASIC". Comparing in one case is what makes
    // `--language BASIC` and `--language basic` the same request.
    std::string lower;
    lower.reserve(name.size());
    for (const char character : name) {
        lower.push_back(static_cast<char>(
            std::tolower(static_cast<unsigned char>(character))));
    }
    const LanguageRow* row = language_row_for_key(lower);
    if (row == nullptr) {
        return std::nullopt;
    }
    return row->language;
}

std::vector<Language> all_languages() {
    std::vector<Language> languages;
    languages.reserve(kLanguageTable.size());
    for (const LanguageRow& row : kLanguageTable) {
        languages.push_back(row.language);
    }
    return languages;
}

std::span<const std::string_view> extensions(Language language) noexcept {
    const LanguageRow* row = language_row(language);
    if (row == nullptr) {
        return {};
    }
    std::size_t count = 0;
    for (const std::string_view candidate : row->extensions) {
        if (!candidate.empty()) {
            ++count;
        }
    }
    return std::span<const std::string_view>{row->extensions.data(), count};
}

bool is_whole_program(Language language) noexcept {
    const LanguageRow* row = language_row(language);
    return row != nullptr && row->whole_program;
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
