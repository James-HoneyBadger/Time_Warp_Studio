// SPDX-License-Identifier: MIT
//
// `native/share/counts.txt` and the gate that reads it. See counts.hpp for why it
// exists; the data file documents the row format and the two halves of the check.

#include "docs/counts.hpp"

#include "docs/text.hpp"

#include <filesystem>
#include <iostream>
#include <set>
#include <string>
#include <string_view>
#include <vector>

namespace docs {

namespace fs = std::filesystem;

namespace {

/// `text` without leading or trailing spaces or tabs.
std::string trimmed(std::string_view text) {
    std::size_t start = 0;
    std::size_t end = text.size();
    const auto space = [](char character) { return character == ' ' || character == '\t'; };
    while (start < end && space(text[start])) {
        ++start;
    }
    while (end > start && space(text[end - 1])) {
        --end;
    }
    return std::string{text.substr(start, end - start)};
}

/// `text` split on `separator`, keeping empty pieces so a missing column is a row
/// to report rather than one to skip.
std::vector<std::string> split_on(std::string_view text, char separator) {
    std::vector<std::string> parts;
    std::size_t start = 0;
    while (true) {
        const std::size_t at = text.find(separator, start);
        if (at == std::string_view::npos) {
            parts.emplace_back(text.substr(start));
            return parts;
        }
        parts.emplace_back(text.substr(start, at - start));
        start = at + 1;
    }
}

/// Whether a page still states a count with the exact phrase its row pins. Exact
/// on purpose: the row is the one place the wording lives, so a reword is a change
/// to the row, not a silent drift a loose match would hide.
bool page_states(std::string_view page_text, std::string_view phrase) {
    return page_text.find(phrase) != std::string_view::npos;
}

/// The extensions the ten languages use. Kept beside the registry rather than read
/// from it: `registry.hpp` is C++ with include directives, and a scanner that
/// parsed it would be a second, brittle definition of the language list.
const std::set<std::string>& example_extensions() {
    static const std::set<std::string> extensions{
        ".bas", ".logo", ".scm", ".st",   ".fs",
        ".apl", ".rexx", ".pilot", ".comal", ".foc"};
    return extensions;
}

/// The number of example programs on disk: a regular file under `Examples/` whose
/// extension is one of the ten languages'. This is the count the row labelled
/// `examples` pins, so the gate can prove it rather than trust it.
std::size_t count_example_programs(const fs::path& root) {
    std::size_t count = 0;
    std::error_code error;
    const fs::recursive_directory_iterator end;
    for (fs::recursive_directory_iterator it{root / "Examples", error}; it != end && !error;
         it.increment(error)) {
        if (it->is_regular_file() &&
            example_extensions().contains(it->path().extension().string())) {
            ++count;
        }
    }
    return count;
}

/// The rows this tool can prove against the tree: the label and the number the
/// tree must agree with. Everything else is checked by the gate that prints it --
/// the suites by ctest, the corpus by `golden-check`, the command rows by
/// `time_warp_gates commands` -- which is why a change that moves one of them
/// edits counts.txt in the same commit, like unbuilt.txt.
bool derived_count(const std::string& label, const fs::path& root,
                   std::size_t& actual) {
    if (label == "examples") {
        actual = count_example_programs(root);
        return true;
    }
    return false;
}

}  // namespace

std::size_t check_counts(const fs::path& root) {
    const std::optional<std::string> content = read_text(root / "native" / "share" /
                                                         "counts.txt");
    if (!content) {
        std::cout << "native/share/counts.txt: missing; the guides' counts cannot be checked\n";
        return 1;
    }

    std::size_t failures = 0;
    for (const std::string_view raw : split_lines(*content)) {
        const std::string line = trimmed(raw);
        if (line.empty() || line.front() == '#') {
            continue;
        }
        const std::vector<std::string> parts = split_on(line, '|');
        if (parts.size() != 3) {
            std::cout << "native/share/counts.txt: a row needs label | value | pages: "
                      << line << '\n';
            ++failures;
            continue;
        }
        const std::string label = trimmed(parts.at(0));
        const std::string value = trimmed(parts.at(1));

        std::size_t derived = 0;
        if (derived_count(label, root, derived) &&
            std::to_string(derived) != value) {
            std::cout << "native/share/counts.txt: '" << label << "' is pinned at " << value
                      << ", but the tree holds " << derived << '\n';
            ++failures;
        }

        for (const std::string& entry : split_on(parts.at(2), ';')) {
            const std::string statement = trimmed(entry);
            if (statement.empty()) {
                continue;
            }
            const std::size_t colon = statement.find(':');
            if (colon == std::string::npos) {
                std::cout << "native/share/counts.txt: '" << label
                          << "' names a page without its phrase: " << statement << '\n';
                ++failures;
                continue;
            }
            const std::string path = trimmed(statement.substr(0, colon));
            const std::string phrase = trimmed(statement.substr(colon + 1));
            const std::optional<std::string> text = read_text(root / path);
            if (!text) {
                std::cout << "native/share/counts.txt: '" << label << "' names " << path
                          << ", which is not readable\n";
                ++failures;
                continue;
            }
            if (!page_states(*text, phrase)) {
                std::cout << path << ": the count row for '" << label << "' expects \""
                          << phrase << "\", and the page does not say it\n";
                ++failures;
            }
        }
    }
    return failures;
}

int counts_self_test() {
    struct Case {
        const char* name;
        std::string_view page;
        std::string_view phrase;
        bool expected;
    };
    static const Case kCases[] = {
        {"a page that states the count", "There are 23 suites, one binary each",
         "23 suites", true},
        {"a page that reworded the count", "There are 22 suites, one binary each",
         "23 suites", false},
        {"a page that dropped the count", "Suites are one binary each", "23 suites",
         false},
        {"a table row with several counts",
         "| Quality gates | 23 CTest suites ... 636 programs; 534 command rows |",
         "534 command rows", true},
        {"a phrase split across a reword",
         "The frozen corpus holds six hundred and thirty-six programs", "636 programs",
         false},
    };

    int failures = 0;
    for (const Case& test : kCases) {
        const bool found = page_states(test.page, test.phrase);
        if (found != test.expected) {
            ++failures;
            std::cout << "  counts self-test: " << test.name << ": expected "
                      << (test.expected ? "a match" : "no match") << '\n';
        }
    }
    if (failures != 0) {
        std::cerr << "  " << failures << " counts self-test failure(s)\n";
        return 1;
    }
    std::cout << "counts self-test: " << std::size(kCases) << " cases\n";
    return 0;
}

}  // namespace docs
