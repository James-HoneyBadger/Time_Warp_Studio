// SPDX-License-Identifier: MIT
//
// The version copies check. See version.hpp for what it is for.

#include "docs/version.hpp"

#include <cstddef>
#include <filesystem>
#include <iostream>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <system_error>
#include <vector>

#include "timewarp/tools/cli.hpp"
#include "docs/text.hpp"

namespace docs {
namespace {

namespace fs = std::filesystem;

/// A hand-written copy of the version, which this command can rewrite.
struct Copy {
    fs::path path;
    std::string description;
    /// `Version-<digits>-orange`, the README's only version marker: its badge.
    bool badge = true;
};

/// Something a release must have that no script should write for you.
struct Requirement {
    fs::path path;
    std::string description;
};

std::string relative_to(const fs::path& path, const fs::path& root) {
    const fs::path relative = path.lexically_relative(root);
    if (relative.empty()) {
        return path.string();
    }
    return relative.string();
}

std::optional<std::string> read_version(const fs::path& file, std::string& error) {
    const std::optional<std::string> content = read_text(file);
    if (!content) {
        error = "cannot read " + file.string();
        return std::nullopt;
    }
    std::string version(*content);
    const std::string_view trimmed = trim(version);
    const auto digit = [](char character) { return character >= '0' && character <= '9'; };

    std::size_t at = 0;
    int groups = 0;
    while (at < trimmed.size()) {
        const std::size_t start = at;
        while (at < trimmed.size() && digit(trimmed[at])) {
            ++at;
        }
        if (at == start) {
            break;
        }
        ++groups;
        if (at < trimmed.size() && trimmed[at] == '.') {
            ++at;
            continue;
        }
        break;
    }
    if (at != trimmed.size() || groups != 3) {
        error = "Invalid VERSION value: '" + std::string(trimmed) + "'";
        return std::nullopt;
    }
    return std::string(trimmed);
}

std::vector<Copy> copies_of(const fs::path& root) {
    return {
        // The README's only version marker is the badge. It used to end with a
        // "- **Version:**" line as well, and tracked that too, but the line was
        // dropped when the page was shortened -- which is what the "no match"
        // error below is for: a copy target that outlives its text stops the
        // release rather than passing unnoticed.
        {root / "README.md", "README version badge", true},
        {root / "docs" / "ARCHITECTURE.md", "architecture guide title", false},
    };
}

std::vector<Requirement> requirements_of(const fs::path& root, std::string_view version) {
    return {{root / "CHANGELOG.md",
             "a changelog section headed '## [" + std::string(version) + "] - <date>'"}};
}

/// The copy's new text, or the same text when it already agrees. `badge` selects
/// one of the two hand-written patterns; only the first match is replaced, as
/// `re.subn(count=1)` did.
std::optional<std::string> updated_text(const Copy& copy, const std::string& content,
                                        std::string_view version, const fs::path& root,
                                        std::string& error) {
    const auto no_match = [&]() {
        error = "no match for " + copy.description + " in " + relative_to(copy.path, root) +
                ": the file has changed shape";
        return std::optional<std::string>{};
    };

    if (copy.badge) {
        constexpr std::string_view kPrefix = "Version-";
        constexpr std::string_view kSuffix = "-orange";
        for (std::size_t at = content.find(kPrefix); at != std::string::npos;
             at = content.find(kPrefix, at + 1)) {
            std::size_t end = at + kPrefix.size();
            const std::size_t digits = end;
            while (end < content.size() && ((content[end] >= '0' && content[end] <= '9') ||
                                            content[end] == '.')) {
                ++end;
            }
            if (end == digits || content.compare(end, kSuffix.size(), kSuffix) != 0) {
                continue;
            }
            std::string updated(content.substr(0, at));
            updated += kPrefix;
            updated += version;
            updated += kSuffix;
            updated += content.substr(end + kSuffix.size());
            return updated;
        }
        return no_match();
    }

    // `(?m)^## Time Warp Studio v[0-9.]+ ` -- the architecture guide's title, which
    // is the one heading that states the version.
    constexpr std::string_view kTitle = "## Time Warp Studio v";
    for (std::size_t at = content.find(kTitle); at != std::string::npos;
         at = content.find(kTitle, at + 1)) {
        if (at != 0 && content[at - 1] != '\n') {
            continue;
        }
        std::size_t end = at + kTitle.size();
        const std::size_t digits = end;
        while (end < content.size() && ((content[end] >= '0' && content[end] <= '9') ||
                                        content[end] == '.')) {
            ++end;
        }
        if (end == digits || end >= content.size() || content[end] != ' ') {
            continue;
        }
        std::string updated(content.substr(0, at));
        updated += kTitle;
        updated += version;
        updated.append(content, end, std::string::npos);
        return updated;
    }
    return no_match();
}

bool has_changelog_section(std::string_view content, std::string_view version) {
    const std::string heading = "## [" + std::string(version) + "] - ";
    for (std::size_t at = content.find(heading); at != std::string_view::npos;
         at = content.find(heading, at + 1)) {
        if (at == 0 || content[at - 1] == '\n') {
            return true;
        }
    }
    return false;
}

}  // namespace

int run_version_command(const timewarp::cli::Invocation& invocation) {
    if (!invocation.positionals({"--release-date"}).empty()) {
        std::cerr << "time_warp_docs: version takes no files\n";
        return 2;
    }
    const fs::path& root = invocation.root;

    std::string error;
    std::optional<std::string> version = read_version(root / "VERSION", error);
    if (!version) {
        std::cerr << "❌ " << error << '\n';
        return 2;
    }

    struct Update {
        Copy copy;
        std::string content;
    };

    const std::vector<Copy> copies = copies_of(root);

    // Read and match everything before writing anything. Writing as each target was
    // reached left a half-applied bump behind when a target went missing.
    std::vector<Update> updates;
    for (const Copy& copy : copies) {
        const std::optional<std::string> content = read_text(copy.path);
        if (!content) {
            std::cerr << "❌ cannot read " << relative_to(copy.path, root) << '\n';
            return 2;
        }
        const std::optional<std::string> updated =
            updated_text(copy, *content, *version, root, error);
        if (!updated) {
            std::cerr << "❌ " << error << '\n';
            return 2;
        }
        if (*updated != *content) {
            updates.push_back({copy, *content});
        }
    }

    const bool write = invocation.has("--write");
    if (write) {
        // Editing each file's current text in order: a file that carries two copies
        // would otherwise come out with one of them updated. (The two copies live in
        // different files today; the loop is here because the Python's was.)
        std::set<fs::path> written;
        for (const Update& update : updates) {
            if (!written.insert(update.copy.path).second) {
                continue;
            }
            const std::optional<std::string> current = read_text(update.copy.path);
            if (!current) {
                std::cerr << "❌ cannot read " << relative_to(update.copy.path, root) << '\n';
                return 2;
            }
            std::string content = *current;
            for (const Update& other : updates) {
                if (other.copy.path != update.copy.path) {
                    continue;
                }
                const std::optional<std::string> next =
                    updated_text(other.copy, content, *version, root, error);
                if (!next) {
                    std::cerr << "❌ " << error << '\n';
                    return 2;
                }
                content = *next;
            }
            if (!write_text(update.copy.path, content)) {
                std::cerr << "❌ cannot write " << relative_to(update.copy.path, root) << '\n';
                return 2;
            }
        }
    }

    if (!updates.empty()) {
        // The emoji reports whether a person has something left to do. After a
        // successful --write there is nothing outstanding, so it reads as a
        // success; without the flag the same state is a failure, and does.
        if (write) {
            std::cout << "✅ Brought " << updates.size() << " copy/copies up to VERSION ("
                      << *version << ").\n";
        } else {
            std::cout << "❌ Stale: " << updates.size() << " copy/copies disagree with VERSION ("
                      << *version << ").\n";
        }
        for (const Update& update : updates) {
            std::cout << "   - " << update.copy.description << " ("
                      << relative_to(update.copy.path, root) << ")\n";
        }
    } else {
        std::cout << "✅ All " << copies.size() << " hand-written copies match " << *version
                  << ".\n";
    }

    std::size_t missing = 0;
    for (const Requirement& requirement : requirements_of(root, *version)) {
        const std::optional<std::string> content = read_text(requirement.path);
        if (!content) {
            std::cerr << "❌ cannot read " << relative_to(requirement.path, root) << '\n';
            ++missing;
            continue;
        }
        if (!has_changelog_section(*content, *version)) {
            std::cerr << "❌ Release paperwork missing: " << requirement.description << '\n';
            ++missing;
        }
    }

    if (missing != 0) {
        return 1;
    }
    if (!updates.empty() && !write) {
        return 1;
    }
    return 0;
}

}  // namespace docs
