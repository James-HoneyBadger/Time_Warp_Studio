// SPDX-License-Identifier: MIT
//
// The link and anchor check. See links.hpp for what it is for.

#include "docs/links.hpp"

#include <algorithm>
#include <filesystem>
#include <iostream>
#include <map>
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

/// `^[a-zA-Z][a-zA-Z0-9+.-]*:` -- http, https, mailto and anything else with a
/// scheme. A network check belongs in CI's own step, not in a gate a contributor
/// runs offline, so these are counted as neither checked nor broken.
bool is_external(std::string_view target) {
    if (target.empty()) {
        return false;
    }
    const auto alpha = [](char character) {
        return (character >= 'a' && character <= 'z') || (character >= 'A' && character <= 'Z');
    };
    if (!alpha(target.front())) {
        return false;
    }
    std::size_t at = 1;
    while (at < target.size()) {
        const char character = target[at];
        const bool allowed = alpha(character) || (character >= '0' && character <= '9') ||
                             character == '+' || character == '.' || character == '-';
        if (!allowed) {
            break;
        }
        ++at;
    }
    return at < target.size() && target[at] == ':';
}

/// Every anchor a Markdown file offers, duplicates numbered as GitHub does.
/// A file that cannot be read offers none, which is what the Python returns: the
/// link into it then fails as `no #fragment in file`, which is the honest report.
std::set<std::string> anchors_of(const fs::path& path) {
    std::set<std::string> result;
    const std::optional<std::string> content = read_text(path);
    if (!content) {
        return result;
    }
    std::map<std::string, int> seen;
    bool in_fence = false;
    for (const std::string_view line : split_lines(*content)) {
        if (is_fence(line, true)) {
            in_fence = !in_fence;
            continue;
        }
        if (in_fence) {
            continue;
        }
        const std::optional<std::string_view> text = heading_text(line);
        if (!text) {
            continue;
        }
        const std::string base = slug(*text);
        const int count = seen[base];
        seen[base] = count + 1;
        result.insert(count == 0 ? base : base + "-" + std::to_string(count));
    }
    return result;
}

/// Orders paths the way Python orders `Path` objects: component by component, so
/// `docs/a/b.md` comes before `docs/a.md` because `a` sorts before `a.md`. The
/// order only decides which failure line is printed first, but a reader comparing
/// the two outputs sees it.
bool path_less(const fs::path& left, const fs::path& right) {
    auto left_part = left.begin();
    auto right_part = right.begin();
    while (left_part != left.end() && right_part != right.end()) {
        if (*left_part != *right_part) {
            return *left_part < *right_part;
        }
        ++left_part;
        ++right_part;
    }
    return left_part == left.end() && right_part != right.end();
}

/// The files the gate reads: the ones named, or `docs/**/*.md`, `README.md` and
/// `Examples/CATALOG.md`.
std::vector<fs::path> documents(const fs::path& root, const std::vector<std::string>& named) {
    if (!named.empty()) {
        std::vector<fs::path> chosen;
        chosen.reserve(named.size());
        for (const std::string& name : named) {
            chosen.emplace_back(name);
        }
        return chosen;
    }

    std::vector<fs::path> found;
    std::error_code failure;
    fs::recursive_directory_iterator entry(root / "docs", fs::directory_options::none, failure);
    const fs::recursive_directory_iterator end;
    while (!failure && entry != end) {
        const fs::path path = entry->path();
        if (path.filename().string().ends_with(".md")) {
            found.push_back(path);
        }
        std::error_code step;
        entry.increment(step);
        if (step) {
            break;
        }
    }
    std::sort(found.begin(), found.end(), path_less);

    std::vector<fs::path> result;
    result.reserve(found.size() + 2);
    result.emplace_back(root / "README.md");
    result.emplace_back(root / "Examples" / "CATALOG.md");
    result.insert(result.end(), found.begin(), found.end());
    return result;
}

/// `(document.parent / path).resolve()`, falling back to a lexical normalisation
/// when the path does not exist yet -- which is the case this check reports.
///
/// Python's `Path` drops a trailing separator as it builds the path (`GUIDE.md/`
/// becomes `GUIDE.md`); `std::filesystem` keeps it, and a trailing separator on a
/// regular file makes `exists` false on POSIX. The strip is what keeps the two
/// agreeing on `[x](GUIDE.md/#anchor)`.
fs::path resolve_against(const fs::path& document, std::string_view relative) {
    std::error_code failure;
    std::string text(relative);
    while (text.size() > 1 && (text.back() == '/' || text.back() == '\\')) {
        text.pop_back();
    }
    const fs::path joined = document.parent_path() / fs::path(text);
    const fs::path canonical = fs::weakly_canonical(joined, failure);
    return failure ? joined.lexically_normal() : canonical;
}

}  // namespace

int run_links_command(const timewarp::cli::Invocation& invocation) {
    const fs::path& root = invocation.root;

    std::vector<std::string> failures;
    std::map<fs::path, std::set<std::string>> anchor_cache;
    std::size_t checked = 0;

    for (const fs::path& document : documents(root, invocation.positionals())) {
        std::error_code failure;
        if (!fs::is_regular_file(document, failure)) {
            failures.push_back(document.string() + ": is not a file");
            continue;
        }
        const std::optional<std::string> content = read_text(document);
        if (!content) {
            // The Python lets `read_text` raise here; the verdict is the same --
            // the run fails -- without the traceback.
            failures.push_back(document.string() + ": cannot be read");
            continue;
        }

        int number = 0;
        bool in_fence = false;
        for (const std::string_view line : split_lines(*content)) {
            ++number;
            // A listing is not a link: a fenced block is skipped whole.
            if (is_fence(line, true)) {
                in_fence = !in_fence;
                continue;
            }
            if (in_fence) {
                continue;
            }
            const std::string stripped = without_inline_code(line);
            for (const std::string_view target : link_targets(stripped)) {
                if (is_external(target)) {
                    continue;
                }
                ++checked;

                const std::size_t hash = target.find('#');
                const std::string_view path_text =
                    hash == std::string_view::npos ? target : target.substr(0, hash);
                const std::string_view fragment =
                    hash == std::string_view::npos ? std::string_view()
                                                   : target.substr(hash + 1);

                fs::path destination = document;
                if (!path_text.empty()) {
                    destination = resolve_against(document, path_text);
                    if (!fs::exists(destination, failure)) {
                        failures.push_back(document.string() + ":" + std::to_string(number) +
                                           ": missing " + std::string(path_text));
                        continue;
                    }
                }
                if (fragment.empty() || fs::is_directory(destination, failure)) {
                    continue;
                }
                std::string suffix = destination.extension().string();
                suffix = lower_ascii(suffix);
                if (suffix != ".md") {
                    continue;
                }
                if (anchor_cache.find(destination) == anchor_cache.end()) {
                    anchor_cache.emplace(destination, anchors_of(destination));
                }
                if (!anchor_cache[destination].contains(std::string(fragment))) {
                    failures.push_back(document.string() + ":" + std::to_string(number) +
                                       ": no #" + std::string(fragment) + " in " +
                                       destination.filename().string());
                }
            }
        }
    }

    for (const std::string& failure : failures) {
        std::cout << "❌ " << failure << '\n';
    }
    if (!failures.empty()) {
        std::cout << "doc links: " << failures.size() << " broken of " << checked << " checked\n";
        return 1;
    }
    if (!invocation.quiet) {
        std::cout << "doc links: " << checked << " checked, all resolve\n";
    }
    return 0;
}

}  // namespace docs
