// SPDX-License-Identifier: MIT
//
// The claims check. See claims.hpp for the three rules and why there are three.

#include "docs/claims.hpp"

#include <cstddef>
#include <filesystem>
#include <iostream>
#include <iterator>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <system_error>
#include <vector>

#include "docs/cli.hpp"
#include "docs/pipes.hpp"
#include "docs/text.hpp"

namespace docs {
namespace {

namespace fs = std::filesystem;

/// Names that no longer exist in this repository. A file may say they were
/// deleted; it may not use them in the present tense. Adding to this list is the
/// whole maintenance cost of the rule.
constexpr const char* kRetired[] = {
    "PythonWorker", "locateWorker",      "timewarp_bridge", "python_bridge_tests",
    "worker.py",    "TIME_WARP_WORKER",  "TIME_WARP_PYTHON",
};

/// Verbs that make a mention a record rather than a claim. Deliberately small and
/// matched on the same line: "The worker classes (...) are deleted" passes,
/// "`python_bridge_tests` runs a program through the real worker" does not.
///
/// The Python pattern spells the copula out -- `\b(was|were|is|are)?\s*(deleted|...)`
/// -- and the group is optional with `\s*` after it, so any occurrence of one of
/// these verbs between word boundaries matches on its own. What is left is the verb
/// list plus the phrases below.
constexpr const char* kPastTense[] = {
    "deleted", "removed", "gone", "retired", "dropped", "replaced", "went",
    "used to", "no longer", "once", "had been", "no part of",
};

/// A region a document marks as history. `CHANGELOG.md` and the porting plan are
/// wholly historical, and say so at the top.
constexpr std::string_view kHistoryOpen = "<!-- history -->";
constexpr std::string_view kHistoryClose = "<!-- /history -->";

/// A line that names a path on purpose as an example -- "create
/// `native/bridge/.../my_lang.py`" in a procedure -- says so, rather than the gate
/// learning a list of placeholder words and missing the eleventh.
constexpr std::string_view kIgnore = "<!-- doc-claims: ignore -->";

/// A file that is a record from top to bottom. `CHANGELOG.md` cannot describe a
/// removal without naming what was removed, so the alternative to this marker is a
/// gate that reports the changelog forever.
constexpr std::string_view kWholeFile = "<!-- history: all -->";

struct Problem {
    int line = 0;
    std::string message;
};

bool is_word_character(char character) {
    return (character >= 'a' && character <= 'z') || (character >= 'A' && character <= 'Z') ||
           (character >= '0' && character <= '9') || character == '_';
}

/// Whether `phrase` occurs in `lower` with a word boundary at each end, which is
/// what `\b...\b` asks for and what keeps "once" out of "announced".
bool has_phrase(std::string_view lower, std::string_view phrase) {
    for (std::size_t at = lower.find(phrase); at != std::string_view::npos;
         at = lower.find(phrase, at + 1)) {
        const bool left = at == 0 || !is_word_character(lower[at - 1]);
        const std::size_t after = at + phrase.size();
        const bool right = after >= lower.size() || !is_word_character(lower[after]);
        if (left && right) {
            return true;
        }
    }
    return false;
}

bool past_tense(std::string_view line) {
    const std::string lower = lower_ascii(line);
    for (const char* phrase : kPastTense) {
        if (has_phrase(lower, phrase)) {
            return true;
        }
    }
    return false;
}

/// Removes a citation suffix: `file.cpp:120-140` and `module.py::test_name` are the
/// two ways this project cites a place inside a file. The leading scan is the
/// regex's leftmost match; the two alternatives are tried in the order it has them.
std::string strip_line_suffix(std::string_view value) {
    const auto digits_from = [](std::string_view text, std::size_t at) {
        while (at < text.size() && text[at] >= '0' && text[at] <= '9') {
            ++at;
        }
        return at;
    };

    for (std::size_t at = 0; at < value.size(); ++at) {
        if (value[at] != ':') {
            continue;
        }
        const std::size_t after_digits = digits_from(value, at + 1);
        if (after_digits > at + 1) {
            if (after_digits == value.size()) {
                return std::string(value.substr(0, at));
            }
            if (value[after_digits] == '-') {
                const std::size_t second = digits_from(value, after_digits + 1);
                if (second > after_digits + 1 && second == value.size()) {
                    return std::string(value.substr(0, at));
                }
            }
        }
        if (at + 1 < value.size() && value[at + 1] == ':') {
            const std::string_view rest = value.substr(at + 2);
            bool has_space = false;
            for (std::size_t scan = 0; scan < rest.size(); scan += 1) {
                if (is_space_at(rest, scan)) {
                    has_space = true;
                    break;
                }
            }
            if (!rest.empty() && !has_space) {
                return std::string(value.substr(0, at));
            }
        }
    }
    return std::string(value);
}

/// The path a backticked token claims, or nullopt when it is not a path claim.
///
/// Conservative on purpose: a gate that fires on `--language basic` or on
/// `turtle_state.py` (a package-relative name that has to be read in context) is a
/// gate somebody switches off.
std::optional<std::string> path_claim(std::string_view token,
                                      const std::set<std::string>& top_level) {
    std::string candidate(trim(token));
    if (candidate.find('/') == std::string::npos || candidate.find(' ') != std::string::npos) {
        return std::nullopt;
    }
    constexpr std::string_view kNotPathCharacters = "<>$*{}|";
    for (const char character : candidate) {
        if (kNotPathCharacters.find(character) != std::string_view::npos) {
            return std::nullopt;
        }
    }
    for (const char* prefix : {"http", "www", "file:", "~"}) {
        if (candidate.starts_with(prefix)) {
            return std::nullopt;
        }
    }
    candidate = candidate.substr(0, candidate.find('#'));
    candidate = strip_line_suffix(candidate);
    const std::string first = candidate.substr(0, candidate.find('/'));
    if (!top_level.contains(first)) {
        return std::nullopt;
    }
    while (!candidate.empty() && candidate.back() == '/') {
        candidate.pop_back();
    }
    return candidate;
}

/// A `make` command in a fenced block: the directory `-C` names, when it names one,
/// and the target.
struct MakeCommand {
    std::optional<std::string> directory;
    std::string target;
};

/// `^\s*make(?:\s+-C\s+(\S+))?\s+([A-Za-z0-9_.-]+)`.
///
/// The optional group is tried first and abandoned when no target follows it, which
/// is what the engine would do: `make -C native` without a target then matches with
/// `-C` as its target, and the caller's "-" test drops it.
std::optional<MakeCommand> parse_make_command(std::string_view line) {
    struct Span {
        std::size_t begin = std::string_view::npos;
        std::size_t end = std::string_view::npos;
        [[nodiscard]] bool found() const { return begin != std::string_view::npos; }
    };

    const auto target_after = [](std::string_view text, std::size_t from) {
        const std::size_t begin = skip_space(text, from);
        if (begin == from) {
            return Span{};  // the regex needs `\s+` before the target
        }
        std::size_t end = begin;
        while (end < text.size()) {
            const char character = text[end];
            const bool allowed = (character >= 'a' && character <= 'z') ||
                                 (character >= 'A' && character <= 'Z') ||
                                 (character >= '0' && character <= '9') || character == '_' ||
                                 character == '.' || character == '-';
            if (!allowed) {
                break;
            }
            ++end;
        }
        if (end == begin) {
            return Span{};
        }
        return Span{begin, end};
    };

    const std::size_t at = skip_space(line, 0);
    if (!line.substr(at).starts_with("make")) {
        return std::nullopt;
    }
    const std::size_t after_make = at + 4;

    const std::size_t space = skip_space(line, after_make);
    if (space > after_make && line.substr(space).starts_with("-C")) {
        const std::size_t name_begin = skip_space(line, space + 2);
        if (name_begin > space + 2) {
            std::size_t name_end = name_begin;
            while (name_end < line.size() && !is_space_at(line, name_end)) {
                ++name_end;
            }
            if (const Span target = target_after(line, name_end); name_end > name_begin &&
                                                                 target.found()) {
                return MakeCommand{
                    std::string(line.substr(name_begin, name_end - name_begin)),
                    std::string(line.substr(target.begin, target.end - target.begin))};
            }
        }
    }

    const Span target = target_after(line, after_make);
    if (!target.found()) {
        return std::nullopt;
    }
    return MakeCommand{std::nullopt,
                       std::string(line.substr(target.begin, target.end - target.begin))};
}

/// The targets a Makefile defines: its rules and its `.PHONY` list.
std::set<std::string> make_targets(const fs::path& path) {
    std::set<std::string> targets;
    const std::optional<std::string> content = read_text(path);
    if (!content) {
        return targets;
    }

    const std::vector<std::string_view> lines = split_lines(*content);

    // `.PHONY: $(FORWARDED)` is how the top-level Makefile lists the targets it
    // forwards to `native/`, so a rule that ignored variables would call
    // `make test` a target that does not exist.
    std::map<std::string, std::vector<std::string>> variables;
    for (const std::string_view line : lines) {
        std::size_t at = 0;
        if (line.empty()) {
            continue;
        }
        const auto alpha = [](char character) {
            return (character >= 'a' && character <= 'z') ||
                   (character >= 'A' && character <= 'Z') || character == '_';
        };
        const auto alnum = [](char character) {
            return (character >= 'a' && character <= 'z') ||
                   (character >= 'A' && character <= 'Z') ||
                   (character >= '0' && character <= '9') || character == '_';
        };
        if (!alpha(line[0])) {
            continue;
        }
        while (at < line.size() && alnum(line[at])) {
            ++at;
        }
        const std::string_view name = line.substr(0, at);
        std::size_t after = skip_space(line, at);
        if (after < line.size() && (line[after] == ':' || line[after] == '?')) {
            ++after;
        }
        if (after >= line.size() || line[after] != '=') {
            continue;
        }
        ++after;
        after = skip_space(line, after);
        std::vector<std::string> words;
        for (const std::string_view word : split_words(line.substr(after))) {
            words.emplace_back(word);
        }
        variables[std::string(name)] = std::move(words);
    }

    for (const std::string_view line : lines) {
        if (line.starts_with(".PHONY:")) {
            for (const std::string_view word : split_words(line.substr(7))) {
                if (word == "\\") {
                    continue;
                }
                if (word.size() >= 3 && word.starts_with("$(") && word.ends_with(")")) {
                    const auto found =
                        variables.find(std::string(word.substr(2, word.size() - 3)));
                    if (found != variables.end()) {
                        targets.insert(found->second.begin(), found->second.end());
                    }
                    continue;
                }
                targets.emplace(word);
            }
            continue;
        }
        if (line.empty() || line[0] == ' ' || line[0] == '\t' || line[0] == '#') {
            continue;
        }
        if (line.starts_with(".")) {
            continue;
        }
        const std::size_t colon = line.find(':');
        if (colon == std::string_view::npos) {
            continue;
        }
        const std::string name(trim(line.substr(0, colon)));
        if (!name.empty() && name.find('=') == std::string::npos &&
            name.find(' ') == std::string::npos) {
            targets.insert(name);
        }
    }
    return targets;
}

std::set<std::string> top_level_directories(const fs::path& root) {
    std::set<std::string> names;
    std::error_code failure;
    fs::directory_iterator entry(root, failure);
    const fs::directory_iterator end;
    while (!failure && entry != end) {
        std::error_code step;
        if (entry->is_directory(step)) {
            names.insert(entry->path().filename().string());
        }
        entry.increment(step);
        if (step) {
            break;
        }
    }
    return names;
}

/// Every tracked Markdown file, which is what a reviewer sees.
///
/// `git ls-files` keeps the build trees, the virtual environment and `node_modules`
/// out of the run without a second ignore file -- the same reason `lint-md` uses it.
std::optional<std::vector<fs::path>> tracked_files(const fs::path& root, std::string& error) {
    const std::string command = "git -C \"" + root.string() + "\" ls-files \"*.md\"";
    const CommandResult result = run_command(command);
    if (!result.started) {
        error = "cannot run git to list the tracked Markdown files";
        return std::nullopt;
    }
    if (result.status != 0) {
        error = "git ls-files failed in " + root.string();
        return std::nullopt;
    }
    std::vector<fs::path> files;
    for (const std::string_view line : split_lines(result.output)) {
        if (trim(line).empty()) {
            continue;
        }
        files.push_back(root / fs::path(std::string(line)));
    }
    return files;
}

/// (line number, message) for every false claim in one document.
std::vector<Problem> check_text(std::string_view text, const fs::path& root,
                                const std::set<std::string>& top_level,
                                const std::set<std::string>& root_targets,
                                const std::set<std::string>& native_targets) {
    std::vector<Problem> problems;
    bool in_history = text.substr(0, text.size() < 1000 ? text.size() : 1000).find(kWholeFile) !=
                      std::string_view::npos;
    bool in_fence = false;

    int number = 0;
    for (const std::string_view line : split_lines(text)) {
        ++number;
        if (line.find(kHistoryOpen) != std::string_view::npos) {
            in_history = true;
            continue;
        }
        if (line.find(kHistoryClose) != std::string_view::npos) {
            in_history = false;
            continue;
        }
        if (is_fence(line, false)) {
            in_fence = !in_fence;
            continue;
        }
        if (line.find(kIgnore) != std::string_view::npos) {
            continue;
        }
        if (in_history) {
            // A record has to be able to name what it records, including a path
            // that has since moved.
            continue;
        }

        for (const std::string_view token : backtick_tokens(line)) {
            const std::optional<std::string> claimed = path_claim(token, top_level);
            if (!claimed) {
                continue;
            }
            std::error_code failure;
            if (!fs::exists(root / fs::path(*claimed), failure)) {
                problems.push_back({number, "`" + *claimed + "` does not exist"});
            }
        }

        for (const char* name : kRetired) {
            if (line.find(name) == std::string_view::npos) {
                continue;
            }
            if (past_tense(line)) {
                continue;
            }
            problems.push_back({number, "`" + std::string(name) +
                                            "` was deleted; this line says it is here"});
        }

        if (in_fence) {
            const std::optional<MakeCommand> command = parse_make_command(line);
            if (!command) {
                continue;
            }
            std::string directory = command->directory.value_or(std::string());
            while (!directory.empty() && directory.back() == '/') {
                directory.pop_back();
            }
            const bool scoped_to_native =
                command->directory.has_value() && directory.ends_with("native");
            const std::set<std::string>& scoped = scoped_to_native ? native_targets : root_targets;
            // `make VAR=value something` and `make $@` are not target claims.
            if (command->target.starts_with("$") || command->target.starts_with("-")) {
                continue;
            }
            if (!scoped.contains(command->target)) {
                problems.push_back(
                    {number, "`make " + command->target + "` is not a target of " +
                                 (command->directory.has_value() ? "native/Makefile"
                                                                 : "the root Makefile")});
            }
        }
    }
    return problems;
}

/// The relative path a report prints, as the Python's `relative_to(REPO_ROOT)`.
std::string report_path(const fs::path& path, const fs::path& root) {
    const fs::path relative = path.lexically_relative(root);
    if (relative.empty()) {
        return path.string();
    }
    return relative.string();
}

/// Proves the three rules fire, and that both escapes suppress them.
///
/// A gate that cannot be shown to fail is a gate nobody should trust: the whole
/// reason this one exists is that a checker which only ever passes looks exactly
/// like a repository with nothing wrong in it.
int self_test(const fs::path& root) {
    struct Case {
        const char* name;
        const char* text;
        std::size_t expected;
    };
    static const Case kCases[] = {
        {"a moved path", "see `native/src/ui/moved.cpp`", 1},
        {"a deleted name", "`PythonWorker` does it", 1},
        {"a name in the past tense", "`PythonWorker` was deleted", 0},
        {"a history region",
         "<!-- history -->\n`PythonWorker` does it\n<!-- /history -->", 0},
        {"a whole-file record", "<!-- history: all -->\n`PythonWorker` does it", 0},
        {"an example path", "`native/my_lang.py`<!-- doc-claims: ignore -->", 0},
        {"a make target that exists", "```\nmake test\n```", 0},
        {"a make target that does not", "```\nmake frobnicate\n```", 1},
        {"a non-path in backticks", "`--language basic`", 0},
    };

    const std::set<std::string> top_level{"native", "docs"};
    const std::set<std::string> targets{"test"};

    std::size_t failures = 0;
    for (const Case& test : kCases) {
        const std::size_t found =
            check_text(test.text, root, top_level, targets, targets).size();
        if (found != test.expected) {
            ++failures;
            std::cout << "  self-test: " << test.name << ": expected " << test.expected
                      << ", got " << found << '\n';
        }
    }
    if (failures != 0) {
        std::cerr << "  " << failures << " self-test failure(s)\n";
        return 1;
    }
    std::cout << "self-test: " << std::size(kCases) << " cases\n";
    return 0;
}

}  // namespace

int run_claims_command(int argc, char** argv) {
    const Arguments arguments =
        parse_arguments(argc, argv, 2, {"--root"}, {"--quiet", "--self-test"});
    if (!arguments.error.empty()) {
        std::cerr << "time_warp_docs: " << arguments.error << '\n';
        return 2;
    }
    if (!arguments.positionals.empty()) {
        std::cerr << "time_warp_docs: claims takes no files\n";
        return 2;
    }
    std::string error;
    const std::optional<fs::path> root = root_of(arguments, error);
    if (!root) {
        std::cerr << "time_warp_docs: " << error << '\n';
        return 2;
    }

    if (arguments.has("self-test")) {
        return self_test(*root);
    }

    const std::set<std::string> top_level = top_level_directories(*root);
    const std::set<std::string> root_targets = make_targets(*root / "Makefile");
    const std::set<std::string> native_targets = make_targets(*root / "native" / "Makefile");

    const std::optional<std::vector<fs::path>> files = tracked_files(*root, error);
    if (!files) {
        std::cerr << "❌ " << error << '\n';
        return 1;
    }

    std::size_t failures = 0;
    for (const fs::path& path : *files) {
        const std::optional<std::string> content = read_text(path);
        if (!content) {
            // The Python lets `read_text` raise here; the verdict is the same --
            // the run fails -- without the traceback.
            std::cerr << "❌ " << report_path(path, *root) << ": cannot be read\n";
            return 1;
        }
        for (const Problem& problem : check_text(*content, *root, top_level, root_targets,
                                                 native_targets)) {
            ++failures;
            std::cout << report_path(path, *root) << ":" << problem.line << ": "
                      << problem.message << '\n';
        }
    }

    if (failures != 0) {
        std::cerr << "\n"
                  << failures
                  << " false claim(s). A path that moved, a name that was deleted, or a target "
                     "that does not exist: fix the sentence, or wrap the record in "
                  << kHistoryOpen << " ... " << kHistoryClose << ".\n";
        return 1;
    }
    if (!arguments.has("quiet")) {
        std::cout << "doc claims: " << files->size() << " files, no false claims\n";
    }
    return 0;
}

}  // namespace docs
