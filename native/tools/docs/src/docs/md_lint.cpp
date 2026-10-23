// SPDX-License-Identifier: MIT
//
// The command: what `lint` asks, and what `time_warp_docs md` prints.
//
// The rules themselves are one file per subject -- headings, code, lists,
// whitespace, inline content -- and the line model they share is `md_internal.hpp`.
// `lint` is the list of them, in rule order, which is also the order their messages
// are written in.

#include "docs/md.hpp"

#include "md_internal.hpp"
#include "md_rules.hpp"

#include "docs/pipes.hpp"
#include "docs/text.hpp"

#include <algorithm>
#include <cstdio>
#include <fstream>

namespace docs::md {

using namespace detail;

std::vector<Issue> lint(std::string_view text) {
    const Document document = parse(text);
    std::vector<Issue> issues;
    md001(document, &issues);
    md003(document, &issues);
    md004(document, &issues);
    md005(document, &issues);
    md007(document, &issues);
    md009(document, &issues);
    md010(document, &issues);
    md012(document, &issues);
    md018(document, &issues);
    md019(document, &issues);
    md022(document, &issues);
    md023(document, &issues);
    md025(document, &issues);
    md026(document, &issues);
    md028(document, &issues);
    md029(document, &issues);
    md030(document, &issues);
    md031(document, &issues);
    md032(document, &issues);
    md033(document, &issues);
    md034(document, &issues);
    md040(document, &issues);
    md041(document, &issues);
    md046(document, &issues);
    md047(text, document, &issues);
    md048(document, &issues);
    md049(document, &issues);
    md055(document, &issues);

    std::stable_sort(issues.begin(), issues.end(),
                     [](const Issue& left, const Issue& right) {
                         return left.line < right.line;
                     });
    return issues;
}

std::string describe(std::string_view line) {
    const Document document = parse(line);
    std::string text;
    for (std::size_t index = 0; index < document.lines.size(); ++index) {
        const Line& entry = document.lines[index];
        const char* kind = "text";
        switch (entry.kind) {
            case Kind::Blank: kind = "blank"; break;
            case Kind::FrontMatter: kind = "front-matter"; break;
            case Kind::FenceOpen: kind = "fence-open"; break;
            case Kind::FenceClose: kind = "fence-close"; break;
            case Kind::Code: kind = "code"; break;
            case Kind::Heading: kind = "heading"; break;
            case Kind::ThematicBreak: kind = "thematic-break"; break;
            case Kind::BlockQuote: kind = "quote"; break;
            case Kind::ListItem: kind = "list"; break;
            case Kind::TableRow: kind = "table-row"; break;
            case Kind::TableDelimiter: kind = "table-delim"; break;
            case Kind::HtmlComment: kind = "comment"; break;
            case Kind::Text: kind = "text"; break;
        }
        text += std::to_string(index + 1) + "\t" + kind;
        if (entry.kind == Kind::Heading) {
            text += " h" + std::to_string(entry.level);
        }
        if (entry.kind == Kind::ListItem) {
            text += std::string{" "} + (entry.ordered ? "ordered" : "bullet") + " indent=" +
                    std::to_string(entry.indent) + " after=" +
                    std::to_string(entry.spaces_after_marker);
        }
        text += "\t" + std::string{rtrim(entry.raw).substr(0, 40)};
        text += "\n";
    }
    return text;
}

int run(const std::vector<std::string>& files, bool quiet) {
    int total = 0;
    int files_with_issues = 0;
    for (const std::string& path : files) {
        const std::optional<std::string> text = read_text(path);
        if (!text.has_value()) {
            std::fprintf(stderr, "❌ %s: cannot be read as UTF-8\n", path.c_str());
            ++total;
            ++files_with_issues;
            continue;
        }
        const std::vector<Issue> issues = lint(*text);
        if (issues.empty()) {
            continue;
        }
        ++files_with_issues;
        total += static_cast<int>(issues.size());
        for (const Issue& issue : issues) {
            std::printf("%s:%d: %s: %s\n", path.c_str(), issue.line, issue.rule.c_str(),
                        issue.message.c_str());
        }
    }
    if (total == 0) {
        if (!quiet) {
            std::printf("markdown: %d file(s), no issues\n", static_cast<int>(files.size()));
        }
        return 0;
    }
    std::fprintf(stderr, "\n%d issue(s) in %d file(s)\n", total, files_with_issues);
    return 1;
}

int run_md_command(const timewarp::cli::Invocation& invocation) {
    // `--explain` is this command's own; everything else it takes -- the root, the
    // quiet, the files -- is the shared vocabulary, already parsed.
    bool explain = false;
    std::vector<std::string> files;
    for (const std::string& argument : invocation.arguments) {
        if (argument == "--explain") {
            explain = true;
        } else if (argument.size() > 1 && argument.front() == '-' && argument != "-") {
            std::fprintf(stderr, "md: unknown option %s\n", argument.c_str());
            return 2;
        } else {
            files.emplace_back(argument);
        }
    }
    const std::string root = invocation.root_given ? invocation.root.string()
                                                   : std::string{};
    const bool quiet = invocation.quiet;

    if (files.empty()) {
        // The tracked files, so a build tree or a vendored directory stays out of
        // the run without a second ignore list -- the same reason the claims gate
        // asks git.
        std::string command = "git ls-files '*.md'";
        if (!root.empty()) {
            command = "cd '" + root + "' && " + command;
        }
        const CommandResult listing = run_command(command);
        if (!listing.started || listing.status != 0) {
            std::fprintf(stderr,
                         "❌ md: `git ls-files` failed; pass the files on the command "
                         "line instead\n");
            return 2;
        }
        std::size_t start = 0;
        while (start < listing.output.size()) {
            const std::size_t end = listing.output.find('\n', start);
            const std::string line =
                listing.output.substr(start, end == std::string::npos ? std::string::npos
                                                                     : end - start);
            if (!line.empty()) {
                files.push_back(root.empty() ? line : root + "/" + line);
            }
            if (end == std::string::npos) {
                break;
            }
            start = end + 1;
        }
        if (files.empty()) {
            std::printf("markdown: no tracked Markdown files\n");
            return 0;
        }
    }
    if (explain) {
        for (const std::string& path : files) {
            const std::optional<std::string> text = read_text(path);
            if (!text.has_value()) {
                std::fprintf(stderr, "❌ %s: cannot be read\n", path.c_str());
                return 2;
            }
            std::fputs(describe(*text).c_str(), stdout);
        }
        return 0;
    }
    return run(files, quiet);
}

}  // namespace docs::md
