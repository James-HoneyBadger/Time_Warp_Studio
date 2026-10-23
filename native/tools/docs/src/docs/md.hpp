// SPDX-License-Identifier: MIT
//
// The Markdown lint: the rules this repository's own documents are held to.
//
// It replaced `markdownlint-cli2`, which needs Node and a network fetch of the
// package on every run -- so `make lint-md` was the one check in the tree that
// could not run on a machine without a JavaScript toolchain, and on this one it
// did not run at all. The rules are the ones `.markdownlint.json` configured,
// stated here in C++ where the build already is.
//
// #### What it checks, and what it does not
//
// Twenty-eight rules from markdownlint's set, chosen because each has caught
// something real in this tree or would have: heading structure and spacing, list
// indentation, marker consistency and numbering, blank lines around headings, lists
// and fences, fence language and style, trailing whitespace, hard tabs, bare URLs,
// inline HTML, tables, and the file's own ending.
//
// What it leaves out:
//
//   MD013 line length        -- disabled in this tree: a wrapped table row and a
//                               wrapped sentence are both fine at 100 columns.
//   MD036 emphasis as heading -- disabled: this project uses a bold lead-in
//                               deliberately in the agent instructions.
//   MD044 proper names       -- never configured, and a list of them would be a
//                               second spelling of every product name.
//   MD051 link fragments     -- `links` answers this, with the anchors it can
//                               resolve, and two implementations would disagree.
//   MD053 unused references  -- the tree has no reference-style links at all.
//   MD020/MD021, closed atx  -- no document closes its headings; MD003 reports a
//                               document that mixes the two styles, which is the
//                               case worth catching.
//
// It does not implement the whole of markdownlint, and it does not pretend to.
// The rules it leaves out are listed here, with a reason each -- mostly
// that they encode a house style (MD013 line length, disabled here) or need a
// CommonMark parser to answer honestly (MD051 link fragments, which
// `check_doc_links.py`'s successor answers instead, with anchors it can resolve).

#pragma once

#include "timewarp/tools/cli.hpp"

#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

namespace docs::md {

/// One finding. `line` is 1-based, as every other gate here reports it.
struct Issue {
    int line = 0;
    std::string rule;
    std::string message;
};

/// Every finding in one document, in line order.
[[nodiscard]] std::vector<Issue> lint(std::string_view text);

/// `md` subcommand. `files` are paths; the caller has already resolved them.
[[nodiscard]] int run(const std::vector<std::string>& files, bool quiet);

/// The `md` command line: `[--root DIR] [--quiet] [FILE ...]`. With no files it
/// lints every tracked Markdown file, which is what `make lint-md` asks for.
[[nodiscard]] int run_md_command(const timewarp::cli::Invocation& invocation);

}  // namespace docs::md
