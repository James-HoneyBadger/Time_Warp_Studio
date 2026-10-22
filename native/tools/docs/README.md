# time_warp_docs

The documentation gates, as one C++20 program.

| Command | What it settles |
| --- | --- |
| `time_warp_docs links` | Every relative link and `#anchor` in the guides resolves. |
| `time_warp_docs claims` | Every backticked path exists, every retired name is discussed in the past tense, and every `make` target a fenced command names exists. |
| `time_warp_docs md` | Every tracked Markdown file satisfies the house rules. |
| `time_warp_docs version` | Every hand-written copy of `VERSION` agrees with it, and the changelog has a section for it. |

`links`, `claims` and `version` are the C++ counterparts of the three Python scripts
that did this until 22.1.0 — `check_doc_links.py`, `check_doc_claims.py` and
`version_sync.py`, all now deleted — and they print the lines those printed: the same
failure text, the same counts, the same exit status, so a reader who has seen one
output recognises the other.

`md` is a different kind of port: it replaces `markdownlint-cli2`, which needed Node
and fetched a package on every run, so `make lint-md` was the one check in the tree
that could not run without a JavaScript toolchain. Thirty of markdownlint's rules are
implemented in `src/docs/md.cpp`; the ones left out are listed in `md.hpp` with a
reason each.

## Building it

```bash
cmake -S native/tools/docs -B native/tools/docs/build
cmake --build native/tools/docs/build -j
```

That produces `native/tools/docs/build/time_warp_docs`. There is nothing to install
and nothing to fetch.

## Why this is a project of its own

The documentation job runs on every documentation change, on a runner that has no
Qt, no display and no virtual environment, and it has to finish in seconds. This
project therefore links nothing: no Qt, no third-party library, no network, no
clock. A C++20 compiler and CMake are the whole requirement.

For the same reason it is not a subdirectory of the native build. Being one would
make it inherit the Qt option set and the IDE's configuration, which is exactly the
dependency the split exists to avoid.

## Usage

```text
time_warp_docs links   [--root DIR] [FILE ...]
time_warp_docs claims  [--root DIR] [--quiet] [--self-test]
time_warp_docs version [--root DIR] [--write]
time_warp_docs md      [--root DIR] [--quiet] [FILE ...]
```

`--root` is the repository to check and defaults to the working directory, which is
where CI and the Makefile run from. The Python derived the same place from
`__file__`; the flag is the only difference in behaviour, and it exists so a scratch
tree can be checked without moving the binary.

`links` checks `README.md`, `Examples/CATALOG.md` and every Markdown file under
`docs/` unless files are named on the command line. `claims` reads
`git ls-files '*.md'`, so build trees and vendored trees stay out of the run without
a second ignore list. `claims --self-test` runs its fixed cases and prints how many
there are.

`version --write` rewrites the copies it finds stale; without it nothing is
modified and a disagreement fails the run, which is what CI uses to refuse a change
where the numbers no longer agree.

`md` lints every tracked Markdown file, or the ones named. `md --explain` prints how
each line was read -- heading, list item, fence, table row -- which is what to reach
for when a rule disagrees with your reading of a document.

## Exit status

| Status | Meaning |
| --- | --- |
| 0 | The check passed. |
| 1 | The check failed: a broken link, a false claim, a stale copy, or a markdown rule. |
| 2 | The command line or an input file did not allow a check. |

## Layout

- `src/main.cpp` -- the four subcommands and the usage text.
- `src/docs/cli.hpp`, `src/docs/cli.cpp` -- the option parsing the three share.
- `src/docs/text.hpp`, `src/docs/text.cpp` -- reading UTF-8, splitting lines, and the
  hand-written scanners for fences, backticked tokens, inline links, headings and
  GitHub's slug. The Python speaks `re`; these are scanners because a regular
  expression that disagreed by one character would report a failure the reference
  does not.
- `src/docs/pipes.hpp`, `src/docs/pipes.cpp` -- the one child process, which asks git
  which Markdown files are tracked.
- `src/docs/links.*`, `src/docs/claims.*`, `src/docs/version.*`, `src/docs/md.*` --
  one gate each. `md` owns the line model every rule in it reads.
