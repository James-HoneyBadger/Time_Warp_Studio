# Time Warp Studio — notes for coding agents

**Project:** an old-school programming environment: ten interpreters from 1964–1980, one
IDE, a debugger in every language, and a turtle canvas. Retro-computing hobbyists are the
first audience; the checked curriculum ships for teaching but the product does not assume it.

**Stack:** C++20 and Qt 6, Linux only. There is one Qt and one platform: nothing in the tree
may be conditioned on Windows or macOS, and nothing needs an interpreter to build, test or
check. `make tree-check` fails if a `.py` file or a Python tool's configuration file comes
back.

---

## Where things are

```text
native/                     the Qt application (the product)
  include/ src/ tests/ tools/
    lang/                   the ten executors and the shared interpreter loop
    scheme/                 the explicit-stack Scheme machine
    lessons/                curriculum, verifier, progress, classroom
    debugger/ project/ sql/ api/ canvas/
  share/                    lessons.json, snippets.json, command_rows.json
  tools/
    gates/                  time_warp_gates: smoke, commands, repl, golden, tree
    docs/                   time_warp_docs: links, claims, version, md (no Qt)
    icon/                   time_warp_icon: the packaged icons
Examples/                   68 programs across the ten languages
docs/                       the documentation set, by audience (see the table below)
```

The build is `cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release && cmake --build
native/build -j`. The IDE is `native/build/time_warp_ide`; `./run.sh` builds if needed and
launches it.

---

## The shape of the code

- **The language registry is one table**: `native/include/timewarp/lang/registry.hpp`. The Qt
  side maps it by position in `native/src/ui/language.cpp` with a `static_assert`, and
  `make registry-check` enforces that no second list appears.
- **An executor is a function, not a class**, and its contract is what
  `native/include/timewarp/lang/turtle_host.hpp` documents: it returns the text to display
  with the message prefixes, and it never throws out. All UI state lives in the Qt
  application, and drawing crosses the thread boundary as commands on a `CanvasCommandQueue`
  that the GUI thread drains into `TurtleCanvas`.
- **Two execution modes.** Whole-program: Scheme, Logo, Smalltalk, Forth. Line-by-line: BASIC,
  APL, PILOT, FOCAL, COMAL, REXX, called once per statement by the shared loop. The distinction
  is real — it decides whether the debugger can pause between statements, and where a pause may
  land in a whole-program language.
- **One expression evaluator** (`timewarp_expr`) serves every arithmetic a language needs, and
  there is no `eval()` anywhere in the tree.
- **One styling system.** `design::tokens()` derives the interface's colours and metrics from
  the active theme, `style::sheet()` builds the stylesheet from those tokens, and
  `icons::glyph()` draws the icons with an icon engine that follows the palette. All 35 themes
  are data in `native/src/ui/theme_table.inc`.

---

## Commands that matter

```bash
make -C native run         # build if needed, then launch
make test                  # the 24 C++ suites
make test-all              # every gate CI runs
make smoke                 # every example program through the native engines
make command-matrix        # one row per command, from native/share/command_rows.json
make repl-matrix           # the same rows through a REPL session
make golden-check          # the ports against the frozen corpus, 634 programs
make teach-check           # every lesson solution and starter
make lint-md doc-links doc-claims tree-check registry-check

./native/build/time_warp_check Examples/basic/hello.bas   # what a program printed and drew
./twc Examples/basic/hello.bas -o hello                   # a standalone executable
./api                                                     # JSON-RPC on stdin/stdout
```

`make test-all` is what a change has to survive. A warning is an error: every target compiles
with `-Wall -Wextra -Wpedantic -Wshadow -Wcast-qual -Wuseless-cast -Wlogical-op
-Wnull-dereference` and `-Werror`.

---

## Conventions

**Message prefixes.** Every string an engine returns carries one: `❌` error, `✅` success,
`ℹ️` information, `🎨` theme or display change, `🚀` execution event, `🐢` drawing action, `📝`
input prompt. That is the output convention and the only place emoji remain — an interface
control carries a drawn icon and a word.

**Comments explain why.** The unusual decisions are documented where they are made; a comment
that restates the code is noise beside them.

**A failure that is silent is the characteristic bug here.** A program that runs and draws
nothing, a command that returns nothing, a lesson whose answer does not work: `smoke` and
`commands` exist because five languages once "succeeded" and produced no ink.

**Prose in the manuals** is British, declarative, second person only in procedures, no
whimsy, no emoji. A claim in a guide is a claim `make doc-claims` will check.

**A behaviour change edits the parity corpus in the same commit.** The gate compares with a
recording, so a deliberate change is a corpus edit.

---

## Documentation

Each fact has one home. The set is organised by audience:

| Directory | Owns |
| --- | --- |
| `docs/user/` | Getting started, the Studio's window and panels, the automation API, questions |
| `docs/languages/` | The ten languages, their reference, and one tutorial per language |
| `docs/lessons/` | The curriculum, the lesson format, and the generated plans |
| `docs/maintainer/` | Architecture, decisions, testing, roadmap, release |
| `docs/project/` | The history archive: the PySide6 port, and every release |
| `docs/INDEX.md` | The map. It is also the table of contents of **Help > Documentation**, built by `native/src/ui/help_browser.cpp` from its headings and tables |

Root and `.github/`: `README.md` (the front door), `CONTRIBUTING.md`, `SECURITY.md`,
`CODE_OF_CONDUCT.md`, `.github/SUPPORT.md`, `.github/DEPLOYMENT.md` (the workflows),
`.github/pull_request_template.md`.

The gates: `time_warp_docs links` (every relative link and anchor resolves), `claims` (every
path, target and name a guide states exists), `version` (the copies of the version agree),
`md` (the house Markdown rules). `docs/INDEX.md` is load-bearing twice: CI checks that every
guide it promises exists, and the application parses it into the Help tree.

---

## Pitfalls

1. **Executor statelessness.** No UI references in an executor; return a string.
2. **A path search that guesses fails at run time.** `native/tools/gates/locate.cpp` is the
   single place the harnesses ask where `time_warp_check` is.
3. **The corpus is weaker than the differential it replaced.** A recording cannot notice a
   mistake in the recording, which is why behaviour changes are corpus edits.
4. **Building outside the checkout breaks two suites** (`classroom_tests`, `ui_layout_tests`)
   because the curriculum resolves relative to the executable. Build in `native/build`. <!-- doc-claims: ignore -->
5. **`$?` after a pipeline measures the last command.** Redirect rather than piping when
   checking an exit code.
6. **The window's minimum is 900 pixels wide**, and the toolbar is built to fit every control
   at that width. Adding a control means checking it at 900.

When in doubt, read `native/src/lang/interpreter.cpp` (the dispatch) and `make help` (the
checks this tree has).
