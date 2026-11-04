# Time Warp Studio

Time Warp Studio is a desktop programming environment for ten languages of the 1960s and
1970s: BASIC, APL, Logo, PILOT, FOCAL, Forth, Scheme, COMAL, REXX and Smalltalk. Each one
runs through its own interpreter, linked straight into the application rather than started
as a separate process. Open a program and it runs; the debugger can stop inside it, in
whichever language the editor is showing.

[![Languages](https://img.shields.io/badge/languages-10-blue)](#the-ten-languages)
[![Platform](https://img.shields.io/badge/platform-Linux-green)](docs/user/getting-started.md)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Version](https://img.shields.io/badge/Version-28.2.0-orange)](https://github.com/James-HoneyBadger/Time_Warp_Studio)

It is written in C++20 against Qt 6 and runs on Linux. The interpreters are libraries in
the same process, which is what lets one debugger serve all ten: a run is stopped,
inspected and resumed in the same panel, whichever language is in the editor.

Each interpreter implements the dialect it is named for, including the parts that do not
carry over from one language to the next. FOCAL is a formula calculator with line labels
and sign branching. PILOT is letter-coded units. APL has its own glyphs and no operator
precedence. Forth is a dictionary of words. Scheme is an explicit-stack CEK machine
without native recursion. Smalltalk sends messages to objects. A program is written in
the dialect of its period and behaves as it did on the machines it was written for.

The editor, debugger, canvas, project model and SQL layer are shared by all ten. A
breakpoint in BASIC and one in Smalltalk are set, inspected and cleared in the same panel.

---

## Install

Each [release](https://github.com/James-HoneyBadger/Time_Warp_Studio/releases/latest)
carries a built application for Linux with the Qt runtime inside it, so no compiler is
required.

The project is Linux only. Everything it builds, tests and ships targets Linux; there is
no Windows or macOS build and no installer for either.
[Getting started](docs/user/getting-started.md) covers the full procedure, the build
requirements and what to do when a build fails.

| You have | Take | Then |
| --- | --- | --- |
| a Linux desktop, nothing to install | `Time_Warp_Studio-<version>-x86_64.AppImage` | Make it executable and run it. No root access, no package manager, no Qt to fetch. |
| any distribution, sandboxed with its own Qt | `Time_Warp_Studio-<version>-x86_64.flatpak` | `flatpak install ./Time_Warp_Studio-<version>-x86_64.flatpak`, then run **Time Warp Studio** from the menu. |
| Ubuntu, Debian, Mint or Pop!_OS | `time-warp-studio_<version>_amd64.deb`, or `_arm64.deb` | `sudo apt install ./time-warp-studio_<version>_amd64.deb` |
| Fedora, RHEL or openSUSE | `time-warp-studio-<version>-1.x86_64.rpm`, or `.aarch64.rpm` | `sudo dnf install ./time-warp-studio-<version>-1.x86_64.rpm` |
| any other Linux, or arm64 | `time-warp-studio-<version>-linux-<arch>.tar.gz` | `tar -xzf … -C / --strip-components=1` |

`uname -m` reports the architecture, `x86_64` or `aarch64`.

## Run from a checkout

```bash
./run.sh                                          # build if needed, then launch
./run.sh Examples/logo/mandala.logo               # open a program
./run.sh --run Examples/basic/hello.bas           # open and run it
./twc Examples/scheme/hello.scm -o hello-scheme   # a standalone executable
./api                                             # JSON-RPC on stdin/stdout
```

`run.sh` builds incrementally and passes its arguments to the IDE, including `--run`,
`--screenshot` and `--export`. `twc` is a terminal compiler that links the engines into a
single executable, and `api` is the automation service. To build by hand:
`cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release && cmake --build native/build -j`.

---

## The ten languages

Every command is listed in the [language reference](docs/languages/reference.md), and the
command gate exercises each row of it on every push. Each language has a
[tutorial](docs/languages/tutorials/).

| Language | Paradigm | Era | Execution |
| --- | --- | --- | --- |
| **BASIC** | Imperative, educational | 1964 | Line by line |
| **APL** | Array, notation as tool | 1966 | Line by line |
| **Logo** | Turtle graphics, educational | 1967 | Whole program |
| **PILOT** | Computer-aided instruction | 1969 | Line by line |
| **FOCAL** | Formula calculator, teaching | 1969 | Line by line |
| **Forth** | Stack, dictionary of words | 1970 | Whole program |
| **Scheme** | Functional, Lisp dialect | 1975 | Whole program |
| **COMAL** | Structured BASIC, educational | 1979 | Line by line |
| **REXX** | Scripting, string processing | 1979 | Line by line |
| **Smalltalk** | Object-oriented, messages | 1980 | Whole program |

The Execution column says how the engine runs a program, and that decides what the
debugger can do. A line-by-line language is called by the shared loop once per statement,
so a pause lands between statements and profiling counts per line. A whole-program
language runs its own loop and chooses where a pause may land: at the end of a Logo
statement, at a Smalltalk message send, at a Forth token.

---

## What the Studio does

| Capability | What it covers |
| --- | --- |
| **Editor and console** | Gutter, current line, syntax colouring, find and replace, find in files, snippets, outline, code formatting, live lint, autosave, version history, two panes side by side |
| **Turtle canvas** | The drawing as it is made, with zoom, pan, grid and auto-fit; a scrubber that replays it stroke by stroke; GIF recording; PNG, JPEG, BMP and SVG export |
| **Debugger, in all ten** | Breakpoints in the gutter, continue, pause, step in, over and out, watch expressions, data breakpoints, a Variables panel and a telemetry HUD |
| **REPL bar** | A persistent session in the current language, with the bindings that language keeps |
| **Projects** | `.twproject` files that group programs without moving them on disk |
| **SQL** | A SQLite database tab, and a `SQL` statement in eight of the ten languages |
| **Teaching** | 20 lessons with 60 checkpoints, checked by running the learner's program; rosters, handouts and marking |
| **Appearance** | 35 themes including the retro CRT sets, a high-contrast variant of each, text at 100%, 125% or 150%, focus and presentation modes |
| **Automation** | JSON-RPC over stdin and stdout, a standalone compiler, scriptable screenshots and exports |

The manual is in four parts:

| You are | Read |
| --- | --- |
| **Using the Studio** | [Getting started](docs/user/getting-started.md), then [the Studio](docs/user/the-studio.md) |
| **Learning or teaching a language** | [The ten languages](docs/languages/README.md), the [reference](docs/languages/reference.md) and the [curriculum](docs/lessons/README.md) |
| **Working on the code** | [Architecture](docs/maintainer/architecture.md), [decisions](docs/maintainer/decisions.md), [testing](docs/maintainer/testing.md) |
| **Following the project** | [Roadmap](docs/maintainer/roadmap.md), [history](docs/project/history.md), [support](.github/SUPPORT.md) |

[docs/INDEX.md](docs/INDEX.md) is the complete map of the documentation, and it is also
the table of contents of **Help > Documentation** in the application: the help inside the
Studio is built from the same files as the manuals in the tree.

---

## What ships with it

- **68 example programs** across the ten languages, each run by a gate. **File > Examples
  Gallery…** lists them with the drawing each program makes; the picture is the program
  run headlessly, not a stored image. The catalogue is
  [Examples/CATALOG.md](Examples/CATALOG.md).
- **20 lessons with 60 checkpoints**, two per language, each checked by running the
  learner's program: `PRINT 6 * 7` and `PRINT 42` pass the same checkpoint, and a drawing
  checkpoint cannot be met by printing a description of a drawing.
- **Notebooks**: prose and programs in one `.twnb` file. A cell is text or a program in a
  language, and **Run** shows its output and its drawing beneath it. Cells do not share
  variables: a notebook is a document, not a session.
- **Error hints** beside an interpreter's message, proposing a correction.
- **Ten tutorials**, one per language, under
  [docs/languages/tutorials](docs/languages/tutorials/).

### Help from inside the Studio

| Key or menu | What it does |
| --- | --- |
| `F1` | Context help: the manual section for the panel, language or control with focus |
| `Ctrl+F1` | Keyword inspector: the reference entry for the word under the cursor |
| Help > **Documentation** | The documentation browser, indexed from `docs/INDEX.md` |
| `F10` or `Ctrl+Shift+P` | The command palette, listing every action in the window by name |
| `F12` | The guided tour, a one-time walk through the window |

---

## Working on the project

| Read | For |
| --- | --- |
| [CONTRIBUTING.md](CONTRIBUTING.md) | Setup, the gates, and the recipe for adding a language, a command or a lesson |
| [docs/maintainer/testing.md](docs/maintainer/testing.md) | The suites, the example gate, the command matrix and the parity corpus |
| [docs/maintainer/architecture.md](docs/maintainer/architecture.md) | How the application is put together, and why |
| [.github/SUPPORT.md](.github/SUPPORT.md) | Where to ask, and what is supported |
| [SECURITY.md](SECURITY.md) | Reporting a vulnerability, privately |
| [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) | The standard expected here |
| [docs/project/history.md](docs/project/history.md) | What changed, release by release, and what the PySide6 port replaced |

### Not built yet

An automatic update check, Snap packaging, an arm64 AppImage, a fully static build, and
automatic publishing to a distribution repository. Each is recorded in
[docs/maintainer/roadmap.md](docs/maintainer/roadmap.md) and registered in
[`native/share/unbuilt.txt`](native/share/unbuilt.txt), so shipping one without
correcting these pages fails `make doc-claims`.

---

## About this repository

The description and topics below are the canonical text for this repository. GitHub's
**About** box is set in the repository settings rather than in a file, so this copy is the
one to keep in step with the project.

### Description

```text
An old-school programming environment: ten interpreters from 1964-1980, a full IDE,
a debugger in every language and turtle graphics.
```

### Topics

```text
retro-computing
ide
interpreter
debugger
turtle-graphics
basic
logo
pilot
comal
rexx
scheme
focal
apl
smalltalk
forth
```

Releases carry `.deb`, `.rpm`, tarball and AppImage packages for Linux in x86_64 and
arm64, plus a `.flatpak` bundle. Building from source is how to work on the application
itself; [getting started](docs/user/getting-started.md) gives the procedure.
