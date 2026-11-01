# Time Warp Studio

A desktop programming environment for ten interpreters of the 1964–1980 period: one
editor, one debugger, one turtle canvas, and no interpreter to install.

[![Languages](https://img.shields.io/badge/languages-10-blue)](#the-ten-languages)
[![Platform](https://img.shields.io/badge/platform-Linux-green)](docs/user/getting-started.md)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Version](https://img.shields.io/badge/Version-28.1.0-orange)](https://github.com/James-HoneyBadger/Time_Warp_Studio)

Time Warp Studio is a C++20 and Qt 6 application for Linux. Ten interpreters are linked
into it as libraries — BASIC, APL, Logo, PILOT, FOCAL, Forth, Scheme, COMAL, REXX and
Smalltalk — so a program starts immediately, the debugger can stop anywhere inside it,
and nothing is installed beside the download.

Each language is an implementation of its own dialect rather than a common core wearing
different keywords. FOCAL is a formula calculator with line labels and sign branching.
PILOT is letter-coded units. APL has its own glyphs and no operator precedence. Forth is
a dictionary of words. Scheme is an explicit-stack CEK machine with no native recursion.
Smalltalk sends messages to objects. The differences are the point: a program written for
1975 runs the way it was written.

The editor, the debugger, the canvas, the project model and the SQL layer are shared. A
run pauses at a statement in BASIC and at a message send in Smalltalk, and it is the same
panel, the same Variables table and the same Continue button in both.

---

## Install

Every [release](https://github.com/James-HoneyBadger/Time_Warp_Studio/releases/latest)
carries a built Linux application with the Qt runtime inside it. A compiler is not
required.

Time Warp Studio is a Linux application. Every artifact this project builds, tests and
ships is for Linux; there is no Windows build, no macOS build and no installer for either.
[Getting started](docs/user/getting-started.md) has the full procedure, the build
requirements and what to do when a build fails.

| You have | Take | Then |
| --- | --- | --- |
| a Linux desktop, nothing to install | `Time_Warp_Studio-<version>-x86_64.AppImage` | Make it executable and run it. No root, no package manager, no Qt to fetch. |
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

`run.sh` builds incrementally and forwards its arguments to the IDE, including `--run`,
`--screenshot` and `--export`. `twc` is a terminal compiler that links the engines into
one small executable, and `api` is the automation service. Building by hand is
`cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release && cmake --build native/build -j`.

---

## The ten languages

Every command is listed in the [language reference](docs/languages/reference.md), and
every row of that reference is exercised by the command gate on each push. Each language
has a [tutorial](docs/languages/tutorials/) that can be worked through in an evening.

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

The execution column is a real distinction. A line-by-line language is called once per
statement by the shared loop, so it can be paused between statements and profiled per
line. A whole-program language runs its own loop and decides where a pause may land: the
end of a Logo statement, a Smalltalk message send, a Forth token.

---

## What the Studio does

| Capability | What it is |
| --- | --- |
| **Editor and console** | Gutter, current line, syntax colouring, find and replace, find in files, snippets, outline, format code, live lint, autosave, version history, two panes side by side |
| **Turtle canvas** | The drawing as it is made, with zoom, pan, grid, auto-fit, a scrubber that replays it stroke by stroke, GIF recording, and PNG/JPEG/BMP/SVG export |
| **Debugging, in all ten** | Breakpoints in the gutter, continue, pause, step in, over and out, watch expressions, data breakpoints, a Variables panel and a telemetry HUD |
| **REPL bar** | A persistent session in the current language, with the bindings each engine keeps and nothing it does not |
| **Projects** | `.twproject` groups that organise programs without moving them on disk |
| **SQL** | A SQLite Database tab, and a `SQL` statement in eight of the ten languages |
| **Assistant, opt-in** | Explain, fix, comment, translate or write a program through a service you configure, or a model on this machine. Off until configured; the key never has to touch the disk |
| **Teaching** | 20 lessons and 60 checkpoints, checked by running the learner's program; rosters, handouts and marking for a class |
| **Appearance** | 35 themes including the retro CRT sets, a high-contrast variant of each, text at 100%, 125% or 150%, focus and presentation modes |
| **Automation** | JSON-RPC over stdin/stdout, a standalone compiler, scriptable screenshots and exports |

The details are in the manual, in four parts:

| You are | Read |
| --- | --- |
| **Using the Studio** | [Getting started](docs/user/getting-started.md), then [the Studio](docs/user/the-studio.md) |
| **Learning or teaching a language** | [The ten languages](docs/languages/README.md), the [reference](docs/languages/reference.md) and the [curriculum](docs/lessons/README.md) |
| **Working on the code** | [Architecture](docs/maintainer/architecture.md), [decisions](docs/maintainer/decisions.md), [testing](docs/maintainer/testing.md) |
| **Following the project** | [Roadmap](docs/maintainer/roadmap.md), [history](docs/project/history.md), [support](.github/SUPPORT.md) |

[docs/INDEX.md](docs/INDEX.md) is the complete map, and it is also the table of contents
of **Help > Documentation** inside the application, so the installed help and the manuals
in the tree are one set of documents.

---

## What ships with it

- **68 example programs** across the ten languages, each one run by a gate. **File >
  Examples Gallery…** shows every program with the drawing it makes as its face, because
  the thumbnail is the program run headlessly rather than a picture of it. The catalogue
  is [Examples/CATALOG.md](Examples/CATALOG.md).
- **20 lessons and 60 checkpoints**, two per language, each checked by *running* the
  learner's program: `PRINT 6 * 7` and `PRINT 42` pass the same checkpoint, and a drawing
  checkpoint cannot be satisfied by printing a description of a drawing.
- **Notebooks** — prose and programs in one `.twnb` file. A cell is text or a program with
  a language; **Run** shows its output and its drawing beneath it. Cells do not share
  variables: a notebook is a document, not a session.
- **Error hints**, which propose a correction beside an interpreter's error message.
- **Ten tutorials**, one per language, under [docs/languages/tutorials](docs/languages/tutorials/).

### Help from inside the Studio

| Key or menu | What it does |
| --- | --- |
| `F1` | Context help — the manual section for the panel, language or control with focus |
| `Ctrl+F1` | Keyword inspector — the reference entry for the word under the cursor |
| Help > **Documentation** | The documentation browser, indexed from `docs/INDEX.md` |
| `F10` or `Ctrl+Shift+P` | The command palette — every action in the window by name |
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

The description and topics below are the canonical ones. GitHub's **About** box is set in
the repository settings rather than in a file, so this is the copy to keep in step with
the project.

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
arm64, plus a `.flatpak` bundle. Building from source is the way to work on the
application itself; [getting started](docs/user/getting-started.md) is the whole
procedure.
