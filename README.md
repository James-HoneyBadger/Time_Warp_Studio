# Time Warp Studio

*An old-school programming environment: eleven languages, a full IDE, and two
complete computers inside it.*

[![C++20](https://img.shields.io/badge/C%2B%2B-20-blue)](https://en.cppreference.com/)
[![Qt](https://img.shields.io/badge/Qt-6%20%7C%205.15-green)](https://www.qt.io/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Version](https://img.shields.io/badge/Version-21.0.0-orange)](https://github.com/James-HoneyBadger/Time_Warp_Studio)

Time Warp Studio is a native C++20 and Qt workbench for people who like old
computers. It is the programming environment the 1980s promised and never quite
delivered: a full IDE, eleven interpreters spanning 1964 to 1980, a turtle canvas,
and a debugger that works in every one of them. Two complete machines are built
into it as well.

The **HB-1** is a bespoke 1982 home computer from a parallel universe. It has one
CRT, a prompt, a ROM socket that accepts any of the eleven languages, and **HB DOS
1.0** on a 512-sector disk. The **CHIP-8** is the 1977 virtual machine that made a
computer out of four kilobytes: live at 60 Hz, with a keypad, an assembler and a
register inspector. Both are working machines rather than a theme applied to an
editor, so you can boot them, type at them, break a program on them, save it to
their disk and find it there tomorrow.

The premise behind the HB-1 is a system where the design sense of the next forty
years arrived in time for the 8-bit boom. Its constraints are period ones — a
prompt, a sector-counted disk, 64K, a CRT with phosphor — while the engines behind
them are modern, native and open to inspection. Nothing about the machines is
decorative: every switch, word and opcode does something you can watch happen.

Time Warp Studio also carries a checked lesson curriculum and classroom tools for
people teaching with it, though nothing in the application assumes a classroom.

---

## Two machines, one Studio

| | **HB-1** | **CHIP-8** |
| --- | --- | --- |
| What it is | A complete home computer: CRT, prompt, eleven-language ROM socket, real editor, turtle canvas, disk | A complete virtual machine: 4 KB of memory, sixteen registers, a 64×32 screen, sixteen keys |
| When it belongs | 1982, where one machine shipped eleven language ROMs | 1977, where the machine was written in software and ran everywhere |
| Storage | **HB DOS 1.0** — 512 sectors of 256 bytes, a catalogue, lock bits, a two-step `INIT` | Your own folder of listings and ROMs, browsed by a library dialog |
| Run it | Tools > **HB-1 Computer…** (`Ctrl+Alt+W`) or `./run.sh --computer` | Tools > **CHIP-8 Emulator…** (`Ctrl+Shift+M`) |
| Read it | [Owner's Manual](docs/hb1/README.md) · [ROMs](docs/hb1/roms.md) · [HB DOS](docs/hb1/hb-dos.md) · [CONFIG](docs/hb1/config.md) · [Inside](docs/hb1/inside.md) | [Machine Manual](docs/chip8/README.md) · [Assembler](docs/chip8/assembly.md) · [Instruction set](docs/chip8/instruction-set.md) |

What you can do with them:

- **Boot a computer that never existed.** `Ctrl+Alt+W` brings the HB-1 up in
  BASIC — banner, prompt, `READY.` — and the ROM socket can be moved to any of the
  eleven languages, each of which keeps its own memory when you switch away and back.
- **Save to a period disk.** `SAVE "HELLO"` writes to a 512-sector volume with a
  catalogue, 30-character names, a per-file lock bit and a two-step `INIT`. The
  image is a single JSON file under `~/.time_warp/disks`, so you can read it, copy
  it, put it in git or hand a friend the same disk.
- **Write CHIP-8 the way it was written.** Assemble a listing in the editor or in
  the CHIP-8 window, load your own ROMs from a folder you point it at, then
  single-step one instruction at a time and watch `V0`–`VF` change.
- **Make it look the way you remember 1982.** Choose from four machine phosphors,
  five strengths of CRT glass and 35 IDE themes, among them Commodore 64, Apple
  II, ZX Spectrum, IBM PC CGA, DOS Blue, Amiga Workbench 2.0, NeXTSTEP Gunmetal,
  Borland Turbo Vision 1991, Windows 2000 and Windows 7.
- **Read the machine, not a changelog.** Each has its own book set: [the
  machines](docs/MACHINES.md), the [HB-1 manuals](docs/hb1/README.md), the
  [CHIP-8 manuals](docs/chip8/README.md).

---

## For the retro-computing mind

The details that matter if you have ever owned one of these machines, or wished
you had:

- **The quirks are kept, and written down.** CHIP-8 shifts `Vx` in place, sprites
  wrap at the edges, `FX55`/`FX65` leave `I` one past the block and `Bnnn` adds
  `V0`. There are no quirk switches: one machine, one manual, with every
  divergence from other emulators stated in
  [the instruction set](docs/chip8/instruction-set.md).
- **The disk is a file.** HB DOS images are JSON under `~/.time_warp/disks`, so
  you can read them, diff them and keep them in a repository. The sector
  arithmetic is real: a program that will not fit is refused, never truncated.
- **Your ROMs, not ours.** The Studio ships no third-party CHIP-8 ROMs, because
  most of the ones in circulation are somebody's copyright. The ROM library
  browses a folder you own, and the four bundled programs are listings you can
  read and edit.
- **The whole look, not a filter.** There are 35 IDE themes — C64, Apple II, ZX
  Spectrum, CGA, Amiga, NeXTSTEP, Borland and the two Windows schemes — plus four
  machine phosphors and CRT glass you can tune from off to heavy. Choosing amber
  recolours the header, the editor, the canvas and the glass together, because the
  palette is one table.
- **Native, offline, inspectable.** Every interpreter is C++ running in process,
  and so are the lesson check, the classroom and the two commands that drive them
  (`time_warp_teach`, `time_warp_learn`). An installed package contains no Python
  file at all; there is no network call and no telemetry. The project is
  MIT-licensed, and both the tests and the manuals are in the repository.

---

## Install it

You do not need a compiler. Every [release](https://github.com/James-HoneyBadger/Time_Warp_Studio/releases/latest)
carries a ready-built application for Windows, macOS and Linux, with the Qt runtime
inside it.

| You have | Take | Then |
| --- | --- | --- |
| Windows 10 or 11 | `time-warp-studio-<version>-win64.exe` | Run it. Installs into Program Files with Start Menu and desktop shortcuts; the `.zip` beside it is the same application without an installer. |
| Windows 7 | `time-warp-studio-<version>-win64-win7.exe` | The same application against Qt 5.15, the last Qt that runs on Windows 7 (Qt 6 needs Windows 10). A `.zip` sits beside it. |
| macOS 14.4 or later, Intel or Apple silicon | `Time_Warp_Studio-<version>-macOS-universal.dmg` | Open it and drag **Time Warp Studio** to Applications. One image, both architectures. |
| a Linux desktop, no wish to install anything | `Time_Warp_Studio-<version>-x86_64.AppImage` | Make it executable and double-click it. No root, no package manager, no Qt to fetch. |
| Ubuntu, Debian, Mint or Pop!_OS | `time-warp-studio_<version>_amd64.deb`, or `_arm64.deb` | Double-click it, or `sudo apt install ./time-warp-studio_<version>_amd64.deb` |
| Fedora, RHEL or openSUSE | `time-warp-studio-<version>-1.x86_64.rpm`, or `.aarch64.rpm` | `sudo dnf install ./time-warp-studio-<version>-1.x86_64.rpm` |
| any other Linux, or arm64 | `time-warp-studio-<version>-linux-<arch>.tar.gz` | `tar -xzf … -C / --strip-components=1` |

On Linux, `uname -m` says which architecture yours is: `x86_64` or `aarch64`. Omit
the architecture from the file name above and the release assets fill it in. The
Windows installer and the macOS disk image are not signed with a paid certificate,
so each asks once — SmartScreen's "More info" and then "Run anyway" on Windows, and
right-click > Open on macOS. Full requirements, the container image and the
Windows 7 build are in [docs/INSTALLATION.md](docs/INSTALLATION.md); building from
source is still supported, and [native/README.md](native/README.md) describes it.

---

## Run from a checkout

```bash
./run.sh                                          # build if needed, then launch
./run.sh Examples/logo/mandala.logo               # open a program
./run.sh --compile Examples/basic/hello.bas --output hello
./run.sh --computer                               # the HB-1 home computer
./twc Examples/scheme/hello.scm -o hello-scheme   # a small standalone executable
./api                                             # JSON-RPC on stdin/stdout
```

`run.sh` builds incrementally and forwards every argument to the IDE, including
`--run`, `--screenshot`, `--export`, `--compile` and `--computer`. `--compile` produces
a copy of `time_warp_player` with the program inside, so the artifact a learner sends
to a friend opens as a program and not as an editor. `twc` and `api` are the terminal
compiler and the automation service; see [docs/USER_GUIDE.md](docs/USER_GUIDE.md).

---

## What else is in the workbench

Everything below is implemented, tested and documented, and the links go to the
manuals rather than to promises. The two machines are described above; this is the
rest of the environment, feature by feature.

| Capability | What it is | Read |
| --- | --- | --- |
| **Eleven languages, one editor** | BASIC, APL, Logo, PILOT, FOCAL, Scheme, CHIP-8, COMAL, REXX, Smalltalk and Forth, each running on a native engine in process — no Python at run time | [LANGUAGE_GUIDE](docs/LANGUAGE_GUIDE.md), [tutorials](docs/tutorials/) |
| **Editor and console** | Gutter, current line, syntax colouring, find and replace, **find in files**, **go to line**, **snippets**, **outline**, **format code**, **live lint**, **autosave**, **version history**, two panes | [USER_GUIDE](docs/USER_GUIDE.md#1-start-and-stop) |
| **Turtle canvas** | Live drawing while a program runs, zoom, pan, grid, auto-fit, turtle marker, **scrubber playback**, **animated GIF recording**, PNG/JPEG/BMP/SVG export | [USER_GUIDE](docs/USER_GUIDE.md#1-start-and-stop) |
| **Debugging, in all eleven** | Gutter breakpoints, Continue/Pause/Step In/Over/Out, **watch expressions**, **data breakpoints**, a Variables panel and a Telemetry HUD | [USER_GUIDE](docs/USER_GUIDE.md#5-debugging) |
| **REPL bar** | A persistent session in the current language; eight of the eleven keep bindings between entries, BASIC has `?`, numbered lines and `RUN`/`LIST`/`NEW` direct mode | [USER_GUIDE](docs/USER_GUIDE.md#3-the-repl-bar) |
| **Projects** | `.twproject` groups that organise programs without moving them on disk | [USER_GUIDE](docs/USER_GUIDE.md#2-projects-and-groups) |
| **SQL** | A SQLite Database tab, plus `SQL` statements in nine languages and through the API | [USER_GUIDE](docs/USER_GUIDE.md#6-sql-and-the-database-tab) |
| **Automation** | JSON-RPC over stdin/stdout, a standalone `twc` compiler, and scriptable screenshots and exports | [API](docs/API.md), [USER_GUIDE](docs/USER_GUIDE.md#7-terminal-tools) |
| **Appearance and display** | 35 themes, high contrast, 100/125/150% text, focus mode, CRT settings and four monitor presets, presentation mode | [USER_GUIDE](docs/USER_GUIDE.md#9-lessons-classroom-and-appearance) |
| **Help that covers the whole thing** | `F1` context help, `Ctrl+F1` keyword inspector, the documentation browser, a guided tour and a shortcut reference | [Help](#help-from-inside-the-studio) |
| **Lessons, if you teach** | 22 lessons and 66 checkpoints checked by running the program, plus rosters, handouts and marking | [lessons/](docs/lessons/README.md) |

### Eleven interpreters, one per era

Each of the eleven is a working implementation of its language's own shape rather
than a made-up dialect. The [Language Guide](docs/LANGUAGE_GUIDE.md) records what
each one supports, command by command, and every row of that reference is
exercised by the command matrix.

| Language | Paradigm | Era | Execution Mode |
| --- | --- | --- | --- |
| **BASIC** | Imperative / Educational | 1964 | Line-by-line |
| **APL** | Array / Notation as tool | 1966 | Line-by-line |
| **Logo** | Turtle Graphics / Educational | 1967 | Whole-program |
| **PILOT** | Computer-Aided Instruction | 1969 | Line-by-line |
| **FOCAL** | Formula calculator / Teaching | 1969 | Line-by-line |
| **Scheme** | Functional / Lisp dialect | 1975 | Whole-program |
| **CHIP-8** | Bytecode machine / Assembly | 1977 | Whole-program |
| **COMAL** | Structured BASIC / Educational | 1979 | Line-by-line |
| **REXX** | Scripting / String processing | 1979 | Line-by-line |
| **Smalltalk** | Object-oriented / Messages all the way down | 1980 | Whole-program |
| **Forth** | Stack / Words all the way down | 1970 | Whole-program |

### The IDE, in detail

- **Code editor** — line-number gutter, current-line highlight, 4-space tab stops, per-language file handling, per-language syntax colouring (comments, strings, numbers, keywords), and find/replace with go-to-line (F3 for the next match, Ctrl+L for a line number)
- **Documents** — tabs, per-file language detection, unsaved-change prompts, new/open/save/save-as, a recent-files menu, and the files from the last session reopened at launch
- **Find and navigate** — find, find and replace (`Ctrl+F`, `Ctrl+H`), next/previous (`F3`, `Shift+F3`), go to line (`Ctrl+L`), and **Find in Files** (`Ctrl+Shift+F`) across a folder or project
- **Command palette** — `Ctrl+Shift+P` or `F10` lists every action in the window, so a feature you cannot find in a menu is three letters away
- **Snippets** — `Ctrl+Shift+I` inserts a language-aware example from a catalogue the build installs (`share/time-warp-studio/snippets.json`; `TIME_WARP_SNIPPETS` points at your own)
- **Outline** — a dock listing every named place in the current program — procedures, labels, functions, methods — as a jump list
- **Format and lint** — `Ctrl+I` re-indents the document for its language, preserving line endings and blank lines, and the editor's lint runs as you type (Settings > Editor turns either off)
- **Autosave and version history** — snapshots on a timer you set, and **File > Version History…** (`Ctrl+Alt+V`) to read or restore any of them, including buffers that were never saved
- **Two panes** — **Split Editor Right** (`Ctrl+\`) shows two documents side by side; **Run Both Panes** (`Ctrl+Alt+B`) runs both headless and reports time, turtle calls and each program's own output in one table — a comparison, not a race
- **Output console** — program output as it is produced, with interpreter errors in their own tab
- **Run controls** — Run (F9) and Stop, with a status bar showing language, cursor position and execution state
- **REPL bar** — the line under the editor runs a command in the current document's language. BASIC, FOCAL, PILOT, COMAL, REXX, APL, Logo and Scheme keep bindings between commands (`LET X = 5` then `PRINT X`); Scheme also keeps definitions and echoes expression values. In BASIC, `?` is shorthand for `PRINT`, a numbered line is stored in the editor as a program line, and `RUN`, `LIST` and `NEW` work as direct-mode commands. Smalltalk, CHIP-8 and Forth run each submission as a whole program, so their state resets
- **Debugging, in all eleven languages** — gutter breakpoints and a 🐞 **Debug** panel (Continue, Pause, Step In/Over/Out, Stop) plus Debug/Continue/Trace on the toolbar. A run can stop at a statement, at a Logo top-level statement, at a Smalltalk message send or before a CHIP-8 instruction. Scheme runs on an explicit-stack machine, so it shows its own call stack and the paused frame's bindings, with values typed by the machine rather than guessed from their printed form; for a native run the Variables panel shows the bindings the pause stopped in. **Watch expressions** evaluate arithmetic over the paused bindings, and **data breakpoints** stop when a named variable changes value
- **Inspection** — a 🔍 **Variables** panel listing every binding with its type and value, filtered by name, and a ⚡ **Telemetry** HUD showing what the run cost and did: steps used, engine time, steps per second, last line, recursion depth, call stack, output lines, drawing commands, BASIC's memory window, and — beside the engine's own counters — the IDE's wall clock and the ink that actually reached the canvas
- **Projects** — a 🗂 **Projects** dock groups programs into named groups without moving them on disk. Create or open a `.twproject`, add files, a whole folder (its programs are grouped by subfolder), and groups, drag a file between groups, and the project reopens with the session; missing files are marked rather than forgotten
- **SQL database** — a 🗄 **Database** tab opens or creates a SQLite file, lists its tables, runs scripts and shows the result grid. A program reaches the same connection with `SQL SELECT ...` (BASIC, FOCAL, PILOT, COMAL, REXX and APL), `SQL ...` in Logo, `(sql "...")` in Scheme, and `SQL run: '...'` in Smalltalk
- **Turtle canvas** — the drawing appears as it is made, with zoom, pan, grid, auto-fit and a turtle marker; export it to PNG/JPEG/BMP (or SVG when the build has the Svg module) with Ctrl+E, or copy it to the clipboard, with or without the grid on screen. A **scrubber** replays the drawing stroke by stroke, and **Record Canvas as GIF…** captures it while it happens (still frames merge into one, so an idle minute is a pause, not a hundred pictures). More than fifty turtle and drawing commands are available across the languages — Logo, BASIC, COMAL, REXX, Scheme, FOCAL, APL and Smalltalk, with PILOT drawing through its `G:` commands and CHIP-8 painting rectangles — and the reference's conventions are kept: heading 0 is north and increases clockwise, and `circle` is a polygon walk. All of that is asserted in the tests
- **Printing and export** — File > Print Code (Ctrl+P) sends the program to a printer as the same A4 page the PDF export writes, and File > Print Drawing sends the drawing scaled to fit the page; both entries appear only in a build with the PrintSupport module. The program also exports as PDF, and the program, output and drawing as one HTML page; a lesson session exports as Markdown or PDF
- **Profiling** — View > Profile Run times every statement of a line-by-line program and ends with a report of line, hits, time and share, longest first. It is off at every launch because timing costs
- **Presentation and access** — 35 themes including the retro CRT sets and the Windows 2000 and Windows 7 schemes, a High Contrast variant of whichever is active, 100/125/150% text sizes, focus mode, CRT settings with four monitor presets, and **Presentation Mode** (`Shift+F11`, `Esc` to leave) for showing the screen to a room
- **Terminal (opt-in)** — Settings > Workspace adds a 💻 Terminal tab that runs commands through your own shell, starting in the current document's folder. It is off by default, stopped and removed in presentation and player modes, and is a command line rather than a terminal emulator: no prompts, no job control, no full-screen programs
- **Automation** — `./api` speaks the JSON-RPC API on stdin/stdout: run a program, keep a REPL session, start and step a debug session, query SQL, edit a project including folder import, and evaluate watches. See [docs/API.md](docs/API.md)
- **Terminal tools** — `./run.sh` builds if needed and launches the IDE; `./twc program.bas -o program` compiles a program into a standalone executable that embeds the interpreter, writes its drawing as SVG, and takes `--db FILE` when the program uses SQL
- **Native engines** — every language runs in-process in C++ with no Python at
  all. Scheme (`timewarp_scheme`) and CHIP-8, Logo and Smalltalk are
  whole-program; BASIC, FOCAL, PILOT, COMAL, APL and REXX are line-by-line on
  the shared `timewarp_lang` engine. The arithmetic each one evaluates goes
  through the native expression evaluator (`timewarp_expr`), and the drawing
  through the same canvas command queue the IDE drains (see
  [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md))

### What is still being built

The Qt IDE is the product, and the PySide6 application that preceded it has been
removed. The port is complete: every phase of
[PORTING_PLAN.md](docs/PORTING_PLAN.md) is implemented and has a test behind it.
What remains unbuilt is a much shorter list — a dedicated player target, an
automatic update check, code-signed Windows and macOS builds, and Flatpak or Snap
packaging. It is tracked in [docs/ROADMAP.md](docs/ROADMAP.md), which is worth a
look before a missing feature is reported as a regression.

### Help from inside the Studio

The application documents itself, starting from whatever you are looking at:

| Key / menu | What it does |
| --- | --- |
| `F1` | **Context Help** — the manual section for the panel, language or control with focus |
| `Ctrl+F1` | **Keyword Inspector** — the command reference entry for a word under the cursor |
| Help > **Documentation** | The documentation browser: every guide in `docs/`, indexed from [INDEX.md](docs/INDEX.md), including the [HB-1](docs/hb1/README.md) and [CHIP-8](docs/chip8/README.md) manuals |
| Help > **Getting Started** | The first-run path through the Studio |
| `F12` | The **Guided Tour**, a one-time walk through the window with a card on each part |
| Help > **Keyboard Shortcuts** | Every shortcut, in one dialog, generated from the actions themselves |
| `Ctrl+Shift+P` or `F10` | The **Command Palette** — every action by name, whether or not it is in a menu |

The browser reads the same Markdown files this repository ships, so the installed
help and the manuals in the tree are one set of documents.

### If you teach

The Studio carries a checked curriculum, though it does not require one:

- **22 lessons and 66 checkpoints**, two per language, each one checked by
  *running* the program rather than by comparing it with an answer: `PRINT 6 * 7`
  and `PRINT 42` are the same answer, and a drawing checkpoint cannot be passed by
  printing a description of a drawing.
- **In the IDE**, the 🎓 **Lessons** panel shows the curriculum, the current
  checkpoint, hints and a Check button.
- **At a terminal**, `time_warp_learn` walks a lesson and `time_warp_teach` writes lesson plans,
  answer keys, a class roster, handouts, marking and progress reports.
- **Self-checking**, because `time_warp_teach validate` runs all sixty-six solutions and
  refuses a starter that already passes its own checkpoint.

See [docs/lessons/](docs/lessons/README.md) for the student and teacher guides.

### What ships with it

- **63 example programs** across the eleven languages, including a **showcase** per
  language that walks its whole surface and ends in a drawing, and
  [`star_patrol.bas`](Examples/basic/star_patrol.bas) — a small game on a
  fixed-coordinate screen. They are worth reading as reference material as much as
  running as examples, and every one of them runs in the test gates.
- **Error hints**, which offer a syntax suggestion beside an interpreter's error
  message.
- **Eleven language tutorials**, one per language, under `docs/tutorials/`.
- **The machine manuals**: the HB-1's five books and the CHIP-8's three, with
  [the machines](docs/MACHINES.md) introducing them.

---

## About this repository

The description and topics below are the canonical ones for this project. GitHub's
**About** box is set in the repository settings rather than in a file, so this is
the copy to keep in step with the project — and the place to check when the two
have drifted apart.

### Description

```text
Old-school programming environment: a native C++20 and Qt workbench with eleven
interpreters from 1964-1980, a full IDE, a debugger in every language, and turtle
graphics -- plus two complete machines inside it: the HB-1, a bespoke 1982 home
computer from a parallel universe with an eleven-language ROM socket, a CRT, a prompt
and its own disk operating system, and a live CHIP-8 with an assembler, keypad and
register inspector.
```

### Topics

```text
retro-computing
emulator
home-computer
virtual-machine
chip8
ide
qt6
cpp
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
```

### Where to start

| Goal | Start here |
| ---- | ---------- |
| Type at a 1982 computer that never existed | [the HB-1 Owner's Manual](docs/hb1/README.md), [HB DOS](docs/hb1/hb-dos.md), and the premise behind both machines in [MACHINES.md](docs/MACHINES.md) |
| Write or run CHIP-8 | [the CHIP-8 manuals](docs/chip8/README.md), [the assembler](docs/chip8/assembly.md), [the instruction set](docs/chip8/instruction-set.md) |
| Explore the eleven languages | [`docs/LANGUAGE_GUIDE.md`](docs/LANGUAGE_GUIDE.md) and [`docs/tutorials/`](docs/tutorials/) |
| Use the editor, debugger, projects, SQL or the API | [`docs/USER_GUIDE.md`](docs/USER_GUIDE.md) |
| Build from source or port it | [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) and [`native/README.md`](native/README.md) |
| Teach with it | [`docs/lessons/README.md`](docs/lessons/README.md) |
| See what is not done yet | [`docs/ROADMAP.md`](docs/ROADMAP.md) |
| Report a bug or ask for a feature | The issue forms in [`.github/ISSUE_TEMPLATE/`](.github/ISSUE_TEMPLATE/) |

---

## Working with the project

- **[CONTRIBUTING.md](CONTRIBUTING.md)** — setup, the gates, and recipes for
  adding a language, a command and a lesson
- **[.github/SUPPORT.md](.github/SUPPORT.md)** — where to ask, and what is
  supported on which platform
- **[.github/pull_request_template.md](.github/pull_request_template.md)** — what
  a pull request is expected to answer
- **[SECURITY.md](SECURITY.md)** — how to report a vulnerability privately
- **[CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md)** — the standard expected here
- **[CHANGELOG.md](CHANGELOG.md)** — what changed, release by release

Releases carry a Windows installer and a portable ZIP, a second Windows installer
for Windows 7 built against Qt 5.15, a universal macOS disk image, and `.deb`,
`.rpm`, tarball and (x86_64) AppImage packages for Linux — all of them listed in
**Install it** above. Building from source is still the way to work on the
application itself, and [docs/INSTALLATION.md](docs/INSTALLATION.md) is the whole
procedure for every platform.
