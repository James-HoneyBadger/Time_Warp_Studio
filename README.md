# Time Warp Studio

*An old-school programming environment: ten languages, a full IDE and a turtle
canvas.*

*Timey wimey wibbly wobbly, time & multiuniverse travel computing.*

[![Languages](https://img.shields.io/badge/languages-10-blue)](#ten-interpreters-one-per-era)
[![Platform](https://img.shields.io/badge/platform-Linux-green)](docs/INSTALLATION.md)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Version](https://img.shields.io/badge/Version-28.0.0-orange)](https://github.com/James-HoneyBadger/Time_Warp_Studio)

Time Warp Studio is a desktop programming environment for people who like old
computers. It is the one the 1980s promised and never quite delivered: a full IDE,
ten interpreters spanning 1964 to 1980, a live turtle canvas, and a debugger that
works in every one of them.

Each language is a working implementation of its own shape rather than a dialect
invented for the occasion — BASIC with numbered lines and a direct mode, APL with
its own glyphs and no operator precedence, Logo's turtle, Forth's dictionary,
Scheme's lambda, Smalltalk's messages — and all of them run natively, in process,
with no interpreter of any kind underneath.

Time Warp Studio also carries a checked lesson curriculum and classroom tools for
people teaching with it, though nothing in the application assumes a classroom.

---

## For the retro-computing mind

The details that matter if you have ever owned one of these computers, or wished
you had:

- **The languages are the real ones, quirks included.** APL evaluates right to
  left with no precedence and has its own glyphs; FOCAL branches on a sign and
  numbers its lines in the text; PILOT is letter-coded units; Forth is a stack and
  a dictionary; Scheme is an explicit-stack machine. Nothing is a made-up dialect,
  and every supported command is in [the Language Guide](docs/LANGUAGE_GUIDE.md),
  with each row exercised by the command matrix.
- **The whole look, not a filter.** There are 35 IDE themes — C64, Apple II, ZX
  Spectrum, CGA, Amiga, NeXTSTEP, Borland and the two Windows schemes — plus CRT
  glass you can tune from off to heavy, and four monitor presets. Choosing amber
  recolours the header, the editor, the canvas and the glass together, because the
  palette is one table.
- **A debugger in every language, and a canvas that keeps up.** Breakpoints in the
  gutter, Continue/Pause/Step, watch expressions and data breakpoints, a Variables
  panel and a Telemetry HUD — and the drawing arriving live while the program runs,
  with playback, GIF recording and PNG/JPEG/BMP/SVG export.
- **Offline and self-contained.** No account and no telemetry, and nothing is sent
  unless you ask: the assistant is off until it is configured. The interpreters, the
  lesson checker, the classroom and the manuals are all part of the one
  application, so it works the same in a basement as on a network. The project is
  MIT-licensed, and the sources, the tests and the manuals are in the repository.

---

## 28.0: the IDE and the languages, and nothing else

**Version 28.0 is the current design.** Ten languages, one IDE, one turtle canvas,
one debugger, and the same gates proving all of it: the suites, the example corpus,
the command matrix, the frozen parity corpus, and the documentation checks. Every
language runs natively and in process.

**What 28.0 removed.** The three emulated machines are gone — the HB-1 home
computer, its 1992 successor the HB-2, and the CHIP-8 virtual machine — together
with the language that ran on it. They were a second product inside the first: a
bus, a video chip, a disk OS, a modem, a blitter, a cassette, a module bay and two
process schedulers, none of which made a program run better. What is left is what
the application is for: writing a program, running it, watching it draw, stopping
it, and reading why. The `SQL` statement, the turtle canvas, the debugger, the
lessons, the assistant and the automation surface are all unchanged.

**The releases behind it.** 27.0 and 26.x built the machines that 28.0 removes.
25.0 finished the IDE: the Terminal tab became a real pseudo-terminal, the assistant
gained three historical voices, a run that finishes behind you can announce itself on
the desktop, and the Flatpak carries its own Qt. **Version 24.0 is archived**: the
golden release, kept exactly as it was built on its own release page, never to change
again.

What comes next is written down with the proof each item would need rather than left
as a wish: [ROADMAP.md](docs/ROADMAP.md).

## Install it

You do not need a compiler. Every [release](https://github.com/James-HoneyBadger/Time_Warp_Studio/releases/latest)
carries a ready-built Linux application, with the Qt runtime inside it.

**Time Warp Studio is a Linux application.** Every artifact this project builds,
tests and ships is for Linux: there is no Windows build, no macOS build and no
installer for either. The [Installation Guide](docs/INSTALLATION.md) has the steps.

| You have | Take | Then |
| --- | --- | --- |
| a Linux desktop, no wish to install anything | `Time_Warp_Studio-<version>-x86_64.AppImage` | Make it executable and double-click it. No root, no package manager, no Qt to fetch. |
| any distribution, and you want it sandboxed with its own Qt | `Time_Warp_Studio-<version>-x86_64.flatpak` | `flatpak install ./Time_Warp_Studio-<version>-x86_64.flatpak`, then run **Time Warp Studio** from the menu. |
| Ubuntu, Debian, Mint or Pop!_OS | `time-warp-studio_<version>_amd64.deb`, or `_arm64.deb` | Double-click it, or `sudo apt install ./time-warp-studio_<version>_amd64.deb` |
| Fedora, RHEL or openSUSE | `time-warp-studio-<version>-1.x86_64.rpm`, or `.aarch64.rpm` | `sudo dnf install ./time-warp-studio-<version>-1.x86_64.rpm` |
| any other Linux, or arm64 | `time-warp-studio-<version>-linux-<arch>.tar.gz` | `tar -xzf … -C / --strip-components=1` |

`uname -m` says which architecture yours is: `x86_64` or `aarch64`. Omit the
architecture from the file name above and the release assets fill it in. Full
requirements and the container image are in
[docs/INSTALLATION.md](docs/INSTALLATION.md); building from source is still
supported, and [native/README.md](native/README.md) describes it.

---

## Run from a checkout

```bash
./run.sh                                          # build if needed, then launch
./run.sh Examples/logo/mandala.logo               # open a program
./run.sh --compile Examples/basic/hello.bas --output hello
./twc Examples/scheme/hello.scm -o hello-scheme   # a small standalone executable
./api                                             # JSON-RPC on stdin/stdout
```

`run.sh` builds incrementally and forwards every argument to the IDE, including
`--run`, `--screenshot`, `--export` and `--compile`. `--compile` produces
a copy of `time_warp_player` with the program inside, so the artifact a learner sends
to a friend opens as a program and not as an editor. `twc` and `api` are the terminal
compiler and the automation service; see [docs/USER_GUIDE.md](docs/USER_GUIDE.md).

---

## What else is in the workbench

Everything below is implemented, tested and documented, and the links go to the
manuals rather than to promises.

| Capability | What it is | Read |
| --- | --- | --- |
| **Ten languages, one editor** | BASIC, APL, Logo, PILOT, FOCAL, Scheme, COMAL, REXX, Smalltalk and Forth, each a working implementation of its own language rather than a dialect invented for the occasion | [LANGUAGE_GUIDE](docs/LANGUAGE_GUIDE.md), [tutorials](docs/tutorials/) |
| **Editor and console** | Gutter, current line, syntax colouring, find and replace, **find in files**, **go to line**, **snippets**, **outline**, **format code**, **live lint**, **autosave**, **version history**, two panes | [USER_GUIDE](docs/USER_GUIDE.md#1-start-and-stop) |
| **Turtle canvas** | Live drawing while a program runs, zoom, pan, grid, auto-fit, turtle marker, **scrubber playback**, **animated GIF recording**, PNG/JPEG/BMP/SVG export | [USER_GUIDE](docs/USER_GUIDE.md#1-start-and-stop) |
| **Debugging, in all ten** | Gutter breakpoints, Continue/Pause/Step In/Over/Out, **watch expressions**, **data breakpoints**, a Variables panel and a Telemetry HUD | [USER_GUIDE](docs/USER_GUIDE.md#4-debugging) |
| **REPL bar** | A persistent session in the current language; the line-by-line languages keep bindings between entries, and BASIC has `?`, numbered lines and `RUN`/`LIST`/`NEW` direct mode | [USER_GUIDE](docs/USER_GUIDE.md#3-the-repl-bar) |
| **Projects** | `.twproject` groups that organise programs without moving them on disk | [USER_GUIDE](docs/USER_GUIDE.md#2-projects-and-groups) |
| **SQL** | A SQLite Database tab, plus `SQL` statements in eight languages and through the API | [USER_GUIDE](docs/USER_GUIDE.md#5-sql-and-the-database-tab) |
| **Automation** | JSON-RPC over stdin/stdout, a standalone `twc` compiler, and scriptable screenshots and exports | [API](docs/API.md), [USER_GUIDE](docs/USER_GUIDE.md#7-terminal-tools) |
| **An AI assistant you configure** | Explain, fix, comment, translate or write a program through the service you choose — OpenAI, DeepSeek, Anthropic, or a model on this machine; off until you set it up, and the key need never touch the disk | [AI](docs/AI.md) |
| **Appearance and display** | 35 themes, high contrast, 100/125/150% text, focus mode, CRT settings and four monitor presets, presentation mode | [USER_GUIDE](docs/USER_GUIDE.md#10-lessons-classroom-and-appearance) |
| **Help that covers the whole thing** | `F1` context help, `Ctrl+F1` keyword inspector, the documentation browser, a guided tour and a shortcut reference | [Help](#help-from-inside-the-studio) |
| **Lessons, if you teach** | 20 lessons and 60 checkpoints checked by running the program, plus rosters, handouts and marking | [lessons/](docs/lessons/README.md) |

### Ten interpreters, one per era

Each of the ten is a working implementation of its language's own shape rather
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
| **Forth** | Stack / Words all the way down | 1970 | Whole-program |
| **Scheme** | Functional / Lisp dialect | 1975 | Whole-program |
| **COMAL** | Structured BASIC / Educational | 1979 | Line-by-line |
| **REXX** | Scripting / String processing | 1979 | Line-by-line |
| **Smalltalk** | Object-oriented / Messages all the way down | 1980 | Whole-program |

### The IDE, in detail

- **Code editor** — line-number gutter, current-line highlight, 4-space tab stops, per-language file handling, per-language syntax colouring (comments, strings, numbers, keywords), and find/replace with go-to-line (F3 for the next match, Ctrl+L for a line number)
- **Documents** — tabs, per-file language detection, unsaved-change prompts, new/open/save/save-as, a recent-files menu, and the files from the last session reopened at launch
- **Find and navigate** — find, find and replace (`Ctrl+F`, `Ctrl+H`), next/previous (`F3`, `Shift+F3`), go to line (`Ctrl+L`), and **Find in Files** (`Ctrl+Shift+F`) across a folder or project, including **Replace All** — which names the count and the files, leaves a document with unsaved changes alone, and reloads the open documents it did change
- **Command palette** — `Ctrl+Shift+P` or `F10` lists every action in the window, so a feature you cannot find in a menu is three letters away
- **Reference** — the 📖 **Reference** tab lists every command for the selected language with the smallest program that runs it and what it does, read from the same table `time_warp_gates commands` drives; **Try it** puts a row's program in a document
- **Snippets** — `Ctrl+Shift+I` inserts a language-aware example from a catalogue the build installs (`share/time-warp-studio/snippets.json`; `TIME_WARP_SNIPPETS` points at your own)
- **Outline** — a dock listing every named place in the current program — procedures, labels, functions, methods — as a jump list
- **Format and lint** — `Ctrl+I` re-indents the document for its language, preserving line endings and blank lines, and the editor's lint runs as you type (Settings > Editor turns either off)
- **Autosave and version history** — snapshots on a timer you set, and **File > Version History…** (`Ctrl+Alt+V`) to read or restore any of them, including buffers that were never saved, with **Compare with what is open** showing the choice as a diff against the editor
- **Two panes** — **Split Editor Right** (`Ctrl+\`) shows two documents side by side; **Run Both Panes** (`Ctrl+Alt+B`) runs both headless and reports time, turtle calls and each program's own output in one table — a comparison, not a race
- **Output console** — program output as it is produced, with interpreter errors in their own tab
- **Run controls** — Run (F9) and Stop, with a status bar showing language, cursor position and execution state
- **REPL bar** — the line under the editor runs a command in the current document's language. BASIC, FOCAL, PILOT, COMAL, REXX, APL, Logo and Scheme keep bindings between commands (`LET X = 5` then `PRINT X`); Scheme also keeps definitions and echoes expression values. In BASIC, `?` is shorthand for `PRINT`, a numbered line is stored in the editor as a program line, and `RUN`, `LIST` and `NEW` work as direct-mode commands. Smalltalk and Forth carry nothing from one submission to the next, and Logo keeps variables but not procedure definitions.
- **Debugging, in all ten languages** — gutter breakpoints and a 🐞 **Debug** panel (Continue, Pause, Step In/Over/Out, Stop) plus Debug/Continue/Trace on the toolbar. A run can stop at a statement, at a Logo top-level statement, at a Smalltalk message send or at a Forth token. Scheme runs on an explicit-stack machine, so it shows its own call stack and the paused frame's bindings, with values typed by the machine rather than guessed from their printed form; for a native run the Variables panel shows the bindings the pause stopped in. **Watch expressions** evaluate arithmetic over the paused bindings, and **data breakpoints** stop when a named variable changes value
- **Inspection** — a 🔍 **Variables** panel listing every binding with its type and value, filtered by name, and a ⚡ **Telemetry** HUD showing what the run cost and did: steps used, engine time, steps per second, last line, recursion depth, call stack, output lines, drawing commands, BASIC's memory window, and — beside the engine's own counters — the IDE's wall clock and the ink that actually reached the canvas
- **Projects** — a 🗂 **Projects** dock groups programs into named groups without moving them on disk. Create or open a `.twproject`, add files, a whole folder (its programs are grouped by subfolder), and groups, drag a file between groups, and the project reopens with the session; missing files are marked rather than forgotten
- **SQL database** — a 🗄 **Database** tab opens or creates a SQLite file, lists its tables, runs scripts and shows the result grid. A program reaches the same connection with `SQL SELECT ...` (BASIC, FOCAL, PILOT, COMAL, REXX and APL), `SQL ...` in Logo, `(sql "...")` in Scheme, and `SQL run: '...'` in Smalltalk
- **Turtle canvas** — the drawing appears as it is made, with zoom, pan, grid, auto-fit and a turtle marker; export it to PNG/JPEG/BMP (or SVG when the build has the Svg module) with Ctrl+E, or copy it to the clipboard, with or without the grid on screen. A **scrubber** replays the drawing stroke by stroke, and **Record Canvas as GIF…** captures it while it happens (still frames merge into one, so an idle minute is a pause, not a hundred pictures). More than fifty turtle and drawing commands are available across the languages — Logo, BASIC, COMAL, REXX, Scheme, FOCAL, APL and Smalltalk, with PILOT drawing through its `G:` commands and Forth through `FD`/`RT`/`LT` and the rest — and the reference's conventions are kept: heading 0 is north and increases clockwise, and `circle` is a polygon walk. All of that is asserted in the tests
- **Printing and export** — File > Print Code (Ctrl+P) sends the program to a printer as the same A4 page the PDF export writes, and File > Print Drawing sends the drawing scaled to fit the page; both entries appear only in a build with the PrintSupport module. The program also exports as PDF, the program, output and drawing as one HTML page, and **File > Export Audio as WAV…** writes what the run sounded — every `SOUND` and `PLAY` the engines synthesised, in the order the program asked for them — which is one file a machine with no audio player can still keep; a lesson session exports as Markdown or PDF
- **Profiling** — View > Profile Run times every statement of a line-by-line program and ends with a report of line, hits, time and share, longest first. It is off at every launch because timing costs
- **Presentation and access** — 35 themes including the retro CRT sets and the Windows 2000 and Windows 7 schemes, a High Contrast variant of whichever is active, 100/125/150% text sizes, focus mode, CRT settings with four monitor presets, and **Presentation Mode** (`Shift+F11`, `Esc` to leave) for showing the screen to a room
- **AI assistant (opt-in)** — the 🤖 **Assistant** tab explains a program, finds what is wrong with it, comments it, translates it or writes a new one, through the service you configure in **Tools > AI Settings…**, with **Ctrl+Shift+A** raising the page. OpenAI, DeepSeek, Anthropic, OpenRouter, or Ollama and LM Studio on this machine; answers stream in token by token and **Stop** ends one half written; **Insert** puts the answer into the editor and nothing runs until you press F9. **Ask About the Last Error** starts from what the run said, a suggested next line can appear while you type (off unless ticked), a conversation can be kept between sessions, and a stored key goes in the platform's own store where there is one. [The manual](docs/AI.md) says exactly what leaves the machine.
- **Terminal (opt-in)** — Settings > Workspace adds a 💻 Terminal tab that runs commands through your own shell, starting in the current document's folder. It is off by default, stopped and removed in presentation and player modes, and is a command line rather than a terminal emulator: no prompts, no job control, no full-screen programs
- **Automation** — `./api` speaks the JSON-RPC API on stdin/stdout: run a program, keep a REPL session, start and step a debug session, query SQL, edit a project including folder import, and evaluate watches. See [docs/API.md](docs/API.md)
- **Terminal tools** — `./run.sh` builds if needed and launches the IDE; `./twc program.bas -o program` compiles a program into a standalone executable that embeds the interpreter, writes its drawing as SVG, and takes `--db FILE` when the program uses SQL
- **Everything built in** — each language runs inside the application, so a
  program starts immediately, the debugger can stop anywhere in it, and running
  the ten needs nothing installed beside the download. How the engines are
  divided up is the [architecture guide](docs/ARCHITECTURE.md)'s subject

### What is still being built

What remains unbuilt is a short list — an automatic update check, Snap packaging, an
arm64 AppImage, a static build and automatic publishing to a distribution repository.
It is tracked in
[docs/ROADMAP.md](docs/ROADMAP.md), which is worth a look before a missing feature is
reported as a regression, and each item is registered in
[`native/share/unbuilt.txt`](native/share/unbuilt.txt) so that shipping one without
correcting these pages fails `make doc-claims` rather than leaving a stale sentence
behind.

### Help from inside the Studio

The application documents itself, starting from whatever you are looking at:

| Key / menu | What it does |
| --- | --- |
| `F1` | **Context Help** — the manual section for the panel, language or control with focus |
| `Ctrl+F1` | **Keyword Inspector** — the command reference entry for a word under the cursor |
| Help > **Documentation** | The documentation browser: every guide in `docs/`, indexed from [INDEX.md](docs/INDEX.md) |
| Help > **Getting Started** | The first-run path through the Studio |
| `F12` | The **Guided Tour**, a one-time walk through the window with a card on each part |
| Help > **Keyboard Shortcuts** | Every shortcut, in one dialog, generated from the actions themselves |
| `Ctrl+Shift+P` or `F10` | The **Command Palette** — every action by name, whether or not it is in a menu |

The browser reads the same Markdown files this repository ships, so the installed
help and the manuals in the tree are one set of documents.

### If you teach

The Studio carries a checked curriculum, though it does not require one:

- **20 lessons and 60 checkpoints**, two per language, each one checked by
  *running* the program rather than by comparing it with an answer: `PRINT 6 * 7`
  and `PRINT 42` are the same answer, and a drawing checkpoint cannot be passed by
  printing a description of a drawing.
- **In the IDE**, the 🎓 **Lessons** panel shows the curriculum, the current
  checkpoint, hints and a Check button.
- **At a terminal**, `time_warp_learn` walks a lesson and `time_warp_teach` writes lesson plans,
  answer keys, a class roster, handouts, marking and progress reports.
- **Self-checking**, because `time_warp_teach validate` runs all sixty solutions and
  refuses a starter that already passes its own checkpoint.

See [docs/lessons/](docs/lessons/README.md) for the student and teacher guides.

### What ships with it

- **68 example programs** across the ten languages, including a **showcase** per
  language that walks its whole surface and ends in a drawing, and
  [`star_patrol.bas`](Examples/basic/star_patrol.bas) — a small game on a
  fixed-coordinate screen. They are worth reading as reference material as much as
  running as examples, and every one of them runs in the test gates.
  **File > Examples Gallery…** shows every one of them with the drawing it makes as
  its face: each thumbnail is the program run headlessly under the engine's own step
  budget, so the gallery is a picture of the shipped programs rather than a list of
  names. A program that only prints gets a card naming its language.
- **Notebooks** — **File > New Notebook…** opens prose and programs in one `.twnb`
  file: a cell is either text or a program with its language, **Run** shows its output
  and its drawing beneath it, and **Run All** walks the file. Cells do not share
  variables; a notebook is a document, not a session.
- **Error hints**, which offer a syntax suggestion beside an interpreter's error
  message.
- **Ten language tutorials**, one per language, under `docs/tutorials/`.

---

## About this repository

The description and topics below are the canonical ones for this project. GitHub's
**About** box is set in the repository settings rather than in a file, so this is
the copy to keep in step with the project — and the place to check when the two
have drifted apart.

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

### Where to start

| Goal | Start here |
| ---- | ---------- |
| Explore the ten languages | [`docs/LANGUAGE_GUIDE.md`](docs/LANGUAGE_GUIDE.md) and [`docs/tutorials/`](docs/tutorials/) |
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
  supported
- **[.github/pull_request_template.md](.github/pull_request_template.md)** — what
  a pull request is expected to answer
- **[SECURITY.md](SECURITY.md)** — how to report a vulnerability privately
- **[CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md)** — the standard expected here
- **[CHANGELOG.md](CHANGELOG.md)** — what changed, release by release

Releases carry `.deb`, `.rpm`, tarball and AppImage packages for Linux, in x86_64
and arm64, plus a `.flatpak` bundle — all of them listed in **Install it** above. Building from source is
still the way to work on the application itself, and
[docs/INSTALLATION.md](docs/INSTALLATION.md) is the whole procedure.
