# Time Warp Studio

A desktop programming environment for ten interpreters of the 1964–1980 period, with
one editor, one debugger and a live turtle canvas.

[![Languages](https://img.shields.io/badge/languages-10-blue)](#ten-interpreters-one-per-era)
[![Platform](https://img.shields.io/badge/platform-Linux-green)](docs/INSTALLATION.md)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Version](https://img.shields.io/badge/Version-28.1.0-orange)](https://github.com/James-HoneyBadger/Time_Warp_Studio)

Time Warp Studio is a C++20 / Qt 6 application for Linux. Ten interpreters are linked
into it as libraries: BASIC, APL, Logo, PILOT, FOCAL, Forth, Scheme, COMAL, REXX and
Smalltalk. There is no interpreter to install, no emulated machine underneath, and no
process boundary between the editor and a running program.

Each language is an implementation of its own dialect rather than a common core with
different keywords. FOCAL is a formula calculator with line labels and sign branching.
PILOT is letter-coded units. APL has its own glyphs and no operator precedence. Forth
is a dictionary of words. Scheme is an explicit-stack CEK machine with no native
recursion. Smalltalk is a message-sending system with blocks and a class library. The
differences are the point: a program written for 1975 runs the way it was written, and
the [language guide](docs/LANGUAGE_GUIDE.md) records every supported command.

The editor, debugger, canvas, project model and SQL layer are shared. A run stops at a
statement in BASIC and at a message send in Smalltalk, but it is the same panel, the
same Variables table and the same Continue button in both.

---

## Install

Every [release](https://github.com/James-HoneyBadger/Time_Warp_Studio/releases/latest)
carries a built Linux application with the Qt runtime inside it. A compiler is not
required.

**Time Warp Studio is a Linux application.** Every artifact this project builds, tests
and ships is for Linux; there is no Windows build, no macOS build and no installer for
either. The [installation guide](docs/INSTALLATION.md) has the full procedure and the
build requirements.

| You have | Take | Then |
| --- | --- | --- |
| a Linux desktop, no wish to install anything | `Time_Warp_Studio-<version>-x86_64.AppImage` | Make it executable and run it. No root, no package manager, no Qt to fetch. |
| any distribution, and you want it sandboxed with its own Qt | `Time_Warp_Studio-<version>-x86_64.flatpak` | `flatpak install ./Time_Warp_Studio-<version>-x86_64.flatpak`, then run **Time Warp Studio** from the menu. |
| Ubuntu, Debian, Mint or Pop!_OS | `time-warp-studio_<version>_amd64.deb`, or `_arm64.deb` | Double-click it, or `sudo apt install ./time-warp-studio_<version>_amd64.deb` |
| Fedora, RHEL or openSUSE | `time-warp-studio-<version>-1.x86_64.rpm`, or `.aarch64.rpm` | `sudo dnf install ./time-warp-studio-<version>-1.x86_64.rpm` |
| any other Linux, or arm64 | `time-warp-studio-<version>-linux-<arch>.tar.gz` | `tar -xzf … -C / --strip-components=1` |

`uname -m` reports the architecture: `x86_64` or `aarch64`. Omit it from the file name
and the release assets resolve it. Building from source is supported and documented in
[native/README.md](native/README.md).

---

## Run from a checkout

```bash
./run.sh                                          # build if needed, then launch
./run.sh Examples/logo/mandala.logo               # open a program
./run.sh --compile Examples/basic/hello.bas --output hello
./twc Examples/scheme/hello.scm -o hello-scheme   # a small standalone executable
./api                                             # JSON-RPC on stdin/stdout
```

`run.sh` builds incrementally and forwards its arguments to the IDE, including `--run`,
`--screenshot`, `--export` and `--compile`. `--compile` writes a copy of
`time_warp_player` with the program embedded, so the artifact runs as a program rather
than opening as an editor. `twc` is the terminal compiler and `api` the automation
service; both are described in the [user guide](docs/USER_GUIDE.md).

---

## What else is in the workbench

Everything in this table is implemented, covered by a test or a gate, and documented in
the manual named beside it.

| Capability | What it is | Read |
| --- | --- | --- |
| **Ten languages, one editor** | BASIC, APL, Logo, PILOT, FOCAL, Scheme, COMAL, REXX, Smalltalk and Forth, each a native implementation of its own dialect | [LANGUAGE_GUIDE](docs/LANGUAGE_GUIDE.md), [tutorials](docs/tutorials/) |
| **Editor and console** | Gutter, current line, syntax colouring, find and replace, find in files, go to line, snippets, outline, format code, live lint, autosave, version history, two panes | [USER_GUIDE](docs/USER_GUIDE.md#1-start-and-stop) |
| **Turtle canvas** | Drawing as it is made, zoom, pan, grid, auto-fit, turtle marker, scrubber playback, animated GIF recording, PNG/JPEG/BMP/SVG export | [USER_GUIDE](docs/USER_GUIDE.md#1-start-and-stop) |
| **Debugging, in all ten** | Gutter breakpoints, continue/pause/step, watch expressions, data breakpoints, a Variables panel and a telemetry HUD | [USER_GUIDE](docs/USER_GUIDE.md#4-debugging) |
| **REPL bar** | A persistent session in the current language; the line-by-line languages keep bindings, and BASIC has `?`, numbered lines and `RUN`/`LIST`/`NEW` direct mode | [USER_GUIDE](docs/USER_GUIDE.md#3-the-repl-bar) |
| **Projects** | `.twproject` groups that organise programs without moving them on disk | [USER_GUIDE](docs/USER_GUIDE.md#2-projects-and-groups) |
| **SQL** | A SQLite Database tab, and a `SQL` statement in eight languages and over the API | [USER_GUIDE](docs/USER_GUIDE.md#5-sql-and-the-database-tab) |
| **Automation** | JSON-RPC over stdin/stdout, a standalone `twc` compiler, scriptable screenshots and exports | [API](docs/API.md), [USER_GUIDE](docs/USER_GUIDE.md#7-terminal-tools) |
| **An assistant you configure** | Explain, fix, comment, translate or write a program through the service you choose, or a model on this machine; off until configured, and the key need never touch the disk | [AI](docs/AI.md) |
| **Appearance and display** | 35 themes, a high-contrast variant, 100/125/150% text, focus mode, CRT settings with four monitor presets, presentation mode | [USER_GUIDE](docs/USER_GUIDE.md#10-lessons-classroom-and-appearance) |
| **Documentation in the application** | `F1` context help, a keyword inspector, the documentation browser, a guided tour and a shortcut reference | [Help](#help-from-inside-the-studio) |
| **Lessons** | 20 lessons and 60 checkpoints, checked by running the program, plus rosters, handouts and marking | [lessons/](docs/lessons/README.md) |

### Ten interpreters, one per era

The [language guide](docs/LANGUAGE_GUIDE.md) records what each language supports,
command by command, and every row of that reference is exercised by the command matrix
on each push.

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

The execution column is a real distinction, not a label. A line-by-line language is
called once per statement by the shared loop, so it can be paused between statements,
profiled per line and walked backwards. A whole-program language runs its own loop, so
it decides where a pause may land: the end of a Logo statement, a Smalltalk message send,
a Forth token.

### The IDE

- **Code editor** — line-number gutter, current-line highlight, four-space tab stops,
  per-language syntax colouring (comments, strings, numbers, keywords), find and replace,
  go-to-line.
- **Documents** — tabs, language by extension, unsaved-change prompts, a recent-files
  menu, and the previous session's files reopened at launch.
- **Navigation** — find and replace (`Ctrl+F`, `Ctrl+H`), next and previous match (`F3`,
  `Shift+F3`), go to line (`Ctrl+L`), and find in files (`Ctrl+Shift+F`) with **Replace
  All**, which names the count and the files, leaves a modified document alone, and
  reloads every open document it changed.
- **Command palette** — `Ctrl+Shift+P` or `F10` lists every action in the window.
- **Reference** — the 📖 Reference tab lists the commands of the current language with
  the smallest program that runs each, read from the same table `time_warp_gates
  commands` drives. **Try it** opens a row's program as a document.
- **Snippets** — `Ctrl+Shift+I` inserts a language-aware example from a catalogue the
  build installs (`share/time-warp-studio/snippets.json`; `TIME_WARP_SNIPPETS` points
  at your own).
- **Outline** — a dock listing every named place in the program: procedures, labels,
  functions, methods.
- **Formatting and linting** — `Ctrl+I` re-indents for the current language, preserving
  line endings and blank lines; lint findings appear as you type. Either can be turned
  off in Settings.
- **Autosave and version history** — snapshots on a configurable timer, and **File >
  Version History…** (`Ctrl+Alt+V`) to read or restore one, including buffers that were
  never saved. **Compare with what is open** shows the choice as a diff.
- **Two panes** — **Split Editor Right** (`Ctrl+\`) for two documents side by side;
  **Run Both Panes** (`Ctrl+Alt+B`) runs both headlessly and reports time, turtle calls
  and output in one table.
- **Console** — program output as it is produced, with interpreter errors in their own
  tab.
- **Run controls** — Run (`F9`) and Stop, with language, cursor position and execution
  state in the status bar.
- **REPL bar** — the line under the editor runs one command in the current document's
  language. BASIC, FOCAL, PILOT, COMAL, REXX, APL, Logo and Scheme keep bindings between
  commands; Scheme also keeps definitions and echoes expression values. In BASIC, `?` is
  shorthand for `PRINT`, a numbered line is stored in the editor as a program line, and
  `RUN`, `LIST` and `NEW` work as direct-mode commands. Smalltalk and Forth carry nothing
  from one entry to the next, and Logo keeps variables but not procedure definitions.
- **Debugging** — gutter breakpoints, a 🐞 Debug panel (continue, pause, step in/over/out,
  stop), and Debug/Continue/Trace on the toolbar. A run can stop at a statement, at a Logo
  top-level statement, at a Smalltalk message send or at a Forth token. Scheme runs on its
  own machine and reports a call stack and frame bindings.
- **Inspection** — a 🔍 Variables panel listing every visible binding with its type and
  value, filtered by name, and a ⚡ Telemetry HUD reporting what the run cost: steps used,
  engine time, steps per second, last line, recursion depth, call stack, output lines,
  drawing commands, BASIC's memory window, and, beside the engine's counters, the IDE's
  wall clock and the ink that reached the canvas.
- **Projects** — a 🗂 Projects dock grouping programs without moving them on disk. Create
  or open a `.twproject`, add files, add a folder (grouped by subfolder), add groups, drag
  a file between groups; the project reopens with the session, and missing files are
  marked rather than forgotten.
- **SQL** — a 🗄 Database tab opens or creates a SQLite file, lists its tables, runs
  scripts and shows the result grid. A program reaches the same connection with `SQL
  SELECT …` (BASIC, FOCAL, PILOT, COMAL, REXX and APL), `SQL …` in Logo, `(sql "…")` in
  Scheme, and `SQL run: '…'` in Smalltalk.
- **Turtle canvas** — the drawing appears as it is made, with zoom, pan, grid, auto-fit
  and a turtle marker. Export to PNG, JPEG or BMP (SVG when the build has Qt's Svg
  module) with `Ctrl+E`, or copy it to the clipboard. A scrubber replays the drawing
  stroke by stroke, and **Record Canvas as GIF…** captures it as it happens, merging
  unchanged frames into longer delay rather than writing a hundred identical ones. More
  than fifty turtle and drawing commands are available across the languages; the
  conventions are the reference's, and the tests assert them — heading 0 is north and
  increases clockwise, and `circle` is a polygon walk.
- **Printing and export** — **File > Print Code** (`Ctrl+P`) and **File > Print Drawing**
  through Qt's PrintSupport module, which is optional; a build without it hides both
  entries. The program exports as PDF, the program with its output and drawing as one
  HTML page, a lesson session as Markdown or PDF, and **File > Export Audio as WAV…**
  writes every `SOUND` and `PLAY` the run synthesised, in order.
- **Profiling** — **View > Profile Run** times every statement of a line-by-line program
  and ends with a report of line, hits, time and share, longest first. It is off at
  launch, because reading a clock around every statement costs.
- **Appearance** — 35 themes including the retro CRT sets, a high-contrast variant of
  whichever is active, text at 100%, 125% or 150%, focus mode, CRT settings with four
  monitor presets, and **Presentation Mode** (`Shift+F11`, `Esc` to leave).
- **Assistant (opt-in)** — the 🤖 Assistant tab explains, fixes, comments, translates or
  writes a program through the service configured in **Tools > AI Settings…**
  (`Ctrl+Shift+A`). OpenAI, DeepSeek, Anthropic, OpenRouter, or a model on this machine.
  Answers stream, **Stop** ends one part-written, **Insert** puts the answer into the
  editor, and nothing runs until `F9`. [The manual](docs/AI.md) states exactly what is
  sent.
- **Terminal (opt-in)** — **Settings > Workspace > Enable the embedded terminal** adds a
  💻 Terminal tab running your own shell on a pseudo-terminal, starting in the current
  document's folder. It is off by default and absent in presentation and player modes.
  The tab is a text transcript, so a program that paints with cursor addressing has its
  escape sequences stripped rather than drawn.
- **Automation** — `./api` speaks JSON-RPC on stdin/stdout: run a program, keep a REPL
  session, start and step a debug session, query SQL, edit a project including folder
  import, evaluate watches. See [docs/API.md](docs/API.md).
- **Terminal tools** — `./twc program.bas -o program` compiles a program into a
  standalone executable that embeds the engine, can write its drawing as SVG, and takes
  `--db FILE` when the program uses SQL.
- **Everything in process** — a program starts immediately, the debugger can stop
  anywhere in it, and running any of the ten needs nothing installed beside the download.
  How the engines are divided up is the [architecture guide](docs/ARCHITECTURE.md).

### Not built yet

The unbuilt list is short: an automatic update check, Snap packaging, an arm64 AppImage,
a fully static build, and automatic publishing to a distribution repository. Each item is
recorded in [docs/ROADMAP.md](docs/ROADMAP.md), which is worth reading before reporting a
missing feature as a regression, and registered in
[`native/share/unbuilt.txt`](native/share/unbuilt.txt) so that shipping one without
correcting these pages fails `make doc-claims`.

### Help from inside the Studio

| Key / menu | What it does |
| --- | --- |
| `F1` | **Context Help** — the manual section for the panel, language or control with focus |
| `Ctrl+F1` | **Keyword Inspector** — the command reference entry for the word under the cursor |
| Help > **Documentation** | The documentation browser: every guide in `docs/`, indexed from [INDEX.md](docs/INDEX.md) |
| Help > **Getting Started** | The first-run path through the Studio |
| `F12` | The **Guided Tour**, a one-time walk through the window with a card on each part |
| Help > **Keyboard Shortcuts** | Every shortcut in one dialog, generated from the actions themselves |
| `Ctrl+Shift+P` or `F10` | The **Command Palette** — every action by name, whether or not it is in a menu |

The browser reads the Markdown files this repository ships, so the installed help and the
manuals in the tree are one set of documents.

### Teaching with it

A checked curriculum ships with the application; nothing in the application assumes it
will be used for teaching.

- **20 lessons and 60 checkpoints**, two per language, each checked by *running* the
  learner's program rather than by comparing it with an answer: `PRINT 6 * 7` and `PRINT
  42` pass the same checkpoint, and a drawing checkpoint cannot be satisfied by printing a
  description of a drawing.
- **In the IDE**, the 🎓 Lessons panel shows the curriculum, the current checkpoint,
  hints and a Check button.
- **At a terminal**, `time_warp_learn` walks a lesson and `time_warp_teach` writes lesson
  plans, answer keys, a class roster, handouts, marking and progress reports.
- **Self-checking**: `time_warp_teach validate` runs all sixty solutions and refuses a
  starter that already passes its own checkpoint.

See [docs/lessons/](docs/lessons/README.md) for the student and teacher guides.

### What ships with it

- **68 example programs** across the ten languages, including one showcase per language
  that exercises its whole surface and ends in a drawing, and
  [`star_patrol.bas`](Examples/basic/star_patrol.bas), a small game on a
  fixed-coordinate screen. Every one of them runs in the test gates. **File > Examples
  Gallery…** shows each with the drawing it makes as its face: the thumbnail is the
  program run headlessly under the engine's step budget, so the gallery is a picture of
  the shipped programs rather than a list of names. A program that only prints gets a
  card naming its language.
- **Notebooks** — **File > New Notebook…** opens prose and programs in one `.twnb` file.
  A cell is either text or a program with a language; **Run** shows its output and its
  drawing beneath it, and **Run All** walks the file. Cells do not share variables: a
  notebook is a document, not a session.
- **Error hints**, which propose a correction beside an interpreter's error message.
- **Ten tutorials**, one per language, under `docs/tutorials/`.

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

- **[CONTRIBUTING.md](CONTRIBUTING.md)** — setup, the gates, and recipes for adding a
  language, a command and a lesson
- **[.github/SUPPORT.md](.github/SUPPORT.md)** — where to ask, and what is supported
- **[.github/pull_request_template.md](.github/pull_request_template.md)** — what a pull
  request is expected to answer
- **[SECURITY.md](SECURITY.md)** — how to report a vulnerability privately
- **[CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md)** — the standard expected here
- **[CHANGELOG.md](CHANGELOG.md)** — what changed, release by release

Releases carry `.deb`, `.rpm`, tarball and AppImage packages for Linux in x86_64 and
arm64, plus a `.flatpak` bundle, all listed under **Install** above. Building from source
is the way to work on the application itself; [docs/INSTALLATION.md](docs/INSTALLATION.md)
is the whole procedure.
