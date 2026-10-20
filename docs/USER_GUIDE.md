# Time Warp Studio — User Guide

This is the manual for the Qt application: how to open it, organise work, try
commands without writing a program, debug, use SQL, and drive the Studio from a
script or a terminal. The per-language command reference is
[LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md), and the tutorials start at
[tutorials/](tutorials/).

> **First time here?** Run `./run.sh`, open
> [`Examples/logo/hello.logo`](../Examples/logo/hello.logo) and press **F9**.

---

## 1. Start and stop

```bash
./run.sh                                      # build if needed, then launch
./run.sh Examples/logo/mandala.logo           # open a program
./run.sh --run Examples/basic/hello.bas       # open and run it
./run.sh --screenshot window.png --run file   # render the window and exit
./run.sh --compile program.bas --output hello # build a standalone artifact
```

`run.sh` builds incrementally with CMake and then replaces itself with the IDE, so
its arguments and exit status are the application's. `TIME_WARP_BUILD_DIR` selects
another build tree (a relative path is resolved from the repository root), and
`TIME_WARP_SKIP_BUILD=1` launches an existing build without asking CMake.

Running from a release package needs none of this: launch **Time Warp Studio**
from the desktop menu, or run the `time_warp_ide` binary directly.

### The window

- **Editor** — tabs, line numbers, current-line highlight, syntax colouring,
  find/replace, find in files, go-to-line, formatting, a minimap and an outline dock.
- **Output and Graphics** — the console, the error list, the turtle log and the
  canvas. A program that prints and draws keeps both in view.
- **Right-column tabs** — Variables, Telemetry HUD, Debug, Lessons, Classroom and
  Database.
- **Projects dock** — the project tree (View → Project Panel, `Ctrl+Shift+E`).
- **REPL bar** — one command at a time under the editor.
- **Status bar** — language, cursor position, run state and the document name.

### Run, stop and input

| Action | How |
| --- | --- |
| Run | **F9** or the ▶ Run toolbar button |
| Run only the selection | `Ctrl+F9` |
| Stop | `Shift+F9` or the Stop button |
| Reset the canvas and output | `Ctrl+Shift+F9` |
| Answer an input prompt | Type it into the dialog and press Return |
| Open a file | **File → Open** (`Ctrl+O`) or pass it on the command line |

The engine stops a runaway program after its own step and time budget; Stop ends
it sooner, at the next statement or instruction.

---

## 2. Projects and groups

A project is a **view over files that already exist**, not a copy of them. It
records a root folder, named groups and the programs in them. Moving a program
between groups never moves it on disk.

### Create or open

1. **Project → New Project...** (`Ctrl+Shift+N`) names the project and chooses its
   root folder. The `.twproject` file is saved beside the programs by default.
2. **Project → Open Project...** (`Ctrl+Shift+O`) reopens one. Recent projects are
   on the same menu.
3. The active project and the recent list reopen with the session.

### Populate it

- **Add Current File to Project** adds the document in front.
- **Add Files to Project...** adds one or more programs.
- **Add Folder to Project...** walks a folder recursively, adds every recognised
  program and files each one under a group named after its subfolder. When the
  folder is outside the project root, the folder's own name becomes the prefix,
  so adding a `class` folder produces groups such as `class/Art`.
- **Add Files** in the panel's context menu, or drag files from the desktop, adds
  them too.

### Organise it

- **New Group...** (`Ctrl+Shift+G`) creates a group such as *Turtle art*, *Loops*
  or *Friday's class*.
- Drag a program onto a group to move it, or onto the project to ungroup it.
  The same moves are in the file's context menu.
- Rename and remove groups from the context menu. Removing a group returns its
  programs to the ungrouped section; it never deletes a file.
- **Reveal Project Folder** opens the root in the file manager.
- A program that has been moved or deleted is marked **missing**, not forgotten.
  Put it back, or remove the entry.

### The file on disk

`.twproject` is a small JSON document. Paths inside the project root are stored
relative to it, so copying the whole folder keeps the project working; files
outside the root stay absolute. Saves are atomic (`QSaveFile`), and a project file
from a newer format is refused instead of being half-read.

Paths can also be managed through the API's `project.*` methods, including
`project.addFolder`. See [API.md](API.md#projectcreate-projectopen-projectsave-projectinfo).

---

## 3. The REPL bar

The REPL line under the editor submits one entry to the current document's
language. It uses the same interpreters as Run, runs off the GUI thread, and sends
drawing through the same canvas queue.

| Language | State between entries |
| --- | --- |
| BASIC, FOCAL, PILOT, COMAL, REXX | Variables and evaluator state persist. `LET X = 5` then `PRINT X` gives 5. |
| APL | Variables and `∇` function definitions persist. |
| Logo | `MAKE` variables persist; `TO ... END` procedure definitions do not, because each entry is run as a whole program. |
| Scheme | The machine persists, so `define` and `set!` survive and expression values are echoed. |
| Smalltalk | Each entry is a complete program; top-level variables, classes and methods do not carry over. |
| CHIP-8 | Each entry is an assembly listing that runs as a whole program; memory and registers reset. |

Practical notes:

- The line clears as soon as a command is handed to the engine.
BASIC also has the classic direct-mode behaviour:
- A line that starts with a number is **stored in the editor** as a program line
  instead of being executed. `10 PRINT "HI"` adds line 10; a later line with the
  same number replaces it; a bare `10` deletes it. Lines are kept in numeric
  order.
- `?` is shorthand for `PRINT`, both in the REPL and in program text:
  `? 6 * 7` prints 42.
- `LIST` prints the current program, `RUN` executes it, and `NEW` clears the
  program and the REPL bindings.
- Other languages keep `NEW` as an ordinary token, so it is still reported as an
  unknown command there.
- A command that draws appears on the canvas exactly like a Run.
- A reader or evaluator error is reported in the Errors tab and marked in the
  output.
- Running a document stops and replaces the REPL session, so a program run does
  not inherit the REPL's variables.
- The automation API exposes the same sessions with `repl.create`, `repl.eval`,
  `repl.reset` and `repl.close`; reset forgets every binding.

---

## 4. Debugging

### Breakpoints

Click the gutter beside a line to set a breakpoint. A breakpoint is keyed to the
source position the gutter shows, is remembered per file, and is removed if the
file is removed. The Debug tab also accepts a source line number directly.

### Start and step

Press **Debug** on the toolbar, or use the Debug tab:

| Control | Effect |
| --- | --- |
| **Debug** | Run and stop at the first breakpoint |
| **Continue** | Resume to the next breakpoint or the end |
| **Pause** | Ask a running program to stop at its next boundary |
| **Step In / Step Over / Step Out** | Advance one unit. Scheme distinguishes calls; the other languages have one statement/command/message/instruction unit, so all three stop at the next boundary |
| **Stop** | End the run |

The paused location is shown as a line, with the procedure name and call depth
where the language has them. Scheme reports its call stack and the paused frame's
bindings; native languages publish their visible bindings to the Variables tab.

### Watch expressions

In **Debug → Watch expressions**, enter an arithmetic expression over the paused
bindings, for example `X * 2` or `TOTAL / COUNT`. The value updates on every pause;
an unknown name is reported as `?` with its error rather than silently read as zero.
Watches are saved with the session.

### Data breakpoints

Under **Break when a variable changes**, type a variable name (or double-click it
in the Variables table). The run stops after the statement or form that changed
its value, and the Debug tab names the variable. Names are case-insensitive.

### Telemetry

The Telemetry HUD shows the engine counters for a run: steps, engine time, steps
per second, last line, recursion depth, call stack, output lines, drawing commands,
BASIC's memory window, and the IDE's wall clock and delivered ink. Use it to tell
"the program is slow" from "the program is drawing thousands of tiny segments".

---

## 5. SQL and the Database tab

The Studio embeds SQLite. The 🗄 **Database** tab opens or
creates a database file, lists its tables, shows a table's `CREATE` statement, and
runs one statement or a whole script. **Run** is `Ctrl+Return`; results appear in a
grid, with the change count and an error message when the statement fails. The
result grid returns at most 1000 rows and says when it truncated.

The Database tab and every program share **one connection**, so this sequence
works:

1. Create `scores` in the Database tab.
2. Run a BASIC program containing `SQL SELECT name, score FROM scores`.
3. The same rows appear in the program's output.

SQL forms by language:

```basic
10 SQL CREATE TABLE scores (name TEXT, score INTEGER)
20 SQL INSERT INTO scores VALUES ('Ada', 42)
30 SQL SELECT name, score FROM scores
```

The shared `SQL` statement works in BASIC, FOCAL, PILOT, COMAL, REXX and APL.
Logo uses `SQL SELECT ...` as a command; Scheme uses `(sql "...")`; Smalltalk uses
`SQL run: '...'`. CHIP-8 has no SQL form, because a bytecode program has no strings
to carry a statement.

SQLite is a build/runtime dependency of the Qt application. The compiled programs
produced by `twc` link it too and accept `--db FILE`.

---

## 6. Terminal tools

### `./run.sh`

Builds if necessary and launches the IDE. Arguments pass through, so
`./run.sh --run program.bas`, `--screenshot`, `--export` and `--compile` all work.

### `./twc` — a small standalone executable

```bash
./twc Examples/basic/hello.bas -o hello
./twc Examples/logo/mandala.logo -o mandala --svg mandala.svg
./twc Examples/basic/report.bas -o report --db school.db
```

`twc` embeds the source as bytes, links the language engine (and SQLite), and
produces an executable that runs the program without the Studio. It works for all
ten languages, including binary CHIP-8 ROMs. Options:

| Option | Meaning |
| --- | --- |
| `-o FILE` | Output executable |
| `--language NAME` | Override the language instead of using the extension |
| `--build-dir DIR` | Native build tree whose libraries to link |
| `--keep` | Keep the generated C++ |
| `--cxx CXX` | Compiler to use |

The generated executable accepts `--svg FILE`, `--no-svg` and, when the program
uses SQL, `--db FILE`. A drawing is written to `<program>.svg` by default when it
has ink.

### The IDE artifact

`time_warp_ide --compile program.bas --output hello`, or
`./run.sh --compile ...`, copies the IDE and appends the program. The artifact opens
a read-only player window, runs the program, and keeps the full canvas and output.
It needs the same Qt runtime as the IDE; `twc` is the smaller alternative.

---

## 7. Automation

`./api` starts a JSON-RPC 2.0 service on stdin/stdout: one JSON object per line,
one response per line, notifications get no response, and batches are arrays. The
full method reference is [API.md](API.md).

```bash
printf '%s\n' \
  '{"jsonrpc":"2.0","id":1,"method":"studio.info"}' \
  '{"jsonrpc":"2.0","id":2,"method":"language.run","params":{"language":"basic","source":"10 PRINT 6 * 7"}}' \
  | ./api
```

The method families are:

| Family | What it does |
| --- | --- |
| `studio.info`, `ping`, `studio.shutdown` | Discover features and languages; liveness; orderly exit |
| `language.run` | Run a program with optional input answers; returns output, errors, variables, drawing and telemetry |
| `repl.create`, `repl.eval`, `repl.reset`, `repl.close` | Persistent REPL sessions |
| `debug.start`, `debug.snapshot`, `debug.continue`, `debug.step`, `debug.stop`, `debug.close` | Start a run at breakpoints, inspect it, step and stop it |
| `sql.open`, `sql.query`, `sql.tables`, `sql.schema`, `sql.close` | Drive the shared SQLite connection |
| `project.create`, `project.open`, `project.save`, `project.info`, `project.addFile`, `project.addFiles`, `project.addFolder`, `project.removeFile`, `project.addGroup`, `project.renameGroup`, `project.removeGroup`, `project.moveFile` | Read and edit projects |
| `watch.evaluate` | Evaluate debugger-style expressions over supplied bindings |

`./api` builds the `time_warp_api` target if needed; `TIME_WARP_SKIP_BUILD=1`
skips that. The same server is a C++ library (`api::Server`) over an `istream` and
an `ostream`, which is how the tests drive it without a process.

---

## 8. Lessons, classroom and appearance

- **Lessons** — the 🎓 Lessons tab lists the curriculum and checks the document in
  front against the current checkpoint. Progress lives in
  `~/.time_warp/progress.json`; `native/tools/learn.py` reads the same file.
- **Classroom** — the 🏫 Classroom tab shows a roster, assignments and marks, and
  moves a whole class as a ZIP bundle. `native/tools/teach.py` is the teacher's CLI
  over the same files; see [lessons/teacher-guide.md](lessons/teacher-guide.md).
- **Themes** — View → Theme chooses one of the 33 themes; View → Next Theme
  (`Shift+F10`) cycles them. High Contrast derives a maximum-contrast variant of
  the current theme.
- **Presentation** — View → Presentation Mode (`Shift+F11`) hides the authoring
  chrome for a projector; Escape leaves it. Focus Mode (`F11`) is the quieter
  single-window variant.
- **Settings** — `Ctrl+,` changes fonts, tab width, minimap, whitespace,
  completion, linting, autosave and the opt-in Terminal tab.

Settings and learner data live under `~/.time_warp`; `TIME_WARP_DATA_DIR` moves the
whole directory, which is how tests avoid touching a real learner's files.

---

## 9. Where to go next

| Need | Document |
| --- | --- |
| Commands for a language | [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) |
| A worked introduction | [tutorials/](tutorials/) |
| Lessons and teaching | [lessons/README.md](lessons/README.md) |
| API methods and payloads | [API.md](API.md) |
| Architecture and tests | [ARCHITECTURE.md](ARCHITECTURE.md), [README_TESTING.md](README_TESTING.md) |
| Installation and packaging | [INSTALLATION.md](INSTALLATION.md) |
| Short answers | [FAQ.md](FAQ.md) |
