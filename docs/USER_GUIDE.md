# Time Warp Studio — User Guide

This is the manual for the application: starting it, organising work, running
commands without writing a program, debugging, SQL, the embedded terminal, the
assistant, automation and appearance. The command reference is
[LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md); the tutorials begin at [tutorials/](tutorials/).

> First time here? Run `./run.sh`, open
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
  find/replace, **find and replace in files**, go-to-line, formatting, a minimap and
  an outline dock. Find in Files (`Ctrl+Shift+F`) can rewrite what it found: the
  confirmation names the count and the number of files, a document with unsaved
  changes is left alone and reported, and every open document it did change is
  reloaded so the editor cannot save the old text back over it. **File > Version
  History…** (`Ctrl+Alt+V`) lists the autosaved copies of a document, and **Compare
  with what is open** shows the chosen copy as a unified diff against the text in the
  editor -- `-` for what restoring takes away, `+` for what it brings back -- because
  "restore this" is a decision about the difference, not about one version.
- **Output and Graphics** — the console, the error list, the turtle log and the
  canvas. A program that prints and draws keeps both in view. The Turtle Log
  tab lists every drawing command the canvas received, numbered and spelled with
  its arguments, so a picture that came out wrong can be read back; a pose strip
  under the canvas shows where the turtle is, which way it faces, what the pen is
  doing and how many commands the run has sent. A program that makes a sound keeps
  it too: **File > Export Audio as WAV…** writes the run's `SOUND` and `PLAY` notes
  to one file, and it records even on a machine with no player installed.
- **Right-column tabs** — Variables, Telemetry, Debug, Lessons, Classroom,
  Database, Reference and Assistant (see [section 8](#8-the-ai-assistant)).
  **Reference** lists every command for the selected language with the smallest
  program that runs it and what it does, read from the same table the command gate
  drives; **Try it** puts a row's program in a new document. It follows the language
  of the document in front every time you raise it.
- **Projects dock** — the project tree (View → Project Panel, `Ctrl+Shift+E`).
- **REPL bar** — one command at a time under the editor.
- **Status bar** — language, cursor position, run state and the document name.

### The tab's context menu

A tab's right-click menu closes and copies documents:

| Entry | What it does |
| --- | --- |
| **Close Tab** | Closes this document. The ✕ and **File > Close Tab** (`Ctrl+W`) are the same request, and `Ctrl+W` closes the document in the pane your cursor is in. |
| **Close Other Tabs** | Closes every other tab of that pane, and does not ask about the one it keeps. |
| **Close All Tabs** | Closes every tab of that pane. |
| **Duplicate Tab** | Opens an unsaved copy of this document — its text, its language and its breakpoints — beside it. A copy rather than a second tab on the same file, because two tabs that write one path lose the later save. |
| **Copy File Path** | Puts the document's path on the clipboard. Only for a document that has been saved. |
| **Open Containing Folder** | Opens the folder the file is in. Only for a document that has been saved. |

Closing asks first when a document has unsaved changes — for **Close Other Tabs** and
**Close All Tabs**, about every one of them before any is removed, so a Cancel leaves
the window exactly as it was.

Everything that acts on a set of documents covers both panes: a document in the split
is unsaved work like any other, so it carries the same unsaved dot on its tab, is
snapshotted by autosave, is asked about when you close the window, and is raised
rather than opened a second time when you open its file again.

Two ends are special. Closing the main pane's only document, when it has never been
saved and has nothing in it, leaves it as it is — its tab name and the language you
picked included — because closing it would only put an identical blank document
back. And closing the split's second pane's last document closes the split, because a
pane with no documents in it is the blank rectangle the split exists to avoid. Any
other last document is replaced by a fresh blank one, so the editor is never left
empty.

### Run, stop and input

| Action | How |
| --- | --- |
| Run | **F9** or the **Run** button in the toolbar |
| Run only the selection | `Ctrl+F9` |
| Stop | `Shift+F9` or the Stop glyph in the toolbar |
| Reset the canvas and output | `Ctrl+Shift+F9` |
| Answer an input prompt | Type it into the dialog and press Return |
| Open a file | **File → Open** (`Ctrl+O`) or pass it on the command line |

Every engine has a budget, and it is the same rule in all ten: a run is stopped
after two million statements or ten seconds of evaluation, whichever comes first, with
a sentence in the console rather than a hung window. Scheme reaches the same limit
through the same clock. A run the budget ended is reported as stopped, not as finished
— `time_warp_check` exits non-zero and names the budget that ended it, so a script
cannot mistake a truncated run for a complete one. **Stop** ends a run sooner, at the
next statement.

### A program with a screen

The canvas normally fits itself to what a program drew. A BASIC program that names
a screen — `SCREEN 13`, `SCREEN 12`, and the rest of the numbered modes — gets the
opposite: a fixed-pixel surface shown at whole-number scale, where `PSET (10, 20)`
is ten pixels right and twenty down from the top-left corner, and `fit to content`
leaves it alone. `POINT` reads a pixel back, `GET`/`PUT` move a sprite, and the
drawing exports at the screen's own size. `SCREEN 0` gives the fitted canvas back.

`Examples/basic/star_patrol.bas` is a program built on it. The mode table is in
[the language guide](LANGUAGE_GUIDE.md#graphics).

### Notebooks

**File > New Notebook…** opens a window where prose and programs live in one file:
add a **+ Prose** cell and write, add a **+ Program** cell, choose its language and
press **Run**. The cell's own output appears beneath it, and a cell that draws shows
the drawing. **Run All** runs every program cell top to bottom. A notebook saves as a
`.twnb` — JSON, so it diffs — and **File > Open Notebook…** loads one back.

Each cell runs on its own: it is the engine's answer, in this process, with a
question answered empty rather than waiting for a person, and it does not share
variables with the cell above it. A notebook is a document, not a session; a lesson
that needs a running session puts one program in one cell.

### Browsing the examples

**File > Examples Gallery…** lists every program that ships with the Studio with the
drawing it makes as its face. Each thumbnail is the program run headlessly: the same
engine a Run uses, under the same step budget, with input answering nothing — so a
program that loops is stopped rather than hung, and one that prints but never draws
shows a card naming its language. They render a few at a time after the window opens,
so the gallery is usable immediately. Double-click a program (or select it and press
**Open**) to open it in a tab; **File > Examples** is still the menu that lists them
by language.

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

### Run the project

A project can name one program as its **entry point** -- the program a reader of
the project should run first, and the one that opens when the project is
launched from a shortcut.

- **Set as Entry Point** in a program's context menu. The entry is marked with ▶
  in the dock, so which file is *the* program is visible without opening the menu.
- **Run Project** (`Ctrl+Alt+R`, or ▶ in the panel's header) runs it.
- **Clear Entry Point** forgets it. Removing the entry's file from the project
  clears it too, so a project never points at a file it no longer holds.

Relative file names in a program resolve against the **project root** when it
runs as part of a project, and against the document's own folder otherwise. That
is what lets a program `OPEN "data/words.txt"` and mean the file beside the
project rather than whatever directory the IDE happened to be launched from.

### The file on disk

`.twproject` is a small JSON document. Paths inside the project root are stored
relative to it, so copying the whole folder keeps the project working; files
outside the root stay absolute. Saves are atomic (`QSaveFile`), and a project file
from a newer format is refused instead of being half-read. Format 2 added the
`entry` key; a format 1 file still opens and simply has no entry point.

Paths can also be managed through the API's `project.*` methods, including
`project.addFolder`, `project.setEntry` and `project.clearEntry`. See
[API.md](API.md#projectcreate-projectopen-projectsave-projectinfo).

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
| Forth | Each entry is a whole program, because a definition spans as many lines as it likes; words defined in one entry are gone by the next, so `: DOUBLE 2 * ;` and `21 DOUBLE .` are two entries only if you retype the definition. |

Practical notes:

- The line clears as soon as a command is handed to the engine.

**BASIC direct mode.** BASIC also has the classic direct-mode behaviour:

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
| **Step In / Step Over / Step Out** | Advance one unit. Scheme distinguishes calls; the other languages have one statement/command/message/token unit, so all three stop at the next boundary |
| **Stop** | End the run |

The paused location is shown as a line, with the procedure name and call depth
where the language has them. Scheme reports its call stack and the paused frame's
bindings; a whole-program language reports the chain its engine keeps — Smalltalk
lists its message sends, outermost first, each as `Counter>>bump` with the line it
is at — and the line-loop languages name the calls they are inside: a `PROGRAM` row
waiting at the line that called, then one row per `GOSUB`, `CALL`, `PROC`, `DO`
group or PILOT unit, the innermost at the statement the run stopped on. A run
standing in the program's own body reports no chain, because nothing is waiting.
A FOCAL `DO` group's lines run inside the `DO` statement rather than through the
run loop, so a breakpoint set on one of them used to never fire; it now stops there
like any other line. Every language publishes its visible bindings to the Variables
tab.

### Going back

A run can be walked backwards. Set **History** in the Debug tab to how many
statements the run should remember — off, 100, 500 or 2000 — and the next run
keeps that many. When it pauses, **Back** returns to the statement before it with
the bindings and output the run had there, and **Back 10** goes ten statements
back, or as far as the run kept. Pressing Back at the oldest statement the run
remembers disables the button rather than doing nothing, and a run started with
History off says so instead of offering it.

What a rewind does and does not undo: the bindings, the printed output and the
language's own scratch state go back with the statement, so carrying on from there
is running the program from there and not a replay. The canvas keeps the ink it
has already been given — the drawing half of going back is the canvas scrubber,
which steps through the display list. Because the console records the session
rather than the run, statements that are re-run print a second time; an `ℹ️ Back 1
statement(s)` line marks where the repeat starts.

Which languages: all six line-by-line ones (BASIC, FOCAL, PILOT, COMAL, APL, REXX),
Logo and Forth — whose unit is a token, and whose 64K data space is only copied when
a program has written to it. Smalltalk stops at its chunk boundaries but cannot be walked
back yet, so the buttons are dead for it and say why; Scheme is not offered it at
all, because that machine's state is an arena and a continuation chain rather than a
value. History costs one copy per statement while it is on; off costs nothing.

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

The Studio embeds SQLite. The **Database** tab opens or
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
`SQL run: '...'`. Two languages have no SQL form: Smalltalk's library reaches the
database instead of a statement carrying the query, and Forth has no word for it.

SQLite is a build/runtime dependency of the Qt application. The compiled programs
produced by `twc` link it too and accept `--db FILE`.

---

## 6. The embedded terminal

**Settings > Workspace > Enable the embedded terminal** adds a Terminal tab beside
the console. It is off until you tick it, and it is a real terminal: the shell runs
on a pseudo-terminal of its own, so it prints its prompt, colours its output, keeps
job control and answers a program that asks how wide the window is. Each line you
type goes to the shell as typed, and the transcript shows the shell's echo of it
rather than a copy printed by the IDE.

What it is not is a *screen*. The tab is a text transcript, so a program that paints
with cursor addressing — `vim`, `top`, a full-screen curses interface — starts and
its escape sequences are removed rather than drawn. The tab is for commands: a build,
a script, `git`, a `make`.

**Interrupt** under the prompt sends `Ctrl+C` to whatever is in the foreground, which
is how a command that will not end on its own ends; the session keeps its variables,
its history and its directory. **Restart** is for the rarer case where the shell
itself is wedged: it ends the shell and everything it started and begins a fresh one
in the same folder. Typing is never dropped without saying so — if a shell stops
reading, the tab tells you the line did not reach it rather than swallowing it.

The shell follows the document: opening `Examples/logo/mandala.logo` moves it to that
folder with a `cd`, so `./build` names the right directory without typing a path.
Closing the window ends the shell and everything it started.

---

## 7. Terminal tools

### `./run.sh`

Builds if necessary and launches the IDE. Arguments pass through, so
`./run.sh --run program.bas`, `--screenshot`, `--export`, `--compile` and
`--computer` all work.

### `./twc` — a small standalone executable

```bash
./twc Examples/basic/hello.bas -o hello
./twc Examples/logo/mandala.logo -o mandala --svg mandala.svg
./twc Examples/basic/report.bas -o report --db school.db
```

`twc` embeds the source as bytes, links the language engine (and SQLite), and
produces an executable that runs the program without the Studio. It works for all
ten languages. Options:

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

`time_warp_ide --compile program.bas --output hello`, or `./run.sh --compile ...`,
copies **`time_warp_player`** — the 2.3MB player, not the 4.7MB IDE — and appends the
program. The artifact opens one program: the canvas, its output, `R` to run again,
`F11` for the whole screen, `Esc` to leave. `--title` and `--author` put a name on it,
which the player shows until the program draws.

A program written for an older version still opens: the trailer is read from the end
of the file, so a format-1 artifact from 17.x reads as format 1 and a format-2 one
carries its metadata. It needs the same Qt runtime as the player; `twc` is the
smaller alternative for a program that needs no window at all.

---

## 8. The AI assistant

The Assistant tab asks a service you configure about the program in front of you.
It is off until you set it up, and nothing is sent until you press a button.

- **Tools > AI Settings…** chooses the service: OpenAI, DeepSeek, Anthropic, Ollama or
  LM Studio on this machine, OpenRouter, or any endpoint that speaks one of the two
  request shapes. **Test connection** proves the key and the model before you ask a
  question.
- **Tools > Assistant** (`Ctrl+Shift+A`) raises the page. **Explain**, **Fix**,
  **Comment**, **Translate** and **Write** are the one-click questions; the box takes
  anything else, and `Ctrl+Enter` sends it.
- **Insert** puts the answer's code into the editor at the cursor, and **Run** (F9)
  then runs it. Nothing the assistant writes runs until you have read it.
- **Send the program** is ticked by default; untick it to send only your words. A
  selection wins over the whole document.
- **Tools > Ask About the Last Error** asks about what the run said — the lines
  selected in the Console, else the last error the Errors tab holds — with the program
  and the output as evidence.
- **Suggest the next line** (AI Settings) shows a suggested line while you type: Tab
  takes it, Esc refuses it. Off unless you tick it, because it sends while you type.
- **Keep the conversation between sessions** (AI Settings) keeps the last twelve turns
  in the data directory and deletes them when unticked; off, a conversation dies with
  the window.
- The key may come from `TIME_WARP_AI_API_KEY` instead of a store, and **Keep the key
  on this machine** unticked leaves it nowhere at all. A stored key goes in the
  system keyring where the build found one, and in the settings file otherwise.

`./native/build/time_warp_ai` is the same assistant from a shell — `check`,
`explain`, `fix`, `write` and the rest. [AI.md](AI.md) is the manual: what is sent,
every environment variable, and what each error means.

---

## 9. Automation

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
| `debug.start`, `debug.snapshot`, `debug.continue`, `debug.step`, `debug.rewind`, `debug.stop`, `debug.close` | Start a run at breakpoints, inspect it, step it, walk it back through the history it kept, and stop it |
| `sql.open`, `sql.query`, `sql.tables`, `sql.schema`, `sql.close` | Drive the shared SQLite connection |
| `project.create`, `project.open`, `project.save`, `project.info`, `project.addFile`, `project.addFiles`, `project.addFolder`, `project.removeFile`, `project.addGroup`, `project.renameGroup`, `project.removeGroup`, `project.moveFile` | Read and edit projects |
| `watch.evaluate` | Evaluate debugger-style expressions over supplied bindings |

`./api` builds the `time_warp_api` target if needed; `TIME_WARP_SKIP_BUILD=1`
skips that. The same server is a C++ library (`api::Server`) over an `istream` and
an `ostream`, which is how the tests drive it without a process.

---

## 10. Lessons, classroom and appearance

- **Lessons** — the Lessons tab lists the curriculum and checks the document in
  front against the current checkpoint, and **Hand In** writes it into the class
  folder under the checkpoint's handout name, which is where marking reads it.
  Progress lives in `~/.time_warp/progress.json`; `native/tools/learn.cpp` reads the
  same file.
- **Classroom** — the Classroom tab shows a roster, assignments and marks, and
  moves a whole class as a ZIP bundle. `native/tools/teach.cpp` is the teacher's CLI
  over the same files; see [lessons/teacher-guide.md](lessons/teacher-guide.md).
- **Themes** — View → Theme chooses one of the 35 themes; View → Next Theme
  (`Shift+F10`) cycles them. High Contrast derives a maximum-contrast variant of
  the current theme. Every theme drives the whole window, not just the code: the
  toolbar, tabs, docks, menus and fields are derived from the theme's colours, so
  a retro set changes the chrome as well as the syntax.
- **Presentation** — View → Presentation Mode (`Shift+F11`) hides the authoring
  chrome for a projector; Escape leaves it. Focus Mode (`F11`) is the quieter
  single-window variant.
- **Settings** — `Ctrl+,` changes fonts, tab width, minimap, whitespace,
  completion, linting, autosave, the opt-in Terminal tab, and whether a run that
  finishes while the window is in the background announces itself through the
  desktop's notification service.

Settings and learner data live under `~/.time_warp`; `TIME_WARP_DATA_DIR` moves the
whole directory, which is how tests avoid touching a real learner's files.

---

## 11. Where to go next

| Need | Document |
| --- | --- |
| Commands for a language | [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) |
| A worked introduction | [tutorials/](tutorials/) |
| Lessons and teaching | [lessons/README.md](lessons/README.md) |
| The AI assistant, and what it sends | [AI.md](AI.md) |
| API methods and payloads | [API.md](API.md) |
| Architecture and tests | [ARCHITECTURE.md](ARCHITECTURE.md), [README_TESTING.md](README_TESTING.md) |
| Installation and packaging | [INSTALLATION.md](INSTALLATION.md) |
| Short answers | [FAQ.md](FAQ.md) |
