# Time Warp Studio — User Guide

This is the manual for the Qt application: how to open it, organise work, meet the
HB-1 home computer, try commands without writing a program, debug, use SQL, ask an
AI assistant about a program, and drive the Studio from a script or a terminal. The per-language command
reference is
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
./run.sh --computer                           # open the HB-1
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
  canvas. A program that prints and draws keeps both in view. The 🐢 Turtle Log
  tab lists every drawing command the canvas received, numbered and spelled with
  its arguments, so a picture that came out wrong can be read back; a pose strip
  under the canvas shows where the turtle is, which way it faces, what the pen is
  doing and how many commands the run has sent. A program that makes a sound keeps
  it too: **File > Export Audio as WAV…** writes the run's `SOUND` and `PLAY` notes
  to one file, and it records even on a machine with no player installed.
- **Right-column tabs** — Variables, Telemetry HUD, Debug, Lessons, Classroom,
  Database, 📖 Reference and Assistant (see [section 8](#8-the-ai-assistant)).
  **Reference** lists every command for the selected language with the smallest
  program that runs it and what it does, read from the same table the command gate
  drives; **Try it** puts a row's program in a new document. It follows the language
  of the document in front every time you raise it.
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

The engine stops a runaway program after its own step and time budget — 10
seconds, two million statements, a hundred thousand program steps — with a
sentence in the console rather than a hung window; Stop ends it sooner, at the next
statement or instruction.

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
| CHIP-8 | Each entry is an assembly listing that runs as a whole program; memory and registers reset. |
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

## 4. The HB-1 computer

Tools > **HB-1 Computer…** (`Ctrl+Alt+W`, or `./run.sh --computer`)
opens a self-contained home computer: one CRT with text and graphics, a boot
message at the top, and a prompt underneath. It is the machine the eleven languages
share, presented the way a 1980s learner would have met them — a 1982 machine from
a parallel universe, with the last forty years of design sense behind its engines.

This section is the IDE-side tour. The machine has its own books, and they are
the complete reference:

- **[Owner's Manual](hb1/README.md)** — specifications, first boot, front
  panel, keys, what the machine remembers, the limits.
- **[The ROM library](hb1/roms.md)** — all eleven languages as cartridges.
- **[HB DOS 1.0](hb1/hb-dos.md)** — every disk word, the catalogue, the
  errors, the image format.
- **[CONFIG](hb1/config.md)** — every setting and default, the phosphors, the
  glass.
- **[Inside the HB-1](hb1/inside.md)** — how it is built.
- **[The machines](MACHINES.md)** — why the HB-1 and the CHIP-8 are the shape
  they are.

**The ROM socket.** The **ROM** selector cold-boots the machine into any of the eleven
languages. The machine name, prompt and boot message change with it:

| ROM | Prompt | Program memory |
| --- | --- | --- |
| BASIC | `READY.` | numbered program lines |
| Logo | `?` | the editor contents |
| Scheme | `>` | the editor contents |
| FOCAL | `*` | the editor contents |
| PILOT | `PILOT:` | the editor contents |
| COMAL | `]` | the editor contents |
| REXX | `REXX>` | the editor contents |
| APL | `APL>` | the editor contents |
| Smalltalk | `ST>` | the editor contents |
| CHIP-8 | `CHIP8>` | the editor contents |

Each ROM keeps its own program. Switching away and back restores what that ROM
was holding, and the selected ROM, screen mode and all eleven program buffers persist
in QSettings between sessions.

**The screen.** **SCREEN** selects **TEXT**, **SPLIT** (the default) or
**GRAPHICS**. In SPLIT the text screen is on the left and the turtle canvas on the
right; a drawing that arrives while TEXT is selected switches to SPLIT once so it
cannot be drawn out of sight. The text side has two pages:

- **PROGRAM** is a real `CodeEditor`: line numbers, syntax colouring, checking,
  undo, cursor movement and selection, exactly as in the main editor.
- **OUTPUT** is the machine's screen: the boot banner, prompt echoes, program output
  and errors.

The prompt is below the screen. **Up** and **Down** recall earlier entries.

**Prompt commands.** These are the machine's own words; everything else goes to
the selected language's persistent REPL session.

| Command | Effect |
| --- | --- |
| `RUN` | Run the program on the PROGRAM page. F9 does the same. `RUN "NAME"` loads a file from the disk and runs it |
| `LIST` | Print the program to OUTPUT |
| `NEW` | Erase the program and reset the REPL bindings |
| `EDIT` | Move to the PROGRAM page |
| `TEXT`, `GRAPHICS`/`GFX`, `SPLIT` | Change the screen mode |
| `CLS`/`CLEAR` | Clear OUTPUT and the drawing (Ctrl+L) |
| `HELP` or a bare `?` | Print the machine's command list |
| `ROMS` | List the installed language ROMs |
| `CONFIG`/`SETUP` | Open the machine's settings screen (Ctrl+, or **CONFIG**) |
| `Shift+F9` | Stop a running program |

**BASIC direct mode.** In the BASIC ROM a numbered line typed at the prompt is
stored on the PROGRAM page in numeric order; a later line with the same number
replaces it, and a bare number deletes it. `?` is shorthand for `PRINT`, so
`? 6 * 7` prints 42. A line such as `2 + 2` is still an expression, not line 2.

**HB DOS.** The machine has a disk — **HB DOS 1.0**, one 512-sector volume called
`HBWARP`, in the manner of the DOS a 1980s learner met. It is where a program goes
when it has to outlive the afternoon; the PROGRAM page itself is only memory.

| Word | What it does |
| --- | --- |
| `CATALOG` (`CAT`) | List the disk: lock marker, ROM, sectors, name |
| `SAVE "NAME"` | Save the program on the PROGRAM page |
| `LOAD "NAME"` / `RUN "NAME"` | Load a file into memory, or load and run it |
| `DELETE "NAME"` (`ERA`) | Erase a file; a locked file refuses |
| `RENAME "OLD","NEW"` | Rename a file |
| `LOCK`/`UNLOCK "NAME"` | Write-protect a file, or release it |
| `VERIFY "NAME"` | Compare a file with the program in memory |
| `TYPE "NAME"` | Print a file to OUTPUT |
| `STATUS` | Volume, sectors used and free, the image's path |
| `DIAL` (`BBS`) | Call WARP BOARD, the bulletin board on the other end of the line |
| `LLIST` | Print the program to the paper tray |
| `LPRINT "TEXT"` | Print a line to the paper tray |
| `INIT` | Erase the whole disk; it asks a second time |

**The telephone.** `DIAL` picks up the line and connects to **WARP BOARD**, a
bulletin board kept in `~/.time_warp/bbs/board.json`. Enter a handle (remembered for
next time), then `B` lists the messages, `R N` reads one, `P SUBJECT` posts -- type
the body and a line with a single `.` to end it -- and `BYE` hangs up. The board is on
the machine rather than on a network, so a class can show what a board *was* without
a single packet leaving the room. [The board's manual](hb1/bbs.md) has the words and
the file format.

The **paper tray** is `paper.txt` in `~/.time_warp/print`, appended by `LLIST` and
`LPRINT` with a headed page each time and named on the console with the line count.
The machine has no printer port and never did: a file is the same artefact a printer
produced -- something to keep, read or print elsewhere -- and it is the one output of
the machine that survives being switched off.

Names are upper-cased and last 30 characters; `SAVE HELLO` and `SAVE "HELLO"` are
the same request. A program is measured in the 256-byte sectors the catalogue
counts in, and a file that would not fit is refused rather than truncated
(`?DISK FULL`). A catalogue looks like this:

```text
DISK VOLUME 254   HBWARP   512 SECTORS

 BASIC      001  HELLO
*LOGO       001  SQUARE

2 FILES   2 SECTORS USED   510 FREE
```

The `*` is the lock. A file remembers the ROM that can run it, so `LOAD "SQUARE"`
cold-boots the machine into Logo before the program arrives. The volume is one
image file, `~/.time_warp/disks/HBWARP.dsk` (`TIME_WARP_DATA_DIR` moves it), so a
program saved in one lesson is there in the next, and a teacher can copy the image
to another machine. The status line shows how many files the disk holds.

Every word's exact refusals, the image format and a worked session are in the
**[HB DOS reference](hb1/hb-dos.md)**.

The computer's REPL sessions are its own: they do not share bindings with the
REPL bar in the editor. Running a program stops the prompt session, exactly as
Run does in the main window, so a program never starts inside a previous
command's variables.

**Settings.** **CONFIG** in the header, `Ctrl+,`, or `CONFIG` at the prompt
opens the machine's configuration screen: three pages covering the boot ROM and
boot screen, the start-up message, prompt history, program memory, the SPLIT
courtesy, four phosphor palettes, two text sizes and the five glass strengths.
Everything except the boot settings takes effect at once; **Apply**/**OK** keeps
it, **Cancel** puts the machine back as it was. The boot ROM and boot screen
apply at the next power-on rather than switching the machine immediately, so a
settings change never discards the program being typed.

Every control, its default and its stored key is in the
**[CONFIG reference](hb1/config.md)**.

**From a terminal.** `./run.sh --computer` opens the machine, booted into the
first file's language if one is named; `--run` then runs its program.
`--screenshot` photographs the machine and `--export` saves the drawing on its
canvas rather than the editor's, so a lesson handout or an automation script sees
the machine. `--machine <file.hb1>` boots from a **[machine image](hb1/README.md#the-whole-machine-as-one-file)**:
the ROM, the screen, every ROM's program, the phosphor and the disk as one file,
written by `SAVE MACHINE` at the machine's own prompt.

### The CHIP-8 machine

The Studio's other machine is the **[CHIP-8](chip8/README.md)** — the 1977
virtual machine, and the program format the editor's CHIP-8 language runs. Tools >
**CHIP-8 Emulator…** (`Ctrl+Shift+M`) opens it live:

- a 64×32 screen (128×64 in SuperChip hi-res), sixty frames a second;
- a clickable keypad mirrored on the PC keyboard (`1`–`4`, `Q`–`R`, `A`–`F`,
  `Z`–`V`);
- **Run/Pause**, single **Step**, **Reset**, and a speed from 1 to 2000 opcodes
  per frame;
- an inspector with **Registers**, **Memory** and the **Listing**, plus a
  **ROM Library** that browses a folder you own;
- sound while the sound timer is non-zero, and a mute;
- **Rewind**: a strip under the screen that goes back through the last thirty seconds
  of the machine, a quarter second at a time. Touching it pauses and captures the
  frame you were looking at, so the right-hand end is exactly where you were;
  **Run** resumes from where you rewound to and drops the future it would have had.
  It replays the drawing, not the luck: a program drawing from `RND` gets its
  registers and pixels back, not its dice roll.

It assembles listings as well as loading binary ROMs, and drag-and-drop works for
`.ch8` and `.c8` files. The editor's Run still handles CHIP-8 as a whole program
for lessons and parity checks; the window is the machine with a clock and a
keypad. The complete reference is in
[the CHIP-8 manuals](chip8/README.md): the
[machine manual](chip8/README.md), the
[assembler](chip8/assembly.md) and the
[instruction set](chip8/instruction-set.md).

---

## 5. Debugging

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
Logo, Forth — whose unit is a token, and whose 64K data space is only copied when a
program has written to it — and CHIP-8, whose unit is an instruction and whose
checkpoint is the whole machine, screen included, so a rewind puts the pixels back
with the registers. Smalltalk stops at its chunk boundaries but cannot be walked
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

## 6. SQL and the Database tab

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
eleven languages, including binary CHIP-8 ROMs. Options:

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

The 🤖 Assistant tab asks a service you configure about the program in front of you.
It is off until you set it up, and nothing is sent until you press a button.

- **Tools > AI Settings…** chooses the service: OpenAI, DeepSeek, Anthropic, Ollama or
  LM Studio on this machine, OpenRouter, or any endpoint that speaks one of the two
  request shapes. **Test connection** proves the key and the model before you ask a
  question.
- **Tools > 🤖 Assistant** (`Ctrl+Shift+A`) raises the page. **Explain**, **Fix**,
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
  Windows Credential Manager, the macOS Keychain or the system keyring where the
  build found one, and in the settings file otherwise.
- Both machines have the same assistant in a dialog: the HB-1's 🤖 button and the
  CHIP-8's **🤖 Assistant…** ask about the program that machine is holding.

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

- **Lessons** — the 🎓 Lessons tab lists the curriculum and checks the document in
  front against the current checkpoint, and **Hand In** writes it into the class
  folder under the checkpoint's handout name, which is where marking reads it.
  Progress lives in `~/.time_warp/progress.json`; `native/tools/learn.cpp` reads the
  same file.
- **Classroom** — the 🏫 Classroom tab shows a roster, assignments and marks, and
  moves a whole class as a ZIP bundle. `native/tools/teach.cpp` is the teacher's CLI
  over the same files; see [lessons/teacher-guide.md](lessons/teacher-guide.md).
- **Themes** — View → Theme chooses one of the 35 themes; View → Next Theme
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
