# The Studio

This is the manual for the window: documents, projects, the REPL bar, debugging, SQL,
the embedded terminal and appearance. The commands a language understands are in the
[language reference](../languages/reference.md); the [assistant](assistant.md) and
[automation](automation.md) have their own pages, because each is a service you set up
separately.

---

## The window

| Part | What it is |
| --- | --- |
| **Editor** | Documents as tabs, a line-number gutter, the current line highlighted, syntax colouring for the language, and a minimap |
| **Output and Graphics** | The console, the error list, the turtle log and the canvas, with a pose strip under the canvas and a scrubber that replays the drawing |
| **Right column** | Variables, Telemetry, Debug, Lessons, Classroom, Database, Reference and Assistant |
| **Projects dock** | The project tree (**View > Project Panel**, `Ctrl+Shift+E`) |
| **REPL bar** | One command at a time, under the editor |
| **Status bar** | The document's path, its language, cursor position and run state |

The status bar is the quickest place to check that the editor and the run agree about the
language: the badge shows what the engine will be asked to run, not what the file
extension suggested.

### Documents

A document's language comes from its extension. Choosing another language in the toolbar
records an override on that document, so selecting REXX for a file called `thing.txt` is
not undone the next time the badges refresh.

Everything that acts on a set of documents covers both panes: a document in the split is
unsaved work like any other, so it carries the same unsaved marker, is snapshotted by
autosave, is asked about when the window closes, and is raised rather than opened a
second time.

Two edge cases are deliberate. Closing the main pane's only document, when it has never
been saved and has nothing in it, leaves it as it is — its tab name and the language you
picked included. Closing the split's last document closes the split, because an empty
pane is the blank rectangle the split exists to avoid.

The tab's context menu:

| Entry | What it does |
| --- | --- |
| **Close Tab** | Closes this document. The close button and `Ctrl+W` are the same request, and `Ctrl+W` acts on the pane your cursor is in |
| **Close Other Tabs** | Closes every other tab of that pane |
| **Close All Tabs** | Closes every tab of that pane |
| **Duplicate Tab** | An unsaved copy of the document — its text, its language and its breakpoints — beside it. A copy rather than a second tab on the same file, because two tabs writing one path lose the later save |
| **Copy File Path** | The document's path on the clipboard, for a document that has been saved |
| **Open Containing Folder** | The folder the file is in |

Closing asks first when a document has unsaved changes. For the two multi-tab entries it
asks about every one of them before removing any, so Cancel leaves the window exactly as
it was.

### Finding things

`Ctrl+F` finds in the document, `F3` and `Shift+F3` move between matches, `Ctrl+L` goes
to a line, and `Ctrl+Shift+F` finds in files. **Replace All** across files names the count
and the number of files before it acts, leaves a document with unsaved changes alone and
says so, and reloads every open document it changed so the editor cannot save the old text
back over the new.

`Ctrl+Alt+V` opens **File > Version History…**: the autosaved copies of the document,
including one for a buffer that was never saved. **Compare with what is open** shows the
chosen copy as a unified diff — `-` for what restoring takes away, `+` for what it brings
back — because "restore this" is a decision about the difference, not about one version.

`Ctrl+Shift+P` or `F10` opens the command palette: every action in the window, by name.

---

## Running a program

| Action | How |
| --- | --- |
| Run | **F9**, or **Run** in the toolbar |
| Run only the selection | `Ctrl+F9` |
| Stop | `Shift+F9`, or the Stop glyph in the toolbar |
| Reset the canvas and the output | `Ctrl+Shift+F9` |
| Answer an input prompt | Type it into the dialog and press Return |

Every engine runs under the same budget: a run stops after two million statements or ten
seconds of evaluation, whichever comes first, and the console says which budget ended it.
A run the budget ended is reported as stopped, not as finished — `time_warp_check` exits
non-zero and names the limit, so a script cannot mistake a truncated run for a complete
one. **Stop** ends a run sooner, at the next statement. The per-engine details are in the
[reference](../languages/reference.md#limits-and-budgets).

### A program with a screen

The canvas normally fits itself to whatever was drawn. A BASIC program that names a screen
— `SCREEN 13`, `SCREEN 12` and the rest of the numbered modes — gets the opposite: a
fixed-pixel surface shown at whole-number scale, where `PSET (10, 20)` is ten pixels right
and twenty down from the top-left corner and *fit to content* leaves it alone. `POINT`
reads a pixel back, `GET` and `PUT` move a sprite, and the drawing exports at the screen's
own size. `SCREEN 0` gives the fitted canvas back.
[`star_patrol.bas`](../../Examples/basic/star_patrol.bas) is built on it.

### The canvas

The drawing appears as it is made. Drag to pan, scroll to zoom, and the toolbar's grid
button shows or hides the grid. **Ctrl+E** exports the drawing as PNG, JPEG or BMP, or as
SVG when the build has Qt's Svg module, and the drawing can be copied to the clipboard.

Two things are worth knowing about the canvas because they are asserted by the tests
rather than left to taste. Heading 0 is north and increases clockwise, so `forward` moves
`x += d·sin(h)` and `y += d·cos(h)`. A `circle` is a polygon walk, not a Bézier curve. Both
are the reference's conventions, kept because drawings made in 1985 still have to look
like themselves.

The scrubber under the canvas replays the display list stroke by stroke, and **Record
Canvas as GIF…** captures the drawing as it is made, merging unchanged frames into longer
delays rather than writing a hundred identical ones. **File > Export Audio as WAV…**
writes every `SOUND` and `PLAY` the run synthesised, in order, even on a machine with no
player installed.

The **Turtle Log** lists every drawing command the canvas received, numbered and spelled
with its arguments, so a picture that came out wrong can be read back. The pose strip
under the canvas shows where the turtle is, which way it faces, what the pen is doing, and
how many commands the run has sent.

### Printing and other exports

**File > Print Code** (`Ctrl+P`) and **File > Print Drawing** go through Qt's PrintSupport
module. A build without that module hides both entries rather than showing dead ones. The
program exports as PDF, the program with its output and drawing as one HTML page, and a
lesson session as Markdown or PDF.

### Notebooks

**File > New Notebook…** opens a window where prose and programs share one file. Add a
**Prose** cell and write; add a **Program** cell, choose its language and press **Run**.
The cell's output appears beneath it, and a cell that draws shows the drawing. **Run All**
walks every program cell top to bottom. A notebook saves as `.twnb`, which is JSON, so it
diffs and reviews.

Each cell runs on its own: the engine's answer, in this process, with input answering
empty rather than waiting for a person, and no variables shared with the cell above. A
notebook is a document, not a session. A lesson that needs a running session puts one
program in one cell.

### The examples gallery

**File > Examples Gallery…** lists every program that ships with the Studio, with the
drawing it makes as its face. Each thumbnail is the program run headlessly: the same
engine a Run uses, the same step budget, input answering nothing. A program that loops is
stopped rather than hung, and one that prints but never draws gets a card naming its
language. Thumbnails render a few at a time after the window opens, so the gallery is
usable immediately. Double-click one, or select it and press **Open**.

---

## Projects

A project is a **view over files that already exist**, not a copy of them. It records a
root folder, named groups and the programs in them; moving a program between groups never
moves it on disk.

**Project > New Project…** (`Ctrl+Shift+N`) names the project and chooses its root folder,
and the `.twproject` file is saved beside the programs by default. **Project > Open
Project…** (`Ctrl+Shift+O`) reopens one; recent projects are on the same menu, and the
active project reopens with the session.

Populating it:

- **Add Current File to Project** adds the document in front.
- **Add Files to Project…** adds chosen programs.
- **Add Folder to Project…** walks a folder recursively and files each program under a
  group named after its subfolder. When the folder is outside the project root, its own
  name becomes the prefix, so adding a `class` folder produces groups such as `class/Art`.
- Dragging files from the desktop adds them too.

Organising it:

- **New Group…** (`Ctrl+Shift+G`) creates a group such as *Turtle art* or *Friday's class*.
- Dragging a program onto a group moves it; dragging it onto the project ungroups it. The
  same moves are in the file's context menu.
- Removing a group returns its programs to the ungrouped section. It never deletes a file.
- A program that has been moved or deleted is marked **missing**, not forgotten.

### The entry point

A project can name one program as its entry point: the program a reader should run first.

- **Set as Entry Point** in a program's context menu. The entry is marked in the dock, so
  which file is *the* program is visible without opening the menu.
- **Run Project** (`Ctrl+Alt+R`, or the run control in the panel's header) runs it.
- **Clear Entry Point** forgets it. Removing the entry's file from the project clears it
  too, so a project never points at a file it no longer holds.

Relative file names resolve against the **project root** when a program runs as part of a
project, and against the document's own folder otherwise. That is what lets a program say
`OPEN "data/words.txt"` and mean the file beside the project rather than whatever
directory the Studio happened to be launched from.

### The file on disk

`.twproject` is a small JSON document. Paths inside the root are stored relative to it, so
copying the folder keeps the project working; files outside the root stay absolute. Saves
are atomic, and a project file from a newer format is refused rather than half-read.
Format 2 added the `entry` key; a format 1 file still opens and simply has no entry point.

`project.*` over the [automation API](automation.md) drives the same file, including
`project.addFolder`, `project.setEntry` and `project.clearEntry`.

---

## The REPL bar

The line under the editor submits one entry to the current document's language. It uses
the same interpreters as Run, runs off the GUI thread, and sends drawing through the same
canvas queue.

| Language | State kept between entries |
| --- | --- |
| BASIC, FOCAL, PILOT, COMAL, REXX | Variables and evaluator state persist: `LET X = 5` then `PRINT X` gives 5 |
| APL | Variables and `∇` definitions persist |
| Logo | `MAKE` variables persist; `TO … END` definitions do not, because each entry runs as a whole program |
| Scheme | The machine persists, so `define` and `set!` survive and expression values are echoed |
| Smalltalk | Each entry is a complete program; top-level variables, classes and methods do not carry over |
| Forth | Each entry is a whole program, because a definition spans as many lines as it likes |

A command that draws appears on the canvas exactly like a Run. A reader or evaluator error
goes to the Errors tab and is marked in the output. Running a document stops and replaces
the REPL session, so a program does not inherit the REPL's variables.

### BASIC direct mode

BASIC keeps the classic behaviour on top of that:

- A line starting with a number is **stored in the editor** as a program line rather than
  executed. `10 PRINT "HI"` adds line 10, a later line with the same number replaces it,
  and a bare `10` deletes it. Lines stay in numeric order.
- `?` is shorthand for `PRINT`, in the REPL and in program text: `? 6 * 7` prints 42.
- `LIST` prints the current program, `RUN` runs it, and `NEW` clears the program and the
  REPL bindings.
- Other languages keep `NEW` as an ordinary token, so it is reported as unknown there.

The [automation API](automation.md) exposes the same sessions as `repl.create`,
`repl.eval`, `repl.reset` and `repl.close`.

---

## Debugging

### Breakpoints

Click the gutter beside a line to set a breakpoint. It is keyed to the source position the
gutter shows, remembered per file, and removed if the file is. The Debug panel also takes a
line number directly.

### Stepping

| Control | Effect |
| --- | --- |
| **Debug** | Run and stop at the first breakpoint |
| **Continue** | Resume to the next breakpoint or the end |
| **Pause** | Ask a running program to stop at its next boundary |
| **Step In, Step Over, Step Out** | Advance one unit. Scheme distinguishes calls; every other language has a single unit — a statement, a command, a message send or a token — so all three stop at the next boundary |
| **Stop** | End the run |

The paused location is shown with the procedure name and call depth where the language
has them. Scheme reports its call stack and the paused frame's bindings. Smalltalk lists
its message sends, outermost first, each as `Counter>>bump` with its line. The line-loop
languages name the calls they are inside: a `PROGRAM` row waiting at the line that called,
then a row per `GOSUB`, `CALL`, `PROC`, `DO` group or PILOT unit, innermost last. A run
standing in the program's own body reports no chain, because nothing is waiting.

The engine decides where a pause may land, which is the practical consequence of the
line-by-line and whole-program split: a FOCAL `DO` group's lines run inside the `DO`
statement rather than through the run loop, and a breakpoint on one of them stops there
like any other line because the engine says so rather than because the debugger guessed.

Every language publishes its visible bindings to the Variables panel, filtered by name.

### Walking a run backwards

Set **History** in the Debug panel to how many statements a run should remember — off,
100, 500 or 2000 — and the next run keeps that many. **Back** and **Back 10** then step
the run backwards with the bindings each statement had, which answers "what was `TOTAL`
on the iteration before it went wrong" without a second run and a print statement.

### Telemetry

The Telemetry panel reports what a run cost: steps, engine time, steps per second, last
line, recursion depth, call stack, output lines, drawing commands, BASIC's memory window,
and — beside the engine's counters — the Studio's own wall clock and the ink that reached
the canvas. It is what distinguishes "the program is slow" from "the program is drawing
forty thousand tiny segments".

---

## SQL and the Database panel

The Studio embeds SQLite. The **Database** panel opens or creates a file, lists its tables,
shows a table's `CREATE` statement, and runs one statement or a whole script. **Run** is
`Ctrl+Return`; results appear in a grid with the change count, and a failing statement
reports its error. The result grid returns at most 1000 rows and says when it truncated.

The panel and every program share **one connection**, so this works:

1. Create `scores` in the Database panel.
2. Run a BASIC program containing `SQL SELECT name, score FROM scores`.
3. The same rows appear in the program's output.

```basic
10 SQL CREATE TABLE scores (name TEXT, score INTEGER)
20 SQL INSERT INTO scores VALUES ('Ada', 42)
30 SQL SELECT name, score FROM scores
```

The `SQL` statement is the Studio's extension rather than a language's, and every
line-by-line engine gets it from the shared loop: BASIC, APL, PILOT, FOCAL, COMAL and
REXX all take `SQL SELECT …`. The whole-program engines supply their own spelling —
Logo's command `SQL SELECT …`, Scheme's `(sql "…")`, Smalltalk's `SQL run: '…'` — and
Forth is the one language with no SQL word at all, because it has no statement form to
extend. A Forth program reaches the database through the [API](automation.md) or not at
all.

The compiled programs `twc` produces link SQLite too and take `--db FILE`.

---

## The embedded terminal

**Settings > Workspace > Enable the embedded terminal** adds a Terminal tab beside the
console. It is off until you tick it, and it is a real terminal: the shell runs on a
pseudo-terminal of its own, so it prints its prompt, colours its output, keeps job
control and answers a program that asks how wide the window is. What you see is the
shell's own echo rather than a copy the Studio printed.

What it is not is a **screen**. The tab is a text transcript, so a program that paints
with cursor addressing — `vim`, `top`, a full-screen curses interface — starts and has its
escape sequences stripped rather than drawn. The tab is for commands: a build, a script,
`git`, `make`.

**Interrupt** sends `Ctrl+C` to whatever is in the foreground, which is how a command that
will not end on its own ends; the session keeps its variables, its history and its
directory. **Restart** is for a wedged shell: it ends the shell and everything it started,
and begins a fresh one in the same folder. A line is never dropped silently — if the shell
stops reading, the tab says the line did not reach it.

The shell follows the document: opening `Examples/logo/mandala.logo` moves it to that
folder, so `./build` names the right directory without typing a path. Closing the window
ends the shell and everything it started.

---

## Appearance and modes

**View > Theme** chooses one of 35 colour schemes; **View > Next Theme** (`Shift+F10`)
cycles them. High Contrast derives a maximum-contrast variant of whichever is active
rather than replacing it. A theme drives the whole window, not only the code: the toolbar,
tabs, docks, menus and fields are derived from the theme's colours, so a retro set changes
the chrome as well as the syntax. The four CRT sets add curvature, scanlines, phosphor
persistence and vignette, each with its own strength.

**View > Presentation Mode** (`Shift+F11`) hides the authoring chrome for a projector, and
Escape leaves it. **Focus Mode** (`F11`) is the quieter single-window variant: the menu bar
stays, for the person at the keyboard. Both restore exactly what they hid, so a panel you
had closed does not reappear when you leave.

**Settings** (`Ctrl+,`) changes fonts, tab width, the minimap, whitespace display,
completion, linting, autosave and its interval, the terminal tab, and whether a run that
finishes while the window is in the background announces itself through the desktop's
notification service. The 100%, 125% and 150% text ladder on the toolbar scales the editor,
the console and the panels together — a reader who needs larger text needs all of it.

Settings and learner data live under `~/.time_warp`. `TIME_WARP_DATA_DIR` moves the whole
directory, which is how the tests avoid touching a real learner's files.

---

## Shortcuts

| Key | Action |
| --- | --- |
| `F9`, `Ctrl+F9`, `Shift+F9`, `Ctrl+Shift+F9` | Run, run the selection, stop, reset the canvas and output |
| `Ctrl+N`, `Ctrl+O`, `Ctrl+S`, `Ctrl+W` | New, open, save, close the tab |
| `F2`, `F3`, `Shift+F3` | Save, find next, find previous |
| `Ctrl+F`, `Ctrl+H`, `Ctrl+L`, `Ctrl+Shift+F` | Find, replace, go to line, find in files |
| `Ctrl+I` | Format code |
| `Ctrl+E`, `Ctrl+P` | Export the drawing, print the code |
| `Ctrl+Shift+I` | Insert a snippet |
| `Ctrl+Alt+V` | Version history |
| `Ctrl+\`, `Ctrl+Alt+B` | Split the editor, run both panes |
| `Ctrl+Shift+E`, `Ctrl+Shift+G` | Project panel, new group |
| `Ctrl+Shift+N`, `Ctrl+Shift+O`, `Ctrl+Alt+R` | New project, open project, run project |
| `Ctrl+,` | Settings |
| `Ctrl+Shift+A`, `Ctrl+Alt+A`, `Ctrl+Alt+H` | Assistant, text size, high contrast |
| `Shift+F10`, `F11`, `Shift+F11` | Next theme, focus mode, presentation mode |
| `F1`, `Ctrl+F1`, `F12`, `F10` | Context help, keyword inspector, guided tour, command palette |
