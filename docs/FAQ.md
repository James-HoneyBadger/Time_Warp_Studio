# Time Warp Studio - Frequently Asked Questions

Answers to common questions about building, running and learning with Time Warp
Studio. For the full listing, see the [documentation index](INDEX.md) and
[INSTALLATION.md](INSTALLATION.md).

---

## Table of Contents

- [General](#general)
- [Installation and Startup](#installation-and-startup)
- [Editing and Files](#editing-and-files)
- [Running Programs](#running-programs)
- [Languages](#languages)
- [Turtle and Graphics](#turtle-and-graphics)
- [Debugging](#debugging)
- [Saving, Autosave, and Version History](#saving-autosave-and-version-history)
- [Exporting and Printing](#exporting-and-printing)
- [Lessons and Learning](#lessons-and-learning)
- [Appearance and Settings](#appearance-and-settings)
- [Troubleshooting](#troubleshooting)
- [Contributing and Project Information](#contributing-and-project-information)

---

## General

### What is Time Warp Studio?

Time Warp Studio is an educational desktop programming environment. It runs ten
programming languages in one IDE, with an integrated turtle graphics canvas and
an output console. It is a C++20 / Qt6 application; every language runs in C++
inside it, and the PySide6 implementation that preceded it has been removed.

### Which languages are supported?

BASIC, APL, Logo, PILOT, FOCAL, Scheme, CHIP-8, COMAL, REXX and Smalltalk. Each
language implements a subset of its traditional form. See the
[Language Guide](LANGUAGE_GUIDE.md) for what each one supports.

### Is Time Warp Studio free?

Yes. It is released under the MIT License. See [LICENSE](../LICENSE).

### Who is it for?

Students, teachers, and anyone who wants to learn programming or compare languages. It is designed for learning first, so error messages and output use plain language and emoji prefixes.

### Does it need an internet connection?

No. The core features work offline.

---

## Installation and Startup

### What do I need to run it?

- A C++20 compiler, CMake 3.20+, and Qt6 (Widgets, Core, Gui; Svg is optional
  and adds SVG drawing export, PrintSupport is optional and adds the two Print
  entries)
- Python 3.10 or newer **only** for the lesson-check engine (and the test
  harnesses). Running a program needs no Python at all, whatever the language.

### How do I build it?

From the repository root:

```bash
cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
cmake --build native/build -j
```

### How do I start the IDE?

```bash
./native/build/time_warp_ide
```

Or `make -C native run`, which builds first. You can open a program directly:
`./native/build/time_warp_ide Examples/logo/mandala.logo`.

### The app crashes at startup with "Illegal instruction." What is wrong?

That message comes from a build whose Qt libraries were compiled for a newer CPU
than the one running them. It is not a bug in your program. Use a build of Qt
matching the machine, or run on a physical machine rather than an old VM.

### Does it run on Windows and macOS?

See the [Installation Guide](INSTALLATION.md) for platform-specific steps.

---

## Editing and Files

### Which file extensions does the Studio recognize?

| Language | Extensions |
| -------- | ---------- |
| BASIC | `.bas`, `.bsc` |
| APL | `.apl`, `.dyalog` |
| Logo | `.logo`, `.lg` |
| PILOT | `.pilot`, `.plt` |
| FOCAL | `.foc`, `.focal` |
| Scheme | `.scm`, `.ss`, `.scheme` |
| CHIP-8 | `.ch8`, `.c8` |
| COMAL | `.comal`, `.cml` |
| REXX | `.rexx`, `.rex` |
| Smalltalk | `.st`, `.smalltalk` |

Unknown extensions open as BASIC by default.

### How do I switch the language of a file?

Use the language picker in the toolbar. The file extension does not change automatically.

### Where is the documentation?

Help → Documentation opens a browser over the guides in this folder, with a
contents tree on the left; Help → Getting Started opens the student guide. The
same pages are in `docs/` in the repository.

### How do I see all keyboard shortcuts?

**Help → Keyboard Shortcuts** (`Shift+F1`) lists every key by layer, and the menus
show each command's own shortcut. `F1` is context help for the word under the cursor,
and `F10` opens the command palette.

### Can I split the editor?

Yes. **File → Split Editor Right** (`Ctrl+\`) shows a second pane; **Close Split**
(`Ctrl+Shift+\`) merges back, and **Move Tab to Other Pane** (`Ctrl+Alt+M`) sends the
current document across. The split is remembered with the session, and each pane keeps
its own documents.

### Is there a terminal?

Yes, if you switch it on: **Settings → Workspace → Enable the embedded
terminal**. A 💻 Terminal tab then appears beside Console, Errors and Turtle Log.
Commands are written to your own `$SHELL`, which starts in the current document's
folder and follows a tab change with a `cd`. It is off by default because it runs
whatever you type with your permissions — nothing there is sandboxed — and it is
removed entirely in presentation and player modes, with the shell stopped.

It is a command line, not a terminal emulator: there is no PTY, so there are no
prompts, no job control and no full-screen programs. `echo`, a compiler or a
script work; `vim` and `top` do not.

### Can I find and replace text?

Yes. `Ctrl+F` finds in the current document, `Ctrl+H` replaces, `F3` and `Shift+F3`
find the next and previous match with the bar closed, `Ctrl+Shift+F` searches every
file, and `Ctrl+L` goes to a line. The editor also suggests words as you type
(`Ctrl+Space` asks anyway), and Settings can show a minimap and render whitespace.

### How do I insert a code snippet?

**Edit → Insert Snippet...** (`Ctrl+Shift+I`) or the Snippets toolbar button opens the
picker, filtered to the current language. Choosing one inserts it at the cursor as a
single undo step, resolving `$CURSOR$` and `$SELECTION$`.

### Can I work with several files in one project?

Documents are tabs, and recent files are remembered. There is no project panel
yet.

### Are my unsaved changes lost if the app closes?

The window asks before closing a modified tab, and autosave keeps a copy of any
document with unsaved changes, so a crash costs at most the last couple of
minutes. Save your work with `Ctrl+S` as usual.

---

## Running Programs

### How do I run a program?

Press `F9`, or use the ▶ Run button in the toolbar. To stop it, press the Stop
button; a program waiting for input can be stopped that way too.

### How do I stop a running program?

Use the Stop button in the toolbar. A program that waits for input can also be stopped this way.

### How do I get input from the user?

Use the input command of your language: `INPUT` in BASIC, `INPUT` in COMAL, `A:` (accept) in PILOT, or `PULL` in REXX.

### Why does my program run forever?

It may contain an endless loop, such as a `GOTO` with no exit condition or a
`while` loop whose condition never becomes false. The line-by-line engine stops a
runaway run on its own budget (2,000,000 steps or 10 seconds, whichever comes first)
and says so; CHIP-8 has its own opcode and frame limits. The Stop button ends either
sooner, at the next statement or instruction.

### Can I run a program from a lesson or example directly?

Yes. Open the file from `Examples/` and press `F9`. The [examples catalog](../Examples/CATALOG.md) lists the sample programs.

---

## Languages

### What can I do in BASIC?

Line-numbered programs with `PRINT`, `INPUT`, `FOR`/`NEXT`, `IF`/`THEN`, `GOTO`,
`GOSUB`, `DIM` arrays, `SUB`/`FUNCTION`, records, the file channel and the VGA
graphics statements — plus Logo's turtle words (`FORWARD`, `RIGHT`, `PENUP`,
`CLEARSCREEN`), which a BASIC line is forwarded to when BASIC does not recognise
it. See [basic.md](tutorials/basic.md).

### What about PILOT?

PILOT is a simple language for interactive lessons. Commands like `T:` (type) and `A:` (accept) handle text and input. See [pilot.md](tutorials/pilot.md).

### What subset of Logo is supported?

Core Logo commands such as `FORWARD`, `BACK`, `RIGHT`, `LEFT`, `PENUP`, `PENDOWN`, `REPEAT`, `CLEARSCREEN`, and procedure definitions with `TO`/`END`. See [logo.md](tutorials/logo.md) for the full list.

### Which Logo colors can I use?

Named colors, hex values, and RGB values all work with `SETPENCOLOR`. See [logo.md](tutorials/logo.md) for the syntax.

### What can I do in COMAL?

COMAL brings structure to BASIC: `IF … ENDIF`, `FOR … NEXT`, `WHILE … ENDWHILE`, `REPEAT … UNTIL`, and procedures declared with `PROC … ENDPROC` and run with `CALL`. See [comal.md](tutorials/comal.md).

### What can I do in REXX?

REXX is a typeless string-processing language: `SAY` to display, `PULL` to read input, `DO` loops, `SELECT`/`WHEN`, `PARSE` to split text, and subroutines with labels and `CALL`. See [rexx.md](tutorials/rexx.md).

### What can I do in Scheme?

Scheme is a small Lisp dialect with lexical closures. Define procedures with `define`/`lambda`, branch with `if`/`cond`, loop with recursion or named `let`, and process lists with `map`, `filter` and `foldl`. See [scheme.md](tutorials/scheme.md).

### What about APL, FOCAL, CHIP-8 and Smalltalk?

APL is array programming with its glyphs and keyword spellings; FOCAL is a
line-numbered formula calculator; CHIP-8 is a bytecode machine with an
integrated assembler; Smalltalk is a message-sending object system with real
blocks and a class library. Each has a tutorial in `docs/tutorials/` and a
reference section in [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md).

### Which language should a beginner start with?

BASIC, Logo or COMAL are common first choices, since their output and drawings are easy to see. REXX and Scheme are good next steps once you are comfortable with variables and loops.

---

## Turtle and Graphics

### Where does turtle output appear?

On the graphics canvas. The canvas is separate from the text output panel.

### How do I zoom or pan the canvas?

Use View → Zoom In and View → Zoom Out.

### How do I clear the canvas?

Run a new program (the canvas clears per run), or use the clear command for your language, such as `CLEARSCREEN` in Logo or `HOME` in COMAL and REXX.

### Can I export a drawing?

Yes. **File → Export Drawing...** (`Ctrl+E`) writes PNG, JPEG or BMP, and SVG
too when the build was made with Qt's SVG component.
**Edit → Copy Drawing** (`Ctrl+Shift+C`) puts it on the clipboard. **File →
Export Code as PDF...** writes the program itself as a printable A4 PDF with
line numbers, and **File → Export Session as HTML...** writes one self-contained
page holding the program, its output and the drawing, which is the file to hand
in or put on a projector. **File → Print Code...** and **File → Print
Drawing...** print directly; see [How do I print?](#how-do-i-print).

### Does the turtle work in every language?

Logo, BASIC, COMAL, REXX, Scheme, FOCAL, APL and Smalltalk have turtle words.
PILOT draws through its `G:` graphics commands, and CHIP-8 paints rectangles
directly. Check each language's tutorial for its graphics commands.

---

## Debugging

### Does the Studio have a debugger?

Yes, for every language. The 🐞 Debug tab, the toolbar's Debug/Continue/Trace actions and gutter breakpoints stop the run at the language's natural boundary: a statement for BASIC, FOCAL, PILOT, COMAL, APL and REXX; a top-level statement for Logo; a message send for Smalltalk; an instruction before CHIP-8 executes it (the tab shows the listing line, or "the next instruction" for a binary ROM). **Continue** runs to the next breakpoint and **Trace** stops at the next boundary.

Scheme additionally reports the call stack and the paused frame's bindings. For the other languages the 🔍 Variables tab shows the bindings the run is stopped in. Not yet: watch expressions and data breakpoints, tracked in [ROADMAP.md](ROADMAP.md).

### How do I set a breakpoint?

Click in the editor's gutter, or put the cursor on the line and press **Ctrl+F8** (or use **Add** in the Debug tab). Breakpoints are kept per file, so reopening a program restores them, and a line outside the document is refused rather than remembered for a line that does not exist.

### How do I start debugging?

Set a breakpoint and Run (F9): the program stops before the statement, message send or instruction on that line. **Debug** on the toolbar raises the Debug tab and runs. While a program runs, **Pause** stops it at the next boundary. **Continue** resumes it.

### What do the step commands do?

For Scheme, **Step In**, **Step Over** and **Step Out** advance the machine by one form, entering or stepping past a call. For every other language there is one step unit, the boundary named above, and all three buttons do the same thing as **Trace**: stop again at the next statement, message send or instruction.

### Can I see variable values while debugging?

Yes, in two places. The 🔍 **Variables** tab lists every binding the engine can see with its type and value, filtered by name — after a run as well as while paused, because a run that never paused still leaves bindings behind. The Debug tab shows the bindings of the frame you are stopped in.

### Can I profile a program?

Yes, for the line-by-line languages (BASIC, FOCAL, PILOT, COMAL, APL and REXX).
Tick **View → Profile Run** and run the program: each statement is timed, and
when the run ends a report lists the lines longest first with their hit count,
total time, share of the run and source text. It is off at every launch on
purpose — a clock read around every statement costs, and a remembered "on" would
make every later run pay for a report nobody asked to see. Whole-program
languages (Logo, Scheme, Smalltalk, CHIP-8) have no statement loop to time, so
the console says so instead.

### Can I go back in time?

No. The engine records nothing per step, and reverse execution is not planned.

---

## Saving, Autosave, and Version History

### How do I save a file?

Press `Ctrl+S` or use File → Save. Use File → Save As to save under a new name.

### What does autosave do?

Every two minutes it keeps a copy of each document that has unsaved changes under
the Studio's data directory, five copies per document. Untitled buffers are
covered too. The interval and the on/off switch are in Settings, and nothing is
written beside your file.

### Where is my version history?

File → Version History (`Ctrl+Alt+V`) lists the saved copies of the current
document, newest first, with a preview. Opening one puts it in a new tab; the
file on disk is never overwritten.

### Does the Studio have Git integration?

No. Use Git or another tool outside the Studio for version control.

### Where are my settings and progress stored?

- Preferences and window settings use Qt QSettings.
- Lesson progress is stored in `~/.time_warp/progress.json`, and a class's
  rosters and handouts in `~/.time_warp/classroom`. `TIME_WARP_DATA_DIR` moves
  both, which is how a test avoids writing a learner's real file.

---

## Exporting and Printing

### How do I export a program?

**File → Export Code as PDF...** writes the program as an A4 PDF with line
numbers, and **File → Export Session as HTML...** writes the program, its output
and the drawing as one self-contained page. A lesson session exports as Markdown
or PDF from the same menu. The drawing itself exports from
[Turtle and Graphics](#turtle-and-graphics).

### How do I print?

**File → Print Code...** (`Ctrl+P`) prints the current program as the same A4
page the PDF export writes, and **File → Print Drawing...** prints the drawing
scaled to fit the page. Both open the system print dialog, so the printer, paper
size and page range are chosen there. The two entries are present only when the
build was made with Qt's PrintSupport module; a build without it does not show
them, and the PDF and HTML exports still work.

---

## Lessons and Learning

### Where are the lessons?

The 🎓 **Lessons** tab is on the right, beside Output, Variables, Telemetry and
Debug. It shows the curriculum and your progress. From a terminal, use
`python3 native/tools/learn.py`.

### Do lessons save my progress?

Yes. Progress is stored in `~/.time_warp/progress.json`.

### Can I skip the welcome tab?

There is no welcome tab.

### Where can I find more examples?

The `Examples/` folder has sample programs for each language. The [examples catalog](../Examples/CATALOG.md) describes them.

---

## Appearance and Settings

### How many themes are there?

There are 33. **View → Theme** lists them — Dracula, Monokai, Catppuccin Mocha,
Gruvbox, Nord, Solarized, the retro CRT sets and the rest — and **View → Next Theme**
cycles through them. **View → High Contrast** (`Ctrl+Alt+H`) applies a maximum-contrast variant of
whichever theme is active, and **View → Screen Mode** pairs the CRT settings with the
amber, green and white-phosphor looks.

### Can I change the font size?

Yes. The **A+** button in the toolbar (or `Ctrl+Alt+A`) steps the interface text
through 100%, 125% and 150%. **Settings** sets the editor's font family, point size and
tab width, and the change is previewed live before it is applied.

---

## Troubleshooting

### My program shows a syntax error. What should I check?

Read the message carefully. It shows the line number and a hint when one is available. Common causes include a missing line number in BASIC or an unmatched `END` in Logo.

### The canvas is blank after running a turtle program. Why?

Check that the program actually moves the turtle with `penup` or `pendown` set correctly. Look for an error message in the output panel.

### Where are the full troubleshooting steps?

See [INSTALLATION.md](INSTALLATION.md) for what a failure at each build step
means, and [ROADMAP.md](ROADMAP.md) for what is deliberately not built yet.

### How do I report a bug?

Open an issue on the project's GitHub repository. Include your OS, Qt version
(`make -C native info` prints it), the language, a short program that reproduces
the problem, and the error message.

---

## Contributing and Project Information

### How can I contribute?

Read [CONTRIBUTING.md](../CONTRIBUTING.md) and the [architecture guide](ARCHITECTURE.md). For a new language, see [Adding a New Language](ARCHITECTURE.md#adding-a-new-language).

### Where do I report a security issue?

Follow the process in [SECURITY.md](../SECURITY.md).

### Who maintains the project?

James Temple <james@honey-badger.org>.

### Where can I read the code of conduct?

See [CODE_OF_CONDUCT.md](../CODE_OF_CONDUCT.md).

### Is there a plugin system?

No. The IDE has no plugin loading, and no plugin API is planned in
[ROADMAP.md](ROADMAP.md).

### What is on the roadmap?

See [ROADMAP.md](ROADMAP.md).

---

**Still stuck?** Start with [README.md](../README.md) and\n[ARCHITECTURE.md](ARCHITECTURE.md), or open an issue on the project's GitHub\nrepository.
