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

Time Warp Studio is an educational desktop programming environment. It runs six
programming languages in one IDE, with an integrated turtle graphics canvas and
an output console. It is a C++20 / Qt6 application; the PySide6 implementation
that preceded it has been removed.

### Which languages are supported?

BASIC, PILOT, Logo, COMAL, REXX, and Scheme, and Python. Each language implements a subset of its traditional form. See the [Language Guide](LANGUAGE_GUIDE.md) for what each one supports.

### Is Time Warp Studio free?

Yes. It is released under the MIT License. See [LICENSE](../LICENSE).

### Who is it for?

Students, teachers, and anyone who wants to learn programming or compare languages. It is designed for learning first, so error messages and output use plain language and emoji prefixes.

### Does it need an internet connection?

No. The core features work offline.

---

## Installation and Startup

### What do I need to run it?

- A C++20 compiler, CMake 3.20+, and Qt6 (Widgets, Core, Gui)
- Python 3.10 or newer, **only** if you use BASIC, PILOT, Logo, COMAL or REXX —
  Scheme and expressions run natively without it

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
| PILOT | `.pilot`, `.plt` |
| Logo | `.logo`, `.lg` |
| COMAL | `.comal`, `.cml` |
| REXX | `.rexx`, `.rex` |
| Scheme | `.scm`, `.ss` |

Unknown extensions open as BASIC by default.

### How do I switch the language of a file?

Use the language picker (`Ctrl+Shift+L`). The file extension does not change automatically.

### How do I open the Command Palette?

Press `Ctrl+Shift+P`. It lists the commands in the Studio.

### How do I see all keyboard shortcuts?

Press `Ctrl+?` or open the Help menu.

### Can I split the editor?

Yes. `Ctrl+\` splits the editor view and `Ctrl+Shift+\` closes the split. `Ctrl+Alt+R` runs the programs in both panes.

### Can I find and replace text?

Yes. `Ctrl+F` finds, `Ctrl+H` replaces, `Ctrl+Alt+G` searches across files, and `Ctrl+G` jumps to a line.

### How do I insert a code snippet?

Press `Ctrl+Shift+I` to use snippets.

### Can I work with several files in one project?

Yes, in part. The project panel lists the files in a project. You can add a new file, add an existing file, rename or remove files, and set the main file.

### Are my unsaved changes lost if the app closes?

Autosave can recover them. Enable it in Preferences. See [Saving, Autosave, and Version History](#saving-autosave-and-version-history).

---

## Running Programs

### How do I run a program?

Press `Ctrl+R` or use the Run menu. To run only the selected text, use `Ctrl+Shift+R`. To run both split-view programs, use `Ctrl+Alt+R`.

### How do I stop a running program?

Use Run → Stop. A program that waits for input can also be stopped this way.

### How do I get input from the user?

Use the input command of your language: `INPUT` in BASIC, `INPUT` in COMAL, `A:` (accept) in PILOT, or `PULL` in REXX.

### Why does my program run forever?

It may contain an endless loop, such as a `GOTO` with no exit condition or a `while` loop whose condition never becomes false. Use Stop, check the loop logic, or step through it with the debugger.

### Can I run a program from a lesson or example directly?

Yes. Open the file from `Examples/` and press `Ctrl+R`. The [examples catalog](../Examples/CATALOG.md) lists the sample programs.

---

## Languages

### What can I do in BASIC?

Line-numbered programs with `PRINT`, `INPUT`, `FOR`/`NEXT`, `IF`/`THEN`, `GOTO`, `GOSUB`, and `DIM` arrays. See [basic.md](tutorials/basic.md).

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

Yes. Use File → Export → Export Graphics to PNG or Export Graphics to SVG. You can also print graphics from File.

### Does the turtle work in every language?

Logo, BASIC, COMAL, REXX, and Scheme have turtle commands. Check each language's tutorial for its graphics commands.

---

## Debugging

### Does the Studio have a debugger?

For **Scheme**, yes: the 🐞 Debug tab drives a real runner with breakpoints, stepping, a call stack, the paused line and that frame's variables. The other nine languages run to completion with no pause protocol — four in the Python bridge's process, five in the native runner — so the Debug tab disables itself and says why.

Not yet: watch expressions, an execution timeline, and the toolbar's Debug/Continue/Trace actions (the panel's own buttons work). Those are tracked in [ROADMAP.md](ROADMAP.md).

### How do I set a breakpoint?

In the Debug tab: type a line number and press **Add**. Breakpoints are listed there and **Remove** takes the selected one off. The editor's gutter does not take breakpoint clicks yet.

### How do I start debugging?

Run the program as usual (F9) and press **Pause** in the Debug tab, or set a breakpoint first: the program stops there. **Continue** resumes it.

### What do the step commands do?

**Step In**, **Step Over** and **Step Out** advance the Scheme machine by one form, entering or stepping past a call. The granularity is a form rather than a line, because that is what the explicit-stack machine executes.

### Can I see variable values while debugging?

Yes, in two places. The 🔍 **Variables** tab lists every binding the engine can see with its type and value, filtered by name — after a run as well as while paused, because a run that never paused still leaves bindings behind. The Debug tab shows the bindings of the frame you are stopped in.

### Can I go back in time?

No. The engine records nothing per step, and reverse execution is not planned.

---

## Saving, Autosave, and Version History

### How do I save a file?

Press `Ctrl+S` or use File → Save. Use File → Save As to save under a new name.

### What does autosave do?

Autosave saves your open work at a set interval, so changes are not lost after a crash. Turn it on or change the interval in File → Preferences (`Ctrl+,`).

### Where is my version history?

Use File → Version History (`Ctrl+Alt+V`) to open the saved versions of the current file.

### Does the Studio have Git integration?

No. Use Git or another tool outside the Studio for full version control. The built-in Version History covers simple local recovery.

### Where are my settings and progress stored?

- Preferences and window settings use Qt QSettings.
- Lesson progress is stored in `~/.time_warp/progress.json`.

---

## Exporting and Printing

### How do I export a program?

Use File → Export. Options include:

- Export Code as PDF
- Export Session as HTML (`Ctrl+Shift+H`)
- Export Lesson as Markdown or PDF

### How do I print?

Use File → Print Code (`Ctrl+P`) or Print Graphics.

---

## Lessons and Learning

### Where are the lessons?

Use Help → Lessons (`Ctrl+Alt+L`). Your lesson progress is saved automatically.

### Do lessons save my progress?

Yes. Progress is stored in `~/.time_warp/progress.json`.

### Can I skip the welcome tab?

Yes. Open File → Preferences and clear the Show Welcome checkbox. The tab returns on the next launch if you turn it back on.

### Where can I find more examples?

The `Examples/` folder has sample programs for each language. The [examples catalog](../Examples/CATALOG.md) describes them.

---

## Appearance and Settings

### How many themes are there?

33, including Dracula, Monokai, Catppuccin Mocha, Gruvbox Dark, Nord, Solarized, GitHub Dark and Light, VS Code Dark and Light, and retro CRT themes. Change the theme from the View menu. The choice persists between sessions.

### Can I change the font size?

Yes. Use View → Font → Font Size.

---

## Troubleshooting

### My program shows a syntax error. What should I check?

Read the message carefully. It shows the line number and a hint when one is available. Common causes include a missing line number in BASIC or an unmatched `END` in Logo.

### The canvas is blank after running a turtle program. Why?

Check that the program actually moves the turtle with `penup` or `pendown` set correctly. Look for an error message in the output panel.

### Where are the full troubleshooting steps?

See the [Troubleshooting Guide](TROUBLESHOOTING_GUIDE.md) and [troubleshooting guide](guides/08-troubleshooting.md).

### How do I report a bug?

Open an issue on the project's GitHub repository. Include your OS, Python version, the language, a short program that reproduces the problem, and the error message.

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

Plugin support is planned but not implemented. Plugin loading is not active in the current release.

### What is on the roadmap?

See [ROADMAP.md](ROADMAP.md).

---

**Still stuck?** Start with the [User Guide](USER_GUIDE.md), or open an issue on the project's GitHub repository.
