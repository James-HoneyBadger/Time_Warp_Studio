# Time Warp Studio - Frequently Asked Questions

Answers to common questions about installing, running, and learning with Time Warp Studio. For full guides, see the [User Guide](docs/USER_GUIDE.md) and the [documentation index](docs/INDEX.md).

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

Time Warp Studio is an educational desktop programming environment. It runs nine programming languages in one IDE, with an integrated turtle graphics canvas, a debugger, lessons, and export tools. It is built with Python and PySide6 (Qt6).

### Which languages are supported?

BASIC, PILOT, Logo, C, Pascal, Prolog, Forth, Brainfuck, and Python. Each language implements a subset of its traditional form. See the [Language Guide](docs/LANGUAGE_GUIDE.md) for what each one supports.

### Is Time Warp Studio free?

Yes. It is released under the MIT License. See [LICENSE](LICENSE).

### Who is it for?

Students, teachers, and anyone who wants to learn programming or compare languages. It is designed for learning first, so error messages and output use plain language and emoji prefixes.

### Does it need an internet connection?

No. The core features work offline.

---

## Installation and Startup

### What do I need to run it?

- Python 3.10 or newer
- PySide6 (6.5 or newer) and Pillow (10 or newer), listed in `Platforms/Python/requirements.txt`
- A CPU with SSSE3, SSE4.1, SSE4.2, and POPCNT support. Older virtual machines and QEMU setups may lack these features.

### How do I install the dependencies?

From the repository root:

```bash
pip install -r Platforms/Python/requirements.txt
```

### How do I start the IDE?

```bash
python3 Platforms/Python/time_warp_ide.py
```

You can also run `python3 run.py`, which sets up a virtual environment and dependencies if needed.

### The app crashes at startup with "Illegal instruction." What is wrong?

This usually means the CPU does not support one of the instruction sets PySide6 needs. It is not a bug in your program. Try a physical machine or a VM with a newer CPU model. See [Troubleshooting](docs/TROUBLESHOOTING_GUIDE.md).

### Does it run on Windows and macOS?

See the [Installation Guide](docs/INSTALLATION.md) for platform-specific steps.

---

## Editing and Files

### Which file extensions does the Studio recognize?

| Language | Extensions |
|----------|------------|
| BASIC | `.bas` |
| PILOT | `.pilot` |
| Logo | `.logo` |
| C | `.c` |
| Prolog | `.pl`, `.pro`, `.prolog` |
| Pascal | `.pas` |
| Forth | `.f`, `.forth`, `.fs` |
| Brainfuck | `.bf` |
| Python | `.py` |

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

Use the input command of your language: `INPUT` in BASIC and `A:` (accept) in PILOT. Python's `input()` does not wait for typed text. It returns an empty string, so use BASIC or PILOT for programs that need user answers.

### Why does my program run forever?

It may contain an endless loop, such as a `GOTO` with no exit condition or a `while` loop whose condition never becomes false. Use Stop, check the loop logic, or step through it with the debugger.

### Can I run a program from a lesson or example directly?

Yes. Open the file from `Examples/` and press `Ctrl+R`. The [examples catalog](Examples/CATALOG.md) lists the sample programs.

---

## Languages

### What can I do in BASIC?

Line-numbered programs with `PRINT`, `INPUT`, `FOR`/`NEXT`, `IF`/`THEN`, `GOTO`, `GOSUB`, and `DIM` arrays. See [basic.md](docs/tutorials/basic.md).

### What about PILOT?

PILOT is a simple language for interactive lessons. Commands like `T:` (type) and `A:` (accept) handle text and input. See [pilot.md](docs/tutorials/pilot.md).

### What subset of Logo is supported?

Core Logo commands such as `FORWARD`, `BACK`, `RIGHT`, `LEFT`, `PENUP`, `PENDOWN`, `REPEAT`, `CLEARSCREEN`, and procedure definitions with `TO`/`END`. See [logo.md](docs/tutorials/logo.md) for the full list.

### Which Logo colors can I use?

Named colors, hex values, and RGB values all work with `SETPENCOLOR`. See [logo.md](docs/tutorials/logo.md) for the syntax.

### Is C a full C compiler?

No. The C executor is an interpreter for a practical subset of C: functions, variables, loops, arrays, and `printf`. It does not compile to native code. See [c.md](docs/tutorials/c.md).

### What can I do in Prolog?

Facts, rules, queries, and backtracking over a small knowledge base. See [prolog.md](docs/tutorials/prolog.md).

### What can I do in Forth?

Stack-based arithmetic, word definitions with `:` and `;`, and loops. Forth programs can also use turtle words. See [forth.md](docs/tutorials/forth.md).

### How does Brainfuck work here?

Brainfuck runs in a whole-program executor with the eight standard commands. The `,` command prompts for input. See [brainfuck.md](docs/tutorials/brainfuck.md).

### Can I use Python?

Yes. Python runs in a sandboxed `exec()` environment. Most built-in functions and operators work, along with these modules:

`math`, `random`, `itertools`, `functools`, `collections`, `string`, `json`, `re`, `copy`, `abc`, `typing`, `decimal`, `fractions`, `statistics`, `heapq`, `bisect`, `operator`, `datetime`, `enum`, `dataclasses`, `time`

### Why can't I import `os`, `sys`, or `turtle`?

For safety, the Python executor blocks system modules such as `os`, `sys`, `subprocess`, and `socket`. File access is also blocked. The `turtle` module is not available as an import, but the executor provides built-in turtle helpers:

- `forward(n)`, `backward(n)`, `right(n)`, `left(n)`
- `penup()`, `pendown()`, `goto(x, y)`, `home()`, `setheading(deg)`
- `color(r, g, b)`, `pensize(n)`, `clear_canvas()`

### Can I write a Python function and call it from the turtle helpers?

Yes. Define your own functions and call the helpers from inside them, as in any normal Python program.

### Which language should a beginner start with?

BASIC or Logo are common first choices, since their output and drawings are easy to see. Python is a good next step once you are comfortable with variables and loops.

---

## Turtle and Graphics

### Where does turtle output appear?

On the graphics canvas. The canvas is separate from the text output panel.

### How do I zoom or pan the canvas?

Use View → Zoom In and View → Zoom Out.

### How do I clear the canvas?

Run a new program (the canvas clears per run), or use the clear command for your language, such as `CLEARSCREEN` in Logo or `clear_canvas()` in Python.

### Can I export a drawing?

Yes. Use File → Export → Export Graphics to PNG or Export Graphics to SVG. You can also print graphics from File.

### Does the turtle work in every language?

Logo, Forth, and Python have turtle commands. Check each language's tutorial for its graphics commands.

---

## Debugging

### Does the Studio have a debugger?

Yes. It supports breakpoints, stepping, a variables panel, a call stack, and an execution timeline. See the [Debugger Guide](docs/DEBUGGER_GUIDE.md).

### How do I set a breakpoint?

Click the left margin of the editor, or press `Ctrl+F8` on the current line. Press `Ctrl+Shift+F8` to clear all breakpoints.

### How do I start debugging?

Use Debug → Start / Continue Debugging. The program pauses at the first breakpoint, or at the first statement if none are set.

### What do the step commands do?

Use the Debug menu for Trace Into, Step Over, and Step Out. Trace Into and Step Over currently perform the same action. See the [Debugger Guide](docs/DEBUGGER_GUIDE.md) for details.

### Can I see variable values while debugging?

Yes. The Variables tab shows each variable and its value when execution pauses. You can also add watch expressions.

### Can I go back in time?

The execution timeline lists each recorded step. Select an entry to jump to that point. You can export the timeline as JSON or save a replay file.

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

The `Examples/` folder has sample programs for each language. The [examples catalog](Examples/CATALOG.md) describes them.

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

See the [Troubleshooting Guide](docs/TROUBLESHOOTING_GUIDE.md) and [docs/guides/08-troubleshooting.md](docs/guides/08-troubleshooting.md).

### How do I report a bug?

Open an issue on the project's GitHub repository. Include your OS, Python version, the language, a short program that reproduces the problem, and the error message.

---

## Contributing and Project Information

### How can I contribute?

Read [CONTRIBUTING.md](CONTRIBUTING.md) and the [architecture guide](ARCHITECTURE.md). For a new language, see [Adding a New Language](ARCHITECTURE.md#adding-a-new-language).

### Where do I report a security issue?

Follow the process in [SECURITY.md](SECURITY.md).

### Who maintains the project?

James Temple <james@honey-badger.org>.

### Where can I read the code of conduct?

See [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).

### Is there a plugin system?

Plugin support is planned but not implemented. Plugin loading is not active in the current release.

### What is on the roadmap?

See [ROADMAP.md](ROADMAP.md).

---

**Still stuck?** Start with the [User Guide](docs/USER_GUIDE.md), or open an issue on the project's GitHub repository.
