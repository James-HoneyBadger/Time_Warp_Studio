# User Guide - Time Warp Studio IDE

Complete guide to using the Time Warp Studio integrated development environment.

---

## Table of Contents

1. [IDE Overview](#ide-overview)
2. [Workspace Layout](#workspace-layout)
3. [Opening and Saving Files](#opening-and-saving-files)
4. [Selecting a Language](#selecting-a-language)
5. [Writing Code](#writing-code)
6. [Running Programs](#running-programs)
7. [Using the Console](#using-the-console)
8. [Graphics Canvas](#graphics-canvas)
9. [Panels](#panels)
10. [Keyboard Shortcuts](#keyboard-shortcuts)
11. [Themes and Customization](#themes-and-customization)
12. [Menu Reference](#menu-reference)

---

## IDE Overview

Time Warp Studio provides a unified development environment with:

- **Code Editor** - Write code with syntax highlighting
- **Console** - See program output and errors
- **Graphics Canvas** - Visualize turtle graphics
- **Panels** - Lessons, Debugger, Variables, Document Outline, and more
- **Menu System** - File, Edit, Run, Debug, View, Tools, and Help menus
- **Keyboard Shortcuts** - Borland Turbo-style keymap with a Function-Key Bar

### First Time Setup

When you launch Time Warp Studio:

1. IDE initializes (2-5 seconds)
2. Config folder created at `~/.time_warp/`
3. Default theme applied
4. IDE ready for coding

---

## Workspace Layout

The IDE window is divided into sections:

```
┌─────────────────────────────────────────────────┐
│ File Edit Run Debug View Tools Help             │  Menu Bar
├─────────────────────────────────────────────────┤
│ [New] [Open] [Save] [Run] [Stop] [Theme]       │  Toolbar
├─────────────────────────────────────────────────┤
│                                                  │
│  ┌──────────────────┐  ┌──────────────────────┐ │
│  │                  │  │   Graphics Canvas    │ │
│  │   Code Editor    │  │   (Turtle Graphics)  │ │
│  │                  │  │                      │ │
│  │                  │  │                      │ │
│  ├──────────────────┤  │                      │ │
│  │  Output Console  │  └──────────────────────┘ │
│  │                  │                          │
│  └──────────────────┘  ┌──────────────────────┐ │
│                        │  Panel Tabs          │ │
│                        │  (Debug, Variables)  │ │
│                        │                      │ │
│                        └──────────────────────┘ │
├─────────────────────────────────────────────────┤
│ 1Help 2Save 3Open ... 9Run 10Menu               │  Function-Key Bar
├─────────────────────────────────────────────────┤
│ BASIC | Ln 1, Col 1 | Ready                   │  Status Bar
└─────────────────────────────────────────────────┘
```

### Main Areas

**Code Editor** (Top-Left)
- Write your program
- Syntax highlighting per language
- Line numbers
- Find/Replace support

**Output Console** (Bottom-Left)
- Program output displayed here
- Error messages highlighted
- Selectable and copyable text

**Graphics Canvas** (Right)
- Turtle graphics rendering
- Zoom and pan controls
- Real-time updates during execution

**Panel Tabs** (Bottom-Right)
- Debug (controls, call stack, timeline)
- Variables Inspector
- Lessons and Document Outline open as docks

---

## Opening and Saving Files

### Creating a New File

```
File → New [Ctrl+N]
```

Creates empty file with default language (BASIC).

### Opening an Existing File

```
File → Open [F3 / Ctrl+O]
```

File browser opens. Select any supported source file (`.bas`, `.pilot`, `.logo`, `.c`, `.pas`, `.pl` / `.pro` / `.prolog`, `.f` / `.forth` / `.fs`, `.bf`, `.py`).

### Saving Your Work

```
File → Save [F2 / Ctrl+S]
```

Saves to current file. If new file, prompts for name.

### Save As...

```
File → Save As [Shift+F2 / Ctrl+Shift+S]
```

Save with new filename or in different location.

### Recent Files

```
File → Recent Files
```

Quick access to 10 most recently opened files.

---

## Selecting a Language

Programs can be written in **9 active languages**:

### Auto-Detection

IDE automatically detects language based on:
- **File extension**: `.bas` → BASIC, `.py` → Python, `.logo` → Logo, etc.
- **Explicit dropdown**: Select language from the toolbar

### Manual Selection

```
View → Language Picker [Ctrl+Shift+L]
```

Dropdown also available in top toolbar.

### Supported Languages

| Language | Extension | Paradigm | Era |
|----------|-----------|----------|-----|
| **BASIC** | .bas | Imperative / Educational | 1964 |
| **Logo** | .logo | Turtle Graphics / Educational | 1967 |
| **PILOT** | .pilot | Computer-Aided Instruction | 1969 |
| **Forth** | .f / .forth / .fs | Stack-based / Concatenative | 1970 |
| **Pascal** | .pas | Structured / Educational | 1970 |
| **C** | .c | Systems / Procedural | 1972 |
| **Prolog** | .pl / .pro / .prolog | Logic / Declarative | 1972 |
| **Python** | .py | Multi-paradigm / Educational | 1991 |
| **Brainfuck** | .bf | Esoteric / Turing | 1993 |

---

## Writing Code

### Editor Features

**Syntax Highlighting**
- Keywords, strings, comments, and numbers are colored per language
- Token colors follow the active theme (View → Theme)

**Line Numbers**
- Shows line number on left
- Useful for debugging (breakpoints link to line numbers)

**Code Folding**
- Collapse/expand procedures and blocks
- Click the fold area in the left margin, or use Ctrl+Shift+[ to fold and Ctrl+Shift+] to unfold the current block

**Auto-Indentation**
- Automatic indentation on new line
- Tab width is fixed at 4 spaces

**Find and Replace**

```
Edit → Find [Ctrl+F]
Edit → Replace [Ctrl+H]
```

Use Ctrl+G to go to specific line number.

### Code Templates

Insert ready-made code snippets via:

```
Edit → Insert Snippet [Ctrl+Shift+I]
```

Or copy from Examples:

```
Help → Browse Examples [Shift+F3]
```

Browse 72 working examples.

---

## Running Programs

### Execute Code

```
Run → Run Program [F9 / Ctrl+R]
```

Executes the program in the current editor.

### Stop Execution

```
Run → Reset Program [Ctrl+F2]
```

Stops currently running program (if infinite loop or slow).

### Clear Output

```
Run → Clear Output
```

Clears the output console. Use Run → Clear Canvas to clear graphics.

### Execution Timing

- **Startup**: Program begins in worker thread
- **Output**: Appears in console as generated
- **Graphics**: Canvas updates in real-time
- **Completion**: Result message shown
- **Timeout**: Programs auto-stop after 10 seconds

---

## Using the Console

The Output Console displays:

**Program Output**
- Text from PRINT statements
- Numbers and strings
- Results of operations

**Error Messages**
- Syntax errors with line number
- Runtime errors with description
- Suggestions for fixes

**Graphics Output**
- Turtle graphics status
- Canvas updates

### Console Features

**Scrolling**
- Auto-scrolls to show latest output
- Manual scroll with mouse wheel
- Keyboard scroll with Page Up/Down

**Selection & Copying**
- Click and drag to select text
- Ctrl+C to copy selected text
- Right-click context menu

**Clearing**
- Run → Clear Output clears the console
- Run → Clear Canvas clears graphics
- Or start a new program

---

## Graphics Canvas

The Canvas displays turtle graphics output in real-time.

### Turtle Graphics

**Turtle Position**
- Small triangle shows turtle
- Angle: 0°=East, 90°=North, etc.

**Drawing**
- Pen can be up (not drawing) or down (drawing)
- Pen color and width configurable
- All lines saved and displayed

### Canvas Controls

**Zoom**
- Scroll wheel to zoom in/out
- Range: 0.05x to 24x magnification

**Pan**
- Click and drag to pan
- Use the Reset View (⊙) button to restore the default zoom and pan

**Grid** (Optional)
- Canvas ⋯ button toggles the coordinate grid
- Shows coordinate grid
- Helpful for positioning

### Canvas Features

**Real-Time Updates**
- Graphics update as program runs

**Reset Canvas**
- Run → Clear Canvas clears graphics
- Each new Run clears automatically
- Or Logo HOME command resets turtle

---

## Panels

Panels open as tabs on the right or as dock widgets.

### Key Panels

**Lessons** (Help → Lessons, Ctrl+F3)
- 18 built-in lessons, 2 per language
- Checkpoints with hints and automatic verification
- Progress saved between sessions

**Debugger** (🐛 Debug tab)
- Step through execution
- Set breakpoints
- View call stack and execution timeline

**Variables Inspector**
- See current variable values
- Watch variables change
- Update during debugging

**Document Outline** (F4)
- Jump to procedures, functions, and labels

**Examples Browser** (Shift+F3)
- Browse 72 working programs
- Copy and modify examples
- Learn from code

---

## Keyboard Shortcuts

Time Warp Studio uses a Borland Turbo-style keymap. Press **Shift+F1** in the IDE for the full live list, or see [KEYBOARD_SHORTCUTS.md](KEYBOARD_SHORTCUTS.md).

### File Operations
- `Ctrl+N` - New file
- `F3` / `Ctrl+O` - Open file
- `F2` / `Ctrl+S` - Save
- `Shift+F2` / `Ctrl+Shift+S` - Save As
- `Shift+F3` - Browse Examples
- `Alt+X` / `Ctrl+Q` - Exit

### Editing
- `Ctrl+Z` - Undo
- `Ctrl+Y` - Redo
- `Ctrl+X` - Cut
- `Ctrl+C` - Copy
- `Ctrl+V` - Paste
- `Ctrl+A` - Select All
- `Ctrl+F` - Find
- `Ctrl+H` - Replace
- `Ctrl+G` - Go to Line

### Execution
- `F9` / `Ctrl+R` - Run program
- `Ctrl+F2` - Reset (stop) program
- `Shift+F9` / `Ctrl+Shift+R` - Run selection

### Debugging
- `Ctrl+F9` - Start debugging / Continue
- `F7` - Trace Into
- `F8` - Step Over
- `Shift+F8` - Step Out
- `Ctrl+F8` - Toggle Breakpoint

### View
- `Ctrl+Plus` - Increase editor font
- `Ctrl+Minus` - Decrease editor font
- `F5` - Zoom (focus mode)
- `F10` / `Ctrl+Shift+P` - Command Palette

### Navigation
- `Page Up` - Scroll up
- `Page Down` - Scroll down
- `Home` - Go to start of line
- `End` - Go to end of line
- `Ctrl+Home` - Go to start of file
- `Ctrl+End` - Go to end of file

---

## Themes and Customization

### Selecting a Theme

```
View → Theme → [Select theme]
```

33 built-in themes:

**Workstation**: Neo-90s Workstation, Borland Turbo Vision 1991, NeXTSTEP Gunmetal, Amiga Workbench 2.0, VGA Amber Lab
**Dark**: Dracula, Monokai, VS Code Dark, GitHub Dark, Nord, One Dark Pro, Solarized Dark, Ocean, Catppuccin Mocha, Gruvbox Dark, Tokyo Night
**Light**: Gruvbox Light, Catppuccin Latte, VS Code Light, GitHub Light, Solarized Light, Spring
**High Contrast**: High Contrast Dark, High Contrast Light
**Accessibility**: Dyslexia Friendly, Accessible Dark Blue
**Retro CRT**: Amber Monochrome, Green Monochrome, IBM PC CGA, Commodore 64, Apple II, DOS Blue, ZX Spectrum

### Customizing Editor

Font family and size: **View → Font**. Font size also changes with Ctrl++ and Ctrl+-.

Tab width is fixed, auto-indent is built in, and line numbers are always shown. See [Settings](guides/06-settings.md) for the full list of fixed editor behavior.

### Theme Persistence

Your theme choice is saved automatically (Qt QSettings) and restored on next launch.

---

## Menu Reference

### File Menu

- **New** [Ctrl+N] - New file
- **Open** [F3 / Ctrl+O] - Open file
- **Save** [F2 / Ctrl+S] - Save current
- **Save As** [Shift+F2 / Ctrl+Shift+S] - Save with new name
- **Recent Files** - Quick access
- **Export** - Graphics to PNG/SVG, Session as HTML [Ctrl+Shift+H], Lesson Report (Markdown/PDF)
- **Print Code** [Ctrl+P], **Print Graphics**, **Export Code as PDF**
- **Version History** [Ctrl+Alt+V]
- **Preferences** [Ctrl+,] - Preferences dialog
- **Exit** [Alt+X / Ctrl+Q] - Quit IDE

### Edit Menu

- **Undo** [Ctrl+Z]
- **Redo** [Ctrl+Y]
- **Cut** [Ctrl+X]
- **Copy** [Ctrl+C]
- **Paste** [Ctrl+V]
- **Find** [Ctrl+F]
- **Find & Replace** [Ctrl+H]
- **Find in Files** [Shift+F4 / Ctrl+Alt+G]
- **Go to Line** [Ctrl+G]
- **Insert Snippet** [Ctrl+Shift+I]
- **Format Code** [Ctrl+Shift+F]

### Run Menu

- **Run Program** [F9 / Ctrl+R] - Execute program
- **Reset Program** [Ctrl+F2] - Stop a run or debug session
- **Run Selection** [Shift+F9 / Ctrl+Shift+R] - Execute selected code
- **Run Both Split Panes** [Ctrl+Alt+R] - Polyglot benchmark
- **Clear Output** / **Clear Canvas**
- **Show Errors Only** [Ctrl+F5]

### Debug Menu

- **Start / Continue Debugging** [Ctrl+F9]
- **Stop Debugging**
- **Pause** [Ctrl+F7] - Pause running program
- **Trace Into** [F7] - Enter functions
- **Step Over** [F8] - Skip functions
- **Step Out** [Shift+F8] - Exit function
- **Toggle Breakpoint** [Ctrl+F8]
- **Clear All Breakpoints** [Ctrl+Shift+F8]

### View Menu

- **Theme** - Select, preview, edit, or cycle themes [Shift+F10] (33 built-in)
- **Font** - Family and size
- **Zoom In / Zoom Out** [Ctrl+Plus / Ctrl+Minus]
- **Screen Mode** and **CRT Effects** [Shift+F7]
- **Split Editor Right** [Shift+F5 / Ctrl+\\], **Close Split** [Ctrl+Shift+\\]
- **Zoom (Focus Mode)** [F5], **Next Pane** [F6]
- **Telemetry HUD** [Shift+F6]
- **Command Palette** [F10 / Ctrl+Shift+P], **Language Picker** [Ctrl+Shift+L]
- **Workspace Decks** [Alt+1..Alt+4]
- **Show Minimap**, **Document Outline** [F4]

### Tools Menu

- **Compare Languages**

### Help Menu

- **User Manual**
- **Context Help** [F1]
- **Keyword Inspector** [Ctrl+F1]
- **Lessons** [Ctrl+F3 / Ctrl+Alt+L]
- **Quick Reference**, **Documentation Index**, **Language Help**
- **Browse Examples** [Shift+F3]
- **Start Guided Tour**
- **Keyboard Shortcuts** [Shift+F1 / Ctrl+?]
- **About Time Warp Studio**

---

## Tips & Tricks

### Quick Development Workflow

1. Open Examples (`Shift+F3`) to see working code
2. Create new file (`Ctrl+N`)
3. Type or paste code
4. Save (`F2`)
5. Run (`F9`)
6. View output in console and canvas
7. Edit as needed and re-run

### Debugging Programs

1. Set breakpoints with `Ctrl+F8`
2. Start debugging with `Ctrl+F9`
3. Execution stops at breakpoint
4. Use Trace Into (`F7`) / Step Over (`F8`) to navigate
5. Watch variables in Variables Inspector
6. View the Timeline in the Debug tab to see execution history

### Learning New Languages

1. Open the Lessons dock (`Ctrl+F3`) and pick a lesson for the language
2. Or open the Examples browser (`Shift+F3`) and filter by language
3. Browse from simple to complex
4. Load an example into the editor
5. Modify and experiment
6. Run (`F9`) to test

---

**For language-specific help, see:**
- [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) - Syntax for all 9 languages
- [DEBUGGER_GUIDE.md](DEBUGGER_GUIDE.md) - Using the debugger
- [Turtle Graphics](guides/04-turtle-graphics.md) - Graphics programming
- [FAQ](../FAQ.md) - Frequently asked questions
- [TROUBLESHOOTING_GUIDE.md](TROUBLESHOOTING_GUIDE.md) - Common problems
