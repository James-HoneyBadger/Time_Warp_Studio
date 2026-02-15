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
9. [Feature Panels](#feature-panels)
10. [Keyboard Shortcuts](#keyboard-shortcuts)
11. [Themes and Customization](#themes-and-customization)
12. [Menu Reference](#menu-reference)

---

## IDE Overview

Time Warp Studio provides a unified development environment with:

- **Code Editor** - Write code with syntax highlighting
- **Console** - See program output and errors
- **Graphics Canvas** - Visualize turtle graphics
- **Feature Panels** - Specialized tools for different tasks
- **Menu System** - 53 functions across 6 menus
- **Keyboard Shortcuts** - Fast workflow efficiency

### First Time Setup

When you launch Time Warp Studio:

1. IDE initializes (2-5 seconds)
2. Config folder created at `~/.Time_Warp/`
3. Default theme applied (Dracula)
4. All 14 feature panels available
5. IDE ready for coding

---

## Workspace Layout

The IDE window is divided into sections:

```
┌─────────────────────────────────────────────────┐
│ File Edit Run Debug View Help                   │  Menu Bar
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
│                        │  Feature Panels      │ │
│                        │  (14 tools)         │ │
│                        │                      │ │
│                        └──────────────────────┘ │
├─────────────────────────────────────────────────┤
│ Ready | Turtle: (0,0,0°) | Mem: 45MB          │  Status Bar
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

**Feature Panels** (Bottom-Right)
- Lesson Mode
- AI Assistant
- Error Explainer
- And 11 others

---

## Opening and Saving Files

### Creating a New File

```
File → New [Ctrl+N]
```

Creates empty file with default language (BASIC).

### Opening an Existing File

```
File → Open [Ctrl+O]
```

File browser opens. Select any `.bas`, `.logo`, `.pilot`, `.c`, `.pas`, `.pro`, or `.f` file.

### Saving Your Work

```
File → Save [Ctrl+S]
```

Saves to current file. If new file, prompts for name.

### Save As...

```
File → Save As [Ctrl+Shift+S]
```

Save with new filename or in different location.

### Recent Files

```
File → Recent Files
```

Quick access to 10 most recently opened files.

---

## Selecting a Language

Programs can be written in 7 different languages:

### Auto-Detection

IDE automatically detects language based on:
- **File extension**: `.bas` → BASIC, `.logo` → Logo
- **File content**: First comment or statement
- **Explicit menu**: Select language from Run menu

### Manual Selection

```
Run → Set Language → [Select from list]
```

Dropdown also available in top toolbar.

### Supported Languages

| Language | Extension | Paradigm |
|----------|-----------|----------|
| **BASIC** | .bas | Procedural |
| **Logo** | .logo | Visual/Graphics |
| **PILOT** | .pilot | Interactive Teaching |
| **C** | .c | Systems |
| **Pascal** | .pas | Structured |
| **Prolog** | .pro | Logic |
| **Forth** | .f | Stack-based |

---

## Writing Code

### Editor Features

**Syntax Highlighting**
- Keywords in blue
- Strings in green
- Comments in gray
- Numbers in purple

**Line Numbers**
- Shows line number on left
- Useful for debugging (breakpoints link to line numbers)

**Code Folding**
- Collapse/expand procedures and blocks
- Click triangle on left margin

**Auto-Indentation**
- Automatic indentation on new line
- Tab width configurable (default 4 spaces)

**Find and Replace**

```
Edit → Find [Ctrl+F]
Edit → Replace [Ctrl+H]
```

Use Ctrl+G to go to specific line number.

### Code Templates

Starter code for each language available via:

```
File → New [Ctrl+N]
```

Select language, gets basic template.

Or copy from Examples:

```
File → Examples [Ctrl+E]
```

Browse 86+ working examples.

---

## Running Programs

### Execute Code

```
Run → Run [Ctrl+R]
```

Executes program in currently editor code.

### Stop Execution

```
Run → Stop [Ctrl+Shift+R]
```

Stops currently running program (if infinite loop or slow).

### Clear Output

```
Run → Clear Output
```

Clears console and canvas for clean slate.

### Execution Timing

- **Startup**: Program begins in worker thread
- **Output**: Appears in console as generated
- **Graphics**: Canvas updates in real-time
- **Completion**: Result message shown
- **Timeout**: Programs auto-stop after 30 seconds

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
- Run → Clear Output clears all
- Or Start new program

---

## Graphics Canvas

The Canvas displays turtle graphics output in real-time.

### Turtle Graphics

**Turtle Position**
- Small triangle shows turtle
- Position displayed in status bar: `Turtle: (x,y,angle°)`
- Angle: 0°=East, 90°=North, etc.

**Drawing**
- Pen can be up (not drawing) or down (drawing)
- Pen color and width configurable
- All lines saved and displayed

### Canvas Controls

**Zoom**
- Scroll wheel to zoom in/out
- Or View → Zoom In/Out
- Range: 0.1x to 5.0x magnification

**Pan**
- Right-click and drag to pan
- Or spacebar + middle mouse drag
- Double-click to reset view

**Grid** (Optional)
- View → Toggle Grid
- Shows coordinate grid
- Helpful for positioning

### Canvas Features

**Real-Time Updates**
- Graphics update as program runs
- No flickering or jumping
- Smooth animation

**Inspect Lines**
- Hover over lines to see details
- Line number and coordinates displayed
- Click line to select

**Reset Canvas**
- Run → Clear Output clears graphics
- Each new Run clears automatically
- Or Logo HOME command resets turtle

---

## Feature Panels

14 specialized panels accessible via tabs at bottom right.

### Key Panels

**Lesson Mode**
- Load structured lessons
- Complete step-by-step challenges
- Track progress

**AI Assistant**
- Get code suggestions
- Ask for debugging help
- Request code explanations

**Error Explainer**
- Understand why errors occur
- Get fix suggestions
- Learn best practices

**Turtle Inspector**
- View turtle state (position, angle, pen)
- Visual representation of turtle
- Useful for debugging graphics

**Debugger**
- Step through execution
- Set breakpoints
- View execution timeline

**Examples Browser**
- Browse 86+ working programs
- Copy and modify examples
- Learn from code

**Variables Inspector**
- See current variable values
- Watch variables change
- Update during debugging

**Project Runner**
- Manage multi-file projects
- Run all files
- Organize code

---

## Keyboard Shortcuts

### File Operations
- `Ctrl+N` - New file
- `Ctrl+O` - Open file
- `Ctrl+S` - Save
- `Ctrl+Shift+S` - Save As
- `Ctrl+E` - Open Examples
- `Ctrl+Q` - Quit

### Editing
- `Ctrl+Z` - Undo
- `Ctrl+Shift+Z` - Redo (or `Ctrl+Y`)
- `Ctrl+X` - Cut
- `Ctrl+C` - Copy
- `Ctrl+V` - Paste
- `Ctrl+A` - Select All
- `Ctrl+F` - Find
- `Ctrl+H` - Replace
- `Ctrl+G` - Go to Line

### Execution
- `Ctrl+R` - Run program
- `Ctrl+Shift+R` - Stop program

### Debugging
- `F10` - Step Over
- `F11` - Step Into
- `F5` - Continue
- `Ctrl+B` - Toggle Breakpoint

### View
- `Ctrl+Plus` - Zoom In (Canvas)
- `Ctrl+Minus` - Zoom Out (Canvas)
- `Ctrl+0` - Reset Zoom
- `F11` - Full Screen (toggle)

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

8 built-in themes:

1. **Dracula** - Dark with purple
2. **Monokai** - Classic code editor
3. **Solarized Dark** - Low contrast
4. **Ocean** - Blue palette
5. **Spring** - Green/pastels
6. **Sunset** - Warm oranges
7. **Candy** - Bright pastels
8. **Forest** - Deep greens

### Customizing Editor

Via Edit menu or Feature Panels → Settings:

- Font family (Monaco, Courier, etc.)
- Font size (8-32 points)
- Tab width (2, 4, 8 spaces)
- Auto-indent (on/off)
- Show line numbers (toggle)
- Show whitespace characters (toggle)

### Theme Persistence

Your theme choice automatically saved to `~/.Time_Warp/config.json` and restored on next launch.

---

## Menu Reference

### File Menu (18 items)

- **New** [Ctrl+N] - New file
- **Open** [Ctrl+O] - Open file
- **Save** [Ctrl+S] - Save current
- **Save As** [Ctrl+Shift+S] - Save with new name
- **Export** - Export as different format
- **Recent Files** - Quick access
- **Examples** [Ctrl+E] - Browse examples
- **Lessons** - Access lessons
- **Close** - Close current file
- **Close All** - Close all files
- **Properties** - File info
- **Print** - Print code
- **Email** - Email file
- **Preferences** - Preferences dialog
- **Exit** [Ctrl+Q] - Quit IDE

### Edit Menu (10 items)

- **Undo** [Ctrl+Z]
- **Redo** [Ctrl+Y or Ctrl+Shift+Z]
- **Cut** [Ctrl+X]
- **Copy** [Ctrl+C]
- **Paste** [Ctrl+V]
- **Select All** [Ctrl+A]
- **Find** [Ctrl+F]
- **Replace** [Ctrl+H]
- **Go to Line** [Ctrl+G]
- **Preferences** - Editor settings

### Run Menu (8 items)

- **Run** [Ctrl+R] - Execute program
- **Run Selection** - Execute selected code
- **Continue** - Resume after pause
- **Stop** [Ctrl+Shift+R] - Stop execution
- **Clear Output** - Clear console
- **Debug Mode** [Toggle] - Enable debugger
- **Set Language** - Choose language
- **Turtle Reset** - Reset graphics

### Debug Menu (10 items)

- **Step Into** [F11] - Enter functions
- **Step Over** [F10] - Skip functions
- **Step Out** [Shift+F11] - Exit function
- **Continue** [F5] - Resume execution
- **Pause** - Pause running program
- **Stop** - Stop execution
- **Breakpoint** [Ctrl+B] - Toggle breakpoint
- **Timeline** - View execution history
- **Watch Variable** - Add variable to watch
- **Stack Trace** - View call stack

### View Menu (10 items)

- **Show Editor** [Toggle]
- **Show Console** [Toggle]
- **Show Canvas** [Toggle]
- **Show Panels** [Toggle]
- **Theme** - Select theme (8 options)
- **Zoom In** [Ctrl+Plus]
- **Zoom Out** [Ctrl+Minus]
- **Reset Zoom** [Ctrl+0]
- **Full Screen** [F11]
- **Grid** [Toggle] - Canvas grid

### Help Menu (8 items)

- **Documentation** - User guide
- **Language Reference** - Language syntax
- **Keyboard Shortcuts** - Shortcut list
- **About Time Warp** - Version info
- **Check for Updates** - Update check
- **Report Issue** - Bug report
- **Settings** - Configuration
- **License** - License info

---

## Tips & Tricks

### Quick Development Workflow

1. Open Examples (`Ctrl+E`) to see working code
2. Create new file (`Ctrl+N`)
3. Type or paste code
4. Save (`Ctrl+S`)
5. Run (`Ctrl+R`)
6. View output in console and canvas
7. Edit as needed and re-run

### Debugging Programs

1. Set breakpoints with `Ctrl+B`
2. Enable Debug Mode from Run menu
3. Run program (`Ctrl+R`)
4. Execute stops at breakpoint
5. Use Step Into/Over to navigate
6. Watch variables in Variables Inspector
7. View Timeline to see execution history

### Learning New Languages

1. Open Examples browser (`Ctrl+E`)
2. Filter by language
3. Browse from simple to complex
4. Copy example (`Ctrl+C`)
5. Create new file (`Ctrl+N`)
6. Paste code (`Ctrl+V`)
7. Modify and experiment
8. Run (`Ctrl+R`) to test

### Managing Projects

1. Create folder for project
2. Create multiple .bas, .logo files
3. Use Project Runner panel
4. Add files to project
5. Run individual files or all

---

**For language-specific help, see:**
- [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) - Syntax for all 7 languages
- [DEBUGGER_GUIDE.md](DEBUGGER_GUIDE.md) - Using the debugger
- [TURTLE_GRAPHICS.md](TURTLE_GRAPHICS.md) - Graphics programming
- [FAQ.md](FAQ.md) - Frequently asked questions
- [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - Common problems
