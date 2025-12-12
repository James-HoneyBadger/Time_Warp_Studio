# Time Warp IDE User Manual

Welcome to Time Warp IDE! This guide will help you master the programming environment.

---

## Table of Contents

1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [The Interface](#the-interface)
4. [The Code Editor](#the-code-editor)
5. [Writing Your First Program](#writing-your-first-program)
6. [Running Programs](#running-programs)
7. [Using Turtle Graphics](#using-turtle-graphics)
8. [Advanced Features](#advanced-features)
9. [Saving and Loading](#saving-and-loading)
10. [Customizing Your Environment](#customizing-your-environment)
11. [Tips and Tricks](#tips-and-tricks)

---

## Introduction

Time Warp IDE is an educational programming environment supporting three core languages with full feature parity, plus experimental options:

- **BASIC** – Beginner-friendly, English-like syntax for learning fundamentals
- **PILOT** – Designed for structured teaching with clear feedback
- **Logo** – Visual learning through turtle graphics and drawing commands
- **Experimental**: Pascal, Prolog, C (incomplete; for advanced exploration)

### Who Should Use Time Warp IDE?

- **Students** learning programming concepts
- **Teachers** needing an integrated classroom tool
- **Educators** who want visual feedback and multiple paradigms
- **Hobbyists** interested in retro computing and classic languages

### Design Philosophy

Every feature in Time Warp IDE prioritizes:
1. **Clear Feedback** – Error messages explain what went wrong
2. **Visual Learning** – Graphics make abstract concepts concrete
3. **Simplicity** – No distracting UI elements or complex workflows
4. **Accessibility** – Support for keyboard navigation and screen readers

---

## Getting Started

### System Requirements

- **Operating System**: Linux, macOS, or Windows (see installation guide)
- **Memory**: 256 MB RAM (512 MB recommended)
- **Disk Space**: 50 MB
- **Display**: 800×600 minimum (1024×768 recommended)
- **Internet**: Not required (optional for cloud features)

### First Launch

1. Open a terminal and navigate to `Platforms/Python/`
2. Run `python time_warp_ide.py`
3. The IDE window opens with:
   - Code editor on the left
   - Graphics canvas on the right
   - Menu bar at the top
   - Status bar at the bottom

You're now ready to write programs!

---

## The Interface

### Menu Bar

#### File Menu
| Command | Shortcut | Purpose |
|---------|----------|---------|
| New | Ctrl+N | Start a fresh program |
| Open | Ctrl+O | Load a saved program |
| Save | Ctrl+S | Save current work |
| Save As | Ctrl+Shift+S | Save with a new name |
| Recent Files | — | Quick access to recent programs |
| Exit | Ctrl+Q | Close the IDE |

#### Edit Menu
| Command | Shortcut | Purpose |
|---------|----------|---------|
| Undo | Ctrl+Z | Reverse last change |
| Redo | Ctrl+Y | Reapply undone change |
| Cut | Ctrl+X | Remove and copy selected text |
| Copy | Ctrl+C | Copy selected text |
| Paste | Ctrl+V | Insert copied text |
| Find | Ctrl+F | Search in code |
| Replace | Ctrl+H | Search and replace text |
| Select All | Ctrl+A | Select entire program |

#### Run Menu
| Command | Shortcut | Purpose |
|---------|----------|---------|
| Run Program | F5 | Execute your code |
| Stop | Shift+F5 | Halt execution |
| Pause | — | Pause execution (debugging) |
| Clear Output | — | Erase output panel |
| Reset Turtle | — | Return turtle to center/start |

#### Language Menu
| Option | Purpose |
|--------|---------|
| BASIC | Switch to BASIC language |
| PILOT | Switch to PILOT language |
| Logo | Switch to Logo language |
| Pascal | Switch to Pascal (experimental) |
| Prolog | Switch to Prolog (experimental) |
| C | Switch to C (experimental) |

**Effect**: Syntax highlighting updates, editor supports language-specific features, error messages use language conventions.

#### View Menu
| Command | Purpose |
|---------|---------|
| Screen Mode | Switch between Text, Graphics, Single, Combined views |
| Theme | Choose from 8 color schemes (Dracula, Monokai, Solarized, etc) |
| Zoom In | Increase editor text size (Ctrl+) |
| Zoom Out | Decrease editor text size (Ctrl-) |
| Reset Zoom | Return to normal size (Ctrl+0) |
| Toggle Output Panel | Show/hide output console |
| Toggle Canvas | Show/hide graphics canvas |
| Toggle Debug Panel | Show/hide execution debugger |
| Variable Inspector | View all variables and values |
| Error Explorer | Detailed error analysis |
| Focus Mode | Hide non-essential UI for distraction-free coding |
| CRT Effect | Retro monitor appearance |

#### Tools Menu
| Command | Purpose |
|---------|---------|
| Settings | Customize IDE behavior |
| Snippets | Insert code templates |
| Themes | Manage and create themes |
| Font Settings | Change editor font/size |
| Keyboard Shortcuts | View and customize hotkeys |

#### Help Menu
| Command | Purpose |
|---------|---------|
| User Manual | Open this documentation |
| Language Guide | Command reference for current language |
| Quick Reference | Common commands and syntax |
| Examples | Open sample programs |
| About | Version and license information |

### Status Bar

Bottom of window shows:
- **Current Language** – BASIC, PILOT, Logo, etc.
- **Cursor Position** – Line and column number
- **File Status** – "Unsaved Changes" indicator
- **Execution Status** – "Running", "Stopped", etc.

### Code Editor

The left panel where you write your program:

**Features**:
- **Syntax Highlighting** – Language keywords are colored for readability
- **Line Numbers** – Easy reference for debugging
- **Auto-Indentation** – Automatic formatting
- **Bracket Matching** – Highlights matching parentheses
- **Error Markers** – Red underlines show problems
- **Code Snippets** – Press Ctrl+Space for template suggestions

**Keyboard Shortcuts**:
- `Ctrl+Space` – Show code suggestions
- `Tab` – Indent line
- `Shift+Tab` – Unindent line
- `Ctrl+/` – Comment/uncomment line
- `Ctrl+L` – Jump to line number

### Output Panel

Displays program output and messages:

**Content Types**:
1. **Standard Output** – Print statements from your program
2. **Errors** – `❌ Error message` (explanatory, not cryptic)
3. **Info** – `ℹ️ Informational messages` (hints, confirmations)
4. **Turtle Actions** – `🐢 Graphics operations` (drawing feedback)
5. **Events** – `🚀 Program events` (start, stop, etc.)

**Colors** indicate message type:
- Red text = errors
- Blue text = information
- Green text = success
- Gray text = debug output

**Right-click menu**:
- Copy all output
- Clear output
- Save to file

### Graphics Canvas

The right panel for visual output:

**Features**:
- **Turtle Rendering** – See your drawing in real-time
- **Zoom Controls** – Scroll to zoom, Ctrl+Scroll to pan
- **Grid** – Optional coordinate grid (View menu)
- **Sprite** – Turtle appearance (arrow, turtle, custom image)
- **Color Palette** – Visual guide to available colors

**Right-click menu**:
- Reset view (zoom/pan)
- Save canvas as image
- Clear canvas
- Toggle grid

---

## The Code Editor

### Language Syntax Highlighting

Each language has color-coded elements:

**BASIC Example**:
```basic
10 REM This is a comment
20 PRINT "Hello, World!"
30 INPUT X
40 IF X > 5 THEN PRINT "Big"
50 FOR I = 1 TO 10
60   PRINT I
70 NEXT I
```

Colors:
- `REM` statements → Gray (comments)
- `PRINT`, `INPUT` → Purple (keywords)
- Strings (`"Hello"`) → Green
- Numbers → Blue

### Code Snippets

Insert template code with `Ctrl+Space`:

Example in BASIC:
- `for` → FOR...NEXT loop template
- `if` → IF...THEN...ELSE template
- `sub` → Subroutine template

---

## Writing Your First Program

### Hello World in Each Language

**BASIC**:
```basic
PRINT "Hello, World!"
```

**PILOT**:
```pilot
ACCEPT: What is your name?
PRINT: "Hello, " Name
```

**Logo**:
```logo
FORWARD 100
RIGHT 90
FORWARD 100
```

### Running Your Program

1. Type or paste code into the editor
2. Press **F5** or click **Run → Run Program**
3. Output appears in the Output panel
4. (Logo) Graphics appear in the Canvas

### Responding to Input

Programs can ask for user input:

**BASIC**:
```basic
INPUT "Enter your age: " age
PRINT "You are " age " years old"
```

**PILOT**:
```pilot
ACCEPT: Enter your name
PRINT: "Hello, " name
```

When the program runs, a dialog appears asking for input. Type your answer and press Enter.

---

## Running Programs

### Execution Modes

**Normal Mode**: Program runs to completion
- Press F5 to start
- Output appears immediately
- Press Shift+F5 to stop a long-running program

**Debug Mode**: Step through your program line-by-line
- Open View → Debug Panel
- Press **Step Over** to execute one line
- Hover over variables to see their values
- Set breakpoints by clicking line numbers

### Error Handling

When your program has an error:

1. **Red box** highlights the problem line
2. **Error message** explains what went wrong
3. **Suggestion** indicates how to fix it

Example error:
```
❌ Syntax Error on line 5:
   Unexpected token "PRINT"
   Did you mean: PRINTF ?
```

### Output Interpretation

Emoji prefixes show message type:
- `❌` = Error that stopped execution
- `✅` = Success confirmation
- `ℹ️` = Informational message
- `🐢` = Turtle graphics action
- `🚀` = Execution event

---

## Using Turtle Graphics

Turtle graphics are a visual way to learn programming. The "turtle" is a small arrow on the canvas that can move, turn, and draw.

### Basic Commands

**Logo Language**:
```logo
FORWARD 100      ;Move forward 100 units
BACK 50          ;Move backward 50 units
RIGHT 45         ;Turn right 45 degrees
LEFT 45          ;Turn left 45 degrees
PENDOWN          ;Start drawing
PENUP            ;Stop drawing
CLEARSCREEN      ;Erase all drawings
HOME             ;Return to center
SHOWTURTLE       ;Make turtle visible
HIDETURTLE       ;Hide turtle
```

### Changing Appearance

```logo
SETCOLOR red          ;Change pen color
SETCOLOR #FF5733      ;Hex color
PENWIDTH 5            ;Pen thickness
SETSHAPE "turtle"     ;Change turtle shape
```

### Drawing Shapes

**Square**:
```logo
REPEAT 4
  FORWARD 100
  RIGHT 90
END
```

**Circle** (approximation):
```logo
REPEAT 360
  FORWARD 1
  RIGHT 1
END
```

**Star**:
```logo
REPEAT 5
  FORWARD 100
  RIGHT 144
END
```

### Canvas Controls

- **Scroll** to pan around
- **Ctrl+Scroll** to zoom in/out
- **Right-click → Reset View** to zoom to fit
- **View → Grid** to show coordinates

---

## Advanced Features

### Debug Panel

View → Debug Panel shows:
- **Current Line** – Execution position
- **Variables** – All variables and values
- **Call Stack** – Function call history
- **Breakpoints** – Stops where you click line numbers
- **Step Controls** – Step into, over, or out

### Variable Inspector

View → Variable Inspector opens a dialog showing:
- Variable name
- Current value
- Data type
- Scope (local, global)

Updates in real-time during debugging.

### Error Explorer

View → Error Explorer shows:
- Detailed error analysis
- Stack trace
- Suggestions for fixes
- Links to documentation

### Focus Mode

View → Focus Mode hides:
- Menu bar (press Esc to show)
- Output panel
- Status bar
- Canvas tabs

Perfect for classroom use where students shouldn't be distracted.

### Themes

Choose from 8 built-in themes in View → Theme:
- **Dracula** – Dark with purple accents
- **Monokai** – Dark with vibrant colors
- **Solarized Dark** – Muted dark tones
- **Ocean** – Blue-based design
- **Spring** – Light green nature colors
- **Sunset** – Warm orange/red tones
- **Candy** – Pastel bright colors
- **Forest** – Dark green theme

Themes persist across sessions (saved to `~/.Time_Warp/config.json`).

### Screen Modes

View → Screen Mode changes layout:

| Mode | Layout | Best For |
|------|--------|----------|
| **Text** | Editor full width | BASIC/PILOT programs |
| **Graphics** | Canvas full width | Logo/graphics focus |
| **Single** | Editor left, Canvas right | Mixed programs |
| **Combined** | Editor top, Canvas bottom | Limited screen space |

### CRT Effect

View → CRT Effect gives a retro monitor appearance with:
- Scanlines
- Color bleeding
- Screen curvature
- Vintage color palette

Great for teaching retro computing concepts!

---

## Saving and Loading

### Save Your Program

**File → Save** (Ctrl+S):
- If file is new, asks for a name
- Extensions: `.bas` (BASIC), `.plt` (PILOT), `.logo` (Logo)
- Saves to current directory or user's Documents folder

**File → Save As** (Ctrl+Shift+S):
- Allows choosing a new name and location
- Useful for making copies or backups

### Open a Program

**File → Open** (Ctrl+O):
- Browse for a `.bas`, `.plt`, or `.logo` file
- File is loaded into the editor
- Language is auto-detected from extension

**File → Recent Files**:
- Quick access to recently opened programs
- Up to 10 recent files shown

### Supported File Types

| Extension | Language | Example |
|-----------|----------|---------|
| `.bas` | BASIC | `program.bas` |
| `.pilot` or `.plt` | PILOT | `lesson.plt` |
| `.logo` | Logo | `drawing.logo` |
| `.txt` | Text (auto-detect) | `code.txt` |

---

## Customizing Your Environment

### Settings

Tools → Settings opens a dialog with:

**Editor**:
- Font family and size
- Tab width (spaces or tabs)
- Line numbers (show/hide)
- Word wrap
- Auto-indent

**Display**:
- Theme selection
- Screen mode
- Canvas grid
- Turtle sprite style

**Behavior**:
- Auto-save interval
- Confirm on exit
- Sound effects (on/off)

### Keyboard Shortcuts

Tools → Keyboard Shortcuts shows all shortcuts and allows customization:

**Common shortcuts**:
- F5 = Run
- Shift+F5 = Stop
- Ctrl+S = Save
- Ctrl+F = Find
- Ctrl+H = Replace

Click any shortcut to change it.

---

## Tips and Tricks

### Pro Tips

1. **Use Code Snippets** – Ctrl+Space suggests templates for loops, conditions, etc.
2. **Check Variable Inspector** – View → Variable Inspector shows all values in real-time
3. **Use Breakpoints** – Click line numbers in Debug Panel to pause execution
4. **Save Frequently** – Ctrl+S saves your work
5. **Test Small Parts** – Run one function/procedure at a time before combining

### Common Mistakes

**Error: "Undefined variable X"**
- Make sure you spelled it correctly
- Variables are case-sensitive (X ≠ x)
- Declare it with INPUT or LET before using

**Error: "Syntax error"**
- Check spelling of keywords (PRINT not PRNT)
- Make sure quotes match ("hello" not "hello')
- Check for missing NEXT/END statements

**Turtle not drawing?**
- Make sure PENDOWN is called (default in Logo)
- Check that pen color isn't white on white background
- Use View → Reset Canvas to clear previous drawings

**Output not showing?**
- Click the Output panel tab (View → Toggle Output Panel)
- Check that the program actually prints something
- Run the program (F5) to generate output

### Learning Resources

**Built-in**:
- Help → Language Guide (command reference)
- Help → Examples (sample programs)
- Help → Quick Reference (cheat sheet)

**Online**:
- Official documentation: `Docs/user/` folder
- Example programs: `Examples/` folder
- Video tutorials: (see online documentation)

### Classroom Tips

**For Teachers**:
- Use Focus Mode to reduce distractions
- Save a "starter template" for students to open
- Use code snippets to give students hints
- Set breakpoints to explain line-by-line execution

**For Students**:
- Start with the "Hello World" example
- Study provided examples before writing your own
- Use the Variable Inspector to understand loops
- Save multiple versions of your work

---

## Getting Help

### Built-in Help

- **Menu → Help** – Access all documentation
- **Hover over error** – Shows explanation
- **Status bar** – Current mode and context

### Online Resources

- **GitHub Issues**: Report bugs at <https://github.com/James-HoneyBadger/Time_Warp/issues>
- **GitHub Discussions**: Ask questions at <https://github.com/James-HoneyBadger/Time_Warp/discussions>
- **Documentation**: `Docs/` folder in the repository

### FAQ Quick Links

- **How do I write a loop?** → See examples in `Examples/`
- **What commands does Logo support?** → Help → Language Guide
- **How do I debug my program?** → View → Debug Panel
- **Can I change the theme?** → View → Theme

---

**Ready to start coding?** Check out the [Programming Guide](01-programming-guide.md) for language-specific instruction!
