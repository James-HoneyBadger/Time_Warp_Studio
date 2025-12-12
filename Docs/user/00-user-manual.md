# Time Warp IDE User Manual

Welcome to Time Warp IDE! This manual will help you get the most out of your programming environment.

---

## Table of Contents

1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [The Interface](#the-interface)
4. [Writing Your First Program](#writing-your-first-program)
5. [Running Programs](#running-programs)
6. [Using Turtle Graphics](#using-turtle-graphics)
7. [Saving and Loading](#saving-and-loading)
8. [Customizing Your Environment](#customizing-your-environment)
9. [Tips and Tricks](#tips-and-tricks)

---

## Introduction

Time Warp IDE is an educational programming environment centered on three core languages, with additional experimental modules:

- **BASIC** - Great for beginners, simple English-like commands
- **PILOT** - Designed for teaching, emphasizes clear communication
- **Logo** - Learn through drawing with turtle graphics
- Experimental: **Pascal**, **Prolog**, **C** (availability varies; features may be incomplete)

### Who Should Use Time Warp IDE?

- **Students** learning their first programming language
- **Teachers** looking for an all-in-one teaching platform
- **Hobbyists** interested in retro computing and classic languages
- **Developers** exploring different programming paradigms

---

## Getting Started

### System Requirements

- **Operating System**: Linux, macOS, or Windows
- **Memory**: 256 MB RAM minimum (512 MB recommended)
- **Disk Space**: 50 MB for installation
- **Display**: 1024×768 resolution or higher

### First Launch (Python)

Launch via the Python application. From the repo root:

1. `cd Platforms/Python`
2. `python3 -m pip install -r requirements.txt`
3. `python3 time_warp_ide.py`

When you start Time Warp IDE, you'll see:

1. **Menu Bar** - Access all features and settings
2. **Code Editor** - Where you write your programs
3. **Output Panel** - See program results and error messages
4. **Status Bar** - Current language and system information

---

## The Interface

### Menu Bar

**File Menu**
- `New` - Start a fresh program
- `Open` - Load an existing program file
- `Save` - Save your current program
- `Save As` - Save with a new name
- `Recent Files` - Quick access to recent work
- `Exit` - Close Time Warp IDE

**Edit Menu**
- `Undo` (Ctrl+Z) - Reverse last change
- `Redo` (Ctrl+Y) - Reapply undone change
- `Cut` (Ctrl+X) - Remove and copy selection
- `Copy` (Ctrl+C) - Copy selection
- `Paste` (Ctrl+V) - Insert copied text
- `Find` (Ctrl+F) - Search in your code
- `Replace` (Ctrl+H) - Search and replace

**Run Menu**
- `Run Program` (F5) - Execute your code
- `Stop` (Shift+F5) - Halt execution
- `Clear Output` - Erase output panel
- `Reset Turtle` - Return turtle to starting position

**Language Menu**
- Select BASIC, PILOT, or Logo
- Syntax highlighting updates automatically
- Experimental languages may appear depending on build state

**View Menu**
- `Zoom In/Out` - Adjust editor text size
- `Toggle Output Panel` - Show/hide results
- `Toggle Turtle Canvas` - Show/hide graphics
- `Full Screen` - Maximize workspace

**Tools Menu**
- `Options` - Customize settings
- `Themes` - Change color schemes
- `Font Settings` - Adjust editor appearance
- `Keyboard Shortcuts` - View and customize hotkeys

**Help Menu**
- `Documentation` - Open this manual
- `Language References` - Command guides
- `Examples` - Sample programs
- `About` - Version and license information

### Code Editor

The editor is where you write your programs. Features include:

- **Syntax Highlighting** - Keywords and commands are colored for clarity
- **Line Numbers** - Easy reference for debugging
- **Auto-Indent** - Automatic code formatting
- **Bracket Matching** - Highlights matching parentheses and braces

### Output Panel

Shows two types of content:

1. **Text Output** - Program messages and print statements
2. **Error Messages** - Clear explanations when something goes wrong

Error messages include:
- Line number where the error occurred
- Type of error (syntax, runtime, logic)
- Suggestion for how to fix it

### Turtle Graphics Canvas

When using Logo, this panel displays:

- Turtle position and heading
- All drawings created by your program
- Grid and coordinates (optional)
- Color palette

---

## Writing Your First Program

Let's write a simple "Hello, World!" program in three different languages.

### BASIC

```basic
10 PRINT "Hello, World!"
20 END
```

1. Select **BASIC** from the Language menu
2. Type the program in the editor
3. Press **F5** to run
4. See "Hello, World!" in the output panel

### PILOT

```pilot
T:Hello, World!
```

1. Select **PILOT** from the Language menu
2. Type `T:Hello, World!`
3. Press **F5**
4. The message appears instantly!

### Logo

```logo
PRINT [Hello, World!]
```

1. Select **Logo** from the Language menu
2. Type the command
3. Press **F5**
4. Output appears in the panel

### What's Happening?

Each language has different syntax, but they all do the same thing: display a message. This is the beauty of Time Warp IDE — you can learn multiple programming approaches in one environment.

Note: Experimental languages (Pascal, Prolog, C) may not be available in all builds.

---

## Running Programs

### Basic Execution

1. **Write** your code in the editor
2. **Press F5** or click Run → Run Program
3. **View results** in the output panel
4. **Fix errors** if needed and run again

### Interactive Input

Some programs ask for user input:

```basic
10 INPUT "What is your name"; NAME$
20 PRINT "Hello, "; NAME$
```

When you run this:
1. Program pauses with a prompt
2. Type your response in the input field
3. Press Enter
4. Program continues with your input

### Stopping Programs

If a program runs too long or enters an infinite loop:

1. **Press Shift+F5** or click Run → Stop
2. Execution halts immediately
3. Output shows where the program stopped
4. Edit and try again

---

## Using Turtle Graphics

Turtle graphics let you create drawings by controlling a "turtle" that moves around the screen.

### Basic Turtle Commands (Logo)

```logo
FORWARD 100      ; Move turtle forward 100 steps
RIGHT 90         ; Turn right 90 degrees
PENUP           ; Lift pen (don't draw)
PENDOWN         ; Lower pen (start drawing)
```

### Drawing a Square

```logo
REPEAT 4 [FORWARD 100 RIGHT 90]
```

This tells the turtle:
- Repeat 4 times:
  - Move forward 100 steps
  - Turn right 90 degrees

Result: A perfect square!

### Colors

```logo
SETPENCOLOR 1    ; Red
FORWARD 100
SETPENCOLOR 2    ; Green
FORWARD 100
SETPENCOLOR 3    ; Blue
FORWARD 100
```

### Complex Drawings

```logo
; Draw a flower
REPEAT 12 [FORWARD 100 RIGHT 150]
```

Experiment with different:
- Distances
- Angles
- Number of repetitions
- Colors

---

## Saving and Loading

### Saving Your Work

**Method 1: Save**
1. File → Save (or Ctrl+S)
2. If first time, choose location and filename
3. File extension added automatically based on language

**Method 2: Save As**
1. File → Save As
2. Choose new location or filename
3. Useful for creating variations

### File Extensions

- `.bas` - BASIC programs
- `.pilot` - PILOT programs
- `.logo` - Logo programs
- `.pas` - Pascal programs
- `.pl` - Prolog programs
- `.c` - C programs

### Loading Programs

**Method 1: Open**
1. File → Open (or Ctrl+O)
2. Browse to your file
3. Language detected automatically

**Method 2: Recent Files**
1. File → Recent Files
2. Click on file name
3. Opens immediately

**Method 3: Drag and Drop**
1. Drag file from your file manager
2. Drop onto Time Warp IDE window
3. File opens and language is set

---

## Customizing Your Environment

### Themes

Change the look of your IDE:

1. Tools → Themes
2. Choose from:
   - **Light** - Bright, high contrast
   - **Dark** - Easy on the eyes
   - **Dracula** - Popular programmer theme
   - **Solarized** - Balanced contrast
   - **Monokai** - Vibrant colors
   - **Ocean** - Blue tones
   - **Forest** - Green tones
   - **Sunset** - Warm colors
3. Preview updates immediately
4. Click Apply

### Font Settings

Adjust text appearance:

1. Tools → Font Settings
2. Choose font family (monospace recommended)
3. Set font size (10-16pt typical)
4. Enable/disable bold and italics

### Editor Preferences

- **Tab Size**: 2, 4, or 8 spaces
- **Auto-Indent**: On/Off
- **Line Wrapping**: On/Off
- **Show Line Numbers**: On/Off

### Keyboard Shortcuts

View and customize keyboard shortcuts:

1. Tools → Keyboard Shortcuts
2. See list of all commands
3. Click to change shortcut
4. Reset to defaults if needed

---

## Tips and Tricks

### Productivity Tips

1. **Use Comments** - Document your code
   ```basic
   10 REM This is a comment in BASIC
   ```
   ```logo
   ; This is a comment in Logo
   ```

2. **Save Frequently** - Press Ctrl+S often

3. **Use the Examples** - Help → Examples has ready-to-run programs

4. **Experiment** - Best way to learn is by trying things

### Debugging Tips

1. **Read Error Messages** - They usually point to the problem
2. **Check Line Numbers** - Errors tell you where to look
3. **Add PRINT Statements** - See what your variables contain
4. **Start Simple** - Test small pieces before combining
5. **Use Comments** - Temporarily disable code to isolate issues

### Learning Tips

1. **Start with One Language** - Master basics before exploring others
2. **Type Examples Yourself** - Don't just copy-paste
3. **Modify Examples** - Change numbers, add features
4. **Build Projects** - Create something you care about
5. **Ask Questions** - Use the community forums

---

## Common Questions

**Q: Which language should I start with?**
A: BASIC or PILOT are the most beginner-friendly. Logo is great if you enjoy visual/artistic programming.

**Q: Can I use Time Warp IDE for serious projects?**
A: While designed for education, Time Warp IDE is fully functional. Many users create real applications!

**Q: My program runs slowly. Why?**
A: Graphics operations can be intensive. Try reducing complexity or disabling the turtle canvas when not needed.

**Q: Can I share my programs?**
A: Yes! Your program files are plain text. Share them by email, cloud storage, or version control systems like Git.

**Q: Is there a command-line version?**
A: Yes! See the [Developer Guide](../developer/00-developer-guide.md) for details on the CLI interpreter.

---

## Next Steps

Now that you know the basics:

- Explore the **[Programming Guide](01-programming-guide.md)** for language-specific tutorials
- Check out **[Quick Reference](02-quick-reference.md)** for command cheat sheets
- Read the **[FAQ](03-faq.md)** for answers to common questions
- Join the **community** to share your creations

Happy coding!

---

*For teachers: See the [Teacher's Guide](../teacher/00-overview.md) for classroom-specific features.*
*For developers: See the [Technical Reference](../developer/01-technical-reference.md) for API documentation.*

## Installing (Windows Installer)

If you download the Windows installer from the releases page, the installer filename includes a version token. Example:

- TimeWarpIDE-Setup-<VERSION>.exe — e.g. TimeWarpIDE-Setup-3.0.0.exe

When installed, the application is placed under Program Files and available from the Start Menu. If you are using the Windows 2000 edition or a CI-built package, installers are produced under `Platforms/Windows2000/dist/` and include the version token embedded with `makensis` using `-DVERSION=<tag>`.

If you are packaging locally using `makensis`, pass OUTDIR and VERSION to produce the same filename layout used by CI:

```bash
makensis -DOUTDIR=Platforms/Windows2000/dist -DVERSION=3.0.0 Platforms/Windows2000/installer/timewarp.nsi
```
