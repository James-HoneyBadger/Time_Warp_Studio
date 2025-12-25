# User Guide

Welcome to Time Warp IDE! This guide covers everything you need to get started and make the most of the IDE.

## Table of Contents

1. [Getting Started](#getting-started)
2. [The IDE Interface](#the-ide-interface)
3. [Working with Code](#working-with-code)
4. [Using the Graphics Canvas](#using-the-graphics-canvas)
5. [Immediate Mode (REPL)](#immediate-mode-repl)
6. [Running Programs](#running-programs)
7. [Themes and Customization](#themes-and-customization)
8. [Troubleshooting](#troubleshooting)

## Getting Started

### Installation

See [Installation Guide](../../INSTALL_NATIVE.md) for detailed setup instructions.

### First Run

1. **Launch the IDE**
   ```bash
   cd Platforms/Python
   python time_warp_ide.py
   ```

2. **Choose a Language** - Select from the dropdown menu (BASIC, PILOT, Logo, Pascal, Prolog, Forth, C)

3. **Write Code** - Type your program in the editor

4. **Run It** - Click the Run button or press `Ctrl+R`

5. **See Results** - Output appears in the Output panel, graphics on the Canvas

## The IDE Interface

### Main Panels

#### 1. **Code Editor** (Left, 85% height)
- Type your program code here
- **Features:**
  - Syntax highlighting for all languages
  - Code snippets and auto-completion
  - Line numbers and error indicators
  - Multiple tabs for multiple files
  - Code search and replace

#### 2. **Immediate Mode** (Left, 15% height)
- Quick REPL for testing code
- Type commands and press Enter to execute
- Great for interactive programming

#### 3. **Output Panel** (Right)
- Shows program output and printed text
- Displays error messages
- Prompts for input with `INPUT` commands

#### 4. **Graphics Canvas**
- Real-time rendering of graphics
- Full support for turtle graphics
- Zoom and pan controls
- Background and pen color settings

#### 5. **Variables Inspector**
- View all variables and their current values
- Update as program runs
- Useful for debugging

#### 6. **Menu Bar**
- **File** - Open, Save, Export programs
- **Edit** - Undo, Redo, Cut, Copy, Paste
- **View** - Switch between panels and layouts
- **Language** - Select programming language
- **Theme** - Choose color scheme
- **Help** - Documentation and examples

### Status Bar

Shows current status:
- `Ready` - Waiting for input
- `Running` - Program executing
- `Error` - Program encountered an error
- `Output` - Program produced output

## Working with Code

### Creating a New Program

1. Click **File → New**
2. Select the language you want to use
3. Start typing your code

### Opening Examples

1. Click **File → Open Examples**
2. Browse the examples directory
3. Choose a program and click Open
4. Study the code or modify and run it

### Saving Your Work

1. Click **File → Save** (or `Ctrl+S`)
2. Choose a location and filename
3. Code is saved with appropriate extension:
   - `.bas` for BASIC
   - `.logo` for Logo
   - `.pilot` for PILOT
   - `.pas` for Pascal
   - `.pl` for Prolog
   - `.f` for Forth
   - `.c` for C

### Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+R` | Run program |
| `Ctrl+S` | Save |
| `Ctrl+O` | Open |
| `Ctrl+Z` | Undo |
| `Ctrl+Y` | Redo |
| `Ctrl+F` | Find |
| `Ctrl+H` | Replace |
| `Tab` | Indent selection |
| `Shift+Tab` | Unindent selection |

## Using the Graphics Canvas

### Turtle Graphics Basics

Most graphics use **turtle graphics** - imagine a turtle on your canvas:

```logo
FORWARD 100      ; Move turtle forward 100 pixels
RIGHT 90         ; Turn right 90 degrees
PENDOWN          ; Start drawing
FORWARD 50       ; Draw a line 50 pixels
PENUP            ; Stop drawing
```

### Canvas Controls

- **Clear** - Clear all drawings
- **Zoom** - `+` and `-` buttons or mouse wheel
- **Pan** - Click and drag to move the view
- **Reset** - Return to default view

### Colors

```logo
PENCOLOR 255 0 0      ; Red (R, G, B)
PENCOLOR 0 255 0      ; Green
PENCOLOR 0 0 255      ; Blue
```

See your language's tutorial for graphics details.

## Immediate Mode (REPL)

### What is REPL?

**REPL** = Read-Eval-Print Loop

Type code, press Enter, see results immediately.

### Examples

**BASIC:**
```basic
X = 5
PRINT X * 2
```

**Logo:**
```logo
REPEAT 4 [FORWARD 100 RIGHT 90]
```

**PILOT:**
```pilot
ACCEPT Y
PRINT Y
```

Perfect for testing code before adding it to your main program.

## Running Programs

### Basic Execution

1. **Write code** in the editor
2. **Click Run** button (or `Ctrl+R`)
3. **See output** in the Output panel

### Input/Output

Programs can interact with the user:

**BASIC:**
```basic
INPUT "Enter your age: " AGE
PRINT "Next year you'll be " AGE + 1
```

When program hits `INPUT`, the IDE waits for you to type a response.

### Debugging

#### Using the Debug Panel

1. Click **View → Debug**
2. Set breakpoints by clicking line numbers
3. Step through code with Step buttons
4. Watch variables change in real-time
5. Use **Stop** to halt execution

#### Error Messages

Errors appear in red with emoji indicators:
- ❌ **Syntax Error** - Invalid code syntax
- ⚠️ **Runtime Error** - Error during execution
- ℹ️ **Information** - FYI messages
- ✅ **Success** - Program completed

## Themes and Customization

### Switching Themes

1. Click **Theme** menu
2. Choose from 23 available themes including:
   - **Dark Themes**: Dracula, Monokai, VS Code Dark, GitHub Dark, Nord, One Dark Pro, Solarized Dark, Ocean, High Contrast Dark
   - **Light Themes**: VS Code Light, GitHub Light, Solarized Light, Spring, High Contrast Light
   - **Retro Themes**: Amber Monochrome, Green Monochrome, IBM PC CGA, Commodore 64, Apple II, DOS Blue, ZX Spectrum
   - **Modern Themes**: JetBrains Mono, Fira Code, Source Code Pro, Cascadia Code, Consolas, Monaco, Menlo, Ubuntu Mono, DejaVu Sans Mono, Liberation Mono, Courier New, Monospace

### Editor Settings

Right-click in editor for options:
- Font size
- Tab width
- Auto-indent
- Show line numbers
- Show whitespace

## Troubleshooting

### Program Won't Run

1. **Check syntax** - Are all keywords spelled correctly?
2. **Check brackets** - Do all `[` have matching `]`?
3. **Look at errors** - Red error messages show what's wrong
4. **Compare examples** - Check similar example programs

### Graphics Don't Appear

1. **Is turtle visible?** - Check `SHOWTURTLE` command
2. **Are colors visible?** - Try changing pen color
3. **Is canvas cleared?** - Try `CLEARSCREEN`
4. **Check code** - Turtle might be off-screen

### Program Runs Slowly

1. **Reduce drawing** - Fewer FORWARD/RIGHT commands
2. **Check loops** - Very large loops take time
3. **Simplify graphics** - Complex fractals are slow
4. **Use Rust version** - Better performance for intensive code

### Getting Help

1. **Check examples** - Browse Examples/ directory
2. **Read tutorials** - See [Tutorials](../tutorials/) folder
3. **Check error messages** - They're usually helpful
4. **Try immediate mode** - Test small code pieces

## Next Steps

- Read [Programming Tutorials](../tutorials/README.md) for language-specific guides
- Explore [Examples](../../Examples/) directory for sample programs
- Check [Technical Reference](../technical/) for advanced topics
- Join our community for help and ideas

---

Happy coding! 🎉
