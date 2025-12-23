# Time Warp IDE - User Guide

Complete guide to using Time Warp IDE on Windows 2000.

## Getting Started

### Installation

1. Run `TimeWarpIDE-Setup-3.0.0.exe`
2. Choose installation directory (default: `C:\Program Files\TimeWarp`)
3. Select components (all recommended)
4. Create desktop shortcut (optional)
5. Click Install

### First Launch

1. Double-click Time Warp IDE desktop icon
2. The IDE opens with:
   - Empty editor window
   - Graphics canvas
   - Output console (bottom)
   - Menu bar and toolbar (top)

## Interface Overview

### Main Window

The IDE uses a Multiple Document Interface (MDI) with:

- **Menu Bar**: File, Edit, Run, Debug, Language, View, Window, Help
- **Toolbar**: Quick access to common operations
- **Status Bar**: Shows language, line/column, and messages
- **Editor Area**: Multiple tabbed code editors
- **Canvas**: Graphics output window
- **Console**: Text output and messages

### Keyboard Shortcuts

See README.md for complete list. Most common:

- `Ctrl+N`: New file
- `Ctrl+O`: Open file
- `Ctrl+S`: Save file
- `F5`: Run program
- `F9`: Toggle debug mode

## Writing Your First Program

### BASIC Hello World

1. Click **File → New** (or press `Ctrl+N`)
2. Select **Language → BASIC**
3. Type:
   ```basic
   PRINT "Hello, World!"
   ```
4. Press `F5` to run
5. See output in console: `Hello, World!`

### PILOT Hello World

1. New file (`Ctrl+N`)
2. Select **Language → PILOT**
3. Type:
   ```pilot
   T: Hello, World!
   ```
4. Press `F5`

### Logo Hello World

1. New file (`Ctrl+N`)
2. Select **Language → Logo**
3. Type:
   ```logo
   PRINT "Hello, World!"
   ```
4. Press `F5`

## Working with Files

### Supported File Types

- `.twb` - Time Warp BASIC
- `.twp` - Time Warp PILOT
- `.twl` - Time Warp Logo

### Opening Files

1. **File → Open** (or `Ctrl+O`)
2. Navigate to file location
3. Select file and click Open
4. File opens in new editor tab

### Saving Files

- **Save**: `Ctrl+S` (saves to current file)
- **Save As**: Choose new filename and location

### Recent Files

Access recently opened files via **File → Recent Files** menu.

## Using the Editor

### Syntax Highlighting

The editor automatically highlights:
- **Blue**: Keywords (PRINT, IF, FORWARD, etc.)
- **Red**: Strings ("Hello")
- **Green**: Comments (' in BASIC, ; in Logo)
- **Purple**: Numbers

### Line Numbers

Line numbers appear in left margin. Click to toggle bookmark.

### Bookmarks

- **Set**: Click line number
- **Next**: `F2`
- **Previous**: `Shift+F2`

### Find and Replace

1. **Edit → Find** (`Ctrl+F`)
2. Enter search text
3. Click Find Next
4. For replace: **Edit → Replace** (`Ctrl+H`)

### Go to Line

1. **Edit → Go To Line** (`Ctrl+G`)
2. Enter line number
3. Press OK

## Running Programs

### Execute

1. Write program in editor
2. Press `F5` (or **Run → Execute**)
3. View output in console/canvas

### Stop Execution

Press `Shift+F5` (or **Run → Stop**) to halt running program.

### Debug Mode

Enable debug mode (`F9`) for step-by-step execution.

## Graphics Canvas

### Viewing Graphics

Graphics output appears in canvas window. Programs using:
- BASIC: `LINE`, `CIRCLE`, `PSET`
- Logo: `FORWARD`, `CIRCLE`, turtle graphics
- PILOT: `G:` commands

### Canvas Controls

- **Zoom**: Mouse wheel or **View → Zoom**
- **Pan**: Click and drag canvas
- **Clear**: Automatically cleared on each run

### Exporting Graphics

1. **File → Export Canvas**
2. Choose format (BMP)
3. Enter filename
4. Click Save

## Using the Console

### Viewing Output

All program text output appears in console window at bottom.

### Console Features

- **Clear**: Right-click → Clear
- **Copy**: Select text, `Ctrl+C`
- **Find**: Right-click → Find
- **Export**: Right-click → Export to File

### Multiple Tabs

Console supports multiple tabs for different output streams (future feature).

## Debugging

### Enable Debug Mode

Press `F9` or **Debug → Toggle Debug Mode**.

### Setting Breakpoints

1. Click line number in editor (red dot appears)
2. Or position cursor and press `F8`

### Stepping Through Code

- **Step Into**: `F11` - Execute one line
- **Step Over**: `F10` - Execute without entering procedures
- **Step Out**: `Shift+F11` - Exit current procedure
- **Continue**: `F5` - Run to next breakpoint

### Watch Window

1. **Debug → Watch**
2. Enter variable name
3. See value update as program runs

### Call Stack

View procedure call stack: **Debug → Call Stack**

## Language Selection

Switch between languages via **Language** menu:

- **Language → BASIC**
- **Language → PILOT**
- **Language → Logo**

Current language shown in status bar.

## Window Management

### Arranging Windows

- **Window → Cascade**: Overlapping windows
- **Window → Tile Horizontally**: Side-by-side
- **Window → Tile Vertically**: Top-and-bottom
- **Window → Arrange Icons**: Organize minimized windows

### Showing/Hiding Panels

- **View → Editor**: Show editor
- **View → Canvas**: Show graphics canvas
- **View → Console**: Toggle console visibility
- **View → Debugger**: Toggle debugger panel
- **View → Toolbar**: Toggle toolbar
- **View → Status Bar**: Toggle status bar

## Example Workflows

### Creating a BASIC Program

1. `Ctrl+N` (New file)
2. **Language → BASIC**
3. Write code with syntax highlighting
4. `Ctrl+S` (Save as `myprogram.twb`)
5. `F5` (Run)
6. Debug if needed (`F9`, `F8` for breakpoints)
7. Export graphics if used

### Interactive PILOT Lesson

1. New file, select PILOT
2. Write interactive lesson with `T:` and `A:`
3. Test with various inputs
4. Save as `.twp` file
5. Share with students

### Logo Graphics Project

1. New file, select Logo
2. Define procedures (`TO SHAPE ... END`)
3. Test each procedure individually
4. Combine into final program
5. Export canvas as BMP
6. Save project as `.twl`

## Tips and Tricks

### Productivity

- Use `Ctrl+S` frequently to avoid losing work
- Bookmark important lines for quick navigation
- Use find (`Ctrl+F`) to locate code sections
- Keep console visible to catch errors

### Learning Languages

- Start with simple examples
- Use the language reference docs (Help menu)
- Experiment with small code snippets
- Build complex programs from simple procedures

### Graphics Programming

- Clear canvas at start of program (CLS or CS)
- Use variables for sizes to make adjustable
- Test turtle graphics step-by-step
- Export successful graphics for documentation

### Debugging

- Add PRINT statements to trace execution
- Use watch window for variable inspection
- Set breakpoints at decision points
- Step through loops one iteration at a time

## Troubleshooting

### Program Won't Run

- Check for syntax errors (highlighted in red)
- Ensure correct language selected
- Verify file is saved
- Check console for error messages

### Graphics Don't Appear

- Ensure canvas window is visible (**View → Canvas**)
- Check if graphics commands are correct
- Verify coordinates are within canvas (0-1023, 0-767)

### Can't Open File

- Check file extension (.twb, .twp, .twl)
- Ensure file isn't open in another program
- Verify file permissions
- Try Save As with new filename

### IDE Crashes or Freezes

- Check for infinite loops in program
- Press `Shift+F5` to stop execution
- Save work frequently
- Restart IDE if unresponsive

## Getting Help

### Built-in Documentation

- **Help → Contents**: General help
- **Help → BASIC Reference**: BASIC language guide
- **Help → PILOT Reference**: PILOT language guide
- **Help → Logo Reference**: Logo language guide

### About

**Help → About** shows version and copyright information.

---

**Version**: 5.0.0  
**Platform**: Windows 2000+  
**Last Updated**: 2025-01-XX
