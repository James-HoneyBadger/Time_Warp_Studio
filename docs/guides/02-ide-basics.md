# IDE Basics - Understanding Time Warp

Learn your way around the Time Warp Studio interface.

---

## The Main Window

```text
┌─────────────────────────────────────────────────────────┐
│ File  Edit  Run  Debug  View  Tools  Help      [_ □ ×] │
├──────────────────────────────────────────────────────────┤
│ Language: [Basic ▼]  [Run] [Stop] [Clear]              │
├──────────────────────┬────────────────────────────────────┤
│                      │                                    │
│                      │                                    │
│   Code Editor        │      Output / Graphics Panel      │
│                      │                                    │
│  (Write your code)   │   (Results appear here)           │
│                      │                                    │
│                      │                                    │
├──────────────────────┴────────────────────────────────────┤
│ Ready                                                    │
├──────────────────────────────────────────────────────────┤
│ FN 1Help 2Save 3Open 4Outline ... 9Run 10Menu           │
└──────────────────────────────────────────────────────────┘
```

The bottom **Function-Key Bar** shows the Borland Turbo-style F-key bindings. Hold **Shift** or **Ctrl** to see those layers, or click the **FN** badge to cycle them.

---

## Components Explained

### 1. Menu Bar

**File Menu**

- New program (Ctrl+N)
- Open existing program (F3 / Ctrl+O)
- Save program (F2 / Ctrl+S)
- Save As (Shift+F2 / Ctrl+Shift+S)
- Export, Print, Version History, Preferences (Ctrl+,)
- Exit IDE (Alt+X / Ctrl+Q)

**Edit Menu**

- Undo/Redo (Ctrl+Z / Ctrl+Y)
- Cut/Copy/Paste (Ctrl+X / Ctrl+C / Ctrl+V)
- Find (Ctrl+F)
- Find & Replace (Ctrl+H)
- Find in Files (Shift+F4 / Ctrl+Alt+G)
- Go to Line (Ctrl+G)

**Run Menu**

- Run Program (F9 / Ctrl+R)
- Reset Program (Ctrl+F2)
- Run Selection (Shift+F9 / Ctrl+Shift+R)
- Clear Output / Clear Canvas

**Debug Menu**

- Start / Continue Debugging (Ctrl+F9)
- Trace Into (F7), Step Over (F8), Step Out (Shift+F8)
- Toggle Breakpoint (Ctrl+F8)

**View Menu**

- Themes (Dracula, Monokai, Solarized, etc.)
- Font family and size
- Zoom In/Out (Ctrl++ / Ctrl+-)
- Split editor, focus mode, command palette (F10)

**Tools Menu**

- Compare Languages

**Help Menu**

- User Manual
- Context Help (F1)
- Lessons (Ctrl+F3)
- Browse Examples (Shift+F3)
- Quick Reference
- Keyboard Shortcuts (Shift+F1)
- About Time Warp Studio

### 2. Toolbar

The toolbar contains quick-access tools:

| Button | Function | Shortcut |
| -------- | ---------- | ---------- |
| Language dropdown | Select programming language | - |
| Run | Execute current program | F9 / Ctrl+R |
| Stop | Halt execution | Ctrl+F2 |
| Clear | Clear output panel | - |

**Language Options (9 total):**

- BASIC, Logo, PILOT — educational classics
- C, Pascal, Forth, Prolog — structured/systems
- Python — multi-paradigm
- Brainfuck — esoteric

### 3. Code Editor (Left Panel)

The code editor is where you write your programs.

**Features:**

- **Syntax Highlighting** - Keywords, strings, numbers in color
- **Line Numbers** - Track line numbers for errors
- **Auto-indentation** - Automatic indenting
- **Text Selection** - Click and drag to select
- **Keyboard Shortcuts** - Standard editing shortcuts

**Common Editor Actions:**

| Action | Shortcut |
| -------- | ---------- |
| Select All | Ctrl+A |
| Copy | Ctrl+C |
| Cut | Ctrl+X |
| Paste | Ctrl+V |
| Undo | Ctrl+Z |
| Redo | Ctrl+Y |
| Find | Ctrl+F |
| Replace | Ctrl+H |
| Goto Line | Ctrl+G |
| Indent | Tab |
| Unindent | Shift+Tab |

### 4. Output / Graphics Panel (Right Panel)

Shows program output and graphics:

**Text Output**

- Program PRINT statements appear here
- Error messages shown in red with ❌
- Status messages with emojis (✅ ℹ️ 🚀)

**Turtle Graphics**

- Drawing canvas for Logo programs
- Turtle position and heading displayed
- Clear button to reset graphics

**Interactive Input**

- Input prompts appear as dialog boxes
- Type your response and press Enter
- Results returned to program

---

## Working with Programs

### Creating a New Program

1. **File → New** or press Ctrl+N
2. Select language from dropdown
3. Start typing code
4. Click **Run** to execute

### Saving Your Work

1. **File → Save** or press F2 (Ctrl+S)
2. Choose location and filename
3. Programs saved as `.bas`, `.logo`, `.py`, etc.

### Opening Existing Programs

1. **File → Open** or press F3 (Ctrl+O)
2. Navigate to program file
3. Click Open
4. Program loads in editor

### Running Programs

1. Make sure language is selected
2. Press **F9** (or Ctrl+R) or click Run button
3. Wait for execution
4. See output in right panel

### Stopping Execution

- Press **Ctrl+F2** (Reset Program) or click Stop button
- Use for infinite loops or long operations

### Clearing Output

- Use **Run → Clear Output** or click Clear button
- Removes all previous output

---

## Language Selection

The language dropdown in the toolbar lets you switch between programming languages.

**Switching Languages:**

1. Click the language dropdown (or press Ctrl+Shift+L for the Language Picker)
2. Select desired language
3. Your code editor syntax updates
4. Ready to write in new language

**Tip:** Each language has its own syntax highlighting to help you write correct code.

---

## Error Messages

When something goes wrong, the IDE shows clear error messages:

**Format:**

```text
❌ Error on line 5: Unexpected token 'FOR'
  BASIC Line: FOR I = 1 TO 10
              ^
```

**Reading Errors:**

- **Line Number** - Where the error occurred
- **Error Type** - What went wrong
- **Context** - The offending line of code
- **Pointer** - Exact position of error (^)

**Common Errors:**

- `SyntaxError` - Incorrect grammar or spelling
- `UndefinedVariable` - Variable not declared
- `DivisionByZero` - Can't divide by 0
- `TypeMismatch` - Wrong data type used

---

## Themes & Customization

### Changing Themes

1. **View → Theme** in menu bar
2. Choose from 33 built-in themes, for example:
   - Dracula (dark, purple)
   - Monokai (dark, colorful)
   - Solarized Dark (dark, balanced)
   - Ocean (dark, blue)
   - Spring (light, green)
   - Nord, Tokyo Night, Catppuccin Mocha
   - Retro CRT themes (Amber, Green, Commodore 64, Apple II)

3. Theme applies immediately (Shift+F10 cycles workstation themes)

### Font Size

**Zoom In/Out:**

- **View → Zoom In** or Ctrl++ (increase)
- **View → Zoom Out** or Ctrl+- (decrease)

### Settings

Settings are stored with Qt QSettings. Auto-save is configured under File → Preferences (Ctrl+,). See [06-settings.md](06-settings.md) for details.

---

## Input/Output

### Printing Output

**BASIC:**

```basic
PRINT "Hello, World!"
```

**Logo:**

```logo
PRINT [Hello World]
```

**Python:**

```python
print("Hello, World!")
```

Output appears in right panel.

### Getting User Input

**BASIC:**

```basic
INPUT "Enter your name: "; NAME$
PRINT "Hello "; NAME$
```

A dialog box appears for input entry.

### Clearing Output

Use **Clear** button or **Run → Clear Output** to reset output panel.

---

## Status Bar

Bottom of window shows status information:

- **Ready** - IDE is idle, ready for input
- **Running...** - Program is executing
- **Completed** - Program finished successfully
- **Error** - Program encountered an error
- Line and column numbers (bottom right)

---

## Tips & Tricks

### Efficient Workflow

1. Write your program
2. Press F2 to save frequently
3. Press F9 to test
4. Check output for errors
5. Fix and repeat

### Testing Programs

- Start with small, simple programs
- Test each feature separately
- Use clear variable names
- Add comments (for applicable languages)
- Gradually increase complexity

### Debugging

1. Read error messages carefully
2. Check line numbers in errors
3. Look at variable values
4. Use PRINT statements to trace execution
5. Run simpler test programs first

### Performance

- Close other applications for faster execution
- Simplify graphics for smoother drawing
- Break large programs into smaller pieces
- Save before running unfamiliar code

---

## Keyboard Shortcuts Reference

Time Warp Studio uses Borland Turbo-style keys; press **Shift+F1** for the full live list.

**Program Control**

- Ctrl+N - New program
- F3 / Ctrl+O - Open program
- F2 / Ctrl+S - Save program
- Alt+X / Ctrl+Q - Exit IDE

**Execution**

- F9 / Ctrl+R - Run program
- Ctrl+F2 - Reset (stop) program
- Shift+F9 / Ctrl+Shift+R - Run selection

**Editor**

- Ctrl+Z - Undo
- Ctrl+Y - Redo
- Ctrl+A - Select all
- Ctrl+C - Copy
- Ctrl+X - Cut
- Ctrl+V - Paste
- Ctrl+F - Find
- Ctrl+H - Find & Replace

**View**

- Ctrl++ - Zoom in
- Ctrl+- - Zoom out
- F10 / Ctrl+Shift+P - Command palette

**Help**

- F1 - Context help for the current language
- Ctrl+F3 - Lessons

---

## Next Steps

- Learn your first language: [BASIC Tutorial](../tutorials/basic.md)
- Explore turtle graphics: [Turtle Graphics Guide](04-turtle-graphics.md)
- Run example programs: Check `Examples/` directory

**Happy programming!**
