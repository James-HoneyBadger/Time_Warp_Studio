# IDE Basics - Understanding Time Warp

Learn your way around the Time Warp IDE interface.

---

## The Main Window

```
┌─────────────────────────────────────────────────────────┐
│ File  Edit  View  Help                          [_ □ ×] │
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
└──────────────────────────────────────────────────────────┘
```

---

## Components Explained

### 1. Menu Bar

**File Menu**
- New program (Ctrl+N)
- Open existing program (Ctrl+O)
- Save program (Ctrl+S)
- Exit IDE (Ctrl+Q)

**Edit Menu**
- Undo/Redo (Ctrl+Z / Ctrl+Y)
- Cut/Copy/Paste (Ctrl+X / Ctrl+C / Ctrl+V)
- Find (Ctrl+F)
- Find & Replace (Ctrl+H)
- Select All (Ctrl+A)

**View Menu**
- Output Panel visibility
- Turtle Graphics visibility
- Themes (Dracula, Monokai, Solarized, etc.)
- Zoom In/Out (Ctrl+/-)
- Reset Zoom (Ctrl+0)

**Help Menu**
- About Time Warp
- User Manual (F1)
- Quick Reference
- Check for Updates

### 2. Toolbar

The toolbar contains quick-access tools:

| Button | Function | Shortcut |
|--------|----------|----------|
| Language dropdown | Select programming language | - |
| Run | Execute current program | Ctrl+R |
| Stop | Halt execution | Ctrl+Shift+R |
| Clear | Clear output panel | Ctrl+E |

**Language Options:**
- BASIC - Classic BASIC language
- PILOT - Computer-based instruction
- Logo - Turtle graphics language
- Python - Python 3 subset
- C - C language (experimental)
- Pascal - Pascal language (experimental)
- Prolog - Logic programming (experimental)

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
|--------|----------|
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

1. **File → Save** or press Ctrl+S
2. Choose location and filename
3. Programs saved as `.bas`, `.logo`, `.py`, etc.

### Opening Existing Programs

1. **File → Open** or press Ctrl+O
2. Navigate to program file
3. Click Open
4. Program loads in editor

### Running Programs

1. Make sure language is selected
2. Press **Ctrl+R** or click Run button
3. Wait for execution
4. See output in right panel

### Stopping Execution

- Press **Ctrl+Shift+R** or click Stop button
- Use for infinite loops or long operations

### Clearing Output

- Press **Ctrl+E** or click Clear button
- Removes all previous output

---

## Language Selection

The language dropdown in the toolbar lets you switch between programming languages.

**Switching Languages:**
1. Click the language dropdown
2. Select desired language
3. Your code editor syntax updates
4. Ready to write in new language

**Tip:** Each language has its own syntax highlighting to help you write correct code.

---

## Error Messages

When something goes wrong, the IDE shows clear error messages:

**Format:**
```
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

1. **View → Themes** in menu bar
2. Choose from available themes:
   - Dracula (dark, purple)
   - Monokai (dark, colorful)
   - Solarized Dark (dark, balanced)
   - Ocean (dark, blue)
   - Spring (light, green)
   - Sunset (dark, warm)
   - Candy (dark, pink)
   - Forest (dark, green)

3. Theme applies immediately

### Font Size

**Zoom In/Out:**
- **View → Zoom In** or Ctrl++ (increase)
- **View → Zoom Out** or Ctrl+- (decrease)
- **View → Reset Zoom** or Ctrl+0 (default)

**Font size range:** 8pt to 24pt

### Settings

Settings stored in `~/.Time_Warp/config.json`:
- Current theme preference
- Font size setting
- Recent files list
- Auto-save setting

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

Use **Clear** button or Ctrl+E to reset output panel.

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
2. Press Ctrl+S to save frequently
3. Press Ctrl+R to test
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

**Program Control**
- Ctrl+N - New program
- Ctrl+O - Open program
- Ctrl+S - Save program
- Ctrl+Q - Quit IDE

**Execution**
- Ctrl+R - Run program
- Ctrl+Shift+R - Stop program
- Ctrl+E - Clear output

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
- Ctrl+0 - Reset zoom

**Help**
- F1 - Open help

---

## Next Steps

- Learn your first language: [BASIC Tutorial](../tutorials/basic.md)
- Explore turtle graphics: [Turtle Graphics Guide](04-turtle-graphics.md)
- Run example programs: Check `Examples/` directory

**Happy programming!**
