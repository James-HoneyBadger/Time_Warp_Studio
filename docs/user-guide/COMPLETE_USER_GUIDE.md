# Time Warp Studio - Complete User Guide

## Getting Started

### System Requirements

**Desktop**:
- Windows 10+, macOS 10.13+, or Linux (Ubuntu 18.04+)
- 4GB RAM minimum (8GB recommended)
- 500MB free disk space
- Modern web browser (Chrome, Firefox, Safari, Edge)

**Mobile**:
- iOS 12+ or Android 8+
- 100MB free disk space

### Installation

#### Web-Based (Recommended)

1. Visit [https://timewarp.io](https://timewarp.io)
2. Click "Sign Up" or "Get Started"
3. Create account with email/username
4. Verify your email (check your inbox)
5. Start coding!

#### Desktop Application

1. Download from [https://timewarp.io/download](https://timewarp.io/download)
2. Run installer for your OS
3. Follow setup wizard
4. Accept terms and select installation path
5. Desktop app will launch automatically

#### Docker

```bash
docker run -d -p 8000:8000 -p 8080:8080 \
  -e DATABASE_URL=postgres://... \
  -e REDIS_URL=redis://... \
  timewarp/studio:latest
```

### First Program

1. **Create a Project**:
   - Click "New Project" or "+"
   - Select language (BASIC recommended for beginners)
   - Give your project a name
   - Click "Create"

2. **Write Code**:
   ```basic
   PRINT "Hello, World!"
   PRINT "Welcome to Time Warp Studio"
   ```

3. **Run Your Code**:
   - Click the green "Run" button or press `Ctrl+Enter`
   - Output appears in the terminal below

4. **Save Your Work**:
   - Click "Save" or press `Ctrl+S`
   - Your work is auto-saved every 30 seconds

---

## Supported Languages

### BASIC
Traditional BASIC programming language with:
- Variables, arrays, loops
- Conditional statements
- Subroutines and GOSUBs
- Built-in functions (SIN, COS, SQRT, etc.)

**Example**:
```basic
REM This is a comment
DIM arr(10)
FOR i = 1 TO 10
  LET arr(i) = i * i
NEXT i
PRINT "Squares from 1 to 10:"
FOR i = 1 TO 10
  PRINT arr(i),
NEXT i
```

### Logo
Turtle graphics language for:
- Drawing and geometry
- Recursive shapes
- Patterns and art
- Mathematical visualization

**Example**:
```logo
TO square :size
  REPEAT 4 [
    FORWARD :size
    RIGHT 90
  ]
END

TO tree :size
  IF :size < 5 [STOP]
  FORWARD :size
  LEFT 30
  tree :size * 0.7
  RIGHT 60
  tree :size * 0.7
  LEFT 30
  BACK :size
END

TREE 100
```

### Pascal
Structured programming with:
- Type declarations
- Procedures and functions
- Records and arrays
- Standard I/O

**Example**:
```pascal
PROGRAM HelloWorld;
VAR
  i: INTEGER;
BEGIN
  FOR i := 1 TO 10 DO
    WriteLn('Line ', i);
END.
```

### Prolog
Logic programming with:
- Facts and rules
- Pattern matching
- Unification
- Backtracking

**Example**:
```prolog
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

?- grandparent(tom, X).
```

### PILOT
Authoring language with:
- Conversational programming
- Menu-driven interfaces
- Instructional content
- User interaction

**Example**:
```pilot
*: Welcome
T: Type your name
A: :name
T: Hello, :name!
M: 1) Continue, 2) Quit
A: :choice
```

### Forth
Stack-based language with:
- Postfix notation
- Word definitions
- Stack manipulation
- Efficiency

**Example**:
```forth
: SQUARE DUP * ;
: CUBE DUP SQUARE * ;

5 SQUARE .    \ Output: 25
3 CUBE .      \ Output: 27
```

### C
System programming with:
- Variables and pointers
- Functions
- Arrays and strings
- File I/O

**Example**:
```c
#include <stdio.h>

int main() {
  printf("Hello, World!\n");
  for (int i = 1; i <= 5; i++) {
    printf("%d\n", i);
  }
  return 0;
}
```

---

## Editor Features

### Code Editor

**Shortcuts**:
- `Ctrl+S` / `Cmd+S`: Save
- `Ctrl+Z` / `Cmd+Z`: Undo
- `Ctrl+Y` / `Cmd+Y`: Redo
- `Ctrl+F` / `Cmd+F`: Find
- `Ctrl+H` / `Cmd+H`: Find & Replace
- `Ctrl+/` / `Cmd+/`: Comment/Uncomment
- `Ctrl+A` / `Cmd+A`: Select All
- `Tab`: Indent
- `Shift+Tab`: Outdent
- `Ctrl+K Ctrl+C`: Comment
- `Ctrl+K Ctrl+U`: Uncomment
- `Alt+Up` / `Option+Up`: Move line up
- `Alt+Down` / `Option+Down`: Move line down

### Code Completion

Type and press `Ctrl+Space` to see suggestions:
- Language keywords
- Variables in scope
- Built-in functions
- Recently used code

### Syntax Highlighting

Automatic highlighting for:
- Keywords (blue)
- Variables (green)
- Strings (red)
- Comments (gray)
- Numbers (purple)

### Code Formatting

Click "Format Code" or press `Ctrl+Shift+F`:
- Auto-indent
- Consistent spacing
- Proper alignment

### Themes

Choose your theme in Settings:
- **Light**: High contrast white background
- **Dark**: Easy on eyes, dark background
- **Dracula**: Modern dark purple
- **Solarized**: Balanced color scheme
- **Ocean**: Cool blue tones
- **Forest**: Green nature theme
- **Sunset**: Warm orange/red tones
- **Candy**: Bright, playful colors

---

## Running Code

### Basic Execution

1. Click the green **Run** button
2. Or press `Ctrl+Enter` / `Cmd+Enter`
3. Output appears in terminal below

### Advanced Execution

**Run with Settings**:
- Click dropdown arrow next to Run button
- Select "Run Settings"
- Configure:
  - Timeout (1-60 seconds)
  - Memory limit
  - Input/Output size limit
  - Debug mode

### Debugging

1. Click "Debug" or press `Ctrl+Shift+D`
2. Code runs in debug mode with:
   - Line-by-line execution
   - Variable inspection
   - Breakpoints
   - Call stack

**Debug Controls**:
- **Play**: Run to next breakpoint
- **Step Over**: Execute current line
- **Step Into**: Enter function
- **Step Out**: Exit function
- **Restart**: Restart debugging

**Breakpoints**:
- Click line number to add breakpoint
- Red dot indicates breakpoint
- Conditional: Right-click for conditions

**Watch Variables**:
- Right-click variable name
- Select "Add to Watch"
- View value in Watch panel

### Output

**Output Terminal**:
- Shows program output line-by-line
- Displays errors in red
- Shows execution time
- Color-coded messages

**Graphics Canvas** (Logo, etc.):
- Displays drawings
- Zoom and pan controls
- Save as image
- Export as SVG

**Error Messages**:
```
❌ Syntax Error on line 5
   Unexpected token: PRINT
   Did you mean: PRINT?
```

---

## Collaboration

### Sharing Projects

1. Click "Share" button
2. Generate shareable link
3. Optional: Set expiration date
4. Optional: Password protect
5. Send link to others

**Share Types**:
- **View Only**: Others can see code only
- **Edit**: Others can edit code
- **Full Access**: Others are collaborators

### Real-time Collaboration

1. Invite collaborators via Settings → Collaborators
2. Each person gets edit link
3. Changes sync in real-time
4. See other users' cursors
5. Chat within IDE

**Collaboration Features**:
- Live code sync
- Cursor positions visible
- Conflict resolution
- Chat and comments
- Activity log

### Comments & Annotations

1. Select text in code
2. Click comment icon
3. Type your comment
4. Press Enter
5. Comments show next to code

---

## File Management

### Creating Files

1. Right-click project name
2. Select "New File"
3. Name your file (.bas, .logo, etc.)
4. Click "Create"

### Uploading Files

1. Click "Upload" or drag file into editor
2. Select file from computer
3. File appears in project
4. Click to edit

### Exporting Code

1. Click "Export" menu
2. Choose format:
   - **Plain Text** (.txt)
   - **PDF** (formatted with syntax)
   - **HTML** (syntax highlighted)

### Managing Projects

**Create Project**:
- Click "New Project" or "+"
- Name and configure
- Select template (optional)
- Click "Create"

**Rename Project**:
- Right-click project
- Select "Rename"
- Type new name
- Press Enter

**Delete Project**:
- Right-click project
- Select "Delete"
- Confirm deletion
- Cannot be undone

**Archive Project**:
- Right-click project
- Select "Archive"
- Hidden from main list
- Can be restored later

---

## Graphics & Visualization

### Logo Turtle Graphics

**Basic Commands**:
```logo
FORWARD 100      ; Move forward
BACK 50          ; Move backward
RIGHT 45         ; Turn right
LEFT 90          ; Turn left
PENUP            ; Stop drawing
PENDOWN          ; Start drawing
HOME             ; Return to center
HIDETURTLE       ; Hide turtle
SHOWTURTLE       ; Show turtle
CLEARSCREEN      ; Clear drawing
```

**Drawing**:
```logo
SETPENSIZE 5     ; Line thickness
SETCOLOR "red"   ; Color name
SETFILLCOLOR "blue" ; Fill color
CIRCLE 50        ; Draw circle
FILL             ; Fill shape
```

**Procedures**:
```logo
TO square :size
  REPEAT 4 [
    FORWARD :size
    RIGHT 90
  ]
END

square 100
```

### Exporting Graphics

1. Run code with graphics
2. Right-click canvas
3. Select "Export As"
4. Choose format:
   - **PNG**: Raster image
   - **SVG**: Vector image
   - **PDF**: Print quality

---

## Settings & Customization

### Editor Settings

**Appearance**:
- Theme selection
- Font size (8-24pt)
- Font family
- Line height
- Tab size

**Behavior**:
- Auto-save interval
- Auto-format on save
- Show whitespace
- Word wrap
- Minimap

**Performance**:
- Syntax highlighting
- Code completion
- Linting level

### User Settings

**Profile**:
- Username
- Avatar
- Bio
- Location
- Preferences

**Privacy**:
- Public profile
- Show statistics
- Allow sharing
- Email notifications

**Advanced**:
- API key for integrations
- Export data
- Delete account

---

## Keyboard Shortcuts

### General

| Action | Windows/Linux | macOS |
|--------|---------------|-------|
| Save | Ctrl+S | Cmd+S |
| Undo | Ctrl+Z | Cmd+Z |
| Redo | Ctrl+Y | Cmd+Shift+Z |
| Cut | Ctrl+X | Cmd+X |
| Copy | Ctrl+C | Cmd+C |
| Paste | Ctrl+V | Cmd+V |
| Find | Ctrl+F | Cmd+F |
| Replace | Ctrl+H | Cmd+H |
| Select All | Ctrl+A | Cmd+A |

### Code

| Action | Windows/Linux | macOS |
|--------|---------------|-------|
| Run | Ctrl+Enter | Cmd+Enter |
| Debug | Ctrl+Shift+D | Cmd+Shift+D |
| Format | Ctrl+Shift+F | Cmd+Shift+F |
| Comment | Ctrl+/ | Cmd+/ |
| Indent | Tab | Tab |
| Outdent | Shift+Tab | Shift+Tab |
| Move Up | Alt+Up | Option+Up |
| Move Down | Alt+Down | Option+Down |

---

## Troubleshooting

### Code Won't Run

**Problem**: "Syntax Error"
- **Solution**: Check for typos in keywords
- **Check**: Make sure strings are quoted
- **Verify**: Parentheses are balanced

**Problem**: "Undefined Variable"
- **Solution**: Make sure variable is declared
- **Check**: Variable name spelled correctly
- **Tip**: Use `DIM` to declare arrays

**Problem**: "Stack Overflow"
- **Solution**: Check for infinite loops
- **Debug**: Add `PRINT` to trace execution
- **Limit**: Increase timeout in settings

### Performance Issues

**Slow Execution**:
- Reduce calculation complexity
- Use fewer array elements
- Optimize loops
- Check for infinite loops

**Memory Issues**:
- Don't create huge arrays
- Clear unused variables
- Use smaller image sizes
- Restart browser if needed

### Graphics Not Showing

**Problem**: Canvas is blank
- **Solution**: Make sure PENDOWN is called
- **Check**: Forward/Back commands issued
- **Zoom**: Try zooming with mouse wheel

**Problem**: Drawing is too small
- **Solution**: Increase step sizes
- **Try**: FORWARD 100 instead of FORWARD 10

---

## Tips & Tricks

### Efficiency

- Use `REPEAT` for loops instead of rewriting code
- Create `PROCEDURES` for reusable code
- Use `VARIABLES` to make code flexible
- Comment code for clarity

### Learning

- Start with simple programs
- Build up gradually in complexity
- Use provided examples
- Read other people's code
- Join community challenges

### Debugging

- Use `PRINT` statements to trace execution
- Check variable values during execution
- Use debugger for complex issues
- Test each function independently
- Check input values

### Performance

- Minimize graphics redraws
- Use efficient algorithms
- Avoid nested loops when possible
- Cache computed values
- Use appropriate data types

---

## FAQ

**Q: Is my code saved automatically?**
A: Yes, auto-save happens every 30 seconds. You can also press Ctrl+S.

**Q: Can I collaborate with others?**
A: Yes, click Share and send the link. Real-time editing is supported.

**Q: What happens to old projects?**
A: Projects are kept indefinitely. Use Archive to hide old projects.

**Q: Can I export my code?**
A: Yes, click Export to save as PDF, HTML, or plain text.

**Q: Is my code private?**
A: Yes, by default. Use Share to make projects public.

**Q: What if I find a bug?**
A: Report it at https://github.com/honey-badger/Time_Warp_Studio/issues

**Q: How do I get help?**
A: Check Help (?) menu or visit https://docs.timewarp.io

---

## Resources

- **Website**: https://timewarp.io
- **Documentation**: https://docs.timewarp.io
- **GitHub**: https://github.com/honey-badger/Time_Warp_Studio
- **Community**: https://community.timewarp.io
- **Discord**: https://discord.gg/timewarp
- **Email Support**: support@timewarp.io

---

## Version History

See [RELEASE_NOTES.md](../RELEASE_NOTES_v5.1.0.md) for detailed version history.

---

**Last Updated**: 2024-01-15
**Version**: 5.1.0
