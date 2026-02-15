# Troubleshooting Guide

Solutions to common Time Warp Studio issues.

---

## Installation & Startup

### "Illegal Instruction" Error

**Error Message:**
```
Illegal instruction (core dumped)
```

**Cause:** Your CPU lacks required features (SSSE3, SSE4.1, SSE4.2, POPCNT)

**Solution:**
- Upgrade to modern CPU (Intel/AMD from 2012+)
- Use cloud computing (AWS, Azure, Google Cloud)
- Use a different computer
- Use Docker on a compatible system

**Status:** This is a system limitation, not a code issue.

### "ModuleNotFoundError: No module named 'PySide6'"

**Error:**
```
ModuleNotFoundError: No module named 'PySide6'
```

**Cause:** Dependencies not installed

**Solution:**
```bash
# Activate your virtual environment
source .venv/bin/activate    # Linux/macOS
.venv\Scripts\activate        # Windows

# Install dependencies
pip install PySide6 Pillow requests
```

### IDE Window Won't Open

**Symptoms:** You run the command but see nothing

**Solutions:**
1. Wait 5-10 seconds (first run is slow)
2. Click in the terminal window
3. Check your display server (on headless systems, use SSH X11)

### IDE Opens but is Blank

**Symptoms:** Window appears but nothing is visible

**Solutions:**
1. Check graphics drivers are up to date
2. Try resizing the window
3. Check taskbar - window might be minimized
4. Disable any graphics acceleration in OS settings

---

## Running Programs

### Program Won't Run

**Symptom:** Click Run but nothing happens

**Check:**
1. ✓ Did you select a language from dropdown?
2. ✓ Is there code in the editor?
3. ✓ Any syntax errors shown in output?

**Solutions:**
- Copy code to a simple text file, check for special characters
- Try a one-line program first: `PRINT "test"`
- Check if language is selected

### "SyntaxError" on Line X

**Error Message:**
```
❌ Error on line 5: Unexpected token 'FOR'
```

**What to do:**
1. Look at the line number shown
2. Check syntax for that line in the tutorial
3. Look for common mistakes:
   - Misspelled keywords
   - Missing quotes around strings
   - Missing END IF, NEXT, WEND

**Example:**
```basic
PRINT "Hello      ' Missing closing quote - ERROR!
FOR I = 1 TO 10   ' Missing NEXT at end - ERROR!
IF X > 5 THEN     ' Missing END IF - ERROR!
```

### Program Produces No Output

**Symptom:** Program runs (✅ shown) but no output appears

**Common Causes:**

1. **Wrong language selected**
   - Check dropdown matches your code

2. **No PRINT statements**
   - Add `PRINT` to see output
   ```basic
   PRINT "Result: "; RESULT
   ```

3. **Output might be in graphics panel**
   - For Logo, output goes to turtle canvas
   - For BASIC text, output goes to text panel

4. **Program uses INPUT**
   - A dialog box appears
   - Look for popup window
   - Click OK or CANCEL

**Solution:**
```basic
' Add this to see if program runs:
PRINT "Program started"
' ... your code ...
PRINT "Program finished"
```

### "UndefinedVariable" Error

**Error:**
```
❌ Error: Undefined variable: TOTAL
```

**Cause:** Variable used before being declared/assigned

**Fix:**
```basic
' WRONG:
PRINT X     ' X doesn't exist yet

' CORRECT:
LET X = 10
PRINT X
```

### "DivisionByZero" Error

**Error:**
```
❌ Error: Division by zero
```

**Cause:** Program tries to divide by zero

**Example:**
```basic
LET RESULT = 10 / 0   ' ERROR!
```

**Fix:**
```basic
IF DIVISOR <> 0 THEN
  LET RESULT = 10 / DIVISOR
ELSE
  PRINT "Cannot divide by zero"
END IF
```

---

## Performance

### Program Runs Slowly

**Symptoms:** Long execution time, frozen UI

**Causes:**
1. Huge loops (millions of iterations)
2. Complex graphics with many elements
3. Deep recursion
4. Inefficient algorithms

**Solutions:**
- Reduce loop iterations
- Simplify graphics (fewer points)
- Limit recursion depth
- Close other applications

### Graphics Don't Display

**Symptoms:** Running Logo but canvas stays blank

**Checks:**
1. Is PENDOWN enabled?
2. Is turtle drawing inside canvas bounds?
3. Is pen color visible on background?

**Test Code:**
```logo
HOME
PENDOWN
FORWARD 100
```

If nothing appears, issue is more serious.

### Infinite Loop Detected

**Symptom:** Program never finishes

**Common Cause:**
```logo
' WRONG - Infinite loop!
WHILE 1 = 1
  FORWARD 10
  ' No condition to exit
WEND

' WRONG - LOOP variable never changes
FOR I = 1 TO 10
  ' I doesn't change, loops forever
NEXT I
```

**Solution:** Use Stop button (Ctrl+Shift+R)

---

## Editing & Saving

### Can't Save File

**Symptom:** Save button does nothing

**Solutions:**
1. Check disk space
2. Verify directory exists and is writable
3. Try saving to different location
4. Check file permissions

### Can't Open File

**Error:**
```
Cannot open file: Permission denied
```

**Solutions:**
1. Check file exists
2. Verify you have read permission
3. Check file isn't corrupted
4. Try copying file to new location

### Unsaved Changes Lost

**Prevention:**
1. Enable auto-save in settings
2. Press Ctrl+S frequently
3. Backup important files
4. Use version control (git)

---

## Themes & Display

### Theme Won't Change

**Symptom:** Select theme but colors don't update

**Solution:**
1. Close and reopen IDE
2. Check file ~/.Time_Warp/config.json exists
3. Delete config to reset to defaults

### Text Too Small/Large

**Solutions:**
- Use Ctrl++ to zoom in
- Use Ctrl+- to zoom out
- Use Ctrl+0 to reset zoom
- Check your OS text size settings

### Garbled Text

**Cause:** Font issues

**Solution:**
1. Change theme (View → Themes)
2. Restart IDE
3. Check OS font installation

---

## Language-Specific Issues

### BASIC: Variables Keep Resetting

**Symptom:** Variable value changes unexpectedly

**Common Cause:** Variable name typo
```basic
LET TOTAL = 10
LET TOTAL = 20      ' Same variable
LET TOTAK = 30      ' Different variable (typo!)
PRINT TOTAL         ' Prints 20, not 30
```

**Solution:** Use consistent variable names

### LOGO: Turtle Off-Screen

**Symptom:** Drawing disappears

**Cause:** Turtle moved beyond canvas bounds (-300 to 300)

**Fix:**
```logo
HOME  ' Return to center
```

### LOGO: Colors Look Wrong

**Symptom:** Expected red, got different color

**Check:**
- Color must be 0-7
- SETPENCOLOR 1 = Red
- SETPENCOLOR 0 = Black (default)

---

## Getting Help

### Can't Find Answer?

1. **Check documentation** - See docs/ folder
2. **Read error message carefully** - It usually explains the problem
3. **Try a simpler version** - Isolate the problem
4. **Check examples** - Examples/ folder has working code

### Reporting Issues

If you find a bug:
1. Write clear description
2. Include the code that causes it
3. Include error message
4. List your OS and Python version
5. Report on GitHub Issues

---

## Performance Optimization

### Optimize Your Code

```basic
' SLOW: Inefficient loop
FOR I = 1 TO 10000
  LET RESULT = RESULT + I
NEXT I

' FASTER: More efficient (same result)
LET SUM = 10000 * 10001 / 2
```

### Optimize Graphics

```logo
' SLOW: Many color changes
FOR I = 1 TO 1000
  SETPENCOLOR (I MOD 8)  ' Changes color 1000 times
  FORWARD 1
NEXT I

' FASTER: Group by color
FOR COLOR = 0 TO 7
  SETPENCOLOR COLOR
  FOR I = 1 TO 125
    FORWARD 1
  NEXT I
NEXT COLOR
```

---

## Common Mistake Checklist

- [ ] Selected correct language?
- [ ] Closed all quotes properly?
- [ ] Matched all FOR with NEXT?
- [ ] Matched all IF with END IF?
- [ ] Variables initialized before use?
- [ ] PRINT statement to show output?
- [ ] No infinite loops?
- [ ] Correct variable names (watch typos)?

---

## If All Else Fails

1. **Restart IDE** - Close and reopen
2. **Check examples** - See working examples
3. **Start fresh** - Create new simple program
4. **Ask for help** - See README.md for support contacts

---

**Remember: Error messages are your friend - they tell you what's wrong!** 🐛
