# Troubleshooting Guide

Solutions for common problems in Time Warp Studio.

---

## Installation Problems

### IDE Won't Start

**Symptom:** Terminal shows error, window doesn't appear

**Solutions:**

1. **Check Python version** (must be 3.10+):
   ```bash
   python --version
   ```
   
2. **Check dependencies installed**:
   ```bash
   pip list | grep -i pyside
   ```
   
   If missing: `pip install PySide6 pillow`

3. **Try verbose startup**:
   ```bash
   python Platforms/Python/time_warp_ide.py --verbose
   ```

4. **Check for port conflict** (if applicable)

### "Illegal instruction" Error

**Symptom:** Process crashes immediately with "Illegal instruction"

**Cause:** CPU lacks required instruction sets (SSSE3/SSE4)

**Solutions:**
- Use modern computer (most laptops/desktops from 2010+)
- Run on cloud server with compatible CPU
- Use Windows Subsystem for Linux 2 (WSL2)
- Contact maintainer for alternative builds

### Permission Denied Error

**Symptom:** "Permission denied" when running scripts

**Solutions:**

Linux/macOS:
```bash
chmod +x Scripts/launch_ide.sh
./Scripts/launch_ide.sh
```

Windows: Run Command Prompt as Administrator

### Module Not Found Error

**Symptom:** "ModuleNotFoundError: No module named 'pyside6'"

**Solutions:**
```bash
# Install missing module
pip install PySide6

# Or reinstall all dependencies
pip install -r requirements.txt

# Check installation
python -c "import PySide6; print(PySide6.__version__)"
```

---

## Execution Problems

### Program Won't Run

**Symptom:** Click Run, nothing happens

**Solutions:**

1. **Check language selected**:
   - Run → Set Language → Choose correct language
   - Or save with proper extension (.bas, .logo, etc.)

2. **Look at console** for error message:
   - Output panel shows syntax errors
   - Fix errors shown

3. **Check for infinite loop**:
   - Run → Stop to halt program
   - Add breakpoint to debug

4. **Clear console**:
   - Run → Clear Output
   - Then try again

### Program Runs But No Output

**Symptom:** Program completes but console is empty

**Solutions:**

1. **Check output commands**:
   - BASIC: Use `PRINT`
   - Logo: Graphics appear on Canvas, not Console
   - PILOT: Check T-units are correct
   - C: Use `printf()`

2. **Example BASIC**:
   ```basic
   PRINT "Hello"    ' This outputs
   ```

3. **Example Logo**:
   ```logo
   FORWARD 100      ' Graphics show on canvas, not console
   CIRCLE 50
   ```

4. **No graphics appearing**:
   - Canvas may be zoomed out (scroll to zoom in)
   - Or moved off-screen (pan to origin)

### "Syntax Error" Messages

**Symptom:** Error message about syntax

**Solutions:**

1. **Check language grammar**:
   - See LANGUAGE_GUIDE.md for correct syntax
   - Common: Missing END, THEN, ;
   - Arrays: Use (X) not [X]

2. **Common BASIC errors**:
   ```basic
   IF X > 10 THEN        ' Must say THEN
   END IF                ' Must end IF block
   ```

3. **Common Logo errors**:
   ```logo
   TO SQUARE             ' Procedure needs name
       FORWARD 100
   END                   ' Must close with END
   ```

### Program Runs Too Slowly

**Symptom:** Graphics lag or program takes forever

**Solutions:**

1. **Check for infinite loops**:
   - Use `Run → Stop` to halt
   - Set breakpoint to debug

2. **Reduce drawing complexity**:
   - Fewer circles/shapes
   - Less recursion depth
   - Simpler graphics

3. **Close unused panels**:
   - Each panel uses CPU
   - Hide panels you don't need

4. **Check canvas size**:
   - Too many drawing commands = slow
   - Run → Clear Output to reset

5. **Example slow code**:
   ```basic
   FOR I = 1 TO 10000        ' Too many iterations
       CIRCLE I
   NEXT I
   ```
   
   Better:
   ```basic
   FOR I = 1 TO 100
       CIRCLE I * 10
   NEXT I
   ```

### Program Hangs (Infinite Loop)

**Symptom:** Program runs forever and won't stop

**Solutions:**

1. **Run → Stop** to halt execution
   - Or press Ctrl+Shift+R

2. **Find the loop**:
   - Set breakpoint in loop
   - Use debugger to step through
   - Check loop condition

3. **Example infinite loop**:
   ```basic
   WHILE TRUE          ' Infinite!
       PRINT "Help"
   WEND
   
   WHILE X < 100       ' Infinite if X never changes
       PRINT X
   WEND                ' Never increments X
   ```
   
   Better:
   ```basic
   X = 0
   WHILE X < 100
       PRINT X
       X = X + 1       ' Increment to exit eventually
   WEND
   ```

---

## Graphics Problems

### Canvas Is Black or Empty

**Symptom:** Run Logo program, canvas stays black

**Solutions:**

1. **Canvas might be zoomed out**:
   - Scroll wheel on canvas to zoom IN
   - Or View → Zoom In

2. **Drawing might be off-screen**:
   - View → Reset Zoom
   - Or double-click canvas to reset view

3. **Program not using graphics**:
   - Make sure using Logo language (.logo file)
   - BASIC: Use SCREEN, LINE, CIRCLE commands
   - Logo: Use FORWARD, CIRCLE, etc.

4. **Example working code**:
   ```logo
   CIRCLE 50
   FORWARD 100
   CIRCLE 75
   ```

### Graphics Appear Then Disappear

**Symptom:** Drawing shows, then vanishes

**Solutions:**

1. **Check for CLEARSCREEN**:
   - Clears all drawing
   - Remove if accidental

2. **Check program completion**:
   - Program ends, canvas clears
   - Intentional behavior

3. **Use Run → Clear Output** manually to reset

### Wrong Colors

**Symptom:** Colors not what you specified

**Solutions:**

1. **Check color format**:
   - Single number: 0-255 (0=red to 255=white)
   - RGB: `SETPENCOLOR 255 0 0` (Red, Green, Blue)

2. **Example colors**:
   ```logo
   SETPENCOLOR 255         ' Red
   SETPENCOLOR 32768       ' Green  
   SETPENCOLOR 128         ' Blue
   SETPENCOLOR 255 0 0     ' Red (RGB)
   SETPENCOLOR 0 255 0     ' Green (RGB)
   ```

---

## Debugger Problems

### Can't Set Breakpoint

**Symptom:** Breakpoint icons don't appear

**Solutions:**

1. **Click correct location**:
   - Click line NUMBER (left margin)
   - Red dot should appear

2. **Only works on executable lines**:
   - Can't set on blank lines
   - Can't set on comments

3. **Example valid breakpoint line**:
   ```basic
   PRINT "Hello"    ' Click here to set breakpoint
   X = 10          ' Click here OK
   ' COMMENT        ' Can't click here
   ```

### Debugger Won't Pause

**Symptom:** Run with breakpoint, but continues past it

**Solutions:**

1. **Check Debug Mode enabled**:
   - Run → Debug Mode (should be checked)
   - Or set another breakpoint

2. **Breakpoint might be past program end**:
   - Set earlier in code
   - Check program termination point

3. **Test with simple code**:
   ```basic
   X = 1
   PRINT "Before"      ' Set breakpoint here
   Y = 2
   PRINT "After"
   ```

### Variables Show Wrong Values

**Symptom:** Variable values in inspector incorrect

**Solutions:**

1. **Check step timing**:
   - Values shown AFTER each step
   - Step forward to see updated values

2. **Check variable scoping**:
   - Variables available in current scope
   - Function local variables visible during function

3. **View Timeline instead**:
   - Debug → Timeline
   - See full history of all variables

---

## File & Saving Problems

### File Won't Save

**Symptom:** Save doesn't work, no file created

**Solutions:**

1. **Check directory exists**:
   - Navigate to valid folder
   - Create folder if needed

2. **Check file permissions**:
   - Ensure write access to directory
   - Try different location

3. **Use Save As**:
   - File → Save As
   - Explicitly choose location

4. **Close read-only files**:
   - Messages if file is locked
   - Close other applications using file

### Lost Work

**Symptom:** Code disappeared, not saved

**Solutions:**

1. **You must Save manually**:
   - Ctrl+S after making changes
   - No auto-save (feature in development)

2. **Check recent files**:
   - File → Recent Files
   - Might find recent version

3. **Look in project folder**:
   - Where you usually save files
   - Find .bas, .logo files

4. **Tips for future**:
   - Save frequently (Ctrl+S)
   - Keep backups
   - Use version control (Git)

### File Size Too Large

**Symptom:** "File too large" error

**Solutions:**

1. **Remove unnecessary code**:
   - Delete unused variables
   - Simplify drawing commands

2. **Split into multiple files**:
   - Too much code in one file
   - Create separate files

3. **Typical limits**:
   - Text files: Typically 10MB+
   - IDE should handle up to 1MB easily

---

## Console & Output Problems

### Text Appears Garbled

**Symptom:** Output shows strange characters

**Solutions:**

1. **Check character encoding**:
   - Some special characters may not display
   - Use ASCII characters (A-Z, a-z, 0-9)

2. **Check string operations**:
   - BASIC: Use proper string syntax
   - Example: `PRINT "Hello"`

3. **Reduce output**:
   - Too much output may corrupt display
   - Limit PRINT statements

### Output Too Long

**Symptom:** Console fills up, slows down

**Solutions:**

1. **Run → Clear Output** to reset console

2. **Reduce program output**:
   - Use less PRINT statements
   - Skip some values in loops

3. **Example bad code**:
   ```basic
   FOR I = 1 TO 1000000     ' Too much output!
       PRINT I
   NEXT I
   ```
   
   Better:
   ```basic
   FOR I = 1 TO 100 STEP 10
       PRINT I
   NEXT I
   ```

### Can't Scroll Console

**Symptom:** Mouse scroll doesn't work on output

**Solutions:**

1. **Click in console area first** to focus
2. **Use arrow keys**:
   - Page Up / Page Down to scroll
   - Home / End for top/bottom
3. **Keyboard shortcuts**:
   - Ctrl+Home - Top of output
   - Ctrl+End - Bottom of output

---

## Theme & UI Problems

### Theme Not Applied

**Symptom:** Change theme, no visible change

**Solutions:**

1. **Restart IDE** to apply theme
   - Some UI elements need restart

2. **Check theme selected**:
   - View → Theme → [Select theme]

3. **Clear cache** (if issues persist):
   - Delete `~/.Time_Warp/config.json`
   - Restart IDE

### UI Elements Invisible

**Symptom:** Buttons, text not visible

**Solutions:**

1. **Try different theme**:
   - View → Theme → [Select different]
   - Some themes may have contrast issues

2. **Increase window size**:
   - Panels stack if window too small
   - Resize window larger

3. **Reset layout**:
   - Close IDE
   - Delete layout config
   - Restart to reset

### Text Too Small/Large

**Symptom:** Can't read editor or console

**Solutions:**

1. **Change editor font size**:
   - Edit → Preferences → Editor
   - Adjust font size (8-32)

2. **Use zoom**:
   - Ctrl++ to zoom in
   - Ctrl+- to zoom out
   - Ctrl+0 to reset

3. **Check system DPI**:
   - High-DPI displays may render small
   - Adjust system display settings

---

## Performance Optimization

### IDE is Slow

**Symptom:** General sluggishness

**Solutions:**

1. **Close unused panels**:
   - View → [Panel name] to toggle
   - Each panel uses system resources

2. **Check task manager** for other processes:
   - Close unnecessary applications
   - Free up system RAM

3. **Reduce canvas rendering**:
   - Avoid very large graphics
   - Run → Clear Output to reset

4. **Restart IDE**:
   - Sometimes memory usage grows
   - Fresh start often helps

### Graphics Render Slowly

**Symptom:** Animation is jerky, updating slowly

**Solutions:**

1. **Reduce drawing commands**:
   - Fewer CIRCLE/LINE commands
   - Simpler graphics patterns

2. **Increase refresh rate**:
   - Not configurable in current version
   - Reduce canvas complexity

3. **Check system resources**:
   - CPU usage high?
   - RAM at limit?
   - Close other apps

---

## Getting Help

If you can't solve your problem:

1. **Check documentation**:
   - USER_GUIDE.md - General IDE usage
   - LANGUAGE_GUIDE.md - Language syntax
   - FAQ.md - Common questions

2. **Search GitHub Issues**:
   - Maybe someone had same problem
   - Check existing fixes

3. **Report bug on GitHub**:
   - Include error message
   - Describe steps to reproduce
   - Include OS, Python version, code example

4. **Email maintainer**:
   - james@honey-badger.org
   - Describe problem in detail

---

## Emergency Reset

If everything breaks, nuclear option:

```bash
# Delete all settings and cache
rm -rf ~/.Time_Warp/

# Or on Windows:
rmdir /s %USERPROFILE%\.Time_Warp

# Restart IDE
python Platforms/Python/time_warp_ide.py
```

This resets to factory configuration but keeps your saved files.

---

**For more help:**
- [FAQ.md](FAQ.md) - Frequently asked questions
- [USER_GUIDE.md](USER_GUIDE.md) - Complete IDE guide
- [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) - Language reference
- [ARCHITECTURE.md](../ARCHITECTURE.md) - Technical details
