# Time Warp IDE - Quick Reference Guide

## PILOT Language Reference

### Basic Commands

| Command | Syntax | Description | Example |
|---------|--------|-------------|---------|
| **T:** | `T:text` | Display text to output | `T:Hello, World!` |
| **A:** | `A:VAR` | Accept input into variable | `A:NAME` |
| **U:** | `U:VAR=expr` | Set variable to expression | `U:AGE=25` |
| **C:** | `C:condition` | Compute condition (true/false) | `C:AGE>18` |
| **Y:** | `Y:` | Execute next T: if last C: was true | See conditional example |
| **N:** | `N:` | Execute next T: if last C: was false | See conditional example |
| **M:** | `M:pattern` | Match last input against pattern | `M:YES` |
| **J:** | `J:label` | Jump to label | `J:START` |
| **L:** | `L:label` | Define label | `L:START` |
| **E:** | `E:` | End program | `E:` |

### Variable Interpolation
- Use `*VAR*` to insert variable value into text
- Example: `T:Hello, *NAME*!`

### Conditional Example
```pilot
T:Enter your age:
A:AGE
C:AGE>=18
Y:
T:You are an adult!
N:
T:You are a minor.
E:
```

---

## BASIC Language Reference

### Essential Commands

| Command | Syntax | Description | Example |
|---------|--------|-------------|---------|
| **PRINT** | `PRINT expr [,expr...]` | Display text/values | `PRINT "Hello", X` |
| **INPUT** | `INPUT VAR` | Get input from user | `INPUT NAME` |
| **LET** | `LET VAR = expr` | Set variable | `LET X = 10` |
| **IF...THEN** | `IF cond THEN stmt` | Conditional execution | `IF X > 5 THEN PRINT "Big"` |
| **FOR...NEXT** | `FOR VAR = start TO end [STEP n]` | Loop | `FOR I = 1 TO 10` |
| **GOTO** | `GOTO line` | Jump to line number | `GOTO 100` |
| **GOSUB** | `GOSUB line` | Call subroutine | `GOSUB 500` |
| **RETURN** | `RETURN` | Return from subroutine | `RETURN` |
| **END** | `END` | End program | `END` |

### Graphics Commands

| Command | Syntax | Description | Example |
|---------|--------|-------------|---------|
| **LINE** | `LINE x1,y1,x2,y2` | Draw line | `LINE 0,0,100,100` |
| **CIRCLE** | `CIRCLE x,y,radius` | Draw circle | `CIRCLE 0,0,50` |

### Built-in Functions

| Function | Description | Example |
|----------|-------------|---------|
| `rand()` | Random number 0-1 | `LET X = rand()` |
| `int(x)` | Integer part | `LET N = int(5.7)` |
| `sin(x)` | Sine (radians) | `LET Y = sin(X)` |
| `cos(x)` | Cosine (radians) | `LET Y = cos(X)` |
| `sqrt(x)` | Square root | `LET S = sqrt(16)` |
| `abs(x)` | Absolute value | `LET A = abs(-5)` |

### Loop Example
```basic
10 FOR I = 1 TO 5
20 PRINT I
30 NEXT I
40 END
```

### Conditional Example
```basic
10 INPUT X
20 IF X > 10 THEN PRINT "Large"
30 IF X <= 10 THEN PRINT "Small"
40 END
```

---

## Logo Language Reference

### Turtle Movement

| Command | Syntax | Description | Example |
|---------|--------|-------------|---------|
| **FORWARD** | `FORWARD n` or `FD n` | Move forward n units | `FORWARD 100` |
| **BACK** | `BACK n` or `BK n` | Move backward n units | `BACK 50` |
| **LEFT** | `LEFT n` or `LT n` | Turn left n degrees | `LEFT 90` |
| **RIGHT** | `RIGHT n` or `RT n` | Turn right n degrees | `RIGHT 45` |
| **PENUP** | `PENUP` or `PU` | Lift pen (don't draw) | `PENUP` |
| **PENDOWN** | `PENDOWN` or `PD` | Lower pen (start drawing) | `PENDOWN` |
| **HOME** | `HOME` | Return to center, face up | `HOME` |

### Drawing Settings

| Command | Syntax | Description | Example |
|---------|--------|-------------|---------|
| **PENWIDTH** | `PENWIDTH n` | Set line width | `PENWIDTH 5` |
| **SETCOLOR** | `SETCOLOR name` or `SETCOLOR #hex` | Set pen color | `SETCOLOR RED` or `SETCOLOR #FF0000` |

### Named Colors
- RED, BLUE, GREEN, YELLOW, ORANGE, PURPLE, PINK, BROWN, BLACK, WHITE, GRAY, CYAN, MAGENTA

### Control Structures

| Command | Syntax | Description | Example |
|---------|--------|-------------|---------|
| **REPEAT** | `REPEAT n [commands]` | Execute commands n times | `REPEAT 4 [FORWARD 50 RIGHT 90]` |
| **TO...END** | `TO NAME :param ... END` | Define procedure | See procedure example |

### Procedure Example
```logo
TO SQUARE :SIZE
  REPEAT 4 [
    FORWARD :SIZE
    RIGHT 90
  ]
END

SQUARE 100
```

### Nested Repeat Example
```logo
REPEAT 8 [
  REPEAT 4 [FORWARD 50 RIGHT 90]
  RIGHT 45
]
```

---

## Common Programming Patterns

### Pattern 1: Input Validation (BASIC)
```basic
10 PRINT "Enter a number 1-10:"
20 INPUT N
30 IF N < 1 THEN 10
40 IF N > 10 THEN 10
50 PRINT "Valid:", N
60 END
```

### Pattern 2: Menu System (PILOT)
```pilot
L:MENU
T:Choose: 1=Start 2=Help 3=Quit
A:CHOICE
M:1
Y:
J:START
M:2
Y:
J:HELP
M:3
Y:
E:
J:MENU
```

### Pattern 3: Drawing Patterns (Logo)
```logo
TO PATTERN :SIZE :SIDES :REPEAT
  REPEAT :REPEAT [
    REPEAT :SIDES [
      FORWARD :SIZE
      RIGHT 360 / :SIDES
    ]
    RIGHT 360 / :REPEAT
  ]
END

PATTERN 50 6 8
```

---

## Tips and Tricks

### PILOT Tips
1. **Use descriptive variable names**: `NAME`, `SCORE`, `ANSWER`
2. **Label important sections**: `L:START`, `L:MENU`, `L:END`
3. **Test conditions with C: before branching**
4. **Use M: for pattern matching user input**

### BASIC Tips
1. **Number lines by 10s**: Leaves room to insert lines later
2. **Use FOR loops** instead of counting manually
3. **GOSUB for reusable code** (like functions)
4. **Validate INPUT** to prevent errors

### Logo Tips
1. **Start simple**: Test procedures with one parameter first
2. **Use procedures**: Break complex drawings into small procedures
3. **PENUP before positioning**: Move without drawing
4. **Experiment with angles**: 360/n for regular polygons
5. **Save your work**: View → Save Canvas as PNG

---

## Debugging Strategies

### Common Errors and Solutions

| Problem | Language | Likely Cause | Solution |
|---------|----------|--------------|----------|
| Infinite loop | BASIC | Missing loop end or GOTO cycle | Add loop counter, check GOTO logic |
| Nothing draws | Logo | Pen is up | Add PENDOWN before drawing |
| Wrong output | PILOT | Variable not set | Check U: commands, verify *VAR* syntax |
| Syntax error | All | Typo in command | Check spelling against reference |
| Program stops early | PILOT | E: too soon | Move E: to actual end |

### Testing Strategies
1. **Test incrementally**: Run after each new feature
2. **Add PRINT statements**: Show variable values (BASIC)
3. **Use T: to trace**: Show progress (PILOT)
4. **Start with small values**: Test loops with 2-3 iterations first
5. **Verify input**: Echo user input back to confirm

---

## Keyboard Shortcuts (in Time Warp IDE)

- **Ctrl+S** or **Cmd+S**: Save file
- **Ctrl+O** or **Cmd+O**: Open file
- **F5** or **▶️ button**: Run program
- **Ctrl+K**: Clear output
- **View → Save Canvas as PNG**: Export graphics

---

## Additional Resources

- **Student Guide**: `docs/STUDENT_GUIDE.md` - Learn the basics
- **Teacher Guide**: `docs/TEACHER_GUIDE.md` - Lesson plans and activities
- **Developer Reference**: `docs/DEVELOPER_REFERENCE.md` - API and internals
- **Examples folder**: `examples/` - Sample programs for all three languages
- **Walkthroughs**: `docs/walkthrough_*.md` - Step-by-step tutorials

---

**Need more help?** Check the example programs in the `examples/` directory for working code you can learn from!
