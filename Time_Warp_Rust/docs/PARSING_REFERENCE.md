# Time Warp IDE - Parsing and Language Detection

## Command Detection Order

The interpreter determines which language executor to use based on the following precedence:

### 1. PILOT Commands (Highest Priority)
**Pattern**: Single letter followed by colon (e.g., `T:`, `A:`, `L:`, `J:`)

**Examples**:
```pilot
T:Hello World
A:NAME
L:START
J:SKIP
```

**Why First**: The `X:` pattern is unique to PILOT and unambiguous.

### 2. User-Defined Logo Procedures (Second Priority)
**Check**: Looks up first word in `logo_procedures` HashMap

**Example**:
```logo
TO SQUARE :SIZE
  REPEAT 4 [FORWARD :SIZE RIGHT 90]
END

SQUARE 100  # Calls user procedure, not any keyword
```

**Why Before Keywords**: Allows users to override/extend with custom procedures without conflicts.

### 3. Logo Keywords (Third Priority)
**Keywords**: `FORWARD`, `FD`, `BACK`, `BK`, `LEFT`, `LT`, `RIGHT`, `RT`, `PENUP`, `PU`, `PENDOWN`, `PD`, `CLEARSCREEN`, `CS`, `HOME`, `SETXY`, `REPEAT`, `TO`, `END`, `SETHEADING`, `SETH`, `SETCOLOR`, `SETPENCOLOR`, `PENWIDTH`, `SETPENSIZE`, `SETBGCOLOR`, `HIDETURTLE`, `HT`, `SHOWTURTLE`, `ST`

**Example**:
```logo
FORWARD 100
REPEAT 4 [RIGHT 90 FORWARD 50]
```

### 4. BASIC Keywords (Fourth Priority)
**Keywords**: `LET`, `PRINT`, `INPUT`, `GOTO`, `IF`, `THEN`, `FOR`, `NEXT`, `GOSUB`, `RETURN`, `REM`, `DIM`, `DATA`, `READ`, `LINE`, `CIRCLE`, `SCREEN`, `CLS`, `LOCATE`

**Example**:
```basic
10 LET X = 5
20 IF X > 3 THEN PRINT "Big"
30 FOR I = 1 TO 10
```

### 5. Default to PILOT (Lowest Priority)
If no patterns match, the command is treated as PILOT.

## BASIC Line Number Resolution

Line numbers are mapped to program line indices for O(1) GOTO/GOSUB lookup:

```basic
10 PRINT "Start"     # Index 0, line_number_map[10] = 0
20 GOTO 50           # Jump to index 2
30 PRINT "Skip"      # Index 1
50 PRINT "End"       # Index 2, line_number_map[50] = 2
```

**Key Points**:
- Line numbers can be non-sequential (10, 20, 50, 100, ...)
- GOTO/GOSUB use the `line_number_map` HashMap for fast lookups
- IF...THEN with line numbers: `IF X > 3 THEN 100`
- Unnumbered lines are allowed and execute sequentially

## Expression Evaluation

The expression evaluator supports:

### Arithmetic Operators
- Addition: `+`
- Subtraction: `-`
- Multiplication: `*`
- Division: `/`
- Exponentiation: `^`
- Modulo: `%`

### Comparison Operators
- Greater than: `>`
- Less than: `<`
- Greater or equal: `>=`
- Less or equal: `<=`
- Equal: `==` or `=` (in conditions)
- Not equal: `!=`

**Returns**: Comparisons return `1.0` for true, `0.0` for false

**Example**:
```basic
10 LET X = 5
20 IF X > 3 THEN PRINT "Yes"     # Evaluates: 5 > 3 → 1.0 (true)
30 IF X == 5 THEN GOSUB 100      # Evaluates: 5 == 5 → 1.0 (true)
```

### Functions
**Trigonometric**: `SIN`, `COS`, `TAN`, `ASIN`, `ACOS`, `ATAN`, `SINH`, `COSH`, `TANH`

**Math**: `SQRT`, `ABS`, `FLOOR`, `CEIL`, `ROUND`, `EXP`, `LOG` (natural), `LOG10`

**Special**: `MIN(a,b)`, `MAX(a,b)`, `POW(base,exp)`, `RAND()`, `INT(x)`

## Case Sensitivity

**All commands are case-insensitive**:
```logo
forward 50    # Works
FORWARD 50    # Works
FoRwArD 50    # Works
```

**Variables are case-insensitive** (internally converted to uppercase):
```basic
LET x = 5
PRINT X       # Prints 5
```

## Error Recovery

The interpreter continues execution after non-fatal errors:

```basic
10 PRINT "Start"
20 GOTO 999          # Error: line not found (logged with ❌)
30 PRINT "Continue"  # Still executes
```

**Fatal errors** (stop execution):
- Maximum iterations exceeded (100,000)
- Execution timeout (10 seconds)
- Stack overflow from deep nesting

## Mixed Language Programs

You can mix languages in a single program:

```text
T:PILOT says hello
10 PRINT "BASIC says hello"
FORWARD 50
T:PILOT again
```

**Execution order**: Sequential, line by line, with language auto-detection per line.

## Security Limits

- **Max tokens**: 1000 per expression (prevents DoS)
- **Max depth**: 100 levels of nesting (prevents stack overflow)
- **Max iterations**: 100,000 (prevents infinite loops)
- **Execution timeout**: 10 seconds

## Example: Logo Procedure Overriding BASIC Keyword

```logo
TO PRINT :MSG
  FORWARD 50
  RIGHT 90
END

PRINT "test"  # Calls Logo procedure, not BASIC PRINT
```

This works because user-defined Logo procedures are checked before BASIC keywords in the detection order.
