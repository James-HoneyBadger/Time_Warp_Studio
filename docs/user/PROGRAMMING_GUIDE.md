# Time Warp Programming Guide

This comprehensive guide covers programming patterns and examples for BASIC, PILOT, and Logo in Time Warp IDE.

## Language Overview

### BASIC (Beginner's All-purpose Symbolic Instruction Code)

BASIC is a simple, line-numbered language perfect for learning fundamental programming concepts.

**Key Features:**
- Line-numbered structure (10, 20, 30...)
- Simple variable assignment with `LET`
- Built-in functions for math and strings
- Structured control flow with `IF/THEN/ELSE`, `FOR/NEXT`, `GOTO`

### PILOT (Programmed Inquiry, Learning Or Teaching)

PILOT is designed for educational applications and interactive lessons with a focus on user interaction.

**Key Features:**
- Command-based structure with single letters (T:, A:, J:)
- Pattern matching for user input
- Branching and conditional logic
- Built-in lesson structuring commands

### Logo

Logo is famous for turtle graphics and functional programming concepts.

**Key Features:**
- Turtle graphics with movement commands
- Functional programming with procedures
- List processing capabilities
- Recursive programming support

## BASIC Programming

### Variables and Data Types

```basic
10 LET NAME$ = "Alice"
20 LET AGE = 25
30 LET PI = 3.14159
40 LET SCORES = [85, 92, 78, 96]
```

**Data Types:**
- Numbers: `LET X = 42`, `LET Y = 3.14`
- Strings: `LET NAME$ = "Hello"`
- Arrays: `LET LIST = [1, 2, 3, 4]`

### Control Structures

**Conditional Statements:**
```basic
10 INPUT "Enter a number: ", NUM
20 IF NUM > 10 THEN PRINT "Big number!" ELSE PRINT "Small number!"
```

**Loops:**
```basic
10 FOR I = 1 TO 10
20   PRINT "Count: "; I
30 NEXT I

40 LET COUNT = 0
50 WHILE COUNT < 5
60   PRINT "Looping..."; COUNT
70   LET COUNT = COUNT + 1
80 WEND
```

**Jumps:**
```basic
10 PRINT "Start"
20 GOTO 50
30 PRINT "This won't print"
40 END
50 PRINT "Jumped here!"
60 END
```

### Functions and Subroutines

```basic
10 GOSUB 100
20 PRINT "Back from subroutine"
30 END

100 PRINT "In subroutine"
110 RETURN
```

### File I/O

```basic
10 OPEN "data.txt" FOR INPUT AS #1
20 INPUT #1, LINE$
30 PRINT "Read: "; LINE$
40 CLOSE #1

50 OPEN "output.txt" FOR OUTPUT AS #2
60 PRINT #2, "Hello, file!"
70 CLOSE #2
```

## PILOT Programming

### Basic Structure

PILOT programs use single-letter commands followed by colons:

```pilot
T: Welcome to PILOT!
A: What is your name?
T: Hello, #ANS! Nice to meet you.
J: *
```

### Commands Reference

**T:** - Type (display text)
**A:** - Accept (get user input)
**M:** - Match (pattern matching)
**J:** - Jump (goto)
**U:** - Use (call subroutine)
**E:** - End program

### Pattern Matching

```pilot
T: Guess a number between 1 and 10
A:
M:1,2,3,4,5
T: Too low! Try higher.
J:GUESS
M:6,7,8,9,10
T: Too high! Try lower.
J:GUESS
M:*
T: You got it!
E:
GUESS:
T: Try again!
J:*
```

### Lesson Structure

```pilot
T: Math Quiz
T: What is 5 + 3?
A:
M:8
T: Correct! Well done.
J:NEXT
M:*
T: Sorry, the answer is 8.
J:NEXT

NEXT:
T: Next question...
```

## Logo Programming

### Turtle Graphics

**Movement:**
```logo
FORWARD 100    ; Move forward 100 units
BACK 50        ; Move backward 50 units
RIGHT 90       ; Turn right 90 degrees
LEFT 45        ; Turn left 45 degrees
```

**Pen Control:**
```logo
PENUP          ; Stop drawing
PENDOWN        ; Start drawing
PENCOLOR [255 0 0]  ; Set pen color to red
PENWIDTH 5     ; Set pen thickness
```

### Procedures

```logo
TO SQUARE :size
  REPEAT 4 [
    FORWARD :size
    RIGHT 90
  ]
END

TO STAR :size
  REPEAT 5 [
    FORWARD :size
    RIGHT 144
  ]
END

; Use the procedures
SQUARE 100
STAR 50
```

### Recursion

```logo
TO TREE :size
  IF :size < 5 [STOP]
  FORWARD :size
  RIGHT 30
  TREE :size * 0.7
  LEFT 60
  TREE :size * 0.7
  RIGHT 30
  BACK :size
END

TREE 100
```

### Variables and Lists

```logo
MAKE "x 42
MAKE "colors [red green blue]
PRINT :x
PRINT FIRST :colors  ; red
PRINT BUTFIRST :colors  ; [green blue]
```

## Cross-Language Patterns

### Drawing Shapes

**BASIC:**
```basic
10 CLS
20 FOR I = 0 TO 360 STEP 10
30   LET X = 100 + 50 * COS(I * 3.14159 / 180)
40   LET Y = 100 + 50 * SIN(I * 3.14159 / 180)
50   CIRCLE X, Y, 2
60 NEXT I
```

**PILOT:**
```pilot
T: Drawing a circle
T: (Imagine a circle being drawn here)
J:*
```

**Logo:**
```logo
TO CIRCLE :radius
  REPEAT 36 [
    FORWARD :radius * 3.14159 * 2 / 36
    RIGHT 10
  ]
END

CIRCLE 50
```

### User Interaction

**BASIC:**
```basic
10 PRINT "What is your name?"
20 INPUT NAME$
30 PRINT "Hello, "; NAME$; "!"
```

**PILOT:**
```pilot
T: What is your name?
A:
T: Hello, #ANS!
```

**Logo:**
```logo
MAKE "name READWORD "What is your name? "
PRINT (SE "Hello, :name "!)
```

### Loops and Repetition

**BASIC:**
```basic
10 FOR I = 1 TO 5
20   PRINT "Count: "; I
30 NEXT I
```

**PILOT:**
```pilot
T: Counting to 5
T:1
T:2
T:3
T:4
T:5
T: Done!
```

**Logo:**
```logo
REPEAT 5 [
  PRINT REP COUNT
  MAKE "COUNT :COUNT + 1
]
```

## Advanced Techniques

### Animation

**BASIC:**
```basic
10 CLS
20 FOR X = 0 TO 200 STEP 5
30   CIRCLE X, 100, 10
40   SLEEP 0.1
50   CLS
60 NEXT X
```

**Logo:**
```logo
TO BOUNCE
  MAKE "x 0
  MAKE "dx 5
  WHILE [TRUE] [
    SETXY :x 0
    CIRCLE 0 0 20
    MAKE "x :x + :dx
    IF OR :x < -200 :x > 200 [MAKE "dx (0 - :dx)]
    WAIT 30
    CLS
  ]
END
```

### Data Structures

**BASIC Arrays:**
```basic
10 DIM SCORES(10)
20 FOR I = 1 TO 10
30   LET SCORES(I) = I * 10
40 NEXT I
```

**Logo Lists:**
```logo
MAKE "scores [10 20 30 40 50]
PRINT ITEM 1 :scores  ; 20 (1-based indexing)
MAKE "scores LPUT 60 :scores  ; Add to end
```

## Debugging Tips

### BASIC
- Use `PRINT` statements to show variable values
- Add line numbers in sequence for easy reference
- Use `STOP` to pause execution

### PILOT
- Use `T:` commands to show program flow
- Test pattern matching with different inputs
- Use `J:` commands to skip sections during testing

### Logo
- Use `PRINT` to display variable values
- Test procedures individually
- Use `SHOW` to display turtle state

## Best Practices

1. **Start Simple**: Begin with basic commands before complex logic
2. **Test Frequently**: Run your program often to catch errors early
3. **Use Comments**: Document your code for future reference
4. **Modular Design**: Break complex programs into smaller procedures
5. **Consistent Naming**: Use meaningful variable and procedure names

## Examples Directory

Explore the `examples/` directory for complete working programs:

- `basic_*.bas` - BASIC programming examples
- `pilot_*.pilot` - PILOT lesson examples
- `logo_*.logo` - Logo graphics examples

Each example demonstrates specific language features and programming techniques.
