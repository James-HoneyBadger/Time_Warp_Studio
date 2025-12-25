# Logo Language Reference

Time Warp IDE Logo Interpreter - Complete command reference.

## Language Overview

Logo is a programming language designed for education, featuring powerful turtle graphics and list processing capabilities.

## Turtle Graphics

### Movement Commands

#### FORWARD / FD
Move turtle forward by specified distance.
```logo
FORWARD 100
FD 50
```

#### BACK / BK
Move turtle backward.
```logo
BACK 50
BK 75
```

#### RIGHT / RT
Turn turtle right by specified degrees.
```logo
RIGHT 90
RT 45
```

#### LEFT / LT
Turn turtle left.
```logo
LEFT 90
LT 120
```

#### HOME
Move turtle to center (512, 384) facing up (0°).
```logo
HOME
```

#### SETXY
Set turtle position to coordinates.
```logo
SETXY 100 200
```

#### SETX / SETY
Set X or Y coordinate only.
```logo
SETX 150
SETY 200
```

#### SETHEADING / SETH
Set turtle heading (0 = up, 90 = right, 180 = down, 270 = left).
```logo
SETHEADING 45
SETH 180
```

### Pen Control

#### PENUP / PU
Lift pen (don't draw when moving).
```logo
PENUP
```

#### PENDOWN / PD
Lower pen (draw when moving).
```logo
PENDOWN
```

#### PENCOLOR / SETPC
Set pen color (0-15).
```logo
SETPENCOLOR 14
SETPC 10
```

#### PENSIZE
Set pen width in pixels.
```logo
PENSIZE 2
```

### Turtle Visibility

#### SHOWTURTLE / ST
Show turtle cursor.
```logo
SHOWTURTLE
```

#### HIDETURTLE / HT
Hide turtle cursor.
```logo
HIDETURTLE
```

### Screen Commands

#### CLEARSCREEN / CS
Clear canvas and reset turtle.
```logo
CLEARSCREEN
CS
```

#### CLEAN
Clear canvas without resetting turtle.
```logo
CLEAN
```

## Control Structures

### REPEAT
Repeat commands specified number of times.
```logo
REPEAT 4 [FORWARD 100 RIGHT 90]
```

Nested repeats:
```logo
REPEAT 8 [
  REPEAT 4 [FORWARD 50 RIGHT 90]
  RIGHT 45
]
```

### IF / IFELSE
Conditional execution.
```logo
IF :X > 0 [PRINT "Positive"]
IFELSE :X > 0 [PRINT "Positive"] [PRINT "Not positive"]
```

### WHILE
Loop while condition is true.
```logo
WHILE [:X < 100] [
  PRINT :X
  MAKE "X :X + 1
]
```

## Procedures

### TO / END
Define a procedure.
```logo
TO SQUARE :SIZE
  REPEAT 4 [FORWARD :SIZE RIGHT 90]
END

SQUARE 100
```

### Procedures with Multiple Parameters
```logo
TO RECTANGLE :WIDTH :HEIGHT
  REPEAT 2 [
    FORWARD :WIDTH
    RIGHT 90
    FORWARD :HEIGHT
    RIGHT 90
  ]
END

RECTANGLE 150 75
```

### Recursive Procedures
```logo
TO SPIRAL :SIZE :ANGLE
  IF :SIZE > 200 [STOP]
  FORWARD :SIZE
  RIGHT :ANGLE
  SPIRAL :SIZE + 5 :ANGLE
END

SPIRAL 10 90
```

### OUTPUT
Return value from procedure.
```logo
TO DOUBLE :X
  OUTPUT :X * 2
END

PRINT DOUBLE 21    ; Prints 42
```

### STOP
Exit procedure early.
```logo
TO COUNTDOWN :N
  IF :N = 0 [STOP]
  PRINT :N
  COUNTDOWN :N - 1
END
```

## Variables

### MAKE
Create or update variable.
```logo
MAKE "X 10
MAKE "NAME "Alice
```

### Reference Variables
Use colon prefix to reference variable value.
```logo
MAKE "SIZE 50
FORWARD :SIZE
PRINT :SIZE
```

### LOCAL
Declare local variable in procedure.
```logo
TO EXAMPLE
  LOCAL "TEMP
  MAKE "TEMP 42
  PRINT :TEMP
END
```

## Lists

### Creating Lists
```logo
MAKE "MYLIST [1 2 3 4 5]
MAKE "NAMES [Alice Bob Charlie]
```

### List Operations

#### FIRST
Get first element.
```logo
PRINT FIRST [1 2 3]    ; Prints 1
```

#### LAST
Get last element.
```logo
PRINT LAST [1 2 3]     ; Prints 3
```

#### BUTFIRST / BF
Get list without first element.
```logo
PRINT BUTFIRST [1 2 3]  ; Prints [2 3]
```

#### BUTLAST / BL
Get list without last element.
```logo
PRINT BUTLAST [1 2 3]   ; Prints [1 2]
```

#### ITEM
Get item at index (1-based).
```logo
PRINT ITEM 2 [A B C]    ; Prints B
```

#### COUNT
Get list length.
```logo
PRINT COUNT [1 2 3 4 5]  ; Prints 5
```

#### FPUT
Add item to front of list.
```logo
MAKE "L FPUT 0 [1 2 3]
PRINT :L    ; Prints [0 1 2 3]
```

#### LPUT
Add item to end of list.
```logo
MAKE "L LPUT 4 [1 2 3]
PRINT :L    ; Prints [1 2 3 4]
```

## Arithmetic

### Operators
```logo
PRINT 5 + 3      ; Addition: 8
PRINT 10 - 4     ; Subtraction: 6
PRINT 6 * 7      ; Multiplication: 42
PRINT 15 / 3     ; Division: 5
PRINT 2 ^ 8      ; Power: 256
PRINT 17 % 5     ; Modulo: 2
```

### Mathematical Functions
```logo
PRINT SQRT 16    ; Square root: 4
PRINT SIN 90     ; Sine (degrees): 1
PRINT COS 0      ; Cosine: 1
PRINT TAN 45     ; Tangent: 1
PRINT ABS -5     ; Absolute value: 5
PRINT INT 3.7    ; Integer part: 3
PRINT ROUND 3.5  ; Round: 4
PRINT RANDOM 100 ; Random 0-99
```

## Logical Operations

### Comparisons
```logo
PRINT 5 > 3      ; TRUE
PRINT 5 < 3      ; FALSE
PRINT 5 = 5      ; TRUE
PRINT 5 <> 3     ; TRUE (not equal)
PRINT 5 >= 5     ; TRUE
PRINT 3 <= 5     ; TRUE
```

### Boolean Operators
```logo
PRINT AND TRUE FALSE    ; FALSE
PRINT OR TRUE FALSE     ; TRUE
PRINT NOT TRUE          ; FALSE
```

## Input/Output

### PRINT / PR
Print value.
```logo
PRINT "Hello
PRINT 42
PRINT [1 2 3]
```

### TYPE
Print without newline.
```logo
TYPE "Hello
TYPE " 
TYPE "World
```

### SHOW
Print in readable format.
```logo
SHOW [1 2 3]
```

### READWORD / RW
Read word from user.
```logo
PRINT "Enter your name:
MAKE "NAME READWORD
PRINT [Hello] :NAME
```

### READLIST / RL
Read line as list.
```logo
PRINT "Enter numbers:
MAKE "NUMS READLIST
PRINT :NUMS
```

## Example Programs

### Square
```logo
TO SQUARE :SIZE
  REPEAT 4 [FORWARD :SIZE RIGHT 90]
END

SQUARE 100
```

### Polygon
```logo
TO POLYGON :SIDES :SIZE
  REPEAT :SIDES [
    FORWARD :SIZE
    RIGHT 360 / :SIDES
  ]
END

POLYGON 6 50   ; Hexagon
POLYGON 8 40   ; Octagon
```

### Spiral
```logo
TO SPIRAL :SIZE
  IF :SIZE > 200 [STOP]
  FORWARD :SIZE
  RIGHT 90
  SPIRAL :SIZE + 5
END

SPIRAL 10
```

### Star
```logo
TO STAR :SIZE
  REPEAT 5 [
    FORWARD :SIZE
    RIGHT 144
  ]
END

STAR 100
```

### Flower
```logo
TO PETAL :SIZE
  REPEAT 2 [
    REPEAT 90 [FORWARD :SIZE / 90 RIGHT 1]
    RIGHT 90
  ]
END

TO FLOWER :SIZE :PETALS
  REPEAT :PETALS [
    PETAL :SIZE
    RIGHT 360 / :PETALS
  ]
END

FLOWER 80 12
```

### Fractal Tree
```logo
TO TREE :SIZE :DEPTH
  IF :DEPTH = 0 [STOP]
  FORWARD :SIZE
  LEFT 30
  TREE :SIZE * 0.7 :DEPTH - 1
  RIGHT 60
  TREE :SIZE * 0.7 :DEPTH - 1
  LEFT 30
  BACK :SIZE
END

TREE 60 5
```

## Reserved Words

```
FORWARD FD BACK BK RIGHT RT LEFT LT
HOME SETXY SETX SETY SETHEADING SETH
PENUP PU PENDOWN PD SETPENCOLOR SETPC
PENSIZE SHOWTURTLE ST HIDETURTLE HT
CLEARSCREEN CS CLEAN
REPEAT IF IFELSE WHILE
TO END OUTPUT STOP LOCAL
MAKE FIRST LAST BUTFIRST BF BUTLAST BL
ITEM COUNT FPUT LPUT
PRINT PR TYPE SHOW READWORD RW READLIST RL
SQRT SIN COS TAN ABS INT ROUND RANDOM
AND OR NOT TRUE FALSE
```

## Best Practices

1. **Use meaningful procedure names**: `DRAW_HOUSE` not `PROC1`
2. **Keep procedures short**: Each does one thing well
3. **Use parameters**: Makes procedures reusable
4. **Add comments**: Use semicolons for inline comments
5. **Test incrementally**: Build complex shapes from simple ones

## Compatibility Notes

- Time Warp Logo uses degrees (not radians)
- Canvas is 1024×768 pixels
- Turtle starts at center (512, 384)
- Compatible with UCBLogo syntax
- Supports recursion with tail-call optimization

---

**Version**: 5.1.0  
**Platform**: Time Warp IDE for Windows 2000
