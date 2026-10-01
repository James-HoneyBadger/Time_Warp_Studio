# Logo Programming & Turtle Graphics Tutorial

Create amazing graphics using Logo's turtle drawing commands!

---

## Introduction to Logo

Logo is a language designed to teach programming through visual feedback. You control a virtual "turtle" that draws pictures as it moves.

### Why Logo?

- ✅ Immediate visual feedback
- ✅ Great for teaching recursion and geometry
- ✅ Fun and creative
- ✅ Learn programming concepts through art
- ✅ Simple yet powerful

---

## Getting Started with Turtle Graphics

### Your First Drawing

```logo
FORWARD 100
RIGHT 90
FORWARD 100
```

This draws two perpendicular lines forming an L shape.

**Understanding:**
- `FORWARD 100` - Move turtle forward 100 pixels
- `RIGHT 90` - Turn turtle right 90 degrees
- Turtle has a pen that draws as it moves

### Drawing a Square

```logo
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
```

This draws a complete square!

### Making it Cleaner with a Loop

```logo
FOR I = 1 TO 4
  FORWARD 100
  RIGHT 90
NEXT I
```

Same result, much cleaner code!

---

## Basic Turtle Commands

### Movement

| Command | Effect |
|---------|--------|
| `FORWARD n` | Move turtle forward n pixels |
| `BACKWARD n` | Move turtle backward n pixels |
| `FD n` | Short for FORWARD |
| `BK n` | Short for BACKWARD |

### Turning

| Command | Effect |
|---------|--------|
| `RIGHT n` | Turn right n degrees |
| `LEFT n` | Turn left n degrees |
| `RT n` | Short for RIGHT |
| `LT n` | Short for LEFT |

### Pen Control

| Command | Effect |
|---------|--------|
| `PENUP` | Lift pen (stop drawing) |
| `PENDOWN` | Lower pen (start drawing) |
| `PU` | Short for PENUP |
| `PD` | Short for PENDOWN |
| `PENWIDTH n` | Set pen thickness |
| `SETPENCOLOR c` | Set pen color (0-7) |
| `CLEAR` | Clear drawing canvas |
| `HOME` | Move turtle to center |

### Pen Colors

Colors 0-7:
- 0: Black
- 1: Red
- 2: Green
- 3: Yellow
- 4: Blue
- 5: Magenta
- 6: Cyan
- 7: White

---

## Drawing Shapes

### Triangle

```logo
FOR I = 1 TO 3
  FORWARD 100
  RIGHT 120
NEXT I
```

### Pentagon

```logo
FOR I = 1 TO 5
  FORWARD 100
  RIGHT 72
NEXT I
```

### Hexagon

```logo
FOR I = 1 TO 6
  FORWARD 100
  RIGHT 60
NEXT I
```

### Circle

```logo
FOR I = 1 TO 360
  FORWARD 1
  RIGHT 1
NEXT I
```

---

## Spirals & Patterns

### Simple Spiral

```logo
FOR I = 1 TO 100
  FORWARD I
  RIGHT 10
NEXT I
```

### Star

```logo
FOR I = 1 TO 5
  FORWARD 100
  RIGHT 144
NEXT I
```

### Flower

```logo
FOR I = 1 TO 6
  FOR J = 1 TO 4
    FORWARD 50
    RIGHT 90
  NEXT J
  RIGHT 60
NEXT I
```

---

## Using Pen Up/Down

### Moving Without Drawing

```logo
' Draw a square
FOR I = 1 TO 4
  FORWARD 50
  RIGHT 90
NEXT I

' Move without drawing
PENUP
FORWARD 100
PENDOWN

' Draw another square
FOR I = 1 TO 4
  FORWARD 50
  RIGHT 90
NEXT I
```

This draws two separate squares.

---

## Colors & Styles

### Colored Shapes

```logo
SETPENCOLOR 1  ' Red
FOR I = 1 TO 4
  FORWARD 100
  RIGHT 90
NEXT I

PENUP
FORWARD 150
PENDOWN

SETPENCOLOR 4  ' Blue
FOR I = 1 TO 4
  FORWARD 100
  RIGHT 90
NEXT I
```

### Thick Lines

```logo
PENWIDTH 5     ' Thick pen
FOR I = 1 TO 4
  FORWARD 100
  RIGHT 90
NEXT I

PENWIDTH 1     ' Normal pen
```

---

## Grid and Coordinates

### Position Commands

```logo
SETX 100       ' Move to X = 100
SETY 50        ' Move to Y = 50
SETHEADING 45  ' Face direction 45 degrees
XCOR           ' Get X coordinate
YCOR           ' Get Y coordinate
```

### Drawing a Grid

```logo
' Vertical lines
FOR X = -150 TO 150 STEP 30
  SETX X
  SETY -150
  PENDOWN
  SETY 150
  PENUP
NEXT X

' Horizontal lines
FOR Y = -150 TO 150 STEP 30
  SETX -150
  SETY Y
  PENDOWN
  SETX 150
  PENUP
NEXT Y
```

---

## Advanced: Recursion

Recursion is when a procedure calls itself. It creates beautiful fractal patterns!

### Recursive Square

```logo
PROCEDURE SQUARE SIZE
  IF SIZE < 5 THEN RETURN
  FORWARD SIZE
  RIGHT 90
  SQUARE SIZE * 0.7
END PROCEDURE

SQUARE 100
```

### Recursive Tree

```logo
PROCEDURE TREE SIZE
  IF SIZE < 5 THEN RETURN
  FORWARD SIZE
  LEFT 30
  TREE SIZE * 0.7
  RIGHT 60
  TREE SIZE * 0.7
  LEFT 30
  BACKWARD SIZE
END PROCEDURE

TREE 50
```

---

## Complete Example Programs

### Program 1: Olympic Rings

```logo
PROCEDURE RING COLOR X Y
  SETPENCOLOR COLOR
  SETX X
  SETY Y
  FOR I = 1 TO 360
    FORWARD 2
    RIGHT 1
  NEXT I
END PROCEDURE

' Draw Olympic rings
RING 1 -100 50     ' Red
RING 2 -50 50      ' Green
RING 4 0 50        ' Blue
RING 3 50 50       ' Yellow
RING 5 100 50      ' Magenta
```

### Program 2: Mandala Pattern

```logo
PROCEDURE MANDALA SIZE SIDES
  FOR I = 1 TO SIDES
    FORWARD SIZE
    RIGHT 360 / SIDES
  NEXT I
END PROCEDURE

FOR SIZE = 20 TO 200 STEP 10
  MANDALA SIZE 8
  RIGHT 5
NEXT SIZE
```

### Program 3: House

```logo
' Draw square base
FOR I = 1 TO 4
  FORWARD 100
  RIGHT 90
NEXT I

' Draw triangle roof
LEFT 30
FORWARD 100
RIGHT 120
FORWARD 100

' Draw door
PENUP
FORWARD 30
RIGHT 90
FORWARD 40
PENDOWN
FOR I = 1 TO 2
  FORWARD 50
  RIGHT 90
  FORWARD 25
  RIGHT 90
NEXT I
```

---

## Tips for Logo Programming

1. **Start Simple** - Practice basic commands first
2. **Use Loops** - Reduce repetition with loops
3. **Test Often** - Run and see results immediately
4. **Comment Code** - Use comments for complex patterns
5. **Experiment** - Try different angles and distances
6. **Use Variables** - Store sizes and colors in variables

---

## Debugging Graphics

**Drawing in wrong place?**
- Use HOME to reset turtle to center
- Check SETX and SETY values

**Wrong colors?**
- Remember colors are 0-7
- Use different colors for different shapes

**Unexpected patterns?**
- Check loop counts
- Verify angle calculations
- Use PENUP to see what's happening

---

## Next Steps

- Experiment with colors and patterns
- Create your own fractals
- Combine shapes into scenes
- Use recursion for complex designs
- Try the examples in Examples/logo/ directory

---

**Happy turtle drawing!** 🐢
