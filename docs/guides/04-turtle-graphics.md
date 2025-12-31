# Turtle Graphics Guide

Master the turtle graphics system in Time Warp IDE.

---

## Overview

Turtle graphics provide a visual way to learn programming. A virtual "turtle" moves around a canvas, drawing lines as it goes. Perfect for creating geometric art and learning coordinate systems.

---

## The Turtle Graphics Canvas

The output panel shows the turtle graphics canvas:

```
┌─────────────────────────┐
│    Turtle Canvas        │
│                         │
│        ↑ (Turtle)       │
│                         │
│  (0, 0 is center)       │
└─────────────────────────┘
```

**Canvas Properties:**
- **Size:** 600 × 600 pixels (approximately)
- **Center:** (0, 0) is the middle
- **Coordinates:** X increases right, Y increases up
- **Heading:** 0° = North, 90° = East, 180° = South, 270° = West

---

## Turtle State

At any time, the turtle has:

1. **Position** - X, Y coordinates
2. **Heading** - Direction facing (0-360°)
3. **Pen State** - Up (not drawing) or Down (drawing)
4. **Pen Color** - Current color (0-7)
5. **Pen Width** - Line thickness

---

## Essential Commands

### Movement

**FORWARD n**
```logo
FORWARD 100
```
Move turtle forward 100 pixels (in current heading direction).

**BACKWARD n**
```logo
BACKWARD 50
```
Move turtle backward 50 pixels.

**Aliases:** FD (FORWARD), BK (BACKWARD)

### Rotation

**RIGHT n**
```logo
RIGHT 90
```
Turn turtle right 90 degrees.

**LEFT n**
```logo
LEFT 45
```
Turn turtle left 45 degrees.

**Aliases:** RT (RIGHT), LT (LEFT)

### Pen Control

**PENUP** / **PENDOWN**
```logo
PENUP
FORWARD 50    ' Move without drawing
PENDOWN
FORWARD 50    ' Now drawing
```

**Aliases:** PU (PENUP), PD (PENDOWN)

### Positioning

**SETX x** / **SETY y**
```logo
SETX 100      ' Move to X = 100
SETY 50       ' Move to Y = 50
```

**SETHEADING angle**
```logo
SETHEADING 45 ' Face northeast
```

**HOME**
```logo
HOME          ' Return to (0, 0) facing north
```

---

## Pen Styling

### Colors

Set pen color with SETPENCOLOR:

```logo
SETPENCOLOR 1  ' Red
FORWARD 100
SETPENCOLOR 4  ' Blue
FORWARD 100
```

**Color Map:**
```
0 = Black       4 = Blue
1 = Red         5 = Magenta
2 = Green       6 = Cyan
3 = Yellow      7 = White
```

### Line Width

Set pen thickness:

```logo
PENWIDTH 1     ' Thin (default)
FORWARD 50

PENWIDTH 5     ' Thick
FORWARD 50

PENWIDTH 10    ' Very thick
FORWARD 50
```

### Clearing

**CLEAR**
```logo
CLEAR          ' Erase all drawings
```

The turtle remains at current position/heading.

---

## Drawing Patterns

### Regular Polygons

**Function to draw any polygon:**

```logo
PROCEDURE POLYGON SIDES SIZE
  LET ANGLE = 360 / SIDES
  FOR I = 1 TO SIDES
    FORWARD SIZE
    RIGHT ANGLE
  NEXT I
END PROCEDURE

' Usage:
POLYGON 6 100    ' Hexagon with 100-pixel sides
```

### Circles

Draw a circle by taking small steps:

```logo
FOR I = 1 TO 360
  FORWARD 1      ' Small step
  RIGHT 1        ' Small turn
NEXT I
```

Increase step size for larger circle:
```logo
FOR I = 1 TO 360
  FORWARD 2      ' Larger steps = bigger circle
  RIGHT 1
NEXT I
```

### Stars

```logo
PROCEDURE STAR POINTS SIZE
  LET ANGLE = 360 / POINTS / 2
  FOR I = 1 TO POINTS * 2
    FORWARD SIZE
    RIGHT ANGLE
  NEXT I
END PROCEDURE

' Draw a 5-pointed star
STAR 5 100
```

---

## Coordinate System

### Understanding Coordinates

```
        N (0°)
        ↑
        |
W ← (0,0) → E
90°      270°
        |
        ↓
        S (180°)
```

**Key Points:**
- **Origin** (0, 0) is center
- **X-axis** increases to the right
- **Y-axis** increases upward
- **Degrees** 0° = North, increases clockwise

### Positioning the Turtle

```logo
HOME           ' Go to (0, 0), face north

SETX 100       ' Jump to x=100, keep y
SETY -50       ' Jump to y=-50, keep x

SETHEADING 90  ' Face east (90°)
```

---

## Advanced Techniques

### Spirals

**Outward spiral:**
```logo
FOR I = 1 TO 100
  FORWARD I      ' Increase distance
  RIGHT 30
NEXT I
```

**Inward spiral:**
```logo
FOR I = 100 TO 1 STEP -1
  FORWARD I      ' Decrease distance
  RIGHT 30
NEXT I
```

### Nested Shapes

```logo
FOR SIZE = 10 TO 100 STEP 10
  FOR I = 1 TO 4
    FORWARD SIZE
    RIGHT 90
  NEXT I
  RIGHT 5        ' Rotate pattern
NEXT SIZE
```

This creates a rotating series of squares.

### Grid Drawing

```logo
' Draw vertical lines
FOR X = -100 TO 100 STEP 20
  SETX X
  SETY -100
  PENDOWN
  SETY 100
  PENUP
NEXT X

' Draw horizontal lines
FOR Y = -100 TO 100 STEP 20
  SETX -100
  SETY Y
  PENDOWN
  SETX 100
  PENUP
NEXT Y
```

---

## Performance Tips

### Drawing Efficiently

1. **Group similar colors** - Change color less often
2. **Use PENUP strategically** - Minimize drawing time
3. **Simplify circles** - Fewer steps for less detail
4. **Limit recursion depth** - Prevent excessive calculations

### Large Drawings

For complex patterns:
- Reduce line width
- Use fewer steps for circles
- Limit color changes
- Consider breaking into smaller pieces

---

## Troubleshooting Graphics

**Drawing appears upside down?**
- Remember: Y increases upward
- Use SETHEADING to face correct direction

**Can't see my drawing?**
- Check if PENUP is enabled
- Verify you're within canvas bounds
- Try CLEAR and redraw

**Colors look wrong?**
- Remember colors are 0-7 only
- Check SETPENCOLOR command
- Try different color number

**Drawing goes off-screen?**
- Check coordinates are within ~-300 to 300
- Use SETX/SETY to reposition
- Draw smaller shapes

**Performance is slow?**
- Simplify pattern
- Reduce number of loops
- Use larger step sizes

---

## Example Programs

### Concentric Circles

```logo
FOR R = 10 TO 100 STEP 10
  SETX 0
  SETY R
  FOR I = 1 TO 360
    FORWARD R * 6.28 / 360
    RIGHT 1
  NEXT I
NEXT R
```

### Checkerboard

```logo
PENWIDTH 2
SETX -100
SETY 100
FOR Y = 0 TO 10
  FOR X = 0 TO 10
    IF (X + Y) MOD 2 = 0 THEN
      SETPENCOLOR 1  ' Red
    ELSE
      SETPENCOLOR 7  ' White
    END IF
    SETX -100 + X * 20
    SETY 100 - Y * 20
    FORWARD 20
  NEXT X
NEXT Y
```

### Flower

```logo
SETPENCOLOR 2  ' Green
FORWARD 50

FOR P = 1 TO 6
  SETPENCOLOR 3  ' Yellow
  RIGHT 60
  FOR I = 1 TO 4
    FORWARD 30
    RIGHT 90
  NEXT I
NEXT P
```

---

## Next Steps

- Create your own patterns
- Combine shapes creatively
- Learn recursion with fractals
- Explore color combinations
- Run examples in Examples/logo/ directory

---

**Keep creating beautiful graphics!** 🎨
