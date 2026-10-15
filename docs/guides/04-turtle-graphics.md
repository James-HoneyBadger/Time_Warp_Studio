# Turtle Graphics Guide

Master the turtle graphics system in Time Warp Studio.

---

## Overview

Turtle graphics provide a visual way to learn programming. A virtual "turtle" moves around a canvas, drawing lines as it goes. Perfect for creating geometric art and learning coordinate systems. The examples in this guide use Logo syntax.

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
- **Size:** 800 × 600 pixels by default
- **Center:** (0, 0) is the middle
- **Coordinates:** X increases right, Y increases up
- **Heading:** 0° = North, 90° = East, 180° = South, 270° = West

---

## Turtle State

At any time, the turtle has:

1. **Position** - X, Y coordinates
2. **Heading** - Direction facing (0-360°)
3. **Pen State** - Up (not drawing) or Down (drawing)
4. **Pen Color** - Current color (a color name, `#RRGGBB` hex value, or RGB list; default white)
5. **Pen Width** - Line thickness (default 2)

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
FORWARD 50    ; Move without drawing
PENDOWN
FORWARD 50    ; Now drawing
```

**Aliases:** PU (PENUP), PD (PENDOWN)

### Positioning

**SETX x** / **SETY y**
```logo
SETX 100      ; Move to X = 100
SETY 50       ; Move to Y = 50
```

**SETHEADING angle**
```logo
SETHEADING 45 ; Face northeast
```

**HOME**
```logo
HOME          ; Return to (0, 0) facing north
```

---

## Pen Styling

### Colors

Set the pen color with SETPENCOLOR. Accepted forms:
- A color name: `SETPENCOLOR "red`
- A hex value: `SETPENCOLOR "#FF0000`
- RGB values in a list: `SETPENCOLOR [255 0 0]`

```logo
SETPENCOLOR "red
FORWARD 100
SETPENCOLOR "blue
FORWARD 100
```

**Named colors** (not case-sensitive):
```
BLACK    WHITE    RED      GREEN    BLUE     YELLOW
CYAN     MAGENTA  PINK     ORANGE   PURPLE   BROWN
LIME     NAVY     TEAL     MAROON   OLIVE    SILVER
GOLD     GRAY     GREY
```

Numeric codes such as `SETPENCOLOR 1` are not color codes. They leave the pen color unchanged.

### Line Width

Set pen thickness (default is 2):

```logo
PENWIDTH 1     ; Thin
FORWARD 50

PENWIDTH 5     ; Thick
FORWARD 50

PENWIDTH 10    ; Very thick
FORWARD 50
```

### Clearing

**CLEAR**
```logo
CLEAR          ; Erase all drawings
```

The turtle remains at current position/heading.

---

## Drawing Patterns

### Regular Polygons

**Procedure to draw any polygon:**

```logo
TO POLYGON :SIDES :SIZE
  MAKE "ANGLE 360 / :SIDES
  REPEAT :SIDES [
    FORWARD :SIZE
    RIGHT :ANGLE
  ]
END

; Usage:
POLYGON 6 100    ; Hexagon with 100-unit sides
```

### Circles

Draw a circle by taking small steps:

```logo
REPEAT 360 [
  FORWARD 1      ; Small step
  RIGHT 1        ; Small turn
]
```

Increase step size for larger circle:
```logo
REPEAT 360 [
  FORWARD 2      ; Larger steps = bigger circle
  RIGHT 1
]
```

### Stars

```logo
TO STAR :POINTS :SIZE
  MAKE "ANGLE 180 - 180 / :POINTS
  REPEAT :POINTS [
    FORWARD :SIZE
    RIGHT :ANGLE
  ]
END

; Draw a 5-pointed star
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
HOME           ; Go to (0, 0), face north

SETX 100       ; Jump to x=100, keep y
SETY -50       ; Jump to y=-50, keep x

SETHEADING 90  ; Face east (90°)
```

---

## Advanced Techniques

### Spirals

**Outward spiral:**
```logo
MAKE "D 1
REPEAT 100 [
  FORWARD :D     ; Increase distance
  RIGHT 30
  MAKE "D :D + 1
]
```

**Inward spiral:**
```logo
MAKE "D 100
REPEAT 100 [
  FORWARD :D     ; Decrease distance
  RIGHT 30
  MAKE "D :D - 1
]
```

### Nested Shapes

```logo
MAKE "SIZE 10
REPEAT 10 [
  REPEAT 4 [
    FORWARD :SIZE
    RIGHT 90
  ]
  RIGHT 5        ; Rotate pattern
  MAKE "SIZE :SIZE + 10
]
```

This creates a rotating series of squares, each one larger than the last.

### Grid Drawing

```logo
; Draw vertical lines
MAKE "X -100
REPEAT 11 [
  SETX :X
  SETY -100
  PENDOWN
  SETY 100
  PENUP
  MAKE "X :X + 20
]

; Draw horizontal lines
MAKE "Y -100
REPEAT 11 [
  SETX -100
  SETY :Y
  PENDOWN
  SETX 100
  PENUP
  MAKE "Y :Y + 20
]
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
- Use a color name, `#RRGGBB` hex value, or RGB list
- Numeric codes such as `SETPENCOLOR 1` are ignored
- Check the SETPENCOLOR command

**Drawing goes off-screen?**
- Check coordinates are within the canvas: about -400 to 400 (X) and -300 to 300 (Y) for the default 800 × 600 canvas
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
MAKE "R 10
REPEAT 10 [
  SETX 0
  SETY :R
  REPEAT 360 [
    FORWARD :R * 6.28 / 360
    RIGHT 1
  ]
  MAKE "R :R + 10
]
```

### Colored Squares

```logo
PENWIDTH 2
REPEAT 6 [
  SETPENCOLOR "red
  REPEAT 4 [FORWARD 40 RIGHT 90]
  SETPENCOLOR "blue
  REPEAT 4 [FORWARD 20 RIGHT 90]
  RIGHT 60
]
```

### Flower

```logo
SETPENCOLOR "green
FORWARD 50

REPEAT 6 [
  SETPENCOLOR "yellow
  RIGHT 60
  REPEAT 4 [
    FORWARD 30
    RIGHT 90
  ]
]
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
