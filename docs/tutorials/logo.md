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
REPEAT 4 [
  FORWARD 100
  RIGHT 90
]
```

Same result, much cleaner code! `REPEAT n [ ... ]` runs the commands inside the brackets `n` times.

---

## Basic Turtle Commands

### Movement

| Command | Effect |
| --------- | -------- |
| `FORWARD n` | Move turtle forward n pixels |
| `BACKWARD n` | Move turtle backward n pixels |
| `FD n` | Short for FORWARD |
| `BK n` | Short for BACKWARD |

### Turning

| Command | Effect |
| --------- | -------- |
| `RIGHT n` | Turn right n degrees |
| `LEFT n` | Turn left n degrees |
| `RT n` | Short for RIGHT |
| `LT n` | Short for LEFT |

### Pen Control

| Command | Effect |
| --------- | -------- |
| `PENUP` | Lift pen (stop drawing) |
| `PENDOWN` | Lower pen (start drawing) |
| `PU` | Short for PENUP |
| `PD` | Short for PENDOWN |
| `PENWIDTH n` | Set pen thickness |
| `SETPENCOLOR c` | Set pen color (color name, `"#RRGGBB"` hex, or `[R G B]` list) |
| `CLEAR` | Clear drawing canvas |
| `HOME` | Move turtle to center |

### Pen Colors

Use a quoted color name (`SETPENCOLOR "red`), a hex value (`SETPENCOLOR "#FF0000`), or an RGB list (`SETPENCOLOR [255 0 0]`).

Named colors: `black`, `white`, `red`, `green`, `blue`, `yellow`, `cyan`, `magenta`, `pink`, `orange`, `purple`, `brown`, `lime`, `navy`, `teal`, `maroon`, `olive`, `silver`, `gold`, `gray`, `grey`.

The default pen color is white. Numeric codes such as `SETPENCOLOR 1` do not select a color and leave the pen color unchanged.

---

## Drawing Shapes

### Triangle

```logo
REPEAT 3 [
  FORWARD 100
  RIGHT 120
]
```

### Pentagon

```logo
REPEAT 5 [
  FORWARD 100
  RIGHT 72
]
```

### Hexagon

```logo
REPEAT 6 [
  FORWARD 100
  RIGHT 60
]
```

### Circle

```logo
REPEAT 360 [
  FORWARD 1
  RIGHT 1
]
```

---

## Spirals & Patterns

### Simple Spiral

```logo
MAKE "D 1
REPEAT 100 [
  FORWARD :D
  RIGHT 10
  MAKE "D :D + 1
]
```

### Star

```logo
REPEAT 5 [
  FORWARD 100
  RIGHT 144
]
```

### Flower

```logo
REPEAT 6 [
  REPEAT 4 [
    FORWARD 50
    RIGHT 90
  ]
  RIGHT 60
]
```

---

## Using Pen Up/Down

### Moving Without Drawing

```logo
; Draw a square
REPEAT 4 [
  FORWARD 50
  RIGHT 90
]

; Move without drawing
PENUP
FORWARD 100
PENDOWN

; Draw another square
REPEAT 4 [
  FORWARD 50
  RIGHT 90
]
```

This draws two separate squares.

---

## Colors & Styles

### Colored Shapes

```logo
SETPENCOLOR "red      ; Red
REPEAT 4 [
  FORWARD 100
  RIGHT 90
]

PENUP
FORWARD 150
PENDOWN

SETPENCOLOR "blue     ; Blue
REPEAT 4 [
  FORWARD 100
  RIGHT 90
]
```

### Thick Lines

```logo
PENWIDTH 5     ; Thick pen
REPEAT 4 [
  FORWARD 100
  RIGHT 90
]

PENWIDTH 1     ; Normal pen
```

---

## Grid and Coordinates

### Position Commands

```logo
SETX 100       ; Move to X = 100
SETY 50        ; Move to Y = 50
SETHEADING 45  ; Face direction 45 degrees
PRINT XCOR     ; Print X coordinate
PRINT YCOR     ; Print Y coordinate
```

### Drawing a Grid

```logo
; Vertical lines
MAKE "X -150
REPEAT 11 [
  SETX :X
  SETY -150
  PENDOWN
  SETY 150
  PENUP
  MAKE "X :X + 30
]

; Horizontal lines
MAKE "Y -150
REPEAT 11 [
  SETX -150
  SETY :Y
  PENDOWN
  SETX 150
  PENUP
  MAKE "Y :Y + 30
]
```

---

## Advanced: Recursion

Recursion is when a procedure calls itself. It creates beautiful fractal patterns!

### Recursive Square

```logo
TO SQUARE :SIZE
  IF :SIZE < 5 [STOP]
  FORWARD :SIZE
  RIGHT 90
  SQUARE :SIZE * 0.7
END

SQUARE 100
```

### Recursive Tree

```logo
TO TREE :SIZE
  IF :SIZE < 5 [STOP]
  FORWARD :SIZE
  LEFT 30
  TREE :SIZE * 0.7
  RIGHT 60
  TREE :SIZE * 0.7
  LEFT 30
  BACK :SIZE
END

TREE 50
```

---

## Complete Example Programs

### Program 1: Olympic Rings

```logo
TO RING :COLOR :X :Y
  SETPENCOLOR :COLOR
  PENUP
  SETXY :X :Y
  PENDOWN
  REPEAT 360 [
    FORWARD 2
    RIGHT 1
  ]
END

; Draw Olympic rings
RING "red -100 50
RING "green -50 50
RING "blue 0 50
RING "yellow 50 50
RING "magenta 100 50
```

### Program 2: Mandala Pattern

```logo
TO MANDALA :SIZE :SIDES
  REPEAT :SIDES [
    FORWARD :SIZE
    RIGHT 360 / :SIDES
  ]
END

MAKE "SIZE 20
REPEAT 19 [
  MANDALA :SIZE 8
  RIGHT 5
  MAKE "SIZE :SIZE + 10
]
```

### Program 3: House

```logo
; Draw square base
REPEAT 4 [
  FORWARD 100
  RIGHT 90
]

; Draw triangle roof
LEFT 30
FORWARD 100
RIGHT 120
FORWARD 100

; Draw door
PENUP
FORWARD 30
RIGHT 90
FORWARD 40
PENDOWN
REPEAT 2 [
  FORWARD 50
  RIGHT 90
  FORWARD 25
  RIGHT 90
]
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

- Use color names (`"red`), hex (`"#FF0000`), or RGB lists (`[255 0 0]`)
- Numeric codes such as `SETPENCOLOR 1` do not change the color
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
