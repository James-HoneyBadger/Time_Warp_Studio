# Turtle Graphics Guide

Learn to create beautiful graphics using Logo turtle graphics in Time Warp Studio.

---

## Overview

Turtle graphics is a way to create visual art by controlling a virtual turtle:

- **Turtle** - A small arrow/triangle on screen
- **Pen** - Can draw or not draw as turtle moves
- **Canvas** - 800×600 pixel drawing area

Think of it like controlling a robot with a pen that can move forward, turn, and draw lines.

---

## Getting Started

### Your First Drawing

Create a file `square.logo`:

```logo
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
```

Run it (Ctrl+R). You should see a square drawn on the canvas!

### Understanding the Turtle

- **Turtle starts** at center (0, 0) facing up (North)
- **Pen is DOWN** - drawing enabled by default
- **Unit** - 1 unit ≈ 1 pixel on canvas
- **Angle** - 0°=East, 90°=North, 180°=West, 270°=South

---

## Basic Commands

### Movement

**FORWARD** (or FD)
```logo
FORWARD 100         ' Move forward 100 units, drawing a line
```

**BACKWARD** (or BK)
```logo
BACKWARD 50         ' Move backward 50 units
```

### Turning

**RIGHT** (or RT)
```logo
RIGHT 90            ' Turn right 90 degrees
FORWARD 50
RIGHT 90
```

**LEFT** (or LT)
```logo
LEFT 45             ' Turn left 45 degrees
```

### Pen Control

**PENUP** (or PU)
```logo
FORWARD 50
PENUP               ' Pen up, stop drawing
FORWARD 50          ' Move without drawing
PENDOWN             ' Pen down, resume drawing
FORWARD 50
```

**PENDOWN** (or PD)
```logo
PENDOWN             ' Start drawing (default state)
```

---

## Shapes

### Circles

```logo
CIRCLE 50           ' Draw circle with radius 50
```

Results in perfect circle centered at current position.

### Example: Multiple Circles

```logo
PENUP
SETPOSITION -100 0
PENDOWN
CIRCLE 50

PENUP
SETPOSITION 0 0
PENDOWN
CIRCLE 75

PENUP
SETPOSITION 100 0
PENDOWN
CIRCLE 50
```

---

## Procedures (Subprograms)

Define reusable sections of code:

```logo
TO SQUARE
    FORWARD 100
    RIGHT 90
    FORWARD 100
    RIGHT 90
    FORWARD 100
    RIGHT 90
    FORWARD 100
    RIGHT 90
END

SQUARE              ' Call the procedure
```

### Procedures with Parameters

```logo
TO RECTANGLE :WIDTH :HEIGHT
    FORWARD :WIDTH
    RIGHT 90
    FORWARD :HEIGHT
    RIGHT 90
    FORWARD :WIDTH
    RIGHT 90
    FORWARD :HEIGHT
END

RECTANGLE 100 50    ' Draw 100×50 rectangle
RECTANGLE 75 25     ' Draw 75×25 rectangle
```

### Star Procedure

```logo
TO STAR :SIZE
    REPEAT 5
        FORWARD :SIZE
        RIGHT 144
    END
END

STAR 100            ' Draw 100-unit star
```

---

## Loops

### REPEAT

```logo
REPEAT 4            ' Repeat next 4 commands
    FORWARD 50
    RIGHT 90
```

### WHILE

```logo
WHILE X < 360
    FORWARD 5
    RIGHT 10
    X = X + 10
END
```

---

## Recursion

Create fractal patterns using recursion:

### Recursive Tree

```logo
TO TREE :SIZE
    IF :SIZE < 5
        FORWARD :SIZE
    ELSE
        FORWARD :SIZE / 2
        RIGHT 30
        TREE :SIZE / 2
        LEFT 60
        TREE :SIZE / 2
        RIGHT 30
        BACKWARD :SIZE / 2
    END
END

TREE 100
```

### Recursive Spiral

```logo
TO SPIRAL :SIZE
    IF :SIZE > 2
        FORWARD :SIZE
        RIGHT 5
        SPIRAL :SIZE - 0.5
    END
END

SPIRAL 50
```

---

## Colors and Styling

### Set Pen Color

```logo
SETPENCOLOR 255     ' Red (0-255 scale)
FORWARD 100

SETPENCOLOR 32768   ' Green
FORWARD 100

SETPENCOLOR 128     ' Blue
FORWARD 100
```

### RGB Colors

```logo
SETPENCOLOR 255 0 0      ' Red (R, G, B)
SETPENCOLOR 0 255 0      ' Green
SETPENCOLOR 0 0 255      ' Blue
```

### Pen Width

```logo
SETPENWIDTH 1       ' Thin line
FORWARD 50

SETPENWIDTH 5       ' Thick line
FORWARD 50

SETPENWIDTH 10      ' Very thick
FORWARD 50
```

---

## Positioning

### Absolute Position

```logo
SETPOSITION 100 50  ' Jump to coordinate (100, 50)
```

### Heading

```logo
SETANGLE 0          ' Face east
FORWARD 50

SETANGLE 90         ' Face north
FORWARD 50

SETANGLE 180        ' Face west
FORWARD 50
```

### Home

```logo
HOME                ' Return to center (0, 0), face north
```

---

## Canvas Control

### Clear Canvas

```logo
CLEARSCREEN         ' Erase everything, reset turtle
```

Or in UI: Run → Clear Output

### Zoom and Pan

In the Canvas area:
- **Scroll wheel** - Zoom in/out
- **Right-click drag** - Pan around
- **Double-click** - Reset zoom

---

## Program Examples

### Polygon Maker

```logo
TO POLYGON :SIDES :SIZE
    REPEAT :SIDES
        FORWARD :SIZE
        RIGHT 360 / :SIDES
    END
END

POLYGON 3 100       ' Triangle
POLYGON 4 100       ' Square
POLYGON 6 100       ' Hexagon
POLYGON 8 100       ' Octagon
```

### Concentric Circles

```logo
FOR I = 10 TO 100 STEP 10
    CIRCLE I
NEXT I
```

### Pattern Art

```logo
TO PATTERN :ANGLE :DISTANCE
    REPEAT 12
        FORWARD :DISTANCE
        CIRCLE 10
        BACKWARD :DISTANCE
        RIGHT :ANGLE
    END
END

PATTERN 30 80
```

### Snowflake (Fractal)

```logo
TO SNOWFLAKE :SIZE
    REPEAT 6
        TO LINE :S
            IF :S < 5
                FORWARD :S
            ELSE
                LINE :S / 3
                LEFT 60
                LINE :S / 3
                RIGHT 120
                LINE :S / 3
                LEFT 60
                LINE :S / 3
            END
        END
        
        LINE :SIZE
        RIGHT 60
    END
END

SNOWFLAKE 100
```

---

## Canvas Coordinates

```
        UP (North)
        90°
        |
        |
-400 ---+--- 400    (East) 0°
        |
        |
       -300 to 300
        (South) 270°
```

- **X-axis**: -400 (left) to 400 (right)
- **Y-axis**: -300 (bottom) to 300 (top)
- **Center**: (0, 0)
- **Turtle starts**: Center facing up

---

## Tips for Beautiful Graphics

### Tip 1: Use Procedures
Reusable shapes make code cleaner and easier to modify.

### Tip 2: Experiment with Colors
Different color combinations create interesting effects.

### Tip 3: Combine Shapes
Circles + lines + procedures = complex art.

### Tip 4: Use Recursion
Fractals create intricate patterns with simple rules.

### Tip 5: Save and Modify
Save versions and experiment with parameters.

---

## Common Patterns

### Mandala

```logo
TO MANDALA :RINGS :SIZE
    REPEAT :RINGS
        CIRCLE :SIZE
        LEFT 10
        :SIZE = :SIZE - 5
    END
END

MANDALA 12 100
```

### Spirograph

```logo
FOR I = 0 TO 360 STEP 5
    SETPOSITION COS(I) * 100 SIN(I) * 100
    CIRCLE 20
NEXT I
```

### Rose Curve

```logo
FOR I = 0 TO 360 STEP 2
    X = COS(5 * I) * COS(I) * 100
    Y = COS(5 * I) * SIN(I) * 100
    SETPOSITION X Y
NEXT I
```

---

## Performance Notes

- Each drawing command updates canvas
- Complex patterns may slow rendering
- Clear canvas if too many lines (Run → Clear Output)
- Use PENUP to skip drawing during positioning

---

## Debugging Graphics

### Use the Canvas Inspector
- In Turtle Inspector panel
- Shows current turtle position and heading
- Updates in real-time during execution

### Step Through Drawing
- Use debugger to step through
- Watch canvas update line by line
- Verify each command

### View Turtle State
In Variables Inspector:
- `turtle_x`, `turtle_y` - Current position
- `turtle_angle` - Current heading
- `pen_down` - Drawing state
- `pen_color` - Current color
- `pen_width` - Current thickness

---

**For more help:**
- [USER_GUIDE.md](USER_GUIDE.md) - IDE usage
- [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) - All language syntax
- [DEBUGGER_GUIDE.md](DEBUGGER_GUIDE.md) - Debugging graphics
- [Examples/logo/](../Examples/logo/) - Example programs
