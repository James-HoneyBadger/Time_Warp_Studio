# Logo Tutorial

**Logo** is the perfect language to start with - it's visual, intuitive, and gives immediate feedback!

## What is Logo?

Logo is all about **Turtle Graphics** - imagine a turtle on your canvas that you can control with commands. The turtle draws as it moves.

## Getting Started

### Hello, Turtle! 🐢

```logo
PRINT "Hello, World!"
```

**Output:** 
```
Hello, World!
```

### Move the Turtle

```logo
FORWARD 100
```

The turtle moves forward 100 pixels and draws a line.

### Turn the Turtle

```logo
RIGHT 90
LEFT 45
```

- `RIGHT 90` - Turn right 90 degrees
- `LEFT 45` - Turn left 45 degrees

## Your First Drawing: A Square

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

**What happens:**
1. Draw forward 100 pixels
2. Turn right 90°
3. Repeat 3 more times
4. You get a square!

**Shorter version using REPEAT:**

```logo
REPEAT 4 [
  FORWARD 100
  RIGHT 90
]
```

## Drawing Basics

### Pen Control

```logo
PENUP                    ; Stop drawing
FORWARD 50               ; Move without drawing
PENDOWN                  ; Start drawing again
FORWARD 50               ; Draw a line
```

### Pen Colors

```logo
PENCOLOR 255 0 0         ; Red (R, G, B)
PENCOLOR 0 255 0         ; Green
PENCOLOR 0 0 255         ; Blue
PENCOLOR 0 0 0           ; Black
PENCOLOR 255 255 255     ; White
```

RGB format: Each number is 0-255
- First number = Red amount
- Second number = Green amount
- Third number = Blue amount

### Pen Width

```logo
PENWIDTH 1               ; Thin line
PENWIDTH 5               ; Thick line
PENWIDTH 10              ; Very thick
```

## Drawing Shapes

### Triangle

```logo
REPEAT 3 [
  FORWARD 100
  RIGHT 120
]
```

For a triangle, turn `360 / 3 = 120` degrees.

### Pentagon

```logo
REPEAT 5 [
  FORWARD 100
  RIGHT 72
]
```

For a pentagon, turn `360 / 5 = 72` degrees.

### Circle (Approximation)

```logo
REPEAT 360 [
  FORWARD 1
  RIGHT 1
]
```

Turn 1 degree 360 times = full circle!

## Variables

Store values and use them:

```logo
MAKE "SIZE 100
FORWARD :SIZE
RIGHT 90
FORWARD :SIZE
```

- `MAKE "VAR value` - Create variable
- `:VAR` - Use variable (with `:` prefix)

## Loops

### REPEAT Loop

```logo
REPEAT 5 [
  PRINT "Hi"
  FORWARD 20
]
```

Do something 5 times.

### FOR Loop (with variable)

```logo
FOR :I 1 5 [
  FORWARD :I * 20
  RIGHT 15
]
```

- `:I` goes from 1 to 5
- Each iteration, I increases by 1

## Conditionals

### IF Statement

```logo
MAKE "X 10
IF :X > 5 [
  PRINT "X is big"
  FORWARD 100
]
```

- Test a condition
- If true, run commands in brackets

### IF-ELSE

```logo
MAKE "X 3
IF :X > 5 [
  PRINT "Big"
] [
  PRINT "Small"
]
```

## Functions (Procedures)

Create your own commands:

```logo
TO SQUARE :SIZE
  REPEAT 4 [
    FORWARD :SIZE
    RIGHT 90
  ]
END

SQUARE 100
SQUARE 50
SQUARE 25
```

- `TO name :param` - Start function definition
- `END` - End function
- `:param` - Parameter (like a variable)

Multiple parameters:

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

RECTANGLE 100 50
```

## Recursion (Advanced)

Calls itself to create fractals:

```logo
TO FRACTAL :SIZE
  IF :SIZE < 5 [STOP]
  FORWARD :SIZE
  RIGHT 60
  FRACTAL :SIZE * 0.7
  LEFT 120
  FRACTAL :SIZE * 0.7
  RIGHT 60
  BACK :SIZE
END

FRACTAL 100
```

### Tree Example

```logo
TO TREE :SIZE
  IF :SIZE < 1 [STOP]
  
  FORWARD :SIZE
  RIGHT 30
  TREE :SIZE * 0.7
  LEFT 60
  TREE :SIZE * 0.7
  RIGHT 30
  BACK :SIZE
END

TREE 50
```

This draws a fractal tree!

## Math Operations

```logo
FORWARD 10 + 20        ; 30
FORWARD 100 - 25       ; 75
FORWARD 5 * 10         ; 50
FORWARD 100 / 2        ; 50
MAKE "X 5 * (3 + 2)    ; 25
```

## Common Commands Reference

### Movement
| Command | Example | What it does |
|---------|---------|--------------|
| `FORWARD` | `FORWARD 100` | Move forward N pixels |
| `BACK` | `BACK 50` | Move backward N pixels |
| `RIGHT` | `RIGHT 90` | Turn right N degrees |
| `LEFT` | `LEFT 45` | Turn left N degrees |
| `SETHEADING` | `SETHEADING 0` | Set angle (0=right, 90=up) |
| `HOME` | `HOME` | Return to center (0,0) |

### Drawing
| Command | Example | What it does |
|---------|---------|--------------|
| `PENDOWN` | `PENDOWN` | Start drawing |
| `PENUP` | `PENUP` | Stop drawing |
| `PENCOLOR` | `PENCOLOR 255 0 0` | Set color (RGB) |
| `PENWIDTH` | `PENWIDTH 3` | Set line thickness |
| `CLEARSCREEN` | `CLEARSCREEN` | Clear canvas |

### Turtle
| Command | Example | What it does |
|---------|---------|--------------|
| `SHOWTURTLE` | `SHOWTURTLE` | Show turtle |
| `HIDETURTLE` | `HIDETURTLE` | Hide turtle |
| `XCOR` | `:X XCOR` | Get X position |
| `YCOR` | `:Y YCOR` | Get Y position |

### Control
| Command | Example | What it does |
|---------|---------|--------------|
| `REPEAT` | `REPEAT 4 [...]` | Repeat block N times |
| `IF` | `IF :X > 5 [...]` | Conditional execution |
| `TO` | `TO FUNC [...]` | Define function |

## Practice Projects

### Project 1: Spirograph

```logo
REPEAT 36 [
  REPEAT 4 [
    FORWARD 100
    RIGHT 90
  ]
  RIGHT 10
]
```

### Project 2: Rainbow Circles

```logo
TO CIRCLE :RADIUS :COLOR
  PENCOLOR :COLOR
  REPEAT 360 [
    FORWARD 1
    RIGHT 1
  ]
END

CIRCLE 50 [255 0 0]
CIRCLE 75 [255 255 0]
CIRCLE 100 [0 255 0]
```

### Project 3: Snowflake

```logo
TO SNOWFLAKE :SIZE
  REPEAT 6 [
    TREE :SIZE
    RIGHT 60
  ]
END

TO TREE :SIZE
  IF :SIZE < 2 [STOP]
  FORWARD :SIZE
  RIGHT 45
  TREE :SIZE * 0.5
  LEFT 90
  TREE :SIZE * 0.5
  RIGHT 45
  BACK :SIZE
END

SNOWFLAKE 100
```

## Tips for Learning Logo

1. **Start simple** - Just FORWARD and RIGHT
2. **Use REPEAT** - Don't write same code twice
3. **Build functions** - Make reusable commands
4. **Use variables** - Parameterize your functions
5. **Use PENUP/PENDOWN** - Create space between drawings
6. **Experiment with colors** - Make it pretty
7. **Use recursion** - Create fractals

## Troubleshooting

| Problem | Solution |
|---------|----------|
| Drawing goes off-screen | Use HOME to center, check coordinates |
| Can't see drawing | Check pen color matches background |
| Program loops forever | Add `IF :SIZE < 1 [STOP]` to recursion |
| Wrong angle | Remember: 360° = full circle, 90° = quarter turn |

## Next Steps

1. ✅ Complete the tutorials above
2. 📂 Open examples from `Examples/logo/`
3. 🎨 Create your own artwork
4. 🚀 Move to BASIC to learn more programming concepts

---

Happy drawing! 🎨
