# Time Warp IDE - Classroom Walkthrough #3: Logo Turtle Graphics

**Topic:** Drawing shapes with Logo  
**Duration:** 25 minutes  
**Skills:** FORWARD, RIGHT, REPEAT, procedures

---

## Setup (2 min)

1. Create a new file: `logo_shapes.logo`
2. Switch to Editor tab

---

## Step 1: Draw a Square (5 min)

Type:

```logo
FORWARD 50
RIGHT 90
FORWARD 50
RIGHT 90
FORWARD 50
RIGHT 90
FORWARD 50
RIGHT 90
```

Run. A square appears on the canvas.

**Explain:** FORWARD moves the turtle. RIGHT turns it clockwise. 4 sides × 90° = full turn.

---

## Step 2: Use REPEAT (5 min)

Replace with:

```logo
REPEAT 4 [FORWARD 50 RIGHT 90]
```

Run. Same square, much shorter code!

**Explain:** REPEAT executes commands in `[...]` N times.

---

## Step 3: Define a Procedure (8 min)

```logo
TO SQUARE :SIZE
  REPEAT 4 [FORWARD :SIZE RIGHT 90]
END

SQUARE 30
SQUARE 60
SQUARE 90
```

Run. Three squares of different sizes appear.

**Explain:** `TO NAME :PARAM ... END` defines a reusable procedure. `:SIZE` is a parameter.

---

## Step 4: Colors (5 min)

Add:

```logo
SETCOLOR RED
SQUARE 40
SETCOLOR BLUE
SQUARE 60
```

Run. Colored squares!

**Explain:** SETCOLOR changes pen color. Named colors (RED, BLUE, etc.) or hex codes (#FF0000) work.

---

## Challenge

Write a procedure `TRIANGLE :SIZE` that draws an equilateral triangle (60° turns).  
Call it three times with different colors and sizes.

**Hint:** Each turn is 120° (exterior angle).

---

## Wrap-up

Students now know:

- Basic turtle movement
- REPEAT for efficiency
- TO/END for procedures
- Color customization

Next: Combine languages and create games.
