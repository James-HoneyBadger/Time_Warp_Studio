# Time Warp IDE - Classroom Walkthrough #2: BASIC Loops

**Topic:** Counting and loops in BASIC  
**Duration:** 20 minutes  
**Skills:** FOR/NEXT, PRINT, variables

---

## Setup (2 min)

1. Create a new file: `basic_counter.bas`
2. Switch to Editor tab

---

## Step 1: Simple Counting (5 min)

Type:

```basic
10 FOR I = 1 TO 5
20 PRINT I
30 NEXT I
40 END
```

Run. You should see numbers 1 through 5 in the Output.

**Explain:** `FOR I = 1 TO 5` loops with I from 1 to 5. `PRINT I` displays the current value.

---

## Step 2: Custom Steps (5 min)

Extend:

```basic
10 FOR I = 0 TO 10 STEP 2
20 PRINT "Even number:", I
30 NEXT I
40 END
```

Run. Only even numbers appear.

**Explain:** `STEP 2` increments by 2 instead of 1.

---

## Step 3: Nested Loops (8 min)

Replace with:

```basic
10 FOR ROW = 1 TO 3
20 FOR COL = 1 TO 4
30 PRINT "*",
40 NEXT COL
50 PRINT ""
60 NEXT ROW
70 END
```

Run. You'll see a 3×4 grid of asterisks.

**Explain:** Inner loop prints columns; outer loop advances rows. The comma in `PRINT "*",` suppresses newline.

---

## Challenge

Write a program that:

1. Asks the user for a number (INPUT N)
2. Prints the multiplication table for that number (1×N through 10×N)

**Hint:** Use a FOR loop and `PRINT I, "x", N, "=", I*N`

---

## Wrap-up

Students now know:

- FOR/NEXT loops with STEP
- Nested loops for grids
- How to build simple multiplication tables

Next: Turtle graphics and Logo.
