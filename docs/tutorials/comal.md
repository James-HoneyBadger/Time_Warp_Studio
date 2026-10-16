# COMAL Tutorial

COMAL-80 (the **COM**mon **A**lgorithmic **L**anguage) was designed to give
BASIC a structured-programming backbone. It keeps BASIC's friendly line-numbered
style but adds Pascal-style blocks — `IF … ENDIF`, `FOR … NEXT`,
`WHILE … ENDWHILE`, and named procedures.

Time Warp Studio runs COMAL line-by-line, so every statement is executed in the
order the interpreter reaches it.

---

## 1. Your first program

```comal
10 PRINT "Hello, World!"
20 END
```

Line numbers are optional. `PRINT` writes text to the output panel; separate
items with `,` (a space is inserted) or `;` (no space).

```comal
10 NAME$ := "Ada"
20 PRINT "Hello, " ; NAME$      →  Hello, Ada
30 PRINT "Total", 6 * 7          →  Total 42
```

## 2. Variables and assignment

COMAL uses `:=` for assignment (a plain `=` is also accepted). Variables ending
in `$` hold strings; everything else is numeric.

```comal
10 COUNT := 0
20 TITLE$ := "Report"
30 COUNT := COUNT + 1
```

## 3. Selection

```comal
10 SCORE := 87
20 IF SCORE >= 90 THEN
30   PRINT "Grade A"
40 ELIF SCORE >= 80 THEN
50   PRINT "Grade B"
60 ELSE
70   PRINT "Keep going"
80 ENDIF
```

A single-line form is also allowed:

```comal
10 IF SCORE > 50 THEN PRINT "pass"
```

## 4. Loops

```comal
10 FOR I := 1 TO 5
20   PRINT I
30 NEXT I
```

`STEP` changes the increment, including negative values:

```comal
10 FOR I := 10 TO 1 STEP -3
20   PRINT I        →  10, 7, 4, 1
30 NEXT I
```

Conditional loops:

```comal
10 N := 0
20 WHILE N < 3
30   N := N + 1
40   PRINT N
50 ENDWHILE
```

```comal
10 N := 0
20 REPEAT
30   N := N + 1
40 UNTIL N >= 3
```

`LOOP … ENDLOOP` repeats forever; use `EXIT` to break out and `END` or `STOP`
to end the program.

## 5. Procedures

```comal
10 PROC GREET(NAME$)
20   PRINT "Hello, " ; NAME$
30 ENDPROC
40 CALL GREET("World")
```

Parameters are bound when `CALL` runs. `RETURN expr` stores a value in the
`RESULT` variable for the caller.

## 6. Arrays and DATA

```comal
10 DIM SCORES(5)
20 DATA 12, 47, 31, 8, 25
30 FOR I := 1 TO 5
40   READ SCORES(I)
50 NEXT I
60 PRINT SCORES(1)
```

## 7. Input

```comal
10 INPUT "Your name: ": NAME$
20 PRINT "Hello, " ; NAME$
```

Numeric variables read numbers, `$` variables read text.

## 8. Turtle graphics

| Command | Effect |
| --- | --- |
| `MOVE n` | Move the turtle forward `n` pixels |
| `TURN n` | Turn right `n` degrees (negative turns left) |
| `PENUP` / `PENDOWN` | Lift / lower the pen |
| `HOME` | Return to the origin |

```comal
10 FOR SIDE := 1 TO 4
20   MOVE 100
30   TURN 90
40 NEXT SIDE
```

## 9. Comments

`REM` and `//` both start a comment that runs to the end of the line.

```comal
10 REM calculate the total
20 TOTAL := 10 * 5   // ten groups of five
```

---

## Quick reference

| Statement | Purpose |
| --- | --- |
| `PRINT a, b` | Display values |
| `INPUT var` | Read a value |
| `var := expr` | Assignment |
| `IF … THEN … ELIF … ELSE … ENDIF` | Selection |
| `FOR i := a TO b STEP s … NEXT` | Counted loop |
| `WHILE cond … ENDWHILE` | Pre-tested loop |
| `REPEAT … UNTIL cond` | Post-tested loop |
| `PROC … ENDPROC` / `CALL` | Procedures |
| `DIM`, `DATA`, `READ`, `RESTORE` | Arrays and data |
| `GOTO n`, `GOSUB n`, `RETURN` | Line-number control flow |
| `MOVE`, `TURN`, `PENUP`, `PENDOWN`, `HOME` | Turtle graphics |
