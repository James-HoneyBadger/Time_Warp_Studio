# COBOL Tutorial

## Introduction

COBOL (COmmon Business-Oriented Language, 1959) was designed for business data processing and is still used in banking, insurance, and government systems worldwide. Its verbose, English-like syntax makes programs self-documenting.

**Key characteristics:**
- Highly readable English-like syntax
- Rigid program structure with four divisions
- Excellent for record-oriented data processing
- Fixed-width column layout (though Time Warp Studio is flexible)

## Program Structure

Every COBOL program has up to four divisions:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-PROGRAM.

       ENVIRONMENT DIVISION.
       (optional — file and platform settings)

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       (variable declarations)

       PROCEDURE DIVISION.
       (executable statements)
```

## Hello World

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Hello, World!"
           STOP RUN.
```

## Variables (DATA DIVISION)

Variables are declared with `PIC` (picture) clauses that define their type and size:

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME         PIC X(20) VALUE "Alice".
       01 WS-AGE          PIC 9(3)  VALUE 30.
       01 WS-SALARY       PIC 9(7)V99 VALUE 55000.00.
       01 WS-FLAG         PIC X(1)  VALUE "Y".
       01 WS-COUNTER      PIC 9(5)  VALUE ZEROS.
       01 WS-MESSAGE      PIC X(50) VALUE SPACES.
```

**Picture symbols:**
- `X` — any character
- `9` — numeric digit
- `V` — implied decimal point
- `A` — alphabetic only
- `S` — sign (negative numbers)
- Number in `()` = repetition count

## Arithmetic Verbs

```cobol
       PROCEDURE DIVISION.
       CALC-PARA.
           MOVE 100 TO WS-COUNTER
           ADD 50 TO WS-COUNTER
           SUBTRACT 20 FROM WS-COUNTER
           MULTIPLY 2 BY WS-COUNTER
           DIVIDE 4 INTO WS-COUNTER
           DISPLAY "Counter: " WS-COUNTER

           COMPUTE WS-COUNTER = (100 + 50 - 20) * 2 / 4
           DISPLAY "Result: " WS-COUNTER
```

## Conditional Logic

```cobol
           IF WS-AGE > 18
               DISPLAY "Adult"
           ELSE
               DISPLAY "Minor"
           END-IF

           EVALUATE WS-FLAG
               WHEN "Y"
                   DISPLAY "Yes"
               WHEN "N"
                   DISPLAY "No"
               WHEN OTHER
                   DISPLAY "Unknown"
           END-EVALUATE
```

## Loops with PERFORM

```cobol
           PERFORM 5 TIMES
               DISPLAY "Hello"
           END-PERFORM

           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 5
               DISPLAY WS-COUNTER
           END-PERFORM

           PERFORM MY-PARAGRAPH 3 TIMES
           STOP RUN.

       MY-PARAGRAPH.
           DISPLAY "In subroutine: " WS-COUNTER.
```

## String Operations

```cobol
           MOVE "Hello" TO WS-NAME
           STRING "Hello" DELIMITED SIZE
                  ", "    DELIMITED SIZE
                  "World" DELIMITED SIZE
               INTO WS-MESSAGE
           DISPLAY WS-MESSAGE

           INSPECT WS-NAME
               TALLYING WS-COUNTER FOR ALL "l"
           DISPLAY "Count of l: " WS-COUNTER
```

## Tables (Arrays)

```cobol
       01 WS-TABLE.
          05 WS-ITEM OCCURS 5 TIMES PIC 9(3).

       PROCEDURE DIVISION.
           MOVE 10 TO WS-ITEM(1)
           MOVE 20 TO WS-ITEM(2)
           MOVE 30 TO WS-ITEM(3)
           DISPLAY WS-ITEM(1)    "  " WS-ITEM(2)
```

## Complete Example: Grade Calculator

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADE-CALC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SCORE    PIC 9(3) VALUE 85.
       01 WS-GRADE    PIC X(2) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PARA.
           EVALUATE TRUE
               WHEN WS-SCORE >= 90
                   MOVE "A" TO WS-GRADE
               WHEN WS-SCORE >= 80
                   MOVE "B" TO WS-GRADE
               WHEN WS-SCORE >= 70
                   MOVE "C" TO WS-GRADE
               WHEN WS-SCORE >= 60
                   MOVE "D" TO WS-GRADE
               WHEN OTHER
                   MOVE "F" TO WS-GRADE
           END-EVALUATE
           DISPLAY "Score: " WS-SCORE " Grade: " WS-GRADE
           STOP RUN.
```

## Quick Reference

| Statement | Purpose |
|-----------|---------|
| `DISPLAY expr` | Print output |
| `MOVE val TO var` | Assignment |
| `ADD x TO y` | y = y + x |
| `SUBTRACT x FROM y` | y = y - x |
| `MULTIPLY x BY y` | y = y * x |
| `DIVIDE x INTO y` | y = y / x |
| `COMPUTE y = expr` | Expression |
| `IF cond ... END-IF` | Conditional |
| `EVALUATE ... END-EVALUATE` | Switch |
| `PERFORM n TIMES` | Loop |
| `PERFORM VARYING ... UNTIL` | Counted loop |
| `STOP RUN` | End program |
