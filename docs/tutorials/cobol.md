# COBOL Programming Tutorial

COBOL (Common Business-Oriented Language) was created in 1959, principally by Grace Hopper's team. It remains critical infrastructure — trillions of dollars of financial transactions run on COBOL every day.

## Program Structure

Every COBOL program has four **DIVISION**s:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       AUTHOR. TIME-WARP-STUDIO.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       PROCEDURE DIVISION.
           DISPLAY "Hello from COBOL!"
           DISPLAY "Welcome to Time Warp Studio"
           STOP RUN.
```

## Data Division

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATA-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER         PIC 9(5)       VALUE 12345.
       01 WS-PRICE          PIC 9(5)V99    VALUE 99.95.
       01 WS-NAME           PIC X(20)      VALUE "Alice".
       01 WS-COUNTER        PIC 9(3)       VALUE ZERO.
       01 WS-TOTAL          PIC 9(7)V99    VALUE ZERO.

       PROCEDURE DIVISION.
           DISPLAY WS-NUMBER
           DISPLAY WS-PRICE
           DISPLAY WS-NAME
           STOP RUN.
```

### Picture Clauses (PIC)

| PIC | Meaning |
|-----|---------|
| `9` | Numeric digit |
| `X` | Any character |
| `A` | Alphabetic character |
| `V` | Implied decimal point |
| `S` | Sign |
| `Z` | Zero-suppress |

## Arithmetic

```cobol
       PROCEDURE DIVISION.
           COMPUTE WS-TOTAL = WS-PRICE * 100 / 100
           ADD 1 TO WS-COUNTER
           SUBTRACT 5 FROM WS-NUMBER
           MULTIPLY 2 BY WS-COUNTER
           DIVIDE WS-NUMBER BY 3 GIVING WS-RESULT
           DISPLAY "Result: " WS-RESULT
           STOP RUN.
```

## Conditionals

```cobol
       PROCEDURE DIVISION.
           MOVE 85 TO WS-SCORE
           IF WS-SCORE >= 90
               DISPLAY "Grade A"
           ELSE IF WS-SCORE >= 80
               DISPLAY "Grade B"
           ELSE
               DISPLAY "Below B"
           END-IF

           EVALUATE WS-DAY
               WHEN "MON"
                   DISPLAY "Monday"
               WHEN "FRI"
                   DISPLAY "Friday"
               WHEN "SAT" "SUN"
                   DISPLAY "Weekend"
               WHEN OTHER
                   DISPLAY "Midweek"
           END-EVALUATE
           STOP RUN.
```

## PERFORM (Loops and Subroutines)

```cobol
       PROCEDURE DIVISION.
           PERFORM PRINT-HEADER

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > 10
               DISPLAY WS-IDX
           END-PERFORM

           STOP RUN.

       PRINT-HEADER.
           DISPLAY "=== Time Warp COBOL ==="
           DISPLAY "======================="
```

## Tables (Arrays)

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FRUITS.
          05 WS-FRUIT       PIC X(15) OCCURS 5 TIMES.
       01 WS-IDX            PIC 9(2).

       PROCEDURE DIVISION.
           MOVE "Apple"  TO WS-FRUIT(1)
           MOVE "Banana" TO WS-FRUIT(2)
           MOVE "Cherry" TO WS-FRUIT(3)
           MOVE "Date"   TO WS-FRUIT(4)
           MOVE "Fig"    TO WS-FRUIT(5)

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > 5
               DISPLAY WS-IDX ": " WS-FRUIT(WS-IDX)
           END-PERFORM
           STOP RUN.
```

## String Manipulation

```cobol
       PROCEDURE DIVISION.
           MOVE "Hello, World!" TO WS-STRING
           INSPECT WS-STRING
               TALLYING WS-COUNT FOR ALL "l"
           DISPLAY "Count of 'l': " WS-COUNT

           STRING "Hello" DELIMITED SPACE
                  ", "    DELIMITED SIZE
                  "World" DELIMITED SPACE
               INTO WS-RESULT
           DISPLAY WS-RESULT
           STOP RUN.
```

## Further Reading

- [Examples/cobol/](../Examples/cobol/) — 10 COBOL example programs
- [Language Guide: COBOL](LANGUAGE_GUIDE.md#cobol)
