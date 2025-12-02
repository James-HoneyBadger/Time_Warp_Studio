# Student Lesson Book — Time Warp IDE

This lesson book is the student-facing companion to the Teacher's 6-week course. It contains short lessons, exercises, checkpoints, and challenges that follow the Student Workbook and Teacher's lesson plan.

Each lesson is short and self-contained; follow along in Time Warp IDE and type the examples yourself.

---

## Week 1 — Getting Started (BASIC)

Lesson 1: Hello, World!
- Type the program:
```basic
10 PRINT "Hello, World!"
20 END
```
- Run with F5. Save as `hello.bas`

Exercise:
1. Change the message to include your name.
2. Add a second PRINT line that prints your favorite food.

Checkpoint (Show teacher): Program runs and prints both lines.

Lesson 2: Variables & Input

Program example:
```basic
10 INPUT "What is your name?"; NAME$
20 PRINT "Hello, "; NAME$
30 END
```

Exercise:
1. Ask for age and print "You are X years old".

Challenge:
Make a short welcome program that asks 3 questions and prints a paragraph about the student.

---

## Week 2 — Loops and Subroutines (BASIC)

Lesson 3: FOR Loops

Example:
```basic
10 FOR I = 1 TO 10
20   PRINT "Line "; I
30 NEXT I
40 END
```

Exercise:
1. Print even numbers from 2 to 20.

Lesson 4: Subroutines

Example:
```basic
10 GOSUB 100
20 PRINT "Back in main"
30 END
100 PRINT "In subroutine" : RETURN
```

Exercise:
1. Write a program that uses GOSUB to print a header and footer.

Project:
Complete the Guessing Game from the workbook.

---

## Week 3 — Turtle Graphics (Logo)

Lesson 5: Turtle Movement

Try:
```logo
FORWARD 100 RIGHT 90 FORWARD 100 RIGHT 90 FORWARD 100 RIGHT 90 FORWARD 100 RIGHT 90
```

Exercise:
1. Use REPEAT to draw a square.

Lesson 6: Procedures and Parameters

Example:
```logo
TO SQUARE :SIZE
  REPEAT 4 [FORWARD :SIZE RIGHT 90]
END
SQUARE 50
```

Challenge:
Draw a flower using multiple circles/procedures.

---

## Week 4 — Structured Programming (Pascal)

Lesson 7: Pascal Basics

Example:
```pascal
program Hello;
begin
  writeln('Hello from Pascal');
end.
```

Exercise:
1. Create a small Pascal program that reads a number and prints its square.

---

## Week 5 — Logic & Rules (Prolog)

Lesson 8: Facts and Queries

Example:
```prolog
parent(alice, bob).
parent(bob, charlie).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

Exercise:
1. Query `grandparent(alice, charlie).`

---

## Week 6 — Capstone Project

Final Project suggestions:

- Guessing Game with score tracking and replay
- Turtle Art Gallery — multiple procedures and color palettes
- Choose-Your-Own-Adventure — branching story using PILOT or BASIC

Final Deliverables:

1. Source code (file saved with appropriate extension)
2. A short README explaining the program and how to run it
3. One screenshot of the program running (or exported drawing)

Grading checklist (student version):
- Program runs without crashing
- Basic features implemented (as described)
- Code is reasonably organized and commented
- Creative extras included (optional)

---

If you want me to produce printable handouts or a PDF version of this lesson book, I can generate one next and add it to the docs/exports folder.
