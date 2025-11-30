# Student Workbook: Learning to Code with Time Warp IDE

Your journey into the exciting world of programming starts here!

---

## Welcome, Future Programmer!

Programming is like learning a new language—a language that lets you talk to computers and make them do amazing things. With Time Warp IDE, you'll learn six different programming languages and discover your favorites.

### What You'll Learn

- How to write programs that solve problems
- Creating colorful graphics and patterns
- Building interactive games and quizzes
- Thinking like a programmer
- How computers understand instructions

### How to Use This Workbook

- **Read** each lesson carefully
- **Type** the examples yourself (don't copy-paste!)
- **Experiment** with changes to see what happens
- **Complete** the exercises
- **Have fun** and be creative!

---

## Unit 1: Your First Programs (BASIC)

### Lesson 1: Hello, World!

Every programmer starts with a "Hello, World!" program. Here's yours:

```basic
10 PRINT "Hello, World!"
20 END
```

**Try It!**
1. Open Time Warp IDE
2. Select **BASIC** from the Language menu
3. Type the program exactly as shown
4. Press **F5** to run

**What Happened?**
The computer displayed your message! Let's understand each part:

- `10` - Line number (helps organize your program)
- `PRINT` - Command to display something
- `"Hello, World!"` - The text to display (in quotes)
- `20 END` - Tells the computer the program is done

**Your Turn!**

Modify the program to:
1. Print your name
2. Print your favorite color
3. Print your age

Example:
```basic
10 PRINT "My name is Alex"
20 PRINT "My favorite color is blue"
30 PRINT "I am 12 years old"
40 END
```

**Challenge:** Can you make it print 10 lines? 20 lines?

### Lesson 2: Talking to the Computer

Computers can remember information using **variables**.

```basic
10 LET NAME$ = "Alex"
20 PRINT "Hello, "; NAME$
30 END
```

**Key Concepts:**
- `LET` assigns a value to a variable
- `NAME$` is a variable that stores text (the $ means text)
- `;` connects pieces of output

**Your Turn!**

Write a program that:
1. Stores your name in a variable
2. Stores your age in a variable
3. Prints "My name is [name] and I am [age] years old"

<details>
<summary>Click here if you need help</summary>

```basic
10 LET NAME$ = "Your Name"
20 LET AGE = 12
30 PRINT "My name is "; NAME$; " and I am "; AGE; " years old"
40 END
```
</details>

**Challenge:** Add more information about yourself (favorite food, hobby, etc.)

### Lesson 3: Computer Math

Computers are great at math!

```basic
10 LET X = 5
20 LET Y = 3
30 LET SUM = X + Y
40 PRINT "5 + 3 = "; SUM
50 END
```

**Math Operators:**
- `+` Addition
- `-` Subtraction
- `*` Multiplication
- `/` Division
- `MOD` Remainder

**Your Turn!**

Write a program that:
1. Calculates 7 × 8
2. Calculates 100 ÷ 4
3. Shows what's left when you divide 17 by 5

**Challenge:** Build a calculator!
```basic
10 INPUT "First number: "; A
20 INPUT "Second number: "; B
30 PRINT "Sum: "; A + B
40 PRINT "Difference: "; A - B
50 PRINT "Product: "; A * B
60 PRINT "Quotient: "; A / B
70 END
```

### Lesson 4: Making Decisions

Programs can make choices with `IF` statements:

```basic
10 INPUT "How old are you? "; AGE
20 IF AGE >= 13 THEN PRINT "You're a teenager!"
30 IF AGE < 13 THEN PRINT "You're not a teenager yet"
40 END
```

**Comparison Operators:**
- `=` Equal to
- `>` Greater than
- `<` Less than
- `>=` Greater than or equal to
- `<=` Less than or equal to
- `<>` Not equal to

**Your Turn!**

Write a program that:
1. Asks for a number
2. Says whether it's positive, negative, or zero

**Challenge:** Number guessing game!
```basic
10 LET SECRET = 42
20 INPUT "Guess my number (1-100): "; GUESS
30 IF GUESS = SECRET THEN PRINT "You got it!" : END
40 IF GUESS < SECRET THEN PRINT "Too low!"
50 IF GUESS > SECRET THEN PRINT "Too high!"
60 GOTO 20
```

### Lesson 5: Repeating Actions (Loops)

Instead of writing the same thing many times, use loops!

```basic
10 FOR I = 1 TO 5
20   PRINT "This is line "; I
30 NEXT I
40 END
```

Output:
```
This is line 1
This is line 2
This is line 3
This is line 4
This is line 5
```

**Your Turn!**

Write a program that:
1. Counts from 1 to 10
2. Counts from 10 down to 1 (hint: use STEP -1)
3. Prints even numbers from 2 to 20

**Challenge:** Times table generator!
```basic
10 INPUT "Which times table? "; N
20 FOR I = 1 TO 12
30   PRINT N; " × "; I; " = "; N * I
40 NEXT I
50 END
```

---

## Unit 2: Drawing with Code (Logo)

### Lesson 6: Meet the Turtle

Logo uses a "turtle" that draws as it moves!

```logo
FORWARD 100
```

**What Happened?**
The turtle moved forward 100 steps and drew a line.

**Basic Commands:**
- `FORWARD` (or `FD`) - Move forward
- `BACK` (or `BK`) - Move backward
- `RIGHT` (or `RT`) - Turn right
- `LEFT` (or `LT`) - Turn left

**Your Turn!**

Make the turtle:
1. Go forward 100 steps
2. Turn right 90 degrees (that's a square corner)
3. Go forward 100 more steps

**Result:** You drew an "L" shape!

### Lesson 7: Shapes Are Easy!

Let's draw a square:

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

**Hmm, that's a lot of typing!** There's a better way:

```logo
REPEAT 4 [FORWARD 100 RIGHT 90]
```

**Much better!** The turtle repeats the instructions in brackets 4 times.

**Your Turn!**

Draw these shapes:
1. A triangle (hint: 3 sides, turn 120°)
2. A pentagon (hint: 5 sides, turn 72°)
3. A hexagon (hint: 6 sides, turn 60°)

**Why those angles?**
- Triangle: 360° ÷ 3 = 120°
- Pentagon: 360° ÷ 5 = 72°
- Hexagon: 360° ÷ 6 = 60°

**Challenge:** Draw an octagon (8 sides)!

### Lesson 8: Colors!

Make your drawings colorful:

```logo
SETPENCOLOR 1     ; Red
FORWARD 100
SETPENCOLOR 2     ; Green
RIGHT 90
FORWARD 100
SETPENCOLOR 3     ; Blue
RIGHT 90
FORWARD 100
```

**Available Colors:**
1. Red
2. Green
3. Blue
4. Yellow
5. Magenta
6. Cyan
7. White
8. Black
9. Orange
10. Purple
... (and more!)

**Your Turn!**

Draw a rainbow square:
1. Each side is a different color
2. Make it as colorful as possible!

**Challenge:** Draw a flower with different colored petals!

### Lesson 9: Teaching the Turtle New Tricks

You can teach the turtle new commands with procedures:

```logo
TO SQUARE
  REPEAT 4 [FORWARD 100 RIGHT 90]
END
```

Now you can just type `SQUARE` and it draws a square!

**Parameters** make procedures flexible:

```logo
TO SQUARE :SIZE
  REPEAT 4 [FORWARD :SIZE RIGHT 90]
END

; Now you can:
SQUARE 50     ; Small square
SQUARE 100    ; Medium square
SQUARE 150    ; Large square
```

**Your Turn!**

Create procedures for:
1. Triangle with size parameter
2. Circle (use `REPEAT 36 [FORWARD 10 RIGHT 10]`)
3. Star

**Challenge:** Create a house using your square and triangle procedures!

---

## Unit 3: Creative Challenges

### Project 1: Spiral Designer

Create amazing spiral patterns:

```logo
TO SPIRAL :SIZE
  IF :SIZE > 200 [STOP]
  FORWARD :SIZE
  RIGHT 90
  SPIRAL :SIZE + 5
END

CLEARSCREEN
SPIRAL 10
```

**Experiment!**
- Change the angle (try 91, 89, 120)
- Change the size increment (+5, +10, +3)
- Add colors that change each step

### Project 2: Interactive Quiz

Build a quiz program:

```basic
10 LET SCORE = 0
20 PRINT "Welcome to the Geography Quiz!"
30 PRINT
40 PRINT "Question 1: What is the capital of France?"
50 INPUT ANSWER$
60 IF ANSWER$ = "Paris" THEN SCORE = SCORE + 1 : PRINT "Correct!"
70 IF ANSWER$ <> "Paris" THEN PRINT "Sorry, it's Paris"
80 PRINT
90 PRINT "Question 2: What is 7 × 8?"
100 INPUT ANSWER
110 IF ANSWER = 56 THEN SCORE = SCORE + 1 : PRINT "Correct!"
120 IF ANSWER <> 56 THEN PRINT "Sorry, it's 56"
130 PRINT
140 PRINT "Your score: "; SCORE; " out of 2"
150 END
```

**Make It Yours!**
- Add more questions
- Try different subjects
- Show percentage score
- Add difficulty levels

### Project 3: Pattern Generator

Create your own turtle art:

```logo
TO FLOWER
  REPEAT 12 [
    SETPENCOLOR RANDOM 16
    REPEAT 4 [FORWARD 100 RIGHT 90]
    RIGHT 30
  ]
END

CLEARSCREEN
FLOWER
```

**Ideas to Try:**
- Spirograph patterns
- Geometric art
- Your name in block letters
- A picture of something you like

---

## Programming Tips

### When You Get Stuck

1. **Read the error message** - It usually tells you what's wrong
2. **Check your spelling** - Computer are picky about exact spelling
3. **Look for typos** - Missing quotes, wrong brackets, etc.
4. **Start over small** - Remove code until it works, then add back
5. **Ask for help** - That's what teachers and classmates are for!

### Becoming a Better Programmer

1. **Practice regularly** - A little bit each day is better than a lot once a week
2. **Type code yourself** - Don't copy-paste, typing helps you learn
3. **Experiment** - Change numbers, try new things, see what happens
4. **Read others' code** - Learn from examples
5. **Have fun!** - Programming should be enjoyable

### Common Mistakes

**Forgetting quotes:**
```basic
PRINT Hello    ❌ Error!
PRINT "Hello"  ✅ Correct!
```

**Wrong brackets:**
```logo
REPEAT 4 FORWARD 100    ❌ Missing brackets
REPEAT 4 [FORWARD 100]  ✅ Correct!
```

**Missing END:**
```basic
10 PRINT "Hello"   ❌ Program never ends
20 END             ✅ Correct!
```

---

## Your Programming Journey

### Beginner (That's You Now!)
✅ Can write simple programs
✅ Understand variables and math
✅ Use IF statements
✅ Create loops
✅ Draw with turtle graphics

### Intermediate (Coming Soon!)
- Write procedures and functions
- Work with lists and arrays
- Build interactive programs
- Create complex graphics
- Debug your own code

### Advanced (Your Goal!)
- Design large programs
- Use multiple languages
- Create original projects
- Help others learn
- Think like a programmer

---

## Bonus Challenges

### Challenge 1: Multiplication Practice
Create a program that quizzes you on times tables.

### Challenge 2: Turtle Race
Draw a racetrack and animate a turtle moving along it.

### Challenge 3: Calculator
Build a calculator that can add, subtract, multiply, and divide.

### Challenge 4: Art Gallery
Create 5 different turtle graphics and display them one after another.

### Challenge 5: Adventure Game
Make a text-based adventure where choices lead to different outcomes.

---

## Vocabulary

**Algorithm**: Step-by-step instructions to solve a problem
**Bug**: An error in a program
**Debug**: Finding and fixing errors
**Loop**: Repeating instructions
**Parameter**: Information you give to a procedure
**Procedure**: A named set of instructions
**Program**: A set of instructions for a computer
**Syntax**: The rules of a programming language
**Variable**: A container that stores information

---

## What's Next?

Congratulations on completing the workbook! You're now a programmer. Keep practicing, stay curious, and most importantly—keep coding!

**Ideas for Continued Learning:**
- Explore the other languages (Pascal, Prolog, C)
- Join a coding club
- Enter programming competitions
- Create programs to help others
- Teach someone else what you've learned

Remember: Every expert programmer started exactly where you are now. Keep going!

---

*Have questions? Check the [User Manual](../user/00-user-manual.md) or ask your teacher!*
