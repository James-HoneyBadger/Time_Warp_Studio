# Time Warp IDE - Programming Challenges

A collection of coding challenges to practice and improve your PILOT, BASIC, and Logo skills!

---

## Beginner Challenges

### Challenge 1: Personal Greeting (PILOT)
**Difficulty**: ⭐  
**Skills**: Input, variables, text output

**Goal**: Create a program that asks for:
- The user's name
- Their favorite hobby
- Their age

Then display a personalized message using all three pieces of information.

**Example Output**:
```
Hello Alice! You are 12 years old and you love reading. That's awesome!
```

<details>
<summary>Solution</summary>

```pilot
T:What is your name?
A:NAME
T:What is your favorite hobby?
A:HOBBY
T:How old are you?
A:AGE
T:Hello *NAME*! You are *AGE* years old and you love *HOBBY*. That's awesome!
E:
```
</details>

---

### Challenge 2: Simple Addition Quiz (BASIC)
**Difficulty**: ⭐  
**Skills**: Variables, expressions, conditionals

**Goal**: Create a math quiz that:
1. Generates two random numbers (1-10)
2. Asks the user to add them
3. Checks if the answer is correct
4. Displays appropriate feedback

**Hint**: Use `rand()` to generate random numbers.

<details>
<summary>Solution</summary>

```basic
10 LET A = int(rand()*10)+1
20 LET B = int(rand()*10)+1
30 PRINT "What is", A, "+", B, "?"
40 INPUT ANSWER
50 IF ANSWER = A+B THEN 80
60 PRINT "Sorry, the answer is", A+B
70 GOTO 100
80 PRINT "Correct! Great job!"
100 END
```
</details>

---

### Challenge 3: Draw a Triangle (Logo)
**Difficulty**: ⭐  
**Skills**: Basic movement, loops

**Goal**: Draw an equilateral triangle using REPEAT.

**Requirements**:
- Each side should be 100 units
- Use exactly 3 lines of Logo code
- The turtle should return to starting position and orientation

**Hint**: Exterior angles of a triangle sum to 360°. Each turn is 120°.

<details>
<summary>Solution</summary>

```logo
REPEAT 3 [FORWARD 100 RIGHT 120]
```
</details>

---

## Intermediate Challenges

### Challenge 4: Temperature Converter (PILOT)
**Difficulty**: ⭐⭐  
**Skills**: Variables, expressions, conditionals

**Goal**: Create a temperature converter that:
1. Asks if the user wants to convert from C to F or F to C
2. Gets the temperature value
3. Performs the conversion
4. Displays the result

**Formulas**:
- Celsius to Fahrenheit: F = C * 1.8 + 32
- Fahrenheit to Celsius: C = (F - 32) / 1.8

<details>
<summary>Solution</summary>

```pilot
T:Temperature Converter
T:Convert: 1=Celsius to Fahrenheit, 2=Fahrenheit to Celsius
A:CHOICE
M:1
Y:
J:CTOF
M:2
Y:
J:FTOC
J:END
L:CTOF
T:Enter temperature in Celsius:
A:C
U:F=C*1.8+32
T:*C* Celsius = *F* Fahrenheit
J:END
L:FTOC
T:Enter temperature in Fahrenheit:
A:F
U:C=(F-32)/1.8
T:*F* Fahrenheit = *C* Celsius
L:END
E:
```
</details>

---

### Challenge 5: Fibonacci Sequence (BASIC)
**Difficulty**: ⭐⭐  
**Skills**: Loops, variables, sequences

**Goal**: Generate and display the first N numbers in the Fibonacci sequence.

**Requirements**:
- Ask user how many numbers to generate (max 15)
- Display each number on a new line
- Start with 0, 1

**Example Output** (for N=7):
```
0
1
1
2
3
5
8
```

<details>
<summary>Solution</summary>

```basic
10 PRINT "How many Fibonacci numbers?"
20 INPUT N
30 IF N > 15 THEN 20
40 LET A = 0
50 LET B = 1
60 PRINT A
70 PRINT B
80 FOR I = 3 TO N
90 LET C = A + B
100 PRINT C
110 LET A = B
120 LET B = C
130 NEXT I
140 END
```
</details>

---

### Challenge 6: Colorful Squares (Logo)
**Difficulty**: ⭐⭐  
**Skills**: Procedures, parameters, colors

**Goal**: Create a procedure that draws a square, then use it to create a pattern of squares in different colors and sizes.

**Requirements**:
- Define a SQUARE procedure with a SIZE parameter
- Draw at least 4 squares of different sizes
- Use at least 3 different colors
- Position them so they don't completely overlap

<details>
<summary>Solution</summary>

```logo
TO SQUARE :SIZE
  REPEAT 4 [FORWARD :SIZE RIGHT 90]
END

SETCOLOR RED
SQUARE 40
PENUP
FORWARD 60
PENDOWN
SETCOLOR BLUE
SQUARE 60
PENUP
BACK 120
PENDOWN
SETCOLOR GREEN
SQUARE 80
PENUP
FORWARD 100
RIGHT 90
FORWARD 100
PENDOWN
SETCOLOR YELLOW
SQUARE 50
```
</details>

---

## Advanced Challenges

### Challenge 7: Text-Based Adventure Game (PILOT)
**Difficulty**: ⭐⭐⭐  
**Skills**: Labels, jumps, conditionals, storytelling

**Goal**: Create a small adventure game with:
- At least 3 different locations/rooms
- At least 2 items the player can find
- At least 2 choices that affect the outcome
- Multiple possible endings

**Structure Example**:
```
START → ROOM1 → (find item) → ROOM2 → (choice) → ENDING
```

<details>
<summary>Partial Solution (extend this!)</summary>

```pilot
L:START
T:You wake up in a dark cave. A torch lights your way.
T:Go left or right?
A:DIR
M:LEFT
Y:
J:LEFT_ROOM
M:RIGHT
Y:
J:RIGHT_ROOM
J:START

L:LEFT_ROOM
T:You find a sword! This might be useful.
T:Continue forward? (YES/NO)
A:CONT
M:YES
Y:
J:DRAGON
J:START

L:RIGHT_ROOM
T:A friendly wizard appears!
T:The wizard offers you a magic shield. Accept? (YES/NO)
A:ACCEPT
M:YES
Y:
T:You now have a magic shield!
J:DRAGON
J:START

L:DRAGON
T:A dragon blocks your path!
T:What do you do? (FIGHT/RUN)
A:ACTION
M:FIGHT
Y:
J:FIGHT_DRAGON
M:RUN
Y:
T:You ran away! Game Over.
E:

L:FIGHT_DRAGON
T:You bravely face the dragon!
T:With your items, you defeat it! Victory!
E:
```
</details>

---

### Challenge 8: Prime Number Checker (BASIC)
**Difficulty**: ⭐⭐⭐  
**Skills**: Loops, conditionals, algorithms

**Goal**: Check if a number is prime.

**Requirements**:
- Ask user for a number
- Check if it's prime (only divisible by 1 and itself)
- Display result
- Handle edge cases (1 is not prime, 2 is prime)

**Optimization**: Only check divisors up to √N

<details>
<summary>Solution</summary>

```basic
10 PRINT "Prime Number Checker"
20 PRINT "Enter a number:"
30 INPUT N
40 IF N < 2 THEN 200
50 IF N = 2 THEN 180
60 LET ISPRIME = 1
70 FOR I = 2 TO int(sqrt(N))+1
80 LET REMAINDER = N - int(N/I)*I
90 IF REMAINDER = 0 THEN 120
100 NEXT I
110 GOTO 150
120 LET ISPRIME = 0
130 LET I = int(sqrt(N))+2
140 NEXT I
150 IF ISPRIME = 1 THEN 180
160 PRINT N, "is NOT prime"
170 GOTO 220
180 PRINT N, "is prime!"
190 GOTO 220
200 PRINT N, "is NOT prime (must be >= 2)"
220 END
```
</details>

---

### Challenge 9: Recursive Tree (Logo)
**Difficulty**: ⭐⭐⭐  
**Skills**: Recursion, procedures, graphics

**Goal**: Create a fractal tree using recursion.

**Requirements**:
- Define a TREE procedure with SIZE and DEPTH parameters
- Base case: if DEPTH = 0, just draw a line
- Recursive case: draw a line, split into 2-3 branches, recurse with smaller size
- Use different colors for different branch levels

**Example Structure**:
```
TREE draws a trunk
  Then turns left, draws a smaller TREE
  Then turns right, draws a smaller TREE
```

<details>
<summary>Solution</summary>

```logo
TO TREE :SIZE :DEPTH
  IF :DEPTH = 0 [STOP]
  
  IF :DEPTH > 3 [SETCOLOR #8B4513]
  IF :DEPTH <= 3 [SETCOLOR #228B22]
  
  FORWARD :SIZE
  
  LEFT 30
  TREE :SIZE * 0.7 :DEPTH - 1
  RIGHT 60
  TREE :SIZE * 0.7 :DEPTH - 1
  LEFT 30
  
  BACK :SIZE
END

PENUP
BACK 100
PENDOWN
TREE 80 6
```
</details>

---

## Expert Challenges

### Challenge 10: Maze Game (PILOT)
**Difficulty**: ⭐⭐⭐⭐  
**Skills**: State management, mapping, complex logic

**Goal**: Create a text-based maze where:
- Player can move N, S, E, W
- Some rooms have items or obstacles
- Track player inventory
- Have a goal room that requires specific items to win

**Requirements**:
- At least 6 connected rooms
- 3+ items to collect
- Win condition requires specific item(s)
- Handle invalid moves

---

### Challenge 11: Animated Bouncing Ball (BASIC with Graphics)
**Difficulty**: ⭐⭐⭐⭐  
**Skills**: Physics simulation, loops, graphics

**Goal**: Simulate a bouncing ball using CIRCLE.

**Requirements**:
- Ball starts at top
- Falls due to "gravity" (accelerating downward)
- Bounces when it hits bottom
- Each bounce is lower (energy loss)
- Use LINE to draw ground

**Hint**: Track position (Y) and velocity (VY). Each frame: Y = Y + VY, VY = VY + gravity

---

### Challenge 12: Mandala Generator (Logo)
**Difficulty**: ⭐⭐⭐⭐  
**Skills**: Advanced procedures, nested loops, symmetry

**Goal**: Create a complex mandala pattern using nested procedures and repetition.

**Requirements**:
- At least 3 levels of nested procedures
- Use REPEAT with at least 8-fold symmetry
- Incorporate multiple colors
- Use at least one recursive element

**Tips**:
- Build small reusable pattern pieces
- Combine patterns in circular arrangements
- Experiment with angle divisions (360/n)

<details>
<summary>Example Structure</summary>

```logo
TO PETAL :SIZE
  REPEAT 2 [
    REPEAT 60 [FORWARD :SIZE / 60 RIGHT 3]
    RIGHT 120
  ]
END

TO FLOWER :SIZE
  REPEAT 8 [
    PETAL :SIZE
    RIGHT 45
  ]
END

TO MANDALA :SIZE
  REPEAT 12 [
    SETCOLOR RED
    FLOWER :SIZE
    RIGHT 30
    SETCOLOR BLUE
    FLOWER :SIZE * 0.7
    RIGHT 30
  ]
END

MANDALA 60
```
</details>

---

## Challenge Categories

### By Language
- **PILOT Challenges**: 1, 4, 7, 10
- **BASIC Challenges**: 2, 5, 8, 11
- **Logo Challenges**: 3, 6, 9, 12

### By Skill Focus
- **Input/Output**: 1, 2, 4
- **Loops**: 2, 3, 5, 6
- **Conditionals**: 2, 4, 7, 8
- **Procedures**: 6, 9, 12
- **Graphics**: 3, 6, 9, 11, 12
- **Algorithms**: 5, 8, 9
- **Game Design**: 7, 10, 11

---

## Tips for Success

1. **Start Small**: Get the basic version working first, then add features
2. **Test Frequently**: Run your code after every small change
3. **Use Examples**: Look at the example programs in `examples/` for inspiration
4. **Debugging**: Add PRINT or T: statements to see what's happening
5. **Be Creative**: Extend these challenges with your own ideas!
6. **Ask for Help**: Check the QUICK_REFERENCE.md for syntax help

---

## Create Your Own Challenges!

Once you've completed these challenges, try creating your own:
1. Think of a problem you want to solve
2. Break it into small steps
3. Choose the right language for the job
4. Code and test each step
5. Share your challenge with others!

**Happy Coding!** 🚀
