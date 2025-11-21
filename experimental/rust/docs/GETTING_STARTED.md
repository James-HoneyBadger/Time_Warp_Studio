# Getting Started with Time Warp IDE

Welcome to Time Warp IDE! This guide will help you write your very first programs in just a few minutes.

---

## Installation and Launch

1. **Install Rust** (if not already installed):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

2. **Get Time Warp**:
   ```bash
   git clone https://github.com/James-HoneyBadger/Time_Warp.git
   cd Time_Warp
   ```

3. **Run the IDE**:
   ```bash
   cargo run
   ```

The Time Warp IDE window will open!

---

## Your First 5 Minutes

### Understanding the Interface

When you open Time Warp IDE, you'll see:

- **Editor Tab**: Where you write your code
- **Output Tab**: Where results appear
- **Canvas Tab**: Where graphics are drawn
- **Run Button (▶️)**: Executes your program

---

## Your First Program in 3 Languages

### 1️⃣ PILOT: "Hello, World!"

**What you'll learn**: Text output, the simplest program possible

**Type this in the Editor**:
```pilot
T:Hello, World!
T:Welcome to programming!
E:
```

**Click Run (▶️)**

You'll see your messages in the Output tab!

**What it does**:
- `T:` displays text
- `E:` ends the program

**Try changing it**: Modify the text to say whatever you want!

---

### 2️⃣ BASIC: Your First Calculation

**What you'll learn**: Variables and math

**Type this**:
```basic
10 PRINT "My Calculator"
20 LET A = 10
30 LET B = 5
40 PRINT A, "+", B, "=", A+B
50 PRINT A, "*", B, "=", A*B
60 END
```

**Click Run**

You'll see math results!

**What it does**:
- `PRINT` displays text and numbers
- `LET` creates variables
- Numbers are expressions you can calculate
- `END` finishes the program

**Try changing it**: Use different numbers for A and B!

---

### 3️⃣ Logo: Draw a Square

**What you'll learn**: Graphics with the turtle

**Type this**:
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

**Click Run, then check the Canvas tab**

You'll see a square!

**What it does**:
- `FORWARD` moves the turtle forward
- `RIGHT` turns the turtle clockwise
- The turtle draws as it moves

**Try changing it**: 
- Change 100 to 50 (smaller square)
- Change 90 to 60 (what shape?)

---

## Your First Interactive Program

Now let's make a program that talks to you!

**Type this in PILOT**:
```pilot
T:What is your name?
A:NAME
T:Hello, *NAME*!
T:Nice to meet you!
E:
```

**Click Run**

You'll see a prompt asking for your name. Type it and press Enter!

**What's new**:
- `A:NAME` asks for input and stores it in NAME
- `*NAME*` puts the value into your text

**Make it better**: Ask for age too:
```pilot
T:What is your name?
A:NAME
T:How old are you?
A:AGE
T:Hello, *NAME*! You are *AGE* years old!
E:
```

---

## Making Decisions

Programs can make choices! Try this:

```pilot
T:What's your favorite color?
A:COLOR
M:RED
Y:
T:Red is bold and strong!
N:
M:BLUE
Y:
T:Blue is calm and peaceful!
N:
T:*COLOR* is a great choice!
E:
```

**What's new**:
- `M:RED` checks if the input matches "RED"
- `Y:` means "yes, it matched" - do next T:
- `N:` means "no, it didn't match" - do next T:

---

## Drawing with Shortcuts

Logo has a shortcut to make shapes easier:

```logo
REPEAT 4 [FORWARD 100 RIGHT 90]
```

This draws the same square but in ONE line!

**What it does**:
- `REPEAT 4` does something 4 times
- `[...]` contains the commands to repeat

**Try this**:
```logo
REPEAT 3 [FORWARD 100 RIGHT 120]
```
What shape is that?

---

## Adding Color

Make your drawings colorful:

```logo
SETCOLOR RED
REPEAT 4 [FORWARD 100 RIGHT 90]

SETCOLOR BLUE
REPEAT 3 [FORWARD 100 RIGHT 120]
```

**Colors available**:
RED, BLUE, GREEN, YELLOW, ORANGE, PURPLE, PINK, BROWN, BLACK, WHITE, GRAY, CYAN, MAGENTA

Or use hex codes like `SETCOLOR #FF69B4` for custom colors!

---

## Your First Game

Let's make a number guessing game in BASIC:

```basic
10 PRINT "I'm thinking of a number 1-10"
20 LET SECRET = int(rand()*10)+1
30 PRINT "Guess the number:"
40 INPUT GUESS
50 IF GUESS = SECRET THEN 100
60 IF GUESS < SECRET THEN 80
70 PRINT "Too high! Try again:"
75 GOTO 40
80 PRINT "Too low! Try again:"
85 GOTO 40
100 PRINT "Correct! The number was", SECRET
110 END
```

**What's new**:
- `rand()` generates a random number
- `int()` makes it a whole number
- `INPUT` gets a number from the user
- `IF...THEN` makes decisions
- `GOTO` jumps to a line number

---

## Next Steps

### Explore the Examples
Open the `examples/` folder and try:
- `examples/pilot_quiz.pilot` - Interactive quiz
- `examples/basic_graphics.bas` - More drawing
- `examples/logo_spirograph.logo` - Beautiful pattern

### Learn More
Read these guides:
- `docs/STUDENT_GUIDE.md` - Full language reference
- `docs/QUICK_REFERENCE.md` - Quick command lookup
- `docs/PROGRAMMING_CHALLENGES.md` - Practice problems

### Create Something!

Try making:
1. **A quiz about yourself** (PILOT)
2. **A calculator for your homework** (BASIC)
3. **A picture or pattern** (Logo)
4. **A simple game** (any language!)

---

## Quick Tips

### When Something Goes Wrong

**Program does nothing?**
- PILOT: Check you have `E:` at the end
- BASIC: Check you have `END`
- Logo: Check your commands are spelled correctly

**Error message?**
- Read the error carefully
- Check spelling of commands
- Look for typos in variable names

**Graphics don't appear?**
- Click the Canvas tab
- In Logo, make sure pen is down (`PENDOWN`)

### Saving Your Work

1. **File → Save** or **Ctrl+S** (Windows/Linux) / **Cmd+S** (Mac)
2. Choose a name ending in:
   - `.pilot` for PILOT programs
   - `.bas` for BASIC programs
   - `.logo` for Logo programs

### Saving Graphics

1. Run your Logo program
2. Go to Canvas tab
3. **View → Save Canvas as PNG**
4. Choose where to save it

---

## Common Questions

**Q: Which language should I use?**
- **PILOT** - For text, stories, quizzes
- **BASIC** - For math, calculations, games
- **Logo** - For drawing and art

**Q: Can I use multiple languages in one project?**
- Not in the same file, but you can make separate programs!

**Q: How do I get better at programming?**
1. Practice regularly (even 10 minutes a day!)
2. Try all the examples
3. Modify examples to make them your own
4. Do the challenges in `PROGRAMMING_CHALLENGES.md`
5. Create your own projects

**Q: I'm stuck. Where can I get help?**
1. Check `QUICK_REFERENCE.md` for command syntax
2. Look at example programs similar to what you want
3. Read error messages carefully - they often tell you the problem
4. Try breaking your problem into smaller pieces
5. Ask a teacher, friend, or online community

---

## Your First Week Challenge

By the end of your first week, try to create:

- ✅ Day 1: "Hello World" in all three languages
- ✅ Day 2: A program that asks your name and age
- ✅ Day 3: A program that does a calculation
- ✅ Day 4: A program that draws a shape
- ✅ Day 5: A quiz with 3 questions
- ✅ Day 6: A number guessing game
- ✅ Day 7: Your own creative project!

---

## Encouragement

**Remember**:
- 🎯 Every programmer started as a beginner
- 💪 Mistakes are how you learn
- 🚀 Start small and build up
- 🎨 Be creative - there's no "wrong" program!
- 🌟 Celebrate your progress, no matter how small

**You're now a programmer!** Keep experimenting, keep learning, and most importantly - have fun!

---

**Ready to dive deeper?** Check out:
- `docs/STUDENT_GUIDE.md` - Complete language guides
- `examples/README.md` - All example programs organized by difficulty
- `docs/PROGRAMMING_CHALLENGES.md` - Test your skills

Happy coding! 🎉👨‍💻👩‍💻
