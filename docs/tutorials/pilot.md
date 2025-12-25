# PILOT Tutorial

**PILOT** (Programmed Inquiry, Learning, Or Teaching) is a pattern-matching language designed for creating educational programs and interactive learning systems.

## Getting Started

### Hello, World! 👋

```pilot
T: Hello, World!
```

**Output:**
```
Hello, World!
```

`T:` means "Type" (print text)

## Basic Commands

### Printing Output (T: - TYPE)

```pilot
T: Welcome to PILOT
T: This is line 1
T: This is line 2
```

**Output:**
```
Welcome to PILOT
This is line 1
This is line 2
```

### Accepting Input (A: - ACCEPT)

```pilot
A: What is your name?$
T: Hello, $!
```

The `$` variable stores the user's input.

**Multiple prompts:**
```pilot
A: First name:$F
A: Last name:$L
T: Hello, $F $L
```

- `$F` stores first answer
- `$L` stores last answer
- `$` alone stores most recent answer

## Variables and Counting

### Simple Variables

```pilot
M: *X = 5
T: X is *X
```

`M:` means "Mark" (set a variable)
`*X` means the value of variable X

**Output:**
```
X is 5
```

### Arithmetic

```pilot
M: *COUNT = 0
M: *COUNT = *COUNT + 1
M: *COUNT = *COUNT + 1
T: Count is *COUNT
```

**Output:**
```
Count is 2
```

### Common Operations

```pilot
M: *X = 10
M: *Y = *X + 5      ; Addition
M: *Z = *X - 3      ; Subtraction
M: *W = *X * 2      ; Multiplication
M: *V = *X / 2      ; Division
```

## Pattern Matching (C: - COMPARE)

PILOT's signature feature - match what user types:

```pilot
A: Is the sky blue?$
C: *$ = blue
Y: Yes, the sky is blue!
N: That's not quite right...
```

- `C:` = Compare (pattern match)
- `Y:` = If match succeeds (Yes)
- `N:` = If match fails (No)

### Matching Examples

**Exact match:**
```pilot
A: What is 2+2?$
C: *$ = 4
Y: Correct!
N: Try again
```

**Pattern with wildcard:**
```pilot
A: Name a color:$
C: *$ = red
Y: Red is a nice color
N: That's a different color
```

**Matching numbers:**
```pilot
A: Enter a number:$
C: *$ = 5
Y: You picked 5
N: That wasn't 5
```

## Conditionals

### Branching with J: (JUMP)

```pilot
T: Pick a number 1-3
A: Your choice:$
C: *$ = 1
J: ONE
C: *$ = 2
J: TWO
C: *$ = 3
J: THREE
J: INVALID

*ONE
T: You picked ONE!
J: END

*TWO
T: You picked TWO!
J: END

*THREE
T: You picked THREE!
J: END

*INVALID
T: Invalid choice!

*END
T: Thank you!
```

- `*LABEL` = Mark a location
- `J: LABEL` = Jump to that location

### Conditional with Numeric Comparison

```pilot
A: Enter your age:$
M: *AGE = *$
C: *AGE >= 18
Y: You are an adult
N: You are a minor
```

## Loops

### Counting Loop

```pilot
M: *I = 1
*LOOP
C: *I <= 5
Y: *
N: J: END

T: I is *I
M: *I = *I + 1
J: LOOP

*END
T: Finished!
```

**Output:**
```
I is 1
I is 2
I is 3
I is 4
I is 5
Finished!
```

### Repeating Until Match

```pilot
*GUESS
A: Guess my number:$
C: *$ = 7
Y: That's it!
N: Try again
N: J: GUESS
```

This repeats until user enters 7.

## Interactive Quiz

```pilot
T: GENERAL KNOWLEDGE QUIZ
T: ---

M: *SCORE = 0

T: Q1: What is the capital of France?
A: Your answer:$
C: *$ = paris
Y: Correct!
M: *SCORE = *SCORE + 1
N: The answer is Paris
N:
T: Q2: What planet is closest to the sun?
A: Your answer:$
C: *$ = mercury
Y: Correct!
M: *SCORE = *SCORE + 1
N: The answer is Mercury
N:
T: ---
T: Your score: *SCORE / 2
```

## String Operations

### Concatenation

```pilot
A: First name:$F
A: Last name:$L
M: *FULL = *F * *L
T: Full name: *FULL
```

**Output:**
```
Full name: JohnDoe
```

### String Matching

```pilot
A: Do you like pizza? (yes/no)$
C: *$ = yes
Y: Great! Pizza is delicious
N: Pizza is pretty tasty though
```

## Common Commands Reference

### Output
| Command | Format | Purpose |
|---------|--------|---------|
| `T:` | `T: text` | Type (print) text |
| `T:` | `T: Value is *X` | Print with variable |

### Input
| Command | Format | Purpose |
|---------|--------|---------|
| `A:` | `A: Prompt$` | Accept input into `$` |
| `A:` | `A: Prompt$VAR` | Accept into `$VAR` |

### Variables
| Command | Format | Purpose |
|---------|--------|---------|
| `M:` | `M: *X = 5` | Mark (set variable) |
| `M:` | `M: *X = *Y + 3` | Arithmetic |

### Control
| Command | Format | Purpose |
|---------|--------|---------|
| `C:` | `C: *$ = answer` | Compare (match) |
| `Y:` | `Y: success action` | If match succeeds |
| `N:` | `N: failure action` | If match fails |
| `J:` | `J: LABEL` | Jump to label |
| `*LABEL` | `*LABELNAME` | Define jump target |

### Special
| Command | Format | Purpose |
|---------|--------|---------|
| `;` | `T: text ; comment` | Comment |
| `E:` | `E:` | End program |

## Complete Example: Number Guessing Game

```pilot
T: Welcome to Number Guessing!
T: I'm thinking of a number 1-10
M: *SECRET = 7
M: *TRIES = 0

*GUESS
T:
A: Your guess:$
M: *GUESS = *$
M: *TRIES = *TRIES + 1

C: *GUESS = *SECRET
Y: J: WIN

C: *GUESS < *SECRET
Y: T: Too low! Try again
Y: J: GUESS

T: Too high! Try again
J: GUESS

*WIN
T: You got it!
T: It took you *TRIES tries
T: The number was *SECRET
```

## Complete Example: Quiz Game

```pilot
T: === CAPITALS QUIZ ===
T:
M: *CORRECT = 0
M: *TOTAL = 0

T: Question 1: What is the capital of France?
A: Your answer:$
M: *TOTAL = *TOTAL + 1
C: *$ = paris
Y: T: Correct!
Y: M: *CORRECT = *CORRECT + 1
N: T: Incorrect. The answer is Paris

T:
T: Question 2: What is the capital of Japan?
A: Your answer:$
M: *TOTAL = *TOTAL + 1
C: *$ = tokyo
Y: T: Correct!
Y: M: *CORRECT = *CORRECT + 1
N: T: Incorrect. The answer is Tokyo

T:
T: === RESULTS ===
T: You got *CORRECT out of *TOTAL correct
M: *PERCENT = *CORRECT * 100 / *TOTAL
T: That's *PERCENT percent!
```

## Complete Example: Menu System

```pilot
*START
T: MAIN MENU
T: 1) Start Game
T: 2) Instructions
T: 3) Quit
A: Choose option:$
C: *$ = 1
J: GAME
C: *$ = 2
J: INSTRUCTIONS
C: *$ = 3
J: QUIT
T: Invalid choice!
J: START

*INSTRUCTIONS
T:
T: This is a learning game
T: Answer the questions correctly
T: Try to get 100%!
T:
J: START

*GAME
T:
T: Question 1: What is 2+2?
A: Answer:$
C: *$ = 4
Y: T: Correct!
N: T: Incorrect!
T:
J: START

*QUIT
T: Goodbye!
E:
```

## Tips for Learning PILOT

1. **Start with T: and A:** - Print and input are key
2. **Use pattern matching** - C: is PILOT's special feature
3. **Practice jumps** - J: and labels organize flow
4. **Track score** - Variables and marking are useful
5. **Build interactivity** - PILOT excels at dialogues

## Common Mistakes

| Mistake | Problem | Fix |
|---------|---------|-----|
| Forgetting `$` | Variable not initialized | `A: Prompt$` |
| Wrong variable name | Variable undefined | `M: *CORRECT = 0` |
| Bad label name | Jump fails silently | Check label spelling |
| No `:` after command | Syntax error | `T: text` not `T text` |
| Infinite loop | Program hangs | Use exit condition with `E:` |

## Comparison to BASIC

| Feature | BASIC | PILOT |
|---------|-------|-------|
| Output | `PRINT` | `T:` |
| Input | `INPUT X` | `A: prompt$X` |
| Conditionals | `IF/THEN/ELSE` | `C: /Y:/N:` |
| Loops | `FOR`, `WHILE` | Labels + `J:` |
| Strength | General math | Educational dialogues |

## Next Steps

1. ✅ Complete tutorials above
2. 📂 Try examples from `Examples/pilot/`
3. 🎮 Build an interactive quiz
4. 🧠 Create a learning program

---

Happy learning! 📚
