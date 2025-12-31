# PILOT Language Tutorial

PILOT (Programmed Inquiry Learning Or Teaching) is an interactive teaching language designed for educational environments. It excels at creating interactive learning experiences with branching logic and user engagement.

## Quick Start

PILOT uses a simple command structure:

```pilot
T: Welcome to PILOT!
A: What is your name?
A:N
T: Hello %N$!
```

- `T:` = Type (output) text
- `A:` = Accept (input) text
- `A:N` = Accept and assign to variable N
- `%variable$` = Insert variable value

## Basic Commands

### Output: T (Type)

Output text to the screen:

```pilot
T: This is output text
T: PILOT is simple and fun!
```

Multiple lines:

```pilot
T: Line 1
T: Line 2
T: Line 3
```

### Input: A (Accept)

Get user input:

```pilot
A: Enter your name
```

Store input in a variable (single letter, A-Z):

```pilot
A: What is your age?
A:X
```

The input is now stored in variable X.

### Variables

Variables in PILOT are single letters (A-Z). Reference them with `%X$`:

```pilot
A: What is your name?
A:N
T: Your name is %N$
```

Variables are case-sensitive. Each letter represents a different variable.

### Conditional Branching

PILOT supports conditional statements using `*`:

```pilot
A: Are you a student? (Y/N)
A:S

*S: Y
T: Great! You're a student.

*S: N
T: Welcome! You're not a student.

*S: *
T: Please answer Y or N.
```

The `*` operator checks if a variable equals a value:
- `*X: value` - If X equals value, continue
- `*X: *` - If X is anything else (wildcard)

### Jumps: J (Jump)

Jump to a label:

```pilot
J: ENDGAME
```

Define labels with parentheses:

```pilot
(ENDGAME)
T: Game Over!
```

Jump with conditions:

```pilot
A: Continue? (Y/N)
A:C

*C: Y
J: START

*C: N
J: END
```

### Loops: E (End)

The `E:` command ends the current block. Use with conditions:

```pilot
(LOOP)
A: Enter a number or 'QUIT' to exit
A:I

*I: QUIT
J: END

T: You entered %I$
J: LOOP

(END)
T: Goodbye!
```

## Complete Example Programs

### Interactive Quiz

```pilot
T: Welcome to the PILOT Quiz!
T: 
T: Question 1: What is 2+2?
A:Q1

*Q1: 4
T: Correct!
J: Q2

*Q1: *
T: Sorry, that's incorrect. The answer is 4.

(Q2)
T: Question 2: What is the capital of France?
A:Q2

*Q2: Paris
T: Correct!
J: SCORE

*Q2: *
T: Sorry, the answer is Paris.

(SCORE)
T: Thanks for taking the quiz!
```

### Number Guessing Game

```pilot
T: I'm thinking of a number between 1 and 100.
T: Can you guess what it is?

(GUESS)
T: 
A: Enter your guess:
A:G

*G: 42
T: You got it! The answer was 42!
J: END

*G: *
T: Try again. Guess another number.
J: GUESS

(END)
T: Thanks for playing!
```

### Personal Information Collector

```pilot
T: Welcome! Let's collect your information.
T:
A: What is your first name?
A:F

A: What is your last name?
A:L

A: How old are you?
A:A

A: What is your favorite color?
A:C

T:
T: Summary of Information:
T: Name: %F$ %L$
T: Age: %A$
T: Favorite Color: %C$

A: Is this information correct? (Y/N)
A:OK

*OK: Y
T: Thank you! Information saved.
J: END

*OK: N
J: WELCOME

*OK: *
T: Please answer Y or N.
J: WELCOME

(END)
T: Goodbye, %F$!
```

### Menu System

```pilot
(MENU)
T: ===== Main Menu =====
T: A) View Profile
T: B) Change Settings
T: C) Play Game
T: D) Exit

A: Choose an option:
A:M

*M: A
J: PROFILE

*M: B
J: SETTINGS

*M: C
J: GAME

*M: D
J: EXIT

*M: *
T: Invalid option. Please try again.
J: MENU

(PROFILE)
T: Your Profile Information
A: What is your name?
A:N
T: Hello, %N$!
J: MENU

(SETTINGS)
T: Settings page
T: (Not implemented yet)
J: MENU

(GAME)
T: Game page
T: (Not implemented yet)
J: MENU

(EXIT)
T: Thank you for using this program!
```

## Tips and Best Practices

1. **Variable Names**: Use meaningful letters (N for Name, A for Age, etc.)

2. **User-Friendly**: Always provide clear prompts:
   ```pilot
   A: Enter your answer: 
   A:X
   ```

3. **Input Validation**: Check user input and prompt for retry:
   ```pilot
   (RETRY)
   A: Please enter Y or N:
   A:R
   *R: Y
   *R: N
   *R: *
   T: Invalid response. Try again.
   J: RETRY
   ```

4. **Comments**: While PILOT doesn't have built-in comments, you can label sections:
   ```pilot
   (SECTION_START)
   T: This section handles...
   ```

5. **Readability**: Use consistent indentation and spacing in your code

## Common Patterns

### Looping Until Valid Input

```pilot
(GETINPUT)
A: Enter Y or N:
A:X

*X: Y
*X: N
*X: *
T: Invalid! Try again.
J: GETINPUT
```

### Counting

Since PILOT variables are single letters, counting is limited:

```pilot
A: Count from 1 to 3
A:C

*C: 1
T: One
*C: 2
T: Two
*C: 3
T: Three
*C: *
T: Invalid number
```

### Nested Conditions

```pilot
A: Are you a student?
A:S

*S: Y
A: What grade are you in?
A:G

*G: 1
T: First grade!

*G: 2
T: Second grade!

*G: *
T: Other grade.

*S: N
T: You're not a student.
```

## Limitations and Workarounds

1. **Single-Letter Variables**: PILOT only supports A-Z variables (26 total)
2. **No Arithmetic**: PILOT cannot perform math operations
3. **No Loops with Counter**: Use jump-based loops instead

## Troubleshooting

**Issue**: "Label not found" error
- **Solution**: Check that labels are spelled correctly and defined with parentheses: `(LABEL)`

**Issue**: Infinite loop
- **Solution**: Make sure your jumps eventually reach a break point or end

**Issue**: Variables not retaining values
- **Solution**: Remember variable names are case-sensitive. Use the same letter consistently.

**Issue**: Input not being stored
- **Solution**: Use `A:X` format to assign input to variable X. Just `A:` won't store the value.

## Advanced Techniques

### State Machine Pattern

```pilot
(STATE_MENU)
T: In menu
A: Next?
A:I
*I: 1
J: STATE_GAME
*I: 0
J: STATE_END

(STATE_GAME)
T: In game
A: Next?
A:I
*I: 1
J: STATE_MENU
*I: 0
J: STATE_END

(STATE_END)
T: Ending
```

## Running PILOT Programs in Time Warp IDE

1. Create a `.pilot` file with your program
2. Select PILOT from the language dropdown
3. Paste your code or load the file
4. Click Run or press Ctrl+R
5. Interact with the program in the Output panel

## Learning Resources

- **Practice**: Start with the quiz example above
- **Experiment**: Modify examples to understand each command
- **Build**: Create your own interactive lesson or game
- **Test**: Always test user input validation

## Next Steps

- Learn [Logo for turtle graphics](logo.md)
- Explore [BASIC for math and algorithms](basic.md)
- Check out [example programs](../Examples/) in the repository

Happy PILOT programming!
