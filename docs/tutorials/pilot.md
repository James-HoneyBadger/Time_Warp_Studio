# PILOT Language Tutorial

PILOT (Programmed Inquiry Learning Or Teaching) is an interactive teaching language designed for educational environments. It excels at creating interactive learning experiences with branching logic and user engagement.

## Quick Start

PILOT uses a simple command structure:

```pilot
T: Welcome to PILOT!
T: What is your name?
A:$NAME
T: Hello $NAME!
```

- `T:` = Type (output) text
- `A:` = Accept (input) text
- `A:$NAME` = Accept and store the answer in variable NAME
- `$NAME` = Insert the value of a variable
- `*LABEL` = Define a label; `J:LABEL` jumps to it
- `M:` = Match the last answer; `TY:` and `TN:` run only if it matched or did not match

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

Store input in a named variable with `A:$NAME`:

```pilot
T: What is your age?
A:$AGE
T: You are $AGE years old.
```

The answer is stored in variable AGE. A plain `A: text` shows the text as the prompt and stores the answer in `ANSWER$`. `A:$AGE` shows `?` as its prompt, so ask the question with a `T:` line first.

### Variables

Store text with `A:$NAME` or `C:$NAME = text`. Store numbers with `C:X = 5`. Reference any variable with `$NAME`. Variable names are not case-sensitive.

```pilot
T: What is your name?
A:$NAME
T: Your name is $NAME
C: N = 5 + 2
T: N is $N
```

### Conditional Branching

PILOT tests the last answer with `M:`. Commands with a `Y` or `N` suffix then run only if the match succeeded or failed:

```pilot
A: Are you a student? (Y/N)
M: Y,YES
TY: Great! You're a student.
TN: Welcome! You're not a student.
```

- `M:a,b,c` checks the answer against each alternative. The match is case-insensitive and succeeds if the answer contains the text.
- `*` and `?` work as wildcards in `M:` patterns.
- The `Y` and `N` suffix works with any command letter, such as `TY:`, `JY:`, and `JN:`.

### Jumps: J (Jump)

Define a label with `*` at the start of a line, then jump to it with `J:`:

```pilot
J: ENDGAME

*ENDGAME
T: Game Over!
```

Jump only when the last match failed with `JN:` (or only when it succeeded with `JY:`):

```pilot
*START
A: Continue? (Y/N)
M: Y,YES
JN: END
T: Continuing...
J: START

*END
T: Goodbye!
```

### Subroutines: U: and E

`U:*LABEL` calls a subroutine. `E:` returns from a subroutine. Outside a subroutine, `E:` ends the program:

```pilot
U:*GREET
T: Back in the main program.
E:

*GREET
T: Hello from the subroutine!
E:
```

Repeat a step until the answer matches by jumping back with `JN:`:

```pilot
*RETRY
A: What is 2 + 2?
M: 4
JN: RETRY
T: Correct!
```

## Complete Example Programs

### Interactive Quiz

```pilot
T: Welcome to the PILOT Quiz!
T: 
T: Question 1: What is 2+2?
A:
M: 4
TY: Correct!
TN: Sorry, the answer is 4.

T: Question 2: What is the capital of France?
A:
M: PARIS
TY: Correct!
TN: Sorry, the answer is Paris.

T: Thanks for taking the quiz!
```

### Number Guessing Game

```pilot
T: I'm thinking of a number between 1 and 4.
T: Can you guess what it is?

*GUESS
A: Enter your guess:
M: 4
TN: Try again.
JN: GUESS
T: You got it! The answer was 4.
T: Thanks for playing!
```

### Personal Information Collector

```pilot
T: Welcome! Let's collect your information.
T:
T: What is your first name?
A:$FIRST

T: What is your last name?
A:$LAST

T: How old are you?
A:$AGE

T: What is your favorite color?
A:$COLOR

T:
T: Summary of Information:
T: Name: $FIRST $LAST
T: Age: $AGE
T: Favorite Color: $COLOR

A: Is this information correct? (Y/N)
M: Y,YES
TY: Thank you! Information saved.
TN: Please run the program again to re-enter your information.

T: Goodbye, $FIRST!
```

### Menu System

```pilot
*MENU
T: ===== Main Menu =====
T: A) View Profile
T: B) Exit

A: Choose an option:
M: A
JY: PROFILE
M: B
JY: EXIT
T: Invalid option. Goodbye.
E:

*PROFILE
T: Your profile information.
E:

*EXIT
T: Thank you for using this program!
E:
```

## Tips and Best Practices

1. **Variable Names**: Use meaningful names (NAME for a name, AGE for an age)

2. **User-Friendly**: Always provide clear prompts:

   ```pilot
   A: Enter your answer:
   T: You answered: $ANSWER
   ```

3. **Input Validation**: Loop back with `JN:` until the answer matches (see Looping Until Valid Input below).

4. **Comments**: Use `R:` (remark) for comments. Lines starting with `REM` are also ignored:

   ```pilot
   R: This section handles the greeting
   *GREETING
   T: Hello!
   ```

5. **Readability**: Use consistent indentation and spacing in your code

## Common Patterns

### Looping Until Valid Input

```pilot
*GETINPUT
A: What is 2 + 2?
M: 4
TN: Invalid! Try again.
JN: GETINPUT
T: Correct!
```

### Counting

Use `C:` to keep a number and print it with `$`:

```pilot
C: N = 1
T: Count: $N
C: N = N + 1
T: Count: $N
C: N = N + 1
T: Count: $N
```

### Checking Several Answers

```pilot
A: What grade are you in? (1 or 2)
M: 1
TY: First grade!
M: 2
TY: Second grade!
```

Each `M:` replaces the result of the previous match.

## Limitations and Workarounds

1. **Text Matching**: `M:` matches substrings and ignores case, so use distinct answers in menus
2. **Arithmetic**: `C:` evaluates expressions such as `N + 1`. `M:` compares text only
3. **Loops**: Use jump-based loops (`J:`, `JN:`) instead of counted loops

## Troubleshooting

**Issue**: "Label not found" error

- **Solution**: Check that labels are spelled correctly and defined on their own line with a leading `*`: `*LABEL`

**Issue**: Infinite loop

- **Solution**: Make sure your jumps eventually reach a break point or end

**Issue**: Variables not retaining values

- **Solution**: Check that the variable name is spelled the same in `A:`/`C:` and in `$NAME`. Names are not case-sensitive.

**Issue**: Input not being stored

- **Solution**: Use `A:$NAME` to store the answer in a named variable. A plain `A:` stores it in `ANSWER$`, so read it with `$ANSWER`.

## Advanced Techniques

### State Machine Pattern

```pilot
*STATE_MENU
T: In menu
A: Next? (1 = game, 0 = end)
M: 1
JY: STATE_GAME
M: 0
JY: STATE_END
T: Unknown choice.
E:

*STATE_GAME
T: In game
E:

*STATE_END
T: Ending
```

## Running PILOT Programs in Time Warp Studio

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
- Check out [example programs](../../Examples/) in the repository

Happy PILOT programming!
