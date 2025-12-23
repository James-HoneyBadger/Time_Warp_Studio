# PILOT Language Reference

Time Warp IDE PILOT Interpreter - Complete command reference.

## Language Overview

PILOT (Programmed Inquiry, Learning, Or Teaching) is an educational programming language designed for creating interactive lessons and tutorials.

## Command Structure

Each PILOT statement begins with a command letter followed by a colon and parameters:

```pilot
T: Text to display
A: variable
C: variable=value
```

## Core Commands

### T: - Type (Display Text)

Display text to the user.

```pilot
T: Hello, World!
T: The answer is $answer
```

Variables are referenced with `$variablename`.

### A: - Accept (Input)

Accept input from user and store in variable.

```pilot
A: name
T: Hello, $name!
A: age
T: You are $age years old.
```

### C: - Compute

Perform calculations and assign to variable.

```pilot
C: total = 5 + 3
T: The total is $total
C: square = $x * $x
```

### M: - Match

Pattern matching for input validation.

```pilot
T: Enter yes or no:
A: response
M: yes
T: You said yes!
M: no
T: You said no!
```

### E: - End

End program execution.

```pilot
T: Program complete.
E:
```

### U: - Use

Call a subroutine or module.

```pilot
U: greet_user
T: Main program continues...

*greet_user
T: Hello from subroutine!
```

### J: - Jump

Conditional or unconditional branch.

```pilot
J: label
J(condition): label

*label
T: Arrived at label
```

### Y: - Yes (True Branch)

Execute if last match succeeded.

```pilot
M: yes
Y: T: You answered affirmatively.
```

### N: - No (False Branch)

Execute if last match failed.

```pilot
M: yes
N: T: You did not answer yes.
```

## Pattern Matching

PILOT supports sophisticated pattern matching:

### Exact Match
```pilot
M: hello
```

### Wildcard Match
```pilot
M: *world*
```
Matches any string containing "world".

### Multiple Patterns
```pilot
M: yes, y, yeah, yep
```
Matches any of the specified alternatives.

### Case-Insensitive
All matches are case-insensitive by default.

## Variables

### Assignment
```pilot
C: score = 0
C: name = John
```

### String Concatenation
```pilot
C: fullname = $firstname + " " + $lastname
```

### Arithmetic Operations
```pilot
C: total = $a + $b
C: product = $x * $y
C: quotient = $a / $b
```

Supported operators: `+`, `-`, `*`, `/`, `^` (power), `%` (modulo)

## Conditional Execution

### Based on Match
```pilot
T: Enter a number:
A: num
M: *1*
Y: T: Contains digit 1
N: T: Does not contain 1
```

### Based on Comparison
```pilot
C: result = $a > $b
J(result): greater
T: A is not greater
J: done
*greater
T: A is greater than B
*done
```

## Subroutines

Define with `*label`, call with `U:label`.

```pilot
*main
T: Starting program
U: get_name
U: greet
E:

*get_name
T: What is your name?
A: name

*greet
T: Hello, $name!
T: Welcome to PILOT!
```

## Special Variables

### System Variables
- `$TIME` - Current time
- `$DATE` - Current date
- `$RANDOM` - Random number 0-1

### Expression Variables
```pilot
C: result = ($a + $b) * $c
```

## Example Programs

### Simple Quiz
```pilot
*start
T: PILOT Quiz Program
T:
T: Question 1: What is 2 + 2?
A: answer
M: 4
Y: T: Correct!
Y: C: score = $score + 1
N: T: Incorrect. The answer is 4.
T:
T: Question 2: What is the capital of France?
A: answer
M: paris
Y: T: Correct!
Y: C: score = $score + 1
N: T: Incorrect. The answer is Paris.
T:
T: Your score: $score out of 2
E:
```

### Interactive Lesson
```pilot
*intro
T: Welcome to the PILOT Tutorial
T: This lesson will teach you about variables.
T:
T: Press ENTER to continue
A: dummy
U: variables_lesson
E:

*variables_lesson
T: Variables store information.
T: Let's create a variable called 'age'.
T:
T: How old are you?
A: age
T:
T: Great! Your age is $age.
C: nextyear = $age + 1
T: Next year you will be $nextyear.
```

### Number Guessing Game
```pilot
*start
C: target = 50
C: guesses = 0
T: I'm thinking of a number between 1 and 100.
U: game_loop
E:

*game_loop
T: Enter your guess:
A: guess
C: guesses = $guesses + 1
M: $target
Y: U: win
C: result = $guess < $target
J(result): too_low
T: Too high!
J: game_loop
*too_low
T: Too low!
J: game_loop

*win
T: Correct! You guessed it in $guesses tries!
```

## Graphics Commands

PILOT in Time Warp IDE includes graphics extensions:

### G: - Graphics
```pilot
G: CLEAR
G: PEN 1
G: MOVE 100,100
G: LINE 200,200
G: CIRCLE 150,150,50
```

## Reserved Words

```
A: C: E: G: J: M: N: T: U: Y:
```

## Best Practices

1. **Use descriptive variable names**: `$score` instead of `$s`
2. **Comment with T:**: Use `T:` statements as inline documentation
3. **Structure with subroutines**: Break complex programs into modules
4. **Validate input with M:**: Always check user input
5. **Provide feedback**: Use `T:` to guide users

## Compatibility Notes

- Time Warp PILOT is based on PILOT/1969 with modern extensions
- Graphics commands are Time Warp-specific
- All matching is case-insensitive
- Variables persist throughout program execution

---

**Version**: 5.0.0  
**Platform**: Time Warp IDE for Windows 2000
