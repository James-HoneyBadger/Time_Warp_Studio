# PILOT: lessons that answer back

PILOT (Programmed Inquiry, Learning Or Teaching) was written in 1962 by John
Starkweather for computer-aided instruction, and it is still the smallest language
here: a statement is a letter, a colon, and its text. `T:` types a line, `A:` takes
an answer, `M:` matches the answer against a list, and `J:` jumps. A PILOT listing
reads as a script for a lesson rather than as code, which is what it is. The
interpreter runs it line by line and the whole design is that matching: a program
asks, compares and branches on what was typed.

Press **F9** to run a document. The output blocks below are what the program prints.

## What a program looks like

```pilot
T: Hello, World
E:
```

```text
Hello, World
```

`T:` types the rest of the line, and `$` names inside it are replaced by variables.
`E:` ends the program; that is its only job outside a subroutine, and a lesson that
runs out of lines stops anyway. There are no line numbers and no punctuation: the
letter before the colon is the statement.

## Step 1: variables and arithmetic

Keep a count, change it, and print it twice.

```pilot
C: N = 3
T: You have $N messages
C: N = N * 2
T: Now $N
E:
```

```text
You have 3 messages
Now 6
```

`C:` computes: it evaluates the expression on its right and assigns it to the name
on its left, and it prints nothing. `$N` in a `T:` line is the variable's value, not
the name. Names are not case sensitive, and `$` substitution happens in every text
statement, so a lesson's wording can carry marks and counts.

## Step 2: jumps and labels

Skip a section of the lesson, then land on it from a label.

```pilot
T: start
J: SECOND
T: this line is skipped
*SECOND
T: landed
E:
```

```text
start
landed
```

A label is a line beginning with `*` and has no other meaning. `J:` takes a label
name, not a line number, and there is no `GOTO` arithmetic: `J: SECOND` goes to
`*SECOND`. Execution continues from there in order, so the jump is a way to choose
which part of the script runs.

## Step 3: subroutines

Call a labelled block and come back to the line after the call.

```pilot
U: *GREET
T: back in the main program
U: *GREET
E:

*GREET
T: hello from the subroutine
E:
```

```text
hello from the subroutine
back in the main program
hello from the subroutine
```

`U:` uses the block and `E:` inside the block returns to the caller, which is how
the same block runs twice. That is the same `E:` that ends the program: inside a
subroutine it is a return, and outside one it is a stop. Blocks used this way are
the shared parts of a lesson, and the main script sits between the calls.

## Step 4: asking, and where the answer goes

Ask a question and store the answer under a name the program chooses.

```pilot
T: What is your name?
A:$NAME
T: Your name is [$NAME]
E:
```

```text
What is your name?
Your name is []
```

The answer is empty in this run because the run report has no keyboard to read, and
the point of the transcript is the second line: `$NAME` printed the answer, and the
answer was nothing, so the brackets hold nothing. `A:$NAME` stores what is typed in
`NAME`; a plain `A:` stores it in `ANSWER$` instead, and its own text is the prompt.
That is why the question is a `T:` line here: a text prompt on `A:` followed by
`A:$NAME` asks the reader twice.

## Step 5: matching the answer

Compare the answer against a list and let the two branches print their own lines.

```pilot
T: What is the capital of France?
A:
M: PARIS
TY: Correct.
TN: No - it is Paris.
E:
```

```text
What is the capital of France?
No - it is Paris.
```

`M:` matches the last answer against a comma-separated list of alternatives. The
comparison ignores case, and a plain alternative matches anywhere in the answer;
`*` and `?` are wildcards for the rest. `TY:` prints only when the match succeeded
and `TN:` only when it failed, and `JY:` and `JN:` do the same for jumps. With no
keyboard attached every answer is empty, so this run takes the no-match branch.
Typing `PARIS` at the prompt in the studio prints `Correct.` instead.

## Where it catches people out

**`M:` matches text, so a menu can match more than one entry.** Because an
alternative is found anywhere in the answer and case is ignored, `M: A` matches an
answer of `a`, `Apple` or `Paris` (the `a` inside it). Give the alternatives
something that cannot collide, and match the longest form:

```pilot
T: Choose: 1 for profile, 2 to leave
A:
M: 1
TY: profile
M: 2
TY: goodbye
E:
```

**A second `A:` replaces the answer.** `M:` always compares the *last* answer, so
the `M:` must sit between the `A:` it tests and the next `A:`.

```pilot
T: Type a word
A:
M: *E*
TY: it has an E in it
E:
```

Put any statement other than `T:` between the answer and the match and the match
tests a different answer than the one intended.

**There is no counted loop.** Repetition is a `J:` or `JN:` back to a label, so
every loop needs an answer to leave on. A loop with no matching branch around it
never ends, and the engine stops it at its iteration limit rather than hanging.

## Commands used here

| Statement | What it does |
| --- | --- |
| `T:` | Types text, with `$NAME` replaced by a variable |
| `C:` | Computes an expression into a variable |
| `A:` | Takes an answer; `A:$NAME` stores it in `NAME` |
| `M:` | Matches the last answer against a list |
| `TY:`, `TN:` | Type only on a match, or only on a miss |
| `J:`, `JY:`, `JN:` | Jump always, on a match, or on a miss |
| `U:` | Call a labelled block |
| `E:` | Return from a block, or end the program |
| `*NAME` | A label |

## Next

The [language guide](../reference.md#pilot) has the rest: `$LEFT`, `$MATCH`
and `$RIGHT` after a match, wildcards, and `P:` for pauses.
[`Examples/pilot/history_quiz.pilot`](../../../Examples/pilot/history_quiz.pilot) is
a ten-question quiz built from these five ideas, and
[`Examples/pilot/hello.pilot`](../../../Examples/pilot/hello.pilot) is the first
program again.
