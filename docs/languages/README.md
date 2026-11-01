# The ten languages

Ten interpreters, each an implementation of its own dialect rather than a common core
wearing different keywords. That is the whole point of the environment: a program written
for 1975 runs the way it was written, with the constraints its author had.

| Language | Paradigm | Era | Execution | What it is for |
| --- | --- | --- | --- | --- |
| **BASIC** | Imperative, educational | 1964 | Line by line | The language of the type-in listing: numbered lines, `GOTO`, and a direct mode |
| **APL** | Array, notation as tool | 1966 | Line by line | Working on whole arrays at once, in a notation with its own glyphs |
| **Logo** | Turtle graphics, educational | 1967 | Whole program | Drawing, and teaching a procedure by watching it move |
| **PILOT** | Computer-aided instruction | 1969 | Line by line | Dialogue: matching an answer and branching on it |
| **FOCAL** | Formula calculator, teaching | 1969 | Line by line | Arithmetic with line labels and sign branching |
| **Forth** | Stack, dictionary of words | 1970 | Whole program | Building a vocabulary of words that solve the problem |
| **Scheme** | Functional, Lisp dialect | 1975 | Whole program | Recursion, lists and functions as values |
| **COMAL** | Structured BASIC, educational | 1979 | Line by line | What BASIC should have been: procedures, `IF`/`ENDIF`, loops that nest |
| **REXX** | Scripting, string processing | 1979 | Line by line | Text: parsing, string functions, and readable control flow |
| **Smalltalk** | Object-oriented, messages | 1980 | Whole program | Objects, messages, blocks and a class library |

Every command of every language is in [the reference](reference.md), and each language has a
[tutorial](tutorials/) written to be worked through in an evening.

## The execution split is real

Six languages are called **line by line**: BASIC, APL, PILOT, FOCAL, COMAL and REXX. The
shared loop hands the executor one statement at a time, which is what lets the debugger pause
between statements, profile per line and walk a run backwards.

Four run as a **whole program**: Logo, Smalltalk, Forth and Scheme. Each has its own loop and
decides where a pause may land — the end of a Logo statement, a Smalltalk message send, a
Forth token. Scheme has a runner of its own, because it keeps a call stack and frame bindings
that the others do not.

Nothing about that distinction is visible in a menu: it is why `Step` behaves slightly
differently in Forth than in BASIC, and the reference says so where it matters.

## What they share

- **One turtle.** Every language draws through the same host, so heading 0 is north and
  increases clockwise in all of them, `forward` is `x += d·sin(h)`, `y += d·cos(h)`, and a
  `circle` is a polygon walk. A drawing made in Logo and one made in COMAL land on the same
  canvas with the same conventions.
- **One budget.** A run stops after two million statements or ten seconds, whichever comes
  first, and says which limit ended it.
- **One SQL connection.** `SQL SELECT …` reaches the database the Database panel has open, in
  nine of the ten. Forth is the exception: it has no statement form to extend, so a Forth
  program reaches the database over the [API](../user/automation.md) or not at all.
- **One error convention.** An engine returns text, and a failure begins with the error
  prefix rather than throwing out of the executor. The console, the Errors panel and the
  [API](../user/automation.md) all read the same string.

## Choosing a first language

| If the learner | Start with | Because |
| --- | --- | --- |
| wants a picture quickly | **Logo** | `REPEAT 4 [FORWARD 60 RIGHT 90]` is a square, and the loop is visible |
| is following an old book | **BASIC** | The listings will run, line numbers and all |
| is nervous about symbols | **PILOT** | The whole language is a dialogue with a match |
| wants arithmetic and nothing else | **FOCAL** | A line label, a formula, a branch |
| knows a modern language | **Forth** or **Scheme** | Both are a different way of thinking, not a reduced version of what they know |
| wants objects | **Smalltalk** | Messages all the way down, and a library to send them to |

The [curriculum](../lessons/README.md) takes the same order: two lessons per language, a
beginner's and an intermediate one.

## How the ten are checked

The languages are not documented and left to hope. Three gates hold them:

- **The command matrix** runs one program per documented command, from
  `native/share/command_rows.json`, and reports any command the executors dispatch on that no
  row or waiver mentions.
- **The parity corpus** compares the engines with the answers the deleted Python reference
  gave for 442 programs, including every drawing call in order.
- **The example gate** runs all 68 shipped programs, and fails when a program that should draw
  produces no ink.

[Testing](../maintainer/testing.md) says what each gate is worth, and where a recording is
weaker than the differential it replaced.
