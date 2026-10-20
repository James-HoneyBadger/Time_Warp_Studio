# The ROM library

Ten languages, one socket. Selecting a ROM on the front panel cold-boots the
machine into it: both engines stop, the screen clears, the ROM's program memory is
brought back, and the banner names the language you are about to use.

This is the parallel-universe premise made concrete. A 1982 machine with a ROM
socket could have shipped the ten languages of the era as cartridges; this one
does, and each cartridge knows how to talk to the learner in its own voice.

- **Ten ROMs:** BASIC, APL, Logo, PILOT, FOCAL, Scheme, CHIP-8, COMAL, REXX,
  Smalltalk.
- **One prompt each:** the machine echoes what you type in the ROM's prompt —
  `READY.` for BASIC, `?` for Logo, `]` for COMAL, and so on.
- **One memory each:** switching away keeps that ROM's program; switching back
  restores it.

---

## The socket

| ROM | Version | Prompt | Banner note |
| --- | --- | --- | --- |
| BASIC | `V2.0` | `READY.` | `BASIC INTERPRETER` |
| APL | `V1.0` | `APL>` | `ARRAY PROCESSOR` |
| Logo | `V1.0` | `?` | `TURTLE LOGO` |
| PILOT | `V1.0` | `PILOT:` | `PROGRAMMED INSTRUCTION` |
| FOCAL | `V1969` | `*` | `FORMULA CALCULATOR` |
| Scheme | `V84` | `>` | `LISP MACHINE` |
| CHIP-8 | `V1.0` | `CHIP8>` | `VIRTUAL CONSOLE` |
| COMAL | `V2.1` | `]` | `STRUCTURED BASIC` |
| REXX | `V3.0` | `REXX>` | `SCRIPT PROCESSOR` |
| Smalltalk | `V80` | `ST>` | `OBJECT WORKBENCH` |

The banner is assembled from those fields:

```text
*** HB-1 ***
Scheme V84 ROM  ·  64K RAM SYSTEM
(C) 1982 HB SYSTEMS  ·  LISP MACHINE
HB DOS 1.0  ·  VOLUME HBWARP
SYSTEM READY
```

CONFIG chooses how much of it appears: **Full** (all five lines), **Short** (the
machine name and the ROM line) or **Off** (an empty screen, as a quiet ROM would
leave it). Unless the message is Off, a cold boot prints the prompt's opening
state last; `READY.` after a BASIC banner is the machine saying it is listening.

---

## How a ROM behaves

Every ROM is the same three things in a different costume:

1. **A program** on the PROGRAM page — what `RUN`, `SAVE` and the disk use.
2. **A prompt session** — what you type when you are exploring.
3. **A drawing surface** — the right-hand canvas in SPLIT, or the whole screen in
   GRAPHICS. A language that cannot draw simply never puts ink there.

The session is where the ten differ. Eight ROMs keep bindings between prompt
lines:

**BASIC, FOCAL, PILOT, COMAL, REXX, APL, Logo, Scheme** — `LET X = 5` then
`PRINT X` works, in whatever spelling the language uses. Scheme additionally
keeps definitions and echoes the value of an expression.

**Smalltalk and CHIP-8** — each submission is a whole program, so state resets
between lines. That is the languages' own nature, not a limitation of the
machine: a Smalltalk workspace is rebuilt per submission, and a CHIP-8 listing is
assembled and run to completion.

`RUN` (F9) always runs the PROGRAM page as one program, on the language's own
engine, and ends the prompt session first. See
[Two ways to run](README.md#two-ways-to-run).

---

## BASIC

The machine's native tongue, and the ROM it boots into.

**Direct mode works.** This is the part a 1982 learner would recognise
immediately:

| You type | What happens |
| --- | --- |
| `10 PRINT "HELLO"` | Line 10 is stored on the PROGRAM page, in numeric order |
| `15 PRINT "AGAIN"` | Line 15 is inserted between 10 and 20 |
| `10 PRINT "HELLO, AGAIN"` | The existing line 10 is replaced |
| `15` | Line 15 is deleted (`LINE 15 DELETED`) |
| `? 6 * 7` | Prints `42`; `?` is short for `PRINT` |
| `2 + 2` | Prints `4` as an expression — a line is only a program line when a keyword, `?`, `'` or `#` follows the number |
| `RUN`, `LIST`, `NEW` | The machine's words, not the language's |

Stored lines appear on the PROGRAM page as you type them, with the cursor left on
the line you just entered. A bare number that is not in the program refuses with
`?LINE 15 NOT FOUND` rather than doing nothing.

**Start with:** `Examples/basic/hello.bas`, then `Examples/basic/showcase.bas`.

---

## APL

Arrays as a language, notation as a tool. The ROM keeps bindings, so a matrix
defined at the prompt is still there on the next line. Drawing goes through the
same turtle words the language has in the editor.

**Start with:** `Examples/apl/arrays_and_turtle.apl`.

---

## Logo

The `?` prompt is the one most people remember. The PROGRAM page holds the
listing; the prompt is where you type `FD 100`, define a procedure, and call it.
Bindings and definitions persist across prompt lines.

**Start with:** `Examples/logo/hello.logo`, then
`Examples/logo/mandala.logo` — the machine draws it on its own canvas.

---

## PILOT

Programmed instruction: text, questions, answers and jumps between them. Useful
for the machine's oldest trick — a lesson that is itself a program.

**Start with:** `Examples/pilot/hello.pilot`, then
`Examples/pilot/brain_trainer.pilot`.

---

## FOCAL

The formula calculator from 1969, with a `*` prompt. Everything is short: a line
of arithmetic, a value stored, a formula evaluated.

**Start with:** `Examples/focal/square.foc`, then
`Examples/focal/showcase.foc`.

---

## Scheme

A Lisp machine in a socket. The prompt keeps definitions, so `(define (square n)
(* n n))` then `(square 12)` behaves exactly as a Lisp machine should, and an
expression typed alone echoes its value.

**Start with:** `Examples/scheme/hello.scm`, then
`Examples/scheme/recursion.scm`.

---

## CHIP-8

The odd one out, and deliberately: in the HB-1 the CHIP-8 ROM takes a **listing**
at the prompt or on the PROGRAM page, assembles it, and runs it on the 64×32
virtual screen, drawing into the canvas. `RUN` executes the whole listing once,
ten opcodes to a frame, up to the frame limit.

That is the right model for demonstrating an instruction, not for playing a
game: there is no keypad on the HB-1's front panel. For a live CHIP-8 — 60 frames
a second, sixteen keys, sound, a register inspector — open the
[CHIP-8 machine](../chip8/README.md) instead. The two share the assembler and the
machine; only the clock and the keypad differ.

**Start with:** `Examples/chip8/paint.ch8` in the CHIP-8 window, or a short
listing at this ROM's prompt from [the assembler](../chip8/assembly.md).

---

## COMAL

Structured BASIC, `]` prompt, procedures that behave. The ROM is the bridge
between BASIC's line numbers and the languages that came after.

**Start with:** `Examples/comal/hello.comal`, then
`Examples/comal/procedures.comal`.

---

## REXX

Strings, parsing and subroutines. Its prompt is the machine's most talkative at
`REXX>`, and its programs are the easiest to read aloud in class.

**Start with:** `Examples/rexx/hello.rexx`, then `Examples/rexx/strings.rexx`.

---

## Smalltalk

Objects and messages, in a `ST>` workspace. Each submission is a whole program,
so the workspace is rebuilt per line — which is also the cleanest way to teach
that a Smalltalk expression is a message send with an answer.

**Start with:** `Examples/smalltalk/shapes.st`, then
`Examples/smalltalk/showcase.st`.

---

## Switching ROMs

Selecting a ROM is a cold boot, not a tab switch:

1. The running program and the prompt session stop.
2. The current ROM's program is written to its own memory.
3. The new ROM's program is brought back.
4. The screen and the canvas are cleared and the banner is printed.

Nothing on the disk moves: `CATALOG` looks the same in all ten ROMs, because the
disk is the machine's, not the language's. A saved file records the ROM that can
run it, so `LOAD "SQUARE"` cold-boots the machine into Logo before the program
arrives. See [HB DOS](hb-dos.md#files-and-their-roms).

---

## Adding a ROM

The socket is the `Language` enum, and a ROM is a row in the machine's table
(version, prompt and banner note) plus an engine. Adding one is the same work as
adding a language to the Studio; `docs/ARCHITECTURE.md` and `native/README.md`
have the recipe, and [Inside the HB-1](inside.md) shows where the ROM table
lives.
