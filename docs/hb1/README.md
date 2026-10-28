# HB-1 Owner's Manual

```text
*** HB-1 ***
BASIC V2.0 ROM  ·  64K RAM SYSTEM
(C) 1982 HB SYSTEMS  ·  BASIC INTERPRETER
HB DOS 1.0  ·  VOLUME HBWARP
SYSTEM READY
READY.
```

This is the manual for the machine the Studio carries: a home computer from a
1982 that never happened, where one socket accepted eleven language ROMs and the
disk was measured in sectors because disks were still physical things. The
machine is real software — everything in this book happens when you type it.

- **Open it:** Tools > **HB-1 Computer…** (`Ctrl+Alt+W`), or `./run.sh --computer`.
- **The surrounding story:** [the machines](../MACHINES.md).
- **The rest of this book:** [ROMs](roms.md) · [HB DOS](hb-dos.md) · [WARP BOARD](bbs.md) ·
  [CONFIG](config.md) · [Inside](inside.md).

---

## Specifications

| | |
| --- | --- |
| Model | HB-1 Personal Computer System |
| ROM socket | Eleven language ROMs: BASIC, APL, Logo, PILOT, FOCAL, Scheme, CHIP-8, COMAL, REXX, Smalltalk, Forth |
| Program memory | 64K, one buffer per ROM, kept when you switch away |
| Display | One CRT, three modes: TEXT, SPLIT (the default), GRAPHICS |
| Text side | PROGRAM (a real code editor) and SCREEN (the tube, painted from memory) |
| Graphics side | The turtle canvas: the same drawing surface the IDE uses |
| Keyboard | The prompt line, with history; function keys below |
| Disk | HB DOS 1.0: one volume, 512 sectors of 256 bytes, 30-character names |
| Monitor | Green, amber, white or ice phosphor; scanlines, vignette, curvature, flicker, glow |
| Power | The lamp in the header. Plugging in is not modelled |

The machine is described as 64K because that is what the status line says and
what the period wanted. What it actually is, is a window onto the Studio's native
engines with a very good costume on: see [Inside the HB-1](inside.md).

---

## First boot

When the machine opens it cold-boots into a ROM — BASIC, unless CONFIG says
otherwise — and prints its **self-test** and then the banner above:

```text
HB-1 SELF TEST
32768 BYTES FREE
RAM CHECK OK (128 CELLS)
ROM CHECK OK (11 ROMS, 3352561743)
HB DOS 1.0  HBWARP
*** HB-1 ***
…
```

The self-test is a test, not a decoration: it writes a pattern into the free RAM it
claims to have and reads it back, and it sums the bytes of every ROM's version and
banner note. A failure says which address failed and stops the boot. It appears when
CONFIG's start-up message is **Full**, because Short and Off are promises about how
much the machine says at power-on and a self-test that always printed would break
them.

Then:

1. **Type a program.** In the BASIC ROM, `10 PRINT "HELLO, TIME TRAVELLER"` at the
   prompt stores line 10 on the PROGRAM page. `20 GOTO 10` stores another.
2. **Press Return.** Nothing runs yet; a numbered line is a program line, and
   storing it is the whole action.
3. **Press F9**, or type `RUN`. The program runs. Its output appears on SCREEN;
   its drawing appears on the right of the screen.
4. **Switch ROMs.** Choose **Logo** in the ROM selector. The machine cold-boots:
   the screen clears, the banner names Logo, and the prompt becomes `?`. The
   BASIC program is still in BASIC's memory; switch back and it is there.
5. **Save your work.** `SAVE "HELLO"` writes the current program to the disk.
   `CATALOG` shows it. Close the machine, reopen it, `CATALOG` again: it is still
   there. That is [HB DOS](hb-dos.md) doing its job.

Nothing above needs a file on disk, a project, or the IDE behind the window. The
machine is complete on its own — which is the point of a home computer.

---

## The front panel

From left to right across the top:

| Control | What it does |
| --- | --- |
| **HB-1** and the tagline | Identity. The tagline reads `PERSONAL COMPUTER SYSTEM · 64K RAM` |
| **ROM** | The language socket. Selecting a ROM cold-boots into it |
| **SCREEN** | `TEXT`, `SPLIT` or `GRAPHICS` |
| **CONFIG** | The machine's settings screen — [all of them](config.md) |
| **POWER** | The lamp **is the switch**: click it and the machine goes dark, the prompt stops answering and the clock stops. Click again and it cold-boots, self-test and all |

Under the screen:

| Control | What it does |
| --- | --- |
| The prompt | Everything typed here is remembered in history and echoed in the ROM's voice |
| **RUN** | Runs the program on the PROGRAM page (F9) |
| **STOP** | Stops a running program (Shift+F9) |
| **CLS** | Clears the screen and the drawing (Ctrl+L) |

### The machine's voice

The HB-1 makes five sounds that no program asked for, and a reader who owned one of
these remembers all five:

| When | What it sounds like |
| --- | --- |
| Power on, cold boot, ROM change | a rising two-note chirp as the tube warms up |
| A line typed at the prompt | one short click |
| `SAVE`, `LOAD`, `CATALOG`, `INIT`, any disk word | the head stepping: a short rattle |
| A program reports an error | a low buzz, once per run and not once per line |
| The tape motor (20.4) | a warble over a hiss |

They are tones built from the same mixer a program's `SOUND` uses — one sound chip,
one voice — and CONFIG has a volume and a mute. A muted machine still *counts* what
it did: muting is a promise about sound, not about history.

A program has a sixth sound: **the latch at `$A005`**. `POKE 40965, 30` writes a code
there and the machine plays it on the next frame, then clears the register itself so
the program does not have to. `100 + code * 8` hertz, 80 milliseconds — enough for a
program to play a scale by poking fifteen values, which is all a one-register beeper
could ever do.

The status line along the bottom is a machine readout, not decoration:

```text
ROM BASIC V2.0   MEM 64K   MODE SPLIT   LINE 1:1   HB DOS 0F   READY
```

It names the ROM and its version, the screen mode, the cursor position on the
PROGRAM page, how many files the disk holds, and whether a program is `RUNNING`
or the machine is `READY`.

### The screen

**TEXT** shows only the text side. **GRAPHICS** shows only the canvas.
**SPLIT** — the default, and the machine's natural state — shows both, with the
text on the left.

The text side has two pages:

- **PROGRAM** — a `CodeEditor`, the same widget the IDE uses: gutter, current
  line, syntax colouring, undo, selection, search. A listing is not pasted into
  a text box here.
- **SCREEN** — the machine's screen: the tube, painted from the memory map below, with
  the banner, prompt echoes, program output and errors written into its cells

A program that draws while the machine is in TEXT switches to SPLIT once, so the
ink cannot land somewhere it cannot be seen. CONFIG can turn that off for a
learner who wants to watch the drawing arrive deliberately.

### The screen is memory

The SCREEN page is not a text box that the machine appends lines to. It is a window
onto the machine's own 64K, and that is the difference between a picture of a home
computer and one:

| Range | What it is |
| --- | --- |
| `$0000`–`$7FFF` | **Free RAM**, 32768 bytes, for `POKE` and for machine code |
| `$8000`–`$8FFF` | **Screen cells**: 80 × 25 character codes, row-major, 2000 bytes |
| `$9000`–`$9FFF` | **Colours**: one ink/paper nibble pair per cell, in the same order |
| `$A000`–`$A0FF` | **I/O registers**: cursor column, cursor row, border, ink, paper, sound latch (`$A005`), frame counter low and high (`$A006`, `$A007`, sixty a second), scrolled flag |
| `$F000`–`$FFFF` | **ROM**: readable, and a `POKE` into it is refused out loud |

So a character is a byte at an address, and the machine shows it:

```basic
10 POKE 32768, 72                 ' 'H' in the top-left corner
20 POKE 32848, 73                 ' 'I' at row 1, column 0
30 LOCATE 5, 10                   ' the cursor, as a register
40 PRINT "HERE"                   ' so this lands at row 5, column 10
50 POKE 32768 + 24 * 80 + 3, 128  ' a graphics character on the bottom row
```

`POKE` and `PEEK` take **expressions** — a program writes the cell it computed —
and the tube scrolls by moving *cells*, so `PEEK` of the top row after a scroll
answers with what is on the screen now. `CLS` clears cells; `COLOR ink, paper` sets
the nibble pair the next characters use; the border is register two.

The status line's `MEM` is a reading rather than a badge: 32K is the free RAM this
machine actually has, and it says so because the number comes from the map.

A machine of 1982 could not show a character the tube has no glyph for, and neither
can this one: the Studio's engines prefix messages with an emoji, and the tube drops
that prefix and transliterates the typographic punctuation in the banner. What is
left is ASCII, which is what the machine's character set is.

### Keys

| Key | Action |
| --- | --- |
| `F9` | Run the program |
| `Shift+F9` | Stop |
| `Ctrl+L` | Clear the screen and the canvas |
| `Ctrl+,` | CONFIG |
| `Up` / `Down` | Previous and next prompt line, up to the history depth CONFIG sets |
| `Ctrl+Alt+W` | Open or raise the machine, from the IDE |

---

## The prompt

The prompt is the machine's own command line. It understands two vocabularies,
in this order:

1. **The machine's own words** (below). These never reach a language.
2. **The selected ROM's language.** Everything else is submitted to a persistent
   session for that ROM, so `LET X = 5` then `PRINT X` works, and so does
   `(define (square n) (* n n))` in Scheme.

In the BASIC ROM there is a third case: a line that starts with a number and then
a keyword is a program line, not a command — see [direct mode](roms.md#basic).

### Machine words

| Word | What it does |
| --- | --- |
| `RUN` | Run the program on the PROGRAM page. With a name, `RUN "NAME"`, load that file and run it |
| `LIST` | Print the program to the screen |
| `NEW` | Erase the program and end the prompt session |
| `EDIT` | Move to the PROGRAM page |
| `TEXT`, `GFX`/`GRAPHICS`, `SPLIT` | Change the screen mode |
| `CLS`/`CLEAR` | Clear the screen and the drawing |
| `ROMS` | List the language ROMs, marking the one in the socket |
| `CONFIG`/`SETUP` | Open the settings screen |
| `HELP`, or a bare `?` | Print the machine's vocabulary |
| `CATALOG`, `SAVE`, `LOAD`, `DELETE`, `RENAME`, `LOCK`, `UNLOCK`, `VERIFY`, `TYPE`, `STATUS`, `LLIST`, `LPRINT`, `INIT` | [HB DOS](hb-dos.md) |
| `DIAL` | [WARP BOARD](bbs.md) |

In the BASIC ROM, the prompt also has the machine's **line editor**, which is the
part of a 1982 BASIC people actually spent their evenings in:

| Word | What it does |
| --- | --- |
| `AUTO [start[, step]]` | Puts the next line number in the prompt and advances it after every line you type; an empty line ends it. With no argument it continues after the listing |
| `RENUM [start[, step]]` | Renumbers the listing **and every jump in it** — `GOTO`, `GOSUB`, `THEN`, `ELSE`, `RESTORE`, `GO TO`, `GO SUB`, including the list after `ON x GOTO 10, 20, 30` |
| `DELETE 20-30` | Deletes a range (`DELETE 20` is one line, `DELETE 20-` is everything from 20) |
| `LIST 20-30` | Lists a range; a bare `LIST` still lists the whole program |
| `UNDO` | Puts back the listing `NEW` erased. One deep, and it says so |
| `TRON` / `TROFF` | Prints each line's number as it runs — `[10]`, `[20]`, … — which is the only way to see which arm of a `GOTO` a program took |

`RENUM` is careful on purpose. A comment (`REM GOTO 40`) and a string
(`PRINT "GOTO 40"`) are prose and are left byte for byte; a target that names a line
the program does not have is **left alone and counted**, because it was already a
runtime error and renumbering it would make it a different one. `DELETE "NAME"` is
still the disk, because a file name is in quotes and a line range is not.

### Two ways to run

There are two execution paths behind the machine, and the difference matters:

- **The prompt session** is a `ReplRunner`: it keeps bindings between lines and
  is where curiosity happens.
- **`RUN`** is a `NativeRunner`: the whole program, once, to completion, on the
  same engine the IDE's Run uses.

`RUN` ends the prompt session first. A program therefore never starts inside
variables left by an earlier command — the machine's version of "put your toys
away before you start". Selecting a different ROM ends the session too. The
next line after either starts a fresh one.

---

## What the machine remembers

The machine writes two different things to two different places, and the
distinction is worth knowing because it is the difference between "my settings"
and "my session".

### Session state — `retroComputer`

| Key | Holds |
| --- | --- |
| `geometry` | The window's size and position |
| `language` | The ROM you were left in |
| `mode` | The screen mode you were left in |
| `programs/<rom>` | Each ROM's program, when CONFIG's *Remember each ROM's program* is on |

With program memory switched off, the machine erases what it already kept, so
the next start cannot show programs the setting says are not remembered.

### Configuration — `retroComputer/config`

Boot ROM, boot screen, start-up message, prompt-history depth, program memory,
switch-to-SPLIT, phosphor, the two text sizes and the five glass strengths. Every
key is listed in [CONFIG](config.md#where-it-is-stored).

### What is *not* remembered

- **Prompt history.** Up and Down recall this session's lines; closing the
  machine forgets them. The history depth CONFIG sets is a budget, not a file.
- **A running program.** Programs do not survive a restart. `SAVE` is how a
  program outlives the afternoon; that is the lesson.

The disk is separate from both, because it is: one image file,
`~/.time_warp/disks/HBWARP.dsk`, moved with the rest of the Studio's data by
`TIME_WARP_DATA_DIR`. See [HB DOS](hb-dos.md#the-image).

---

## The whole machine as one file

A machine is more than a program: it is the ROM it is left in, the screen, every
ROM's program, the phosphor and the configuration, and the disk. Two words write that
as one document and boot from it:

```basic
SAVE MACHINE "HV"          ' writes HV.hb1 in the Studio's data directory
LOAD MACHINE "HV"          ' cold-boots from it
SAVE MACHINE "hv.hb1"      ' a name with a .hb1 on it, or a path, is used as given
```

`SAVE MACHINE` reports how many programs and how many files it wrote, because a
machine that saved nothing and said "OK" is the failure this project keeps guarding
against. `LOAD MACHINE` refuses — with a sentence, leaving the machine as it was —
a file that is not a machine image or one written by a newer Studio
(`this machine image is format 3; this build reads up to format 1`).

The image is JSON and reads like one, so a teacher can inspect it, put it in version
control, or make one by hand:

```json
{
  "format": 1,
  "name": "HV",
  "language": "logo",
  "mode": 1,
  "programs": { "logo": "REPEAT 4 [FD 60 RT 90]" },
  "config": { "phosphor": 1, "screenTextSize": 14 },
  "volume": { "volume": "HBWARP", "files": [] }
}
```

`language` is a ROM's protocol key (`basic`, `logo`, `pilot`, … — the eleven the Studio
has), `mode` is `0` TEXT, `1` GRAPHICS or `2` SPLIT (the order of the machine's own
`ScreenMode`, which is what the number is), and `volume` is the HB DOS volume
itself — the file it lives in is the reader's business, so a disk handed to somebody
else is a volume rather than a path on this machine.

---

## Running the machine from a terminal

```bash
./run.sh --computer                                  # open the machine
./run.sh --computer Examples/basic/hello.bas --run    # boot its language and run it
./run.sh --computer --screenshot hb1.png             # photograph the machine
./run.sh --computer Examples/logo/mandala.logo --run --export mandala.png
./run.sh --machine hv.hb1                            # boot a machine image
./run.sh --machine hv.hb1 --screenshot hv.png        # photograph that machine
```

With `--computer`, `--run`, `--screenshot` and `--export` act on the machine
rather than the editor: the screenshot is of the machine's window, and the export
is the ink on the machine's canvas. A script that demonstrates the boot screen
therefore captures the boot screen, not the IDE with a small window on top of it.

The binary takes the same options directly:

```bash
./native/build/time_warp_ide --computer --help
```

---

## The limits, stated plainly

A machine is defined by what it will not do.

| Limit | Value | What happens at the edge |
| --- | --- | --- |
| ROMs | Ten, one at a time | Nothing else exists to select |
| Program memory | One buffer per ROM | Switching keeps it; closing the machine keeps it only if *Remember each ROM's program* is on |
| Screen | TEXT, SPLIT, GRAPHICS | A whole-program run cannot show more |
| Disk volume | One, named `HBWARP` | There are no directories and no second volume |
| Disk size | 512 sectors of 256 bytes | A file that will not fit is refused (`?DISK FULL`), never truncated |
| File name | 30 characters, upper-cased | `SAVE Hello` and `SAVE "HELLO"` are the same request |
| Lock | Per file | A locked file refuses `SAVE` over it and `DELETE`; it can still be `TYPE`d, `LOAD`ed and renamed, and the lock travels with the name |
| `INIT` | Must be said twice | Any other line in between is a change of mind |

The machine will not crash, corrupt a save, or leave a key stuck. Those are not
period features; they are the parallel-universe part.

---

## Where next

- Learn the eleven ROMs: [the ROM library](roms.md).
- Keep a program: [HB DOS 1.0](hb-dos.md).
- Call somebody: [WARP BOARD](bbs.md), the board on the other end of the line.
- Make it yours: [CONFIG](config.md).
- Change the machine's mind: [Inside the HB-1](inside.md).
- Meet its sibling: [the CHIP-8](../chip8/README.md).
