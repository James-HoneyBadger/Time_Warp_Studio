# CHIP-8 Tutorial

CHIP-8 is Joseph Weisbecker's bytecode interpreter from 1977 - the machine the
first home computers were taught with. It is not a text language. A program is a
sequence of two-byte opcodes, and the display is a 64x32 framebuffer of lit and
unlit pixels.

So Time Warp Studio's CHIP-8 is two things at once:

- an **assembler**, which turns mnemonics into opcodes, and
- the **machine**, which runs them.

It runs whole-program, because an opcode stream cannot be interpreted a line at a
time: the listing is assembled in full and then run.

---

## 1. A program

```chip8
; A comment
start:  CLS
        LD   V0, 0x20
        LD   V1, 0x10
        LD   I, block
        DRW  V0, V1, 2
        HALT

block:  DB   0xF0
        DB   0x90
```

`CLS` clears the screen, `DRW` draws, `HALT` stops. Labels work as they do in any
assembler: `start:` and `block:` name addresses, and a label is where the next
instruction or data byte lands.

## 2. Numbers

Decimal unless marked, and marked either way round, because both spellings appear
in listings:

| Written | Value |
| ------- | ----- |
| `41` | decimal 41 |
| `0x41` | 65 |
| `41h` | 65 |

## 3. Directives

| Directive | Does |
| --------- | ---- |
| `DB n` | Emit one byte |
| `DW n` | Emit one word |
| `HALT` | Stop the machine |
| `; keys <digits>` | Present keypad state for the run |

`DB` is how you write a sprite. Each byte is one row of eight pixels, with the
**most significant bit on the left**, so `0xF0` is the left four pixels of a row
and `0x0F` the right four.

## 4. The registers

Sixteen 8-bit registers `V0`-`VF`, plus `I`, a 12-bit address register used to
point at memory.

```chip8
LD   V0, 0x41       V0 := 65
LD   V1, V0         V1 := V0
ADD  V0, 1          V0 := V0 + 1
LD   I, gfx         I now points at the data labelled gfx
```

`VF` doubles as a flag register: arithmetic and `DRW` set it, and that is how you
test for a carry or a collision.

## 5. Arithmetic and logic

```chip8
ADD  V0, 1          V0 := V0 + 1
ADD  V0, V1         V0 := V0 + V1
SUB  V0, V1         V0 := V0 - V1
SUBN V0, V1         V0 := V1 - V0
OR   V0, V1         V0 := V0 OR V1
AND  V0, V1         V0 := V0 AND V1
XOR  V0, V1         V0 := V0 XOR V1
SHR  V0             V0 := V0 >> 1, in place
SHL  V0             V0 := V0 << 1, in place
RND  V0, 0x0F       V0 := random AND 0x0F
```

`SHR` and `SHL` shift `V0` **in place**. A later generation of emulators shifted
`Vy` and stored into `Vx`; this follows the 1977 interpreter instead. The y
operand is accepted and ignored, so both spellings assemble.

## 6. Skipping and branching

```chip8
JP   draw           jump
JP   V0, draw       jump to draw + V0, the jump table
CALL sub            call a subroutine
RET                 return from it
SE   V0, 1          skip the next instruction if V0 = 1
SNE  V0, 1          skip the next instruction if V0 != 1
SKP  V0             skip if the key in V0 is held
SKNP V0             skip if it is not held
```

Testing a register is a *skip*, not a branch: you put the jump straight after the
instruction you want to skip.

The jump-table form is `Bnnn`, which always adds `V0` to the address. Naming any
other register is an assembler error rather than an instruction that silently
ignores it.

`SYS addr` assembles but is ignored: it calls a routine on the RCA 1802 that
hosted the original interpreter, which this machine cannot run.

## 7. Drawing

```chip8
LD   F, V0          I points at the glyph for the hex digit in V0
DRW  V0, V1, 5      draw 5 rows at (V0, V1)
```

`DRW` draws an n-byte sprite with its top-left corner at the coordinates in two
registers. Two behaviours are worth knowing:

- **Sprites wrap** around the edges of the screen.
- **Drawing over a lit pixel turns it off, and sets `VF`.** That is how CHIP-8
  erases, and how it detects a collision.

```chip8
        LD   V0, 0x2C
        LD   V1, 0x06
        LD   I, frame
        DRW  V0, V1, 3      draw it
        ADD  V0, 1
        DRW  V0, V1, 3      draw it one pixel right: the shared pixels go out
```

`LD F, Vx` uses the machine's built-in font, a 4x5 hex digit set the machine keeps
in memory at `0x50`. Every CHIP-8 program is written against it.

`LD B, Vx` writes the decimal digits of `Vx` into memory, and `LD [I], Vx` /
`LD Vx, [I]` move a block of memory. Both leave `I` **one past the block**, as
the 1977 interpreter does; CHIP-48 and SCHIP leave `I` alone. Set `I` again if
the next instruction needs the start of the block.

## 8. The keypad

A program runs to completion inside one call, so the engine has no way to hand
input to something that never yields control. Keypad state is therefore
**presented by directive**:

```chip8
; keys 13
start:  LD   V0, K          V0 := the first held key, or 0
        LD   V1, 6
        LD   I, dot
        DRW  V0, V1, 1
        HALT
dot:    DB   0x80
```

`; keys 13` marks keys 1 and 3 as held down, so `LD Vx, K` reads 1.

## 9. Timers and frames

There are two timers: `DT` (delay) and `ST` (sound).

```chip8
LD   DT, V0         set the delay timer
LD   V0, DT         read it
```

Frames are **simulated**, not timed: ten opcodes make a frame and the timers tick
once per frame, with no real clock involved. So a program runs as fast as the
budget allows, and a fixed number of frames is the limit. A program that never
halts reports that it stopped rather than hanging:

```text
ℹ️ CHIP-8: stopped after 600 frames
```

## 10. Loading a binary ROM

A `.ch8` file that contains a **NUL byte** is a binary ROM rather than a listing.
Its bytes are copied to `0x200` unchanged, up to the 3584 bytes that fit before
the end of memory, and assembled at all only if no NUL is present. That means the
same extension covers both the listings in this tutorial and ROMs from elsewhere.

A ROM opens **read-only** in the editor. Its bytes are decoded one-to-one so that
saving it writes back the same program, and a keystroke in it would be a corrupt
ROM waiting to be saved. A listing is still editable source.

## 11. The interactive emulator

A Run executes a program to completion in a simulated 600 frames, which is right
for a lesson but wrong for a game. **Tools > CHIP-8 Emulator** (Ctrl+Shift+M)
opens the same bytes as a live machine:

- the 64x32 screen, scaled with square pixels;
- the 16-key pad, clickable, and the PC keys `1-4`, `Q-R`, `A-F`, `Z-V`;
- **Run/Pause** (F5), **Step** (F6) and **Reset** (F7);
- a speed control in opcodes per 60 Hz frame -- ten is the original pace;
- the buzzer while the sound timer is above zero, with a Mute switch;
- registers, timers, cycles, the call stack and a memory hex dump beside the
  screen;
- a **Listing** tab: type a program and press **Assemble & Load** (Ctrl+Return)
  to assemble and run it without leaving the window. A listing that does not
  assemble reports the error there and leaves the running program alone.

It opens on the bundled [`paint.ch8`](../../Examples/chip8/paint.ch8): move the
cursor with `2`, `8`, `4` and `6`, paint with `5`, and clear with `0`.

Open any other `.ch8` with the toolbar or drop it on the window. `LD Vx, K` waits
for a key here instead of reading `; keys`, and the delay and sound timers follow
the wall clock. If the current document is a CHIP-8 program, opening the emulator
loads it straight away.

## Quick reference

| Mnemonic | Meaning |
| -------- | ------- |
| `CLS` | Clear the display |
| `SYS addr` | Ignored; the original's RCA 1802 call |
| `JP addr` / `JP V0, addr` | Jump |
| `CALL addr` / `RET` | Subroutine call and return |
| `SE` / `SNE` | Skip if equal / not equal |
| `LD Vx, byte` / `LD Vx, Vy` / `LD I, addr` | Load |
| `LD F, Vx` | Point `I` at a hex glyph |
| `LD B, Vx` | Decimal digits of `Vx` into memory |
| `LD [I], Vx` / `LD Vx, [I]` | Memory block out / in |
| `ADD` `SUB` `SUBN` `OR` `AND` `XOR` | Arithmetic and logic |
| `SHR Vx` / `SHL Vx` | Shift in place |
| `RND Vx, byte` | Random, masked |
| `DRW Vx, Vy, n` | Draw an n-byte sprite |
| `SKP Vx` / `SKNP Vx` | Skip on key held / not held |
| `LD Vx, K` | Read the first held key |
| `LD Vx, DT` / `LD ST, Vx` | Read the delay timer / set the sound timer |
| `DB` `DW` `HALT` `; keys` | Directives |

See also
[`Examples/chip8/font_and_frames.ch8`](../../Examples/chip8/font_and_frames.ch8),
and the [CHIP-8 reference](../LANGUAGE_GUIDE.md#chip-8).
