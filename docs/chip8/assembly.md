# The CHIP-8 assembler

Time Warp Studio assembles CHIP-8 listings: readable text that becomes the bytes a
1977 machine would have been given. This book is the assembler's language. The
[instruction set](instruction-set.md) says what the assembled bytes do; this says
how to write them.

A listing is also the format the Studio ships its own CHIP-8 programs in
(`Examples/chip8/*.ch8`), which is why they can be read, taught and edited rather
than being opaque blobs.

---

## A listing

```text
; paint.ch8 -- a one-pixel paint program.
;
;   2 / 8 / 4 / 6   move the cursor up / down / left / right
;   5               paint the pixel under the cursor
;   0               clear the canvas

start:  CLS
        LD V0, 32           ; x
        LD V1, 16           ; y
loop:   LD I, cursor
        DRW V0, V1, 1       ; show the cursor
        LD V2, K            ; wait for a key
        ...
cursor: DB 0x80
```

One instruction or directive per line. A line is:

```text
[label:] [MNEMONIC operand[, operand ...]] [; comment]
```

### Comments

Everything from `;` to the end of the line is ignored.

One comment is not a comment: a line whose text begins `; keys` seeds the initial
keypad for the whole-program run — see [seeding the keypad](#seeding-the-keypad).

### Labels

A label is `name:` at the start of a line, optionally followed by an instruction:

```text
loop:   DRW V0, V1, 1
target:
        JP target
```

- The name starts with a letter or `_`, continues with letters, digits or `_`.
- Labels are **case-sensitive**; mnemonics and registers are not.
- A label is the address of what follows it. Referencing one is how you write a
  jump that survives editing the lines above it.
- A label defined twice is an error naming the line and the name.

### Operands

Separate operands with commas, spaces, or both. `DRW V0, V1, 1`,
`DRW V0 V1 1` and `DRW V0,V1,1` are the same instruction.

Case is free: `ld v0, 5` is `LD V0, 5`.

### Numbers

| Spelling | Value |
| --- | --- |
| `255` | decimal |
| `0xFF`, `0XFF` | hexadecimal, either `x` |
| `FFh`, `0ffH` | hexadecimal, `h` suffix |
| `0x200`, `512`, `200h` | the address 512 |

A byte operand must be 0–255; a word operand 0–65535. A negative value is refused
rather than wrapped, because a listing with a negative byte is a listing with a
mistake.

### Registers

`V0` through `VF`, any case. Two hex digits after the `V` is a named register,
never a register followed by an operand: `V10` is not `V1` and `0`.

---

## Directives and pseudo-instructions

| Form | Bytes | Meaning |
| --- | --- | --- |
| `DB value` | 1 | One byte of data, exactly as given |
| `DW value` | 2 | One 16-bit word, big-endian: high byte first |
| `HALT` | 2 | `0x0000` — stop the machine |
| `SYS address` | 2 | `0NNN` — an RCA 1802 call, which every interpreter since 1977 ignores |

`DB` is how sprites and tables are written. `DW` is the escape hatch for an
opcode the assembler has no mnemonic for — the SuperChip screen opcodes are the
usual case:

```text
        DW 0x00FF           ; hi-res: 128x64
        DW 0x00C2           ; scroll down 2
```

`HALT` is an addition, not a 1977 instruction: `0x0000` is unused by the original
set, so a program can stop cleanly instead of spinning on a jump-to-self to mark
its own end. The machine treats it as "halted, successfully".

---

## Instruction forms

| Mnemonic | Forms | Encoding |
| --- | --- | --- |
| `CLS` | `CLS` | `00E0` |
| `RET` | `RET` | `00EE` |
| `JP` | `JP addr` | `1nnn` |
| `JP` | `JP V0, addr` | `Bnnn` (V0 is added at run time; another register is refused) |
| `CALL` | `CALL addr` | `2nnn` |
| `SE` | `SE Vx, byte` / `SE Vx, Vy` | `3xkk` / `5xy0` |
| `SNE` | `SNE Vx, byte` / `SNE Vx, Vy` | `4xkk` / `9xy0` |
| `LD` | `LD Vx, byte` / `LD Vx, Vy` | `6xkk` / `8xy0` |
| `LD` | `LD I, addr` | `Annn` |
| `LD` | `LD Vx, DT` | `Fx07` |
| `LD` | `LD Vx, K` | `Fx0A` |
| `LD` | `LD DT, Vx` / `LD ST, Vx` | `Fx15` / `Fx18` |
| `LD` | `LD F, Vx` | `Fx29` |
| `LD` | `LD B, Vx` | `Fx33` |
| `LD` | `LD [I], Vx` / `LD Vx, [I]` | `Fx55` / `Fx65` |
| `ADD` | `ADD Vx, byte` / `ADD Vx, Vy` | `7xkk` / `8xy4` |
| `ADD` | `ADD I, Vx` | `Fx1E` |
| `OR` | `OR Vx, Vy` | `8xy1` |
| `AND` | `AND Vx, Vy` | `8xy2` |
| `XOR` | `XOR Vx, Vy` | `8xy3` |
| `SUB` | `SUB Vx, Vy` | `8xy5` |
| `SUBN` | `SUBN Vx, Vy` | `8xy7` |
| `SHR` | `SHR Vx` (or `SHR Vx, Vy`) | `8xy6` |
| `SHL` | `SHL Vx` (or `SHL Vx, Vy`) | `8xyE` |
| `RND` | `RND Vx, byte` | `Cxkk` |
| `DRW` | `DRW Vx, Vy, rows` | `Dxyn`, `rows` 0–15 |
| `SKP` | `SKP Vx` | `Ex9E` |
| `SKNP` | `SKNP Vx` | `ExA1` |

Two deliberate leniencies, for listings written for later machines:

- `SHR`/`SHL` accept a `Vy` operand and ignore it. This machine shifts `Vx` in
  place, as 1977 did; a listing that names the source register still assembles.
- `JP` with a register names `V0` only, and refuses anything else. `Bnnn` has no
  field for a register, so accepting `JP V5, addr` would silently ignore the `V5`.

---

## Seeding the keypad

The live window's keypad is a hands-on thing. The editor's whole-program run has
no hands, so a program that waits for a key would see nothing. A listing can seed
the keypad with a comment:

```text
; keys 5
start:  LD V0, K
        ...
```

Any line whose first non-space text is `; keys` contributes every hexadecimal
digit after it to the initial key mask. `; keys 1F` holds keys `1` and `F`.
Several such lines combine. The scan is literal — every hex digit on the rest of
the line counts — so write the mask last (`; keys -- 1F` is fine, `; keys F and 1`
is not what it looks like). It costs nothing at run time and is ignored by the
machine once the program starts — it is a testing and demonstration aid, not an
input mechanism for people. Use the window for that.

---

## What the assembler refuses

Errors are reported with the line they came from, and an assembly error leaves a
running program alone.

| Message | Cause |
| --- | --- |
| `line N: MNEMONIC takes …` | Wrong operand count or kind |
| `line N: MNEMONIC is not an instruction form this assembler knows` | Unknown mnemonic |
| `line N: JP uses V0 only; Bnnn adds V0` | `JP Vx, addr` with `x ≠ 0` |
| `line N: DRW draws at most 15 rows` | `DRW` with `rows` above 15 |
| `line N: label NAME is defined twice` | Duplicate label |
| `'text' is not a number` | Bad literal |
| `text is not a byte` / `… is not a 16-bit value` | Out of range |
| `'text' is not a register (V0-VF)` | Bad register |
| `the program needs N bytes and memory ends at 0x1000` | Listing too large |
| `the ROM is N bytes; memory from 0x200 holds 3584` | Binary ROM too large |

At run time the machine adds:

| Message | Cause |
| --- | --- |
| `RET with an empty call stack` | `RET` without a `CALL` |
| `unknown opcode 0xNNNN at 0xNNN` | A `DW` or binary byte that is not an instruction |
| `unknown arithmetic opcode 8xyN` / `unknown opcode FxNN` | Same, inside those families |
| `the program counter ran past the end of memory` | Fell off the end of the listing |

The exact spelling of these messages is asserted by the engine's tests; they are
the text a learner reads, so they are part of the interface.

---

## A worked listing

```text
start:  LD V0, 5
        LD V1, 5
        LD I, sprite
        DRW V0, V1, 1
        HALT
sprite: DB 0x80
```

Assembled, byte by byte:

| Address | Bytes | From |
| --- | --- | --- |
| `0x200` | `60 05` | `LD V0, 5` |
| `0x202` | `61 05` | `LD V1, 5` |
| `0x204` | `A2 0A` | `LD I, sprite` — the label is at `0x20A` |
| `0x206` | `D0 11` | `DRW V0, V1, 1` |
| `0x208` | `00 00` | `HALT` |
| `0x20A` | `80` | `sprite: DB 0x80` |

That is a single lit pixel at column 5, row 5, and a clean halt. It is also the
smallest useful program in the book: load registers, point `I` at data, draw,
stop.

A larger example that uses almost everything — labels, sprites, `DB`, `DW`, the
font, hi-res and scrolling — is
[`Examples/chip8/hires.ch8`](../../Examples/chip8/hires.ch8).

---

## Assembly is two passes

1. **Layout.** Walk the lines, record every label's address, and size every
   statement (`DB` is one byte, everything else two). A label can therefore be
   used before it is defined.
2. **Encode.** Walk again and write the bytes, resolving labels.

That is why a label on its own line costs nothing, and why an instruction that
needs a label defined later still assembles. It is also why the listing's size is
known before any byte is written — the memory-limit error is a layout error, not a
surprise at run time.

---

## Writing a program to run in the window

1. Open Tools > **CHIP-8 Emulator…**. `paint.ch8` starts.
2. Switch to the **Listing** tab and replace the text.
3. `Ctrl+Return` — **Assemble & Load**. Errors appear above the button with their
   line numbers; the running program is untouched.
4. **Run**, then **Step**, and watch the Registers tab.
5. Save the file as `.ch8`. The next `Open ROM…` assembles it again — a listing is
   the file, not a build product.

To ship the program as a binary instead, assemble it once and write the bytes; the
reader accepts either, because the NUL byte decides.

---

## Where next

- [The instruction set](instruction-set.md) — what each encoding does.
- [The machine](README.md) — the window, the keys, the inspector.
- [`Examples/chip8/`](../../Examples/chip8/) — four complete listings.
