# The CHIP-8 instruction set, as implemented

This is not a general CHIP-8 document. It is the reference for **this** machine:
the exact 1977 choices, the SuperChip extensions it also accepts, and the places
where it deliberately refuses to be helpful. Where emulators disagree, this book
says what this one does.

- How to write these opcodes: [the assembler](assembly.md).
- Where they run: [the machine](README.md).
- Why the quirks are kept: [`chip8.hpp`](../../native/include/timewarp/lang/chip8.hpp).

---

## The machine

| Part | Size | Notes |
| --- | --- | --- |
| Memory | 4096 bytes | Programs at `0x200`; the screen is **not** memory-mapped |
| `V0`–`VF` | 16 × 8 bits | `VF` is the flag register |
| `I` | 12 bits | An address; wraps at `0xFFF` where it is incremented |
| `PC` | 12 bits, in effect | Points at the next instruction |
| Stack | As deep as the calls nest | Holds return addresses; `RET` on an empty stack is an error |
| Delay timer | 8 bits | Counts down at 60 Hz |
| Sound timer | 8 bits | Counts down at 60 Hz; the buzzer sounds while it is non-zero |
| Display | 64×32, or 128×64 hi-res | One bit per pixel, in its own buffer |
| Keypad | 16 keys | Read as a bit mask |

### Memory map

| Range | Holds |
| --- | --- |
| `0x000`–`0x04F` | Interpreter work area; nothing here is used |
| `0x050`–`0x09F` | The 4×5 font: sixteen glyphs (0–9, A–F), five bytes each |
| `0x0A0`–`0x13F` | The 8×10 font that `FX30` selects: sixteen glyphs, ten bytes each |
| `0x140`–`0x1FF` | Unused |
| `0x200`–`0xFFF` | Programs |

The 4×5 glyphs are widened to 8 pixels when they are built, so `DRW` reads them
like any other sprite. The 8×10 face is a SuperChip addition and is drawn as an
ordinary 8-row sprite ten rows tall — `DRW Vx, Vy, 10`, not `0`.

---

## Timing

One instruction is fetched, decoded and executed; `PC` then points at the next
one. Skip instructions advance `PC` by two extra bytes.

Two clocks exist:

| Context | Clock |
| --- | --- |
| The editor's whole-program run | Ten opcodes per frame at 60 Hz, timers tick once per frame, up to 600 frames |
| The CHIP-8 window | 1–2000 opcodes per frame at 60 Hz (default 10), timers tick once per frame |

Ten opcodes per frame is the original interpreter's pace: roughly 600 instructions
a second, which is why 1977 programs look the way they do. The timers are frame
counters, not wall-clock stopwatches, so a program that changes speed changes how
fast its timers run — exactly as on the machines this imitates.

`LD Vx, K` (`FX0A`) is the one instruction that can take longer than its frame. In
the window it waits, leaving `PC` on the instruction until a key is held. In a
whole-program run there is no way to deliver a later press, so it reads the key
mask once and moves on — see [seeding the keypad](assembly.md#seeding-the-keypad).

---

## Reading an opcode

An opcode is two bytes, read high first. Write it as `NNNN` and its parts are:

```text
N N N N
│ │ │ └─ nnn / NNN: a 12-bit address, or an 8-bit constant
│ │ └─── y: a register number, 0–F
│ └───── x: a register number, 0–F
└─────── the family
```

So `Dxyn` is "family D, register x, register y, n rows", and `D0 11` is
`DRW V0, V1, 1`.

---

## The opcodes

### `0nnn` — machine-level

| Opcode | Mnemonic | Behaviour |
| --- | --- | --- |
| `00E0` | `CLS` | Clear the display at the current grid size |
| `00EE` | `RET` | Pop the stack into `PC`; an empty stack is an error |
| `0000` | `HALT` | Stop the machine, successfully — this machine's own extension |
| `0NNN` | `SYS addr` | **Ignored.** It called RCA 1802 code, which nothing here can run |
| `00Cn` | — | Scroll the display down `n` rows; the top is cleared |
| `00FB` | — | Scroll right 4 pixels; the left gap is cleared |
| `00FC` | — | Scroll left 4 pixels; the right gap is cleared |
| `00FD` | — | Exit the interpreter: halt, successfully |
| `00FE` | — | Switch to 64×32 lores and clear |
| `00FF` | — | Switch to 128×64 hi-res and clear |

`SYS` is accepted by the assembler and does nothing at run time. Every interpreter
since the original ignores it, and a ROM containing one stays runnable.

Selecting a mode always clears the screen, even when the machine is already in
that mode: a ROM that selects its mode expects a blank grid.

### Jumps and calls

| Opcode | Mnemonic | Behaviour |
| --- | --- | --- |
| `1nnn` | `JP addr` | `PC = addr` |
| `2nnn` | `CALL addr` | Push `PC`, then `PC = addr` |
| `Bnnn` | `JP V0, addr` | `PC = addr + V0` |
| `00EE` | `RET` | Return from a call |

`Bnnn` adds `V0` at run time. The assembler refuses `JP Vx, addr` for any other
register, because the encoding has nowhere to put it.

### Skips

| Opcode | Mnemonic | Skips the next instruction when |
| --- | --- | --- |
| `3xkk` | `SE Vx, byte` | `Vx == kk` |
| `4xkk` | `SNE Vx, byte` | `Vx != kk` |
| `5xy0` | `SE Vx, Vy` | `Vx == Vy` |
| `9xy0` | `SNE Vx, Vy` | `Vx != Vy` |
| `Ex9E` | `SKP Vx` | the key named by `Vx` is held |
| `ExA1` | `SKNP Vx` | it is not |

A skip advances `PC` by two bytes — over one two-byte instruction. A `DB` after a
skip is still one byte, so a listing that puts data inline after a skip is not
guaranteed the layout it expects; put data after the code.

### Loads

| Opcode | Mnemonic | Behaviour |
| --- | --- | --- |
| `6xkk` | `LD Vx, byte` | `Vx = kk` |
| `8xy0` | `LD Vx, Vy` | `Vx = Vy` |
| `Annn` | `LD I, addr` | `I = addr` |
| `Fx07` | `LD Vx, DT` | `Vx = delay timer` |
| `Fx0A` | `LD Vx, K` | Wait for a key; `Vx` = the lowest held key. Whole-program runs read the mask once |
| `Fx15` | `LD DT, Vx` | Delay timer = `Vx` |
| `Fx18` | `LD ST, Vx` | Sound timer = `Vx` |
| `Fx29` | `LD F, Vx` | `I` = the 4×5 glyph for `Vx & 0xF` |
| `Fx30` | — | `I` = the 8×10 glyph for `Vx & 0xF` (SuperChip) |
| `Fx33` | `LD B, Vx` | Write `Vx` as three decimal digits at `I`, `I+1`, `I+2` |
| `Fx55` | `LD [I], Vx` | Write `V0`–`Vx` to memory at `I` |
| `Fx65` | `LD Vx, [I]` | Read `V0`–`Vx` from memory at `I` |
| `Fx75` | — | Save `V0`–`Vx` to the SuperChip RPL flags |
| `Fx85` | — | Restore them |

`FX55` and `FX65` leave `I` **one past the block** (`I += x + 1`), as the 1977
interpreter does. CHIP-48 and SCHIP leave `I` where it was; programs exist for
both, and this machine is the older one.

### Arithmetic and logic

| Opcode | Mnemonic | Behaviour | `VF` |
| --- | --- | --- | --- |
| `7xkk` | `ADD Vx, byte` | `Vx += kk` | unchanged |
| `8xy1` | `OR Vx, Vy` | `Vx \|= Vy` | unchanged |
| `8xy2` | `AND Vx, Vy` | `Vx &= Vy` | unchanged |
| `8xy3` | `XOR Vx, Vy` | `Vx ^= Vy` | unchanged |
| `8xy4` | `ADD Vx, Vy` | `Vx += Vy` | 1 on carry past 255 |
| `8xy5` | `SUB Vx, Vy` | `Vx -= Vy` | 1 when `Vx >= Vy` (no borrow) |
| `8xy7` | `SUBN Vx, Vy` | `Vx = Vy - Vx` | 1 when `Vy >= Vx` |
| `8xy6` | `SHR Vx` | `Vx >>= 1` | the bit shifted out |
| `8xyE` | `SHL Vx` | `Vx <<= 1` | the bit shifted out |
| `Fx1E` | `ADD I, Vx` | `I = (I + Vx) & 0xFFF` | unchanged |

The shifts are **in place on `Vx`**, as in 1977, and the `y` nibble is ignored.
The assembler accepts `SHR Vx, Vy` for compatibility and still assembles `8xy6`.

`FX1E` does not set `VF` on overflow; some later interpreters do. This one is the
1977 one.

### Drawing

| Opcode | Mnemonic | Behaviour |
| --- | --- | --- |
| `Dxyn` | `DRW Vx, Vy, n` | Draw an 8×`n` sprite from `I` at (`Vx`, `Vy`) |

- The sprite is XORed onto the screen. Any lit pixel that turns off sets `VF = 1`;
  a frame with no collisions clears it first.
- Coordinates **wrap**: `x = Vx % width`, `y = Vy % height`. A sprite drawn at
  column 63 continues at column 0.
- `n = 0` is SuperChip's large sprite: sixteen rows. In hi-res each row is two
  bytes (16 pixels); in lores each of the eight columns is doubled, which is the
  8×16 shape a lores ROM expects.
- The screen is its own buffer, not part of the 4 KiB address space, so drawing
  can never overwrite a program.

### Random

| Opcode | Mnemonic | Behaviour |
| --- | --- | --- |
| `Cxkk` | `RND Vx, byte` | `Vx = randomByte & kk` |

The generator is seeded from the operating system once per run, so two runs
differ. It is the same generator the reference implementation uses, accessed
through the same interface.

---

## Fields this machine does not have

| Thing | Status |
| --- | --- |
| The RCA 1802 (`SYS`) | Ignored |
| Quirk switches (`VF` reset on logic ops, `I` unchanged after `FX55`, clipping instead of wrapping) | None; the machine is 1977, not configurable |
| XO-CHIP (planes, audio pattern, `F000 nnnn`, long `I`) | Not implemented |
| `00Cn` in lores | Implemented; scrolls the 64×32 grid |
| Two-player keypads, gamepad extensions | Not implemented |

An unsupported opcode is an error naming it and the address, not a silent no-op.
Silence is how a program becomes impossible to debug.

---

## Worked disassembly

The worked listing from [the assembler](assembly.md#a-worked-listing) is six
statements. Read back:

```text
0x200  60 05   LD V0, 5          V0 = 5
0x202  61 05   LD V1, 5          V1 = 5
0x204  A2 0A   LD I, 0x20A       I points at the sprite
0x206  D0 11   DRW V0, V1, 1     XOR one row: 0x80 at column 5, row 5
0x208  00 00   HALT              this machine's stop
0x20A  80      (data)            the sprite: bit 7 set
```

And a SuperChip sequence, from `hires.ch8`:

```text
0x200  00 E0   CLS               clear at 64x32
0x202  00 FF   (hi-res)          switch to 128x64, clearing again
0x204  60 08   LD V0, 8
0x206  61 04   LD V1, 4
0x208  A2 xx   LD I, ring
0x20A  D0 10   DRW V0, V1, 0     16x16 sprite: two bytes a row in hi-res
0x20C  00 C2   (scroll down 2)
0x20E  00 FC   (scroll left 4)
```

That is the whole vocabulary of the machine in sixteen bytes: clear, choose a
grid, point at data, draw, move the world.

---

## Where this is proven

| Suite | What it pins |
| --- | --- |
| `lang_tests` | The assembler, the machine, every flag, the skips, the wrap, the fonts, hi-res grids, `CLS` in both modes |
| `chip8_emulator_tests` | The window's machine: loading, stepping, the clock, the keypad, sound, the ROM library |
| `make golden-check` | Every CHIP-8 program in the recorded corpus, compared with the reference's answer byte for byte |

The quirks in this book are not folklore: each one is asserted, because a quirk
nobody tests is a bug waiting to be reported as a feature.
