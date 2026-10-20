# CHIP-8 Machine Manual

```text
 CHIP-8  ·  4096 BYTES  ·  16 REGISTERS  ·  64×32  ·  16 KEYS
      1977 — the computer that was never a computer
```

CHIP-8 is the other machine in Time Warp Studio, and the older one. It was never
hardware: it was a virtual machine written in 1977 so that the same small program
could run on a COSMAC VIP, a Telmac, an ETI-660 and everything after them. Four
kilobytes of memory, sixteen eight-bit registers, a twelve-bit address register,
a stack, two timers, a sixty-four by thirty-two monochrome screen and sixteen
keys — and from that, games, tools and a small culture.

It is in the Studio because it is the smallest complete computer worth calling
one, and because a learner who has watched a `DRW` light a pixel has understood
something no amount of prose can supply.

- **The sibling machine:** [the HB-1](../hb1/README.md) · [the machines](../MACHINES.md)
- **The rest of this book:** [the assembler](assembly.md) ·
  [the instruction set](instruction-set.md)

---

## Two ways to run a program

The Studio runs CHIP-8 in two places, and they are different on purpose.

| | The editor | The CHIP-8 window |
| --- | --- | --- |
| Open | Any `.ch8` document, then **Run** (F9) | Tools > **CHIP-8 Emulator…** (`Ctrl+Shift+M`) |
| Model | A whole program, run to completion | A live machine, sixty frames a second |
| Speed | Ten opcodes per frame, 600 frames — 60 seconds of machine time | 1 to 2000 opcodes per frame, adjustable while running |
| Keys | The `; keys` line seeds the keypad; `LD Vx, K` reads it once | A real keypad, changing under the program's hands |
| Output | The framebuffer is drawn on the turtle canvas | A dedicated 64×32 screen |
| Debugging | Gutter breakpoints; the machine pauses before an instruction | Run, Pause and single **Step**, with a register and memory inspector |
| Sound | The `sound_timer` is not played | A buzzer follows the sound timer; mute with one click |

Use the editor to check that a listing assembles, to demonstrate an instruction,
or to see it in a lesson. Use the window to play, to paint, to watch registers
change at human speed — and whenever the program waits for a key.

Both run the same assembler and the same `Chip8Machine`. The only difference is
who owns the clock.

---

## The window

```text
┌─────────────────────────────────────────────────────────────────┐
│ [▶ Run] [Step] [Reset]  [Assemble & Load]  [Open ROM…] [ROM Library…] │
│ Speed [ 10 ] opcodes/frame   ☐ Mute                             │
├───────────────────────────────┬─────────────────────────────────┤
│                               │  Registers │ Memory │ Listing  │
│      64 × 32 screen           │  V0  00  0  …                   │
│      (128 × 64 in hi-res)     │  I   0x200                      │
│                               │  PC  0x200                      │
│                               │  …                              │
├───────────────────────────────┤                                 │
│  1 2 3 C                      │                                 │
│  4 5 6 D                      │                                 │
│  7 8 9 E                      │                                 │
│  A 0 B F                      │                                 │
├───────────────────────────────┴─────────────────────────────────┤
│ ▶ running   PC 0x202 · 41 cycles · 10 opcodes/frame (600 Hz)    │
└─────────────────────────────────────────────────────────────────┘
```

- **Run / Pause** — starts and stops the clock. The label changes with the state.
- **Step** — one instruction, while paused. The inspector updates with it.
- **Reset** — rebuilds the machine from the loaded program. The program is kept,
  so this is the button a learner wants after a game ends.
- **Assemble & Load** (`Ctrl+Return`) — assembles the listing in the Listing tab
  and runs it. A listing that does not assemble leaves the running program alone
  and prints the error in the tab.
- **Open ROM…** — a file dialog for `.ch8` and `.c8`.
- **ROM Library…** — browses a folder of programs (below).
- **Speed** — opcodes executed per 60 Hz frame. Ten is the original pace; 2000
  is as fast as one frame may run without monopolising the window.
- **Mute** — silences the buzzer without stopping the timer.

The status line is the machine readout: state, program counter, cycles executed,
and the speed. States are `open a program`, `running`, `paused`, `halted`,
`waiting for a key` and `error`.

You can drag a `.ch8` or `.c8` file onto the window. Files dropped are still
subject to the rule below: a `.ch8` may be a listing or a binary ROM.

### The screen

Sixty-four by thirty-two pixels, each scaled with nearest-neighbour so a lit pixel
stays a square. In SuperChip hi-res the grid becomes 128×64 — the same 2:1 shape
with every pixel half the size — and the widget replaces the image rather than
rejecting it for the wrong pixel count.

### The keypad

The pads are mirrored. Click them, or use the PC keyboard:

| PC keys | `1 2 3 4` | `Q W E R` | `A S D F` | `Z X C V` |
| --- | --- | --- | --- | --- |
| CHIP-8 keys | `1 2 3 C` | `4 5 6 D` | `7 8 9 E` | `A 0 B F` |

The keypad is where a learner's hands already are, so keys reach the machine
without clicking first. A focus change releases every key: the machine can never
be left believing a key is down for the rest of the run.

### The inspector

| Tab | Shows |
| --- | --- |
| Registers | `V0`–`VF`, `I`, `PC`, the delay and sound timers, the cycle count, and the call stack |
| Memory | The whole 4 KiB, refreshed per frame |
| Listing | Your source, with a status line and **Assemble & Load** |

The snapshot is taken once per frame, so the inspector never reads a
half-executed instruction.

The CHIP-8 window has no breakpoints of its own: use **Step**. Gutter breakpoints
belong to the editor's whole-program Run, where the machine pauses before the
instruction whose listing line carries the breakpoint.

### Sound

The buzzer sounds while the sound timer is non-zero, and stops when the machine
pauses, halts or is reset. It is the same `SoundPlayer` the rest of the Studio
uses.

---

## Loading programs

A CHIP-8 file is one of two things, decided by its bytes:

| The file | What happens |
| --- | --- |
| **A listing** — text, no NUL byte | Assembled, then copied into memory at `0x200` |
| **A binary ROM** — contains a NUL byte | Copied byte for byte to `0x200`, up to 3584 bytes |

The test is the NUL byte because a listing never contains one and a real ROM
almost always does. A binary ROM has no listing to show, so the Listing tab says
so and invites you to replace it.

A ROM larger than 3584 bytes is refused with its actual size and the limit.

### The ROM library

`ROM Library…` points at a folder you choose and lists every `.ch8`, `.c8`, `.rom`
and `.bin` under it, recursively, sorted by the path inside the folder — so a
`games/` and a `hires/` collection stay grouped. A filter narrows the list. The
folder is remembered (`chip8/romFolder`), and the dialog starts at your home
directory until you pick one.

**The Studio ships no third-party ROMs.** Most CHIP-8 programs that circulate
online are somebody else's copyright, and several are unlicensed ports of games
that still are; bundling them would be redistribution the project has no right to.
The library is the lawful half of the problem: you point it at a folder you have
the right to use.

### What is bundled

The programs under [`Examples/chip8/`](../../Examples/chip8/) are listings
written for this project, and they are the ones you can open without any
download:

| Program | What it shows |
| --- | --- |
| `paint.ch8` | A one-pixel paint program: waiting for keys, XOR drawing, wrap-around coordinates |
| `font_and_frames.ch8` | The built-in digit font, drawn and tested |
| `hires.ch8` | SuperChip hi-res, scrolling, the 16×16 sprite and the large font |
| `showcase.ch8` | The surface of the machine in one program |

Opening the CHIP-8 window with a CHIP-8 document on screen loads that document. With
any other document — or none — it loads `paint.ch8` and starts it, so the machine
is alive the moment it appears. If that file is not installed,
`font_and_frames.ch8` is the fallback; if neither is there, the window opens with
an empty listing rather than an error. A window that already has a program is
never overwritten: the learner may have typed into the listing or reached a state
worth keeping.

---

## What the machine is

| | |
| --- | --- |
| Memory | 4096 bytes; programs start at `0x200` |
| Font | The 4×5 digits at `0x50`–`0x9F`, and the 8×10 face that `FX30` selects |
| Registers | `V0`–`VF`, eight bits each; `VF` is the flag register |
| Address register | `I`, twelve bits |
| Stack | As deep as the calls nest; `RET` on an empty stack is an error |
| Timers | Delay and sound, sixty ticks a second |
| Display | 64×32 lores, 128×64 SuperChip hi-res |
| Keys | Sixteen, read as a mask |

The quirks and the exact opcode behaviour are in
[the instruction set](instruction-set.md). The short version is that this machine
is the 1977 one: Vx shifts in place, sprites wrap at the edges, `FX55`/`FX65`
leave `I` one past the block, and `SYS` does nothing because nothing here can call
an RCA 1802.

---

## Limits, stated plainly

| Limit | Value | At the edge |
| --- | --- | --- |
| Memory | 4096 bytes | A listing that needs more refuses to assemble |
| ROM from a file | 3584 bytes (`0x200`–`0xFFF`) | Refused with the size and the limit |
| Registers | 16 × 8 bits, one 12-bit `I` | Opcodes mask, as the machine always did |
| Whole-program run | 600 frames, 1 000 000 opcodes | A frame-limit stop is reported as information, not an error |
| Window speed | 1–2000 opcodes per 60 Hz frame | Clamped by the spin box |
| Keys | 16 | There is no seventeenth |

A program that never halts is a legitimate CHIP-8 program — an animation, a game
waiting for a key. The editor therefore reports the 600-frame stop with `ℹ️` and
says the program had not halted, rather than calling it a failure.

---

## Where next

- Write a listing: [the assembler](assembly.md).
- Read the opcodes: [the instruction set](instruction-set.md).
- Learn the language: [tutorials/chip8.md](../tutorials/chip8.md) and
  [LANGUAGE_GUIDE.md](../LANGUAGE_GUIDE.md#chip-8).
- Compare machines: [the HB-1](../hb1/README.md).
