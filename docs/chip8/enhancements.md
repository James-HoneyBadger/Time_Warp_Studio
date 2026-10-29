# CHIP-8 enhancements

Time Warp Studio 24.0 is the golden release: the machine described in
[the machine manual](README.md), its [assembler](assembly.md) and its
[instruction set](instruction-set.md) are finished, frozen and validated by the
gates in [README_TESTING](../README_TESTING.md). Everything here is a **proposal for
after 24.0** — written as a specification with a way to prove each item, so a
future release has something to build to.

The machine's own doctrine stays: **one machine, one manual, no quirk switches.**
Every proposal below either adds a tool around the machine or completes a documented
part of it. None of them changes what an existing opcode does.

---

## What the machine is missing, honestly

The CHIP-8 is complete as a 1977 machine: four kilobytes, sixteen registers, a
stack, two timers, a 64×32 screen, sixteen keys, an assembler, a live window and a
rewind. What it lacks is the apparatus a *person* needs to study it:

| Gap | Why it matters |
| --- | --- |
| Breakpoints exist in the editor, not in the window | A learner watching a game in the window can Step, but cannot say "stop at 0x2A4" |
| There is no trace | The inspector shows the registers *now*; nothing shows how they got that way |
| Memory is a viewer, not a watch | A program that writes one byte per frame is invisible in a refreshed hex dump |
| A machine state can be rewound but not kept | Rewind is thirty seconds; a lesson needs "this is the state at the collapse" tomorrow |
| The ROM library lists file names | A folder of ROMs has no titles, authors or notes |
| A SuperChip program can use `00Cn`/`00FB` but the manual calls it a partial set | The documented subset is a list rather than a compatibility statement |

---

## Functionality proposals

### 1. Breakpoints in the window

**What.** A gutter and a PC field in the Listing tab, and a `0xNNN` entry beside
**Step** that arms a breakpoint. Run stops before the instruction at that address,
with the inspector showing the state it is about to run from. `Run to here` from the
listing's context menu sets a temporary one.

**Why.** The editor already pauses before an instruction whose listing line carries
a gutter breakpoint, and the machine underneath is the same `Chip8Machine`. The
window is where a learner watches a game, so it is where the breakpoint belongs.
It also completes the rewind story: a rewind lands on an instruction, and a
breakpoint is how a reader chooses which instruction matters.

**What changes.** `Chip8Window` keeps an address set and calls the emulator's
existing step path instead of the run loop when it is armed; the emulator gains
`set_breakpoint(int)` and `at_breakpoint()`. Nothing in `Chip8Machine` changes.

**Proof.** `chip8_emulator_tests` asserts that a breakpoint fires before the
instruction, that the PC equals the address, that Continue reaches it again, and
that Reset clears armed breakpoints while keeping the program.

### 2. An instruction trace

**What.** A **Trace** tab holding the last sixty-four instructions: address,
disassembly, and the registers the instruction changed. A row can be clicked to
open the memory at its address.

**Why.** Stepping shows one instruction; a trace shows the loop that made the
register what it is. It is the difference between a debugger and a logic analyser,
and it is the single most useful thing a reader can have when a sprite flickers.

**What changes.** The emulator records into a ring buffer of
`{pc, opcode, before, after}`; the window formats it. The buffer is off unless the
tab is open, because a machine that allocates sixty-four rows a frame is a machine
that slowed down to teach.

**Proof.** The trace shows the exact instruction sequence for a five-instruction
program; a register delta is right for `ADD`, `XOR` and `DRW` (including `VF`); the
ring buffer drops the oldest row and keeps the newest.

### 3. Memory watch

**What.** Addresses added to a watch list, with the previous value beside the
current one and a change highlighted for one frame. Watches are saved with the ROM
library entry.

**Why.** `FX55` writes a block, `FX33` writes three digits, and either is invisible
in a scrolling hex dump. A watch is how a reader sees a byte move.

**What changes.** The Memory tab gains a watch pane over the same `memory()`
accessor; the emulator exposes a frame counter so a change can be attributed to a
frame.

**Proof.** A watched address shows a change on the frame `FX55` runs and no change
on the frames around it; a watch on ROM space is refused with a sentence.

### 4. State snapshots as files

**What.** **Save state…** and **Load state…** writing a documented `.c8s`: the
`Chip8State`, the screen mode, the frame count, the loaded listing's name and the
assembler's source. Loading a state restores the machine *and* the listing it came
from.

**Why.** Rewind keeps thirty seconds in memory; a lesson needs the state at the
moment of the bug, kept in a folder with the program. The struct already exists —
`Chip8State` is what the rewind checkpoint carries — so this is a file format
rather than a second mechanism.

**What changes.** A serializer beside `chip8_emulator.cpp`, a menu entry on the
window, and a manual section for the format. The JSON carries a version and the
source's SHA-256, so a state that does not match the listing says so instead of
restoring pixels over the wrong program.

**Proof.** A round trip restores the framebuffer byte for byte and the registers
exactly; a state loaded against an edited listing reports the mismatch and offers
the listing it came with; a state from a newer format version is refused by name.

### 5. A ROM library with titles

**What.** An optional `library.json` in the folder the library browses, mapping a
file to a title, an author, a year and a note. The library shows the title and the
note; a folder without the file keeps working exactly as now.

**Why.** The project ships no third-party ROMs for copyright reasons, so the folder
is the user's own — and a folder of `game1.ch8` … `game9.ch8` is what that looks
like. The manifest is the difference between a file list and a collection, and it is
also where a per-program speed preset belongs.

**What changes.** `chip8_library.cpp` reads the manifest if present; a missing or
damaged manifest falls back to file names rather than refusing to browse.

**Proof.** A manifest's titles and notes appear in that order; a file with no entry
shows its own name; a damaged manifest is ignored and the list is still usable.

### 6. Completing the SuperChip subset

**What.** Plane 1 and 2 (`00FC`/`00FD`), the scroll amounts `00C0`–`00CF` and
`00FB`/`00FC` with a documented pixel count, and `DRW` with `N=0` in both screen
modes — one 16×16 sprite in hi-res, one 8×16 sprite in lo-res, stated once.

**Why.** The machine already implements `00FF`, `00FE`, `00FB`, `00FC`, `00C4`
and `F030`; the subset is real but scattered through the manual. Completing it
means a SuperChip listing runs without a paragraph explaining what will not work.
The doctrine is kept because there is still no switch: one machine, one defined set
of opcodes, described in one table.

**What changes.** Three opcodes in the `Chip8Machine` dispatch and their rows in
[the instruction set](instruction-set.md). The assembler gains `PLANE`, `SCROLL`
or keeps `DW` — the existing listings use `DW` and stay valid either way.

**Proof.** Each opcode has a row in `native/share/command_rows.json` and a case in
`lang_tests`; plane 1 pixels do not appear in plane 0; a scroll amount of zero
changes nothing.

### 7. Assembler ergonomics

**What.** `EQU` constants, `.org` for data placement, `DB "text"` for strings, and
`REPT n … ENDR` for repeated data.

**Why.** A sprite table today is thirty `DB` lines, and an address today is a
number a reader cannot name. These four forms are what an assembler of the period
had, they change no encoding, and they make a listing teachable.

**What changes.** The two-pass assembler in `native/src/lang/chip8.cpp` gains four
directives; every existing listing assembles unchanged, which is the compatibility
test.

**Proof.** The whole `Examples/chip8/` folder assembles to the same bytes as before
(a byte-for-byte comparison in `lang_tests`); `EQU` used before definition works
because of the two-pass layout; an unknown constant is an error naming the line.

### 8. Hands that reach the machine

**What.** A remappable PC keyboard map, and a gamepad binding for the sixteen keys
through the `GAMEPAD` support the Studio already has.

**Why.** The keypad's PC mapping is fixed and documented; a learner with one of the
many modern controllers lying beside the keyboard expects it to work, and the
Studio already reads one for other languages.

**What changes.** A binding table in `Chip8Window`, saved in QSettings, with the
current map as the default so nothing moves for an existing user.

**Proof.** A remapped key reaches the machine as the same key bit; a focus change
still releases every key; a controller button press and its release produce the
same mask as the equivalent PC key.

---

## What stays out, and why

- **A quirk switch.** The machine's divergences from other emulators are documented
  in [the instruction set](instruction-set.md) and are the machine. A checkbox that
  changes `FX55` would make every answer to "what does this do?" conditional.
- **Networked play.** Two machines sharing a keypad is a different product, and
  nothing in this one needs an account.
- **Automatic ROM downloading.** The project ships no third-party ROMs, and a
  browser that fetches them would be doing exactly that with extra steps.

---

## Order of work, if the proposals are taken

1. **Breakpoints in the window** — smallest change, largest teaching payoff.
2. **Instruction trace** — follows from the breakpoint's pause path.
3. **Memory watch** — the same pane, a different question.
4. **State snapshots** — the `Chip8State` struct is already the format.
5. **Library manifest** — data, not machinery.
6. **The SuperChip subset** — opcodes and manual rows.
7. **Assembler forms** — last, because the listings must keep assembling.
8. **Key binding** — usability, once the machine is complete.
