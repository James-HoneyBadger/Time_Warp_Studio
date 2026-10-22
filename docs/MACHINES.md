# The machines

Time Warp Studio is a time machine that pretends to be an IDE. Two machines are
built into it, and they are the reason it exists rather than being another editor
with a Run button:

- the **[HB-1](hb1/README.md)** — a personal computer from a 1982 that never
  happened, shipped with ten language ROMs in one socket, a prompt, a CRT and a
  disk; and
- the **[CHIP-8](chip8/README.md)** — the 1977 virtual machine: sixty-four by
  thirty-two pixels, sixteen keys, four kilobytes, and more instruction per byte
  than anything that came before it.

Both are real programs. Both run the same engines as the editor. Neither is a
screenshot, a theme, or a video of a computer. You can type at them, break them,
save your work to their disks and come back tomorrow.

---

## A system from a parallel universe

The HB-1 is not an emulator of a machine that existed. It is what a 1982 home
computer might have been if the people who designed it had also known everything
the next four decades taught us about making computers kind to learn on.

That single premise decides everything about it:

- **One ROM socket, eleven languages.** The parallel 1982 did not have a format war
  over BASIC. Its machine shipped with BASIC, Logo, PILOT, FOCAL, COMAL, REXX,
  Scheme, APL, Smalltalk and CHIP-8, each a cartridge you select from the front
  panel, each keeping its own memory when you switch away.
- **A disk in the old measurements.** The volume is 512 sectors of 256 bytes,
  named files with a lock bit, a catalogue with a `*` beside the protected ones,
  and a two-step `INIT` — because that is how a 1982 learner learned that a disk
  is a physical thing you can destroy. Underneath, the image is one JSON file you
  can copy, read and hand to somebody else.
- **A prompt with modern manners.** The prompt echoes in the machine's voice and
  refuses in the machine's voice (`?FILE LOCKED`), but it is a persistent session
  with real bindings, and the text above it on the PROGRAM page is a full code
  editor with undo, syntax colouring and a gutter.
- **A monitor you can tune.** Green, amber, white or ice phosphor, scanlines,
  vignette, curvature, flicker and glow — each a slider from nothing to
  everything. The glass is drawn over the screen and never takes the keyboard.
- **Nothing you can lose by exploring.** CONFIG previews every change before you
  accept it and Cancel puts the machine back. Breakpoints, the Variables panel,
  watch expressions and telemetry work behind period controls. The worst thing a
  program can do is fill 512 sectors and stop.

The result is a machine that feels inevitable and impossible at the same time:
the constraints are 1982's, the engines are the Studio's, and the only thing
missing is the cruelty.

---

## The timey-wimey, wibbly-wobbly parts

Anachronism on purpose, item by item. The left column is what a 1982 learner
would recognise; the right is what is actually running.

| Period thing | What is really there | Why both |
| --- | --- | --- |
| `*** HB-1 ***` and `(C) 1982 HB SYSTEMS` | The same native engines the IDE runs, in process, no Python | The banner sets the stakes; the engine removes the crashes |
| 512 sectors of 256 bytes | One JSON image under `~/.time_warp/disks` | Sector accounting teaches that storage is finite; JSON means a teacher can copy the disk |
| `?` for `PRINT`; numbered lines | A real code editor on the PROGRAM page | Classic direct mode and modern editing are the same program seen two ways |
| A catalogue with a lock bit | `HbDos`, a widget-free class with rollback on a failed write | The period gave the metaphor; the class makes it testable |
| A CRT with phosphor and glass | `retroPalette` plus `CrtOverlay`, applied to the whole machine | The fantasy is total: choosing amber recolours header, editor, canvas and glass together |
| A settings screen you reach with `CONFIG` | A live-preview dialog that persists under `retroComputer/config` | The ritual is period; the preview and Cancel are courtesy |
| A 64×32 CHIP-8 screen | A 60 Hz machine with timers, a keypad, a register inspector and a memory view | The chip is small; the tools around it are not |
| A prompt | A persistent REPL with bindings in eight of the eleven ROMs | You can learn interactively, as the 1980s promised, and keep what you defined |

The machines also disagree with history where history was simply wrong. Two
examples:

- **SYS is a no-op.** `0NNN` called a routine on the RCA 1802 that hosted the
  original CHIP-8 interpreter. Nothing here can run it, so the machine ignores it
  — as every interpreter since the original has — and the assembler still accepts
  the mnemonic so old listings assemble.
- **`HALT` exists.** `0x0000` is unused by the original set, so a program can stop
  instead of spinning on a jump-to-self to mark its own end.

---

## The two machines at a glance

| | HB-1 | CHIP-8 |
| --- | --- | --- |
| What it is | A complete home computer: CRT, prompt, ROM socket, editor, canvas, disk | A virtual machine: 4K of memory, sixteen registers, sixteen keys |
| Year, in its universe | 1982 | 1977 |
| Screen | TEXT / SPLIT / GRAPHICS sharing one CRT | 64×32 monochrome, or 128×64 in SuperChip hi-res |
| Languages | All ten, one ROM at a time | One: CHIP-8 opcodes |
| Storage | HB DOS 1.0, 512 sectors, one volume | Your own files; the ROM library browses a folder you choose |
| Interaction | A prompt with history, BASIC direct mode, F9 to run | A keypad, 60 frames a second, Run/Step/Reset |
| Where it lives | Tools > **HB-1 Computer…** (`Ctrl+Alt+W`), or `./run.sh --computer` | Tools > **CHIP-8 Emulator…** (`Ctrl+Shift+M`) |
| Manual | [HB-1 Owner's Manual](hb1/README.md) | [CHIP-8 Machine Manual](chip8/README.md) |
| Teaches | That a computer is a system: memory, storage, languages, a screen | That a computer is a machine: fetch, decode, execute, 4K at a time |

They are deliberately different kinds of thing. The HB-1 is a place to live; the
CHIP-8 is a thing to take apart. Together they bracket the era the Studio is
named for: the chip that made a computer out of almost nothing, and the home
computer that made a computer out of a chip.

---

## What they share

Neither machine is a special case inside the application:

- both draw through the same `CanvasCommandQueue` the editor drains into the
  turtle canvas;
- both run programs on the same native engines — `timewarp_lang` for the
  line-by-line languages, `timewarp_scheme` for Scheme, and the CHIP-8 assembler
  and machine that the editor's whole-program Run uses;
- both use `NativeRunner` for a run that finishes and `ReplRunner` for a session
  that does not, which is why a program can never start inside a previous
  command's variables;
- both are driven, in tests, through the same public surface a user touches:
  `submitCommand()`, `runProgram()`, `emulator()->step()`, `disk()`.

Both machines are native and neither involves a second interpreter. What the
parity gate compares against is a recorded corpus, not a live engine — see
[ARCHITECTURE.md](ARCHITECTURE.md) and [README_TESTING.md](README_TESTING.md).

---

## The manuals

### The HB-1

| Book | What it covers |
| --- | --- |
| [Owner's Manual](hb1/README.md) | The front panel, first boot, everyday operation, what the machine remembers, the limits |
| [The ROM library](hb1/roms.md) | All eleven languages as ROMs: banner, prompt, direct mode, session, worked examples |
| [HB DOS 1.0](hb1/hb-dos.md) | The disk: every word, the catalogue, the errors, the image format |
| [CONFIG](hb1/config.md) | Every setting, every default, the phosphors and the glass, where it is stored |
| [Inside the HB-1](hb1/inside.md) | How a keystroke becomes ink: the objects behind the screen |

### The CHIP-8

| Book | What it covers |
| --- | --- |
| [Machine Manual](chip8/README.md) | The two ways to run, the window, the keypad, debugging, sound, the ROM library, the limits |
| [The assembler](chip8/assembly.md) | Listings: syntax, labels, directives, errors, worked programs |
| [The instruction set](chip8/instruction-set.md) | Every opcode as implemented, the 1977 quirks, timing, and a full worked disassembly |

### Around them

- [USER_GUIDE.md](USER_GUIDE.md) — the IDE the machines live in: projects, the
  REPL bar, debugging, SQL, automation.
- [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) — the command reference for all eleven
  languages, reachable from either machine.
- [tutorials/](tutorials/) — one tutorial per language.
- [ARCHITECTURE.md](ARCHITECTURE.md) — how the pieces are put together, for
  people changing them.
