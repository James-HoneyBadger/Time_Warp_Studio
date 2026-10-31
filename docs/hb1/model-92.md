# The HB-1 Model 92

Time Warp Studio 25.0 is the current release — 24.0, the golden one, is archived —
and the machine described in [the owner's manual](README.md) is finished and
validated by the gates in [README_TESTING](../README_TESTING.md). Everything on this
page is a **proposal for after 25.0**, written down rather than wished for, so that a
future release has a specification to build to and a way to prove it is done.

Where [enhancements](enhancements.md) proposes what a 1982 machine left undone, this
page proposes the machine the same company would have shipped ten years later. It is
the same exercise in the same terms — what it is, why the machine wants it, and what
would count as proof — at the scale of a new computer rather than a new word.

Nothing here changes 25.0.

---

## What a decade asked of a computer

1992 is not 1982 with a faster clock. It is the year the personal computer stopped
being a single-tasking appliance and became a *system*, in five jumps, and the HB-1
has taken none of them:

| Jump | The 1992 answer | Where the HB-1 stands |
| --- | --- | --- |
| Address space | 32-bit, flat, megabytes, some of it protected | 64 KiB with a hardwired map, 32 KiB of it free ([hb_memory.hpp](../../native/include/timewarp/ui/hb_memory.hpp)) |
| Display | a bitmap framebuffer, a blitter, hardware sprites, an RGB output | 80 × 25 character cells, and "colour" is a brightness ramp on a monochrome tube ([hb_tube.cpp](../../native/src/ui/hb_tube.cpp)) |
| Sound | sampled PCM, several channels at once, envelopes | a one-byte latch and five sounds the machine makes on its own |
| Storage | directories on a floppy and a hard disk, with timestamps | 512 sectors of 256 bytes, one flat volume ([HB DOS](hb-dos.md)) |
| Concurrency | a desktop, a pointer, and several programs resident | one language at a time: selecting a ROM cold-boots the machine |

The last one is the real gap, and it is the one this project is unusually placed to
close. Today the ROM socket is mutually exclusive — eleven listings are kept, and one
prompt and one run are live. In 1982 that exclusivity *was* the machine: one language
at a time was what a home computer could afford. In 1992 it was the thing the whole
industry spent its money abolishing.

---

## The thesis

> The Model 92 is the machine where the ROM socket became an operating system, and
> the eleven languages became eleven things running at once.

That is the 1992 answer to the premise in [the machines](../MACHINES.md): the 1982
machine's signature idea was *one socket, eleven languages*, so its successor's is
*one desktop, eleven processes*. The eleven languages stop being cartridges and become
the built-in process images, which is also — wryly — what the IDE around this machine
has been all along. The machine stops being a place you run one thing and becomes a
place you keep several.

It is one window, not two machines. The 1992 bus *contains* the 1982 map: the old
addresses still answer exactly as they do today, as `MODE 0`, and a 1982 program runs
on the Model 92 without being told it is on a different computer. That is not a
courtesy to old programs; it is the regression suite. The 1 614 lines of `retro_*`
tests that pin today's behaviour become the proof that the overhaul is a superset
rather than a replacement.

---

## The specimen

| | HB-1 (1982) | HB-1 Model 92 |
| --- | --- | --- |
| Processor | none: the native engines behind a very good costume | **HB-16**: 16-bit data, 24-bit address, eight registers, cycle-counted |
| RAM | 64 KiB, 32 KiB free, fixed | 1 MiB standard, 4 MiB maximum, banked by the bus |
| Display | monochrome 80 × 25 tube | text (compatible), 320 × 200 × 256, 640 × 480 × 16; blitter; 16 sprites; collision |
| Colour | one phosphor, sixteen brightness steps | 256-entry RGB DAC, and the phosphor stays as a *monitor* |
| Sound | one latch, five machine sounds | four channels of 8-bit PCM at 22 050 Hz, envelopes, a 32-slot waveform bank |
| Storage | HB DOS 1.0: 128 KiB, one volume, flat | HB DOS 2.0: a 1.44 MB floppy and a 40 MB hard disk, directories, timestamps, attributes |
| Input | a keyboard | a keyboard, a pointer with two buttons, two joysticks, an RTC, a UART, MIDI out |
| Operating system | a prompt | HB-OS: round-robin preemption on the 60 Hz tick, eight processes, a window each |
| Modules | the `Language` enum | the enum *plus* loadable ones: a script, a binary, a saved machine |

---

## The rules this overhaul is built by

The five rules below are not new ones invented for this page. They are the ones the
machine already keeps, restated because a change this size is where they get lost.

1. **Everything a program can see is in memory somewhere.** The tube paints
   `HbMemory` rather than a log; a device that cannot be `POKE`d is a screenshot.
2. **A device is a value type with no widgets.** `HbDos` is the model: plain calls, one
   image, and the window owns the pixels.
3. **No wall clock inside the machine.** `HbClock` is the only clock, so a replay of
   the same number of ticks is byte-identical. Every test in this page depends on it.
4. **Nothing is lost.** The 1982 map is a mode, the phosphor tube is a monitor, and the
   existing suites pin it all.
5. **The book ships with the machine.** A device without a memory map page and a
   demonstration program is a device only its author can use.

---

## Specification proposals

### 1. The bus, and the machine as an object

**What.** One `HbMachine` that owns an `HbBus` and a set of devices. The bus routes
an address to a device by *region*, and the region table is data in one generated
include — `hb_devices.inc` — in the manner of the theme table and the language
registry, so the map a program sees, the map the manual prints and the map the bus
routes with are one list. Devices share one small interface: `read`, `write`, `peek`,
`poke`, `tick`, `reset`. `MachineImage` becomes a snapshot of the whole machine —
RAM, framebuffer, device state, disk — and carries a format number, so a version-1
image from 24.0 still loads and is written back as version 2.

**Why.** The machine's state is currently spread across `HbMemory`, `HbDos`,
`MachineImage`, the settings file and seven `retro_computer_*.cpp` files. Every
capability on this page touches three of those. Nothing is cheap until the machine is
one object with a documented map, and the map is currently prose in a header comment
that a compiler has never read.

**What changes.** `hb_bus.{hpp,cpp}`, `hb_device.hpp`, `hb_machine.{hpp,cpp}`,
`hb_devices.inc`; `MachineImage` gains RAM, video and device fields and a format
number; the window keeps its pixels and loses its address arithmetic.

**Proof.** A new harness subcommand boots an image, runs a program, and prints the
registers, the screen text and a hash of the framebuffer — the machine's counterpart
to `time_warp_check`, and the way every later proposal in this page is verified from a
shell rather than by clicking. Plus: save, restore and compare a checksum of the whole
machine; a device given a page by the table answers on that page; an address no
region claims reads zero and refuses a write.

---

### 2. A video chip, and the first colour the machine has ever had

**What.** One framebuffer, several modes. `MODE 0` is today's text: 80 × 25 cells at
`$8000`, byte for byte. `MODE 1` is 320 × 200 with one byte per pixel, so a `POKE`
draws. `MODE 2` is 640 × 480 with four bits per pixel, sixteen colours from the DAC.
A blitter does fill, copy, masked copy, line, glyph and XOR without the processor
touching the pixels. Sixteen sprites of 16 × 16 are composed over the bitmap, with a
collision register. The DAC is 256 entries of three bits per channel, written through
a palette port.

**Why.** It is the difference a reader feels in the first second, and it is the
foundation of two later proposals: a pointer needs a framebuffer to have a pointer in,
and a desktop needs windows that overlap. It also discharges
[enhancements](enhancements.md) 4 properly, with a sprite pass the whole machine
shares rather than one bolted onto a character grid.

**The aesthetic, decided.** Colour does not replace the tube. CONFIG keeps
`PhosphorTint` and gains a **monitor**: **HB Mono**, which is the ramp exactly as it
is today, or **HB Colour**, which is the DAC. Both are honest views of the same
framebuffer, and the machine's promise — that what you see corresponds to something
real in memory — holds on either.

**What changes.** `hb_video.{hpp,cpp}`; `HbTube` paints the framebuffer and keeps the
cell path as a mode; a sprite table in the I/O page; a monitor row in CONFIG; a
palette page in the manual.

**Proof.** `MODE 0` renders a recorded listing byte-identically to today's tube — the
compatibility claim, asserted rather than asserted about. Composition order is
asserted on pixels. A collision sets the register and reading it clears it. A sprite
whose shape lies outside the framebuffer draws nothing rather than garbage. A blit
costs a fixed, deterministic number of ticks.

---

### 3. Four channels instead of a beep

**What.** A four-channel mixer of 8-bit unsigned samples at 22 050 Hz, with per-channel
volume and a three-stage envelope, producing buffers on the frame tick and handing
them to the audio sink the IDE already uses. A 32-slot waveform bank in memory that
`POKE` can rewrite — the period's digi-drum — and `SOUND` grows a channel form so a
language can play a chord without touching a register.

**Why.** The single latch is the last part of the machine that is a costume: it plays
one of five noises and cannot be made to play anything else. Sampled audio is the most
1992 thing a program can do, and the machine's voice becomes something a *program* has,
not only something the machine does.

**What changes.** `hb_audio.{hpp,cpp}`; the latch becomes channel 0's trigger, so every
existing `SOUND` still sounds; a waveform bank in the I/O space; the five machine
sounds stay exactly as they are, outside the mixer, because they belong to the
hardware rather than to the program.

**Proof.** A four-note chord's buffer is asserted sample by sample. Rewriting a
waveform changes the next buffer and not the one being played. With no audio sink
present — which is the case in a test — the mixer still advances deterministically and
says nothing.

---

### 4. HB DOS 2.0

**What.** Two drives: a 1.44 MB floppy and a 40 MB hard disk, both images on the host,
both honouring `TIME_WARP_DATA_DIR` as every other piece of machine state does.
Directories, which means `MKDIR`, `CHDIR`, `RMDIR` and a path form for every existing
word. Timestamps and a read-only and a hidden attribute. `COPY`, `MOVE`, and `FREE`,
which draws a sector map in the tube rather than printing a count. `STARTUP` on a
volume autoboots, as [enhancements](enhancements.md) 8 proposes, and it belongs here
because a disk with directories is a disk with a boot directory.

**Why.** A disk without `COPY` makes the user re-type, and a disk without directories
cannot be navigated by a desktop. The 30-character name limit stays: this is a retro
machine, not a Linux box, and the limit is part of what makes a catalogue a catalogue.

**What changes.** `HbDos` gains drives, a tree and the new words; the image format
carries a version, and a version-1 volume reads as a directory-less root, so every
program on an existing `HBWARP.dsk` still loads. `STATUS` reports the map.

**Proof.** Every existing [retro_dos_tests.cpp](../../native/tests/retro_dos_tests.cpp)
case passes unchanged against a version-1 image. A version-2 round trip preserves the
tree. `FREE` accounts for every sector with none lost or double-counted. `COPY` of a
locked file is allowed and of a missing file refused, in the machine's voice.

---

### 5. The things a 1992 machine plugs in

**What.** Five small devices on their own bus pages: a pointer (position, buttons, and
a movement delta a program reads without losing events), two joysticks (two axes and
two buttons each, routed through the gamepad support the IDE already has so the editor
benefits from the same work), a real-time clock so `DATE$` and `TIME$` are readings
rather than constants, a UART with a byte of FIFO, and MIDI out through the audio
device.

**Why.** A pointer is what the desktop in proposal 7 is for, and a clock is what makes a
filesystem's timestamps mean anything. Each of these costs one bus page and turns a
native capability the Studio already has into something the machine can reach.

**What changes.** `hb_input.{hpp,cpp}`, `hb_rtc.{hpp,cpp}`, and register definitions in
`hb_devices.inc`; `DATE$`/`TIME$` and the joystick statements learn the machine path,
keeping the sparse fallback that the editor and the parity corpora see.

**Proof.** A pointer event at a known cell answers with that cell. The register and the
language statement produce the same bytes on the serial line. The clock reads a time a
test injects, so no test waits for a second to pass — rule 3 above.

---

### 6. HB-16, the assembler and the monitor

**What.** A processor, and the three tools that make one usable. HB-16 is 16-bit data
with a 24-bit address space, eight general registers plus stack, program counter and
flags, little-endian, with register, indirect, indexed, absolute and immediate
addressing and a table of cycle counts that is data rather than code. An assembler in
the manner of the CHIP-8 one — listings, labels, directives — turns text into bytes.
A monitor disassembles, edits memory, sets a breakpoint and steps, in the tube. `SYS`
calls it from BASIC, and an `INT` instruction is the documented way a machine-code
program asks the operating system for anything.

**Why.** [enhancements](enhancements.md) 2 proposed sixteen invented opcodes so that
`POKE` would have something to call. A machine in 1992 had an *instruction set*, a
monitor in ROM and an assembler within reach; that is what an owner learned a computer
on, and it is what makes the module bay in proposal 8 real. The CHIP-8 machine in this
tree is the precedent for the shape: assemble, run, step, read the registers.

**What changes.** `hb_cpu.{hpp,cpp}`, `hb_asm.{hpp,cpp}`, `hb_monitor.{hpp,cpp}`; a
registers and disassembly panel in the machine window; a manual page for the
instruction set; `SYS` in the BASIC executor behind the machine bus.

**Proof.** An instruction-set corpus, one case per opcode and addressing mode, each
asserting the result *and* the cycle count. Assemble, disassemble and assemble again
returns the same bytes. `SYS` into ROM, or into a page with no code, reports rather
than executing zeros. A breakpoint stops the processor with the right registers
visible, which is what makes the panel worth having.

---

### 7. HB-OS: the desktop, and eleven processes

**What.** A windowed desktop on the machine's own screen: a pointer, windows that move,
resize, overlap and take focus, an icon strip, and a bar that lists what is running.
Under it, a round-robin scheduler on the 60 Hz tick, up to eight processes, each with a
window and its own engine session. The ROM selector becomes the task list; the 1982
`SCREEN` modes become a window size. A process that errors is killed and named, and the
desktop survives it.

**Why.** This is the proposal the rest of the page exists to make possible, and it is
the one that makes the machine a place rather than an appliance. It is also the honest
1992 answer to the eleven-language socket: in 1982 you chose a language and cold-booted
into it; in 1992 you keep four of them open. Nothing in the engine layer has to change
to allow it — a session is already per-visit and per-language — which is the strongest
argument that the idea belongs to this machine and not to a wish.

**What changes.** `hb_scheduler.{hpp,cpp}`, `hb_desktop.{hpp,cpp}`, `hb_window.hpp`; the
window's single prompt becomes a task bar over N sessions; settings record the layout.

**Proof.** Two programs run interleaved and both make progress — a BASIC `FOR` and a
Logo turtle, asserted on ink rather than on a log. Z-order, hit testing and focus are
asserted on pixels. A process that throws does not take the desktop down. A replay of
N ticks is byte-identical, which is the whole reason the scheduler reads `HbClock` and
never the wall.

---

### 8. The module bay

**What.** The ROM socket stops being an enum. A module is a file in a bay directory,
and there are three kinds: a **script** module, which is a list of prompt lines (the
replayable scripts of [enhancements](enhancements.md) 10, promoted to a format); a
**binary** module, which is HB-16 code with a header saying where it loads and what it
calls; and an **image** module, which is a saved machine. `MODULE` lists what is in the
bay, and a loaded module appears in the selector and boots like a built-in ROM.

**Why.** 1992 is the year the cartridge gave way to the floppy and the CD, and in this
parallel universe the eleven built-in ROMs stay exactly where they are while the socket
becomes something a user can fill. It is the payoff of proposals 4, 6 and 7 together:
storage to hold a module, a processor to run one, and a desktop to show one in.

**What changes.** `hb_module.{hpp,cpp}`; a bay directory beside the disks; one word in
the vocabulary; a row in CONFIG naming the bay.

**Proof.** A script module records, is saved, is listed and replays to the same screen
text. A binary module loads at its stated address and runs. An image module boots the
machine into that state. A malformed module is refused with a sentence and leaves the
machine alone.

---

## How this absorbs the proposals already written down

Nothing in [enhancements](enhancements.md) is dropped. Each proposal either becomes a
step in this page or stays a step of its own, and saying which is the point of the
table:

| Existing proposal | Where it goes |
| --- | --- |
| 1. The tape drive | stays: a second storage device, and the one the 1982 mode keeps |
| 2. A monitor and `SYS` | proposal 6, with a real instruction set instead of sixteen opcodes |
| 3. Programmable function keys | proposal 7, where a desktop has a macro strip and a reason to have one |
| 4. Hardware sprites | proposal 2 |
| 5. RAM size as a setting | proposal 1: a banked bus makes the size a reading, not a constant |
| 6. Light pen, serial, note registers | proposal 5, as the pointer, the UART and four channels |
| 7. HB DOS 1.1 | proposal 4, because directories subsume `COPY` and `FREE` |
| 8. Autoboot from the disk | proposal 4, one word once a disk has a boot directory |
| 9. Editing that names what it edits | unchanged, and worth doing first: it is small and it is felt daily |
| 10. Replayable prompt scripts | proposal 8, as the script module |

---

## What it deliberately is not

- **Not a network.** The WARP BOARD stays a local board: a teaching machine must not
  need a server to demonstrate what a network was for, and a 1992 machine with a modem
  is better served by the BBS it already has than by an empty socket.
- **Not a second interpreter.** Every language stays native, and the machine keeps
  running the same engines the editor runs.
- **Not an emulator of a real computer.** The Model 92 is what this parallel universe
  built, so it is designed to be kind in exactly the places a real 1992 machine was
  cruel — and documented where it chooses to be.
- **Not a twelfth language.** The eleven stay eleven. They stop taking turns.
- **Not a theme.** No part of this is a skin: every pixel is a byte, every byte is at an
  address, and every address is in the map the manual prints.

---

## Where the new code goes

The window stays the window: it owns the pixels, the glass and the settings file, and
it keeps getting smaller relative to the machine. What grows is one value type per
device, each with no widgets, each driven through the bus, each with a suite:

| New | What it is |
| --- | --- |
| `hb_bus.{hpp,cpp}`, `hb_device.hpp`, `hb_devices.inc` | the map, as data |
| `hb_machine.{hpp,cpp}` | the machine as one object, and the snapshot |
| `hb_video.{hpp,cpp}` | framebuffer, modes, blitter, sprites, DAC |
| `hb_audio.{hpp,cpp}` | four channels and the waveform bank |
| `hb_input.{hpp,cpp}`, `hb_rtc.{hpp,cpp}` | pointer, joysticks, clock, UART |
| `hb_cpu.{hpp,cpp}`, `hb_asm.{hpp,cpp}`, `hb_monitor.{hpp,cpp}` | HB-16 and its tools |
| `hb_scheduler.{hpp,cpp}`, `hb_desktop.{hpp,cpp}`, `hb_window.hpp` | HB-OS |
| `hb_module.{hpp,cpp}` | the bay |

`retro_*.cpp` stays split by subject, and the structure gate's per-file budget is the
reason each device is its own file rather than a section of a larger one.

The harness gains one subcommand, and the smoke gate gains the demonstration programs:

```bash
./native/build/time_warp_gates hb Examples/hb1/desktop.s --ticks 120 --screen
```

---

## Numbers

Targets, so a later release can be measured rather than argued about:

| | |
| --- | --- |
| RAM | 1 MiB standard, 4 MiB maximum, the 1982 map a mode |
| Video | 320 × 200 × 256 and 640 × 480 × 16, plus the text mode, at 60 Hz |
| Sprites | 16, 16 × 16, one collision register |
| Audio | 4 channels, 8-bit, 22 050 Hz, 32-slot waveform bank |
| Storage | 1.44 MB floppy and 40 MB hard disk, directories, 30-character names |
| Processes | 8, round-robin, one window each |
| Processor | HB-16, 24-bit address, ~48 opcodes, a cycle count per opcode |
| New code | roughly 9 000 lines of machine and tests: the machine is 5 088 lines and its suites 1 614 today, so this more than doubles it |

---

## Order of work, if the proposals are taken

1. **The bus and the machine object.** Invisible, and everything else is cheaper with
   it. It also produces the harness that proves the rest.
2. **The video chip.** The first thing a reader sees, and the prerequisite for the
   desktop and the pointer.
3. **HB DOS 2.0.** Directories before a desktop that browses them.
4. **The audio chip.** Independent of the two above; can be done in parallel by anyone
   who wants a satisfying, self-contained piece of work.
5. **The input devices.** The pointer is only worth having once video exists, and the
   desktop needs both.
6. **HB-16, the assembler and the monitor.** The largest single item; independent of
   the display work, so it can run alongside it.
7. **The module bay.** Small once 4, 6 and 7 exist, and it is what makes the socket a
   user's rather than the factory's.
8. **HB-OS and the desktop.** Last, because it depends on more of the rest than any
   other proposal, and because it is the one to cut if the appetite is not there.
9. **The books.** Not last: each proposal ships its own page, and `hardware.md` — the
   generated memory map — is written in step 1, not after step 8. This line is here to
   say that the documentation is part of each step, which is how this tree has always
   done it.

Each step is a document, a value type, a test and a demonstration program, in that
order — the same order the rest of this project is built in.

---

## What would make this the wrong plan

Written down so it can be argued with:

- **If the compatibility mode is not free, stop at step 1.** A 1992 machine that
  cannot run a 1982 program without being asked to is a second machine wearing the
  first one's badge, and the page above is wrong about that. The existing `retro_*`
  suites are the test of it, and they either keep passing untouched or the premise is
  false.
- **If the scheduler cannot be deterministic, cut proposal 7.** A desktop whose frame
  depends on the host clock is a demo, not a machine, and rule 3 above is the rule that
  keeps this project's tests honest.
- **If the desktop makes the eight processes feel like tabs, cut it.** The IDE already
  has tabs. What a 1992 machine has that a tabbed editor does not is the *cost* of
  preemption — a program that does not get its turn — and if that cannot be felt, the
  proposal has bought nothing the Studio did not already have.
- **If `hb_devices.inc` becomes a second source of truth, stop and fix the first one.**
  The map has exactly one home, or the manual and the bus will disagree within a
  release.
