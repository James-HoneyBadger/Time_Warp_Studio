# The HB-2

The HB-1 Model 92, built: a 1992 computer from the same parallel universe as the
HB-1, with a bus, a framebuffer, four channels of sampled audio, hardware sprites and
a processor of its own. It is the machine the [Model 92 proposal](../hb1/model-92.md)
described, and this page is its owner's manual.

```bash
# The machine, in the window: Tools > HB-2 Model 92 (Ctrl+Alt+2)
./native/build/time_warp_ide --hb2

# Assemble and run a listing, and see what it did
./native/build/time_warp_gates hb Examples/hb2/desktop.s --ticks 4 --screen

# Two processes on the desktop, both making progress
./native/build/time_warp_gates hb --tasks Examples/hb2/task_a.s Examples/hb2/task_b.s --ticks 6

# The memory map, from the one table the bus routes with
./native/build/time_warp_gates hb --map

# The suite that pins all of it
ctest --test-dir native/build -R hb2_tests --output-on-failure
```

Everything on this page is real software: every pixel is a byte, every byte is at an
address in the map below, and every address is one the bus answers on. There is no
theme here and no screenshot.

---

## What it is

| | HB-1 (1982) | HB-2 (1992) |
| --- | --- | --- |
| Processor | none: the native engines behind a costume | **HB-16**: 16-bit data, 24-bit address, eight registers, counted cycles |
| RAM | 64 KiB, 32 KiB free, fixed | **1 MiB**, and the 1982 map as `MODE 0` |
| Display | monochrome 80 x 25 tube | text (the same cells), 320 x 200 x 256, 640 x 480 x 16, a blitter, 16 sprites |
| Colour | one phosphor, sixteen brightness steps | a 256-entry RGB DAC, three bits a channel |
| Sound | one latch, five machine noises | four channels of 8-bit PCM at 22 050 Hz, envelopes, a 32-slot waveform bank |
| Input | a keyboard | a pointer with two buttons, two joysticks, a real-time clock, a serial port |
| Operating system | a prompt | `INT` services, on a 60 Hz tick |

The 1982 map is not emulated, it is *kept*: `MODE 0` routes `$8000` to the text cells,
`$9000` to the attributes, `$A000` to the I/O page and `$F000` to ROM, so a program
written for the HB-1 runs here without being told it is on a different machine. That
promise is what the *MODE 0: the 1982 map, unchanged* case in `hb2_tests` asserts.

---

## The memory map

One table -- `native/include/timewarp/hb2/devices.inc` -- and the bus routes with it,
the harness prints it and this page was written from it. There is no second copy to
disagree with.

| Region | Addresses | What it is |
| --- | --- | --- |
| `ram` | `$000000..$0FFFFF` | 1 MiB of RAM. A program and its data live here |
| `vram` | `$100000..$1257FF` | The framebuffer: 320 x 200 bytes, or 640 x 480 at two pixels a byte |
| `registers` | `$200000..$200FFF` | The video chip's registers, DAC port, blitter, sprites and pattern bank |
| `audio` | `$201000..$201FFF` | Four channels, the master volume, the 1982 latch, the waveform bank |
| `input` | `$202000..$2020FF` | The pointer and two joysticks |
| `rtc` | `$202100..$20211F` | Seconds, minutes, hours, day, month, year, a tick counter |
| `uart` | `$202200..$20220F` | A serial port: one byte in, one byte out |
| `rom` | `$210000..$21FFFF` | 64 KiB of ROM, filled with `$FF` so a jump into it is an invalid instruction |
| `screen` (MODE 0) | `$008000..$0087CF` | The 1982 text cells, 80 x 25, byte for byte |
| `attributes` (MODE 0) | `$009000..$0097CF` | The 1982 ink and paper, one byte a cell |
| `io1982` (MODE 0) | `$00A000..$00A0FF` | The 1982 I/O page, which is the first 256 bytes of the video registers |
| `rom1982` (MODE 0) | `$00F000..$00FFFF` | The 1982 ROM window |

An address no region claims reads **zero** and refuses the write. The refusal is
counted, not thrown: a stray `POKE` is a program's mistake, and the harness reports
how many it made.

---

## The video chip

Three modes, one framebuffer. `MODE 0` is the 1982 tube; `MODE 1` is 320 x 200 with
one byte per pixel, so a store draws; `MODE 2` is 640 x 480 with four bits per pixel
and sixteen colours from the DAC.

Registers, at `$200000` plus the offset:

| Offset | Register | Notes |
| --- | --- | --- |
| `$00` | Cursor column | The 1982 register, in the same place, clamped to 0..79 |
| `$01` | Cursor row | 0..24 |
| `$02..$04` | Border, ink, paper | The 1982 registers |
| `$05` | Sound latch | Writing a note sounds channel zero; the frame drains it |
| `$06..$07` | Frame counter | Low and high byte, sixty a second |
| `$08` | Mode | 0, 1 or 2 |
| `$10..$1B` | Blitter geometry | X0, Y0, X1, Y1, destination X and Y, each low byte then high |
| `$1C` | Blitter colour | |
| `$1D` | Blitter operation | 1 fill, 2 line, 3 copy, 4 masked copy, 5 xor, 6 glyph |
| `$1E` | Blitter go | A store runs the operation |
| `$1F..$20` | Last ticks | What the last operation cost |
| `$21..$23` | Source address | 24-bit: low, middle, high, for copy and glyph |
| `$30..$33` | DAC index, red, green, blue | Three bits a channel |
| `$48` | Collision | A read returns it and clears it |
| `$50` | Sprites | Sixteen blocks of eight: enable, x (two bytes), y (two bytes), pattern, colour |
| `$800` | Sprite patterns | Sixteen 16 x 16 masks, thirty-two bytes each, two bytes a row |

**The blitter** exists because a processor that moves every pixel spends all its time
doing it. Each operation costs a fixed, counted number of ticks -- a fill is
`2 + area/8`, a line is `2 + pixels`, a copy is `2 + area/4`, a glyph is 10 -- and
leaves that number in the status register, so a program can time itself.

**Sprites** compose over the bitmap in index order, so a higher number is in front.
Two sprites that paint the same pixel set the collision register; reading it clears
it, because the machine's promise is that a program that wants to know whether they
met asks once. A sprite whose shape lies outside the screen draws nothing rather than
garbage.

---

## The sound chip

Four channels, 8-bit unsigned samples at 22 050 Hz. Each channel is eight registers:

| Offset | Register | Notes |
| --- | --- | --- |
| `+0` | Enable | Bit 0; writing it starts the envelope and resets the phase |
| `+1` | Waveform | 0..31, a slot in the bank |
| `+2..+3` | Period | Samples a cycle, 16-bit |
| `+4` | Volume | 0..255 |
| `+5` | Attack | Samples of fade-in |
| `+6` | Decay | Samples of fade-down to the sustain level |
| `+7` | Sustain | 0..255 |

The **waveform bank** is 32 slots of 32 samples from `$201100`; a store into it
rewrites the instrument, which is what a digi-drum was. Slot 0 is the square the 1982
latch produced.

The **1982 latch** is at `$41`: a store triggers channel zero, so every existing
`SOUND` still sounds. The five machine noises stay outside the mixer, because they
belong to the hardware rather than to a program.

Mixing is deterministic -- same registers, same frames, same bytes -- which is what
lets a test assert a chord sample by sample. With no speaker attached (the case in
every test) the mixer still advances, so an envelope that should have decayed has.

---

## The peripherals

| Device | Addresses | Registers |
| --- | --- | --- |
| Pointer | `$202000` | `$00/$01` x, `$02/$03` y, `$04` buttons, `$06` and `$08` movement since the last read |
| Joysticks | `$202010` | Two blocks of four: x, y, buttons, spare |
| Clock | `$202100` | `$00` seconds, `$01` minutes, `$02` hours, `$03` day, `$04` month, `$05` year, `$06` ticks |
| Serial | `$202200` | `$00` data, `$01` status: bit 0 a byte is waiting, bit 1 the transmitter is clear |

The clock is the machine's own, never the host's: it advances one second per sixty
frame ticks, and a test injects a reading rather than waiting for one. A month is
28 days, and this manual says so.

---

## The operating system

`INT` is the documented way a program asks the machine for anything. The vector is an
immediate, the arguments are registers, and the table is small on purpose.

| Vector | Service | Arguments |
| --- | --- | --- |
| 0 | Exit | `R0` the exit code; the machine stops, and the harness reports it |
| 1 | Print a character | `R0`, written at the cursor and to the transcript |
| 2 | Frame counter | `R0` = the frame low word |
| 3 | Fill | `R0` x, `R1` y, `R2` width, `R3` height, `R4` colour |
| 4 | Line | `R0` x0, `R1` y0, `R2` x1, `R3` y1, `R4` colour |
| 5 | Choose the video mode | `R0` = 0, 1 or 2; `MODE 0` also restores the 1982 map |
| 6 | Set a DAC entry | `R0` index, `R1` red, `R2` green, `R3` blue (0..7 each) |
| 7 | Clear | `R0` the colour in a bitmap mode; the text screen in `MODE 0` |
| 8 | Print a string | `R0` the address of a zero-terminated string in RAM |
| 9 | Place a sprite | `R0` index, `R1` x, `R2` y, `R3` pattern, `R4` colour |
| 10 | Clear the text screen | |
| 11 | Read the clock | `R0` year, `R1` month, `R2` day, `R3` hours, `R4` minutes, `R5` seconds |
| 12 | Write a serial byte | `R0` |
| 13 | Load a sprite pattern | `R0` pattern, `R1` the address of its thirty-two bytes in RAM |
| 14 | Play a note | `R0` channel, `R1` waveform, `R2` period, `R3` volume |
| 15 | Write one waveform sample | `R0` slot, `R1` index, `R2` the sample |

An unknown vector is a fault that names the vector. No vector is a silent no-op.

---

## The demonstrations

Six listings in [Examples/hb2](../../Examples/hb2), each gated by `hb2_tests` and by
`make hb-check`: a program that runs and leaves the screen as it found it fails both.

| Program | What it shows |
| --- | --- |
| [`palette.s`](../../Examples/hb2/palette.s) | 640 x 480 x 16, sixteen bars, and the DAC rewritten under them |
| [`blitter.s`](../../Examples/hb2/blitter.s) | A frame, a fan of lines, and an XOR bar that erases itself |
| [`sprite.s`](../../Examples/hb2/sprite.s) | A 16 x 16 mask loaded into the bank, two sprites, and a collision |
| [`hello.s`](../../Examples/hb2/hello.s) | The 1982 text map written by hand, and the 1992 print call |
| [`chord.s`](../../Examples/hb2/chord.s) | Four channels, a waveform the program writes, and a meter |
| [`desktop.s`](../../Examples/hb2/desktop.s) | Overlapping windows and a task bar: what a framebuffer is for |

---

## The window

**Tools > HB-2 Model 92…** (`Ctrl+Alt+2`), or `time_warp_ide --hb2`. Four pages:

| Page | What it is |
| --- | --- |
| **Screen** | The glass. It paints the framebuffer the chip composed, through the DAC, in whichever mode the program chose; click it to give the machine the keyboard, and typing reaches `INT 16` |
| **Prompt** | The machine's words, below. `HELP` lists them |
| **Monitor** | The registers, a disassembly at the program counter, and Step, Break and Run-to-breakpoint |
| **Program** | The listing the machine runs: `F9` assembles and runs it, and it can be saved to `A:` |

The frame timer ticks the machine sixty times a second, stops when the program does,
and hands the four-channel mixer to the Studio's speaker while anything is sounding.

---

## HB DOS 2.0

Two drives: `A:` is a 1.44 MB floppy, `B:` a 40 MB hard disk. Directories, 30-character
names, timestamps from the machine's own clock, and `read-only`, `hidden` and `locked`
attributes. Files are charged in 512-byte sectors, a write that does not fit is refused
rather than truncated, and `FREE` accounts for every sector with none lost or
double-counted. The image is text with a format number, and a version-1 volume -- an
HB-1 disk -- reads as a root full of files.

## HB-OS

Eight processes, round-robin on the frame tick, each with its own registers, its own
console and its own window on the desktop. A process prints into its window; the
desktop composes windows in z order with a task bar and a pointer. A process that
faults is killed and named, and the desktop survives it. The clock is the machine's, so
the same number of ticks produces the same screen.

## The prompt

`HELP` lists everything. The words are the machine's:

| Word | Does |
| --- | --- |
| `DIR` `CD` `MKDIR` `RMDIR` | the disk's catalogue and its directories |
| `TYPE` `DELETE` `COPY` `MOVE` `RENAME` `LOCK` `UNLOCK` | files |
| `FORMAT` `FREE` `DRIVE` | the volumes and the sector map |
| `LIST` `NEW` `SAVE` `LOAD` | the listing in the machine's editor, and the disk |
| `RUN` `SPAWN` `TASKS` `KILL` | one program, a process, and what is running |
| `REGS` `DIS` `DUMP` `POKE` `STEP` `BREAK` `RUNTO` | the monitor |
| `MODULE LIST` `MODULE SAVE` `MODULE BOOT` | the bay |
| `SERIAL` `TIME` `CLS` | the serial port, the clock, the screen |

---

## What is built, and what is not

Built, with tests: the bus and its one map; the video chip, its three modes, the DAC,
the blitter, the sprites and the ROM font; the sound chip and the waveform bank; the
pointer, the keyboard, the clock and the serial port; HB-16, its assembler, its
disassembler and its fault reporting; the `INT` services; HB DOS 2.0; HB-OS and the
desktop; the monitor; the module bay; the prompt; the machine snapshot; the window; and
the harness.

**Not built**, and named so that no page promises them: `SYS` in BASIC (a language
program cannot yet call the machine's services), the tape drive and the joysticks'
routing through the IDE's gamepad support, and the module bay's third kind of module
booting a *running* machine's processes rather than a fresh one.

[The proposal](../hb1/model-92.md) is the specification all of this was written
against, and [the roadmap](../ROADMAP.md) says what it cost.
