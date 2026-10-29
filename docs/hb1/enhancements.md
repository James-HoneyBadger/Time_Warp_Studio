# HB-1 enhancements

Time Warp Studio 24.0 is the golden release: the machine described in
[the owner's manual](README.md) is finished, frozen and validated by the gates in
[README_TESTING](../README_TESTING.md). Everything on this page is a **proposal for
after 24.0**, written down rather than wished for, so that a future release has a
specification to build to and a way to prove it is done.

Each proposal says what it is, why the machine wants it in 1982 terms, and what
would count as proof. Nothing here changes 24.0.

---

## What the machine is missing, honestly

The HB-1 has the parts a home computer of 1982 had: a processor, memory the screen
is part of, a disk, a printer, a modem, a tape motor that sounds and does nothing
else, and eleven ROMs. The gaps are the ones a period machine would have filled:

| Gap | Why it matters |
| --- | --- |
| The tape motor makes a noise but there is no tape | A 1982 machine's second storage medium is a cassette; the sound is already modelled, so the device is half there |
| `POKE` can write a byte but nothing can run it | The manual says free RAM is "for `POKE` and for machine code", and there is no way to call machine code |
| `MEM` reports 32K because the map says so | A machine sold in 16K and upgraded to 64K is the story of the era, and a fixed size cannot tell it |
| The function keys are not programmable | Every competing machine let a user bind a key, and it is how a lesson is delivered in a classroom |
| The tube has no sprites | A video chip with hardware sprites is what separated 1982's machines from 1980's |
| HB DOS has no copy and no free-space map | `STATUS` counts sectors; it does not show where they are |

---

## Specification proposals

### 1. The tape drive

**What.** `CSAVE "NAME"` and `CLOAD "NAME"` on a virtual cassette. A tape holds an
ordered list of files, a header word each (name, length, checksum), and a counter
the prompts name. Loading is sequential, so `CLOAD` past a file means "next", and
loading a name that has already gone by answers with the counter value rather than
pretending the tape is a disk.

**Why.** The tape motor is already the sixth sound in
[the machine's voice](README.md#the-machines-voice); the device it belongs to is
missing. More importantly, a cassette forces the two habits that a disk hides:
saving once and loading in order, and understanding that a tape is a position rather
than a tree.

**What changes.** `hb_tape.{hpp,cpp}` beside `hb_dos`, one JSON image under
`~/.time_warp/tapes` per cassette, two machine words, and a CONFIG row naming the
cassette and the counter position. `SAVE` stays the disk's; the tape is a second
device, not a replacement.

**Proof.** `retro_computer_tests` gains a case per promise: a save then a rewind
then a load returns the same listing; a load of a name behind the head reports the
counter and does not move; a tape whose checksum fails says so and leaves the
program alone; the motor sound plays once per device word, as it does for the disk.

### 2. A monitor, and machine code that runs

**What.** A `MON` word that opens a one-line monitor, and `SYS address` in BASIC
that calls the byte-code interpreter at `address`. Sixteen documented opcodes:
`LDA`, `STA`, `LDX`, `INX`, `ADD`, `SUB`, `JMP`, `JZ`, `CMP`, `OUT`, `IN`, `HLT`,
`SHL`, `SHR`, `AND`, `ORA` — enough for a counting loop, a screen fill and a delay.

**Why.** The map already says `$0000`–`$7FFF` is "free RAM, for `POKE` and for
machine code", and only half that sentence is true. A monitor is also the most
period-correct tool there is: it is how a 1982 owner learned what a processor was.

**What changes.** `hb_monitor.{hpp,cpp}` with a `HbMonitor::run(memory, address)`
that steps the byte code, a `SYS` statement in the BASIC executor guarded on
`machine_attached()`, and a manual page listing the opcodes and their encodings.

**Proof.** `retro_computer_tests` writes a five-byte program with `poke()` and
asserts the screen changed; the monitor gets a round trip (disassemble, edit,
run); a `SYS` into ROM or into an empty cell reports rather than running zeros.

### 3. Programmable function keys

**What.** `KEY n,"text"` binds F1–F8 to a string the prompt inserts; `KEY n` alone
clears it; `KEYS` lists them. Bindings are saved on the disk image so a classroom
machine keeps its macros.

**Why.** It is how the era typed `PRINT ` forty times an evening, and how a teacher
puts `RUN "DEMO"` one key away from a learner.

**What changes.** A `HbKeys` value type, eight words in the prompt vocabulary, and
a row in CONFIG to clear them all.

**Proof.** `retro_computer_tests`: a binding survives a RAM change and a ROM
change; a cleared binding inserts nothing; `KEYS` prints exactly what is bound.

### 4. Hardware sprites and a collision register

**What.** Eight sprite slots in the I/O page (`$A009`–`$A00F`): x, y, shape
address, enable, and one collision flag the program reads with `PEEK`. The tube
composes them over the character cells.

**Why.** It is the feature that made 1982 machines feel like 1982 machines, and it
gives `POKE` something visible to control beyond characters.

**What changes.** `HbTube` gains a sprite pass; the memory map document gains the
register table; a demo program draws a moving sprite with nothing but `POKE`.

**Proof.** The composition order is asserted on pixels (sprite over cell, later
slot over earlier), the collision flag is set when two sprites overlap and cleared
by a read, and a slot pointing at ROM draws nothing rather than garbage.

### 5. RAM size as a setting

**What.** CONFIG offers 16K, 32K, 48K and 64K. `MEM` reports the configured size,
and `POKE` above it is refused with the address and the limit.

**Why.** Every machine of the period was sold in steps, and a program written for a
16K machine is a different exercise from one written for 64K. It also makes `MEM` a
reading of the configuration rather than a constant with a story attached.

**What changes.** `HbMemory` takes a size; the self-test counts the RAM it actually
has; the status line reads from the machine.

**Proof.** The self-test prints the configured size; a `POKE` one byte past the
limit is refused and one byte inside it lands; a saved program that needs more than
the configured RAM is refused by `LOAD` with the shortfall named.

### 6. A light pen, a serial port and note registers

**What.** Three small devices on the I/O page: a light-pen register pair returning
the cell under the pointer, a serial transmit/receive pair routed through the
existing `timewarp_io` `SERIAL` support, and a note register (frequency, duration)
that replaces the single sound latch for programs that want a melody.

**Why.** They are the three peripherals a 1982 advert promised, they cost one page
of registers, and each turns an existing native capability into something the
machine can reach with `POKE`.

**What changes.** Register definitions and two manual pages; `SOUND` gains a
machine-latch path so `SOUND` and the register agree.

**Proof.** A pointer event at a known cell answers with that cell; `SERIAL` through
the register and through the language command produce the same bytes; a note
written to the register reaches the audio queue once.

---

## Functionality proposals

### 7. HB DOS 1.1

**What.** `COPY "OLD","NEW"`, `FREE` (a sector map drawn in the tube), timestamps
in the catalogue, and wildcard `RENAME`. `STATUS` prints the map rather than only a
count.

**Why.** A disk without `COPY` is a disk that makes the user re-type. A map is what
teaches that 512 sectors are real: a program that will not fit is refused today,
and this shows *why*.

**What changes.** `HbDos` gains three words and one field per catalogue entry; the
image format carries a version bump, and an older image is read as version 1.

**Proof.** A copy of a locked file is allowed and of a missing one refused; the map
accounts for every sector; a version-1 image loads and is written back as version 2
without losing a program.

### 8. Autoboot from the disk

**What.** On a cold boot, if the volume holds `STARTUP`, the machine lists it and
runs it after the banner. CONFIG has a switch to turn it off.

**Why.** An autobooting disk is what made a home computer a device rather than a
toy: put the disk in, switch on, and the program is there.

**What changes.** One call in the boot sequence after the self-test, one CONFIG
row, and a `STARTUP` badge in the catalogue.

**Proof.** A volume with `STARTUP` prints the banner and runs; without it the
machine stops at `READY.`; with the switch off neither happens; a `STARTUP` that
fails leaves the prompt usable.

### 9. Editing that names what it edits

**What.** `EDIT n` jumps the PROGRAM page to line `n`, `FIND "text"` and
`CHANGE "old","new"` work on the listing, and `LIST n-` runs to the end.

**Why.** The line editor has `AUTO`, `RENUM`, `DELETE`, `UNDO` and `TRON`; a
learner still has to scroll to find a line by eye, and a teacher still cannot
rename a variable across a listing.

**What changes.** Four words in the machine's vocabulary, all acting on the same
listing `RENUM` already rewrites, with the same rule that a string is prose.

**Proof.** `CHANGE` does not touch a string literal or a `REM`; `FIND` reports
every hit with its line number; `EDIT` moves the caret and the current-line mark.

### 10. Replayable prompt scripts

**What.** `DO "NAME"` runs a saved sequence of prompt lines, with `PAUSE` between
them, and `SCRIPT` starts recording what is typed.

**Why.** It is a batch file, the period word for it, and it is how a lesson, a
demo or a bug report becomes one thing a teacher can hand over. It also gives the
machine's own demonstrations a file format.

**What changes.** A `.do` file beside the program on the disk image, or the same
text in the program buffer; the prompt reads one line, runs it, and moves on.

**Proof.** A recorded script replays to the same screen text; a failing line stops
the script and names it; a paused script resumes on a key.

---

## Order of work, if the proposals are taken

1. **RAM size** — smallest change, and it makes the self-test more honest.
2. **Tape** — a second device with the sound already present.
3. **DOS 1.1** — `COPY` first, the map second.
4. **Autoboot** — one call, once the disk can be relied on.
5. **Monitor and `SYS`** — the largest change; it needs its own manual page.
6. **Sprites, light pen, serial, notes** — one page of registers at a time.
7. **Function keys and scripts** — usability, once the machine underneath is done.

Each step is a document, a value type, a test and a demo program, in that order —
the same order the rest of this project is built in.
