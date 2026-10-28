# HB DOS 1.0 Reference Manual

```text
HB DOS 1.0   HB SYSTEMS 1982   VOLUME HBWARP
```

HB DOS is the HB-1's disk operating system. It is deliberately a 1982 one: one
volume, named files, a catalogue, a write-protect tab per file, a size column in
sectors, and one destructive word that has to be said twice. It is also, quietly,
one JSON file you can copy to another machine, read in a text editor, and put
under version control if you are so inclined.

- **Owners' manual:** [HB-1](README.md) · **ROMs:** [roms.md](roms.md) ·
  **Settings:** [config.md](config.md)
- **The class behind it:** [`hb_dos.hpp`](../../native/include/timewarp/ui/hb_dos.hpp)

---

## What the disk is

| | |
| --- | --- |
| Name | `HBWARP` (the volume's name; `DISK VOLUME 254` in the catalogue) |
| Geometry | 512 sectors of 256 bytes — 128 KiB, in the measurements a 5.25" disk was sold in |
| Files | Named programs, each with the ROM that can run it, a lock bit and a sector count |
| Directories | None |
| Volumes | One |
| Image | `~/.time_warp/disks/HBWARP.dsk`, a JSON file |
| Relocated by | `TIME_WARP_DATA_DIR` |

A file's size is the number of 256-byte sectors its source occupies, rounded up,
with a minimum of one. A program of 257 bytes is two sectors; an empty program is
one. The count is over the UTF-8 bytes of the source, so a program full of
accented characters is bigger than the same program in plain ASCII — which is
true of real disks too, and worth knowing when a save is refused.

The disk belongs to the machine, not to a ROM. `CATALOG` looks the same in BASIC
as it does in Smalltalk, and a Logo file saved from the Logo ROM is visible from
every other ROM.

---

## The words

The prompt reads the first word, upper-cases it, and treats the rest as arguments.
Quotes are optional: `SAVE HELLO` and `SAVE "HELLO"` are the same request.
A comma separates arguments outside quotes, so `RENAME "OLD","NEW"` and
`RENAME OLD NEW` both work.

| Word | Alias | Arguments | What it does |
| --- | --- | --- | --- |
| `CATALOG` | `CAT` | — | List the disk |
| `SAVE` | — | `"NAME"` | Write the PROGRAM page to the disk |
| `LOAD` | — | `"NAME"` | Read a file into memory, cold-booting its ROM if needed |
| `RUN` | — | `"NAME"` | `LOAD`, then run |
| `DELETE` | `ERA` | `"NAME"` | Erase a file |
| `RENAME` | `REN` | `"OLD","NEW"` | Rename a file |
| `LOCK` | — | `"NAME"` | Set the write-protect bit |
| `UNLOCK` | — | `"NAME"` | Clear it |
| `VERIFY` | `VER` | `"NAME"` | Compare a file with the PROGRAM page |
| `TYPE` | — | `"NAME"` | Print a file to OUTPUT |
| `STATUS` | — | — | Volume, sectors, file count, image path |
| `LLIST` | — | — | Print the PROGRAM page to the paper tray |
| `LPRINT` | — | `"TEXT"` | Print a line to the paper tray |
| `INIT` | — | — | Erase the volume — twice |

`RUN` with no argument at all is the machine's own word for "run the program",
not a disk command; see [the prompt](README.md#the-prompt).

### Names

A name is trimmed, has one layer of matching quotes removed, and is upper-cased.
It must be 1 to 30 characters of letters, digits, spaces, `-`, `_` or `.`.
Anything else is `?BAD FILE NAME`.

That is why the catalogue is shouty, and why `SAVE Hello World` and
`SAVE "hello world"` both produce `HELLO WORLD`. It is also why a name cannot be
mistaken for a path: there are no subdirectories to escape into.

---

## The catalogue

```text
CATALOG
DISK VOLUME 254   HBWARP   512 SECTORS

 BASIC      001  HELLO
*LOGO       001  SQUARE

2 FILES   2 SECTORS USED   510 FREE
```

Read left to right:

- **`*`** — the file is locked. A space means it is not.
- **The type column** — the ROM that can run the file, in capitals, padded to ten
  characters: `BASIC`, `LOGO`, `SMALLTALK`, `CHIP-8`.
- **The size** — three digits, zero-padded: sectors used by that file.
- **The name** — as catalogued.

Files appear in the order they were first saved. Renaming moves the name, not the
position; rewriting a file keeps its place.

`STATUS` is the rest of the story:

```text
STATUS
HB DOS 1.0   VOLUME HBWARP
512 SECTORS   3 USED   509 FREE
2 FILES
IMAGE /home/you/.time_warp/disks/HBWARP.dsk
```

---

## Each word in full

### `SAVE "NAME"`

Writes the PROGRAM page. The file records the ROM you are in, so the disk knows
how to run it later.

```text
SAVE "HELLO"
HELLO SAVED   2 SECTORS
```

Refuses, in this order:

| Situation | Message |
| --- | --- |
| No name given | `?FILENAME NEEDED` |
| The PROGRAM page is empty | `?NO PROGRAM` |
| The name is not a legal name | `?BAD FILE NAME` |
| A file of that name is locked | `?FILE LOCKED` |
| It would not fit | `?DISK FULL` |
| The image cannot be written | `?DISK ERROR` |

Saving over an unlocked file replaces it. The old size stops counting against
the volume at the same moment the new one starts, so re-saving a program that
grew by a sector is checked against the volume as it will be, not as it was.

A refused save changes nothing. The volume in memory is rolled back if the image
cannot be written, so `CATALOG` never lists a file the disk does not have.

### `LOAD "NAME"`

Reads a file into memory. If the file belongs to a different ROM, the machine
cold-boots that ROM first — the disk knows what can run its files.

```text
LOAD "SQUARE"
SQUARE LOADED
```

### `RUN "NAME"`

`LOAD`, then run. This is how a learner starts the afternoon: `RUN "SQUARE"`.

### `DELETE "NAME"` / `ERA "NAME"`

Erases an unlocked file, and only an unlocked file.

```text
DELETE "HELLO"
HELLO DELETED
```

A locked file refuses with `?FILE LOCKED`. That is the entire point of the lock.

### `RENAME "OLD","NEW"` / `REN`

Renames a file in place.

```text
RENAME "SQUARE","BOX"
SQUARE RENAMED TO BOX
```

Refuses with `?FILE NOT FOUND` if the old name is not there, and `?FILE EXISTS`
if the new name is already taken by a different file. Renaming a file to the name
it already has is allowed and does nothing surprising.

The lock travels with the file: renaming a locked file is allowed, and the new
name is still locked. The lock protects the *contents* from being overwritten or
erased, not the name from being changed — exactly as a write-protect tab behaves.

### `LOCK "NAME"` / `UNLOCK "NAME"`

```text
LOCK "SQUARE"
SQUARE LOCKED
```

`?FILE NOT FOUND` if there is no such file. Locking an already-locked file is
harmless.

### `VERIFY "NAME"` / `VER`

Compares the file with the PROGRAM page, byte for byte.

```text
VERIFY "HELLO"
HELLO VERIFIED
```

or

```text
VERIFY "HELLO"
?HELLO DIFFERS FROM MEMORY
```

This is the word a learner uses after editing: load, change, verify, find out
whether the change is the one they meant.

### `TYPE "NAME"`

Prints the file. The header names the file, its ROM and its size; the lines
follow unchanged.

```text
TYPE "HELLO"
HELLO  BASIC  001 SECTORS
10 PRINT "HELLO, TIME TRAVELLER"
20 GOTO 10
```

### `INIT`

The only destructive word, and the machine makes you mean it:

```text
INIT
?INIT ERASES EVERY FILE ON THE DISK
TYPE INIT AGAIN TO GO AHEAD
INIT
HB DOS 1.0   VOLUME HBWARP
ALL FILES ERASED
```

- The second `INIT` must be the **next** line's first word. Any other line —
  a machine word, a language command — is a change of mind and cancels the
  pending erase. (An empty prompt line does nothing at all, including that.)
- `INIT` erases locked files too, and resets the volume's name.
- The warning is the machine refusing in its own voice; the question mark is how
  this machine says "are you sure".

---

## Files and their ROMs

Every file stores the ROM that saved it, as the language's settings key
(`basic`, `logo`, `smalltalk`, …). Three things use it:

1. **`CATALOG` and `TYPE`** show the ROM in capitals.
2. **`LOAD` and `RUN`** cold-boot that ROM when it is not the one in the socket.
3. **Nothing else.** The file is source text; the ROM is a hint, not a format.

If you edit the image by hand and put a nonsense ROM in a file, `LOAD` still loads
it; the catalogue shows the key in capitals and the program arrives in the ROM
you were already using.

---

## The image

One JSON document, written indented so a human can read it and a diff can be
reviewed:

```json
{
    "dos": "HB DOS 1.0",
    "volume": "HBWARP",
    "files": [
        {
            "name": "HELLO",
            "language": "basic",
            "source": "10 PRINT \"HELLO, TIME TRAVELLER\"\n20 GOTO 10",
            "locked": false
        }
    ]
}
```

| Situation | What happens |
| --- | --- |
| The file does not exist | The machine has a blank, formatted volume — not an error |
| The JSON is malformed | `?DISK UNREADABLE` at boot, and the volume is empty for the session |
| The file cannot be read | `?DISK ERROR` |
| The directory cannot be made | `?NO DISK` — no saving, ever, rather than a silent lie |
| A write fails part-way | The in-memory volume is rolled back; the old image stays |

The image is written in full on every change. At 128 KiB of addressable programs,
that is not a performance question; it is what makes "the disk is a file" true.

Copy the file to another machine and the programs go with it. Put it in a class
folder and a teacher has every learner's saved program. Delete it and the machine
has a blank disk, because a 1982 machine would.

---

## A worked session

One afternoon, start to finish:

```text
CATALOG
DISK VOLUME 254   HBWARP   512 SECTORS

 NO FILES

0 FILES   0 SECTORS USED   512 FREE

10 PRINT "HELLO, TIME TRAVELLER"
LINE 10 STORED
20 PRINT "THE MACHINE REMEMBERS YOU"
LINE 20 STORED

RUN
HELLO, TIME TRAVELLER
THE MACHINE REMEMBERS YOU

SAVE "HELLO"
HELLO SAVED   1 SECTORS

LOCK "HELLO"
HELLO LOCKED

DELETE "HELLO"
?FILE LOCKED

INIT
?INIT ERASES EVERY FILE ON THE DISK
TYPE INIT AGAIN TO GO AHEAD

CATALOG
DISK VOLUME 254   HBWARP   512 SECTORS

*BASIC      001  HELLO

1 FILES   1 SECTORS USED   511 FREE
```

Note the last part: the pending `INIT` was cancelled by `CATALOG`, which is
exactly the behaviour the machine promises — saying anything else is a change of
mind.

---

## Where this is proven

The disk has its own tests, because a filesystem that loses a program is worse
than no filesystem. `native/tests/retro_computer_tests.cpp` drives the same words
a user types — `SAVE`, `LOCK`, `DELETE`, `RENAME`, `VERIFY`, `TYPE`, `INIT` and a
restart — and asserts the catalogue, the refusals and the image on disk. The
class itself ([`hb_dos.cpp`](../../native/src/ui/hb_dos.cpp)) has no widgets, so
the same code serves the prompt and the tests.

---

## The printer, which is a file

The HB-1 has no printer port. `LLIST` and `LPRINT` append a headed page to
`paper.txt` in `~/.time_warp/print`, and the console answers with the line count and
the file name:

```text
LLIST
PRINTED 3 LINE(S) TO paper.txt
```

Appending rather than replacing is deliberate: a printer that overwrote its own last
page would lose a listing somebody had already taken off the machine. `LPRINT` with
nothing after it is `?LPRINT WHAT` rather than a blank page.

The paper is a file because that is what the output of a printer is -- something to
keep, read, back up, or hand to a real printer -- and because it is the one output of
this machine that survives being switched off.

## What it will not do

- No directories, links, wildcards or file dates. A 1982 DOS would have had some
  of these; this one is deliberately smaller than the machine it runs on.
- No partial saves and no truncation. A program that does not fit is refused with
  `?DISK FULL`, and the file that is there stays exactly as it was.
- No undelete. `INIT` is the one way to lose everything, and it asks first.
- No case-sensitive names. The volume is from 1982; it shouts.

Those absences are the lesson. The disk is small, finite and legible; every byte
it holds is a program somebody typed.
