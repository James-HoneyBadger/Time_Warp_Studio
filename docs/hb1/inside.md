# Inside the HB-1

This is the engineer's book: what the machine is made of, why it is made that
way, and where to put a new word. The user-facing books are the
[Owner's Manual](README.md), [ROMs](roms.md), [HB DOS](hb-dos.md) and
[CONFIG](config.md).

The whole machine is four libraries, and the window itself is seven files:

| File | What it is |
| --- | --- |
| [`retro_computer.hpp`](../../native/include/timewarp/ui/retro_computer.hpp) / `retro_computer_*.cpp` | The window, split by what it does: `ui` (front panel, screen, status), `basic` (the line editor, AUTO, renumbering), `machine` (the ROM, the runners, the canvas, the images), `dos` (the prompt's half of the disk words), `boot` (power, self test, the frame tick) and `settings` (persistence and the dialog), over `support` and `retro_computer_internal.hpp` |
| [`retro_settings.hpp`](../../native/include/timewarp/ui/retro_settings.hpp) / [`retro_settings.cpp`](../../native/src/ui/retro_settings.cpp) | The configuration: palettes, the three-page dialog, the stored group |
| [`hb_dos.hpp`](../../native/include/timewarp/ui/hb_dos.hpp) / [`hb_dos.cpp`](../../native/src/ui/hb_dos.cpp) | The disk: a widget-free class, one JSON image |
| [`panels.hpp`](../../native/include/timewarp/ui/panels.hpp) | `CrtOverlay`, the glass, shared with the IDE |

The machine's **insides** are two more translation units, added in 20.1 because a
machine whose memory is a map nothing reads is a costume:

| File | What it is |
| --- | --- |
| [`hb_memory.hpp`](../../native/include/timewarp/ui/hb_memory.hpp) / [`hb_memory.cpp`](../../native/src/ui/hb_memory.cpp) | The 64K: one address space, screen cells, colour attributes, I/O registers, and a scroll that moves memory |
| [`hb_tube.hpp`](../../native/include/timewarp/ui/hb_tube.hpp) / [`hb_tube.cpp`](../../native/src/ui/hb_tube.cpp) | The screen: a widget that paints that memory, with a cursor, a scrollback and a character set |

It is tested by [`retro_computer_tests.cpp`](../../native/tests/retro_computer_tests.cpp),
which opens real windows and drives the same public methods a user's hands reach.

---

## The shape of it

```text
RetroComputerWindow
├── header            HB-1 identity · ROM selector · SCREEN selector · CONFIG · POWER
├── QSplitter
│   ├── QTabWidget
│   │   ├── PROGRAM   CodeEditor (the same widget the IDE uses)
│   │   └── SCREEN    HbTube, painting HbMemory: the machine's screen
│   └── TurtleCanvas  the same canvas the IDE drains
├── prompt            QLineEdit + RUN / STOP / CLS
├── status            ROM · MEM · MODE · LINE · HB DOS · READY
├── CanvasCommandQueue + CanvasBridge
├── ReplRunner        one persistent session per ROM visit
├── NativeRunner      one whole-program run
├── HbMemory          the 64K: RAM, screen cells, attributes, registers
├── HbDos             HBWARP.dsk
└── programs_         one QString per Language, the ROM memories
```

The window owns its **own** queue and bridge, its own runners, and its own disk.
Nothing in it shares state with the editor's REPL bar, so a program started on
the machine cannot inherit a binding typed in the IDE. That is the single most
important design decision in the file.

---

## How a keystroke becomes ink

Type `FD 100` at the Logo prompt:

1. **`submitCommand()`** trims the line and adds it to the history budget.
2. It echoes it in the ROM's voice: `? FD 100`.
3. It tries the **machine words** — this line is not one.
4. It ensures a **`ReplRunner`** exists for Logo, creating one on first use.
5. The runner submits the line to the engine on its own thread. Output comes back
   as `outputAvailable` and is appended to OUTPUT.
6. The turtle command goes onto the **`CanvasCommandQueue`**; the bridge wakes
   the GUI thread, which drains the queue into **`TurtleCanvas`**.
7. `commandFinished` drains once more, fits the drawing to the view and updates
   the status line.

`RUN` takes the other path, and the differences are deliberate:

- it **stops the REPL session first** — one engine at a time, so two programs can
  never draw into the same tube;
- it prints `* RUN *`, resets the canvas, and starts a **`NativeRunner`**;
- a program that asks for input gets a real dialog, because the machine has no
  keyboard channel of its own;
- at the end it prints `READY.` (or `?STOPPED`, or the engine's error), and if
  the program drew while the screen was in TEXT, it switches to SPLIT once.

Both paths reach the same canvas through the same queue the IDE uses. There is no
second renderer and no second drawing model.

---

## The ROM socket is an enum

`Language` is the socket, and it is also the key for everything else:

| Thing | Keyed by |
| --- | --- |
| The program buffer | `programs_[static_cast<int>(language)]` |
| The stored program | `retroComputer/programs/<settingsKey>` |
| The file's ROM on disk | `DosFile::language` |
| The banner, prompt and version | `romFor(language)` — one switch, one table |

`romFor()` is the whole of a ROM's visible personality: a version, a prompt and a
banner note. Adding a language to the Studio therefore gives it a ROM for free,
as long as `nativeLanguage()` can hand the prompt a native engine. A language
with no engine gets `?NO LANGUAGE ROM` rather than a half-working prompt.

`setLanguage()` is a **cold boot**, and it is worth knowing exactly what it does,
because every behaviour in [roms.md](roms.md) follows from it:

1. If a machine is already booted, write the current program into its buffer.
2. Stop the runner and the REPL session.
3. Swap the buffer for the new ROM's.
4. Clear the screen and the canvas.
5. Print the banner, then the prompt.

---

## The palette is one table

`retroPalette(PhosphorTint)` returns nine colours. They are used by:

- `machineStyleSheet()`, which paints the machine with tokens (`@panel@`,
  `@bright@`, …) rather than `.arg()` — a hundred-line stylesheet with twenty
  placeholders is a place where one missing argument silently shifts every colour
  after it;
- the PROGRAM editor's palette;
- the canvas surface colour handed to the runners, so turtle ink matches the
  phosphor;
- the CRT overlay's tint.

Choosing Amber in CONFIG calls `applySettings()`, which re-derives all four from
the one table. That is why the machine recolours as a whole rather than tinting a
single widget.

---

## Preview, accept, revert

`openSettings()` is a small state machine, and it is the reason CONFIG can be
explored safely:

```text
openSettings()
   ├── remember settings_before_dialog_ = settings_
   ├── previewRequested  → applySettings(preview, /*persist=*/false)
   └── finished
         ├── Accepted   → setSettings(dialog->settings())   → apply + save
         └── Rejected   → applySettings(settings_before_dialog_, false)
```

`setSettings()` is the only writer. A cancel can therefore not leave a setting the
user did not choose, and the dialog itself changes nothing outside itself.

---

## The disk has no widgets

`HbDos` is a plain class: names, a list of `DosFile`, a volume name, an image
path. Every operation returns a `bool` and fills a `QString* error`:

```cpp
bool write(const QString& name, const QString& language,
           const QString& source, QString* error = nullptr);
```

That shape is what lets the same code serve the prompt and the tests. The
window's DOS words are thin: parse the arguments, call `HbDos`, print what
happened in the machine's voice. See [HB DOS](hb-dos.md) for the contract and
[the tests](../../native/tests/retro_computer_tests.cpp) for the proof.

Two invariants are worth naming because they are easy to break:

- **Every mutating operation commits immediately, and rolls the in-memory volume
  back if the write fails.** A `CATALOG` therefore never lists a file the image
  does not contain.
- **A file that does not fit is refused before anything changes.** The check is
  against the volume *after* the write (`used - old + new`), so replacing a file
  with a larger one is judged correctly.

---

## How a statement reaches the screen

`POKE` is not a language feature that happens to draw; it is an output to the machine.

```text
a program's POKE 32768, 72
   └── BasicMachine::poke            (native/src/lang/basic_features.cpp)
        └── Interpreter::machine_poke           the bus
             └── RetroComputerWindow's lambda   (machineBus())
                  └── HbMemory::poke            one byte at $8000
                       └── HbTube::paintEvent   reads the cell, paints the glyph
```

The bus is `Interpreter::MachineBus` -- five hooks: peek, poke, set the cursor, set
the ink and paper, clear the screen. **Unset**, the same statement falls back to the
sparse map a language carries, which is what the IDE editor, `time_warp_check` and
the parity corpora see; that is why a language feature can gain machine hardware
without changing a single recorded answer.

Two threads meet here. The engine calls the bus on the runner's thread and the tube
paints on the GUI thread, so a byte can land while a frame is being drawn. That is
deliberate and it is what the hardware did: a character cell is one byte, so the
worst case is a frame that shows the previous character, never a half-written value.

---

## Adding a machine word

1. Decide whether it is a **machine word** (no language should see it) or a
   language feature. Machine words live in `handleMachineCommand()`. Language
   features belong in the engine, where the IDE gets them too.
2. Add the branch, using `headWord(command)` for the verb and `dosArguments()` /
   `firstArgument()` for the rest. Those two helpers strip quotes and split on
   commas, so a word behaves the same as the DOS words.
3. Print through `appendConsole()` and update `updateStatus()` if the word can
   change the file count or the ROM.
4. Teach `HELP` — the vocabulary table is prose, and a word missing from it is a
   word users will not find.
5. Add a test to `retro_computer_tests.cpp` that types the word through
   `submitCommand()` and reads the console back. The suite is deliberately about
   what a user can see.

---

## Why the Python engine is not involved

Every ROM runs on a native engine: `timewarp_lang` for the line-by-line
languages, `timewarp_scheme` for Scheme, and the assembler and machine behind
CHIP-8. The machine never starts `python3`, and neither does anything else in the product:
the native engines are the only implementation. The parity gate compares them with a
recorded corpus rather than with a live reference. See
[ARCHITECTURE.md](../ARCHITECTURE.md).

That is also why the HB-1 loads instantly and cannot fail for want of an
interpreter: it is the same in-process code the editor runs, wearing a CRT.

---

## Where the story lives in the code

The fiction is not a skin over an unrelated engine; it is the design brief, and
the comments say so where it mattered:

- `hb_dos.hpp` — why a disk is a JSON file and why `INIT` asks twice;
- `retro_settings.hpp` — why a monitor is a palette and not a tint;
- `retro_computer.hpp` — why the prompt is a session and `RUN` is not;
- `panels.hpp` (`CrtOverlay`) — why curvature is a bezel and not a shader;
- `chip8.hpp` — why the machine keeps the 1977 quirks rather than the convenient
  ones.

Reading those five comments is the fastest way to understand what this machine is
for.
