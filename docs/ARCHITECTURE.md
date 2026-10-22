# Architecture Guide

## Time Warp Studio v17.1.0 — System Design and Implementation

> **This page describes the shipping product.** It was rewritten alongside the move
> to C++20 / Qt6. Earlier revisions documented the PySide6 implementation, which has
> been removed, and named modules (`ui/*.py`, `themes.py`, the Python threading
> model) that no longer exist. Nothing here describes code that is not in the tree.
> Where the interface carries a control whose feature is not built yet, this page
> says so; [ROADMAP.md](ROADMAP.md) tracks the work.

---

## Table of Contents

1. [Overview](#overview)
2. [Process and thread model](#process-and-thread-model)
3. [The canvas command queue](#the-canvas-command-queue)
4. [The Qt application](#the-qt-application)
5. [The native engines](#the-native-engines)
6. [The language bridge](#the-language-bridge)
7. [Extension points](#extension-points)
8. [Tests and gates](#tests-and-gates)
9. [Configuration and persistence](#configuration-and-persistence)
10. [What is not here yet](#what-is-not-here-yet)

---

## Overview

Time Warp Studio is a desktop workbench for old-school programming: ten
interpreters from 1964–1980 — **BASIC, APL, Logo, PILOT, FOCAL, Scheme, CHIP-8,
COMAL, REXX and Smalltalk** — with an integrated turtle graphics canvas, and two
complete machines. It is a C++20 / Qt application — Qt 6 for the release, Qt 5.15
for the Windows 7 build — and all ten languages run in C++ inside it. The Python
engine ships as the **reference** the parity gates, the command matrix and the
lesson check use; no program is sent to it.

So the product is **one GUI process**. The child process remains only for a
lesson check, where a second interpreter must not share the document's state;
that channel is JSON lines, one message per line — program text going in and a
stream of JSON messages coming back.

### The moving parts

| Part | Sources | Runs in | Role |
| --- | --- | --- | --- |
| `timewarp_ui` — the IDE | `native/src/ui/`, `native/include/timewarp/ui/` | GUI thread | Documents, editor, console, canvas, menus, settings |
| `timewarp_canvas` | `native/src/canvas/` | producer: interpreter thread; consumer: GUI thread | Carries drawing across the thread boundary |
| `timewarp_canvas_qt` | `native/src/canvas/qt_bridge.cpp` | GUI thread | Qt glue for that queue: wakeup and draining |
| `timewarp_scheme` | `native/src/scheme/` | runner thread, in-process | Scheme, on an explicit-stack machine with no native recursion |
| `timewarp_lang` | `native/src/lang/` | runner thread, in-process | The shared interpreter loop and every line-by-line executor: BASIC, FOCAL, PILOT, COMAL, APL and REXX, plus whole-program CHIP-8, Logo and Smalltalk |
| `timewarp_debugger` | `native/src/debugger/` | its own `QThread` | Runs a Scheme program and services pause / step / breakpoint requests |
| `timewarp_expr` | `native/src/token.cpp`, `lexer.cpp`, `evaluator.cpp` | runner thread, in-process | The ported expression evaluator, linked into `timewarp_lang` — see the note below |
| `timewarp_lessons` | `native/src/lessons/` | GUI thread, or a command | The curriculum, the verifier, the learner's progress and the classroom. See the libraries section |
| the reference engine | `native/bridge/` | a separate Python process, in development only | The fixture the parity corpus was recorded from and the harnesses drive. Not installed, and never reached by the application |

> **`timewarp_expr` is part of a Run now.** It builds, it has 485 checks of its
> own, `native/tools/differential_check.py` compares it against the Python
> reference on a fixed corpus, and `timewarp_lang` links it: BASIC, FOCAL,
> PILOT, COMAL, APL and Logo evaluate every arithmetic expression through it. The
> parity gate is what keeps the native evaluator honest.

### Two contracts holding the design together

**Interpreters do not touch widgets.** A language engine never sees a `QWidget`. It
turns turtle calls into `CanvasCommand` values and pushes them into a
`CanvasCommandQueue`; the GUI thread drains that queue and paints. That is what
makes a program drawing 100,000 segments cost 100,000 lock-free appends rather than
100,000 Qt signal deliveries.

**Executors are stateless command processors.** An engine turns a statement into
text and nothing else. No engine holds a reference to the canvas, the console or the
window, so the same engine is usable from a test with no GUI at all.

---

## Process and thread model

There are three places code can run, and the rules differ between them.

| Context | Owns | May touch |
| --- | --- | --- |
| **GUI thread** | Every widget, `QSettings`, the canvas display list | Anything |
| **Runner thread** — `debugger::DebuggerRunner`, a `QThread` | A running Scheme program and its machine state | The canvas queue, through a host adapter. Never a widget |
| **Lesson check** — `lessons::ProgressStore` | The curriculum and the learner's program | `RecordingHost` instead of a canvas, so nothing on screen is touched |

Every language runs in process. There is no worker process: the Python engine is a
development fixture, not a runtime component, and nothing in the application starts
it.

```mermaid
flowchart LR
    subgraph GUI["GUI thread"]
        W[MainWindow] --> C[TurtleCanvas]
        B[CanvasBridge] --> C
        LP[LessonPanel] --> LS["lessons::ProgressStore"]
        DR[DebuggerRunner] -. queued signals .-> W
    end
    subgraph RT["runner thread"]
        DR2["DebuggerRunner::run()<br/>Scheme machine"] --> H[QueuedTurtleHost]
        NR["NativeRunner<br/>timewarp_lang"] --> H
    end
    H -- "push() then wake()" --> Q[(CanvasCommandQueue)]
    Q --> B
    LS --> RH[RecordingHost]
```

### Control going *in* deliberately does not use slots

`DebuggerRunner` subclasses `QThread`, and its `run()` **is** the interpreter loop. A
thread cannot spin an interpreter loop and a Qt event loop at the same time, so that
thread has no event loop and a queued slot invocation to the runner would never be
delivered — it would sit in a queue nobody reads.

That interacts with a classic Qt trap: a `QThread` *object* lives in the thread that
constructed it, so its slots run there rather than in `run()`. Subclassing `QThread`
and expecting `run()`'s thread to receive slot calls is the most common Qt threading
mistake.

The consequence is deliberate. `request_stop()` and the rest are **plain thread-safe
methods, not slots**: they publish an atomic command and bump a notification counter,
and they never touch interpreter state. The reverse direction is ordinary Qt and is
used freely — signals emitted from `run()` arrive queued on the GUI thread.

The park/unpark word is *incremented* rather than merely notified, because
`std::atomic::wait` re-checks the value after every notification and goes back to
sleep if it has not changed. Calling `notify_all()` on a word nobody modified
releases nobody. A counter also means the runner cannot miss a request, since the
sequence number is read *before* the command is tested.

### Latency

The runner consults its controller at the **start of every compound form**, so a
pause or a step lands within one expression of being requested rather than at an
arbitrary quantum boundary. With no controller attached the cost disappears entirely.

### Closing the window during a runaway program

`MainWindow::closeEvent` stops every engine before it does anything else. `~QThread`
joins and the bridge worker holds a child process, so a window closed mid-run would
otherwise appear to hang. The join is bounded by one form, because that is how often
the stop flag is checked.

---

## The canvas command queue

### The problem it solves

An interpreter runs on its own thread and wants to draw; Qt widgets may only be
touched from the thread that owns them. The removed Python application bridged the
two by emitting a Qt signal per turtle call — one queued-connection post, one heap
allocation and one event-loop wakeup for every `forward(...)`. A turtle program in a
tight loop emits those hundreds of thousands of times a second, so the marshalling
cost dwarfed the drawing.

The queue replaces "one signal per command" with "one buffer for all of them": the
producer appends to a preallocated ring with no allocation and no syscall, and the
consumer drains a whole batch under a single wakeup.

### How it works

- **A bounded byte ring with free-running counters.** `head` and `tail` are monotonic
  byte offsets and are never wrapped, so both "empty" and "full" are representable and
  the buffer index is just `offset & mask`. Capacity is a power of two, so the mask
  replaces a modulo.
- **Release/acquire pairing.** The producer writes a whole record and *then* publishes
  `head` with a release store; the consumer acquires `head`, so observing
  `tail < head` guarantees the entire record is readable. `tail` is published the same
  way in the other direction. Each counter has exactly one writer, which is what makes
  this lock-free without a mutex.
- **Variable-length records.** A 72-byte `CanvasCommand` header, then an optional
  point list and optional UTF-8 text, with the length in the header. No arbitrary
  limit on either.
- **Blocking without a mutex.** When the ring is full the producer parks in
  `std::atomic::wait`: no `std::mutex`, no `condition_variable`, no kernel object
  until the wait spins down. The waiter parks on a *dedicated* wakeup word that every
  notifier increments — see the thread model above for why that is not a detail.

### Policy: never drop

A full ring **blocks the producer**; it never discards a command. A dropped turtle
segment is a wrong picture that looks like a correct one, which is the failure mode
this repository treats as the worst kind. `push_for()` and `wait_for_data()` are the
bounded variants, and they poll, because C++20 has `wait` / `notify_one` /
`notify_all` but not `wait_for` / `wait_until` — those are C++23.

### Coalescing

Pushes that are `is_coalescible()` and have an **identical adjacent predecessor**
collapse into one record. It happens in a producer-private staging buffer, never by
rewriting a published slot, because a 72-byte store is not atomic. In practice a
million state pushes coalesce to a few hundred records, so the Qt bridge posts
hundreds of wakeups instead of a million.

### The Qt side

`CanvasBridge` — in `qt_bridge.cpp`, the only Qt-facing file in the canvas library —
drives the queue from the GUI event loop. Its wakeup is a **two-phase handshake, not
a latch**: `drainNow()` clears the flag, drains, and then loops while the flag was set
again. A plain latch strands commands: clearing *after* a drain lets a push landing
mid-drain see the flag still set, skip its post, and then wait for a wakeup already
consumed; clearing *before* a drain has the mirror-image problem. The two sides agree
through an atomic read-modify-write on the same word, and those are totally ordered,
so exactly one of them takes responsibility. The worst case is a redundant drain that
applies nothing.

Signals are per **batch**, never per command: the bridge drains into a plain callback
and emits one signal carrying a count.

---

## The Qt application

### The window

`time_warp_ide` → `timewarp::ui::MainWindow`. The widget tree — editor column on the
left, output and tooling on the right, function-key bar underneath — and every sizing
decision that changes how the window behaves when resized were translated from the
removed PySide6 implementation: splitter orientations and handle widths,
`childrenCollapsible(false)` where it matters, stretch factors and the initial
`setSizes()` values. Those numbers are load-bearing; they are what stops the REPL bar
being dragged to nothing and what gives the editor 60% of the width. They are
transcribed rather than re-derived, and asserted by `ui_layout_tests`.

Three deliberate departures:

- **Every widget is named.** Qt needs an `objectName` for `saveState()` /
  `restoreState()` to persist dock and toolbar geometry at all, the stylesheet scopes
  by it, and the layout tests find nodes with it.
- **Splitter positions are persisted**, which the Python window did not do: a carefully
  dragged editor/output divider survived no restart.
- **Menus delegate.** The action catalogue belongs with the feature that owns it, not
  in one 600-line method.

### Documents

A document's identity lives on its widget: the file path is a dynamic property on the
editor and the language is derived from that path. A parallel table of paths keyed by
tab index would have to be repaired on every tab close, move and reorder, and the one
time it was not would load the wrong file's text into the wrong tab.

Two rules worth knowing:

- **A failed open has two modes.** `OpenFailure::ReportToUser` shows a dialog, which is
  right for a file the user just picked. `OpenFailure::Silent` is required for files
  named on the command line: a modal dialog at startup blocks a script or a CI run
  forever, and a mistyped path is exactly when that must not happen.
- **An already-open file is raised, not loaded twice**, so the same file cannot be
  edited in two places and saved over itself.

### The editor

`CodeEditor` is a `QPlainTextEdit` with a line-number gutter, a highlighted current
line, cursor tracking, per-language syntax colouring, breakpoint markers and an
optional minimap. Find/replace is `FindBar` over the editor's own search; the
outline dock is `OutlinePanel` over the document's language structure; Format Code
is the formatter service. They are separate widgets and services because each has
to work without a window and each has its own test.

The colouring is `SyntaxHighlighter`, and its rules are per language rather than one
shared approximation: REXX comments are `--` and `/* ... */`, Smalltalk's comment is a
**double**-quoted string while its string is a single-quoted one, FOCAL has no comment
syntax at all, and PILOT's units are a letter and a colon at the start of a line. The
keyword tables are the engine's own command names, so the colouring cannot teach a
word the engine would reject.

Searching is split the same way. `CodeEditor` owns it — `findNext`, `matchCount`,
`replaceAll`, `replaceCurrent` — because the document is there, and `FindBar` is the
row of controls that drives those four calls. That is what lets the wrapping, the
case handling and the single-undo-step Replace All be tested against a document with
no window in the stack.

Persistence is in two halves on purpose. `saveLayout` restores the window and
`saveSession` restores the *documents*, so `--reset-layout` can put the window back
to its defaults without losing the list of files somebody was working on. The recent
list is rebuilt every time the menu opens rather than once at startup, for the same
reason the grid's tick is: a menu that remembers its own contents disagrees with
settings the moment anything else writes them.

### The turtle canvas

`TurtleCanvas` is the graphics surface, and it is a **consumer**: the interpreter never
touches it.

- **A display list, not a pixmap.** Repainting on resize, on zoom and on pan needs the
  drawing again, so the ink is kept as geometry in world coordinates. A pixmap would go
  blurry when zoomed and could not be re-projected at all.
- **The turtle state is mirrored here.** The host adapter translates turtle calls
  one-to-one and statelessly — a `Forward` command carries a distance, not the heading
  to move along or the pen colour to draw with — so something has to replay the sequence
  and hold the state. The GUI is where it belongs; the alternative would be copying the
  state into every command.
- **The coordinate conventions are the reference's, transcribed rather than chosen**,
  from `graphics/turtle_state.py`: heading 0 is **north** and increases clockwise,
  `forward` is `x += d·sin(h), y += d·cos(h)`, and `circle` is a polygon walk. A canvas
  that draws a mirrored or quarter-turned version of the same program is worse than no
  canvas at all. `turtle_canvas_tests` asserts all three.
- **History is in draw order, and scrubbing only limits the view.** A third list
  records whether each primitive is a stroke or a shape and its index, so a
  rectangle painted between two lines comes back between them. `paintEvent`
  paints the first `visibleFrameCount()` entries; `renderedDrawing`, export and
  `fitToContent` use the whole list. Keeping the two ideas apart is what lets the
  scrubber be a view control rather than a mode the rest of the window has to
  know about.

### The two machines

Both machines are documented as machines, not as features: [MACHINES.md](MACHINES.md)
is the premise, the [HB-1 books](hb1/README.md) and the
[CHIP-8 books](chip8/README.md) are the manuals, and
[Inside the HB-1](hb1/inside.md) is the engineer's companion to this section. The
short version: they are not skins. Each owns its clock, its input and its
storage, and both draw through the same queue as the editor.

### The CHIP-8 machine

CHIP-8 is two things in this tree, and the split is deliberate: `assemble_chip8`
turns a listing into bytes, and `Chip8Machine` executes bytes. An assembled wrong
opcode and a wrongly executed right opcode are different bugs, so they have
different tests. `execute_chip8` binds them into the editor's whole-program
executor.

`Chip8Window` is a second top-level window, not a mode of the main one: the
editor's Run keeps the whole-program executor, while `Chip8Emulator` owns a
`QTimer` at about 60 Hz, executes a configurable number of opcodes per frame,
ticks the delay and sound timers once a frame, and applies key presses as they
arrive. The clock and the keypad are the only things the window adds — the
assembler, the machine, the fonts and the quirks are the same code the editor
runs. `Chip8Screen` paints the 64x32 framebuffer (128x64 in SuperChip hi-res)
with an integer scale, `Chip8Keypad` mirrors the same key state, and the inspector
reads a `Chip8Snapshot` copied after each frame, so it can never read a
half-executed instruction. The window is parented to `MainWindow` and reused;
closing it alone ends the run, and the next invocation builds a fresh machine
rather than touching a dead one. The API and the quirks are in
[the instruction set](chip8/instruction-set.md).

### The HB-1

`RetroComputerWindow` is the third top-level machine window, alongside the CHIP-8
emulator. It is deliberately self-contained: its own `CanvasCommandQueue` and
`CanvasBridge`, its own persistent `ReplRunner`, and its own `NativeRunner` for a
whole-program `RUN`. Nothing in it shares state with the editor's REPL bar, so a
program started from the computer cannot inherit a binding from a command typed in
the IDE.

The fiction is the design brief, not a coat of paint. The constraints are period —
one ROM socket, 512 sectors of 256 bytes, a prompt, four phosphors — and every
modern capability behind them is there to remove the cruelty, not the character.
[Inside the HB-1](hb1/inside.md) is the long version; the four comments it cites
(`hb_dos.hpp`, `retro_settings.hpp`, `retro_computer.hpp`, `panels.hpp`) are the
short one.

The ROM selector is a `Language`; `romFor()` supplies the version, prompt and boot
note, and `setLanguage()` performs a cold boot -- stop both engines, swap the
program buffer, clear the tube, print the banner. Each language's source is held
in `programs_`, keyed by the enum value and persisted in QSettings, which is why
switching ROMs and returning restores that ROM's memory. The screen is a
`QSplitter` whose two children are a `QTabWidget` (a real `CodeEditor` on the
PROGRAM page and the boot/output console on OUTPUT) and the `TurtleCanvas`; the
mode selector shows one or both. A `CrtOverlay` covers the frame as glass rather
than as a second surface, so it is transparent to input.

`RetroComputerSettings` (`retro_settings.hpp`) is the machine's configuration: the
boot ROM and boot screen, the start-up message, the prompt-history budget, whether
programs are remembered, whether drawing switches to SPLIT, the phosphor, the two
text sizes and the five glass strengths. It is persisted under
`retroComputer/config` by the dialog and by nothing else — the window's own
`saveSettings()` writes session state (geometry, ROM, screen, program buffers), so
cancelling a preview cannot leave a configuration the user did not choose.
`retroPalette()` is the one table of monitor colours; the stylesheet, the program
editor's palette, the canvas surface and the CRT overlay all derive from it, which
is why choosing Amber changes the whole machine rather than tinting one widget.

`HbDos` (`hb_dos.{hpp,cpp}`) is the machine's disk: one volume of named programs,
each with the ROM that runs it, a lock bit, and a size counted in 256-byte sectors
against a 512-sector volume. It is a plain class with no widgets, so the same code
serves the prompt and the tests; it persists as one JSON image in the application's
data directory (`disks/HBWARP.dsk`), and every change is committed immediately and
rolled back in memory when the write fails. The window's DOS words are thin: parse
the arguments, call `HbDos`, print what happened in the machine's voice. `INIT` is
the one destructive word, and the window makes it be said twice, with any other
line in between cancelling it.

The prompt has two paths. The machine's own words (`RUN`, `LIST`, `NEW`, `EDIT`,
`TEXT`, `GFX`, `SPLIT`, `CLS`, `HELP`, `ROMS`, `CONFIG`, and the DOS words
`CATALOG`, `STATUS`, `SAVE`, `LOAD`, `DELETE`, `RENAME`, `LOCK`, `UNLOCK`,
`VERIFY`, `TYPE`, `INIT`) are handled in the window; in the BASIC ROM, a numbered
line is inserted into the editor in numeric order and a bare number deletes it. Everything else is submitted to the `ReplRunner` for the
selected ROM, which keeps bindings between entries. `RUN` stops that session and
starts a `NativeRunner`, the same engine the editor's Run uses, so the two cannot
both draw into the tube.

### The guided tour

`tour.cpp` owns a first-run overlay and the one-time hints built on the same
anchors. A step names a widget's `objectName` rather than a coordinate, so the
card cannot drift away from the control it describes; `ui_layout_tests` asserts
that every name in the catalogue still resolves, which turns a rename into a
test failure. "Seen" lives in `QSettings` under `tour/seen`, and each hint under
`tour/coach/<key>`, so a second window in the same installation does not repeat
them. The overlay dims the window around the anchor and swallows clicks; its card
is centred when the anchor is hidden. Settings re-arms the tour, and the Help
menu can show it on demand.

### Languages

`native/src/ui/language.cpp` holds the table: display name, settings key and file
extensions for each of the six, plus `languageFromKey` and `languageForFile`. The enum
order matches the Python registry's, and new languages are **append-only**, because
persisted state references these by identity rather than by position.

`languageForFile` answers `Unknown` for an unrecognised extension rather than falling
back to BASIC. The Python registry does fall back; here that fallback is the *caller's*
decision, because guessing would hide the fact that the file name says nothing about
the language.

### Styling

`native/src/ui/ui_style.cpp` owns the stylesheet, as one sheet applied once to the
window. Qt copies a widget's stylesheet to its children and re-polishes all of them on
every `setStyleSheet()` call, so six calls mean six full passes over a window with
hundreds of widgets; composing the sections and setting the result once removes that,
and gives a theme manager a single string to replace. Rules are scoped by object name,
because a bare `QTabWidget { ... }` cascades to *every* descendant tab widget —
including the output sub-tabs nested inside a page of the widget being styled. Colour
roles are written with Qt's `palette(...)` function, so the sheet follows the active
palette.

**The theme system is built.** `native/src/ui/theme_table.inc` carries the 35 schemes
— the removed implementation's 33 plus Windows 2000 and Windows 7 — as data:
name, background, foreground, editor colours and the `dark`/`mid`/`shadow` roles this
sheet reads. `themes.cpp` resolves the optional fields, applies the palette and
composes the QSS. View > Theme selects one, View > Next Theme cycles, and View >
High Contrast derives a maximum-contrast variant of the active theme rather than
replacing it.

---

## The native engines

### `timewarp_scheme` — Scheme with no native recursion

The reference evaluated by recursing, so its evaluation depth *was* native call stack
depth, and CPython's 1000-frame limit capped a Scheme program at roughly 200 calls. The
port replaces every recursive site with an explicit, heap-backed continuation stack: **a
tail-recursive loop of 1,000,000 calls runs with a continuation stack 4 frames deep**,
and non-tail recursion is bounded by memory rather than by the native stack.

Values are 16-byte tagged unions pointing into a region allocator, so cycles cost
nothing; symbols are interned; frames are a `std::variant` of about eighteen
continuation types. A frame is popped before its handler runs and pushed back only when
there is more work, which is what yields proper tail calls.

Scheme keeps its own runner (`DebuggerRunner`) on its own thread, which is what gives
it the call stack and frame bindings the other engines do not keep. Every other
language runs in-process through `NativeRunner`, one run at a time.

### `timewarp_expr` — the expression evaluator

Ported from `utils/expression_evaluator.py` and gated against it. A 32-byte
trivially-copyable token that borrows its text from the source, so lexing does not
allocate; a token cache keyed on the expression text with node-stable keys; and a sorted
builtin table with a switch rather than a table of `std::function`. Three phases are
exposed separately — `tokenize`, `to_rpn`, `evaluate_rpn` — so a caller can compile once
and execute many times, with `evaluate()` as the memoising convenience path.

Every language's arithmetic goes through it: the executors call
`Interpreter::evaluate_expression`, which is this evaluator, and the parity gate
(`differential_check.py`) compares it against the Python reference.

### Parity as the acceptance test

Both engines are compared against their Python originals on a fixed corpus by
`native/tools/differential_check.py` and `native/tools/scheme_differential_check.py`,
and the language port by `native/tools/lang_differential_check.py` over the
command-table rows and every example. All three must report **0 unexpected
mismatches**; the divergences that are accepted are
printed and listed, each with a reason. That is what makes "did the port stay faithful?"
a measurable question rather than a matter of opinion.

---

### Projects, SQL and the automation API

Four libraries sit beside the engines, each with one job and a test suite:

- **`timewarp_project`** (`native/include/timewarp/project/project.hpp`) is a JSON
  view over programs that already exist. A project is the file (`<name>.twproject`),
  a root, an ordered list of groups and a list of files with the group each belongs
  to. Paths inside the root are stored relatively, so a copied folder keeps working;
  the save goes through `QSaveFile`; a load validates every field before replacing
  the open project, so a broken file cannot destroy the one already open. Adding a folder
  walks it for recognised programs and files each under its subfolder's group.
  The panel is a tree over this model and never edits it directly — every gesture is a signal
  the window applies to the model and then saves, which is why a drag and a context
  menu cannot disagree about where a file lives.
- **`timewarp_sql`** wraps one SQLite connection in `sql::Database`. It is the
  connection both the 🗄 Database tab and a program's `SQL` statement use, guarded by
  a mutex because one may run on a runner thread while the other is on the GUI
  thread. Results carry columns, rows, the change count and an error; the row cap
  (1000) is part of the type, not a thing the panel decides. Table and schema
  lookups bind their names rather than pasting them into SQL.
- **`timewarp_lessons`** (`native/include/timewarp/lessons/`) is the curriculum and
  the verdict. `curriculum.cpp` reads `share/lessons.json` and refuses a checkpoint
  with no expectation or two — one that would pass or fail everybody, silently —
  while `verify.cpp` runs a learner's program through the engines with
  `RecordingHost` as the surface, so a lesson is checkable with no window. The three
  expectations are the reference's: what a program printed (engine commentary such
  as 🎨 is not output), what the turtle drew (pen segments and shapes counted
  separately, and whether it finished where it started), and a reported error, which
  is checked before anything else because a checkpoint whose answer *is* a failure
  would otherwise be swallowed by the branch that reports a broken program.
  `progress.cpp` keeps a learner's place in `~/.time_warp/progress.json` in the
  shape that file has always had. The IDE's panel, `time_warp_teach` and the
  `lessons_tests` suite all ask this library, which is what makes a learner's
  verdict and a teacher's mark one function's answer.
- **`timewarp_api`** is JSON-RPC 2.0 over newline-delimited JSON, with no transport
  of its own. `api::Server` takes an `istream` and an `ostream`, which is how it can
  be driven by a process (`time_warp_api`), a test (string streams) or an embedding
  application. It owns a `sql::Database` and a `project::Project` and delegates
  program execution to the same `Interpreter` and `Machine` the IDE uses; the REPL
  sessions it keeps are that same `ReplSession` the bar drives, and its debug
  sessions wrap `lang::DebugSession` and `DebuggerRunner` rather than growing a
  third stepping loop. That sharing is the
  design rule: an API that grew its own interpreter would be a second answer to
  "what does this program do".

`native/tools/twc.py` generates a small C++ launcher for a program, embeds the
source as bytes (a CHIP-8 ROM is binary), links `timewarp_lang` or
`timewarp_scheme`, and uses `graphics::RecordingHost` — the same recorder the API
returns — to write the drawing as SVG. The result needs neither Qt nor the Studio
at run time.

## The language bridge

### Why a separate process

All ten languages now have a C++ interpreter. The Python engine is kept for a
different job: it is the **reference** the parity gates compare each port against,
the curriculum checker the Lessons panel calls, and the engine the command matrix
drives. Keeping it means "did the port stay faithful?" is a question a machine
answers instead of a matter of opinion.

The IDE itself no longer runs a program there. Each port removed its path through
the bridge, and the last one (BASIC) removed the runtime case entirely. The child
process remains the answer for work the IDE should not run in its own process: a
reference implementation that is deliberately allowed to fail, and a lesson check
that must not disturb the learner's document.

### The protocol

JSON lines, one message per line.

**In** (GUI → worker):

| Message | Meaning |
| --- | --- |
| `run` | Run a program. The source travels as a JSON string, so any program text is safe |
| `in-reply` | Answer a pending input request |
| `stop` | Stop the program |

**Out** (worker → GUI):

| Message | Meaning |
| --- | --- |
| `ready` | Handshake, sent once before the first run |
| `out` | One line of program output |
| `canvas` | A batch of drawing commands |
| `pose` | Where the turtle ended up |
| `err` | A failure, for the error panel |
| `in` | The program is waiting for a line of input |
| `done` | The run ended: `ok`, an error string, and any unsupported features |

Three things the bridge is careful about:

- **A worker that dies is an error, not a hang.** A process that exits without a `done`
  message is reported as a failure, with the stderr text, rather than leaving a run that
  never finishes.
- **A message can be split across reads.** `QProcess::readyRead` says nothing about
  message boundaries, so partial lines are buffered until their newline arrives.
- **A stray `print` cannot corrupt the protocol.** The worker redirects its own stdout
  for exactly that reason; stderr is captured separately and only ever shown as
  diagnostics.

`pose` is a signal rather than a canvas command because painting the *marker* is not
something the command stream expresses: its commands say what to draw, not where the
turtle ended up.

### Finding the worker

`locateWorker()` searches in a fixed order — `TIME_WARP_PYTHON` and `TIME_WARP_WORKER`
first, then beside the executable and two levels up (the build-tree layout), then the
working directory; then the interpreter from the repository's `.venv`, then `python3`
and `python` from `PATH`. It reports **what it looked for** when it fails, because a
missing bridge has to say which half is missing. The order is explicit rather than clever
on purpose: for a cross-process launcher, "it works on my machine" means "the paths
happen to line up here".

### The engine

`native/bridge/time_warp/` is the engine the PySide6 application used, with its UI
removed. It is standard library only; `pyttsx3` (BASIC `SAY`) and `pygame` / `inputs`
(BASIC `SOUND` / `GAMEPAD`) are imported defensively and are not required.

| Directory | Contents |
| --- | --- |
| `core/` | `interpreter.py` (dispatch and program state), `language_registry.py`, `config.py`, `sandbox.py` |
| `languages/` | The ten reference executors, and `base.py`, which documents the protocol |
| `graphics/` | `turtle_state.py` |
| `features/` | Language-level modules reached *by name* from a program |
| `utils/` | `expression_evaluator.py` — the reference `timewarp_expr` is gated against — plus error hints and validators |
| `tests/` | The engine's own suite, plus `command_table.py` |

### The executor pattern

Each language is a **function**, not a class. `languages/base.py` documents the
protocol — both execution modes have the *same* signature and differ only in what
`source` holds:

```python
def execute_logo(interpreter: Interpreter, source: str, turtle: TurtleState) -> str:
    """Execute the source and return the text to display."""
```

- **Whole-program executors** receive the entire source. Scheme, CHIP-8, Logo and
  Smalltalk are dispatched through `_WHOLE_PROGRAM_EXECUTORS` in
  `core/interpreter.py`.
- **Line-by-line executors** are called once per logical line/command: BASIC, PILOT,
  Logo, COMAL, REXX, FOCAL and APL. Logo names the parameter `command` for that
  reason, and splits multiple commands on one line itself.

The contract:

**Must not** store UI references, update the canvas directly, or modify the window.

**May** read and write interpreter state — variables, turtle position — and return
strings as output.

Jumping between lines is done by assigning `interpreter.current_line`, a 0-based index
into `interpreter.program_lines`. After the executor returns, if `current_line` is not
`physical_line - 1` the main loop re-dispatches without incrementing, so "run line X
next" means `current_line = X`.

Errors are returned as strings prefixed with `❌` rather than raised out of the executor
for the ordinary case.

### Output conventions

Every message the engine produces carries an emoji prefix, and the IDE's presentation
depends on the distinction:

| Prefix | Meaning |
| --- | --- |
| `❌` | Error or exception |
| `✅` | Success confirmation |
| `ℹ️` | Information |
| `🎨` | Theme or UI change |
| `🚀` | Execution event |
| `🐢` | Turtle graphics action |
| `📝` | Input prompt |

---

## Extension points

### Adding a language

1. **The executor**, in `native/bridge/time_warp/languages/my_lang.py`, as a function with
   the signature above.
2. **Register it in the bridge**: import it in `languages/__init__.py`, add
   `Language.MY_LANG` in `core/interpreter.py`, and add the extension in
   `Language.from_extension()` and `core/language_registry.py`.
3. **Register it on the Qt side** in `native/src/ui/language.cpp`: display name, settings
   key, extensions. The IDE needs this to name the language and to pick it from a file
   name.
4. **Add an example** under `Examples/my_lang/` — adding the ones that draw to
   `DRAWING_EXAMPLES` in `native/tools/example_smoke.py`, or a program that draws
   nothing passes by being silent — and a row to
   `native/bridge/time_warp/tests/command_table.py` so every new command is covered by
   `native/tools/command_matrix.py`.
5. **Add a lesson** so the curriculum can teach it: two lessons in
   `native/share/lessons.json`, then `make teach-check` (`time_warp_teach validate`
   once the tree is built) until it is silent. A checkpoint is checked by *running* the learner's program, so this
   is also the only thing that proves the lesson's transcript.
6. **Document it**: a reference section in `docs/LANGUAGE_GUIDE.md`, a tutorial in
   `docs/tutorials/` (the docs workflow requires one per language), a row in the README's
   language table, and a line per example in `Examples/CATALOG.md`.

A language ported to C++ skips the bridge entirely: it gets an executor in
`timewarp_lang`, and `MainWindow::runCurrentDocument` dispatches to it when
`executor_for()` returns non-null. Every language is now ported, so no runtime
dispatch reaches the bridge; the same `NativeRunner` (a `QThread` feeding
`CanvasCommandQueue`) runs any executor that appears in that table, so the window
needs no change per language.

### Adding a canvas command

1. Add the kind to `CanvasCommandKind` in `include/timewarp/canvas/command.hpp`, with the
   payload documented in its comment.
2. Give it a name in `src/canvas/command.cpp`.
3. Say whether it collapses by adding it to `is_coalescible()` — or leaving it out, which
   is the default — in `include/timewarp/canvas/command.hpp`. That single predicate is
   the only thing deciding whether an adjacent duplicate is merged.
4. Handle it in `TurtleCanvas::apply()`.
5. Emit it from `QueuedTurtleHost` in `host_adapter.hpp` for
   a native engine.

### Replacing a placeholder panel

The right column's **Variables**, **Telemetry HUD**, **Debug**, **Lessons** and
**Classroom** tabs are real panels (`VariablesPanel`, `TelemetryPanel`, `DebugPanel`,
`LessonPanel`, `ClassroomPanel` in `src/ui/panels.cpp`), driven by data the window gets
from two places. The outline dock was the last `PlaceholderPanel`; the console's
**Terminal** tab is a real shell (`TerminalWidget`), added only when the user enables
it. Nothing ships a placeholder now, and `PlaceholderPanel` remains in `panels.hpp` as
the documented way to stand in for a panel that does not exist yet -- a real widget
with honest size hints, so the layout can be built and tested before the feature is.

Replacing one is a single construction site in `buildRightColumn()` or
`buildOutputColumn()`, plus wiring in `runCurrentDocument()` or `adoptBridgeWorker()` for
anything that needs interpreter data -- which is exactly how the three inspect panels
were filled in:

- **The native runner** reports the variables after every run (`NativeRunner`'s
  snapshot) and the window adds the HUD's engine counters. The bridge path is
  still there for a language with no executor, and a lesson check uses the worker;
  no current language takes the bridge path.
- **Scheme** reports nothing over the bridge, so the window reads
  `DebuggerRunner::snapshot()` -- including a final snapshot taken when a run ends
  without pausing, because a finished program still has bindings and "no variables" is
  a claim, not an absence.
- Either way the window appends the rows only it can vouch for: wall clock, and how much
  ink actually reached the canvas (`strokeCount`/`totalStrokeLength`). The engine's
  counters and the IDE's are shown side by side rather than merged, so a drawing that
  never arrived is visible as two rows that disagree.
- **The profiler** uses the same line loop the debugger pauses in.
  `Interpreter::set_profiling` wraps each statement in a clock and keeps a hit and
  time total per program line; `NativeRunner` copies the report out when the run
  ends (the interpreter does not outlive it), and `ProfileReportDialog` shows it
  longest first. The clock starts after a debug pause, so waiting for Continue is
  not charged to the statement. A whole-program language has no such loop and
  reports nothing rather than zeros.

Every language can be stepped. The six line-loop languages stop at a statement through
`DebugSession::note_line`; Logo stops at a top-level statement, Smalltalk at a message
send and CHIP-8 before an instruction through `note_source_line`. Scheme is the only
one that also reports a call stack and frame bindings, because it is the only machine
that keeps them. `DebugPanel`'s buttons drive whichever runner is paused.

**The lesson panel was never a placeholder.** `LessonPanel` is fully implemented. It holds no curriculum and runs no program: it
asks the bridge for the lesson list, displays the checkpoint, and sends the learner's
source back to be checked. That is deliberate, and it is what makes the panel's green tick
and `native/tools/learn.py`'s verdict the same function's answer rather than two opinions
that drift. The protocol it uses is in [docs/lessons/FORMAT.md](lessons/FORMAT.md).

It also never touches a document. "Open" raises `openCodeRequested` and the *window*
decides where the code goes, because a panel has no business knowing about tabs.

The curriculum is loaded when the tab is first looked at, not when the window is built: a
user who never opens the lesson panel should not pay for a Python process at start-up.

Three widgets are worth knowing about because what they cover is easy to
underestimate:

- `ImmediateModePanel` is a REPL prompt and entry line with the layout contract the Python
  sets (a 44px floor). `commandSubmitted` drives `ReplRunner`, a `QThread` that keeps
  one `ReplSession` per language (and a persistent Scheme `Machine`) alive between
  entries. `Interpreter::run_snippet` replaces the program text while preserving
  variables, arrays and per-executor state, so a command sees the previous command's
  bindings. Whole-program languages run each submission as a complete program; the
  guide says which bindings survive for each language. In BASIC,
  `handleBasicDirectCommand` intercepts numbered lines and stores them in the
  editor in numeric order, and handles `LIST`, `RUN` and `NEW` before the engine
  sees them. The same `?`-for-`PRINT` rule lives in the BASIC executor, so it works
  in the bar and in a program.
- `FunctionKeyBar` reproduces all three modifier layers (normal, Shift, Ctrl) over F1-F10,
  plus the deck selector, and every key dispatches to an action that exists.
- `CrtOverlay` draws scanlines, vignette, curvature, flicker and a phosphor tint; View >
  CRT Settings sets the five and View > Screen Mode pairs them with a theme. It installs
  itself as an event filter on the widget it covers rather than reaching into
  `parent().rect()` from a resize handler, which would silently stop working the moment
  the overlay was reparented.

The rule that replaced the old placeholder controls is that **an entry is added when it
works**: Debug, Continue, Trace, Snippets, A+, find-in-files, the theme menu and the
function-key layers are all live, and no menu or toolbar entry is a disabled promise.

---

## Tests and gates

Twenty-one C++ suites run by `ctest`, plus the engine's own pytest suite, three parity
gates and a command matrix. All of them must stay green, in every build tree.

| Gate | Command | What it protects |
| --- | --- | --- |
| C++ suites (23) | `make -C native test` | Lexer, evaluator, Scheme, native languages, canvas queue, canvas bridge, debug, debugger runner, UI layout, UI documents, turtle canvas, syntax highlighter, media, editor services, help browser, CHIP-8 emulator, retro computer, project, SQL, API, audio, lessons, classroom |
| Debug tree | `make -C native test-debug` | Keeps `Q_ASSERT` and standard-library hardening live; a suite green in Release can crash here |
| Sanitisers | `make -C native asan` | Reads and writes of memory that is no longer valid, which no assertion can see. `ASAN=undefined` for UBSan |
| Engine tests | `make engine-test` | The bridge engine: the ten executors, the evaluator, the validators, the lesson curriculum and the classroom |
| Example corpus | `make smoke` | Every `Examples/**` program, through the reference engine, including the 30 that must produce ink |
| Parity gates | `differential_check.py`, `scheme_differential_check.py`, `lang_differential_check.py` | The ported engines against the Python reference: 0 unexpected mismatches |
| Command matrix | `native/tools/command_matrix.py` | One row per documented command, through the real bridge |
| Lesson curriculum | `teach.py validate` (and `test_lessons.py` on every build) | That every checkpoint's own solution passes, that no starter already passes, and that every language has a lesson |
| Version metadata | `Scripts/version_sync.py` | That every hand-written copy of the version agrees with `VERSION` |

Two of these exist for a failure the others cannot see:

- **The example corpus** is the gate for a language that runs "successfully" and draws
  nothing, which is what a user reports as "the graphics don't show up". Five languages
  once did exactly that. It counts the ink present on the final canvas — stable across
  runs — rather than every intermediate frame, because that earlier metric moved by
  hundreds between identical runs and could not be compared with anything.
- **The command matrix** drives one row per documented command through the real bridge. It
  is what found the BASIC keyword-boundary bug that silently swallowed assignments to any
  variable whose name began with a keyword, the `ON TIMER` off-by-one, and Logo's `SHOW`,
  which had never evaluated anything.

The C++ build is its own linter: `-Wall -Wextra -Wpedantic -Wshadow -Wcast-qual
-Wuseless-cast -Wlogical-op -Wnull-dereference` with `-Werror`, so a warning fails the
build.

Qt is discovered with `find_package(${TIME_WARP_QT} QUIET COMPONENTS Core)`, plus
`Widgets` for `timewarp_ui`. When they are absent the Qt-facing targets are skipped and
everything else — including the queue's own tests — still builds and runs. `Svg` is asked
for separately and only by `timewarp_ui`: it enables SVG drawing export, and without it
that format is absent rather than broken. Nothing outside the canvas Qt adapter, the
debugger runner and `src/ui/` includes a Qt header.

`TIME_WARP_QT` is `Qt6` or `Qt5`, and `-DTIME_WARP_QT5=ON` is what chooses the second.
It exists for one reason: Qt 6 requires Windows 10, so a Windows 7 build cannot use it and
Qt 5.15 — the last Qt that runs on 7 SP1 — must. The two versions differ in a handful of
names and in whether `QList` and `QVector` are one type, and those differences live in two
small headers, `timewarp/core/qt_compat.hpp` and `timewarp/core/qt_gui_compat.hpp`, rather
than in a conditional at each call site. Every `Qt6::` reference in `native/CMakeLists.txt`
is `${TIME_WARP_QT}::`, so a target linked in one build is linked in the other.

---

## Configuration and persistence

| What | Where | Note |
| --- | --- | --- |
| Window geometry, dock and toolbar state | `QSettings` — `TimeWarpStudio/TimeWarpStudio.conf` | Keys under `layout/` |
| Splitter positions | `QSettings`, `layout/splitters` | An array whose index is positional, so reordering it without bumping the count restores the wrong geometry |
| User content: projects, lesson progress, exports | `~/.time_warp/` | **Shared with the removed Python IDE on purpose** |
| Lesson progress | `~/.time_warp/progress.json` | One entry per lesson; written on every change |
| A class: roster, handouts, submissions | `~/.time_warp/classroom/` | One file per checkpoint, plain enough to zip and hand on |
| Any of the above, elsewhere | `TIME_WARP_DATA_DIR` | A portable install, or a test that must not write a learner's real file |
| A missing or malformed setting | The layout default | A settings file from an older build leaves that splitter at its default rather than producing a window with a zero-width pane and no way back |

The settings identity is deliberately **not** the Python IDE's (`TimeWarp` / `IDE`). The
two schemas are unrelated, so sharing a scope would work today only because no key happens
to collide — the kind of coincidence that becomes silent corruption the moment somebody
adds a matching key. Unlike a build error, that would surface as a window restoring into a
state the user cannot drag their way out of.

User *content* is the opposite case and gets the opposite answer. Content belongs to the
user rather than to an implementation, so a program written in one build must open in the
other; only settings are private. Carrying preferences across would be a *migration* —
read the Python settings once, translate the theme name and recent files, write this
build's own keys — and it is **not built**: the two identities are separate, so a user
coming from the Python application starts from this build's defaults.

---

## What is not here yet

[ROADMAP.md](ROADMAP.md) is the authority. The architectural gaps, in one place:

- **The debugger is a runner, not a full debugger.** `DebuggerRunner` supports
  pause, step and breakpoints, and `debug_tests` covers the machinery; `DebugPanel`
  drives it for Scheme. Every other language stops through the shared
  `DebugSession`: a statement for the line-loop languages, a top-level statement
  for Logo, a message send for Smalltalk and an instruction for CHIP-8. Watch
  expressions are evaluated over the paused bindings by `timewarp::lang`'s watch
  evaluator; data breakpoints compare the bindings after each statement (or at a
  whole-program pause point) and stop when a watched name changes. What is missing
  is a call stack for languages that do not keep one.
- **The editor** has syntax colouring, find/replace, find-in-files, a minimap, a
  formatter, language-specific checking as you type (squiggles, gutter markers,
  hover text), optional completion, auto-indent, breakpoint markers in the gutter,
  a document outline and a full settings dialog with a live preview. What it lacks
  is multi-cursor editing.
- **The console** has no clickable line links, no error-only filter and no
  save-to-file. The **Terminal** is a real shell but no terminal emulator: without a
  PTY there are no prompts, job control or full-screen programs, which is deliberate.
- **Files and sessions** have autosave and version history — `AutosaveStore`
  keeps bounded snapshots under the data directory and the version-history dialog
  reopens one in a new tab — and the split editor: two panes that each hold their
  own documents, persisted with the session. The 🗂 **Projects** dock is a tree
  over a `.twproject` (`timewarp_project`): named groups, files that stay where
  they are on disk, and a project that reopens with the session.
- **Help** is a documentation browser (`native/src/ui/help_browser.cpp`) over the
  installed `docs/`, with its contents tree read from `INDEX.md` rather than
  written a second time. F1 opens the current language's tutorial at the word
  under the cursor, Ctrl+F1 the filterable keyword inspector, and Shift+F1 the
  function-key reference. The guided tour and its one-time coach marks are built,
  and re-armable from Settings.
- **Themes and fonts are in**: 33 schemes, applied through the palette plus a
  theme block, and the editor's font, size and tab width are user settings.
- **Printing and the code/session/lesson exports are in.** File > Print Code and
  File > Print Drawing go through Qt6::PrintSupport, which is optional: a build
  without it hides both entries rather than showing dead ones. The program and
  the lesson session export as PDF, the session as HTML and the lesson session as
  Markdown; the drawing exports as an image or, with Qt6::Svg, as vectors.
- **Packaging** has `make install` with a desktop entry and an icon, and a tagged
  release builds `.deb`, `.rpm` and tarball artifacts for x86_64 and arm64, an
  AppImage for x86_64, and a container image from the same install rules. Flatpak,
  Snap, a Windows installer and a macOS bundle are not built.
- **Lesson checking still runs in the Python engine**, which is what keeps
  `native/bridge/` itself, which is development tooling now.
- **No pixel framebuffer.** Graphics are a display list of strokes and shapes
  with fit-to-content, so there is no `GET`/`PUT` sprite capture and no fixed
  screen coordinate system; a game draws primitives and polls `INKEY$`.
- **Speech** (`SAY`) reports that speech synthesis is unavailable: there is no
  speech synthesiser in the build, and the reference never shipped one either.
