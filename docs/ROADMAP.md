# Roadmap

Time Warp Studio is a C++20 / Qt6 IDE for ten educational languages. This page is
the honest state of it: what works, what is next, and what is deliberately not
being built.

The PySide6 implementation that came before this one has been **removed**. Where
this page says a feature is "not ported", it existed there and does not exist here
yet — please check the list below before reporting one as a regression.

[PORTING_PLAN.md](PORTING_PLAN.md) is the sequenced plan for those features: what
each one becomes in this codebase, the test that proves it, and the order they are
worth building in. That plan was written by diffing the removed tree against this
one, so its list is longer than the paragraphs below and its categories are
derived rather than remembered.

---

## Working today

| Area | State |
| --- | --- |
| Shell | Documents and tabs, menus, toolbars, status bar, settings persistence, the last session's files restored at launch |
| Editor | Line-number gutter, current-line highlight, per-language syntax colouring, find/replace, find in files, go-to-line, tab stops, fonts, error squiggles with gutter markers, completion, auto-indent, Format Code |
| Console | Streaming output, and interpreter errors in their own tab |
| Canvas | Live drawing, zoom, pan, grid, auto-fit, turtle marker, scrubber playback, animated GIF recording, raster and SVG export |
| Display | 33 themes, high contrast, A+ text sizes (100/125/150%), focus mode, CRT settings, four Monitor presets |
| Execution | Run (F9) and Stop; one program at a time; unsaved-change prompts |
| Languages | 10 languages, 49 verified example programs |
| Native engines | Expression evaluator (`timewarp_expr`) and Scheme (`timewarp_scheme`) |
| Bridged engines | None — every language runs in process. `native/bridge/` remains as the reference the parity gates and the lesson check use |
| Native executors | All ten — BASIC, FOCAL, PILOT, COMAL, APL and REXX line-by-line; CHIP-8, Logo, Smalltalk and Scheme whole-program |
| Printing | File > Print Code (Ctrl+P) and Print Drawing, through the optional Qt6 PrintSupport |
| Profiling | View > Profile Run times every statement in the line-by-line languages and ends with a per-line report, longest first |
| Terminal | Opt-in in Settings > Workspace: an embedded shell in the document's folder, absent in presentation and player modes |
| Debugging | Breakpoints, Pause, Continue and Trace in all ten languages; Scheme adds a stack and frame bindings |
| Packaging | `make install` with the desktop entry and icon; `DESTDIR` staging |
| Quality gates | 17 CTest suites in 4 build configurations, 3 parity gates, 560 engine tests, 446 command rows, 472 parity programs |

---

## Next: interpreter defects the command examination found

`native/tools/command_matrix.py` drives one row per documented command through the
real bridge (`native/bridge/time_warp/tests/command_table.py`). Examining every
command turned up defects that no gate had been looking for. Those fixed have a row
of their own so they cannot come back silently; those still open are listed here
rather than left as folklore, and where a row cannot express them the reason is
written down in `WAIVERS`.

**Still open**

None. Every defect below has a regression row in `command_table.py` and, where the
behaviour is expressible in a unit test, a case in the language's test module. The
section stays because the next port has to keep them fixed: a port that quietly
restores one of these is a regression even though it is new code.

**Fixed during the examination** (each has a regression row, and the first two also a
unit test)

- **An unresolved name is reported, not echoed.** `PRINT UNSET` printed `UNSET`,
  `PRINT NOSUCHFN(3)` printed `NOSUCHFN(3)` with no error, and after `DEFSTR A-Z`
  `PRINT A` printed `A`. An unset numeric variable is now 0, as BASIC defines it, and
  an unknown function is `❌ Unknown function: NAME`. The `xfail(strict=True)` that
  announced the gap now passes as an ordinary test.
- **`PEEK(address)` and `IN(port)` work in expressions.** The names are registered on
  the evaluator for BASIC only, so `X = PEEK(1000)` reads the same memory the
  statement form writes. The evaluator's function table is now per-instance: the
  reference's `EOF` registration was mutating the class dictionary and giving `EOF`
  to every other language's evaluator too.
- **`STICK(n)` and `STRIG(n)` exist**, reading the gamepad manager `JOYINIT` starts
  (axis 0-3, button 0-7). With no backend they answer 0 rather than failing.
- **`TYPE$(x)` reports the interpreter's type** (`INTEGER`, `LONG`, `SINGLE`,
  `DOUBLE`, `STRING`) instead of printing nothing; `PRINT` was reading it as a string
  array because the item ended in `)` with `$(` inside.
- **`RESUME NEXT` works without line numbers.** The error handler records the line
  *index* as well as the BASIC line number, so an unnumbered program resumes after
  the statement that failed.
- **`SPEAK` reaches the speech handler**, the name the feature module documents.
- **COMAL `FUNC` is callable in an expression.** `PRINT DOUBLE(21)` answers 42 through
  a new per-interpreter function hook, in both the reference and the native executor;
  a body this cannot read reports an error rather than answering 0.
- **Logo's `OUTPUT` value reaches its caller.** `PRINT DOUBLE 21` prints 42 and
  `SHOW P` prints the value, in both the reference and `timewarp_lang`.

- A keyword boundary bug: `startswith("DO")` also matched `DOG = 42`, so the `DO`
  statement swallowed the assignment. Every variable whose name began with a keyword
  (`DOUBLE`, `RUNNER`, `CLOSER`, `NEXTX`, `LOOPS`, …) silently lost its value. The
  affected keywords were `NEXT`, `DO`, `LOOP`, `RUN`, `SYSTEM`, `CLOSE`, `RANDOMIZE`,
  `SLEEP`, `RESUME`, `JOYINIT` and `RESTORE`.
- `ON TIMER` could never work: the dispatcher sliced one character too many and threw
  away the `(`, so the handler rejected the format its own error message demanded.
- `JOYINIT` reported an absent *optional* backend with the error prefix, which the IDE
  shows as the program failing.
- **`SHOW` and `TYPE` never evaluated their argument.** They were stubs that returned
  the source text, so `SHOW 1 + 2` printed `1 + 2` and `SHOW :X` printed `:X` — the
  command whose whole purpose is to display a value displayed the code instead.
  `PRINT` had always worked, so the two disagreed on the same expression.
- **A reporter could not be applied to a list literal.** `_consume_logo_args`
  balanced parentheses but not brackets, so `FIRST [1 2 3]` handed FIRST just `[`
  (which it then treated as a one-character word) while `FIRST :LIST` worked. Every
  reporter taking a list — FIRST, LAST, BUTFIRST, BUTLAST, ITEM, COUNT — was affected.
- **A reporter named in `LOGO_COMMANDS` was split away from the command that used
  it**, so `PRINT THING "X` became `PRINT` plus `THING "X`: PRINT lost its argument
  and THING ran on its own. Reporters cannot begin a statement, so they now never
  start a new command.
- **FOCAL's `ASK` answer never reached the program.** The statement recorded the reply
  in a private dictionary while the engine substituted from `interpreter.variables`, so
  the prompt appeared and `T N` printed `0.00` whatever was typed. The statement now
  writes to the same place the engine reads from.
- **FOCAL's `FPI` and `FRAN` needed parentheses they should not have.** FOCAL writes
  them bare -- `T FPI` -- and requiring `()` sent the name to the variable lookup,
  where an unset name is 0, so the documented form printed `0.00`.
- **APL's `⍕` printed a character code instead of digits.** `FORMAT` took the
  nested-display path, which reads a number as the character with that code, so `⍕42`
  answered `*`. The keyword spellings `+REDUCE` and `+SCAN` also failed: the operator
  split compared raw tokens, so only the glyph forms derived a function.
- **CHIP-8's `JP V0, addr` encoded the register into the address.** The high nibble of
  the jump table's base came from `V0` rather than from the instruction, so the jump
  landed at address 6.
- **PILOT's `LINK`/`LOAD` could never run.** A line beginning `L:` is the *label
  declaration* -- the loader registers it as one -- so the Link/Load unit returned
  before it was reached, and the long forms remapped to exactly such a line. It is now
  unreachable code no longer; the unit reports that lesson chaining is not implemented,
  because a silent success is the failure mode this whole examination exists to catch.
- **REXX's `SPACE` ignored its count argument.** `SPACE(string, n, pad)` puts `n` pad
  characters between the words; the count was dropped, so `SPACE('a b c', 0)` answered
  `a b c` -- a plausible wrong answer rather than a missing one.
- **`command_inventory.py` compared its "not a command" list case-sensitively** while
  the list held lowercase entries (`nil`, `false`, `binary`), so those entries did
  nothing and were counted as unwritten work. Only the case-insensitive entries were
  filtering, which is why the report asked for rows that could not exist.
- The corpus gate counted every intermediate frame, and how many frames stream before
  a `CLEARSCREEN` depends on the 50 ms flush cadence — so the "strokes" figure moved by
  hundreds between identical runs and could not be compared with anything. It now
  counts the ink present on the final canvas, which is stable across runs. This was
  investigated as a suspected loss of drawing and was **not** one: the engine is
  deterministic and the final drawing was always complete.

**Nothing is open in Logo.** The last gap — a procedure's `OUTPUT` value never reaching
its caller — is fixed in both the reference and `timewarp_lang`, with a row and unit
tests; `PRINT DOUBLE 21` now prints 42.

---

## Documentation accuracy

Documentation that overstates the product costs a newcomer the same time a bug does,
and is harder to notice. Claims are corrected rather than quietly dropped. Three that
were wrong have been fixed: BASIC's `SPEAK` was documented before either executor
dispatched it (both spellings work now); the README called the shared expression
evaluator in-process before `timewarp_expr` was linked (it is linked now, and the
README says so); and the FAQ described the removed PySide6 application's autosave,
version history, themes and shortcuts as if they existed (it now describes this
application, and the features it lacks are listed here).

The editor's missing syntax highlighting was the fourth, and it has been **built rather
than re-documented**: `native/src/ui/syntax_highlighter.cpp` now colours each language
with its own rules, and `syntax_highlighter_tests` asserts the colour at named
positions.

`docs/ARCHITECTURE.md` has been **rewritten** around the Qt application and the engine.
It no longer describes the removed PySide6 implementation, and it names what is not
wired up rather than assuming it: the expression evaluator that does not participate in
a Run, the REPL whose plumbing is absent, and the debugger's missing depth (watch
expressions). It is now the system-level
description; `native/README.md` remains the detail on the C++ libraries.

---

## The four newest languages are complete

**APL, FOCAL, CHIP-8 and Smalltalk** arrived after the original six. Each now has
everything the others have:

- an **executor** and a **test module** in `native/bridge/time_warp/`,
- a **worked example** under `Examples/` that `example_smoke.py` runs and checks for
  ink (all four draw),
- a **command reference** in `docs/LANGUAGE_GUIDE.md` and a **tutorial** in
  `docs/tutorials/`,
- **command-table rows** covering every literal `command_inventory.py` finds in their
  executors, so `command_matrix.py` drives them the way it drives BASIC.

Writing those rows is what turned the inventory's counts into the list of defects
above: nine of them were found by a row that had no answer, not by reading the code.

**Lessons now cover all ten languages.** Every language has two, three checkpoints
each: sixty in all, with a plan, a worked answer and hints for every checkpoint, reachable
from a terminal (`learn.py`) and from the 🎓 Lessons panel in the IDE.

The curriculum is data, and `teach.py validate` **runs all sixty solutions through the
engine** and checks that none of the sixty starters already passes. That pass is what makes
a curriculum maintainable: without it, a lesson whose answer does not work would sit in
the tree looking correct, because data is not exercised by anything unless something
exercises it deliberately. See `docs/lessons/FORMAT.md`.

**CHIP-8 runs SuperChip hi-res.** Both the C++ machine and the Python reference
implement the 1.1 core -- 64x32 and 128x64 modes, 00Cn/00FB/00FC scrolls, 00FD
exit, DXY0's 16x16 sprite, FX30's 8x10 font and the FX75/FX85 flags -- so the
hi-res and SuperChip ROMs a collection brings can run. MegaChip's planes and audio
are not implemented and are reported as unknown opcodes rather than half-drawn.
The emulator's **ROM Library...** browses a folder the user supplies; the studio
ships no ROMs, because most circulating CHIP-8 programs are somebody else's
copyright and several are unlicensed ports of games that still are.

Four limits are recorded rather than left to be discovered:

- **FOCAL's `!` suppresses the newline inside one TYPE, not across statements.** The
  engine logs each statement's output separately, so a trailing newline is added by
  the logger rather than by the statement. This is an engine-level constraint, not a
  gap in the executor.
- **CHIP-8 has no interactive keypad.** A whole-program language runs to completion
  inside one call and never yields control, so `LD Vx, K` reads the first key held
  according to the listing's `; keys` directive, or 0.
- **APL's turtle position is `TX`, `TY` and `THETA`.** Deliberately two letters:
  `X`, `Y` and `H` are ordinary APL variables, and taking them would break the most
  natural line anyone writes.
- **Smalltalk has no metaclass tower, no exceptions and no image.** A class object
  holds two method dictionaries rather than being an instance of a metaclass, so
  `Counter class` answers the class itself. That is enough for everything the
  language is taught for and is the part of Smalltalk-80 a C++ port would otherwise
  have to reproduce in full.

---

## Ports complete: what remains of the bridge

Every language runs in C++ now. The Python engine in `native/bridge/` was the
reference each port was judged against, and it keeps three jobs that the gates
depend on:

1. `lang_differential_check.py` runs every program through both engines.
2. `command_matrix.py` and `example_smoke.py` drive the reference.
3. The Lessons panel asks the worker to check a learner's program, so the
   panel's verdict and `teach.py`'s are one function's answer.

The IDE itself no longer sends a program to it: `executor_for()` answers for all
ten languages, so `runInBridge` has no runtime case left.

**BASIC was the last port.** The core language, `PRINT`/`INPUT`/`FOR`/`WHILE`,
narrowing variable types, `SUB`/`FUNCTION`, records and the file channel, the VGA
statements, and the reference's Logo fallback (a `.bas` file's `FORWARD` is
Logo's `FORWARD`) are all in `native/src/lang/basic*.cpp`. It is gated by the
same rows and examples the reference is: 137 programs, 0 differences.

The next step is to move lesson checking into the native engine; only then can
`native/bridge/` and the `python_worker` target be deleted together.

BASIC's extended statement set is ported too: shapes, particles, fractals, music,
speech and the gamepad words are in `basic_features.cpp` and `basic_library.cpp`,
and the command table exercises them. What is now true, and what is still limited:

- **Sound is real** (`native/src/audio/music.cpp` + `native/src/ui/sound_player.cpp`).
  `BEEP`, `SOUND` and `PLAY` synthesise square/triangle/sine/noise at equal
  temperament, parse `PLAY`'s notation (tempo, lengths, octaves, accidentals,
  dotted notes, rests, `N` numbers) and play through the first of `paplay`,
  `aplay`, `ffplay` or `play` on the machine. The acknowledgement text is still
  the reference's, so parity is unchanged. **Limits**: one sound at a time (a new
  tone stops the previous one); no mixing; a machine with none of those players
  is silent, and `audio_available()` says so.
- **`SAY` refuses rather than pretending.** There is no speech synthesiser in
  the build, and the reference's was an optional `pyttsx3` it never shipped
  either. Both answer `Speech synthesis not available`, which the parity gate
  compares.
- **`INKEY$` reads the canvas's keyboard** while a program runs. A headless run
  has no keyboard and answers empty, which is what the matrix records.
- **`TIME$`, `DATE$` and `TIMER` read the machine's local clock**, as the
  reference does. They are not constants, so the parity rows assert their shape
  (`LEN(TIME$)`) rather than a value.
- **`SHELL` stays blocked.** The reference refuses it for security and the port
  keeps that answer.
- **Ports are native extensions.** `SERIAL` and `PARALLEL`
  (`native/src/io/ports.cpp`) open raw POSIX devices for the serial and parallel
  families, with timeouts and a write that waits; `DRAW` and `WAIT` are the other
  two. The reference engine never implemented any of them, so they are documented
  in `docs/LANGUAGE_GUIDE.md` and **not** in `command_table.py` — the parity gates
  cannot compare a command one side does not have.
- **Graphics for games** is turtle-and-primitive based: `SCREEN`, `PSET`,
  `LINE`, `CIRCLE`, `PAINT`, `DRAW`, `SPRITE` (position tracking), text at a
  point, palette indices, plus `INKEY$` and `WAIT` for the loop. **Not built**: a
  pixel framebuffer with `GET`/`PUT` sprite capture and a fixed screen
  coordinate system (the canvas is a display list with fit-to-content), and a
  sound-free `STICK`/`STRIG` gamepad backend on this platform.

---

## Next: finish the IDE itself

**Debugging** — every language can stop. `DebuggerRunner` drives Scheme, and the
**Debug** tab is a real panel over it: Continue, Pause, Step In/Over/Out, Stop,
breakpoints, the line, the procedure, the call stack and the frame's bindings.
The six line-by-line languages (BASIC, FOCAL, PILOT, COMAL, APL, REXX) stop at a
statement through a `DebugSession` in the shared interpreter loop; the whole-program
languages stop at their own natural boundary — Logo at a top-level statement,
Smalltalk at a message send, CHIP-8 before an instruction (the listing line is kept
per opcode address). Gutter breakpoints are persisted per file and pruned when the
file is gone, and the panel's buttons drive whichever runner is paused. What is
still missing is *depth*: no watch expressions, no data breakpoints, and a
Smalltalk Program Counter is shown as a line rather than a stack frame.

**Variables and the Telemetry HUD** are built too, and both answer the question a
placeholder could not: *what did that run actually do?* The **Variables** tab lists every
binding the engine can see, with the type it knows it by, filtered by name. The
**Telemetry HUD** shows labelled counters — steps used, engine time, steps per second,
last line, recursion depth, call stack, output lines, drawing commands, and BASIC's
memory window — and puts the IDE's own rows (wall clock, ink on the canvas) beside them,
so a drawing the engine counted but the canvas never received reads as two rows that
disagree rather than as silence.

**Editor** — the document outline is built: `outline.{hpp,cpp}` parses the named
places out of Logo, BASIC, COMAL, REXX, PILOT, Scheme and APL sources, and the
**Document Outline** dock (`OutlinePanel`) lists them and jumps on a click,
refreshed on the same debounce as the checker. The
minimap and whitespace marks are built (`native/src/ui/code_editor.cpp`: a
sampled strip in the right margin that clicks to scroll, and the document's own
`ShowTabsAndSpaces` option so the marks follow selection and scrolling without
per-character painting). Error markers and auto-completion are built
(`native/src/ui/lint.cpp`, with squiggles, gutter triangles and tooltips), and so
are Format Code (`native/src/ui/formatter.cpp`) and Find in Files
(`native/src/ui/find_in_files.cpp`, Ctrl+Shift+F, which skips binaries, build
trees and VCS directories and reports a broken pattern as an error rather than as
"no matches").

Two of the entries that used to be on that list are built. Per-language syntax
colouring is `native/src/ui/syntax_highlighter.cpp`, one rule table per language
because the ten disagree about what a comment even is. Find, replace and go-to-line
are `FindBar` (`native/src/ui/find_bar.cpp`), which sits under the editor tabs: the
*searching* lives on `CodeEditor` — with wrapping, an undo-able Replace All — so the
behaviour is testable without a window, and the bar is the view over it. F3 and
Shift+F3 find the next and previous match with the bar closed, and go-to-line is
Ctrl+L, because the canvas already owns Ctrl+G.

**Console** — clickable `line N` links into the editor, an error-only filter, and
save-to-file.

**Files and sessions** — recent files and restoring the last session at launch are
built: `MainWindow::saveSession` records the documents that were open,
`restoreSession` reopens them, and File > Open Recent lists the last ten paths (with
a Clear Menu entry). Data safety is built too: `AutosaveStore`
(`native/src/ui/autosave.cpp`) keeps five snapshots of each document with unsaved
changes under the data directory, and `VersionHistoryDialog`
(`native/src/ui/version_history.cpp`, Ctrl+Alt+V) previews them and opens one in a
new tab without writing the file. The **split editor** is built too: File > Split
Editor Right (Ctrl+\), Close Split and Move Tab to Other Pane move documents
between the two panes, the split is saved with the session, and Run > Run Both
Panes compares two programs headless in one table (output, time, ink) with no
shared variable bus. The **terminal** is built as an opt-in: Settings > Workspace
turns on a shell in the document's folder, and presentation and player modes remove
it. There is no project panel yet.

**Also unbuilt** — font *family* selection and the REPL bar's interpreter wiring
(the bar is laid out and its signal exists; nothing is connected to it). Printing
(File > Print Code, Ctrl+P, and File > Print Drawing), code PDF export, session
HTML and lesson-session export as Markdown or PDF are built, as are the
accessibility controls: View > High Contrast (Ctrl+Alt+H), the A+ text-size ladder,
View > Focus Mode (F11), View > Presentation Mode (Shift+F11) and View > CRT
Settings with the four Screen Mode presets.
The snippet library is built:
Edit > Insert Snippet (Ctrl+Shift+I) reads `native/share/snippets.json`, which
ships beside the application, and every snippet in it is syntax this build runs.

**Input** — the input round-trip (the prompt, and the `in`/`in-reply` protocol the
reference worker uses) is implemented,
but it has no automated coverage, because the prompt is modal. It needs a manual
pass with a PILOT or BASIC program.

---

## Next: the retro workstation feel

**Themes are built.** All 33 of the removed implementation's schemes are back
(`native/src/ui/themes.cpp`, data in `theme_table.inc`, generated from the Python's
table): the retro sets (Amber Lab, Borland Turbo Vision, NeXTSTEP, Amiga Workbench,
CGA, C64, Apple II, DOS Blue, ZX Spectrum), the modern editor palettes (Dracula,
Nord, Solarized, Catppuccin, Tokyo Night, Gruvbox) and the accessibility sets (high
contrast, dyslexia friendly, two monochromes). View > Theme lists them, Shift+F10
cycles them, the choice is persisted, and one record reaches everything: the
application palette, the window's own rules, the editor's syntax colours, the
canvas paper and the lesson panel's verdict colours. `themes::paletteFor` also
sets the `dark`/`mid`/`shadow` roles the base stylesheet reads — a palette that
left those at the platform's values paints light-theme borders on a dark window,
which is the failure mode that makes a "ported" theme look half finished.

**CRT effects** — the overlay (`CrtOverlay`, `native/src/ui/panels.cpp`) draws
scanlines and is now switchable (View > CRT Scanlines, Shift+F7). Curvature,
vignette, glow and flicker are not drawn, and there is no intensity control in the
UI.

**The function-key bar is built**, with the Python's three modifier layers, a badge
that cycles them, and a dispatcher behind every key; **the command palette** is
built too (F10, Ctrl+Shift+P). What remains from that list: the workspace decks —
the deck selector offers four, and
`2:SPLIT` answers that the split workspace is not built (the second editor pane
exists in the layout but nothing creates a tab in it).

**Turtle showpiece** — exporting the drawing and copying it to the clipboard are
built: File > Export Drawing (or **Ctrl+E**), Edit > Copy Drawing, and
`time_warp_ide --run --export drawing.png` for a script. What is exported is the
*drawing* — the same `paintInk` the window paints with, without the grid or the
turtle marker, twice the on-screen size — so a saved picture is a picture rather than
a screenshot.

**SVG export is built** and needs Qt6::Svg, which the build asks for as an
optional component: with it, the save dialog and `--export` offer `.svg` and the
file holds vector paths painted by the same `paintInk`; without it, neither
offers the format. **Printing is built** the same way, through Qt6::PrintSupport:
File > Print Code (Ctrl+P) lays out the program as the A4 document the PDF export
writes, and File > Print Drawing renders the canvas at the printer's resolution
and scales it to the page. Without the component neither entry appears. Still
missing here: a turtle-state inspector and a drawing-command log. The PDF of the
*code* and the session HTML are built, and neither needed a new component:
`QPdfWriter` is part of Qt Gui.

---

## Distributing a program

`time_warp_ide --compile program.bas --output hello` writes a standalone executable:
a byte-for-byte copy of the IDE with the program appended as a trailer
(`native/include/timewarp/ui/standalone.hpp` documents the format). Running it opens
the window on the drawing, with the authoring UI hidden, the document read-only and
the program already run — what a learner means by "send it to somebody".

Limits, stated rather than discovered:

- The artifact links against Qt like the IDE does, so it needs the same Qt runtime.
  A static build is not made.
- Each artifact carries a full copy of the IDE (about 6MB), not a minimal player.
  A dedicated `time_warp_player` target would be smaller and is the obvious next
  step; it is not built.
- The payload is appended, not compiled: the program is still interpreted, at the
  same speed. Nothing is optimised and nothing is hidden — `strings hello` finds the
  source, which is the right answer for a teaching tool.

---

## Packaging

Built, from a tag, by `release.yml`: a **`.deb`**, an **`.rpm`**, an **AppImage**
and a **tarball** for x86_64 and arm64, each with a checksum, attached to the
GitHub Release; and a **container image** on `ghcr.io`. `cpack` produces the
packages from the same `install()` rules the desktop entry and icon already used,
so a package cannot contain a different set of files from a local install.

Still not built: **Flatpak or Snap** (both need a runtime and a manifest that this
repository does not carry), a **Windows installer**, a **macOS bundle**, an
**arm64 AppImage** (the tooling exists; the pipeline does not exercise it yet),
**automatic publishing to a distro repository**, and a **static build** — the
packages depend on the distribution's Qt 6.

---

## Deliberately out of scope for now

A web (WebAssembly) build, and calling one language from another.

The **examples browser** is now a File menu grouped by language rather than the
old window with categories, difficulty and search. **Accessibility presets** are
built (phase 8 of [PORTING_PLAN.md](PORTING_PLAN.md)): high contrast, the A+ text
ladder, focus mode, the CRT settings dialog and the four Screen Mode presets. The
guided tour
and its one-time coach marks (phase 3.3) are built: Help > Guided Tour, a
first-run overlay anchored to the Run button, canvas, lessons panel and
function-key bar, re-armable from Settings.

The **lessons system**, with progress tracking and a classroom mode, used to be on this
list. It is now implemented — see `docs/lessons/`. The class list is in the IDE as
well: the right column's Classroom page shows the roster, the current assignment
and one row of marks per learner, read through the bridge from the same files
`teach.py` writes. Presentation mode (View > Presentation Mode) hides the chrome
and takes the lesson panel and console to the size and window style
`teach.py present` sets, a lesson session can be exported as Markdown or PDF with
its checkpoints, this session's verdicts, the program and the output, and the
class can be bundled or unpacked from the same page. The **telemetry HUD** was on
it too and is now the Telemetry tab described above.

---

## How to contribute

1. Fork the repository and create a feature branch.
2. Read [CONTRIBUTING.md](../CONTRIBUTING.md) for the build and test workflow.
3. Check [open issues](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues)
   for `good first issue` labels.
4. Run `make test-all && make lint` before opening a pull request.

**Maintainer:** James Temple — [james@honey-badger.org](mailto:james@honey-badger.org)
