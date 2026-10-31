# Roadmap

Time Warp Studio is an old-school programming environment for eleven languages and two machines. This page is
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

## 24.0 is archived, and 25.0 is the current design

**Version 24.0 is archived.** It was the golden release, the last one built for
Windows and macOS, and it is kept exactly as it was built on its own release page and
will not change again. Nothing here removes it: an archive is a thing you keep.

**Version 25.0 is the current design,
*timey wimey wibbly wobbly, time & multiuniverse travel computing*.** The eleven
languages, the IDE, the two machines, the curriculum and the documentation set are
complete, and the gates in [README_TESTING.md](README_TESTING.md) prove it on every
push. The feature set is **not frozen**: a release after this one takes fixes and
features alike, and each arrives with the test that proves it.

What comes after is written down rather than wished for:

| For | Proposal |
| --- | --- |
| The HB-1 | [hb1/enhancements.md](hb1/enhancements.md) — a tape drive, a monitor and `SYS`, programmable function keys, sprites, RAM size, HB DOS 1.1, autoboot |
| The HB-1, in 1992 | [hb1/model-92.md](hb1/model-92.md) — the overhaul: a bus and a device map, a video chip, four-channel audio, HB DOS 2.0, input devices, HB-16 with an assembler and a monitor, a module bay, and a desktop running eleven processes |
| The CHIP-8 | [chip8/enhancements.md](chip8/enhancements.md) — breakpoints in the window, an instruction trace, memory watch, state files, a library manifest, the rest of the SuperChip subset |

Each proposal states what it is, why the machine wants it in period terms, and what
would count as proof, so a later release has a specification to build to instead of
a memory of a conversation.

---

## Working today

| Area | State |
| --- | --- |
| Shell | Documents and tabs, menus, toolbars, status bar, settings persistence, the last session's files restored at launch |
| Editor | Line-number gutter, current-line highlight, per-language syntax colouring, find/replace, find in files, go-to-line, tab stops, fonts, error squiggles with gutter markers, completion, auto-indent, Format Code, minimap, outline |
| Projects | `.twproject` files, named groups, files and folders added to a project, drag-and-drop grouping, missing-file reporting, recent projects and session restore |
| REPL | A persistent session per language under the editor; Scheme keeps definitions and echoes values, the line-loop languages and Logo keep bindings, while Smalltalk, CHIP-8 and Forth run each entry as a whole program |
| Console | Streaming output, interpreter errors in their own tab, clickable line links (`[12]`, `❌ [line 30]`, `at line 30:`) that go to the line the program numbers, an errors-only filter, and **File > Save Output As…** |
| Canvas | Live drawing, zoom, pan, grid, auto-fit, turtle marker, scrubber playback, animated GIF recording, raster and SVG export, the pose strip and the command log, and a fixed-coordinate screen mode (`SCREEN`) with sprites, pixel read-back and a game example; in screen mode the wheel zooms the screen about the pointer and a click reads back the pixel under it |
| Display | 35 themes, high contrast, A+ text sizes (100/125/150%), focus mode, CRT settings, four Monitor presets |
| Execution | Run (F9) and Stop; one program at a time; unsaved-change prompts |
| Input | A program can ask for a line and wait for it — `INPUT`, FOCAL's `ASK`, PILOT's `ACCEPT`, REXX's `PULL` — on whichever engine runs it; BASIC's round trip is asserted on the engine, on `NativeRunner`, and through the API |
| HB-1 machine | Tools > HB-1 Computer (`Ctrl+Alt+W`): one CRT with a boot intro, a language ROM socket for all eleven languages, TEXT/SPLIT/GRAPHICS modes, a program editor, a turtle canvas, and a prompt with BASIC direct mode; CONFIG (`Ctrl+,`) sets the boot ROM and screen, start-up message, phosphor palette, text sizes and CRT glass; each ROM's program, the ROM, the screen mode and the configuration persist; HB DOS 1.0 stores programs on a 512-sector volume with an Apple DOS-style catalogue, lock bits and a two-step INIT. Manuals: [the machines](MACHINES.md), [HB-1](hb1/README.md) |
| CHIP-8 machine | Tools > CHIP-8 Emulator (`Ctrl+Shift+M`): a live 60 Hz CHIP-8 with a 64×32 screen (128×64 hi-res), keypad, Run/Pause/Step/Reset, 1–2000 opcodes per frame, register/memory/listing inspector, sound, and a ROM library the user points at a folder. It assembles listings and loads binary ROMs; the editor's CHIP-8 Run is the whole-program path. Manuals: [machine](chip8/README.md), [assembler](chip8/assembly.md), [instruction set](chip8/instruction-set.md) |
| Languages | 11 languages, 82 example programs, three of them written for the HB-1 |
| Native engines | Expression evaluator (`timewarp_expr`) and Scheme (`timewarp_scheme`) |
| Bridged engines | None. Every language runs in process, nothing the product runs starts Python, and the reference engine itself is deleted — the parity gate compares against a frozen corpus instead |
| Native executors | All eleven — BASIC, FOCAL, PILOT, COMAL, APL and REXX line-by-line; CHIP-8, Logo, Smalltalk, Scheme and Forth whole-program |
| Printing | File > Print Code (Ctrl+P) and Print Drawing, through the optional Qt6 PrintSupport |
| Profiling | View > Profile Run times every statement in the line-by-line languages and ends with a per-line report, longest first |
| Terminal | Opt-in in Settings > Workspace: a shell on a real pseudo-terminal in the document's folder, absent in presentation and player modes |
| Debugging | Breakpoints, Pause, Continue and Trace in all eleven languages; a run can be walked back in every language but Smalltalk, whose heap is an object graph, and Scheme, whose state is an arena; Scheme adds a stack and frame bindings, and Smalltalk reports its own message-send chain; watch expressions and data breakpoints in the Debug panel |
| Database | A SQLite-backed Database tab, the shared `SQL` statement in nine languages, scripts, tables and schema; compiled programs link SQLite |
| Automation | JSON-RPC over stdin/stdout (`language.run`, `repl.*`, `debug.*`, `sql.*`, `project.*`, `watch.evaluate`) and the `twc` terminal compiler |
| Packaging | `make install`; tagged releases build `.deb`, `.rpm`, AppImage, tarball and container artifacts for Linux |
| Quality gates | 26 CTest suites in 4 build configurations, on Linux; the frozen-corpus gate over 671 programs (479 language, 192 Scheme); 566 command rows run as programs and through the REPL; 81 example programs through the smoke gate; and the documentation gates (links, claims, version, Markdown lint) |

---

## Next: interpreter defects the command examination found

`time_warp_gates commands` drives one row per documented command through
`time_warp_check` and the C++ executors (`native/share/command_rows.json`). Examining
every command turned up defects that no gate had been looking for. Those fixed have a row
of their own so they cannot come back silently; those still open are listed here
rather than left as folklore, and where a row cannot express them the reason is
written down in `WAIVERS`.

**Still open**

Nothing: the last defect here -- a COMAL `FUNC` body that could only be declarations,
assignments and one `RETURN` -- is closed. A function body now runs as statements, so
`IF`, the loops and a call to another function all work inside one, and `RETURN`
leaves the body and answers the expression that called it. `LOCAL` works the same way
it has since 23.2.0: the caller's value goes back on every exit path. The section
stays because the next port has to keep these fixed.

A FOCAL `DO` group used to run to the end of the program because there was no
`RETURN`. At 23.2.0 there is one: `RETURN` leaves the innermost group and hands
control back to the line after the `DO`, a `RETURN` with no group around it is
reported rather than quietly stopping the program, and `focal/showcase.foc` no longer
has to put its group last.

One gap closed itself: at 22.4.0 a COMAL program read `mark(8)` as zero four lines
after reading it correctly inside a loop. It has not reproduced since, in the program
or in any isolation, and `comal/results_table.comal` reads the extremes directly now
with the smoke gate behind it. It is recorded here because a symptom nobody can
explain is worth knowing about.

Every defect below has a regression row in `native/share/command_rows.json` and, where the
behaviour is expressible in a unit test, a case in the language's test module. The
section stays because the next port has to keep them fixed: a port that quietly
restores one of these is a regression even though it is new code.

**Fixed while writing the examples at 22.4.1**, all found by programs rather than by
reading, and each with a command row: BASIC's `FUNCTION ... END FUNCTION` could not be
called at all (the body's statements now run, text answers and printed output
included); `STR$` refused an array element; `PRINT USING` printed only its first value
and swallowed the space that separates two fields; `EXIT FOR`, `EXIT DO`, `EXIT WHILE`,
`EXIT FUNCTION` and `EXIT SUB` did not exist, so a function could not return early;
`at:`, `at:put:`, `first` and `last` were on `Array` but not on `Collection`, so an
`OrderedCollection` "did not understand at:"; FOCAL's `;` separated commands only
inside a `FOR`. One corpus entry (`space_simulation.bas`) carries a divergence note as
a result: the reference printed error lines where the program asked for a table.

**Fixed during the earlier examination** (each has a regression row, and the first two also a
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

**Found since, by building the game example:** the run loop detected a jump by
comparing the program counter before and after a statement, which cannot see a jump
to the line already executing. Two defects came from that one cause, and both are
fixed in both engines with a note here:

- An empty-bodied `FOR` loop ended its *enclosing* loop after one iteration,
  silently: `NEXT` returned to the line it was already on, the jump was invisible,
  and the inner frame stayed on the stack for the outer `NEXT` to consume.
- `10 GOTO 10` **ended the program** instead of looping, so a learner's first
  infinite loop did not loop.

The cause is fixed rather than each symptom: `Interpreter::jump_to` records the
jump (and `jump_to` in the reference), every executor's jump goes through it, and
the workaround inside `NEXT` is gone.

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
It no longer describes the removed PySide6 implementation. The gaps it recorded at
that point — the REPL whose plumbing was absent, and the debugger's missing depth
(watch expressions) — have since been built and the sections now describe them. It
is the system-level description; `native/README.md` remains the detail on the C++
libraries.

---

## The four newest languages are complete

**APL, FOCAL, CHIP-8 and Smalltalk** arrived after the original six. Each now has
everything the others have:

- a native **executor** in `native/src/lang/`, and rows in the recorded parity
  corpus that `make golden-check` compares it with,
- a **worked example** under `Examples/` that `make smoke` runs and checks for ink
  (all four draw),
- a **command reference** in `docs/LANGUAGE_GUIDE.md` and a **tutorial** in
  `docs/tutorials/`,
- **command rows** covering every literal the inventory
  (`native/tools/gates/inventory.cpp`) finds in their executors, so `make
  command-matrix` drives them the way it drives BASIC.

Writing those rows is what turned the inventory's counts into the list of defects
above: nine of them were found by a row that had no answer, not by reading the code.

**Lessons now cover all eleven languages.** Every language has two, three checkpoints
each: sixty-six in all, with a plan, a worked answer and hints for every checkpoint,
reachable from a terminal (`time_warp_learn`) and from the 🎓 Lessons panel in the IDE.

The curriculum is data, and `time_warp_teach validate` **runs all sixty-six solutions through the
engine** and checks that none of the sixty-six starters already passes. That pass is what makes
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
- **CHIP-8 run as a program has no interactive keypad.** The whole-program
  executor runs to completion inside one call and never yields control, so `LD Vx, K`
  reads the first key held according to the listing's `; keys` directive, or 0. The
  Tools > CHIP-8 Emulator window is the interactive path, with a live keypad.
- **APL's turtle position is `TX`, `TY` and `THETA`.** Deliberately two letters:
  `X`, `Y` and `H` are ordinary APL variables, and taking them would break the most
  natural line anyone writes.
- **Smalltalk has no metaclass tower, no exceptions and no image.** A class object
  holds two method dictionaries rather than being an instance of a metaclass, so
  `Counter class` answers the class itself. That is enough for everything the
  language is taught for and is the part of Smalltalk-80 a C++ port would otherwise
  have to reproduce in full.

---

## Ports complete: the bridge is gone

Every language runs in C++ now, and the Python engine those ports were judged
against has been deleted. The three jobs it kept have each been taken over:

1. **The parity gate** compares each port with a *recording* —
   `native/tests/golden/lang_corpus.json.gz`, the reference's answers to 470
   programs, which carry the program text so the corpus outlives the engine.
2. **`make command-matrix` and `make smoke`** (`time_warp_gates commands` and
   `smoke`) drive the native engines, so a language that runs and draws nothing is
   still caught.
3. **The lesson check** is `timewarp_lessons` — the same library the Lessons panel
   asks, so the panel's verdict and `time_warp_teach`'s are one function's answer.

That is a weaker first item than a live differential and the difference is written
down rather than glossed: a recording cannot notice a mistake in the recording. In
exchange the repository needs no Python toolchain, and the gate runs against the
engines the product actually ships rather than against the ones that were ported
*from*.

**BASIC was the last port.** The core language, `PRINT`/`INPUT`/`FOR`/`WHILE`,
narrowing variable types, `SUB`/`FUNCTION`, records and the file channel, the VGA
statements, and the reference's Logo fallback (a `.bas` file's `FORWARD` is
Logo's `FORWARD`) are all in `native/src/lang/basic*.cpp`. It is gated by the
same rows and examples the reference is: 138 programs, 0 differences.

Lesson checking, the curriculum and the classroom are native, the worker classes are
gone from the application and from the installed payload, and the reference engine
itself is deleted. What a contributor needs to run every gate is a C++ compiler,
CMake and Qt — no interpreter, no virtual environment, and no second language to
keep in step.

BASIC's extended statement set is ported too: shapes, particles, fractals, music,
speech and the gamepad words are in `basic_features.cpp` and `basic_library.cpp`,
and the command table exercises them. What is now true, and what is still limited:

- **Sound is real, and it mixes** (`native/src/audio/music.cpp`,
  `native/src/audio/mixer.cpp` + `native/src/ui/sound_player.cpp`). `BEEP`,
  `SOUND` and `PLAY` synthesise square/triangle/sine/noise at equal
  temperament, parse `PLAY`'s notation (tempo, lengths, octaves, accidentals,
  dotted notes, rests, `N` numbers) and play through the first of `paplay`,
  `aplay`, `afplay`, `ffplay` or `play` on the machine. `SOUND` joins the mix instead of
  replacing it: up to eight voices are summed sample by sample
  (`timewarp::audio::Mixer`, a pure function so `audio_tests` can assert that two
  notes are audible together on a machine with no sound card), and the mix is
  rendered to one file, so there is still one player process. `PLAY` is a tune
  and stops the mix first, because a note list is sequential. The
  acknowledgement text is still the reference's, so parity is unchanged.
  **Limits**: a machine with none of those players is silent, and
  `audio_available()` says so; `PLAY` has no chord syntax, so a chord is written
  as several `SOUND` statements.
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
  in `docs/LANGUAGE_GUIDE.md` and **not** in `command_rows.json` — the parity gate
  cannot compare a command one side does not have.
- **Graphics for games** is a screen, not a fitted drawing: `SCREEN` (text and
  the numbered modes 1, 2, 7, 9, 12 and 13), `PSET`, `LINE`, `CIRCLE`, `PAINT`,
  `POINT` read-back, `GET`/`PUT` sprite capture, `DRAW`, `SPRITE` (position
  tracking), text at a point, palette indices, `INKEY$`, `JOYINIT`/`STICK`/`STRIG`
  and `WAIT` for the loop. A program that names a screen gets a framebuffer painted
  at fixed coordinates; anything else keeps the fit-to-content display list. The
  numbered modes, sprite capture, `POINT` and the gamepad are native extensions the
  reference never had, so `lang_tests` and the shipped example are their gate
  rather than the parity corpus.

---

## 17.0

A major version here is a claim, not a number. 16.0 claimed that all ten languages
run in C++. 17.0 claims two: the product contains no Python, and a program can draw
and play a game. This section is what stands behind them.

**The product has no Python in it.** The bridge had two runtime jobs left, and
both are native now. `timewarp_lessons` (`native/src/lessons/`) reads the
curriculum from `native/share/lessons.json` — the file moved out of the reference's
package, so a package and a checkout teach the same course — and judges a program
through the engines with `RecordingHost` as the surface, which is what makes a
lesson checkable on a machine with no window. `time_warp_teach validate` runs all
sixty worked answers and all sixty starters through it: every answer passes, every
starter fails, every language is taught. `time_warp_learn` is the learner's side of
the same library — `list`, `show`, `start`, `check`, `hint`, `solution`,
`progress`, `reset` — so a learner at a terminal and a learner in the panel are
asking one verifier. The `lessons_tests` suite asserts the same
thing, so the curriculum is data that something executes deliberately rather than
data that sits in the tree looking correct.

The Lessons and Classroom panels are views over that library now: opening a tab
loads in process, a check and a mark run in process, and the worker signals they
used to listen to are gone. `ui_layout_tests` raises the Lessons tab in a window
with no bridge and asserts the twenty lessons arrive.

The classroom — rosters, handouts, marking, bundles — is `lessons::Classroom`, with
one deliberate addition: bundles are written and read with zlib, so they stay the
plain zips `time_warp_teach` writes and any school can open. `classroom_tests` covers the
promises a teacher relies on, and a bundle made by either side opens in the other.

<!-- history -->
The worker classes (`PythonWorker`, `locateWorker`, the `timewarp_bridge` target and
its suite) were deleted at 17.0, and `native/bridge/` has not been installed since: a
staged `cmake --install` contains **no Python file at all**.
<!-- /history -->

The parity gates' remaining dependency is done too: the reference's answers to
476 programs -- and three Forth ones recorded from this engine, which had no reference
run -- are frozen in
`native/tests/golden/lang_corpus.json.gz`, program text included, and
`make golden-check` compares the C++ engines with them without importing Python.
`time_warp_gates golden --record <language>` writes entries for a language's
example programs from the engine this build ships; after the reference's deletion the
corpus *is* the record, and editing an example without re-recording is reported as a
corpus problem rather than as a guess. See [README_TESTING.md](README_TESTING.md).

**A program can draw a game.** All of it is in the tree now. `SCREEN 0` and the
numbered modes 1, 2, 7, 9, 12 and 13 size a real pixel surface
(`timewarp::lang::ScreenMode` carries them as data), `POINT` reads a pixel back as
`&H00RRGGBB`, `GET`/`PUT` capture a sprite into an array and restore it, and
`JOYINIT`/`STICK`/`STRIG` are answered by `/dev/input/js*` instead of by the
constant 0 the reference had no backend to replace. A program that names a screen
now gets a framebuffer to look at: the canvas paints it at whole-number scale at
fixed coordinates, stops fitting itself to the ink, and exports it at its own size. A
screen is a place you can look *at*, not a picture you press Play on: the wheel zooms
about the pointer so a 160x100 sprite stays legible, a click reports the pixel under
it in the status bar as a coordinate and `&H00RRGGBB`, the strip under the canvas
names the mode the program asked for, and the scrubber explains that it is disabled
rather than looking broken, because a framebuffer is not a display list to replay;
the display list is still built, so the Turtle Log, the scrubber and the vector
export keep working. `Examples/basic/star_patrol.bas` is a game built on all of it,
and `lang_tests` runs it and asserts that it asked for a screen, placed a sprite and
read a pixel.

The screen scrolls under a zoom as of 17.1: the wheel zooms about the pointer, the
scale snaps to a whole number of device pixels so a one-pixel line stays one pixel
wide, a click reports the pixel under it, and the strip under the canvas names the
mode. What it still does not do is let a menu choose the mode: the program names it.

The reference implemented none of it, so this is the first feature the parity gates
cannot compare: its own suite (`lang_tests`) and its own example are what hold it
up, and the gamepad rows are skipped by the gate rather than compared — a joystick
is hardware, and a gate that reported one as a language difference would be
measuring the wrong machine.

### The ladder

| Release | Claim |
| --- | --- |
| 16.5 ✓ | The framebuffer and fixed-coordinate modes, `GET`/`PUT`, gamepad input |
| 16.6 ✓ | Audio that mixes — several voices in process, not one `paplay` per note |
| 17.0 ✓ | No Python in the product: lessons and classroom in C++, the worker deleted, nothing Python installed; `make golden-check` is the parity gate |
| 17.1 ✓ | A screen you can look at — zoom about the pointer, click for a pixel, the mode named in the strip — sound on Windows through PowerShell, and a file fence made of components rather than a string prefix |
| 18.0 ✓ | the reference engine deleted and every gate driving the native engines; the teacher's and learner's commands are the C++ ones; `time_warp_player` and a format-2 artefact replacing the copy-of-the-IDE; the two machines exportable as files |
| 19.0 ✓ | Every command the canvas declares, it draws: fills, pen styles, gradients, bezier curves, sprite objects, palette cycling and the fixed-coordinate screen — and `turtle_canvas_tests` walks the enum so a vocabulary that outruns the surface fails the build |
| 19.1 ✓ | The vocabulary closes at 44 of 44 with no waivers — `Pitch`/`Roll` deleted because the camera lives in `TurtlePose` — and two of those commands reach a program: Logo's `PENSTYLE`, and BASIC's `PALETTE CYCLE`, which had been moving a palette nobody was looking at |
| 19.2 ✓ | Every command the canvas declares, a program can ask for: Logo's `BEZIER`, BASIC's `SPRITE`/`SPRITEMOVE`/`SPRITESHOW`/`SPRITEHIDE`/`GRADIENT`/`ELLIPSEGRADIENT` — and the three statements that had been acknowledging work they never did now do it |
| 19.3 ✓ | The HB-1 is a file: `SAVE MACHINE`/`LOAD MACHINE` write and boot one `.hb1` holding the ROM, the screen, every program, the phosphor and the disk; `--machine` boots one from a terminal |
| 20.0 ✓ | The HB-1's line editor: `AUTO`, `RENUM` (which rewrites every jump), `DELETE a-b`, `LIST a-b`, `UNDO` after `NEW`, and `TRON`/`TROFF` — the first release of HB-1 2.0, where the machine's insides stop being decoration |
| 20.1 ✓ | The 64K is real and the screen is memory: `POKE` a character into `$8000` and the tube shows it, `LOCATE` moves the cursor `PRINT` writes at, `CLS` clears cells, `MEM` reports the map, and a `POKE` into ROM is refused out loud |
| 20.2 ✓ | The machine has a clock, a voice and a switch: a self-test that counts the RAM it has, a sound latch a program pokes, five sounds nobody asked for, a 60 Hz frame counter, `SLEEP` that waits and `ON TIMER` that fires, and a POWER lamp that turns the machine off |
| 21.0 ✓ | The eleventh language and a palette that means one thing: Forth — dictionary, colon definitions, three control structures, `VARIABLE`, `RECURSE`, turtle words, `POKE`/`PEEK` into the machine's memory — with 86 command rows, two lessons and three programs; and `SETPC 1` now names the colour the reference gave slot 1 in every language instead of near-black |
| 22.0 ✓ | The harnesses are C++: `time_warp_gates` asks the four questions the Python scripts asked -- smoke, commands, repl, golden -- the command table is `native/share/command_rows.json`, and seven scripts left the tree with every make target keeping its name |
| 22.1 ✓ | No Python: the documentation gates and the version tool are `time_warp_docs` (its own CMake project, no Qt), the icon generator is `time_warp_icon` and reproduces the packaged icons byte for byte, `./twc` is a wrapper around the IDE's compiler, and `make tree-check` fails if a `.py` comes back |
| 22.2 ✓ | The Markdown lint is C++: `time_warp_docs md` -- twenty-eight rules, `--explain` to see how a line was read -- replacing `markdownlint-cli2` and its `.markdownlint.json`, with the CI job promoted from advisory to a gate; running it found a line-splitting bug in the shared scanner that had been cutting every `✅` bullet in two |
| 23.5 ✓ | The debt before a claim: Forth joins the parity corpus (three programs recorded from this engine by `golden --record`, because no reference run exists) and the console gains the line links, error filter and save that this page had already been claiming; the editor gains several carets; and `time_warp_docs claims` reads `native/share/unbuilt.txt` so a page that calls a shipped feature missing fails a gate instead of standing |
| 23.6 ✓ | A run can be walked backwards: `Interpreter::snapshot()`/`restore()` copy a statement's position, bindings, output and the language's own scratch state, `set_history` keeps the last N of them and `rewind` goes back through them; `debug.start` takes `history` and `debug.rewind` returns to a statement with the bindings and output it had. The test is the property a person would check: go back, run on, and the transcript is what it was |
| 23.7 ✓ | And a person can press it: the Debug tab's **History** depth decides how much a run remembers, **Back** and **Back 10** walk the pause back through it, and both are live exactly when they can move — never at the oldest statement, never for Scheme, which says why. The engine goes as far back as it kept instead of refusing, and never forward, and the panel asks the engine how far back that is before it offers the button |
| 23.8 ✓ | A language that runs its own loop can be walked back too: the whole-program pause point takes the executor's checkpoint, records it and returns a restored one to adopt, so Logo keeps its position, procedures, words, pen, pose and generator in a snapshot and a rewind puts the run back on an earlier statement. The API's transcript rolls back with it, and the console marks where a re-run begins |
| 23.9 ✓ | Forth joins the debugger: a pause before each token, with the position, both stacks, the dictionary, the compile state and the text printed so far travelling in a checkpoint — and the 64K data space copied only when a program has actually written to it. A `.fs` breakpoint can fire for the first time, and the run can be walked back through it |
| 24.0 ✓ | CHIP-8 can be walked back too: the checkpoint is the machine's whole state — registers, memory, timers, RPL flags, the frame count, the instruction's slot within the frame and the framebuffer — taken before each instruction and only when a history was asked for, and a rewind repaints the restored screen so the pause shows the pixels the instruction it returned to was drawn on. The test is the drawing rather than a transcript, because a CHIP-8 program prints nothing: an interrupted run that rewinds and replays leaves the same pixels, in the same order, as the run nobody touched |

Smaller items belong in one of those releases rather than a release of their own:
Snap and an arm64 AppImage, and the distribution work described under
[Packaging](#packaging). What used to be listed here shipped: Smalltalk reports its
call stack as frames rather than a Program Counter (23.3.0), and the HB-1 has its own
voice — boot tone, key click, disk rattle, error buzz and tape motor — through
`MachineVoice` (20.2).

---

## Next: finish the IDE itself

**Debugging** — every language can stop. `DebuggerRunner` drives Scheme, and the
**Debug** tab is a real panel over it: Continue, Pause, Step In/Over/Out, Stop,
breakpoints, the line, the procedure, the call stack and the frame's bindings.
The six line-by-line languages (BASIC, FOCAL, PILOT, COMAL, APL, REXX) stop at a
statement through a `DebugSession` in the shared interpreter loop; the whole-program
languages stop at their own natural boundary — Logo at a top-level statement,
Smalltalk at a message send, Forth at a token, CHIP-8 before an instruction (the
listing line is kept per opcode address). Forth was the last to join (23.9): its
interpreter had no pause point at all, so a breakpoint in a `.fs` file could not
fire and nothing said so. Gutter breakpoints are persisted per file and pruned when the
file is gone, and the panel's buttons drive whichever runner is paused. **Watch
expressions** are evaluated by the same expression engine a program uses, over the
bindings the pause reported, and **data breakpoints** stop a run when a named
variable changes — after the statement that changed it for the line-loop
languages, at the next form for Scheme, and at the next natural boundary for a
whole-program language. **Smalltalk's call stack is built**: the runtime records
the message sends that have not returned, and a pause publishes them — outermost
first, each as `Class>>selector` with the line it is at — to the Debug panel and to
`debug.*` over the API. The same pass fixed the lines they are reported with: a
chunk is parsed on its own, and its sends used to be numbered from 1 inside the
chunk, so a breakpoint set by clicking the gutter could never match inside a
method. The chain mechanism is `Interpreter::set_call_stack_probe`, so a machine
that runs its own loop and has a stack to show joins by answering the probe rather
than by growing the debugger. **The line-loop languages publish theirs too.** Their
executors are called once per statement, so a probe over the executor would not
outlive the statement that made it; the return stacks they already keep — BASIC's
`GOSUB` and `CALL`, COMAL's `PROCEDURE` and `GOSUB`, PILOT's `U:` units, FOCAL's
`DO` groups and REXX's `CALL` — are mirrored on the interpreter at exactly the
points they are pushed and popped, and a pause reports them outermost first under a
`PROGRAM` frame that waits at the line its first call was made from. A `DO` group's
lines run inside the `DO` statement rather than through the loop, so that is also
the only place a breakpoint inside a group can be honoured — which it now is, and
did not use to be. Logo and CHIP-8 keep no return stack, so they report no chain.

**A run can be walked backwards, in the engine and over the API.** `Interpreter`
can copy where it is — the statement, every binding, the text printed so far, the
drawing count and the language's own scratch state — and put it back:
`snapshot()`, `restore()`, `set_history(depth)` and `rewind(steps)`. The loop
takes a copy each time it reaches a statement, up to the depth asked for, so a
session started with `history` can return to one it has already passed;
`debug.start` takes that number and `debug.rewind` goes back through the copies,
answering the bindings and the output the run had at the statement it returns to.
`lang_tests` asserts the property a person would check rather than the mechanism:
rewind, run on, and the transcript is what it was. Two honest limits, both stated
in [API.md](API.md) rather than discovered: the canvas keeps the ink it has
already been given — the drawing half of a rewind belongs to the scrubber, which
steps back through the display list — and Scheme cannot be rewound at all, because
its state is an arena and a continuation chain rather than a value.

**The controls are in the Debug tab** (23.7): a **History** depth — off, 100, 500
or 2000 statements, persisted — that the next run keeps, and **Back** and
**Back 10** to walk the pause back through it. The buttons are live exactly when
they can act: not off, not at the oldest statement the run remembers, and not
Scheme, which is offered nothing rather than a control that cannot move. A
snapshot taken for a statement is not taken twice when a rewind puts the run back
on it, which is what makes "there is nothing further back" a thing the panel can
know.

**Forth joined in 23.9**, and it had the opposite problem from Logo: no pause point
at all, so a breakpoint in a `.fs` file could never fire. A token is now the unit,
the checkpoint carries both stacks, the dictionary, the compile state and the text
printed so far, and the 64K data space is copied only when a program has written
to it. **CHIP-8 followed in 24.0**: the machine already had one value for its whole
state, so the checkpoint is that value — registers, memory, timers, the frame count,
the instruction's slot within the frame and the framebuffer — taken before each
instruction and only when a history was asked for, and a rewind repaints the
restored screen so the pause shows the pixels the instruction it returned to was
drawn on. Smalltalk is the one still out: its heap is an object graph with identity,
which a rewind would have to copy without losing.

**Logo joined them (23.8), and the mechanism is the general one.** A whole-program
executor keeps its own statement loop, so the interpreter had nowhere to record a
position; the pause point those executors already called now takes the executor's
checkpoint, records it, and hands a restored one back to adopt. Logo's checkpoint
is its position, its procedures, words, lists, pen, pose and seeded generator, and
a test stops a four-statement program, rewinds two statements, and finds the
earlier statement with `A` at 1 and no output at all. Smalltalk has the same pause
point and no checkpoint yet, so it says so instead of offering the button; Scheme is
out for a reason of its own, written down.

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
because the languages disagree about what a comment even is. Find, replace and go-to-line
are `FindBar` (`native/src/ui/find_bar.cpp`), which sits under the editor tabs: the
*searching* lives on `CodeEditor` — with wrapping, an undo-able Replace All — so the
behaviour is testable without a window, and the bar is the view over it. F3 and
Shift+F3 find the next and previous match with the bar closed, and go-to-line is
Ctrl+L, because the canvas already owns Ctrl+G.

**Console** — clickable `line N` links into the editor, an error-only filter, and
save-to-file are built: `consoleLineLink` reads the three shapes the engine writes
(`[12]`, `❌ [line 30]`, `at line 30:`) and `textLineForSourceLine` turns the number
into the line the program itself numbers, the filter hides rather than deletes, and
**File > Save Output As…** writes what the console shows.

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
it.

**Projects are built.** The 🗂 **Projects** dock is a tree of the project's groups
and programs: Project > New Project (Ctrl+Shift+N) creates a `.twproject` beside
the programs, Open Project (Ctrl+Shift+O) reopens one, and the panel adds files or a
whole folder (grouped by subfolder), creates groups, moves a file between groups by drag-and-drop or its context menu,
and marks a file that has gone missing instead of silently forgetting it. A group
is a name in the project, never a directory, so reorganising a project never moves
a program on disk. The model (`native/src/project/project.cpp`) stores paths inside
the project root relatively, saves through `QSaveFile`, refuses a newer format
rather than half-reading it, and is covered by `project_tests`. The active project
and the recent list are session state.

**The REPL bar is wired.** `ReplRunner` owns one interpreter per language session
on its own thread, so `LET X = 5` followed by `PRINT X` works, and Scheme keeps its
machine so `(define x 5)` is still there on the next line. The bar clears when the
command is handed over, running a document starts a fresh session, and drawing goes
through the same canvas queue as a normal run.

**SQL is built.** `timewarp_sql` wraps one SQLite connection, and the 🗄
**Database** tab opens or creates a file, lists tables, runs one statement or a
script and shows the result grid (1000 rows, then `truncated`). A program uses the
same connection with `SQL ...`: the shared interpreter loop provides it for BASIC,
FOCAL, PILOT, COMAL, REXX and APL, Logo's command loop calls it directly,
Smalltalk exposes it as `SQL run:`, and Scheme has the `(sql "...")`
primitive. SQLite is a required dependency of the Qt
application; `docs/API.md` documents the API surface that includes the same
connection.

**Automation is built.** `time_warp_api` speaks JSON-RPC 2.0 over
newline-delimited JSON on stdin/stdout: `language.run`, `repl.*`, `debug.*`,
`sql.*`, `project.*` and `watch.evaluate`, with batches, notifications and the
standard error codes. `debug.*` starts a session, pauses at line or data
breakpoints, steps and returns the bindings, drawing and output at each stop. The launcher is `./api`. `./twc` compiles a program into a standalone
executable for every language (embedding the source, linking the engine, and
writing a drawing as SVG), and `./run.sh` builds and launches the IDE.

**Also built** — printing (File > Print Code, Ctrl+P, and File > Print Drawing),
code PDF export, session HTML and lesson-session export as Markdown or PDF, and the
accessibility controls: View > High Contrast (Ctrl+Alt+H), the A+ text-size ladder,
View > Focus Mode (F11), View > Presentation Mode (Shift+F11) and View > CRT
Settings with the four Screen Mode presets.
The snippet library is built:
Edit > Insert Snippet (Ctrl+Shift+I) reads `native/share/snippets.json`, which
ships beside the application, and every snippet in it is syntax this build runs.

**Input is built and covered.** The round trip is the prompt plus the answer that
comes back: a line-loop language asks through `Interpreter::request_input`, which
the window answers with the dialog's reply, and the bridged worker asks with
`{"t": "in"}` and is answered with `{"t": "in-reply"}`. Three tests now drive it
rather than asking a person to: `lang_tests` asserts that BASIC's `INPUT` hands the
caller the program's own prompt (`"NAME "`), that the answer lands in the variable
at its type, and that an answer nobody gave leaves a numeric variable at 0 instead
of stopping the program; `ui_layout_tests` drives `NativeRunner` end to end, so the
`inputRequested` → `answerInput` pair the dialog sits on is exercised and the value
shows up in the Variables snapshot; and `lessons_tests` runs a program through the
same verifier the Lessons panel asks, so the panel's verdict and a teacher's mark are
one function's answer. The dialog itself is still modal and is verified by hand — what is covered is
everything on either side of it.

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

**CRT effects** — the overlay (`CrtOverlay`, `native/src/ui/panels_crt.cpp`) draws
scanlines, vignette, curvature, flicker and a phosphor tint. View > CRT Scanlines
(Shift+F7) switches it, View > CRT Settings sets the five intensities, and the four
Screen Mode presets pair a theme with them.

**The function-key bar is built**, with the Python's three modifier layers, a badge
that cycles them, and a dispatcher behind every key; **the command palette** is
built too (F10, Ctrl+Shift+P). The workspace decks are live: the deck selector
offers four, and the split-workspace deck (`2:SPLIT`) toggles the second editor
pane, which holds documents of its own and is saved with the session.

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
and scales it to the page. Without the component neither entry appears.

**The turtle is inspectable.** A pose strip under the canvas shows X, Y, heading,
pen state, colour and width, and how many drawing commands the run has sent; the
🐢 Turtle Log tab lists those commands in order, numbered and spelled with their
arguments (`FORWARD 100`, `SETPENCOLOR #FF8800`, `DRAWPOLYLINE 3 points #00FF00`),
capped at the last 2000 so a long run cannot make the log cost more than the
drawing. Both are views over the canvas rather than copies of its state: the strip
is refreshed from `turtleStateChanged`, and the log is filled by the same call that
applies the command to the canvas, so the two cannot disagree about what was drawn.
The PDF of the *code* and the session HTML are built, and neither needed a new
component: `QPdfWriter` is part of Qt Gui.

---

## Distributing a program

`time_warp_ide --compile program.bas --output hello --title "Star Patrol" --author
"Ada"` writes a standalone executable: a byte-for-byte copy of **`time_warp_player`**
with the program appended as a trailer (`native/include/timewarp/ui/standalone.hpp`
documents the format). Running it opens one program and nothing else — the canvas, its
output, `R` to run again, `F11` for the whole screen, `Esc` to leave — with the name
and the author on a title card until the program draws.

The player is a real executable and not a hidden-menu IDE: it links
`timewarp::ui_core` (the canvas, the runner, the sound, the palette) rather than the
shell, and it is **2.3MB against the IDE's 4.7MB**. `--compile` copies whichever of
the two it finds beside itself, so a package that ships both produces player
artifacts and a tree with only the IDE still produces something that runs.

Format 2 is what carries the metadata: the program's title, its author, the size it
was written for, a background colour and a CRT strength, as JSON ahead of the source.
The trailer is read from the end, so a format-1 artifact written by 17.x still opens —
and `player_tests` checks that by building one by hand.

Limits, stated rather than discovered:

- The artifact links against Qt like the players do, so it needs the same Qt runtime.
  A static build is not made.
- The payload is appended, not compiled: the program is still interpreted, at the
  same speed. Nothing is optimised and nothing is hidden — `strings hello` finds the
  source, which is the right answer for a teaching tool.
- `./twc` is still the smallest alternative, for a program that does not need a
  window at all: it links the interpreter and the drawing recorder into an executable
  that writes an SVG.

---

## Packaging

Built, from a tag, by `release.yml`:

- a **`.deb`**, an **`.rpm`** and a **tarball** for x86_64 and arm64, plus an
  **AppImage** for x86_64.
- a **`.flatpak` bundle**, built from `packaging/flatpak/` against the KDE runtime,
  so it carries its own Qt.
- a **container image** on `ghcr.io`.

Every file has a checksum beside it. `cpack` produces them from the same
`install()` rules the desktop entry and icon already used, so no package can contain
a different set of files from a local install. The Qt runtime is the distribution's,
which is what the `.deb` and `.rpm` declare as a dependency; the AppImage is the one
artifact that carries its own.

This is a Linux application. The Windows installer, the Windows 7 rebuild and the
macOS disk image were 24.0's — the last cross-platform release, and now archived —
and 25.0 builds and ships for Linux only.

Still not built: publishing to the **Flathub** repository (the manifest is here and
CI builds it into a bundle; a Flathub submission is a review process rather than a
file), **Snap**, an **arm64 AppImage** (the tooling exists; the pipeline does not
exercise it yet), **automatic publishing to a distro repository**, and a **static
build** — the packages depend on the distribution's Qt 6.

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
and one row of marks per learner, read from the same files `time_warp_teach`
writes. Presentation mode (View > Presentation Mode) hides the chrome
and takes the lesson panel and console to the size and window style
`time_warp_teach present` sets, a lesson session can be exported as Markdown or PDF with
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
