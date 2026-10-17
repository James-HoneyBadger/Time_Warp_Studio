# Roadmap

Time Warp Studio is a C++20 / Qt6 IDE for ten educational languages. This page is
the honest state of it: what works, what is next, and what is deliberately not
being built.

The PySide6 implementation that came before this one has been **removed**. Where
this page says a feature is "not ported", it existed there and does not exist here
yet — please check the list below before reporting one as a regression.

---

## Working today

| Area | State |
| --- | --- |
| Shell | Documents and tabs, menus, toolbars, status bar, settings persistence, the last session's files restored at launch |
| Editor | Line-number gutter, current-line highlight, per-language syntax colouring, find/replace, go-to-line, tab stops, fonts |
| Console | Streaming output, and interpreter errors in their own tab |
| Canvas | Live drawing, zoom, pan, grid, auto-fit, turtle marker |
| Execution | Run (F9) and Stop; one program at a time; unsaved-change prompts |
| Languages | 10 languages, 48 verified example programs |
| Native engines | Expression evaluator (`timewarp_expr`) and Scheme (`timewarp_scheme`) |
| Bridged engines | BASIC, REXX and Smalltalk — `native/bridge/` |
| Native executors | FOCAL, PILOT, COMAL, CHIP-8, Logo and APL — `timewarp_lang`, in process |
| Packaging | `make install` with the desktop entry and icon; `DESTDIR` staging |
| Quality gates | 13 CTest suites in 4 build configurations, 3 parity gates, 551 engine tests, 437 command rows, 273 parity programs |

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

## Next: documentation accuracy

Documentation that overstates the product costs a newcomer the same time a bug does,
and is harder to notice. Claims have been corrected rather than quietly dropped: BASIC
was described as having a `SPEAK` command (the executor dispatches `SAY`; `SPEAK`
reports "Unknown BASIC command"), and the README described the shared expression
evaluator as running in-process when `timewarp_expr` is not linked into the application
at all — nothing under `src/ui/` includes it and `timewarp_ui` does not link it, so
expressions in a Run are still the Python engine's.

The editor's missing syntax highlighting was the third, and it has been **built rather
than re-documented**: `native/src/ui/syntax_highlighter.cpp` now colours each language
with its own rules, and `syntax_highlighter_tests` asserts the colour at named
positions.

`docs/ARCHITECTURE.md` has been **rewritten** around the Qt application and the engine.
It no longer describes the removed PySide6 implementation, and it names what is not
wired up rather than assuming it: the expression evaluator that does not participate in
a Run, the REPL whose plumbing is absent, the outline and terminal pages that are still
placeholders, and the debugger's missing breadth. It is now the system-level
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

## Next: port the languages, then delete the bridge

The bridge exists only because three languages are not native yet. Each port
removes its path through it, and the differential gates are the contract.

**APL is done.** It was the smallest and the only one with no dependency on the
interpreter: arrays, glyphs and keyword spellings, reduce and scan, the turtle
words and `∇` definitions all live in `native/src/lang/apl.cpp`, gated by the
same 46 rows and example the reference is. That is the shape each remaining port
takes.

1. **BASIC** — the largest remaining port: the core language first, then the
   extended statement set.
2. **REXX** — strings, subroutines and the `PARSE` family.
3. **Smalltalk** — whole-program, like Scheme and CHIP-8.

Once the last of them lands, `native/bridge/` and the `python_worker` target are
deleted together; nothing else may depend on them.

BASIC's extended statement set (music, speech, shapes, particles, fractals,
gamepad) is deferred until the core language is byte-compatible: those are
language-level features reached by name, and they are what makes BASIC's port
large rather than trivial.

---

## Next: finish the IDE itself

**Debugging** — `DebuggerRunner` is wired for Scheme, and the **Debug** tab is a real
panel driving it: Continue, Pause, Step In/Over/Out, Stop and breakpoints all reach the
runner, which reports the line, the procedure, the call stack and the frame's bindings.
What is still missing is *breadth and depth*: only Scheme can be stepped (the four
bridged languages run to completion in another process, and the native executors have
no pause protocol, so the panel says so rather than offering dead buttons), the
toolbar's Debug/Continue/Trace actions do not drive the
panel yet, and there are no watch expressions and no step granularity finer than a form.

**Variables and the Telemetry HUD** are built too, and both answer the question a
placeholder could not: *what did that run actually do?* The **Variables** tab lists every
binding the engine can see, with the type it knows it by, filtered by name. The
**Telemetry HUD** shows labelled counters — steps used, engine time, steps per second,
last line, recursion depth, call stack, output lines, drawing commands, and BASIC's
memory window — and puts the IDE's own rows (wall clock, ink on the canvas) beside them,
so a drawing the engine counted but the canvas never received reads as two rows that
disagree rather than as silence.

**Editor** — find in files, a minimap, error markers in the gutter, breakpoint
markers, auto-completion and a document outline are still missing.

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
a Clear Menu entry). Crash recovery and autosave are still missing — the window
prompts on close, but a process that dies does not.

**Input** — the bridged languages' input round-trip (the `in`/`in-reply` protocol
and the prompt dialog) is implemented but has no automated coverage, because the
prompt is modal. It needs a manual pass with a PILOT or BASIC program.

---

## Next: the retro workstation feel

**Themes** — the removed implementation had 33 (including Amber, Green, C64 and
Apple II). They are to be carried as data and applied through QSS plus a palette
in `native/src/ui/ui_style.cpp`. **No theme is ported yet**; treat the styling as
provisional until this lands.

**Then** — CRT effects (scanlines, curvature, vignette, glow, flicker), a
function-key bar with its Shift/Ctrl layers, screen modes, focus mode, and a
command palette.

**Turtle showpiece** — exporting the drawing and copying it to the clipboard are
built: File > Export Drawing (or **Ctrl+E**), Edit > Copy Drawing, and
`time_warp_ide --run --export drawing.png` for a script. What is exported is the
*drawing* — the same `paintInk` the window paints with, without the grid or the
turtle marker, twice the on-screen size — so a saved picture is a picture rather than
a screenshot.

Still missing here: animation with a speed control (the toolbar slider sets the
bridge's speed, nothing animates the display list), **SVG and PDF** export, which need
the QtSvg and QtPrintSupport components the build does not currently ask for, a
turtle-state inspector and a drawing-command log.

---

## Packaging

Today: `make -C native install`, a desktop entry, an icon, and `uninstall`.

Not built: `.deb`/`.rpm`, Flatpak or Snap, AppImage, a Windows installer, a macOS
bundle, and any publishing to a package registry. The previous packaging built the
removed Python application and was deleted with it, and the desktop entry and icon
are now in place for a real packaging effort to build on.

---

## Deliberately out of scope for now

These existed in the removed implementation and are not planned for the current
release cycle: the examples browser, the accessibility presets, the onboarding
wizard, a web (WebAssembly) build, and calling one language from another.

The **lessons system**, with progress tracking and a classroom mode, used to be on this
list. It is now implemented — see `docs/lessons/`. The **telemetry HUD** was on it too
and is now the Telemetry tab described above.

---

## How to contribute

1. Fork the repository and create a feature branch.
2. Read [CONTRIBUTING.md](../CONTRIBUTING.md) for the build and test workflow.
3. Check [open issues](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues)
   for `good first issue` labels.
4. Run `make test-all && make lint` before opening a pull request.

**Maintainer:** James Temple — [james@honey-badger.org](mailto:james@honey-badger.org)
