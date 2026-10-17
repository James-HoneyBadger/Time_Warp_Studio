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
| Bridged engines | BASIC, PILOT, Logo, COMAL, REXX — `native/bridge/` |
| Packaging | `make install` with the desktop entry and icon; `DESTDIR` staging |
| Quality gates | 12 CTest suites in 4 build configurations, 2 parity gates, 522 engine tests, 429 command rows |

---

## Next: interpreter defects the command examination found

`native/tools/command_matrix.py` drives one row per documented command through the
real bridge (`native/bridge/time_warp/tests/command_table.py`). Examining every
command turned up defects that no gate had been looking for. Those fixed have a row
of their own so they cannot come back silently; those still open are listed here
rather than left as folklore, and where a row cannot express them the reason is
written down in `WAIVERS`.

**Still open**

1. **An unresolved name in an expression is echoed as literal text.** `PRINT` of an
   unset variable prints its own name, an unknown function prints its own call, and
   `PRINT NOSUCHFN(3)` prints `NOSUCHFN(3)` with no error at all — so a typo is
   presented as a result. Authentic BASIC would give 0 for an unset numeric variable
   and report an unknown function. In a teaching environment this is the most costly
   failure mode there is; `test_lang_basic.py` carries an `xfail(strict=True)` that
   will announce the fix.
2. **`PEEK(address)` and `IN(port)` are not usable in expressions.** The statement
   forms work and have rows, but `X = PEEK(1000)` reports `Unknown function: PEEK`;
   the handlers' own docstrings record that the evaluator integration was never done.
3. **`STICK()`/`STRIG()` do not exist.** `JOYINIT`'s docstring documents them as the
   way to read a gamepad; there is no handler for either.
4. **`DEFSTR A-Z` then `PRINT A` prints `A`.** The default string type is honoured on
   assignment (`PRINT A$` reads it back) but not in the `PRINT` lookup.
5. **`RESUME NEXT` does nothing on unnumbered lines.** The numbered form works and has
   a row; without line numbers there is no error line to resume after.
6. **`TYPE$` has no handler.** The literal only ever appears as a guard
   (`not cmd.startswith("TYPE$")`), so `PRINT TYPE$(x)` quietly prints nothing.
7. **COMAL `FUNC` cannot be called as an expression.** `PRINT DOUBLE(21)` yields 0;
   the documented path is `CALL DOUBLE(21)` and then `RESULT`, which has a row.
8. **`SPEAK` is documented but not implemented.** The executor dispatches `SAY`.

**Fixed during the examination** (each has a regression row, and the first two also a
unit test)

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

**Still open in Logo**

- **A procedure's `OUTPUT` value never reaches its caller.** `P` prints the value, but
  `PRINT DOUBLE 21` prints `DOUBLE 21` and `SHOW P` prints `P`. The call is covered by
  a row; the value is waived.

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
a Run, the debugger that is a runner without panels, the REPL whose plumbing is absent,
and the placeholder tabs. It is now the system-level description; `native/README.md`
remains the detail on the C++ libraries.

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

The bridge exists only because nine languages are not native yet. Each port
removes its path through it, and the differential gates are the contract.

1. **BASIC** — then delete BASIC from the bridge.
2. **Logo** — turtle semantics are already asserted in `turtle_canvas_tests`.
3. **PILOT**, **COMAL**, **REXX**.

BASIC's extended statement set (music, speech, shapes, particles, fractals,
gamepad) is deferred until the core language is byte-compatible: those are
language-level features reached by name, and they are what makes BASIC's port
large rather than trivial.

---

## Next: finish the IDE itself

**Debugging** — the `DebuggerRunner` is wired for Scheme and Stop works;
Debug/Continue/Trace do not yet drive a panel. Then: breakpoints, watch
expressions, a call stack, variable inspection, and step granularity finer than a
form.

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

**Turtle showpiece** — animation with a speed control, export to PNG/SVG/PDF and
copy to clipboard, a turtle-state inspector and a drawing-command log.

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
wizard, the telemetry HUD, a web (WebAssembly) build, and calling one language from
another.

The **lessons system**, with progress tracking and a classroom mode, used to be on this
list. It is now implemented — see `docs/lessons/`.

---

## How to contribute

1. Fork the repository and create a feature branch.
2. Read [CONTRIBUTING.md](../CONTRIBUTING.md) for the build and test workflow.
3. Check [open issues](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues)
   for `good first issue` labels.
4. Run `make test-all && make lint` before opening a pull request.

**Maintainer:** James Temple — [james@honey-badger.org](mailto:james@honey-badger.org)
