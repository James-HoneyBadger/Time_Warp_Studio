# Roadmap

Time Warp Studio is a C++20 / Qt6 IDE for six educational languages. This page is
the honest state of it: what works, what is next, and what is deliberately not
being built.

The PySide6 implementation that came before this one has been **removed**. Where
this page says a feature is "not ported", it existed there and does not exist here
yet — please check the list below before reporting one as a regression.

---

## Working today

| Area | State |
| --- | --- |
| Shell | Documents and tabs, menus, toolbars, status bar, settings persistence |
| Editor | Line-number gutter, current-line highlight, tab stops, fonts |
| Console | Streaming output, and interpreter errors in their own tab |
| Canvas | Live drawing, zoom, pan, grid, auto-fit, turtle marker |
| Execution | Run (F9) and Stop; one program at a time; unsaved-change prompts |
| Languages | 6 languages, 44 verified example programs |
| Native engines | Expression evaluator (`timewarp_expr`) and Scheme (`timewarp_scheme`) |
| Bridged engines | BASIC, PILOT, Logo, COMAL, REXX — `native/bridge/` |
| Packaging | `make install` with the desktop entry and icon; `DESTDIR` staging |
| Quality gates | 11 CTest suites in 4 build configurations, 2 parity gates, 269 engine tests, 279 command rows |

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
and is harder to notice. Two claims have been corrected: the editor was described as
having per-language syntax highlighting (there is no `QSyntaxHighlighter` anywhere),
and BASIC was described as having a `SPEAK` command (the executor dispatches `SAY`;
`SPEAK` reports "Unknown BASIC command").

`docs/ARCHITECTURE.md` is still largely a PySide6-era document — its UI architecture,
signal/slot, threading, theme-manager and module-inventory sections describe the
implementation that was removed. It now carries a prominent accuracy note naming what
is stale, but the page needs rewriting around the Qt application and the engine. Until
it is, treat `native/README.md` as the description of the shipping product.

---

## Next: port the languages, then delete the bridge

The bridge exists only because five languages are not native yet. Each port
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

**Editor** — per-language syntax highlighting is **not** implemented: there is no
`QSyntaxHighlighter` subclass anywhere, and the gutter and current-line highlight are
not colouring. Also missing: find/replace, find in files, go-to-line, minimap, error
markers in the gutter, breakpoint markers, auto-completion, document outline.

**Console** — clickable `line N` links into the editor, an error-only filter, and
save-to-file.

**Files and sessions** — recent files, restore the last session on launch, crash
recovery, autosave.

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
release cycle: the lessons system with progress tracking, classroom/presentation
mode, the examples browser, the accessibility presets, the onboarding wizard, the
telemetry HUD, a web (WebAssembly) build, and calling one language from another.

---

## How to contribute

1. Fork the repository and create a feature branch.
2. Read [CONTRIBUTING.md](../CONTRIBUTING.md) for the build and test workflow.
3. Check [open issues](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues)
   for `good first issue` labels.
4. Run `make test-all && make lint` before opening a pull request.

**Maintainer:** James Temple — [james@honey-badger.org](mailto:james@honey-badger.org)
