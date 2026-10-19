# Porting plan: what the PySide6 application had and this one does not

The PySide6 application was removed at commit `c30e76a` — 36 modules under
`Platforms/Python/time_warp/ui/`, 15 under `features/`, about 17.5k lines of UI.
This is the plan for the features it had that the Qt application does not, in the
order they are worth building.

It is a **plan**, not a promise: every phase names a test and a gate, and a phase
that cannot meet its own test does not ship. `docs/ROADMAP.md` records what is
actually built; this records what is intended and why.

---

## What this plan is not

Eleven modules in the old tree were **never imported by anything** — no menu
entry, no dock, no timer. They are not regressions and porting them would be
building new features, so they are out of scope here:
`project_tree.py`, `cics_panel.py`, `dbms_window.py`, `sql_panel.py`,
`terminal_3278.py`, `error_explorer.py`, `feature_panels.py`,
`feature_integration.py`, `phase_vii_x_panels.py`, `boot_screen.py`,
`collaboration_client.py`.

Also out of scope: the old application's Python-specific machinery (`REPLThread`,
`InterpreterThread`, `feature_integration`) — the C++ runners replaced it, and
nothing about that layer is missing a user-facing behaviour.

---

## Ground rules

1. **A feature arrives with its test.** Every phase below names the suite and, in
   the "done when" line, the assertion that proves it. A port without one is a
   port nobody can tell from a regression.
2. **No control ships before it works.** The repository's rule holds: a menu entry
   is added when it does something, and until then the action id reports in the
   status bar. `performAction` already reserves `split_editor` and
   `find_in_files` this way — the phases below *replace* those branches.
3. **The old file is the specification, not the standard.** Three old behaviours
   were wrong and are corrected here rather than copied: the polyglot shared
   variable bus (a toy that made two programs interfere), the automatic update
   poll (a network request nobody asked for), and the formatter's cursor restore
   (it clamped to the document, not the line).
4. **No new dependency without a decision.** Four candidates need one; they are in
   the table at the end, each with a recommendation, and no phase depends on one
   whose row has not been decided.
5. **Settings and data follow the existing conventions.** Preferences go to
   `QSettings` with the `editor/…` style keys; user data goes under the data
   directory that `TIME_WARP_DATA_DIR` moves, so a test never writes a learner's
   real files.
6. **Docs land in the same commit.** The ROADMAP state table, `ARCHITECTURE.md`
   where a subsystem is described, and a CHANGELOG entry for anything a user can
   see. `make lint-md` clean, `python3 Scripts/version_sync.py` clean.

---

## Sequence

| # | Phase | Features | Size | Depends on |
| --- | --- | --- | --- | --- |
| 0 | Foundations | data dir, panel registration, test fixtures | S | — |
| 1 | Editor completion | Find in Files ✅, Format Code ✅, Snippets ✅, whitespace ✅, minimap ✅, run history ✅, run selection ✅, reset ✅ | M | 0 |
| 2 | Data safety | autosave ✅, version history ✅ | M | 0 |
| 3 | Help and learnability | documentation browser ✅, F1 ✅, keyword inspector ✅, guided tour ✅ | M | 0 |
| 4 | Canvas time travel | scrubber ✅, GIF recording, SVG export | M | 4.3 needs a dependency decision |
| 5 | Export and print | PDF, print, session HTML | M | 5.2 needs a dependency decision |
| 6 | Debugging breadth | breakpoints everywhere, pause/step for line-by-line languages, profiler | L | 0, best after 1 |
| 7 | Workspace | document outline, terminal, split editor, run both panes | M | 7.3 needs 1.5 |
| 8 | Accessibility and display | high contrast, font scale, focus mode, CRT settings | S–M | 0 |
| 9 | Classroom | roster panel, presentation mode, lesson session export, bundles | M | 9.4 needs an archive decision |

Size: **S** ≈ a session, **M** ≈ two or three, **L** ≈ a week of part-time work.

Phases 1, 2, 3 and 8 are independent and can land in any order. Phase 6 is the
long pole and the one with the most classroom value; start it early if the
schedule matters more than the quick wins.

---

## Phase 0 — Foundations

**Goal:** the four things every later phase needs, with no behaviour change.

- `native/include/timewarp/ui/app_paths.hpp`, `src/ui/app_paths.cpp`:
  `QString dataDir()` — `TIME_WARP_DATA_DIR`, else
  `QStandardPaths::AppDataLocation`, created on demand. The bridge already honours
  that variable (`core/config.py`); the C++ side does not, and autosave, history
  and bundles all need it. A test sets the variable to a `QTemporaryDir` and
  asserts the helper follows it.
- **Panel registration in one place.** `MainWindow::showPanel` holds `kPanels`;
  `buildRightColumn` builds the pages. A new panel needs both, and the plan's
  phases add four. Add a small `PanelId` enum in `main_window.hpp` with a
  `constexpr` table beside the existing one, so a missing mapping is a compile
  error rather than a panel that cannot be raised.
- **Test fixtures.** `native/tests/temp_dir.hpp`: a `QTemporaryDir` wrapper that
  fails the check with the path, plus `writeFile(path, text)`. Every phase that
  touches disk uses it instead of ad-hoc temp files.
- **This document** linked from `docs/INDEX.md` and `docs/ROADMAP.md`.

**Done when:** `ctest --test-dir native/build` still green, `app_paths` has a
test, and `make lint-md` is clean.

---

## Phase 1 — Editor completion

The largest cluster of small, visible gaps. All of it is `native/src/ui/`.

### 1.1 Find in Files (M)

Old: `ui/find_in_files.py` (610 lines) with a worker thread and a results tree.

- New `find_in_files.{hpp,cpp}`: a `QDialog` with a term field, options (case,
  whole word, regular expression, include glob), a root taken from the current
  document's folder, and a results tree grouped by file.
- Scanning runs on a worker `QThread` and posts batches back by queued signal —
  the pattern `PythonWorker` already uses, and the same trap applies: the
  connection context must be the dialog, so a result cannot outlive it.
- Skip files by size and by a NUL byte in the first block; cap the result count
  and say so rather than truncating silently.
- Double-click a row: `openFile(path)` then `goToLine(line)` — add
  `MainWindow::goToLine(int)` beside `goToLinePrompt()`, which currently can only
  prompt.
- Replace the reserved `find_in_files` branch in `performAction` and add
  File → Find in Files (Ctrl+Shift+F). The command palette picks it up for free
  because it reads the menu bar.

**Tests** (`find_in_files_tests`, new suite): hits and ordering in a temp tree;
case/whole-word/regex options; a binary file is skipped; an invalid pattern
reports an error and searches nothing; the cap is honoured; opening a hit lands
the cursor on the right line.

### 1.2 Format Code (S)

Old: `utils/code_formatter.py` — rule tables, not a parser: indent on `FOR`,
`WHILE`, `SUB`, `FUNCTION`, `IF`, `SELECT`, `CASE`; dedent on `NEXT`, `WEND`,
`LOOP`, `END …`, `ELSE`; `ELSE`/`ELSEIF`/`CASE` do both.

- New `formatter.{hpp,cpp}` with one table per language and a
  `FormatResult{text, message}`. Re-indent only; do not re-case keywords (the old
  formatter upper-cased BASIC, which mangles string literals).
- Preserve the cursor by **line and column within the line**, not by absolute
  offset — the old code clamped to the document length and moved the caret to the
  end of the file whenever indentation shortened the text.
- Wire Edit → Format Code (Ctrl+Shift+F is taken by 1.1; use Ctrl+I), status bar
  reports "formatted N lines" or "already formatted".

**Tests** (`formatter_tests`): one case per language with before/after text, an
already-formatted document is unchanged, a string containing an indent keyword is
not treated as a block, and the cursor round-trips.

### 1.3 Snippets (M)

Old: `ui/snippets.py` (641), `snippet_dialog.py` (230), and `features/snippets.json` —
64 lines keyed by language, each a list of `{name, code}` for BASIC, Logo, Pascal,
Prolog, Forth and C.

- **The catalogue has to be recovered and rewritten.** It is not in
  `native/bridge/` (that directory is the reference *engine*; snippets are an IDE
  concern), so `git show c30e76a:Platforms/Python/time_warp/features/snippets.json`
  is the source, and three of its six languages no longer exist. Keep the BASIC and
  Logo entries, write the other eight languages, and ship the result as a new file
  — `native/share/snippets.json`, installed to
  `share/time-warp-studio/snippets.json` so a package has it, with a resolver
  shaped like `exampleRoot()` (`TIME_WARP_SNIPPETS` → installed → checkout).
- New `snippets.{hpp,cpp}`: `Snippet{id, name, language, description, body}`,
  loaded once with `QJsonDocument` and validated — a snippet whose language this
  build does not know is dropped and reported, not silently kept.
- New `snippet_dialog.{hpp,cpp}`: filter by name and by the current language,
  preview pane, insert at the cursor. `$CURSOR$` in a body marks where the caret
  goes; `$SELECTION$` is replaced by the selection.
- Enable the toolbar button that is currently `notPorted`.

**Tests** (`snippets_tests`): the shipped file parses; every entry's language maps
to a `Language` and every language has at least two snippets; insertion replaces
the selection; the caret lands on `$CURSOR$`; an unknown placeholder is left as
written rather than swallowed; the resolver finds the installed and checkout
layouts.

### 1.4 Whitespace rendering and minimap (M)

Old: `MinimapWidget` and `_WhitespaceHighlighter` in `editor.py`.

- `CodeEditor::setShowWhitespace(bool)` — paint spaces as mid-dots and tabs as
  arrows in `paintEvent`, honouring the current theme's dim colour.
- `Minimap` as a narrow sibling widget in `code_editor.{hpp,cpp}`: paint one row
  per source line from a cached line-length/sample, a viewport rectangle, and a
  click-to-jump. It must not repaint the full document on every keystroke: sample
  at most `height()` lines.
- Preferences gains both toggles, default off for whitespace and on for the
  minimap.

**Tests** (`ui_layout_tests` additions): the toggles set paint flags and the
minimap has a fixed width; a click maps to a line; toggling does not change the
document text.

### 1.5 Run Selection, Reset Program, Run History (M)

Old: `run_selection`, `_reset_program`, run history in a dropdown, persisted.

- `NativeRunner` and `DebuggerRunner` take an optional source override, so a
  selection runs without being written to a file. The Run button's popup menu
  gains "Run Selection" and "Run from Start" (reset), both currently absent.
- Run history: the last ten runs as `QSettings` entries (`run/history`), each with
  the source, language and timestamp. The Run popup lists them, newest first,
  restores the source into a new tab on click, and has "Clear History".
- A reset clears the canvas and the interpreter state without touching the
  document — the old one reloaded the program from the editor, which is the same
  thing plus a lost cursor.

**Tests** (`ui_document_tests` additions): a selection run executes only the
selection and prints only its output; reset clears the canvas but keeps the text;
history round-trips through a fresh `MainWindow` and is capped at ten.

### 1.6 Errors-only filter — already served, so no port (decision)

The old `_toggle_error_filter` filtered **one** output panel that held the prints
and the failure together. This application split them: program output goes to the
Console, interpreter errors to the Errors tab, and `reportError` raises that tab
when there is one. A console-only filter would therefore show *nothing* for a
program that printed and then failed — strictly worse than the split.

The action `filter_errors` already raises the Errors tab, which is the behaviour
the old toggle was trying to achieve. Recorded here rather than implemented:
porting the widget-level filter would be copying a design this application
already improved on.

**Phase done when:** all six land, `ui_layout_tests` and the two new suites are
green, and no `notPorted` button remains in `buildToolBar`.

---

## Phase 2 — Data safety

### 2.1 Autosave (M)

Old: `features/autosave_manager.py` (188) — a copy every 120 s for modified
documents, plus a manifest.

- New `autosave.{hpp,cpp}`: a `QTimer` driven by `Preferences` settings
  (`editor/autosave_enabled`, `editor/autosave_seconds`, matching the old keys),
  writing to `<dataDir>/autosave/<sha1 of path>.bak` plus `manifest.json` with
  path, timestamp and byte count. An untitled document is autosaved under its
  session id, because a lost unsaved buffer is exactly the case this exists for.
- On startup, a manifest entry newer than its file (or whose file is gone) offers
  recovery in the status bar and a dialog; never a modal on the startup path — the
  repository has that rule for a reason.
- Prune: keep the newest five per path.

**Tests** (`autosave_tests`): a modified document is autosaved and an unmodified
one is not; recovery restores the text; a newer file on disk suppresses the
offer; pruning keeps five; the directory follows `TIME_WARP_DATA_DIR`.

### 2.2 Version history (S)

Old: `ui/version_history_dialog.py` (203) over the same store.

- New `version_history.cpp` dialog: versions for the current path, timestamp and
  size, preview, "Open in a new tab" — never overwrite the file, so a wrong choice
  costs a tab rather than work.

**Tests:** the list is ordered and complete, and restoring opens a new document
with the stored text.

---

## Phase 3 — Help and learnability

### 3.1 Documentation browser (M)

Old: `mixins/help_mixin.py` — Documentation Index, Getting Started, IDE Basics,
FAQ, and a guide per language, all read from `docs/`.

- New `help_browser.{hpp,cpp}`: a `QDialog` with a `QTextBrowser`, a contents tree
  built from `docs/INDEX.md`'s own table rows (so the browser cannot promise a
  document the index does not), and a docs-root resolver:
  `TIME_WARP_DOCS` → installed `share/doc/time-warp-studio` → the checkout's
  `docs/`.
- **Ship the docs.** Add an install rule for `docs/` (and the two top-level
  READMEs already packaged) so a `.deb` has the same help as a build tree. This is
  a packaging change with a check: the release workflow already lists the staged
  tree, so a missing `share/doc/time-warp-studio/INDEX.md` is visible there.
- Help menu gains "Documentation" and "Getting Started"; "About" stays.

One deliberate limit: the tree offers only documents inside the installed tree.
The index also links to the repository's README, the issue forms and the example
catalogue, and those are not installed beside the docs; showing those rows in a
package would offer dead links. The in-page references to `Examples/` and the
native sources are likewise a checkout convenience.

**Tests** (`help_browser_tests`): the resolver finds both layouts (env override
for the test); every relative link in the built contents tree resolves to a file
that exists — the dead-link check, run without a browser; a missing docs root
shows a message rather than an empty pane.

### 3.2 F1 context help and the keyword inspector (S)

Old: F1 for context-sensitive help, Ctrl+F1 for the inspector
(`main_window.py:2212`, `_show_keyword_inspector`).

- F1 opens the current language's guide from 3.1, scrolled to the command under
  the cursor (`CodeEditor::wordAtCursor()`).
- Ctrl+F1 opens a filterable list of the language's keywords with the guide's
  one-line description.
- The keyword table is the **same list** `syntax_highlighter` colours. A test
  asserts set equality, so a keyword added to the highlighter without a
  description fails the suite rather than vanishing from the inspector.

**Tests:** F1 on a Logo document opens `logo.md`; every highlighted keyword has an
inspector row; a word that is not a keyword opens the guide's index.

### 3.3 Guided tour and coach marks (M)

Old: `ui/onboarding.py` (389), `ui/coach_marks.py` (335).

- New `tour.{hpp,cpp}`: a first-run overlay with four steps — run this program,
  look at the canvas, open the lessons, find the palette — each anchored to a
  widget by object name (the object names are already a contract). Persist
  "seen" in `QSettings`; a Preferences toggle re-arms it.
- Coach marks reuse the same anchor mechanism for one-time hints (the lessons
  dock, the run history), capped and dismissible.

**Tests:** the anchor list resolves every object name it names (a rename breaks
the test, not the tour); "seen" is persisted; a second window does not re-show it.

---

## Phase 4 — Canvas time travel

### 4.1 The scrubber (M)

Old: `canvas.py:168` — a slider that replays the drawing after a run
(`scrub_to_frame`, 548).

- `TurtleCanvas` already owns the display list. Add `history()` (the segments and
  shapes in draw order), `setVisibleFrame(int)` and `visibleFrameCount()`.
- New `scrubber.cpp`: a play/pause button and a slider under the canvas, shown
  only when a run produced ink. Playback is a `QTimer` at a fixed 60 fps advancing
  N frames, with N chosen so a long drawing still finishes in ~10 s.
- Export, `fitToContent` and the ink count in the telemetry row always use the
  **full** list; scrubbing is a view, not a state change.
- This is what the removed speed slider should have been: it controls something
  the application can actually do.

**Tests** (`turtle_canvas_tests`): drawing 100 segments and scrubbing to 40 paints
40; playback reaches the full count; export after scrubbing produces the same PNG
hash as export without it.

**Done.** `TurtleCanvas` now keeps a `history()` of typed entries alongside the two
primitive lists, and `paintInk(painter, frames)` walks it in draw order — the old
paint loop drew every stroke and then every shape, which put a rectangle painted
between two lines underneath both. `scrubber.{hpp,cpp}` is the strip, mounted with
the canvas in one pane under the output splitter and hidden until `frameCount()`
is positive. `setVisibleFrame` is deliberately silent (no `contentChanged`), which
is what keeps a scrub from being mistaken for new ink. Tests cover the prefix
count, both ink counters, the export invariant by comparing rendered images, the
interleaved order, clamping, and playback from the end.

### 4.2 GIF recording (S–M, needs a decision)

Old: `_start_gif_recording`, `_capture_record_frame`, `_save_gif`.

Qt6 writes single-frame GIF only, so this needs either a dependency (giflib) or an
in-tree encoder. **Recommendation:** a self-contained `gif_encoder.{hpp,cpp}` — a
GIF89a header, a global palette quantised to 6×6×6, LZW compression — about 250
lines with no dependency, tested against a known 8×8 frame's byte length and a
decoded round trip through `QImageReader` (Qt reads GIF natively, which gives the
test its oracle).

### 4.3 SVG export (S, needs a dependency decision)

`QSvgGenerator` is in Qt6::Svg, a separate package (`libqt6svg6`). If the
dependency is accepted, add an optional component to `CMakeLists.txt`, hide the
menu entry when `Qt6::Svg_FOUND` is false, and add the package to the CPack
dependency lists. Otherwise record SVG export as not ported and stop promising it.

---

## Phase 5 — Export and print

### 5.1 PDF of the code (S)

`QPdfWriter` is in Qt Gui — **no new dependency**. A4, monospace, line numbers,
the current document's title and the language in the header. Add File → Export
Code as PDF.

**Tests:** the file starts with `%PDF`, has more than one page for a 200-line
document, and is byte-identical across two runs (no timestamps in the content).

### 5.2 Print Code and Print Graphics (S, needs PrintSupport)

Dialogs and printers are Qt6::PrintSupport (`libqt6printsupport6`). Optional
component, hidden without it, dependency added to the packages if taken.

### 5.3 Session as HTML (S)

`QTextDocument::toHtml()` for the text, the canvas PNG embedded as a base64 data
URI, the console output in a `<pre>`. One self-contained file a teacher can put on
a projector.

**Tests:** the output parses as HTML (a token check, not a parser), embeds one
`data:image/png;base64,`, and contains the program text escaped once.

---

## Phase 6 — Debugging breadth (the long pole)

Today a Scheme run can pause because `DebuggerRunner` drives an explicit-stack
machine. The other nine languages run in `NativeRunner` with no pause protocol at
all, and the Debug/Continue/Trace buttons are disabled for that reason.

### 6.1 Breakpoints everywhere (M)

- Per-document breakpoint set on `CodeEditor` (a `QSet<int>` of zero-based lines)
  painted in the gutter, toggled by clicking it or F5, and honoured by whichever
  runner executes the document.
- Persistence: `QSettings` under `breakpoints/<absolute path>` as a comma-separated
  line list; prune entries for files that no longer exist at load. This is the
  semantics of the old `encode_breakpoints`/`decode_breakpoints` pair, without its
  JSON blob.

**Tests** (`debug_tests`): a breakpoint survives a new `MainWindow`; a stale path
is dropped; toggling the gutter sets exactly one line; the marker paints.

### 6.2 Pause and step for line-by-line languages (L)

The six line-by-line executors (BASIC, FOCAL, PILOT, COMAL, APL, REXX) share
`Interpreter`, which already calls `note_step()` per statement. The work is:

- A `DebugSession` in `native/include/timewarp/lang/debug.hpp`: an atomic
  `step` / `continue` / `stop` command word, a breakpoint set, and the current
  line. C++20 `atomic::wait` re-checks its value, so a command must bump a
  sequence counter before notifying — the trap already recorded in this
  repository's notes.
- `Interpreter::note_step()` consults it: at a breakpoint, or in step mode, it
  blocks on the condition variable until the UI sends a command, publishing the
  pause through the runner's existing `paused` signal.
- `NativeRunner` gains `requestPause/requestStepOver/requestStepOut/requestContinue`
  forwarding to the session; `DebugPanel` stops caring which runner is active and
  the three toolbar buttons become live for every language.

**Tests** (`debug_tests` + `lang_tests`): a BASIC program with a breakpoint on
line 3 stops before executing line 3; step-over advances exactly one statement;
continue runs to the next breakpoint or the end; stop unwinds and the runner
reports a clean stop, not a crash.

### 6.3 Whole-program languages (L, later)

CHIP-8 pauses between frames, Smalltalk between message sends, Logo between
statements (its loader already streams them). Each is a separate small change once
6.2's session exists; none of them needs the machine rewritten.

### 6.4 Profiler (M)

Old: `utils/profiler.py` plus a toggle and a report dialog ("N lines executed").

- `Interpreter` records per-line execution counts and elapsed time when a profiling
  flag is set (the same hook 6.2 uses).
- New report dialog: lines sorted by time, with the source line beside each. A
  checkable View → Profile Run, off by default because the timing costs.

**Tests:** a loop of ten iterations reports ten hits on its body line; profiling
off changes nothing in the telemetry; the report is sorted.

---

## Phase 7 — Workspace

### 7.1 Document outline (M)

Old: `ui/document_outline.py` (338) — Logo procedures, BASIC subroutines and
labels, click to jump.

- New `outline.{hpp,cpp}`: a model built from the document text (Logo `TO … END`,
  BASIC `SUB`/`FUNCTION`/`LABEL:`/numbered lines, COMAL `PROC`, REXX `LABEL:`),
  refreshed on the same 350 ms debounce the linter uses.
- New `OutlinePanel` replacing the `PlaceholderPanel` in `buildDocks`, backed by
  the same model, with click-to-jump through `goToLine`.
- The current address space has an `outline` action that only toggles the dock; it
  keeps working.

**Tests** (`ui_layout_tests` + `outline_tests`): each language's fixture yields the
expected names and lines; an edit updates the model after the debounce; clicking
an item puts the cursor on its line; a document with no structure shows an empty
state rather than a blank box.

### 7.2 Terminal (M, needs a decision)

Old: `ui/terminal_widget.py` (277) — an embedded shell.

An embedded shell in a teaching IDE is a support and safety cost: it runs anything,
in a classroom it is a way around the sandbox, and it needs a policy for the
demo/presentation mode. **Recommendation:** build it (a `QProcess` on `$SHELL`,
cwd = the document's folder) but default it **off**, behind a Preferences toggle,
disabled entirely in classroom and player modes, and label the tab with what it
does. If that policy is rejected, delete the placeholder tab and record the
decision — a placeholder that says "not yet ported" forever is worse than an
absence.

**Tests:** with the setting on, `echo hi` prints `hi`; the process is killed on
window close; with the setting off the tab is absent.

### 7.3 Split editor actions (S)

`editor_tabs_secondary_` exists but nothing can open it. Add File → Split Editor
Right, Close Split, and moving the current tab between panes, replacing the
reserved `split_editor` branch. Persist the split state with the layout.

**Tests** (`ui_layout_tests`): split makes the second pane visible with the
document in it; close merges the documents back; the state survives a restart.

### 7.4 Run Both Panes (M)

Old: `run_dual_split_benchmark` — ran both panes and compared them, optionally
sharing a variable bus between the two languages.

- Run both documents, collect output, elapsed time and ink count, and show a
  comparison table in the output area.
- **Drop the shared variable bus.** Two programs writing each other's variables is
  a demonstration of a mechanism, not a measurement, and it makes the benchmark
  unreproducible. If a polyglot exercise is wanted later, it should be a lesson,
  not a menu action.

**Tests:** both programs run, their outputs are attributed correctly, a failing
one does not stop the other, and the table reports real elapsed times.

---

## Phase 8 — Accessibility and display

### 8.1 High contrast (S)

`AccessibilityFeatures.toggle_high_contrast` (Ctrl+Alt+H). Apply a maximum-contrast
palette derived from the active theme (foreground and background forced to black
and white, accents kept for syntax), and restore the theme's palette on toggle.
The A+ toolbar button stops being disabled (see 8.2).

**Tests:** contrast ratio of text against background is at least 7:1 in both dark
and light themes; toggling twice returns the original palette exactly.

### 8.2 Font scale presets (S)

A+ cycles 100/125/150%; applies to the editor, console and panels, and persists.
`LessonPanel::setPresentationFont` already exists and takes the same value, so the
panel does not need a second mechanism.

**Tests:** each preset sets the expected point size on all three surfaces; it
survives a restart.

### 8.3 Focus mode (S)

Old: `focus_mode.py` (136). Hides docks, toolbars and the status bar; Esc leaves it;
state is per-session, not persisted.

**Tests:** entering hides the named widgets and leaving restores exactly those that
were visible; Esc is bound while the mode is on and unbound after.

### 8.4 CRT settings (S)

`CrtOverlay` already has the setters (`setScanlineIntensity`, and the vignette,
curvature, flicker and phosphor effects are in its painter). Wire a dialog to them
with live preview and persist the values; the overlay currently reads a fixed set.

**Tests:** each slider reaches the overlay; values round-trip through `QSettings`;
zero intensity paints no overlay.

### 8.5 Screen-mode presets (S)

Map the old Monitor styles (amber, green, white phosphor, normal) onto an existing
theme plus CRT settings in one action, rather than a parallel display system.

**Tests:** each preset selects the expected theme and overlay values.

---

## Phase 9 — Classroom UI

The engine already exists in `native/bridge/time_warp/features/classroom_mode.py`
and is reachable from `teach.py`. This phase is the panel that uses it.

### 9.1 Classroom panel (M)

- New `ClassroomPanel` in `panels.cpp`: roster list, per-student progress from the
  lesson store, "open student folder".
- Storage stays where it is — the bridge's `classroom_mode` under the data
  directory. The panel talks to it through the worker protocol with two new
  request types (`classroom-roster`, `classroom-mark`) rather than a second C++
  copy of the roster format. That keeps `teach.py` and the panel reading one file.

**Tests** (`classroom_panel_tests`): a roster written by the Python engine is
listed; progress matches the lesson store; a missing roster shows an empty state;
the panel never writes a file the engine cannot read back (round-trip through the
engine's own loader).

### 9.2 Presentation mode (S)

Full screen, large font (reuse `setPresentationFont`), chrome hidden. Distinct from
8.3 focus mode in one respect: it is for an audience, so it also scales the lesson
panel and the console.

### 9.3 Lesson session export (S)

Markdown (no dependency) and PDF (5.1's writer). One file per lesson session: the
lesson, the checkpoints, the learner's code, the verdicts.

### 9.4 Classroom bundles (M, needs an archive decision)

Qt has no public ZIP API. Options: a `.tar.gz` produced by invoking `tar` through
`QProcess` (present everywhere, easy to test round-trip), a JSON bundle with the
files inline (no compression, no dependency, gets large), or a dependency
(libarchive). **Recommendation:** tar.gz via `QProcess`, tested by a round trip
with the real `tar`, and documented as such.

---

## Dependency decisions

| Candidate | Needed by | Recommendation |
| --- | --- | --- |
| Qt6::Svg (`libqt6svg6`) | 4.3 SVG export | Optional component, entry hidden without it. It is a small package and the CPack dependency lists are already maintained by hand. |
| Qt6::PrintSupport (`libqt6printsupport6`) | 5.2 printing | Optional component. Printing is the least-used item here; hide it when absent rather than take the dependency for everyone. |
| Qt6::Network (`libqt6network6`) | 3.4 update check | **Defer.** Make the check a button that reports the latest release, or drop it. An automatic poll is a background request the learner did not ask for. |
| giflib, or an in-tree encoder | 4.2 GIF | In-tree encoder (no dependency). Qt reads GIF, so the test has an oracle. |
| `tar` (runtime) | 9.4 bundles | Accept, documented, with a round-trip test. Alternative is a dependency with a larger footprint. |

---

## Gates, per phase

Every phase must leave all of these green, in this order:

```bash
cmake --build native/build -j
ctest --test-dir native/build --output-on-failure        # 15 suites, and the new one
python3 native/tools/differential_check.py native/build/expr_cli
python3 native/tools/lang_differential_check.py          # 461/0/9
python3 native/tools/example_smoke.py                   # 49 programs
python3 native/tools/command_matrix.py                   # no new untested rows
PYTHONPATH=native/bridge python3 -m pytest -q
python3 native/tools/teach.py validate
python3 Scripts/version_sync.py
make lint-md
```

A phase that changes a public name (`Language`, a settings key, a panel object
name) also updates `docs/ARCHITECTURE.md` and the object-name contract asserted in
`ui_layout_tests`.

---

## First three commits

1. **Phase 0** — `app_paths`, the panel table, the temp-dir fixture, this document
   and its links. No user-visible change.
2. **Phase 1.2 + 1.6** (Format Code, errors-only filter) — the two smallest, and
   they prove the pattern: new module, new suite, action wired, docs updated.
3. **Phase 2** (autosave and version history) — the first feature that protects a
   learner's work, and the first that needs the data directory Phase 0 added.

Then 1.1 (Find in Files) and 6.1 (breakpoints everywhere) in parallel if there are
two people; 6.2 is the single largest item in the plan and should be started as
early as it can be staffed.
