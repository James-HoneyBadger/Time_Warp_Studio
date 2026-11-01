# Architecture

## Time Warp Studio v28.1.0 — System design

Time Warp Studio 28.1.0 is one desktop process for Linux. Ten interpreters, the Qt 6
window that drives them, the lesson machinery, the SQLite connection, the assistant and
the JSON-RPC server are all libraries in that process. Nothing crosses a process boundary
to run a program, and there is no interpreter anywhere in the tree: the Python reference
the ports were judged against was deleted, and what remains of it is a recorded corpus.

This page is the map: what the pieces are, where they live and what each one may touch.
The reasoning behind them, and the traps hit on the way, are in
[decisions.md](decisions.md). The suites and gates that hold the shape are in
[testing.md](testing.md).

---

## One process

`time_warp_ide` is the product. Two other kinds of executable are built from the same
sources: `time_warp_player`, which links only the part of the interface needed to show a
program, and the command-line tools under `native/tools/`, which link the engines with no
window at all.

Two child processes can exist, both optional and both asked for. The embedded terminal runs
the user's own shell on a pseudo-terminal (`native/src/ui/pty_session.cpp`), and the audio
player is handed a WAV through the first of `pw-play`, `paplay`, `aplay`, `ffplay` or `play`
that is installed (`native/src/ui/sound_player.cpp`). Neither is on the path a program's
execution takes.

| Library | Sources | Runs in | Role |
| --- | --- | --- | --- |
| `timewarp_expr` | `native/src/token.cpp`, `lexer.cpp`, `evaluator.cpp` | engine thread | The expression evaluator every arithmetic executor shares |
| `timewarp_lang` | `native/src/lang/` | engine thread | The shared interpreter loop, the value type, dispatch, and the nine executors |
| `timewarp_scheme` | `native/src/scheme/` | engine thread | Scheme on an explicit-stack machine, with no native recursion |
| `timewarp_canvas` | `native/src/canvas/` | producer and consumer | Carries drawing across the thread boundary |
| `timewarp_canvas_qt` | `native/src/canvas/qt_bridge.cpp` | GUI thread | Qt glue for that queue: wakeup and draining |
| `timewarp_audio` | `native/src/audio/` | either | `SOUND` and `PLAY`: notation, tones, mixing, WAV |
| `timewarp_io` | `native/src/io/` | engine thread | `SERIAL`, `PARALLEL`, `STICK` and `STRIG` |
| `timewarp_debugger` | `native/src/debugger/` | its own thread | Runs a Scheme program and services pause, step, breakpoint and stop |
| `timewarp_project` | `native/src/project/` | GUI thread | The `.twproject` model |
| `timewarp_sql` | `native/src/sql/` | either | One SQLite connection, shared by the Database tab and a program's `SQL` |
| `timewarp_lessons` | `native/src/lessons/` | GUI thread or a command | Curriculum, verifier, progress, classroom |
| `timewarp_ai` | `native/src/ai/` | GUI thread | The assistant: configuration, prompts, one network client |
| `timewarp_api` | `native/src/api/` | its own process or a test | JSON-RPC 2.0 with no transport of its own |
| `timewarp_ui_core` | `native/src/ui/` (canvas, runner, sound, themes, tokens, stylesheet) | GUI thread | Everything `time_warp_player` links |
| `timewarp_ui` | `native/src/ui/` (the rest) | GUI thread | Documents, editor, console, docks, settings |

The language table is the one piece of that list with no library: `timewarp/lang/registry.hpp`
is a header-only, Qt-free `constexpr` array, so the executors, the window and the gates
read the same ten rows.

### The two contracts

**Engines do not touch widgets.** A language engine never sees a `QWidget` and never
includes a Qt header. It turns a turtle call into a value on a `CanvasCommandQueue`, and
the GUI thread drains that queue into the canvas. A program drawing a hundred thousand
segments costs a hundred thousand appends to a ring, not a hundred thousand Qt signal
deliveries.

**Executors are stateless command processors.** An executor turns a statement, or a
program, into text and drawing commands and keeps no UI state of its own. The same
function is therefore usable from the window, from `time_warp_check`, from a lesson check
and from the API, and the answer to "what did this program do" has one author.

---

## The thread model

| Context | Owns | May touch |
| --- | --- | --- |
| **GUI thread** | Every widget, `QSettings`, the canvas display list | Anything |
| **Engine thread** (`NativeRunner`, `DebuggerRunner`, `ReplRunner`) | One run's interpreter or machine state | The canvas queue through a host adapter. Never a widget |
| **Lesson check** | The curriculum and the learner's program | `RecordingHost` instead of a canvas, so the screen is untouched |

One run at a time. `MainWindow::runCurrentDocument` stops whatever is running, constructs
the runner the language needs and starts it; there is no pool and no queue of runs.

### Control going in does not use slots

`DebuggerRunner` subclasses `QThread` and its `run()` **is** the interpreter loop. A
thread cannot spin an interpreter loop and a Qt event loop at the same time, so the runner
side has no event loop and a queued slot invocation aimed at it would never be delivered.
There is a second trap underneath that one: a `QThread` object lives in the thread that
constructed it, so its slots execute on the GUI thread even when `run()` is elsewhere.

Both traps are answered the same way. `request_pause()`, `request_continue()`,
`request_step_into()`, `request_step_over()`, `request_step_out()` and `request_stop()`
are plain thread-safe methods, callable from anywhere, that publish into an explicit
command channel and never touch interpreter state. Signals in the other direction are
ordinary Qt and are used freely: a signal emitted from `run()` arrives queued on the GUI
thread.

The wakeup word is incremented rather than merely notified, because `std::atomic::wait`
re-checks the value after every notification and goes back to sleep if it is unchanged.
`notify_all()` on a word nobody modified releases nobody. A counter also means a request
cannot be slept through, since the sequence is read before the command is tested, so a
request that lands between the two is visible on the next pass.

The line-loop languages use a different mechanism for the same job. `lang::DebugSession`
publishes the pause and waits on a condition variable, with a sequence counter captured
before the wait so a `step()` that overtook the handshake is already visible to the
predicate (`native/include/timewarp/lang/debug.hpp`). A condition variable rules out a
lost wakeup; the counter rules out a lost command.

### Latency, and closing a runaway program

The controller is consulted at the **start of every compound form**, so a pause or a step
lands within one expression of being requested rather than at some quantum boundary. With no
controller attached the check is a single pointer test, so an ordinary run pays nothing.

`MainWindow::closeEvent` stops every engine before it does anything else, and `~QThread` joins,
so a window closed mid-run does not appear to hang.

---

## The canvas command queue

An interpreter runs on its own thread and wants to draw; a widget may only be painted from
the thread that owns it. The removed Python implementation bridged the two by emitting a
Qt signal per turtle call, so every `forward(...)` cost a queued-connection post, a heap
allocation and an event-loop wakeup. A turtle program in a tight loop emits those hundreds
of thousands of times a second, and the marshalling cost dwarfed the drawing.

`CanvasCommandQueue` (`native/include/timewarp/canvas/command_queue.hpp`) replaces one
signal per command with one buffer for all of them.

- **A bounded byte ring with free-running counters.** `head` and `tail` are monotonic byte
  offsets and never wrap, so both "empty" and "full" are representable and the buffer index
  is `offset & mask`. Capacity is a power of two, so the mask replaces a modulo.
- **Release/acquire pairing.** The producer writes a whole record and then publishes
  `head` with a release store; the consumer acquires it, so observing `tail < head`
  guarantees the entire record is readable. Each counter has exactly one writer, which is
  what makes the ring lock-free without a mutex.
- **Variable-length records.** A 72-byte `CanvasCommand` header (`static_assert`ed to that
  size), then an optional point list and optional UTF-8 text, with both lengths in the
  header. No arbitrary limit on a polygon or a string, and records pack across the end of
  the ring rather than being padded.
- **Blocking without a mutex.** A full ring parks the producer in `std::atomic::wait`. No
  mutex, no condition variable and no kernel object until the wait spins down. The waiter
  parks on `space_released_`, a dedicated word that both the consumer and `close()`
  increment.

**A full ring blocks; it never discards.** A dropped turtle segment is a wrong picture that
looks like a correct one. `push_for()` and `wait_for_data()` are the bounded variants, and
they poll, because C++20 has `wait`, `notify_one` and `notify_all` but not `wait_for`.

**Coalescing happens in producer-private staging.** Pushes that `is_coalescible()` and have
an identical adjacent predecessor collapse into one record. A published slot is never
rewritten, because a 72-byte store is not atomic and the consumer could see half of it; the
pending command is held back instead and published when a non-coalescible command arrives,
on `flush()`, or once `set_coalesce_limit()` replacements have accumulated. `PenUp` and
`PenDown` are excluded even from each other: the pair is a toggle, not two assignments to
one value.

The consequence for a caller is that the consumer can lag by up to one staged command, so a
paused interpreter flushes. The host adapter does that, and the bridge's poll timer picks up
the rest.

The Qt side is `CanvasBridge` (`native/include/timewarp/canvas/qt_bridge.hpp`), which drives
the queue from the GUI event loop. Its wakeup is a two-phase handshake rather than a latch:
`drainNow()` clears the flag, drains, and loops while the flag was set again. Clearing after
a drain lets a push landing mid-drain see the flag set, skip its post and then wait for a
wakeup already consumed; clearing before has the mirror-image problem. The two sides agree
through an atomic read-modify-write on the same word, so exactly one takes responsibility and
the worst case is a redundant drain that applies nothing. Signals are per **batch**, never
per command: one `batch_applied(count)` per non-empty pass.

---

## The window

`time_warp_ide` builds a `timewarp::ui::MainWindow` from one file per section
(`native/src/ui/main_window_construction.cpp` and its neighbours) over the shared keys in
`main_window_internal.hpp`; `time_warp_player` builds a window that links the core only.

The widget tree is the editor column on the left, the output and tooling column on the
right, and the function-key bar underneath. The sizing decisions left over from the removed
implementation are load-bearing: splitter orientations, handle widths,
`childrenCollapsible(false)` wherever a zero-width pane would leave no handle to drag back,
stretch factors and the initial `setSizes()` values. They are transcribed rather than
re-derived and asserted by `ui_layout_tests`, which also grows the window and checks that
the editor and output panes keep their 3:2 proportion.

Three rules hold the rest together. **Every widget is named**, because Qt needs an
`objectName` for `saveState()` and `restoreState()` to persist dock and toolbar geometry at
all, the stylesheet scopes by it, and the layout tests find nodes with it. **Splitter
positions are persisted** alongside window and dock state, under `layout/splitters`.
**Menus delegate**: the action catalogue belongs with the feature that owns it, not in one
method that builds every menu.

`ResizableTabWidget` answers one Qt behaviour directly: `QTabWidget::minimumSizeHint()` is
the maximum of every page's hint, and Qt's trick of reparenting hidden pages out of the
layout does not remove them from that calculation. One large page therefore pins the
minimum size of the whole window while a small page is showing. The subclass returns a
small constant minimum instead and derives the preferred size from the current page, which
is what makes the right-hand column draggable and the window usable on a small display.

The toolbar is one bar with two weights: the labelled actions that start, save and run a
program, then an expanding spacer, then the icon strip for the transport, the display
controls and Settings. Fourteen controls fit at the window's own 900-pixel minimum.

### Documents

A document's identity lives on its widget: the file path is a dynamic property on the
editor, the language override is another, and the language is otherwise derived from the
path. A parallel table of paths keyed by tab index would have to be repaired on every tab
close, move and reorder, and the first time it was not would load one file's text into
another file's tab.

Two rules are deliberate. An open has two failure modes, `OpenFailure::ReportToUser` for a
file the user picked and `OpenFailure::Silent` for a file named on the command line,
because a modal dialog at startup blocks a script or a CI run forever and a mistyped path is
exactly when that must not happen. A file already open is raised rather than loaded twice,
so the same file cannot be edited in two places and saved over itself.

Saves go through `QSaveFile`, so a failure leaves the previous contents intact rather than
truncating the program to zero bytes. A UTF-8 byte-order mark is stripped on load, because
left in place it is an invisible first character and the first line then fails to parse.

### The editor

`CodeEditor` is a `QPlainTextEdit` with a line-number gutter, a current-line highlight,
cursor tracking, per-language syntax colouring, breakpoint markers and an optional minimap
that samples each line on its own row while there is room for one. Find and replace is
`FindBar` over the editor's own search; the outline is `OutlinePanel`; Format Code is the
formatter service; checking as you type is the linter. Each is a separate widget or service
because each has to work without a window and each has its own test.

The colouring rules are per language rather than one shared approximation. REXX comments are
`--` and `/* ... */`, Smalltalk's comment is a double-quoted string while its string is
single-quoted, FOCAL has no comment syntax at all, and PILOT's units are a letter and a
colon at the start of a line. The keyword tables are the engine's own command names, so the
colouring cannot teach a word the engine would reject.

Search is split the same way: `CodeEditor` owns `findNext`, `matchCount`, `replaceAll` and
`replaceCurrent`, and `FindBar` is the row of controls that drives those four calls, which is
what lets wrapping, case handling and single-undo-step Replace All be tested with no window
in the stack.

Persistence is in two halves on purpose. `saveLayout` restores the window, `saveSession`
restores the documents, and `--reset` puts the window back to its defaults without losing
the list of files somebody was working on. The recent list is rebuilt whenever the menu
opens rather than once at startup, because a menu that remembers its own contents disagrees
with the settings the moment anything else writes them.

---

## The drawing surface

`TurtleCanvas` (`native/src/ui/turtle_canvas_*.cpp`) is a consumer: the interpreter never
touches it.

- **A display list, not a pixmap.** Repainting on resize, zoom and pan needs the drawing
  again, so the ink is kept as geometry in world coordinates. A pixmap would blur when zoomed
  and could not be re-projected at all.
- **Everything the vocabulary declares, it draws.** `CanvasCommandKind` has 43 working
  kinds, all 43 are painted, and the waiver table in `turtle_canvas_tests` is empty. The test
  walks the enum and refuses a kind that is neither painted nor waived, because a vocabulary
  that outruns the surface is a promise the product cannot keep.
- **Everything it draws, a program can ask for.** The other half of the same promise: Logo's
  `BEZIER` and `PENSTYLE`, and BASIC's sprite and gradient statements, are why the walk has
  no waivers at all.
- **The turtle state is mirrored here.** The host adapter translates turtle calls one-to-one
  and statelessly, so a `Forward` command carries a distance and not the heading to move
  along; something has to replay the sequence and hold the state, and the GUI is where that
  belongs.
- **The conventions are the reference's**, transcribed rather than chosen: heading 0 is north
  and increases clockwise, `forward` is `x += d·sin(h), y += d·cos(h)`, and `circle` is a
  polygon walk of chords rather than an arc. A canvas that draws a mirrored or
  quarter-turned version of the same program is worse than no canvas at all.
- **History is in draw order and scrubbing only limits the view.** A third list records
  whether each primitive is a stroke or a shape and its index, so a rectangle painted between
  two lines comes back between them. `paintEvent` draws the first `visibleFrameCount()`
  entries; `renderedDrawing`, export and `fitToContent` use the whole list. Keeping the two
  ideas apart is what makes the scrubber a view control rather than a mode the rest of the
  window has to know about.
- **Unrenderable commands are counted, not dropped.** `commandsIgnored()` and the run's
  status message report anything the surface could not honour.

---

## The languages

There are ten: BASIC, APL, Logo, PILOT, FOCAL, Scheme, COMAL, REXX, Smalltalk and Forth.
The three emulated machines that 28.0 removed — the HB-1, the HB-2 and CHIP-8 — were a
second product inside the first, and the CHIP-8 language went with them.

### One table

`native/include/timewarp/lang/registry.hpp` holds the ten languages once: identity, display
name, protocol key, extensions and whole-program flag, in enumerator order so a row can be
found by index. It is header-only and Qt-free, so the executors, the window and the gates
all read it. `static_assert`s pin the size and the order.

`native/src/ui/language.cpp` is a Qt adapter over that table: `languageFromKey`,
`languageForFile`, display names for the selector and settings keys for persistence. A file
extension that matches nothing answers `Unknown` rather than falling back to BASIC; the
fallback is the caller's decision, because guessing hides the fact that the file name says
nothing about the language.

### The executor pattern

An executor is a function, not a class, and the signature is one declaration
(`native/include/timewarp/lang/executor.hpp`):

```cpp
using Executor = std::string (*)(Interpreter&, std::string_view, TurtleHost&);
```

- **Whole-program** (Logo, Smalltalk, Forth) receive the entire program and run to
  completion. `is_whole_program` (`native/src/lang/language.cpp`) reads the registry's flag.
- **Line-by-line** (BASIC, APL, PILOT, FOCAL, COMAL, REXX) are called once per logical line
  or command, with the whole statement in `source`.

Scheme is in the registry with the whole-program flag set but is *not* an executor:
`executor_for` answers `nullptr` for it, and that null is how a caller tells the machine
apart from the executors. Jumping between lines is an assignment to
`interpreter.current_line`, a 0-based index into `interpreter.program_lines`; after an
executor returns, if `current_line` is not the previous line plus one, the loop re-dispatches
without incrementing, so "run line X next" means `current_line = X`.

An executor may read and write interpreter state — variables, arrays, turtle position — and
return text. It must not hold a UI reference, must not draw, and must not modify the window;
ordinary errors come back as text with a marker prefix rather than as an exception.

### Output markers

Every message an engine returns begins with a marker character drawn from a fixed set of
seven, and the interface's presentation depends on the distinction. The characters are literals
in the executors; the table names each by what it depicts, and the interface's own controls and
tabs carry drawn icons rather than markers.

| Marker | Meaning |
| --- | --- |
| cross | An error or a failed statement |
| tick | A success confirmation |
| information mark | Something the program is told |
| palette | A theme or interface change |
| rocket | An execution event |
| turtle | A drawing action |
| memo | An input prompt |

### The shared loop

`native/src/lang/interpreter.cpp` owns the program lines, the variables, the output buffer,
the statement budget (`kMaxSteps`, two million, reported as a cut-off run rather than as a
crash) and the current line. `evaluate_expression` routes every arithmetic expression
through `timewarp_expr`. `run_snippet` replaces the program text while preserving variables,
arrays and per-executor state, which is what a REPL entry needs. `set_profiling` wraps each
statement in a clock and keeps a hit and time total per line.

---

## The Scheme machine

`timewarp_scheme` (`native/src/scheme/`) is a state machine with two states and one loop:
reduce the current expression or push a frame and descend, and when a value is ready, pop
the top frame and let it consume it. **No member of `Machine` calls another member
recursively.**

The rule that makes it work is that a frame is popped before its handler runs and is pushed
back only when there is more work. When a handler has no further work the value flows to the
enclosing frame, reusing its position, which is exactly a tail call; when it must continue it
pushes itself back, so non-tail recursion consumes heap frames rather than native stack
frames. A tail-recursive loop of a million calls runs with a continuation stack four frames
deep.

`Value` is a 16-byte tagged union whose heap members are pointers into a region allocator,
never owned, so values stay trivially copyable and cycles cost nothing. Symbols are interned,
so symbol equality and variable lookup are pointer comparisons. Frames are a `std::variant`
of about eighteen continuation types, and the reader, the printer and structural equality
each have their own explicit work stack.

`scheme::Host` carries the step budget and the turtle sink as virtuals, so the same machine
embeds in the window and runs headless in a test; `Host::tick` is charged every 512 steps.
Scheme is the only engine that keeps a call stack and frame bindings, and therefore the only
one with a debugger at that depth.

---

## The expression evaluator

`timewarp_expr` is the arithmetic every language shares. Lexing, parsing and evaluation are
three exposed phases — `tokenize`, `to_rpn`, `evaluate_rpn` — so a caller can compile once
and execute many times, with `evaluate()` as the memoising convenience path.

- `Token` is a 32-byte trivially copyable struct whose text is a `string_view` into the
  expression source, so lexing performs no allocation. Identifier case is normalised into a
  stack buffer at lookup rather than at lex time, which is what lets the token cache share
  views into its own stored string.
- Builtins resolve through a sorted table with a binary search, then a `switch` over an
  enum. The table is `static_assert`ed to be sorted.
- The token cache stores tokens only, so mutating a variable and re-evaluating the same text
  observes the new value, and a failed lex rolls its entry back instead of poisoning it.
- Numbers are doubles and integers are 64-bit. `int(x)` is range checked before the cast,
  shifts are clamped to 63 bits, and domain errors are explicit so `SQRT(-1)` reports a
  domain error instead of leaking a `nan` into the console.

Every executor reaches it as `Interpreter::evaluate_expression`, and the parity gate is what
keeps it honest.

---

## Parity

The language engines are compared with a frozen corpus by `time_warp_gates golden`: 442
programs in `native/tests/golden/lang_corpus.json.gz`, comparing output lines, every drawing
call in order and the final variables, with zero differences as the verdict. The Scheme
machine has its own 192-program corpus compared as whole output text, and the 24 entries
documented as divergent carry the reference's text and the reason; the check asserts that
each still differs, so an entry whose note has gone stale fails. The corpus is weaker than
the live differential it replaced, and says so: a recording cannot notice a mistake in the
recording. A deliberate behaviour change therefore edits the corpus entry in the same commit
that changes the behaviour.

---

## The debugger

There are two debuggers because there are two shapes of engine.

**Scheme** runs on `DebuggerRunner`, which supports pause, step into, step over, step out,
breakpoints, a reconstructed call stack and the frame's bindings. `Machine::step_quantum(n)`
runs at most `n` steps and returns finished, exhausted or paused, and resuming from a pause
is exact: a program suspended at every opportunity produces byte-identical output and an
identical step count to one that never suspends. Breakpoints match a form's opening line and
fire on every form that opens there. Source positions live in a side table keyed by the
stable `Pair*`, populated only when a map is requested, because widening `Value` or `Pair`
would tax every value to serve a feature almost no run uses.

**Every other language** stops through the shared `lang::DebugSession`, which the interpreter
consults through `note_line` before each statement it is about to run. The unit is a
statement for the six line-loop languages, a top-level statement for Logo, a message send
for Smalltalk and a token for Forth. Watch expressions are evaluated over the paused bindings,
and data breakpoints compare the bindings after each statement and stop when a watched name
changes. Every language that keeps a return stack publishes it as a call chain; Logo and
Forth keep none.

`DebugPanel` forwards its buttons to whichever runner is paused. The profiler uses the same
line loop: `Interpreter::set_profiling` times each statement, `NativeRunner` copies the report
out when the run ends, and `ProfileReportDialog` shows it longest first.

---

## The workspace libraries

Five libraries sit beside the engines, each with one job.

**`timewarp_project`** is a JSON view over programs that already exist: a root, an ordered
list of groups, and files with the group each belongs to. Paths inside the root are stored
relatively, so a copied folder keeps working. A save goes through `QSaveFile`, and a load
validates every field before replacing the open project, so a broken file cannot destroy the
one already open. Adding a folder walks it for recognised programs and files each under its
subfolder's group. The Projects dock is a tree over this model and never edits it directly:
every gesture is a signal the window applies to the model and then saves, which is why a
drag and a context menu cannot disagree about where a file lives.

**`timewarp_sql`** wraps one SQLite connection in `sql::Database`. It is the connection both
the Database tab and a program's `SQL` statement use, guarded by a mutex because one may run
on an engine thread while the other is on the GUI thread. A result carries columns, rows, the
change count and an error, and the 1000-row cap is part of the type rather than a decision
the panel makes. Table and schema lookups bind their names rather than pasting them into SQL.

**`timewarp_lessons`** is the curriculum and the verdict. `curriculum.cpp` reads
`native/share/lessons.json` — 20 lessons and 60 checkpoints — and refuses a checkpoint with
no expectation or two. `verify.cpp` runs a learner's program through the engines with
`RecordingHost` as the surface, so a lesson is checkable with no window. The three
expectations are what a program printed (engine commentary is not output), what the turtle
drew (pen segments and shapes counted separately, and whether it ended where it started),
and a reported error, which is checked first because a checkpoint whose answer *is* a
failure would otherwise be swallowed. `progress.cpp` keeps a learner's place in
`progress.json`; `classroom.cpp` keeps the roster, the assignment, the presentation settings,
the marks and the bundle. The Lessons panel, `timewarp_teach` and `timewarp_learn` all ask
this library, so a learner's verdict and a teacher's mark are one function's answer.

**`timewarp_ai`** is split at the only line that matters: what to ask and how to shape a
request are pure functions (`prompt.cpp` composes the system prompt and the user turn;
`config.cpp` reads, validates and rewrites the settings), and `client.cpp` is the one class
that opens a socket. Two wire shapes are supported, OpenAI-compatible and Anthropic's
Messages API, which is why a vendor such as DeepSeek is a preset holding an endpoint and a
model rather than a third protocol. The configuration never holds the key it sends:
`effective_key()` prefers `TIME_WARP_AI_API_KEY`, a stored key goes to the platform's own
store through `keyring.cpp`, and every message passes through `redact()` before it is shown.
`history.cpp` is the opt-in conversation file. The client is asynchronous and every request
streams; `time_warp_ai` waits on an event loop rather than the library growing a second,
blocking path. The manual is [the assistant](../user/assistant.md).

**`timewarp_api`** is JSON-RPC 2.0 over newline-delimited JSON with no transport of its own.
`api::Server` takes an `istream` and an `ostream`, which is how a process, a test with string
streams or an embedding application all drive it. It owns a `sql::Database` and a
`project::Project`, and it delegates execution to the same `Interpreter` and `Machine` the
window uses. Its REPL sessions are the same `ReplSession` the REPL bar drives, and its debug
sessions wrap `lang::DebugSession` and `DebuggerRunner` rather than growing a third stepping
loop. The surface is documented in [automation](../user/automation.md).

`time_warp_ide --compile` writes a standalone artifact: the source is embedded as bytes, the
generated player links `timewarp_lang` or `timewarp_scheme`, and `graphics::RecordingHost`
writes the drawing as SVG. `./twc` is a small launcher around that option, and the result
needs neither the Studio nor a second interpreter at run time.

---

## Styling

Three layers, in order: a theme's data, the tokens derived from it, and the stylesheet built
from those tokens.

**The data.** `native/src/ui/theme_table.inc` carries 35 schemes as records of string
literals. `themes::resolve()` is the one place a record becomes `Colours`, including the
high-contrast adjustment, and `paletteFor()` turns that into the application palette, which
is what dialogs, message boxes and the native parts of controls read.

**The tokens.** `design::tokens()` (`native/src/ui/design.cpp`) derives the vocabulary an
interface is built from: six surfaces, two lines, three text ranks, one accent with its
hover, pressed, on-accent and wash states, the theme's own selection pair, the state colours,
scrollbar shades and the spacing and type metrics. None of those are in a theme record, and
all of them have the same relationship for all 35. Two rules keep a derivation safe on a
retro set: the direction is the theme's, so a light theme's raised surface is darker, and
the theme's own colours win wherever it named one. Where a derivation would be unreadable —
a keyword picked to sit on a navy editor, used as an accent on a grey window —
`reachContrast()` steps the colour away from its surface until it clears a floor.

`design::uiFont()` and `design::monoFont()` name ranked fallback lists rather than one
family, so a machine with a modern interface face gets it and a machine without one lands on
its own sans or fixed-width face rather than on a font that is not installed.

**The sheet.** `native/src/ui/ui_style.cpp` is one stylesheet, applied once, built from the
tokens. It is applied once because Qt copies a widget's stylesheet to its children and
re-polishes all of them on every `setStyleSheet()` call. Sections are scoped by object name,
because a bare `QTabWidget { ... }` cascades to every descendant tab widget, including the
output sub-tabs nested inside a page of the widget being styled.

**The icons.** A stylesheet cannot recolour an image, and a `url(...)` per theme per state
would be a PNG set multiplied by 35 themes, so the icons are drawn: `icons::glyph()`
(`native/src/ui/icons.cpp`) answers with a `QIconEngine` that asks the palette at paint time,
so a theme change repaints every icon with nothing rebuilt, and a disabled or selected state
is answered with the matching palette entry rather than a second set of images. No interface
control carries an emoji in its label.

View > Theme selects a scheme, View > Next Theme cycles, and View > High Contrast derives a
maximum-contrast variant of the active theme rather than replacing it.

---

## Configuration and persistence

| What | Where | Note |
| --- | --- | --- |
| Window geometry, dock and toolbar state | `QSettings`, `TimeWarpStudio/TimeWarpStudio` | Keys under `layout/` |
| Splitter positions | `QSettings`, `layout/splitters` | A positional array, so reordering it without bumping the count restores the wrong geometry |
| Tour and coach marks | `QSettings`, `tour/seen` and `tour/coach/<key>` | Per installation, not per window |
| User content: projects, progress, exports, disks | `~/.time_warp/` | Shared with the removed Python IDE on purpose: content belongs to the user |
| Lesson progress | `~/.time_warp/progress.json` | One entry per lesson, written on every change |
| A class | `~/.time_warp/classroom/` | Roster, assignment, `presentation.json`, one file per checkpoint |
| Any of the above, elsewhere | `TIME_WARP_DATA_DIR` | A portable install, or a test that must not write a learner's real file |
| A missing or malformed setting | The layout default | A file from an older build leaves one splitter at its default rather than producing a pane with no way back |

The settings identity is deliberately not the Python IDE's, and the two schemas are never
shared; [decisions.md](decisions.md) says why.

---

## Extension points

### Adding a language

A language is five things, and each one is a compile error or a failed gate if it is missed.

1. The identity and the executor. Add the `Language` member to
   `native/include/timewarp/lang/language.hpp`, the row to
   `native/include/timewarp/lang/registry.hpp`, and the executor in `native/src/lang/`,
   registered in `executor_for`.
2. The Qt side, in `native/src/ui/language.cpp`. The adapter maps onto the registry by
   position, and a `static_assert` holds the two lists together.
3. An example under `Examples/`, with the ones that draw added to `drawingExamples()` in
   `native/tools/gates/smoke.cpp`, and rows in `native/share/command_rows.json` so every new
   command is covered by `time_warp_gates commands`.
4. Lessons in `native/share/lessons.json`, then `make teach-check` until it is silent. A
   checkpoint is checked by running the learner's program, so this is also the only thing
   that proves the lesson's transcript.
5. The prose: a section in the language reference, a tutorial in
   [the tutorials](../languages/tutorials/), a row in the README's language table, and a
   line per example in `Examples/CATALOG.md`.

Nothing in the window changes per language. `runCurrentDocument` dispatches through
`executor_for()` and the same `NativeRunner` runs any executor in that table.

### Adding a canvas command

Add the kind to `CanvasCommandKind` in `native/include/timewarp/canvas/command.hpp` with its
payload in the comment, give it a name in `native/src/canvas/command.cpp`, decide whether it
collapses by adding it to `is_coalescible()`, handle it in `TurtleCanvas::apply()`, and emit
it from `QueuedTurtleHost` in `native/include/timewarp/canvas/host_adapter.hpp`. Then paint
it, or add it to the waiver table and say why.

### Adding a panel

The right column's Variables, Telemetry, Debug, Lessons, Classroom, Database, Reference and
Assistant pages are real panels in `native/include/timewarp/ui/panels.hpp`, implemented in
`native/src/ui/panels_inspectors.cpp`, `panels_lessons.cpp`, `panels_classroom.cpp` and their
neighbours, driven by data the window gets from the runners. `PlaceholderPanel` remains as
the documented way to stand in for a panel that does not exist yet: a real widget with honest
size hints, so the layout can be built and tested before the feature is.

A panel is a view. `LessonPanel` holds no curriculum and runs no program: it raises
`curriculumNeeded`, the window loads `timewarp_lessons` and hands back summaries, and a
check is judged by the library the two commands ask. It never touches a document either;
"Open" raises `openCodeRequested` and the window decides where the code goes. The same
division holds for the inspect panels, which add only the rows the window can vouch for,
such as wall clock and how much ink reached the canvas, and show those beside the engine's
counters rather than merged, so a drawing that never arrived is visible as two rows that
disagree. The curriculum loads when the tab is first looked at, not when the window is
built: a user who never opens the Lessons panel should not pay for reading and validating a
curriculum they do not want.

---

## The build

`native/CMakeLists.txt` is a schedule of includes and options; the build itself is seven
modules under `native/cmake/`.

| Module | What it defines |
| --- | --- |
| `version.cmake` | The product version, read from `VERSION` |
| `warnings.cmake` | `timewarp_enable_warnings(target)` and `timewarp_add_suite(name, libs...)` |
| `libraries.cmake` | The Qt-free libraries: expression, Scheme, language, canvas, audio, I/O |
| `qt.cmake` | Qt 6, and the `TIME_WARP_QT` name the other modules use for it |
| `tools.cmake` | The command-line tools that need no Qt: `time_warp_check`, the gates, the icon generator |
| `libraries-qt.cmake` | The Qt libraries: canvas bridge, debugger, SQL, projects, lessons, API, the interface |
| `packaging.cmake` | Install rules, the desktop entry and CPack |

`include()` rather than `add_subdirectory()` on purpose: one directory scope keeps the
thousands of relative source paths in those files relative to `native/`, which is what they
assume. `time_warp_gates structure` fails if `CMakeLists.txt` grows past 200 lines or a
source file past 1,500 without being named on a waiver list that may only shrink.

Qt is discovered with `find_package(${TIME_WARP_QT} QUIET COMPONENTS Core)`, plus `Widgets`
for the interface. When they are absent the Qt-facing targets are skipped and everything else
still builds and runs. `Svg` and `PrintSupport` are asked for separately and only by the
interface: without them, SVG export and the two Print entries are absent rather than broken.
Nothing outside the canvas bridge, the debugger runner and `native/src/ui/` includes a Qt
header.

Every tool under `native/tools/` speaks one command-line vocabulary, defined in
`native/include/timewarp/tools/cli.hpp`: `--root`, `--json`, `--quiet` and `--help`, accepted
in any position, with status 2 for a command line that cannot be used and status 1 for checks
that ran and failed. The header links nothing, not even Qt, because the documentation gates
build in a project of their own with no desktop stack. The gates themselves, and what each
one protects, are in [testing.md](testing.md).
