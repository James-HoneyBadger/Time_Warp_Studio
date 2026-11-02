# Decisions

This page is the record of what was chosen, what was rejected and why. It exists because most
of these decisions have a plausible alternative that somebody will otherwise propose again, and
because several of them were reached by walking into the wall first.

The shape of the system is in [architecture.md](architecture.md); the suites that hold these
decisions in place are in [testing.md](testing.md); where the project is going is
[roadmap.md](roadmap.md).

A note on tense. Where something was wrong before, this page says what was wrong in one clause
and what is true now. That is deliberate: a decision without its alternative is a preference,
and a decision without its history is a rule nobody can safely revisit.

---

## What the port replaced

The application was a PySide6 program. It is a C++20 / Qt 6 program, and the Python side is
gone: application, engine, tests and the harnesses that drove them. Each library still
carries the shape of the file it replaced, which is why the provenance is worth one table.

| Library | Replaced | Why the replacement took the shape it did |
| --- | --- | --- |
| `timewarp_expr` | `utils/expression_evaluator.py` | A self-contained, allocation-sensitive unit with no Qt dependency whose behaviour was already pinned by a test suite, so fidelity was already a measurable question |
| `timewarp_scheme` | `languages/scheme.py` | The recursive evaluator was the thing worth replacing: its depth *was* CPython's call depth |
| `timewarp_lang` | `languages/*.py` | The reference's executors were plain functions, so the C++ ones are plain functions too |
| `timewarp_canvas` | `ui/output.py` and `ui/canvas.py` | Not a port of a file. It replaces the *mechanism* by which drawing crossed a thread boundary |

`timewarp_expr` came first because it was the unit with the fewest unknowns. Every port since
has proved itself the same way: record what the reference did, then compare against the
recording.

### Parity, and what a recording cannot do

The reference engine was deleted in 18.0. What remains is `lang_corpus.json.gz`, 444 programs
with the output they printed, every drawing call in order and the variables they left, and
`scheme_corpus.json.gz`, 192 programs compared as whole output text. `time_warp_gates golden`
compares the ports against both, and zero differences is the verdict.

This is weaker than a live differential, and the weakening is deliberate rather than
regretted. A recording cannot notice a mistake in the recording, and it cannot answer a
question nobody asked before. Two rules follow. A deliberate behaviour change **edits the
corpus entry in the same commit** that changes the behaviour, with no separate corpus release
and no "we will regenerate it later"; and a divergence that is kept carries the reference's
text **and the reason** beside it, with the gate asserting that the port still differs. A note
that has gone stale fails the run, which is how three divergences were retired when the port
caught up. Programs using randomness are in neither corpus, because both sides seeded from the
operating system and agreement was never a property they could have.

### What was removed, and why

**The Python engine, at 18.0.** It ran the languages that had not been ported, then only the
ones a live differential still compared, and it was also the curriculum checker and the engine
the command matrix drove. Once every language was native and the answers were frozen, it was a
second implementation of the product with no remaining job.

**The HB-1, the HB-2 and CHIP-8, at 28.0.** Three emulated computers lived inside the
application, each with its own clock, storage, manuals and caveats, and each was a second
product. Removing them removed the language that ran on the last one. Ten languages remain, and
the application is what it was for: write a program, run it, watch it draw, stop it, read why.

**The two-row toolbar, at 28.1.** Two stacked bars plus a third row of function keys made the
chrome look like a 1994 application with a second row bolted on. The split it bought, keeping
the display controls reachable on a narrow window, is what one bar and an expanding spacer keep
without the height.

**`themes::stylesheetFor()`, at 28.1.** Its rules were the second half of a sheet, and a
stylesheet assembled from two places is a stylesheet where neither place can answer what a
control looks like. The sheet is composed in one place now.

**The emoji, at 28.1, in the interface only.** Toolbar labels, tab titles, sub-tabs, dock titles
and the language badge carried emoji, which are rendered by whichever font supplies them, at
that font's weight, in colours that ignore the theme. They were replaced by drawn icons and
words. The engine's output markers are untouched: that is a message convention, not an icon set.

---

## The expression evaluator

**Tokens are trivially copyable and own nothing.** The Python token carried an `Any` payload
that was either a float or a string, and the obvious C++ mapping,
`std::variant<double, std::string>`, would allocate per identifier and make the token vector
non-memcpy-able. A 32-byte POD whose `text` borrows from the expression source makes lexing
allocation-free.

**Keywords are matched without copying.** The reference built `name.upper()`, allocating a
string, merely to test membership in a seven-element tuple. The port compares in place against
a length-dispatched `constexpr` table.

**Identifier case is normalised at lookup, not at lex time.** Normalising into a 96-byte stack
buffer, with a reusable member string as fallback, keeps the upper-case data model the
reference had while leaving tokens with nothing to own, which is what makes the token cache
safe to share views with.

**Builtins resolve through a sorted table and a binary search, then a `switch`.** A dictionary
of Python callables would have become a map of `std::function`, adding an indirection and a
possible allocation per call. The table is `static_assert`ed to be sorted, so an edit that
breaks the order fails the build rather than a lookup.

**The token cache stays, and stores tokens only.** Variable values are resolved during RPN
execution, so mutating a variable and re-evaluating the same text observes the new value. The
cache inserts the key first and lexes from the *stored* string: `unordered_map` is node-based,
so the key's buffer never moves and the borrowed views stay valid. A failed lex rolls the entry
back rather than poisoning the cache.

**Hardening where C++ has no bignums.** `int(x)` is range checked before the cast, shifts are
clamped to 63 bits, and domain errors are explicit so `SQRT(-1)` reports a domain error instead
of leaking a `nan` into a console where every number is printed.

### What it costs

The same eight-expression corpus, 160 000 evaluations, `-O3`, one machine. The figures are
ranges because run-to-run noise on one machine is a few per cent, and a single figure would
claim a precision the measurement does not have.

| Mode | Python reference | C++20 port | Speed-up |
| --- | --- | --- | --- |
| hot (token cache on) | 5 770 ns/eval | 180–200 ns/eval | about 30× |
| cold (cache cleared) | 11 070 ns/eval | 300–315 ns/eval | about 36× |

Both implementations return the identical aggregate for the corpus, which is independent
corroboration that the two agree on the numbers.

### Deliberate divergences

Everything else in the corpus matches the reference exactly. These do not, on purpose.

1. **`0x` and `0o` literals work.** In the reference the radix branch sat *after* the decimal
   branch and was unreachable, so `0x10` lexed as `0` followed by the variable `X10` and failed.
   The port restores the documented intent. `&HFF`, which did work, is unchanged.
2. **String equality compares strings.** The reference implemented `==` as
   `abs(a - b) < 1e-10`, so string equality could never succeed: it raised a type error. Ordering
   comparisons work lexicographically as well.
3. **`0 ^ -1` raises a division-by-zero error**, and `(-8) ^ 0.5` raises a clear error instead
   of producing a complex number that then failed to convert.
4. **Math domain errors are explicit** for `SQRT`, `LOG*`, `ASIN` and `ACOS`, matching the
   `math` module's behaviour with one standard message rather than the old module's text.
5. **Integers wider than 64 bits are rejected** rather than silently widened, because C++ has no
   bignum.
6. **Shifts by 64 are clamped to 63**, because shifting a 64-bit value by its width is undefined
   behaviour in C++ while Python widens.

Two messages were also reworded where the reference leaked the interpreter's own innards, so
`LEN(5)` says that `LEN` wants a string rather than that a float has no length. Success and
failure classification is identical in every case.

### Quirks kept on purpose

Changing any of these would change what an existing program does, so they are carried in the
tests as behaviour rather than tolerated as bugs. The token budget is checked at the top of the
scan loop *before* whitespace is skipped, so a trailing blank after the budget is exhausted
still errors; a type suffix on a function name is consumed and discarded, so `LEN$(A$)` calls
`LEN`; `RND` and `RAND` shadow a variable of the same name and, in call form, pop and ignore one
numeric argument; the number scanner consumes only digits and dots, so `1e300` lexes as `1`
followed by the variable `E300`; `MIN`, `MAX`, `POW` and `ATAN2` are the only two-argument
builtins; and a user array shadows a builtin of the same name.

### The two bugs the differential run caught

Both were invisible to the unit tests and obvious to a byte-for-byte comparison, which is the
argument for having one.

- A `std::cout << "OK " << evaluate(...)` chain emitted the prefix *before* a throwing
  evaluation, so failures printed `OK ERR …`.
- The cache stored an empty token vector when lexing threw, turning a syntax error into
  `Invalid expression` on the second attempt.

---

## Scheme

**Why this was the exercise worth doing.** The reference evaluated by recursing, so its
evaluation depth *was* native call-stack depth, and CPython's thousand-frame limit capped a
Scheme program at roughly two hundred calls. The port replaces every recursive site with an
explicit, heap-backed continuation stack: a tail-recursive loop of a million calls runs with a
continuation stack four frames deep, and non-tail recursion is bounded by memory.

**What was actually recursive was established by reading, not by assuming.** The reference
contains a trampoline — `_TAIL`, `_PENDING`, `_TAIL_FORMS` — and it is dead code: no special
form returns `_TAIL`, nothing pushes to `_PENDING`, `_TAIL_FORMS` is never read, and the one
`pop()` would have raised `IndexError` had it ever been reached. Every apparent tail call was
ordinary recursion. That is the reason the frame design could be chosen from first principles
rather than reverse-engineered from working tail behaviour.

**One rule makes the machine work: a frame is popped before its handler runs and pushed back
only when there is more work.** A handler with no further work lets the value flow to the
enclosing frame, reusing its position, which *is* a tail call. A handler that must continue
pushes itself back, so non-tail recursion costs heap frames. `begin_sequence()` applies the
same idea one level down: a sequence of one form pushes nothing at all, and a sequence pops
itself before evaluating its last form, so the last form of every body is a tail position.

| Workload | Scale | Peak continuation depth |
| --- | --- | --- |
| tail-call loop | 1,000,000 calls | 4 |
| non-tail recursion | 200,000 levels | 200,004 (heap) |
| list construction | 200,000 elements | 5 |
| printing a long list | 200,000 elements | 4 |
| deep structural `equal?` | 200,000 levels | 5 |

The reference aborts all of those at roughly two hundred calls.

**Values are pointers into a region allocator, never owned.** `Value` is a 16-byte tagged
union, so it stays trivially copyable and trivially destructible, which matters because values
are copied constantly. It also means `set-cdr!` cycles and `letrec` self-reference cost
nothing, where `shared_ptr` would leak them. Symbols are interned, so symbol equality and
variable lookup are pointer comparisons. **Two structural departures were forced by dropping
Python.** A vector was a list in the reference and parsed code was *also* a list, and the two
collapsed into each other, which is the `case` bug below; here `Vector` is its own type and code
is always pairs, which also makes quoting the identity.

### A reference bug that is fixed rather than reproduced

`case` never worked in the reference. Its special form called a list conversion on parsed
code, which was a Python list rather than a pair chain, so the conversion walked nothing and
every `case` form, `else` branch included, raised "expected a proper list". Reproducing that
byte for byte would have meant shipping a dead special form, so it is fixed, and the fix is
recorded in the corpus with its reason.

### Divergences kept

**Depth** is the point of the exercise. Deep tail and non-tail recursion, deep list
construction and deep structural equality succeed here and abort in the reference.

**The unspecified value renders as `#<unspecified>`.** The reference printed Python's default
object rendering, which embeds a heap address. An address is not reproducible information.

**Exceptions that escaped the reference's executor are proper Scheme errors here.** It caught
three exception types and let the rest through, so ordinary mistakes crashed the engine
instead of reporting a problem: a negative square root, an index out of range, a wrong arity
and a division by zero all escaped. Each now reports the procedure and the fault.

**Integers wider than 64 bits promote to a double** rather than widening, so `(expt 2 100)`
prints in exponent form rather than as exact digits.

**Integral reals beyond 2^63 are printed with every digit.** This looks like a divergence and
is not: the reference rendered an integral real through an exact integer conversion, so a
fixed-format conversion is the faithful one and the exponent form would have been wrong.

**Non-ASCII case mapping is ASCII-only**, where Python's is Unicode-aware.

### Quirks kept

Each is verified against the reference, and each is carried in the tests as behaviour rather
than tolerated as a bug, because changing one would change what an existing program does.
Falsiness is Python's: only `#f` is false, `#t` equals `1` and `#f` equals `0`, and a symbol
equals a string with the same characters, so `(equal? 'abc "abc")` is true. There is no
dotted-pair syntax and no vector literal: `.` and `#` are ordinary symbols, and only `lambda`'s
rest parameter gives the dot meaning. Characters are plain strings, a string nested in a list is
written quoted even under `display`, and integral reals print as integers. Arithmetic follows
Python's: `round` is half-to-even, `quotient` truncates while `modulo` is floored, and
`integer?` is true only for exact integers. Binding follows it too: missing arguments bind to the
empty list, surplus ones are dropped, and `set!` on an unbound variable defines it. `cond`
supports `=>` and a test-only clause yields the test's own value; `substring` and `list-tail`
clamp like slices and count negative indices from the end; `string->number` accepts a radix and
ignores it; and a misspelt `string=?` with a trailing space is registered as well, so both
spellings work.

### The bugs the differential run caught

- **An overflow helper relied on undefined behaviour.** It computed a product and then tested
  the product divided by one operand. Signed overflow is undefined, the compiler folded the
  test away under the assumption that overflow cannot happen, and the port silently produced
  wrapped results for `(expt 2 63)` and for a product of two large values. Magnitudes are now
  compared before multiplying, and both cases are regression-tested.
- **Integral reals beyond 2^63 printed in exponent form**, where the reference printed every
  digit.
- **`map` and `filter` skipped every other element**, because the frame's index was initialised
  one step ahead of the row most recently dispatched.
- **`"."` parsed as the real zero**, which broke a rest parameter by turning its marker into a
  number.
- **Quasiquote needed a session stack, not a flat one**: a quasiquote inside an unquote would
  otherwise clobber the outer template's partially built accumulator.

Every expectation in the suite and in the corpus came from *running* the reference, not from
reading it. That is how the `case` bug was found rather than assumed absent.

---

## The canvas command queue

**The problem was a signal per command.** `ui/output.py` ran the interpreter on a thread and
talked to the interface with a Qt signal per message, so turtle graphics paid a queued-connection
post, a heap allocation and an event-loop wakeup for every `forward(...)`. A loop drawing two
hundred thousand segments paid it two hundred thousand times, and the marshalling cost dominated
the drawing. Worse, the interpreter mutated a turtle state that the canvas read, which was a
data race the GIL only covered by accident.

**One producer and one consumer, so the ring can be genuinely lock-free.** The application runs
one interpreter at a time, and `head` is written only by the producer and `tail` only by the
consumer, which is what makes the read-test-publish sequences safe with no lock at all. If a
second producer were ever needed, this would have to be redesigned rather than extended.

**Byte offsets that never wrap** make "empty" and "full" both representable and reduce the
buffer index to a mask, which is why the capacity must be a power of two. **Release/acquire
pairing** on the two counters is what guarantees that observing space means the record in it is
readable, so no partial read is possible.

**Records are variable length and packed across the end of the ring** rather than padded to a
fixed size, so the ring stays dense and a polygon or a string has no arbitrary cap.

**Blocking uses `std::atomic::wait`, not a mutex**, so there is no kernel object until the wait
spins down.

**Coalescing happens in producer-private staging, and this is the subtle part.** Rewriting a
published slot would let the consumer observe a half-written 72-byte record, because that store
is not atomic. The pending command is therefore *held back*: nothing published is ever mutated
and the race cannot arise. It is published when a non-coalescible command arrives, on `flush()`,
or once the coalesce limit is reached, so a caller that only ever sets state still progresses.

**Only an identical adjacent predecessor collapses**, so a heading, a move and another heading
keeps both headings. Collapsing across the move would change where the pen travelled. `PenUp` and
`PenDown` are excluded even from each other, because the pair is a toggle rather than two
assignments to one value. The queue therefore **never drops and never reorders**: nothing is
dropped because superseded state is the one thing that cannot matter, and nothing is reordered
because the staged command waits in place. The liveness consequence is stated rather than hidden:
a staged command is not yet visible, so the consumer can lag by one, which is why a paused
interpreter flushes and why the bridge keeps a poll timer.

### The Qt bridge, and why a latch was not enough

The bridge hands work to the GUI thread with `QMetaObject::invokeMethod` and collapses a storm
of wakes into a single pending invocation. Collapsing with a plain latch strands commands:
clearing the flag *after* a drain lets a push landing mid-drain see it set, skip its post and
then wait for a wakeup already consumed, and clearing it *before* has the mirror image. The
two-phase handshake — clear, drain, loop while the flag was set again — works because both
sides agree through an atomic read-modify-write on the same word, and those are totally
ordered, so exactly one takes responsibility.

Signals are emitted per **batch**, and commands go to a plain sink rather than through a
signal, because emitting one signal per command would reintroduce exactly the cost the queue
exists to remove.

### What it costs

| Workload | Throughput | Per command |
| --- | --- | --- |
| single-threaded push and drain | 58.1 M/s | 17.2 ns |
| one producer and consumer through a ring too small to hold the run | 10.8 M/s | 92.3 ns |
| state pushes, coalescing on | 166 M/s | 6.0 ns |
| geometry pushes, nothing coalescible | 58.8 M/s | 17.0 ns |

The second row is the honest one: the producer genuinely fills the ring and parks, and the
consumer wakes it. The payoff is the coalescing figure: a million consecutive state changes
collapse to under a thousand published records, so the bridge posts under a thousand wakeups
rather than a million.

### The bug the tests caught was in the anti-deadlock valve

`close()` originally unblocked a parked producer by notifying the tail counter. It deadlocked.
`std::atomic::wait` re-checks the value after every notification and goes back to sleep if it is
unchanged, so notifying a word you do not modify never releases a waiter. Since the tail has a
single writer and `close()` is not it, `close()` could not bump it. The producer now parks on a
dedicated word that both the consumer and `close()` increment.

That was the mechanism whose entire job is to guarantee a blocked producer can always terminate,
hanging forever. The lesson generalised: every wakeup word in this tree is incremented, never
merely notified.

A few test assumptions were also corrected, and each was the test's error rather than the
queue's: one asserted that a long run must have blocked the producer, which is false when the
consumer keeps up, and two pushed more commands than the ring holds without draining, which
deadlocks by design under a never-drop policy.

---

## The debugger

**Control going in does not go through slots, and that is the whole design.** A `QThread` object
lives in the thread that constructed it, so its slots run on the GUI thread rather than in
`run()`. Subclassing `QThread` and expecting the worker to receive slot calls is the classic Qt
threading mistake, and here there is no event loop on the runner side to deliver them anyway.

So `request_*` are plain thread-safe methods that publish into an explicit command channel, and
the reverse direction uses signals, which are genuinely safe from a worker thread. The
asymmetry is not an accident of implementation: it is where the Python version's bugs lived,
because pause and step travelled through shared mutable state that both threads read and no
single place said what a pause request *is*.

**The wakeup word is incremented, never merely notified**, for the reason the canvas queue
taught. The sequence is also snapshotted *before* the command is tested, so a request that lands
between the two cannot be slept through, and `Stop` is sticky as well as posted so it survives
being requested before the thread has started.

**Breakpoints are published as replacement sets with a generation counter**, so the hot path is
one atomic load per form and never takes a lock, and a breakpoint added mid-run takes effect on
the next form.

**A breakpoint matches a form's opening line and fires on every form that opens there.** The
obvious refinement — stop only on the first form of the line — needs to know that execution left
the line and came back, and that is not always observable: a tail-recursive loop whose body sits
on one line never reports another line, so a breakpoint added there would silently never fire.
Over-stopping costs one extra press of continue; a breakpoint that does nothing cannot be
worked around by the user. This is a choice, not an oversight, and it is stated as a known
limit.

**The machine became resumable because it was already a state machine.** `load()` prepares a
program without running it, `step_quantum(n)` runs at most `n` steps and reports finished,
exhausted or paused, and `run_loaded()` is the unlimited case. A controller is consulted at the
start of every compound form, and a null controller makes that a pointer test. Suspension is
cheap precisely because there is no native stack to preserve: returning out of the loop leaves
everything on the heap frame stack. That is the dividend from the explicit-stack rewrite.

Resumption is exact, and both facts are asserted: the controller is consulted **before** the
step is charged, and the suspended form is **re-tested rather than re-entered**, so a program
that suspends at every opportunity produces byte-identical output and an identical step count.

**A tail call replaces the caller's marker instead of stacking inside it.** A naive marker per
call would turn a million-iteration loop into a million markers. Replacement is not a
compromise: a tail call genuinely does not deepen the stack, so a five-thousand-iteration loop
reports a two-entry stack and the report stays honest. Markers are pushed only while a
controller is attached, so an ordinary run's frame layout is untouched.

Each marker also saves the **caller's** tail-position base. Deriving it from the marker's own
height looks equivalent and is not: the frames between the caller's base and the marker belong
to the call site and are gone when the marker is popped, so a derived height compares against a
stack shape that no longer exists and the next tail call is silently lost. A test recurses three
hundred times through a nested call followed by a tail call to pin that down.

**Source positions live in a side table keyed by the stable pair address**, populated only when
a map is requested, because widening `Value` or `Pair` would tax every value to serve a feature
almost no run uses. Every pair on a list's *spine* is recorded, not just the head, because a
procedure's body is a cdr-derived sublist and would otherwise have no position at all.

### The bugs the tests caught

- **A nested call lost the tail call that followed it**, through the base bookkeeping above. The
  simple tail-call tests passed throughout.
- **`Machine::emit` was erased by a macro.** Qt defines `emit`, so a member with that name is
  silently deleted in any translation unit that includes a Qt header. The symptom was an
  inexplicable parse error in a header that had compiled for weeks. It is `append_output` now,
  with a note saying why.
- **`load_forms` did not reset the step count**, which was harmless until a debugger displayed
  it and a fresh program inherited the previous program's count.
- **Continue re-fired the breakpoint it resumed from.** Returning from a suspension re-tests the
  same form, which matches the same breakpoint, which suspends again, so a program could never
  get past a breakpoint. The test controller models the behaviour precisely because every test
  in that area depends on it.
- **Only list heads had spans**, so every procedure body looked position-less.
- **A queued signal was delivered after its receiver's frame had returned.** The harness
  connected signals with the application as context and captured a stack-local recorder by
  reference; a queued connection outlives the emit, so an event from one test was delivered
  during the next. AddressSanitizer named it exactly — use after return, pointing at the frame
  that owned the recorder — where the plain symptom was a segfault two tests later inside an
  unrelated string destructor. The connection context is now the object that owns the captured
  state, so its destructor discards pending events. This is the failure mode the sanitizer build
  exists for.

### Known limits, stated rather than discovered

Stop granularity is a compound form, so a program of nothing but literals has nothing to check
between its first form and its end. Step-over is form-level rather than line-level, and will
stop again on a sibling sub-expression of the same line, because a line-level stepper needs the
source map to answer "the next form on a different line". A breakpoint on a line that no form
*opens* on never fires, which is consistent with reporting the opening line at a stop.

---

## The window

The Python window supplied the layout, and most of its sizing decisions were right. What is
worth recording is the list of things that were changed, and why.

| Change | Reason |
| --- | --- |
| No back-pointer to the window | The Python tab widget held a `MainWindow` reference and reached through two levels of privacy for every non-local action. Here every such action is a signal, so the widget can be built and tested alone and tab state stays owned by whoever owns the files |
| One stylesheet, applied once | Qt copies a widget's stylesheet to its children and re-polishes all of them on every call; the Python made six such calls on a window with hundreds of widgets |
| Scoped selectors | A bare `QTabWidget { ... }` cascades to every descendant, including the output sub-tabs nested inside a page of the styled widget, which is why the Python needed a compensating override |
| Splitter positions persisted | The Python saved geometry and dock state but not splitters, so a dragged divider was forgotten on every restart |
| The overlay tracks its own target | The Python resized the CRT overlay from the window's resize handler via the parent's rectangle, which stops working the moment the overlay is reparented. An event filter on the widget it covers is the mechanism, and the window's own resize handler disappeared |
| Menus delegate | The Python built every menu in one long method; here the action catalogue belongs with the feature that owns it |
| Widgets are named | A stored layout silently skips an unnamed toolbar or dock, the stylesheet scopes by name and the tests find nodes by name. Naming is a precondition for two of those |
| Standard key sequences | The standard keys follow the desktop's own convention instead of being spelled as literals |

**`ResizableTabWidget` exists because of one Qt behaviour.** `QTabWidget::minimumSizeHint()` is
the maximum of every page's hint, and Qt's reparenting of hidden pages does not remove them from
that calculation, so one large page pins the whole window's minimum size. A subclass that
returns a small constant minimum and derives the preferred size from the current page is what
makes the right-hand column draggable and the window usable on a small display. It is asserted
directly: a large page beside a tiny one must still report the tiny one's minimum.

**A document's identity lives on its widget.** A table keyed by tab index has to be repaired on
every close, move and reorder, and the one time it is not is the one time the wrong file's text
is written to the wrong name.

Four smaller decisions follow from what a text editor can do to somebody's work. A file that is
already open is raised rather than loaded twice, because two copies of one file means the last
save wins and the other copy's work disappears with no undo. Saves go through `QSaveFile`, so a
failure leaves the previous contents intact instead of truncating the program to zero bytes. A
byte-order mark is stripped on load, because left in place it is an invisible first character
and the first line fails to parse with an error at a column that looks empty. Newlines are
normalised on load, so a file with CRLF endings round-trips as LF.

**An open has two failure modes on purpose.** Reporting to the user is right for a file somebody
picked. Silent is required for files named on the command line, because a modal dialog at
startup blocks a script or a CI run forever, and a mistyped path is exactly when that must not
happen. The unreadable-file and cancelled-save paths are tested through the silent mode, because
a test that triggered a modal dialog would hang rather than fail.

### Two findings that only rendering the window could produce

- **The toolbar did not fit a 1400-pixel window.** Thirteen controls, and the trailing ones were
  clipped with no extension button to reach them; at 1920 pixels they all fit. A structural test
  would have passed. 28.1 settled it by moving the display controls onto the first bar past an
  expanding spacer, where fourteen controls fit at the window's own 900-pixel minimum.
- **A fresh window must have a document tab.** Without one the editor pane is an empty box and
  the tab bar is invisible, so the editor column reads as broken.

The status bar painting a message over the document's path was visible in exactly one place,
a screenshot, and nowhere else, which is why rendering the window is part of the routine rather
than a special effort.

---

## The styling layer

**A theme record is data, not a design.** None of the colours an interface needs — a hover, a
muted text rank, a pressed accent, a sunken field — are in a theme, and all of them have to have
the same relationship for every theme. Deriving them in one place, from the theme's own
`Colours`, is what made it possible to add a token layer without editing any of the 35 records.

**Two rules keep a derivation safe on a retro set.** The direction is the theme's, so a light
theme's raised surface is darker rather than lighter; and the theme's own colour wins wherever it
named one, so a derivation can only fill gaps.

**Where a derivation would be unreadable, the floor wins.** A keyword chosen to sit on a navy
editor makes a poor accent on a grey window, and a border colour can be a shade off the page it
frames, so the contrast helper steps such a colour away from its surface until it clears a floor.
Every theme is covered by the same rules, which is why the derivation is a function and not a
table of 35 answers.

**Icons are drawn rather than shipped.** A stylesheet cannot recolour an image, and a URL per
theme per state would be a PNG set multiplied by 35 themes. An icon engine that asks the palette
at paint time means a theme change repaints every icon with nothing rebuilt, and a disabled or
selected state costs a palette lookup rather than a second set of images.

**Fonts are ranked lists rather than one family.** The Python build named a fixed-width face that
is not installed on a stock Linux machine, so it fell through to whatever Qt resolved: a
different font per distribution, on the one surface where column alignment is the point.

**The minimap is a map.** Stretching a two-line document into two grey blocks across the whole
strip told a reader nothing; each line gets its own row while there is room for one, on the
gutter's surface, clear of the scroll bar that used to be painted over its right-hand column.

**One toolbar, two weights.** Labels for the actions that start, save and run a program, glyphs
for transport and display, and a single tinted control — Run — for the one action the window is
for.

---

## The build

**`native/Makefile` is an interface, not a second build system.** It owns the targets, the
defaults, the job count and the out-of-tree layout; CMake owns the compilation. Compiling these
sources directly would mean reimplementing automoc, Qt component discovery, the
position-independent code flags and include paths, every library in dependency order and every
test registration, and the copy would be the one that rots.

**Each build type gets its own tree.** Changing the build type in place recompiles everything
anyway, so separate directories cost nothing and let a release build and a debug build coexist; a
sanitised tree gets one directory per sanitiser for the same reason. **Warnings are errors, in one
place**: the flag set is applied by one function rather than written at each target, so a new
target cannot be added without it. **GCC 11 is the floor** because the lexer parses literals with
floating-point `std::from_chars`, which older standard libraries do not implement.

**CI guards a failure a green build would hide.** A configure that cannot find Qt Widgets still
exits zero and silently skips the entire interface, so the job reads its own configure log and
fails if the interface was skipped. Before the suites it runs the shipped entry point — version,
help, and a screenshot of a file that does not exist — because "links" and "starts" are different
claims.

**Qt components are optional on purpose, and each one removes a feature rather than breaking
one.** Svg enables vector export; PrintSupport supplies the two Print entries. Without them the
save filter and the help text do not name a format that would fail, and nothing outside the
canvas bridge, the debugger runner and the interface includes a Qt header.

**The structure gate has a budget with headroom.** The build file may not pass 200 lines and a
source file may not pass 1,500, and the waiver list may only shrink. The budget was 2,000 while
the tree still had monoliths, when the largest file was 3,548 lines, and it came down when every
file fitted, because a budget with no headroom is the one that gets raised.

**One command-line vocabulary for every tool.** `--root`, `--json`, `--quiet` and `--help`,
accepted in any position, status 2 for a usage error and status 1 for a failed check, with each
tool generating its usage message from the table that dispatches.

---

## Documentation as a gate

Three rules were worth mechanising, and each exists because the alternative was a review that
had to remember. Every promised file must exist and every relative link and anchor must resolve,
because a tree restructured by audience breaks links in files nobody opened. Every path and name
a guide mentions must still be there, with a history marker for a region that has to name what
it records and a short list of retired names that may only appear in the past tense. And every
hand-written copy of the version must agree with the version file, because a version quoted in
four places is four chances to be wrong.

The gate that is not written down is the one this page serves: a decision that was expensive to
reach should survive the person who reached it.
