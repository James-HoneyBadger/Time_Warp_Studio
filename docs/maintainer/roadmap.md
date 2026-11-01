# Roadmap

This page records what is built, what is deliberately not built, and what is known to be
wrong. The release-by-release record — what each version changed and why — is in
[the history](../project/history.md).

The current design is 28.x: ten languages, the IDE, the curriculum and this documentation
set, with the gates in [testing](testing.md) running on every push. The feature set is not
frozen, and a later release takes fixes and features alike, each arriving with the test
that proves it.

---

## Open work

Five items, none of them a gap in the product as it ships. Each is registered in
`native/share/unbuilt.txt`, which `make doc-claims` checks against the pages that mention
it, so shipping one without correcting the documentation fails a gate.

| Item | Why it is not there |
| --- | --- |
| Automatic update check | The application has no account, no telemetry and one network call, which is the assistant. An update check would be a second one, and the packaging story — four formats and a container — makes "is there a newer version" a question the package manager already answers |
| Snap | A third sandbox format beside Flatpak and AppImage, for a distribution channel this project does not build for otherwise |
| arm64 AppImage | The ARM build of the bundling tool is not exercised by the release pipeline, and an artifact nobody has run is not one to publish. arm64 is served by the `.deb`, `.rpm` and tarball |
| Fully static build | Qt cannot be linked statically in a way that keeps font rendering, platform plugins and the SVG module working without a build of Qt to match. The AppImage is the answer that ships |
| Publishing to a distro repository (`distro repository` in `unbuilt.txt`) | The `.deb` and `.rpm` are on the release page. A repository is an ongoing commitment to a signing key and an archive, not a build step |

---

## Working today

| Area | State |
| --- | --- |
| Shell | Documents and tabs, menus, one toolbar, status bar, settings persistence, the last session's files restored at launch |
| Editor | Line-number gutter, current-line highlight, per-language syntax colouring, find and replace, find in files, go-to-line, tab stops, fonts, error squiggles with gutter markers, completion, auto-indent, Format Code, minimap, outline, two panes |
| Projects | `.twproject` files, named groups, files and folders added to a project, drag-and-drop grouping, missing-file reporting, entry point, recent projects and session restore |
| REPL | A persistent session per language under the editor, with the bindings each engine keeps — Scheme keeps definitions and echoes values, the line-loop languages and Logo keep variables, Smalltalk and Forth carry nothing forward |
| Console | Streaming output, interpreter errors in their own tab, clickable line links that go to the line the program numbers, an errors-only filter, and **File > Save Output As…** |
| Canvas | Live drawing, zoom, pan, grid, auto-fit, turtle marker, scrubber playback, animated GIF recording, raster and SVG export, the pose strip, the command log, and a fixed-coordinate screen mode with sprites and pixel read-back |
| Display | 35 themes derived through one token layer, high contrast, the 100/125/150% text ladder, focus mode, presentation mode, CRT settings with four monitor presets |
| Execution | Run, run the selection, stop, and one program at a time; a budget of two million statements or ten seconds in every engine |
| Input | A program can ask for a line and wait: `INPUT`, FOCAL's `ASK`, PILOT's `ACCEPT`, REXX's `PULL` |
| Languages | Ten, with 68 example programs |
| Engines | All ten native and in process: BASIC, FOCAL, PILOT, COMAL, APL and REXX line by line; Logo, Smalltalk, Scheme and Forth whole-program |
| Debugging | Breakpoints, pause, continue, step, watch expressions and data breakpoints in all ten; rewind in eight — every language but Smalltalk, which has no checkpoint yet, and Scheme, whose state is an arena and a continuation chain |
| Database | A SQLite Database panel, the shared `SQL` statement in nine languages, scripts, tables and schema; compiled programs link SQLite |
| Automation | JSON-RPC over stdin and stdout, and the `twc` terminal compiler |
| Packaging | `make install`; tagged releases build `.deb`, `.rpm`, AppImage, tarball, Flatpak and a container image for Linux |
| Quality gates | 24 CTest suites in four build configurations; the frozen-corpus gate over 634 programs; 532 command rows run as programs and through the REPL; 67 example programs through the smoke gate; the documentation gates |

---

## Not built, on purpose

A web (WebAssembly) build, and calling one language from another. Both would have to
invent a machine that never existed, which is the one thing this environment is not for.

Two features that used to be on this list shipped and stayed: the examples browser became
**File > Examples Gallery…**, and the accessibility presets became the theme, text-size,
focus and presentation controls in **View**.

---

## Interpreter defects found by documenting every command

`time_warp_gates commands` drives one row per documented command through `time_warp_check`
and the C++ executors, from `native/share/command_rows.json`. Examining every command
turned up defects no unit test had been looking for. The fixed ones have a row of their own,
so they cannot come back silently; the section stays because the next port has to keep them
fixed.

**Nothing is open.** The last defect — a COMAL `FUNC` body that could only be declarations,
assignments and one `RETURN` — is closed: a function body now runs as statements, so `IF`,
the loops and a call to another function all work inside one, and `RETURN` leaves the body
and answers the expression that called it. `LOCAL` behaves as it has since 23.2.0: the
caller's value goes back on every exit path.

Two earlier ones are worth remembering because of what they say about the corpus. A FOCAL
`DO` group used to run to the end of the program, because there was no `RETURN`; at 23.2.0
there is one, and a `RETURN` with no group around it is reported rather than quietly
stopping the program. And at 22.4.0 a COMAL program read `mark(8)` as zero four lines after
reading it correctly inside a loop: it has never reproduced, in the program or in isolation,
and `comal/results_table.comal` reads the extremes directly now. That one is recorded here
because a symptom nobody can explain is worth more than the confidence of a gate that
happens to pass.

### Defects found while rewriting the manuals

Two shipped examples print engine errors and nothing notices, because `smoke` requires a
program to *run* rather than to run cleanly:

| Example | What it does |
| --- | --- |
| `Examples/pilot/math_drill.pilot` | Twenty-six lines use `IF … THEN <command>`, which this PILOT does not implement (its conditional is `J:Y:` / `J:N:` on the last match). Six errors are reported and the program still reports `ok` |
| `Examples/pilot/brain_trainer.pilot` | `B:` conditions at lines 397–399 fail with `Invalid expression` |

Both are recorded here rather than fixed in a documentation change, because rewriting a
shipped program's logic is a product change with its own test. The other three errors a run
of the PILOT examples reports are `typing_tutor.pilot` printing its own messages with the
error prefix as plain text, which is misleading rather than broken.

The wider point is worth keeping: the example gate checks that a program runs and draws what
it should, and it does not check that the console is free of error lines. A program that
prints an error and exits successfully is the same class of silent failure the smoke gate was
built for, one level further in.

### Engine behaviour the reference documents that is probably wrong

Writing the reference meant running every command rather than reading the old prose, and
fifteen claims in it turned out to be false. Some are documentation errors, now corrected in
the page; the ones below are engine behaviour that is more likely a defect than a design.
Each is recorded rather than changed, because each needs a decision and a test.

| What | What happens |
| --- | --- |
| BASIC `SELECT CASE` | Every `CASE` body runs; the statement reports which matched and executes them all. A multi-value clause and `CASE ELSE` raise `Invalid CASE expression` and run anyway |
| BASIC `CONST` | Does not stop assignment: `SIZE = 9` after `CONST SIZE = 5` is accepted |
| BASIC `INT` | Floors, so `INT(-3.7)` is -4 rather than truncating toward zero |
| BASIC statement separators | `WHILE … : WEND` reports `WHILE without WEND`; `FOR … : NEXT` runs two passes of three; one-line `SUB` and `TYPE` fail |
| BASIC `1e6` | Prints 0: E-notation without an explicit sign is not read |
| Logo `MAP`, `FILTER`, `REDUCE` | Exist but run their template as a command and report `Invalid numbers for SUM` |
| Logo `COUNT` | Of a multi-item list answers 1 |
| Logo `LOCAL` | Does not restore the caller's value, and `LOCAL "X` creates a name containing the quote |
| Logo quotes in a body | `PRINT "HI"` inside brackets prints `HI"`; the closing quote has to be omitted |
| Logo reporters | `PRINT` and `SHOW` evaluate fourteen named reporters; anything else is echoed |
| Smalltalk `new` | Leaves instance variables nil unless the class defines `initialize` |
| REXX `PARSE PULL` | Takes one target; a second name is silently left unset |
| PILOT `F:` | `WRITE` needs two arguments and `OPEN` needs a mode; `LINK` is unimplemented |

Three of these are the kind a learner meets in the first hour — `SELECT CASE`, `INT` and the
Logo quote rule — and each is the same shape: the language is plausible, the behaviour is
silently different, and nothing says so. The reference states what happens in every case, so
the documentation is honest in the meantime; the engine is not.

---

## Where the work goes next

There is no ordered plan for the next release, and pretending to one would be inventing
commitments. What there is:

- **the open five above**, each of which is a decision about scope rather than a task;
- **the deficiencies a gate finds**, which arrive as a failing row rather than as an idea;
- **the issues**, where a bug report with a reproducing program is the most valuable thing
  anybody can send.

The one standing invitation is a language: the shape of adding one is written down in
[CONTRIBUTING.md](../../CONTRIBUTING.md), it needs an executor, a registry row, command
rows, an example, a tutorial, a lesson and a corpus entry, and the gates will say so at each
step it is missed.
