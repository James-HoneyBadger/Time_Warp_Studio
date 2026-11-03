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
| Automatic update check | The application has no account, no telemetry and no network calls of its own. An update check would be the first one, and the packaging story — four formats and a container — makes "is there a newer version" a question the package manager already answers |
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
| Quality gates | 24 CTest suites in four build configurations; the frozen-corpus gate over 636 programs; 534 command rows run as programs and through the REPL; 67 example programs through the smoke gate; the documentation gates |

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

### Engine behaviour the reference documented that was wrong

Writing the reference meant running every command rather than reading the old prose, and
fifteen claims in it turned out to be false. Some were documentation errors, corrected in
the page. Thirteen were engine behaviour that looked more like a defect than a design, and
each was recorded rather than changed, because each needed a decision and a test.

Eleven of the thirteen were defects, and each is now fixed with a test behind it:

| What | What it did | What it does |
| --- | --- | --- |
| BASIC `SELECT CASE` | Every `CASE` body ran; the statement reported which matched and executed them all | The first clause that matches is the only one whose body runs. A clause is a value, a list, `a TO b`, `IS <comparison>` or `ELSE` |
| BASIC `CONST` | Accepted an assignment to the name: `SIZE = 9` after `CONST SIZE = 5` | Refuses it with `Cannot assign to constant SIZE` |
| BASIC statement separators | `WHILE … : WEND` reported `WHILE without WEND`; `FOR … : NEXT` ran the body three times instead of twice; one-line `SUB` and `TYPE` failed obscurely | A colon-separated line runs as a unit, so its loops repeat inside it and either spelling of a `FOR` header counts the same. A definition split by colons is refused by name |
| BASIC `1e6` | Printed 0: E-notation without an explicit sign was not read as a number | The number scanner takes the exponent in either spelling, with or without a sign |
| Logo `MAP`, `FILTER`, `REDUCE` | Ran the template as a command and reported `Invalid numbers for SUM` | Read the template as an expression with `?`, `?1` and `?2` bound, and still run a template that is commands |
| Logo `COUNT` | Of a multi-item list answered 1; of a list held in a name it counted the characters of the text `:name` | Counts the whole list, wherever it is written, and a word's characters |
| Logo `LOCAL` | Did not restore the caller's value, and `LOCAL "X` made a name containing the quote | Restores the caller's binding on return, and both spellings name the same variable |
| Logo quotes in a body | `PRINT "HI"` inside brackets printed `HI"` | The closing quote of a word form is silent |
| Logo reporters | `PRINT` and `SHOW` evaluated fourteen named reporters; anything else was echoed | Every word that reports a value is run, so `PRINT COUNT [1 2 3]` prints 3 |
| REXX `PARSE PULL` | Took one target; a second name was silently left unset | One word per name and the remainder into the last, as REXX does |
| PILOT `F:` | `WRITE` needed two arguments and `OPEN` needed a mode, so the common spelling of either wrote nothing; `LINK` was unimplemented | The mode is optional and defaults to reading, the text of `WRITE` is the rest of the unit, and `LINK` chains to another lesson |

The other two are not defects, and the reference now says why instead of listing them as
suspicious:

- **BASIC `INT` floors.** `INT(-3.7)` is -4 because that is what every BASIC of the period
  answered, and `FIX` is the word that truncates toward zero.
- **Smalltalk `new` leaves instance variables nil.** That is Smalltalk: `initialize` is the
  convention that gives them a value, and `new` sends it when the class defines one.

Reading the Logo procedure code for the manual turned up a defect the table did not
have: a name could hold a number, a word and a list at once, and each command read whichever
one it looked for first. `MAKE "X 5` followed by `MAKE "X "hello` still answered 5 to `SHOW`,
while `FIRST :X` read a list a third statement had left behind; a procedure's input did not
shadow a number the caller held under the same name; a list passed to a procedure arrived as
the text of its brackets; and `:name` as a reporter argument was read as the text of the name
rather than as its value, so `SUM :A 2` reported `Invalid numbers for SUM`. A name now holds
one value, `MAKE` and a procedure's input share one reading of what a value is, and the
reporters use it. The one recorded consequence is
`Examples/logo/fractal_forest.logo`, whose spiral now sets the pen colour on every pass
instead of only the first: the calls were dropped because the colour arrived as the text
`:CLR`, and the ink is the same either way.

One related wart is recorded rather than fixed: `FOREACH` runs a *command* body, so `?` in
`FOREACH [1 2 3] [FD ?]` is not bound, while `MAP` and `FILTER` bind it because a template
there is an expression. Binding it in a command body means rewriting the text, and a `?`
inside a quoted word would be rewritten with it, so the rule needs a decision before the
code. The same applies to `MAX 1 2` and `MIN 1 2`: a function takes its arguments in a
bracket, `MAX(1, 2)`, and a two-argument function written with a space is not read. The
reference states the form that works rather than the form a Logo book would show.

Each of these had the same shape: the language was plausible, the behaviour was silently
different, and nothing said so. `lang_tests` now asserts every one of them, and the corpus
entries whose program changed carry a note saying which side is the reference's.

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
