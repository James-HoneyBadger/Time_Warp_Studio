# Changelog

<!-- history: all -->

## [23.0.0] - 2026-10-22

The infrastructure, made modular: one home for the language table, a build that is
read one part at a time, three libraries where there was one doing twelve jobs, every
monolith in the tree split along the sections that were already commented inside it,
the two largest suites split one file per subject, and one command-line vocabulary for
the five tools. Two new gates keep each of those from growing back.

### Changed

- **One language table.** The eleven languages were described twice -- display name,
  key and extensions in `src/ui/language.cpp`, switches over the same names in
  `src/lang/language.cpp` -- and CHIP-8's `.ch8`/`.c8` were written in four files
  besides. `native/include/timewarp/lang/registry.hpp` is the one home: a constexpr
  table, header-only and Qt-free, that the executors, the Qt shell, the tools and the
  gates all read. The UI keeps its own enum (it has `Unknown`) and maps by position
  with a `static_assert`.
- **The build in parts.** `native/CMakeLists.txt` was 1,841 lines in one scope with
  the warning policy written at 37 call sites. It is 116 lines and six modules under
  `native/cmake/`, with `timewarp_enable_warnings(target)` and
  `timewarp_add_suite(name, libs…)` saying each thing once.
  `cmake --build --target help` lists the same 527 entries before and after.
- **Three libraries where there was one.** `timewarp_audio` and `timewarp_io` come out
  of `timewarp_lang`, and each language becomes an object library, so a change to the
  Logo executor compiles one translation unit: `touch logo.cpp` then a `-j1` build
  reports exactly `tw_lang_logo.dir/src/lang/logo.cpp.o`. The link structure is
  unchanged -- one `timewarp::lang` for everything that consumes the engines.
- **`main_window.cpp` (6,245 lines) is eight files**, one per section that was already
  commented in it, over `main_window_internal.hpp` for the keys and helpers they share.
- **`panels.cpp` (2,047 lines) is six files**, one per panel group.
- **`basic_statements.cpp` (2,066 lines) is seven**, and `logo.cpp` (3,548) is six over
  `logo_internal.hpp`, and `retro_computer.cpp` (2,472) is seven over
  `retro_computer_internal.hpp`. The Logo class moved from an anonymous namespace into
  a named one, because a type several translation units define methods for needs one
  definition they all agree on.
- **The two largest suites are one file per subject.** `ui_layout_tests` (4,010 lines,
  85 cases) is eight files under `tests/ui_layout/` and `lang_tests` (3,150, 191) is
  eight under `tests/lang/`, each with a `support.hpp` for what they share and a
  `cases.hpp` for the order `main` runs them in -- written down rather than discovered,
  because the cases share a `QSettings` store, a snippet catalogue and a seeded
  generator.
- **One command-line vocabulary.** `native/include/timewarp/tools/cli.hpp` gives all
  five tools `--root`, `--json PATH`, `--quiet` and `--help` in any position, a
  `cli::Command` table per tool so the usage message comes from the list that
  dispatches, status 2 for a line that cannot be used and 1 for a check that failed, and
  a `cli::Report` that prints findings as prose or as one JSON object. The documentation
  gates' own argument parser is gone with it.

### Added

- **`time_warp_gates registry`** fails if any C++ line names a language's display name
  and its key together -- a table row by another name. It found ten in its first run,
  all comments, so it reads code and not prose; a duplicate injected into `value.cpp`
  made it fail, as it should.
- **`time_warp_gates structure`** fails on a source file past 2,000 lines that is not on
  a waiver list, on a build file past 200, and on a waiver that is no longer needed --
  the list may only shrink, which is how `main_window.cpp` and `panels.cpp` left it.
- **`cli_tests`** pins that vocabulary: the two spellings of `--root`, an option after
  the command, `--`, a dangling value, the JSON escaping, and that a command's own value
  options do not come back as positionals.
- The three gates are in `make test-all`, and `structure`'s waiver list is now **empty**.

### Fixed

- `ui::languageFromKey` stays case-sensitive and `lang::language_from_key` stays
  lenient: the first version of the registry broke all 560 command rows by making the
  UI lookup accept `BASIC`, and the suite caught it before the split was believed.
- Five documents named `main_window.cpp` and `panels.cpp` after they were split; the
  claims gate refused to let a path that moved pass, and it caught
  `retro_computer.cpp` in `copilot-instructions.md` too.
- **A language test that had never run.** Splitting `lang_tests` into
  `tests/lang/cases.hpp` made the count visible: 191 cases were defined and 190 were
  called, so `test_basic_sprite_statements_refuse_a_name_they_never_captured` had been
  dead since it was written. It is registered, and it passes.

## [22.4.1] - 2026-10-22

Eight defects in the interpreters, found by writing programs rather than by reading
code, and fixed. Two of them were documented features that could not be used at all.

### Fixed

- **BASIC's `FUNCTION ... END FUNCTION` could not be called.** The definition parsed
  and the call threw `Unknown function: TWICE`, in either order. A function body is
  statements, so the expression evaluator could not run it; the machine now does --
  parameters are bound and restored, the body's lines are driven to its `END
  FUNCTION`, loops and nested `IF`s inside it work, and what the body printed is
  emitted ahead of the line that called it. Text functions (`FUNCTION GRADE$(M)`)
  answer text instead of being coerced to 0.
- **`STR$` refused an array element.** `STR$(7)` worked, `STR$(A(1))` raised `Unknown
  function: STR`: the element looked like a call to a function named after the array,
  so the string side gave up and the arithmetic side, which has no `STR$`, answered.
  `STR$(A(1) * 2)` is fixed with it.
- **`PRINT USING` printed only its first value**, and a space between two `#` fields
  was swallowed into the first one, so `PRINT USING "## ##"; 3; 7` printed `3`.
- **`EXIT FOR`, `EXIT DO`, `EXIT WHILE`, `EXIT FUNCTION` and `EXIT SUB` did not
  exist.** A function could not return early, and a loop could only be left with a
  flag variable -- in a dialect whose `FUNCTION` is otherwise complete.
- **Smalltalk's `at:`, `at:put:`, `first` and `last` were on `Array` but not on
  `Collection`**, so a program that built an `OrderedCollection` was told it "does not
  understand at:".
- **FOCAL's `;` separated commands only inside a `FOR`.** Anywhere else it reached the
  expression tokenizer as an unexpected character. Fixing it exposed a second defect
  in the fix itself -- a `;` inside a string made the splitter recurse on the whole
  statement -- which the same example caught before it left the machine.

### Changed

- Twelve command rows and 560 in the table, run as programs and through the REPL.
  `docs/LANGUAGE_GUIDE.md` documents block functions, named subroutines and `EXIT`,
  and every sample in it was run.
- `docs/ROADMAP.md` records what is still open -- COMAL's single-expression `FUNC`,
  FOCAL's `DO` group running to the end of the program -- and the COMAL array read
  that stopped reproducing and cannot be explained.
- One corpus entry, `space_simulation.bas`, now carries a divergence note: the
  recorded reference could not print a formatted table through `STR$` into an array
  element and every row was an error line, so the port's complete table is a
  difference worth writing down rather than a regression to chase.

## [22.4.0] - 2026-10-22

The examples were the part of the tree nobody had asked whether it was good. Six new
programs, in the five languages whose sets were thin, and four interpreter defects
that only writing them could find.

### Added

- **`rexx/log_analysis.rexx`** -- a web log triaged: PARSE VAR, an `ELSE IF` chain,
  padded columns, internal subroutines reading `ARG(n)`, and a verdict rather than
  just figures.
- **`focal/numerics.foc`** -- Newton's method and a summed series, a formatted table,
  and the same table drawn as columns.
- **`apl/sensor_readings.apl`** -- a week of readings: reductions for the summary
  figures, `⍋` for the ranking, a labelled report built with `⍕`, and a drawing whose
  size the mean decides.
- **`smalltalk/ledger.st`** -- an Account that refuses what it cannot pay: instance
  state, a class-side constructor, blocks, and a report built with the collection
  protocol.
- **`comal/results_table.comal`** -- a class set of marks: DATA and READ, an exchange
  sort, PROC and FUNC with parameters, a ranked table with a histogram, and a bar
  chart.
- **`basic/gradebook.bas`** -- a class report: two parallel arrays, a ranked table
  built with `FORMAT$`, and a bar chart drawn with `LINE ... BF`.

### Fixed

- **REXX's `IF ... THEN` with the statement on the next line ran the statement
  whether or not the condition held.** The shape most REXX is written in was wrong:
  `IF score > 50 THEN` / `SAY 'pass'` printed `pass` for a score of 10, and the `ELSE`
  after it was then reached as a bare statement and reported as unknown.
- **REXX's `ELSE` was never skipped after a taken branch, and `ELSE IF` was not
  supported at all.** A taken `THEN DO ... END` executed the statement after its
  `ELSE` as well, and any `ELSE IF` chain failed to parse. Both are now handled by one
  scanner that works out where the whole clause ends -- one statement, a DO block, or
  a chain -- whether it is being run or skipped. Nested IFs work, because the skips
  are a list rather than one line.
- **REXX's `LEFT` and `RIGHT` ignored their length when the text was short**, so a
  report written in columns printed as one run-together line. They now pad to the
  width, with the optional third argument, which is what the two functions are for.
- Four command rows and one REPL waiver cover the above: the next-line body, the
  `ELSE IF` chain, padding, and `ARG(n)`.

### Changed

- `make smoke` runs 69 programs (was 63) and asserts ink for 37 of them; the command
  table is 550 rows (was 546); `Examples/CATALOG.md` describes all 69.
- `docs/ROADMAP.md` records the four defects the new examples found and did not fix:
  BASIC's `FUNCTION` cannot be called at all, `STR$` rejects an array element,
  `PRINT USING` prints only its first value, and one COMAL program reads an array
  element as zero outside the loop that read it correctly. Writing them down is the
  point -- a language the IDE offers should not have a documented feature that cannot
  be used, and a known gap in a list is a gap somebody can close.

## [22.3.2] - 2026-10-22

No identifier or file in the tree is named after Python any more. `MersenneRandom`
was the first; this is the rest, and the gate that should have caught them was
matching only the extension.

### Changed

- **Every Python-named helper renamed** for what it reproduces rather than what it
  was recorded from: `python_number_text` and `python_float_or_zero` in the BASIC
  paths became `printed_number` and `float_or_zero`; Logo's `python_float_repr`,
  `python_int`, `python_str`, `python_list_repr` and `python_g` became `repr_real`,
  `repr_integer`, `repr_value`, `repr_list` and `general_text`; Scheme's
  `python_str` became `shown_text`; the Markdown linter's `is_python_space` became
  `is_whitespace_char`. Behaviour is unchecked by that difference -- the frozen
  corpus agrees on all 468 programs as before.

### Fixed

- **`pytest.ini` was still tracked**, configuring a test runner for
  `native/bridge/time_warp/tests`, deleted five releases ago. `make tree-check`
  passed it because the gate only matched `*.py`. The gate now also refuses the
  files that exist only to configure a Python tool or hold its output --
  `pytest.ini`, `tox.ini`, `setup.py`/`cfg`, `pyproject.toml`, `requirements*.txt`,
  `Pipfile`, `poetry.lock`, `mypy.ini`, `ruff.toml`, `.pylintrc`, `MANIFEST.in` and
  the `__pycache__`/`.pytest_cache`/`.ruff_cache`/`.mypy_cache` directories -- and
  it is what found `pytest.ini` the moment it was widened.
- **`.vscode/settings.json` was configuring a Python environment**: Pylance
  indexing, pylint and flake8 arguments, and a `python.defaultInterpreterPath`
  pointing at a `.venv` that no longer exists. It now holds only the build-tree
  excludes and the CMake source directory.
- **`.env.example` told the reader to generate a secret with `python -c ...`**; it
  says `openssl rand -hex 32`.
- **The language-request issue template** described the deleted bridge in the
  present tense ("the bridge runs Python implementations and the differential gates
  compare the C++ ports against them"); it now points at the frozen corpus. Two
  comments in `app.hpp` and `interpreter.hpp` described the old build in the present
  tense as well, and `app.hpp` promised a settings migration "with the theme
  system" that never happened -- it now says nothing reads the old file.
- **`native/CMakeLists.txt`'s install comment** described installing "the bridge,
  which is where `locateWorker()` looks for it" and named `python_worker.cpp` among
  the files that search a macOS bundle; it describes the payload that is actually
  installed.

What remains is prose: comments and guides that explain the semantics a port
reproduces ("Python's `int()` truncates toward zero") and the record of what was
removed. Those are citations, and they make a reader's understanding better, not a
claim that Python is here.

## [22.3.1] - 2026-10-22

`PythonRandom` was the last thing in the tree named after a language the project no
longer contains. It is `MersenneRandom` in `mersenne_random.{hpp,cpp}`: the class
implements MT19937 with a documented seeding and sampling, and the name now says
which generator it is rather than which interpreter it was recorded from.

Its comments were also making a claim the gates do not support. They said the
recorded corpus pinned the sequence; no corpus program calls `RND`, `FRAN` or
`RANDOM` at all, and the command rows assert only that those commands produce
output. The true statement is in the header now: the sequence is held by the draws
asserted in `lang_tests`, recorded before the reference was deleted, and that test
is the only thing standing between a change here and every seeded program printing
different numbers.

The sequence itself is unchanged: `lang_tests` still answers with the same three
draws for seed 1, and all 24 suites, the corpora and the gates are green.

## [22.3.0] - 2026-10-22

An audit of 19.1 to 22.2 against the tree as it now stands, and the fixes it asked
for. The releases were each honest when they landed; what had drifted since was the
*surrounding writing* -- guides still describing the reference engine as though it
were here, counts that moved, and one feature that shipped with no documentation at
all.

### Added

- **Run Project is documented.** `.twproject` format 2 added an `entry` at 22.0.0
  and no guide mentioned it: not the Projects chapter, not the file-on-disk
  section, not the API reference. `docs/USER_GUIDE.md` now covers the entry point,
  the ▶ marker, `Ctrl+Alt+R`, the resolution of relative file names against the
  project root, and what happens when the entry's file leaves the project.
- **`project.info` reports `entry`**, as an absolute path or null, and there are
  `project.setEntry` and `project.clearEntry` methods. The Projects dock grew that
  field and the automation surface did not, so a build script could not ask which
  program a project means. Asserted in `api_tests`.

### Fixed

- **`executor_for()`'s Scheme comment named the Python bridge**, which was deleted
  at 18.0. Its present-tense claim ("still driven by the Python bridge") was the one
  place in the tree that said a component existed. Scheme is the explicit-stack
  machine, and the comment now says so. The same false claim was corrected in
  `executor.hpp`, `main_window.hpp`, `turtle_canvas.hpp`, `interpreter.cpp` and
  `native/README.md`, whose "answers for all eleven" was wrong in the other
  direction: it answers for ten and returns null for Scheme.
- **`native/CMakeLists.txt` still introduced itself as the build "for the native
  C++20 expression engine extracted from the Time Warp Studio Python interpreter"**,
  and described the language library as waiting for the bridge.
- **`native/README.md`'s tools tree listed seven deleted Python scripts** and none of
  the C++ tools that replaced them, and its example counts said 60 and 30 where the
  gate finds 63 and 33. The `.github/RELEASE_CHECKLIST.md` said 60 too.
- **Stale counts across the guides**: 21 suites in `SUPPORT.md`, 23 in
  `INSTALLATION.md`, `README_TESTING.md` and `native/README.md`, "ten languages" in
  three places, and the ROADMAP's gates row still advertising "575 engine tests, 448
  command rows, 470 parity programs" for a toolchain that has been replaced. Every
  figure is now the one the gates print: 24 suites, 546 rows, 63 examples, 662
  corpus programs.
- **`docs/PORTING_PLAN.md`'s "Gates, per phase"** listed ten commands, five of them
  Python scripts deleted at 22.1.0, and told the reader every phase must leave them
  green. The block is now marked as the record it is, pointing at `make test-all`.
- **`.github/workflows/version.yml` watched `Scripts/`**, deleted at 22.1.0, so a
  change to the version tool that lives in `native/tools/docs/` did not trigger the
  job that checks the version. `DEPLOYMENT.md` documented the same ghost.
- **`CONTRIBUTING.md`'s language recipe** still named `core/language_registry.py`,
  and `.github/copilot-instructions.md` still listed ten languages and pointed the
  turtle state at `graphics/turtle_state.py`.

### Changed

- **The canvas vocabulary gate derives its expected count.** The test that asserts
  every declared command draws compared `kEveryKind` against a hand-written `43`, so
  adding a kind to the enum and forgetting the list passed silently -- the exact
  failure the gate exists to catch. It now walks the enum until `to_string` stops
  naming one, and a removed kind fails the test (proved by removing one).
- **`make test-all` runs `lint-md`**, which was a gate in CI and the one check a
  local `test-all` skipped. `tree-check` is declared phony, and the help text names
  every gate, `repl-matrix` included.

## [22.2.0] - 2026-10-22

The Markdown lint is the project's own, and it found a bug in the shared scanner.

`make lint-md` was the last check in the tree that needed a toolchain from outside it:
`npx --yes markdownlint-cli2` downloads Node and a package on every run. On this
machine there is no `npx` at all, so the target had never run — which makes the
`docs/DEPLOYMENT.md` claim that "`make lint-md` reports nothing" a claim about a
report nobody had seen. It has now run, and it had things to say.

### Added

- **`time_warp_docs md`** — the Markdown lint as C++, in the documentation-tools
  project that needs a compiler and nothing else. Twenty-eight rules from
  markdownlint's set: heading increment, style, spacing, single-title and trailing
  punctuation; list marker style, indentation, marker spacing and numbering; blank
  lines around headings, lists and fences; fence language and fence character;
  trailing whitespace, hard tabs, multiple blank lines and the file's own ending;
  bare URLs, inline HTML, table pipe style and column counts. The rules it does not
  implement are listed in `md.hpp` with a reason each.
- **`md --explain`** prints how every line was read — heading, list item with its
  indent and marker spacing, fence, table row — which is what to reach for when a
  rule disagrees with a reader.

### Fixed

- **A line was split in the middle of an emoji.** The shared `split_lines` treated a
  bare `0x85` byte as Unicode NEL, and `✅` is `e2 9c 85`: every bulleted line in
  the guides was being cut in two, before the first character of its text. The links
  and claims gates share that scanner, so this was not only the new rule's problem —
  it is why a heading whose text began with a tick-mark could be read as a list item
  and a list could look like a paragraph. Only a decoded multi-byte character can be
  a line boundary now.
- **Markdown that was actually wrong**: two unlabelled code fences in
  `native/tools/docs/README.md`, and in `native/README.md` a sentence that had been
  chopped in half in `84c65b0` — `...runInBridge has no runtime case` in one
  paragraph and the orphaned `left; adding an executor is still the whole change a
  new language needs.` fifty lines later, with the audio and ports bullets in
  between. Rejoined, and the chip8 paragraph that still described the deleted
  reference engine now describes the corpus.

### Changed

- **The markdown-lint CI job is now a gate.** It was one `super-linter` invocation
  with `continue-on-error`, which could not fail a pull request and did not run at
  all without Node. It builds `time_warp_docs` and runs `md`, so it can fail, and it
  does so on the project's own rules rather than a third party's. The spell check
  stays advisory and moved into its own job.
- **`.markdownlint.json` is gone.** The rules live in `md.cpp`, one statement of
  them rather than a config file and an implementation that could drift.

## [22.1.0] - 2026-10-22

No Python. The last five scripts are C++, and the tree holds none at all — which is
a claim a gate now enforces rather than a claim a reader has to trust.

### Added

- **`time_warp_docs`** (`native/tools/docs/`) — the two documentation gates and the
  version tool as one dependency-free C++ project: `links`, `claims [--self-test]`,
  `version [--write]`. Its own CMake project with no Qt, so the documentation job
  still answers in seconds while building it with a compiler instead of needing an
  interpreter. The claims gate's nine self-test cases and its Makefile/`$(FORWARDED)`
  parsing came across whole.
- **`time_warp_icon`** (`native/tools/icon/`) — the icon generator in C++, zlib only.
  It reproduces the three packaged icons **byte for byte** (md5 identical for the
  PNG, ICO and ICNS), which is the test: a generator that draws the same picture
  differently would churn the committed assets on every run. `--check` compares what
  it would write with what is on disk, so a release can assert the icons are current.
- **`time_warp_gates tree`** — fails if any tracked file is a `.py`. The removal
  needed a gate for the same reason every other claim here has one: a deletion that
  nothing enforces grows back.

### Removed

- **Five scripts and their scaffolding**: `check_doc_links.py`,
  `check_doc_claims.py`, `version_sync.py`, `generate_icon.py`, `twc.py`, plus
  `native/tools/pyproject.toml` and the `.venv`/`__pycache__`/`pytest`/`ruff`
  ignore rules and the ruff pre-commit hook. Twelve scripts and 4,648 lines in
  total across 22.0.0 and this release.
- **`./twc` is a wrapper around the IDE's own compiler.** The Python script
  generated a C++ runner and compiled it; `time_warp_ide --compile` had grown the
  same feature natively, so the script was a second implementation of one thing.
  `./twc program.bas -o name` still works, and `--output` works too.

### Changed

- **The documentation jobs build a tool instead of needing an interpreter.**
  `docs.yml`, `version.yml` and `release.yml` each configure and build
  `native/tools/docs` — one compiler, no Qt, no network — and then run it. The
  stated reason those jobs were Python-only was "it must not depend on the C++
  build"; that is still true, and it is now a C++ tool the size of one directory.
- **`make lint` is the build.** Ruff linted the harnesses; there is nothing left for
  it to lint, and every target is compiled `-Werror`, so the target now says that
  rather than shelling out to a tool that is not installed.

### Notes for a reader checking the port

- Both ports were checked the way the harness port was: old and new run on the same
  tree, output compared, then the new one made to fail on purpose. The documentation
  tools agree on all four commands (`455 checked, all resolve`; `58 files, no false
  claims`; `self-test: 9 cases`; `All 2 hand-written copies match`), and on a scratch
  document with a dead link and a moved path both report the same two failures in the
  same order. The icon generator was checked by hash, and `--check` was made to fail
  by changing one byte of the committed PNG.
- The doc-tools port found two of its own bugs in the process — a link scan that did
  not skip fenced blocks, and a trailing `/` in a target — both fixed against the
  Python's behaviour rather than against a guess.

## [22.0.0] - 2026-10-22

The harnesses are C++, and a project runs as a project. Two sentences, both about
the same thing: the tree should not need a language the product does not use, and a
folder of programs should be something you can run rather than something you can
only browse.

Four thousand lines of Python ran this project's most important checks, and the
checks were the reason the tree still needed an interpreter: a contributor could not
run `make command-matrix` without one, and the CI jobs that answer "does every
example still draw" depended on a language the product does not use.

### A project runs as a project

- **`.twproject` has an entry point** (format 2, and a format 1 file still loads).
  **Project > Set Current File as Entry Point**, the dock's ▶ **Run** button, the
  **Run Project** action (`Ctrl+Alt+R`), and the entry marked with ▶ in the tree. A
  project that names an entry it does not list adds it on load, and removing the
  entry's file clears the entry: what Run Project runs is always a program the dock
  shows.
- **A run's file fence is the project, not the launcher's directory.** Until this
  release `OPEN "notes.txt"` in BASIC wrote beside whatever directory the IDE was
  started from -- the repository root, or `$HOME` from a desktop launcher. A run now
  confines relative names to the project's root when one is open, else to the
  document's own folder, and only falls back to the working directory for an unsaved,
  unprojected file. `time_warp_check` and the corpora are unchanged: no base set
  means the process's directory, as before.

### Added

- **`time_warp_gates`** — one binary, four questions, built with the product:
  - `smoke`: every `Examples/**` program through the native engines, with the ones
    that must produce ink named, so "it ran and drew nothing" still fails.
  - `commands`: all 546 command rows, one program each, judged on output, drawing or
    a reported error, then the coverage report against the commands the executors
    actually dispatch on.
  - `repl`: the same rows through `repl.eval`, one `time_warp_api` process, with a
    restart on a wedged request so one broken row cannot hide the rest.
  - `golden`: the ports against the frozen reference corpora, gzipped corpus included
    (zlib directly, because gzip framing is not the zlib wrapper `qUncompress` reads).
- **`native/share/command_rows.json`** — the command table as data: 546 rows, the
  one waiver, and the rules a row follows. It was `command_table.py`, which was
  data wearing a Python extension.

### Removed

- **Seven scripts**: `harness.py`, `example_smoke.py`, `command_matrix.py`,
  `command_table.py`, `command_inventory.py`, `repl_matrix.py`, `golden_check.py`.
  The `make` targets keep their names — `smoke`, `command-matrix`, `repl-matrix`,
  `golden-check` — so every guide that names one still points at something real.

### Notes for a reader checking the port

- The port was verified by running both sides on the same tree and comparing the
  text: the example corpus agrees to the stroke (63 programs, 5,712 strokes in
  BASIC and every other count identical), the command matrix agrees to the row and
  to every coverage figure (26/63/0/4/9/0/15/7/0/81), the REPL matrix agrees on all
  546 rows, and `golden --all` agrees on 468 language programs and 168 Scheme ones.
  Two differences were found and fixed during that comparison: the example corpus
  counts ink alone (a turtle `RIGHT` is activity, not ink), and Python's float
  formatting turns the colour `#8E9092` into `inf` — a mangling the corpus was
  recorded through, which `QString::number` did not reproduce.
- Each new gate was then made to fail on purpose, to prove it can: a row given the
  wrong expected text, and a corpus entry given the wrong recorded answer. Both were
  caught, with exit status 1, and both files restored byte-identically.

## [21.0.0] - 2026-10-22

The eleventh language, and a palette that means one thing. Forth is a whole language
in about a thousand lines, which is the reason it was invented; the palette fix is
the kind of defect that hides behind a passing test, and it was found by drawing with
the new language.

### Added

- **Forth.** The eleventh language, and the first stack machine in the studio: a data
  stack, a dictionary, colon definitions (`: SQUARE DUP * ;`), the three control
  structures (`IF/ELSE/THEN`, `BEGIN/UNTIL`, `BEGIN/WHILE/REPEAT`, `DO/LOOP`/`+LOOP`
  with `I`, `J`, `LEAVE` and `EXIT`), `VARIABLE`/`CONSTANT`, 64 KiB of data space
  with 16-bit `@`/`!` and byte `C@`/`C!`, `RECURSE`, and the turtle words. It runs
  whole-program — a definition spans as many lines as it likes — and it is registered
  in every table a language has to be in: the engine enum, the IDE's language table,
  the highlighter, the linter's and outline's "no per-language checks yet" cases, the
  snippets, the tutorial, the ROM socket (`V1.0`, prompt `ok`, note `STACK LANGUAGE`)
  and the curriculum.
- **`POKE` and `PEEK` in Forth**, which reach the *machine*: `65 32768 POKE` writes
  the first character cell of the HB-1's tube, and a write into ROM is refused out
  loud. With no machine attached the same words read and write a map of their own, so
  a poke and a peek still agree — the same rule the interpreter's other languages
  follow.
- **Two Forth lessons** (`forth_first_words`, `forth_defining`), three checkpoints
  each, so the curriculum is 22 lessons and 66 checkpoints and every language is still
  taught.
- **Three Forth programs** under `Examples/forth/`: `hello.fs` (stack, definitions,
  recursion, a loop), `turtle.fs` (three figures in three colours) and `showcase.fs`
  (the language's whole surface, ending in a drawing).
- **86 command rows for Forth**, one per word, plus the inventory pattern that reads
  Forth's `{name, Primitive}` table — without it, ninety percent of the language was
  invisible to the coverage report. `command_inventory.py` now sees 74 command-shaped
  words and 0 are untested.

### Fixed

- **A palette slot meant a different colour in every language.** `SETPC 1` in Logo, a
  bare `1` in BASIC and `1 SETPC` in Forth all name palette slot 1, and the reference
  seeds sixteen colours before a program runs. The IDE's host seeded three of its own
  — white, near-black, pale green — and answered grey for the rest, so `SETPC 1` drew
  in near-black on a dark canvas: **ink that is there and cannot be seen.** The table
  is now written once (`reference_palette_colour` in `turtle_host.hpp`), the three
  hosts and BASIC all ask it, and the Logo test that pinned `#101014` now pins
  `#0000AA` with a comment saying why.
- **`PALETTE CYCLE` did not rotate the palette the engine resolves against.** It sent
  the rotation to the canvas and left the host's copy alone, so a `SETPC 4` after a
  cycle named the colour slot 4 used to mean.
- **`BYE` did not end the program.** It cleared the loop stack and returned, so every
  word after it still ran. It now stops between words and unwinds the call frames.
- **`LEAVE` inside an `IF` failed to compile.** It looked for a `DO` on the top of the
  control stack, which an `IF` had hidden — the shape every real loop leaves in.
- **`." hello"` printed ` hello`.** The space after the quote is Forth's delimiter,
  not the first character of the text.
- **`IMMEDIATE` was only accepted while compiling**, so the ordinary
  `: F 1 ; IMMEDIATE` — the form a book shows — was an undefined word.

### A gate that says what it does not know

- The golden corpus is what the *reference engine* answered, and the reference never
  had Forth, so a ported language with no recorded programs used to look covered by a
  clean run. `golden_check.py` now prints `note forth: no recorded corpus -- this gate
  says nothing about it; the command matrix and lang_tests do`. A blind spot is stated
  rather than implied by silence.

## [20.2.0] - 2026-10-22

The machine has a clock, a voice and a switch. Three things a window cannot do: keep
time, make a noise nobody asked for, and be *off*.

### Added

- **`MachineVoice`** — the five sounds an HB-1 makes by itself: a two-note chirp at
  power-on (and at a ROM change), a click per prompt line, the disk head stepping for
  every DOS word, a buzz once per failed run, and the tape warble waiting for 20.4.
  Built from the same `Mixer` a program's `SOUND` uses, with a **volume and a mute in
  CONFIG**. A muted machine still counts what it did: `count(Event::Boot)` is a fact
  about the machine, `sounded()` is a fact about the speaker, and the tests assert both.
- **The sound latch** at `$A005`: `POKE 40965, 30` and the machine plays 340 Hz for
  80 ms on the next frame, then clears the register itself. That is all a
  one-register beeper could ever do, and it is enough to play a scale.
- **`HbClock`** — sixty frames a second, the frame counter in the I/O page
  (`$A006`/`$A007`), and the latch serviced once a frame.
- **A power-on self-test** that walks the free RAM it claims to have and sums every
  ROM's version and banner note, printing what it found: `32768 BYTES FREE`,
  `RAM CHECK OK (128 CELLS)`, `ROM CHECK OK (10 ROMS, 3078602950)`. It runs when the
  start-up message is **Full**, because Short and Off are promises about how much the
  machine says.
- **The POWER lamp is a switch.** Off is off: the tube goes dark, the prompt stops
  answering, the clock stops and a run is stopped with it. On is a cold boot.

### Fixed

- **`SLEEP` did not sleep.** It printed "ℹ️ SLEEP 0.1s" and returned — the same
  acknowledged-nothing defect `LOCATE` and `SPRITE` had, and the reason a program that
  wanted to wait could not. It now really waits, clamped to sixty seconds, in
  two-millisecond slices so **a stop request breaks the wait**.
- **`ON TIMER` never fired.** `ON TIMER(n) GOSUB line` stored nothing, and `TIMER ON`
  set a flag no code read. The interval and the handler are now kept, the handler runs
  at a statement boundary — never inside a statement, never inside itself — and a
  handler that is not in the program switches the timer off instead of failing every
  statement.
- **The self-test wiped the two lines it had just printed** by clearing memory after
  the RAM walk. It now tests first and prints afterwards, and puts the RAM back the way
  it found it.

### Deferred, with the reason

**`CONT` is not in this release.** A real `CONT` needs the run to *pause* at a
statement and resume with its control stacks intact, which is `DebugSession`'s
machinery — and the machine has no debug session attached at all. Attaching one is the
same work as 20.3's `MONITOR`, where it also buys a machine-code debugger. A `CONT`
built out of "run it again from the next line" would lose a `FOR` frame and be a lie in
the one case that matters.

## [20.1.0] - 2026-10-22

The 64K is real, and the screen is memory. Until this release `POKE` wrote into a map
nothing read, `LOCATE` validated its arguments and returned, and `MEM 64K` on the
status line was a string literal: a machine whose memory is inert is a costume with a
keyboard.

### Added

- **`HbMemory`** — one 64 KiB address space with a documented map: 32K of free RAM,
  2000 screen cells at `$8000`, a nibble pair of ink and paper per cell at `$9000`, a
  page of I/O registers at `$A000` (cursor, border, ink, paper, sound latch, frame
  counter, scrolled flag), ROM at `$F000` where a write is refused out loud. A scroll
  moves *cells*, so `PEEK` of the top row after a scroll answers with what is on the
  screen.
- **`HbTube`** — the machine's screen, painted from that memory: a cell grid, the
  phosphor's brightness ramp for the sixteen palette numbers, a blinking cursor, the
  border register as a bezel, a scrollback (wheel, `Ctrl+PageUp`) and a translation
  table. The SCREEN page replaces the old OUTPUT log; the PROGRAM page is unchanged.
- **The sixteen graphics characters** at codes 128–143 — solid block, halves,
  quadrants, checkerboard, three rules and three bars — as eight bytes of bitmap
  each. A font cannot draw those, and they are how a machine with only a text screen
  drew a picture: `POKE 32768, 128` is a solid block.
- **`Interpreter::MachineBus`** — five hooks (peek, poke, cursor, ink/paper, clear
  screen). Unset, every statement keeps the sparse map it has always used, which is
  what the IDE, `time_warp_check` and the parity corpora see: a language gained
  hardware without a recorded answer changing.
- `POKE`, `PEEK`, `LOCATE`, `COLOR` and `CLS` now mean what they say on the machine.
  `MEM` on the status line reports the free RAM the map actually has (32K) instead of
  a badge.

### Fixed

- **`POKE` and `PEEK` took literals, not expressions.** `POKE 32768 + 4 * 80 + 5, 65`
  was an error on every machine that ever had a POKE; a program that computes the cell
  it writes is the normal case, not the exotic one.
- **`--machine` could load an image and then run the *previous session's* program.**
  The editor's text was written back into the ROM's memory *after* the image filled it,
  so the session's program won. Found by screenshotting the machine rather than by a
  test, which is why the release also asserts the round trip.
- **The 19.3 manual had the screen-mode numbers backwards** (`mode` is the machine's
  `ScreenMode`: 0 TEXT, 1 GRAPHICS, 2 SPLIT). Corrected in the manual, and the image
  round-trip test pins it.

### Fixed (second pass, found by looking at the screen)

- **Program output could be written after the `POKE`s that followed it.** Output
  reached the tube through a queued GUI callback while `POKE` wrote memory from the
  engine's thread, so a program's `CLS` message landed on top of the cells it had
  just poked. The output now goes through the bus *on the engine's thread, in order*,
  and the queued signal only fills the scrollback -- which is what a machine's
  teletype did: print when the program says it.

### Anachronisms, stated

- **The scrollback.** A 25-row tube of 1982 ate everything above it. This one keeps the
  rows it has written, because a learner reading a listing needs them, and the manual
  says so rather than pretending.
- **A translation table.** The tube has 256 character codes and no glyph for an emoji,
  so an engine's message prefix is dropped and the banner's typographic dash becomes a
  hyphen -- exactly what a machine of the period did with a character it could not
  spell.

## [20.0.0] - 2026-10-22

The HB-1's line editor, which is the part of a 1982 BASIC people actually spent their
evenings in. The machine had numbered lines and nothing else: no `AUTO`, no `RENUM`,
no ranges, no trace, and a `NEW` that was one keystroke of regret.

This is the first release of **HB-1 2.0** — the series that makes the machine's insides
load-bearing rather than decorative. See `docs/ROADMAP.md` for the ladder and
`docs/hb1/inside.md` for what the machine is made of.

### Added

- **`AUTO [start[, step]]`** — the next line number is *in the prompt*, so the line a
  reader types already carries it and the ordinary numbered-line path stores it. AUTO
  therefore needs no second parser, and the prompt looks the way a machine of the period
  looked. An empty line ends it; with no argument it continues after the listing.
- **`RENUM [start[, step]]`** — renumbers the listing **and every jump in it**: `GOTO`,
  `GOSUB`, `THEN`, `ELSE`, `RESTORE`, the two-word `GO TO`/`GO SUB`, and the list after
  `ON x GOTO 10, 20, 30`. A comment or a string literal that looks like a jump is left
  byte for byte, and a target that names a line the program does not have is left alone
  and **counted** — it was already a runtime error, and rewriting it would have made it a
  different one.
- **`DELETE a-b`** (`DELETE 20`, `DELETE 20-`, `DELETE -30`) and **`LIST a-b`**.
  `DELETE "NAME"` is still HB DOS's, because a file name is in quotes and a range is not.
- **`UNDO`** — `NEW` now keeps the listing it erased, once, and says so. A machine of the
  period said nothing and meant it; a learner losing an afternoon to a two-key mistake is
  not a lesson worth teaching.
- **`TRON`/`TROFF`** — each statement's line number is printed as the program reaches it,
  through the same output stream as `PRINT`, so a trace and a result interleave.
  `Interpreter::set_tracing` is the engine half: one flag, no second channel and no second
  thread, and the IDE can have a trace panel later for the same price.

### Fixed

- **`RESTORED 0 LINE(S)`** in the first draft of `UNDO`: the count was taken from the
  PROGRAM page, which is empty by definition at that moment, instead of from the listing
  being put back. The test asserts the number, which is what caught it.

### Note on the series

HB-1 2.0 lands as **20.0–20.2** first — the editor, then real memory and a
memory-mapped screen, then the clock, the voice and the power switch — before the V20
workshop items (Forth, i18n, the quality floor, packaging, the documentation book) move
to 21.0. The machine work continues afterwards with the processor, the tape, the printer
and the second drive.

## [19.3.0] - 2026-10-22

The HB-1 is a file you can hand to somebody.

Its configuration was in QSettings, its disk in `~/.time_warp/disks` and each ROM's
program in the settings too — the right shape for a machine you own and the wrong one
for a machine you give away. That is now one document: the ROM it is left in, the
screen, every ROM's program, the phosphor and configuration, and the volume.

### Added

- **`SAVE MACHINE "NAME"` and `LOAD MACHINE "NAME"`** at the machine's prompt (also
  spelled `SAVEMACHINE`). A bare name is `<data dir>/machines/<NAME>.hb1`; a name
  ending in `.hb1`, or a path, is used as given. Saving reports how many programs and
  how many files went into the image, because a save that wrote nothing and said "OK"
  is the failure that keeps recurring here.
- **`time_warp_ide --machine <file.hb1>`** boots one and raises the machine: the ROM
  socket, the screen mode, the programs, the phosphor and the disk, with no IDE
  document around it.
- `MachineImage` (`native/include/timewarp/ui/machine_image.hpp`): the format, its
  versioning, and `resolvePath`. `HbDos` gained `toJson`/`fromJson`, and
  `RetroComputerSettings` gained the same pair, so the field list lives next to the
  two functions that already own it rather than in a third place.
- The CONFIG help page lists the two new words, and `docs/hb1/README.md` documents the
  format with an example a reader can edit by hand.

### Fixed

- **`SAVE MACHINE "HV"` wrote `MACHINE.hb1`.** The argument reader starts after the
  first word, so the two-word spelling handed `MACHINE` to the file name and the name
  that was typed went nowhere. The test asserts the file exists at the path the name
  asks for, which is what caught it.

### Refused, deliberately

- A machine image from a **newer** build is refused with both format numbers in the
  sentence rather than half-read. A machine that silently dropped the fields it did
  not understand — and then saved itself back over the file — would be worse than one
  that said no.
- `time_warp_player --machine` is **not** in this release. The player links
  `timewarp::ui_core` and not the IDE's window layer, which is where the HB-1 lives;
  booting a machine there would have meant a 2.3 MB artifact becoming 4.5 MB again or
  a machine subsystem split out of the UI. The IDE takes `--machine` instead, so the
  feature exists and the player stays the size it was.

## [19.2.0] - 2026-10-22

Every command the canvas declares, a program can now ask for — and three statements
that said they had done something now do it.

### The claim

The canvas has drawn fills, gradients, bezier curves, sprite objects and palette
cycling since 19.0, and until now exactly one of those had a word in any language.
A feature no program can reach is not a feature, it is a field in a struct. This
release closes the gap from the other side: `BEZIER` for Logo, `SPRITE`,
`SPRITEMOVE`, `SPRITESHOW`, `SPRITEHIDE`, `GRADIENT` and `ELLIPSEGRADIENT` for BASIC.
The vocabulary walk stands at **43 declared kinds, 43 drawn, no waivers**.

### Added

- **`BEZIER cx1 cy1 cx2 cy2 x y`** in Logo: a cubic curve from where the turtle
  stands, ending with the turtle on the curve's end point facing along its final
  tangent, so two of them joined make a smooth curve rather than a corner. The pen
  is lifted for the reposition — a curve with a straight line across it is not the
  curve that was asked for.
- **`SPRITE name, x, y, w, h`** captures a rectangle of the screen as a named
  bitmap; **`SPRITE name, x, y`** places or re-places it, **`SPRITEMOVE name, dx, dy`**
  moves it by a delta, **`SPRITESHOW`/`SPRITEHIDE`** show and hide it, and
  **`SPRITE name, OFF`** releases it. The hotspot is the captured block's top-left
  corner, which is the corner `PUT` places a block by — so a captured sprite goes
  back exactly where it came from.
- **`GRADIENT x1, y1, x2, y2, first, second [, angle]`** and
  **`ELLIPSEGRADIENT x1, y1, x2, y2, first, second`**: a rectangle and an ellipse
  filled across themselves.
- **`SETPC n`** with a bare number sets the pen to that palette slot, which is what
  the reference's own examples mean by it.

### Fixed

- **`SPRITE` printed "🎮 Sprite SHIP at (10, 20) size 30x40" and drew nothing.** A
  statement that acknowledges work it did not do is the exact failure the example
  smoke test exists for, and this one had been in the command table passing its row
  for several releases, because the row asked whether the statement *said*
  something. It now captures, places, moves and hides.
- **`SETPC 1` changed no colour at all.** A bare number fell down a branch that
  accepted it and returned; the pen stayed white and the picture came out wrong.
- **A `PUT` larger than one queue chunk was drawn in the wrong place.** The host
  subtracted each chunk's row offset instead of adding it, so every chunk after the
  first was stamped on top of the first. Blocks under 16k pixels — every small
  sprite, every test — looked perfect. `test_a_large_pixel_block_is_placed_where_it_was_asked_for`
  fails against the old arithmetic and passes against the new.
- **Moving, showing or hiding a sprite did nothing in screen mode.** The display
  list moved and the picture did not: the bitmap path had cases for defining and
  placing a sprite and none for the other three. Each placement now remembers the
  pixels it covered, so hiding one puts them back rather than leaving a hole.

### Removed

- **`CanvasCommandKind::ClearSvgaMode`**, for the same reason `Pitch`/`Roll` went in
  19.1: nothing emitted it. `Reset` already returns the surface to fitting its ink
  and clears the fixed extent with it, so a second way to say so was vocabulary
  nothing could ask for.

### Notes

- The golden corpus keeps the **reference's** answers, and `SPRITE` is the one place
  the port deliberately differs from them: the two affected rows carry a
  `divergence` note saying why. The check fails on a stale note — if the port ever
  agrees with the reference again, the note must be deleted rather than left to
  misdescribe the code.
- The command table's `SPRITE` rows had to grow a `SCREEN 1` line, because capturing
  a bitmap needs a bitmap to capture from. 460 rows, all of them behaving.

## [19.1.0] - 2026-10-22

The vocabulary closes: **44 declared commands, 44 drawn, no waivers**. The last two —
`Pitch` and `Roll` — were deleted rather than implemented, because the third dimension
turned out to belong to the host and not to the canvas; and the two things the canvas
learned in 19.0 that a program could not reach are now reachable.

### Removed

- **`CanvasCommandKind::Pitch` and `Roll`.** The 3-D turtle already works: `TurtlePose`
  holds the orientation basis, rotates it on pitch/roll, and **projects before it
  speaks**, so what the canvas receives is two-dimensional geometry and always was.
  Two kinds that no host could emit had been in the vocabulary since the port, and
  every consumer dropped them. Deleting them is the fix; drawing them would have been
  a second camera. The waiver table in `turtle_canvas_tests` is now empty, which is a
  stronger claim than a table with two honest excuses.

### Added

- **`PENSTYLE n [, length]` in Logo** — 0 solid, 1 dashed, 2 dotted, 3 dash-dot,
  4 invisible — with a command-table row, a guide section and a test that drives it
  through `NativeCanvasHost` and compares the *pixels* against a solid line. A native
  extension rather than a reference command, said so in the guide: the canvas could
  draw a dashed pen from 19.0 and nothing could ask for one.
- `TurtleHost::set_pen_style` and `cycle_palette`, default no-ops so a host with no
  palette is not wrong to ignore them; `NativeCanvasHost` and `RecordingHost` override
  both.

### Fixed

- **`PALETTE CYCLE` in BASIC moved a palette nobody was looking at.** The statement
  rotated the engine's own raster palette and never told the canvas, so the printed
  acknowledgement said "Palette cycled" and the picture did not change — the exact
  shape of a statement that does nothing. `turtle_.cycle_palette()` now emits
  `CyclePalette`, and `PALETTE i, r, g, b` already reached the canvas. The
  command-table row passed throughout, because it asks whether the statement *said*
  something.

### Still open, and stated

The other five kinds the canvas learned in 19.0 — gradients, bezier curves, sprite
objects, fills from a program, and the virtual screen — still have no language syntax.
`FILLED` in Logo reaches the fill path, and that is the one that exists in the
reference. Inventing five more statements (in which languages, with what argument
shapes) is a design decision worth making deliberately rather than as the tail of a
release, so it is written here rather than guessed at.

## [19.0.0] - 2026-10-22

The canvas now draws everything the vocabulary declares. Of the 46 commands the
command stream can carry, 28 were drawn and **18 fell into a `default:` that counted
them and dropped them** — fills, pen styles, bezier curves, gradients, sprite objects,
palette cycling and a virtual-screen mode. There were makers for exactly one of the
eighteen, and no engine emitted any of them, so nothing was broken *today* and nothing
could be built, either: the vocabulary made a promise the surface could not keep, and
no gate noticed.

### Added

- **Sixteen of the eighteen now draw.** Fills (`BeginFill`/`EndFill` as the
  reference's polygon walk, not a paint bucket, plus `FillLastShape`), pen styles
  (dashed, dotted, dash-dot and an invisible pen that leaves no ink *and* no history
  entry), cubic and quadratic `BezierCurve`, rectangle and ellipse **gradients** with
  the angle honoured, **named sprite objects** (`DefineSprite` carries a pixel grid and
  a hotspot; `PlaceSprite`, `MoveSprite`, `ShowSprite` and `HideSprite` act on the
  newest placement), `SetPaletteColor` plus **`CyclePalette`** — which recolours every
  stroke using a slot, the effect a C64 border is made of — and `SetSvgaMode` as a
  fixed world extent instead of a fit to the content.
- **Makers for all of them** (`make_fill_begin`, `make_rect_gradient`,
  `make_sprite_place`, `make_cycle_palette`, …), because a kind with no way to be
  constructed is a declaration, not a command. The sprite's *name* travels in the
  trailing text, like a `DrawText`'s string, so the 72-byte record stays fixed.
- **The gate: a vocabulary walk** (`turtle_canvas_tests`). It enumerates every kind,
  pushes a minimal command of that kind through the real widget, and requires it to be
  **drawn** or named in a `kNotDrawn` table with a written reason. The count is pinned,
  so a command added to the vocabulary fails the suite until somebody says, in writing,
  whether it draws. Two kinds are waived: `Pitch` and `Roll`, the 3-D camera, which is
  workstream W19.2.
- **A pixel test for fill colour**, because "a shape exists" was already true.

### Fixed

- **A filled polygon was painted in the pen's colour, not its own.** `FILLED "red"
  [fd 100 rt 90 …]` carries the fill in `b` beside the flag that says there is one, and
  the painter filled with the stroke — so every filled Logo polygon was one colour.
  The command-table row passed throughout, because it asks whether the command *did*
  something, not what colour it did it in. Both paint paths now use the requested fill,
  and the new test fails against the old line.

### Changed

- The raster path (a program that names a `SCREEN`) learns the same shapes, so a screen
  program is not silently lossy where a drawing one is not: gradients, ellipses, sprite
  definitions and placements, and filled polygons all reach the framebuffer.
- `reset()` clears the pen style, the palette, the sprite definitions and the virtual
  screen, so a program cannot inherit the previous one's dash pattern or palette slots.

### Still open, and stated

- **The engines do not emit the new kinds yet.** There is no `PENSTYLE`, `GRADIENT`,
  `BEZIER` or sprite-object syntax in any of the ten languages, and `TurtleHost` has no
  methods for them. Exposing them is the next slice: it touches `TurtleHost`, three
  languages' parsers, the guide, the command table and the corpus. What this release
  fixes is the part the surface was lying about.
- `Pitch`/`Roll` stay undrawn, in `kNotDrawn`, until W19.2.

## [18.1.0] - 2026-10-22

A program now leaves the workshop as a program. `--compile` writes a copy of
**`time_warp_player`** — the canvas, the runner, the sound and the palette, and nothing
else — instead of a copy of the IDE with the authoring UI hidden at start-up. 2.3MB
against 4.7MB, and the artifact opens one program rather than a workshop with the menus
switched off.

The version after the engine deletion rather than part of it, for the reason the two
are the same argument: an artifact is a copy of a player that links the engines the
product ships, and there is no longer a second interpreter for it to compete with.

### Added

- **`time_warp_player`** (`native/tools/player.cpp`): a program leaves the workshop as
  a program. `--compile` writes a 2.3MB copy of it rather than a 4.7MB copy of the IDE
  with the authoring UI hidden at start-up, and the artifact opens in one window — the
  drawing, its output, `R` to run again, `F11` for the whole screen, `Esc` to leave —
  with the author's name on a title card until the program draws. It links
  `timewarp::ui_core` — the canvas, the runner, the sound and the palette — and not the
  shell. No editor, no docks, no REPL bar, no SQL, no terminal, no session restore: a
  player with an editor in it is an IDE with the menus hidden, which is the thing this
  replaces.
- **`timewarp::ui_core`**: those pieces split out of `timewarp_ui`, so a player can
  link the showing without the authoring. Six files moved and live in exactly one
  library; `player_tests` proves the split by linking them without the shell.
- **Standalone format 2**: a JSON metadata block ahead of the source — title, author,
  the size the program was written for, a background colour, a CRT strength.
  `--compile --title … --author …` fills it in. The trailer is still read from the end,
  so a format-1 artifact written by 17.x opens and reports itself as format 1.
- **`--compile` copies the player when it finds one**, and the IDE otherwise: a package
  produces small artifacts and a bare tree still produces something that runs, without a
  build flag deciding it. The player is installed beside the IDE because that is where
  the search looks.
- **`player_tests`** (the 24th suite): spawns the real executable headless and looks at
  the frames it writes. A drawing example screenshots at the size asked for with more
  than two colours in it; a compiled artifact, run as its own executable, draws; a text
  file is refused with exit code 2 and a sentence; `--version` prints.

### Changed

- `time_warp_player` is installed beside the IDE, because `--compile` looks for it
  there. A package that shipped only the IDE would compile every program into a copy of
  the whole application.
- `docs/ROADMAP.md`'s "Distributing a program", `docs/USER_GUIDE.md` and `README.md`
  describe the artifact as a player copy and name the format-2 fields.

## [18.0.0] - 2026-10-22

The Python engine is gone. One language builds this repository, tests it and gates it:
C++ for the product, and a `python3` for the harness scripts, which drive the engines
the product ships.

17.0 said the *product* contained no Python; that was true and not enough. The
repository still held 65 Python files and 33,306 lines in `native/bridge/`, and every
gate that mattered needed a virtual environment to run. `git ls-files '*.py'` now lists
twelve scripts: nine in `native/tools/` (the harnesses, the curated command table and
the two documentation gates), `twc.py`, and two in `Scripts/` — and nothing else.

The Python engine is gone. One language builds this repository, tests it and gates
it: C++ for the product, and a `python3` for the harness scripts, which drive the
engines the product ships.

17.0 said the *product* contained no Python; that was true and not enough. The
repository still held 65 Python files and 33,306 lines in `native/bridge/`, and
every gate that mattered needed a virtual environment to run. `git ls-files '*.py'`
now lists twelve scripts: nine in `native/tools/` (the harnesses, the curated command
table and the two documentation gates), `twc.py`, and two in `Scripts/` — and nothing
else.

### Removed

- **`native/bridge/`** — the reference engine: the ten executors, the expression
  evaluator, the validators, the lesson curriculum reader, the classroom and the
  JSON-lines worker. 68 files.
- **`Scripts/audit_interpreters.py`**, which walked the engine's executor modules.
- **The three differential harnesses**, which each imported an interpreter from it:
  `differential_check.py` (expressions), `lang_differential_check.py` (languages) and
  `scheme_differential_check.py` (Scheme), with `scheme_reference.py` and
  `scheme_probe.py` behind the last one.
- **`make engine-test`** (the engine's pytest suite), **`make golden-record`** (which
  asked the engine to freeze the corpus again) and the `lint` target's dependency on
  `native/bridge/pyproject.toml`.
- **The Windows CI step that drove `worker.py`** directly, and the three
  `PYTHONPATH: native/bridge` environment blocks.

### Added

- **`native/tools/golden_check.py`** — the parity gate, in one command
  (`make golden-check`). It compares the ports with 470 recorded language programs
  (output lines, every drawing call in order, the variables left behind) and 192
  recorded Scheme programs (whole output text), and imports no Python to do it: each
  entry carries the program text as well as the answer, which is what lets a gate
  outlive the engine it was recorded from. Scheme's 24 accepted divergences are kept
  as notes rather than compared, and an entry whose note has gone stale **fails** —
  which retired three of them, because the port had caught up with the reference.
- **`native/tests/golden/scheme_corpus.json.gz`** — 192 programs, recorded from the
  reference before it was deleted, in the hour before it was deleted and using the
  reference restored from git. It is the same trick the language corpus used.
- **`native/tools/command_table.py`** — the curated command rows moved out of the
  engine's test package. The table is data, not engine code: it names each command
  and the smallest program that exercises it, and the three harnesses that read it
  now import it from here.
- **`native/tools/harness.py`** — one answer to "which `time_warp_check`, and which
  checkout?", so the two harnesses cannot disagree about it.
- **`native/tools/pyproject.toml`** — ruff and mypy configuration, moved with the
  scripts it configures. `make lint` checks `native/tools` now.
- **`native/tools/golden_check.py`** — the parity gate, in one command
  (`make golden-check`). It compares the ports with 470 recorded language programs
  (output lines, every drawing call in order, the variables left behind) and 192
  recorded Scheme programs (whole output text), and imports no Python to do it: each
  entry carries the program text as well as the answer, which is what lets a gate
  outlive the engine it was recorded from. Scheme's 24 accepted divergences are kept
  as notes rather than compared, and an entry whose note has gone stale **fails** —
  which retired three of them, because the port had caught up with the reference.
- **`native/tests/golden/scheme_corpus.json.gz`** — 192 programs, recorded from the
  reference before it was deleted, in the hour before it was deleted and using the
  reference restored from git. It is the same trick the language corpus used.
- **`native/tools/command_table.py`** — the curated command rows moved out of the
  engine's test package. The table is data, not engine code: it names each command
  and the smallest program that exercises it, and the three harnesses that read it
  now import it from here.
- **`native/tools/harness.py`** — one answer to "which `time_warp_check`, and which
  checkout?", so the two harnesses cannot disagree about it.
- **`native/tools/pyproject.toml`** — ruff and mypy configuration, moved with the
  scripts it configures. `make lint` checks `native/tools` now.

### Changed

- **`command_inventory.py` reads the C++ executors.** It parsed the Python ones with
  `ast`; the engines that ship are in `native/src/lang/`, so measuring completeness
  anywhere else measured the thing being deleted. It recognises the four dispatch
  shapes this codebase uses — `cmd == "X"`, `starts_with_upper(text, "TO ")`, brace
  lists of literals, and Smalltalk's `add(class, "selector", …)` registration — and
  it accepts APL's non-ASCII glyphs as commands, which the letter test had called
  punctuation.
- **The completeness number is bigger, and that is the honest answer.** The report
  used to say "0 candidates neither tested nor waived" for Smalltalk; it says 81 now,
  because the Python inventory could not see a selector registered with `add(…)`. A
  clean bill from a blind spot is worse than a number that needs a sentence of
  explanation, so the report explains what it counts: statements have rows, functions
  and library selectors are covered by `lang_tests`.
- **The curriculum gate moved to `native.yml`**, the job that has built
  `time_warp_teach`; `docs.yml` keeps the text checks that need no compiler.

### Fixed

- **The documentation gate earned its keep on this change.** Deleting the engine
  falsified thirty-one sentences across thirteen files — `docs/ARCHITECTURE.md`
  documented a live "engine" section, `docs/INSTALLATION.md` promised a Python
  prerequisite, `.github/SUPPORT.md` offered the engine as an unsupported API,
  `docs/MACHINES.md` and `docs/LANGUAGES_GUIDE.md` pointed at test modules under
  `native/bridge/`, and `native/README.md`'s tool listing still described five
  deleted scripts. `check_doc_claims.py` found all of them, and the two that were
  genuinely history are wrapped in `<!-- history -->` regions rather than deleted.

### The trade, stated

A live differential is stronger than a recording: it notices a mistake in *both*
engines at once. Comparing with a frozen corpus cannot say the recording is right, so
a deliberate behaviour change edits the corpus entry in the same commit that changes
the behaviour. What that buys is a gate that runs against the engines the product
ships, needs no second interpreter, and cannot rot when one drifts.

## [17.3.0] - 2026-10-22

The teacher's command has every verb the Python one had, and the Python one is gone.
Ten commands were missing from `time_warp_teach` — `outline`, `plans`, `answers`,
`new`, `roster add|remove`, `assign`, `distribute`, `collect`, `report`, `present` —
which is why `teach.py` had to stay. Twelve hundred lines of Python across the two
CLIs are now none.

### Added

- **`time_warp_teach plans --write <dir>`**, and the renderer behind it
  (`timewarp::lessons::render_plans`, `native/src/lessons/plans.cpp`). It is verified
  the hardest way available: the 60 committed lesson plans in `docs/lessons/plans/`
  were rendered by the Python tool, and the C++ renderer reproduces all eleven files
  **byte for byte** apart from the sentence that names the command which generated
  them. Escaped prose, fenced code, prerequisite lines and the index table all came
  out identical, so the port is not "close enough" — it is the same document.
- **The other nine verbs**, each a thin call into `timewarp_lessons` — which already
  had `addStudent`, `distribute`, `collect`, `markAll`, `classProgress` and
  `setPresentation`, so the work was plumbing and argument handling. `--language`
  accepts either spelling (`basic`, `BASIC`, `chip8`, `CHIP-8`) because the key and
  the display name are two facts and a teacher should not have to know which one a
  given command compares.
- `time_warp_teach validate --quiet`, which is what the CI step asks for.

### Removed

- `native/tools/teach.py` and `native/tools/learn.py` (1,284 lines). Nothing in the
  build, the workflows or the guides names them; `time_warp_teach` and
  `time_warp_learn` are the commands.

### Changed

- **The curriculum gate moved to the job that has the tool.** `docs.yml` used to run
  `teach.py validate` and regenerate the plans with nothing but `python3`, which is
  why it answered in seconds; both checks need `time_warp_teach` now, so they run in
  `native.yml` beside the build, and the documentation job keeps the text checks
  (index, links, claims, tutorials) that need no compiler.

## [17.2.1] - 2026-10-22

The documentation gate checked links, and a link that resolves can still be a false
claim: `docs/ARCHITECTURE.md` documented `locateWorker()`, a function 17.0 deleted;
`native/README.md` said a harness protocol "still lives in `native/bridge/worker.py`";
`docs/ROADMAP.md` owed a device allow-list that has been in `ports.cpp` for several
releases; and every generated lesson plan named the curriculum at its pre-17.0 path.
None of that was reachable by a link checker.

### Added

- **`native/tools/check_doc_claims.py`** (`make doc-claims`, `.github/workflows/docs.yml`):
  three rules that settle a class of defect a machine can settle — a repository path
  named in backticks exists, a retired name is discussed in the past tense, and a
  `make <target>` is a target of the Makefile the command would consult. Two escapes,
  both deliberate: `<!-- history -->` ... `<!-- /history -->` for a record that has to
  name what it records, `<!-- history: all -->` for a whole file, and
  `<!-- doc-claims: ignore -->` for a path that is an example rather than a claim.
  `--self-test` proves the rules fire and both escapes suppress them, which is the
  only way to know a checker that passes is not a checker that cannot fail.

### Fixed

Thirty false claims, found by the new gate on its first run:

- **`native/share/lessons.json` is where the curriculum lives**, and every generated
  lesson plan said `native/bridge/time_warp/features/lessons.json` — the path it had
  before 17.0 moved it. `teach.py write-plans` writes the new one and the plans are
  regenerated; the docs job's `git diff --exit-code` is what keeps them that way.
- **The bridge is described as live in five files**: `docs/ARCHITECTURE.md`'s
  "Finding the worker" section is now a history region in the past tense followed by
  what replaced it; `docs/lessons/FORMAT.md` §8 says nothing drives the protocol any
  more and its gate table names `lessons_tests` instead of a deleted C++-era suite;
  `native/README.md`, `CONTRIBUTING.md`'s tree diagram and `.github/copilot-
  instructions.md`'s architecture map say the same.
- **`docs/ARCHITECTURE.md`'s "Adding a language"** put the reference executor first.
  Since 17.0 the native executor is what a language is; the old steps are kept as a
  history region so the record of how the ten were ported survives.
- `.github/DEPLOYMENT.md` said the docs job watches the moved curriculum path and that
  `native.yml` runs examples "through the reference engine".

## [17.2.0] - 2026-10-22

`make smoke` and `make command-matrix` used to ask `native/bridge/worker.py` what a
program did, which made a Python interpreter part of this repository's test suite
long after the product stopped needing one. They ask `time_warp_check` now — the
native engines, through the same recorder the API and the lesson check use.

The change immediately earned its keep: it found a BASIC bug that two releases of
green gates had not.

### Added

- **`time_warp_check`** (`native/tools/check.cpp`): runs one program through the
  native engines and reports what it printed and what it drew — as text, or as the
  `out`/`canvas`/`done` JSON lines the harnesses read. `--language` is required:
  the extension table lives in the IDE, and a second copy here would be a third
  statement of one fact. Scheme goes through the Scheme machine, because that is
  what the IDE does and `executor_for` deliberately does not answer for it.
- `native/tools/harness.py`: one answer to "which `time_warp_check`, and which
  checkout?", so the two harnesses cannot disagree about it.
- `make check` builds the tool; `make smoke` and `make command-matrix` depend on it.

### Fixed

- **A lowercase array name was a different array.** `DIM dat(3)` then
  `PRINT dat(0)` answered `Unknown function: dat`, while `PRINT DAT(0)` worked:
  the assignment statement and the `Variable` branch of the evaluator upper-cased
  a name before use, and a *call* was looked up exactly as written. BASIC is
  case-insensitive, so this was simply wrong — and `Examples/basic/math_explorer.bas`
  is written in lower case, which meant the example had only ever run because the
  smoke harness ran it through the reference engine. `lang_tests` has the case.

### Changed

- The example gate's ink total for Scheme and Logo moved slightly (85 against 121,
  7868 against 7885). It now counts what the canvas recorder recorded rather than
  what the bridge streamed, and the two disagree about a handful of operations that
  change the drawing without adding to it. Every program that must draw still
  draws, which is what the gate is for; the numbers are a trend, not a contract.

## [17.1.1] - 2026-10-22

A program confined to a directory could reach a sibling of it. `OPEN` resolved a
name against the working directory and then checked that the answer *began with*
it — and `/home/me/run` is a prefix of `/home/me/run2`, so
`OPEN "../run2/notes.txt"` from `/home/me/run` wrote outside the fence. It is a
component test now, the fence confines rather than refuses, and `lang_tests`
fails against the old code with two checks and passes with the new one.

Worth saying plainly because the classroom makes it matter: marking a class runs
thirty programs written by other people, unattended. This is a fence around one
directory, not a privilege boundary — the threat model in `SECURITY.md` says so in
the terms the code actually implements.

## [17.1.0] - 2026-10-22

Two things a program could do but a user could not watch. A `SCREEN` was a picture
you pressed Play on: the wheel and the click did nothing, so a 160x100 sprite was
whatever size the window happened to make it and "what colour is that pixel?" had
no answer short of editing the source. And sound was real on Linux and macOS and
silent on Windows, which is where most of the games are run.

### Added

- **The screen is interactive.** `TurtleCanvas::framebufferTarget()` is now the
  one place the screen's placement is decided — whole-number fit times the zoom,
  plus the drag — and `paintFramebuffer`, the wheel and the new
  `framebufferPixelAt` all read it, so the picture and the answer about the
  picture cannot disagree. The wheel zooms the screen about the pointer, and the
  scale snaps to a whole number of device pixels per screen pixel above 2x so a
  one-pixel line stays one pixel wide; a click emits `pixelPicked(x, y, colour)`
  and the status bar answers `pixel (10, 20) = #FF0000`, which is the first
  question about a screen that came out wrong.
- **The strip says which mode is on.** `TurtleInspector::setScreenMode` shows
  `SCREEN 320x200` while a program has a screen, because on a screen X and Y are
  pixels from the top-left, not the turtle's world, and a learner reading
  "X 156 Y 180" beside a 320x200 game has the wrong explanation for it. The
  scrubber is disabled and says why while a screen is up: a framebuffer shows its
  last frame, so there is no display list to rewind.
- **Sound on Windows.** `powershell.exe` is the player there — no extra install,
  because `Media.SoundPlayer` is in the framework — and a Windows machine is no
  longer the one platform where `BEEP`, `SOUND` and `PLAY` were silent. The file
  path goes inside a script rather than after an argument, so it is quoted as a
  PowerShell literal with its own quote doubled: a path under
  `C:\Users\O'Brien\` stays one token, and `ui_layout_tests` pins that.

## [17.0.0] - 2026-10-22

The product contains no Python. The engine that decided whether a learner's program
satisfied a checkpoint — and the classroom behind it — was the last thing the
application asked a second interpreter to do. Both are C++ now, the worker classes
are gone, and `cmake --install` produces a payload with no Python file in it. The
other half of the claim is what 16.5 and 16.6 built: a program can name a screen,
read a pixel back, capture a sprite, and sound two voices at once.

### Added

- **`timewarp_classroom`** (`native/src/lessons/classroom.hpp`): the roster, the
  assignment, handing out starters, collecting and marking, the class's
  presentation size, and bundles. Marking calls the same `verify_checkpoint` a
  learner's own check calls, so a teacher's mark and a green tick cannot disagree —
  and it touches nobody's progress, because collecting a class must not move a
  learner's place or write into the teacher's own.
- **Bundles are plain zips, read and written with zlib.** A class made by
  `teach.py` opens here and one made here opens in `teach.py`; both directions are
  checked by hand and the reader refuses a member that would land outside the
  classroom. `time_warp_teach bundle` and `unbundle` are the commands.
- **The worker is gone.** `PythonWorker`, `locateWorker`, the `timewarp_bridge`
  target and `python_bridge_tests` are deleted; the row types the panels draw moved
  to `include/timewarp/ui/reports.hpp`, where they name a table rather than a
  protocol. `runInBridge` went with them: every language has an executor, and the
  branch that could have started Python now reports that it cannot happen.
- **Nothing Python is installed.** The `install(DIRECTORY bridge/)` rule is gone,
  along with the advice in the installation guide about `TIME_WARP_WORKER` and
  `TIME_WARP_PYTHON`. A staged install carries the IDE, the API, the two commands,
  the curriculum, the snippets, the examples, the documentation and the Qt runtime.

- **`timewarp_lessons`** (`native/src/lessons/`): the curriculum loader, the
  verifier and the progress file, in C++ and Qt Core only. The verifier runs a
  program through the engines with `RecordingHost` as the surface, so a lesson can
  be judged on a machine with no window; the three expectations are the
  reference's — printed lines, what the turtle drew, and a reported error — and so
  are the messages a learner reads.
- **The curriculum is shipped data.** `lessons.json` moved from the reference
  engine's package to `native/share/lessons.json`, installed beside the snippets,
  and both the application and the reference now read that one file. A package and
  a checkout teach the same course.
- **`time_warp_teach validate`** runs all sixty worked answers and all sixty
  starters through the native engine: every answer passes its checkpoint, every
  starter fails it, and every language has a lesson. `make teach-check` is the
  entry point, and the same pass is a CTest suite.
- **`ProgressStore`** keeps a learner's place in `~/.time_warp/progress.json` in
  the shape that file has always had, with a hint count, a reset, and completion
  per lesson. An unreadable file is ignored rather than fatal: a learner's progress
  is worth more than a tidy log, and refusing to start class because it is corrupt
  is the worse failure.
- **`time_warp_learn`** is the learner's side of the same library: `list`, `show`,
  `start` (writes the current starter to a file), `check`, `hint`, `solution`,
  `progress` and `reset`. The counter of the reference's `learn.py`, and it gives
  the same verdicts for the same reason.
- **`lessons_tests`**: the whole curriculum through the engine, plus the loader's
  refusals, the message a wrong answer gets, and the progress round trip.
- **`classroom_tests`**: handing out never overwrites a learner's work, "not handed
  in" stays different from "handed in and wrong", marking writes no progress, and a
  bundle round-trips between two classrooms.

### Fixed

- **A jump to the line already executing ended the program.** The run loop detects
  a jump by comparing the program counter before and after a statement, so
  `10 GOTO 10` looked like "no jump" and fell through to the end of the program;
  a learner's first infinite loop did not loop. The empty-bodied `FOR` defect fixed
  in 16.5.0 was the same bug, worked around inside `NEXT`; the cause is fixed now:
  `Interpreter::jump_to` records the jump, every executor's jump goes through it,
  and the `NEXT` workaround is gone. Fixed in both engines, with a unit test in each
  and a budget test that pins it.

### Changed

- **Both panels are views over the library.** Opening the Lessons tab loads the
  curriculum in process and a check runs in process; the Classroom page reads the
  roster, marks the handed-in files and writes a bundle in process. The worker
  signals they used to listen to are gone. `ui_layout_tests` proves the Lessons
  side: the tab is raised in a window with no bridge started, and the twenty
  lessons still arrive.
- **`time_warp_teach` and `time_warp_learn` are installed** beside the IDE, so the
  claim that the panel and the tools give one answer holds outside a checkout.

### Fixed

- **Marking a class no longer freezes the window.** The Classroom page marks
  every handed-in file, and a mark runs the learner's program: a runaway handout
  takes its whole ten-second budget, and doing that on the GUI thread stopped the
  window for one file and for as long as the class had files like it. Marking runs
  on `ClassroomMarker`, a thread of its own, over jobs that carry the source and
  the checkpoint so it never reaches back into the class. `classroom_tests` asserts
  the split agrees with the one-call form.
- **A `work_dir` written with `~` expands to the home directory**, as it does in
  the reference. A literal `~` directory would have put a learner's work somewhere
  nobody looks.

### Packaging

- **`zlib` is a declared dependency, and `python3` no longer is.** The Debian and
  RPM packages named `python3` for a lesson check that no longer runs there, and
  classroom bundles needed `zlib` declared instead. The container image installs
  `zlib1g` and no longer carries Python; the Windows jobs install `zlib` from
  vcpkg beside SQLite, because MSVC has no system copy and Qt does not expose the
  one it links privately. The requirements table and the security policy name it.

### Notes and limits

- `native/bridge/` remains in the checkout as development tooling: it is the
  fixture the parity corpus was recorded from, and what `example_smoke.py`,
  `command_matrix.py` and `repl_matrix.py` drive. `make golden-check` is the parity
  gate and imports no Python. Deleting the fixture, and porting those three
  harnesses to a native `timewarp_check`, is 17.1.
- The standalone artifact is still a copy of the IDE with the program appended;
  `time_warp_player` is 17.1 too.
- Two behavioural fixes came with the port rather than after it: a passed final
  checkpoint says `🏆 Lesson complete:` again, and hints are counted per request and
  clamped to the last one, which is what `FORMAT.md` documents.

## [16.6.0] - 2026-10-22

Sound gains a second voice. `SOUND` had been a single tone: a new note stopped the
one before it, because the machine played one WAV through one player process. That
is right for a tune and wrong for a game, where the laser has to be audible over
the engine.

### Added

- **A mixer** (`timewarp::audio::Mixer`, `native/src/audio/mixer.cpp`): up to
  eight voices, summed sample by sample, with the synth's own attack and release
  so a note does not click. It is a pure function of its voices — render into a
  buffer, nothing else — which is what lets `audio_tests` assert that two notes
  are audible *together* on a machine with no sound card at all.
- **`SOUND` joins the mix.** The note is added to what is still sounding and the
  mix is re-rendered to one file, so there is still one player process and
  several voices in it. A short effect fired over a long tone is now in the
  samples instead of replacing them.
- **A new suite**: `audio_tests` (7 cases, 44 checks) covers two voices, an effect
  over a tone, the sum being clipped rather than wrapped, the voice limit being
  reported rather than silently dropping a sound, rests, deterministic noise, and
  the render time limit. 22 suites in all.

### Notes

- `PLAY` is still a tune: a note list stops the mix and plays its notes in order,
  because that is what a note list means. Chord syntax (`[CEG]`) is not parsed;
  a chord is written as several `SOUND` statements, which now genuinely sound
  together.
- The sink is unchanged: a WAV through the first of `paplay`, `aplay`, `ffplay`
  or `play`. A machine with none of them is silent and `audio_available()` says
  so — the honest version of "this machine has no sound", rather than a feature
  that pretends.

## [16.5.0] - 2026-10-22

The machine gets a screen it can read, and a screen it can show. `SCREEN 1` had
been a mode name since the port — it recorded a width and a height and drew nothing
with them — and the two things a BASIC program of the era actually did with it,
capture a sprite and ask what colour a pixel is, had no answer. Neither had a
gamepad: `STICK`/`STRIG` were documented, implemented and returned 0, because the
reference's backend was an optional import it never shipped. And the drawing
surface was still a picture that fitted itself to its ink, which is the opposite of
what a game needs.

### Added

- **A pixel surface behind `SCREEN 1`** (`timewarp::lang::Raster`): a fixed
  800x600-by-default buffer with clipping, Bresenham lines, filled and outlined
  rectangles, arcs sampled the way `CIRCLE` samples them, and a flood fill. The
  canvas keeps its display list and keeps drawing the primitives, so the picture
  and the parity gate's command stream are unchanged; the buffer is what the
  statements can read back.
- **`POINT(x, y)`** answers the colour at a pixel as `&H00RRGGBB`, and `-1` when
  the program has no graphics screen — which is how a program asks whether it has
  one.
- **`GET`/`PUT` sprites.** `GET (x1,y1)-(x2,y2), A` captures a rectangle into a
  numeric array that carries its own width and height in elements 0 and 1, and
  `PUT (x, y), A` places it again. The restored block is visible: the IDE's canvas
  draws it as runs of filled pixels, the recording host a compiled program writes
  its SVG with draws one rect per pixel, and the gate's trace reports where and
  how large the block was. The `#` spelling is still the file channel;
  `GET #1, A` and `PUT #9, , V$` behave exactly as before.
- **A gamepad.** `JOYINIT` opens the first `/dev/input/js*` the machine has and
  reports the device; `STICK(n)` answers 0-255 with a resting stick at 128 and
  `STRIG(n)` answers 1 while a button is held. `TIME_WARP_GAMEPAD` names a device
  instead of taking the first. Windows and macOS report no pad rather than a
  backend that cannot read one.
- **`SCREEN` picks a real display mode.** `SCREEN 0` text, `1` (800x600 by
  default, or a size the program names), `2` (640x200), `7` (320x200), `9`
  (640x350), `12` (640x480) and `13` (320x200, 256 colours), carried as a table
  (`timewarp::lang::ScreenMode`) so the raster, the canvas and the error message
  that lists the choices cannot disagree. A number that was never a mode is
  refused with the list of the ones that were. The numbered modes above 1 are a
  native addition; the reference never had them, so `lang_tests` and the shipped
  game are what hold them up rather than the parity gate.
- **A screen, not a fitted drawing.** A program that names a screen switches the
  drawing surface to a framebuffer: (0, 0) is the top-left pixel, x grows right and
  y grows down, the picture is painted at whole-number scale with no smoothing,
  and `fitToContent` leaves it alone. The display list is still built — the Turtle
  Log, the scrubber and the vector export read it — so the screen is a second
  renderer over the same run, not a replacement for the record of it. Export
  writes the screen at its own scale (`renderedDrawing`, and `.svg` embeds it),
  which is what makes a saved game frame the frame the program drew.
- **`Examples/basic/star_patrol.bas`**, a program built on all of it: a captured
  sprite ship, a star field, a shot whose hits are found with `POINT`, and a score
  bar. It runs 240 frames and stops, so it can be run without a person at the keys;
  `lang_tests` runs it through the native engine and asserts that it asked for a
  320x200 screen, placed a sprite and read a pixel.
- **A parity gate that outlives its reference.** `lang_differential_check.py
  --record` freezes the Python engine's answers to every command-table row and
  every example program, program text included, into
  `native/tests/golden/lang_corpus.json.gz`; `--golden` compares the
  C++ engines with that file and imports no Python at all. `make golden-check` and
  `make golden-record` are the two entry points, and each recorded entry carries a
  SHA-256 of its program so a stale corpus is reported as a corpus problem rather
  than as an engine mismatch. See
  [README_TESTING.md](docs/README_TESTING.md).

### Fixed

- **A loop with an empty body ended its outer loop.** `FOR I = 1 TO 4 : NEXT I` is
  legal BASIC, and its `NEXT` sits at the line the loop returns to — so "go back to
  the body" is a jump to the statement already running. The run loop detects a jump
  by comparing the program counter before and after a statement, which cannot see
  that, so the inner frame stayed on the stack and the *enclosing* loop's `NEXT`
  consumed it: the outer loop ran exactly once, silently. Fixed in the port and in
  the reference, with a regression row, a case in each engine's tests, and a note
  in the roadmap. Found while writing the game example, which needs a loop whose
  body is nothing.
- **`PUT` drew a block at half its size.** The canvas bounded the pixel payload by
  `point_count()`, a point being two doubles, and a block carries one double per
  pixel: a 2x2 sprite drew its first row and stopped. The payload convention is
  written down beside `make_put_pixels`, which is the one command it applies to.
- **`RUN` left the previous run's screen behind.** It reset the mode, the size and
  the palette but not the pixels, so a program could read back the picture its
  predecessor drew; it also left the gamepad armed, which meant `STICK` answered
  without the new run's `JOYINIT`. Both are part of what a fresh run means and both
  are reset now.
- **The differential gate no longer reports a joystick as a language bug.** The
  `JOYINIT`, `STICK` and `STRIG` rows are skipped in both modes, because their
  answer depends on the machine: the reference says "no gamepad" on every machine
  and the port finds the one that is plugged in.

### Notes

- The raster is the program's memory of what it drew; the framebuffer is what the
  canvas shows when a program asks for one. Both exist because the two jobs are
  different: `POINT` must answer in the program's coordinates on every machine, and
  the picture must be able to fit itself to the ink when a program draws a turtle
  fractal rather than a game.
- What the screen does not do yet: scroll under a zoom (the widget scales to fit
  and centres), and the IDE has no menu entry of its own for the mode — a program
  names it. Both are 16.6-17.0 material rather than gaps in this claim.
- `Examples/basic/star_patrol.bas` is listed in `example_smoke.py`'s
  `NATIVE_ONLY` set: the reference engine cannot run the numbered modes or sprite
  capture, so running it there would measure the reference's gaps. Its gate is
  `lang_tests`, which runs it natively.

## [16.4.4] - 2026-10-21

The canvas explains itself. The Turtle Log tab had been there since the window was
first built and nothing ever wrote to it — a placeholder that looked finished — and
the one thing the roadmap still listed as missing from the canvas was a way to see
what the turtle had been told and where it ended up.

### Added

- **The 🐢 Turtle Log lists the drawing.** Every command the canvas receives is
  appended, numbered and spelled with its arguments — `FORWARD 100`,
  `SETPENCOLOR #FF8800`, `DRAWPOLYLINE 3 points #00FF00`, `DRAWTEXT "hi" at 5 6
  (size 12)` — so a picture that came out wrong can be read back as the sequence
  that made it. The last 2000 lines are kept, because a looping program can emit
  tens of thousands and a log that outgrows the drawing is not a log. The run's
  commands replace the previous run's, and clearing the canvas clears the record of
  how it was made.
- **A pose strip under the canvas.** X, Y, heading, pen state, colour and width,
  plus how many drawing commands the run has sent, refreshed as the turtle moves.
  It sits under the drawing rather than in a dock because a value that changes
  while the turtle turns is only useful if it can be read without looking away.

Both are views over the canvas rather than copies of its state: the strip is
refreshed from `TurtleCanvas::turtleStateChanged`, and the log is filled by the same
call that applies the command to the canvas — `MainWindow::applyCanvasCommand`, the
bridge's sink — so the two cannot disagree about what was drawn.

### Fixed

- **`turtleStateChanged` reported less than it promised.** The canvas emits it for
  "moved, turned, or changed pen", but `apply()` compared only position and
  heading, so `PENUP` left the status readouts — and the new strip — showing a pen
  that was no longer down. A pen colour, a pen width and the turtle's visibility
  are state too and are compared now.
- **`setTurtlePose` ignored a heading-only arrival.** A pose from the bridge that
  turned the turtle without moving it emitted nothing, which left the marker tilted
  by a heading no readout had been told about.

### Documentation and tests

- **The input round trip has automated coverage**, which the roadmap had recorded
  as impossible because the prompt is modal. The dialog is; everything on either
  side of it is not, and the failure mode was silence — a program stopped at its
  first prompt and the run ended with no error at all. BASIC's `INPUT` is now
  asserted on the engine (the program's own prompt, the answer in the variable, 0
  when nobody answered), on `NativeRunner` (`inputRequested` → `answerInput`, with
  the value reaching the Variables snapshot) and through the real worker's
  `in`/`in-reply`, which is the path the lesson check uses.
- `docs/ROADMAP.md` and `docs/USER_GUIDE.md` describe the log and the strip, and
  the roadmap's "still missing" list no longer names them.

## [16.4.3] - 2026-10-21

### Packaging

- **The container image runs natively on arm64.** Every tag was amd64, because
  Arch -- the distribution the project is developed on -- publishes no arm64 image.
  An ARM laptop or a Raspberry Pi therefore emulated every instruction, and a
  plain `docker run` on one failed with `exec format error`. The image is now
  built on Ubuntu 24.04, whose compiler and Qt 6.4 are the ones the Linux job
  already builds and tests against, and each tag is a manifest list for
  `linux/amd64` and `linux/arm64`.
- **The container's user is still uid 1000.** Ubuntu's base image spends 1000 on
  its own `ubuntu` user, so `studio` is created explicitly with that id. The
  guide's `-v "$PWD:/work"` example writes into a directory the person running it
  owns, and any other id would have turned the export into a permission error.
- The runtime stage now fails its own build if `ldd` finds the binary needs a
  library the stage does not carry, naming what is missing. A dropped Qt plugin or
  SQLite driver is a failed release rather than a user's first `docker run`.

## [16.4.2] - 2026-10-21

### Packaging

- **The container image is published under the name the guide prints.** Releases
  pushed `ghcr.io/<owner>/time_warp_studio` -- the repository's name, lowercased --
  while `docs/INSTALLATION.md` told people to pull
  `ghcr.io/<owner>/time-warp-studio`, which is what every other artifact here is
  called. A package under that name has survived since before the repository was
  renamed, so the documented `docker run` did not fail: it started 14.1.2. The
  image is now built and pushed under the product's name, and the same tags are
  pushed to the repository's name with it, so the notes of the releases before
  this one keep resolving -- to this build rather than to a leftover.

## [16.4.1] - 2026-10-20

The release 16.4.0 described below could not be built: its macOS job stopped on a
bundle that could not load Qt from inside itself, and then on the disk image it
had just made. This is that release with the packaging that finishes, so nothing
it lists has changed.

### Packaging

- **The macOS bundle loads its own frameworks.** The installed bundle now carries
  `@executable_path/../Frameworks` as its rpath. The install step stripped the
  build tree's rpath and left `@rpath/QtSvg.framework/…` pointing at nothing, so
  the staged application aborted in `dyld` the first time it was run, before the
  disk image was even made -- `macdeployqt` copies the frameworks in but rewrites
  only what it can resolve.
- **The disk image opens without an agreement to accept.** CPack put the MIT
  licence on the image as a volume agreement, because the project's policy level
  predates `CMP0133`, and `hdiutil` cancelled every non-interactive mount --
  the release job's own check included -- now that `-acceptlicense` no longer
  exists. The agreement is switched off, the licence travels in
  `Contents/Resources/share/doc/time-warp-studio/LICENSE` the way the deb and rpm
  carry theirs, and the release job mounts the image and runs the copy on it.
- **The release job checks what it would ship.** The macOS job tests the staged
  install rather than a path the build tree never produced, and every action the
  pipeline uses is pinned to a major that runs on Node 24.

## [16.4.0] - 2026-10-20

The Studio leaves Linux: a release now carries a native Windows installer and a
universal macOS disk image beside the Linux packages, built and tested on all
three desktops. Two new schemes name the machines it runs on -- Windows 2000 and
Windows 7 -- and the Windows 7 one is a real build as well as a look, because Qt 6
requires Windows 10 and Qt 5.15 does not. Getting there meant compiling the same
sources against both Qt versions, which is now a configure option rather than a
fork.

### Added

- **Windows and macOS packages.** A release now carries a native Windows installer
  (`time-warp-studio-<version>-win64.exe`) and a portable ZIP, and a universal macOS
  disk image (`Time_Warp_Studio-<version>-macOS-universal.dmg`), beside the Linux
  packages and the AppImage. A release is built and tested on all three desktops,
  and each artifact carries a `.sha256`.
- **The Windows build is a desktop application, not a console one.** The
  executable has an icon and a version resource, links as a GUI-subsystem binary so
  no console window sits behind the IDE, and attaches to the prompt that started it
  when run as `time_warp_ide --version` or `--compile`. The NSIS installer puts
  `bin\time_warp_ide.exe` and `share\` under `Program Files\Time Warp Studio`,
  adds Start Menu and desktop shortcuts, and carries the Qt runtime that
  `windeployqt` deploys during the install.
- **The macOS build is a `.app` bundle inside a disk image.** `Time Warp Studio.app`
  holds the bridge, the examples, the snippet catalogue, the documentation and the
  licence in `Contents/Resources`, deploys its Qt frameworks with `macdeployqt`, and
  is ad-hoc signed so it starts on Apple silicon. The image is universal: one file
  for Intel and Apple silicon, built with Qt 6.12 and declaring macOS 14.4 -- that
  release's own floor -- as its minimum. Source builds on macOS 26 need Qt 6.10 or
  newer, because 6.8 and 6.9 link a framework the newest SDK no longer ships.
- **A Windows 2000 theme.** The 2000 "Windows Classic" scheme -- 3D-face grey
  chrome, white document surfaces, the navy selection -- joins the theme table as
  the thirty-fourth scheme, beside the C64, Apple II and CRT sets.
- **A Windows 7 build, and a Windows 7 theme.** Qt 6 requires Windows 10, so the
  release now also carries `time-warp-studio-<version>-win64-win7.exe`: the same
  application built against Qt 5.15, the last Qt that runs on Windows 7 SP1. The
  build system takes `-DTIME_WARP_QT5=ON` to compile against Qt 5 and
  `-DTIME_WARP_WIN7=ON` to state the Windows 7 API level, so a call into anything
  newer is a compile error rather than a missing entry point. The Aero scheme
  joins the theme table as the thirty-fifth: the glass frame's pale chrome, the
  Aero selection blue, and Visual Studio 2010's syntax colours.

### Fixed

- **A light theme drew white ink on white paper.** The pen a program starts with
  was the reference's white whatever the theme, so Windows 2000 -- and High
  Contrast Light before it -- ran a Logo square, reported four strokes, and showed
  nothing. The starting ink now comes from the paper: white on a dark surface, the
  theme's text colour on a light one. A program that names a colour still wins.
- **Path discovery understands a bundle.** The bridge, examples, snippets and help
  pages are looked for under `Contents/Resources/share` as well as beside `bin/`. A
  Finder-launched bundle finds python.org's and Homebrew's Python, which `PATH`
  would not contain, and the Windows build skips the Store's `python.exe`
  app-execution alias instead of opening the Store when no interpreter is
  installed.
- **`PLAY` and the machines' beeps on macOS.** `afplay` joins the players the
  sound layer looks for, so a sound on a Mac is heard rather than reported as
  unavailable. Windows has no equivalent command-line player, so sound there stays
  silent, as the reference engine's optional backends already are.
- **The bridge died at its first non-ASCII reply on Windows.** Its streams were
  whatever the locale made them, which on Windows is cp1252, while the protocol is
  UTF-8 and the messages are written as real characters. The first lesson title or
  emoji therefore raised `UnicodeEncodeError`, which is a `ValueError`, which the
  emitter reads as a client that has gone away -- so the worker said `ready`,
  exited, and every bridge test reported a bare timeout. The worker now sets its
  streams to UTF-8 before it says anything, and a stream that still cannot encode
  is an error rather than a silent exit. A regression test drives the real worker
  under `PYTHONIOENCODING=cp1252` and `ascii`, which is how the bug was proved to
  exist rather than merely assumed fixed.
- **A hung run could deadlock the window.** Stopping a program waited for its
  thread to notice the stop flag, but a thread blocked on a full canvas queue can
  only notice it once the GUI drains the queue -- and the GUI was parked in the
  wait. The wait now drains the queue between slices, so Stop always returns.
- **A Mac's serial ports were refused.** The port policy allows the device
  families a school plugs in, and the list was Linux's: `/dev/pts/` for a
  pseudo-terminal and nothing for the `/dev/ttys#` macOS uses instead, nor the
  `/dev/cu.*` and `/dev/tty.*` a USB serial adapter appears as. A Mac user with a
  board plugged in was told the path was not a port they may open; now it is.
- **Floating-point `std::to_chars` and `std::from_chars` are not everywhere.**
  Apple's standard library gained them late, so the dependency-free formatting the
  expression evaluator uses now has a `snprintf`/`strtod` fallback. It is not a
  last resort left untested: `TIME_WARP_FORCE_CHARCONV_FALLBACK=1` builds the
  fallback on a machine that has the real thing, and every differential gate passes
  through it.

### Changed

- `cpack` now produces the platform's native artifact: `NSIS` and `ZIP` on
  Windows, `DragNDrop` on macOS, `DEB` on Linux. Both new generators package the
  same `install()` rules the Linux packages use, so a local install and a package
  cannot contain different files.
- The sources build against Qt 6 or Qt 5.15 from one tree. The names that moved
  between the versions are two small headers -- `timewarp/core/qt_compat.hpp` and
  `timewarp/core/qt_gui_compat.hpp` -- rather than a conditional at every call
  site, so a fix in either build is a fix in both.
- The macOS build targets **macOS 14.4**, which is Qt 6.12's own floor, and the
  Windows builds target the 64-bit Windows 10/11 desktop. The Windows 7 artifact is
  built against Qt 5.15.2 and states the Windows 7 API level; it is built and
  tested on Windows 11, because no CI runner is older, and that caveat is in
  `docs/INSTALLATION.md` rather than left for the user to discover.
- Every continuous-integration job runs its suites with a per-test timeout, so a
  test that hangs is a named failure with the output it had produced instead of a
  job that sits there until the runner tires of it. That is how the deadlock above
  was found.

## [16.3.0] - 2026-10-20

The HB-1 arrives: a bespoke 1982 home computer inside the Studio, with a
ten-language ROM socket, its own CONFIG screen and HB DOS on a 512-sector volume.
The machines get their own manual set, the README is reframed for retro-computing
hobbyists, and CHIP-8 clears its hi-res screen correctly.

### Added

- **The HB-1.** Tools > HB-1 Computer… (`Ctrl+Alt+W`, or
  `./run.sh --computer`) opens a self-contained home computer: a boot intro at the
  top of one CRT, a prompt below it, a selectable language ROM for all ten
  languages, and TEXT/SPLIT/GRAPHICS screen modes. The text side is a real code
  editor on its PROGRAM page and the machine's boot/output screen on OUTPUT; the
  graphics side is the same turtle canvas and the same native engines the IDE
  runs. Each ROM keeps its own program, all ten buffers, the selected ROM and the
  screen mode persist between sessions, the prompt has history, and BASIC direct
  mode (numbered lines, `LIST`, `RUN`, `NEW`, `?`) works at the machine keyboard.
  The screen is covered by the existing CRT glass without intercepting input.
  **CONFIG** (`Ctrl+,`) opens a settings screen for the machine: boot ROM, boot
  screen and start-up message, prompt-history depth, whether each ROM's program is
  remembered, whether drawing switches a text screen to SPLIT, four phosphors
  (green, amber, white, ice) that recolour the whole machine, independent screen
  and program text sizes, and the five glass strengths. Changes preview live,
  Cancel puts the machine back, and the settings persist.
  **HB DOS 1.0** gives the machine a disk: one 512-sector volume with `CATALOG`,
  `SAVE`, `LOAD`, `RUN "name"`, `DELETE`, `RENAME`, `LOCK`/`UNLOCK`, `VERIFY`,
  `TYPE`, `STATUS` and a two-step `INIT`, catalogued in the style of Apple DOS 3.3
  in 256-byte sectors. A locked file refuses to be overwritten or deleted, a file
  that will not fit is refused rather than truncated, and each file remembers the
  ROM that can run it. The volume is one JSON image under the application's data
  directory, so programs survive a restart and a teacher can copy the disk.
  With `--computer`, `--run`, `--screenshot` and `--export` act on the machine
  rather than the editor, so a script captures the boot screen and the drawing the
  program made on the machine's canvas.

### Documentation

- **The machines have books.** [docs/MACHINES.md](docs/MACHINES.md) states the
  premise — a system from a parallel universe — what the HB-1 and the CHIP-8
  share, and the deliberate anachronisms item by item. `docs/hb1/` is the Owner's
  Manual, the ROM library, the HB DOS 1.0 reference, the CONFIG reference and an
  engineering companion; `docs/chip8/` is the machine manual, the assembler and
  the complete instruction set with its 1977 quirks. Both sets are installed with
  the application and listed in Help > Documentation.
- **The README leads with capability.** A capability inventory links every part of
  the Studio — editor, canvas, debugger, projects, SQL, lessons, automation,
  appearance, the help system and both machines — to the book that owns it. The
  IDE guide, FAQ, language guide, architecture notes, roadmap and the in-tree
  agent instructions follow the same map.
- **The README is framed for retro-computing hobbyists.** It leads with the
  Studio as a complete old-school programming environment — a full IDE, ten
  interpreters from 1964–1980, and the two machines — with the lesson curriculum
  reduced to a short "If you teach" section. A new "For the retro-computing mind"
  section states the kept quirks, the JSON disk image, the user-owned ROM policy,
  the theme and phosphor work, and the native/offline stance. The same framing
  follows the About box, `--help`, the package descriptions, the desktop entry,
  the container label and the release notes.
- **The guides are gated.** `make doc-links` — and the Docs workflow — checks
  every relative link and `#anchor` across `docs/`, `README.md` and
  `Examples/CATALOG.md`; the workflow now also requires the machine books to
  exist and runs when the checker changes.

### Fixed

- **CHIP-8 `CLS` in hi-res.** Clearing the screen used the lores buffer size, so a
  `00E0` after `00FF` truncated the 128x64 framebuffer; the next `DRW` could write
  past it and nothing below the sixteenth row could be painted. Clearing now uses
  the current grid, and `lang_tests` pins both the buffer size and a pixel on the
  bottom row.
- **Build: the SQLite target.** CMake 4.3 renamed `FindSQLite3`'s imported
  target from `SQLite::SQLite3` to `SQLite3::SQLite3` and deprecated the old
  spelling, so every configure on a newer CMake warned against the link line.
  The build now uses whichever name the module provides, keeping the CMake 3.20
  floor working without the warning.

## [16.2.0] - 2026-10-20

Projects, a persistent REPL with BASIC direct mode, SQL, watch expressions and
data breakpoints, a JSON-RPC automation API, and terminal compilation join the
IDE. These are additive changes; existing languages, programs, settings and
lesson ids are unchanged.

### Added

- **A showcase program for every language.** `Examples/<language>/showcase.<ext>`
  is one program per language that walks through that language's whole surface —
  its statements, its data, its control flow, the idioms that are its own, and a
  drawing at the end — so a reader can see what the language does before opening
  a manual. BASIC mixes PRINT, arrays, all three loops, SUB/FUNCTION, error
  handling, graphics and sound; Logo uses lists, procedures, recursion and the
  pen; PILOT goes through its text, answer, match, compute, branch and
  subroutine units; COMAL and REXX show their loop forms and string machinery;
  Scheme shows higher-order procedures and closures; APL shows each function
  family and the right-to-left rule; FOCAL shows its sign branch and function
  library; CHIP-8 draws a montage with the font, BCD, collision and hi-res;
  Smalltalk defines two classes and queries a collection three ways. All ten
  draw, so all ten are in `DRAWING_EXAMPLES`, and the parity gates cover them:
  483 programs, 0 differences.

- **Projects.** A 🗂 Projects dock and a `.twproject` model group programs into
  named groups without moving them on disk: create, open, add files, add a whole
  folder (its programs are grouped by subfolder), add and rename groups, drag
  between groups, and see a missing file rather than losing it silently. The model
  stores relative paths inside the root, saves atomically and refuses a newer
  format; the active project and recent list are session state. `project.addFolder`
  exposes the folder import to the API.
- **The REPL bar is wired.** A persistent session per language runs commands off
  the GUI thread and draws through the same canvas queue. BASIC, FOCAL, PILOT,
  COMAL, REXX, APL, Logo and Scheme keep state between entries; Scheme keeps its
  machine and echoes expression values, while Smalltalk and CHIP-8 run each entry
  as a whole program. BASIC's `NEW` clears the session's bindings; in the IDE it
  also clears the editor. A numbered BASIC line typed at the REPL is stored in
  the editor as a program line, `LIST` and `RUN` are direct-mode commands, and
  `?` is accepted as shorthand for `PRINT`.
- **Watch expressions and data breakpoints.** The Debug panel evaluates
  arithmetic over the paused bindings and stops a run when a named variable
  changes, for the line-loop languages, Scheme and the whole-program languages.
- **SQL.** A SQLite-backed 🗄 Database tab opens or creates a database, browses
  tables and schemas, runs scripts and shows results. Programs reach the same
  connection with `SQL ...` in the line-by-line languages and Logo, `(sql "...")`
  in Scheme and `SQL run: '...'` in Smalltalk; CHIP-8 has no SQL form.
- **Automation API.** `./api` speaks JSON-RPC 2.0 over newline-delimited JSON:
  `language.run`, `repl.*`, `debug.*`, `sql.*`, `project.*` and
  `watch.evaluate`, with batches, notifications and standard error codes. The
  API's drawing object now reports turtle `actions` (including turns and pen
  changes) beside ink `commands`.
  `docs/API.md` documents it and the linkable C++ libraries behind it; the new
  [user guide](docs/USER_GUIDE.md) covers the IDE itself.
- **Terminal tools.** `./run.sh` builds if needed and launches the IDE; `./twc`
  compiles a program into a standalone executable for every language, embedding
  the source, linking the engine, writing a drawing as SVG and taking `--db FILE`
  for a program that uses SQL.

### Fixed

- The command and REPL matrices accept the documented “not available” refusal
  from an optional backend (BASIC `SAY`/`SPEAK`), so the gates pass on a machine
  with no speech engine instead of failing on an optional dependency. The refusal
  is only accepted for rows marked `optional`; any other error still fails. A new
  `--python` option lets the command matrix choose the worker interpreter, which
  is how both the backed and unbacked paths are exercised.

## [16.1.0] - 2026-10-19

The port is finished: every feature the removed PySide6 application had is back,
each landed with the test that proves it, and `docs/PORTING_PLAN.md` records the
phases in the order they landed. The canvas can be scrubbed, recorded as a GIF,
printed and exported as vectors; a program can be profiled line by line, paused in
any of the ten languages, and handed to a class as a bundle, a lesson session or a
presentation. Nothing is removed and no promise changes, so this is a minor
release.

### Added

- **SVG drawing export.** File > Export Drawing (Ctrl+E) and `--export` gain
  `.svg` in a build that finds Qt6::Svg: `QSvgGenerator` paints the same ink the
  screen and the raster writer paint, so the vectors cannot drift from the
  picture, and it writes through `QSaveFile`, so a failed write leaves no half
  file where a good drawing was. One user unit is one widget pixel, which is why
  the export takes no scale factor, and two exports of one drawing are
  byte-identical. A build without the component does not list the format, fails
  `.svg` with a reason, and does not name it in `--help`; the `.deb` and `.rpm`
  gain the Svg package only when the binary links it.
- **Printing.** File > Print Code (Ctrl+P) sends the program to a printer as the
  same A4 document the PDF export writes — one HTML source, so a printed program
  and an exported one cannot disagree — and File > Print Drawing renders the
  canvas for the printer's own resolution and scales it to fit the page, keeping
  its aspect ratio. Both need Qt6::PrintSupport, which the build asks for as an
  optional component: without it neither entry is added, rather than one being
  shown dead, and the PDF and HTML exports still work. The page layout takes a
  `QPagedPaintDevice`, so the tests drive it with a `QPdfWriter` and with a real
  `QPrinter` writing PDF, with no print system or dialog. `printCode` has no
  error result on purpose: `QTextDocument::print` reports an unusable device only
  by printing nothing, and probing it first costs a blank page.
- **Per-line profiling.** View > Profile Run times every statement of a
  line-by-line program and, when the run ends, a report lists the lines longest
  first with hits, time, share and the source text. The clock starts after a
  debug pause, so waiting for Continue is not charged to the statement; the
  toggle is off at every launch because a clock read around every statement
  costs, and a remembered "on" would make every later run pay for a report
  nobody asked to see. A whole-program language (Logo, Scheme, Smalltalk,
  CHIP-8) has no statement loop to time, and the console says so rather than
  showing an empty table. A profiled run takes the same steps and prints the
  same output as an unprofiled one.
- **An opt-in embedded terminal.** Settings > Workspace adds a 💻 Terminal tab
  that writes each line to `$SHELL` (with the current document's folder as its
  working directory, and a silent `cd` when the tab changes) and appends what the
  shell prints. It is off by default because it runs whatever is typed with the
  user's permissions, and presentation and player modes remove the tab and stop
  the shell — a process nobody can see is a process nobody can stop. Closing the
  window stops it too. There is no PTY on purpose: prompts, job control and
  full-screen programs are absent, so `vim` fails rather than taking the IDE over.
- **Stepping every language.** The whole-program languages stop too: Logo before a
  top-level statement, Smalltalk before a message send, and CHIP-8 before every
  instruction (the assembler keeps the listing line of each opcode by address, so a
  gutter breakpoint names a line rather than an address; the display still repaints
  at the frame boundary). `DebugSession` gained a source-line pause point beside the
  statement-index one, with a once-per-visit rule so a line holding several sends
  does not re-pause on Continue but does pause again when a loop returns to it, and
  an atomic fast path so an ordinary CHIP-8 run pays one load per opcode. The
  Variables panel and the Debug tab are refreshed at a pause, and the Debug tab's
  buttons drive whichever runner is paused. Stop reaches these languages too: the
  session's stop flag is checked at the same pause points, so a looping CHIP-8 or
  Smalltalk program no longer runs to its own frame or send limit first.
- **CHIP-8 runs SuperChip hi-res.** The machine and the Python reference both
  implement the SuperChip 1.1 core: 00FE/00FF select 64x32 or 128x64 (clearing the
  screen), 00Cn/00FB/00FC scroll, 00FD exits, DXY0 draws a 16x16 sprite in hi-res
  (8x16 with doubled columns in the original mode), FX30 points at an 8x10 font
  derived from the 4x5 set, and FX75/FX85 round-trip the RPL flags. The emulator
  screen follows the mode, both modes filling the same 2:1 canvas. MegaChip's
  planes and audio are not implemented and are reported as unknown opcodes. A new
  [`hires.ch8`](Examples/chip8/hires.ch8) example draws and scrolls in the mode.
- **A CHIP-8 ROM library.** The emulator's ROM Library... browses a folder of
  programs recursively (.ch8, .c8, .rom, .bin), filters by name, and remembers the
  folder after a program is opened. Time Warp Studio ships no ROMs: most CHIP-8
  programs in circulation are somebody else's copyright, so the library reads a
  folder the user has the right to use rather than bundling one.
- **Record the canvas as an animated GIF.** File > Record Canvas as GIF starts
  sampling the canvas ten times a second; running a program records the drawing
  appearing, and stopping asks where to save it. The GIF writer is in tree
  (`ui/gif_encoder.{hpp,cpp}`: GIF89a, a fixed 6×6×6 palette and LZW), so this
  needs no Pillow and no extra package, and Qt reads GIF so the test decodes what
  the encoder wrote, pixel for pixel. A frame that did not change becomes extra
  time on the frame before it, and frames are taken at one size so resizing the
  window mid-recording cannot break the file.
- **Class bundles from the classroom page.** Save Bundle... writes the class list,
  its work and its settings to one archive, and Load Bundle... unpacks one (asking
  first whether files that are already there should be replaced). Both are the
  engine's own ZIP through `classroom-bundle`/`classroom-unbundle` requests, so the
  file the IDE writes is the one `teach.py unbundle` reads and vice versa -- there
  is no second archive format and no runtime `tar` dependency. A successful load
  re-reads the class on screen.
- **Lesson session export.** File > Export Lesson Session as Markdown or PDF
  writes one file describing the lesson being worked on: what it teaches, every
  checkpoint with the verdict it was given in this session (passed, not passed,
  and never tried are kept apart), the program in the editor and the console
  output. The panel now remembers every verdict it sees rather than only the one
  on screen, and Restart clears them. The Markdown fence outlasts any backtick run
  in the program, and the PDF goes through the same writer and metadata
  normalisation as Export Code as PDF, so two exports of one session are one file.
- **Presentation mode, for teaching from the front.** View > Presentation Mode
  (Shift+F11) is focus mode with an audience: the menu bar, both toolbars, the
  function-key row, the status bar and every dock go away, and the lesson panel
  and console grow to the size the class is projected at. The size and whether
  the window takes the whole screen come from `teach.py present`, so a teacher
  who ran `present 30 --windowed` on a shared desktop is not overruled by a
  button. It asks the bridge for the setting when it is not already known, and
  Escape leaves the mode, restoring exactly the chrome that was there.
- **A classroom page in the IDE.** The right column's sixth page is the teacher's
  side of the curriculum: the class list, what has been assigned and the note
  that goes with it, and one row of marks per learner per lesson with how many
  checkpoints were handed in and how many passed. Open Student Folder takes you
  to the selected learner's work. Everything is read through the language
  bridge from the files `teach.py` already writes -- `ClassroomMode` is the one
  reader of `roster.json`, so the marks the panel shows and `teach.py collect`
  reports are one function's answer. Refresh re-reads those files, so a class
  changed from the command line appears without restarting the IDE. An empty
  class is an empty page with a line saying so, not a failure, and opening the
  page does not create folders for anybody.
- **A document outline, a split editor, and a two-program comparison.** The
  **Document Outline** dock lists the named places in the current program --
  Logo procedures, BASIC subroutines and labels, COMAL `PROC`, REXX labels,
  PILOT `*labels`, Scheme `define`s and APL `∇` functions -- and one click jumps
  there. File > Split Editor Right (Ctrl+\\) moves the current document into a
  second pane, Close Split merges everything back, and Move Tab to Other Pane
  sends one document across; the split and which side each file was on come back
  after a restart. Run > Run Both Panes (Ctrl+Alt+B) runs both documents headless
  and prints one table of outcome, engine time and ink, followed by each
  program's own output under its own name. There is no shared variable bus: a
  benchmark that lets each program write the other's variables is not
  reproducible.
- **Breakpoints and pausing for the line-by-line languages.** Clicking the gutter
  sets a breakpoint (a dot beside the line number), and the points are kept per
  file, survive a restart and are pruned when the program is deleted. Running a
  BASIC, FOCAL, PILOT, COMAL, APL or REXX program stops *before* a breakpointed
  statement; the toolbar's Debug, Continue and Trace are live again, with Trace
  running exactly one statement and Continue going to the next breakpoint or the
  end. Stop unwinds a paused run cleanly rather than leaving a thread asleep.
  Scheme keeps its own debugger and panel; whole-program languages still run to
  completion, because they have no statement boundary to stop at.
- **Accessibility and display controls.** View > High Contrast (Ctrl+Alt+H) gives
  the *current* theme a maximum-contrast variant: the surface becomes black or
  white in the direction the theme already goes and every syntax accent is lifted
  until it clears the WCAG 7:1 floor, so the chosen theme survives and only its
  legibility changes. The A+ button and Ctrl+Alt+A cycle the text size through
  100%, 125% and 150% across the editor, the console logs and the panels, and the
  step is remembered. View > Focus Mode (F11) hides both toolbars, the
  function-key bar, the status bar and every dock, with Esc as the way out. View >
  CRT Settings adds vignette, curvature, flicker and a phosphor tint beside the
  scanlines, with a live preview on the real overlay and values saved under
  `display/crt/*`; View > Screen Mode pairs a theme with those values in one
  action (Normal, Amber, Green, White Phosphor).
- **Export the code as a PDF, and a whole session as one HTML page.** File >
  Export Code as PDF writes an A4 document with line numbers and the program's
  name and language in the header; `QPdfWriter` ships with Qt, so this added no
  dependency. File > Export Session as HTML writes a single self-contained page —
  the program, its output, and the drawing embedded as a PNG data URI — which is
  the file to hand in or project. Exports are deterministic: the PDF writer's
  creation dates and document UUID are normalised, so exporting one program twice
  produces the same bytes.
- **Scrubbing the drawing.** A playback strip under the canvas — play/pause and
  a frame slider — appears as soon as a run has left ink. The canvas keeps the
  primitives in draw order (strokes and shapes interleaved as they were drawn),
  and the strip only changes how many of them are *shown*: export, fit-to-content
  and the ink counters still use the whole drawing, so scrubbing cannot lose a
  picture. Playback advances at 60 Hz, with the step chosen so a long drawing
  still finishes in about ten seconds, and new ink rejoins the live view.
- **A guided tour and one-time coach marks.** A first run opens a four-step tour
  anchored to the real controls — Run, the canvas, the Lessons panel and the
  function-key bar — each step dimming the rest of the window and naming what the
  control is for. `tour/seen` in `QSettings` means a second window or launch does
  not repeat it; Help > Guided Tour (F12) shows it on demand, and Settings > Help
  re-arms it. Two hints use the same anchors once each: the Lessons panel and the
  history arrow on Run.
- **A live CHIP-8 emulator.** Tools > CHIP-8 Emulator (Ctrl+Shift+M) opens the
  current `.ch8` program, or one chosen from the toolbar or dropped on the
  window, as an interactive machine rather than a run to completion. It has a
  60 Hz clock with a speed control in opcodes per frame, real delay and sound
  timers, a clickable 4x4 keypad plus the `1-4`/`Q-R`/`A-F`/`Z-V` keys, Run/
  Pause/Step/Reset (F5/F6/F7), a buzzer while the sound timer is above zero with
  a Mute switch, and registers, timers, cycles, call stack and a memory hex dump
  beside the screen. `LD Vx, K` waits for a press here instead of reading the
  `; keys` directive. Run in the editor keeps its whole-program model, so the
  lessons and the parity gates still have one deterministic answer. The window
  opens on the bundled `Examples/chip8/paint.ch8`, and a **Listing** tab
  assembles typed source with Ctrl+Return; a listing that does not assemble
  reports the error there and leaves the running program untouched.
- **Binary CHIP-8 ROMs are runnable, and the opcode set is complete.** A `.ch8`
  file containing a NUL byte is a ROM rather than a listing: its bytes copy to
  `0x200` unchanged, up to the 3584 that fit before the end of memory, and the
  editor opens it read-only because a keystroke in a binary would be a corrupted
  program waiting to be saved. It is copied one-to-one, so saving writes back the
  program that was loaded. `SYS` (`0NNN`) now assembles and is ignored, as every
  interpreter since the original does; `JP V0, addr` refuses a register other
  than `V0` rather than encoding it into the address; and `LD [I], Vx` /
  `LD Vx, [I]` leave `I` one past the block, as the 1977 interpreter does. The
  reference engine and the port share all of these rules, and the parity gate
  covers them.
- **F1 context help and Ctrl+F1 keyword inspector.** F1 opens the current
  document's language tutorial, scrolled to the word under the cursor; a word
  the guide does not contain leaves the guide at the top rather than doing
  nothing. Ctrl+F1 lists every word the editor colours for that language,
  filterable, with the guide's own line beside it. The list is the highlighter's
  vocabulary rather than a second table, so a word added to the colouring cannot
  vanish from the inspector. F1 no longer opens the shortcut reference; that is
  Help > Keyboard Shortcuts, which keeps the function-key bar's Shift+F1.
- **Help > Documentation, and Getting Started.** A help window with a contents
  tree built from `docs/INDEX.md`'s own tables — so it cannot promise a page the
  index does not list — and a Markdown-rendered page beside it. Links between
  pages open in the window and external links in the user's browser. The docs are
  now installed to `share/doc/time-warp-studio`, and the resolver reads
  `TIME_WARP_DOCS`, then the installed tree, then the checkout's `docs/`, so a
  build and a package show the same pages. A missing documentation tree explains
  itself rather than showing an empty pane. Getting Started opens the student
  guide directly.
- **Autosave and Version History.** Every document with unsaved changes is
  snapshotted under the Studio's data directory, five copies per document, at an
  interval Settings controls (two minutes by default). Untitled buffers are
  covered too, which is the case with nothing on disk to recover from. The
  manifest is written through a temporary file and renamed, so a crash during a
  snapshot cannot lose the list of the others. At startup, a copy newer than its
  file is reported in the status bar — never a modal — and File > Version History
  (Ctrl+Alt+V) lists a document's copies newest first with a preview. Opening one
  puts it in a new, unsaved tab and never writes the file, so choosing the wrong
  version costs a tab rather than today's work.
- **Run Selection, Reset Program, and a list of what you ran.** Run >
  Run Selection (Ctrl+F9) executes just the selected lines, in the same engine,
  without writing them to a file; an empty selection is refused with a message
  rather than quietly running the whole program. Run > Reset Program
  (Ctrl+Shift+F9) stops anything running and clears the canvas and output while
  keeping the document. The Run button's popup now lists the last ten runs —
  path, language, and the first line of each — and clicking one reopens and runs
  it.
- **A minimap, and visible whitespace.** The strip at the right edge is the whole
  document sampled one row per pixel, with the current view marked; clicking or
  dragging it scrolls. It reserves its own margin, so it cannot cover the end of a
  long line, and switching it off gives the width back. `Show spaces and tabs`
  renders a middle dot per space and an arrow per tab, which is what makes an
  indent of four spaces and one of a tab look different. Both are in Settings and
  persist.
- **Edit > Insert Snippet (Ctrl+Shift+I).** A catalogue of small runnable programs
  for all ten languages — a loop, a procedure, a sprite, a turtle walk — filtered by
  language and by name, previewed, and inserted at the cursor. `$CURSOR$` marks
  where the caret lands and `$SELECTION$` wraps what is selected; a whole
  insertion is one undo step. The catalogue is `native/share/snippets.json`,
  installed with the application, and every construct in it appears in a program
  under `Examples/` or in the language guide — a snippet that teaches syntax the
  engine rejects is worse than an empty library. Rewritten from the removed
  `features/snippets.json`, three of whose six languages no longer exist.
- **Edit > Find in Files (Ctrl+Shift+F).** Searches the folder of the current
  document and opens a hit at its line. Skips binaries, files over 2 MB and build
  or VCS directories (`.git`, `build/`, `node_modules/`, `__pycache__/`), searches
  a selection by pre-filling the term, stops on request, and reports an invalid
  regular expression as an error rather than as "no matches". The scan is one
  function -- `searchInFiles` -- that both the dialog's thread and the tests call,
  so the skipping rules exist in one place.
- **Edit > Format Code (Ctrl+I).** Re-indents BASIC, COMAL, REXX and Logo by their
  block structure — the loop bodies, `IF`/`END IF` arms, `ELSE` branches, Logo
  procedures and bracketed blocks — using the editor's tab width. It changes
  nothing but the left margin: strip both versions and the text is identical, a
  second format does nothing, and the cursor returns to the same line and column.
  The other six languages say they have no blocks to indent rather than
  pretending to format. Ported from `utils/code_formatter.py`, with the bug that
  left every `ELSE` body one level out fixed.

## [16.0.0] - 2026-10-18

All ten languages now run in C++: nothing is routed to the Python engine at run
time, and it has been demoted to the reference the parity gates compare against.
The IDE that runs them gained the features the port had been missing — themes,
working function keys, a command palette, an editor that checks a program as it is
typed, sound that is audible, and ports a program can talk to. Programs can also
leave the IDE: `--compile` writes a standalone executable.

Two of those change what the product *promises* rather than what it can do. The
language lineup and the run-time dependency are the project's central claim, and
`SERIAL`/`PARALLEL` put device nodes within a program's reach — which is why they
are allow-listed, and why that is a security surface rather than a feature.

### Added

- **The 33 themes are back.** The removed PySide6 application shipped 33 named
  colour schemes — the retro sets (Amber Lab, Turbo Vision, NeXTSTEP, Amiga
  Workbench, CGA, C64, Apple II, DOS Blue, ZX Spectrum) as well as the modern
  editor palettes (Dracula, Nord, Solarized, Catppuccin, Tokyo Night) and the
  accessibility sets. `native/src/ui/themes.cpp` carries them as one record each,
  generated from the Python's table rather than re-invented, so a learner who had
  picked "Amiga Workbench 2.0" gets the same colours back. View > Theme lists all
  33, Shift+F10 cycles them, the choice is remembered, and applying one sets the
  palette, the window's own rules, the editor's syntax colours and the canvas
  paper — a theme that recoloured the chrome and left the code blue would not be
  a theme.
- **The function keys work.** The bar under the window had been drawing ten
  labelled keys since the port and doing nothing with them: its signal was
  connected to a *second signal* of the same name, so a click was re-emitted to
  nobody. It now has the Python's three layers (normal, shift, control), a badge
  that cycles them, and one dispatcher behind every key, the matching shortcut
  and the command palette. Thirty keys, each doing what its label says; the ones
  whose feature is not built (split editor, find in files) say so in the status
  bar rather than doing nothing.
- **A command palette** (F10, Ctrl+Shift+P) over every menu entry, filtered as
  you type and showing each command's shortcut. Built from the live menu bar, so
  it cannot list a command that does not exist or miss one that does.
- **Standalone executables.** `time_warp_ide --compile program.bas --output
  hello` writes a copy of the IDE with the program inside it: double-click it and
  it opens on the drawing, with the authoring UI hidden and the document
  read-only. The payload is a documented trailer (`native/include/timewarp/ui/
  standalone.hpp`), so the artifact is an ordinary executable that happens to
  have some bytes after its segments. `--force` replaces an existing file and
  nothing else does.
- **Keyboard reference** (F1, Shift+F1), built from the same key table the bar
  draws, so help and bar cannot disagree.
- **CRT scanlines** are switchable (View > CRT Scanlines, Shift+F7). The overlay
  has existed since the port and had no way to be turned on.
- **The editor is a real editor.** Language-specific checking as you type
  (`native/src/ui/lint.cpp`, one rule set per language, from the engine's own
  command inventory): unbalanced brackets, unterminated strings and comments,
  `FOR` without `NEXT`, `IF` without `THEN`, PILOT lines with no unit letter, and
  a keyword one letter from a real one, with the spelling suggested. Findings are
  a squiggle, a triangle in the gutter, and a tooltip with the sentence. Plus
  optional word completion over the language's keywords and the document's own
  names (Ctrl+Space asks even when the setting is off), auto-indent that follows
  block openers, Tab/Shift+Tab over a selection, and a Preferences dialog (Ctrl+,)
  with a live preview of a sample that is deliberately wrong in two places — so
  changing a setting shows what checking looks like. Font, size, tab width,
  gutter, current-line band, wrapping, indent, completion and checking are all
  remembered per user.
- **Sound that is actually audible.** `BEEP`, `SOUND` and `PLAY` were
  acknowledge-only: they printed "🔊 SOUND 440Hz for 55ms" and played silence,
  because the retired Python build made audio optional behind `pygame`. The
  native build synthesises the period-correct waves itself
  (`native/src/audio/music.cpp`: square, triangle, sine, noise; equal
  temperament; envelopes so notes do not click), parses `PLAY`'s notation — tempo,
  lengths, octaves, accidentals, dotted notes, rests, `N` note numbers — and
  hands a WAV to the first of `paplay`, `aplay`, `ffplay` or `play` it finds. No
  new build dependency; a machine with none of those is silent but not broken,
  and the language's acknowledgement is still the reference's text, so the parity
  gates are unchanged.
- **Serial and parallel ports** (`native/src/io/ports.cpp`), so a program can talk
  to a board: `SERIAL OPEN #1, "/dev/ttyUSB0", 9600`, `SERIAL WRITE`, `SERIAL
  READ #1, A$, 2000`, `SERIAL CLOSE`, and `PARALLEL OPEN/WRITE/BYTE/CLOSE` for the
  data lines. Raw POSIX `termios`, no library, timeouts on every read, writes
  that wait rather than drop bytes. The device families a school plugs in are
  allowed and everything else is refused with a sentence — a program that can
  open any device node can also open a disk — with `TIME_WARP_PORTS` as the
  teacher's escape hatch. Tested against a pseudo-terminal, which is a real
  device with the other end in the test.
- **`INKEY$` reads the keyboard.** It was the empty string by documented design,
  which made every game unwinnable. The canvas forwards typed keys to the running
  program through the host, one key per call, queued; a headless run still
  answers empty, which is what the gates record.
- **`DRAW` and `WAIT`.** `DRAW "U20 R20 D20 L20"` executes GW-BASIC's path string
  through the turtle (U/D, R/L, E/H, F/G, B, N, M, C), and `WAIT ms` paces a frame
  loop.
- **File > Open Example.** A fresh install used to open onto an empty window with
  nothing to run. The example programs now travel with the install tree
  (`/usr/share/time-warp-studio/examples`) and the File menu lists them by
  language, so the first thing a learner does after installing is one click. A
  build tree finds them in `Examples/`; `TIME_WARP_EXAMPLES` points the menu at a
  folder of your own.
- **Something a non-technical user can install.** A release now ships a `.deb`, an
  `.rpm`, an AppImage and a tarball for x86_64 and arm64, plus a container image —
  all built from one tagged tree by `.github/workflows/release.yml`, with
  checksums beside every file. The AppImage is the one-file answer: double-click
  it, no root, nothing installed. The packages declare their Qt 6 and `python3`
  dependencies and put the binary, engine, desktop entry and icon where the system
  expects them.

### Fixed

- **The X on a tab closed nothing.** `setTabsClosable(true)` draws the button and
  then emits `QTabWidget::tabCloseRequested`, which nothing was connected to: the
  port had wired only `ResizableTabWidget`'s own signal (middle-click and the
  context menu), so the one control a user reaches for first was painted and
  inert. Forwarded in the tab widget's constructor, so both tab groups get it, and
  `ui_layout_tests` now clicks the real button rather than the signal.
- A docstring on `FunctionKeyBar` and on the toolbar described ports that had not
  happened; the disabled controls carry the reason instead.
- F3 was bound twice (Open on the toolbar, Find Next in the Edit menu), which Qt
  resolves by warning and firing whichever it prefers. F3 is Find Next, as in
  every other editor; Open keeps Ctrl+O.
- The lesson panel's verdict was hard-coded green-on-white, which is unreadable
  on the green-phosphor and amber themes. It takes its colours from the theme.
- A new tab opened after a theme was applied was syntax-coloured for the previous
  one: the colours were set once, at construction.
- `SyntaxHighlighter::setColors` updated the formats but not the rules, which hold
  *copies* of them, so a colour change repainted nothing.
- The turtle-speed slider was removed rather than finished: nothing animates the
  display list, so it controlled an ability the application does not have. The
  Settings button that replaced it opens the editor preferences.

### Added: the language ports

- **BASIC runs in C++.** The largest port, in five files: `basic.cpp` (variables,
  the load-time scan, the drawing helpers), `basic_statements.cpp` (the dispatch),
  `basic_text.cpp` (the reference's string expression evaluator),
  `basic_features.cpp` (shapes, fractals, music, particles, speech) and
  `basic_library.cpp` (the geometry those features draw). The core language,
  narrowing types, `SUB`/`FUNCTION`, records, the file channel, the VGA
  statements, `ON ERROR`/`RESUME` and the reference's Logo fallback are all
  there: a `.bas` file's `FORWARD 15` is Logo's `FORWARD`, drawn by Logo's turtle
  code, exactly as the reference forwards it. 137 programs run through both
  engines with **0 differences**, and seven cases join `lang_tests`. **Every one
  of the ten languages now runs in process**; nothing is routed to the Python
  engine at run time.
- **BASIC's remaining string and clock surface is covered.** `CHR$`, `FORMAT$`
  and `REPEAT$` printed nothing because PRINT read any name containing `$(` as a
  string array; the same defect existed in the reference and is fixed there too.
  `FORMAT$` now understands both `%` conversions and `#` masks, and `TIME$`,
  `DATE$` and `TIMER` read the local clock instead of answering empty/0. Seven
  parity rows and the command matrix cover them.
- **Four more languages** — **APL**, **FOCAL**, **CHIP-8** and **Smalltalk**, taking the
  lineup to ten. APL and FOCAL run line by line; CHIP-8 and Smalltalk run whole
  program. All four draw, all four are in the example corpus, and each has a
  command reference in `docs/LANGUAGE_GUIDE.md` and a tutorial in `docs/tutorials/`.
- **Command coverage for every language.** `command_table.py` grew from 279 rows to
  437, and every language now has zero commands that are neither exercised by a row
  nor waived — the four older languages had accumulated gaps (21 in PILOT, 18 in
  REXX) that the new rows closed.
- **APL runs in C++.** `native/src/lang/apl.cpp` is the port: numbers, strings and
  nested arrays; the glyphs and the keyword spellings (`IOTA`, `RHO`, `REDUCE` …);
  pervasive scalar functions, so `1 2 3 + 10` is `11 12 13`; reduce and scan; the
  structural functions; `∇` definitions; and the turtle words with `TX`, `TY` and
  `THETA`. The same 46 command rows and example run through both engines with **0
  differences**, and eight cases join `lang_tests`. APL is no longer bridged.
- **REXX runs in C++.** `native/src/lang/rexx.cpp` is the port: typeless string
  values, abuttal and `||` concatenation, the full operator set, `PARSE` and
  `PULL`, `IF` and the `DO` forms (count, `TO`, `OVER`, `WHILE`, `UNTIL`,
  `FOREVER`), `SELECT`, labels with `CALL`/`RETURN`, the built-in functions and
  the turtle words. 35 programs run through both engines with **0 differences**,
  and seven cases join `lang_tests`. REXX is no longer bridged.
- **Smalltalk runs in C++.** The object system is four files: `smalltalk.cpp`
  (tokens, parser, sends, printing), `smalltalk_library.cpp` (classes and
  primitives), `smalltalk_driver.cpp` (chunks and the top-level echo) and
  `smalltalk_internal.hpp`. Classes and methods are created at run time, blocks
  are real closures, `^` is a non-local return, cascades send to the original
  receiver, and the class library covers the corpus — numbers, collections,
  strings, booleans, Transcript and Turtle. 22 programs run through both engines
  with **0 differences**, and seven cases join `lang_tests`. Smalltalk is no
  longer bridged.

### Fixed: the language ports

- **FOCAL** — `ASK`'s answer never reached the program: the statement wrote to a
  private dictionary while the engine substituted from `interpreter.variables`, so
  the prompt appeared and the variable stayed 0. `FPI` and `FRAN` no longer require
  the parentheses FOCAL does not write, and `ASK "prompt", N` now accepts the comma
  every other FOCAL statement uses.
- **APL** — `⍕` (FORMAT) printed a character code rather than digits (`⍕42` answered
  `*`), and the keyword spellings `+REDUCE` and `+SCAN` did not work at all.
- **CHIP-8** — `JP V0, addr` encoded the register into the address's high nibble, so
  a jump table landed at address 6.
- **PILOT** — `LINK` and `LOAD` could never run: a line beginning `L:` is the label
  declaration, so the unit returned before it was reached and the long forms remapped
  to exactly such a line. It now reports that lesson chaining is not implemented
  instead of returning nothing.
- **REXX** — `SPACE(string, n, pad)` ignored its count, so `SPACE('a b c', 0)`
  answered `a b c`.
- **`command_inventory.py`** compared its "not a command" list case-sensitively while
  the list held lowercase entries, so those entries did nothing and the completeness
  report asked for rows that could not exist.
- **Unresolved names are reported, not echoed.** `PRINT UNSET` printed `UNSET`,
  `PRINT NOSUCHFN(3)` printed `NOSUCHFN(3)` with no error, and after `DEFSTR A-Z`
  `PRINT A` printed `A`. An unset numeric variable is 0, as BASIC defines it; an
  unknown function is an error. The evaluator's function table is also per-instance
  now, so one language's `EOF` registration no longer leaks into the other nine.
- **BASIC's expression functions.** `X = PEEK(1000)` and `PRINT IN(60)` work, and
  `STICK(n)`/`STRIG(n)` read the gamepad `JOYINIT` starts. `TYPE$(x)` answers the
  interpreter's type instead of printing nothing, and `SPEAK` reaches the speech
  handler.
- **BASIC array and function reads in `PRINT`.** `MID$("HELLO", 1, 3)` printed
  nothing because the `$(` of an array read claimed every `$`-suffixed function;
  `BED$(I)` printed nothing because the index was read as a literal instead of
  evaluated, which blanked two columns of `community_garden.bas`; and a `PRINT`
  that neither evaluator could read used to answer 0, hiding a typo, where the
  reference reports the evaluator's message.
- **`RESUME NEXT` without line numbers** resumes after the statement that failed; the
  error handler now records the line index as well as the BASIC line number.
- **COMAL `FUNC` in an expression.** `PRINT DOUBLE(21)` answers 42 in both engines
  through a new per-interpreter function hook; a body the executor cannot read
  reports an error rather than answering 0.
- **Logo `OUTPUT` reaches its caller.** `PRINT DOUBLE 21` prints 42 and `SHOW P`
  prints the value, in both engines.

### Lessons, for students and teachers

- **Twenty lessons, sixty checkpoints** — two for each of the ten languages, with a
  plan, a worked answer and hints for every checkpoint.
- **Checked by running the learner's program** in a headless engine, so `PRINT 6 * 7`
  and `PRINT 42` are the same answer, and a drawing checkpoint cannot be passed by
  printing about drawing. `expect_lines`, `expect_turtle` (pen segments, filled shapes
  and closure) and `expect_error` are the three kinds of expectation.
- **The 🎓 Lessons panel** in the IDE, the one page of the right column that is a real
  port rather than a placeholder. It hosts no curriculum and runs nothing itself: it
  drives the bridge, which is what keeps a panel's verdict and a teacher's mark the same
  function's answer. Six new protocol messages, with the solutions deliberately *not*
  in the curriculum payload — a lesson whose answer travels with its questions has been
  spoiled before it is read.
- **`native/tools/learn.py`** — list, walk, check, hint, answer, progress, reset, for a
  learner at a terminal.
- **`native/tools/teach.py`** — lesson plans with objectives and teacher notes, answer
  keys, curriculum validation, a class roster, handout distribution that never
  overwrites started work, marking that writes nobody's progress, class progress
  reports, zip bundles a class can be moved in, a presentation size for teaching from
  the front, and a lesson scaffold whose skeleton the loader accepts.
- **The curriculum is checked, not trusted.** `teach.py validate` runs all sixty
  solutions and all sixty starters through the engine: a solution that does not pass is
  a lesson nobody can finish, and a starter that already passes has nothing to do. Both
  are invisible in a file listing, because lessons are data and nothing else runs them.
- **`docs/lessons/`** — a hub, a student guide, a teacher guide, a technical reference
  for the curriculum format and protocol, and generated plans for all ten languages.
- **`TIME_WARP_DATA_DIR`** now relocates everything the studio keeps for a user. It
  exists because a test recorded lesson hints into a real learner's `progress.json` —
  the hint counter being cumulative is how the leak surfaced.

### For contributors, and for the repository itself

- **Community files.** Issue forms for a bug, a feature and a language, a pull-request
  template, `SUPPORT.md`, `CODEOWNERS` and a deployment guide, so the questions a
  newcomer asks first have somewhere explicit to go. The bug form asks which
  environment the problem happened in, because a built-from-source IDE and an
  installed one take different paths to the bridge.
- **`docs.yml` now checks something.** It regenerates the lesson plans and fails on a
  diff, requires a tutorial for every language, requires a file for every guide
  `docs/INDEX.md` promises, and runs `teach.py validate`. A curriculum that has
  drifted from the plans a teacher would teach from is invisible in a file listing; it
  is a red check now.
- **Corrected counts.** Several documents still described five bridged languages and
  44 examples, the contributing guide listed six languages, and it offered a
  discussion board the project does not answer on.
- **A credential cannot be committed.** `.pre-commit-config.yaml` now runs
  `detect-secrets` against a committed baseline, so a key pasted into a file fails
  the commit instead of reaching GitHub. The baseline is what keeps the hook usable:
  it records what the tree already holds, so only a secret added *since* is reported
  — and a hook that cries wolf gets switched off. `CONTRIBUTING.md` says how to
  install the hooks and how to silence one false positive at a time; `SECURITY.md`
  says what to do if a secret does reach a commit: rotate it, then rewrite the
  history, because removing it in a later commit does not remove it from the clones.
- **The lint hook passes over the whole tree.** `native/tools/` had never been
  linted, and two findings surfaced when it was: a `lambda` assigned to a name in
  `scheme_differential_check.py`, and an ambiguous `l` in `teach.py`. Both are fixed
  rather than excluded, so `pre-commit run --all-files` is green.

### Syntax colouring in the editor

- **Per-language highlighting** — `native/src/ui/syntax_highlighter.cpp`, one rule
  table per language. The ten disagree about what a comment is: REXX has `--` and
  `/* ... */`, Smalltalk's comment is a **double**-quoted string while its string is a
  single-quoted one, FOCAL has no comment syntax at all, and PILOT's units are a letter
  and a colon at the start of a line. So the rules are data rather than one shared
  approximation with a keyword list bolted on, and the keyword tables are the engine's
  own command names — a highlighter that colours a word the engine would reject teaches
  the wrong vocabulary.
- **Twelve CTest suites.** `syntax_highlighter_tests` colours a bare `QTextDocument`
  and asserts the colour at a named position. That is the only kind of check that
  works here: coloured text one token out of place still looks like a working editor.

### Find, replace and go-to-line

- **`FindBar`** — a bar under the editor tabs, hidden until Edit > Find asks for it.
  Replace is the same bar with a second row. Enter finds the next match, Shift+Enter
  the previous one, Escape closes it and F3/Shift+F3 keep finding with it closed.
- **The searching lives on `CodeEditor`** — `findNext` (wrapping, which is what makes
  a search that started midway through the file reach the text above it), `matchCount`
  (re-counted whenever the document changes, so it cannot claim matches that have been
  deleted), `replaceAll` (one undo step: replacing a document in twelve pieces means
  twelve undos to get back) and `replaceCurrent`.
- **Go to Line** — Ctrl+L. Not Ctrl+G, which the canvas grid already owns: two actions
  on one key is an ambiguous shortcut that Qt resolves by guessing.
- The About box now names all ten languages instead of the original six.

### The last session comes back

- **Recent files** — File > Open Recent lists the last ten paths, most recent first,
  with a Clear Menu entry. Opening a file that is already open bumps it to the top
  rather than adding a second entry: raising a tab is using the file.
- **The session is restored at launch** — the documents that were open come back,
  unless a file was named on the command line (a file argument is a request for that
  file) or `--screenshot` is in use (a screenshot has to be reproducible from the
  command line, not from somebody's last session). A file that has been deleted or
  moved since is skipped silently, because a prompt at startup that cannot be
  dismissed is worse than a tab that is not there.
- **`saveLayout` and `saveSession` are separate** — `--reset-layout` puts the window
  back to its defaults without forgetting what was being edited.

### Save the drawing

- **File > Export Drawing** (Ctrl+E) writes PNG, JPEG or BMP; **Edit > Copy Drawing**
  (Ctrl+Shift+C) puts it on the clipboard; and `time_warp_ide --run program.logo
  --export drawing.png` does the same from a script, which is what a teacher rendering
  a class's work needs and what the tests use.
- **The export is the drawing, not a screenshot.** It goes through the same `paintInk`
  the window paints with, so the picture cannot drift from what is on screen, and it
  leaves out the grid and the turtle marker because those are chrome. It renders at
  twice the on-screen size, because a picture worth saving is worth the pixels.
- **SVG and PDF are not in this release.** They need the QtSvg and QtPrintSupport
  components, which the build does not ask for; PNG/JPEG/BMP need nothing that is not
  already linked.
- Exporting an empty canvas says so in the status bar rather than writing a blank
  picture.

### What a run actually did: Variables, Telemetry and Debug

The right column's three remaining placeholders are real panels now. Both
inspection panels read data the engine always had and nobody had asked it for.

- **🔍 Variables** lists every binding the engine can see, with the type it knows it by
  and its value, and filters by name. The bridge grew `time_warp/utils/inspection.py`
  to turn engine state into rows (`variable_rows`), so a learner sees `NAME = ada` and a
  programmer sees `REPCOUNT = 4` without either of them reading a debugger.
- **⚡ Telemetry HUD** shows what the run cost and what it did: steps used out of the
  cap, engine time, steps per second, last line, recursion depth, call stack depth,
  variables, turtle pose and pen, output lines and drawing commands, plus BASIC's
  16-byte memory window. Beside them sit the rows only the IDE can vouch for — wall
  clock, and how much ink reached the canvas — so a drawing the engine counted but the
  canvas never received reads as two rows that disagree.
- **🐞 Debug** drives `DebuggerRunner` from the panel: Continue, Pause, Step In, Step
  Over, Step Out, Stop, and a breakpoint list. Pausing shows the line, the procedure,
  the call stack and that frame's bindings. Only Scheme can be stepped — the nine
  bridged languages run to completion in another process with no pause protocol — so
  the panel disables itself with a sentence saying why rather than offering buttons that
  do nothing.
- **A run that never paused still leaves bindings.** `DebuggerRunner` now takes a final
  snapshot when a program ends, so the Variables panel shows what the program finished
  with instead of nothing, and `DebugVariable` carries the machine's own `type_name`
  rather than a guess made from the printed text.
- The three panels are emptied at the start of every run: a previous run's numbers
  sitting in a HUD while a new one starts read as this run's.

### FOCAL, PILOT, COMAL, CHIP-8 and Logo run in C++, and the engine for the rest

Five languages have left the bridge. The change that matters is not the ports
themselves but the seam they establish: an executor is now a C++ function, and the
IDE asks `executor_for(language)` whether it can run in process.

- **`timewarp_lang`** — a dependency-free C++20 library with the shared interpreter
  loop: program storage, variables, streamed output, synchronous input, the run
  budget and telemetry. An executor is a plain function
  `(Interpreter&, source, TurtleHost&) -> std::string`, the same shape as the
  Python engine's, because the reference's behaviour is what the port is checked
  against.
- **`TurtleHost`** — the drawing seam, deliberately Qt-free. A test records calls
  as text; the IDE's `NativeCanvasHost` turns them into `CanvasCommand`s.
- **FOCAL** is ported whole, including the rules a reimplementation gets wrong:
  branching by sign, `DO` returning at the end of the program, undefined variables
  as zero, and `!` suppressing a newline inside one `TYPE`.
- **PILOT** is ported whole: the unit letters and their `Y`/`N` conditions,
  labels and `U:`/`S:` subroutines with `E:` returns, `C:`'s string functions,
  `G:` graphics, `F:` files, hints, delays, pauses and the long-form keywords.
  Its `C:`/`B:` arithmetic runs on the native expression evaluator, so `timewarp_expr`
  is now linked into a Run rather than merely built.
- **COMAL** is ported whole: IF/ELIF/ELSE/ENDIF, FOR/NEXT, WHILE/ENDWHILE,
  REPEAT/UNTIL, LOOP/EXIT/ENDLOOP, PROC/FUNC with CALL/RETURN, DIM arrays,
  DATA/READ/RESTORE, GOTO/GOSUB, PRINT/INPUT and the turtle statements. Its loop
  frames live on an explicit stack, so `NEXT` finds its own `FOR` through blocks
  that have already closed.
- **CHIP-8** is ported whole: the two-pass assembler, all 35 opcodes, the 1977
  choices (Vx shifts in place, sprites wrap) and the frame model, with the same
  limits reported instead of silently applied.
- **Logo** is ported whole, and it is the one that is not a line-by-line language:
  a `TO ... END` block *defines* a procedure rather than running, and a bracketed
  command can span lines, so the statement boundaries are decided by a loader
  before anything executes. Everything the reference does is reproduced, including
  the parts that look like bugs: `LOCAL "X` really does make a variable called
  `"X`, a list printed through `SHOW` comes out as `['1.0', '2.0']`, and the
  splitter that starts a new statement at a procedure name once all of that
  procedure's arguments are present (the rule that kept a second call inside a
  `REPEAT` body from being swallowed, which is how a whole layer of
  `Examples/logo/mandala.logo` used to vanish).
- **The drawing seam grew the parts Logo needed** — a 3-D pose with the
  reference's Rodrigues rotations and its `1 + z/400` projection, so
  `turtle_3d.logo` still draws a helix instead of a flat spiral; `draw_polygon` for
  `STAMP`, `DOT` and `FILLED`; a font size on `draw_text` for `LABEL`; and `reset`
  for `CS`, which is not `CLEAN`: it puts the turtle, the pen and the canvas back
  the way a run starts.
- **A native run now starts on the reference's surface** — the dark background and
  the white pen — because a logo-style program may draw before it names a colour,
  and white ink on the IDE's white canvas is a run that reports four strokes and
  shows nothing.
- **The telemetry HUD measures whole-program runs.** An executor with its own
  statement loop reports its steps (`Interpreter::note_step`), so a Logo run's
  Steps row is a number rather than a standing zero.
- **Python-compatible randomness** — `FRAN` and `RND` run on a reproduction of
  CPython's Mersenne Twister, seeded the way `random.seed(int)` seeds it. A port
  with `std::mt19937` would be a different generator, and the differential harness
  could not tell a defect from a different sequence.
- **The IDE dispatches natively.** `MainWindow` gives a ported language to
  `NativeRunner`, a `QThread` that streams output, answers input prompts and feeds
  the canvas queue. The Variables and Telemetry panels are filled from the same
  run, so they do not know which engine produced the table.
- **A parity gate** — `native/tools/lang_differential_check.py` runs every
  command-table row and every example through both engines and compares output
  lines, every drawing call in order, and the final variables: **225 of 228
  programs agree, 0 differences**, the three that use a random function being
  reported and skipped. The corpus includes the nine Logo examples, among them the
  720- and 800-iteration fractal drawings, so the gate covers recursion, procedure
  restoration and the statement splitter as well as the primitives.
- BASIC, REXX, APL and Smalltalk still run through the bridge, and the bridge
  remains the thing to delete. Each port removes one path through it.

---

## [15.0.0] - 2026-10-16

The implementation was replaced wholesale: Time Warp Studio is now a C++20 / Qt6
desktop application, and the language lineup and packaging that went with the previous
implementation are gone. Both changes were made after `v14.1.2` and shipped together,
so both are recorded here.

### The Qt application is now the product

Time Warp Studio is a **C++20 / Qt6 desktop application** (`native/`). The PySide6
implementation that came before it has been **removed**; what remains of Python is a
temporary bridge the Qt application spawns for the five languages that are not ported
yet.

#### Added

- **Qt application** (`native/`) — `time_warp_ide`: documents and tabs, code editor
  with line-number gutter and current-line highlight, streaming output console with
  interpreter errors in their own tab, turtle canvas with zoom, pan, grid and
  auto-fit, menus, toolbar and status bar, and `make install` with a desktop entry
  and icon.
- **Native engines** — the expression evaluator (`timewarp_expr`) and Scheme
  (`timewarp_scheme`, an explicit-stack CEK machine), each with a parity gate against
  the Python reference (153 and 167 cases, zero unexplained divergence).
- **Language bridge** (`native/bridge/`) — a JSON-lines worker the application spawns
  for BASIC, PILOT, Logo, COMAL and REXX. It is scheduled for deletion one language at
  a time as each is ported.
- **Test and examination tooling** — 11 CTest suites, the bridge engine's own suite,
  `example_smoke.py` (all 44 examples through the real bridge), `command_matrix.py`
  with `command_table.py` (one row per documented command, 279 rows), and
  `command_inventory.py`, which extracts the implemented command set to check that
  table for completeness.
- **CI** (`.github/workflows/native.yml`) covering the build, the suites and the gates.

#### Removed

- The entire PySide6 application, its launchers (`run.py`, `run.sh`), its Docker and
  developer tooling, and the workflows that packaged and published it.
- The C, Pascal, Prolog, Forth, Brainfuck and Python executors.

#### Fixed during the port's examination

Examining every documented command through the real bridge turned up defects that no
gate had been looking for:

- **A keyword boundary bug in BASIC.** `startswith("DO")` also matched `DOG = 42`, so
  the `DO` statement swallowed the assignment: the variable was never set and `PRINT`
  then echoed the name back. Any variable named `DOUBLE`, `RUNNER`, `CLOSER`, `NEXTX`
  or similar silently lost its value. Eleven keywords were affected.
- **`ON TIMER` could never work.** The dispatcher sliced one character too many,
  discarding the `(`, so the handler rejected the format its own error message asked
  for.
- **`SHOW` and `TYPE` in Logo never evaluated their argument.** Both returned their
  source text, so `SHOW 1 + 2` displayed `1 + 2`. `PRINT` had always worked, so the
  two disagreed about the same expression.
- **A reporter could not be applied to a list literal** — `FIRST [1 2 3]` received
  just `[`, because argument consumption balanced parentheses but not brackets.
- **An input statement stopped the program.** The worker called `execute()` once, so
  every program that read input ended at its first prompt; resumed runs needed a loop.
- **The worker ran a program inline**, which meant `in-reply` and cooperative stop
  could never be delivered while a program was running. Stop now works.
- **`JOYINIT`** reported an absent *optional* backend with the error prefix, which the
  IDE presents as the program failing.

### Language lineup change — six active languages

Time Warp Studio now ships exactly **six** languages: **BASIC, PILOT, Logo,
COMAL, REXX, and Scheme**. The C, Pascal, Prolog, Forth, Brainfuck, and Python
executors have been removed from the product, along with their examples,
tutorials, snippets, lessons, and tests.

#### Added

- **COMAL** executor (`languages/comal.py`) — structured BASIC with
  `IF/ELIF/ELSE/ENDIF`, `FOR/NEXT`, `WHILE/ENDWHILE`, `REPEAT/UNTIL`,
  `LOOP/ENDLOOP`, `PROC/ENDPROC` + `CALL`, `DIM`, `DATA/READ`, `GOTO`/`GOSUB`,
  and turtle `MOVE`/`TURN` commands.
- **REXX** executor (`languages/rexx.py`) — typeless string processing with
  `SAY`, `PULL`, `PARSE`, `IF/THEN/ELSE`, `DO` (TO/BY/WHILE/UNTIL/FOREVER/
  OVER), `SELECT/WHEN`, labels + `CALL`/`ARG`/`RETURN`, `LEAVE`/`ITERATE`, a
  full expression evaluator (including `||` and abuttal concatenation) and 20+
  built-in string/number functions.
- **Scheme** executor (`languages/scheme.py`) — whole-program Lisp interpreter
  with lexical closures, `define`/`lambda`, `if`/`cond`/`case`/`when`/`unless`,
  `let`/`let*`/`letrec`/named `let`, `quote`/`quasiquote`, `set!`, list and
  vector primitives, `map`/`filter`/`foldl`/`apply`, and turtle bindings.
- New tutorials (`docs/tutorials/comal.md`, `rexx.md`, `scheme.md`), example
  programs (`Examples/comal/`, `Examples/rexx/`, `Examples/scheme/`), code
  snippets, and two guided lessons per new language.

#### Removed

- Executors: `brainfuck.py`, `c_lang_fixed.py`, `forth.py`, `pascal.py`,
  `prolog.py`, `python_lang.py`, plus their tests, examples, and tutorials.
- `Language` enum members `C`, `PASCAL`, `PROLOG`, `FORTH`, `BRAINFUCK` and
  `PYTHON_LANG`.

#### Changed

- `_WHOLE_PROGRAM_EXECUTORS` now contains Scheme only; the subprocess sandbox
  applies to Scheme.
- Syntax highlighting, comment toggling, keyword reference cards, document
  outlines, code formatters, snippet library, lessons, status-bar emoji,
  project icons, telemetry HUD, and output prompts all cover the new lineup.
- Example verifier default language map updated (`comal`, `rexx`, `scheme`).

All notable changes to Time Warp Studio will be documented in this file.

> **Scope note:** The current release actively supports **6 languages**:
> BASIC, PILOT, Logo, COMAL, REXX, and Scheme, and Python. Older
> entries below mention additional language executors (Smalltalk,
> Perl, Ruby, Erlang, Haskell, JavaScript, Lua, HyperTalk, PostScript,
> LISP/Scheme, Tcl, 6502 Assembly) that were built at the time but have since
> been removed from `time_warp/languages/` and are no longer part of the
> product. These entries are kept as historical record only.

## [14.1.2] - 2026-10-15

### Packaging

- Matched the Python distribution name to the existing PyPI project `timewarp-ide`.

## [14.1.1] - 2026-10-15

### Packaging

- Included the desktop and standalone editor entry-point modules in the Python wheel.
- Documented installation directly from the GitHub Release wheel.

## [14.1.0] - 2026-10-15

### Studio Improvements

- Added a lessons dock with built-in lessons and checkpoint verification.
- Added a Borland Turbo-style keymap and live Function-Key Bar.
- Expanded graphics, sound, and Prolog capabilities.
- Removed obsolete prototypes and aligned documentation and packaging with the supported Studio.

## [14.0.0] - 2026-10-01

### Editor and Workflow Improvements

- Added Go to Line navigation, clickable error locations, and error-only output filtering.
- Added a searchable document outline for supported procedure and function declarations.
- Added recovery for unsaved editor tabs after an unexpected shutdown.
- Made lesson authoring and file-dialog filters use the canonical nine-language registry.
- Corrected current release documentation and package metadata to match the active language set.

## [13.0.0] - 2026-06-01

### New Languages

- **Smalltalk executor** (`languages/smalltalk.py`) — Smalltalk-80 interpreter (~830 lines) supporting:
  - `Transcript show:/showCr:/print:` for output; temp variable declarations `| x y |`
  - Assignment `:=`, conditional `ifTrue:/ifFalse:/ifTrue:ifFalse:`
  - Iteration: `timesRepeat:`, `to:do:`, `to:by:do:`, `do:`, `whileTrue:`, `whileFalse:`
  - Collections: `OrderedCollection new`, `add:`, `at:put:`, `select:`, `collect:`, `reject:`, `inject:into:`
  - String messages: `,` (concat), `size`, `reversed`, `asUppercase`, `asLowercase`, `copyFrom:to:`
  - Block closures `[...]` with correct outer-scope variable sharing
  - Number messages: `factorial`, `sqrt`, `abs`, `max:`, `min:`, `raisedTo:`, `gcd:`
  - Turtle graphics via `Turtle forward:`, `Turtle right:`, etc.
  - 31/31 tests passing; 5 example programs in `Examples/smalltalk/`

### Syntax Highlighting

- **Smalltalk** — keyword/unary/binary message highlighting, `"..."` comment style, string literals

## [12.0.0] - 2026-06-01

### New Languages

- **Perl 5 executor** (`languages/perl.py`) — full Perl 5 interpreter (~2450 lines) supporting:
  - Scalar, array, and hash variables with proper scoping (`my`, `our`, `local`)
  - Control flow: `if`/`elsif`/`else`/`unless`, `while`/`until`, `for`/`foreach` (C-style and range), `do...while`
  - Statement modifiers: `print "x\n" if $cond;`, `last if $i > 3;`, `next if $i == 3;`
  - Subroutines with `@_` argument passing, closures, and recursive calls
  - Regular expressions: matching (`=~`), substitution (`s///`), global (`/g`), capture groups (`$1`–`$9`)
  - String builtins: `uc`, `lc`, `length`, `substr`, `index`, `rindex`, `reverse`, `sprintf`, `chomp`, `chop`
  - Array builtins: `push`, `pop`, `shift`, `unshift`, `splice`, `sort`, `reverse`, `map`, `grep`, `join`, `split`
  - Hash builtins: `keys`, `values`, `each`, `exists`, `delete`
  - Math builtins: `abs`, `int`, `sqrt`, `sin`, `cos`, `atan2`, `log`, `exp`, `rand`, `srand`
  - Turtle graphics: `forward`, `backward`, `left`, `right`, `penup`, `pendown`, `color`, `setheading`, `home`
  - 36/36 tests passing; 5 example programs in `Examples/perl/`

### Bug Fixes

- **Perl executor** — fixed 8 bugs found during test development:
  - `$i++` incorrectly parsed as binary `+` → infinite loops in `while` and C-style `for`
  - `_split_statements` did not split at `}` when outermost block closed → trailing statements after loops were silently dropped
  - Statement modifier loop ran after `print`/`last`/`next`/`return` handlers → `last if $cond` treated `if` as a label
  - `_contains_block` didn't detect unclosed string quotes → false modifier matches
  - `"a" . "b"` matched as a single double-quoted literal by `endswith('"')` check
  - `push @a, 3` extended array with wrong items when `@a` was expanded during arg evaluation
  - `split(/,/, $str)` failed because `/,/` was split at the comma by `_split_by_comma`
  - `_emit` split on `\n` eagerly → partial-line `print "$i "` calls produced separate output lines instead of accumulating

## [11.0.0] - 2026-06-01

### Major Features

- **Project File-Tree Panel** (`ui/project_tree.py`) — new `ProjectTreePanel` docked on the far left of the main window. Shows all tracked project files with language emoji icons; double-click to open in a new tab; right-click context menu: Add File, Remove from Project, Set as Main File. Auto-populates when a `.twsproj` project is opened and saves on any change.

- **Plugin Manifest Validation + Capability Sandboxing** (`features/plugin_system.py`) — plugins are now scanned with `ast.parse()` (no code execution) before loading. Plugins that import dangerous modules (`ctypes`, `cffi`, `subprocess`, `socket`, etc.) without declaring the corresponding capability in a `manifest.json` file are blocked. Blocked plugins are listed with a clear error message; safe plugins load unchanged.

- **Plugin Marketplace SQLite Persistence** (`features/plugin_marketplace.py`) — marketplace listings, reviews, releases, developers, and templates now survive application restarts via a SQLite database at `~/.time_warp/databases/marketplace.db`. All in-memory state is loaded from the database on startup and written on every mutation.

- **Real LAN Collaboration TCP Send/Receive** (`features/collaboration.py`) — `_send_message()` now actually transmits to peers over TCP using a 4-byte length-prefixed JSON framing protocol. Host binds on port 54321; joiner connects at session join. Background `_recv_loop` threads dispatch incoming messages (code updates, cursor moves, chat, sync requests) to registered event callbacks. `disconnect()` cleanly closes all sockets.

- **Student Roster + Assignment Distribution** (`features/classroom_mode.py`) — added `Student` and `StudentRoster` dataclasses, persisted to `~/.time_warp/classroom_roster.json`. New methods: `add_student()`, `remove_student()`, `save_roster()`, `distribute_assignment()` (writes bundle files to each student's work directory), and `collect_submissions()` (zips all student work into a timestamped archive in the bundles directory).

- **Persistent Terminal History** (`ui/terminal_widget.py`) — shell commands are now appended to `~/.time_warp/terminal_history` (plain text, one per line). On next launch the history is pre-loaded into the up/down arrow history buffer (up to 200 entries in memory, unlimited on disk).

### Previously Completed (v10.x)

- **Assembly 6502 executor** (`languages/asm6502.py`) — 2-pass assembler, 64 KB virtual machine, 40+ opcodes, all addressing modes
- **Code folding** (`ui/editor.py`) — ▼/▶ gutter glyphs, `toggle_fold()`, hides/shows QTextBlock ranges
- **Split editor** (`ui/main_window.py`) — `QSplitter`-based dual-pane editing (View → Split Editor)

### Bug Fixes

- **Ruby executor** (`languages/ruby.py`) — full initial implementation with extensive bug fixes:
  - `_rfind_op` now guards against treating `<<` (append operator) as two separate `<` operators — `r << x` no longer mis-splits into `r <` and `x`
  - `_split_statements` pre-expands semicolon-separated statements so `stmt1; stmt2` inside `{...}` blocks is correctly treated as two statements
  - `break if COND` / `next if COND` / `return if COND` modifier-only forms now handled correctly (no left-hand value before the keyword)
  - Negative array subscript access (`seq[-1]`, `seq[-2]`) now works via `_eval_chain`
  - Turtle API bindings corrected: `penup()`, `pendown()`, `setheading()`, `pencolor()`, `setpenwidth()`
  - Multi-line `do...end` blocks no longer split at binary operators (`\n` guard in `_eval`)
  - Multi-line array/hash literals now grouped correctly via `paren_depth` tracking in `_split_statements`
  - Assignment regex uses `re.DOTALL` so multi-line values are captured
  - String interpolation `#{}` correctly handles nested braces via brace-depth tracking in `_eval_dstring`
  - Block auto-splatting: `{|word, count| ...}` blocks automatically destructure single-array arguments
  - `||=` and `&&=` compound assignment operators now supported
  - All 5 Ruby demo programs pass; 217/217 coverage tests pass

- **Python executor** (`languages/python_lang.py`) — fixed all 4 Python demo programs

- **HyperTalk executor** (`languages/hypertalk.py`):
  - Fixed `BREAK`/`CONTINUE`/`RETURN` signals not propagating out of `if...end if` blocks — `exit repeat` inside an `if` block no longer causes the enclosing `repeat while` loop to run all 100,000 iterations
  - Fixed compound arithmetic expressions with embedded function calls (e.g. `put random(25) + 15 into var`) — the Python-eval fallback now pre-substitutes function calls before evaluation, so `random(n) + k` evaluates correctly
  - Fixed `_call_fn` RETURN value handling: `_exec_lines` unwraps `("RETURN", val)` tuples before returning, so call sites now check `if result is not None` instead of the tuple wrapper. User-defined functions now correctly return their computed values.
  - Fixed greedy function-call regex: replaced `^(\w+)\s*\((.*)\)$` with a balanced-parenthesis scan so expressions like `sin(r) / cos(r)` are no longer misidentified as a single function call.
  - Wrapped `float(args[0])` in `try/except` to prevent `ValueError` when a built-in receives a non-numeric first argument.
- **Erlang executor** — fixed boolean pattern matching: atoms `true`/`false` in `case` clauses now match Python `True`/`False` from built-in functions like `is_prime/1`
- **Erlang executor** — fixed `_split_fun_clauses` infinite loop caused by semicolons inside `if...end` blocks being incorrectly treated as clause separators; keyword-depth tracking (`if/case/begin/receive...end`) now prevents this

### Demo Program Fixes (timeout → 0 timeouts)

- **`Examples/erlang/number_theory.erl`** — reduced amicable-pairs search range (2000→300) and perfect-numbers loop (1..50→1..30) to complete within the 15-second test budget
- **`Examples/forth/mathematical_wonders.forth`** — fixed `SIEVE-MARK` stack corruption (`DUP *` → `DUP DUP *`), removed erroneous `ELSE DROP` in `BUILD-SIEVE`, added `4DUP` word definition, added `FVARIABLE`/`F@`/`F!`/`F+!` support to the Forth executor, reduced Hanoi threshold to avoid deep Python recursion
- **`Examples/lua/turtle_patterns.lua`** — reduced all spiral/curve step counts 10× (3000→300, 5000→500, etc.)
- **`Examples/hypertalk/adventure_game.htalk`** — resolved via the HyperTalk executor bug fixes above

### New Language Features

- **HyperTalk** — `find [whole|partial|string|chars|word] <text> [in <container>]` command: searches a variable or the output buffer and stores the match in `it` (roadmap Q2 2026)
- **HyperTalk** — `go [to] <card|next|prev|first|last>` navigation command (logs destination; no-op in desktop mode)
- **Prolog** — `\+(Goal)` negation-as-failure now parsed correctly from source (previously only worked when called from rule bodies, not in queries)
- **Prolog** — arithmetic comparison operators (`<`, `>`, `=<`, `>=`, `=:=`, `=\=`) now accept arbitrary arithmetic expressions on both sides, not just bare variable names
- **Prolog** — new built-ins: `format/2` (with `~w`/`~d`/`~a`/`~n` directives), `string_concat/3`, `number_string/2`, `split_string/4`, `string_length/2`, `string_lower/2`, `string_upper/2`
- **Forth executor** — `FVARIABLE`, `F@`, `F!`, `F+!` for float variable storage

### Infrastructure

- **`run.py`** — launcher now installs core deps (PySide6, Pillow) first; if the full `requirements.txt` install fails (e.g. `asyncpg` compilation issues on some platforms), a warning is shown and the desktop IDE still launches successfully
- **`Examples/CATALOG.md`** — added `javascript/functional.js` and `javascript/async_patterns.js`

## [10.1.0] - 2026-04-25

### Examples (Round 2 — 20 new showcase programs)

- **BASIC** — `space_simulation.bas`: Solar system explorer with planetary data table, Kepler's third law, light travel times, escape velocities, and turtle orbital graphics
- **Logo** — `geometric_artistry.logo`: Koch snowflake grid, Islamic 8-star pattern, rosette mandala, hexagonal tiling, and optical art spiral
- **PILOT** — `math_drill.pilot`: 20-question arithmetic flashcard drill (addition, subtraction, multiplication, division) with letter grade scoring
- **C** — `game_of_life.c`: Conway's Game of Life with glider, blinker, toad, beacon, R-pentomino, and pulsar patterns on a toroidal grid
- **Pascal** — `grade_book.pas`: Student grade book for 20 students with letter grades, GPA, statistics (mean/median/std dev), and score histogram
- **Forth** — `cellular_automata.forth`: 1D elementary cellular automata — Rule 30 (chaos), Rule 90 (Sierpiński), Rule 110 (Turing-complete), Rule 184 (traffic)
- **Prolog** — `family_genealogy.pl`: 4-generation family tree (22 members) with relationship inference — cousins, aunts/uncles, ancestors, and generational distance
- **Python** — `statistics_suite.py`: Statistical analysis with mean/median/mode/quartiles/skewness/kurtosis, z-scores, normality checks, and turtle bar chart
- **Lua** — `turtle_patterns.lua`: Mathematical curves — spirograph (hypotrochoid/epitrochoid), rose curves, Lissajous figures, Archimedean spirals, star polygons
- **JavaScript** — `mandelbrot.js`: ASCII Mandelbrot set, 4 Julia sets, Burning Ship fractal, Tricorn; zoom views (Seahorse Valley, etc.) with density gradient chars
- **Ruby** — `word_games.rb`: Anagram finder, Scrabble scorer, palindrome checker (14 phrases), word frequency bar chart, Pig Latin translator, edit distance
- **Haskell** — `morse_code.hs`: Morse encoder/decoder with binary tree for O(log n) decode, timing analysis, dot/dash ratio, letter complexity, round-trip verification
- **Scheme** — `lambda_calculus.scm`: Lambda calculus evaluator — Church booleans/numerals/pairs/lists, Y combinator, beta reduction, SKI combinators
- **Erlang** — `number_theory.erl`: Number theory library — prime sieve, factorization, GCD/LCM, Euler's totient, amicable pairs, modular arithmetic, CRT, Collatz, Catalan
- **Smalltalk** — `ascii_patterns.st`: Pascal's triangle, number spiral, diamonds (solid and numbered), checkerboard, Sierpiński triangle, multiplication table, nested boxes
- **HyperTalk** — `calculator.htalk`: Scientific calculator — arithmetic, powers/roots, factorials, trig (sin/cos/tan in degrees), logarithms (log2/log10/ln), unit conversion, statistics
- **Rust** — `game_of_life.rs`: Conway's Game of Life with `Grid` struct/impl, toroidal boundaries, 7 classic patterns, period detection, and simulation statistics
- **Perl** — `text_analytics.pl`: Text analytics — Flesch-Kincaid readability, word frequency with bar chart, bigram/trigram n-grams, sentence statistics, KWIC concordance

### Documentation

- **Examples/CATALOG.md**: Updated from 87 to 107 programs; added all 20 Round 2 entries under each language section
- **README.md**: Updated Examples project tree from 67/19-language counts to 107/20-language counts with accurate per-language file totals

## [10.0.0] - 2026-04-25

### New Languages

- **Perl 5** — scalars/arrays/hashes, regex (`=~`, `s///`, `qw()`), subroutines, turtle graphics commands (`forward`, `back`, `left`, `right`, `penup`, `pendown`, `pencolor`, `setpos`, `circle`)

### Graphics Enhancements

- **SVGA mode** — `set_svga_mode(width, height)` on `TurtleState` enables a fixed-resolution virtual canvas (default 800×600) rendered with full anti-aliasing inside a chrome bezel; `clear_svga_mode()` returns to unlimited coordinates
- **Cubic Bezier curves** — `bezier_curve(cp1x, cp1y, cp2x, cp2y, end_x, end_y)` draws a smooth vector path from the current turtle position
- **Gradient fills** — `draw_rect_gradient()` and `draw_ellipse_gradient()` fill shapes with linear or radial colour gradients using stop-based colour maps
- **Sprite system** — `define_sprite(name, pixel_rows, palette)` defines named pixel-art sprites from string rows; `place_sprite(name, x, y, scale, rotation, z_order)` stamps them; `move_sprite(name, dx, dy)` translates the most recent instance
- **Advanced pen styles** — `set_pen_style(dash, cap, join)` controls dash patterns, line caps (`round`/`flat`/`square`), and joins (`round`/`miter`/`bevel`) for all subsequent shapes
- **Z-ordering** — shapes are now sorted by `z_order` before rendering; higher numbers paint on top
- Canvas: added `QLinearGradient`/`QRadialGradient`/`QPainterPath`/`QPolygonF` rendering paths; gradient-fill support extended to polygons and ellipses; sprite shape type rendered with per-pixel alpha, scale, and rotation transforms

### Sound / Music Enhancements

- **ADSR envelopes** — new `ADSREnvelope` dataclass with `attack_ms`, `decay_ms`, `sustain_level`, `release_ms`; automatically shapes amplitude for each note
- **New waveforms** — `pulse` (configurable duty cycle, `@Pn` MML command) and `noise` (white noise, `@W5`)
- **4-channel polyphony** — new `MusicChannel` dataclass; `MusicPlayer.mix_channels()` sums up to 4 independent synthesised streams into a single WAV; `parse_multichannel(dict)` convenience wrapper
- **Extended MML syntax** — `@Wn` (waveform 0–5), `@Datk,dcy,sus,rel` (ADSR), `@Pn` (pulse duty %), `@Cn` (channel 0–3), `[CEG]` chord notation for simultaneous notes
- 4 new built-in sound effects: `DRONE`, `BUZZ`, `STATIC`, `BEEP2`

### Language Executor Improvements

- **APL**: outer product operator (`∘.fn` e.g. `A ∘.× B`), format `⍕`, execute `⍎`
- **C**: `struct`/`union` type definitions, struct variable declarations, member access (`p.x`), `typedef` for scalar types

### UI Improvements

- **Canvas**: Added "Export SVG…" (🖼) toolbar button alongside existing "Save PNG…"
- SVG export via `PySide6.QtSvg.QSvgGenerator` with antialiasing and full turtle graphics rendering

### Examples & Documentation

- Added Ruby examples: `Examples/ruby/hello.rb`, `Examples/ruby/oop_demo.rb`
- Added Erlang examples: `Examples/erlang/hello.erl`, `Examples/erlang/pattern_match.erl`
- Added Rust examples: `Examples/rust/hello.rs`, `Examples/rust/structs.rs`
- Added tutorial docs: `docs/tutorials/ruby.md`, `docs/tutorials/erlang.md`, `docs/tutorials/rust.md`

### UI & Learning Experience

- Added a new Learning Hub with guided challenge launchers, remix actions, quick access to reference material, and one-click tutor help
- Added a Project Explorer with recent-project tracking, persistent workspace roots, and direct file opening from the IDE
- Added File menu and toolbar actions for opening project folders and revisiting recent projects
- Improved the AI Assistant with beginner-friendly tutor summaries for the current code context
- Updated welcome/help surfaces to highlight the new learning and project workflows

## [9.0.0] - 2026-03-28

### Security

- **Dockerfile**: Removed hardcoded `DATABASE_URL` and `REDIS_URL` credentials; environment variables now injected at runtime
- **Dockerfile.backend**: Added non-root `appuser` (UID 1000) with `USER` directive
- **Docker build stages**: Fixed lowercase `as` to uppercase `AS` per Dockerfile best practices
- **Docker Compose**: Removed deprecated `version` fields from `docker-compose.yml` and `docker/docker-compose.backend.yml`

### Code Quality

- **Interpreter**: Fixed 25 type annotation errors in `core/interpreter.py`
  - `__init__` signature now uses `Optional[Language] = None`
  - All instance attributes declared before `_init_state()` call
  - Added null guards for optional attributes
  - `execute()` accepts `TurtleState | None` with lazy creation
  - Removed unused `datetime` import
- **Version label**: Updated Dockerfile `VERSION` label from 8.0.0 to 9.0.0

### Documentation

- **ARCHITECTURE.md**: Rewrote Module Overview with all subpackages (core, languages, UI, graphics, features, utils, tests, tools)
- **README.md**: Fixed theme count (25→28), rewrote project structure tree to match actual layout
- **docs/INDEX.md**: Added missing Debugger Guide link in Feature Guides section
- **CHANGELOG.md**: Expanded 9.0.0 entry with detailed change descriptions
- **ROADMAP.md**: Rewrote with concrete milestones replacing generic filler content

### Infrastructure

- Added `*.log` pattern to `.gitignore`
- Removed stray `interpreter.log` files from repository
- Updated GitHub repository description and topics via API

## [8.1.0] - 2025-06-15

### Security

- **Pascal executor**: Replaced unsafe `eval()` with `ExpressionEvaluator` for safe math expression evaluation
- **JavaScript executor**: Fixed 8 bare `except:` clauses with specific exception types (`ValueError`, `TypeError`, `KeyError`, etc.)
- **Lua executor**: Fixed bare `except:` clause with specific exception types

### Interpreter Enhancements

- **HyperTalk**: Added `add/subtract/multiply/divide` math commands, `do`, `wait`, `visual effect`, `sort`, `beep`, `exit repeat`, `next repeat`, chunk expression ranges (`char i to j of`), built-in properties (`the date`, `the time`, `the ticks`, `the random`), new math functions (`average`, `sum`, `ln`, `log2`, `exp`, `atan`, `annuity`, `compound`)
- **PILOT**: Added `?` single-character wildcard and substring matching (SuperPILOT), match variables (`$LEFT`, `$MATCH`, `$RIGHT`), string functions (`LEN`, `UPPER`, `LOWER`, `TRIM`, `LEFT`, `RIGHT`, `MID`, `CONCAT`, `REPLACE`, `REVERSE`), enhanced graphics commands (`SETHEADING`, `HIDETURTLE`, `SHOWTURTLE`, `ARC`, `FILL`, `DOT`, `STAMP`, `TEXT`, `SPEED`), `H:` hint and `N:` no-match commands, tab support in `T:`
- **Lua**: Replaced stub coroutine implementation with threading-based yield/resume cycling using `threading.Event` synchronization with 5-second timeout
- **JavaScript**: Added `Symbol.hasInstance` and `Symbol.toPrimitive` support, fixed Symbol transpiler stripping

### UI Improvements

- **Find & Replace**: Added case sensitivity, regex, and whole word matching checkboxes; live match highlighting with match count display; regex-aware find and replace
- **Auto-Completion**: Enhanced to extract identifiers from the current document in addition to language keywords
- **Themes**: Added **Catppuccin Mocha** and **Gruvbox Dark** themes (28 themes total)

### Code Quality

- Removed 5 leftover debug print comments from BASIC and Logo executors
- All bare `except:` clauses replaced with specific exception types

### Demo Programs (new examples)

- **BASIC**: Maze generator with recursive backtracking
- **Logo**: Spirograph / hypotrochoid mathematical art
- **PILOT**: Typing speed tutor
- **C**: 3×3 matrix calculator
- **Pascal**: Number guessing game
- **Prolog**: Logic puzzle solver with unification
- **Forth**: Geometric turtle art
- **Brainfuck**: Fibonacci sequence generator
- **JavaScript**: Pub/Sub event system
- **HyperTalk**: Interactive quiz builder
- **Lua**: Traffic light state machine

### Documentation

- Updated README with v8.1.0 features
- Created CHANGELOG.md

---

## [8.0.0] - 2025-06-01

### Initial Release

- 12 programming language executors: BASIC, PILOT, Logo, COMAL, REXX, and Scheme, JavaScript, Lua, HyperTalk, Erlang
- PySide6 (Qt6) desktop IDE with editor, canvas, and output panels
- 26 built-in themes including retro CRT and accessibility options
- Integrated debugger with timeline recording and rewind
- Turtle graphics with 50+ drawing commands
- Lesson system with auto-verification
- AI assistant and error explainer panels
- Initial example programs across all 12 languages
- Comprehensive automated test suite
