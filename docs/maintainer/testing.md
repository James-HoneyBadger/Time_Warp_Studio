# Testing

The product is the Qt application in `native/`. Its engines, lesson checks and validation
gates are C++ suites and tools built from this tree. The Python reference engine was
deleted and parity is measured against the corpus it left behind, so nothing here needs an
interpreter of any kind.

Run everything from the repository root.

---

## The suites

```bash
cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release   # configure once
cmake --build native/build -j
ctest --test-dir native/build --output-on-failure
```

There are 24 suites, one binary each, named for what they cover:

| Suite | Covers |
| --- | --- |
| `lexer_tests`, `evaluator_tests` | `timewarp_expr`: tokens and arithmetic, shared by every language |
| `lang_tests` | The ten executors and the shared interpreter loop |
| `scheme_tests` | The explicit-stack Scheme machine |
| `debug_tests`, `debugger_runner_tests` | Breakpoints, stepping, rewind, and the Scheme runner on its own thread |
| `canvas_queue_tests`, `canvas_bridge_tests` | The command queue between an engine and the canvas |
| `turtle_canvas_tests` | The display list, the projection, the exports, the reference's drawing conventions |
| `ui_layout_tests` | The window: panels, docks, menus, modes, the theme table |
| `ui_document_tests` | Documents, tabs, language detection, open and save |
| `editor_services_tests` | Find, outline, formatting, linting, autosave, version history, the terminal's text handling |
| `syntax_highlighter_tests` | Colour per language |
| `media_tests`, `audio_tests`, `player_tests` | Export, sound, and the player artifact |
| `lessons_tests`, `classroom_tests` | The curriculum, the verifier, progress, rosters, marking, bundles |
| `help_browser_tests` | The documentation resolver and the index parser |
| `project_tests` | `.twproject` and its groups |
| `sql_tests` | The shared SQLite connection |
| `cli_tests` | `twc` and the command-line surface |
| `api_tests` | The JSON-RPC server over an in-memory stream |
| `ai_tests` | The assistant's configuration, prompts and exchange, against a transport of the test's own rather than a network |

Other build trees must stay green:

```bash
make -C native test-debug     # Debug
make -C native asan           # AddressSanitizer
make -C native asan ASAN=undefined SAN_DIR=build-ubsan \
  ASAN_FLAGS='-fsanitize=undefined -fno-omit-frame-pointer -g'   # UndefinedBehaviorSanitizer
```

---

## The example corpus

Every `Examples/**` program is run through the native engines by `time_warp_check`, the same
recorder the API and the lessons use. This is the gate for the failure unit tests cannot
see: a language that runs "successfully" and draws nothing, which is what a user reports as
"the graphics do not show up".

```bash
make smoke            # or: ./native/build/time_warp_gates smoke
```

Sixty-eight programs ship; `smoke` runs 67, because `basic/star_patrol.bas` uses what the
native port added and the reference never had, and is run by `lang_tests` instead. The
programs that must produce ink are listed in `drawingExamples()` in
`native/tools/gates/smoke.cpp`: a program on that list that draws nothing fails the run.

The command matrix is the other half. Every row in `native/share/command_rows.json` is
executed through `time_warp_check` and must produce output, drawing or a reported error, and
the harness reports any command the executors dispatch on that no row or waiver mentions.

```bash
make command-matrix   # or: ./native/build/time_warp_gates commands
make repl-matrix      # the same rows through a fresh REPL session
```

The REPL is a separate path on purpose: commands that depend on bindings, drawing or
whole-program entry semantics are exercised there rather than assumed to work.

---

## Parity

The C++ engines are compared with a **recorded corpus**: the Python reference's answers,
frozen before the engine was deleted. Zero differences is the verdict, and the documented
divergences are expected and printed.

```bash
make golden-check                                  # both corpora, 636 programs
./native/build/time_warp_gates golden --scheme     # the Scheme machine alone
```

The language corpus, `native/tests/golden/lang_corpus.json.gz`, holds 444 programs: every
command-table row and every example for a ported language, with the output lines it printed,
every drawing call in order, and the variables it left. The Scheme corpus holds 192 programs
and compares the whole output text, which is what the Scheme harness always compared.

Nine of the language entries were recorded from *this* engine rather than from the deleted
reference: the three Forth programs, because Forth arrived at 21.0 after the reference could
no longer run it, and six command rows whose program changed when the defect the row
recorded was fixed. Each carries `recorded_from` saying so, and
`time_warp_gates golden --record forth` makes the Forth recording again.

### What the gate is worth

The program text travels with the answer in both corpora, which is what lets the gate run on
a tree with no second interpreter. There is no `--record` for the rest any more: the engine
the answers came from is deleted, so a deliberate behaviour change edits the corpus entry in
the same commit that changes the behaviour.

The price is stated rather than glossed: **a gate that compares with a recording cannot
notice a mistake in the recording.** The live differential could. What the recording buys is
a gate that runs against the engines the product ships, needs no interpreter toolchain, and
cannot rot when a second implementation drifts.

Each entry carries a SHA-256 of its program, so editing an example without re-recording is
reported as a corpus problem rather than as an engine mismatch. A tampered or stale entry is
a failure, not a skip.

The Scheme corpus keeps 24 documented divergences, each with the reference's text and a
reason. The check asserts both halves: that the port still differs, so an entry whose note
has gone stale fails, and that the reason is written down.

Three kinds of program are deliberately absent from the language corpus because they cannot
be recorded: the `TIME$` and `DATE$` rows read the clock, `INPUT #` reads a file another row
wrote, and the `JOYINIT`, `STICK` and `STRIG` rows read whatever gamepad is plugged in. Each
is exercised by `lang_tests` against the engine directly. The gamepad is the instructive
one: the reference shipped no backend and answered "none", while the port opens
`/dev/input/js0`, so comparing them would report a plugged-in joystick as a language
difference on the machine that has one and pass on the machine that does not.

---

## Curriculum and documentation

```bash
./native/build/time_warp_teach validate                 # every solution passes, every starter fails
./native/build/time_warp_teach plans --write docs/lessons/plans
git diff --exit-code --stat docs/lessons/plans          # generated plans match the curriculum
make lint-md                                            # Markdown style
make doc-links                                          # every relative link and anchor resolves
make doc-claims                                         # every path and name a guide mentions exists
./native/build/time_warp_docs version                   # the version copies agree
```

`validate` runs every lesson solution through its checkpoint and refuses a starter that
already passes. `plans --write` regenerates the teacher-facing plans from the curriculum,
and CI fails if the regenerated files differ from the committed ones, which is why those ten
files are never edited by hand.

`doc-links` walks every Markdown file, resolves each relative link, and checks each `#anchor`
against the heading GitHub would slug it from. `doc-claims` is the other direction: it reads
the paths, targets, command names and counts a guide states and fails when one is not there,
so a claim about a file that has moved cannot survive a push.

`docs/INDEX.md` is load-bearing twice over: the documentation gates check that every guide it
promises exists, and the application parses it into the table of contents of **Help >
Documentation**, which is why `help_browser_tests` asserts that every row resolves to a file
and that the `Language tutorials` section lists one tutorial per language.

---

## The API and the terminal tools

The JSON-RPC server is covered by `api_tests` and can also be smoke-tested as a process:

```bash
printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"studio.info"}' | ./api
TIME_WARP_SKIP_BUILD=1 ./twc Examples/basic/hello.bas -o /tmp/hello
/tmp/hello --no-svg
```

The generated executable is a real artifact, so that smoke test proves the compiler and the
link step as well as the program.

---

## Lint

The build is the lint. Every target compiles with `-Wall -Wextra -Wpedantic -Wshadow
-Wcast-qual -Wuseless-cast -Wlogical-op -Wnull-dereference` and `-Werror`, so a warning
fails the build rather than travelling. `make lint` is kept as the name for that and says
so; `make lint-md` is the Markdown rules over every tracked file.

There is no second language to lint: `native/tools/docs` is C++, and `make tree-check` fails
if a `.py` file or a Python tool's configuration file comes back.

The commit-time hook is `detect-secrets`. It stops a credential reaching GitHub and compares
against `.secrets.baseline`, so it reports only what is added after the baseline was taken.
[CONTRIBUTING.md](../../CONTRIBUTING.md) has the install command and how to silence a single
false positive.
