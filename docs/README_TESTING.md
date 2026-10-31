# Testing Time Warp Studio

The product is the Qt application in `native/`, and its engines, lesson checks and
validation gates are C++ suites and tools built from this tree. The Python reference
engine was removed; parity uses its frozen recorded corpus.

Run everything from the repository root.

## The C++ suites

```bash
# Configure once
cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release

# Build and test
cmake --build native/build -j
ctest --test-dir native/build --output-on-failure
```

There are 26 suites, one binary each, named for what they cover:

`lexer_tests`, `evaluator_tests`, `scheme_tests`, `lang_tests` (the native
executors and the shared interpreter loop), `canvas_queue_tests`,
`canvas_bridge_tests`, `debug_tests`, `debugger_runner_tests`,
`ui_layout_tests`, `ui_document_tests`, `turtle_canvas_tests`,
`syntax_highlighter_tests`, `audio_tests`, `media_tests`, `player_tests`,
`lessons_tests`, `classroom_tests`, `editor_services_tests`,
`help_browser_tests`, `chip8_emulator_tests` (the live machine and its window),
`retro_computer_tests` (the HB-1), `project_tests`, `sql_tests`, `cli_tests`,
`api_tests` and `ai_tests` (the assistant's configuration, prompts and exchange,
against a transport of the test's own rather than a network).

Other build trees, all of which must stay green:

```bash
make -C native test-debug     # Debug
make -C native asan           # AddressSanitizer
make -C native asan ASAN=undefined SAN_DIR=build-ubsan \
  ASAN_FLAGS='-fsanitize=undefined -fno-omit-frame-pointer -g'   # UBSan
```

## The example corpus

Every `Examples/**` program is run through the native engines, by
`time_warp_check`, which is the same recorder the API and the lessons use. This is
the gate for the failure the unit tests cannot see — a language that runs
"successfully" and draws nothing, which is what a user reports as "the graphics
don't show up".

```bash
make smoke            # or: ./native/build/time_warp_gates smoke
```

The programs that must produce ink are listed in `drawingExamples()` inside
`native/tools/gates/smoke.cpp`; a program that is expected to draw and does not fails
the run.

The command matrix is the other half of that gate: every row in
`native/share/command_rows.json` is executed through `time_warp_check` and must
produce output, drawing or a reported error. It also reports what the C++ executors
dispatch on that no row or waiver mentions.

```bash
make command-matrix   # or: ./native/build/time_warp_gates commands
```

The REPL is a separate path: each row is submitted through a fresh native
`repl.*` session, so commands that depend on bindings, drawing or whole-program
entry semantics are exercised there too.

```bash
make repl-matrix      # or: ./native/build/time_warp_gates repl
```

## Parity gates

The C++ engines are compared with a **recorded corpus** — the Python reference's
answers, frozen before the engine was deleted. `0 differences` is the verdict, and
documented divergences are listed in the harness output and are expected.

```bash
make golden-check                                  # both corpora; 671 programs
./native/build/time_warp_gates golden --scheme     # the Scheme machine alone
```

The language corpus (`native/tests/golden/lang_corpus.json.gz`) holds 479 programs:
every command-table row and every example for a ported language, with the output
lines it printed, every drawing call in order, and the variables it left behind.
The Scheme corpus (`scheme_corpus.json.gz`) holds 192 programs and compares the
whole output text, because that is what the Scheme harness always compared.

Three of the language entries were recorded from *this* engine rather than from the
deleted reference: Forth arrived at 21.0, after the reference could no longer run it.
Each carries `recorded_from` saying so, and `time_warp_gates golden --record forth`
is the command that makes the recording again. That is the honest limit of the gate
for those three programs -- a recording cannot notice a mistake in the recording --
and the reason the section above says the corpus is weaker than the live differential
it replaced.

### Why a recording, and what it costs

The program text travels with the answer in both corpora, which is what lets the
gate run on a tree with no second interpreter. There is no `--record` any more: the
engine the answers came from is deleted, so a deliberate behaviour change edits the
corpus entry in the same commit that changes the behaviour — the same discipline the
rest of the suite asks for.

The price is stated plainly rather than glossed: **a gate that compares with a
recording cannot notice a mistake in the recording.** The live differential could.
What that buys is a gate that runs against the engines the product ships, needs no
Python toolchain, and cannot rot when a second interpreter drifts.

A note on the Scheme corpus: its 24 documented divergences keep the *reference's*
text and a reason. The check asserts two things about each — that the port still
differs (an entry whose note has gone stale fails, which is how three of them were
retired when the port caught up) and that the reason is written down.

Three kinds of program are not in the language corpus because they cannot be
recorded: the `TIME$` and `DATE$` rows read the clock, `INPUT #` reads a file
another row wrote, and the `JOYINIT`/`STICK`/`STRIG` rows read whatever gamepad is
plugged in. Each is exercised by `lang_tests` against the engine directly. A gamepad
is hardware: the reference shipped no backend and answered "none", while the port
opens `/dev/input/js0`, so comparing the two would report a plugged-in joystick as a
language difference on the machine that has one and pass on the machine that does
not.

A tampered or stale entry is a failure, not a skip: each entry carries a SHA-256 of
its program, so editing an example without re-recording is reported as a corpus
problem rather than as a mismatch in the engine.

## Curriculum and documentation gates

```bash
./native/build/time_warp_teach validate                 # lessons, checkpoints and solutions
./native/build/time_warp_teach plans --write docs/lessons/plans
git diff --exit-code --stat docs/lessons/plans         # generated plans match
make lint-md                                           # Markdown style
make doc-links                                         # every link and anchor resolves
make doc-claims                                        # every path and name it mentions exists
```

`validate` runs every lesson solution through its checkpoint and refuses a starter
that already passes. `plans --write` regenerates the teacher-facing plans from the
curriculum; CI fails if the regenerated files differ from the committed ones.
`doc-links` walks `docs/**/*.md` and `README.md`, resolves every relative link and
checks every `#anchor` against the headings GitHub would slug it from, so the two
machine manuals and the guides that point at them cannot drift apart silently.

## The API and terminal tools

The JSON-RPC server is covered by `api_tests` and can be smoke-tested as a process:

```bash
printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"studio.info"}' | ./api
TIME_WARP_SKIP_BUILD=1 ./twc Examples/basic/hello.bas -o /tmp/hello
/tmp/hello --no-svg
```

The generated executable is a real artifact, so the smoke test proves the compiler
and the link step as well as the program.

## Lint

```bash
make lint    # ruff over the bridge engine
```

That covers the bridge only. The commit-time hooks cover more:
`pre-commit install` runs `ruff`, `ruff-format` and `detect-secrets` over the
files a commit touches. `detect-secrets` is the one that stops a credential
reaching GitHub; it compares against `.secrets.baseline`, so it reports only
what has been added since the baseline was taken. `CONTRIBUTING.md` has the
install commands and the way to silence a single false positive.

The C++ side is its own linter: the build carries `-Wall -Wextra -Wpedantic
-Wshadow -Wcast-qual -Wuseless-cast -Wlogical-op -Wnull-dereference` with
`-Werror`, so a warning fails the build.
