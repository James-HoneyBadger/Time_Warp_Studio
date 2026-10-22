# Testing Time Warp Studio

The product is the Qt application in `native/`, so the primary suites are C++
suites driven by CTest. The Python engine that the parity gates compare against,
and that checks lessons, has its own small pytest suite.

Run everything from the repository root.

## The C++ suites

```bash
# Configure once
cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release

# Build and test
cmake --build native/build -j
ctest --test-dir native/build --output-on-failure
```

There are 23 suites: the lexer, the expression evaluator, the Scheme machine, the
native language engine, the canvas command queue, the Qt canvas bridge, the debug and
debugger runner, the UI layout, the UI documents, the turtle canvas, the syntax
highlighter, the mixer, the lessons library, the classroom, sound and ports, the
editor's text services
(formatting, snippets, find-in-files, autosave), the help browser, the live CHIP-8
machine with its window, the HB-1 computer, the project file, the SQL
database, and the JSON-RPC API.

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
python3 native/tools/example_smoke.py
```

The 30 programs that must produce ink are listed in `DRAWING_EXAMPLES` inside that
script; a program that is expected to draw and does not fails the run.

The command matrix is the other half of that gate: every row in
`native/tools/command_table.py` is executed through `time_warp_check` and must
produce output, drawing or a reported error. It also reports what the C++ executors
dispatch on that no row or waiver mentions.

```bash
python3 native/tools/command_matrix.py
```

The REPL is a separate path: each row is submitted through a fresh native
`repl.*` session, so commands that depend on bindings, drawing or whole-program
entry semantics are exercised there too.

```bash
python3 native/tools/repl_matrix.py
```

Both are also `make command-matrix` and `make repl-matrix`.

## Parity gates

The C++ engines are compared with a **recorded corpus** — the Python reference's
answers, frozen before the engine was deleted. `0 differences` is the verdict, and
documented divergences are listed in the harness output and are expected.

```bash
make golden-check                      # both corpora; 662 programs
python3 native/tools/golden_check.py --scheme   # the Scheme machine alone
```

The language corpus (`native/tests/golden/lang_corpus.json.gz`) holds 470 programs:
every command-table row and every example for a ported language, with the output
lines it printed, every drawing call in order, and the variables it left behind.
The Scheme corpus (`scheme_corpus.json.gz`) holds 192 programs and compares the
whole output text, because that is what the Scheme harness always compared.

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
