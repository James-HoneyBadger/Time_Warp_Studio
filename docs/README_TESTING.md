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
`native/bridge/time_warp/tests/command_table.py` is executed and must produce output,
drawing or a reported error. It reports commands that no test or example exercises.

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

## The bridge engine's own tests

The suite needs `pytest`, which is not in the standard library, so it runs from
the repository's virtual environment. `make engine-test` is the command to use:
it picks `.venv/bin/python3` when that exists and falls back to `python3`.

```bash
make engine-test                                          # from the repository root
PYTHONPATH=native/bridge .venv/bin/python3 -m pytest -q    # what it runs
```

A system interpreter without `pytest` fails with `No module named pytest`. Give
the virtual environment a `pytest` with `python3 -m venv .venv` and
`.venv/bin/pip install pytest ruff`, as the troubleshooting section of
[INSTALLATION.md](INSTALLATION.md) describes.

## Parity gates

The C++ engines are compared against the Python reference on a fixed corpus. All
three must report **0 unexpected mismatches**; documented divergences are listed in
the harness output and are expected.

The language gate runs every command-table row and every example for a ported
language through both engines and compares output lines, every drawing call in
order, and the final variables. Programs using randomness are reported and skipped:
both sides seed from the OS, so they cannot agree by construction.

```bash
python3 native/tools/differential_check.py native/build/expr_cli
cd native/tools && python3 scheme_differential_check.py
python3 native/tools/lang_differential_check.py   # 474 programs, 0 differences, 9 skipped
```

### The recorded corpus

The reference engine is going away, and a gate that needs it cannot check a tree
that no longer has it. `--record` freezes the reference's answers into
`native/tests/golden/lang_corpus.json.gz` — the program text travels with the
answer, so the corpus outlives the command table it was recorded from — and
`--golden` compares the C++ ports with that file instead of a live interpreter.
Golden mode imports no Python at all, which the gate itself is run to prove.

```bash
make golden-check    # native/build/lang_cli against the corpus; needs no Python
make golden-record   # re-freeze from the reference; needs the bridge
```

Three programs are excluded from the corpus and cannot be recorded: the `TIME$`
and `DATE$` rows read the clock, and `INPUT # reads it back` reads a file another
row wrote. The live gate still compares them, because both engines run moments
apart in the same directory; a file recorded today cannot know either.

The `JOYINIT`, `STICK` and `STRIG` rows are skipped in *both* modes. A gamepad is
hardware: the reference shipped no backend and answers "none", while the port opens
`/dev/input/js0`, so comparing the two would report a plugged-in joystick as a
language difference on the machine that has one and pass on the machine that does
not.

A tampered or stale entry is a failure, not a skip: each entry carries a SHA-256
of its program, so editing an example without re-recording is reported as a
corpus problem rather than as a mismatch in the engine.

## Curriculum and documentation gates

```bash
python3 native/tools/teach.py validate                 # lessons, checkpoints and solutions
python3 native/tools/teach.py write-plans docs/lessons/plans
git diff --exit-code --stat docs/lessons/plans         # generated plans match
make lint-md                                           # Markdown style
make doc-links                                         # every link and anchor resolves
```

`validate` runs every lesson solution through its checkpoint and refuses a starter
that already passes. `write-plans` regenerates the teacher-facing plans from the
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
