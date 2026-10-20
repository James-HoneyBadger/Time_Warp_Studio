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

There are 17 suites: the lexer, the expression evaluator, the Scheme machine, the
native language engine, the canvas command queue, the Qt canvas bridge, the debug and
debugger runner, the UI layout, the UI documents, the turtle canvas, the syntax
highlighter, the Python bridge, sound and ports, the editor's text services
(formatting, snippets, find-in-files, autosave), the help browser, and the live
CHIP-8 machine with its window.

Other build trees, all of which must stay green:

```bash
make -C native test-debug     # Debug
make -C native asan           # AddressSanitizer
make -C native asan ASAN=undefined SAN_DIR=build-ubsan \
  ASAN_FLAGS='-fsanitize=undefined -fno-omit-frame-pointer -g'   # UBSan
```

## The example corpus

Every `Examples/**` program is run through the real bridge. This is the gate for
the failure the unit tests cannot see — a language that runs "successfully" and
draws nothing, which is what a user reports as "the graphics don't show up".

```bash
python3 native/tools/example_smoke.py
```

The 30 programs that must produce ink are listed in `DRAWING_EXAMPLES` inside that
script; a program that is expected to draw and does not fails the run.

## The bridge engine's own tests

```bash
PYTHONPATH=native/bridge python3 -m pytest -q
make -C native engine-test    # equivalent, from the root Makefile
```

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
python3 native/tools/lang_differential_check.py   # 482 programs, 0 differences
```

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
