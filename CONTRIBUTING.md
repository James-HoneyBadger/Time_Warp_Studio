# Contributing to Time Warp Studio

Welcome! Thank you for considering contributing to Time Warp Studio. This guide explains how to contribute.

---

## Code of Conduct

This project is committed to providing a welcoming and inclusive environment. All contributors must follow our code of conduct.

---

## Ways to Contribute

### 1. Report Bugs

Found a bug? Use the
[bug report form](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues/new?template=bug_report.yml).
It asks for the four things needed to reproduce a problem, because a prose issue
usually leaves one of them out:

1. **Check [docs/ROADMAP.md](docs/ROADMAP.md) first.** A dedicated player target,
   an automatic update check and Flatpak/Snap or Windows/macOS packaging are
   deliberately not built yet, so a missing feature from that list is a feature
   request rather than a regression.
2. **Check existing issues** — it may already be reported.
3. **Fill in the form.** The environment it asks for (built from source or
   installed, and the platform) changes where to look: the two take different
   paths to the bridge.

### 2. Suggest Features

Use the [feature request form](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues/new?template=feature_request.yml).
The question it is really asking is **what are you trying to do?** — the situation
rather than the solution, because that leaves room for a better answer than either
of us had in mind.

For a **new language**, use the
[language request form](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues/new?template=language_request.yml).
It asks what gap the language fills and what it costs, because a language is not
one file: an executor, the enum and registry entry, the IDE's language table, an
example, command-table rows, a tutorial, a reference section and two lessons.

### 3. Improve Documentation

Documentation needs constant improvement:

- Fix typos and unclear explanations
- Add examples and clarifications  
- Create tutorials for common tasks
- Improve README and guides

### 4. Write Code

Three recipes cover most contributions:

- **A language.** The procedure is in
  [ARCHITECTURE.md § Adding a language](docs/ARCHITECTURE.md#adding-a-language) and
  [FORMAT.md § 9](docs/lessons/FORMAT.md#9-adding-lessons-for-a-new-language). A
  language is not finished until every item exists, and a PR that stops halfway
  leaves a language the IDE knows but the lesson panel cannot teach: the executor,
  its cases in `executor_for` and in the IDE's table in `native/src/ui/language.cpp`,
  an example under `Examples/`, command rows, a tutorial, a reference section, and
  two lessons.
- **A command.** Add it to the executor, add a row to the `rows` list in
  `native/share/command_rows.json`, and run `make command-matrix`. The row is what
  proves the command
  produces output, drawing or a reported error — silence is this project's
  characteristic bug. If a row's label is prose, name the command the inventory
  knows in `also=(...)`, or it is reported as untested forever.
- **A lesson.** `./native/build/time_warp_teach new <id> --language NAME` prints a
  skeleton the loader accepts, `--append` adds it to the curriculum, and
  `time_warp_teach validate` says what is still missing. It refuses a solution that does
  not pass its own checkpoint and a starter that already passes, unless
  `starter_is_broken` records that the failure *is* the lesson. Then regenerate
  the plans with `time_warp_teach plans --write docs/lessons/plans`.
- **Documentation.** `.github/workflows/docs.yml` enforces the parts that can be
  checked: every guide [docs/INDEX.md](docs/INDEX.md) promises exists, every
  relative link and `#anchor` resolves (`make doc-links`), every language has a
  tutorial, and the lesson plans match the curriculum. Each fact has one home —
  the [HB-1](docs/hb1/README.md) and [CHIP-8](docs/chip8/README.md) books own their
  machines, and the IDE guide links to them rather than restating them.

A change that alters a program's output is a documentation change too: the guides
and the lessons contain transcripts of what programs print.

### 5. Write Tests

Help improve code quality:

- Write unit tests
- Write integration tests
- Improve test coverage
- Find and document edge cases

---

## Development Setup

### Prerequisites

The application is C++20 and Qt6, and so is everything that tests it: there is no
interpreter in the tree, so a checkout needs a compiler and nothing else.

| To work on | You need |
| ---------- | -------- |
| The IDE and the C++ suites | A C++20 compiler (GCC 11+ or Clang 14+), CMake 3.20+, Qt6 (Widgets, Core, Gui; Svg optional for SVG export, PrintSupport optional for printing) and the SQLite 3 development package |
| The gates (`make smoke`, `command-matrix`, `repl-matrix`, `golden-check`, `tree-check`, `qt5-check`) | The same build: they are `time_warp_gates`, built with the product |
| The documentation gates (`make doc-links`, `doc-claims`) | A C++20 compiler. `time_warp_docs` is its own small project with no Qt |
| `make lint-md` | A C++20 compiler: it is `time_warp_docs md`, built with the documentation tools |
| Any of it | Git and a command line |

### Clone Repository

```bash
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio
```

### Configure and build

The application needs no dependency step: Qt6, CMake and a C++20 compiler are all it
takes. Configure once, then build.

```bash
cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
cmake --build native/build -j
```

The gates and the documentation tools are built the same way — `make smoke` and
`make doc-links` build what they need first. There is no virtual environment, no
`pip install` and no interpreter to provide.

### Verify Setup

```bash
# Build and run the IDE
make -C native run

# The 24 C++ suites
make test
```

Both should work without errors.

### Install the Git hooks

```bash
pipx install pre-commit    # the hook runner itself, which needs Python to install
pre-commit install
```

That installs one hook: `detect-secrets` fails the commit when a file contains
something shaped like a credential. There is no Python linter any more, because
there is no Python: the build is compiled `-Werror`, and `make tree-check` fails if
a `.py` file or a Python tool's config file comes back.

`detect-secrets` compares against the committed baseline in `.secrets.baseline`,
which records what the tree already holds. That is deliberate: without it every
historical false positive would be re-reported on each run, and a hook that cries
wolf gets switched off. With it, only a secret added *since* the baseline stops
the commit — which is the case worth stopping.

The reason to care is not tidiness. A key that reaches GitHub is world-readable,
and deleting it in a later commit does not remove it: the blob stays in history
and in every fork and clone, so recovery means rewriting history *and* rotating
the key.

When the hook flags a false positive, silence that one line rather than
weakening the detection:

```text
EXAMPLE_TOKEN = "not-a-real-token"  # pragma: allowlist secret
```

When a real value is removed from a file, or the baseline is audited and the
finding is intentional, regenerate it and review the diff:

```bash
detect-secrets scan --baseline .secrets.baseline
```

---

## Code Style

### C++ Code Standards

- **Standard**: C++20, matching the style of the file you are editing
- **Warnings are errors**: the build carries `-Wall -Wextra -Wpedantic -Wshadow
  -Wcast-qual -Wuseless-cast -Wlogical-op -Wnull-dereference` with `-Werror`
  (disable with `-DTIME_WARP_EXPR_WERROR=OFF` while experimenting)
- **Comments explain why.** The unusual decisions in this codebase — the turtle's
  compass heading, the bridge protocol, the coalescing canvas queue — are
  documented where they are made; match that

### The gates

The harnesses are `time_warp_gates`, one binary built with the product: `smoke`
runs every example, `commands` runs every row of the command table, `repl` runs the
same rows through the API, and `golden` compares the ports with the frozen
corpora. The rows are data in `native/share/command_rows.json`, and the inventory
that measures their coverage reads the executors' own sources
(`native/tools/gates/inventory.cpp`).

The documentation gates are `time_warp_docs` (`native/tools/docs`, one small
project with no Qt): `links`, `claims` and `version`. `time_warp_icon` writes the
packaged icons. None of them is part of the product, and nothing installed contains
them — there is no Python in the tree at all, which `make tree-check` enforces.

Two things in the application are not executors and break just as easily: the lesson
curriculum (`native/share/lessons.json`) and the teacher's side
(`classroom.cpp`). A checkpoint is checked by *running* the learner's
program, so an executor change can fail a lesson without touching the lesson —
which is why `make engine-test` is not optional here.

```bash
make lint        # the build, which is warning-free by construction
make lint-md     # every tracked Markdown file against the house rules
make doc-claims  # every path, name and make target the guides name exists
```

### Example Code Standards

```python
def calculate_sum(numbers: list[int]) -> int:
    """
    Sum a list of integers.
    
    Args:
        numbers: List of integers to sum
        
    Returns:
        Sum of all numbers
        
    Raises:
        TypeError: If numbers is not a list of integers
    """
    if not isinstance(numbers, list):
        raise TypeError("numbers must be a list")
    
    total = 0
    for num in numbers:
        total += num
    
    return total
```

---

## Git Workflow

### 1. Create Feature Branch

```bash
git checkout -b feature/your-feature-name

# Or for bug fixes
git checkout -b fix/your-bug-name
```

Branch naming:

- Feature: `feature/description`
- Bug: `fix/description`
- Docs: `docs/description`<!-- doc-claims: ignore -->
- Test: `test/description`

### 2. Make Changes

- Keep commits focused and clear
- Write descriptive commit messages
- One logical change per commit

```bash
# Good commit message
git commit -m "Add BASIC integer division operator (/)"

# Bad commit message
git commit -m "Fixed stuff"
```

### 3. Run Tests & Validation

```bash
# Run test suite (offscreen Qt)
make test

# Check code style
make lint

# Every example program, through the reference engine
make smoke

# One C++ suite by name
ctest --test-dir native/build -R turtle_canvas_tests --output-on-failure
```

All must pass before submitting a PR. CI (`native.yml`) builds the application and
runs the suites in Release, Debug, AddressSanitizer and UndefinedBehaviorSanitizer
configurations, the differential gates, the example corpus, and the engine's
own tests.

### 4. Push to Your Fork

```bash
git push -u origin feature/your-feature-name
```

### 5. Create Pull Request

On GitHub:

1. Click "New Pull Request"
2. Select your feature branch
3. Fill in the form GitHub shows you. It is
   [.github/pull_request_template.md](.github/pull_request_template.md), and every
   item on it is either a gate that must have been run or a decision no gate can
   make. There is a table for the gate results; fill in what you ran.

Example PR description:

```markdown
## Description
Adds support for integer division operator in BASIC language.

## Changes
- Add `/` operator parsing to BASIC executor
- 5 / 2 returns 2 (integer division)
- Add tests for division edge cases

## Why
Requested by Issue #42 - BASIC language completeness.

## Testing
- Unit tests: 3 new tests added
- Integration: Tested with demo_basic.bas
- Edge cases: Zero division handled

## Checklist
- [x] Tests pass
- [x] style follows PEP 8
- [x] Documentation added
- [x] No breaking changes
```

### 6. Code Review

- Maintainer reviews your PR
- Requests changes if needed
- Approves and merges when ready

---

## Testing Requirements

### Run Test Suite

```bash
# The 21 C++ suites
make test

# One suite, by name
ctest --test-dir native/build -R turtle_canvas_tests --output-on-failure

# The other build trees
make test-debug
make asan

# Every example program, through the native engines
make smoke

# All of the above, plus lint
make test-all && make lint

# The Markdown: every tracked .md file, against the rules in time_warp_docs md
make lint-md
```

### Writing Tests

C++ tests live in `native/tests/` as `*_tests.cpp`, one executable per suite, each
registered with CTest in `native/CMakeLists.txt`. They use the tiny assertion
helpers in `native/tests/check.hpp`.

```cpp
// File: native/tests/my_feature_tests.cpp

#include "check.hpp"
#include "timewarp/canvas/command_queue.hpp"
from time_warp.graphics.turtle_state import TurtleState

class TestMyFeature:
    """Test my new feature"""

    def setup_method(self):
        """Setup before each test"""
        self.interpreter = Interpreter()
        self.turtle = TurtleState()

    def test_feature_works(self):
        """Test that feature works"""
        self.interpreter.load_program('PRINT 5 / 2', 'BASIC')
        result = self.interpreter.execute(self.turtle)
        assert "2" in "\n".join(result)

    def test_feature_edge_case(self):
        """Test edge case"""
        self.interpreter.load_program('PRINT 5 / 0', 'BASIC')
        result = self.interpreter.execute(self.turtle)
        assert any("\u274c" in line for line in result)
```

### Test Coverage

Add assertions to the relevant `native/tests/*_tests.cpp`. There is no coverage
tool in the tree: a suite that fails when a claim is broken is the measure this
project uses, and every fix is expected to arrive with one.

---

## Documentation

### Update Docs When

- Adding new feature
- Changing existing behavior
- Fixing docs, typos, clarity
- Adding examples

### Files to Update

- `README.md` - If user-visible change
- `docs/ARCHITECTURE.md` - If internal change
- `docs/INSTALLATION.md` - If build or run steps change
- `docs/LANGUAGE_GUIDE.md` - If language syntax
- `docs/hb1/` - If the HB-1's words, disk, settings or screen change
- `docs/chip8/` - If the assembler, an opcode or the emulator window changes
- `docs/MACHINES.md` - If either machine, or what they share, changes
- `docs/ROADMAP.md` - If a planned item lands or is deferred
- `native/README.md` - If the Qt application's design or behaviour changes

### Documentation Standard

```markdown
# Feature Title

Brief description of feature.

## Example

Code example showing usage.

## Related

Links to related docs or code.
```

---

## Adding a New Language

The procedure is in
[ARCHITECTURE.md § Adding a language](docs/ARCHITECTURE.md#adding-a-language); this
is the checklist it implies. A language is a feature rather than a file: the IDE
has to know it, the corpus has to exercise it, and the lesson panel has to be able
to teach it. Stopping after step 2 leaves a language the tests know and the IDE
cannot open.

### 1. Create the executor

File: `native/src/lang/newlang.cpp`<!-- doc-claims: ignore -->, with its header in
`native/include/timewarp/lang/`<!-- doc-claims: ignore -->

```cpp
std::string execute_newlang(Interpreter& interpreter, std::string_view source,
                            TurtleHost& turtle) {
    // A statement at a time (the line-by-line shape) or the whole source at once
    // (the whole-program shape). Either way: return the text, never throw out of
    // the executor, and prefix a failure with the error emoji.
    return "❌ NewLang: not implemented yet\n";
}
```

Errors are *returned as a line beginning with the error emoji*, never raised out of
the executor: the interpreter, the canvas and the lesson checker all read the
returned text.

### 2. Register it

Every table a language has to be in, and each one a compile error or a test failure
if you miss it:

- `native/src/lang/language.cpp` — the `Language` enum member, `display_name`,
  `key`, `all_languages`, `is_whole_program` and `executor_for`
- `native/src/ui/language.cpp` — display name, settings key and extensions. CHIP-8
  is the trap here: the enum member is `CHIP8` and the display name is `CHIP-8`
- the switches that have a case per language: `syntax_highlighter.cpp`, `lint.cpp`,
  `outline.cpp`, `native_runner.cpp`, `retro_computer.cpp` (the ROM socket) and
  `curriculum.cpp` (the extension)
- `native/CMakeLists.txt` — the source file

The build is `-Werror` with switch-exhaustiveness warnings, so a missing case is a
build failure rather than a language that silently does nothing.

### 3. Test it

Assertions go in `native/tests/lang_tests.cpp`<!-- doc-claims: ignore -->, through the
same `Interpreter` the IDE runs:

```cpp
void test_newlang_prints_hello() {
    RecordingHost host;
    Interpreter interpreter{Language::NewLang, host};
    interpreter.load_program("print \"Hello\"");
    interpreter.run();
    TW_CHECK_EQ(interpreter.output().at(0), std::string{"Hello"});
}
```

Then add a row per command to the `rows` list in `native/share/command_rows.json` and
run `make command-matrix`: it fails on a row that has no effect and on a command in
the inventory that has no row.

### 4. Add examples

Directory: `Examples/newlang/`, starting with `hello`.<!-- doc-claims: ignore --> Add any program that draws
to `drawingExamples()` in `native/tools/gates/smoke.cpp`, or a program that draws
nothing passes by being silent.

### 5. Teach it

Two lessons in `native/share/lessons.json`:
`./native/build/time_warp_teach new <id> --language NEWLANG` prints a skeleton the
loader accepts. Then run `time_warp_teach validate`, which refuses a solution that does
not pass its own checkpoint, a starter that already passes, and a starter that
fails unless `starter_is_broken` says the failure is the lesson.

### 6. Document it

- `docs/LANGUAGE_GUIDE.md` — a reference section
- `docs/tutorials/newlang.md` — a tutorial<!-- doc-claims: ignore -->; `.github/workflows/docs.yml` requires
  one per language
- `README.md` — the row in the language table
- `Examples/CATALOG.md` — one line per example

---

## Submitting Changes

### Before Submitting

```bash
# 1. Update to latest main
git fetch origin
git rebase origin/main

# 2. Run the full test suite, all build trees, the corpus gate and lint
make test-all && make lint && make lint-md

# 3. Update docs
# - README
# - docs/ARCHITECTURE.md and docs/INSTALLATION.md
# - native/README.md for design decisions
```

### Submission Checklist

- [ ] Feature/fix complete
- [ ] Tests written and passing
- [ ] Build is warning-free (`-Werror`)
- [ ] Bridge code (if touched) is ruff clean and standard-library only
- [ ] `make smoke` is unchanged — every example still runs, and the 30 that must draw still draw
- [ ] Lesson changes pass `./native/build/time_warp_teach validate`
- [ ] Documentation updated, including anything the change made untrue
- [ ] No breaking changes (or documented)
- [ ] Commit messages clear
- [ ] Branch based on latest main
- [ ] Ready for code review

### After Submission

- Respond to review comments
- Make requested changes
- Re-run test suite
- Update PR description if needed
- Be patient - maintainers volunteer

---

## Project Structure

Understanding the codebase:

```text
Time_Warp_Studio/
├── native/                       # THE APPLICATION (C++20 / Qt6)
│   ├── CMakeLists.txt
│   ├── include/ src/             # ui/, canvas/, scheme/, lang/, lessons/,
│   │                             #   debugger/, project/, sql/, api/
│   ├── tests/                    # 26 CTest suites, and the golden corpora
│   ├── share/                    # lessons.json, snippets.json, command_rows.json
│   └── tools/                    # time_warp_ide.cpp, time_warp_api.cpp
│                                 #   and the commands and gates: time_warp_check,
│                                 #   time_warp_learn, time_warp_teach,
│                                 #   time_warp_gates (smoke, commands, repl,
│                                 #   golden, tree, in gates/), time_warp_docs
│                                 #   (links, claims, version, in docs/),
│                                 #   time_warp_icon (the packaged icons)
├── Examples/                     # 63 example programs across the 11 languages
├── docs/                         # the guides
│   ├── USER_GUIDE.md             #   the IDE manual (projects, REPL, SQL, API)
│   ├── API.md                    #   JSON-RPC methods and libraries
│   ├── tutorials/                #   one tutorial per language
│   └── lessons/                  #   the curriculum, both guides and the plans
├── run.sh                        # build (if needed) and launch the IDE
├── twc                           # compile one program to an executable
├── api                           # JSON-RPC on stdin/stdout
├── Makefile                      # Convenience targets (delegate to native/)
└── [Root docs]                   # README, CONTRIBUTING, SECURITY, LICENSE
```

See docs/ARCHITECTURE.md for detailed structure.

---

## Getting Help

### Questions?

[.github/SUPPORT.md](.github/SUPPORT.md) is the authority on where to ask and on
what is supported. In short:

- **Bugs and ideas**: the issue forms linked above
- **Documentation**: start at [docs/INDEX.md](docs/INDEX.md)
- **Email**: <james@honey-badger.org>
- **Security problems**: [SECURITY.md](SECURITY.md) — please do not open a public issue

### Still Confused?

- Look at existing PRs
- Check similar code as reference
- Ask in an issue *before* starting something large — a new language or a new
  panel is a lot of work to find out it is not wanted

---

## Recognition

Contributors are recognized in:

- Commit history
- GitHub contributors page
- Project changelog

Thank you for contributing!

---

**Resources:**

- [ARCHITECTURE.md](docs/ARCHITECTURE.md) - Technical design and the extension points
- [docs/INDEX.md](docs/INDEX.md) - Every guide, and which to read when
- [README.md](README.md) - Project overview
- [SUPPORT.md](.github/SUPPORT.md) - Where to ask, and what is supported
- [SECURITY.md](SECURITY.md) - How to report a vulnerability
- [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) - Expected behavior
