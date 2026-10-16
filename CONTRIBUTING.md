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

1. **Check [docs/ROADMAP.md](docs/ROADMAP.md) first.** Themes, find/replace,
   auto-completion, the examples browser and the debugger panels are deliberately
   not built yet, so a missing feature from that list is a feature request rather
   than a regression.
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
  the enum and `core/language_registry.py`, the IDE's table in
  `native/src/ui/language.cpp`, an example under `Examples/`, command-table rows, a
  tutorial, a reference section, and two lessons.
- **A command.** Add it to the executor, add a row to
  `native/bridge/time_warp/tests/command_table.py`, and run
  `python3 native/tools/command_matrix.py`. The row is what proves the command
  produces output, drawing or a reported error — silence is this project's
  characteristic bug. If a row's label is prose, name the command the inventory
  knows in `also=(...)`, or it is reported as untested forever.
- **A lesson.** `python3 native/tools/teach.py new <id> --language NAME` prints a
  skeleton the loader accepts, `--append` adds it to the curriculum, and
  `teach.py validate` says what is still missing. It refuses a solution that does
  not pass its own checkpoint and a starter that already passes, unless
  `starter_is_broken` records that the failure *is* the lesson. Then regenerate
  the plans with `teach.py write-plans docs/lessons/plans`.
- **Documentation.** `.github/workflows/docs.yml` enforces the parts that can be
  checked: every guide [docs/INDEX.md](docs/INDEX.md) promises exists, every
  language has a tutorial, and the lesson plans match the curriculum.

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

The application is C++20 and Qt6. Python is needed only for the temporary bridge
engine, which runs the nine languages that are not ported to C++ yet.

| To work on | You need |
| ---------- | -------- |
| The IDE and the C++ suites | A C++20 compiler (GCC 11+ or Clang 14+), CMake 3.20+, Qt6 (Widgets, Core, Gui) |
| The bridge engine | Python 3.10 or higher, standard library only |
| The documentation gates | Python 3, and `node` for `markdownlint-cli2` |
| Any of it | Git and a command line |

### Clone Repository

```bash
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio
```

### Create Virtual Environment

```bash
# Linux/macOS
python3 -m venv .venv
source .venv/bin/activate

# Windows
python -m venv .venv
.venv\Scripts\activate
```

### Install Dependencies

The application itself needs no dependency step: Qt6, CMake and a C++20 compiler
are all it takes. Configure once, then build.

```bash
cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
cmake --build native/build -j
```

Contributors who touch a bridged language also need the bridge's test
tooling. Nine of the ten languages run through it — BASIC, PILOT, Logo, COMAL,
REXX, FOCAL, APL, CHIP-8 and Smalltalk; Scheme is the only one with a C++
interpreter:

```bash
python3 -m venv .venv
.venv/bin/pip install pytest ruff
```

Optional extras for those languages are not required: `pyttsx3` (BASIC `SAY`),
`pygame` and `inputs` (BASIC `SOUND`/`GAMEPAD`) are imported defensively.

### Verify Setup

```bash
# Build and run the IDE
make -C native run

# The 11 C++ suites
make test
```

Both should work without errors.

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

### Bridge engine code (Python)

The Python in `native/bridge/` is the temporary engine for the nine languages
that are not ported to C++. It is standard-library only, `ruff` clean, and each
executor is a stateless function returning text with an emoji prefix.

Two things in there are not executors and break just as easily: the lesson
curriculum (`features/lessons.json`) and the teacher's side
(`features/classroom_mode.py`). A checkpoint is checked by *running* the learner's
program, so an executor change can fail a lesson without touching the lesson —
which is why `make engine-test` is not optional here.

```bash
make lint        # ruff check native/bridge
make engine-test # pytest over the engine
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
- Docs: `docs/description`
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

# Every example program, through the real bridge
make smoke

# One C++ suite by name
ctest --test-dir native/build -R turtle_canvas_tests --output-on-failure
```

All must pass before submitting a PR. CI (`native.yml`) builds the application and
runs the suites in Release, Debug, AddressSanitizer and UndefinedBehaviorSanitizer
configurations, both differential gates, the example corpus, and the engine's
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
# The 11 C++ suites
make test

# One suite, by name
ctest --test-dir native/build -R turtle_canvas_tests --output-on-failure

# The other build trees
make test-debug
make asan

# The bridge engine's own tests
make engine-test

# Every example program, through the real bridge
make smoke

# All of the above, plus lint
make test-all && make lint
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

Aim for >80% coverage on new code. For the C++ suites, add assertions to the
relevant `native/tests/*_tests.cpp`; for the bridge engine:

```bash
.venv/bin/python -m pytest native/bridge/time_warp/tests -q \
  --cov=time_warp --cov-report=html
# Coverage report in htmlcov/index.html
```

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

File: `native/bridge/time_warp/languages/newlang.py`

```python
from __future__ import annotations
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


class NewLangError(Exception):
    """Raised while parsing or evaluating NewLang; reported with the error emoji."""


def execute_newlang(interpreter: Interpreter, source: str, turtle: TurtleState) -> str:
    """Execute NewLang source and return output lines."""
    output_lines = []
    try:
        output_lines.append(f"✅ {_parse_and_execute(source)}")
    except NewLangError as exc:
        output_lines.append(f"❌ NewLang: {exc}")
    return "\n".join(output_lines) + "\n"
```

Errors are *returned as a line beginning with the error emoji*, never raised out
of the executor: the interpreter, the canvas and the lesson checker all read the
returned text.

### 2. Register it

- `languages/__init__.py` — import and export the executor
- `core/interpreter.py` — the `Language` enum member, the import, the dispatch
  entry, `Language.from_extension()`, and `_WHOLE_PROGRAM_EXECUTORS` if the
  executor takes the whole source at once rather than one line at a time
- `core/language_registry.py` — display name, folder name, extensions and
  execution mode (`line` or `whole`)
- `features/classroom_mode.py` — the handout file extension
- `native/src/ui/language.cpp` — display name, settings key and extensions. CHIP-8
  is the trap here: the enum member is `CHIP8` and the display name is `CHIP-8`

`test_interpreter_inventory.py` asserts the registry and the enum are the same
set, so half an entry fails the suite rather than shipping.

### 3. Test it

File: `native/bridge/time_warp/tests/test_lang_newlang.py`

```python
from time_warp.core.interpreter import Language
from .conftest_lang import run, has, no_errors, first_error

NEWLANG = Language.NEWLANG


def test_hello_world():
    out = run('print "Hello"', NEWLANG)
    assert has(out, "Hello")
    assert no_errors(out)
```

The second argument is the enum member, not its name. Then add a row per command
to `native/bridge/time_warp/tests/command_table.py` and run
`python3 native/tools/command_matrix.py`: it fails on a row that has no effect and
on a command in the inventory that has no row.

### 4. Add examples

Directory: `Examples/newlang/`, starting with `hello`. Add any program that draws
to `DRAWING_EXAMPLES` in `native/tools/example_smoke.py`, or a program that draws
nothing passes by being silent.

### 5. Teach it

Two lessons in `native/bridge/time_warp/features/lessons.json`:
`python3 native/tools/teach.py new <id> --language NEWLANG` prints a skeleton the
loader accepts. Then run `teach.py validate`, which refuses a solution that does
not pass its own checkpoint, a starter that already passes, and a starter that
fails unless `starter_is_broken` says the failure is the lesson.

### 6. Document it

- `docs/LANGUAGE_GUIDE.md` — a reference section
- `docs/tutorials/newlang.md` — a tutorial; `.github/workflows/docs.yml` requires
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
make test-all && make lint

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
- [ ] `make smoke` is unchanged — every example still runs, and the 14 that must draw still draw
- [ ] Lesson changes pass `python3 native/tools/teach.py validate`
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
│   ├── include/ src/             # ui/, canvas/, scheme/, debugger/, bridge/
│   ├── tests/                    # 11 CTest suites
│   ├── tools/                    # time_warp_ide.cpp and the gates:
│   │                             #   learn.py, teach.py, command_matrix.py,
│   │                             #   example_smoke.py, differential_check.py
│   └── bridge/                   # TEMPORARY Python engine (9 unported languages)
│       ├── worker.py             #   the JSON-lines worker the IDE spawns
│       └── time_warp/            #   the engine itself
│           ├── core/             #     interpreter.py, language_registry.py
│           ├── languages/        #     one stateless function per language
│           ├── features/         #     lesson_system.py + lessons.json,
│           │                     #     classroom_mode.py (the teacher's side)
│           ├── graphics/         #     turtle_state.py
│           └── tests/            #     the engine's pytest suite
├── Examples/                     # 48 example programs across the 10 languages
├── docs/                         # the guides
│   ├── tutorials/                #   one tutorial per language
│   └── lessons/                  #   the curriculum, both guides and the plans
├── Scripts/                      # audits, version sync, build helper
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
