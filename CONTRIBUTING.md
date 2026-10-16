# Contributing to Time Warp Studio

Welcome! Thank you for considering contributing to Time Warp Studio. This guide explains how to contribute.

---

## Code of Conduct

This project is committed to providing a welcoming and inclusive environment. All contributors must follow our code of conduct.

---

## Ways to Contribute

### 1. Report Bugs

Found a bug? Report it on GitHub Issues:

1. **Check existing issues** - Maybe already reported
2. **Create new issue** with:
   - Clear title and description
   - Steps to reproduce
   - Expected vs actual behavior
   - System info (OS, Python version, hardware)
   - Error messages or screenshots

### 2. Suggest Features

Have an idea? Open a GitHub Discussion:

1. **Describe the feature** clearly
2. **Explain the use case** - Why is it needed?
3. **Provide examples** - How would it work?
4. **Discuss alternatives** - What else could solve this?

### 3. Improve Documentation

Documentation needs constant improvement:

- Fix typos and unclear explanations
- Add examples and clarifications  
- Create tutorials for common tasks
- Improve README and guides

### 4. Write Code

### 5. Write Tests

Help improve code quality:

- Write unit tests
- Write integration tests
- Improve test coverage
- Find and document edge cases

---

## Development Setup

### Prerequisites

- Python 3.10 or higher
- Git
- Basic command-line knowledge

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

Contributors who touch the five bridged languages (BASIC, PILOT, Logo, COMAL,
REXX) also need the bridge's test tooling:

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

The Python in `native/bridge/` is the temporary engine for the five unported
languages. It is standard-library only, `ruff` clean, and each executor is a
stateless function.

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
3. Fill in PR details:
   - **Title**: Clear, concise description
   - **Description**: What does this change?
   - **Why**: Why is this change needed?
   - **Testing**: How did you test it?
   - **Checklist**:
     - [ ] Tests pass
     - [ ] Code follows style guide
     - [ ] Documentation updated
     - [ ] No breaking changes (or documented)

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

A language that is not yet native runs in the bridge, which is temporary by
design: each language ported to C++ deletes its path through it. Registering a
seventh language means touching the bridge and the Qt side.

### 1. Create Executor Module

File: `native/bridge/time_warp/languages/newlang.py`

```python
from __future__ import annotations
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

def execute_newlang(interpreter: Interpreter, source: str, turtle: TurtleState) -> str:
    """Executor for NewLang language."""
    output_lines = []
    try:
        # Parse and execute
        result = _parse_and_execute(source)
        output_lines.append(f"✅ {result}")
    except Exception as e:
        output_lines.append(f"❌ Error: {e}")
    return "\n".join(output_lines) + "\n"

def _parse_and_execute(source: str) -> str:
    """Parse and execute."""
    pass
```

### 2. Register Executor

File: `native/bridge/time_warp/core/interpreter.py`

```python
from ..languages.newlang import execute_newlang
# Add Language.NEWLANG enum member
# Add entry in _WHOLE_PROGRAM_EXECUTORS dict (whole-program languages only)
```

Also import it in `native/bridge/time_warp/languages/__init__.py` and add the
extension in `core/language_registry.py`.

### 3. Add Auto-Detection

Add to `Language.from_extension()` in `core/interpreter.py`, and to the Qt-side
language table in `native/src/ui/language.cpp` so the IDE knows the display name,
settings key and extensions.

```python
".nlg": cls.NEWLANG,
```

### 4. Create Tests

File: `native/bridge/time_warp/tests/test_lang_newlang.py`

```python
from .conftest_lang import run, ok, has, no_errors

class TestNewLang:
    """Test NewLang executor"""
    
    def test_hello_world(self):
        """Test basic output"""
        out = run('print "Hello"', "NEWLANG")
        assert has(out, "Hello")
        assert no_errors(out)
```

### 5. Add Examples

Directory: `Examples/newlang/`

- `01_hello_world.ext`
- `02_variables.ext`
- `03_loops.ext`
- etc.

### 6. Update Documentation

- Add to LANGUAGE_GUIDE.md
- Update README.md "Supported Languages" section
- Create tutorial if complex

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
- [ ] Documentation updated
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
│   ├── tools/                    # time_warp_ide.cpp + Python harnesses
│   └── bridge/                   # TEMPORARY Python engine (5 unported languages)
│       ├── worker.py             #   the JSON-lines worker the IDE spawns
│       └── time_warp/            #   interpreter, executors, turtle state
├── Examples/                     # 44 example programs across 6 languages
├── docs/                         # Language guide, tutorials, architecture
├── Scripts/                      # audits, version sync, build helper
├── Makefile                      # Convenience targets (delegate to native/)
└── [Root docs]                   # README, CONTRIBUTING, LICENSE
```

See docs/ARCHITECTURE.md for detailed structure.

---

## Getting Help

### Questions?

- **Email**: <james@honey-badger.org>
- **GitHub Issues**: Report bugs
- **GitHub Discussions**: Ask questions  
- **Documentation**: Check docs/

### Still Confused?

- Look at existing PRs
- Check similar code as reference
- Ask in GitHub issue before starting

---

## Recognition

Contributors are recognized in:

- Commit history
- GitHub contributors page
- Project changelog

Thank you for contributing!

---

**Resources:**

- [ARCHITECTURE.md](docs/ARCHITECTURE.md) - Technical design
- [README.md](README.md) - Project overview
- [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) - Expected behavior
