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

```bash
# Core dependencies
pip install -r requirements.txt

# Development dependencies for testing and code quality
pip install -r Platforms/Python/requirements-dev.txt
```

### Verify Setup

```bash
# Run IDE
python Platforms/Python/time_warp_ide.py

# Run tests
python test_runner.py --basic
```

Both should work without errors.

---

## Code Style

### Python Code Standards

- **Style**: PEP 8 compliance
- **Line length**: 100 characters maximum
- **Formatter**: Use `black`
- **Linter**: Must pass `flake8` and `pylint`
- **Type hints**: Use with `mypy` strict mode
- **Docstrings**: Add to all public functions/classes

### Formatter & Linting

```bash
# Format code
black time_warp/ --line-length 100

# Check style
flake8 time_warp/
pylint time_warp/

# Type checking
mypy time_warp/ --strict
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
# Run test suite
python test_runner.py --comprehensive

# Check code style
black --check .
flake8 .
pylint time_warp/ --disable=all --enable=fatal
mypy time_warp/ --strict

# Run specific component tests
pytest tests/test_core_interpreter.py -v
```

All must pass before submitting PR.

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
# Quick tests
python test_runner.py --basic

# Comprehensive
python test_runner.py --comprehensive

# Specific file
pytest tests/test_basic.py -v

# With coverage
pytest --cov=time_warp tests/ --cov-report=html
```

### Writing Tests

Tests go in `tests/` directory:

```python
# File: tests/test_my_feature.py

import pytest
from time_warp.languages.basic import BasicExecutor
from time_warp.core.interpreter import TimeWarpInterpreter

class TestMyFeature:
    """Test my new feature"""
    
    def setup_method(self):
        """Setup before each test"""
        self.interpreter = TimeWarpInterpreter()
        self.basic = self.interpreter.basic
    
    def test_feature_works(self):
        """Test that feature works"""
        result = self.basic.execute_command("PRINT 5 / 2")
        assert "2" in result  # Integer division
    
    def test_feature_edge_case(self):
        """Test edge case"""
        with pytest.raises(ZeroDivisionError):
            self.basic.execute_command("PRINT 5 / 0")
```

### Test Coverage

Aim for >80% coverage on new code:

```bash
pytest --cov=time_warp --cov-report=html tests/
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
- `ARCHITECTURE.md` - If internal change
- `docs/USER_GUIDE.md` - If UI change
- `docs/LANGUAGE_GUIDE.md` - If language syntax
- `docs/TROUBLESHOOTING.md` - If known issue

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

Want to add a new language executor? Follow these steps:

### 1. Create Executor Module

File: `Platforms/Python/time_warp/languages/newlang.py`

```python
from . import LanguageExecutor

class NewLangExecutor(LanguageExecutor):
    """Executor for NewLang language"""
    
    def __init__(self, interpreter):
        self.interpreter = interpreter
    
    def execute_command(self, command: str) -> str:
        """Execute command in NewLang"""
        try:
            # Parse and execute
            result = self._parse_and_execute(command)
            return f"✅ {result}\n"
        except Exception as e:
            return f"❌ Error: {e}\n"
    
    def _parse_and_execute(self, command: str):
        """Parse and execute"""
        pass
```

### 2. Register Executor

File: `Platforms/Python/time_warp/core/interpreter.py`

```python
from .languages.newlang import NewLangExecutor

class TimeWarpInterpreter:
    def __init__(self):
        # ... existing executors
        self.newlang = NewLangExecutor(self)
```

### 3. Add Auto-Detection

In `execute()` method:

```python
def execute(self, code: str, language: Language = None):
    if language == Language.NEWLANG or code.startswith("%%"):
        return self.newlang.execute_command(code)
    # ... rest of detection
```

### 4. Create Tests

File: `tests/test_newlang.py`

```python
class TestNewLang:
    """Test NewLang executor"""
    
    def test_hello_world(self):
        """Test basic output"""
        # Write tests
        pass
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

# 2. Run full test suite
python test_runner.py --comprehensive

# 3. Check code style
black --check .
flake8 .
mypy time_warp/ --strict

# 4. Update docs
# - README
# - Architecture docs
# - API docs
```

### Submission Checklist

- [ ] Feature/fix complete
- [ ] Tests written and passing
- [ ] Code follows PEP 8
- [ ] Type hints added
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

```
Time_Warp_Studio/
├── Platforms/Python/
│   ├── time_warp_ide.py          # Main entry point
│   ├── time_warp/
│   │   ├── core/                 # Interpreter, debugger
│   │   ├── languages/            # 7 executors
│   │   ├── ui/                   # PySide6 UI
│   │   ├── graphics/             # Turtle graphics
│   │   ├── features/             # Feature panels
│   │   └── tools/                # Utilities
│   ├── tests/                    # Test suite
│   └── requirements.txt          # Dependencies
├── Examples/                     # Example programs
├── docs/                         # User documentation
└── [Root docs]                  # README, LICENSE, etc.
```

See ARCHITECTURE.md for detailed structure.

---

## Getting Help

### Questions?

- **Email**: james@honey-badger.org
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
- [ARCHITECTURE.md](ARCHITECTURE.md) - Technical design
- [README.md](README.md) - Project overview
- [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) - Expected behavior
