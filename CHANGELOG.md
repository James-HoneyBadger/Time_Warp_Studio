# Changelog

All notable changes to Time Warp Studio will be documented in this file.

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
- **Fortran executor**: Added character whitelist regex guard before sandboxed eval
- **JavaScript executor**: Fixed 8 bare `except:` clauses with specific exception types (`ValueError`, `TypeError`, `KeyError`, etc.)
- **Lua executor**: Fixed bare `except:` clause with specific exception types
- **Assembly executor**: Fixed bare `except:` clause with `(ValueError, TypeError)`

### Interpreter Enhancements
- **HyperTalk**: Added `add/subtract/multiply/divide` math commands, `do`, `wait`, `visual effect`, `sort`, `beep`, `exit repeat`, `next repeat`, chunk expression ranges (`char i to j of`), built-in properties (`the date`, `the time`, `the ticks`, `the random`), new math functions (`average`, `sum`, `ln`, `log2`, `exp`, `atan`, `annuity`, `compound`)
- **JCL**: Added `SET` symbolic parameters with `&symbol` resolution, `IF/THEN/ELSE/ENDIF` conditionals with return code comparison, inline `PROC/PEND` definitions, `JCLLIB ORDER`, `INCLUDE MEMBER`
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

### Demo Programs (25 new examples)
- **BASIC**: Maze generator with recursive backtracking
- **Logo**: Spirograph / hypotrochoid mathematical art
- **PILOT**: Typing speed tutor
- **C**: 3×3 matrix calculator
- **Pascal**: Number guessing game
- **Prolog**: Logic puzzle solver with unification
- **Forth**: Geometric turtle art
- **Python**: Fractal explorer (Mandelbrot, Sierpinski, Dragon curve)
- **Lua**: Traffic light state machine
- **Scheme**: Metacircular evaluator
- **COBOL**: Sales report generator
- **Brainfuck**: Fibonacci sequence generator
- **Assembly**: String processing toolkit
- **JavaScript**: Pub/Sub event system
- **Fortran**: Projectile motion physics simulator
- **REXX**: String manipulation toolkit
- **Smalltalk**: OOP geometry shapes hierarchy
- **HyperTalk**: Interactive quiz builder
- **Haskell**: Parser combinator library
- **APL**: Statistical analysis suite
- **SQL**: School database with complex queries
- **JCL**: Quarterly data migration job
- **CICS**: ATM transaction processing
- **SQR**: Employee directory report

### Documentation
- Updated README with v8.1.0 features, corrected example counts (93 total)
- Created CHANGELOG.md

---

## [8.0.0] - 2025-06-01

### Initial Release
- 24 programming language executors
- PySide6 (Qt6) desktop IDE with editor, canvas, and output panels
- 26 built-in themes including retro CRT effects
- Integrated debugger with timeline recording and rewind
- Turtle graphics with 50+ drawing commands
- Lesson system with auto-verification
- AI assistant and error explainer panels
- 93 example programs across all languages
- 500+ passing tests
