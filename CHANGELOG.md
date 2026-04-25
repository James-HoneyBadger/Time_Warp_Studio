# Changelog

All notable changes to Time Warp Studio will be documented in this file.

## [10.1.0] - 2026-04-25

### Examples (Round 2 — 20 new showcase programs)

- **BASIC** — `space_simulation.bas`: Solar system explorer with planetary data table, Kepler's third law, light travel times, escape velocities, and turtle orbital graphics
- **Logo** — `geometric_artistry.logo`: Koch snowflake grid, Islamic 8-star pattern, rosette mandala, hexagonal tiling, and optical art spiral
- **PILOT** — `math_drill.pilot`: 20-question arithmetic flashcard drill (addition, subtraction, multiplication, division) with letter grade scoring
- **C** — `game_of_life.c`: Conway's Game of Life with glider, blinker, toad, beacon, R-pentomino, and pulsar patterns on a toroidal grid
- **Pascal** — `grade_book.pas`: Student grade book for 20 students with letter grades, GPA, statistics (mean/median/std dev), and score histogram
- **Forth** — `cellular_automata.forth`: 1D elementary cellular automata — Rule 30 (chaos), Rule 90 (Sierpiński), Rule 110 (Turing-complete), Rule 184 (traffic)
- **Prolog** — `family_genealogy.pl`: 4-generation family tree (22 members) with relationship inference — cousins, aunts/uncles, ancestors, and generational distance
- **Python** — `statistics_suite.py`: Statistical analysis with mean/median/mode/quartiles/skewness/kurtosis, z-scores, normality checks, and turtle bar chart
- **Lua** — `turtle_patterns.lua`: Mathematical curves — spirograph (hypotrochoid/epitrochoid), rose curves, Lissajous figures, Archimedean spirals, star polygons
- **JavaScript** — `mandelbrot.js`: ASCII Mandelbrot set, 4 Julia sets, Burning Ship fractal, Tricorn; zoom views (Seahorse Valley, etc.) with density gradient chars
- **Ruby** — `word_games.rb`: Anagram finder, Scrabble scorer, palindrome checker (14 phrases), word frequency bar chart, Pig Latin translator, edit distance
- **Haskell** — `morse_code.hs`: Morse encoder/decoder with binary tree for O(log n) decode, timing analysis, dot/dash ratio, letter complexity, round-trip verification
- **Scheme** — `lambda_calculus.scm`: Lambda calculus evaluator — Church booleans/numerals/pairs/lists, Y combinator, beta reduction, SKI combinators
- **Erlang** — `number_theory.erl`: Number theory library — prime sieve, factorization, GCD/LCM, Euler's totient, amicable pairs, modular arithmetic, CRT, Collatz, Catalan
- **Smalltalk** — `ascii_patterns.st`: Pascal's triangle, number spiral, diamonds (solid and numbered), checkerboard, Sierpiński triangle, multiplication table, nested boxes
- **HyperTalk** — `calculator.htalk`: Scientific calculator — arithmetic, powers/roots, factorials, trig (sin/cos/tan in degrees), logarithms (log2/log10/ln), unit conversion, statistics
- **REXX** — `number_systems.rexx`: Base conversion (binary/octal/hex/base-36), Roman numeral encoder/decoder, bitwise AND/OR/XOR/NOT, palindromic numbers across bases
- **Rust** — `game_of_life.rs`: Conway's Game of Life with `Grid` struct/impl, toroidal boundaries, 7 classic patterns, period detection, and simulation statistics
- **Perl** — `text_analytics.pl`: Text analytics — Flesch-Kincaid readability, word frequency with bar chart, bigram/trigram n-grams, sentence statistics, KWIC concordance

### Documentation

- **Examples/CATALOG.md**: Updated from 87 to 107 programs; added all 20 Round 2 entries under each language section
- **README.md**: Updated Examples project tree from 67/19-language counts to 107/20-language counts with accurate per-language file totals

## [10.0.0] - 2026-04-25

### New Languages
- **Perl 5** — scalars/arrays/hashes, regex (`=~`, `s///`, `qw()`), subroutines, turtle graphics commands (`forward`, `back`, `left`, `right`, `penup`, `pendown`, `pencolor`, `setpos`, `circle`)

### Graphics Enhancements
- **SVGA mode** — `set_svga_mode(width, height)` on `TurtleState` enables a fixed-resolution virtual canvas (default 800×600) rendered with full anti-aliasing inside a chrome bezel; `clear_svga_mode()` returns to unlimited coordinates
- **Cubic Bezier curves** — `bezier_curve(cp1x, cp1y, cp2x, cp2y, end_x, end_y)` draws a smooth vector path from the current turtle position
- **Gradient fills** — `draw_rect_gradient()` and `draw_ellipse_gradient()` fill shapes with linear or radial colour gradients using stop-based colour maps
- **Sprite system** — `define_sprite(name, pixel_rows, palette)` defines named pixel-art sprites from string rows; `place_sprite(name, x, y, scale, rotation, z_order)` stamps them; `move_sprite(name, dx, dy)` translates the most recent instance
- **Advanced pen styles** — `set_pen_style(dash, cap, join)` controls dash patterns, line caps (`round`/`flat`/`square`), and joins (`round`/`miter`/`bevel`) for all subsequent shapes
- **Z-ordering** — shapes are now sorted by `z_order` before rendering; higher numbers paint on top
- Canvas: added `QLinearGradient`/`QRadialGradient`/`QPainterPath`/`QPolygonF` rendering paths; gradient-fill support extended to polygons and ellipses; sprite shape type rendered with per-pixel alpha, scale, and rotation transforms

### Sound / Music Enhancements
- **ADSR envelopes** — new `ADSREnvelope` dataclass with `attack_ms`, `decay_ms`, `sustain_level`, `release_ms`; automatically shapes amplitude for each note
- **New waveforms** — `pulse` (configurable duty cycle, `@Pn` MML command) and `noise` (white noise, `@W5`)
- **4-channel polyphony** — new `MusicChannel` dataclass; `MusicPlayer.mix_channels()` sums up to 4 independent synthesised streams into a single WAV; `parse_multichannel(dict)` convenience wrapper
- **Extended MML syntax** — `@Wn` (waveform 0–5), `@Datk,dcy,sus,rel` (ADSR), `@Pn` (pulse duty %), `@Cn` (channel 0–3), `[CEG]` chord notation for simultaneous notes
- 4 new built-in sound effects: `DRONE`, `BUZZ`, `STATIC`, `BEEP2`



### Language Executor Improvements
- **APL**: outer product operator (`∘.fn` e.g. `A ∘.× B`), format `⍕`, execute `⍎`
- **Pascal**: `WITH <record> DO` statement for record field shorthand access
- **C**: `struct`/`union` type definitions, struct variable declarations, member access (`p.x`), `typedef` for scalar types

### UI Improvements
- **Canvas**: Added "Export SVG…" (🖼) toolbar button alongside existing "Save PNG…"
- SVG export via `PySide6.QtSvg.QSvgGenerator` with antialiasing and full turtle graphics rendering

### Examples & Documentation
- Added Ruby examples: `Examples/ruby/hello.rb`, `Examples/ruby/oop_demo.rb`
- Added Erlang examples: `Examples/erlang/hello.erl`, `Examples/erlang/pattern_match.erl`
- Added Rust examples: `Examples/rust/hello.rs`, `Examples/rust/structs.rs`
- Added tutorial docs: `docs/tutorials/ruby.md`, `docs/tutorials/erlang.md`, `docs/tutorials/rust.md`

### UI & Learning Experience

- Added a new Learning Hub with guided challenge launchers, remix actions, quick access to reference material, and one-click tutor help
- Added a Project Explorer with recent-project tracking, persistent workspace roots, and direct file opening from the IDE
- Added File menu and toolbar actions for opening project folders and revisiting recent projects
- Improved the AI Assistant with beginner-friendly tutor summaries for the current code context
- Updated welcome/help surfaces to highlight the new learning and project workflows

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
- 28 built-in themes including retro CRT and accessibility options
- Integrated debugger with timeline recording and rewind
- Turtle graphics with 50+ drawing commands
- Lesson system with auto-verification
- AI assistant and error explainer panels
- 97 example programs across all languages
- Comprehensive automated test suite
