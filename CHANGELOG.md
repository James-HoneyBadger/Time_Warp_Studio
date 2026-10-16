# Changelog

## [15.0.0] - 2026-10-16

The implementation was replaced wholesale: Time Warp Studio is now a C++20 / Qt6
desktop application, and the language lineup and packaging that went with the previous
implementation are gone. Both changes were made after `v14.1.2` and shipped together,
so both are recorded here.

### The Qt application is now the product

Time Warp Studio is a **C++20 / Qt6 desktop application** (`native/`). The PySide6
implementation that came before it has been **removed**; what remains of Python is a
temporary bridge the Qt application spawns for the five languages that are not ported
yet.

#### Added

- **Qt application** (`native/`) — `time_warp_ide`: documents and tabs, code editor
  with line-number gutter and current-line highlight, streaming output console with
  interpreter errors in their own tab, turtle canvas with zoom, pan, grid and
  auto-fit, menus, toolbar and status bar, and `make install` with a desktop entry
  and icon.
- **Native engines** — the expression evaluator (`timewarp_expr`) and Scheme
  (`timewarp_scheme`, an explicit-stack CEK machine), each with a parity gate against
  the Python reference (153 and 167 cases, zero unexplained divergence).
- **Language bridge** (`native/bridge/`) — a JSON-lines worker the application spawns
  for BASIC, PILOT, Logo, COMAL and REXX. It is scheduled for deletion one language at
  a time as each is ported.
- **Test and examination tooling** — 11 CTest suites, the bridge engine's own suite,
  `example_smoke.py` (all 44 examples through the real bridge), `command_matrix.py`
  with `command_table.py` (one row per documented command, 279 rows), and
  `command_inventory.py`, which extracts the implemented command set to check that
  table for completeness.
- **CI** (`.github/workflows/native.yml`) covering the build, the suites and the gates.

#### Removed

- The entire PySide6 application, its launchers (`run.py`, `run.sh`), its Docker and
  developer tooling, and the workflows that packaged and published it.
- The C, Pascal, Prolog, Forth, Brainfuck and Python executors.

#### Fixed during the port's examination

Examining every documented command through the real bridge turned up defects that no
gate had been looking for:

- **A keyword boundary bug in BASIC.** `startswith("DO")` also matched `DOG = 42`, so
  the `DO` statement swallowed the assignment: the variable was never set and `PRINT`
  then echoed the name back. Any variable named `DOUBLE`, `RUNNER`, `CLOSER`, `NEXTX`
  or similar silently lost its value. Eleven keywords were affected.
- **`ON TIMER` could never work.** The dispatcher sliced one character too many,
  discarding the `(`, so the handler rejected the format its own error message asked
  for.
- **`SHOW` and `TYPE` in Logo never evaluated their argument.** Both returned their
  source text, so `SHOW 1 + 2` displayed `1 + 2`. `PRINT` had always worked, so the
  two disagreed about the same expression.
- **A reporter could not be applied to a list literal** — `FIRST [1 2 3]` received
  just `[`, because argument consumption balanced parentheses but not brackets.
- **An input statement stopped the program.** The worker called `execute()` once, so
  every program that read input ended at its first prompt; resumed runs needed a loop.
- **The worker ran a program inline**, which meant `in-reply` and cooperative stop
  could never be delivered while a program was running. Stop now works.
- **`JOYINIT`** reported an absent *optional* backend with the error prefix, which the
  IDE presents as the program failing.

### Language lineup change — six active languages

Time Warp Studio now ships exactly **six** languages: **BASIC, PILOT, Logo,
COMAL, REXX, and Scheme**. The C, Pascal, Prolog, Forth, Brainfuck, and Python
executors have been removed from the product, along with their examples,
tutorials, snippets, lessons, and tests.

#### Added

- **COMAL** executor (`languages/comal.py`) — structured BASIC with
  `IF/ELIF/ELSE/ENDIF`, `FOR/NEXT`, `WHILE/ENDWHILE`, `REPEAT/UNTIL`,
  `LOOP/ENDLOOP`, `PROC/ENDPROC` + `CALL`, `DIM`, `DATA/READ`, `GOTO`/`GOSUB`,
  and turtle `MOVE`/`TURN` commands.
- **REXX** executor (`languages/rexx.py`) — typeless string processing with
  `SAY`, `PULL`, `PARSE`, `IF/THEN/ELSE`, `DO` (TO/BY/WHILE/UNTIL/FOREVER/
  OVER), `SELECT/WHEN`, labels + `CALL`/`ARG`/`RETURN`, `LEAVE`/`ITERATE`, a
  full expression evaluator (including `||` and abuttal concatenation) and 20+
  built-in string/number functions.
- **Scheme** executor (`languages/scheme.py`) — whole-program Lisp interpreter
  with lexical closures, `define`/`lambda`, `if`/`cond`/`case`/`when`/`unless`,
  `let`/`let*`/`letrec`/named `let`, `quote`/`quasiquote`, `set!`, list and
  vector primitives, `map`/`filter`/`foldl`/`apply`, and turtle bindings.
- New tutorials (`docs/tutorials/comal.md`, `rexx.md`, `scheme.md`), example
  programs (`Examples/comal/`, `Examples/rexx/`, `Examples/scheme/`), code
  snippets, and two guided lessons per new language.

#### Removed

- Executors: `brainfuck.py`, `c_lang_fixed.py`, `forth.py`, `pascal.py`,
  `prolog.py`, `python_lang.py`, plus their tests, examples, and tutorials.
- `Language` enum members `C`, `PASCAL`, `PROLOG`, `FORTH`, `BRAINFUCK` and
  `PYTHON_LANG`.

#### Changed

- `_WHOLE_PROGRAM_EXECUTORS` now contains Scheme only; the subprocess sandbox
  applies to Scheme.
- Syntax highlighting, comment toggling, keyword reference cards, document
  outlines, code formatters, snippet library, lessons, status-bar emoji,
  project icons, telemetry HUD, and output prompts all cover the new lineup.
- Example verifier default language map updated (`comal`, `rexx`, `scheme`).

All notable changes to Time Warp Studio will be documented in this file.

> **Scope note:** The current release actively supports **6 languages**:
> BASIC, PILOT, Logo, COMAL, REXX, and Scheme, and Python. Older
> entries below mention additional language executors (Smalltalk,
> Perl, Ruby, Erlang, Haskell, JavaScript, Lua, HyperTalk, PostScript,
> LISP/Scheme, Tcl, 6502 Assembly) that were built at the time but have since
> been removed from `time_warp/languages/` and are no longer part of the
> product. These entries are kept as historical record only.

## [14.1.2] - 2026-10-15

### Packaging

- Matched the Python distribution name to the existing PyPI project `timewarp-ide`.

## [14.1.1] - 2026-10-15

### Packaging

- Included the desktop and standalone editor entry-point modules in the Python wheel.
- Documented installation directly from the GitHub Release wheel.

## [14.1.0] - 2026-10-15

### Studio Improvements

- Added a lessons dock with built-in lessons and checkpoint verification.
- Added a Borland Turbo-style keymap and live Function-Key Bar.
- Expanded graphics, sound, and Prolog capabilities.
- Removed obsolete prototypes and aligned documentation and packaging with the supported Studio.

## [14.0.0] - 2026-10-01

### Editor and Workflow Improvements

- Added Go to Line navigation, clickable error locations, and error-only output filtering.
- Added a searchable document outline for supported procedure and function declarations.
- Added recovery for unsaved editor tabs after an unexpected shutdown.
- Made lesson authoring and file-dialog filters use the canonical nine-language registry.
- Corrected current release documentation and package metadata to match the active language set.

## [13.0.0] - 2026-06-01

### New Languages

- **Smalltalk executor** (`languages/smalltalk.py`) — Smalltalk-80 interpreter (~830 lines) supporting:
  - `Transcript show:/showCr:/print:` for output; temp variable declarations `| x y |`
  - Assignment `:=`, conditional `ifTrue:/ifFalse:/ifTrue:ifFalse:`
  - Iteration: `timesRepeat:`, `to:do:`, `to:by:do:`, `do:`, `whileTrue:`, `whileFalse:`
  - Collections: `OrderedCollection new`, `add:`, `at:put:`, `select:`, `collect:`, `reject:`, `inject:into:`
  - String messages: `,` (concat), `size`, `reversed`, `asUppercase`, `asLowercase`, `copyFrom:to:`
  - Block closures `[...]` with correct outer-scope variable sharing
  - Number messages: `factorial`, `sqrt`, `abs`, `max:`, `min:`, `raisedTo:`, `gcd:`
  - Turtle graphics via `Turtle forward:`, `Turtle right:`, etc.
  - 31/31 tests passing; 5 example programs in `Examples/smalltalk/`

### Syntax Highlighting

- **Smalltalk** — keyword/unary/binary message highlighting, `"..."` comment style, string literals

## [12.0.0] - 2026-06-01

### New Languages

- **Perl 5 executor** (`languages/perl.py`) — full Perl 5 interpreter (~2450 lines) supporting:
  - Scalar, array, and hash variables with proper scoping (`my`, `our`, `local`)
  - Control flow: `if`/`elsif`/`else`/`unless`, `while`/`until`, `for`/`foreach` (C-style and range), `do...while`
  - Statement modifiers: `print "x\n" if $cond;`, `last if $i > 3;`, `next if $i == 3;`
  - Subroutines with `@_` argument passing, closures, and recursive calls
  - Regular expressions: matching (`=~`), substitution (`s///`), global (`/g`), capture groups (`$1`–`$9`)
  - String builtins: `uc`, `lc`, `length`, `substr`, `index`, `rindex`, `reverse`, `sprintf`, `chomp`, `chop`
  - Array builtins: `push`, `pop`, `shift`, `unshift`, `splice`, `sort`, `reverse`, `map`, `grep`, `join`, `split`
  - Hash builtins: `keys`, `values`, `each`, `exists`, `delete`
  - Math builtins: `abs`, `int`, `sqrt`, `sin`, `cos`, `atan2`, `log`, `exp`, `rand`, `srand`
  - Turtle graphics: `forward`, `backward`, `left`, `right`, `penup`, `pendown`, `color`, `setheading`, `home`
  - 36/36 tests passing; 5 example programs in `Examples/perl/`

### Bug Fixes

- **Perl executor** — fixed 8 bugs found during test development:
  - `$i++` incorrectly parsed as binary `+` → infinite loops in `while` and C-style `for`
  - `_split_statements` did not split at `}` when outermost block closed → trailing statements after loops were silently dropped
  - Statement modifier loop ran after `print`/`last`/`next`/`return` handlers → `last if $cond` treated `if` as a label
  - `_contains_block` didn't detect unclosed string quotes → false modifier matches
  - `"a" . "b"` matched as a single double-quoted literal by `endswith('"')` check
  - `push @a, 3` extended array with wrong items when `@a` was expanded during arg evaluation
  - `split(/,/, $str)` failed because `/,/` was split at the comma by `_split_by_comma`
  - `_emit` split on `\n` eagerly → partial-line `print "$i "` calls produced separate output lines instead of accumulating

## [11.0.0] - 2026-06-01

### Major Features

- **Project File-Tree Panel** (`ui/project_tree.py`) — new `ProjectTreePanel` docked on the far left of the main window. Shows all tracked project files with language emoji icons; double-click to open in a new tab; right-click context menu: Add File, Remove from Project, Set as Main File. Auto-populates when a `.twsproj` project is opened and saves on any change.

- **Plugin Manifest Validation + Capability Sandboxing** (`features/plugin_system.py`) — plugins are now scanned with `ast.parse()` (no code execution) before loading. Plugins that import dangerous modules (`ctypes`, `cffi`, `subprocess`, `socket`, etc.) without declaring the corresponding capability in a `manifest.json` file are blocked. Blocked plugins are listed with a clear error message; safe plugins load unchanged.

- **Plugin Marketplace SQLite Persistence** (`features/plugin_marketplace.py`) — marketplace listings, reviews, releases, developers, and templates now survive application restarts via a SQLite database at `~/.time_warp/databases/marketplace.db`. All in-memory state is loaded from the database on startup and written on every mutation.

- **Real LAN Collaboration TCP Send/Receive** (`features/collaboration.py`) — `_send_message()` now actually transmits to peers over TCP using a 4-byte length-prefixed JSON framing protocol. Host binds on port 54321; joiner connects at session join. Background `_recv_loop` threads dispatch incoming messages (code updates, cursor moves, chat, sync requests) to registered event callbacks. `disconnect()` cleanly closes all sockets.

- **Student Roster + Assignment Distribution** (`features/classroom_mode.py`) — added `Student` and `StudentRoster` dataclasses, persisted to `~/.time_warp/classroom_roster.json`. New methods: `add_student()`, `remove_student()`, `save_roster()`, `distribute_assignment()` (writes bundle files to each student's work directory), and `collect_submissions()` (zips all student work into a timestamped archive in the bundles directory).

- **Persistent Terminal History** (`ui/terminal_widget.py`) — shell commands are now appended to `~/.time_warp/terminal_history` (plain text, one per line). On next launch the history is pre-loaded into the up/down arrow history buffer (up to 200 entries in memory, unlimited on disk).

### Previously Completed (v10.x)

- **Assembly 6502 executor** (`languages/asm6502.py`) — 2-pass assembler, 64 KB virtual machine, 40+ opcodes, all addressing modes
- **Code folding** (`ui/editor.py`) — ▼/▶ gutter glyphs, `toggle_fold()`, hides/shows QTextBlock ranges
- **Split editor** (`ui/main_window.py`) — `QSplitter`-based dual-pane editing (View → Split Editor)

### Bug Fixes

- **Ruby executor** (`languages/ruby.py`) — full initial implementation with extensive bug fixes:
  - `_rfind_op` now guards against treating `<<` (append operator) as two separate `<` operators — `r << x` no longer mis-splits into `r <` and `x`
  - `_split_statements` pre-expands semicolon-separated statements so `stmt1; stmt2` inside `{...}` blocks is correctly treated as two statements
  - `break if COND` / `next if COND` / `return if COND` modifier-only forms now handled correctly (no left-hand value before the keyword)
  - Negative array subscript access (`seq[-1]`, `seq[-2]`) now works via `_eval_chain`
  - Turtle API bindings corrected: `penup()`, `pendown()`, `setheading()`, `pencolor()`, `setpenwidth()`
  - Multi-line `do...end` blocks no longer split at binary operators (`\n` guard in `_eval`)
  - Multi-line array/hash literals now grouped correctly via `paren_depth` tracking in `_split_statements`
  - Assignment regex uses `re.DOTALL` so multi-line values are captured
  - String interpolation `#{}` correctly handles nested braces via brace-depth tracking in `_eval_dstring`
  - Block auto-splatting: `{|word, count| ...}` blocks automatically destructure single-array arguments
  - `||=` and `&&=` compound assignment operators now supported
  - All 5 Ruby demo programs pass; 217/217 coverage tests pass

- **Python executor** (`languages/python_lang.py`) — fixed all 4 Python demo programs

- **HyperTalk executor** (`languages/hypertalk.py`):
  - Fixed `BREAK`/`CONTINUE`/`RETURN` signals not propagating out of `if...end if` blocks — `exit repeat` inside an `if` block no longer causes the enclosing `repeat while` loop to run all 100,000 iterations
  - Fixed compound arithmetic expressions with embedded function calls (e.g. `put random(25) + 15 into var`) — the Python-eval fallback now pre-substitutes function calls before evaluation, so `random(n) + k` evaluates correctly
  - Fixed `_call_fn` RETURN value handling: `_exec_lines` unwraps `("RETURN", val)` tuples before returning, so call sites now check `if result is not None` instead of the tuple wrapper. User-defined functions now correctly return their computed values.
  - Fixed greedy function-call regex: replaced `^(\w+)\s*\((.*)\)$` with a balanced-parenthesis scan so expressions like `sin(r) / cos(r)` are no longer misidentified as a single function call.
  - Wrapped `float(args[0])` in `try/except` to prevent `ValueError` when a built-in receives a non-numeric first argument.
- **Erlang executor** — fixed boolean pattern matching: atoms `true`/`false` in `case` clauses now match Python `True`/`False` from built-in functions like `is_prime/1`
- **Erlang executor** — fixed `_split_fun_clauses` infinite loop caused by semicolons inside `if...end` blocks being incorrectly treated as clause separators; keyword-depth tracking (`if/case/begin/receive...end`) now prevents this

### Demo Program Fixes (timeout → 0 timeouts)

- **`Examples/erlang/number_theory.erl`** — reduced amicable-pairs search range (2000→300) and perfect-numbers loop (1..50→1..30) to complete within the 15-second test budget
- **`Examples/forth/mathematical_wonders.forth`** — fixed `SIEVE-MARK` stack corruption (`DUP *` → `DUP DUP *`), removed erroneous `ELSE DROP` in `BUILD-SIEVE`, added `4DUP` word definition, added `FVARIABLE`/`F@`/`F!`/`F+!` support to the Forth executor, reduced Hanoi threshold to avoid deep Python recursion
- **`Examples/lua/turtle_patterns.lua`** — reduced all spiral/curve step counts 10× (3000→300, 5000→500, etc.)
- **`Examples/hypertalk/adventure_game.htalk`** — resolved via the HyperTalk executor bug fixes above

### New Language Features

- **HyperTalk** — `find [whole|partial|string|chars|word] <text> [in <container>]` command: searches a variable or the output buffer and stores the match in `it` (roadmap Q2 2026)
- **HyperTalk** — `go [to] <card|next|prev|first|last>` navigation command (logs destination; no-op in desktop mode)
- **Prolog** — `\+(Goal)` negation-as-failure now parsed correctly from source (previously only worked when called from rule bodies, not in queries)
- **Prolog** — arithmetic comparison operators (`<`, `>`, `=<`, `>=`, `=:=`, `=\=`) now accept arbitrary arithmetic expressions on both sides, not just bare variable names
- **Prolog** — new built-ins: `format/2` (with `~w`/`~d`/`~a`/`~n` directives), `string_concat/3`, `number_string/2`, `split_string/4`, `string_length/2`, `string_lower/2`, `string_upper/2`
- **Forth executor** — `FVARIABLE`, `F@`, `F!`, `F+!` for float variable storage

### Infrastructure

- **`run.py`** — launcher now installs core deps (PySide6, Pillow) first; if the full `requirements.txt` install fails (e.g. `asyncpg` compilation issues on some platforms), a warning is shown and the desktop IDE still launches successfully
- **`Examples/CATALOG.md`** — added `javascript/functional.js` and `javascript/async_patterns.js`

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
- **JavaScript executor**: Fixed 8 bare `except:` clauses with specific exception types (`ValueError`, `TypeError`, `KeyError`, etc.)
- **Lua executor**: Fixed bare `except:` clause with specific exception types

### Interpreter Enhancements

- **HyperTalk**: Added `add/subtract/multiply/divide` math commands, `do`, `wait`, `visual effect`, `sort`, `beep`, `exit repeat`, `next repeat`, chunk expression ranges (`char i to j of`), built-in properties (`the date`, `the time`, `the ticks`, `the random`), new math functions (`average`, `sum`, `ln`, `log2`, `exp`, `atan`, `annuity`, `compound`)
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

### Demo Programs (new examples)

- **BASIC**: Maze generator with recursive backtracking
- **Logo**: Spirograph / hypotrochoid mathematical art
- **PILOT**: Typing speed tutor
- **C**: 3×3 matrix calculator
- **Pascal**: Number guessing game
- **Prolog**: Logic puzzle solver with unification
- **Forth**: Geometric turtle art
- **Brainfuck**: Fibonacci sequence generator
- **JavaScript**: Pub/Sub event system
- **HyperTalk**: Interactive quiz builder
- **Lua**: Traffic light state machine

### Documentation

- Updated README with v8.1.0 features
- Created CHANGELOG.md

---

## [8.0.0] - 2025-06-01

### Initial Release

- 12 programming language executors: BASIC, PILOT, Logo, COMAL, REXX, and Scheme, JavaScript, Lua, HyperTalk, Erlang
- PySide6 (Qt6) desktop IDE with editor, canvas, and output panels
- 26 built-in themes including retro CRT and accessibility options
- Integrated debugger with timeline recording and rewind
- Turtle graphics with 50+ drawing commands
- Lesson system with auto-verification
- AI assistant and error explainer panels
- Initial example programs across all 12 languages
- Comprehensive automated test suite
