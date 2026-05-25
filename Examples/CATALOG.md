# Examples Catalog

A reference guide to the example programs included with **Time Warp Studio**.
Open any file via **File → Open** or by browsing the `Examples/` folder.

---

## Index of Examples

### Quick Links

- **[BASIC](#basic-gw-basic--qbasic-style)** - Educational programming
- **[Brainfuck](#brainfuck)** - Minimalistic esolang
- **[C](#c)** - Systems programming
- **[COBOL](#cobol)** - Business data processing
- **[Erlang](#erlang)** - Concurrent functional programming
- **[Forth](#forth)** - Stack-based programming
- **[HyperTalk](#hypertalk)** - HyperCard scripting
- **[JavaScript](#javascript)** - Web scripting
- **[LISP/Scheme](#lispscheme)** - Functional / symbolic computation
- **[Logo](#logo)** - Turtle graphics
- **[Lua](#lua)** - Lightweight scripting
- **[Pascal](#pascal)** - Structured programming
- **[PILOT](#pilot-programmed-inquiry-learning-or-teaching)** - Instructional programming
- **[PostScript](#postscript)** - Stack-based page description
- **[Prolog](#prolog)** - Logic programming
- **[Python](#python)** - General-purpose (sandboxed)
- **[Ruby](#ruby)** - Dynamic object-oriented scripting
- **[Tcl](#tcl)** - Embeddable scripting

---

## BASIC (GW-BASIC / QBASIC style)

| File | Description |
|------|-------------|
| `basic/hello.bas` | Hello World with PRINT and line numbers |
| `basic/adventure.bas` | Text adventure game with ON/GOTO branching |
| `basic/budget_tracker.bas` | Simple income/expense tracker with arrays |
| `basic/maze_generator.bas` | Recursive maze generation using GOSUB |
| `basic/statistics.bas` | Statistical calculations with DATA/READ |
| `basic/math_explorer.bas` | **SHOWCASE** — Mandelbrot ASCII art, prime sieve, Fibonacci, Lissajous turtle curves |
| `basic/space_simulation.bas` | **SHOWCASE** — Solar system explorer: planetary data, Kepler's law, escape velocities, orbital turtle graphics |

---

## Brainfuck

| File | Description |
|------|-------------|
| `brainfuck/hello.bf` | Hello World — the classic 8-instruction program |
| `brainfuck/fibonacci.bf` | Fibonacci sequence generator |
| `brainfuck/programs.bf` | Collection of short BF programs |
| `brainfuck/multiplication_table.bf` | **SHOWCASE** — 9×9 multiplication table via nested loops and ASCII digit output |

---

## C

| File | Description |
|------|-------------|
| `c/hello.c` | Hello World — printf + main |
| `c/linked_list.c` | Singly-linked list with insert/delete/traverse |
| `c/matrix_calculator.c` | Matrix addition, multiplication, transpose |
| `c/rpn_calc.c` | Reverse Polish Notation calculator with stack |
| `c/sorting_algorithms.c` | Bubble, selection, insertion, merge sort |
| `c/algorithms_showcase.c` | **SHOWCASE** — Merge sort, prime sieve, memoized Fibonacci, matrix ops, string analysis |
| `c/game_of_life.c` | **SHOWCASE** — Conway's Game of Life: glider, blinker, toad, beacon, R-pentomino, toroidal grid |

---

## Erlang

| File | Description |
|------|-------------|
| `erlang/hello.erl` | Hello World — io:format and module declaration |
| `erlang/pattern_match.erl` | Pattern matching and recursive functions || `erlang/binary_strings.erl` | Binary strings, bytes, and bitstring operations |
| `erlang/functional_patterns.erl` | Higher-order functions, list comprehensions, closures |
| `erlang/algorithms.erl` | Classic algorithms in functional style || `erlang/actor_bank.erl` | **SHOWCASE** — Actor-style bank simulation, state machines, list comprehensions, prime sieve |
| `erlang/number_theory.erl` | **SHOWCASE** — Primes, sieve, GCD/LCM, Euler's totient, amicable numbers, modular arithmetic, CRT, Collatz |

---

## Forth

| File | Description |
|------|-------------|
| `forth/hello.f` | Hello World — stack-based I/O |
| `forth/rpn_calculator.forth` | Interactive RPN calculator words |
| `forth/turtle_art.forth` | Turtle graphics using Forth words |
| `forth/mathematical_wonders.forth` | **SHOWCASE** — Prime sieve, Fibonacci, Pi via Leibniz, Towers of Hanoi, turtle spirals |
| `forth/cellular_automata.forth` | **SHOWCASE** — 1D elementary cellular automata: Rule 30, 90, 110, 184 with CREATE/ALLOT arrays |

---

## HyperTalk

| File | Description |
|------|-------------|
| `hypertalk/hello.htalk` | Hello World — put … into, answer |
| `hypertalk/contact_stack.ht` | Contact card stack simulation |
| `hypertalk/quiz_builder.htalk` | Quiz with repeat/ask/answer flow |
| `hypertalk/adventure_game.htalk` | **SHOWCASE** — Text adventure with rooms, inventory, riddles, dragon combat, scoring |
| `hypertalk/calculator.htalk` | **SHOWCASE** — Scientific calculator: arithmetic, trig, logarithms, unit conversion, statistics |

---

## JavaScript

| File | Description |
|------|-------------|
| `javascript/hello.js` | Hello World — console.log |
| `javascript/data_structures.js` | Stack, queue, linked list, binary tree |
| `javascript/event_system.js` | Simple pub/sub event emitter |
| `javascript/todo_app.js` | In-memory to-do list with CRUD |
| `javascript/algorithm_masterclass.js` | **SHOWCASE** — BST, graph BFS/DFS, heap sort, currying, generators, infinite primes |
| `javascript/mandelbrot.js` | **SHOWCASE** — ASCII Mandelbrot set, Julia sets, Burning Ship, Tricorn; zoom views; density gradient |
| `javascript/functional.js` | **SHOWCASE** — Higher-order functions, closures, memoization, partial application, pipelines, group-by |
| `javascript/async_patterns.js` | Async patterns — callbacks, promise chains, error-first style, badge/user pipelines |

---

## Logo

| File | Description |
|------|-------------|
| `logo/hello.logo` | Hello World — PRINT and basic FORWARD/RIGHT |
| `logo/fractal_forest.logo` | Recursive tree fractal using TO/END |
| `logo/galaxy.logo` | Spiral galaxy pattern |
| `logo/mandala.logo` | Symmetric mandala with REPEAT |
| `logo/spirograph.logo` | Spirograph curves using nested REPEAT |
| `logo/fractal_gallery.logo` | **SHOWCASE** — Koch snowflake, Sierpinski triangle, fractal tree, rose curve, golden spiral |
| `logo/geometric_artistry.logo` | **SHOWCASE** — Koch snowflake grid, Islamic 8-star, rosette mandala, hexagonal tiling, optical art |

---

## Lua

| File | Description |
|------|-------------|
| `lua/hello.lua` | Hello World — print() |
| `lua/music_theory.lua` | Note frequencies, chord generation |
| `lua/rpg_engine.lua` | Mini RPG combat engine with tables |
| `lua/state_machine.lua` | Generic finite state machine |
| `lua/adventure_engine.lua` | **SHOWCASE** — RPG dungeon crawler with OOP metatables, combat, inventory, shop |
| `lua/turtle_patterns.lua` | **SHOWCASE** — Mathematical curves: spirograph, hypotrochoid, rose curves, Lissajous, Archimedean spiral |

---

## Pascal

| File | Description |
|------|-------------|
| `pascal/hello.pas` | Hello World — writeln |
| `pascal/hangman.pas` | Hangman game with string handling |
| `pascal/inventory.pas` | Inventory system with records and files |
| `pascal/number_game.pas` | Guess-the-number with while loop |
| `pascal/cipher_lab.pas` | **SHOWCASE** — ROT13, Caesar, Vigenère, Affine, frequency analysis, columnar transposition |
| `pascal/grade_book.pas` | **SHOWCASE** — Student grade book: 20 students, statistics, histogram, GPA report, bubble sort |

---

## PILOT (Programmed Inquiry, Learning Or Teaching)

| File | Description |
|------|-------------|
| `pilot/hello.pilot` | Hello World — T: type command |
| `pilot/history_quiz.pilot` | Multiple-choice history quiz |
| `pilot/typing_tutor.pilot` | Typing practice drills |
| `pilot/brain_trainer.pilot` | **SHOWCASE** — 15-question science quiz with scoring, hints, conditional branching |
| `pilot/math_drill.pilot` | **SHOWCASE** — 20-question arithmetic flashcard drill covering +−×÷ with letter grade scoring |

---

## Prolog

| File | Description |
|------|-------------|
| `prolog/hello.pl` | Hello World — facts and write/nl |
| `prolog/expert_system.pl` | Simple rule-based diagnosis system |
| `prolog/family_tree.pl` | Family tree with ancestor/descendent queries |
| `prolog/puzzle_solver.pl` | Sudoku-like constraint puzzle |
| `prolog/knowledge_engine.pl` | **SHOWCASE** — Animal ontology, genealogy KB, graph coloring, meta-reasoning |
| `prolog/family_genealogy.pl` | **SHOWCASE** — 4-generation family tree (22 members), relationship inference: cousins, ancestors, distances |

---

## COBOL

| File | Description |
|------|-------------|
| `cobol/hello.cob` | Hello World — IDENTIFICATION DIVISION, DISPLAY |
| `cobol/counter.cob` | Counter demo with PERFORM and UNTIL |
| `cobol/fibonacci.cob` | Fibonacci sequence via iterative PERFORM |
| `cobol/grade_calculator.cob` | Numeric grade to letter grade conversion |
| `cobol/grade_table.cob` | Grade table with formatted report output |
| `cobol/multiplication_table.cob` | 10×10 multiplication table using nested PERFORM |
| `cobol/string_operations.cob` | String inspect, move, concatenation operations |
| `cobol/bank_account.cob` | **SHOWCASE** — Bank account simulation: deposit, withdraw, balance enquiry, interest |

---

## LISP/Scheme

| File | Description |
|------|-------------|
| `lisp/hello.scm` | Hello World — display, newline, arithmetic, string operations |
| `lisp/recursion.scm` | Recursive algorithms with accumulator pattern and tail-call optimisation |
| `lisp/list_processing.scm` | map, filter, fold, quasiquote, association lists, sorting |
| `lisp/data_structures.scm` | Closures as objects, vectors, association lists, records |
| `lisp/turtle_art.scm` | **SHOWCASE** — Turtle graphics via recursion: fractals, spirals, geometric patterns |

---

## PostScript

| File | Description |
|------|-------------|
| `postscript/hello.ps` | Hello World — stack-based output |
| `postscript/arithmetic.ps` | Arithmetic operators and stack manipulation |
| `postscript/counter.ps` | Variables and loops with def and repeat |
| `postscript/fibonacci.ps` | Fibonacci sequence on the operand stack |
| `postscript/loops.ps` | for, repeat, and loop constructs |
| `postscript/strings.ps` | String operations: concat, search, length |
| `postscript/sorting.ps` | **SHOWCASE** — Bubble sort with array operations and stack discipline |

---

## Tcl

| File | Description |
|------|-------------|
| `tcl/hello.tcl` | Hello World — set and puts |
| `tcl/fibonacci.tcl` | Fibonacci sequence via proc and recursion |
| `tcl/fizzbuzz.tcl` | FizzBuzz from 1 to 30 |
| `tcl/lists_and_strings.tcl` | List operations: lappend, lsort, string commands |
| `tcl/procedures.tcl` | Procedures, arguments, and factorial recursion |
| `tcl/sorting.tcl` | **SHOWCASE** — Bubble sort and selection sort with array manipulation |

---

## Ruby

| File | Description |
|------|-------------|
| `ruby/hello.rb` | Hello World — string interpolation, iterators, string methods |
| `ruby/fibonacci.rb` | Fibonacci sequence — recursive, iterative, and memoised approaches |
| `ruby/classes_demo.rb` | OOP showcase — classes, inheritance, modules, attr_accessor, class methods |
| `ruby/data_processing.rb` | **SHOWCASE** — Arrays, hashes, enumerable methods (sort_by, group_by, tally, select) |
| `ruby/turtle_art.rb` | Turtle graphics — colourful spiral using nested square drawing |

---

## Python

| File | Description |
|------|-------------|
| `python/hello.py` | Hello World — f-strings, loops, dicts, basic operations |
| `python/algorithms.py` | Sorting and searching — bubble sort, merge sort, binary search, Sieve of Eratosthenes |
| `python/list_comprehensions.py` | **SHOWCASE** — List/dict/set comprehensions, map/filter/reduce, functional patterns |
| `python/turtle_art.py` | Turtle graphics — concentric polygons with colour cycling |

---

## Haskell

| File | Description |
|------|-------------|
| `haskell/hello.hs` | Hello World — do-notation, putStrLn, string concatenation |
| `haskell/list_processing.hs` | **SHOWCASE** — map, filter, fold, zip, list comprehensions, Pythagorean triples |
| `haskell/recursion.hs` | Pattern matching — factorial, Fibonacci, custom list functions, quicksort, mergesort |
| `haskell/higher_order.hs` | Higher-order functions — composition, currying, flip, const, zipWith, any/all |
| `haskell/algorithms.hs` | Algorithms — primes, GCD/LCM, Collatz, Caesar cipher, run-length encoding |

---

*Total: 119 example programs across 19 languages.*
*Files marked **SHOWCASE** are full-featured demo applications.*
