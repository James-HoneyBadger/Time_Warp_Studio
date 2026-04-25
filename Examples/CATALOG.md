# Examples Catalog

A reference guide to the 107 example programs included with **Time Warp Studio**.
Open any file via **File → Open** or by browsing the `Examples/` folder.

---

## Index of Examples

### Quick Links

- **[BASIC](#basic-gw-basic--qbasic-style)** - Educational programming
- **[Brainfuck](#brainfuck)** - Minimalistic esolang
- **[C](#c)** - Systems programming
- **[Erlang](#erlang)** - Concurrent functional programming
- **[Forth](#forth)** - Stack-based programming
- **[Haskell](#haskell)** - Functional programming
- **[HyperTalk](#hypertalk)** - HyperCard scripting
- **[JavaScript](#javascript)** - Web scripting
- **[Logo](#logo)** - Turtle graphics
- **[Lua](#lua)** - Lightweight scripting
- **[Pascal](#pascal)** - Structured programming
- **[PILOT](#pilot-programmed-inquiry-learning-or-teaching)** - Instructional programming
- **[Prolog](#prolog)** - Logic programming
- **[Python](#python)** - General-purpose scripting
- **[REXX](#rexx)** - IBM scripting
- **[Ruby](#ruby)** - Dynamic object-oriented programming
- **[Rust](#rust)** - Systems programming with memory safety
- **[Scheme](#scheme)** - Lisp-family functional programming
- **[Smalltalk](#smalltalk)** - Object-oriented programming

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
| `erlang/pattern_match.erl` | Pattern matching and recursive functions |
| `erlang/actor_bank.erl` | **SHOWCASE** — Actor-style bank simulation, state machines, list comprehensions, prime sieve |
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

## Haskell

| File | Description |
|------|-------------|
| `haskell/hello.hs` | Hello World — putStrLn and IO monad |
| `haskell/functional_showcase.hs` | map, filter, fold, list comprehensions |
| `haskell/parser_combinator.hs` | Toy parser combinator library |
| `haskell/functional_masterpiece.hs` | **SHOWCASE** — Expression parser, type classes, Church numerals, infinite streams, Y combinator |
| `haskell/morse_code.hs` | **SHOWCASE** — Morse encoder/decoder with binary tree, timing analysis, round-trip verification |

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

## Python

| File | Description |
|------|-------------|
| `python/hello.py` | Hello World — print() |
| `python/cipher_tools.py` | Caesar, Vigenère, ROT13 ciphers |
| `python/fractal_explorer.py` | Turtle-based fractal drawing |
| `python/task_manager.py` | CLI task manager with persistence |
| `python/text_analyzer.py` | Word frequency, readability metrics |
| `python/lsystem_turtle.py` | **SHOWCASE** — L-System fractals: Koch, Sierpinski, Dragon curve, Hilbert, Barnsley Fern |
| `python/statistics_suite.py` | **SHOWCASE** — Statistical analysis: mean/median/mode/quartiles/skewness, z-scores, turtle bar chart |

---

## REXX

| File | Description |
|------|-------------|
| `rexx/hello.rex` | Hello World — say |
| `rexx/sysadmin_toolkit.rexx` | System administration utilities |
| `rexx/text_processor.rexx` | Text manipulation with REXX stem variables |
| `rexx/data_analyzer.rexx` | **SHOWCASE** — Word frequency, statistics, string toolkit, ROT13, Fibonacci, prime sieve |
| `rexx/number_systems.rexx` | **SHOWCASE** — Base conversion (bin/oct/hex/36), Roman numerals, bitwise ops, palindromic numbers |

---

## Scheme

| File | Description |
|------|-------------|
| `scheme/hello.scm` | Hello World — display + newline |
| `scheme/functional_toolkit.scm` | Higher-order functions, currying, Y combinator |
| `scheme/metacircular.scm` | Metacircular evaluator (Scheme in Scheme) |
| `scheme/quiz_game.scm` | Interactive quiz using tail recursion |
| `scheme/puzzle_solver.scm` | **SHOWCASE** — 8-Queens (92 solutions), Y combinator, lazy streams, CPS, call/cc, BST |
| `scheme/lambda_calculus.scm` | **SHOWCASE** — Lambda calculus: Church encoding, beta reduction, Y combinator, SKI combinators |

---

## Smalltalk

| File | Description |
|------|-------------|
| `smalltalk/hello.st` | Hello World — Transcript show: |
| `smalltalk/bank_system.st` | Bank account class with inheritance |
| `smalltalk/shapes_demo.st` | Shape hierarchy with polymorphism |
| `smalltalk/design_patterns.st` | **SHOWCASE** — Observer, Strategy, Composite, Command patterns; collections deep-dive |
| `smalltalk/ascii_patterns.st` | **SHOWCASE** — Pascal's triangle, number spiral, diamonds, checkerboard, Sierpinski, multiplication table |

---

## Ruby

| File | Description |
|------|-------------|
| `ruby/hello.rb` | Hello World — puts and basic syntax |
| `ruby/oop_demo.rb` | Classes, inheritance, and mixins |
| `ruby/ecosystem_simulation.rb` | **SHOWCASE** — Lotka-Volterra predator-prey simulation, 50-year ASCII population charts |
| `ruby/word_games.rb` | **SHOWCASE** — Anagram finder, Scrabble scorer, palindrome checker, word frequency, edit distance |

---

## Rust

| File | Description |
|------|-------------|
| `rust/hello.rs` | Hello World — fn main and println! |
| `rust/structs.rs` | Structs, impl blocks, and ownership |
| `rust/algorithms_library.rs` | **SHOWCASE** — Generic Stack/Queue, BST enum, sorting, HashMap, number theory, statistics |
| `rust/game_of_life.rs` | **SHOWCASE** — Conway's Game of Life: Grid struct/impl, 7 patterns, toroidal grid, period detection |

---

---

## Perl

| File | Description |
|------|-------------|
| `perl/dna_analyzer.pl` | **SHOWCASE** — DNA translation, ORF finder, GC content, regex motif search, k-mer analysis |
| `perl/text_analytics.pl` | **SHOWCASE** — Flesch-Kincaid readability, word frequency, bigrams/trigrams, concordance, sentence stats |

---

*Total: 107 example programs across 20 languages.*
*Files marked **SHOWCASE** are full-featured demo applications.*
