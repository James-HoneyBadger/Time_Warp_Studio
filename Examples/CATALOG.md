# Examples Catalog

A reference guide to the example programs included with **Time Warp Studio**.
Open any file via **File → Open** or by browsing the `Examples/` folder.

---

## Index of Examples

### Quick Links

- **[6502 Assembly](#6502-assembly)** - Machine / educational
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
- **[Perl 5](#perl-5)** - Practical scripting and text processing
- **[APL](#apl)** - Array programming language (⍳⍴+/×/)
- **[REXX](#rexx)** - Restructured eXtended eXecutor
- **[Smalltalk](#smalltalk)** - Object-oriented message-passing
- **[PILOT](#pilot-programmed-inquiry-learning-or-teaching)** - Instructional programming
- **[PostScript](#postscript)** - Stack-based page description
- **[Prolog](#prolog)** - Logic programming
- **[Python](#python)** - General-purpose (sandboxed)
- **[Ruby](#ruby)** - Dynamic object-oriented scripting
- **[Tcl](#tcl)** - Embeddable scripting


---

## 6502 Assembly

| File | Description |
|------|-------------|
| `asm6502/hello.asm` | Hello World — character and string output via ROM calls |
| `asm6502/counting.asm` | Counting loops — 1-10, 10-1 countdown, even numbers using INX/BNE |
| `asm6502/fibonacci.asm` | Fibonacci — zero-page variables, 8-bit arithmetic, loop with INC/CMP |
| `asm6502/registers.asm` | **SHOWCASE** — arithmetic (ADC/SBC/ASL/LSR), bitwise (AND/ORA/EOR), transfers |
| `asm6502/subroutines.asm` | Subroutines — JSR/RTS, stack (PHA/PLA), nested calls |
| `asm6502/bubble_sort.asm` | Bubble sort — zero-page array, compare-and-swap, optimised early-exit |
| `asm6502/string_utils.asm` | String utilities — STRLEN, STRCPY, STRUPR via zero-page pointer arithmetic |
| `asm6502/math_ops.asm` | Math operations — 8-bit MUL8 (shift-add), DIV8 (shift-subtract), ADD16 |

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
| `brainfuck/cat.bf` | Cat program — echo stdin to stdout |
| `brainfuck/countdown.bf` | Countdown from 9 to 0 using a decrement loop |
| `brainfuck/rot13.bf` | ROT13 encoder — letter-range detection, conditional offsetting |
| `brainfuck/calculator.bf` | Simple addition calculator — two single-digit inputs |

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
| `c/string_utils.c` | String utilities — length, reverse, toupper/tolower, trim, split, startswith/endswith |

---

## Erlang

| File | Description |
|------|-------------|
| `erlang/hello.erl` | Hello World — io:format and module declaration |
| `erlang/pattern_match.erl` | Pattern matching and recursive functions || `erlang/binary_strings.erl` | Binary strings, bytes, and bitstring operations |
| `erlang/functional_patterns.erl` | Higher-order functions, list comprehensions, closures |
| `erlang/algorithms.erl` | Classic algorithms in functional style || `erlang/actor_bank.erl` | **SHOWCASE** — Actor-style bank simulation, state machines, list comprehensions, prime sieve |
| `erlang/number_theory.erl` | **SHOWCASE** — Primes, sieve, GCD/LCM, Euler's totient, amicable numbers, modular arithmetic, CRT, Collatz |
| `erlang/concurrency.erl` | **SHOWCASE** — Actors, ping-pong, worker pool, parallel Fibonacci via spawn/receive |

---

## Forth

| File | Description |
|------|-------------|
| `forth/hello.f` | Hello World — stack-based I/O |
| `forth/rpn_calculator.forth` | Interactive RPN calculator words |
| `forth/turtle_art.forth` | Turtle graphics using Forth words |
| `forth/mathematical_wonders.forth` | **SHOWCASE** — Prime sieve, Fibonacci, Pi via Leibniz, Towers of Hanoi, turtle spirals |
| `forth/cellular_automata.forth` | **SHOWCASE** — 1D elementary cellular automata: Rule 30, 90, 110, 184 with CREATE/ALLOT arrays |
| `forth/fibonacci.forth` | Fibonacci — iterative and recursive definitions, do-loop |
| `forth/sorting.forth` | Sorting — bubble sort and selection sort on a Forth array |
| `forth/string_processing.forth` | String processing — count, index, substring, uppercase, tokenize |

---

## HyperTalk

| File | Description |
|------|-------------|
| `hypertalk/hello.htalk` | Hello World — put … into, answer |
| `hypertalk/contact_stack.ht` | Contact card stack simulation |
| `hypertalk/quiz_builder.htalk` | Quiz with repeat/ask/answer flow |
| `hypertalk/adventure_game.htalk` | **SHOWCASE** — Text adventure with rooms, inventory, riddles, dragon combat, scoring |
| `hypertalk/calculator.htalk` | **SHOWCASE** — Scientific calculator: arithmetic, trig, logarithms, unit conversion, statistics |
| `hypertalk/number_guessing.htalk` | Number guessing game — ask/answer, repeat loop, high/low hints |
| `hypertalk/todo_list.htalk` | To-do list — add/remove/complete items, list display |
| `hypertalk/flashcards.htalk` | Flashcard drill — geography facts, score tracking, missed-card review |

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
| `logo/turtle_3d.logo` | **SHOWCASE** — 3D turtle: helix, starburst, spiral using PITCH, ROLL, ENABLE3D |

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
| `lua/data_structures.lua` | Data structures — tables as arrays, dicts, sets, stack, and queue |
| `lua/oop_demo.lua` | OOP with metatables — classes, inheritance, mixins, operator overloading |

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
| `pascal/linked_list.pas` | Pointer-based singly-linked list — insert, delete, search, reverse |
| `pascal/sorting.pas` | Sorting algorithms — bubble, insertion, selection, and quicksort |

---

## Perl 5

| File | Description |
|------|-------------|
| `perl/hello.perl` | Hello World — output, variables, conditionals, loops |
| `perl/strings.perl` | **SHOWCASE** — string builtins: uc/lc, substr, index, split/join, sprintf, reverse |
| `perl/arrays.perl` | **SHOWCASE** — arrays, hashes, push/pop/shift, map/grep, sort, slices |
| `perl/regex.perl` | **SHOWCASE** — regex matching, captures, substitution, global match, validation |
| `perl/turtle_art.perl` | **SHOWCASE** — turtle graphics: nested hexagons, spiral square, petal flower |
| `perl/data_structures.perl` | Data structures — arrays, hashes, AoH, HoA, sets, queues |
| `perl/text_processing.perl` | Text processing — split/join, regex extraction, word count, CSV parsing |
| `perl/algorithms.perl` | Algorithms — quicksort, binary search, sieve, GCD/LCM, Fibonacci |

---

## PILOT (Programmed Inquiry, Learning Or Teaching)

| File | Description |
|------|-------------|
| `pilot/hello.pilot` | Hello World — T: type command |
| `pilot/history_quiz.pilot` | Multiple-choice history quiz |
| `pilot/typing_tutor.pilot` | Typing practice drills |
| `pilot/brain_trainer.pilot` | **SHOWCASE** — 15-question science quiz with scoring, hints, conditional branching |
| `pilot/math_drill.pilot` | **SHOWCASE** — 20-question arithmetic flashcard drill covering +−×÷ with letter grade scoring |
| `pilot/science_quiz.pilot` | Science quiz — 10 questions on physics, chemistry, biology with scoring |
| `pilot/vocabulary_trainer.pilot` | Vocabulary trainer — word definitions, hints, repeat missed words |
| `pilot/number_game.pilot` | Number guessing game — conditional branching, high/low feedback |

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
| `prolog/list_operations.pl` | List operations — append, member, length, reverse, nth, flatten, permutations |
| `prolog/sorting.pl` | Sorting — quicksort, merge sort, insertion sort, bubble sort |

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
| `lisp/higher_order.scm` | Higher-order functions — compose, curry, partial application, Y-combinator |
| `lisp/number_theory.scm` | Number theory — primes, GCD, Fibonacci, perfect numbers, Collatz |
| `lisp/pattern_matching.scm` | Pattern matching — cond, and/or dispatch, let/letrec, association list lookup |

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
| `tcl/oop_demo.tcl` | TclOO demo — classes, inheritance, mixins, Printable mixin, introspection |
| `tcl/algorithms.tcl` | Algorithms — bubble sort, merge sort, binary search, GCD/LCM, prime sieve |

---

## Ruby

| File | Description |
|------|-------------|
| `ruby/hello.rb` | Hello World — string interpolation, iterators, string methods |
| `ruby/fibonacci.rb` | Fibonacci sequence — recursive, iterative, and memoised approaches |
| `ruby/classes_demo.rb` | OOP showcase — classes, inheritance, modules, attr_accessor, class methods |
| `ruby/data_processing.rb` | **SHOWCASE** — Arrays, hashes, enumerable methods (sort_by, group_by, tally, select) |
| `ruby/turtle_art.rb` | Turtle graphics — colourful spiral using nested square drawing |
| `ruby/blocks_procs_lambdas.rb` | Blocks, procs & lambdas — closures, yield, method objects, composition |
| `ruby/sorting_algorithms.rb` | Sorting algorithms — bubble, insertion, selection, merge, quicksort |
| `ruby/modules_mixins.rb` | Modules and mixins — Comparable, Enumerable, custom modules, extend/include |

---

## Python

| File | Description |
|------|-------------|
| `python/hello.py` | Hello World — f-strings, loops, dicts, basic operations |
| `python/algorithms.py` | Sorting and searching — bubble sort, merge sort, binary search, Sieve of Eratosthenes |
| `python/list_comprehensions.py` | **SHOWCASE** — List/dict/set comprehensions, map/filter/reduce, functional patterns |
| `python/turtle_art.py` | Turtle graphics — concentric polygons with colour cycling |
| `python/data_structures.py` | Data structures — list, dict, set, stack, queue, defaultdict, Counter |
| `python/oop_demo.py` | OOP — classes, inheritance, dunder methods, dataclasses, abstract base classes |
| `python/generators.py` | Generators and itertools — yield, send, generator expressions, infinite sequences |
| `python/decorators.py` | Decorators — function decorators, class decorators, functools.wraps, retry, timer |

---

## APL

| File | Description |
|------|-------------|
| `apl/hello.apl` | Hello World — `⎕←` print, string output |
| `apl/arrays.apl` | Array operations — `⍳`, `⍴`, reshape, catenate, take/drop, reverse, grade-up |
| `apl/math.apl` | **SHOWCASE** — reductions `+/`, scans `+\`, inner/outer product, absolute value |
| `apl/statistics.apl` | Statistics — mean, max/min, sorted order via `⍋`, membership `∊` |
| `apl/turtle_art.apl` | Turtle graphics — polygon spiral using APL loop syntax |
| `apl/sorting.apl` | Sorting — grade-up `⍋`, grade-down `⍒`, matrix sort, rank, membership `∊` |
| `apl/string_processing.apl` | String processing — ⎕C, character testing, word split, Caesar cipher, frequency |
| `apl/game_of_life.apl` | Game of Life — Conway's Life using array operations (blinker, glider) |

---

## REXX

| File | Description |
|------|-------------|
| `rexx/hello.rexx` | Hello World — SAY output |
| `rexx/strings.rexx` | String operations — LENGTH, UPPER, LOWER, REVERSE, SUBSTR, CENTER, STRIP |
| `rexx/loops.rexx` | **SHOWCASE** — DO counted/step/WHILE, SELECT/WHEN, nested LEAVE |
| `rexx/procedures.rexx` | Subroutines — CALL/RETURN, RESULT variable, recursion, multiple arguments |
| `rexx/turtle_art.rexx` | Turtle graphics — square spiral with PENDOWN/FORWARD/RIGHT |
| `rexx/arrays.rexx` | Arrays (stem variables) — REXX compound variables, sorting, searching |
| `rexx/mathematics.rexx` | Mathematics — arithmetic, NUMERIC DIGITS, prime sieve, GCD/LCM |
| `rexx/parsing.rexx` | Parsing — PARSE, WORD/WORDS, tokenizer, CSV parser, template matching |

---

## Smalltalk

| File | Description |
|------|-------------|
| `smalltalk/hello.st` | Hello World — Transcript showCr: |
| `smalltalk/objects.st` | Objects via closures — counter simulation, message dispatch |
| `smalltalk/collections.st` | **SHOWCASE** — OrderedCollection, select:, collect:, inject:into: |
| `smalltalk/algorithms.st` | Algorithms — factorial, Fibonacci, bubble sort with blocks |
| `smalltalk/turtle_art.st` | Turtle graphics — spiral using timesRepeat: |
| `smalltalk/inheritance.st` | Inheritance and polymorphism — Animal/Dog/Cat class hierarchy, overriding |
| `smalltalk/blocks_closures.st` | Blocks and closures — first-class blocks, recursion, collection protocol |
| `smalltalk/conditionals.st` | Conditionals — ifTrue:ifFalse:, caseOf:otherwise:, respondsTo:, grade classifier |

---

## Haskell

| File | Description |
|------|-------------|
| `haskell/hello.hs` | Hello World — do-notation, putStrLn, string concatenation |
| `haskell/list_processing.hs` | **SHOWCASE** — map, filter, fold, zip, list comprehensions, Pythagorean triples |
| `haskell/recursion.hs` | Pattern matching — factorial, Fibonacci, custom list functions, quicksort, mergesort |
| `haskell/higher_order.hs` | Higher-order functions — composition, currying, flip, const, zipWith, any/all |
| `haskell/algorithms.hs` | Algorithms — primes, GCD/LCM, Collatz, Caesar cipher, run-length encoding |
| `haskell/sorting.hs` | Sorting — quicksort, merge sort, insertion sort, counting sort, radix sort |
| `haskell/data_types.hs` | Algebraic data types — Maybe, Either, Tree, custom records, pattern matching |
| `haskell/typeclasses.hs` | Type classes — Eq, Ord, Show, custom instances, Functor, Foldable |

---

*Total: 196 example programs across 24 languages.*
*Files marked **SHOWCASE** are full-featured demo applications.*
