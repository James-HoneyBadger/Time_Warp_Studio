# Examples Catalog

A reference guide to the 97 example programs included with **Time Warp Studio**.
Open any file via **File → Open** or by browsing the `Examples/` folder.

---

## Index of Examples

### Quick Links

- **[APL](#apl)** - Array programming
- **[Assembly](#assembly-x86-style-simulation)** - Low-level programming
- **[BASIC](#basic-gw-basic-qbasic-style)** - Educational programming
- **[Brainfuck](#brainfuck)** - Minimalistic esolang
- **[C](#c)** - Systems programming
- **[COBOL](#cobol)** - Business applications
- **[CICS](#cics)** - Transaction processing
- **[Forth](#forth)** - Stack-based programming
- **[Fortran](#fortran)** - Scientific computing
- **[Haskell](#haskell)** - Functional programming
- **[Hypertalk](#hypertalk)** - HyperCard scripting
- **[JavaScript](#javascript)** - Web scripting
- **[JCL](#jcl)** - Job control language
- **[Logo](#logo)** - Turtle graphics
- **[Lua](#lua)** - Lightweight scripting
- **[Pascal](#pascal)** - Structured programming
- **[Pilot](#pilot)** - Instructional programming
- **[Prolog](#prolog)** - Logic programming
- **[Python](#python)** - General-purpose scripting
- **[REXX](#rexx)** - IBM scripting
- **[Scheme](#scheme)** - Lisp-family functional programming
- **[Smalltalk](#smalltalk)** - Object-oriented programming
- **[SQL](#sql)** - Database queries
- **[SQR](#sqr)** - Reporting language

---

## APL

| File | Description |
|------|-------------|
| `apl/hello.apl` | Hello World — basic output and array operations |
| `apl/matrix_ops.apl` | Matrix operations: inner product, transpose, reshape |
| `apl/statistics_suite.apl` | Mean, variance, standard deviation as APL idioms |

---

## Assembly (x86-style simulation)

| File | Description |
|------|-------------|
| `assembly/hello.asm` | Hello World — registers and syscall simulation |
| `assembly/showcase.asm` | Arithmetic, memory, and stack operations |
| `assembly/string_ops.asm` | String manipulation via byte-level operations |

---

## BASIC (GW-BASIC / QBASIC style)

| File | Description |
|------|-------------|
| `basic/hello.bas` | Hello World with PRINT and line numbers |
| `basic/adventure.bas` | Text adventure game with ON/GOTO branching |
| `basic/budget_tracker.bas` | Simple income/expense tracker with arrays |
| `basic/maze_generator.bas` | Recursive maze generation using GOSUB |
| `basic/statistics.bas` | Statistical calculations with DATA/READ |

---

## Brainfuck

| File | Description |
|------|-------------|
| `brainfuck/hello.bf` | Hello World — the classic 8-instruction program |
| `brainfuck/fibonacci.bf` | Fibonacci sequence generator |
| `brainfuck/programs.bf` | Collection of short BF programs |

---

## C

| File | Description |
|------|-------------|
| `c/hello.c` | Hello World — printf + main |
| `c/linked_list.c` | Singly-linked list with insert/delete/traverse |
| `c/matrix_calculator.c` | Matrix addition, multiplication, transpose |
| `c/rpn_calc.c` | Reverse Polish Notation calculator with stack |
| `c/sorting_algorithms.c` | Bubble, selection, insertion, merge sort |

---

## CICS (IBM CICS transaction simulation)

| File | Description |
|------|-------------|
| `cics/hello.cics` | Hello World — EXEC CICS SEND TEXT |
| `cics/atm_transaction.cics` | ATM balance inquiry and withdrawal |
| `cics/customer_inquiry.cics` | Customer record lookup with EXEC CICS READ |
| `cics/order_entry.cics` | Order entry form with validation |

---

## COBOL

| File | Description |
|------|-------------|
| `cobol/hello.cob` | Hello World — IDENTIFICATION/PROCEDURE DIVISION |
| `cobol/account_ledger.cob` | Bank account ledger with PERFORM loops |
| `cobol/inventory_control.cob` | Inventory management with FILE SECTION |
| `cobol/payroll_processing.cob` | Payroll computation with COMPUTE |
| `cobol/report_generator.cob` | Formatted report output with WRITE |

---

## Forth

| File | Description |
|------|-------------|
| `forth/hello.f` | Hello World — stack-based I/O |
| `forth/rpn_calculator.forth` | Interactive RPN calculator words |
| `forth/turtle_art.forth` | Turtle graphics using Forth words |

---

## Fortran (FORTRAN 77 style)

| File | Description |
|------|-------------|
| `fortran/hello.f77` | Hello World — FORMAT and WRITE |
| `fortran/numerical_methods.f77` | Newton–Raphson, bisection, trapezoidal rule |
| `fortran/physics_sim.f77` | Projectile motion simulation |

---

## Haskell

| File | Description |
|------|-------------|
| `haskell/hello.hs` | Hello World — putStrLn and IO monad |
| `haskell/functional_showcase.hs` | map, filter, fold, list comprehensions |
| `haskell/parser_combinator.hs` | Toy parser combinator library |

---

## HyperTalk

| File | Description |
|------|-------------|
| `hypertalk/hello.htalk` | Hello World — put … into, answer |
| `hypertalk/contact_stack.ht` | Contact card stack simulation |
| `hypertalk/quiz_builder.htalk` | Quiz with repeat/ask/answer flow |

---

## JavaScript

| File | Description |
|------|-------------|
| `javascript/hello.js` | Hello World — console.log |
| `javascript/data_structures.js` | Stack, queue, linked list, binary tree |
| `javascript/event_system.js` | Simple pub/sub event emitter |
| `javascript/todo_app.js` | In-memory to-do list with CRUD |

---

## JCL (IBM Job Control Language simulation)

| File | Description |
|------|-------------|
| `jcl/hello.jcl` | Hello World — JOB/EXEC/DD cards |
| `jcl/daily_batch.jcl` | Daily batch job with multiple steps |
| `jcl/data_migration.jcl` | Dataset copy and transform job |
| `jcl/month_end.jcl` | Month-end processing job stream |
| `jcl/payroll_run.jcl` | Payroll batch with IEBGENER |

---

## Logo

| File | Description |
|------|-------------|
| `logo/hello.logo` | Hello World — PRINT and basic FORWARD/RIGHT |
| `logo/fractal_forest.logo` | Recursive tree fractal using TO/END |
| `logo/galaxy.logo` | Spiral galaxy pattern |
| `logo/mandala.logo` | Symmetric mandala with REPEAT |
| `logo/spirograph.logo` | Spirograph curves using nested REPEAT |

---

## Lua

| File | Description |
|------|-------------|
| `lua/hello.lua` | Hello World — print() |
| `lua/music_theory.lua` | Note frequencies, chord generation |
| `lua/rpg_engine.lua` | Mini RPG combat engine with tables |
| `lua/state_machine.lua` | Generic finite state machine |

---

## Pascal

| File | Description |
|------|-------------|
| `pascal/hello.pas` | Hello World — writeln |
| `pascal/hangman.pas` | Hangman game with string handling |
| `pascal/inventory.pas` | Inventory system with records and files |
| `pascal/number_game.pas` | Guess-the-number with while loop |

---

## PILOT (Programmed Inquiry, Learning Or Teaching)

| File | Description |
|------|-------------|
| `pilot/hello.pilot` | Hello World — T: type command |
| `pilot/history_quiz.pilot` | Multiple-choice history quiz |
| `pilot/typing_tutor.pilot` | Typing practice drills |

---

## Prolog

| File | Description |
|------|-------------|
| `prolog/hello.pl` | Hello World — facts and write/nl |
| `prolog/expert_system.pl` | Simple rule-based diagnosis system |
| `prolog/family_tree.pl` | Family tree with ancestor/descendent queries |
| `prolog/puzzle_solver.pl` | Sudoku-like constraint puzzle |

---

## Python

| File | Description |
|------|-------------|
| `python/hello.py` | Hello World — print() |
| `python/cipher_tools.py` | Caesar, Vigenère, ROT13 ciphers |
| `python/fractal_explorer.py` | Turtle-based fractal drawing |
| `python/task_manager.py` | CLI task manager with persistence |
| `python/text_analyzer.py` | Word frequency, readability metrics |

---

## REXX

| File | Description |
|------|-------------|
| `rexx/hello.rex` | Hello World — say |
| `rexx/sysadmin_toolkit.rexx` | System administration utilities |
| `rexx/text_processor.rexx` | Text manipulation with REXX stem variables |

---

## Scheme

| File | Description |
|------|-------------|
| `scheme/hello.scm` | Hello World — display + newline |
| `scheme/functional_toolkit.scm` | Higher-order functions, currying, Y combinator |
| `scheme/metacircular.scm` | Metacircular evaluator (Scheme in Scheme) |
| `scheme/quiz_game.scm` | Interactive quiz using tail recursion |

---

## Smalltalk

| File | Description |
|------|-------------|
| `smalltalk/hello.st` | Hello World — Transcript show: |
| `smalltalk/bank_system.st` | Bank account class with inheritance |
| `smalltalk/shapes_demo.st` | Shape hierarchy with polymorphism |

---

## SQL (T-SQL / SQL Server 2000 style)

| File | Description |
|------|-------------|
| `sql/hello.sql` | Hello World — SELECT and PRINT |
| `sql/erp_schema.sql` | ERP database schema creation |
| `sql/erp_reports.sql` | Reporting queries on the ERP schema |
| `sql/school_database.sql` | School records with joins and views |

---

## SQR (Structured Query Reporter)

| File | Description |
|------|-------------|
| `sqr/hello.sqr` | Hello World — PRINT with page layout |
| `sqr/employee_directory.sqr` | Employee directory report |
| `sqr/gl_report.sqr` | General ledger summary report |
| `sqr/payroll_report.sqr` | Payroll register with subtotals |

---

## Demo

| File | Description |
|------|-------------|
| `demo/ERP_OVERVIEW.txt` | Overview of the ERP demo programs |

---

*Total: 97 example programs across 24 languages.*
