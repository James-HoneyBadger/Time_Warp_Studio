# Examples Catalog

This catalog lists the example programs currently shipped with Time Warp Studio.
It is intentionally limited to the nine active languages.

## Active Languages

- BASIC
- PILOT
- Logo
- C
- Pascal
- Prolog
- Forth
- Brainfuck
- Python

## BASIC

| File | Description |
| --- | --- |
| `basic/hello.bas` | Hello World and basic output |
| `basic/adventure.bas` | Text adventure control flow |
| `basic/budget_tracker.bas` | Variables and tabular calculations |
| `basic/math_explorer.bas` | Interactive arithmetic exploration |
| `basic/maze_generator.bas` | Maze generation and branching |
| `basic/space_simulation.bas` | Simulation-style loops and state |
| `basic/statistics.bas` | Basic statistics calculations |

## PILOT

| File | Description |
| --- | --- |
| `pilot/hello.pilot` | Hello World and text output |
| `pilot/brain_trainer.pilot` | Quiz with scoring and branching |
| `pilot/history_quiz.pilot` | Multiple-choice history quiz |
| `pilot/math_drill.pilot` | Arithmetic practice drill |
| `pilot/number_game.pilot` | Number guessing game |
| `pilot/science_quiz.pilot` | Science quiz with scoring |
| `pilot/typing_tutor.pilot` | Typing practice drills |
| `pilot/vocabulary_trainer.pilot` | Vocabulary practice and hints |

## Logo

| File | Description |
| --- | --- |
| `logo/hello.logo` | Introductory turtle drawing |
| `logo/fractal_forest.logo` | Recursive turtle composition |
| `logo/fractal_gallery.logo` | Collection of recursive patterns |
| `logo/galaxy.logo` | Layered radial turtle art |
| `logo/geometric_artistry.logo` | Geometric shape studies |
| `logo/mandala.logo` | Repeated radial patterns |
| `logo/spirograph.logo` | Spirograph-style curves |
| `logo/turtle_3d.logo` | Perspective-inspired turtle drawing |

## C

| File | Description |
| --- | --- |
| `c/hello.c` | Hello World and standard output |
| `c/algorithms_showcase.c` | Algorithm demonstrations |
| `c/game_of_life.c` | Conway's Game of Life |
| `c/linked_list.c` | Linked-list data structures |
| `c/matrix_calculator.c` | Matrix operations |
| `c/rpn_calc.c` | Reverse Polish notation calculator |
| `c/sorting_algorithms.c` | Sorting algorithm comparisons |
| `c/string_utils.c` | String-processing utilities |

## Pascal

| File | Description |
| --- | --- |
| `pascal/hello.pas` | Structured Hello World |
| `pascal/cipher_lab.pas` | Text transformation and ciphers |
| `pascal/grade_book.pas` | Structured records and calculations |
| `pascal/hangman.pas` | Console game and control flow |
| `pascal/inventory.pas` | Inventory data processing |
| `pascal/linked_list.pas` | Linked-list structures |
| `pascal/number_game.pas` | Number guessing game |
| `pascal/sorting.pas` | Sorting algorithms |

## Prolog

| File | Description |
| --- | --- |
| `prolog/hello.pl` | Facts, queries, and output |
| `prolog/hello.pro` | Prolog Hello World variant |
| `prolog/expert_system.pl` | Rule-based diagnosis system |
| `prolog/family_tree.pl` | Family and ancestor queries |
| `prolog/family_genealogy.pl` | Multi-generation relationship inference |
| `prolog/knowledge_engine.pl` | Knowledge-base reasoning showcase |
| `prolog/list_operations.pl` | List predicates and transformations |
| `prolog/puzzle_solver.pl` | Constraint-style puzzle solving |
| `prolog/sorting.pl` | Sorting with recursive predicates |

## Forth

| File | Description |
| --- | --- |
| `forth/hello.f` | Words, output, and stack basics |
| `forth/cellular_automata.forth` | Cellular automata experiment |
| `forth/fibonacci.forth` | Fibonacci calculations |
| `forth/mathematical_wonders.forth` | Mathematical word definitions |
| `forth/rpn_calculator.forth` | Stack-based calculator |
| `forth/sorting.forth` | Sorting with stack operations |
| `forth/string_processing.forth` | String-processing words |
| `forth/turtle_art.forth` | Turtle graphics from Forth |

## Brainfuck

| File | Description |
| --- | --- |
| `brainfuck/hello.bf` | Classic Brainfuck Hello World |
| `brainfuck/calculator.bf` | Arithmetic demonstration |
| `brainfuck/cat.bf` | Input/output loop |
| `brainfuck/countdown.bf` | Counter program |
| `brainfuck/fibonacci.bf` | Fibonacci-style output |
| `brainfuck/multiplication_table.bf` | Multiplication table experiment |
| `brainfuck/programs.bf` | Brainfuck program collection |
| `brainfuck/rot13.bf` | ROT13 transformation |

## Python

| File | Description |
| --- | --- |
| `python/hello.py` | Hello World and basic operations |
| `python/algorithms.py` | Sorting, searching, and prime algorithms |
| `python/data_structures.py` | Lists, dictionaries, sets, stacks, and queues |
| `python/decorators.py` | Function and class decorators |
| `python/generators.py` | Generators and itertools |
| `python/list_comprehensions.py` | Comprehension and functional patterns |
| `python/oop_demo.py` | Classes, inheritance, and dataclasses |
| `python/turtle_art.py` | Turtle polygons and color cycling |

## Plugins

| File | Description |
| --- | --- |
| `plugins/hello_plugin/__init__.py` | Example plugin package entry point |
| `plugins/hello_plugin/plugin.py` | Minimal plugin implementation |

## Validation

The example verifier runs active examples in isolated subprocesses and reports
syntax, execution, and timeout failures. Run it from the repository root with:

```bash
PYTHONPATH=Platforms/Python .venv/bin/python test_all_examples.py
```
