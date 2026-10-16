# Examples Catalog

This catalog lists the example programs currently shipped with Time Warp Studio.
It is intentionally limited to the six active languages.

## Active Languages

- BASIC
- PILOT
- Logo
- COMAL
- REXX
- Scheme

## BASIC

| File | Description |
| --- | --- |
| `basic/hello.bas` | Hello World and basic output |
| `basic/adventure.bas` | Text adventure control flow |
| `basic/budget_tracker.bas` | Variables and tabular calculations |
| `basic/community_garden.bas` | Garden co-op harvest, sales, and water planning |
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
| `pilot/studio_pathfinder.pilot` | Branching skills diagnostic with hints and coaching |
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
| `logo/community_campus.logo` | Turtle-drawn campus garden and plaza |
| `logo/mandala.logo` | Repeated radial patterns |
| `logo/spirograph.logo` | Spirograph-style curves |
| `logo/turtle_3d.logo` | Perspective-inspired turtle drawing |

## COMAL

| File | Description |
| --- | --- |
| `comal/hello.comal` | Hello World and PRINT formatting |
| `comal/loops.comal` | FOR, WHILE and REPEAT loops |
| `comal/procedures.comal` | PROC declarations and CALL |
| `comal/data_stats.comal` | DIM, DATA/READ and accumulators |
| `comal/number_guess.comal` | INPUT-driven guessing game |
| `comal/turtle_art.comal` | MOVE/TURN turtle drawings |

## REXX

| File | Description |
| --- | --- |
| `rexx/hello.rexx` | SAY output and abuttal concatenation |
| `rexx/loops.rexx` | DO TO/BY, DO WHILE and DO OVER iterations |
| `rexx/strings.rexx` | PARSE and built-in string functions |
| `rexx/subroutines.rexx` | CALL, ARG and RETURN with RESULT |
| `rexx/select_menu.rexx` | SELECT/WHEN, LEAVE and ITERATE |
| `rexx/turtle_spiral.rexx` | Turtle graphics driven by DO loops |

## Scheme

| File | Description |
| --- | --- |
| `scheme/hello.scm` | display and newline output |
| `scheme/recursion.scm` | factorial, Fibonacci and countdown |
| `scheme/lists.scm` | car/cdr, map, filter and foldl |
| `scheme/higher_order.scm` | map/filter/foldl and procedure composition |
| `scheme/closures.scm` | Closures and mutable local state |
| `scheme/turtle_polygons.scm` | Turtle polygons drawn from Scheme |

## FOCAL

| File | Description |
| --- | --- |
| `focal/square.foc` | A square and a polygon, with a FOR loop and a sign branch |

## CHIP-8

| File | Description |
| --- | --- |
| `chip8/font_and_frames.ch8` | A font glyph, a checker pattern and a frame, with a collision |

## APL

| File | Description |
| --- | --- |
| `apl/arrays_and_turtle.apl` | Iota, reduce, reshape and a user-defined function, then a hexagon |

## Validation

The example verifier runs active examples in isolated subprocesses and reports
syntax, execution, and timeout failures. Run it from the repository root with:

```bash
python3 native/tools/example_smoke.py
```
