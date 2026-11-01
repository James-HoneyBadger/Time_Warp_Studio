# Examples Catalog

This catalog lists the 68 example programs currently shipped with Time Warp Studio.
It covers all ten active languages.

Each language has a **showcase** program — the first row in each section below —
that walks through that language's whole surface: its statements, its data types,
its control flow, the features unique to it, and a drawing at the end. The
smaller examples beside them each teach one idea; the showcases are the tour.

## Active Languages

- BASIC
- APL
- Logo
- PILOT
- FOCAL
- Scheme
- COMAL
- REXX
- Smalltalk
- Forth

## BASIC

| File | Description |
| --- | --- |
| `basic/showcase.bas` | The weather station: strings, arrays, all three loops, procedures, error handling, turtle graphics and sound |
| `basic/star_patrol.bas` | A small game on `SCREEN 13`: a captured sprite ship, a star field, a shot whose hits are found with `POINT`, and a score bar |
| `basic/gradebook.bas` | A class report: two parallel arrays, a ranked table built with `FORMAT$`, and a bar chart drawn with `LINE ... BF` |
| `basic/hello.bas` | Hello World and basic output |
| `basic/adventure.bas` | Text adventure control flow |
| `basic/budget_tracker.bas` | Variables and tabular calculations |
| `basic/community_garden.bas` | Garden co-op harvest, sales, and water planning |
| `basic/math_explorer.bas` | Interactive arithmetic exploration |
| `basic/maze_generator.bas` | Maze generation and branching |
| `basic/space_simulation.bas` | Simulation-style loops and state |
| `basic/statistics.bas` | Basic statistics calculations |
| `basic/observatory.bas` | Five orbital radii read from DATA and drawn as concentric paths |

## PILOT

| File | Description |
| --- | --- |
| `pilot/showcase.pilot` | The night sky: text, answers, matching, compute, branches, subroutines, hints and turtle graphics |
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
| `logo/showcase.logo` | The cartographer's turtle: words, lists, procedures, recursion, conditionals and pen control |
| `logo/hello.logo` | Introductory turtle drawing |
| `logo/fractal_forest.logo` | Recursive turtle composition |
| `logo/fractal_gallery.logo` | Collection of recursive patterns |
| `logo/galaxy.logo` | Layered radial turtle art |
| `logo/geometric_artistry.logo` | Geometric shape studies |
| `logo/community_campus.logo` | Turtle-drawn campus garden and plaza |
| `logo/mandala.logo` | Repeated radial patterns |
| `logo/spirograph.logo` | Spirograph-style curves |
| `logo/turtle_3d.logo` | Perspective-inspired turtle drawing |
| `logo/constellation.logo` | A twelve-ray compass rose built from a reusable turtle procedure |

## COMAL

| File | Description |
| --- | --- |
| `comal/results_table.comal` | A class set of marks: DATA and READ, an exchange sort, PROC and FUNC with parameters, a ranked table with a histogram and a bar chart the turtle draws |
| `comal/showcase.comal` | The marks board: output, selection, FOR (with STEP), WHILE and REPEAT loops, procedures, functions, arrays and turtle graphics |
| `comal/hello.comal` | Hello World and PRINT formatting |
| `comal/loops.comal` | FOR, WHILE and REPEAT loops |
| `comal/procedures.comal` | PROC declarations and CALL |
| `comal/data_stats.comal` | DIM, DATA/READ and accumulators |
| `comal/number_guess.comal` | INPUT-driven guessing game |
| `comal/turtle_art.comal` | MOVE/TURN turtle drawings |

## REXX

| File | Description |
| --- | --- |
| `rexx/log_analysis.rexx` | A web log triaged: PARSE VAR, an `ELSE IF` chain, padded columns via `LEFT`/`RIGHT`, internal subroutines reading `ARG(n)`, and a verdict rather than just figures |
| `rexx/showcase.rexx` | The text analyst: typeless values, PARSE, DO TO/BY, DO OVER and DO WHILE, functions, subroutines and turtle graphics |
| `rexx/hello.rexx` | SAY output and abuttal concatenation |
| `rexx/loops.rexx` | DO TO/BY, DO WHILE and DO OVER iterations |
| `rexx/strings.rexx` | PARSE and built-in string functions |
| `rexx/subroutines.rexx` | CALL, ARG and RETURN with RESULT |
| `rexx/select_menu.rexx` | SELECT/WHEN, LEAVE and ITERATE |
| `rexx/turtle_spiral.rexx` | Turtle graphics driven by DO loops |

## Scheme

| File | Description |
| --- | --- |
| `scheme/showcase.scm` | The expression machine: numbers, strings, recursion, lists, closures and turtle graphics |
| `scheme/hello.scm` | display and newline output |
| `scheme/recursion.scm` | factorial, Fibonacci and countdown |
| `scheme/lists.scm` | car/cdr, map, filter and foldl |
| `scheme/higher_order.scm` | map/filter/foldl and procedure composition |
| `scheme/closures.scm` | Closures and mutable local state |
| `scheme/turtle_polygons.scm` | Turtle polygons drawn from Scheme |

## FOCAL

| File | Description |
| --- | --- |
| `focal/numerics.foc` | Newton's method and a summed series, a formatted table, and the same table drawn as columns |
| `focal/showcase.foc` | The numbers desk: formatting, the function library, sign branches, FOR, DO groups and turtle graphics |
| `focal/square.foc` | A square and a polygon, with a FOR loop and a sign branch |

## APL

| File | Description |
| --- | --- |
| `apl/showcase.apl` | The array workshop: every function family, right-to-left evaluation, a defined function and turtle geometry |
| `apl/arrays_and_turtle.apl` | Iota, reduce, reshape and a user-defined function, then a hexagon |
| `apl/sensor_readings.apl` | A week of readings: reductions for the summary figures, `⍋` for the ranking, a labelled report built with `⍕`, and a drawing whose size the mean decides |

## Smalltalk

| File | Description |
| --- | --- |
| `smalltalk/ledger.st` | An Account class that refuses what it cannot pay: instance state, a class-side constructor, blocks, and a report built with the collection protocol |
| `smalltalk/showcase.st` | The shape factory: classes, inheritance and super, blocks, collections and turtle graphics |
| `smalltalk/shapes.st` | A Polygon class, nested figures drawn by a block, and Transcript output |

## Forth

| File | Description |
| --- | --- |
| `forth/showcase.fs` | The stack workshop: the stack words, definitions, control structures, variables and memory, recursion, and three figures in three colours |
| `forth/hello.fs` | The first program: `.` and `CR`, a recursive `FACT`, a counted loop and `EMIT` |
| `forth/turtle.fs` | Nested squares, a pentagon and a star, each in its own palette colour |

## Validation

The example gate runs every program through the engines the IDE ships
(`time_warp_check`) and reports syntax and execution failures, and fails when a
program listed as drawing produces no ink. Run it from the repository root with:

```bash
make smoke
```
