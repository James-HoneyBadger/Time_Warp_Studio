# Examples

The 77 example programs shipped with Time Warp Studio, across the ten languages,
and the programs that ship for the three machines beside it: eighteen CHIP-8
listings and nine HB-2 assembly programs.

Each language has a **showcase** — the first row of its section — that exercises the
language's whole surface: its statements, its data types, its control flow, the
features particular to it, and a drawing at the end. The smaller programs beside it
each demonstrate one idea.

## Languages

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
| `hb1/tube_writer.bas` | The screen is memory: characters and colours written with `POKE` into `$8000`/`$9000`, read back with `PEEK`, and a turtle frame round the result |
| `hb1/ram_probe.bas` | The 32K below the screen: a pattern written across it, three cells read back, and one bar drawn per byte |
| `hb1/frame_clock.bas` | Both clocks: the frame counter in registers `$A006`/`$A007`, and `ON TIMER` interrupting a loop, with one spoke per interrupt |
| `hb1/colour_chart.bas` | The phosphor ramp: sixteen steps of brightness written into the attribute byte, as ink, as paper, and as a strip |
| `hb1/sound_board.bas` | The machine's voice, asked for on purpose: eight notes as `SOUND frequency, ticks`, with a block poked into the tube for each |
| `hb1/memory_map.bas` | The map, read rather than believed: four `PEEK`s from four regions, the hole that answers zero, and the ROM that refuses a write |
| `hb1/canvas_lines.bas` | The other surface: axes, eight filled bars and two circles on the 320 x 200 canvas, with the tube narrating beside it |
| `hb1/sprite_lab.bas` | A sprite made out of nothing: drawn with `PSET`, taken with `GET`, and `PUT` a hundred and twenty times across the frame |

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
| `hb1/turtle_garden.logo` | The HB-1 in another ROM: a house, a sun and two flowers, in Logo |

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

## The machines

Three programs ship beside the Studio, each an application of its own. Their
programs are not written in the ten languages and are run by their machines'
own gates rather than by `make smoke`.

### The HB-1

| File | Description |
| --- | --- |
| `hb1/HBWARP.dsk` | The HB DOS volume the machine ships with: three saved programs, named in the catalogue |

The nine HB-1 programs are in the BASIC and Logo tables above, because they are
BASIC and Logo programs. They run under `make smoke` like any other example; the
disk image is data, not a program.

### The HB-2

| File | Description |
| --- | --- |
| `hb2/palette.s` | 640 x 480 x 16, sixteen bars, and the DAC rewritten under them |
| `hb2/blitter.s` | A frame, a fan of lines, and an XOR bar that erases itself |
| `hb2/sprite.s` | A 16 x 16 mask loaded into the bank, two sprites, and a collision |
| `hb2/hello.s` | The 1982 text map written by hand, and the 1992 print call |
| `hb2/chord.s` | Four channels, a waveform the program writes, and a meter |
| `hb2/desktop.s` | Overlapping windows and a task bar: what a framebuffer is for |
| `hb2/tape.s` | A file recorded, the head rewound and the file played back and printed |
| `hb2/task_a.s`, `hb2/task_b.s` | Two processes that never halt, run together with `time_warp_gates hb --tasks` |

### CHIP-8

| File | Description |
| --- | --- |
| `chip8/tour.ch8` | Five pages, each one behaviour: the font, XOR and collision, carry, wrapping, the in-place shift |
| `chip8/showcase.ch8` | The montage: hi-res, the font, sprites, collision, BCD, arithmetic and the keypad |
| `chip8/hires.ch8` | SuperChip hi-res, scrolling and the large `FX30` font |
| `chip8/hires_rings.ch8` | One 16 x 16 `DXY0` sprite drawn at three places on a 128 x 64 screen |
| `chip8/paint.ch8` | A one-pixel paint program driven by the keypad |
| `chip8/pattern_loom.ch8` | XOR weaving: the same sprite drawn twice inverts what it covered |
| `chip8/waveform.ch8` | A plotted curve read from a data table with `LD Vx, [I]` |
| `chip8/collision_lab.ch8` | Draw, erase, and an overlap that leaves only the edges, with `VF` |
| `chip8/font_ledger.ch8` | `LD B, Vx` and `FX29`: 147 becomes three font digits on the screen |
| `chip8/font_and_frames.ch8` | The built-in digit font, drawn and tested |
| `chip8/keypad_probe.ch8` | `FX0A`, waiting for a key |
| `chip8/rewind_lab.ch8` | Three sprites placed for a breakpoint-and-rewind exercise |
| `chip8/bounce.ch8` | A comet that bounces off the four edges and leaves its trail behind |
| `chip8/catch.ch8` | A small arcade game: drops fall, a paddle catches them |
| `chip8/countdown.ch8` | Nine to zero, once a second, with a bar that drains as it goes |
| `chip8/maze.ch8` | A box with walls in it and a dot you walk around them |
| `chip8/spinner.ch8` | Four shapes turning in place, twelve frames apart |
| `chip8/typewriter.ch8` | Type on the keypad and the machine writes it down |

## Validation

The example gate runs every program through the engines the IDE ships
(`time_warp_check`), reports syntax and execution failures, and fails when a program
listed as drawing produces no ink. From the repository root:

```bash
make smoke     # 94 of the 104 programs
```

`basic/star_patrol.bas` is the one language example smoke leaves out: it uses what the
native port added and the reference never had, so `lang_tests` runs it and asserts that it
draws, which is the same gate on the other side of the port. The nine HB-2 programs are
assembled and run by `hb2_tests`, which drives its own machine.

The same programs are what **File > Examples Gallery…** shows, each with the drawing it
makes as its face, because the thumbnail is the program run headlessly rather than a picture
of it.
