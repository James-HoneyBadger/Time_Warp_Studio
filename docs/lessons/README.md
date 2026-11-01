# The curriculum

Twenty lessons in ten languages, two per language, with sixty checkpoints. A checkpoint is
met when the learner's program **runs** and does what the lesson asked, so the curriculum is
checked by the engine rather than compared against an answer.

Nothing in the application assumes the curriculum will be used. It is data, shipped beside
the application, and the IDE, `time_warp_learn` and `time_warp_teach` all read the same file.

---

## The twenty lessons

| Language | Beginner | Intermediate |
| --- | --- | --- |
| BASIC | First Steps in BASIC | Loops and Decisions in BASIC |
| APL | Arrays in APL | Defined Functions and Shape in APL |
| Logo | Turtle Shapes in Logo | Logo Procedures |
| PILOT | Talking with PILOT | Labels, Jumps and Subroutines in PILOT |
| FOCAL | First Steps in FOCAL | Branching in FOCAL |
| Scheme | Scheme Basics | Recursion and Lists in Scheme |
| COMAL | COMAL Basics | COMAL Procedures and Decisions |
| REXX | REXX Basics | REXX Loops and Subroutines |
| Smalltalk | Objects and Messages in Smalltalk | Classes and Blocks in Smalltalk |
| Forth | First Words in Forth | Loops, Variables and Recursion in Forth |

The plans — objectives, teacher notes, and both the starting point and the worked answer for
every checkpoint — are generated from the curriculum into
[`plans/`](plans/README.md). Those files are output: `time_warp_teach plans --write
docs/lessons/plans` writes them and CI fails on a diff, so they are never edited by hand.

### How a checkpoint is checked

Not by looking at the source. The learner's program runs in a headless interpreter and its
behaviour is compared with what the checkpoint asked for:

- **the lines it printed**, exactly, ignoring the executors' own commentary, or
- **what it drew** — pen segments, filled shapes, whether the shape closed, or
- **the error it reported**, where recognising the failure is the lesson.

That is why `PRINT 6 * 7` and `PRINT 42` pass the same checkpoint, and why a checkpoint
about drawing cannot be passed by printing a description of a drawing. It is also why the
check runs in the engine: the panel's tick and the teacher's mark are one function's answer
rather than two opinions that can drift apart.

### The curriculum is checked, not trusted

The lessons are data, so nothing exercises them by accident. One command runs every
checkpoint's own solution and every checkpoint's starting code through the engine:

```text
$ ./native/build/time_warp_teach validate
lessons: 20   checkpoints: 60

✅ every solution passes its own checkpoint
✅ no starter already passes, so every checkpoint has work to do
✅ every language has a lesson
```

A lesson whose solution does not pass is a lesson nobody can finish, and a lesson whose
starter already passes has nothing to do. Both are invisible in a file listing and both are
found here. The same pass runs as an ordinary suite, `lessons_tests`, on every build.

### Where progress lives

`~/.time_warp/progress.json`, unless `TIME_WARP_DATA_DIR` says otherwise. The Lessons panel,
`time_warp_learn` and `time_warp_teach report` read the same file.

---

## Working through a lesson

At a terminal:

```bash
./native/build/time_warp_learn list                  # the twenty, in teaching order
./native/build/time_warp_learn show 1.2              # the checkpoint and what it asks
./native/build/time_warp_learn start 1.2             # write the starting program into the editor's folder
./native/build/time_warp_learn check prog.bas        # run it against the checkpoint
./native/build/time_warp_learn hint 1.2              # the next hint, one at a time
```

In the IDE the Lessons panel does the same: the curriculum by language, the current
checkpoint, its hints, **Open** to put the starting program in a document, **Check** to run
what you have, and **Hand In** to write it into the class folder under the checkpoint's
handout name. **Check** runs the document in front of you, so the loop is edit, check, read
the verdict, edit.

A verdict names what failed rather than what it wanted: it reports the lines the program
printed next to the ones the checkpoint asked for, or the drawing it made next to the one
required. Nothing is scored until you hand in, and the hint is a step, not the answer —
**Answer** shows the worked program, and it is there to be read rather than pasted.

A lesson in the IDE checks the document in front, so a lesson is a normal editing session:
the same tabs, the same Run, the same debugger. Nothing here writes outside the data
directory and the folder you handed in from; a lesson cannot break the machine it runs on.

---

## Running a class

`time_warp_teach` is the teacher's command. It works on the same files the Classroom panel
reads and writes, so either may be used, in any order.

### Adding the class

```bash
./native/build/time_warp_teach roster add "Ada Lovelace"
./native/build/time_warp_teach roster add "Grace Hopper"
./native/build/time_warp_teach roster                    # the class list, with ids
./native/build/time_warp_teach roster remove <id>
```

The roster lives under `~/.time_warp/classroom`, which `TIME_WARP_DATA_DIR` moves. The
Classroom panel shows the same list and adds the same way.

### Setting and handing out work

```bash
./native/build/time_warp_teach assign basics.logo        # set the work for the class
./native/build/time_warp_teach distribute                # write each learner's starter file
./native/build/time_warp_teach collect                   # gather what has been handed in
./native/build/time_warp_teach report                    # where each learner has got to
```

`distribute` writes the starting program into each learner's folder under the handout name,
and `collect` reads what came back. Marking is the same check the learner's own **Check**
presses: the program is run and its behaviour compared, so a mark is an engine's answer
rather than a reading of the source.

### Moving a class between machines

```bash
./native/build/time_warp_teach bundle class.zip          # the roster, the work and the marks
./native/build/time_warp_teach unbundle class.zip        # --replace to overwrite
```

A bundle is a zip, so it opens with the tools a school already has. It carries the roster,
the assignments, each learner's handed-in program and the marks, and it is the way a class
moves between a teacher's machine and a classroom one.

### A scheme of work

```bash
./native/build/time_warp_teach list        # the lessons in teaching order
./native/build/time_warp_teach outline     # grouped by language
./native/build/time_warp_teach plans       # one lesson plan per language, on stdout
./native/build/time_warp_teach answers     # the worked answers, for a teacher's eye
```

The scheme of work is two lessons per language, beginner then intermediate. A class that
starts with Logo can do both in one sitting; a class working through BASIC can spend a week
on the pair. Nothing enforces an order beyond the one the curriculum states.

### Teaching from the front

**View > Presentation Mode** (`Shift+F11`, Escape to leave) hides the authoring chrome and
takes the lesson panel and the console to a size a room can read. `time_warp_teach present`
prints the size and window style a projector wants, and `--windowed` asks for a windowed
class rather than a full-screen one. A lesson session exports as Markdown or PDF with its
checkpoints, this session's verdicts, the program and the output, which is what a teacher
keeps after the lesson.

### Writing a new lesson

```bash
./native/build/time_warp_teach new 11.1 --language basic --title "Sorting in BASIC"
./native/build/time_warp_teach validate
```

`new` prints a skeleton the loader accepts. A lesson needs an id, a language, a title, two to
four checkpoints with a starting program and a solution, and a hint per checkpoint;
`validate` refuses anything that does not pass its own check. Then regenerate the plans, or
CI will report the difference:

```bash
./native/build/time_warp_teach plans --write docs/lessons/plans
```

The [lesson format](FORMAT.md) is the file's field-by-field reference, and the panel and both
commands read that one file.

---

## In the IDE

| Where | What |
| --- | --- |
| Lessons panel | The curriculum, the current checkpoint, hints, **Open**, **Check**, **Hand In**, **Answer**, **Restart**, and the progress bar for the language |
| Classroom panel | The roster, the current assignment, one row of marks per learner, **Open Student Folder**, **Refresh**, **Save Bundle…**, **Load Bundle…** |
| View > Presentation Mode | The lesson at a projector's size, chrome hidden |
| File > Export Lesson Session… | This session as Markdown or PDF, with its checkpoints and verdicts |
