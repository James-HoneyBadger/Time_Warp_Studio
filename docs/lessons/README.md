# Lessons

Time Warp Studio is a teaching environment, and lessons are the teaching
material: twenty-two short pieces of curriculum, two for each of the eleven languages,
each one a sequence of checkpoints that a learner works through and the studio
**checks by running what they wrote**.

```text
./native/build/time_warp_learn list
./native/build/time_warp_learn start basic_first_steps
```

Or open the **🎓 Lessons** tab in the IDE and pick one from the list.

---

## Two audiences, one curriculum

A lesson is the same object for both audiences, and the difference is how it is
read:

| | Student | Teacher |
| --- | ------- | ------- |
| **Tool** | `native/tools/learn.cpp` | `native/tools/teach.cpp` |
| **In the IDE** | the 🎓 Lessons panel | the same panel, projected |
| **Wants** | the next checkpoint, a hint, a verdict | the plan, the answer key, who is where |
| **Guide** | [student-guide.md](student-guide.md) | [teacher-guide.md](teacher-guide.md) |

## What is here

| File | What it is |
| ---- | ---------- |
| `docs/lessons/README.md` | This page: how the lessons work and where everything is |
| `docs/lessons/student-guide.md` | Working through a lesson, and what the verdicts mean |
| `docs/lessons/teacher-guide.md` | Planning, answer keys, marking, bundles, presentation |
| `docs/lessons/FORMAT.md` | The technical reference: the curriculum file, how a checkpoint is checked, the protocol |
| `docs/lessons/plans/*.md` | The lesson plans, one file per language, generated from the curriculum |

The machine-readable curriculum is [`native/share/lessons.json`](../../native/share/lessons.json),
and the code that loads, checks and records it is
[`timewarp_lessons`](../../native/include/timewarp/lessons/curriculum.hpp) — the
same library the Lessons panel asks, `time_warp_teach` validates and
`time_warp_learn` walks a learner through. There is no second reader: the curriculum
has one implementation, and every client of it is that library.

## The twenty-two lessons

| Language | Beginner | Intermediate |
| -------- | -------- | ------------ |
| BASIC | First Steps in BASIC | Loops and Decisions in BASIC |
| PILOT | Talking with PILOT | Labels, Jumps and Subroutines in PILOT |
| Logo | Turtle Shapes in Logo | Logo Procedures |
| COMAL | COMAL Basics | COMAL Procedures and Decisions |
| REXX | REXX Basics | REXX Loops and Subroutines |
| Scheme | Scheme Basics | Recursion and Lists in Scheme |
| FOCAL | First Steps in FOCAL | Branching in FOCAL |
| APL | Arrays in APL | Defined Functions and Shape in APL |
| CHIP-8 | Drawing with CHIP-8 | Sprites and Pixels in CHIP-8 |
| Smalltalk | Objects and Messages in Smalltalk | Classes and Blocks in Smalltalk |
| Forth | First Words in Forth | Loops, Variables and Recursion in Forth |

Sixty-six checkpoints in all. The plans — objectives, teacher notes, both the
starting point and the worked answer for every checkpoint — are in
[`docs/lessons/plans/`](plans/README.md).

## How a checkpoint is checked

Not by looking at the source. The learner's program is **run** in a headless
interpreter and its behaviour is compared with what the checkpoint asked for:

- **the lines it printed**, exactly, ignoring the executors' own commentary, or
- **what it drew** — pen segments, filled shapes, whether the shape closed, or
- **the error it reported**, where recognising the failure is the lesson.

That is why `PRINT 6 * 7` and `PRINT 42` are the same answer, and why a
checkpoint about drawing cannot be passed by printing about drawing. It is also
why checking lives in the engine: the panel's green tick and the teacher's mark
are the same function's answer rather than two opinions that can drift apart.

## The curriculum is checked, not trusted

The lessons are **data**, so nothing tests them by accident. One command runs
every checkpoint's own solution, and every checkpoint's starting code, through
the engine:

```text
$ ./native/build/time_warp_teach validate
lessons: 20   checkpoints: 60

✅ every solution passes its own checkpoint
✅ no starter already passes, so every checkpoint has work to do
✅ every language has a lesson
```

A lesson whose solution does not pass is a lesson nobody can finish, and a lesson
whose starter already passes has nothing to do. Both are invisible in a file
listing and both are found here. The same pass runs as an ordinary test
(`lessons_tests`) on every build.

## Where a learner's progress lives

In `~/.time_warp/progress.json`, one entry per lesson: the first unfinished
checkpoint, whether the lesson is complete, and how many hints were used. It is
written on every change rather than on exit, because the application can be
closed at any moment.

Set `TIME_WARP_DATA_DIR` to keep that elsewhere — a portable install, or a test
that must not write into somebody's real file.
