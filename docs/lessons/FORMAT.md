# Lessons: technical reference

The curriculum, how a checkpoint is checked down to the detail, where progress is
kept, and the protocol the IDE drives it with.

Readers who want to *use* the lessons want the [student guide](student-guide.md)
or the [teacher guide](teacher-guide.md) instead.

---

## 1. The curriculum is data, and where it lives

```text
native/share/
  lessons.json       the curriculum                  ← edit this to change a lesson
native/src/lessons/  the library that reads it:
  curriculum.cpp     loads it, validates it
  verify.cpp         runs a learner's program and judges the checkpoint
  progress.cpp       what a learner has finished
  classroom.cpp      rosters, handouts, marking, bundles
```

`lessons.json` is data rather than code for three reasons, and each one has been
paid for at least once:

- the panels and tools that show the curriculum can be replaced without touching
  it, which is how the same twenty-two lessons are reachable from a terminal, from the
  IDE, and from a printed plan;
- a teacher can author a lesson without editing C++;
- **the whole corpus can be verified in one pass**, which is what
  `time_warp_teach validate` does and what nothing else in the project could do for a
  curriculum expressed as code.

## 2. Schema

```json
{
  "version": 2,
  "lessons": [
    {
      "id": "basic_first_steps",
      "title": "First Steps in BASIC",
      "language": "BASIC",
      "difficulty": "beginner",
      "description": "Print text, store numbers in variables, and do arithmetic.",
      "objectives": ["Write a PRINT statement that displays text"],
      "teacher_notes": "How to teach it, and what learners usually get wrong.",
      "prerequisites": ["some_earlier_lesson"],
      "checkpoints": [ /* see below */ ]
    }
  ]
}
```

### Lesson fields

| Field | Required | Notes |
| ----- | -------- | ----- |
| `id` | yes | Unique across the file. Progress is keyed by it, so two lessons sharing one would share a learner's place in them. `lower_snake_case` by convention. |
| `title` | yes | Shown in the panel and the plans. |
| `language` | yes | A `Language` enum member name: `BASIC`, `PILOT`, `LOGO`, `COMAL`, `REXX`, `SCHEME`, `FOCAL`, `CHIP8`, `APL`, `SMALLTALK`. Hyphens are accepted and folded to underscores. |
| `difficulty` | yes | One of `beginner`, `intermediate`, `advanced`. |
| `description` | no | One sentence: what the lesson is about. |
| `objectives` | no | What the learner will be able to do. Printed in the plan. |
| `teacher_notes` | no | Teacher-facing prose. Printed in the plan. |
| `prerequisites` | no | Lesson ids that must come first. **Refused if they name a lesson that does not exist, or one that is taught later in the file.** |

### Checkpoint fields

| Field | Required | Notes |
| ----- | -------- | ----- |
| `title` | yes | Shown in the checkpoint list. |
| `description` | yes | What the finished program must do. This is the instruction the learner reads. |
| `starter_code` | yes | What the learner begins with. |
| `solution` | yes | The worked answer. **Must pass its own checkpoint** — see §5. |
| `hints` | yes | One or more, ordered from a nudge to nearly the answer. |
| `expect_lines` / `expect_turtle` / `expect_error` | exactly one | See §3. |

Exactly one expectation is required. None means a checkpoint that would pass
everybody; two means one of them is silently ignored. Both are refused when the
curriculum loads rather than discovered in front of a class.

| Extra field | Notes |
| ----------- | ----- |
| `starter_is_broken` | `true` when the starter is *deliberately* a program that fails, because meeting the failure is the lesson. Only then is a starter that reports an error accepted; see §5. |

## 3. The three expectations

### `expect_lines`

```json
"expect_lines": ["1", "2", "3"]
```

The lines the program must print, **exactly, in that order**. Compared as a list,
so printing one line too many is not "close enough".

The comparison ignores the executors' own commentary. Every one of the prefixes
below is a *report about the run* rather than program output, and a checkpoint
about arithmetic must not have to predict them:

```text
✅  ℹ️  🎨  🚀  🐢  📝  💡  📎  ⚠️
```

Two details the extraction gets right on purpose:

- **A newline is appended to any output entry that lacks one** before the whole
  output is split. The executors disagree about whether an entry includes its
  newline, and a whole-program language can return several lines in one entry; a
  language that omitted the newline would otherwise have its first two lines glued
  together and a *correct* program would fail.
- **Interior blank lines are kept** — a program may legitimately print one — while
  the empty tail that a final newline produces is not.

### `expect_turtle`

```json
"expect_turtle": {"segments": 4, "returns_home": true}
```

| Key | Meaning |
| --- | ------- |
| `segments` | Exactly this many pen segments were drawn |
| `min_segments` | At least this many |
| `shapes` | Exactly this many filled shapes were painted |
| `min_shapes` | At least this many |
| `returns_home` | The turtle ended within 0.5 of where it started |

Pen segments and filled shapes are counted separately because **two different
things draw**: the turtle languages lay down pen segments, and CHIP-8 is a 64x32
framebuffer that paints lit pixels as filled rectangles. A CHIP-8 checkpoint that
counted pen segments would count zero however good the program was.

Every failure message names the count it looked at, because "you drew 3 and this
needs 4" is the sentence that tells a learner what to change.

### `expect_error`

```json
"expect_error": "Unknown BASIC command"
```

The fragment the program must **report as an error**. Looked for anywhere in the
output rather than at the start of it, because executors wrap their errors
differently (`[line 10]` here, a language name there) and a lesson about
recognising a failure should not depend on the wrapper.

This is checked **before** the generic "your program reported an error" branch.
That ordering matters: for this checkpoint the failure *is* the answer, and the
generic branch would otherwise swallow every correct response to it. (It did, for
the length of one test.)

## 4. Running a learner's program

`run_lesson_program` builds a fresh `Interpreter` and `TurtleState`, loads the
source, and executes it with:

- **a three-second budget** (`VERIFY_TIME_LIMIT`). A check that hangs is worse
  than a check that fails: a learner cannot tell a program that is thinking from
  one that is stuck, and three seconds is far beyond any checkpoint here.
- **a refusal to be prompted.** A lesson program that waits for keyboard input
  cannot be checked by somebody who is not there, so a pending prompt is appended
  as an error and the checkpoint fails with it. That is a deliberate limitation
  rather than an oversight: it is why no checkpoint asks the learner to write a
  program that reads input.

An error in the learner's program is reported as itself rather than as a failed
checkpoint — "you have a typo on line 30" is a different thing to tell somebody
than "the answer is not right yet".

## 5. What `validate_curriculum` refuses

Run as `time_warp_teach validate`, `time_warp_learn --self-check`, and on every build
by `lessons_tests`, which asks the same library the same question.

For each checkpoint:

1. **Its own solution must pass.** A lesson nobody can finish is invisible to
   every other test in the project.
2. **Its starter must not pass.** A learner handed a program that already works
   has nothing to do. Comparing the two strings does not catch this: a starter can
   differ from the solution by a comment, or by a line the program never reaches,
   and still pass.
3. **Its starter must not report an error**, unless `starter_is_broken` says so.
   A starter that errors puts a broken program in front of a learner before they
   have typed a line.
4. **A stale `starter_is_broken` is refused too.** A flag that has stopped being
   true is worse than no flag: it excuses a starter that has quietly become
   harmless.

Across the file: a checkpoint with no expectation or with two, a checkpoint with
no hints, a starter identical to its solution, a duplicate id, an unknown
language, an unknown difficulty, a prerequisite naming a lesson that does not
exist or that is taught later, and any language in the studio with no lesson.

## 6. Progress

```text
~/.time_warp/progress.json
```

```json
{
  "version": 1,
  "lessons": {
    "basic_first_steps": {"checkpoint": 2, "completed": false, "hints_used": 1}
  }
}
```

- `checkpoint` is the **first unfinished** checkpoint, clamped to the lesson.
- Written on every change rather than on exit: the application can be closed at any
  moment and a lost afternoon is worse than a small write.
- An unreadable file is reported and then ignored rather than raised. Losing the
  file is bad; refusing to start is worse.
- Entries for lessons no longer in the curriculum are dropped on the next save.
- Re-passing an earlier checkpoint does **not** move a learner backwards.

`TIME_WARP_DATA_DIR` relocates all of this. It exists because a test that records
hints must not write into a learner's file — and because it *did*, the hint
counter being cumulative is how the leak was noticed.

## 7. The classroom

```text
~/.time_warp/classroom/            override with `TEACH_TIME_WARP_CLASSROOM`
  roster.json                      students and where their work lives
  assignment.json                  what was set most recently
  presentation.json                {"font_size": 18, "fullscreen": true}
  students/<id>/<lesson>-<n>.<ext> one file per checkpoint
  students/<id>/progress.json      that learner's place in the curriculum
```

- Names are sanitised into directory names, so a student called `../../etc/passwd`
  cannot point the classroom outside itself.
- `distribute` writes starter code and **never overwrites an existing file**.
- `mark` reads the file once and the mark is taken from those bytes. It calls the
  verifier **directly rather than through a `LessonManager`**, so marking writes no
  progress at all — a teacher collecting a class cannot move a learner's place in
  the curriculum, nor their own.
- `unbundle` resolves each member path before writing and refuses anything outside
  the classroom.

## 8. The reference harness protocol

<!-- history -->
The IDE and the two commands drive all of this through `timewarp_lessons`, in
process. What follows is the *reference engine's* harness protocol, recorded for
anyone reading how the port was checked. Nothing drives it any more: the example and
command harnesses spawn `native/tools/check.cpp` (`time_warp_check`), which asks the
same questions of the engines the product ships. The worker and its engine went in
18.0, and the harnesses that drove them are `time_warp_gates` now.
<!-- /history -->

Requests and replies, one JSON object per line:

| Request | Reply | Notes |
| ------- | ----- | ----- |
| `{"t":"lessons"}` | `{"t":"lessons","items":[…],"presentation":{…}}` | The curriculum **without the solutions** |
| `{"t":"lesson-progress"}` | `{"t":"lesson-progress","items":[…],"completed":n,"total":m}` | One row per lesson |
| `{"t":"lesson-hint","lesson":id,"checkpoint":n}` | `{"t":"lesson-hint","text":…,"n":k,"total":t}` | `n` is 1-based |
| `{"t":"lesson-solution","lesson":id,"checkpoint":n}` | `{"t":"lesson-solution","text":…}` | Asked for on purpose |
| `{"t":"lesson-check","lesson":id,"checkpoint":n,"source":…}` | `{"t":"lesson-check","passed":…,"message":…,…}` | Runs the learner's program |
| `{"t":"lesson-reset","lesson":id}` | `{"t":"lesson-reset","ok":true,"lesson":id}` | Names the lesson |

Two decisions worth knowing:

- **The solutions are not in the `lessons` payload.** It is sent to every client
  that opens the panel, and a lesson whose answer travels with its questions has
  been spoiled before it is read. `LessonCheckpoint` has no field for one.
- **A `lesson-check` is refused while a program is running.** Both run the engine,
  so two at once would share module state. The refusal is reported as
  `"a program is running; stop it before checking a lesson"` — the refusal is the
  feature, and it is also the useful thing to be told.

An error reply is `{"t":<kind>,"ok":false,"error":"…"}`, and every one of them
clears the pending flag: a failed request that left it set would lock the panel
out of every request after it.

The C++ side of the panel's tables is `LessonSummary`, `LessonCheckpoint` and
`LessonProgress` in
[`reports.hpp`](../../native/include/timewarp/ui/reports.hpp); the library behind them
is [`timewarp_lessons`](../../native/include/timewarp/lessons/progress.hpp).

## 9. Adding lessons for a new language

1. Add the language to the engine's `Language` enum (`native/src/lang/language.cpp`,
   which carries the display name too) and to `native/src/ui/language.cpp` for the
   IDE's table.
2. Add at least one lesson to `lessons.json`. `time_warp_teach new <id> --language NAME`
   prints a skeleton the loader accepts.
3. Author the checkpoints, then run `time_warp_teach validate` until it is silent.
4. `time_warp_teach plans --write docs/lessons/plans` to refresh the plan documents.
5. `time_warp_learn list` and the panel should both offer it — no further
   registration is needed, because both read the same file.

## 10. Where this is tested

| Gate | What it covers |
| ---- | -------------- |
| `lessons_tests` | The curriculum is sound; the verifier's rules; progress; hint clamping; the validator's refusals; roster, handouts, marking, roll-up, bundles, presentation and path escapes, through the library the IDE and both commands call |
| `ui_layout_tests` (`lesson panel`) | The panel's widgets, its place as the fifth right-column tab, and that it starts empty |
| `time_warp_teach validate` | The same curriculum pass as a standalone command, for an author |
