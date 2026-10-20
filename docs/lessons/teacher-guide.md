# Teacher guide: planning, marking and running a class

Everything a teacher needs is in `native/tools/teach.py`. Half of it needs no
class at all — it is about the curriculum — and half of it is about the people in
front of you.

```text
python3 native/tools/teach.py --help
```

Nothing here is a separate copy of the lessons. The plans are generated from the
curriculum the learners are working from, the marks come from the same verifier a
learner's own **Check** button uses, and the answer keys are the checkpoint
solutions themselves. If a lesson changes, every one of those changes with it.

---

## 1. Is the curriculum sound?

Run this first, and again after editing any lesson:

```text
$ python3 native/tools/teach.py validate
lessons: 20   checkpoints: 60

✅ every solution passes its own checkpoint
✅ no starter already passes, so every checkpoint has work to do
✅ every language has a lesson
```

Three things are being checked, and each one is a lesson nobody could learn from:

- **A solution that does not pass its own checkpoint** is a lesson nobody can
  finish. It is invisible in a file listing, because the curriculum is data and
  nothing else in the project runs it.
- **A starter that already passes** has nothing for the learner to do. Comparing
  the starter with the solution does not catch this: a starter can differ from the
  solution by a comment and still pass.
- **A starter that reports an error** puts a broken program in front of a learner
  before they have typed a line. That is only allowed where the author says so
  (`starter_is_broken`), which is how "make this runaway procedure stop" can be
  taught honestly.

A language with no lesson at all is also reported, which is what stops the panel
quietly offering a language the curriculum does not cover.

## 2. The scheme of work

```text
python3 native/tools/teach.py outline
```

Prints the twenty lessons grouped by language, in teaching order, with each
lesson's difficulty, its checkpoint count and what it needs first. This is the
view to plan a term from.

## 3. Lesson plans

```text
python3 native/tools/teach.py plans --lang BASIC
python3 native/tools/teach.py plans > all-plans.txt
```

Each plan carries what you need to teach it and nothing you do not:

- the lesson's **purpose** in one sentence
- its **prerequisites**
- its **learning objectives**, one per thing the learner should be able to do
- **teacher notes** — how to teach it, and what learners usually get wrong
- every **checkpoint** with the handout filename and how it is checked

The teacher notes are written for the moment something goes wrong in the room.
For example, `smalltalk_classes` records that a definition reports itself, so the
first checkpoint's output begins with `Counter` and `Counter >> value` before the
answer — a teacher who does not expect those lines reads them as a broken lesson.

The same plans are committed as documents, one file per language, in
[`docs/lessons/plans/`](plans/README.md). Regenerate them after editing the
curriculum:

```text
python3 native/tools/teach.py write-plans docs/lessons/plans
```

They are **generated rather than written**, because two copies of a lesson drift
and the copy that drifts is always the one being read.

## 4. Answer keys

```text
python3 native/tools/teach.py answers logo_shapes
python3 native/tools/teach.py answers --all > answers.txt
```

Each checkpoint's **worked answer** with what it is checked against, so you can
see both the code and the criterion in the same place:

```text
1. One line  (draws: segments=1)

    FORWARD 100

2. A square  (draws: segments=4, returns_home=True)

    REPEAT 4 [ FORWARD 100 RIGHT 90 ]
```

These are the solutions the validator already proved. If a learner's answer is
different but the check passed, it is right — the check runs the program rather
than comparing the text, and `FORWARD 50 FORWARD 50` is as good an answer to
checkpoint 1 as `FORWARD 100` is, as long as it draws one line.

## 5. A class

The classroom lives in `~/.time_warp/classroom`, and the layout is plain enough to
read, zip and hand to somebody else:

```text
classroom/
  roster.json                    the students, and where their work lives
  assignment.json                what was set most recently
  presentation.json              font size and fullscreen
  students/
    ada-lovelace/
      logo_shapes-1.logo         work handed out, and handed back
      logo_shapes-2.logo
      logo_shapes-3.logo
      progress.json              that learner's place in the curriculum
```

**One file per checkpoint, not per lesson.** A lesson's checkpoints are separate
exercises that build one skill — three short programs, not one program in three
stages — so the checkpoint number is in the filename and a learner knows which
exercise they are in.

Use `--classroom DIR` to keep a class somewhere else.

### Adding the class

```text
$ python3 native/tools/teach.py roster add "Ada Lovelace"
✅ added Ada Lovelace as ada-lovelace

$ python3 native/tools/teach.py roster add "Grace Hopper"
$ python3 native/tools/teach.py roster
```

The id is derived from the name unless you pass `--student-id`. Removing somebody
from the list **leaves their files alone**: a class list is administration, and it
must not be able to delete an afternoon's work.

### Setting and handing out work

```text
$ python3 native/tools/teach.py assign logo_shapes --note "For Tuesday"
✅ assigned 1 lesson(s): logo_shapes

$ python3 native/tools/teach.py distribute
  /home/you/.time_warp/classroom/students/ada-lovelace/logo_shapes-1.logo
  ...
✅ handed out 6 file(s) to 2 student(s)
```

`distribute` writes out each checkpoint's **starting program**, never the
solution, and **never overwrites work that has already started**. Handing the
sheet out again mid-lesson is the commonest way to lose a learner's afternoon, so
the second run hands out only the files that are not there.

### Taking it in

```text
$ python3 native/tools/teach.py collect -v
  … ada-lovelace logo_shapes            2/3 checkpoint(s) passed
        logo_shapes-1.logo           checkpoint 1: pass
        logo_shapes-2.logo           checkpoint 2: pass
        logo_shapes-3.logo           checkpoint 3: not yet
  … grace-hopper logo_shapes            0/3 checkpoint(s) passed
```

Each file is marked by **running it** against its own checkpoint, so a mark and a
learner's own verdict can never disagree. Marking is a pure read: it writes no
progress at all, which means collecting a class cannot move a learner's place in
the curriculum, and cannot move yours.

The roll-up counts the files that passed rather than the run up to the first gap.
The exercises are independent, so a learner who did 1 and 3 has done two of three,
and hiding the third would send you to re-teach something already learnt.

### Where each learner has got to

```text
$ python3 native/tools/teach.py report
student       name                   started  finished  hints
ada-lovelace  Ada Lovelace                 1         1      2
grace-hopper  Grace Hopper                 0         0      0
```

This reads each learner's own `progress.json`, so it answers **"what have they
done in the studio"**. `collect` answers **"what have they handed in"**. They are
different questions and both commands say which they asked.

One learner in detail: `report --student ada`.

To read a single progress file with no classroom at all — for instance one copied
off a learner's machine — point `--progress` at it:

```text
python3 native/tools/teach.py --progress /media/usb/progress.json report
```

### Moving a class between machines

```text
python3 native/tools/teach.py bundle class.zip
python3 native/tools/teach.py --classroom /path/new unbundle class.zip
```

A bundle is the whole classroom directory zipped: the roster, the assignment, the
presentation settings and every learner's work. Useful on a locked-down network,
or for carrying a class home.

`unbundle` leaves existing files alone unless you pass `--replace`, because the
two cases are genuinely different: setting up a new classroom wants to be
additive, and refreshing one wants to be authoritative. A bundle whose member
names try to point outside the classroom is refused rather than followed.

The Classroom panel has the same pair as **Save Bundle...** and **Load Bundle...**.
They ask the engine, so the file the IDE writes is the one `teach.py unbundle`
reads and the other way round. Loading asks whether to replace files that are
already there, because that is the one answer it cannot take back; the panel then
re-reads the class it has just changed.

## 6. Teaching from the front

```text
$ python3 native/tools/teach.py present 24
✅ presentation size 24, fullscreen True
```

Sets the size a lesson is projected at, in points. The IDE's lesson panel reads it
when it loads the curriculum, so the panel a teacher opens is at the size they chose
rather than at whatever the panel thought was sensible.

`present 24 --windowed` keeps the IDE a window instead of taking the whole screen,
for a machine driving a projector from a desktop that is also in use.

In the IDE, **View > Presentation Mode** (Shift+F11) is the rest of it: the menu,
the toolbars, the function-key row, the status bar and the docks go away, and the
lesson panel and the console take this size. It reads the setting when the mode is
entered, so a teacher who has not opened a panel yet still gets the size they
chose. Escape leaves the mode and puts the chrome back.

`present` with no number reports the current setting.

**File > Record Canvas as GIF...** records the drawing appearing, for a handout
or a class page: it samples the canvas ten times a second and stops asking where
to save when you switch it off. A turtle drawing is usually a few seconds; a long
one is a large file, because every changed frame is a picture.

At the end of a session, **File > Export Lesson Session as Markdown** (or **as
PDF**) writes one file: the lesson and what it teaches, every checkpoint with the
verdict it was given this session, the program in the editor and the console
output. Passed, tried-and-not-passed and never-tried are three different things in
the report, because they are three different things to a teacher; Restart clears
that lesson's verdicts. The Markdown is plain enough to paste into a report or a
class hand-in, and the PDF is the same content on A4.

## 7. Writing a new lesson

```text
python3 native/tools/teach.py new my_topic --language BASIC
```

Prints a lesson skeleton in the shape the **loader accepts**, so an author
starting from it cannot lose an afternoon to a schema mistake. It is printed
rather than written: the shipped curriculum is a real file, and a scaffolding
command should not be able to damage it by accident. `--append` puts it into the
curriculum; after that, `validate` will tell you what is still missing
(the placeholder checkpoint has no expectation yet).

The full schema, and what every field means, is in
[FORMAT.md](FORMAT.md).

## 8. In the IDE

The **🎓 Lessons** panel is the learner's view. A teacher uses it for the same
thing plus the **Open** button: put the checkpoint's starting program on the
projector, talk through it, then **Check** a learner's attempt from their own
document.

The panel deliberately cannot show the whole answer until **Answer** is pressed,
so a projected panel does not spoil the exercise for the room.

Next to it is the **🏫 Classroom** panel, which is the teacher's view: the class
list, what has been assigned and the note that goes with it, and one row per
learner per lesson showing how many checkpoints were handed in and how many
passed. **Open Student Folder** jumps to the selected learner's work. It reads
the same files `teach.py` writes — the panel's marks and `collect`'s are the same
marking code — so a class can be set up from the command line and then watched
from the IDE, or the other way round. **Refresh** re-reads those files, so a
student added or a handout marked while the IDE is open appears without
restarting it. An empty class shows a line saying how to make one rather than an
empty table.

---

**Next:** [the curriculum format](FORMAT.md) · [the generated lesson plans](plans/README.md)
· [the student guide](student-guide.md) · the [language tutorials](../tutorials/)
for a refresher on the language itself
