# Student guide: working through a lesson

A lesson is a short piece of one language, split into **checkpoints**. Each
checkpoint gives you a program with something missing, tells you what the
finished program should do, and can check your work by running it. There are
twenty lessons — two for each language — and sixty checkpoints.

You can work through them at a terminal or in the IDE's **🎓 Lessons** panel.
Both check your work the same way, so you can switch between them freely.

---

## 1. Seeing what there is

```text
$ python3 native/tools/learn.py list
────────────────────────────────────────────────────────────────────
Lessons
────────────────────────────────────────────────────────────────────
    basic_first_steps   BASIC     beginner     First Steps in BASIC
    basic_loops         BASIC     intermediate Loops and Decisions in BASIC
    pilot_talking       PILOT     beginner     Talking with PILOT
    ...
  ✓ smalltalk_classes   SMALLTALK intermediate Classes and Blocks in Smalltalk

2 of 20 lessons complete. `learn.py walk <lesson>` to start one.
```

`✓` means finished, `…` means started, and a blank means not started. You do not
have to type a whole lesson id: the start of one is enough, so
`learn.py walk basic_first` finds `basic_first_steps`.

Only want one language? `learn.py list --lang logo`.

## 2. Working through one

```text
python3 native/tools/learn.py walk logo_shapes
```

The walk opens the current checkpoint, writes its starting program to a file, and
waits. Edit that file in whatever editor you like, then come back and press
Return.

```text
────────────────────────────────────────────────────────────────────
Checkpoint 1 of 3: One line
────────────────────────────────────────────────────────────────────
Draw a single straight line 100 steps long.

Your file: /home/you/.time_warp/lessons/logo_shapes-1.logo

────────────────────────────────────────────────────────────────────
[Enter] check my file   [h] hint   [s] show the answer   [n] next   [q] quit
>
```

- **Enter** — run your program and see whether it does what was asked
- **h** — the next hint, one at a time. Press it again for a stronger one.
- **s** — the worked answer, on purpose
- **n** — move to the next checkpoint without passing this one
- **q** — stop; your file is left exactly as you left it

You never have to start again to come back later. Nothing overwrites a file you
have already begun: `walk` finds your attempt and leaves it alone.

## 3. What a verdict looks like

This is the interesting part, because the studio *ran your program* rather than
comparing it with an answer:

```text
Not yet. The turtle drew 3 pen segment(s); this checkpoint needs exactly 4.
```

```text
Not yet. Expected output:
55

Your program printed:
0
```

```text
Your program reported an error:
❌ [line 1] Unknown Logo command: FROWARD
```

Three different things, and they are worth telling apart:

- **"Your program printed…"** — the program ran and the answer is not right yet.
  Both sides are shown, so you can see what changed.
- **"Your program reported an error"** — the program did not run. That is a
  different problem from a wrong answer, and this message means the first one.
- **"The turtle drew 3 pen segment(s)"** — a drawing checkpoint, where the
  answer is what you drew rather than what you printed.

Nothing prints "passed" unless your program really did what the checkpoint asked
for. A checkpoint checked with no expectation at all would pass everybody, so the
curriculum refuses to contain one.

## 4. When you are stuck

Press **h**. Hints come out one at a time, in order, from a nudge to something
much closer to the answer. The first one is usually enough, and the second one
only helps somebody who has already tried the first.

Hint 1 of 2 · "A square needs a turn after each side."

If you are still stuck, press **s**. It shows the worked answer, and it says so
first. There is no penalty and no record of shame — but a hint is usually a
better use of the next two minutes than an answer is, because the answer tells
you nothing about where your own attempt went wrong.

## 5. Where you are

```text
$ python3 native/tools/learn.py progress
────────────────────────────────────────────────────────────────────
Your progress
────────────────────────────────────────────────────────────────────
  ✓ logo_shapes      3/3 checkpoints
  … basic_first_steps 1/3 checkpoints  (2 hint(s))
```

Your place is saved as you go, so `walk` always opens the checkpoint you were
last on rather than the first one. To go back to the beginning of a lesson:

```text
python3 native/tools/learn.py reset logo_shapes
```

That forgets your progress in the lesson. **Your files are not touched**, so you
can restart with your old attempt still there if you want to compare.

## 6. In the IDE

Open the **🎓 Lessons** tab in the right-hand column. You get the same lessons:

- the **lesson list** at the top, grouped by language
- the **checkpoint list**, with `✓` against the ones you have done
- **Open** — puts the current checkpoint's starting program in a document
- **Check** — runs the program in that document and tells you how it went
- **Hint** and **Answer** — the same as `h` and `s`
- **Restart** — the same as `reset`

The verdict and the checkpoint text appear under the buttons, and the bar at the
bottom shows how many of the twenty lessons you have finished.

A checkpoint's program is never printed for you in the panel, and the worked
answer is not downloaded until you press **Answer**. You are meant to type it.

## 7. Doing it in the right order

The second lesson for each language assumes the first. Nothing stops you doing
them out of order, but if `basic_loops` feels harder than it should, it is because
it expects that you have done `basic_first_steps`.

`learn.py show <lesson>` prints everything about a lesson before you start it:
what it is for, what you will be able to do afterwards, and the description of
every checkpoint.

## 8. Take it to a machine

When a checkpoint passes, the same program runs on the machines the Studio
carries. Press `Ctrl+Alt+W` for the [HB-1](../hb1/README.md): choose the language's
ROM, paste the program on the PROGRAM page and press `F9`. The machine keeps each
ROM's program, so you can leave an answer there and come back to it — and
`SAVE "NAME"` puts it on a 1982 disk that survives a restart.

For something completely different, `Ctrl+Shift+M` opens the
[CHIP-8](../chip8/README.md), which is a computer small enough to understand in
an afternoon. Its [tutorial](../tutorials/chip8.md) starts from the same place
your lessons did: one instruction at a time.

## Nothing here can break your machine

A lesson program runs with a short time limit and is stopped if it does not
finish, so a loop you accidentally made infinite reports itself instead of
hanging. A checkpoint's program must not wait for the keyboard, and one that does
is reported rather than left waiting for somebody who is not there.

---

**Next:** [the teacher guide](teacher-guide.md) · [how a checkpoint is checked](FORMAT.md)
· the [language tutorials](../tutorials/) for the language you are learning
