#!/usr/bin/env python3
"""Guided lessons at a prompt: walk a lesson, check your work, get a hint.

This is the **student** tool. The same lessons and the same checking run in the
IDE's lesson panel; this exists so a learner on a machine with no display, or one
who would rather work in their own editor, can do the whole curriculum.

    python3 native/tools/learn.py list
    python3 native/tools/learn.py show basic_first_steps
    python3 native/tools/learn.py walk basic_first_steps
    python3 native/tools/learn.py check basic_first_steps basic_first_steps-1.bas
    python3 native/tools/learn.py progress

`walk` is the one to start with: it opens the current checkpoint, writes the
starter code to a file you can edit, and waits for you to say you are ready to
have it checked.

Where your work goes: a directory of your own, ``~/.time_warp/lessons`` by
default, with one file per checkpoint. Nothing is hidden and nothing is
generated that you cannot open and edit by hand -- the whole point of a lesson is
that you type the answer.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

# The engine is not installed as a package: it is the `bridge` directory beside
# this one, and a tool run by path has to say so itself.
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "bridge"))

from time_warp.core.config import APP_DATA_DIR  # noqa: E402
from time_warp.core.interpreter import Language  # noqa: E402
from time_warp.features.lesson_system import (  # noqa: E402
    Lesson,
    LessonError,
    LessonManager,
    LessonStatus,
    validate_curriculum,
)

#: Where a learner's work-in-progress files live. Beside the progress file, so
#: one directory holds everything the studio keeps for this person.
DEFAULT_WORK_DIR = APP_DATA_DIR / "lessons"

_TICK = "✅"
_CROSS = "❌"
_INFO = "ℹ️"
_STUDENT = "🎓"


def _rule(title: str = "") -> str:
    line = "─" * 68
    return f"{line}\n{title}\n{line}" if title else line


def work_path(work_dir: Path, lesson: Lesson, index: int) -> Path:
    """Where checkpoint *index* of *lesson* is edited.

    The naming matches the teacher's handouts (`<lesson>-<n>.<ext>`), so a file
    that came from a class and a file made here are the same file, and a learner
    who moves between the two does not end up with two copies of their work.
    """
    from time_warp.features.classroom_mode import language_extensions

    extension = language_extensions().get(lesson.language.name, ".txt")
    return Path(work_dir) / f"{lesson.id}-{index + 1}{extension}"


def open_checkpoint(
    manager: LessonManager, lesson: Lesson, index: int, work_dir: Path
) -> tuple[Path, bool]:
    """Prepare a checkpoint for editing. Returns (path, was_created).

    A file that already exists is **never** overwritten: a learner who has partly
    written an answer and comes back later must find their own attempt, not the
    starter again.
    """
    path = work_path(work_dir, lesson, index)
    if path.exists():
        return path, False
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(lesson.checkpoints[index].starter_code, encoding="utf-8")
    return path, True


def print_lesson(lesson: Lesson, manager: LessonManager | None = None) -> None:
    print(_rule(f"{_STUDENT} {lesson.title}"))
    print(f"{lesson.language_name} · {lesson.difficulty} · {lesson.checkpoint_count} checkpoints")
    print()
    print(lesson.description)
    if lesson.objectives:
        print()
        print("What you will be able to do:")
        for objective in lesson.objectives:
            print(f"  • {objective}")
    print()
    for index, checkpoint in enumerate(lesson.checkpoints):
        marker = " "
        if manager is not None:
            done = manager.current_index(lesson.id)
            marker = _TICK if index < done else " "
        print(f"  {marker} {index + 1}. {checkpoint.title}")
    print()


def print_checkpoint(lesson: Lesson, index: int, work: Path) -> None:
    checkpoint = lesson.checkpoints[index]
    print(_rule(f"Checkpoint {index + 1} of {lesson.checkpoint_count}: {checkpoint.title}"))
    print(checkpoint.description)
    print()
    print(f"Your file: {work}")
    print()


def _language_names() -> list[str]:
    return [language.name for language in Language]


# ---------------------------------------------------------------------------
# Commands
# ---------------------------------------------------------------------------


def cmd_list(options: argparse.Namespace, manager: LessonManager) -> int:
    language = None
    if options.language:
        name = options.language.upper().replace("-", "_")
        if name not in _language_names():
            print(f"{_CROSS} {options.language!r} is not a language here.", file=sys.stderr)
            print(f"   Try one of: {', '.join(_language_names())}", file=sys.stderr)
            return 2
        language = Language[name]

    lessons = manager.list_lessons(language)
    if not lessons:
        print(f"{_INFO} no lessons for that language yet")
        return 1

    width = max(len(lesson.id) for lesson in lessons)
    print(_rule("Lessons"))
    for lesson in lessons:
        status = manager.status(lesson.id)
        mark = {LessonStatus.NOT_STARTED: " ", LessonStatus.IN_PROGRESS: "…", LessonStatus.COMPLETED: _TICK}[status]
        print(
            f"  {mark} {lesson.id:<{width}}  {lesson.language_name:<9} "
            f"{lesson.difficulty:<12} {lesson.title}"
        )
    done, total = manager.completed_count()
    print()
    print(f"{done} of {total} lessons complete. `learn.py walk <lesson>` to start one.")
    return 0


def cmd_show(options: argparse.Namespace, manager: LessonManager) -> int:
    lesson = manager.find(options.lesson)
    print_lesson(lesson, manager)
    for index, checkpoint in enumerate(lesson.checkpoints):
        print(_rule(f"{index + 1}. {checkpoint.title}"))
        print(checkpoint.description)
        print()
        print("Starting point:")
        for line in checkpoint.starter_code.rstrip("\n").split("\n"):
            print(f"    {line}")
        print()
    return 0


def cmd_start(options: argparse.Namespace, manager: LessonManager) -> int:
    lesson = manager.find(options.lesson)
    index = options.checkpoint - 1 if options.checkpoint else manager.current_index(lesson.id)
    if index >= lesson.checkpoint_count:
        print(f"{_TICK} {lesson.title} is finished. `learn.py reset {lesson.id}` to do it again.")
        return 0
    work, created = open_checkpoint(manager, lesson, index, Path(options.work_dir))
    print_checkpoint(lesson, index, work)
    print("Starter code written." if created else "Your existing work is here, untouched.")
    print(f"Edit it, then: learn.py check {lesson.id} {work.name}")
    return 0


def cmd_check(options: argparse.Namespace, manager: LessonManager) -> int:
    lesson = manager.find(options.lesson)
    index = options.checkpoint - 1 if options.checkpoint else manager.current_index(lesson.id)
    if index >= lesson.checkpoint_count:
        print(f"{_TICK} {lesson.title} is already finished.")
        return 0

    path = Path(options.file)
    if not path.is_absolute() and not path.exists():
        candidate = Path(options.work_dir) / options.file
        if candidate.exists():
            path = candidate
    if not path.is_file():
        print(f"{_CROSS} no file at {options.file}", file=sys.stderr)
        return 2

    result = manager.verify(lesson.id, index, path.read_text(encoding="utf-8"))
    print(result.message)
    if result.passed:
        print()
        done, total = manager.completed_count()
        if manager.status(lesson.id) is LessonStatus.COMPLETED:
            print(f"{_TICK} That was the last checkpoint. {done} of {total} lessons complete.")
        else:
            print(f"Next: learn.py walk {lesson.id}")
        return 0
    print()
    print(f"{_INFO} `learn.py hint {lesson.id}` if you would like a nudge.")
    return 1


def cmd_hint(options: argparse.Namespace, manager: LessonManager) -> int:
    lesson = manager.find(options.lesson)
    index = options.checkpoint - 1 if options.checkpoint else manager.current_index(lesson.id)
    already = manager.hints_used(lesson.id)
    text = manager.hint(lesson.id, index, already)
    total = len(lesson.checkpoints[index].hints)
    print(f"{_INFO} Hint {min(already + 1, total)} of {total}: {text}")
    return 0


def cmd_solution(options: argparse.Namespace, manager: LessonManager) -> int:
    """Show the answer, on purpose and with a warning.

    A learner who is stuck at midnight needs the answer more than they need to be
    protected from it, so this exists -- but it says what it is first, and it does
    not pretend to have been earned.
    """
    lesson = manager.find(options.lesson)
    index = options.checkpoint - 1 if options.checkpoint else manager.current_index(lesson.id)
    checkpoint = lesson.checkpoints[index]
    print(f"{_INFO} This is the worked answer to checkpoint {index + 1}. Try `hint` first.")
    print()
    print(checkpoint.solution)
    print()
    print(f"Checking it for you gives: {manager.verify(lesson.id, index, checkpoint.solution).message}")
    return 0


def cmd_walk(options: argparse.Namespace, manager: LessonManager) -> int:
    """The interactive walk: open, edit, check, repeat."""
    lesson = manager.find(options.lesson)
    work_dir = Path(options.work_dir)

    while True:
        index = manager.current_index(lesson.id)
        if index >= lesson.checkpoint_count:
            print(f"{_TICK} {lesson.title} is finished.")
            return 0

        work, created = open_checkpoint(manager, lesson, index, work_dir)
        print()
        print_checkpoint(lesson, index, work)
        if created:
            print(f"{_INFO} The starting point is in that file.")
        else:
            print(f"{_INFO} You already have work in that file.")

        action = _ask(lesson, index, manager)
        if action == "quit":
            print(f"{_INFO} Your work is in {work_dir}. Come back with `learn.py walk {lesson.id}`.")
            return 0
        if action == "skip":
            print(f"{_INFO} Skipping checkpoint {index + 1}.")
            _skip(manager, lesson, index)
            continue
        if action == "solution":
            print()
            print(lesson.checkpoints[index].solution)
            continue

        result = manager.verify(lesson.id, index, work.read_text(encoding="utf-8"))
        print()
        print(result.message)
        if result.passed and manager.status(lesson.id) is LessonStatus.COMPLETED:
            print()
            print(f"{_TICK} You have finished {lesson.title}. `learn.py list` for what is next.")
            return 0


def _ask(lesson: Lesson, index: int, manager: LessonManager) -> str:
    """One turn of the walk. Always gives the prompt back on a bare Enter."""
    checkpoint = lesson.checkpoints[index]
    hint_number = 0
    while True:
        print("─" * 68)
        print("[Enter] check my file   [h] hint   [s] show the answer   [n] next   [q] quit")
        try:
            answer = input("> ").strip().lower()
        except (EOFError, KeyboardInterrupt):
            print()
            return "quit"
        if answer in ("", "c", "check"):
            return "check"
        if answer in ("q", "quit", "x"):
            return "quit"
        if answer in ("n", "next", "skip"):
            return "skip"
        if answer in ("s", "solution", "answer"):
            return "solution"
        if answer in ("h", "hint", "?"):
            text = manager.hint(lesson.id, index, hint_number)
            hint_number += 1
            print(f"{_INFO} {text}")
            if hint_number >= len(checkpoint.hints):
                print(f"{_INFO} That was the last hint. `s` shows the answer.")


def _skip(manager: LessonManager, lesson: Lesson, index: int) -> None:
    """Move past a checkpoint without passing it.

    Marks the checkpoint as visited so the walk does not sit on it, because being
    stuck on one exercise and wanting to see the next is a legitimate thing to
    want. The lesson does not count as complete.
    """
    entry = manager._entry(lesson.id)  # noqa: SLF001 - the store is the manager's
    if index >= entry.get("checkpoint", 0):
        entry["checkpoint"] = index + 1
    manager.save_progress()


def cmd_progress(options: argparse.Namespace, manager: LessonManager) -> int:
    rows = manager.progress_report()
    started = [row for row in rows if row["status"] != LessonStatus.NOT_STARTED.value]
    print(_rule("Your progress"))
    if not started:
        print("Nothing started yet. `learn.py list` to see what there is.")
        return 0
    width = max(len(row["id"]) for row in started)
    for row in started:
        mark = _TICK if row["status"] == LessonStatus.COMPLETED.value else "…"
        hints = f"  ({row['hints_used']} hint(s))" if row["hints_used"] else ""
        print(
            f"  {mark} {row['id']:<{width}}  {row['checkpoint']}/{row['checkpoints']} "
            f"checkpoints{hints}"
        )
    done, total = manager.completed_count()
    print()
    print(f"{done} of {total} lessons complete.")
    return 0


def cmd_reset(options: argparse.Namespace, manager: LessonManager) -> int:
    lesson = manager.find(options.lesson)
    manager.reset(lesson.id)
    print(f"{_INFO} {lesson.title} is back to the start. Your files are untouched.")
    return 0


def cmd_languages(options: argparse.Namespace, manager: LessonManager) -> int:
    print(_rule("Languages with lessons"))
    for language in Language:
        lessons = manager.list_lessons(language)
        titles = ", ".join(lesson.title for lesson in lessons)
        print(f"  {language.name:<10} {len(lessons)} lesson(s)  {titles}")
    return 0


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="learn.py",
        description="Work through Time Warp Studio's guided lessons.",
        epilog="Every command takes a lesson id, and the start of one is enough: "
        "`walk basic_first` finds `basic_first_steps`.",
    )
    parser.add_argument(
        "--lang",
        dest="language",
        help="only this language (BASIC, PILOT, LOGO, COMAL, REXX, SCHEME, APL, FOCAL, CHIP8, SMALLTALK)",
    )
    parser.add_argument(
        "--work-dir",
        default=str(DEFAULT_WORK_DIR),
        help=f"where your lesson files are kept (default: {DEFAULT_WORK_DIR})",
    )
    parser.add_argument(
        "--self-check",
        action="store_true",
        help="check that every lesson can actually be finished, then stop",
    )
    subparsers = parser.add_subparsers(dest="command")

    subparsers.add_parser("list", help="the lessons, and where you are in them")
    subparsers.add_parser("languages", help="lessons grouped by language")

    show = subparsers.add_parser("show", help="everything about one lesson")
    show.add_argument("lesson")

    start = subparsers.add_parser("start", help="open the current checkpoint in a file")
    start.add_argument("lesson")
    start.add_argument("--checkpoint", type=int, help="a particular checkpoint (1-based)")
    start.add_argument("--force", action="store_true", help="overwrite an existing file")

    check = subparsers.add_parser("check", help="check a file against the current checkpoint")
    check.add_argument("lesson")
    check.add_argument("file")
    check.add_argument("--checkpoint", type=int, help="a particular checkpoint (1-based)")

    hint = subparsers.add_parser("hint", help="the next hint for the current checkpoint")
    hint.add_argument("lesson")
    hint.add_argument("--checkpoint", type=int)

    solution = subparsers.add_parser("solution", help="show the worked answer")
    solution.add_argument("lesson")
    solution.add_argument("--checkpoint", type=int)

    walk = subparsers.add_parser("walk", help="work through a lesson step by step")
    walk.add_argument("lesson")

    subparsers.add_parser("progress", help="what you have finished")

    reset = subparsers.add_parser("reset", help="start a lesson again")
    reset.add_argument("lesson")

    return parser


COMMANDS = {
    "list": cmd_list,
    "languages": cmd_languages,
    "show": cmd_show,
    "start": cmd_start,
    "check": cmd_check,
    "hint": cmd_hint,
    "solution": cmd_solution,
    "walk": cmd_walk,
    "progress": cmd_progress,
    "reset": cmd_reset,
}


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    options = parser.parse_args(argv)
    if options.self_check:
        return check_curriculum()
    if not options.command:
        parser.print_help()
        return 1

    try:
        manager = LessonManager()
    except (LessonError, OSError) as exc:
        print(f"{_CROSS} the lessons could not be loaded: {exc}", file=sys.stderr)
        return 2

    if options.command == "start" and getattr(options, "force", False):
        lesson = manager.find(options.lesson)
        index = (
            options.checkpoint - 1 if options.checkpoint else manager.current_index(lesson.id)
        )
        work_path(Path(options.work_dir), lesson, index).unlink(missing_ok=True)

    try:
        return COMMANDS[options.command](options, manager)
    except LessonError as exc:
        print(f"{_CROSS} {exc}", file=sys.stderr)
        return 2


def check_curriculum() -> int:
    """Is every lesson finishable? The same pass the teacher tool runs.

    Reachable from here because a learner who finds a checkpoint that cannot be
    finished has found a bug, and should be able to say so with a command rather
    than a description.
    """
    try:
        lessons = LessonManager().list_lessons()
    except (LessonError, OSError) as exc:
        print(f"{_CROSS} the lessons could not be loaded: {exc}", file=sys.stderr)
        return 2
    problems = validate_curriculum(lessons, languages=list(Language))
    checkpoints = sum(lesson.checkpoint_count for lesson in lessons)
    if not problems:
        print(f"{_TICK} all {len(lessons)} lessons and {checkpoints} checkpoints check out")
        return 0
    for problem in problems:
        print(f"{_CROSS} {problem}")
    return 1


if __name__ == "__main__":
    sys.exit(main())
