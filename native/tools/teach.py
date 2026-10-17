#!/usr/bin/env python3
"""Everything a teacher needs: plans, answer keys, marking, and a class list.

This is the **teacher** tool. It drives the same curriculum and the same verifier
that a learner's own "check" uses, so a mark here and a green tick there can never
disagree.

    python3 native/tools/teach.py validate
    python3 native/tools/teach.py outline
    python3 native/tools/teach.py plans --lang BASIC
    python3 native/tools/teach.py answers logo_shapes
    python3 native/tools/teach.py write-plans docs/lessons/plans

    python3 native/tools/teach.py roster add "Ada Lovelace"
    python3 native/tools/teach.py assign logo_shapes --note "For Tuesday"
    python3 native/tools/teach.py distribute
    python3 native/tools/teach.py collect
    python3 native/tools/teach.py report
    python3 native/tools/teach.py present 24

The first four need no class at all: they are about the curriculum, and they are
what a teacher uses to prepare. The rest use a classroom directory
(``~/.time_warp/classroom``) that can be zipped and moved to another machine.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

# The engine is not installed as a package: it is the `bridge` directory beside
# this one, and a tool run by path has to say so itself.
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "bridge"))

from time_warp.core.config import PROGRESS_JSON  # noqa: E402
from time_warp.core.interpreter import Language  # noqa: E402
from time_warp.features.classroom_mode import (  # noqa: E402
    CLASSROOM_DIR,
    MAX_PRESENTATION_FONT,
    MIN_PRESENTATION_FONT,
    ClassroomError,
    ClassroomMode,
    language_extensions,
    outline,
)
from time_warp.features.lesson_system import (  # noqa: E402
    Lesson,
    LessonError,
    LessonManager,
    LessonStatus,
    load_curriculum,
    validate_curriculum,
)

_TICK = "✅"
_CROSS = "❌"
_INFO = "ℹ️"
_TEACH = "🍎"


def _rule(title: str = "") -> str:
    line = "═" * 72
    return f"{line}\n{title}\n{line}" if title else line


def _expectation(checkpoint: Lesson) -> str:  # type: ignore[valid-type]
    """How a checkpoint is checked, in one line, for a printed plan."""
    if checkpoint.expect_lines is not None:
        joined = (
            " / ".join(checkpoint.expect_lines)
            if checkpoint.expect_lines
            else "(nothing)"
        )
        return f"prints: {joined}"
    if checkpoint.expect_turtle is not None:
        parts = [f"{key}={value}" for key, value in checkpoint.expect_turtle.items()]
        return "draws: " + ", ".join(parts)
    return f"reports an error containing: {checkpoint.expect_error}"


# ---------------------------------------------------------------------------
# The curriculum
# ---------------------------------------------------------------------------


def cmd_validate(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    """Run every solution and every starter through the verifier.

    This is the gate on the curriculum as *data*: nothing else in the project
    executes it, so without this pass a lesson nobody can finish would sit in the
    tree looking correct.
    """
    lessons = (
        load_curriculum(classroom._lessons_path or None)
        if classroom._lessons_path
        else load_curriculum()
    )
    problems = validate_curriculum(lessons, languages=list(Language))
    checkpoints = sum(lesson.checkpoint_count for lesson in lessons)
    print(f"lessons: {len(lessons)}   checkpoints: {checkpoints}")
    print()
    if options.quiet and not problems:
        print(f"{_TICK} sound")
        return 0
    if not problems:
        print(f"{_TICK} every solution passes its own checkpoint")
        print(f"{_TICK} no starter already passes, so every checkpoint has work to do")
        print(f"{_TICK} every language has a lesson")
        return 0
    print(f"{_CROSS} {len(problems)} problem(s):")
    for problem in problems:
        print(f"  · {problem}")
    return 1


def cmd_outline(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    grouped = outline(LessonManager())
    if not grouped:
        print(f"{_CROSS} the curriculum is empty", file=sys.stderr)
        return 1
    print(_rule("Scheme of work"))
    for language, lessons in grouped.items():
        print()
        print(f"{language}")
        for lesson in lessons:
            needs = (
                f"   (after {', '.join(lesson['prerequisites'])})"
                if lesson["prerequisites"]
                else ""
            )
            print(
                f"  · {lesson['title']} — {lesson['difficulty']}, {lesson['checkpoints']} checkpoints{needs}"
            )
            print(f"    {lesson['description']}")
    print()
    print("Print a plan with: teach.py plans --lang <LANGUAGE>")
    return 0


def cmd_plans(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    lessons = LessonManager().list_lessons()
    if options.language:
        language = _language_argument(options.language)
        if language is None:
            return 2
        lessons = [lesson for lesson in lessons if lesson.language is language]
    if not lessons:
        print(f"{_INFO} no lessons for that language")
        return 1

    for lesson in lessons:
        print(_plan_text(lesson))
        print()
    return 0


def _plan_text(lesson: Lesson) -> str:
    """One lesson as a teacher's plan: what it is for, then how it is checked."""
    lines = [
        _rule(f"{_TEACH} {lesson.title}"),
        f"id: {lesson.id}    language: {lesson.language_name}    "
        f"difficulty: {lesson.difficulty}",
        "",
        lesson.description,
    ]
    if lesson.prerequisites:
        lines += ["", "Prerequisites: " + ", ".join(lesson.prerequisites)]
    if lesson.objectives:
        lines += ["", "Learning objectives:"]
        lines += [
            f"  {index}. {text}" for index, text in enumerate(lesson.objectives, 1)
        ]
    if lesson.teacher_notes:
        lines += ["", "Teacher notes:", _wrap(lesson.teacher_notes, 72, "  ")]
    lines += ["", "Checkpoints:"]
    for index, checkpoint in enumerate(lesson.checkpoints, 1):
        lines += [
            f"  {index}. {checkpoint.title}",
            f"     {checkpoint.description}",
            f"     checks — {_expectation(checkpoint)}",
            f"     handout — {lesson.id}-{index}{language_extensions().get(lesson.language.name, '.txt')}",
        ]
    return "\n".join(lines)


def _wrap(text: str, width: int, indent: str = "") -> str:
    """Wrap to *width*, because a plan is read on paper and in a terminal."""
    words = text.split()
    lines: list[str] = []
    current = indent
    for word in words:
        if len(current) + len(word) + 1 > width and current.strip():
            lines.append(current.rstrip())
            current = indent
        current += word + " "
    if current.strip():
        lines.append(current.rstrip())
    return "\n".join(lines)


def cmd_answers(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    manager = LessonManager()
    lessons = manager.list_lessons()
    if not options.all:
        if not options.lesson:
            print(f"{_CROSS} name a lesson, or use --all", file=sys.stderr)
            return 2
        lessons = [manager.find(options.lesson)]

    for lesson in lessons:
        print(_rule(f"{_TEACH} Answer key — {lesson.title}"))
        for index, checkpoint in enumerate(lesson.checkpoints, 1):
            print()
            print(f"{index}. {checkpoint.title}  ({_expectation(checkpoint)})")
            print()
            for line in checkpoint.solution.rstrip("\n").split("\n"):
                print(f"    {line}")
        print()
    return 0


def cmd_write_plans(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    """Write the lesson plans out as documents.

    Generated from the curriculum rather than written beside it: two copies of a
    lesson drift, and the one that drifts is always the one being read.
    """
    directory = Path(options.directory)
    directory.mkdir(parents=True, exist_ok=True)
    lessons = LessonManager().list_lessons()

    by_language: dict[str, list[Lesson]] = {}
    for lesson in lessons:
        by_language.setdefault(lesson.language_name, []).append(lesson)

    written: list[Path] = []
    for language, group in by_language.items():
        path = directory / f"{language.lower().replace('-', '')}.md"
        path.write_text(_plans_markdown(language, group), encoding="utf-8")
        written.append(path)

    index = directory / "README.md"
    index.write_text(_plans_index(by_language), encoding="utf-8")
    written.append(index)

    for path in written:
        print(f"wrote {path}")
    return 0


def _plans_markdown(language: str, lessons: list[Lesson]) -> str:
    lines = [
        f"# {language} lesson plans",
        "",
        f"Two lessons, {sum(lesson.checkpoint_count for lesson in lessons)} checkpoints. "
        "Generated by `python3 native/tools/teach.py write-plans docs/lessons/plans` "
        "from `native/bridge/time_warp/features/lessons.json` — edit the curriculum, "
        "not this file.",
        "",
    ]
    for lesson in lessons:
        if lesson.prerequisites:
            lines += [
                f"> Needs: {', '.join(f'`{item}`' for item in lesson.prerequisites)}",
                "",
            ]
        lines += [
            f"## {_plain(lesson.title)}",
            "",
            f"`{lesson.id}` · {lesson.difficulty} · {lesson.checkpoint_count} checkpoints",
            "",
            _plain(lesson.description),
            "",
        ]
        if lesson.objectives:
            lines += ["### Learning objectives", ""]
            lines += [f"- {_plain(text)}" for text in lesson.objectives]
            lines += [""]
        if lesson.teacher_notes:
            lines += ["### Teacher notes", "", _plain(lesson.teacher_notes), ""]
        lines += ["### Checkpoints", ""]
        for index, checkpoint in enumerate(lesson.checkpoints, 1):
            handout = f"{lesson.id}-{index}{language_extensions().get(lesson.language.name, '.txt')}"
            lines += [
                f"#### {index}. {_plain(checkpoint.title)}",
                "",
                _plain(checkpoint.description),
                "",
                f"Handout file: `{handout}` · checked by {_expectation(checkpoint)}",
                "",
                "Starting point:",
                "",
            ]
            lines += _fenced(checkpoint.starter_code, lesson)
            lines += ["", "Worked answer:", ""]
            lines += _fenced(checkpoint.solution, lesson)
            # A plain list rather than a collapsible block: an HTML `<details>` is
            # not rendered by every Markdown reader, and this project's own lint
            # refuses inline HTML outright.
            lines += ["", "Hints, in the order a learner should see them:", ""]
            lines += [
                f"{position}. {_plain(text)}"
                for position, text in enumerate(checkpoint.hints, 1)
            ]
            lines += [""]
        lines += ["---", ""]
    return "\n".join(lines).rstrip() + "\n"


def _plain(text: str) -> str:
    """Lesson prose, made safe to put in a Markdown document.

    Lesson prose is *about programming languages*, so it contains asterisks,
    underscores, backticks and lines beginning with `+` -- every one of which
    Markdown reads as formatting. `6 * 7` opened an emphasis span that ran to the
    next asterisk in the paragraph, and a hint reading `+ adds its arguments.`
    became a nested list.

    Escaping here rather than in the curriculum keeps two readers honest at once:
    the terminal plans (`teach.py plans`) print the words as written, and the
    generated document renders them as written. Backticks are escaped too -- a
    generator that emits the repository's markdown has no business guessing which
    words were meant to be code.
    """
    escaped = (
        text.replace("\\", "\\\\")
        .replace("*", "\\*")
        .replace("_", "\\_")
        .replace("`", "\\`")
    )
    # A leading marker would start a list or a heading inside whatever it sits in.
    return re.sub(r"^([-+#>])", r"\\\1", escaped, flags=re.MULTILINE)


def _fenced(code: str, lesson: Lesson) -> list[str]:
    fence = _fence_for(lesson.language)
    return [f"```{fence}", code.rstrip("\n"), "```"]


#: The markdown language name for a fence, so a rendered plan highlights. Missing
#: one is not an error: an unlabelled fence still renders.
_FENCES = {
    "BASIC": "basic",
    "PILOT": "text",
    "LOGO": "logo",
    "COMAL": "text",
    "REXX": "rexx",
    "SCHEME": "scheme",
    "APL": "apl",
    "FOCAL": "text",
    "CHIP8": "asm",
    "SMALLTALK": "smalltalk",
}


def _fence_for(language: Language) -> str:
    return _FENCES.get(language.name, "text")


def _plans_index(by_language: dict[str, list[Lesson]]) -> str:
    lines = [
        "# Lesson plans",
        "",
        "One plan per language, generated from the curriculum by",
        "`python3 native/tools/teach.py write-plans docs/lessons/plans`. For the",
        "learner's view of the same material see the tutorials in `docs/tutorials/`,",
        "and for how the material is delivered see `docs/lessons/README.md`.",
        "",
        "| Language | Lessons | Checkpoints | Plan |",
        "| -------- | ------- | ----------- | ---- |",
    ]
    for language, lessons in by_language.items():
        checkpoints = sum(lesson.checkpoint_count for lesson in lessons)
        filename = f"{language.lower().replace('-', '')}.md"
        lines.append(
            f"| {language} | {len(lessons)} | {checkpoints} | [{filename}]({filename}) |"
        )
    lines += [
        "",
        f"**Total: {sum(len(v) for v in by_language.values())} lessons, "
        f"{sum(sum(lesson.checkpoint_count for lesson in v) for v in by_language.values())} checkpoints.**",
        "",
    ]
    return "\n".join(lines)


def cmd_new(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    """Scaffold a lesson.

    The skeleton is built so that it **loads**: an author starting from a shape the
    loader accepts cannot lose an afternoon to a schema mistake. It is printed by
    default and only appended with `--append`, because the shipped curriculum is a
    real file and a scaffolding command should not be able to damage it by
    accident.
    """
    language = _language_argument(options.language)
    if language is None:
        return 2
    lesson_id = options.lesson_id.strip().lower().replace(" ", "_")
    if not lesson_id:
        print(f"{_CROSS} a lesson needs an id", file=sys.stderr)
        return 2

    if not options.append:
        print(
            json.dumps(
                _blank_lesson(lesson_id, language, options.title or lesson_id), indent=2
            )
        )
        print()
        print(
            f"{_INFO} add `--append` to put it in the curriculum, then run `validate`."
        )
        return 0

    from time_warp.features.lesson_system import LESSONS_FILE

    data = json.loads(LESSONS_FILE.read_text(encoding="utf-8"))
    if any(item.get("id") == lesson_id for item in data["lessons"]):
        print(
            f"{_CROSS} there is already a lesson with the id {lesson_id!r}",
            file=sys.stderr,
        )
        return 2
    data["lessons"].append(
        _blank_lesson(lesson_id, language, options.title or lesson_id)
    )
    LESSONS_FILE.write_text(
        json.dumps(data, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
    )
    print(f"{_TICK} appended {lesson_id} to {LESSONS_FILE}")
    print(f"{_INFO} it will not validate until you replace the placeholder checkpoint.")
    return 1


def _blank_lesson(lesson_id: str, language: Language, title: str) -> dict:
    """A lesson skeleton in the shape the loader accepts."""
    return {
        "id": lesson_id,
        "title": title,
        "language": language.name,
        "difficulty": "beginner",
        "description": "What this lesson is about, in a sentence.",
        "objectives": ["One thing the learner will be able to do"],
        "teacher_notes": "How to teach it, and what learners usually get wrong.",
        "prerequisites": [],
        "checkpoints": [
            {
                "title": "First step",
                "description": "What the finished program must do.",
                "starter_code": "",
                "solution": "",
                "hints": ["A first nudge.", "A second, more direct nudge."],
                "expect_lines": [""],
            }
        ],
    }


def _language_argument(value: str) -> Language | None:
    name = value.upper().replace("-", "_")
    if name not in {language.name for language in Language}:
        print(f"{_CROSS} {value!r} is not a language here.", file=sys.stderr)
        print(
            f"   Try: {', '.join(language.name for language in Language)}",
            file=sys.stderr,
        )
        return None
    return Language[name]


# ---------------------------------------------------------------------------
# The class
# ---------------------------------------------------------------------------


def cmd_roster(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    if options.roster_command == "add":
        student = classroom.add_student(options.name, options.student_id or "")
        print(f"{_TICK} added {student.name} as {student.student_id}")
    elif options.roster_command == "remove":
        removed = classroom.remove_student(options.student_id)
        print(
            f"{_TICK} removed {options.student_id}"
            if removed
            else f"{_INFO} no such student"
        )
        if removed:
            print(f"{_INFO} their files are still in {classroom.root / 'students'}")
    rows = classroom.roster.students
    if not rows:
        print(f'{_INFO} the class list is empty. `roster add "Ada Lovelace"`')
        return 0
    print()
    width = max(len(student.student_id) for student in rows)
    for student in rows:
        print(f"  {student.student_id:<{width}}  {student.name}")
    return 0


def cmd_assign(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    manager = LessonManager()
    ids = [manager.find(name).id for name in options.lessons]
    assignment = classroom.assign(ids, options.note or "")
    print(f"{_TICK} assigned {len(ids)} lesson(s): {', '.join(ids)}")
    if assignment.note:
        print(f"{_INFO} note: {assignment.note}")
    print(f"{_INFO} `teach.py distribute` to hand the starter files out")
    return 0


def cmd_distribute(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    written = classroom.distribute(options.lesson or None)
    if not written:
        print(f"{_INFO} nothing to hand out: every file is already there")
        return 0
    for path in written:
        print(f"  {path}")
    print(
        f"{_TICK} handed out {len(written)} file(s) to {len(classroom.roster)} student(s)"
    )
    return 0


def cmd_collect(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    submissions = classroom.collect(options.lesson or None)
    if not submissions:
        print(f"{_INFO} nothing collected: no handed-out files and no work")
        return 1
    for row in classroom.lesson_marks(submissions):
        verdict = _TICK if row["complete"] else "…"
        print(
            f"  {verdict} {row['student']:<12} {row['lesson']:<22} "
            f"{row['passed']}/{row['checkpoints']} checkpoint(s) passed"
        )
        if options.verbose:
            for item in submissions:
                if (
                    item.student_id == row["student"]
                    and item.lesson_id == row["lesson"]
                ):
                    print(f"        {item.filename:<28} {item.summary()}")
    complete = sum(1 for row in classroom.lesson_marks(submissions) if row["complete"])
    print()
    print(f"{complete} complete, {len(submissions)} file(s) collected")
    return 0


def cmd_report(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    """Where each learner has got to, read from their own progress files.

    Distinct from `collect`, which marks handed-in files. Both are printed, and
    they say which they are, because "finished" and "handed in" are different
    questions and a teacher needs to know which one they asked.
    """
    if options.student:
        return _student_report(options, classroom)

    rows = classroom.class_progress()
    if not rows:
        print(f'{_INFO} no students. `teach.py roster add "Ada Lovelace"`')
        return 1
    print(_rule("Class progress"))
    print(f"{'student':<14}{'name':<22}{'started':>8}{'finished':>10}{'hints':>7}")
    for row in rows:
        print(
            f"{row['id']:<14}{row['name']:<22}{row['started']:>8}"
            f"{row['completed']:>10}{row['hints_used']:>7}"
        )
    print()
    print(f"of {rows[0]['lessons']} lessons, read from each learner's progress file")
    return 0


def _student_report(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    matches = classroom.roster.find(options.student)
    if not matches:
        print(f"{_CROSS} no student matching {options.student!r}", file=sys.stderr)
        return 1
    if len(matches) > 1:
        print(
            f"{_CROSS} {options.student!r} matches {len(matches)}: "
            + ", ".join(student.student_id for student in matches),
            file=sys.stderr,
        )
        return 1
    student = matches[0]
    print(_rule(f"{student.name} ({student.student_id})"))
    rows = classroom.progress_for(student.student_id)
    for row in rows:
        if row["status"] == LessonStatus.NOT_STARTED.value:
            continue
        mark = _TICK if row["status"] == LessonStatus.COMPLETED.value else "…"
        hints = f"  ({row['hints_used']} hint(s))" if row["hints_used"] else ""
        print(
            f"  {mark} {row['title']} — {row['checkpoint']}/{row['checkpoints']}{hints}"
        )
    if all(row["status"] == LessonStatus.NOT_STARTED.value for row in rows):
        print("  nothing started yet")
    return 0


def cmd_bundle(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    path = classroom.create_bundle(Path(options.output))
    print(f"{_TICK} wrote {path}")
    print(
        f"{_INFO} {len(classroom.roster)} student(s), assignment, and presentation settings"
    )
    return 0


def cmd_unbundle(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    roster = classroom.import_bundle(Path(options.bundle), replace=options.replace)
    print(f"{_TICK} imported {len(roster)} student(s) into {classroom.root}")
    print(
        f"{_INFO} existing files were {'replaced' if options.replace else 'left alone'}"
    )
    return 0


def cmd_present(options: argparse.Namespace, classroom: ClassroomMode) -> int:
    if options.size is None:
        settings = classroom.presentation
        print(f"font size: {settings.font_size}   fullscreen: {settings.fullscreen}")
        print(f"range: {MIN_PRESENTATION_FONT}..{MAX_PRESENTATION_FONT}")
        return 0
    settings = classroom.set_presentation(options.size, fullscreen=not options.windowed)
    print(
        f"{_TICK} presentation size {settings.font_size}, fullscreen {settings.fullscreen}"
    )
    print(f"{_INFO} the lesson panel in the IDE reads this when you open it")
    return 0


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="teach.py",
        description="Lesson plans, answer keys, marking and a class list for Time Warp Studio.",
        epilog="Lesson names may be abbreviated: `answers logo_sh` finds `logo_shapes`.",
    )
    parser.add_argument(
        "--classroom",
        default=str(CLASSROOM_DIR),
        help=f"where the class's files live (default: {CLASSROOM_DIR})",
    )
    parser.add_argument(
        "--progress",
        default=str(PROGRESS_JSON),
        help=f"a learner's progress file, for `report` without a classroom (default: {PROGRESS_JSON})",
    )
    subparsers = parser.add_subparsers(dest="command")

    validate = subparsers.add_parser("validate", help="check the whole curriculum")
    validate.add_argument(
        "--quiet", action="store_true", help="only say whether it is sound"
    )

    subparsers.add_parser("outline", help="the scheme of work, grouped by language")

    plans = subparsers.add_parser("plans", help="print the lesson plans")
    plans.add_argument("--lang", dest="language", help="only this language")

    answers = subparsers.add_parser("answers", help="the worked answers")
    answers.add_argument("lesson", nargs="?")
    answers.add_argument("--all", action="store_true", help="every lesson")

    write_plans = subparsers.add_parser(
        "write-plans", help="write the plans out as documents"
    )
    write_plans.add_argument("directory")

    new = subparsers.add_parser("new", help="scaffold a new lesson")
    new.add_argument("lesson_id")
    new.add_argument("--language", required=True)
    new.add_argument("--title")
    new.add_argument(
        "--append", action="store_true", help="add it to the curriculum file"
    )

    roster = subparsers.add_parser("roster", help="the class list")
    roster_sub = roster.add_subparsers(dest="roster_command")
    roster_add = roster_sub.add_parser("add")
    roster_add.add_argument("name")
    roster_add.add_argument("--student-id")
    roster_remove = roster_sub.add_parser("remove")
    roster_remove.add_argument("student_id")

    assign = subparsers.add_parser("assign", help="set work for the class")
    assign.add_argument("lessons", nargs="+")
    assign.add_argument("--note")

    distribute = subparsers.add_parser("distribute", help="hand the starter files out")
    distribute.add_argument("--lesson", action="append", help="only these lessons")

    collect = subparsers.add_parser("collect", help="mark what has been handed in")
    collect.add_argument("--lesson", action="append")
    collect.add_argument(
        "-v", "--verbose", action="store_true", help="one line per file"
    )

    report = subparsers.add_parser("report", help="where each learner has got to")
    report.add_argument("--student", help="one learner")

    bundle = subparsers.add_parser("bundle", help="zip the classroom")
    bundle.add_argument("output")

    unbundle = subparsers.add_parser("unbundle", help="unpack a classroom bundle")
    unbundle.add_argument("bundle")
    unbundle.add_argument(
        "--replace", action="store_true", help="overwrite existing files"
    )

    present = subparsers.add_parser("present", help="the size a lesson is projected at")
    present.add_argument("size", nargs="?", type=int)
    present.add_argument("--windowed", action="store_true", help="not fullscreen")

    return parser


COMMANDS = {
    "validate": cmd_validate,
    "outline": cmd_outline,
    "plans": cmd_plans,
    "answers": cmd_answers,
    "write-plans": cmd_write_plans,
    "new": cmd_new,
    "roster": cmd_roster,
    "assign": cmd_assign,
    "distribute": cmd_distribute,
    "collect": cmd_collect,
    "report": cmd_report,
    "bundle": cmd_bundle,
    "unbundle": cmd_unbundle,
    "present": cmd_present,
}


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    options = parser.parse_args(argv)
    if not options.command:
        parser.print_help()
        return 1

    # `report` without a classroom reads a learner's own progress file, so it must
    # be able to run on a machine where no class has ever been set up.
    if options.command == "report" and not options.student:
        root = Path(options.classroom)
        if not root.exists() and Path(options.progress).is_file():
            return _progress_file_report(Path(options.progress))

    try:
        classroom = ClassroomMode(root=Path(options.classroom))
    except (LessonError, OSError) as exc:
        print(f"{_CROSS} the curriculum could not be loaded: {exc}", file=sys.stderr)
        return 2

    try:
        return COMMANDS[options.command](options, classroom)
    except (ClassroomError, LessonError) as exc:
        print(f"{_CROSS} {exc}", file=sys.stderr)
        return 2


def _progress_file_report(path: Path) -> int:
    """A one-learner report from a progress file and nothing else."""
    manager = LessonManager(progress_path=path)
    rows = manager.progress_report()
    started = [row for row in rows if row["status"] != LessonStatus.NOT_STARTED.value]
    print(_rule(f"Progress — {path}"))
    if not started:
        print("  nothing started yet")
        return 0
    for row in started:
        mark = _TICK if row["status"] == LessonStatus.COMPLETED.value else "…"
        print(f"  {mark} {row['title']} — {row['checkpoint']}/{row['checkpoints']}")
    done, total = manager.completed_count()
    print()
    print(f"{done} of {total} lessons complete")
    return 0


if __name__ == "__main__":
    sys.exit(main())
