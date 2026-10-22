"""Guided lessons: the curriculum, automatic checkpoint checking, saved progress.

A lesson is a short piece of curriculum in one language: a title, what it is for,
and a sequence of **checkpoints**. A checkpoint hands the learner a program with
something missing, describes what the finished program should do, and can check
the result. Checking is not pattern-matching on the source: the learner's program
is **run** in a headless interpreter and its behaviour is compared with the
checkpoint's expectation. That is the only way to be sure `PRINT 6*7` and
`PRINT 42` are the same answer, and it is why the curriculum belongs in the
engine rather than in the editor that displays it.

The curriculum lives in ``lessons.json`` beside this module. It is data, not
code, for three reasons: the panels that show it can be replaced without
touching it, a teacher can author a lesson without editing Python, and the whole
corpus can be checked in one pass -- ``validate_curriculum`` runs every
checkpoint's own solution through the verifier and reports any that would not
pass. A lesson whose solution fails its own check is a bug, and that pass is what
finds it.

Three expectations are possible, and a checkpoint must have exactly one:

``expect_lines``
    The lines the program must print, exactly, in order. Informational lines the
    executors emit alongside output (see ``_STATUS_PREFIXES``) are ignored, so a
    language that announces a definition or reports a stopped machine does not
    fail a checkpoint about arithmetic.

``expect_turtle``
    What must be drawn: pen segments (``segments``, ``min_segments``) and/or
    filled shapes (``shapes``, ``min_shapes``), plus ``returns_home`` for a shape
    that must close. **Both counts exist because two different things draw.** The
    turtle languages lay down pen segments; CHIP-8 is a 64x32 framebuffer and
    paints lit pixels as filled rectangles, so a CHIP-8 checkpoint that counted
    pen segments would count zero however good the program was.

``expect_error``
    A fragment the program must report as an error. Used where recognising the
    failure *is* the lesson.

Progress is kept in the user's data directory (``~/.time_warp/progress.json``),
one entry per lesson, so a learner can leave and come back. It is written on
every change rather than on exit, because the application can be closed at any
moment and a lost afternoon is worse than a small write.
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any, Iterable, Sequence

from ..core.config import PROGRESS_JSON
from ..core.interpreter import Interpreter, Language
from ..core.language_registry import metadata_for
from ..graphics.turtle_state import TurtleState
from ..logging_config import get_logger

logger = get_logger(__name__)

#: The curriculum. Beside this module so it travels with the engine.
# The curriculum is data the application ships, not something the reference
# engine owns: `native/share/lessons.json` is installed beside the snippets and
# read by the C++ lesson checker too, so a learner's verdict and a teacher's mark
# are one file's answer rather than two copies that can disagree.
LESSONS_FILE: Path = (
    Path(__file__).resolve().parents[3] / "share" / "lessons.json"
)

#: How long a learner's program may run while being checked.
#:
#: A check that hangs is worse than a check that fails: a learner cannot tell a
#: program that is thinking from a program that is stuck. Three seconds is far
#: beyond any checkpoint in the curriculum.
VERIFY_TIME_LIMIT = 3.0

#: Prefixes the executors use for commentary rather than program output.
#:
#: Every one of these is a *report about the run* -- a definition acknowledged, a
#: theme changed, a machine stopped -- so a checkpoint that expects the learner's
#: own output must not have to predict them. Keeping the list here rather than in
#: each checkpoint is what makes `expect_lines` say what the learner sees.
_STATUS_PREFIXES = ("✅", "ℹ️", "🎨", "🚀", "🐢", "📝", "💡", "📎", "⚠️")

#: Spellings the curriculum may use for difficulty, easiest first.
DIFFICULTIES = ("beginner", "intermediate", "advanced")


class LessonError(Exception):
    """Raised when the curriculum itself is wrong, or a lesson is unknown."""


class LessonStatus(Enum):
    """How far a learner has got with one lesson."""

    NOT_STARTED = "not_started"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"


@dataclass(frozen=True)
class Checkpoint:
    """One checkable step of a lesson."""

    title: str
    description: str
    starter_code: str
    solution: str
    hints: tuple[str, ...] = ()
    expect_lines: tuple[str, ...] | None = None
    expect_turtle: dict[str, Any] | None = None
    expect_error: str | None = None
    #: True when the starter is *deliberately* a program that fails, because
    #: meeting the failure is the lesson -- a procedure that never stops, a class
    #: asked for a method it does not have. Without this the validator cannot tell
    #: a written decision from an accident, and a starter that errors is normally
    #: a defect: the learner meets a broken program before writing a line.
    starter_is_broken: bool = False

    @property
    def expectation(self) -> str:
        """Which kind of check this checkpoint uses, for the panel and the tools."""
        if self.expect_lines is not None:
            return "lines"
        if self.expect_turtle is not None:
            return "turtle"
        return "error"


@dataclass(frozen=True)
class Lesson:
    """A themed sequence of checkpoints in one language."""

    id: str
    title: str
    language: Language
    difficulty: str
    description: str
    checkpoints: tuple[Checkpoint, ...]
    objectives: tuple[str, ...] = ()
    teacher_notes: str = ""
    prerequisites: tuple[str, ...] = ()

    @property
    def checkpoint_count(self) -> int:
        return len(self.checkpoints)

    @property
    def language_name(self) -> str:
        """The language's display name, as the panels and the documents show it.

        Taken from the registry rather than from the enum member, because the two
        are not the same string: the member is a Python identifier and spells
        CHIP-8 as `CHIP8`. A lesson plan that told a class to write CHIP8 would be
        wrong about the language's name in the one place it is written down.
        """
        return metadata_for(self.language.name).display_name


@dataclass
class VerifyResult:
    """Outcome of running a learner's program against a checkpoint."""

    passed: bool
    message: str
    output: list[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Running a learner's program
# ---------------------------------------------------------------------------


def printed_lines(output: Iterable[str]) -> list[str]:
    """The lines a program printed, ignoring commentary and the trailing newline.

    Each entry of *output* is a logical line, but the executors are not
    consistent about whether they include the newline, and a whole-program
    language can return several lines in one entry. **A newline is appended to
    any entry that lacks one**, so both shapes split the same way; without that,
    a language that omits it would have its first two lines glued together and a
    correct program would fail.

    Interior blank lines are kept -- a program may legitimately print one -- while
    the empty tail that a final newline produces is not.
    """
    blob = "".join(entry if entry.endswith("\n") else entry + "\n" for entry in output)
    lines = blob.split("\n")
    while lines and not lines[-1].strip():
        lines.pop()
    return [
        line.strip()
        for line in lines
        if not line.strip().startswith(_STATUS_PREFIXES)
    ]


def run_lesson_program(source: str, language: Language) -> tuple[list[str], TurtleState]:
    """Run *source* headlessly and return its output and its turtle.

    A lesson program that waits for input cannot be checked without a person
    sitting there, so a pending prompt is reported as a failure rather than
    answered.
    """
    interpreter = Interpreter()
    interpreter.MAX_EXECUTION_TIME = VERIFY_TIME_LIMIT
    turtle = TurtleState()
    interpreter.load_program(source, language=language)
    output = list(interpreter.execute(turtle))
    if interpreter.pending_input:
        output.append("❌ A lesson program must not wait for keyboard input.")
    return output, turtle


def _check_lines(expected: Sequence[str], output: list[str]) -> VerifyResult:
    printed = printed_lines(output)
    if list(expected) == printed:
        return VerifyResult(True, "✅ Checkpoint passed!", output)
    shown = "\n".join(printed) if printed else "(nothing)"
    wanted = "\n".join(expected)
    return VerifyResult(
        False,
        f"Not yet. Expected output:\n{wanted}\n\nYour program printed:\n{shown}",
        output,
    )


def _check_turtle(
    spec: dict[str, Any], turtle: TurtleState, start: tuple[float, float]
) -> VerifyResult:
    """Compare what was drawn with what the checkpoint asked for.

    Pen segments and filled shapes are counted separately, and the checkpoint may
    constrain either or both. Every failure names the count it looked at, because
    "you drew 3 but 4 were expected" is the sentence that tells a learner what to
    change.
    """
    drawn = len(turtle.lines)
    exact = spec.get("segments")
    if exact is not None and drawn != int(exact):
        return VerifyResult(
            False,
            f"Not yet. The turtle drew {drawn} pen segment(s); "
            f"this checkpoint needs exactly {exact}.",
        )
    at_least = spec.get("min_segments")
    if at_least is not None and drawn < int(at_least):
        return VerifyResult(
            False,
            f"Not yet. The turtle drew {drawn} pen segment(s); this checkpoint "
            f"needs at least {at_least}.",
        )

    shapes = len(turtle.shapes)
    exact_shapes = spec.get("shapes")
    if exact_shapes is not None and shapes != int(exact_shapes):
        return VerifyResult(
            False,
            f"Not yet. The program painted {shapes} shape(s); "
            f"this checkpoint needs exactly {exact_shapes}.",
        )
    min_shapes = spec.get("min_shapes")
    if min_shapes is not None and shapes < int(min_shapes):
        return VerifyResult(
            False,
            f"Not yet. The program painted {shapes} shape(s); this checkpoint "
            f"needs at least {min_shapes}.",
        )

    if spec.get("returns_home"):
        gap = math.hypot(turtle.x - start[0], turtle.y - start[1])
        if gap > 0.5:
            return VerifyResult(
                False,
                "Not yet. The shape is not closed: the turtle ended "
                f"{gap:.0f} steps away from where it started.",
            )
    return VerifyResult(True, "✅ Checkpoint passed!", [])


def _check_error(wanted: str, output: list[str]) -> VerifyResult:
    """Decide a checkpoint whose answer is a reported failure.

    The fragment is looked for anywhere in the output rather than at the start of
    it, because executors wrap their errors differently -- `[line 10]` here, a
    language name there -- and a lesson about recognising a failure should not
    depend on the wrapper.
    """
    if any(wanted.lower() in line.lower() for line in output):
        return VerifyResult(True, "✅ Checkpoint passed!", output)
    return VerifyResult(
        False,
        f"Not yet. This checkpoint needs the program to report:\n{wanted}",
        output,
    )


def verify_checkpoint(
    checkpoint: Checkpoint, source: str, language: Language
) -> VerifyResult:
    """Run *source* and decide whether it satisfies *checkpoint*.

    An error in the learner's program is reported as itself rather than as a
    failed checkpoint: "you have a typo on line 30" is a different thing to tell
    somebody than "the answer is not right yet", and a learner stuck on the first
    does not need the second.

    The one exception is a checkpoint whose expectation *is* an error, where the
    failure is the answer. That is checked first, because the branch that reports
    a broken program would otherwise swallow every correct answer to it.
    """
    if not source.strip():
        return VerifyResult(False, "There is nothing to check yet.")

    sentinel = TurtleState()
    start = (sentinel.x, sentinel.y)
    output, turtle = run_lesson_program(source, language)

    if checkpoint.expect_error is not None:
        return _check_error(checkpoint.expect_error, output)

    errors = [line.strip() for line in output if line.lstrip().startswith("❌")]
    if errors:
        return VerifyResult(
            False, f"Your program reported an error:\n{errors[0]}", output
        )

    if checkpoint.expect_lines is not None:
        return _check_lines(checkpoint.expect_lines, output)

    if checkpoint.expect_turtle is not None:
        return _check_turtle(checkpoint.expect_turtle, turtle, start)

    return VerifyResult(False, "This checkpoint has no expectation to check against.")


# ---------------------------------------------------------------------------
# The curriculum
# ---------------------------------------------------------------------------


def _checkpoint_from(raw: dict[str, Any], lesson_id: str) -> Checkpoint:
    """One checkpoint from its JSON form, refusing an ambiguous one.

    A checkpoint with no expectation, or with more than one, cannot be checked --
    and would silently pass or silently fail every learner. It is an error here
    rather than a surprise in front of a class.
    """
    expectations = [
        key for key in ("expect_lines", "expect_turtle", "expect_error") if key in raw
    ]
    if len(expectations) != 1:
        raise LessonError(
            f"lesson {lesson_id!r}: checkpoint {raw.get('title')!r} must have "
            f"exactly one expectation, found {expectations or 'none'}"
        )
    for required in ("title", "description", "starter_code", "solution"):
        if not raw.get(required):
            raise LessonError(
                f"lesson {lesson_id!r}: checkpoint {raw.get('title')!r} has no "
                f"{required}"
            )
    return Checkpoint(
        title=raw["title"],
        description=raw["description"],
        starter_code=raw["starter_code"],
        solution=raw["solution"],
        hints=tuple(raw.get("hints", ())),
        expect_lines=tuple(raw["expect_lines"]) if "expect_lines" in raw else None,
        expect_turtle=raw.get("expect_turtle"),
        expect_error=raw.get("expect_error"),
        starter_is_broken=bool(raw.get("starter_is_broken", False)),
    )


def _lesson_from(raw: dict[str, Any]) -> Lesson:
    lesson_id = raw.get("id", "")
    if not lesson_id:
        raise LessonError("a lesson in the curriculum has no id")
    language_name = str(raw.get("language", "")).upper().replace("-", "_")
    try:
        language = Language[language_name]
    except KeyError as exc:
        raise LessonError(
            f"lesson {lesson_id!r}: unknown language {raw.get('language')!r}"
        ) from exc
    checkpoints = tuple(
        _checkpoint_from(cp, lesson_id) for cp in raw.get("checkpoints", ())
    )
    if not checkpoints:
        raise LessonError(f"lesson {lesson_id!r} has no checkpoints")
    difficulty = str(raw.get("difficulty", "")).lower()
    if difficulty not in DIFFICULTIES:
        raise LessonError(
            f"lesson {lesson_id!r}: difficulty {difficulty!r} is not one of "
            f"{', '.join(DIFFICULTIES)}"
        )
    return Lesson(
        id=lesson_id,
        title=raw["title"],
        language=language,
        difficulty=difficulty,
        description=raw.get("description", ""),
        checkpoints=checkpoints,
        objectives=tuple(raw.get("objectives", ())),
        teacher_notes=raw.get("teacher_notes", ""),
        prerequisites=tuple(raw.get("prerequisites", ())),
    )


def load_curriculum(path: Path = LESSONS_FILE) -> list[Lesson]:
    """Read the curriculum, in the order it should be taught.

    Duplicate ids are refused: progress is keyed by id, so two lessons sharing
    one would share a learner's place in them.
    """
    data = json.loads(Path(path).read_text(encoding="utf-8"))
    lessons: list[Lesson] = []
    seen: set[str] = set()
    for raw in data.get("lessons", ()):
        lesson = _lesson_from(raw)
        if lesson.id in seen:
            raise LessonError(f"two lessons share the id {lesson.id!r}")
        seen.add(lesson.id)
        lessons.append(lesson)
    if not lessons:
        raise LessonError(f"the curriculum at {path} is empty")
    return lessons


def validate_curriculum(
    lessons: Sequence[Lesson],
    *,
    languages: Sequence[Language] | None = None,
    check_solutions: bool = True,
) -> list[str]:
    """Every way the curriculum could be wrong, as sentences.

    The two passes that matter both *run* the lesson's own code:

    1. **Each checkpoint's solution must pass its own checkpoint.** A lesson
       nobody can finish is invisible to every other test in the project -- the
       curriculum is data, and nothing else exercises it.
    2. **Each checkpoint's starter code must NOT pass.** A learner handed a
       program that already works has nothing to do, and would learn that the
       exercise is to press a button. Comparing the two strings would not catch
       it: a starter can differ from the solution by a comment, or by a line the
       program never reaches, and still pass.

    Returns an empty list when the curriculum is sound.
    """
    problems: list[str] = []
    covered = {lesson.language for lesson in lessons}
    for language in languages or ():
        if language not in covered:
            problems.append(f"no lesson covers {language.name}")

    for lesson in lessons:
        for index, checkpoint in enumerate(lesson.checkpoints):
            where = f"{lesson.id}[{index}] {checkpoint.title!r}"
            if checkpoint.expectation == "lines" and not checkpoint.expect_lines:
                problems.append(f"{where}: expect_lines is empty")
            if checkpoint.expectation == "turtle":
                spec = checkpoint.expect_turtle or {}
                counted = ("segments", "min_segments", "shapes", "min_shapes")
                if not any(key in spec for key in counted) and not spec.get("returns_home"):
                    problems.append(f"{where}: expect_turtle says nothing to check")
            if not checkpoint.hints:
                problems.append(f"{where}: has no hints")
            if checkpoint.solution == checkpoint.starter_code:
                problems.append(
                    f"{where}: the starter code already is the solution, so there "
                    "is nothing for the learner to do"
                )
            if not check_solutions:
                continue
            result = verify_checkpoint(checkpoint, checkpoint.solution, lesson.language)
            if not result.passed:
                problems.append(f"{where}: its own solution does not pass -- {result.message}")
            started = verify_checkpoint(checkpoint, checkpoint.starter_code, lesson.language)
            if started.passed:
                problems.append(
                    f"{where}: the starter code already passes, so the learner has "
                    "nothing to do"
                )
            else:
                reports = any(line.lstrip().startswith("❌") for line in started.output)
                if reports and not checkpoint.starter_is_broken:
                    # A starter that *errors* is a different problem from one that
                    # is wrong: the learner meets a broken program before writing a
                    # line, and learns that the starting point is not to be
                    # trusted. It is allowed only where the author has said so.
                    problems.append(
                        f"{where}: the starter code reports an error rather than a "
                        "wrong answer (set starter_is_broken if that is the lesson)"
                    )
                elif checkpoint.starter_is_broken and not reports:
                    # A flag that is no longer true is worse than no flag: it
                    # excuses a starter that has quietly stopped being broken.
                    problems.append(
                        f"{where}: starter_is_broken is set but the starter does "
                        "not report an error"
                    )
    return problems


def lessons_for_language(lessons: Sequence[Lesson], language: Language) -> list[Lesson]:
    return [lesson for lesson in lessons if lesson.language is language]


# ---------------------------------------------------------------------------
# The manager: curriculum plus a learner's place in it
# ---------------------------------------------------------------------------


class LessonManager:
    """Lists lessons, checks checkpoints, and remembers where a learner got to."""

    def __init__(
        self,
        lessons_path: Path = LESSONS_FILE,
        progress_path: Path = PROGRESS_JSON,
    ) -> None:
        self.lessons_path = Path(lessons_path)
        self.progress_path = Path(progress_path)
        self.lessons: list[Lesson] = load_curriculum(self.lessons_path)
        self._by_id: dict[str, Lesson] = {lesson.id: lesson for lesson in self.lessons}
        self._progress: dict[str, dict[str, Any]] = self._load_progress()

    # ---- curriculum ---------------------------------------------------------

    def list_lessons(self, language: Language | None = None) -> list[Lesson]:
        if language is None:
            return list(self.lessons)
        return lessons_for_language(self.lessons, language)

    def lesson(self, lesson_id: str) -> Lesson:
        try:
            return self._by_id[lesson_id]
        except KeyError as exc:
            raise LessonError(f"no lesson with the id {lesson_id!r}") from exc

    def find(self, prefix: str) -> Lesson:
        """A lesson by id, or by the start of one, for typing at a prompt.

        An ambiguous prefix is refused rather than resolved arbitrarily: picking
        one of two lessons for somebody who typed half a name is how a learner
        ends up in the wrong one.
        """
        text = prefix.strip().lower()
        if text in self._by_id:
            return self._by_id[text]
        matches = [lesson for lesson in self.lessons if lesson.id.startswith(text)]
        if len(matches) == 1:
            return matches[0]
        if not matches:
            raise LessonError(f"no lesson starts with {prefix!r}")
        raise LessonError(
            f"{prefix!r} matches {len(matches)} lessons: "
            + ", ".join(lesson.id for lesson in matches)
        )

    # ---- progress -----------------------------------------------------------

    def _load_progress(self) -> dict[str, dict[str, Any]]:
        try:
            data = json.loads(self.progress_path.read_text(encoding="utf-8"))
        except FileNotFoundError:
            return {}
        except (OSError, json.JSONDecodeError) as exc:
            # A learner's progress is worth more than a tidy log, so an
            # unreadable file is reported and then ignored rather than raised:
            # losing the file is bad, but refusing to start is worse.
            logger.warning("Ignoring unreadable lesson progress %s: %s", self.progress_path, exc)
            return {}
        lessons = data.get("lessons", {}) if isinstance(data, dict) else {}
        return {
            key: value
            for key, value in lessons.items()
            if key in self._by_id and isinstance(value, dict)
        }

    def save_progress(self) -> None:
        self.progress_path.parent.mkdir(parents=True, exist_ok=True)
        payload = {"version": 1, "lessons": self._progress}
        self.progress_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")

    def _entry(self, lesson_id: str) -> dict[str, Any]:
        return self._progress.setdefault(
            lesson_id, {"checkpoint": 0, "completed": False, "hints_used": 0}
        )

    def current_index(self, lesson_id: str) -> int:
        """Index of the first unfinished checkpoint, clamped to the lesson."""
        total = self.lesson(lesson_id).checkpoint_count
        index = int(self._progress.get(lesson_id, {}).get("checkpoint", 0))
        return max(0, min(index, total))

    def status(self, lesson_id: str) -> LessonStatus:
        entry = self._progress.get(lesson_id)
        if not entry:
            return LessonStatus.NOT_STARTED
        if entry.get("completed"):
            return LessonStatus.COMPLETED
        return LessonStatus.IN_PROGRESS

    def hints_used(self, lesson_id: str) -> int:
        return int(self._progress.get(lesson_id, {}).get("hints_used", 0))

    def completed_count(self) -> tuple[int, int]:
        """(lessons completed, lessons in the curriculum)."""
        done = sum(
            1 for lesson in self.lessons if self.status(lesson.id) is LessonStatus.COMPLETED
        )
        return done, len(self.lessons)

    def reset(self, lesson_id: str) -> None:
        self.lesson(lesson_id)
        self._progress.pop(lesson_id, None)
        self.save_progress()

    # ---- checkpoint actions -------------------------------------------------

    def hint(self, lesson_id: str, index: int, hint_number: int = 0) -> str:
        """Hint *hint_number* (0-based, clamped to the last), recorded as used.

        Hints are handed out one at a time rather than all at once: the second
        one only helps somebody who has already tried the first.
        """
        checkpoint = self._checkpoint(lesson_id, index)
        if not checkpoint.hints:
            return "This checkpoint has no hints. Read the description again."
        entry = self._entry(lesson_id)
        entry["hints_used"] = entry.get("hints_used", 0) + 1
        self.save_progress()
        return checkpoint.hints[min(max(0, hint_number), len(checkpoint.hints) - 1)]

    def _checkpoint(self, lesson_id: str, index: int) -> Checkpoint:
        lesson = self.lesson(lesson_id)
        if not 0 <= index < lesson.checkpoint_count:
            raise LessonError(
                f"{lesson_id} has {lesson.checkpoint_count} checkpoints; "
                f"there is no checkpoint {index}"
            )
        return lesson.checkpoints[index]

    def verify(self, lesson_id: str, index: int, source: str) -> VerifyResult:
        """Check *source* against a checkpoint, and advance the learner if it passes."""
        lesson = self.lesson(lesson_id)
        checkpoint = self._checkpoint(lesson_id, index)
        result = verify_checkpoint(checkpoint, source, lesson.language)
        if result.passed:
            entry = self._entry(lesson_id)
            if index >= entry.get("checkpoint", 0):
                entry["checkpoint"] = index + 1
            if entry["checkpoint"] >= lesson.checkpoint_count:
                entry["completed"] = True
                result.message = f"🏆 Lesson complete: {lesson.title}"
            self.save_progress()
        return result

    # ---- reporting ----------------------------------------------------------

    def progress_report(self) -> list[dict[str, Any]]:
        """One row per lesson, for the progress panel and the teacher's report."""
        rows: list[dict[str, Any]] = []
        for lesson in self.lessons:
            index = self.current_index(lesson.id)
            status = self.status(lesson.id)
            rows.append(
                {
                    "id": lesson.id,
                    "title": lesson.title,
                    "language": lesson.language_name,
                    "difficulty": lesson.difficulty,
                    "status": status.value,
                    "checkpoint": index,
                    "checkpoints": lesson.checkpoint_count,
                    "hints_used": self.hints_used(lesson.id),
                }
            )
        return rows
