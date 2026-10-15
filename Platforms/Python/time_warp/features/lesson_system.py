"""Guided lessons: built-in curriculum, automatic checkpoint verification, saved progress.

Lessons live in ``lessons.json`` next to this module. Each checkpoint is checked
by actually running the learner's program in a headless interpreter and
comparing either its printed lines (``expect_lines``) or the turtle drawing
(``expect_turtle``) with the checkpoint's specification.
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path

from ..core.config import PROGRESS_JSON
from ..core.interpreter import Interpreter, Language
from ..graphics.turtle_state import TurtleState
from ..logging_config import get_logger

logger = get_logger(__name__)

LESSONS_FILE = Path(__file__).with_name("lessons.json")
VERIFY_TIME_LIMIT = 3.0
# Informational lines executors emit alongside program output.
_STATUS_PREFIXES = ("✅", "ℹ️", "🎨", "🚀", "🐢", "📝", "💡", "📎", "⚠️")


class LessonStatus(Enum):
    """Learner progress through a lesson."""

    NOT_STARTED = "not_started"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"


@dataclass
class Checkpoint:
    """One verifiable step of a lesson."""

    title: str
    description: str
    starter_code: str
    solution: str
    hints: list[str]
    expect_lines: list[str] | None = None
    expect_turtle: dict | None = None


@dataclass
class Lesson:
    """A themed sequence of checkpoints in one language."""

    id: str
    title: str
    description: str
    language: Language
    difficulty: str
    checkpoints: list[Checkpoint]


@dataclass
class VerifyResult:
    """Outcome of running a learner's program against a checkpoint."""

    passed: bool
    message: str
    output: list[str] = field(default_factory=list)


def run_lesson_program(source: str, language: Language) -> tuple[list[str], TurtleState]:
    """Run *source* headlessly with a short time budget; return output and turtle."""
    interp = Interpreter()
    interp.MAX_EXECUTION_TIME = VERIFY_TIME_LIMIT
    turtle = TurtleState()
    interp.load_program(source, language=language)
    output = list(interp.execute(turtle))
    if interp.pending_input:
        output.append("❌ Lesson programs must not wait for keyboard input")
    return output, turtle


def verify_checkpoint(
    checkpoint: Checkpoint, source: str, language: Language
) -> VerifyResult:
    """Run *source* and decide whether it satisfies *checkpoint*."""
    start_x, start_y = TurtleState().x, TurtleState().y
    output, turtle = run_lesson_program(source, language)

    errors = [line.strip() for line in output if line.lstrip().startswith("❌")]
    if errors:
        return VerifyResult(False, f"Your program reported an error:\n{errors[0]}", output)

    if checkpoint.expect_lines is not None:
        printed = [
            line.strip()
            for line in output
            if line.strip() and not line.strip().startswith(_STATUS_PREFIXES)
        ]
        if printed != checkpoint.expect_lines:
            shown = "\n".join(printed) if printed else "(nothing)"
            wanted = "\n".join(checkpoint.expect_lines)
            return VerifyResult(
                False,
                f"Not yet. Expected output:\n{wanted}\n\nYour program printed:\n{shown}",
                output,
            )

    spec = checkpoint.expect_turtle
    if spec:
        drawn = len(turtle.lines)
        wanted_segments = spec.get("segments")
        if wanted_segments is not None and drawn != wanted_segments:
            return VerifyResult(
                False,
                f"Not yet. The turtle drew {drawn} line(s); "
                f"this checkpoint needs exactly {wanted_segments}.",
                output,
            )
        if spec.get("returns_home"):
            gap = math.hypot(turtle.x - start_x, turtle.y - start_y)
            if gap > 0.5:
                return VerifyResult(
                    False,
                    "Not yet. The shape is not closed: the turtle ended "
                    f"{gap:.0f} steps away from where it started.",
                    output,
                )

    return VerifyResult(True, "✅ Checkpoint passed!", output)


class LessonManager:
    """Loads the curriculum, verifies checkpoints, and persists progress."""

    def __init__(
        self,
        lessons_path: Path = LESSONS_FILE,
        progress_path: Path = PROGRESS_JSON,
    ) -> None:
        self.lessons_path = Path(lessons_path)
        self.progress_path = Path(progress_path)
        self.lessons: dict[str, Lesson] = self._load_lessons()
        self._progress: dict[str, dict] = self._load_progress()

    # ---- curriculum ---------------------------------------------------------

    def _load_lessons(self) -> dict[str, Lesson]:
        data = json.loads(self.lessons_path.read_text(encoding="utf-8"))
        lessons: dict[str, Lesson] = {}
        for raw in data["lessons"]:
            checkpoints = []
            for cp in raw["checkpoints"]:
                if "expect_lines" not in cp and "expect_turtle" not in cp:
                    raise ValueError(
                        f"Lesson {raw['id']!r} checkpoint {cp['title']!r} has no expectation"
                    )
                checkpoints.append(
                    Checkpoint(
                        title=cp["title"],
                        description=cp["description"],
                        starter_code=cp["starter_code"],
                        solution=cp["solution"],
                        hints=list(cp.get("hints", [])),
                        expect_lines=cp.get("expect_lines"),
                        expect_turtle=cp.get("expect_turtle"),
                    )
                )
            lessons[raw["id"]] = Lesson(
                id=raw["id"],
                title=raw["title"],
                description=raw["description"],
                language=Language[raw["language"]],
                difficulty=raw["difficulty"],
                checkpoints=checkpoints,
            )
        return lessons

    def list_lessons(self) -> list[Lesson]:
        return list(self.lessons.values())

    # ---- progress -----------------------------------------------------------

    def _load_progress(self) -> dict[str, dict]:
        try:
            data = json.loads(self.progress_path.read_text(encoding="utf-8"))
        except FileNotFoundError:
            return {}
        except (OSError, json.JSONDecodeError) as exc:
            logger.warning("Ignoring unreadable lesson progress %s: %s", self.progress_path, exc)
            return {}
        lessons = data.get("lessons", {}) if isinstance(data, dict) else {}
        return {k: v for k, v in lessons.items() if k in self.lessons and isinstance(v, dict)}

    def save_progress(self) -> None:
        self.progress_path.parent.mkdir(parents=True, exist_ok=True)
        payload = {"version": 1, "lessons": self._progress}
        self.progress_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")

    def _entry(self, lesson_id: str) -> dict:
        return self._progress.setdefault(
            lesson_id, {"checkpoint": 0, "completed": False, "hints_used": 0}
        )

    def current_index(self, lesson_id: str) -> int:
        """Index of the first unfinished checkpoint (== count when completed)."""
        total = len(self.lessons[lesson_id].checkpoints)
        idx = int(self._progress.get(lesson_id, {}).get("checkpoint", 0))
        return max(0, min(idx, total))

    def status(self, lesson_id: str) -> LessonStatus:
        entry = self._progress.get(lesson_id)
        if not entry:
            return LessonStatus.NOT_STARTED
        if entry.get("completed"):
            return LessonStatus.COMPLETED
        return LessonStatus.IN_PROGRESS

    def hints_used(self, lesson_id: str) -> int:
        return int(self._progress.get(lesson_id, {}).get("hints_used", 0))

    def start(self, lesson_id: str) -> None:
        self._entry(lesson_id)
        self.save_progress()

    def reset(self, lesson_id: str) -> None:
        self._progress.pop(lesson_id, None)
        self.save_progress()

    # ---- checkpoint actions -------------------------------------------------

    def hint(self, lesson_id: str, index: int, hint_number: int) -> str:
        """Return hint *hint_number* (0-based, clamped) and record that it was used."""
        hints = self.lessons[lesson_id].checkpoints[index].hints
        if not hints:
            return "No hints for this checkpoint."
        entry = self._entry(lesson_id)
        entry["hints_used"] = entry.get("hints_used", 0) + 1
        self.save_progress()
        return hints[min(hint_number, len(hints) - 1)]

    def verify(self, lesson_id: str, index: int, source: str) -> VerifyResult:
        """Verify checkpoint *index*; on success unlock the next one."""
        lesson = self.lessons[lesson_id]
        result = verify_checkpoint(lesson.checkpoints[index], source, lesson.language)
        if result.passed:
            entry = self._entry(lesson_id)
            if index >= entry.get("checkpoint", 0):
                entry["checkpoint"] = index + 1
            if entry["checkpoint"] >= len(lesson.checkpoints):
                entry["completed"] = True
                result.message = f"🏆 Lesson complete: {lesson.title}!"
            self.save_progress()
        return result
