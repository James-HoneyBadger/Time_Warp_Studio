"""Guided lessons mode for Time Warp Studio.

Provides step-by-step walkthroughs with checkpoints and auto-verified outputs.
"""

from dataclasses import dataclass
from enum import Enum
from typing import Callable, List, Optional


class LessonStatus(Enum):
    """Lesson completion status."""

    NOT_STARTED = "not_started"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"


@dataclass
class Checkpoint:
    """A single lesson checkpoint."""

    title: str
    description: str
    starter_code: str
    expected_output: str
    hints: List[str]
    solution: str


@dataclass
class Lesson:
    """A complete lesson with multiple checkpoints."""

    id: str
    title: str
    description: str
    language: str  # "basic", "pilot", "logo"
    difficulty: str  # "beginner", "intermediate", "advanced"
    checkpoints: List[Checkpoint]
    status: LessonStatus = LessonStatus.NOT_STARTED
    current_checkpoint: int = 0


class LessonManager:
    """Manages lessons and checkpoint progression."""

    def __init__(self):
        self.lessons: dict[str, Lesson] = {}
        self.current_lesson: Optional[Lesson] = None
        self.validators: dict[str, Callable] = {}
        self._init_builtin_lessons()

    def _init_builtin_lessons(self):
        """Initialize built-in lessons."""
        # BASIC lesson
        self.add_lesson(
            Lesson(
                id="basic_hello_world",
                title="Hello, World! in BASIC",
                description="Learn your first BASIC program",
                language="basic",
                difficulty="beginner",
                checkpoints=[
                    Checkpoint(
                        title="Print a message",
                        description="Use PRINT to output text",
                        starter_code='PRINT "Hello, World!"',
                        expected_output="Hello, World!",
                        hints=["Use PRINT keyword", "Put text in quotes"],
                        solution='PRINT "Hello, World!"',
                    ),
                    Checkpoint(
                        title="Print multiple lines",
                        description="Output on separate lines",
                        starter_code='PRINT "Line 1"\nPRINT "Line 2"',
                        expected_output="Line 1\nLine 2",
                        hints=["Use PRINT twice", "Each PRINT on new line"],
                        solution='PRINT "Line 1"\nPRINT "Line 2"',
                    ),
                ],
            )
        )

        # Logo lesson
        self.add_lesson(
            Lesson(
                id="logo_square",
                title="Draw a Square in Logo",
                description="Learn turtle graphics basics",
                language="logo",
                difficulty="beginner",
                checkpoints=[
                    Checkpoint(
                        title="Draw one side",
                        description="Move forward 100 pixels",
                        starter_code="FORWARD 100",
                        expected_output="[turtle moved]",
                        hints=["Use FORWARD", "Type FORWARD 100"],
                        solution="FORWARD 100",
                    ),
                    Checkpoint(
                        title="Draw a square",
                        description="Create a square with 4 sides",
                        starter_code="REPEAT 4 [\n  FORWARD 100\n  RIGHT 90\n]",
                        expected_output="[square drawn]",
                        hints=["Use REPEAT 4", "Turn 90 degrees each time"],
                        solution="REPEAT 4 [\n  FORWARD 100\n  RIGHT 90\n]",
                    ),
                ],
            )
        )

    def add_lesson(self, lesson: Lesson) -> None:
        """Register a lesson."""
        self.lessons[lesson.id] = lesson

    @staticmethod
    def _slugify_lesson_id(title: str) -> str:
        """Create a safe lesson identifier from a title."""
        cleaned = "".join(ch.lower() if ch.isalnum() else "_" for ch in title)
        while "__" in cleaned:
            cleaned = cleaned.replace("__", "_")
        return cleaned.strip("_") or "custom_lesson"

    def _unique_lesson_id(self, base_id: str) -> str:
        """Ensure the lesson identifier is unique within the manager."""
        if base_id not in self.lessons:
            return base_id

        suffix = 2
        candidate = f"{base_id}_{suffix}"
        while candidate in self.lessons:
            suffix += 1
            candidate = f"{base_id}_{suffix}"
        return candidate

    def import_lesson_data(self, data: dict) -> Lesson:
        """Build and register a lesson from serializable data."""
        title = str(data.get("title", "Custom Lesson")).strip() or "Custom Lesson"
        lesson_id = str(data.get("id") or self._slugify_lesson_id(title))
        if lesson_id in self.lessons and self.lessons[lesson_id].title != title:
            lesson_id = self._unique_lesson_id(lesson_id)

        checkpoint_defs = data.get("checkpoints") or []
        checkpoints: List[Checkpoint] = []
        for item in checkpoint_defs:
            hints = item.get("hints", [])
            if isinstance(hints, str):
                hints = [hint.strip() for hint in hints.split("|") if hint.strip()]
            checkpoints.append(
                Checkpoint(
                    title=str(item.get("title", "Checkpoint")).strip() or "Checkpoint",
                    description=str(item.get("description", "")).strip(),
                    starter_code=str(item.get("starter_code", "")),
                    expected_output=str(item.get("expected_output", "")),
                    hints=list(hints),
                    solution=str(item.get("solution", "")),
                )
            )

        if not checkpoints:
            raise ValueError("Lesson must include at least one checkpoint")

        lesson = Lesson(
            id=lesson_id,
            title=title,
            description=str(data.get("description", "")).strip(),
            language=str(data.get("language", "basic")).strip().lower() or "basic",
            difficulty=str(data.get("difficulty", "beginner")).strip().lower()
            or "beginner",
            checkpoints=checkpoints,
        )
        self.add_lesson(lesson)
        return lesson

    def export_lesson_data(self, lesson_id: str) -> dict:
        """Return a lesson as plain serializable data."""
        lesson = self.lessons[lesson_id]
        return {
            "id": lesson.id,
            "title": lesson.title,
            "description": lesson.description,
            "language": lesson.language,
            "difficulty": lesson.difficulty,
            "checkpoints": [
                {
                    "title": checkpoint.title,
                    "description": checkpoint.description,
                    "starter_code": checkpoint.starter_code,
                    "expected_output": checkpoint.expected_output,
                    "hints": list(checkpoint.hints),
                    "solution": checkpoint.solution,
                }
                for checkpoint in lesson.checkpoints
            ],
        }

    def start_lesson(self, lesson_id: str) -> bool:
        """Start a lesson."""
        if lesson_id not in self.lessons:
            return False

        self.current_lesson = self.lessons[lesson_id]
        self.current_lesson.status = LessonStatus.IN_PROGRESS
        self.current_lesson.current_checkpoint = 0
        return True

    def get_current_checkpoint(self) -> Optional[Checkpoint]:
        """Get current checkpoint."""
        if not self.current_lesson:
            return None
        if self.current_lesson.current_checkpoint >= len(
            self.current_lesson.checkpoints
        ):
            return None
        return self.current_lesson.checkpoints[self.current_lesson.current_checkpoint]

    def check_output(self, output: str) -> bool:
        """Verify checkpoint output."""
        checkpoint = self.get_current_checkpoint()
        if not checkpoint:
            return False

        # Simple string matching; can be enhanced with custom validators
        expected = checkpoint.expected_output.strip()
        actual = output.strip()

        return expected in actual or actual == expected

    def advance_checkpoint(self) -> bool:
        """Move to next checkpoint."""
        if not self.current_lesson:
            return False

        self.current_lesson.current_checkpoint += 1

        if self.current_lesson.current_checkpoint >= len(
            self.current_lesson.checkpoints
        ):
            self.current_lesson.status = LessonStatus.COMPLETED
            return False  # No more checkpoints

        return True

    def get_hint(self) -> Optional[str]:
        """Get next hint for current checkpoint."""
        checkpoint = self.get_current_checkpoint()
        if not checkpoint or not checkpoint.hints:
            return None
        return checkpoint.hints[0]

    def get_solution(self) -> Optional[str]:
        """Get solution for current checkpoint."""
        checkpoint = self.get_current_checkpoint()
        if not checkpoint:
            return None
        return checkpoint.solution

    def list_lessons(self) -> List[Lesson]:
        """Get all available lessons."""
        return list(self.lessons.values())

    def get_progress(self) -> dict:
        """Get lesson progress summary."""
        if not self.current_lesson:
            return {"status": "idle"}

        return {
            "lesson_id": self.current_lesson.id,
            "lesson_title": self.current_lesson.title,
            "checkpoint": self.current_lesson.current_checkpoint + 1,
            "total_checkpoints": len(self.current_lesson.checkpoints),
            "status": self.current_lesson.status.value,
        }
