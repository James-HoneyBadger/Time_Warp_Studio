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
