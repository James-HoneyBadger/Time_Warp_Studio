"""Feature modules for Time Warp Studio.

New features: lessons, examples browser, turtle preview, theme editor, autosave, classroom mode.
"""

from .autosave_manager import AutosaveManager, FileHistory, FileVersion
from .classroom_mode import ClassroomMode, PresentationMode, WorkspaceBundle
from .examples_browser import Difficulty, Example, ExamplesBrowser, Language
from .lesson_system import Checkpoint, Lesson, LessonManager, LessonStatus
from .theme_editor import Theme, ThemeColors, ThemeManager
from .turtle_preview import (
    TurtlePreview,
    TurtleState,
    TurtleStroke,
)

__all__ = [
    "LessonManager",
    "Lesson",
    "Checkpoint",
    "LessonStatus",
    "ExamplesBrowser",
    "Example",
    "Language",
    "Difficulty",
    "TurtlePreview",
    "TurtleStroke",
    "TurtleState",
    "ThemeManager",
    "Theme",
    "ThemeColors",
    "AutosaveManager",
    "FileHistory",
    "FileVersion",
    "ClassroomMode",
    "WorkspaceBundle",
    "PresentationMode",
]
