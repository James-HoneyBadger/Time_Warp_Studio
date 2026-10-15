"""Feature modules for Time Warp Studio: lessons, examples, autosave, classroom mode.

``lesson_system`` is imported directly (it depends on ``core.interpreter``,
which itself imports this package).
"""

from .autosave_manager import AutosaveManager, FileHistory, FileVersion
from .classroom_mode import ClassroomMode, PresentationMode, WorkspaceBundle
from .examples_browser import Difficulty, Example, ExamplesBrowser

__all__ = [
    "ExamplesBrowser",
    "Example",
    "Difficulty",
    "AutosaveManager",
    "FileHistory",
    "FileVersion",
    "ClassroomMode",
    "WorkspaceBundle",
    "PresentationMode",
]
