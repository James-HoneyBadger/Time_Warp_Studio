"""Per-tab editor state dataclass.

Replaces the three parallel dicts (tab_files, tab_modified, tab_languages)
previously kept on MainWindow with a single, coherent TabState object per
open editor tab.  All mutations go through the same dict key so there is no
way for the three values to fall out of sync.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Optional

if TYPE_CHECKING:
    from ..core.interpreter import Language


def _default_language():
    """Deferred default to avoid circular imports at module load time."""
    from ..core.interpreter import Language  # pylint: disable=import-outside-toplevel
    return Language.BASIC


@dataclass
class TabState:
    """State associated with a single editor tab.

    Attributes:
        file:     Absolute path of the file on disk, or None if unsaved.
        modified: True when the in-memory content differs from the saved file.
        language: The active programming language for this tab.
        running:  True while the interpreter thread is executing this tab's code.
    """

    file: Optional[str] = None
    modified: bool = False
    language: "Language" = field(default_factory=_default_language)
    running: bool = False

    def reset_to_saved(self) -> None:
        """Mark the tab as clean (saved) without changing the file path."""
        self.modified = False

    def mark_modified(self) -> None:
        """Mark the tab as having unsaved changes."""
        self.modified = True
