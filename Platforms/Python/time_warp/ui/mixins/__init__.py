"""UI mixin classes that decompose MainWindow into focused responsibilities.

Each mixin is a plain class whose methods reference ``self.*`` attributes
provided by :class:`~time_warp.ui.main_window.MainWindow`.  They are
combined through multiple inheritance so that MainWindow gains all methods
without any API change.
"""

from .collaboration_mixin import CollaborationMixin
from .debug_mixin import DebugMixin
from .export_mixin import ExportMixin
from .file_ops_mixin import FileOperationsMixin
from .help_mixin import HelpDocsMixin
from .classroom_mixin import ClassroomMixin

__all__ = [
    "CollaborationMixin",
    "ClassroomMixin",
    "DebugMixin",
    "ExportMixin",
    "FileOperationsMixin",
    "HelpDocsMixin",
]
