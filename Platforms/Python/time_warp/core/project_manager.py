"""Project manager for Time Warp Studio.

Manages .twsproj project files (JSON format) that bundle together
multiple source files, run configurations, and session state.
"""

from __future__ import annotations

import json
import os
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any


PROJECT_EXTENSION = ".twsproj"
PROJECT_VERSION = 1


@dataclass
class ProjectFile:
    """A single file tracked within a project."""

    path: str  # Relative path from project dir
    language: str = "BASIC"
    is_main: bool = False


@dataclass
class Project:
    """In-memory representation of a .twsproj file."""

    name: str
    version: int = PROJECT_VERSION
    description: str = ""
    files: list[ProjectFile] = field(default_factory=list)
    main_file: str = ""  # Relative path of the entry-point file
    theme: str = ""  # Last-used theme name (empty = use global)

    # ------------------------------------------------------------------ #
    #  Serialisation                                                       #
    # ------------------------------------------------------------------ #

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "version": self.version,
            "description": self.description,
            "files": [asdict(f) for f in self.files],
            "main_file": self.main_file,
            "theme": self.theme,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "Project":
        files = [ProjectFile(**f) for f in data.get("files", [])]
        return cls(
            name=data.get("name", "Untitled"),
            version=data.get("version", PROJECT_VERSION),
            description=data.get("description", ""),
            files=files,
            main_file=data.get("main_file", ""),
            theme=data.get("theme", ""),
        )


class ProjectManager:
    """Load, save, and manage Time Warp Studio projects.

    A project is stored as a JSON file with the ``.twsproj`` extension.
    ``ProjectManager`` does not hold any Qt state — it is a pure
    data-layer helper suitable for use outside the UI.

    Usage::

        pm = ProjectManager()
        project = pm.create("My Project", "/home/user/projects/my_project")
        pm.add_file(project, "hello.bas", language="BASIC", is_main=True)
        pm.save(project, "/home/user/projects/my_project/my_project.twsproj")
        loaded = pm.load("/home/user/projects/my_project/my_project.twsproj")
    """

    MAX_RECENT = 10

    def __init__(self) -> None:
        self._recent: list[str] = []  # absolute paths, most-recent first

    # ------------------------------------------------------------------ #
    #  CRUD                                                                #
    # ------------------------------------------------------------------ #

    def create(self, name: str, project_dir: str | None = None) -> Project:
        """Create a new empty project.

        Args:
            name: Human-readable project name.
            project_dir: Optional directory; if given and it doesn't
                exist it will be created.

        Returns:
            A fresh :class:`Project` instance.
        """
        if project_dir:
            os.makedirs(project_dir, exist_ok=True)
        return Project(name=name)

    def load(self, path: str) -> Project:
        """Load a project from a ``.twsproj`` file.

        Args:
            path: Absolute path to the project file.

        Returns:
            The parsed :class:`Project`.

        Raises:
            FileNotFoundError: If *path* does not exist.
            ValueError: If the file is not valid JSON or is missing
                required fields.
        """
        path = os.path.abspath(path)
        if not os.path.isfile(path):
            raise FileNotFoundError(f"Project file not found: {path}")

        with open(path, encoding="utf-8") as fh:
            try:
                data = json.load(fh)
            except json.JSONDecodeError as exc:
                raise ValueError(f"Invalid project file: {exc}") from exc

        project = Project.from_dict(data)
        self._add_recent(path)
        return project

    def save(self, project: Project, path: str) -> None:
        """Persist *project* to *path*.

        The parent directory is created if it does not exist.

        Args:
            project: The project to save.
            path: Destination file path (should end in ``PROJECT_EXTENSION``).
        """
        path = os.path.abspath(path)
        os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

        with open(path, "w", encoding="utf-8") as fh:
            json.dump(project.to_dict(), fh, indent=2)

        self._add_recent(path)

    # ------------------------------------------------------------------ #
    #  File list helpers                                                   #
    # ------------------------------------------------------------------ #

    def add_file(
        self,
        project: Project,
        relative_path: str,
        language: str = "BASIC",
        is_main: bool = False,
    ) -> ProjectFile:
        """Add a file entry to *project*.

        Args:
            project: The project to modify.
            relative_path: Path relative to the project directory.
            language: Language enum name (e.g. ``"BASIC"``).
            is_main: Whether this is the primary run target.

        Returns:
            The new :class:`ProjectFile`.
        """
        pf = ProjectFile(path=relative_path, language=language, is_main=is_main)
        project.files.append(pf)
        if is_main:
            project.main_file = relative_path
        return pf

    def remove_file(self, project: Project, relative_path: str) -> None:
        """Remove a file entry by its relative path."""
        project.files = [f for f in project.files if f.path != relative_path]
        if project.main_file == relative_path:
            project.main_file = project.files[0].path if project.files else ""

    # ------------------------------------------------------------------ #
    #  Recent files                                                        #
    # ------------------------------------------------------------------ #

    @property
    def recent_projects(self) -> list[str]:
        """Return most-recently-used project paths (newest first)."""
        return list(self._recent)

    def load_recent_from_list(self, paths: list[str]) -> None:
        """Restore recent list from persisted storage (e.g. QSettings)."""
        self._recent = [p for p in paths if os.path.isfile(p)][: self.MAX_RECENT]

    def _add_recent(self, path: str) -> None:
        """Add *path* to the recent list, deduplicating and capping size."""
        if path in self._recent:
            self._recent.remove(path)
        self._recent.insert(0, path)
        self._recent = self._recent[: self.MAX_RECENT]

    # ------------------------------------------------------------------ #
    #  Convenience                                                         #
    # ------------------------------------------------------------------ #

    @staticmethod
    def default_project_path(project_dir: str, name: str) -> str:
        """Return the conventional project-file path inside *project_dir*."""
        safe_name = "".join(c if c.isalnum() or c in "-_" else "_" for c in name)
        return str(Path(project_dir) / f"{safe_name}{PROJECT_EXTENSION}")
