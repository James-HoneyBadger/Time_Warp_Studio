"""Classroom mode for Time Warp Studio.

Presentation mode, workspace bundles, and classroom utilities.
"""

import json
import zipfile
from dataclasses import asdict, dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional


@dataclass
class Student:
    """A student in the classroom roster."""

    name: str
    student_id: str
    work_dir: str = ""  # Absolute path to their work directory (empty = default)

    def get_work_dir(self, base_dir: Path) -> Path:
        """Return the resolved work directory for this student."""
        if self.work_dir:
            return Path(self.work_dir)
        return base_dir / self.student_id


@dataclass
class StudentRoster:
    """Classroom roster persisted to JSON."""

    teacher: str = ""
    students: List[Student] = field(default_factory=list)

    def to_dict(self) -> dict:
        return {"teacher": self.teacher, "students": [asdict(s) for s in self.students]}

    @classmethod
    def from_dict(cls, data: dict) -> "StudentRoster":
        return cls(
            teacher=data.get("teacher", ""),
            students=[Student(**s) for s in data.get("students", [])],
        )


@dataclass
class WorkspaceBundle:
    """A bundled workspace for sharing or distribution."""

    name: str
    description: str
    created: datetime
    files: Dict[str, str]  # filename -> content
    settings: Dict[str, Any]
    lessons: Optional[List[str]] = None

    def to_dict(self) -> dict:
        """Convert to dictionary."""
        return {
            "name": self.name,
            "description": self.description,
            "created": self.created.isoformat(),
            "files": self.files,
            "settings": self.settings,
            "lessons": self.lessons or [],
        }


@dataclass
class PresentationMode:
    """Presentation settings."""

    enabled: bool = False
    read_only: bool = True
    enlarged_ui: bool = True
    font_size: int = 16
    hide_menus: bool = False
    fullscreen: bool = False


class ClassroomMode:
    """Manage classroom features."""

    def __init__(self):
        self.presentation_mode = PresentationMode()
        from ..core.config import BUNDLES_DIR, APP_DATA_DIR

        self.bundles_dir = BUNDLES_DIR
        self._roster_path = APP_DATA_DIR / "classroom_roster.json"
        self._ensure_bundle_dir()
        self.roster = self._load_roster()

    def _ensure_bundle_dir(self) -> None:
        """Create bundles directory if needed."""
        self.bundles_dir.mkdir(parents=True, exist_ok=True)

    # ------------------------------------------------------------------
    # Student Roster
    # ------------------------------------------------------------------

    def _load_roster(self) -> StudentRoster:
        """Load roster from disk, or return empty roster."""
        if self._roster_path.exists():
            try:
                data = json.loads(self._roster_path.read_text(encoding="utf-8"))
                return StudentRoster.from_dict(data)
            except (json.JSONDecodeError, KeyError, TypeError):
                pass
        return StudentRoster()

    def save_roster(self) -> None:
        """Persist the current roster to disk."""
        self._roster_path.parent.mkdir(parents=True, exist_ok=True)
        self._roster_path.write_text(
            json.dumps(self.roster.to_dict(), indent=2), encoding="utf-8"
        )

    def add_student(self, name: str, student_id: str, work_dir: str = "") -> Student:
        """Add a student to the roster and persist."""
        student = Student(name=name, student_id=student_id, work_dir=work_dir)
        # Replace if student_id already exists
        self.roster.students = [s for s in self.roster.students if s.student_id != student_id]
        self.roster.students.append(student)
        self.save_roster()
        return student

    def remove_student(self, student_id: str) -> bool:
        """Remove a student by ID. Returns True if found and removed."""
        before = len(self.roster.students)
        self.roster.students = [s for s in self.roster.students if s.student_id != student_id]
        if len(self.roster.students) < before:
            self.save_roster()
            return True
        return False

    def distribute_assignment(
        self, bundle: "WorkspaceBundle", students: Optional[List[Student]] = None
    ) -> Dict[str, str]:
        """Write assignment files from *bundle* into each student's work directory.

        Returns a dict mapping student_id → status message.
        """
        targets = students if students is not None else self.roster.students
        base_dir = Path.home() / "student_work"
        results: Dict[str, str] = {}
        for student in targets:
            dest = student.get_work_dir(base_dir)
            try:
                dest.mkdir(parents=True, exist_ok=True)
                for filename, content in bundle.files.items():
                    (dest / filename).write_text(content, encoding="utf-8")
                results[student.student_id] = f"✅ Distributed to {dest}"
            except OSError as exc:
                results[student.student_id] = f"❌ {exc}"
        return results

    def collect_submissions(
        self, assignment_name: str, students: Optional[List[Student]] = None
    ) -> Path:
        """Zip up all student work for *assignment_name* into a single archive.

        Returns the path to the created ZIP file.
        """
        targets = students if students is not None else self.roster.students
        base_dir = Path.home() / "student_work"
        safe_name = assignment_name.replace(" ", "_").replace("/", "_")
        out_zip = self.bundles_dir / f"submissions_{safe_name}_{datetime.now().strftime('%Y%m%d_%H%M%S')}.zip"
        out_zip.parent.mkdir(parents=True, exist_ok=True)
        with zipfile.ZipFile(out_zip, "w") as zf:
            for student in targets:
                work_dir = student.get_work_dir(base_dir)
                if not work_dir.is_dir():
                    continue
                for fpath in sorted(work_dir.rglob("*")):
                    if fpath.is_file():
                        arc_name = f"{student.student_id}/{fpath.relative_to(work_dir)}"
                        zf.write(fpath, arc_name)
        return out_zip

    def start_presentation(self, font_size: int = 16, fullscreen: bool = True) -> None:
        """Start presentation mode."""
        self.presentation_mode.enabled = True
        self.presentation_mode.font_size = font_size
        self.presentation_mode.fullscreen = fullscreen
        self.presentation_mode.read_only = True
        self.presentation_mode.enlarged_ui = True

    def stop_presentation(self) -> None:
        """Stop presentation mode."""
        self.presentation_mode.enabled = False

    def is_presentation_mode(self) -> bool:
        """Check if in presentation mode."""
        return self.presentation_mode.enabled

    # Workspace Bundles

    def create_bundle(
        self,
        name: str,
        description: str,
        files: Dict[str, str],
        settings: Dict[str, Any],
        lessons: Optional[List[str]] = None,
    ) -> WorkspaceBundle:
        """Create a workspace bundle."""
        bundle = WorkspaceBundle(
            name=name,
            description=description,
            created=datetime.now(),
            files=files,
            settings=settings,
            lessons=lessons or [],
        )
        return bundle

    def export_bundle(self, bundle: WorkspaceBundle, output_path: Path) -> bool:
        """Export bundle to ZIP file."""
        try:
            with zipfile.ZipFile(output_path, "w") as zf:
                # Write metadata
                metadata = bundle.to_dict()
                zf.writestr("bundle.json", json.dumps(metadata, indent=2))

                # Write files
                for filename, content in bundle.files.items():
                    zf.writestr(f"files/{filename}", content)

            return True
        except (ValueError, TypeError):
            return False

    def import_bundle(self, bundle_path: Path) -> Optional[WorkspaceBundle]:
        """Import bundle from ZIP file."""
        try:
            with zipfile.ZipFile(bundle_path, "r") as zf:
                # Read metadata
                metadata_text = zf.read("bundle.json").decode("utf-8")
                metadata = json.loads(metadata_text)

                # Read files
                files = {}
                for name in zf.namelist():
                    if name.startswith("files/"):
                        filename = name[6:]  # Remove "files/" prefix
                        files[filename] = zf.read(name).decode("utf-8")

                bundle = WorkspaceBundle(
                    name=metadata["name"],
                    description=metadata["description"],
                    created=datetime.fromisoformat(metadata["created"]),
                    files=files,
                    settings=metadata.get("settings", {}),
                    lessons=metadata.get("lessons", []),
                )

                return bundle
        except (ValueError, TypeError):
            return None

    def list_bundles(self) -> List[WorkspaceBundle]:
        """List available bundles."""
        bundles = []

        for bundle_file in self.bundles_dir.glob("*.zip"):
            bundle = self.import_bundle(bundle_file)
            if bundle:
                bundles.append(bundle)

        return sorted(bundles, key=lambda b: b.created, reverse=True)

    # Classroom Utilities

    def create_assignment_bundle(
        self,
        title: str,
        starter_code: Dict[str, str],
        expected_output: str,
        hints: List[str],
    ) -> WorkspaceBundle:
        """Create an assignment bundle."""
        bundle = WorkspaceBundle(
            name=f"Assignment: {title}",
            description=f"Complete the {title} assignment",
            created=datetime.now(),
            files=starter_code,
            settings={
                "assignment": True,
                "expected_output": expected_output,
                "hints": hints,
            },
        )
        return bundle

    def create_lesson_bundle(
        self,
        title: str,
        lesson_files: Dict[str, str],
        lesson_ids: List[str],
    ) -> WorkspaceBundle:
        """Create a lesson bundle."""
        bundle = WorkspaceBundle(
            name=f"Lesson: {title}",
            description=f"Learn {title}",
            created=datetime.now(),
            files=lesson_files,
            settings={"lesson": True},
            lessons=lesson_ids,
        )
        return bundle

    def get_classroom_settings(self) -> dict:
        """Get classroom mode settings."""
        return {
            "presentation_mode": {
                "enabled": self.presentation_mode.enabled,
                "read_only": self.presentation_mode.read_only,
                "enlarged_ui": self.presentation_mode.enlarged_ui,
                "font_size": self.presentation_mode.font_size,
                "hide_menus": self.presentation_mode.hide_menus,
                "fullscreen": self.presentation_mode.fullscreen,
            }
        }
