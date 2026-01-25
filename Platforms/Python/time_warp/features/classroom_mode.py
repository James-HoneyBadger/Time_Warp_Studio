"""Classroom mode for Time Warp Studio.

Presentation mode, workspace bundles, and classroom utilities.
"""

import json
import zipfile
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional


@dataclass
class WorkspaceBundle:
    """A bundled workspace for sharing or distribution."""

    name: str
    description: str
    created: datetime
    files: Dict[str, str]  # filename -> content
    settings: Dict[str, any]
    lessons: List[str] = None

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
        self.bundles_dir = Path.home() / ".time_warp" / "bundles"
        self._ensure_bundle_dir()

    def _ensure_bundle_dir(self) -> None:
        """Create bundles directory if needed."""
        self.bundles_dir.mkdir(parents=True, exist_ok=True)

    # Presentation Mode

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
        settings: Dict[str, any],
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
        except Exception:
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
        except Exception:
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
