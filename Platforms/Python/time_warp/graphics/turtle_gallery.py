"""
Turtle Gallery - Save, share, and replay turtle drawings with metadata.
"""

from __future__ import annotations

import json
import time
from dataclasses import asdict, dataclass
from datetime import datetime
from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ..graphics.turtle_state import TurtleState


@dataclass
class DrawingMetadata:
    """Metadata for a turtle drawing."""

    title: str
    author: str
    created_at: str
    language: str
    code: str
    canvas_width: int
    canvas_height: int
    duration_ms: int
    step_count: int


@dataclass
class DrawingStep:
    """Single step in a turtle drawing."""

    timestamp_ms: int
    command: str
    x: float
    y: float
    heading: float
    pen_down: bool
    color: str


class TurtleGallery:
    """Manager for saving, loading, and replaying turtle drawings."""

    def __init__(self, gallery_dir: Path | None = None):
        if gallery_dir is None:
            gallery_dir = Path.home() / ".Time_Warp" / "gallery"
        self.gallery_dir = gallery_dir
        self.gallery_dir.mkdir(parents=True, exist_ok=True)

        self.recording_steps: list[DrawingStep] = []
        self.recording_start_time: float | None = None
        self.is_recording = False

    def start_recording(self):
        """Start recording turtle movements."""
        self.is_recording = True
        self.recording_steps = []
        self.recording_start_time = time.time()

    def stop_recording(self):
        """Stop recording turtle movements."""
        self.is_recording = False

    def record_step(
        self,
        command: str,
        turtle: TurtleState,
        color: str = "black",
    ):
        """Record a single turtle drawing step."""
        if not self.is_recording or self.recording_start_time is None:
            return

        elapsed_ms = int((time.time() - self.recording_start_time) * 1000)

        step = DrawingStep(
            timestamp_ms=elapsed_ms,
            command=command,
            x=turtle.x,
            y=turtle.y,
            heading=turtle.heading,
            pen_down=turtle.pen_down,
            color=color,
        )

        self.recording_steps.append(step)

    def save_drawing(
        self,
        title: str,
        author: str,
        language: str,
        code: str,
        canvas_width: int = 1024,
        canvas_height: int = 768,
    ) -> Path:
        """Save current recording as a drawing file."""
        if not self.recording_steps:
            raise ValueError("No recording to save")

        duration_ms = (
            self.recording_steps[-1].timestamp_ms
            if self.recording_steps
            else 0
        )

        metadata = DrawingMetadata(
            title=title,
            author=author,
            created_at=datetime.now().isoformat(),
            language=language,
            code=code,
            canvas_width=canvas_width,
            canvas_height=canvas_height,
            duration_ms=duration_ms,
            step_count=len(self.recording_steps),
        )

        # Create safe filename
        safe_title = "".join(
            c for c in title if c.isalnum() or c in (" ", "-", "_")
        )
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"{safe_title}_{timestamp}.json"
        filepath = self.gallery_dir / filename

        # Save as JSON
        data = {
            "metadata": asdict(metadata),
            "steps": [asdict(step) for step in self.recording_steps],
        }

        with open(filepath, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2)

        return filepath

    def load_drawing(
        self, filepath: Path
    ) -> tuple[DrawingMetadata, list[DrawingStep]]:
        """Load a drawing from file."""
        with open(filepath, "r", encoding="utf-8") as f:
            data = json.load(f)

        metadata = DrawingMetadata(**data["metadata"])
        steps = [DrawingStep(**step) for step in data["steps"]]

        return metadata, steps

    def list_drawings(self) -> list[tuple[Path, DrawingMetadata]]:
        """List all drawings in the gallery."""
        drawings = []

        for filepath in sorted(self.gallery_dir.glob("*.json"), reverse=True):
            try:
                with open(filepath, "r", encoding="utf-8") as f:
                    data = json.load(f)
                    metadata = DrawingMetadata(**data["metadata"])
                    drawings.append((filepath, metadata))
            except (json.JSONDecodeError, KeyError, TypeError):
                continue

        return drawings

    def export_as_svg(
        self,
        filepath: Path,
        metadata: DrawingMetadata,
        steps: list[DrawingStep],
    ) -> Path:
        """Export drawing as SVG file."""
        svg_path = filepath.with_suffix(".svg")

        # SVG header
        svg_lines = [
            f'<svg width="{
                metadata.canvas_width}" height="{
                metadata.canvas_height}" '
            'xmlns="http://www.w3.org/2000/svg">',
            '  <rect width="100%" height="100%" fill="white"/>',
            f'  <g transform="translate({
                metadata.canvas_width /
                2}, '
            f'{
                    metadata.canvas_height /
                    2}) scale(1, -1)">',
        ]

        # Draw paths
        prev_step = None
        for step in steps:
            if prev_step and step.pen_down:
                svg_lines.append(
                    f'    <line x1="{prev_step.x}" y1="{prev_step.y}" '
                    f'x2="{step.x}" y2="{step.y}" '
                    f'stroke="{step.color}" stroke-width="2"/>'
                )
            prev_step = step

        svg_lines.append("  </g>")
        svg_lines.append("</svg>")

        with open(svg_path, "w", encoding="utf-8") as f:
            f.write("\n".join(svg_lines))

        return svg_path

    def replay_generator(
        self,
        steps: list[DrawingStep],
        speed: float = 1.0,
    ):
        """
        Generate replay steps with timing information.
        Yields (step, elapsed_time, total_time).
        """
        if not steps:
            return

        total_time = steps[-1].timestamp_ms / 1000.0
        start_time = time.time()

        for step in steps:
            target_time = (step.timestamp_ms / 1000.0) / speed
            elapsed = time.time() - start_time

            # Wait if we're ahead
            if elapsed < target_time:
                time.sleep(target_time - elapsed)

            yield step, elapsed, total_time


def create_share_link(
    filepath: Path, base_url: str = "https://timewarp.art"
) -> str:
    """Create a shareable link for a drawing (placeholder)."""
    # In a real implementation, this would upload to a sharing service
    return f"{base_url}/gallery/{filepath.stem}"
