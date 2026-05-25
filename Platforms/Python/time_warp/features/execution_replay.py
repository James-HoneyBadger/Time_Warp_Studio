"""Visual execution replay for understanding algorithms."""

import json
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple


class VisualizationType(Enum):
    """Types of visualizations supported."""

    ARRAY = "array"
    GRAPH = "graph"
    MATRIX = "matrix"
    TURTLE = "turtle"
    VARIABLE_STATE = "variable_state"
    STACK = "stack"


@dataclass
class VisualizationFrame:
    """A single frame in visualization playback."""

    frame_index: int
    line_no: int
    visualization_type: VisualizationType
    data: Dict[str, Any]  # Type-specific visualization data
    variables: Dict[str, Any]
    timestamp: float
    description: str = ""


class VisualizationRecorder:
    """Records visualization frames during execution."""

    def __init__(self):
        """Initialize recorder."""
        self.frames: List[VisualizationFrame] = []
        self.current_frame = 0
        self.is_recording = False

    def start_recording(self):
        """Start recording frames."""
        self.frames.clear()
        self.current_frame = 0
        self.is_recording = True

    def stop_recording(self):
        """Stop recording frames."""
        self.is_recording = False

    def record_frame(
        self,
        line_no: int,
        viz_type: VisualizationType,
        data: Dict[str, Any],
        variables: Dict[str, Any],
        description: str = "",
    ) -> int:
        """
        Record a visualization frame.

        Returns: Frame index
        """
        if not self.is_recording:
            return -1

        frame = VisualizationFrame(
            frame_index=len(self.frames),
            line_no=line_no,
            visualization_type=viz_type,
            data=data,
            variables=variables.copy(),
            timestamp=len(self.frames) * 0.1,  # Simulated 100ms per frame
            description=description,
        )

        self.frames.append(frame)
        return frame.frame_index

    def get_current_frame(self) -> Optional[VisualizationFrame]:
        """Get current frame."""
        if 0 <= self.current_frame < len(self.frames):
            return self.frames[self.current_frame]
        return None

    def next_frame(self):
        """Move to next frame."""
        if self.current_frame < len(self.frames) - 1:
            self.current_frame += 1

    def prev_frame(self):
        """Move to previous frame."""
        if self.current_frame > 0:
            self.current_frame -= 1

    def seek_to_frame(self, index: int):
        """Seek to specific frame."""
        if 0 <= index < len(self.frames):
            self.current_frame = index

    def seek_to_line(self, line_no: int):
        """Seek to first frame at specific line."""
        for i, frame in enumerate(self.frames):
            if frame.line_no == line_no:
                self.current_frame = i
                return

    def get_frame_at(self, index: int) -> Optional[VisualizationFrame]:
        """Get frame by index."""
        if 0 <= index < len(self.frames):
            return self.frames[index]
        return None

    def get_frames_by_line(self, line_no: int) -> List[VisualizationFrame]:
        """Get all frames for a specific line."""
        return [f for f in self.frames if f.line_no == line_no]

    def export_animation(self) -> Dict[str, Any]:
        """Export frames as animation data."""
        return {
            "total_frames": len(self.frames),
            "fps": 10,  # 10 frames per second
            "frames": [
                {
                    "index": f.frame_index,
                    "line": f.line_no,
                    "type": f.visualization_type.value,
                    "data": f.data,
                    "variables": {k: str(v) for k, v in f.variables.items()},
                    "description": f.description,
                }
                for f in self.frames
            ],
        }

    def save_twreplay(self, path: str | Path) -> None:
        """Serialize all recorded frames to a ``.twreplay`` JSON file.

        The file format is a superset of :meth:`export_animation` with a
        ``format_version`` header so future loaders can migrate old files.

        Args:
            path: Destination file path (should end in ``.twreplay``).
        """
        payload: Dict[str, Any] = {
            "format_version": 1,
            "timestamp": __import__("datetime").datetime.now(
                __import__("datetime").timezone.utc
            ).isoformat(),
            **self.export_animation(),
        }
        Path(path).write_text(json.dumps(payload, indent=2), encoding="utf-8")

    @classmethod
    def load_twreplay(cls, path: str | Path) -> "VisualizationRecorder":
        """Load a previously saved ``.twreplay`` file.

        Returns a new :class:`VisualizationRecorder` populated with the
        stored frames, ready for playback.

        Args:
            path: Path to a ``.twreplay`` file.

        Raises:
            ValueError: If the file contains an unsupported format version.
            json.JSONDecodeError: If the file is not valid JSON.
        """
        raw = json.loads(Path(path).read_text(encoding="utf-8"))
        if raw.get("format_version", 1) != 1:
            raise ValueError(
                f"Unsupported .twreplay format version: {raw['format_version']}"
            )
        recorder = cls()
        recorder.frames = []
        for item in raw.get("frames", []):
            try:
                viz_type = VisualizationType(item["type"])
            except (KeyError, ValueError):
                viz_type = VisualizationType.VARIABLE_STATE
            recorder.frames.append(
                VisualizationFrame(
                    frame_index=item.get("index", len(recorder.frames)),
                    line_no=item.get("line", 0),
                    visualization_type=viz_type,
                    data=item.get("data", {}),
                    variables=item.get("variables", {}),
                    timestamp=item.get("timestamp", 0.0),
                    description=item.get("description", ""),
                )
            )
        return recorder


class ArrayVisualizer:
    """Visualizes array/list data."""

    @staticmethod
    def create_frame(
        array: List[Any],
        highlighted_indices: Optional[List[int]] = None,
        description: str = "",
    ) -> Dict[str, Any]:
        """
        Create visualization frame for array.

        Args:
            array: List to visualize
            highlighted_indices: Indices to highlight (for sorting, searching)
            description: Frame description

        Returns:
            Frame data dict
        """
        return {
            "type": "array",
            "values": [str(v) for v in array],
            "length": len(array),
            "highlighted": highlighted_indices or [],
            "description": description,
        }

    @staticmethod
    def visualize_ascii(
        array: List[Any], highlighted_indices: Optional[List[int]] = None
    ) -> str:
        """Generate ASCII visualization of array."""
        lines = []
        lines.append("┌" + "─" * (len(array) * 4 - 1) + "┐")

        # Values
        values = []
        for i, val in enumerate(array):
            val_str = str(val)[:3]  # Limit to 3 chars
            if highlighted_indices and i in highlighted_indices:
                values.append(f"│{val_str:>3}")
            else:
                values.append(f"│{val_str:>3}")
        values.append("│")
        lines.append("".join(values))

        # Indices
        indices = []
        for i in range(len(array)):
            indices.append(f"│{i:>3}")
        indices.append("│")
        lines.append("".join(indices))

        lines.append("└" + "─" * (len(array) * 4 - 1) + "┘")

        return "\n".join(lines)


class MatrixVisualizer:
    """Visualizes 2D matrix data."""

    @staticmethod
    def create_frame(
        matrix: List[List[Any]],
        highlighted_cells: Optional[List[Tuple[int, int]]] = None,
        description: str = "",
    ) -> Dict[str, Any]:
        """Create visualization frame for matrix."""
        return {
            "type": "matrix",
            "rows": len(matrix),
            "cols": len(matrix[0]) if matrix else 0,
            "values": [[str(v) for v in row] for row in matrix],
            "highlighted": highlighted_cells or [],
            "description": description,
        }

    @staticmethod
    def visualize_ascii(
        matrix: List[List[Any]],
        highlighted_cells: Optional[List[Tuple[int, int]]] = None,
    ) -> str:
        """Generate ASCII visualization of matrix."""
        highlighted = set(highlighted_cells) if highlighted_cells else set()
        lines = []

        for r, row in enumerate(matrix):
            row_str = "│ "
            for c, val in enumerate(row):
                val_str = str(val)[:2]
                if (r, c) in highlighted:
                    row_str += "┌─┐ "
                else:
                    row_str += f"{val_str:>2} "
            row_str += "│"
            lines.append(row_str)

        return "\n".join(lines)


class StackVisualizer:
    """Visualizes stack operations."""

    @staticmethod
    def create_frame(
        stack: List[Any], operation: str = "", description: str = ""
    ) -> Dict[str, Any]:
        """Create visualization frame for stack."""
        return {
            "type": "stack",
            "values": [str(v) for v in reversed(stack)],  # Top is first
            "depth": len(stack),
            "operation": operation,
            "description": description,
        }

    @staticmethod
    def visualize_ascii(stack: List[Any]) -> str:
        """Generate ASCII visualization of stack."""
        lines = []

        if not stack:
            lines.append("┌─────┐")
            lines.append("│     │  ← Empty")
            lines.append("└─────┘")
        else:
            # Draw from top to bottom
            lines.append("┌─────┐")
            for i, val in enumerate(reversed(stack)):
                val_str = str(val)[:5]
                if i == 0:
                    lines.append(f"│{val_str:>5}│  ← Top (most recent)")
                else:
                    lines.append(f"│{val_str:>5}│")
            lines.append("└─────┘")

        return "\n".join(lines)


class ExecutionReplayPlayer:
    """Plays back recorded execution with visualization."""

    def __init__(self, recorder: VisualizationRecorder):
        """Initialize player."""
        self.recorder = recorder
        self.is_playing = False
        self.playback_speed = 1.0  # 1x speed

    def play(self):
        """Start playback."""
        self.is_playing = True
        self.recorder.current_frame = 0

    def pause(self):
        """Pause playback."""
        self.is_playing = False

    def resume(self):
        """Resume playback."""
        self.is_playing = True

    def stop(self):
        """Stop playback and reset."""
        self.is_playing = False
        self.recorder.current_frame = 0

    def set_speed(self, speed: float):
        """
        Set playback speed.

        Args:
            speed: Speed multiplier (0.5 = half speed, 2.0 = double speed)
        """
        self.playback_speed = max(0.1, speed)

    def next_frame(self):
        """Move to next frame."""
        self.recorder.next_frame()

    def prev_frame(self):
        """Move to previous frame."""
        self.recorder.prev_frame()

    def seek(self, frame_index: int):
        """Seek to frame."""
        self.recorder.seek_to_frame(frame_index)

    def get_current_visualization(self) -> Optional[str]:
        """Get ASCII visualization of current frame."""
        frame = self.recorder.get_current_frame()
        if not frame:
            return None

        if frame.visualization_type == VisualizationType.ARRAY:
            return ArrayVisualizer.visualize_ascii(
                frame.data.get("values", []), frame.data.get("highlighted", [])
            )
        elif frame.visualization_type == VisualizationType.MATRIX:
            return MatrixVisualizer.visualize_ascii(
                frame.data.get("values", []), frame.data.get("highlighted", [])
            )
        elif frame.visualization_type == VisualizationType.STACK:
            return StackVisualizer.visualize_ascii(frame.data.get("values", []))

        return None

    def get_frame_info(self) -> Optional[Dict[str, Any]]:
        """Get info about current frame."""
        frame = self.recorder.get_current_frame()
        if not frame:
            return None

        return {
            "index": frame.frame_index,
            "line": frame.line_no,
            "type": frame.visualization_type.value,
            "description": frame.description,
            "variables": frame.variables,
            "total_frames": len(self.recorder.frames),
        }


class AlgorithmVisualizer:
    """Helper class for visualizing common algorithms."""

    @staticmethod
    def visualize_bubble_sort(arr: List[int]) -> VisualizationRecorder:
        """Record bubble sort visualization."""
        recorder = VisualizationRecorder()
        recorder.start_recording()

        data = arr.copy()
        line_no = 1

        for i in range(len(data)):
            for j in range(len(data) - 1 - i):
                # Highlight being compared
                recorder.record_frame(
                    line_no=line_no,
                    viz_type=VisualizationType.ARRAY,
                    data=ArrayVisualizer.create_frame(data, [j, j + 1]),
                    variables={"i": i, "j": j},
                    description=f"Comparing {data[j]} and {data[j + 1]}",
                )

                if data[j] > data[j + 1]:
                    data[j], data[j + 1] = data[j + 1], data[j]
                    recorder.record_frame(
                        line_no=line_no + 1,
                        viz_type=VisualizationType.ARRAY,
                        data=ArrayVisualizer.create_frame(data, [j, j + 1]),
                        variables={"i": i, "j": j},
                        description=f"Swapped: {data[j + 1]} and {data[j]}",
                    )

        recorder.stop_recording()
        return recorder

    @staticmethod
    def visualize_binary_search(arr: List[int], target: int) -> VisualizationRecorder:
        """Record binary search visualization."""
        recorder = VisualizationRecorder()
        recorder.start_recording()

        left, right = 0, len(arr) - 1

        while left <= right:
            mid = (left + right) // 2

            recorder.record_frame(
                line_no=1,
                viz_type=VisualizationType.ARRAY,
                data=ArrayVisualizer.create_frame(arr, [left, mid, right]),
                variables={
                    "left": left,
                    "mid": mid,
                    "right": right,
                    "target": target,
                },
                description=f"Searching for {target} in range [{left}, {right}]",
            )

            if arr[mid] == target:
                recorder.record_frame(
                    line_no=2,
                    viz_type=VisualizationType.ARRAY,
                    data=ArrayVisualizer.create_frame(arr, [mid]),
                    variables={"result": mid},
                    description=f"Found {target} at index {mid}",
                )
                break
            elif arr[mid] < target:
                left = mid + 1
            else:
                right = mid - 1

        recorder.stop_recording()
        return recorder
