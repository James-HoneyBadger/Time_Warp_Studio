"""Inline turtle preview for Logo code.

Live thumbnail that updates while editing, with stroke tracking.
"""

import math
from dataclasses import dataclass
from enum import Enum
from typing import List, Tuple


class TurtleCommand(Enum):
    """Turtle graphics commands."""

    FORWARD = "forward"
    BACKWARD = "backward"
    LEFT = "left"
    RIGHT = "right"
    PENUP = "penup"
    PENDOWN = "pendown"
    SETCOLOR = "setcolor"
    SETWIDTH = "setwidth"
    HOME = "home"
    CLEAR = "clear"


@dataclass
class TurtleStroke:
    """A single turtle stroke."""

    x1: float
    y1: float
    x2: float
    y2: float
    color: str
    width: int
    line_number: int


@dataclass
class TurtleState:
    """Current turtle state."""

    x: float = 0
    y: float = 0
    angle: float = 90  # 0=right, 90=up
    pen_down: bool = True
    color: str = "black"
    width: int = 1


class TurtlePreview:
    """Generate preview from Logo code."""

    def __init__(self, width: int = 200, height: int = 200):
        self.width = width
        self.height = height
        self.state = TurtleState()
        self.strokes: List[TurtleStroke] = []
        self.command_map = self._build_command_map()

    def _build_command_map(self) -> dict:
        """Map command names to handlers."""
        return {
            "forward": self._forward,
            "fd": self._forward,
            "backward": self._backward,
            "bk": self._backward,
            "back": self._backward,
            "left": self._left,
            "lt": self._left,
            "right": self._right,
            "rt": self._right,
            "penup": self._penup,
            "pu": self._penup,
            "pendown": self._pendown,
            "pd": self._pendown,
            "setpencolor": self._setcolor,
            "setcolor": self._setcolor,
            "setpenwidth": self._setwidth,
            "home": self._home,
            "clear": self._clear,
        }

    def parse_and_draw(self, code: str) -> List[TurtleStroke]:
        """Parse Logo code and generate strokes."""
        self.state = TurtleState()
        self.strokes = []

        lines = code.split("\n")
        for line_num, line in enumerate(lines, 1):
            self._process_line(line.strip(), line_num)

        return self.strokes

    def _process_line(self, line: str, line_num: int) -> None:
        """Process a single line of Logo code."""
        if not line or line.startswith(";"):
            return

        # Simple command parsing
        tokens = line.lower().split()
        if not tokens:
            return

        cmd = tokens[0]
        args = tokens[1:] if len(tokens) > 1 else []

        if cmd in self.command_map:
            try:
                handler = self.command_map[cmd]
                handler(args, line_num)
            except Exception:
                pass  # Silently skip invalid commands

    def _forward(self, args: List[str], line_num: int) -> None:
        """Move forward."""
        if not args:
            return
        try:
            distance = float(args[0])
            rad = math.radians(self.state.angle)
            x2 = self.state.x + distance * math.cos(rad)
            y2 = self.state.y + distance * math.sin(rad)

            if self.state.pen_down:
                self.strokes.append(
                    TurtleStroke(
                        self.state.x,
                        self.state.y,
                        x2,
                        y2,
                        self.state.color,
                        self.state.width,
                        line_num,
                    )
                )

            self.state.x = x2
            self.state.y = y2
        except ValueError:
            pass

    def _backward(self, args: List[str], line_num: int) -> None:
        """Move backward."""
        if not args:
            return
        try:
            distance = float(args[0])
            rad = math.radians(self.state.angle + 180)
            x2 = self.state.x + distance * math.cos(rad)
            y2 = self.state.y + distance * math.sin(rad)

            if self.state.pen_down:
                self.strokes.append(
                    TurtleStroke(
                        self.state.x,
                        self.state.y,
                        x2,
                        y2,
                        self.state.color,
                        self.state.width,
                        line_num,
                    )
                )

            self.state.x = x2
            self.state.y = y2
        except ValueError:
            pass

    def _left(self, args: List[str], line_num: int) -> None:
        """Turn left."""
        if not args:
            return
        try:
            angle = float(args[0])
            self.state.angle += angle
            self.state.angle %= 360
        except ValueError:
            pass

    def _right(self, args: List[str], line_num: int) -> None:
        """Turn right."""
        if not args:
            return
        try:
            angle = float(args[0])
            self.state.angle -= angle
            self.state.angle %= 360
        except ValueError:
            pass

    def _penup(self, args: List[str], line_num: int) -> None:
        """Lift pen."""
        self.state.pen_down = False

    def _pendown(self, args: List[str], line_num: int) -> None:
        """Lower pen."""
        self.state.pen_down = True

    def _setcolor(self, args: List[str], line_num: int) -> None:
        """Set pen color."""
        if args:
            self.state.color = args[0]

    def _setwidth(self, args: List[str], line_num: int) -> None:
        """Set pen width."""
        if args:
            try:
                self.state.width = int(args[0])
            except ValueError:
                pass

    def _home(self, args: List[str], line_num: int) -> None:
        """Return to center."""
        self.state.x = 0
        self.state.y = 0
        self.state.angle = 90

    def _clear(self, args: List[str], line_num: int) -> None:
        """Clear screen."""
        self.strokes.clear()
        self.state = TurtleState()

    def get_bounds(self) -> Tuple[float, float, float, float]:
        """Get bounding box of all strokes."""
        if not self.strokes:
            return (-100, -100, 100, 100)

        xs = []
        ys = []
        for stroke in self.strokes:
            xs.extend([stroke.x1, stroke.x2])
            ys.extend([stroke.y1, stroke.y2])

        return (min(xs), min(ys), max(xs), max(ys))

    def get_strokes_for_line(self, line_num: int) -> List[TurtleStroke]:
        """Get strokes drawn by a specific line."""
        return [s for s in self.strokes if s.line_number == line_num]
