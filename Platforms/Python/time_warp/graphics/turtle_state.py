"""
Turtle graphics state for Logo-style drawing
Turtle graphics state management for Time Warp Studio
"""

import math
from dataclasses import dataclass
from typing import Callable, List, Optional, Tuple

# Color name to RGB mapping
COLOR_NAMES = {
    "BLACK": (0, 0, 0),
    "WHITE": (255, 255, 255),
    "RED": (255, 0, 0),
    "GREEN": (0, 255, 0),
    "BLUE": (0, 0, 255),
    "YELLOW": (255, 255, 0),
    "CYAN": (0, 255, 255),
    "MAGENTA": (255, 0, 255),
    "PINK": (255, 192, 203),
    "GRAY": (128, 128, 128),
    "GREY": (128, 128, 128),
}

DEFAULT_PALETTE_16 = {
    0: (0, 0, 0),
    1: (0, 0, 170),
    2: (0, 170, 0),
    3: (0, 170, 170),
    4: (170, 0, 0),
    5: (170, 0, 170),
    6: (170, 85, 0),
    7: (170, 170, 170),
    8: (85, 85, 85),
    9: (85, 85, 255),
    10: (85, 255, 85),
    11: (85, 255, 255),
    12: (255, 85, 85),
    13: (255, 85, 255),
    14: (255, 255, 85),
    15: (255, 255, 255),
}


@dataclass
class TurtleLine:
    """
    A line segment drawn by the turtle

    Represents a single draw operation with start/end points, color, and width.
    Used for rendering and export to image formats.
    """

    start_x: float
    start_y: float
    end_x: float
    end_y: float
    color: Tuple[int, int, int]  # RGB
    width: float


@dataclass
class TurtleShape:
    """Generic shape for higher-level drawing primitives."""

    shape_type: str
    points: List[Tuple[float, float]]
    color: Tuple[int, int, int]
    width: float = 1.0
    fill_color: Optional[Tuple[int, int, int]] = None
    text: Optional[str] = None
    font_size: int = 12
    align: str = "left"


class TurtleState:  # pylint: disable=too-many-instance-attributes
    """
    Turtle graphics state for Logo-style drawing

    Maintains turtle position, heading, pen state, and drawing history.
    Canvas coordinates: (0,0) is center, Y-axis inverted (up is negative).

    Example:
        >>> turtle = TurtleState()
        >>> turtle.forward(100)  # Move forward 100 pixels
        >>> turtle.right(90)     # Turn right 90 degrees
        >>> turtle.forward(100)  # Draw an L shape
    """

    def __init__(self) -> None:
        self.x: float = 0.0
        self.y: float = 0.0
        self.heading: float = 0.0  # degrees, 0 = up (north), 90 = right (east)
        self.pen_down: bool = True
        self.pen_color: Tuple[int, int, int] = (255, 255, 255)  # White
        self.pen_width: float = 2.0
        self.canvas_width: float = 800.0
        self.canvas_height: float = 600.0
        self.lines: List[TurtleLine] = []
        self.shapes: List[TurtleShape] = []
        self.visible: bool = True
        self.bg_color: Tuple[int, int, int] = (10, 10, 20)  # Dark background
        self.on_change: Optional[Callable[[], None]] = None
        self.palette: dict[int, Tuple[int, int, int]] = dict(DEFAULT_PALETTE_16)
        self._last_fillable_shape_index: Optional[int] = None

    def _notify_change(self) -> None:
        """Notify listeners of state change"""
        if self.on_change:
            self.on_change()  # pylint: disable=not-callable

    def forward(self, distance: float) -> None:
        """Move forward, drawing if pen is down"""
        rad = math.radians(self.heading)
        old_x = self.x
        old_y = self.y

        # In standard math/Logo coordinates (0=Up, 90=Right):
        # x += distance * sin(heading)
        # y += distance * cos(heading)
        #
        # The UI canvas (canvas.py) applies a Y-flip (scale(zoom, -zoom)).
        # So we should use standard Cartesian coordinates here.
        #
        # Previous implementation used inverted Y (y -= ...), which assumed
        # screen coordinates (Y down). But since canvas.py flips Y,
        # we should use standard math coordinates (Y up).

        self.x += distance * math.sin(rad)
        self.y += distance * math.cos(rad)

        if self.pen_down:
            self.lines.append(
                TurtleLine(
                    old_x,
                    old_y,
                    self.x,
                    self.y,
                    self.pen_color,
                    self.pen_width,
                )
            )
        self._notify_change()

    def back(self, distance: float) -> None:
        """Move backward"""
        self.forward(-distance)

    def left(self, angle: float) -> None:
        """Turn left (degrees)"""
        self.heading -= angle
        self.heading = self.heading % 360.0
        self._notify_change()

    def right(self, angle: float) -> None:
        """Turn right (degrees)"""
        self.heading += angle
        self.heading = self.heading % 360.0
        self._notify_change()

    def goto(self, x: float, y: float) -> None:
        """Move to absolute position, drawing if pen down"""
        if self.pen_down:
            self.lines.append(
                TurtleLine(
                    self.x,
                    self.y,
                    x,
                    y,
                    self.pen_color,
                    self.pen_width,
                )
            )
        self.x = x
        self.y = y
        self._notify_change()

    def setx(self, x: float) -> None:
        """Set X coordinate, drawing if pen down"""
        self.goto(x, self.y)

    def sety(self, y: float) -> None:
        """Set Y coordinate, drawing if pen down"""
        self.goto(self.x, y)

    def home(self) -> None:
        """Return to center (0,0) and reset heading to face up"""
        self.goto(0.0, 0.0)
        self.heading = 0.0
        self._notify_change()

    def clear(self) -> None:
        """Clear all drawn lines"""
        self.lines.clear()
        self.shapes.clear()
        self._last_fillable_shape_index = None
        self._notify_change()

    def reset(self) -> None:
        """Reset turtle to initial state"""
        self.x = 0.0
        self.y = 0.0
        self.heading = 0.0
        self.pen_down = True
        self.pen_color = (255, 255, 255)
        self.pen_width = 2.0
        self.lines.clear()
        self.shapes.clear()
        self._last_fillable_shape_index = None
        self.visible = True
        self._notify_change()

    def penup(self) -> None:
        """Lift pen (stop drawing)"""
        self.pen_down = False
        self._notify_change()

    def pendown(self) -> None:
        """Lower pen (start drawing)"""
        self.pen_down = True
        self._notify_change()

    def setcolor(self, r: int, g: int, b: int) -> None:
        """Set pen color (RGB 0-255)"""
        self.pen_color = (self._clamp(r), self._clamp(g), self._clamp(b))
        self._notify_change()

    def pencolor(self, color) -> None:
        """Set pen color by name, hex, or RGB tuple"""
        if isinstance(color, str):
            color = color.upper()
            if color in COLOR_NAMES:
                self.pen_color = COLOR_NAMES[color]
            elif color.startswith("#") and len(color) == 7:
                # Hex color like #FF0000
                r = int(color[1:3], 16)
                g = int(color[3:5], 16)
                b = int(color[5:7], 16)
                self.pen_color = (r, g, b)
            else:
                # Try to parse as RGB values separated by spaces
                parts = color.split()
                if len(parts) == 3:
                    try:
                        r, g, b = map(int, parts)
                        self.pen_color = (r, g, b)
                    except ValueError:
                        pass  # Keep current color
        elif isinstance(color, tuple) and len(color) == 3:
            self.pen_color = color
        self._notify_change()

    def setpenwidth(self, width: float) -> None:
        """Set pen width"""
        self.pen_width = width
        self._notify_change()

    def setbgcolor(self, r: int, g: int, b: int) -> None:
        """Set background color (RGB 0-255)"""
        self.bg_color = (self._clamp(r), self._clamp(g), self._clamp(b))
        self._notify_change()

    def setheading(self, angle: float) -> None:
        """Set absolute heading (0 = up, clockwise)"""
        self.heading = angle % 360.0
        self._notify_change()

    def hideturtle(self) -> None:
        """Hide turtle cursor"""
        self.visible = False
        self._notify_change()

    def showturtle(self) -> None:
        """Show turtle cursor"""
        self.visible = True
        self._notify_change()

    def circle(self, radius: float, extent: float = 360.0) -> None:
        """Draw a circle or arc with given radius and extent"""
        if extent == 0:
            return

        # Save current position
        start_x = self.x
        start_y = self.y

        # Calculate number of segments for smooth circle
        num_segments = max(12, int(abs(extent) / 10))
        angle_step = extent / num_segments

        for _ in range(num_segments):
            self.forward(radius * math.radians(angle_step))
            self.right(angle_step)

        # For complete circles, close the path
        if extent >= 360.0:
            self.goto(start_x, start_y)

    def draw_point(
        self,
        x: float,
        y: float,
        color: Optional[Tuple[int, int, int]] = None,
    ) -> None:
        """Draw a single point without moving the turtle."""
        point_color = color or self.pen_color
        self.shapes.append(
            TurtleShape(
                shape_type="point",
                points=[(x, y)],
                color=point_color,
                width=max(1.0, self.pen_width),
            )
        )
        self._notify_change()

    def draw_line(
        self,
        x1: float,
        y1: float,
        x2: float,
        y2: float,
        color: Optional[Tuple[int, int, int]] = None,
        width: Optional[float] = None,
    ) -> None:
        """Draw a line without moving the turtle."""
        line_color = color or self.pen_color
        self.shapes.append(
            TurtleShape(
                shape_type="line",
                points=[(x1, y1), (x2, y2)],
                color=line_color,
                width=width if width is not None else self.pen_width,
            )
        )
        self._notify_change()

    def draw_rect(
        self,
        x1: float,
        y1: float,
        x2: float,
        y2: float,
        color: Optional[Tuple[int, int, int]] = None,
        fill_color: Optional[Tuple[int, int, int]] = None,
        width: Optional[float] = None,
    ) -> None:
        """Draw a rectangle without moving the turtle."""
        rect_color = color or self.pen_color
        points = [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
        self.shapes.append(
            TurtleShape(
                shape_type="rect",
                points=points,
                color=rect_color,
                width=width if width is not None else self.pen_width,
                fill_color=fill_color,
            )
        )
        self._last_fillable_shape_index = len(self.shapes) - 1
        self._notify_change()

    def draw_polygon(
        self,
        points: List[Tuple[float, float]],
        color: Optional[Tuple[int, int, int]] = None,
        fill_color: Optional[Tuple[int, int, int]] = None,
        width: Optional[float] = None,
    ) -> None:
        """Draw a polygon without moving the turtle."""
        if len(points) < 2:
            return
        poly_color = color or self.pen_color
        self.shapes.append(
            TurtleShape(
                shape_type="polygon",
                points=points,
                color=poly_color,
                width=width if width is not None else self.pen_width,
                fill_color=fill_color,
            )
        )
        if len(points) >= 3:
            self._last_fillable_shape_index = len(self.shapes) - 1
        self._notify_change()

    def draw_ellipse(
        self,
        x: float,
        y: float,
        rx: float,
        ry: float,
        color: Optional[Tuple[int, int, int]] = None,
        fill_color: Optional[Tuple[int, int, int]] = None,
        width: Optional[float] = None,
    ) -> None:
        """Draw an ellipse centered at (x, y)."""
        ellipse_color = color or self.pen_color
        self.shapes.append(
            TurtleShape(
                shape_type="ellipse",
                points=[(x, y), (rx, ry)],
                color=ellipse_color,
                width=width if width is not None else self.pen_width,
                fill_color=fill_color,
            )
        )
        self._last_fillable_shape_index = len(self.shapes) - 1
        self._notify_change()

    def draw_arc(
        self,
        x: float,
        y: float,
        radius: float,
        start_angle: float,
        end_angle: float,
        color: Optional[Tuple[int, int, int]] = None,
        width: Optional[float] = None,
        segments: int = 60,
    ) -> None:
        """Draw an arc centered at (x, y)."""
        if radius <= 0:
            return
        if segments < 4:
            segments = 4
        arc_color = color or self.pen_color
        if end_angle < start_angle:
            start_angle, end_angle = end_angle, start_angle
        span = end_angle - start_angle
        steps = max(4, int(abs(span) / 360.0 * segments))
        points = []
        for step in range(steps + 1):
            angle = start_angle + (span * step / steps)
            rad = math.radians(angle)
            px = x + radius * math.cos(rad)
            py = y + radius * math.sin(rad)
            points.append((px, py))
        self.shapes.append(
            TurtleShape(
                shape_type="polyline",
                points=points,
                color=arc_color,
                width=width if width is not None else self.pen_width,
            )
        )
        self._notify_change()

    def draw_text(
        self,
        x: float,
        y: float,
        text: str,
        color: Optional[Tuple[int, int, int]] = None,
        font_size: int = 12,
        align: str = "left",
    ) -> None:
        """Draw text at a given location."""
        if not text:
            return
        text_color = color or self.pen_color
        self.shapes.append(
            TurtleShape(
                shape_type="text",
                points=[(x, y)],
                color=text_color,
                text=text,
                font_size=font_size,
                align=align,
            )
        )
        self._notify_change()

    def fill_last_shape(self, fill_color: Tuple[int, int, int]) -> bool:
        """Apply fill color to the most recent fillable shape."""
        if self._last_fillable_shape_index is None:
            return False
        if self._last_fillable_shape_index >= len(self.shapes):
            return False
        shape = self.shapes[self._last_fillable_shape_index]
        shape.fill_color = fill_color
        self._notify_change()
        return True

    def set_palette_color(self, index: int, r: int, g: int, b: int) -> None:
        """Set a palette index to an RGB value."""
        self.palette[int(index)] = (self._clamp(r), self._clamp(g), self._clamp(b))
        self._notify_change()

    def resolve_color(self, value, default: Optional[Tuple[int, int, int]] = None):
        """Resolve a color value (name, hex, RGB tuple, or palette index)."""
        if value is None:
            return default if default is not None else self.pen_color
        if isinstance(value, (list, tuple)) and len(value) == 3:
            return (self._clamp(value[0]), self._clamp(value[1]), self._clamp(value[2]))
        if isinstance(value, str):
            color = value.strip()
            if color.startswith("#") and len(color) == 7:
                try:
                    r = int(color[1:3], 16)
                    g = int(color[3:5], 16)
                    b = int(color[5:7], 16)
                    return (r, g, b)
                except ValueError:
                    return default if default is not None else self.pen_color
            name = color.upper()
            if name in COLOR_NAMES:
                return COLOR_NAMES[name]
            parts = color.split()
            if len(parts) == 3:
                try:
                    r, g, b = map(int, parts)
                    return (self._clamp(r), self._clamp(g), self._clamp(b))
                except ValueError:
                    return default if default is not None else self.pen_color
            try:
                idx = int(color)
                return self._palette_color(idx)
            except ValueError:
                return default if default is not None else self.pen_color
        if isinstance(value, (int, float)):
            return self._palette_color(int(value))
        return default if default is not None else self.pen_color

    def _palette_color(self, index: int) -> Tuple[int, int, int]:
        if index in self.palette:
            return self.palette[index]
        if 0 <= index <= 255:
            gray = self._clamp(index)
            return (gray, gray, gray)
        return self.pen_color

    @staticmethod
    def _clamp(value) -> int:
        try:
            return max(0, min(255, int(value)))
        except (TypeError, ValueError):
            return 0
