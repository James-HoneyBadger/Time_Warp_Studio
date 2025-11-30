"""
Turtle graphics state for Logo-style drawing
Ported from Rust: time_warp_unified::graphics::mod.rs
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

    def __init__(self):
        self.x: float = 0.0
        self.y: float = 0.0
        self.heading: float = 0.0  # degrees, 0 = up
        self.pen_down: bool = True
        self.pen_color: Tuple[int, int, int] = (255, 255, 255)  # White
        self.pen_width: float = 2.0
        self.canvas_width: float = 800.0
        self.canvas_height: float = 600.0
        self.lines: List[TurtleLine] = []
        self.visible: bool = True
        self.bg_color: Tuple[int, int, int] = (10, 10, 20)  # Dark background
        self.on_change: Optional[Callable[[], None]] = None

    def _notify_change(self):
        """Notify listeners of state change"""
        if self.on_change:
            self.on_change()  # pylint: disable=not-callable

    def forward(self, distance: float):
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

    def back(self, distance: float):
        """Move backward"""
        self.forward(-distance)

    def left(self, angle: float):
        """Turn left (degrees)"""
        self.heading -= angle
        self.heading = self.heading % 360.0
        self._notify_change()

    def right(self, angle: float):
        """Turn right (degrees)"""
        self.heading += angle
        self.heading = self.heading % 360.0
        self._notify_change()

    def goto(self, x: float, y: float):
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

    def setx(self, x: float):
        """Set X coordinate, drawing if pen down"""
        self.goto(x, self.y)

    def sety(self, y: float):
        """Set Y coordinate, drawing if pen down"""
        self.goto(self.x, y)

    def home(self):
        """Return to center (0,0) and reset heading"""
        self.goto(0.0, 0.0)
        self.heading = 0.0
        self._notify_change()

    def clear(self):
        """Clear all drawn lines"""
        self.lines.clear()
        self._notify_change()

    def reset(self):
        """Reset turtle to initial state"""
        self.x = 0.0
        self.y = 0.0
        self.heading = 0.0
        self.pen_down = True
        self.pen_color = (255, 255, 255)
        self.pen_width = 2.0
        self.lines.clear()
        self.visible = True
        self._notify_change()

    def penup(self):
        """Lift pen (stop drawing)"""
        self.pen_down = False
        self._notify_change()

    def pendown(self):
        """Lower pen (start drawing)"""
        self.pen_down = True
        self._notify_change()

    def setcolor(self, r: int, g: int, b: int):
        """Set pen color (RGB 0-255)"""
        self.pen_color = (r, g, b)
        self._notify_change()

    def pencolor(self, color):
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

    def setpenwidth(self, width: float):
        """Set pen width"""
        self.pen_width = width
        self._notify_change()

    def setbgcolor(self, r: int, g: int, b: int):
        """Set background color (RGB 0-255)"""
        self.bg_color = (r, g, b)
        self._notify_change()

    def setheading(self, angle: float):
        """Set absolute heading (0 = up, clockwise)"""
        self.heading = angle % 360.0
        self._notify_change()

    def hideturtle(self):
        """Hide turtle cursor"""
        self.visible = False
        self._notify_change()

    def showturtle(self):
        """Show turtle cursor"""
        self.visible = True
        self._notify_change()

    def circle(self, radius: float, extent: float = 360.0):
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
