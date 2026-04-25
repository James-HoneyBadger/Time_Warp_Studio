"""
Turtle graphics state management for Time Warp Studio.

Manages turtle position, heading, pen state, and drawing commands
for Logo-style graphics across all 24 language executors.
"""

import math
from dataclasses import dataclass, field
from typing import Callable, Dict, List, Optional, Tuple

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
    "ORANGE": (255, 165, 0),
    "PURPLE": (128, 0, 128),
    "BROWN": (139, 69, 19),
    "LIME": (0, 255, 0),
    "NAVY": (0, 0, 128),
    "TEAL": (0, 128, 128),
    "MAROON": (128, 0, 0),
    "OLIVE": (128, 128, 0),
    "SILVER": (192, 192, 192),
    "GOLD": (255, 215, 0),
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

# Optimization: Precompute sine and cosine values for common angles
PRECOMPUTED_TRIG = {angle: (math.sin(math.radians(angle)), math.cos(math.radians(angle))) for angle in range(0, 360)}


def optimized_sin(angle: float) -> float:
    """Retrieve precomputed sine value for an angle."""
    return PRECOMPUTED_TRIG[int(angle) % 360][0]


def optimized_cos(angle: float) -> float:
    """Retrieve precomputed cosine value for an angle."""
    return PRECOMPUTED_TRIG[int(angle) % 360][1]


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
class TurtleGradient:
    """Gradient descriptor for shape fills."""

    kind: str  # "linear" or "radial"
    stops: List[Tuple[float, Tuple[int, int, int]]]  # (position 0..1, RGB)
    # Linear: start/end points in turtle coords
    x1: float = 0.0
    y1: float = 0.0
    x2: float = 100.0
    y2: float = 0.0
    # Radial: centre + radius
    cx: float = 0.0
    cy: float = 0.0
    radius: float = 100.0


@dataclass
class TurtleSprite:
    """A named, reusable bitmap sprite defined as a pixel grid."""

    name: str
    width: int
    height: int
    # pixels[row][col] = (r, g, b) or None (transparent)
    pixels: List[List[Optional[Tuple[int, int, int]]]]
    hotspot_x: int = 0   # pivot point (pixels from left)
    hotspot_y: int = 0   # pivot point (pixels from top)


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
    # Vector / SVGA extensions
    gradient: Optional[TurtleGradient] = None      # gradient fill (overrides fill_color)
    z_order: int = 0                                # painter order (lower = behind)
    rotation: float = 0.0                           # degrees (for sprites)
    scale_x: float = 1.0
    scale_y: float = 1.0
    sprite_name: Optional[str] = None              # reference into TurtleState.sprites
    pen_dash: Optional[List[float]] = None          # dash pattern [on, off, ...]
    pen_cap: str = "round"                          # "round", "flat", "square"
    pen_join: str = "round"                         # "round", "miter", "bevel"
    control_points: Optional[List[Tuple[float, float]]] = None  # for bezier


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
        self._filling: bool = False
        self._fill_start_line_index: int = 0
        # SVGA / sprite extensions
        self.svga_mode: bool = False           # True = 800×600 virtual canvas
        self.svga_width: int = 800
        self.svga_height: int = 600
        self.sprites: dict[str, TurtleSprite] = {}         # sprite definitions
        self._pen_dash: Optional[List[float]] = None
        self._pen_cap: str = "round"
        self._pen_join: str = "round"

    @property
    def angle(self) -> float:
        """Alias for heading (compatibility with standard turtle interface)."""
        return self.heading

    @angle.setter
    def angle(self, value: float) -> None:
        """Set heading via angle alias."""
        self.heading = value % 360.0

    def begin_fill(self) -> None:
        """Begin recording lines for fill. Call end_fill() to fill the shape."""
        self._filling = True
        self._fill_start_line_index = len(self.lines)

    def end_fill(self) -> None:
        """End fill recording and create a filled polygon from recorded lines."""
        if not self._filling:
            return
        self._filling = False
        # Collect points from lines drawn since begin_fill
        fill_lines = self.lines[self._fill_start_line_index :]
        if not fill_lines:
            return
        points: List[Tuple[float, float]] = [
            (fill_lines[0].start_x, fill_lines[0].start_y)
        ]
        for line in fill_lines:
            points.append((line.end_x, line.end_y))
        if len(points) >= 3:
            self.shapes.append(
                TurtleShape(
                    shape_type="polygon",
                    points=points,
                    color=self.pen_color,
                    width=self.pen_width,
                    fill_color=self.pen_color,
                )
            )
            self._last_fillable_shape_index = len(self.shapes) - 1
            self._notify_change()

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

    # ------------------------------------------------------------------
    # SVGA resolution mode
    # ------------------------------------------------------------------

    def set_svga_mode(self, width: int = 800, height: int = 600) -> None:
        """Enable Super-VGA virtual canvas mode.

        In this mode the canvas renders a fixed-resolution virtual screen
        (default 800×600) with full anti-aliasing and crisp pixel mapping.
        The turtle coordinate origin (0,0) maps to the centre of the screen.
        """
        self.svga_mode = True
        self.svga_width = max(320, int(width))
        self.svga_height = max(200, int(height))
        self.canvas_width = float(self.svga_width)
        self.canvas_height = float(self.svga_height)
        self._notify_change()

    def clear_svga_mode(self) -> None:
        """Return to unlimited-canvas (logical-coordinate) mode."""
        self.svga_mode = False
        self._notify_change()

    # ------------------------------------------------------------------
    # Pen style helpers
    # ------------------------------------------------------------------

    def set_pen_style(
        self,
        dash: Optional[List[float]] = None,
        cap: str = "round",
        join: str = "round",
    ) -> None:
        """Set advanced pen styling.

        Args:
            dash: Dash pattern as [on_len, off_len, ...] or None for solid.
            cap:  Line cap style — 'round', 'flat', or 'square'.
            join: Line join style — 'round', 'miter', or 'bevel'.
        """
        self._pen_dash = dash
        self._pen_cap = cap if cap in ("round", "flat", "square") else "round"
        self._pen_join = join if join in ("round", "miter", "bevel") else "round"
        self._notify_change()

    # ------------------------------------------------------------------
    # Vector drawing: Bezier curves
    # ------------------------------------------------------------------

    def bezier_curve(
        self,
        cp1x: float, cp1y: float,
        cp2x: float = 0.0, cp2y: float = 0.0,
        end_x: float = 0.0, end_y: float = 0.0,
        color: Optional[Tuple[int, int, int]] = None,
        width: Optional[float] = None,
    ) -> None:
        """Draw a cubic Bezier curve from the current turtle position.

        For a quadratic curve supply only cp1x/cp1y as the single control
        point and set cp2x/cp2y equal to end_x/end_y.

        Args:
            cp1x, cp1y: First control point.
            cp2x, cp2y: Second control point (same as end for quadratic).
            end_x, end_y: End point.
            color: Stroke colour (defaults to pen_color).
            width: Stroke width (defaults to pen_width).
        """
        bc = color or self.pen_color
        bw = width if width is not None else self.pen_width
        self.shapes.append(
            TurtleShape(
                shape_type="bezier",
                points=[(self.x, self.y), (end_x, end_y)],
                color=bc,
                width=bw,
                control_points=[(cp1x, cp1y), (cp2x, cp2y)],
                pen_dash=self._pen_dash,
                pen_cap=self._pen_cap,
                pen_join=self._pen_join,
            )
        )
        if self.pen_down:
            # Also advance turtle position to end point
            self.x = end_x
            self.y = end_y
        self._notify_change()

    # ------------------------------------------------------------------
    # Vector drawing: gradient fills
    # ------------------------------------------------------------------

    def draw_rect_gradient(
        self,
        x1: float, y1: float, x2: float, y2: float,
        stops: List[Tuple[float, Tuple[int, int, int]]],
        kind: str = "linear",
        border_color: Optional[Tuple[int, int, int]] = None,
        width: Optional[float] = None,
    ) -> None:
        """Draw a rectangle with a gradient fill.

        Args:
            x1, y1, x2, y2: Rectangle corners.
            stops: List of (position, rgb) pairs, e.g. [(0.0, (255,0,0)), (1.0, (0,0,255))].
            kind: 'linear' (left→right) or 'radial' (centre outward).
            border_color: Outline colour (None = no outline).
            width: Outline width.
        """
        grad = TurtleGradient(
            kind=kind,
            stops=stops,
            x1=x1, y1=y1, x2=x2, y2=y2,
            cx=(x1 + x2) / 2, cy=(y1 + y2) / 2,
            radius=max(abs(x2 - x1), abs(y2 - y1)) / 2,
        )
        points = [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
        bc = border_color or self.pen_color
        bw = width if width is not None else self.pen_width
        self.shapes.append(
            TurtleShape(
                shape_type="rect_gradient",
                points=points,
                color=bc,
                width=bw if border_color else 0.0,
                gradient=grad,
            )
        )
        self._last_fillable_shape_index = len(self.shapes) - 1
        self._notify_change()

    def draw_ellipse_gradient(
        self,
        cx: float, cy: float, rx: float, ry: float,
        stops: List[Tuple[float, Tuple[int, int, int]]],
        border_color: Optional[Tuple[int, int, int]] = None,
        width: Optional[float] = None,
    ) -> None:
        """Draw an ellipse with a radial gradient fill."""
        grad = TurtleGradient(
            kind="radial",
            stops=stops,
            cx=cx, cy=cy, radius=max(rx, ry),
        )
        bc = border_color or self.pen_color
        bw = width if width is not None else self.pen_width
        self.shapes.append(
            TurtleShape(
                shape_type="ellipse_gradient",
                points=[(cx, cy), (rx, ry)],
                color=bc,
                width=bw if border_color else 0.0,
                gradient=grad,
            )
        )
        self._last_fillable_shape_index = len(self.shapes) - 1
        self._notify_change()

    # ------------------------------------------------------------------
    # Sprite management
    # ------------------------------------------------------------------

    def define_sprite(
        self,
        name: str,
        pixel_rows: List[str],
        palette: Optional[Dict[str, Tuple[int, int, int]]] = None,
        transparent_char: str = ".",
        hotspot_x: int = 0,
        hotspot_y: int = 0,
    ) -> None:
        """Define a named sprite from a list of string rows.

        Each character in a row maps to a colour via *palette*.  '.' (or
        *transparent_char*) means transparent.

        Example::

            turtle.define_sprite("ship", [
                "..W..",
                ".WWW.",
                "WWWWW",
                ".W.W.",
            ], palette={"W": (255, 255, 255)})
        """
        pal = palette or {}
        height = len(pixel_rows)
        width = max((len(r) for r in pixel_rows), default=0)
        pixels: List[List[Optional[Tuple[int, int, int]]]] = []
        for row_str in pixel_rows:
            row: List[Optional[Tuple[int, int, int]]] = []
            for ch in row_str.ljust(width):
                if ch == transparent_char:
                    row.append(None)
                elif ch in pal:
                    row.append(pal[ch])
                else:
                    # Try to look up in COLOR_NAMES by single-char shorthand
                    row.append(None)
            pixels.append(row)
        self.sprites[name] = TurtleSprite(
            name=name,
            width=width,
            height=height,
            pixels=pixels,
            hotspot_x=hotspot_x,
            hotspot_y=hotspot_y,
        )

    def place_sprite(
        self,
        name: str,
        x: float,
        y: float,
        scale: float = 1.0,
        rotation: float = 0.0,
        z_order: int = 0,
        scale_x: float = 1.0,
        scale_y: float = 1.0,
    ) -> None:
        """Place a sprite on the canvas.

        Args:
            name:     Sprite name (must have been defined via define_sprite).
            x, y:     Position in turtle coordinates.
            scale:    Uniform scale multiplier (shorthand; overridden by scale_x/y).
            rotation: Rotation in degrees (clockwise).
            z_order:  Painting order; higher numbers paint on top.
            scale_x:  Horizontal scale (default = scale).
            scale_y:  Vertical scale (default = scale).
        """
        if scale != 1.0:
            scale_x = scale_x if scale_x != 1.0 else scale
            scale_y = scale_y if scale_y != 1.0 else scale
        self.shapes.append(
            TurtleShape(
                shape_type="sprite",
                points=[(x, y)],
                color=self.pen_color,
                width=0.0,
                sprite_name=name,
                rotation=rotation,
                scale_x=scale_x,
                scale_y=scale_y,
                z_order=z_order,
            )
        )
        self._notify_change()

    def move_sprite(self, name: str, dx: float, dy: float) -> None:
        """Translate the *last* placed instance of a named sprite."""
        for shape in reversed(self.shapes):
            if shape.shape_type == "sprite" and shape.sprite_name == name:
                x, y = shape.points[0]
                shape.points[0] = (x + dx, y + dy)
                self._notify_change()
                return

