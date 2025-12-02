"""
Shape library for Time Warp IDE turtle graphics.
Provides pre-built shapes for easy drawing.
"""

import math
from dataclasses import dataclass
from typing import TYPE_CHECKING, List, Tuple

if TYPE_CHECKING:
    from ..graphics.turtle_state import TurtleState


@dataclass
class ShapeCommand:
    """A single drawing command for a shape."""

    action: (
        str  # 'move', 'draw', 'pen_up', 'pen_down', 'color', 'fill_start', 'fill_end'
    )
    x: float = 0.0
    y: float = 0.0
    color: str = ""


def generate_polygon(
    sides: int, size: float, cx: float = 0, cy: float = 0
) -> List[Tuple[float, float]]:
    """Generate vertices for a regular polygon.

    Args:
        sides: Number of sides (3=triangle, 4=square, etc.)
        size: Radius from center to vertex
        cx, cy: Center coordinates

    Returns:
        List of (x, y) vertex coordinates
    """
    points = []
    angle_step = 2 * math.pi / sides
    # Start at top
    start_angle = -math.pi / 2

    for i in range(sides):
        angle = start_angle + i * angle_step
        x = cx + size * math.cos(angle)
        y = cy + size * math.sin(angle)
        points.append((x, y))

    return points


def generate_star(
    points: int, outer_radius: float, inner_radius: float, cx: float = 0, cy: float = 0
) -> List[Tuple[float, float]]:
    """Generate vertices for a star shape.

    Args:
        points: Number of star points
        outer_radius: Radius to outer points
        inner_radius: Radius to inner points
        cx, cy: Center coordinates

    Returns:
        List of (x, y) vertex coordinates
    """
    vertices = []
    angle_step = math.pi / points  # Half step for alternating
    start_angle = -math.pi / 2

    for i in range(points * 2):
        angle = start_angle + i * angle_step
        radius = outer_radius if i % 2 == 0 else inner_radius
        x = cx + radius * math.cos(angle)
        y = cy + radius * math.sin(angle)
        vertices.append((x, y))

    return vertices


def generate_heart(
    size: float, cx: float = 0, cy: float = 0
) -> List[Tuple[float, float]]:
    """Generate vertices for a heart shape.

    Args:
        size: Overall size of the heart
        cx, cy: Center coordinates

    Returns:
        List of (x, y) vertex coordinates
    """
    points = []
    # Heart parametric equation
    for i in range(100):
        t = i * 2 * math.pi / 100
        x = 16 * (math.sin(t) ** 3)
        y = (
            13 * math.cos(t)
            - 5 * math.cos(2 * t)
            - 2 * math.cos(3 * t)
            - math.cos(4 * t)
        )
        # Scale and translate
        x = cx + x * size / 20
        y = cy - y * size / 20  # Flip Y for screen coords
        points.append((x, y))

    return points


def generate_arrow(
    length: float, width: float, cx: float = 0, cy: float = 0, direction: float = 0
) -> List[Tuple[float, float]]:
    """Generate vertices for an arrow shape.

    Args:
        length: Length of the arrow
        width: Width of the arrow head
        cx, cy: Base coordinates
        direction: Angle in degrees (0 = right, 90 = up)

    Returns:
        List of (x, y) vertex coordinates
    """
    # Arrow pointing right at origin
    hw = width / 2
    shaft_width = width / 4
    head_length = length / 3

    raw_points = [
        (0, shaft_width),  # Shaft bottom left
        (length - head_length, shaft_width),  # Shaft bottom right
        (length - head_length, hw),  # Head bottom
        (length, 0),  # Tip
        (length - head_length, -hw),  # Head top
        (length - head_length, -shaft_width),  # Shaft top right
        (0, -shaft_width),  # Shaft top left
    ]

    # Rotate and translate
    rad = math.radians(direction)
    cos_a, sin_a = math.cos(rad), math.sin(rad)

    points = []
    for px, py in raw_points:
        rx = px * cos_a - py * sin_a + cx
        ry = px * sin_a + py * cos_a + cy
        points.append((rx, ry))

    return points


def generate_spiral(
    turns: float, start_radius: float, end_radius: float, cx: float = 0, cy: float = 0
) -> List[Tuple[float, float]]:
    """Generate vertices for a spiral shape.

    Args:
        turns: Number of spiral turns
        start_radius: Starting radius
        end_radius: Ending radius
        cx, cy: Center coordinates

    Returns:
        List of (x, y) vertex coordinates
    """
    points = []
    steps = int(turns * 36)  # 36 points per turn

    for i in range(steps + 1):
        t = i / steps
        angle = t * turns * 2 * math.pi
        radius = start_radius + t * (end_radius - start_radius)
        x = cx + radius * math.cos(angle)
        y = cy + radius * math.sin(angle)
        points.append((x, y))

    return points


def generate_gear(
    teeth: int, outer_radius: float, inner_radius: float, cx: float = 0, cy: float = 0
) -> List[Tuple[float, float]]:
    """Generate vertices for a gear shape.

    Args:
        teeth: Number of gear teeth
        outer_radius: Radius to tooth tips
        inner_radius: Radius to tooth valleys
        cx, cy: Center coordinates

    Returns:
        List of (x, y) vertex coordinates
    """
    points = []
    tooth_angle = 2 * math.pi / teeth

    for i in range(teeth):
        base_angle = i * tooth_angle
        # Each tooth has 4 points
        for j, (offset, radius) in enumerate(
            [
                (0, inner_radius),
                (tooth_angle * 0.2, outer_radius),
                (tooth_angle * 0.5, outer_radius),
                (tooth_angle * 0.7, inner_radius),
            ]
        ):
            angle = base_angle + offset
            x = cx + radius * math.cos(angle)
            y = cy + radius * math.sin(angle)
            points.append((x, y))

    return points


def generate_cross(
    size: float, thickness: float, cx: float = 0, cy: float = 0
) -> List[Tuple[float, float]]:
    """Generate vertices for a cross/plus shape.

    Args:
        size: Overall size (width and height)
        thickness: Thickness of the cross arms
        cx, cy: Center coordinates

    Returns:
        List of (x, y) vertex coordinates
    """
    hs = size / 2  # half size
    ht = thickness / 2  # half thickness

    # 12-point cross shape
    points = [
        (cx - ht, cy - hs),  # Top arm left
        (cx + ht, cy - hs),  # Top arm right
        (cx + ht, cy - ht),  # Inner top right
        (cx + hs, cy - ht),  # Right arm top
        (cx + hs, cy + ht),  # Right arm bottom
        (cx + ht, cy + ht),  # Inner bottom right
        (cx + ht, cy + hs),  # Bottom arm right
        (cx - ht, cy + hs),  # Bottom arm left
        (cx - ht, cy + ht),  # Inner bottom left
        (cx - hs, cy + ht),  # Left arm bottom
        (cx - hs, cy - ht),  # Left arm top
        (cx - ht, cy - ht),  # Inner top left
    ]

    return points


def generate_diamond(
    width: float, height: float, cx: float = 0, cy: float = 0
) -> List[Tuple[float, float]]:
    """Generate vertices for a diamond shape.

    Args:
        width: Width of the diamond
        height: Height of the diamond
        cx, cy: Center coordinates

    Returns:
        List of (x, y) vertex coordinates
    """
    hw, hh = width / 2, height / 2
    return [
        (cx, cy - hh),  # Top
        (cx + hw, cy),  # Right
        (cx, cy + hh),  # Bottom
        (cx - hw, cy),  # Left
    ]


def generate_crescent(
    outer_radius: float,
    inner_radius: float,
    offset: float,
    cx: float = 0,
    cy: float = 0,
) -> List[Tuple[float, float]]:
    """Generate vertices for a crescent moon shape.

    Args:
        outer_radius: Outer circle radius
        inner_radius: Inner circle radius
        offset: Horizontal offset of inner circle
        cx, cy: Center coordinates

    Returns:
        List of (x, y) vertex coordinates
    """
    points = []

    # Outer arc (right side)
    for i in range(50):
        angle = -math.pi / 2 + i * math.pi / 49
        x = cx + outer_radius * math.cos(angle)
        y = cy + outer_radius * math.sin(angle)
        points.append((x, y))

    # Inner arc (reversed)
    for i in range(50):
        angle = math.pi / 2 - i * math.pi / 49
        x = cx + offset + inner_radius * math.cos(angle)
        y = cy + inner_radius * math.sin(angle)
        points.append((x, y))

    return points


class ShapeLibrary:
    """Library of pre-built shapes for turtle graphics."""

    @staticmethod
    def draw_polygon(
        turtle: "TurtleState", sides: int, size: float, fill: bool = False
    ) -> str:
        """Draw a regular polygon.

        Args:
            turtle: TurtleState instance
            sides: Number of sides
            size: Size (radius)
            fill: Whether to fill the shape
        """
        if sides < 3:
            return "❌ Polygon needs at least 3 sides\n"

        cx, cy = turtle.x, turtle.y
        points = generate_polygon(sides, size, cx, cy)

        if fill:
            turtle.begin_fill()

        # Move to first point
        turtle.penup()
        turtle.goto(points[0][0], points[0][1])
        turtle.pendown()

        # Draw sides
        for px, py in points[1:]:
            turtle.goto(px, py)
        turtle.goto(points[0][0], points[0][1])  # Close shape

        if fill:
            turtle.end_fill()

        # Return to center
        turtle.penup()
        turtle.goto(cx, cy)
        turtle.pendown()

        return f"🔷 Drew {sides}-sided polygon\n"

    @staticmethod
    def draw_star(
        turtle: "TurtleState",
        points: int,
        outer_size: float,
        inner_size: float = None,
        fill: bool = False,
    ) -> str:
        """Draw a star shape.

        Args:
            turtle: TurtleState instance
            points: Number of star points
            outer_size: Outer radius
            inner_size: Inner radius (default: outer_size * 0.4)
            fill: Whether to fill the shape
        """
        if points < 3:
            return "❌ Star needs at least 3 points\n"

        if inner_size is None:
            inner_size = outer_size * 0.4

        cx, cy = turtle.x, turtle.y
        vertices = generate_star(points, outer_size, inner_size, cx, cy)

        if fill:
            turtle.begin_fill()

        turtle.penup()
        turtle.goto(vertices[0][0], vertices[0][1])
        turtle.pendown()

        for px, py in vertices[1:]:
            turtle.goto(px, py)
        turtle.goto(vertices[0][0], vertices[0][1])

        if fill:
            turtle.end_fill()

        turtle.penup()
        turtle.goto(cx, cy)
        turtle.pendown()

        return f"⭐ Drew {points}-pointed star\n"

    @staticmethod
    def draw_heart(turtle: "TurtleState", size: float, fill: bool = False) -> str:
        """Draw a heart shape."""
        cx, cy = turtle.x, turtle.y
        points = generate_heart(size, cx, cy)

        if fill:
            turtle.begin_fill()

        turtle.penup()
        turtle.goto(points[0][0], points[0][1])
        turtle.pendown()

        for px, py in points[1:]:
            turtle.goto(px, py)
        turtle.goto(points[0][0], points[0][1])

        if fill:
            turtle.end_fill()

        turtle.penup()
        turtle.goto(cx, cy)
        turtle.pendown()

        return "❤️ Drew heart\n"

    @staticmethod
    def draw_arrow(
        turtle: "TurtleState",
        length: float,
        width: float = None,
        fill: bool = False,
    ) -> str:
        """Draw an arrow in the turtle's current direction."""
        if width is None:
            width = length / 3

        cx, cy = turtle.x, turtle.y
        direction = turtle.angle
        points = generate_arrow(length, width, cx, cy, direction)

        if fill:
            turtle.begin_fill()

        turtle.penup()
        turtle.goto(points[0][0], points[0][1])
        turtle.pendown()

        for px, py in points[1:]:
            turtle.goto(px, py)
        turtle.goto(points[0][0], points[0][1])

        if fill:
            turtle.end_fill()

        turtle.penup()
        turtle.goto(cx, cy)
        turtle.pendown()

        return "➡️ Drew arrow\n"

    @staticmethod
    def draw_spiral(
        turtle: "TurtleState",
        turns: float,
        start_size: float,
        end_size: float,
    ) -> str:
        """Draw a spiral."""
        cx, cy = turtle.x, turtle.y
        points = generate_spiral(turns, start_size, end_size, cx, cy)

        turtle.penup()
        turtle.goto(points[0][0], points[0][1])
        turtle.pendown()

        for px, py in points[1:]:
            turtle.goto(px, py)

        return f"🌀 Drew spiral ({turns} turns)\n"

    @staticmethod
    def draw_gear(
        turtle: "TurtleState",
        teeth: int,
        outer_size: float,
        inner_size: float = None,
        fill: bool = False,
    ) -> str:
        """Draw a gear shape."""
        if inner_size is None:
            inner_size = outer_size * 0.7

        cx, cy = turtle.x, turtle.y
        points = generate_gear(teeth, outer_size, inner_size, cx, cy)

        if fill:
            turtle.begin_fill()

        turtle.penup()
        turtle.goto(points[0][0], points[0][1])
        turtle.pendown()

        for px, py in points[1:]:
            turtle.goto(px, py)
        turtle.goto(points[0][0], points[0][1])

        if fill:
            turtle.end_fill()

        turtle.penup()
        turtle.goto(cx, cy)
        turtle.pendown()

        return f"⚙️ Drew gear ({teeth} teeth)\n"

    @staticmethod
    def draw_cross(
        turtle: "TurtleState",
        size: float,
        thickness: float = None,
        fill: bool = False,
    ) -> str:
        """Draw a cross/plus shape."""
        if thickness is None:
            thickness = size / 3

        cx, cy = turtle.x, turtle.y
        points = generate_cross(size, thickness, cx, cy)

        if fill:
            turtle.begin_fill()

        turtle.penup()
        turtle.goto(points[0][0], points[0][1])
        turtle.pendown()

        for px, py in points[1:]:
            turtle.goto(px, py)
        turtle.goto(points[0][0], points[0][1])

        if fill:
            turtle.end_fill()

        turtle.penup()
        turtle.goto(cx, cy)
        turtle.pendown()

        return "✚ Drew cross\n"

    @staticmethod
    def draw_diamond(
        turtle: "TurtleState",
        width: float,
        height: float = None,
        fill: bool = False,
    ) -> str:
        """Draw a diamond shape."""
        if height is None:
            height = width * 1.5

        cx, cy = turtle.x, turtle.y
        points = generate_diamond(width, height, cx, cy)

        if fill:
            turtle.begin_fill()

        turtle.penup()
        turtle.goto(points[0][0], points[0][1])
        turtle.pendown()

        for px, py in points[1:]:
            turtle.goto(px, py)
        turtle.goto(points[0][0], points[0][1])

        if fill:
            turtle.end_fill()

        turtle.penup()
        turtle.goto(cx, cy)
        turtle.pendown()

        return "💎 Drew diamond\n"

    @staticmethod
    def list_shapes() -> List[str]:
        """List all available shape names."""
        return [
            "polygon",
            "star",
            "heart",
            "arrow",
            "spiral",
            "gear",
            "cross",
            "diamond",
        ]


# Global instance
_shape_library: ShapeLibrary = None


def get_shape_library() -> ShapeLibrary:
    """Get the global shape library instance."""
    global _shape_library  # pylint: disable=global-statement
    if _shape_library is None:
        _shape_library = ShapeLibrary()
    return _shape_library
