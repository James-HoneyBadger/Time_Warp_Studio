"""Turtle graphics canvas widget."""

# Many Qt widgets use camelCase method names (e.g. paintEvent) which are
# idiomatic in Qt but trigger naming checks; also this widget intentionally
# holds several state attributes for rendering. Silence those specific
# warnings locally.
# pylint: disable=no-name-in-module

import math

from PySide6.QtCore import QPointF, Qt
from PySide6.QtGui import QColor, QMouseEvent, QPainter, QPen, QWheelEvent
from PySide6.QtWidgets import QWidget


class TurtleCanvas(
    QWidget
):  # pylint: disable=invalid-name,too-many-instance-attributes,unused-argument
    """Canvas for rendering turtle graphics."""

    def __init__(self, parent=None):
        super().__init__(parent)

        # Turtle state
        self.turtle = None
        self.lines = []

        # View transform
        self.zoom = 1.0
        self.offset_x = 0.0
        self.offset_y = 0.0

        # Pan state
        self.panning = False
        self.last_pan_pos = None

        # Background color
        self.bg_color = QColor(40, 42, 54)  # Dracula background

        # Minimum size
        self.setMinimumSize(400, 400)

    def set_turtle_state(self, turtle):
        """Set turtle state and repaint."""
        self.turtle = turtle
        self.lines = turtle.lines.copy()
        # Adopt background color from turtle state if available
        try:
            r, g, b = getattr(turtle, "bg_color", (40, 42, 54))
            self.bg_color = QColor(int(r), int(g), int(b))
        except (TypeError, ValueError):
            # Fallback to default theme background
            self.bg_color = QColor(40, 42, 54)
        self.update()

    def clear(self):
        """Clear canvas."""
        self.turtle = None
        self.lines = []
        self.zoom = 1.0
        self.offset_x = 0.0
        self.offset_y = 0.0
        self.update()

    def paintEvent(self, event):  # noqa: ARG002
        """Paint turtle graphics."""
        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing)

        # Fill background
        painter.fillRect(self.rect(), self.bg_color)

        # Get canvas center
        center_x = self.width() / 2
        center_y = self.height() / 2

        # Apply transform
        painter.translate(center_x + self.offset_x, center_y + self.offset_y)
        # Flip Y-axis for math coordinates
        painter.scale(self.zoom, -self.zoom)

        # Draw coordinate axes (light gray)
        pen = QPen(QColor(100, 100, 100), 1)
        pen.setStyle(Qt.DashLine)
        painter.setPen(pen)

        # X axis
        painter.drawLine(-5000, 0, 5000, 0)
        # Y axis
        painter.drawLine(0, -5000, 0, 5000)

        # Draw origin marker
        pen = QPen(QColor(150, 150, 150), 2)
        painter.setPen(pen)
        painter.drawEllipse(QPointF(0, 0), 5, 5)

        # Draw turtle lines
        for line in self.lines:
            color = QColor(line.color[0], line.color[1], line.color[2])
            pen = QPen(color, line.width)
            pen.setCapStyle(Qt.RoundCap)
            pen.setJoinStyle(Qt.RoundJoin)
            painter.setPen(pen)

            painter.drawLine(
                int(line.start_x),
                int(line.start_y),
                int(line.end_x),
                int(line.end_y),
            )

        # Draw turtle cursor if present
        if self.turtle and self.turtle.visible:
            self.draw_turtle_cursor(painter)

    def draw_turtle_cursor(self, painter):  # pylint: disable=too-many-locals
        """Draw turtle cursor."""
        x = self.turtle.x
        y = self.turtle.y
        heading = self.turtle.heading

        # Turtle color (green)
        pen = QPen(QColor(80, 250, 123), 2)
        painter.setPen(pen)

        # Draw triangle pointing in heading direction
        size = 15

        # Convert heading to radians (0° = up = -90° in Qt)
        angle = math.radians(heading - 90)

        # Three points of triangle
        tip_x = x + size * math.cos(angle)
        tip_y = y + size * math.sin(angle)

        left_angle = angle + math.radians(140)
        left_x = x + (size / 2) * math.cos(left_angle)
        left_y = y + (size / 2) * math.sin(left_angle)

        right_angle = angle - math.radians(140)
        right_x = x + (size / 2) * math.cos(right_angle)
        right_y = y + (size / 2) * math.sin(right_angle)

        # Draw triangle
        painter.drawLine(int(tip_x), int(tip_y), int(left_x), int(left_y))
        painter.drawLine(int(left_x), int(left_y), int(right_x), int(right_y))
        painter.drawLine(int(right_x), int(right_y), int(tip_x), int(tip_y))

    def wheelEvent(self, event: QWheelEvent):
        """Handle zoom with mouse wheel."""
        delta = event.angleDelta().y()

        if delta > 0:
            # Zoom in
            self.zoom *= 1.1
        else:
            # Zoom out
            self.zoom /= 1.1

        # Clamp zoom
        self.zoom = max(0.1, min(10.0, self.zoom))

        self.update()

    def mousePressEvent(self, event: QMouseEvent):
        """Start panning."""
        if event.button() == Qt.MiddleButton or (
            event.button() == Qt.LeftButton and event.modifiers() & Qt.ControlModifier
        ):
            self.panning = True
            self.last_pan_pos = event.pos()
            self.setCursor(Qt.ClosedHandCursor)

    def mouseMoveEvent(self, event: QMouseEvent):
        """Handle panning."""
        if self.panning and self.last_pan_pos:
            delta = event.pos() - self.last_pan_pos
            self.offset_x += delta.x()
            self.offset_y += delta.y()
            self.last_pan_pos = event.pos()
            self.update()

    def mouseReleaseEvent(self, event: QMouseEvent):
        """Stop panning."""
        if event.button() == Qt.MiddleButton or event.button() == Qt.LeftButton:
            self.panning = False
            self.last_pan_pos = None
            self.setCursor(Qt.ArrowCursor)

    def reset_view(self):
        """Reset zoom and pan."""
        self.zoom = 1.0
        self.offset_x = 0.0
        self.offset_y = 0.0
        self.update()
