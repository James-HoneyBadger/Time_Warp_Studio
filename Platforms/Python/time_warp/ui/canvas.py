"""Turtle graphics canvas widget."""

# Many Qt widgets use camelCase method names (e.g. paintEvent) which are
# idiomatic in Qt but trigger naming checks; also this widget intentionally
# holds several state attributes for rendering. Silence those specific
# warnings locally.
# pylint: disable=no-name-in-module

import math

from PySide6.QtCore import QPointF, QRectF, Qt
from PySide6.QtGui import (
    QColor,
    QFont,
    QImage,
    QMouseEvent,
    QPainter,
    QPen,
    QWheelEvent,
)
from PySide6.QtWidgets import QWidget

from .screen_modes import ModeType, ScreenMode, ScreenModeManager


class TurtleCanvas(
    QWidget
):  # pylint: disable=invalid-name,too-many-instance-attributes,unused-argument
    """Canvas for rendering turtle graphics with retro screen mode support."""

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

        # Screen mode support
        self.screen_mode_manager = ScreenModeManager()
        self.screen_mode_enabled = False  # When True, simulate retro resolution

        # Minimum size
        self.setMinimumSize(400, 400)

    def set_screen_mode(self, mode: ScreenMode):
        """Set the current screen mode for rendering."""
        self.screen_mode_manager.set_mode(mode.mode_number)
        self.screen_mode_enabled = True
        self.update()

    def set_screen_mode_enabled(self, enabled: bool):
        """Enable or disable screen mode simulation."""
        self.screen_mode_enabled = enabled
        self.update()

    def set_turtle_state(self, turtle):
        """Set turtle state and repaint."""
        # DEBUG: Log state updates
        import sys
        print(f"[CANVAS] set_turtle_state: {len(turtle.lines)} lines", file=sys.stderr)
        
        self.turtle = turtle
        self.lines = turtle.lines.copy()
        print(
            f"[CANVAS] Copied lines: self.lines now has {len(self.lines)} lines",
            file=sys.stderr,
        )
        
        # Adopt background color from turtle state if available
        try:
            r, g, b = getattr(turtle, "bg_color", (40, 42, 54))
            self.bg_color = QColor(int(r), int(g), int(b))
        except (TypeError, ValueError):
            # Fallback to default theme background
            self.bg_color = QColor(40, 42, 54)
        
        print("[CANVAS] Calling self.update()", file=sys.stderr)
        # Use repaint() instead of update() to force immediate redraw
        # update() schedules a paint event, but doesn't guarantee immediate rendering
        # repaint() forces immediate rendering, which is more reliable for graphics
        self.repaint()
        print("[CANVAS] repaint() called", file=sys.stderr)

    def clear(self):
        """Clear canvas."""
        self.turtle = None
        self.lines = []
        self.zoom = 1.0
        self.offset_x = 0.0
        self.offset_y = 0.0
        # Use repaint() for immediate clearing
        self.repaint()

    def paintEvent(self, _event):
        """Paint turtle graphics."""
        painter = QPainter(self)

        # DEBUG: Log paint events
        import sys
        if len(self.lines) > 0:
            print(
                f"[CANVAS] paintEvent: {len(self.lines)} lines, zoom={self.zoom}",
                file=sys.stderr,
            )

        if self.screen_mode_enabled:
            self._paint_retro_mode(painter)
        else:
            self._paint_normal_mode(painter)

    def _paint_normal_mode(self, painter: QPainter):
        """Paint in normal high-resolution mode."""
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

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
        pen.setStyle(Qt.PenStyle.DashLine)
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
            # Adjust pen width inversely to zoom so it stays visible at all zoom levels
            adjusted_width = (
                line.width / max(self.zoom, 0.1) if self.zoom != 0 else line.width
            )
            pen = QPen(color, adjusted_width)
            pen.setCapStyle(Qt.PenCapStyle.RoundCap)
            pen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)
            painter.setPen(pen)

            painter.drawLine(
                int(line.start_x),
                int(line.start_y),
                int(line.end_x),
                int(line.end_y),
            )

        # Draw turtle cursor if present
        if self.turtle and self.turtle.visible:
            self._draw_turtle_cursor(painter)

    def _paint_retro_mode(self, painter: QPainter):
        """Paint with retro screen mode simulation (pixelated)."""
        mode = self.screen_mode_manager.get_current_mode()

        # Disable antialiasing for pixelated look
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, False)

        # Fill background
        painter.fillRect(self.rect(), self.bg_color)

        # Calculate virtual screen dimensions
        if mode.mode_type == ModeType.TEXT:
            virtual_w = mode.width * mode.char_width
            virtual_h = mode.height * mode.char_height
        else:
            virtual_w = mode.width
            virtual_h = mode.height

        # Calculate scale to fit canvas while maintaining aspect ratio
        canvas_w = self.width()
        canvas_h = self.height()

        # Apply aspect ratio correction for non-square pixels
        corrected_h = virtual_h * mode.aspect_ratio

        scale_x = canvas_w / virtual_w
        scale_y = canvas_h / corrected_h
        scale = min(scale_x, scale_y) * 0.9  # 90% to leave border

        # Calculate offset to center the virtual screen
        scaled_w = virtual_w * scale
        scaled_h = corrected_h * scale
        offset_x = (canvas_w - scaled_w) / 2
        offset_y = (canvas_h - scaled_h) / 2

        # Draw virtual screen border
        border_color = QColor(60, 60, 60)
        painter.setPen(QPen(border_color, 2))
        painter.setBrush(self.bg_color)
        painter.drawRect(
            int(offset_x - 2), int(offset_y - 2), int(scaled_w + 4), int(scaled_h + 4)
        )

        # Draw mode info in corner
        painter.setPen(QColor(100, 100, 100))
        font = QFont("Courier", 8)
        painter.setFont(font)
        mode_text = f"MODE {mode.mode_number}: {mode.resolution_str}"
        painter.drawText(5, 15, mode_text)

        # Save state and apply virtual screen transform
        painter.save()
        painter.translate(offset_x, offset_y)
        painter.scale(scale, scale * mode.aspect_ratio)

        # For graphics modes, draw with pixel snapping
        if mode.mode_type == ModeType.GRAPHICS:
            # Draw coordinate axes
            pen = QPen(QColor(60, 60, 60), 1)
            pen.setStyle(Qt.PenStyle.DashLine)
            painter.setPen(pen)

            # Center of virtual screen
            cx = virtual_w / 2
            cy = virtual_h / 2

            # Draw turtle lines with pixel snapping
            for line in self.lines:
                color = QColor(line.color[0], line.color[1], line.color[2])
                # Thicker lines for visibility at low res
                pen = QPen(color, max(1, line.width))
                painter.setPen(pen)

                # Map turtle coordinates to virtual screen
                # Turtle (0,0) = center, positive Y = up
                x1 = int(cx + line.start_x)
                y1 = int(cy - line.start_y)  # Flip Y
                x2 = int(cx + line.end_x)
                y2 = int(cy - line.end_y)

                # Clamp to virtual screen bounds
                x1 = max(0, min(virtual_w - 1, x1))
                y1 = max(0, min(virtual_h - 1, y1))
                x2 = max(0, min(virtual_w - 1, x2))
                y2 = max(0, min(virtual_h - 1, y2))

                painter.drawLine(x1, y1, x2, y2)

            # Draw turtle cursor
            if self.turtle and self.turtle.visible:
                self._draw_retro_turtle(painter, cx, cy)

        else:
            # Text mode - draw character grid
            self._draw_text_mode_grid(painter, mode)

        painter.restore()

    def _draw_retro_turtle(self, painter: QPainter, cx: float, cy: float):
        """Draw turtle cursor in retro mode."""
        x = int(cx + self.turtle.x)
        y = int(cy - self.turtle.y)
        heading = self.turtle.heading

        pen = QPen(QColor(80, 250, 123), 2)
        painter.setPen(pen)

        size = 8  # Smaller for retro res
        angle = math.radians(heading - 90)

        tip_x = x + size * math.cos(angle)
        tip_y = y - size * math.sin(angle)

        left_angle = angle + math.radians(140)
        left_x = x + (size / 2) * math.cos(left_angle)
        left_y = y - (size / 2) * math.sin(left_angle)

        right_angle = angle - math.radians(140)
        right_x = x + (size / 2) * math.cos(right_angle)
        right_y = y - (size / 2) * math.sin(right_angle)

        painter.drawLine(int(tip_x), int(tip_y), int(left_x), int(left_y))
        painter.drawLine(int(left_x), int(left_y), int(right_x), int(right_y))
        painter.drawLine(int(right_x), int(right_y), int(tip_x), int(tip_y))

    def _draw_text_mode_grid(self, painter: QPainter, mode: ScreenMode):
        """Draw text mode character grid."""
        # Draw subtle grid lines
        grid_color = QColor(50, 50, 50, 50)
        pen = QPen(grid_color, 1)
        painter.setPen(pen)

        # Vertical lines
        for col in range(mode.width + 1):
            x = col * mode.char_width
            painter.drawLine(x, 0, x, mode.height * mode.char_height)

        # Horizontal lines
        for row in range(mode.height + 1):
            y = row * mode.char_height
            painter.drawLine(0, y, mode.width * mode.char_width, y)

        # Draw cursor position indicator
        cursor_col = mode.width // 2
        cursor_row = mode.height // 2
        cursor_x = cursor_col * mode.char_width
        cursor_y = cursor_row * mode.char_height

        cursor_color = QColor(255, 255, 255, 180)
        painter.fillRect(
            QRectF(cursor_x, cursor_y, mode.char_width, mode.char_height), cursor_color
        )

    def _draw_turtle_cursor(self, painter: QPainter):
        """Draw turtle cursor in normal mode."""
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
        if event.button() == Qt.MouseButton.MiddleButton or (
            event.button() == Qt.MouseButton.LeftButton
            and event.modifiers() & Qt.KeyboardModifier.ControlModifier
        ):
            self.panning = True
            self.last_pan_pos = event.pos()
            self.setCursor(Qt.CursorShape.ClosedHandCursor)

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
        if (
            event.button() == Qt.MouseButton.MiddleButton
            or event.button() == Qt.MouseButton.LeftButton
        ):
            self.panning = False
            self.last_pan_pos = None
            self.setCursor(Qt.CursorShape.ArrowCursor)

    def reset_view(self):
        """Reset zoom and pan."""
        self.zoom = 1.0
        self.offset_x = 0.0
        self.offset_y = 0.0
        self.update()

    def export_to_png(self, filepath: str, width: int = 800, height: int = 600) -> bool:
        """Export canvas content to PNG file.

        Args:
            filepath: Path to save the PNG file
            width: Image width in pixels
            height: Image height in pixels

        Returns:
            True if export succeeded, False otherwise
        """
        # Create image with specified size
        image = QImage(width, height, QImage.Format.Format_ARGB32)
        image.fill(self.bg_color)

        # Create painter for the image
        painter = QPainter(image)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Render canvas content
        self._render_to_painter(painter, width, height)

        painter.end()

        # Save image
        return image.save(filepath, b"PNG")

    def export_to_svg(self, filepath: str, width: int = 800, height: int = 600) -> bool:
        """Export canvas content to SVG file.

        Args:
            filepath: Path to save the SVG file
            width: Image width
            height: Image height

        Returns:
            True if export succeeded, False otherwise
        """
        try:
            # pylint: disable=import-outside-toplevel
            from PySide6.QtSvg import QSvgGenerator
        except ImportError:
            return False

        generator = QSvgGenerator()
        generator.setFileName(filepath)
        generator.setSize(QRectF(0, 0, width, height).size().toSize())
        generator.setViewBox(QRectF(0, 0, width, height))
        generator.setTitle("Time Warp IDE - Turtle Graphics")

        painter = QPainter(generator)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Fill background
        painter.fillRect(0, 0, width, height, self.bg_color)

        # Render content
        self._render_to_painter(painter, width, height)

        painter.end()
        return True

    def _render_to_painter(self, painter: QPainter, width: int, height: int):
        """Render canvas content to a painter.

        Args:
            painter: QPainter to render to
            width: Render width
            height: Render height
        """
        # Calculate center
        center_x = width / 2
        center_y = height / 2

        # Apply transform
        painter.translate(center_x, center_y)
        # Flip Y-axis for math coordinates
        painter.scale(1, -1)

        # Draw coordinate axes
        pen = QPen(QColor(100, 100, 100), 1)
        pen.setStyle(Qt.PenStyle.DashLine)
        painter.setPen(pen)
        painter.drawLine(-5000, 0, 5000, 0)
        painter.drawLine(0, -5000, 0, 5000)

        # Draw origin marker
        pen = QPen(QColor(150, 150, 150), 2)
        painter.setPen(pen)
        painter.drawEllipse(QPointF(0, 0), 5, 5)

        # Draw turtle lines
        for line in self.lines:
            color = QColor(line.color[0], line.color[1], line.color[2])
            pen = QPen(color, line.width)
            pen.setCapStyle(Qt.PenCapStyle.RoundCap)
            pen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)
            painter.setPen(pen)

            painter.drawLine(
                int(line.start_x),
                int(line.start_y),
                int(line.end_x),
                int(line.end_y),
            )

        # Draw turtle cursor if present
        if self.turtle and self.turtle.visible:
            self._draw_turtle_cursor(painter)

    def get_print_document(self):
        """Get a printable representation of the canvas.

        Returns:
            QImage suitable for printing
        """
        # Create high-res image for printing
        width = 2400  # 8 inches at 300 DPI
        height = 1800  # 6 inches at 300 DPI

        image = QImage(width, height, QImage.Format_ARGB32)
        image.fill(QColor(255, 255, 255))  # White background for printing

        painter = QPainter(image)
        painter.setRenderHint(QPainter.Antialiasing)

        # Render content
        self._render_to_painter(painter, width, height)

        painter.end()
        return image
