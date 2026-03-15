"""Turtle graphics canvas widget."""

# Many Qt widgets use camelCase method names (e.g. paintEvent) which are
# idiomatic in Qt but trigger naming checks; also this widget intentionally
# holds several state attributes for rendering. Silence those specific
# warnings locally.
# pylint: disable=no-name-in-module

import math
import os

from PySide6.QtCore import QPointF, QRectF, Qt, QTimer, Signal
from PySide6.QtGui import (
    QColor,
    QFont,
    QImage,
    QMouseEvent,
    QPainter,
    QPen,
    QWheelEvent,
)
from PySide6.QtWidgets import (
    QApplication,
    QFileDialog,
    QHBoxLayout,
    QLabel,
    QPushButton,
    QVBoxLayout,
    QWidget,
)

from .screen_modes import ModeType, ScreenMode, ScreenModeManager


class TurtleCanvas(
    QWidget
):  # pylint: disable=invalid-name,too-many-instance-attributes,unused-argument
    """Canvas for rendering turtle graphics with retro screen mode support."""

    # Emitted whenever the turtle position/heading changes (for status strip)
    turtle_position_changed = Signal(float, float, float)  # x, y, heading

    # Zoom limits — single source of truth
    _ZOOM_MIN = 0.05
    _ZOOM_MAX = 24.0

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

        # Grid overlay
        self._grid_enabled = False

        # Screen mode support
        self.screen_mode_manager = ScreenModeManager()
        self.screen_mode_enabled = False  # When True, simulate retro resolution

        # Animation player state
        self._anim_running = False
        self._anim_frame = 0
        self._anim_timer = QTimer(self)
        self._anim_timer.setInterval(30)  # ~33 fps
        self._anim_timer.timeout.connect(self._anim_step)
        self._anim_btn: QPushButton | None = None  # set in toolbar setup

        # Minimum size — small enough to allow the splitter to resize freely
        self.setMinimumSize(200, 200)

        # Outer container: toolbar + canvas + position strip
        # (We expose self as the canvas widget; the wrapper is in _build_wrapper)
        self._pos_label: QLabel | None = None

        # Overlay toolbar (zoom / reset controls)
        self._setup_canvas_toolbar()

    # ------------------------------------------------------------------
    # Canvas overlay toolbar
    # ------------------------------------------------------------------

    _TOOLBAR_BTN_STYLE = """
        QPushButton {
            background-color: rgba(30, 31, 45, 175);
            color: #cdd6f4;
            border: 1px solid rgba(255, 255, 255, 0.25);
            border-radius: 4px;
            padding: 2px 6px;
            font-size: 11px;
            font-weight: bold;
            min-width: 24px;
        }
        QPushButton:hover {
            background-color: rgba(80, 100, 180, 210);
            border-color: rgba(255, 255, 255, 0.5);
        }
        QPushButton:pressed {
            background-color: rgba(50, 70, 140, 220);
        }
    """

    def _setup_canvas_toolbar(self):
        """Create a small overlay toolbar anchored to the top-right corner."""
        self._canvas_toolbar = QWidget(self)
        self._canvas_toolbar.setObjectName("CanvasToolbar")
        toolbar_layout = QHBoxLayout(self._canvas_toolbar)
        toolbar_layout.setContentsMargins(3, 3, 3, 3)
        toolbar_layout.setSpacing(3)

        def _btn(icon: str, tip: str, cb) -> QPushButton:
            b = QPushButton(icon)
            b.setToolTip(tip)
            b.setStyleSheet(self._TOOLBAR_BTN_STYLE)
            b.setFixedWidth(28)
            b.setFixedHeight(24)
            b.clicked.connect(cb)
            return b

        toolbar_layout.addWidget(_btn("⊕", "Zoom In  (+)", self._zoom_in))
        toolbar_layout.addWidget(_btn("⊖", "Zoom Out  (-)", self._zoom_out))
        toolbar_layout.addWidget(_btn("⊙", "Reset View", self._reset_view))
        toolbar_layout.addWidget(_btn("⊡", "Fit to Screen", self._fit_to_screen))
        self._anim_btn = _btn("▶", "Play / Pause Animation", self._toggle_animation)
        toolbar_layout.addWidget(self._anim_btn)
        toolbar_layout.addWidget(
            _btn("📋", "Copy to Clipboard", self._copy_to_clipboard)
        )
        toolbar_layout.addWidget(
            _btn("💾", "Save PNG…", self._save_png)
        )
        self._grid_btn = _btn("⋯", "Toggle Grid Overlay", self._toggle_grid)
        self._grid_btn.setCheckable(True)
        toolbar_layout.addWidget(self._grid_btn)

        # Position strip below canvas
        self._pos_label = QLabel("x: 0  y: 0  ∠: 0°")
        self._pos_label.setStyleSheet(
            "QLabel { background: rgba(20,20,30,160); color: #8be9fd; "
            "padding: 1px 6px; font-size: 10px; font-family: 'Courier New'; "
            "border-top: 1px solid rgba(255,255,255,0.15); }"
        )
        self._pos_label.setFixedHeight(18)
        self._canvas_toolbar.adjustSize()
        self._canvas_toolbar.raise_()

    def _position_canvas_toolbar(self):
        """Reposition the toolbar to the top-right corner."""
        if hasattr(self, "_canvas_toolbar"):
            tb = self._canvas_toolbar
            tb.adjustSize()
            tb.move(self.width() - tb.width() - 4, 4)
        if hasattr(self, "_pos_label") and self._pos_label:
            lbl = self._pos_label
            lbl.setGeometry(0, self.height() - lbl.height(), self.width(), lbl.height())
            lbl.raise_()

    def resizeEvent(self, event):
        """Reposition overlay toolbar on resize."""
        super().resizeEvent(event)
        self._position_canvas_toolbar()

    # ------------------------------------------------------------------
    # Zoom / pan helpers called by the toolbar buttons
    # ------------------------------------------------------------------

    def _zoom_in(self):
        """Increase canvas zoom by 25%."""
        self.zoom = min(self.zoom * 1.25, self._ZOOM_MAX)
        self.update()

    def _zoom_out(self):
        """Decrease canvas zoom by 20%."""
        self.zoom = max(self.zoom / 1.25, self._ZOOM_MIN)
        self.update()

    def _reset_view(self):
        """Reset zoom and pan to default."""
        self.zoom = 1.0
        self.offset_x = 0.0
        self.offset_y = 0.0
        self.update()

    def _fit_to_screen(self):
        """Scale and centre the view to fit all drawn content."""
        if not self.lines:
            self._reset_view()
            return

        xs = [c for line in self.lines for c in (line.start_x, line.end_x)]
        ys = [c for line in self.lines for c in (line.start_y, line.end_y)]
        if not xs:
            self._reset_view()
            return

        min_x, max_x = min(xs), max(xs)
        min_y, max_y = min(ys), max(ys)
        content_w = max(max_x - min_x, 1.0)
        content_h = max(max_y - min_y, 1.0)
        padding = 40.0

        zoom_x = (self.width() - padding * 2) / content_w
        zoom_y = (self.height() - padding * 2) / content_h
        self.zoom = max(0.05, min(zoom_x, zoom_y, 24.0))

        # Centre content: offset so the bounding-box centre maps to origin
        cx = (min_x + max_x) / 2.0
        cy = (min_y + max_y) / 2.0
        self.offset_x = -cx * self.zoom
        # Y is flipped in the painter (scale Y = -zoom), so offset is positive
        self.offset_y = cy * self.zoom
        self.update()

    # ------------------------------------------------------------------
    # Animation player
    # ------------------------------------------------------------------

    def _toggle_animation(self):
        """Start or pause step-through animation of turtle lines."""
        if self._anim_running:
            self._anim_timer.stop()
            self._anim_running = False
            if self._anim_btn:
                self._anim_btn.setText("▶")
        else:
            # Restart from beginning if finished
            if self._anim_frame >= len(self.lines):
                self._anim_frame = 0
            self._anim_running = True
            self._anim_timer.start()
            if self._anim_btn:
                self._anim_btn.setText("⏸")

    def _anim_step(self):
        """Advance animation by one frame."""
        if self._anim_frame < len(self.lines):
            self._anim_frame += 1
            self.update()
        else:
            self._anim_timer.stop()
            self._anim_running = False
            if self._anim_btn:
                self._anim_btn.setText("▶")

    # ------------------------------------------------------------------
    # PNG save to file
    # ------------------------------------------------------------------

    def _save_png(self):
        """Render canvas to a PNG file chosen by the user."""
        filename, _ = QFileDialog.getSaveFileName(
            self,
            "Save Canvas as PNG",
            os.path.expanduser("~"),
            "PNG Images (*.png);;All Files (*)",
        )
        if not filename:
            return
        if not filename.lower().endswith(".png"):
            filename += ".png"
        w, h = max(self.width(), 1), max(self.height(), 1)
        image = QImage(w, h, QImage.Format.Format_ARGB32)
        image.fill(self.bg_color)
        painter = QPainter(image)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        self._render_to_painter(painter, w, h)
        painter.end()
        image.save(filename, "PNG")

    # ------------------------------------------------------------------
    # Grid overlay toggle
    # ------------------------------------------------------------------

    def _toggle_grid(self):
        """Toggle the coordinate grid overlay."""
        self._grid_enabled = not self._grid_enabled
        self.update()

    def toggle_grid(self, enabled: bool) -> None:
        """Programmatically enable or disable the grid overlay."""
        self._grid_enabled = enabled
        if hasattr(self, "_grid_btn"):
            self._grid_btn.setChecked(enabled)
        self.update()

    # ------------------------------------------------------------------
    # Clipboard copy
    # ------------------------------------------------------------------

    def _copy_to_clipboard(self):
        """Render canvas to a QImage and copy it to the system clipboard."""
        w, h = max(self.width(), 1), max(self.height(), 1)
        image = QImage(w, h, QImage.Format.Format_ARGB32)
        image.fill(self.bg_color)
        painter = QPainter(image)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        self._render_to_painter(painter, w, h)
        painter.end()
        QApplication.clipboard().setImage(image)

    def set_screen_mode(self, mode: ScreenMode):
        """Set the current screen mode for rendering."""
        self.screen_mode_manager.set_mode(mode.mode_number)
        self.screen_mode_enabled = True
        self.update()

    def set_screen_mode_enabled(self, enabled: bool):
        """Enable or disable screen mode simulation."""
        self.screen_mode_enabled = enabled
        self.update()

    def _draw_grid(self, painter: QPainter):
        """Draw a coordinate grid at 50-unit spacing (in turtle coords)."""
        grid_color = QColor(80, 80, 100, 60)
        label_color = QColor(120, 120, 140, 180)
        pen = QPen(grid_color, 0.5 / max(self.zoom, 0.01))
        pen.setStyle(Qt.PenStyle.DotLine)
        painter.setPen(pen)
        spacing = 50
        half = 2000
        for v in range(-half, half + spacing, spacing):
            painter.drawLine(v, -half, v, half)  # vertical
            painter.drawLine(-half, v, half, v)  # horizontal
        # Labels (drawn in scaled coords, small font)
        font = QFont("Courier", int(8 / max(self.zoom, 0.01)))
        painter.setFont(font)
        painter.setPen(QPen(label_color))
        for v in range(-half, half + spacing, spacing * 2):
            if v != 0:
                painter.drawText(QPointF(v + 2, 10), str(v))
                painter.drawText(QPointF(4, -v + 3), str(v))

    def set_turtle_state(self, turtle):
        """Set turtle state and repaint."""
        self.turtle = turtle
        self.lines = turtle.lines.copy()

        # Reset animation state when new turtle data arrives
        self._anim_timer.stop()
        self._anim_running = False
        self._anim_frame = len(self.lines)  # show all by default
        if self._anim_btn:
            self._anim_btn.setText("▶")

        # Adopt background color from turtle state if available
        try:
            r, g, b = getattr(turtle, "bg_color", (40, 42, 54))
            self.bg_color = QColor(int(r), int(g), int(b))
        except (TypeError, ValueError):
            # Fallback to default theme background
            self.bg_color = QColor(40, 42, 54)

        # Use repaint() instead of update() to force immediate redraw
        self.repaint()

        # Update position strip and emit signal
        if hasattr(turtle, "x") and hasattr(turtle, "y") and hasattr(turtle, "heading"):
            x = round(float(turtle.x), 1)
            y = round(float(turtle.y), 1)
            h = round(float(turtle.heading), 1)
            if hasattr(self, "_pos_label") and self._pos_label:
                self._pos_label.setText(f"x: {x}  y: {y}  ∠: {h}°")
            self.turtle_position_changed.emit(x, y, h)

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

        # Optional coordinate grid
        if self._grid_enabled:
            self._draw_grid(painter)

        # Draw turtle lines (use animation slice when player is active)
        visible_lines = (
            self.lines[: self._anim_frame] if self._anim_running else self.lines
        )
        for line in visible_lines:
            color = QColor(line.color[0], line.color[1], line.color[2])
            # Adjust pen width inversely to zoom so it stays visible at all
            # zoom levels. Zoom is always in [0.05, 24.0] so division is safe.
            adjusted_width = line.width / max(self.zoom, 0.1)
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

        # Draw turtle shapes if available
        if self.turtle and hasattr(self.turtle, "shapes"):
            self._draw_turtle_shapes(painter)

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
            int(offset_x - 2),
            int(offset_y - 2),
            int(scaled_w + 4),
            int(scaled_h + 4),
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
            QRectF(cursor_x, cursor_y, mode.char_width, mode.char_height),
            cursor_color,
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

    def _draw_turtle_shapes(self, painter: QPainter):
        """Draw high-level turtle shapes (point, line, rect, polygon, ellipse, text)."""
        if not hasattr(self.turtle, "shapes"):
            return

        for shape in self.turtle.shapes:
            self._draw_single_shape(painter, shape)

    def _draw_single_shape(self, painter: QPainter, shape):
        """Draw a single turtle shape."""
        shape_type = shape.shape_type
        color = QColor(shape.color[0], shape.color[1], shape.color[2])

        if shape_type == "point":
            # Single pixel/point
            if len(shape.points) > 0:
                x, y = shape.points[0]
                painter.fillRect(int(x) - 1, int(y) - 1, 2, 2, color)

        elif shape_type == "line":
            # Two-point line
            if len(shape.points) >= 2:
                pen = QPen(color, shape.width)
                pen.setCapStyle(Qt.PenCapStyle.RoundCap)
                pen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)
                painter.setPen(pen)
                x1, y1 = shape.points[0]
                x2, y2 = shape.points[1]
                painter.drawLine(int(x1), int(y1), int(x2), int(y2))

        elif shape_type == "rect":
            # Rectangle (outline and/or fill)
            if len(shape.points) >= 2:
                x1, y1 = shape.points[0]
                x2, y2 = shape.points[1]
                rect = QRectF(min(x1, x2), min(y1, y2), abs(x2 - x1), abs(y2 - y1))
                if shape.fill_color:
                    fill = QColor(
                        shape.fill_color[0], shape.fill_color[1], shape.fill_color[2]
                    )
                    painter.fillRect(rect, fill)
                pen = QPen(color, shape.width)
                painter.setPen(pen)
                painter.drawRect(rect)

        elif shape_type == "polygon":
            # Closed polygon
            if len(shape.points) >= 3:
                from PySide6.QtGui import QPolygonF

                poly = QPolygonF([QPointF(x, y) for x, y in shape.points])
                if shape.fill_color:
                    fill = QColor(
                        shape.fill_color[0], shape.fill_color[1], shape.fill_color[2]
                    )
                    painter.setBrush(fill)
                    painter.drawPolygon(poly)
                    painter.setBrush(Qt.BrushStyle.NoBrush)
                pen = QPen(color, shape.width)
                painter.setPen(pen)
                painter.drawPolygon(poly)

        elif shape_type == "polyline":
            # Open polyline
            if len(shape.points) >= 2:
                pen = QPen(color, shape.width)
                pen.setCapStyle(Qt.PenCapStyle.RoundCap)
                pen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)
                painter.setPen(pen)
                for i in range(len(shape.points) - 1):
                    x1, y1 = shape.points[i]
                    x2, y2 = shape.points[i + 1]
                    painter.drawLine(int(x1), int(y1), int(x2), int(y2))

        elif shape_type == "ellipse":
            # Ellipse (center and radii)
            if len(shape.points) >= 2:
                x, y = shape.points[0]
                rx, ry = shape.points[1]
                rect = QRectF(x - rx, y - ry, 2 * rx, 2 * ry)
                if shape.fill_color:
                    fill = QColor(
                        shape.fill_color[0], shape.fill_color[1], shape.fill_color[2]
                    )
                    painter.setBrush(fill)
                    painter.drawEllipse(rect)
                    painter.setBrush(Qt.BrushStyle.NoBrush)
                pen = QPen(color, shape.width)
                painter.setPen(pen)
                painter.drawEllipse(rect)

        elif shape_type == "text":
            # Text label
            if len(shape.points) > 0 and shape.text:
                x, y = shape.points[0]
                font = QFont("Courier New", shape.font_size)
                painter.setFont(font)
                painter.setPen(color)
                painter.drawText(int(x), int(y), shape.text)

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
        self.zoom = max(self._ZOOM_MIN, min(self._ZOOM_MAX, self.zoom))

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
        generator.setTitle("Time Warp Studio - Turtle Graphics")

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
