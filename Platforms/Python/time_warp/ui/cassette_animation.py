# pylint: disable=too-few-public-methods
"""Cassette tape save/load animation dialog.

This module provides a fun retro animation that simulates saving
and loading programs from a cassette tape, just like the old days
on Apple II, C64, ZX Spectrum, etc.
"""

# pylint: disable=no-name-in-module

import math

from PySide6.QtCore import QPointF, Qt, QTimer, Signal
from PySide6.QtGui import (
    QBrush,
    QColor,
    QFont,
    QPainter,
    QPen,
    QRadialGradient,
)
from PySide6.QtWidgets import (
    QDialog,
    QHBoxLayout,
    QLabel,
    QPushButton,
    QVBoxLayout,
    QWidget,
)


class CassetteWidget(QWidget):
    """Widget that displays animated cassette tape reels."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setMinimumSize(400, 200)

        # Animation state
        self.reel_angle = 0.0
        self.is_playing = False
        self.progress = 0.0  # 0-1 progress of save/load
        self.direction = 1  # 1 = forward (save/load), -1 = rewind

        # Animation timer
        self.timer = QTimer(self)
        self.timer.timeout.connect(self._animate)

        # Colors
        self.cassette_color = QColor(40, 40, 40)
        self.reel_color = QColor(60, 60, 60)
        self.tape_color = QColor(139, 90, 43)  # Brown tape
        self.hub_color = QColor(200, 200, 200)
        self.label_color = QColor(255, 200, 100)  # Warm label color
        self.window_color = QColor(80, 60, 40)  # Tape window

    def start_animation(self, direction: int = 1):
        """Start tape animation."""
        self.is_playing = True
        self.direction = direction
        self.timer.start(33)  # ~30 FPS

    def stop_animation(self):
        """Stop tape animation."""
        self.is_playing = False
        self.timer.stop()

    def set_progress(self, progress: float):
        """Set save/load progress (0-1)."""
        self.progress = max(0.0, min(1.0, progress))
        self.update()

    def _animate(self):
        """Update animation frame."""
        # Rotate reels
        speed = 5.0 if self.direction > 0 else -8.0  # Rewind is faster
        self.reel_angle += speed
        if self.reel_angle >= 360:
            self.reel_angle -= 360
        elif self.reel_angle < 0:
            self.reel_angle += 360

        self.update()

    def paintEvent(self, _event):  # pylint: disable=invalid-name
        """Paint the cassette tape."""
        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing)

        # Get dimensions
        w = self.width()
        h = self.height()

        # Background
        painter.fillRect(self.rect(), QColor(30, 30, 30))

        # Cassette body dimensions
        cassette_w = min(w - 40, 360)
        cassette_h = min(h - 40, 180)
        cassette_x = (w - cassette_w) / 2
        cassette_y = (h - cassette_h) / 2

        # Draw cassette body
        self._draw_cassette_body(
            painter, cassette_x, cassette_y, cassette_w, cassette_h
        )

        # Draw reels
        reel_radius = cassette_h * 0.25
        left_reel_x = cassette_x + cassette_w * 0.3
        right_reel_x = cassette_x + cassette_w * 0.7
        reel_y = cassette_y + cassette_h * 0.45

        # Calculate tape amounts on each reel based on progress
        if self.direction > 0:
            left_tape = 1.0 - self.progress  # Decreasing
            right_tape = self.progress  # Increasing
        else:
            left_tape = self.progress
            right_tape = 1.0 - self.progress

        self._draw_reel(painter, left_reel_x, reel_y, reel_radius, left_tape)
        self._draw_reel(painter, right_reel_x, reel_y, reel_radius, right_tape)

        # Draw tape path (visible tape between reels)
        self._draw_tape_path(
            painter,
            left_reel_x,
            right_reel_x,
            reel_y,
            reel_radius,
            cassette_y + cassette_h * 0.7,
        )

        # Draw label
        self._draw_label(painter, cassette_x, cassette_y, cassette_w, cassette_h)

    def _draw_cassette_body(
        self, painter: QPainter, x: float, y: float, w: float, h: float
    ):
        """Draw the main cassette body."""
        # Main body
        painter.setBrush(QBrush(self.cassette_color))
        painter.setPen(QPen(QColor(60, 60, 60), 2))
        painter.drawRoundedRect(int(x), int(y), int(w), int(h), 10, 10)

        # Tape window (where you see the tape)
        window_w = w * 0.6
        window_h = h * 0.35
        window_x = x + (w - window_w) / 2
        window_y = y + h * 0.3

        painter.setBrush(QBrush(self.window_color))
        painter.setPen(QPen(QColor(100, 80, 60), 1))
        painter.drawRoundedRect(
            int(window_x), int(window_y), int(window_w), int(window_h), 5, 5
        )

        # Screw holes in corners
        screw_color = QColor(80, 80, 80)
        painter.setBrush(QBrush(screw_color))
        painter.setPen(Qt.NoPen)
        screw_radius = 4
        offsets = [(15, 15), (w - 15, 15), (15, h - 15), (w - 15, h - 15)]
        for ox, oy in offsets:
            painter.drawEllipse(
                int(x + ox - screw_radius),
                int(y + oy - screw_radius),
                screw_radius * 2,
                screw_radius * 2,
            )

    def _draw_reel(
        self,
        painter: QPainter,
        x: float,
        y: float,
        radius: float,
        tape_amount: float,
    ):
        """Draw a tape reel with spokes."""
        # Calculate tape radius (more tape = larger visual radius)
        min_tape_radius = radius * 0.4
        max_tape_radius = radius * 0.9
        tape_range = max_tape_radius - min_tape_radius
        tape_radius = min_tape_radius + tape_range * tape_amount

        # Draw tape on reel (brown ring)
        if tape_amount > 0.05:
            gradient = QRadialGradient(x, y, tape_radius)
            gradient.setColorAt(0.3, QColor(100, 60, 30))
            gradient.setColorAt(0.7, self.tape_color)
            gradient.setColorAt(1.0, QColor(80, 50, 25))

            painter.setBrush(QBrush(gradient))
            painter.setPen(Qt.NoPen)
            painter.drawEllipse(QPointF(x, y), tape_radius, tape_radius)

        # Draw reel hub
        hub_radius = radius * 0.35
        painter.setBrush(QBrush(self.hub_color))
        painter.setPen(QPen(QColor(150, 150, 150), 1))
        painter.drawEllipse(QPointF(x, y), hub_radius, hub_radius)

        # Draw center hole
        center_radius = radius * 0.15
        painter.setBrush(QBrush(QColor(40, 40, 40)))
        painter.drawEllipse(QPointF(x, y), center_radius, center_radius)

        # Draw rotating spokes
        painter.setPen(QPen(QColor(100, 100, 100), 2))
        spoke_len = hub_radius - center_radius - 2
        for i in range(6):
            angle = math.radians(self.reel_angle + i * 60)
            inner_x = x + center_radius * math.cos(angle)
            inner_y = y + center_radius * math.sin(angle)
            outer_x = x + (center_radius + spoke_len) * math.cos(angle)
            outer_y = y + (center_radius + spoke_len) * math.sin(angle)
            painter.drawLine(int(inner_x), int(inner_y), int(outer_x), int(outer_y))

    def _draw_tape_path(
        self,
        painter: QPainter,
        left_x: float,
        right_x: float,
        reel_y: float,
        reel_radius: float,
        head_y: float,
    ):
        """Draw the visible tape path between reels."""
        # Tape goes from left reel, down around the head, to right reel
        painter.setPen(QPen(self.tape_color, 3))

        # Left reel to head
        painter.drawLine(
            int(left_x),
            int(reel_y + reel_radius * 0.3),
            int(left_x + 10),
            int(head_y),
        )
        # Across the head
        painter.drawLine(
            int(left_x + 10),
            int(head_y),
            int(right_x - 10),
            int(head_y),
        )
        # Head to right reel
        painter.drawLine(
            int(right_x - 10),
            int(head_y),
            int(right_x),
            int(reel_y + reel_radius * 0.3),
        )

        # Draw tape head (the read/write mechanism)
        head_x = (left_x + right_x) / 2
        painter.setBrush(QBrush(QColor(150, 150, 150)))
        painter.setPen(QPen(QColor(100, 100, 100), 1))
        painter.drawRect(int(head_x - 15), int(head_y - 5), 30, 12)

    def _draw_label(self, painter: QPainter, x: float, y: float, w: float, h: float):
        """Draw the cassette label."""
        label_w = w * 0.5
        label_h = h * 0.15
        label_x = x + (w - label_w) / 2
        label_y = y + h * 0.08

        # Label background
        painter.setBrush(QBrush(self.label_color))
        painter.setPen(QPen(QColor(200, 150, 80), 1))
        painter.drawRect(int(label_x), int(label_y), int(label_w), int(label_h))

        # Label text
        font = QFont("Courier", 8, QFont.Bold)
        painter.setFont(font)
        painter.setPen(QColor(60, 40, 20))

        text = "TIME WARP"
        text_rect = painter.fontMetrics().boundingRect(text)
        text_x = label_x + (label_w - text_rect.width()) / 2
        text_y = label_y + (label_h + text_rect.height()) / 2 - 2
        painter.drawText(int(text_x), int(text_y), text)


class CassetteDialog(QDialog):
    """Dialog showing cassette tape animation for save/load operations."""

    # Signals
    operation_complete = Signal()
    operation_cancelled = Signal()

    def __init__(
        self,
        parent=None,
        operation: str = "SAVING",
        filename: str = "PROGRAM",
    ):
        super().__init__(parent)

        self.operation = operation
        self.filename = filename
        self.progress = 0.0
        self._cancelled = False

        self.setWindowTitle(f"📼 {operation}...")
        self.setModal(True)
        self.setMinimumSize(450, 320)
        self.resize(450, 320)

        # Remove window frame for retro look
        self.setWindowFlags(Qt.Dialog | Qt.FramelessWindowHint)

        self._setup_ui()

        # Progress timer
        self.progress_timer = QTimer(self)
        self.progress_timer.timeout.connect(self._update_progress)
        self.progress_increment = 0.0

    def _setup_ui(self):
        """Setup dialog UI."""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(20, 20, 20, 20)
        layout.setSpacing(15)

        # Set dark background
        self.setStyleSheet("""
            QDialog {
                background-color: #1a1a1a;
                border: 3px solid #444;
                border-radius: 10px;
            }
            QLabel {
                color: #00ff00;
                font-family: 'Courier New', monospace;
            }
            QPushButton {
                background-color: #333;
                color: #00ff00;
                border: 2px solid #00aa00;
                border-radius: 5px;
                padding: 8px 20px;
                font-family: 'Courier New', monospace;
                font-weight: bold;
            }
            QPushButton:hover {
                background-color: #004400;
            }
            QPushButton:pressed {
                background-color: #006600;
            }
        """)

        # Title
        title = QLabel(f"📼 {self.operation} TO CASSETTE")
        title.setAlignment(Qt.AlignCenter)
        title.setStyleSheet("font-size: 16px; font-weight: bold;")
        layout.addWidget(title)

        # Filename
        file_label = QLabel(f'"{self.filename}"')
        file_label.setAlignment(Qt.AlignCenter)
        file_label.setStyleSheet("font-size: 12px; color: #ffff00;")
        layout.addWidget(file_label)

        # Cassette widget
        self.cassette = CassetteWidget()
        layout.addWidget(self.cassette)

        # Status label
        self.status_label = QLabel("PRESS PLAY ON TAPE")
        self.status_label.setAlignment(Qt.AlignCenter)
        self.status_label.setStyleSheet("font-size: 14px;")
        layout.addWidget(self.status_label)

        # Progress bar (ASCII style)
        self.progress_label = QLabel("[          ] 0%")
        self.progress_label.setAlignment(Qt.AlignCenter)
        self.progress_label.setStyleSheet("font-size: 12px;")
        layout.addWidget(self.progress_label)

        # Buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        self.cancel_btn = QPushButton("STOP")
        self.cancel_btn.clicked.connect(self._on_cancel)
        button_layout.addWidget(self.cancel_btn)

        button_layout.addStretch()
        layout.addLayout(button_layout)

    def start(self, duration_ms: int = 3000):
        """Start the save/load animation.

        Args:
            duration_ms: How long the animation should take in milliseconds
        """
        self.progress = 0.0
        self._cancelled = False

        # Calculate progress increment per frame (30 FPS)
        frames = duration_ms / 33
        self.progress_increment = 1.0 / frames

        self.cassette.start_animation(direction=1)
        self.progress_timer.start(33)
        status = "LOADING..." if "LOAD" in self.operation else "SAVING..."
        self.status_label.setText(status)

    def _update_progress(self):
        """Update progress bar and animation."""
        if self._cancelled:
            return

        self.progress += self.progress_increment

        if self.progress >= 1.0:
            self.progress = 1.0
            self._complete()
            return

        self.cassette.set_progress(self.progress)
        self._update_progress_display()

    def _update_progress_display(self):
        """Update the ASCII progress bar."""
        filled = int(self.progress * 10)
        progress_bar = "█" * filled + "░" * (10 - filled)
        percent = int(self.progress * 100)
        self.progress_label.setText(f"[{progress_bar}] {percent}%")

    def _complete(self):
        """Handle operation complete."""
        self.progress_timer.stop()
        self.cassette.stop_animation()
        self.status_label.setText("COMPLETE!")
        self.progress_label.setText("[██████████] 100%")

        # Auto-close after brief delay
        QTimer.singleShot(500, self._finish_complete)

    def _finish_complete(self):
        """Emit complete signal and close."""
        self.operation_complete.emit()
        self.accept()

    def _on_cancel(self):
        """Handle cancel button."""
        self._cancelled = True
        self.progress_timer.stop()
        self.cassette.stop_animation()
        self.status_label.setText("STOPPED")
        self.operation_cancelled.emit()
        self.reject()


def show_cassette_save(parent, filename: str, duration_ms: int = 2000) -> bool:
    """Show cassette save animation.

    Args:
        parent: Parent widget
        filename: Name of file being saved
        duration_ms: Animation duration

    Returns:
        True if completed, False if cancelled
    """
    dialog = CassetteDialog(parent, "SAVING", filename)
    dialog.start(duration_ms)
    return dialog.exec() == QDialog.Accepted


def show_cassette_load(parent, filename: str, duration_ms: int = 2500) -> bool:
    """Show cassette load animation.

    Args:
        parent: Parent widget
        filename: Name of file being loaded
        duration_ms: Animation duration

    Returns:
        True if completed, False if cancelled
    """
    dialog = CassetteDialog(parent, "LOADING", filename)
    dialog.start(duration_ms)
    return dialog.exec() == QDialog.Accepted
