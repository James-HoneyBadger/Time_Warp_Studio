"""CRT effect overlay for retro display simulation.

This module provides visual effects that simulate vintage CRT monitors:
- Scanlines
- Screen curvature (barrel distortion)
- Phosphor glow/bloom
- Screen flicker
- Color bleeding
"""

# pylint: disable=no-name-in-module

import random

from PySide6.QtCore import QRectF, Qt, QTimer
from PySide6.QtGui import (
    QBrush,
    QColor,
    QLinearGradient,
    QPainter,
    QPainterPath,
    QPen,
    QRadialGradient,
)
from PySide6.QtWidgets import QWidget


class CRTEffectOverlay(QWidget):
    """Transparent overlay that renders CRT effects over any widget."""

    def __init__(self, parent=None):
        super().__init__(parent)

        # Make overlay transparent to mouse events
        self.setAttribute(Qt.WA_TransparentForMouseEvents)
        self.setAttribute(Qt.WA_TranslucentBackground)

        # Effect settings
        self.scanlines_enabled = True
        self.scanline_intensity = 0.15  # 0-1, opacity of scanlines
        self.scanline_spacing = 2  # Pixels between scanlines

        self.curvature_enabled = True
        self.curvature_amount = 0.02  # Barrel distortion strength

        self.glow_enabled = True
        self.glow_intensity = 0.1  # Phosphor glow strength

        self.vignette_enabled = True
        self.vignette_intensity = 0.3  # Edge darkening

        self.flicker_enabled = False
        self.flicker_timer = None
        self.flicker_brightness = 1.0

        self.color_bleed_enabled = False

        # Overall effect enabled
        self._enabled = False

    def set_enabled(self, enabled: bool):
        """Enable or disable all CRT effects."""
        self._enabled = enabled
        if enabled:
            self.show()
            self.raise_()
            if self.flicker_enabled:
                self._start_flicker()
        else:
            self.hide()
            self._stop_flicker()
        self.update()

    def is_enabled(self) -> bool:
        """Check if effects are enabled."""
        return self._enabled

    def set_scanlines(self, enabled: bool, intensity: float = 0.15, spacing: int = 2):
        """Configure scanline effect."""
        self.scanlines_enabled = enabled
        self.scanline_intensity = max(0.0, min(1.0, intensity))
        self.scanline_spacing = max(1, spacing)
        self.update()

    def set_curvature(self, enabled: bool, amount: float = 0.02):
        """Configure screen curvature effect."""
        self.curvature_enabled = enabled
        self.curvature_amount = max(0.0, min(0.1, amount))
        self.update()

    def set_glow(self, enabled: bool, intensity: float = 0.1):
        """Configure phosphor glow effect."""
        self.glow_enabled = enabled
        self.glow_intensity = max(0.0, min(0.5, intensity))
        self.update()

    def set_vignette(self, enabled: bool, intensity: float = 0.3):
        """Configure vignette (edge darkening) effect."""
        self.vignette_enabled = enabled
        self.vignette_intensity = max(0.0, min(1.0, intensity))
        self.update()

    def set_flicker(self, enabled: bool):
        """Configure screen flicker effect."""
        self.flicker_enabled = enabled
        if enabled and self._enabled:
            self._start_flicker()
        else:
            self._stop_flicker()

    def _start_flicker(self):
        """Start flicker animation."""
        if not self.flicker_timer:
            self.flicker_timer = QTimer(self)
            self.flicker_timer.timeout.connect(self._update_flicker)
            self.flicker_timer.start(50)  # 20 FPS flicker

    def _stop_flicker(self):
        """Stop flicker animation."""
        if self.flicker_timer:
            self.flicker_timer.stop()
            self.flicker_timer = None
            self.flicker_brightness = 1.0
            self.update()

    def _update_flicker(self):
        """Update flicker brightness."""
        # Subtle brightness variation
        self.flicker_brightness = 0.95 + random.random() * 0.1
        self.update()

    def paintEvent(self, _event):  # pylint: disable=invalid-name
        """Paint CRT effects."""
        if not self._enabled:
            return

        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing)

        rect = self.rect()

        # Apply flicker as overall brightness
        if self.flicker_enabled:
            flicker_color = QColor(0, 0, 0, int((1.0 - self.flicker_brightness) * 30))
            painter.fillRect(rect, flicker_color)

        # Draw vignette (edge darkening)
        if self.vignette_enabled:
            self._draw_vignette(painter, rect)

        # Draw scanlines
        if self.scanlines_enabled:
            self._draw_scanlines(painter, rect)

        # Draw screen curvature edge shadow
        if self.curvature_enabled:
            self._draw_curvature_shadow(painter, rect)

        # Draw phosphor glow (subtle bloom around edges)
        if self.glow_enabled:
            self._draw_glow(painter, rect)

        # Draw CRT bezel/frame effect
        self._draw_bezel(painter, rect)

    def _draw_scanlines(self, painter: QPainter, rect: QRectF):
        """Draw horizontal scanlines."""
        scanline_color = QColor(0, 0, 0, int(self.scanline_intensity * 255))
        pen = QPen(scanline_color, 1)
        painter.setPen(pen)

        y = 0
        while y < rect.height():
            painter.drawLine(0, y, int(rect.width()), y)
            y += self.scanline_spacing

    def _draw_vignette(self, painter: QPainter, rect: QRectF):
        """Draw vignette (darkening at edges)."""
        center_x = rect.width() / 2
        center_y = rect.height() / 2
        radius = max(center_x, center_y) * 1.5

        gradient = QRadialGradient(center_x, center_y, radius)
        gradient.setColorAt(0.0, QColor(0, 0, 0, 0))
        gradient.setColorAt(0.5, QColor(0, 0, 0, 0))
        gradient.setColorAt(0.8, QColor(0, 0, 0, int(self.vignette_intensity * 100)))
        gradient.setColorAt(1.0, QColor(0, 0, 0, int(self.vignette_intensity * 200)))

        painter.setBrush(QBrush(gradient))
        painter.setPen(Qt.NoPen)
        painter.drawRect(rect)

    def _draw_curvature_shadow(self, painter: QPainter, rect: QRectF):
        """Draw shadow effect simulating curved screen edges."""
        # Top edge shadow
        top_gradient = QLinearGradient(0, 0, 0, rect.height() * 0.1)
        top_gradient.setColorAt(0, QColor(0, 0, 0, int(self.curvature_amount * 1000)))
        top_gradient.setColorAt(1, QColor(0, 0, 0, 0))
        painter.fillRect(QRectF(0, 0, rect.width(), rect.height() * 0.1), top_gradient)

        # Bottom edge shadow
        bottom_gradient = QLinearGradient(0, rect.height() * 0.9, 0, rect.height())
        bottom_gradient.setColorAt(0, QColor(0, 0, 0, 0))
        bottom_gradient.setColorAt(
            1, QColor(0, 0, 0, int(self.curvature_amount * 1000))
        )
        painter.fillRect(
            QRectF(0, rect.height() * 0.9, rect.width(), rect.height() * 0.1),
            bottom_gradient,
        )

        # Left edge shadow
        left_gradient = QLinearGradient(0, 0, rect.width() * 0.05, 0)
        left_gradient.setColorAt(0, QColor(0, 0, 0, int(self.curvature_amount * 800)))
        left_gradient.setColorAt(1, QColor(0, 0, 0, 0))
        painter.fillRect(
            QRectF(0, 0, rect.width() * 0.05, rect.height()), left_gradient
        )

        # Right edge shadow
        right_gradient = QLinearGradient(rect.width() * 0.95, 0, rect.width(), 0)
        right_gradient.setColorAt(0, QColor(0, 0, 0, 0))
        right_gradient.setColorAt(1, QColor(0, 0, 0, int(self.curvature_amount * 800)))
        painter.fillRect(
            QRectF(rect.width() * 0.95, 0, rect.width() * 0.05, rect.height()),
            right_gradient,
        )

    def _draw_glow(self, painter: QPainter, rect: QRectF):
        """Draw subtle phosphor glow effect."""
        # Create a soft inner glow
        glow_color = QColor(255, 255, 255, int(self.glow_intensity * 50))

        # Draw subtle highlight in center
        center_x = rect.width() / 2
        center_y = rect.height() / 2
        glow_radius = min(center_x, center_y) * 0.8

        gradient = QRadialGradient(center_x, center_y, glow_radius)
        gradient.setColorAt(0, glow_color)
        gradient.setColorAt(1, QColor(255, 255, 255, 0))

        painter.setBrush(QBrush(gradient))
        painter.setPen(Qt.NoPen)
        painter.drawEllipse(
            int(center_x - glow_radius),
            int(center_y - glow_radius),
            int(glow_radius * 2),
            int(glow_radius * 2),
        )

    def _draw_bezel(self, painter: QPainter, rect: QRectF):
        """Draw CRT monitor bezel effect."""
        # Outer dark border
        border_width = 3
        border_color = QColor(20, 20, 20, 200)

        pen = QPen(border_color, border_width)
        painter.setPen(pen)
        painter.setBrush(Qt.NoBrush)

        # Rounded rectangle for bezel
        path = QPainterPath()
        path.addRoundedRect(
            QRectF(
                border_width / 2,
                border_width / 2,
                rect.width() - border_width,
                rect.height() - border_width,
            ),
            8,
            8,
        )
        painter.drawPath(path)

    def resizeEvent(self, event):  # pylint: disable=invalid-name
        """Handle resize to match parent."""
        super().resizeEvent(event)
        if self.parent():
            self.setGeometry(self.parent().rect())


class CRTCanvas(QWidget):
    """A canvas widget with built-in CRT effects.

    This combines a drawing area with an overlay for CRT simulation.
    """

    def __init__(self, parent=None):
        super().__init__(parent)

        # CRT effect overlay
        self.crt_overlay = CRTEffectOverlay(self)
        self.crt_overlay.setGeometry(self.rect())

    def set_crt_enabled(self, enabled: bool):
        """Enable or disable CRT effects."""
        self.crt_overlay.set_enabled(enabled)

    def resizeEvent(self, event):  # pylint: disable=invalid-name
        """Ensure overlay covers full widget."""
        super().resizeEvent(event)
        self.crt_overlay.setGeometry(self.rect())
        self.crt_overlay.raise_()
