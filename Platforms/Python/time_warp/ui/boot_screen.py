"""Retro CRT-style animated boot/splash screen.

Shown briefly on application startup (or when a CRT theme is active).
The animation runs through three phases:
  1. Power-on phosphor bloom — white flash fading in ~200 ms
  2. Scan-line wipe — a bright horizontal bar sweeps top-to-bottom
  3. Text materialise — the studio title types itself onto the screen

The widget is frameless, stays on top, and calls ``finished`` when done.
The caller (``main_window.__init__``) should call ``show()`` before
constructing the main window, then connect ``finished`` to
``main_window.show()`` / ``splash.close()``.
"""

from __future__ import annotations

import math

from PySide6.QtCore import Property, QEasingCurve, QPropertyAnimation, Qt, QTimer
from PySide6.QtGui import (
    QColor,
    QFont,
    QLinearGradient,
    QPainter,
    QPen,
    QRadialGradient,
)
from PySide6.QtWidgets import QApplication, QWidget

from .. import __version__

# Colour palettes per CRT flavour
_PALETTES: dict[str, dict[str, QColor]] = {
    "amber": {
        "bg": QColor(10, 6, 0),
        "phosphor": QColor(255, 176, 0),
        "dim": QColor(80, 50, 0),
        "bloom": QColor(255, 220, 80, 40),
    },
    "green": {
        "bg": QColor(0, 8, 0),
        "phosphor": QColor(0, 255, 80),
        "dim": QColor(0, 60, 20),
        "bloom": QColor(80, 255, 80, 30),
    },
    "c64": {
        "bg": QColor(40, 40, 80),
        "phosphor": QColor(160, 160, 220),
        "dim": QColor(80, 80, 120),
        "bloom": QColor(160, 160, 255, 30),
    },
    "default": {
        "bg": QColor(5, 10, 20),
        "phosphor": QColor(180, 220, 255),
        "dim": QColor(40, 60, 90),
        "bloom": QColor(100, 160, 255, 25),
    },
}

_TITLE_LINES = [
    "╔══════════════════════════════════════╗",
    "║                                      ║",
    "║         TIME  WARP  STUDIO           ║",
    "║                                      ║",
    "║   Educational Multi-Language IDE     ║",
    f"║{f'Version {__version__}':^38}║",
    "║                                      ║",
    "╚══════════════════════════════════════╝",
]
_SUBTITLE = "LOADING SYSTEM…"
_BOOT_MESSAGES = [
    "Initialising language cores   OK",
    "Loading theme engine          OK",
    "Mounting turtle graphics      OK",
    "Starting debugger             OK",
    "Ready.",
]


class RetroBootScreen(QWidget):
    """Frameless full-screen retro CRT boot animation."""

    # Total animation duration in ms
    TOTAL_MS = 2400

    def __init__(
        self, crt_flavour: str = "default", parent: QWidget | None = None
    ) -> None:
        super().__init__(parent, Qt.WindowType.FramelessWindowHint | Qt.WindowType.SplashScreen)
        self.setAttribute(Qt.WidgetAttribute.WA_DeleteOnClose)
        self.setWindowModality(Qt.WindowModality.ApplicationModal)

        palette = _PALETTES.get(crt_flavour, _PALETTES["default"])
        self._bg = palette["bg"]
        self._phosphor = palette["phosphor"]
        self._dim = palette["dim"]
        self._bloom = palette["bloom"]

        # Animation state (all 0.0–1.0)
        self._bloom_alpha: float = 0.0      # phase 1 flash
        self._scan_pos: float = -1.0        # phase 2 scan bar y position (−1 = hidden)
        self._text_reveal: float = 0.0      # phase 3 text chars revealed (0-1)
        self._boot_line: int = 0            # which boot message line is shown
        self._flicker: float = 1.0          # subtle screen flicker

        self._phase = 0
        self._timer = QTimer(self)
        self._timer.setInterval(16)  # ~60 fps
        self._timer.timeout.connect(self._tick)
        self._elapsed = 0

        # Resize to fill the primary screen
        screen = QApplication.primaryScreen()
        if screen:
            self.setGeometry(screen.geometry())
        else:
            self.resize(800, 600)

        self.finished = _Signal_Shim(self)

    # ------------------------------------------------------------------
    # Animation tick
    # ------------------------------------------------------------------

    _PHASE_DURATIONS = [250, 500, 1650]  # ms per phase

    def start(self) -> None:
        self._timer.start()

    def _tick(self) -> None:
        self._elapsed += 16
        import random as _random

        # Phase 0: bloom flash
        if self._phase == 0:
            t = self._elapsed / self._PHASE_DURATIONS[0]
            # rises quickly then decays to a small glow
            self._bloom_alpha = max(0.0, 1.0 - t * 2.0) + 0.05
            if self._elapsed >= self._PHASE_DURATIONS[0]:
                self._phase = 1
                self._elapsed = 0

        # Phase 1: scan bar wipe
        elif self._phase == 1:
            t = self._elapsed / self._PHASE_DURATIONS[1]
            self._scan_pos = t  # 0 (top) → 1 (bottom)
            self._bloom_alpha = 0.05 + 0.02 * math.sin(self._elapsed * 0.05)
            if self._elapsed >= self._PHASE_DURATIONS[1]:
                self._phase = 2
                self._elapsed = 0
                self._scan_pos = -1.0

        # Phase 2: text reveal + boot messages
        elif self._phase == 2:
            total = self._PHASE_DURATIONS[2]
            t = self._elapsed / total
            self._text_reveal = min(1.0, t * 1.4)  # title done at 70%
            self._boot_line = min(
                len(_BOOT_MESSAGES),
                int((t - 0.3) * len(_BOOT_MESSAGES) / 0.6) + 1,
            )
            self._flicker = 0.93 + 0.07 * _random.random()
            if self._elapsed >= total:
                self._timer.stop()
                self.finished._emit()
                return

        self.update()

    # ------------------------------------------------------------------
    # Painting
    # ------------------------------------------------------------------

    def paintEvent(self, _event) -> None:  # type: ignore[override]
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        w, h = self.width(), self.height()

        # Background
        p.fillRect(0, 0, w, h, self._bg)

        # Phosphor bloom radial glow at centre
        if self._bloom_alpha > 0:
            grad = QRadialGradient(w / 2, h / 2, max(w, h) * 0.6)
            c = QColor(self._bloom)
            c.setAlphaF(min(1.0, self._bloom_alpha * 0.8))
            grad.setColorAt(0.0, c)
            grad.setColorAt(1.0, QColor(0, 0, 0, 0))
            p.fillRect(0, 0, w, h, grad)

        # Scan bar (phase 1)
        if 0.0 <= self._scan_pos <= 1.0:
            bar_h = max(6, h // 30)
            bar_y = int(self._scan_pos * (h + bar_h)) - bar_h
            sg = QLinearGradient(0, bar_y, 0, bar_y + bar_h)
            bright = QColor(self._phosphor)
            bright.setAlphaF(0.9)
            sg.setColorAt(0.0, QColor(0, 0, 0, 0))
            sg.setColorAt(0.4, bright)
            sg.setColorAt(0.6, bright)
            sg.setColorAt(1.0, QColor(0, 0, 0, 0))
            p.fillRect(0, bar_y, w, bar_h, sg)

        # Scanlines
        p.setPen(QPen(QColor(0, 0, 0, 60)))
        for y in range(0, h, 3):
            p.drawLine(0, y, w, y)

        # Text (phase 2)
        if self._text_reveal > 0:
            self._paint_text(p, w, h)

        # Vignette
        vg = QRadialGradient(w / 2, h / 2, max(w, h) * 0.7)
        vg.setColorAt(0.6, QColor(0, 0, 0, 0))
        vg.setColorAt(1.0, QColor(0, 0, 0, 160))
        p.fillRect(0, 0, w, h, vg)

        # Subtle flicker
        if self._flicker < 1.0:
            p.fillRect(
                0, 0, w, h,
                QColor(0, 0, 0, int((1.0 - self._flicker) * 60)),
            )

        p.end()

    def _paint_text(self, p: QPainter, w: int, h: int) -> None:
        font = QFont("Courier New", max(10, min(18, w // 55)))
        font.setBold(True)
        p.setFont(font)
        fm = p.fontMetrics()

        total_chars = sum(len(line) for line in _TITLE_LINES)
        shown = int(self._text_reveal * total_chars)

        block_h = fm.height() * (len(_TITLE_LINES) + 2)
        y0 = (h - block_h) // 2 - h // 10

        # Title box, revealed character by character
        chars_left = shown
        for i, line in enumerate(_TITLE_LINES):
            line_y = y0 + i * fm.height()
            if chars_left <= 0:
                break
            visible = line[: min(len(line), chars_left)]
            chars_left -= len(line)

            # Centre the line
            x0 = (w - fm.horizontalAdvance(line)) // 2

            # Dim version first
            p.setPen(self._dim)
            p.drawText(x0, line_y, line)

            # Bright overlay for revealed chars
            p.setPen(self._phosphor)
            p.drawText(x0, line_y, visible)

        # Boot messages below title
        msg_y = y0 + len(_TITLE_LINES) * fm.height() + fm.height() * 2
        small_font = QFont("Courier New", max(8, min(12, w // 80)))
        p.setFont(small_font)
        sfm = p.fontMetrics()
        for i in range(min(self._boot_line, len(_BOOT_MESSAGES))):
            x0 = (w - sfm.horizontalAdvance(_BOOT_MESSAGES[i])) // 2
            p.setPen(self._dim)
            p.drawText(x0, msg_y + i * sfm.height(), _BOOT_MESSAGES[i])

        # Cursor blink after last char
        if shown >= total_chars:
            import time as _time
            if int(_time.time() * 2) % 2:
                last_y = y0 + (len(_TITLE_LINES) - 1) * fm.height()
                cx = (w + fm.horizontalAdvance(_TITLE_LINES[-1])) // 2
                p.setPen(self._phosphor)
                p.drawText(cx + 4, last_y, "█")


class _Signal_Shim:
    """Minimal callable-as-signal shim so RetroBootScreen works without
    inheriting from QObject (avoids double-QObject hierarchy issues)."""

    def __init__(self, owner: QWidget) -> None:
        self._callbacks: list = []
        self._owner = owner

    def connect(self, fn) -> None:
        self._callbacks.append(fn)

    def _emit(self) -> None:
        for fn in self._callbacks:
            fn()
