"""Coach-mark onboarding overlay for Time Warp Studio.

Presents interactive step-by-step highlight callouts on first launch to
orient new users to the key areas of the IDE.
"""

# pylint: disable=no-name-in-module

from PySide6.QtCore import QRect, QSettings, Qt, QTimer

from ..core.config import QSETTINGS_APP, QSETTINGS_ORG
from PySide6.QtGui import QColor, QFont, QPainter, QPainterPath, QPen
from PySide6.QtWidgets import (
    QDialog,
    QHBoxLayout,
    QLabel,
    QPushButton,
    QVBoxLayout,
    QWidget,
)


class CoachMarkOverlay(QWidget):
    """Transparent full-window overlay that draws a highlight spotlight.

    It is laid over the parent window and draws a semi-transparent dimming
    layer with a rect cut-out around the target widget, plus a callout
    bubble containing the step description text.
    """

    def __init__(self, parent):
        super().__init__(parent)
        self.setAttribute(Qt.WA_TransparentForMouseEvents, False)
        self.setAttribute(Qt.WA_TranslucentBackground, True)
        self.setWindowFlags(Qt.SubWindow)

        self._target_rect: QRect = QRect()
        self._callout_title: str = ""
        self._callout_body: str = ""
        self._callout_above: bool = False  # draw callout above or below target

        # Forward / back / close controls sit in a floating panel
        self._panel = QWidget(self)
        panel_layout = QVBoxLayout(self._panel)
        panel_layout.setContentsMargins(12, 10, 12, 10)
        panel_layout.setSpacing(6)

        self._title_label = QLabel()
        self._title_label.setAlignment(Qt.AlignCenter)
        self._title_label.setFont(QFont("Arial", 12, QFont.Bold))
        self._title_label.setStyleSheet("color: white;")
        panel_layout.addWidget(self._title_label)

        self._body_label = QLabel()
        self._body_label.setAlignment(Qt.AlignCenter)
        self._body_label.setWordWrap(True)
        self._body_label.setStyleSheet("color: #ddd;")
        self._body_label.setMaximumWidth(320)
        panel_layout.addWidget(self._body_label)

        btn_row = QHBoxLayout()
        self._prev_btn = QPushButton("◀ Back")
        self._prev_btn.setFixedWidth(80)
        self._next_btn = QPushButton("Next ▶")
        self._next_btn.setFixedWidth(80)
        self._close_btn = QPushButton("✕ Close")
        self._close_btn.setFixedWidth(80)
        self._close_btn.setStyleSheet(
            "QPushButton { background: #e74c3c; color: white; font-weight: bold; "
            "border-radius: 4px; }"
            "QPushButton:hover { background: #c0392b; }"
        )
        btn_row.addWidget(self._prev_btn)
        btn_row.addWidget(self._next_btn)
        btn_row.addWidget(self._close_btn)
        panel_layout.addLayout(btn_row)

        self._step_label = QLabel()
        self._step_label.setAlignment(Qt.AlignCenter)
        self._step_label.setStyleSheet("color: #aaa; font-size: 10px;")
        panel_layout.addWidget(self._step_label)

        self._panel.setStyleSheet("""
            QWidget {
                background-color: rgba(30, 30, 50, 220);
                border-radius: 10px;
                border: 2px solid rgba(255, 255, 255, 0.3);
            }
            QPushButton {
                background-color: rgba(255,255,255,0.12);
                color: white;
                border: 1px solid rgba(255,255,255,0.3);
                border-radius: 4px;
                padding: 4px 8px;
                font-weight: bold;
            }
            QPushButton:hover {
                background-color: rgba(255,255,255,0.22);
            }
        """)
        self._panel.adjustSize()

    # ------------------------------------------------------------------
    # Visual update
    # ------------------------------------------------------------------

    def set_step(
        self,
        target_rect: QRect,
        title: str,
        body: str,
        step: int,
        total: int,
        above: bool = False,
    ):
        """Configure the overlay for the given step."""
        self._target_rect = target_rect
        self._callout_title = title
        self._callout_body = body
        self._callout_above = above

        self._title_label.setText(title)
        self._body_label.setText(body)
        self._step_label.setText(f"Step {step} of {total}")

        self._prev_btn.setEnabled(step > 1)

        # Position the callout panel
        self._panel.adjustSize()
        pw = self._panel.width()
        ph = self._panel.height()
        cx = target_rect.center().x() - pw // 2
        cx = max(10, min(cx, self.width() - pw - 10))
        if above:
            cy = target_rect.top() - ph - 20
        else:
            cy = target_rect.bottom() + 20
        cy = max(10, min(cy, self.height() - ph - 10))
        self._panel.move(cx, cy)

        self.update()

    # ------------------------------------------------------------------
    # Painting
    # ------------------------------------------------------------------

    def paintEvent(self, _event):
        """Dim everything except the target rectangle spotlight."""
        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing)

        # Full dim layer
        path = QPainterPath()
        path.addRect(0, 0, self.width(), self.height())

        if not self._target_rect.isNull():
            # Cut out the spotlight
            spotlight = QPainterPath()
            r = self._target_rect.adjusted(-6, -6, 6, 6)
            spotlight.addRoundedRect(r, 8, 8)
            path = path.subtracted(spotlight)

        painter.fillPath(path, QColor(0, 0, 0, 160))

        # Highlight ring around spotlight
        if not self._target_rect.isNull():
            painter.setPen(QPen(QColor(81, 162, 255, 200), 2))
            painter.drawRoundedRect(self._target_rect.adjusted(-6, -6, 6, 6), 8, 8)


class CoachMarkManager:
    """Manages the multi-step coach mark walkthrough.

    Create one instance, call ``start()`` to begin the first-launch tour.
    The tour is only shown once; subsequent launches are silently skipped.
    """

    # Ordered list of (attr_name, title, description)
    # attr_name must match an attribute on the main window
    STEPS = [
        (
            "editor_tabs",
            "📝 Code Editor",
            "Write your programs here. Switch between multiple open files using "
            "the tabs.  All 24 languages are supported!",
        ),
        (
            "language_combo",
            "💻 Language Selector",
            "Pick a programming language from the drop-down and the editor will "
            "apply matching syntax highlighting and execution engine.",
        ),
        (
            "output",
            "📋 Output Panel",
            "Program results appear here.  Errors are shown in red with "
            "suggestions on how to fix them.",
        ),
        (
            "canvas",
            "🎨 Graphics Canvas",
            "Logo, BASIC SCREEN, and other graphical output is rendered here. "
            "Supports zoom and pan.",
        ),
        (
            "immediate_mode",
            "⚡ REPL / Immediate Mode",
            "Type single commands and run them instantly — great for exploring a "
            "language or testing small expressions.",
        ),
        (
            "statusbar",
            "📊 Status Bar",
            "The status bar shows the active language, cursor position, and other "
            "quick indicators.  Click the SQL badge to open the workbench.",
        ),
    ]

    SETTINGS_KEY = "onboarding/coach_marks_shown"

    def __init__(self, main_window):
        self._mw = main_window
        self._overlay: CoachMarkOverlay | None = None
        self._step_index: int = 0

    def should_show(self) -> bool:
        """Return True if the tour has not yet been completed."""
        s = QSettings(QSETTINGS_ORG, QSETTINGS_APP)
        return not s.value(self.SETTINGS_KEY, False, type=bool)

    def mark_shown(self):
        """Persist that the tour has been shown."""
        s = QSettings(QSETTINGS_ORG, QSETTINGS_APP)
        s.setValue(self.SETTINGS_KEY, True)

    def start(self, force: bool = False):
        """Start the coach mark tour.

        Args:
            force: If True, show even if already completed.
        """
        if not force and not self.should_show():
            return

        self._step_index = 0
        self._overlay = CoachMarkOverlay(self._mw.centralWidget())
        self._overlay.resize(self._mw.centralWidget().size())
        self._overlay._next_btn.clicked.connect(self._next_step)
        self._overlay._prev_btn.clicked.connect(self._prev_step)
        self._overlay._close_btn.clicked.connect(self._close)
        self._overlay.show()
        self._show_step()

    # ------------------------------------------------------------------
    # Navigation
    # ------------------------------------------------------------------

    def _widget_for_step(self, attr: str):
        """Return the widget referenced by the step attr (or None)."""
        return getattr(self._mw, attr, None)

    def _show_step(self):
        """Render the current step."""
        if self._overlay is None:
            return
        self._overlay.resize(self._mw.centralWidget().size())

        attr, title, body = self.STEPS[self._step_index]
        widget = self._widget_for_step(attr)

        if widget is not None:
            # Map widget geometry to central widget co-ordinates
            target = widget.rect()
            target_global = widget.mapToGlobal(target.topLeft())
            local = self._mw.centralWidget().mapFromGlobal(target_global)
            rect = QRect(local, target.size())
        else:
            # Fall back to centre of the window
            rect = QRect()

        total = len(self.STEPS)
        above = self._step_index >= total // 2  # flip callout side at midpoint
        self._overlay.set_step(
            rect,
            title,
            body,
            self._step_index + 1,
            total,
            above=above,
        )

        # Update Next button text for last step
        is_last = self._step_index >= total - 1
        self._overlay._next_btn.setText("Finish ✓" if is_last else "Next ▶")

    def _next_step(self):
        total = len(self.STEPS)
        if self._step_index >= total - 1:
            self._close()
        else:
            self._step_index += 1
            self._show_step()

    def _prev_step(self):
        if self._step_index > 0:
            self._step_index -= 1
            self._show_step()

    def _close(self):
        self.mark_shown()
        if self._overlay:
            self._overlay.hide()
            self._overlay.deleteLater()
            self._overlay = None
