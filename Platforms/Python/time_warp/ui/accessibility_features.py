"""High-contrast toggle (Ctrl+Alt+H) for Time Warp Studio."""

from PySide6.QtGui import QKeySequence, QShortcut
from PySide6.QtWidgets import QMainWindow

HIGH_CONTRAST_THEME = "High Contrast Dark"


class AccessibilityFeatures:
    """Toggle between the user's theme and the high-contrast theme."""

    def __init__(self, main_window: QMainWindow):
        self.main_window = main_window
        self._previous_theme: str | None = None
        self.high_contrast_shortcut = QShortcut(
            QKeySequence("Ctrl+Alt+H"), self.main_window
        )
        self.high_contrast_shortcut.activated.connect(self.toggle_high_contrast)

    @property
    def high_contrast_active(self) -> bool:
        return self._previous_theme is not None

    def toggle_high_contrast(self) -> None:
        tm = self.main_window.theme_manager
        if self._previous_theme is None:
            self._previous_theme = tm.current_theme_name
            self.main_window.change_theme(HIGH_CONTRAST_THEME)
        else:
            self.main_window.change_theme(self._previous_theme)
            self._previous_theme = None
