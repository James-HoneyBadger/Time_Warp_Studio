"""Focus Mode - Minimal UI toggle for distraction-free coding."""

# pylint: disable=no-name-in-module

from __future__ import annotations
from typing import TYPE_CHECKING
from PySide6.QtWidgets import QWidget
from PySide6.QtCore import QObject, Signal

if TYPE_CHECKING:
    from PySide6.QtWidgets import QMainWindow


class FocusModeManager(QObject):
    """Manages focus mode state and UI transitions."""

    focus_mode_changed = Signal(bool)

    def __init__(self, main_window: QMainWindow):
        super().__init__(main_window)
        self.main_window = main_window
        self.is_focus_mode = False
        self.tooltips_enabled = True

        # Store original visibility states
        self.saved_states: dict[str, bool] = {}

    def toggle_focus_mode(self):
        """Toggle between focus and normal mode."""
        self.is_focus_mode = not self.is_focus_mode
        self._apply_focus_mode(self.is_focus_mode)
        self.focus_mode_changed.emit(self.is_focus_mode)

    def enable_focus_mode(self):
        """Enter focus mode."""
        if not self.is_focus_mode:
            self.toggle_focus_mode()

    def disable_focus_mode(self):
        """Exit focus mode."""
        if self.is_focus_mode:
            self.toggle_focus_mode()

    def _apply_focus_mode(self, enabled: bool):
        """Apply or restore UI visibility states."""
        if enabled:
            self._save_and_hide_ui_elements()
        else:
            self._restore_ui_elements()

    def _save_and_hide_ui_elements(self):
        """Hide non-essential UI elements and save their states."""
        self.saved_states.clear()

        # Hide menu bar
        if hasattr(self.main_window, "menuBar"):
            menu_bar = self.main_window.menuBar()
            if menu_bar:
                self.saved_states["menuBar"] = menu_bar.isVisible()
                menu_bar.setVisible(False)

        # Hide status bar
        if hasattr(self.main_window, "statusBar"):
            status_bar = self.main_window.statusBar()
            if status_bar:
                self.saved_states["statusBar"] = status_bar.isVisible()
                status_bar.setVisible(False)

        # Hide toolbars
        if hasattr(self.main_window, "findChildren"):
            # pylint: disable=import-outside-toplevel
            from PySide6.QtWidgets import QToolBar

            toolbars = self.main_window.findChildren(QToolBar)
            for i, toolbar in enumerate(toolbars):
                key = f"toolbar_{i}"
                self.saved_states[key] = toolbar.isVisible()
                toolbar.setVisible(False)

        # Hide dock widgets (keep only editor and output)
        if hasattr(self.main_window, "findChildren"):
            # pylint: disable=import-outside-toplevel
            from PySide6.QtWidgets import QDockWidget

            docks = self.main_window.findChildren(QDockWidget)
            for i, dock in enumerate(docks):
                # Keep essential docks visible
                if "output" in dock.windowTitle().lower():
                    continue

                key = f"dock_{i}"
                self.saved_states[key] = dock.isVisible()
                dock.setVisible(False)

    def _restore_ui_elements(self):
        """Restore UI elements to their saved states."""
        # Restore menu bar
        if hasattr(self.main_window, "menuBar"):
            menu_bar = self.main_window.menuBar()
            if menu_bar and "menuBar" in self.saved_states:
                menu_bar.setVisible(self.saved_states["menuBar"])

        # Restore status bar
        if hasattr(self.main_window, "statusBar"):
            status_bar = self.main_window.statusBar()
            if status_bar and "statusBar" in self.saved_states:
                status_bar.setVisible(self.saved_states["statusBar"])

        # Restore toolbars
        if hasattr(self.main_window, "findChildren"):
            # pylint: disable=import-outside-toplevel
            from PySide6.QtWidgets import QToolBar

            toolbars = self.main_window.findChildren(QToolBar)
            for i, toolbar in enumerate(toolbars):
                key = f"toolbar_{i}"
                if key in self.saved_states:
                    toolbar.setVisible(self.saved_states[key])

        # Restore dock widgets
        if hasattr(self.main_window, "findChildren"):
            # pylint: disable=import-outside-toplevel
            from PySide6.QtWidgets import QDockWidget

            docks = self.main_window.findChildren(QDockWidget)
            for i, dock in enumerate(docks):
                key = f"dock_{i}"
                if key in self.saved_states:
                    dock.setVisible(self.saved_states[key])

        self.saved_states.clear()

    def toggle_tooltips(self):
        """Toggle tooltip visibility."""
        self.tooltips_enabled = not self.tooltips_enabled
        self._apply_tooltip_state(self.tooltips_enabled)

    def _apply_tooltip_state(self, enabled: bool):
        """Enable or disable tooltips globally."""
        if hasattr(self.main_window, "findChildren"):
            widgets = self.main_window.findChildren(QWidget)
            for widget in widgets:
                if not enabled:
                    widget.setToolTip("")
                # Note: Original tooltips would need to be saved/restored
                # for full functionality

    def set_noise_level(self, level: str):
        """
        Set UI noise level.

        Args:
            level: 'silent', 'quiet', 'normal', 'verbose'
        """
        # Implementation would filter status messages based on level
        # Placeholder for future implementation

    def get_focus_mode_shortcuts(self) -> dict[str, str]:
        """Get keyboard shortcuts for focus mode."""
        return {
            "toggle_focus": "F11",
            "toggle_tooltips": "Ctrl+Shift+T",
            "toggle_menu": "Alt+M",
            "toggle_status": "Alt+S",
        }
