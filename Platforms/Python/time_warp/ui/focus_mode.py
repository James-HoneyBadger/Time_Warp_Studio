"""Focus Mode - Minimal UI toggle for distraction-free coding."""

# pylint: disable=no-name-in-module

from __future__ import annotations

from typing import TYPE_CHECKING

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QWidget

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
        mw = self.main_window

        # Hide menu bar
        if hasattr(mw, "menuBar"):
            menu_bar = mw.menuBar()
            if menu_bar:
                self.saved_states["menuBar"] = menu_bar.isVisible()
                menu_bar.setVisible(False)

        # Hide status bar
        if hasattr(mw, "statusBar"):
            status_bar = mw.statusBar()
            if status_bar:
                self.saved_states["statusBar"] = status_bar.isVisible()
                status_bar.setVisible(False)

        # Hide toolbars
        if hasattr(mw, "findChildren"):
            # pylint: disable=import-outside-toplevel
            from PySide6.QtWidgets import QToolBar

            toolbars = mw.findChildren(QToolBar)
            for i, toolbar in enumerate(toolbars):
                key = f"toolbar_{i}"
                self.saved_states[key] = toolbar.isVisible()
                toolbar.setVisible(False)

        # Hide the right panel (output / canvas / debug / variables)
        if hasattr(mw, "right_tabs"):
            self.saved_states["right_tabs"] = mw.right_tabs.isVisible()
            mw.right_tabs.setVisible(False)

        # Hide the REPL / immediate-mode bar
        if hasattr(mw, "immediate_mode"):
            self.saved_states["immediate_mode"] = mw.immediate_mode.isVisible()
            mw.immediate_mode.setVisible(False)

        # Enter fullscreen
        self.saved_states["was_maximized"] = mw.isMaximized()
        mw.showFullScreen()

    def _restore_ui_elements(self):
        """Restore UI elements to their saved states."""
        mw = self.main_window

        # Restore menu bar
        if hasattr(mw, "menuBar"):
            menu_bar = mw.menuBar()
            if menu_bar and "menuBar" in self.saved_states:
                menu_bar.setVisible(self.saved_states["menuBar"])

        # Restore status bar
        if hasattr(mw, "statusBar"):
            status_bar = mw.statusBar()
            if status_bar and "statusBar" in self.saved_states:
                status_bar.setVisible(self.saved_states["statusBar"])

        # Restore toolbars
        if hasattr(mw, "findChildren"):
            # pylint: disable=import-outside-toplevel
            from PySide6.QtWidgets import QToolBar

            toolbars = mw.findChildren(QToolBar)
            for i, toolbar in enumerate(toolbars):
                key = f"toolbar_{i}"
                if key in self.saved_states:
                    toolbar.setVisible(self.saved_states[key])

        # Restore right panel
        if hasattr(mw, "right_tabs") and "right_tabs" in self.saved_states:
            mw.right_tabs.setVisible(self.saved_states["right_tabs"])

        # Restore REPL bar
        if hasattr(mw, "immediate_mode") and "immediate_mode" in self.saved_states:
            mw.immediate_mode.setVisible(self.saved_states["immediate_mode"])

        # Restore window mode
        if self.saved_states.get("was_maximized"):
            mw.showMaximized()
        else:
            mw.showNormal()

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
