"""Debug mixin — debugger UI methods.

Extracted from ``MainWindow`` to reduce file size. These methods handle
starting, stopping, stepping through debug sessions, breakpoint
management, and debug timeline export.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import TYPE_CHECKING

from PySide6.QtWidgets import QFileDialog

if TYPE_CHECKING:
    pass


class DebugMixin:
    """Debug-related methods mixed into MainWindow."""

    def _connect_debug_signals(self):
        """Connect debug panel signals to main window methods."""
        self.debug_panel.start_debug.connect(self.start_debug)
        self.debug_panel.stop_debug.connect(self.stop_debug)
        self.debug_panel.step_into.connect(self.debug_step_into)
        self.debug_panel.step_over.connect(self.debug_step_over)
        self.debug_panel.step_out.connect(self.debug_step_out)
        self.debug_panel.continue_execution.connect(self.debug_continue)
        self.debug_panel.pause_execution.connect(self.debug_pause)
        self.debug_panel.goto_line.connect(self._goto_line)
        self.debug_panel.breakpoint_panel.clear_all_requested.connect(
            self.clear_all_breakpoints
        )

        # Connect output panel debug signals
        self.output.debug_paused.connect(self._on_debug_paused)
        self.output.output_streamed.connect(self._on_debug_output_stream)
        self.output.debug_frame_recorded.connect(self.debug_panel.append_timeline_frame)
        self.output.debug_timeline_ready.connect(self.debug_panel.set_timeline)
        self.debug_panel.timeline_frame_selected.connect(
            self._on_timeline_frame_selected
        )
        self.debug_panel.export_timeline_requested.connect(self.export_debug_timeline)
        self.debug_panel.step_granularity_changed.connect(
            self._set_debug_step_granularity
        )
        self.output.debug_timeline_ready.connect(self._on_debug_timeline_ready)

    def _goto_line(self, line: int):
        """Go to a specific line in the current editor."""
        editor = self.get_current_editor()
        if editor:
            editor.goto_line(line)

    def start_debug(self):
        """Start debugging the current program."""
        editor = self.get_current_editor()
        if not editor:
            return

        current_info = self.get_current_tab_info()
        language = current_info["language"]

        # Ensure output uses the current language
        self.output.set_language(language)

        # Switch to debug tab
        self.right_tabs.setCurrentWidget(self.debug_panel)

        code = editor.toPlainText()

        # Get breakpoints from editor
        breakpoints = editor.get_breakpoints()

        # Update debug state
        self._is_debugging = True
        self._is_paused = False
        self._update_debug_ui()

        self.statusbar.showMessage("🐛 Debugging...")

        # Start execution in debug mode
        self.output.run_program(
            code,
            self.canvas,
            debug_mode=True,
            breakpoints=breakpoints,
            debug_step_granularity=self._debug_step_granularity,
        )

        # Update debug panel
        self.debug_panel.set_debugging(True)
        self.debug_panel.update_breakpoints(breakpoints)
        self.debug_panel.clear_output_stream()
        self.debug_panel.clear_timeline()
        self.output.clear_debug_timeline()
        self.debug_panel.clear_timeline()

    def stop_debug(self):
        """Stop debugging."""
        self.output.stop_execution()
        self._is_debugging = False
        self._is_paused = False
        self._current_debug_line = 0

        # Clear current line indicator in editor
        editor = self.get_current_editor()
        if editor:
            editor.clear_current_line()

        self._update_debug_ui()
        self.debug_panel.set_debugging(False)
        self.debug_panel.set_paused(False)
        self.statusbar.showMessage("Debugging stopped")

    def debug_continue(self):
        """Continue execution until next breakpoint."""
        if self._is_debugging:
            self._is_paused = False
            self._update_debug_ui()
            self.output.resume_execution()
            self.debug_panel.set_paused(False)
            self.statusbar.showMessage("▶ Continuing...")

    def debug_pause(self):
        """Pause execution."""
        if self._is_debugging and not self._is_paused:
            if self.output.exec_thread and self.output.exec_thread.interp:
                self.output.exec_thread.interp.pause_execution()
            self.statusbar.showMessage("⏸ Pausing...")

    def debug_step_into(self):
        """Step into the next line."""
        if self._is_debugging and self._is_paused:
            self.output.step_execution()
            self.statusbar.showMessage("↓ Stepping...")

    def debug_step_over(self):
        """Step over the current line (same as step into for now)."""
        self.debug_step_into()

    def debug_step_out(self):
        """Step out of current subroutine (continue until return)."""
        self.debug_continue()

    def _on_debug_paused(self, line: int, variables: dict):
        """Handle debug pause event from interpreter."""
        self._is_paused = True
        self._current_debug_line = line
        self._update_debug_ui()

        # Update editor current line
        editor = self.get_current_editor()
        if editor:
            editor.set_current_line(line)

        # Update debug panel
        self.debug_panel.set_paused(True, line)
        self.debug_panel.update_variables(variables)

        # Update variable inspector
        self.variable_inspector.update_variables(variables)

        # Update turtle state in debug panel
        turtle_state = self._get_turtle_state_snapshot()
        if turtle_state:
            self.debug_panel.update_turtle_state(turtle_state)

        # Get call stack from interpreter if available
        if self.output.exec_thread and self.output.exec_thread.interp:
            interp = self.output.exec_thread.interp
            if hasattr(interp, "call_stack"):
                self.debug_panel.update_call_stack(interp.call_stack)

        self.statusbar.showMessage(f"🔴 Paused at line {line}")

    def _on_timeline_frame_selected(self, frame):
        """Update UI from a selected timeline frame."""
        if not frame:
            return
        line = getattr(frame, "line", 0)
        variables = getattr(frame, "variables", {}) or {}

        editor = self.get_current_editor()
        if editor and line:
            editor.set_current_line(line)

        self.variable_inspector.update_variables(variables)
        if hasattr(self, "debug_panel"):
            self.debug_panel.update_variables(variables)

        snapshot = self.output.get_debug_turtle_snapshot(frame)
        if snapshot:
            self.canvas.set_turtle_state(snapshot)

    def _set_debug_step_granularity(self, granularity: str):
        """Update debug stepping granularity for the next session."""
        self._debug_step_granularity = granularity

    def _on_debug_timeline_ready(self, timeline):
        """Capture latest debug timeline."""
        self._last_debug_timeline = timeline

    def export_debug_timeline(self):
        """Export current debug timeline as JSON."""
        if not self._last_debug_timeline:
            self.statusbar.showMessage("No debug timeline available")
            return
        path, _ = QFileDialog.getSaveFileName(
            self,
            "Export Debug Timeline",
            "debug_timeline.json",
            "JSON Files (*.json);;All Files (*)",
        )
        if not path:
            return
        data = self._last_debug_timeline.export_timeline_json()
        try:
            Path(path).write_text(json.dumps(data, indent=2), encoding="utf-8")
            self.statusbar.showMessage("Debug timeline exported")
        except OSError:
            self.statusbar.showMessage("Failed to export timeline")

    def _on_debug_output_stream(self, text: str, _output_type: str):
        """Forward output stream into the debug panel."""
        self.debug_panel.append_output_stream(text)

    def _get_turtle_state_snapshot(self) -> dict | None:
        """Capture a snapshot of the turtle state if available."""
        if not self.output.exec_thread:
            return None
        turtle = getattr(self.output.exec_thread, "turtle", None)
        if not turtle:
            return None
        return {
            "x": getattr(turtle, "x", 0.0),
            "y": getattr(turtle, "y", 0.0),
            "heading": getattr(turtle, "heading", 0.0),
            "pen_down": getattr(turtle, "pen_down", True),
            "pen_color": getattr(turtle, "pen_color", (255, 255, 255)),
            "pen_width": getattr(turtle, "pen_width", 2.0),
            "visible": getattr(turtle, "visible", True),
            "lines": len(getattr(turtle, "lines", []) or []),
        }

    def _update_debug_ui(self):
        """Update debug-related UI elements."""
        is_debugging = self._is_debugging
        is_paused = self._is_paused

        # Run menu actions
        self.run_action.setEnabled(not is_debugging)
        self.debug_start_action.setEnabled(not is_debugging)
        self.debug_stop_action.setEnabled(is_debugging)
        self.debug_continue_action.setEnabled(is_paused)
        self.debug_pause_action.setEnabled(is_debugging and not is_paused)
        self.debug_step_into_action.setEnabled(is_paused)
        self.debug_step_over_action.setEnabled(is_paused)
        self.debug_step_out_action.setEnabled(is_paused)

        # Toolbar buttons
        if hasattr(self, "debug_btn"):
            self.debug_btn.setEnabled(not is_debugging)
        if hasattr(self, "continue_btn"):
            self.continue_btn.setEnabled(is_paused)
        if hasattr(self, "step_btn"):
            self.step_btn.setEnabled(is_paused)

        self.stop_action.setEnabled(is_debugging)

    def toggle_breakpoint_at_cursor(self):
        """Toggle breakpoint at the current cursor line."""
        editor = self.get_current_editor()
        if editor:
            cursor = editor.textCursor()
            line = cursor.blockNumber() + 1
            editor.toggle_breakpoint(line)
            self._update_breakpoints_display()

    def clear_all_breakpoints(self):
        """Clear all breakpoints in all editors."""
        for i in range(self.editor_tabs.count()):
            editor = self.editor_tabs.widget(i)
            if editor and hasattr(editor, "clear_breakpoints"):
                editor.clear_breakpoints()
        self._update_breakpoints_display()
        self.statusbar.showMessage("All breakpoints cleared")

    def _update_breakpoints_display(self):
        """Update the breakpoints panel with current breakpoints."""
        editor = self.get_current_editor()
        if editor:
            breakpoints = editor.get_breakpoints()
            self.debug_panel.update_breakpoints(breakpoints)
