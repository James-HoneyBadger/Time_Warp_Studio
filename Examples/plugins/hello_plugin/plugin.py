"""Hello Plugin for Time Warp Studio.

This is the minimal example plugin that demonstrates the plugin API.

What it does:
  - Prints a greeting on startup
  - Optionally logs the current language to the output panel (if IDE available)
  - Prints a goodbye message on shutdown

Copy this directory to ``plugins/hello_plugin/`` in your Time Warp Studio
workspace root to activate it.
"""

from __future__ import annotations

from typing import Any, Optional


class HelloPlugin:
    """Minimal Time Warp Studio plugin — says hello and goodbye."""

    def __init__(self) -> None:
        self.ide: Optional[Any] = None

    def initialize(self, ide: Optional[Any]) -> None:
        """Called once by PluginManager after the IDE has started.

        Args:
            ide: The main IDE window instance (PySide6 QMainWindow subclass),
                 or None when running outside the full IDE (e.g. tests).
        """
        self.ide = ide
        print("✅ Hello Plugin initialized!")
        print("ℹ️  Hello from the plugin system — Time Warp Studio is running.")

        # If running inside the full IDE, try to emit a message to the output panel
        if ide is not None:
            self._greet_via_output_panel()

    def shutdown(self) -> None:
        """Called by PluginManager when the IDE closes."""
        print("ℹ️  Hello Plugin shutting down. Goodbye!")

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _greet_via_output_panel(self) -> None:
        """Write a greeting to the IDE output panel if available."""
        # The output panel attribute name may vary — guard with hasattr
        for attr in ("output_panel", "output_widget", "_output"):
            panel = getattr(self.ide, attr, None)
            if panel is not None and hasattr(panel, "append_text"):
                panel.append_text("ℹ️ Hello Plugin is active!\n")
                return
            if panel is not None and hasattr(panel, "appendPlainText"):
                panel.appendPlainText("ℹ️ Hello Plugin is active!")
                return
        # Fall back to stdout if panel not found
        print("ℹ️ Hello Plugin: output panel not accessible.")
