"""Export / print mixin — file export and printing methods.

Extracted from ``MainWindow`` to reduce file size. Handles PNG, SVG,
HTML session export, PDF export, and printing of code and graphics.
"""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

from PySide6.QtCore import Qt
from PySide6.QtPrintSupport import QPrinter
from PySide6.QtWidgets import QFileDialog, QMessageBox

if TYPE_CHECKING:
    pass


class ExportMixin:
    """Export and print methods mixed into MainWindow."""

    def _export_to_png(self, _checked: bool = False):
        """Export canvas graphics to PNG file."""
        filepath, _ = QFileDialog.getSaveFileName(
            self, "Export to PNG", "", "PNG Images (*.png);;All Files (*)"
        )
        if filepath:
            if not filepath.lower().endswith(".png"):
                filepath += ".png"
            if self.canvas.export_to_png(filepath):
                self.statusbar.showMessage(f"Exported to {filepath}", 3000)
            else:
                self.statusbar.showMessage("Export failed", 3000)

    def _export_to_svg(self, _checked: bool = False):
        """Export canvas graphics to SVG file."""
        filepath, _ = QFileDialog.getSaveFileName(
            self, "Export to SVG", "", "SVG Files (*.svg);;All Files (*)"
        )
        if filepath:
            if not filepath.lower().endswith(".svg"):
                filepath += ".svg"
            if self.canvas.export_to_svg(filepath):
                self.statusbar.showMessage(f"Exported to {filepath}", 3000)
            else:
                self.statusbar.showMessage("Export failed", 3000)

    def _export_session_html(self, _checked: bool = False):
        """Export the current session (code + output) as a self-contained HTML file."""
        filepath, _ = QFileDialog.getSaveFileName(
            self, "Export Session as HTML", "", "HTML Files (*.html);;All Files (*)"
        )
        if not filepath:
            return
        if not filepath.lower().endswith((".html", ".htm")):
            filepath += ".html"

        # Gather content
        editor = self.get_current_editor()
        code_text = editor.toPlainText() if editor else ""
        output_text = (
            self.output.toPlainText() if hasattr(self.output, "toPlainText") else ""
        )

        # Detect language label
        lang_name = "Unknown"
        if hasattr(self, "language_combo"):
            lang_name = self.language_combo.currentText().replace("💻 ", "")

        import datetime
        import html as _html

        timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        def _esc(s: str) -> str:
            return _html.escape(s)

        html_content = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Time Warp Studio Session — {_esc(lang_name)}</title>
<style>
  body {{ font-family: system-ui, sans-serif; background: #1e1e2e; color: #cdd6f4;
         margin: 0; padding: 1.5rem; }}
  h1   {{ font-size: 1.4rem; color: #89b4fa; margin-bottom: .25rem; }}
  .meta {{ font-size: .8rem; color: #6c7086; margin-bottom: 1.5rem; }}
  h2   {{ font-size: 1rem; color: #a6e3a1; margin: 1.2rem 0 .4rem; }}
  pre  {{ background: #181825; border: 1px solid #313244; border-radius: 6px;
         padding: 1rem; overflow-x: auto; white-space: pre-wrap; word-break: break-word;
         font-family: "Fira Code", "Courier New", monospace; font-size: .9rem;
         line-height: 1.5; }}
  .code {{ color: #cba6f7; }}
  .output {{ color: #a6e3a1; }}
</style>
</head>
<body>
<h1>🕹️ Time Warp Studio Session</h1>
<div class="meta">Language: <strong>{_esc(lang_name)}</strong> &nbsp;|&nbsp; Exported: {_esc(timestamp)}</div>
<h2>📝 Source Code</h2>
<pre class="code">{_esc(code_text)}</pre>
<h2>📤 Output</h2>
<pre class="output">{_esc(output_text)}</pre>
</body>
</html>
"""
        try:
            with open(filepath, "w", encoding="utf-8") as fh:
                fh.write(html_content)
            self.statusbar.showMessage(f"Session exported to {filepath}", 4000)
        except OSError as exc:
            self.statusbar.showMessage(f"Export failed: {exc}", 4000)

    def _print_code(self, _checked: bool = False):
        """Print the current code."""
        try:
            # pylint: disable=import-outside-toplevel
            from PySide6.QtPrintSupport import QPrintDialog
        except ImportError:
            self.statusbar.showMessage("Print support not available", 3000)
            return

        ed = self.get_current_editor()
        if not ed:
            return

        printer = QPrinter(QPrinter.PrinterMode.HighResolution)
        dialog = QPrintDialog(printer, self)
        if dialog.exec():
            ed.print_(printer)

    def _print_graphics(self, _checked: bool = False):
        """Print the canvas graphics."""
        try:
            # pylint: disable=import-outside-toplevel
            from PySide6.QtGui import QPainter
            from PySide6.QtPrintSupport import QPrintDialog
        except ImportError:
            self.statusbar.showMessage("Print support not available", 3000)
            return

        printer = QPrinter(QPrinter.PrinterMode.HighResolution)
        dialog = QPrintDialog(printer, self)
        if dialog.exec():
            image = self.canvas.get_print_document()
            painter = QPainter(printer)
            rect = painter.viewport()
            size = image.size()
            size.scale(rect.size(), Qt.AspectRatioMode.KeepAspectRatio)
            painter.setViewport(rect.x(), rect.y(), size.width(), size.height())
            painter.setWindow(image.rect())
            painter.drawImage(0, 0, image)
            painter.end()

    def _export_code_pdf(self, _checked: bool = False):
        """Export the current code as a PDF file."""
        try:
            # pylint: disable=import-outside-toplevel
            from PySide6.QtPrintSupport import QPrinter as _QPrinter
        except ImportError:
            self.statusbar.showMessage("PDF export not available", 3000)
            return

        ed = self.get_current_editor()
        if not ed:
            return

        default_name = ""
        info = self.get_current_tab_info()
        if info.get("file"):
            default_name = str(Path(info["file"]).with_suffix(".pdf"))

        filepath, _ = QFileDialog.getSaveFileName(
            self, "Export Code as PDF", default_name, "PDF Files (*.pdf)"
        )
        if not filepath:
            return

        printer = _QPrinter(_QPrinter.PrinterMode.HighResolution)
        printer.setOutputFormat(_QPrinter.OutputFormat.PdfFormat)
        printer.setOutputFileName(filepath)
        ed.print_(printer)
        self.statusbar.showMessage(f"PDF saved: {filepath}", 3000)

    def _show_version_history(self, _checked: bool = False):
        """Open the version history browser for the current file."""
        info = self.get_current_tab_info()
        file_path = info.get("file")
        if not file_path:
            QMessageBox.information(
                self,
                "Version History",
                "Save the file first to track its version history.",
            )
            return

        from ..version_history_dialog import VersionHistoryDialog

        ed = self.get_current_editor()
        current_content = ed.toPlainText() if ed else ""
        path = Path(file_path)

        dlg = VersionHistoryDialog(
            self._autosave_manager, path, current_content, parent=self
        )
        if dlg.exec() == VersionHistoryDialog.Accepted:
            content = dlg.restored_content
            if content is not None and ed is not None:
                ed.setPlainText(content)
                self.set_current_tab_info(modified=True)
                self.statusbar.showMessage("Version restored", 3000)
