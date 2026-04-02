"""Classroom / lesson mixin — presentation mode, bundles, lesson export.

Extracted from ``MainWindow`` to reduce file size.
"""

from __future__ import annotations

import html
import time
import uuid
from pathlib import Path
from typing import TYPE_CHECKING

from PySide6.QtGui import QTextDocument
from PySide6.QtPrintSupport import QPrinter
from PySide6.QtWidgets import QFileDialog

if TYPE_CHECKING:
    from ..collaboration_client import CollaborationOperation  # noqa: F401


class ClassroomMixin:
    """Classroom, lesson, and presentation methods mixed into MainWindow."""

    def start_presentation_mode(self, settings: dict):
        """Enable presentation mode with classroom settings."""
        self.classroom_mode.start_presentation(
            font_size=settings.get("font_size", 16),
            fullscreen=settings.get("fullscreen", True),
        )

        if self._pre_presentation_font_size is None:
            self._pre_presentation_font_size = self.theme_manager.current_font_size
        if self._pre_presentation_menu_visible is None:
            self._pre_presentation_menu_visible = self.menuBar().isVisible()
        if self._pre_presentation_was_fullscreen is None:
            self._pre_presentation_was_fullscreen = self.isFullScreen()

        if settings.get("read_only", True):
            self._set_editors_read_only(True)
        if settings.get("font_size"):
            self.change_font_size(settings["font_size"])
        if settings.get("hide_menus"):
            self.menuBar().setVisible(False)
        if settings.get("fullscreen", True):
            self.showFullScreen()

        self._classroom_lock_theme = bool(settings.get("lock_theme"))
        self.statusbar.showMessage("Presentation mode enabled")

    def stop_presentation_mode(self):
        """Disable presentation mode and restore settings."""
        self.classroom_mode.stop_presentation()
        self._set_editors_read_only(False)

        if self._pre_presentation_menu_visible is not None:
            self.menuBar().setVisible(self._pre_presentation_menu_visible)
        if self._pre_presentation_font_size is not None:
            self.change_font_size(self._pre_presentation_font_size)
        if self._pre_presentation_was_fullscreen:
            self.showFullScreen()
        else:
            self.showNormal()

        self._classroom_lock_theme = False
        self.statusbar.showMessage("Presentation mode disabled")

    def _set_editors_read_only(self, read_only: bool):
        """Toggle read-only mode for all editors."""
        for i in range(self.editor_tabs.count()):
            editor = self.editor_tabs.widget(i)
            if editor:
                editor.setReadOnly(read_only)

    def export_classroom_bundle(self, name: str, description: str):
        """Export a classroom bundle from open tabs."""

        files = {}
        for i in range(self.editor_tabs.count()):
            editor = self.editor_tabs.widget(i)
            if not editor:
                continue
            filename = self._ts(i).file
            if filename:
                file_key = Path(filename).name
            else:
                language = self._ts(i).language
                ext = self._language_extension(language)
                file_key = f"untitled_{i + 1}{ext}"
            files[file_key] = editor.toPlainText()

        settings = {
            "theme": self.theme_manager.current_theme_name,
            "font_size": self.theme_manager.current_font_size,
        }
        bundle = self.classroom_mode.create_bundle(
            name=name,
            description=description,
            files=files,
            settings=settings,
        )

        path, _ = QFileDialog.getSaveFileName(
            self,
            "Export Classroom Bundle",
            "",
            "Bundle Files (*.zip);;All Files (*)",
        )
        if not path:
            return
        if not path.lower().endswith(".zip"):
            path += ".zip"

        success = self.classroom_mode.export_bundle(bundle, Path(path))
        if success:
            self.statusbar.showMessage("Bundle exported")
        else:
            self.statusbar.showMessage("Bundle export failed")

    def import_classroom_bundle(self, path: str):
        """Import a classroom bundle and open its files."""
        from ...core.interpreter import Language

        bundle = self.classroom_mode.import_bundle(Path(path))
        if not bundle:
            self.statusbar.showMessage("Bundle import failed")
            return

        for filename, content in bundle.files.items():
            language = Language.from_extension(Path(filename).suffix)
            self.create_new_tab(Path(filename).name, content, language)
            self.set_current_tab_info(file=None, modified=False, language=language)

        theme_name = bundle.settings.get("theme")
        if theme_name:
            self.change_theme(theme_name)

        self.statusbar.showMessage(f"Imported bundle: {bundle.name}")

    def broadcast_sample_code(self):
        """Broadcast current code to collaboration session if connected."""
        if not self.collaboration_client.connected:
            self.statusbar.showMessage("Collaboration not connected")
            return
        editor = self.get_current_editor()
        if not editor:
            return

        from ..collaboration_client import CollaborationOperation  # noqa: F811

        operation = CollaborationOperation(
            id=uuid.uuid4().hex,
            user_id=(
                self.collaboration_client.user.id
                if self.collaboration_client.user
                else ""
            ),
            type="replace",
            position=0,
            content=editor.toPlainText(),
            timestamp=time.time(),
        )

        if self.collaboration_client.send_operation(operation):
            self.statusbar.showMessage("Broadcasted sample code")
        else:
            self.statusbar.showMessage("Broadcast failed")

    def collect_student_outputs(self):
        """Save current output log for classroom collection."""
        path, _ = QFileDialog.getSaveFileName(
            self,
            "Save Output Log",
            "",
            "Text Files (*.txt);;All Files (*)",
        )
        if not path:
            return
        if not path.lower().endswith(".txt"):
            path += ".txt"

        try:
            with open(path, "w", encoding="utf-8") as handle:
                handle.write(self.output.toPlainText())
            self.statusbar.showMessage("Output log saved")
        except OSError:
            self.statusbar.showMessage("Failed to save output log")

    def _apply_lesson_code(self, language_name: str, code: str):
        """Load lesson starter code into the current editor."""
        from ...core.interpreter import Language

        editor = self.get_current_editor()
        if not editor:
            self.new_file()
            editor = self.get_current_editor()
        if not editor:
            return

        language_map = {
            "basic": Language.BASIC,
            "pilot": Language.PILOT,
            "logo": Language.LOGO,
            "c": Language.C,
            "pascal": Language.PASCAL,
            "prolog": Language.PROLOG,
            "forth": Language.FORTH,
            "python": Language.PYTHON,
            "lua": Language.LUA,
            "scheme": Language.SCHEME,
            "cobol": Language.COBOL,
            "brainfuck": Language.BRAINFUCK,
            "assembly": Language.ASSEMBLY,
            "javascript": Language.JAVASCRIPT,
            "fortran": Language.FORTRAN,
            "rexx": Language.REXX,
            "smalltalk": Language.SMALLTALK,
            "hypertalk": Language.HYPERTALK,
            "haskell": Language.HASKELL,
            "apl": Language.APL,
            "sql": Language.SQL,
            "sql server": Language.SQL,
            "jcl": Language.JCL,
            "cics": Language.CICS,
        }
        lang = language_map.get(language_name.lower(), Language.BASIC)

        editor.setPlainText(code)
        editor.set_language(lang)
        self.set_current_tab_info(modified=True, language=lang)
        self.output.set_language(lang)

        if hasattr(self, "language_combo"):
            for i in range(self.language_combo.count()):
                if self.language_combo.itemData(i) == lang:
                    self.language_combo.setCurrentIndex(i)
                    break

    def export_lesson_session(self, format_name: str):
        """Export lesson session to Markdown or PDF."""
        panel = self.feature_manager.get_feature_panel("lesson_mode")
        if not panel or not hasattr(panel, "get_session_snapshot"):
            self.statusbar.showMessage("Lesson mode not available")
            return

        snapshot = panel.get_session_snapshot()
        editor = self.get_current_editor()
        code = editor.toPlainText() if editor else ""
        output_text = self.output.toPlainText()

        default_name = "lesson_session"
        if format_name == "pdf":
            path, _ = QFileDialog.getSaveFileName(
                self,
                "Export Lesson Session (PDF)",
                f"{default_name}.pdf",
                "PDF Files (*.pdf);;All Files (*)",
            )
        else:
            path, _ = QFileDialog.getSaveFileName(
                self,
                "Export Lesson Session (Markdown)",
                f"{default_name}.md",
                "Markdown Files (*.md);;All Files (*)",
            )

        if not path:
            return

        export_path = Path(path)
        image_path = export_path.with_suffix("").with_name(
            export_path.stem + "_canvas.png"
        )

        image_saved = self._save_canvas_screenshot(image_path)

        if format_name == "pdf":
            html_doc = self._render_lesson_html(
                snapshot, code, output_text, image_path if image_saved else None
            )
            printer = QPrinter(QPrinter.PrinterMode.HighResolution)
            printer.setOutputFormat(QPrinter.OutputFormat.PdfFormat)
            printer.setOutputFileName(str(export_path))
            doc = QTextDocument()
            doc.setHtml(html_doc)
            doc.print_(printer)
            self.statusbar.showMessage("Lesson PDF exported")
        else:
            markdown = self._render_lesson_markdown(
                snapshot, code, output_text, image_path if image_saved else None
            )
            try:
                export_path.write_text(markdown, encoding="utf-8")
                self.statusbar.showMessage("Lesson Markdown exported")
            except OSError:
                self.statusbar.showMessage("Failed to export Markdown")

    def _render_lesson_markdown(
        self, snapshot: dict, code: str, output_text: str, image_path: Path | None
    ) -> str:
        """Render lesson session as Markdown."""
        language = snapshot.get("language", "")
        md_lines = ["# Lesson Session Report", ""]
        if snapshot.get("lesson_title"):
            md_lines.append(f"## Lesson: {snapshot.get('lesson_title')}")
            md_lines.append(snapshot.get("lesson_description", ""))
            md_lines.append("")
            md_lines.append(f"- Status: {snapshot.get('status', 'unknown')}")
            md_lines.append(
                "- Checkpoint: "
                f"{snapshot.get('checkpoint_index', 0)}"
                f"/{snapshot.get('checkpoint_total', 0)}"
            )
            if snapshot.get("checkpoint_title"):
                md_lines.append(
                    f"- Current checkpoint: {snapshot.get('checkpoint_title')}"
                )
            md_lines.append("")

        md_lines.append("## Code")
        fence_lang = language.lower() if isinstance(language, str) else ""
        md_lines.append(f"```{fence_lang}")
        md_lines.append(code.rstrip())
        md_lines.append("```")
        md_lines.append("")

        md_lines.append("## Output")
        md_lines.append("```")
        md_lines.append(output_text.rstrip())
        md_lines.append("```")

        if image_path:
            md_lines.append("")
            md_lines.append("## Canvas")
            md_lines.append(f"![Canvas]({image_path.name})")

        md_lines.append("")
        return "\n".join(md_lines)

    def _render_lesson_html(
        self, snapshot: dict, code: str, output_text: str, image_path: Path | None
    ) -> str:
        """Render lesson session as HTML for PDF export."""
        title = html.escape(snapshot.get("lesson_title", "Lesson Session"))
        description = html.escape(snapshot.get("lesson_description", ""))
        status = html.escape(snapshot.get("status", "unknown"))
        checkpoint = (
            f"{snapshot.get('checkpoint_index', 0)}"
            f"/{snapshot.get('checkpoint_total', 0)}"
        )
        checkpoint_title = html.escape(snapshot.get("checkpoint_title", ""))

        code_block = html.escape(code)
        output_block = html.escape(output_text)

        image_html = ""
        if image_path:
            image_html = f'<h2>Canvas</h2><img src="{image_path}" width="640" />'

        return (
            "<html><body>"
            f"<h1>Lesson Session Report</h1>"
            f"<h2>Lesson: {title}</h2>"
            f"<p>{description}</p>"
            f"<p><strong>Status:</strong> {status}</p>"
            f"<p><strong>Checkpoint:</strong> {checkpoint}</p>"
            f"<p><strong>Current checkpoint:</strong> {checkpoint_title}</p>"
            "<h2>Code</h2>"
            f"<pre>{code_block}</pre>"
            "<h2>Output</h2>"
            f"<pre>{output_block}</pre>"
            f"{image_html}"
            "</body></html>"
        )

    def _save_canvas_screenshot(self, path: Path) -> bool:
        """Save current canvas screenshot to disk."""
        if not hasattr(self, "canvas"):
            return False
        pixmap = self.canvas.grab()
        return pixmap.save(str(path), "PNG")
