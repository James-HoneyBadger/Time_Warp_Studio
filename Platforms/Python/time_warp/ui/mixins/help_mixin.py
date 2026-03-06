"""Help / documentation mixin — help dialogs, markdown conversion, about.

Extracted from ``MainWindow`` to reduce file size.
"""

from __future__ import annotations

import re
from pathlib import Path
from typing import TYPE_CHECKING

from PySide6.QtWidgets import (
    QDialog,
    QDialogButtonBox,
    QMessageBox,
    QTextBrowser,
    QVBoxLayout,
)

if TYPE_CHECKING:
    pass


class HelpDocsMixin:
    """Help / documentation methods mixed into MainWindow."""

    def _get_docs_path(self) -> Path:
        """Get path to the docs directory."""
        # Go up from ui -> time_warp -> Python -> Platforms -> Time_Warp_Studio
        return Path(__file__).parent.parent.parent.parent.parent.parent / "docs"

    def _show_help_dialog(self, title: str, filepath: Path):
        """Show a help dialog with markdown content."""
        dialog = QDialog(self)
        dialog.setWindowTitle(title)
        dialog.setMinimumSize(700, 500)
        dialog.resize(800, 600)

        layout = QVBoxLayout(dialog)

        browser = QTextBrowser()
        browser.setOpenExternalLinks(True)
        browser.setStyleSheet("""
            QTextBrowser {
                font-family: 'Segoe UI', 'DejaVu Sans', sans-serif;
                font-size: 12pt;
                padding: 10px;
                background-color: palette(base);
            }
        """)

        if filepath.exists():
            content = filepath.read_text(encoding="utf-8")
            html_text = self._markdown_to_html(content)
            browser.setHtml(html_text)
        else:
            browser.setPlainText(f"Documentation not found: {filepath}")

        layout.addWidget(browser)

        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Close)
        buttons.rejected.connect(dialog.close)
        layout.addWidget(buttons)

        dialog.exec()

    def _markdown_to_html(self, md: str) -> str:
        """Convert simple markdown to HTML."""
        lines = md.split("\n")
        html_lines = []
        in_code_block = False
        in_list = False

        for line in lines:
            # Code blocks
            if line.startswith("```"):
                if in_code_block:
                    html_lines.append("</pre>")
                    in_code_block = False
                else:
                    html_lines.append(
                        "<pre style='background:#2d2d2d; "
                        "color:#f8f8f2; padding:10px; "
                        "border-radius:4px; overflow-x:auto;'>"
                    )
                    in_code_block = True
                continue

            if in_code_block:
                line = (
                    line.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
                )
                html_lines.append(line)
                continue

            # Headers
            if line.startswith("# "):
                html_lines.append(f"<h1>{line[2:]}</h1>")
            elif line.startswith("## "):
                html_lines.append(f"<h2>{line[3:]}</h2>")
            elif line.startswith("### "):
                html_lines.append(f"<h3>{line[4:]}</h3>")
            elif line.startswith("#### "):
                html_lines.append(f"<h4>{line[5:]}</h4>")
            # Horizontal rule
            elif line.strip() == "---":
                html_lines.append("<hr>")
            # List items
            elif line.strip().startswith("- ") or line.strip().startswith("* "):
                if not in_list:
                    html_lines.append("<ul>")
                    in_list = True
                item = line.strip()[2:]
                html_lines.append(f"<li>{item}</li>")
            else:
                if in_list and not line.strip().startswith(("- ", "* ")):
                    html_lines.append("</ul>")
                    in_list = False
                # Bold and inline code
                processed = line
                processed = re.sub(r"\*\*(.+?)\*\*", r"<b>\1</b>", processed)
                code_style = (
                    "<code style='background:#e0e0e0;"
                    "padding:2px 4px;border-radius:3px;'>"
                )
                pattern = r"`(.+?)`"
                replacement = code_style + r"\1</code>"
                processed = re.sub(pattern, replacement, processed)
                if processed.strip():
                    html_lines.append(f"<p>{processed}</p>")
                else:
                    html_lines.append("<br>")

        if in_list:
            html_lines.append("</ul>")
        if in_code_block:
            html_lines.append("</pre>")

        return "\n".join(html_lines)

    def show_user_manual(self):
        """Show the user manual."""
        docs_path = self._get_docs_path() / "guides" / "01-getting-started.md"
        self._show_help_dialog("Getting Started with Time Warp Studio", docs_path)

    def show_quick_reference(self):
        """Show quick reference guide."""
        docs_path = self._get_docs_path() / "guides" / "02-ide-basics.md"
        self._show_help_dialog("IDE Basics", docs_path)

    def show_programming_guide(self):
        """Show programming guide."""
        docs_path = self._get_docs_path() / "reference" / "faq.md"
        self._show_help_dialog("FAQ - Frequently Asked Questions", docs_path)

    def show_language_help(self, language: str):
        """Show help for a specific language."""
        lang_docs = {
            "basic": ("BASIC Programming Tutorial", "tutorials/basic.md"),
            "pilot": ("PILOT Language Guide", "tutorials/pilot.md"),
            "logo": ("Logo Programming & Turtle Graphics", "tutorials/logo.md"),
        }

        if language not in lang_docs:
            msg = f"No documentation for {language.upper()}"
            QMessageBox.information(self, "Help", msg)
            return

        title, doc_path = lang_docs[language]
        docs_path = self._get_docs_path() / doc_path

        if not docs_path.exists():
            msg = f"Documentation file not found: {doc_path}"
            QMessageBox.information(self, "Help", msg)
            return

        self._show_help_dialog(title, docs_path)

    def show_doc_index(self):
        """Show documentation index."""
        docs_path = self._get_docs_path() / "INDEX.md"
        self._show_help_dialog("Documentation Index", docs_path)

    def show_about(self):
        """Show about dialog."""
        QMessageBox.about(
            self,
            "About Time Warp Studio",
            "<h2>Time Warp Studio</h2>"
            "<p>Version 8.0.0 — Advanced Features Release</p>"
            "<p>Educational programming environment supporting:</p>"
            "<ul>"
            "<li>24 programming languages</li>"
            "<li>Integrated turtle graphics and canvas</li>"
            "<li>Full debugger with timeline replay</li>"
            "<li>SQL workbench, CICS terminal emulation</li>"
            "</ul>"
            "<p><b>Author:</b> James Temple</p>"
            '<p><a href="https://github.com/James-HoneyBadger/Time_Warp_Studio">'
            "github.com/James-HoneyBadger/Time_Warp_Studio</a><br>"
            '<a href="https://github.com/James-HoneyBadger/Time_Warp_Studio/'
            'tree/main/docs">'
            "Documentation</a></p>",
        )
