"""Find in Files — workspace-wide search and replace dialog."""

from __future__ import annotations

import re
from pathlib import Path
from typing import TYPE_CHECKING

from PySide6.QtCore import Qt, QThread, Signal
from PySide6.QtGui import QColor, QFont, QTextCharFormat, QTextCursor
from PySide6.QtWidgets import (
    QApplication,
    QCheckBox,
    QDialog,
    QDialogButtonBox,
    QGroupBox,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QLineEdit,
    QMessageBox,
    QProgressBar,
    QPushButton,
    QSpinBox,
    QTextEdit,
    QTreeWidget,
    QTreeWidgetItem,
    QVBoxLayout,
    QWidget,
)

if TYPE_CHECKING:
    pass

# ---------------------------------------------------------------------------
# Background search worker
# ---------------------------------------------------------------------------


class _SearchWorker(QThread):
    """Runs the file-system and in-memory search off the UI thread."""

    result_ready = Signal(
        str, int, str, int
    )  # filepath, line_no (1-based), line_text, col (0-based)
    finished_signal = Signal(int)  # total match count

    def __init__(
        self,
        pattern: str,
        sources: list[tuple[str, str]],  # [(display_name, content), ...]
        case_sensitive: bool,
        whole_word: bool,
        use_regex: bool,
    ):
        super().__init__()
        self._pattern = pattern
        self._sources = sources
        self._case_sensitive = case_sensitive
        self._whole_word = whole_word
        self._use_regex = use_regex
        self._cancelled = False

    def cancel(self):
        self._cancelled = True

    def run(self):
        flags = 0 if self._case_sensitive else re.IGNORECASE

        if self._use_regex:
            pat = self._pattern
        else:
            pat = re.escape(self._pattern)

        if self._whole_word:
            pat = r"\b" + pat + r"\b"

        try:
            compiled = re.compile(pat, flags)
        except re.error:
            self.finished_signal.emit(0)
            return

        total = 0
        for display_name, content in self._sources:
            if self._cancelled:
                break
            for lineno, line in enumerate(content.splitlines(), start=1):
                m = compiled.search(line)
                if m:
                    self.result_ready.emit(
                        display_name, lineno, line.rstrip("\n"), m.start()
                    )
                    total += 1

        self.finished_signal.emit(total)


# ---------------------------------------------------------------------------
# Dialog
# ---------------------------------------------------------------------------


class FindInFilesDialog(QDialog):
    """Workspace-wide search (and optional replace) dialog.

    Usage::

        dlg = FindInFilesDialog(main_window)
        dlg.show()

    The dialog stays open (non-modal) so users can navigate results while
    keeping it visible.
    """

    def __init__(self, main_window, parent: QWidget | None = None):
        super().__init__(parent or main_window)
        self._mw = main_window
        self._worker: _SearchWorker | None = None
        # Maps display_name → (tab_index or None, filepath or None)
        self._source_meta: dict[str, tuple[int | None, str | None]] = {}

        self.setWindowTitle("Find in Files")
        self.setMinimumSize(700, 500)
        self.setSizeGripEnabled(True)
        self.setWindowFlag(Qt.WindowType.WindowMaximizeButtonHint, True)

        self._build_ui()

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self):
        root = QVBoxLayout(self)
        root.setSpacing(6)

        # ── Search bar ────────────────────────────────────────────────
        search_group = QGroupBox("Search")
        sg_layout = QVBoxLayout(search_group)

        row1 = QHBoxLayout()
        self._search_edit = QLineEdit()
        self._search_edit.setPlaceholderText("Search pattern…")
        self._search_edit.returnPressed.connect(self._run_search)
        row1.addWidget(QLabel("Find:"))
        row1.addWidget(self._search_edit)
        sg_layout.addLayout(row1)

        row2 = QHBoxLayout()
        self._replace_edit = QLineEdit()
        self._replace_edit.setPlaceholderText("Replacement (optional)…")
        row2.addWidget(QLabel("Replace:"))
        row2.addWidget(self._replace_edit)
        sg_layout.addLayout(row2)

        opts = QHBoxLayout()
        self._case_cb = QCheckBox("Match Case")
        self._word_cb = QCheckBox("Whole Word")
        self._regex_cb = QCheckBox("Regex")
        self._open_only_cb = QCheckBox("Open Tabs Only")
        self._open_only_cb.setChecked(True)
        self._ctx_spin = QSpinBox()
        self._ctx_spin.setRange(0, 5)
        self._ctx_spin.setValue(0)
        self._ctx_spin.setPrefix("±")
        self._ctx_spin.setSuffix(" ctx lines")
        self._ctx_spin.setFixedWidth(110)
        self._ctx_spin.setToolTip("Show this many context lines around each match")
        opts.addWidget(self._case_cb)
        opts.addWidget(self._word_cb)
        opts.addWidget(self._regex_cb)
        opts.addWidget(self._open_only_cb)
        opts.addWidget(self._ctx_spin)
        opts.addStretch()
        sg_layout.addLayout(opts)

        # Path filter (visible only when "Open Tabs Only" is unchecked)
        path_row = QHBoxLayout()
        self._path_filter_edit = QLineEdit()
        self._path_filter_edit.setPlaceholderText("Path glob filter, e.g. *.bas")
        path_row.addWidget(QLabel("Files:"))
        path_row.addWidget(self._path_filter_edit)
        self._path_row_widget = QWidget()
        self._path_row_widget.setLayout(path_row)
        self._path_row_widget.hide()
        sg_layout.addWidget(self._path_row_widget)

        self._open_only_cb.toggled.connect(
            lambda checked: self._path_row_widget.setVisible(not checked)
        )

        root.addWidget(search_group)

        # ── Action buttons ────────────────────────────────────────────
        btn_row = QHBoxLayout()
        self._find_btn = QPushButton("🔍 Find All")
        self._find_btn.setDefault(True)
        self._find_btn.clicked.connect(self._run_search)

        self._replace_btn = QPushButton("Replace All in Results")
        self._replace_btn.clicked.connect(self._replace_all)
        self._replace_btn.setEnabled(False)

        self._copy_btn = QPushButton("📋 Copy Results")
        self._copy_btn.setToolTip("Copy all search results to the clipboard")
        self._copy_btn.clicked.connect(self._copy_results)
        self._copy_btn.setEnabled(False)

        self._cancel_btn = QPushButton("Stop")
        self._cancel_btn.setEnabled(False)
        self._cancel_btn.clicked.connect(self._cancel_search)

        btn_row.addWidget(self._find_btn)
        btn_row.addWidget(self._replace_btn)
        btn_row.addWidget(self._copy_btn)
        btn_row.addStretch()
        btn_row.addWidget(self._cancel_btn)
        root.addLayout(btn_row)

        # ── Progress bar ──────────────────────────────────────────────
        self._progress = QProgressBar()
        self._progress.setRange(0, 0)  # indeterminate
        self._progress.setTextVisible(False)
        self._progress.hide()
        root.addWidget(self._progress)

        # ── Results tree ──────────────────────────────────────────────
        self._results = QTreeWidget()
        self._results.setHeaderLabels(["Location", "Match"])
        self._results.header().setSectionResizeMode(
            0, QHeaderView.ResizeMode.ResizeToContents
        )
        self._results.header().setSectionResizeMode(1, QHeaderView.ResizeMode.Stretch)
        self._results.setAlternatingRowColors(True)
        self._results.setUniformRowHeights(True)
        self._results.itemActivated.connect(self._on_result_activated)
        font = QFont("Courier New", 9)
        self._results.setFont(font)
        root.addWidget(self._results)

        # ── Status bar ────────────────────────────────────────────────
        self._status_label = QLabel("Enter a search term and click Find All.")
        self._status_label.setAlignment(Qt.AlignmentFlag.AlignLeft)
        root.addWidget(self._status_label)

        # Close button
        close_btn_box = QDialogButtonBox(QDialogButtonBox.StandardButton.Close)
        close_btn_box.rejected.connect(self.close)
        root.addWidget(close_btn_box)

    # ------------------------------------------------------------------
    # Search logic
    # ------------------------------------------------------------------

    def _collect_sources(self) -> list[tuple[str, str]]:
        """Build list of (display_name, content) to search."""
        self._source_meta.clear()
        sources: list[tuple[str, str]] = []
        mw = self._mw

        # Always include all open tabs
        for i in range(mw.editor_tabs.count()):
            editor = mw.editor_tabs.widget(i)
            if editor is None:
                continue
            content = editor.toPlainText()
            tab_title = mw.editor_tabs.tabText(i).rstrip("●").strip()
            info = mw._tab_states.get(i)
            filepath = info.file if info else None
            display = filepath or f"[Tab] {tab_title}"
            sources.append((display, content))
            self._source_meta[display] = (i, filepath)

        if self._open_only_cb.isChecked():
            return sources

        # Also search project / workspace files on disk
        project_dir: str | None = None
        if hasattr(mw, "_current_project_path") and mw._current_project_path:
            project_dir = str(Path(mw._current_project_path).parent)
        elif hasattr(mw, "settings"):
            project_dir = mw.settings.value("last_dir")

        if not project_dir:
            return sources

        glob_pat = self._path_filter_edit.text().strip() or "**/*.*"
        already = {meta[1] for meta in self._source_meta.values() if meta[1]}

        try:
            base = Path(project_dir)
            for fpath in base.glob(glob_pat):
                if not fpath.is_file():
                    continue
                abs_str = str(fpath)
                if abs_str in already:
                    continue  # already included as open tab
                # Only read text-ish files (< 1 MB)
                if fpath.stat().st_size > 1_000_000:
                    continue
                try:
                    content = fpath.read_text(encoding="utf-8", errors="replace")
                    rel = str(fpath.relative_to(base))
                    sources.append((rel, content))
                    self._source_meta[rel] = (None, abs_str)
                except OSError:
                    pass
        except Exception:  # pylint: disable=broad-exception-caught
            pass

        return sources

    def _run_search(self):
        pattern = self._search_edit.text()
        if not pattern:
            self._status_label.setText("Enter a search pattern first.")
            return

        if self._worker and self._worker.isRunning():
            self._worker.cancel()
            self._worker.wait()

        sources = self._collect_sources()
        self._results.clear()
        self._replace_btn.setEnabled(False)

        self._worker = _SearchWorker(
            pattern=pattern,
            sources=sources,
            case_sensitive=self._case_cb.isChecked(),
            whole_word=self._word_cb.isChecked(),
            use_regex=self._regex_cb.isChecked(),
        )
        self._worker.result_ready.connect(self._on_match)
        self._worker.finished_signal.connect(self._on_search_done)

        self._find_btn.setEnabled(False)
        self._cancel_btn.setEnabled(True)
        self._progress.show()
        self._status_label.setText("Searching…")
        self._worker.start()

    def _cancel_search(self):
        if self._worker:
            self._worker.cancel()

    # Groups results by file — each file is a top-level item
    _file_items: dict[str, QTreeWidgetItem]

    def _on_match(self, display_name: str, lineno: int, line_text: str, col: int):
        if not hasattr(self, "_file_items"):
            self._file_items = {}

        if display_name not in self._file_items:
            file_item = QTreeWidgetItem(self._results)
            file_item.setText(0, display_name)
            file_item.setExpanded(True)
            bold = QFont()
            bold.setBold(True)
            file_item.setFont(0, bold)
            file_item.setData(0, Qt.ItemDataRole.UserRole, display_name)
            self._file_items[display_name] = file_item

        ctx = self._ctx_spin.value()
        parent = self._file_items[display_name]

        # Insert context lines before match if requested
        if ctx > 0:
            meta = self._source_meta.get(display_name)
            src_lines: list[str] = []
            if meta is not None:
                tab_idx, filepath = meta
                if tab_idx is not None:
                    mw = self._mw
                    editor = mw.editor_tabs.widget(tab_idx)
                    if editor:
                        src_lines = editor.toPlainText().splitlines()
                elif filepath and Path(filepath).is_file():
                    try:
                        src_lines = (
                            Path(filepath)
                            .read_text(encoding="utf-8", errors="replace")
                            .splitlines()
                        )
                    except OSError:
                        pass
            first_ctx = max(0, lineno - 1 - ctx)
            last_ctx = min(len(src_lines), lineno + ctx)  # exclusive
            for ctx_i, ctx_line in enumerate(
                src_lines[first_ctx:last_ctx], start=first_ctx + 1
            ):
                is_match_line = ctx_i == lineno
                ci = QTreeWidgetItem(parent)
                ci.setText(0, f":{ctx_i}")
                ci.setText(1, ctx_line.rstrip("\n"))
                if is_match_line:
                    ci.setData(0, Qt.ItemDataRole.UserRole, display_name)
                    ci.setData(0, Qt.ItemDataRole.UserRole + 1, ctx_i)
                    ci.setData(0, Qt.ItemDataRole.UserRole + 2, col)
                    bold_f = QFont()
                    bold_f.setBold(True)
                    ci.setFont(1, bold_f)
                else:
                    # context-only row — make it slightly dimmer
                    ci.setForeground(1, QColor("#888888"))
        else:
            child = QTreeWidgetItem(parent)
            child.setText(0, f":{lineno}")
            child.setText(1, line_text.strip())
            child.setData(0, Qt.ItemDataRole.UserRole, display_name)
            child.setData(0, Qt.ItemDataRole.UserRole + 1, lineno)
            child.setData(0, Qt.ItemDataRole.UserRole + 2, col)

        # Update file-level match count
        count = parent.data(0, Qt.ItemDataRole.UserRole + 1) or 0
        parent.setData(0, Qt.ItemDataRole.UserRole + 1, count + 1)
        parent.setText(
            0, f"{display_name}  ({count + 1} match{'es' if count + 1 != 1 else ''})"
        )

    def _on_search_done(self, total: int):
        self._find_btn.setEnabled(True)
        self._cancel_btn.setEnabled(False)
        self._progress.hide()
        self._file_items = {}

        noun = "match" if total == 1 else "matches"
        self._status_label.setText(f"{total} {noun} found.")

        self._copy_btn.setEnabled(total > 0)
        if total > 0 and self._replace_edit.text():
            self._replace_btn.setEnabled(True)

    # ------------------------------------------------------------------
    # Navigation
    # ------------------------------------------------------------------

    def _on_result_activated(self, item: QTreeWidgetItem, _col: int):
        display_name = item.data(0, Qt.ItemDataRole.UserRole)
        lineno = item.data(0, Qt.ItemDataRole.UserRole + 1)
        match_col = item.data(
            0, Qt.ItemDataRole.UserRole + 2
        )  # may be None for file nodes

        if display_name is None:
            return

        meta = self._source_meta.get(display_name)
        if meta is None:
            return

        tab_idx, filepath = meta

        mw = self._mw
        if tab_idx is not None:
            # Switch to the already-open tab
            mw.editor_tabs.setCurrentIndex(tab_idx)
            editor = mw.editor_tabs.widget(tab_idx)
        elif filepath and Path(filepath).is_file():
            # Open the file in a new tab
            mw.load_file(filepath)
            editor = mw.get_current_editor()
        else:
            return

        if editor is None or not isinstance(lineno, int):
            return

        # Build search regex from current pattern to locate exact match extent
        pattern = self._search_edit.text()
        flags = 0 if self._case_cb.isChecked() else re.IGNORECASE
        pat = pattern if self._regex_cb.isChecked() else re.escape(pattern)
        if self._word_cb.isChecked():
            pat = r"\b" + pat + r"\b"
        try:
            compiled = re.compile(pat, flags)
        except re.error:
            compiled = None

        # Navigate to the correct line
        block = editor.document().findBlockByLineNumber(lineno - 1)
        cursor = QTextCursor(block)

        # Try to select the exact match text on this line
        if compiled is not None and match_col is not None:
            line_text = block.text()
            m = compiled.search(line_text, match_col)
            if m:
                cursor.setPosition(block.position() + m.start())
                cursor.setPosition(
                    block.position() + m.end(), QTextCursor.MoveMode.KeepAnchor
                )
            else:
                cursor.movePosition(QTextCursor.MoveOperation.EndOfBlock)
        else:
            cursor.movePosition(QTextCursor.MoveOperation.EndOfBlock)

        editor.setTextCursor(cursor)
        editor.centerCursor()
        editor.setFocus()

        # Highlight all matches in the opened file using extra selections
        self._highlight_matches_in_editor(editor, compiled)

    def _highlight_matches_in_editor(self, editor, compiled) -> None:
        """Add extra-selection highlights for all matches in *editor*."""
        if compiled is None:
            return
        highlight_fmt = QTextCharFormat()
        highlight_fmt.setBackground(QColor("#FFD700"))  # gold highlight
        highlight_fmt.setForeground(QColor("#000000"))

        selections: list[QTextEdit.ExtraSelection] = []
        text = editor.toPlainText()
        for m in compiled.finditer(text):
            sel = QTextEdit.ExtraSelection()
            sel.cursor = QTextCursor(editor.document())
            sel.cursor.setPosition(m.start())
            sel.cursor.setPosition(m.end(), QTextCursor.MoveMode.KeepAnchor)
            sel.format = highlight_fmt
            selections.append(sel)
            if len(selections) >= 2000:  # safety cap
                break
        editor.setExtraSelections(selections)

    def _copy_results(self) -> None:
        """Copy all search results to the clipboard as plain text."""
        lines: list[str] = []
        root = self._results.invisibleRootItem()
        for fi in range(root.childCount()):
            file_item = root.child(fi)
            lines.append(file_item.text(0))
            for ci in range(file_item.childCount()):
                child = file_item.child(ci)
                lines.append(f"  {child.text(0)}  {child.text(1)}")
        if lines:
            QApplication.clipboard().setText("\n".join(lines))
            self._status_label.setText(f"Copied {len(lines)} lines to clipboard.")

    # ------------------------------------------------------------------
    # Replace
    # ------------------------------------------------------------------

    def _replace_all(self):
        replacement = self._replace_edit.text()
        pattern = self._search_edit.text()
        if not pattern:
            return

        flags = 0 if self._case_cb.isChecked() else re.IGNORECASE
        if not self._regex_cb.isChecked():
            pat = re.escape(pattern)
        else:
            pat = pattern
        if self._word_cb.isChecked():
            pat = r"\b" + pat + r"\b"

        try:
            compiled = re.compile(pat, flags)
        except re.error as exc:
            QMessageBox.critical(self, "Invalid Pattern", str(exc))
            return

        replaced_total = 0
        mw = self._mw

        # Replace only in open tabs that are in the results
        for i in range(mw.editor_tabs.count()):
            editor = mw.editor_tabs.widget(i)
            if editor is None:
                continue

            info = mw._tab_states.get(i)
            filepath = info.file if info else None
            tab_title = mw.editor_tabs.tabText(i).rstrip("●").strip()
            display = filepath or f"[Tab] {tab_title}"

            if display not in self._source_meta:
                continue

            original = editor.toPlainText()
            new_text, n = compiled.subn(replacement, original)
            if n:
                editor.setPlainText(new_text)
                mw.set_current_tab_info(
                    file=filepath,
                    modified=True,
                    language=info.language if info else None,
                )
                replaced_total += n

        self._status_label.setText(
            f"Replaced {replaced_total} occurrence{'s' if replaced_total != 1 else ''}."
        )

    # ------------------------------------------------------------------

    def show_with_selection(self):
        """Open the dialog and pre-fill the search box from the current selection."""
        mw = self._mw
        editor = mw.get_current_editor() if hasattr(mw, "get_current_editor") else None
        if editor:
            sel = editor.textCursor().selectedText().strip()
            if sel:
                self._search_edit.setText(sel)
        self.show()
        self._search_edit.selectAll()
        self._search_edit.setFocus()
