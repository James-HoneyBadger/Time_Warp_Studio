"""Project file-tree panel for Time Warp Studio.

``ProjectTreePanel`` is a collapsible left-side dock panel that shows:
- Project name as a root node
- All tracked source files as child nodes (icon by language)
- Right-click context menu: Add File, Rename, Remove from Project, Set as Main

Double-clicking a file emits ``file_open_requested(abs_path: str)`` so the
main window can open it in a new tab.

Usage (from main_window.py)::

    self._project_panel = ProjectTreePanel(self)
    self._project_panel.file_open_requested.connect(self.load_file)
    splitter.insertWidget(0, self._project_panel)
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING, Optional

from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QAction, QFont, QIcon
from PySide6.QtWidgets import (
    QFileDialog,
    QHBoxLayout,
    QInputDialog,
    QLabel,
    QMenu,
    QMessageBox,
    QPushButton,
    QSizePolicy,
    QTreeWidget,
    QTreeWidgetItem,
    QVBoxLayout,
    QWidget,
)

from ..core.interpreter import Language
from ..core.project_manager import ProjectFile, ProjectManager

if TYPE_CHECKING:
    from ..core.project_manager import Project

logger = logging.getLogger(__name__)

# Language → emoji/text icon mapping (no image files needed)
_LANG_ICON: dict[str, str] = {
    "BASIC": "🔵",
    "LOGO": "🐢",
    "C": "⚙️",
    "PASCAL": "📐",
    "PROLOG": "🔬",
    "FORTH": "🔢",
    "BRAINFUCK": "🧠",
    "PILOT": "✈️",
}


class ProjectTreePanel(QWidget):
    """Left-panel widget showing the currently open project's file tree."""

    #: Emitted when the user double-clicks a file; carries the absolute path.
    file_open_requested = Signal(str)
    #: Emitted after the project is modified (file added/removed/renamed).
    project_changed = Signal()

    def __init__(self, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self._project: Optional["Project"] = None
        self._project_dir: Optional[Path] = None

        self.setMinimumWidth(160)
        self.setMaximumWidth(320)
        self.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Expanding)

        self._build_ui()

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self) -> None:
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Header bar
        header = QWidget()
        header.setFixedHeight(28)
        header.setStyleSheet(
            "background: palette(mid); border-bottom: 1px solid palette(dark);"
        )
        h_layout = QHBoxLayout(header)
        h_layout.setContentsMargins(6, 0, 4, 0)

        title_lbl = QLabel("PROJECT")
        title_lbl.setFont(QFont(title_lbl.font().family(), 8, QFont.Weight.Bold))
        h_layout.addWidget(title_lbl)
        h_layout.addStretch()

        new_file_btn = QPushButton("📝")
        new_file_btn.setFixedSize(20, 20)
        new_file_btn.setToolTip("New file in project")
        new_file_btn.clicked.connect(self._new_file)
        h_layout.addWidget(new_file_btn)

        add_btn = QPushButton("+")
        add_btn.setFixedSize(20, 20)
        add_btn.setToolTip("Add existing file to project")
        add_btn.clicked.connect(self._add_file)
        h_layout.addWidget(add_btn)

        layout.addWidget(header)

        # Tree
        self._tree = QTreeWidget()
        self._tree.setHeaderHidden(True)
        self._tree.setRootIsDecorated(True)
        self._tree.setAnimated(True)
        self._tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._tree.customContextMenuRequested.connect(self._show_context_menu)
        self._tree.itemDoubleClicked.connect(self._on_double_click)
        self._tree.setStyleSheet("""
            QTreeWidget {
                border: none;
                background: palette(base);
                font-size: 12px;
            }
            QTreeWidget::item:selected {
                background: palette(highlight);
                color: palette(highlighted-text);
            }
            QTreeWidget::item { padding: 2px 0; }
        """)
        layout.addWidget(self._tree)

        # Empty-state label
        self._empty_label = QLabel("No project open.\nUse File → Open Project…")
        self._empty_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._empty_label.setStyleSheet("color: palette(mid); padding: 12px; font-size: 11px;")
        self._empty_label.setWordWrap(True)
        layout.addWidget(self._empty_label)

        self._tree.hide()

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def load_project(self, project: "Project", project_path: str) -> None:
        """Populate the tree from *project*.  Call after opening a project."""
        self._project = project
        self._project_dir = Path(project_path).parent
        self._refresh()

    def clear_project(self) -> None:
        """Reset to empty state."""
        self._project = None
        self._project_dir = None
        self._tree.clear()
        self._tree.hide()
        self._empty_label.show()

    def highlight_file(self, abs_path: str) -> None:
        """Select the tree item matching *abs_path* (the currently active tab)."""
        if self._project_dir is None:
            return
        try:
            rel = str(Path(abs_path).relative_to(self._project_dir))
        except ValueError:
            return
        root = self._tree.invisibleRootItem()
        project_root = root.child(0) if root.childCount() > 0 else None
        if project_root is None:
            return
        for i in range(project_root.childCount()):
            item = project_root.child(i)
            if item and item.data(0, Qt.ItemDataRole.UserRole) == rel:
                self._tree.setCurrentItem(item)
                return

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _refresh(self) -> None:
        """Rebuild the tree from the current project."""
        self._tree.clear()
        if self._project is None:
            self._tree.hide()
            self._empty_label.show()
            return

        self._empty_label.hide()
        self._tree.show()

        root_item = QTreeWidgetItem([f"📁 {self._project.name}"])
        root_item.setFont(0, QFont(root_item.font(0).family(), 10, QFont.Weight.Bold))
        root_item.setFlags(root_item.flags() & ~Qt.ItemFlag.ItemIsSelectable)
        self._tree.addTopLevelItem(root_item)

        for pfile in self._project.files:
            icon = _LANG_ICON.get(pfile.language.upper(), "📄")
            label = f"{icon} {Path(pfile.path).name}"
            if pfile.path == self._project.main_file:
                label += "  ★"
            item = QTreeWidgetItem([label])
            item.setData(0, Qt.ItemDataRole.UserRole, pfile.path)
            item.setToolTip(0, pfile.path)
            root_item.addChild(item)

        root_item.setExpanded(True)

    def _abs(self, rel_path: str) -> Optional[str]:
        if self._project_dir is None:
            return None
        return str(self._project_dir / rel_path)

    def _on_double_click(self, item: QTreeWidgetItem, _col: int) -> None:
        rel = item.data(0, Qt.ItemDataRole.UserRole)
        if not rel:
            return
        abs_path = self._abs(rel)
        if abs_path and Path(abs_path).is_file():
            self.file_open_requested.emit(abs_path)
        else:
            QMessageBox.warning(
                self,
                "File Not Found",
                f"'{rel}' does not exist on disk.\n"
                "The file may have been moved or deleted.",
            )

    def _show_context_menu(self, pos: object) -> None:
        item = self._tree.itemAt(pos)  # type: ignore[arg-type]
        rel = item.data(0, Qt.ItemDataRole.UserRole) if item else None

        menu = QMenu(self)

        new_file_act = QAction("New File…", self)
        new_file_act.triggered.connect(self._new_file)
        menu.addAction(new_file_act)

        new_folder_act = QAction("New Folder…", self)
        new_folder_act.triggered.connect(self._new_folder)
        menu.addAction(new_folder_act)

        menu.addSeparator()

        add_act = QAction("Add Existing File…", self)
        add_act.triggered.connect(self._add_file)
        menu.addAction(add_act)

        if rel:
            menu.addSeparator()

            open_act = QAction("Open", self)
            open_act.triggered.connect(lambda: self._on_double_click(item, 0))
            menu.addAction(open_act)

            set_main_act = QAction("Set as Main File", self)
            set_main_act.triggered.connect(lambda: self._set_main(rel))
            set_main_act.setEnabled(self._project is not None and rel != self._project.main_file)
            menu.addAction(set_main_act)

            menu.addSeparator()

            rename_act = QAction("Rename…", self)
            rename_act.triggered.connect(lambda: self._rename_file(rel))
            menu.addAction(rename_act)

            delete_act = QAction("Delete", self)
            delete_act.triggered.connect(lambda: self._delete_file(rel))
            menu.addAction(delete_act)

            menu.addSeparator()

            remove_act = QAction("Remove from Project", self)
            remove_act.triggered.connect(lambda: self._remove_file(rel))
            menu.addAction(remove_act)

        menu.exec(self._tree.mapToGlobal(pos))  # type: ignore[arg-type]

    def _add_file(self) -> None:
        if self._project is None or self._project_dir is None:
            QMessageBox.information(self, "No Project", "Open a project first.")
            return
        paths, _ = QFileDialog.getOpenFileNames(
            self,
            "Add Files to Project",
            str(self._project_dir),
            "Source Files (*.bas *.logo *.c *.pas *.pl *.4th *.bf *.pilot *.py);;All Files (*)",
        )
        for path in paths:
            try:
                rel = str(Path(path).relative_to(self._project_dir))
            except ValueError:
                # File outside project dir — copy not supported; skip with warning
                QMessageBox.warning(
                    self,
                    "Outside Project",
                    f"'{path}' is outside the project directory and cannot be added.",
                )
                continue
            if any(pf.path == rel for pf in self._project.files):
                continue  # already tracked
            lang = Language.from_extension(Path(path).suffix).name
            self._project.files.append(ProjectFile(path=rel, language=lang))
        self._refresh()
        self.project_changed.emit()

    def _new_file(self) -> None:
        if self._project is None or self._project_dir is None:
            QMessageBox.information(self, "No Project", "Open a project first.")
            return
        name, ok = QInputDialog.getText(
            self,
            "New File",
            "File name (e.g. main.bas):",
            text="main.bas",
        )
        if not ok or not name.strip():
            return
        rel = name.strip().replace("\\", "/")
        if any(pf.path == rel for pf in self._project.files):
            QMessageBox.warning(self, "Already Tracked", f"'{rel}' is already in the project.")
            return
        try:
            ProjectManager().create_file(
                self._project,
                str(self._project_dir),
                rel,
                content="",
            )
        except FileExistsError:
            QMessageBox.warning(self, "File Exists", f"'{rel}' already exists on disk.")
            return
        except ValueError as exc:
            QMessageBox.critical(self, "Invalid Path", str(exc))
            return
        self._refresh()
        self.project_changed.emit()
        abs_path = self._abs(rel)
        if abs_path:
            self.file_open_requested.emit(abs_path)

    def _new_folder(self) -> None:
        if self._project is None or self._project_dir is None:
            QMessageBox.information(self, "No Project", "Open a project first.")
            return
        name, ok = QInputDialog.getText(
            self,
            "New Folder",
            "Folder name (e.g. src):",
        )
        if not ok or not name.strip():
            return
        rel = name.strip().replace("\\", "/")
        try:
            ProjectManager().create_folder(self._project, str(self._project_dir), rel)
        except ValueError as exc:
            QMessageBox.critical(self, "Invalid Path", str(exc))
            return
        self._refresh()

    def _rename_file(self, rel: str) -> None:
        if self._project is None or self._project_dir is None:
            return
        new_name, ok = QInputDialog.getText(
            self,
            "Rename File",
            "New name:",
            text=Path(rel).name,
        )
        if not ok or not new_name.strip():
            return
        new_rel = str(Path(rel).parent / new_name.strip()) if "/" in rel else new_name.strip()
        try:
            ProjectManager().rename_file(
                self._project, str(self._project_dir), rel, new_rel
            )
        except (FileNotFoundError, FileExistsError, ValueError) as exc:
            QMessageBox.critical(self, "Rename Failed", str(exc))
            return
        self._refresh()
        self.project_changed.emit()

    def _delete_file(self, rel: str) -> None:
        if self._project is None or self._project_dir is None:
            return
        answer = QMessageBox.warning(
            self,
            "Delete File",
            f"Permanently delete '{Path(rel).name}' from disk?\nThis cannot be undone.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.Cancel,
            QMessageBox.StandardButton.Cancel,
        )
        if answer != QMessageBox.StandardButton.Yes:
            return
        try:
            ProjectManager().delete_file(self._project, str(self._project_dir), rel)
        except (FileNotFoundError, ValueError) as exc:
            QMessageBox.critical(self, "Delete Failed", str(exc))
            return
        self._refresh()
        self.project_changed.emit()

    def _remove_file(self, rel: str) -> None:
        if self._project is None:
            return
        answer = QMessageBox.question(
            self,
            "Remove File",
            f"Remove '{Path(rel).name}' from the project?\n(The file on disk is not deleted.)",
        )
        if answer != QMessageBox.StandardButton.Yes:
            return
        self._project.files = [f for f in self._project.files if f.path != rel]
        if self._project.main_file == rel:
            self._project.main_file = self._project.files[0].path if self._project.files else ""
        self._refresh()
        self.project_changed.emit()

    def _set_main(self, rel: str) -> None:
        if self._project is None:
            return
        self._project.main_file = rel
        for pf in self._project.files:
            pf.is_main = pf.path == rel
        self._refresh()
        self.project_changed.emit()
