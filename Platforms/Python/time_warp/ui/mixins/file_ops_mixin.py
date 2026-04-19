"""File operations mixin — new, open, save, recent files.

Extracted from ``MainWindow`` to reduce file size.
"""

from __future__ import annotations

from functools import partial
from pathlib import Path
from typing import TYPE_CHECKING, Any

from PySide6.QtGui import QAction
from PySide6.QtWidgets import QFileDialog, QMessageBox

if TYPE_CHECKING:
    from PySide6.QtCore import QSettings
    from PySide6.QtWidgets import QStatusBar, QTabWidget, QWidget

    from ..editor import CodeEditor  # noqa: F401

    _FileOpsMixinBase = QWidget
else:
    _FileOpsMixinBase = object


class FileOperationsMixin(_FileOpsMixinBase):
    """File I/O methods mixed into MainWindow.

    Uses a conditional base (``QWidget`` for type checkers, ``object``
    at runtime) so that static analysers accept ``self`` where Qt
    dialogs expect a ``QWidget``, without interfering with shiboken's
    C++ object initialisation.

    Attributes below are provided by the host ``MainWindow`` class at
    runtime; they are declared here only so that static type-checkers
    can verify usage within the mixin.
    """

    editor_tabs: QTabWidget
    statusbar: QStatusBar
    settings: QSettings

    # These methods are defined on MainWindow; stubs keep the checker happy.
    def create_new_tab(self, *args: Any, **kwargs: Any) -> None: ...  # noqa: E704
    def set_current_tab_info(self, **kwargs: Any) -> None: ...  # noqa: E704
    def _ts(self, index: int) -> Any: ...  # noqa: E704

    def new_file(self):
        """Create new file."""
        self.create_new_tab()
        if hasattr(self, "statusbar"):
            self.statusbar.showMessage("New file created")

    def open_file(self):
        """Open file dialog."""
        last_dir = self.settings.value("last_dir", str(Path.home()))

        filename, _ = QFileDialog.getOpenFileName(
            self,
            "Open File",
            last_dir,
            "Time Warp Files (*.bas *.pilot *.logo *.c *.pas *.pro *.f *.py *.lua *.scm *.rkt *.cob *.cbl *.bf *.asm *.s *.js *.f77 *.for *.rex *.rexx *.st *.htalk *.hs *.apl *.sql *.jcl *.cics);;"
            "BASIC Files (*.bas);;"
            "PILOT Files (*.pilot);;"
            "Logo Files (*.logo);;"
            "Python Files (*.py);;"
            "Lua Files (*.lua);;"
            "Scheme Files (*.scm *.rkt);;"
            "COBOL Files (*.cob *.cbl);;"
            "Brainfuck Files (*.bf);;"
            "Assembly Files (*.asm *.s);;"
            "JavaScript Files (*.js);;"
            "FORTRAN Files (*.f77 *.for);;"
            "REXX Files (*.rex *.rexx);;"
            "Smalltalk Files (*.st);;"
            "HyperTalk Files (*.htalk);;"
            "Haskell Files (*.hs);;"
            "APL Files (*.apl);;"
            "All Files (*.*)",
            options=QFileDialog.Option.DontUseNativeDialog,
        )

        if filename:
            self.settings.setValue("last_dir", str(Path(filename).parent))
            self.load_file(filename)

    def open_project_folder(self):
        """Open a project folder in the explorer."""
        default_dir = self.settings.value(
            "project_explorer/root_path",
            self.settings.value("last_dir", str(Path.home())),
        )

        directory = QFileDialog.getExistingDirectory(
            self,
            "Open Project Folder",
            str(default_dir),
            options=QFileDialog.Option.DontUseNativeDialog,
        )
        if directory:
            self.open_project_root(directory)

    def open_project_root(self, root_path) -> bool:
        """Open a specific folder as the current project root."""
        path = Path(root_path).expanduser()
        if path.is_file():
            path = path.parent

        if not path.exists() or not path.is_dir():
            QMessageBox.warning(
                self,
                "Project Folder Not Found",
                f"Could not open project folder:\n{path}",
            )
            return False

        normalized = str(path.resolve())
        self.settings.setValue("last_dir", normalized)

        if hasattr(self, "_remember_project_root"):
            self._remember_project_root(normalized)

        panel = None
        if hasattr(self, "feature_manager"):
            panel = self.feature_manager.get_feature_panel("project_explorer")
            self.feature_manager.toggle_feature_panel("project_explorer", visible=True)

        if panel and hasattr(panel, "set_root_path"):
            panel.set_root_path(normalized)

        if hasattr(self, "statusbar"):
            self.statusbar.showMessage(f"Opened project: {normalized}")
        return True

    def load_file(self, filename):
        """Load file into current tab."""
        from ...core.interpreter import Language

        try:
            with open(filename, "r", encoding="utf-8") as f:
                content = f.read()

            ext = Path(filename).suffix
            language = Language.from_extension(ext)

            current_editor = self.get_current_editor()
            if (
                current_editor
                and hasattr(current_editor, "set_language")
                and not self.get_current_tab_info()["modified"]
                and not self.get_current_tab_info()["file"]
            ):
                current_editor.setPlainText(content)
                current_editor.set_language(language)
                tab_title = Path(filename).name
                current_index = self.editor_tabs.currentIndex()
                self.editor_tabs.setTabText(current_index, tab_title)
                self.set_current_tab_info(
                    file=filename, modified=False, language=language
                )
            else:
                tab_title = Path(filename).name
                self.create_new_tab(tab_title, content, language)
                self.set_current_tab_info(
                    file=filename, modified=False, language=language
                )

            for i in range(self.language_combo.count()):
                if self.language_combo.itemData(i) == language:
                    self.language_combo.setCurrentIndex(i)
                    break

            self.output.set_language(language)
            self.add_recent_file(filename)
            if hasattr(self, "_remember_project_root"):
                self._remember_project_root(str(Path(filename).parent))
            self.statusbar.showMessage(f"Loaded: {filename}")

        except (OSError, UnicodeDecodeError) as e:
            QMessageBox.critical(
                self, "Error Loading File", f"Could not load file:\n{e}"
            )

    def save_file(self):
        """Save the current tab to disk (calls Save As if not yet named)."""
        current_index = self.editor_tabs.currentIndex()
        if current_index < 0:
            return False

        editor = self.get_current_editor()
        if editor is None:
            return False

        filename = self._ts(current_index).file
        if not filename:
            return self.save_tab_as(current_index)

        try:
            # Save the existing content as a version before overwriting
            try:
                existing_content = editor.toPlainText()
                self._autosave_manager.autosave_file(Path(filename), existing_content)
            except Exception:  # pylint: disable=broad-except
                pass  # Autosave failure must never block a real save

            with open(filename, "w", encoding="utf-8") as f:
                f.write(editor.toPlainText())
            self.set_current_tab_info(file=filename, modified=False)
            self.statusbar.showMessage(f"Saved: {filename}")
            return True
        except OSError as e:
            QMessageBox.critical(
                self, "Error Saving File", f"Could not save file:\n{e}"
            )
            return False

    def save_tab(self, tab_index: int) -> bool:
        """Save a specific tab to disk (use Save As flow if not yet named).

        Returns True on success, False on error or cancellation.
        """
        if tab_index < 0 or tab_index >= self.editor_tabs.count():
            return False

        editor: Any = self.editor_tabs.widget(tab_index)
        if editor is None:
            return False

        filename = self._ts(tab_index).file
        if not filename:
            return self.save_tab_as(tab_index)

        try:
            with open(filename, "w", encoding="utf-8") as f:
                f.write(editor.toPlainText())
            self.set_current_tab_info(file=filename, modified=False)
            self.statusbar.showMessage(f"Saved: {filename}")
            self.add_recent_file(filename)
            return True
        except OSError as e:
            QMessageBox.critical(
                self, "Error Saving File", f"Could not save file:\n{e}"
            )
            return False

    def save_tab_as(self, tab_index: int) -> bool:
        """Prompt user for filename and save a specific tab to disk.

        Returns True on success, False on cancel/error.
        """
        if tab_index < 0 or tab_index >= self.editor_tabs.count():
            return False

        editor: Any = self.editor_tabs.widget(tab_index)
        if editor is None:
            return False

        last_dir = str(self.settings.value("last_dir", str(Path.home())))

        filename, _ = QFileDialog.getSaveFileName(
            self,
            "Save File As",
            last_dir,
            "Time Warp Files (*.bas *.pilot *.logo *.c *.pas *.pro *.f *.py *.lua *.scm *.cob *.bf *.asm *.js *.f77 *.rex *.st *.htalk *.hs *.apl *.sql *.jcl *.cics);;SQL Files (*.sql);;JCL Files (*.jcl);;CICS Files (*.cics);;All Files (*.*)",
            options=QFileDialog.Option.DontUseNativeDialog,
        )

        if not filename:
            return False

        self.settings.setValue("last_dir", str(Path(filename).parent))

        try:
            with open(filename, "w", encoding="utf-8") as f:
                f.write(editor.toPlainText())

            self._ts(tab_index).file = filename
            self._ts(tab_index).modified = False
            self.editor_tabs.setTabText(tab_index, Path(filename).name)
            self.set_current_tab_info(file=filename, modified=False)
            self.add_recent_file(filename)
            self.statusbar.showMessage(f"Saved: {filename}")
            return True
        except OSError as e:
            QMessageBox.critical(
                self, "Error Saving File", f"Could not save file:\n{e}"
            )
            return False

    def save_file_as(self) -> bool:
        """Save the currently active tab using Save As dialog."""
        current_index = self.editor_tabs.currentIndex()
        if current_index < 0:
            return False
        return self.save_tab_as(current_index)

    def add_recent_file(self, filename):
        """Add file to recent files list."""
        recent = self.settings.value("recent_files", [])
        if not isinstance(recent, list):
            recent = []

        if filename in recent:
            recent.remove(filename)
        recent.insert(0, filename)
        recent = recent[:10]

        self.settings.setValue("recent_files", recent)
        self.update_recent_files_menu()

    def update_recent_files_menu(self):
        """Update recent files menu."""
        self.recent_menu.clear()

        recent = self.settings.value("recent_files", [])
        if not isinstance(recent, list):
            recent = []

        if not recent:
            action = QAction("No recent files", self)
            action.setEnabled(False)
            self.recent_menu.addAction(action)
        else:
            for filename in recent:
                action = QAction(Path(filename).name, self)
                action.triggered.connect(partial(self.load_file, filename))
                self.recent_menu.addAction(action)

    def update_recent_projects_menu(self):
        """Update recent project roots menu."""
        if not hasattr(self, "recent_projects_menu"):
            return

        self.recent_projects_menu.clear()
        recent = self.settings.value("project_explorer/recent_paths", [])
        if isinstance(recent, str):
            recent = [recent] if recent else []
        elif not isinstance(recent, list):
            recent = list(recent) if recent else []

        recent = [path for path in recent if Path(path).exists()]

        if not recent:
            action = QAction("No recent projects", self)
            action.setEnabled(False)
            self.recent_projects_menu.addAction(action)
            return

        for root_path in recent:
            action = QAction(Path(root_path).name or root_path, self)
            action.setStatusTip(root_path)
            action.triggered.connect(partial(self.open_project_root, root_path))
            self.recent_projects_menu.addAction(action)
