"""Main window for Time Warp IDE."""

import getpass
from functools import partial
from pathlib import Path

# pylint: disable=no-name-in-module
# Large UI file with many helper methods and nested classes — allow the
# line-count threshold to be exceeded for readability and maintenance.
# pylint: disable=too-many-lines
from PySide6.QtCore import QSettings, Qt, QThread, QTimer, Signal
from PySide6.QtGui import QAction, QKeySequence
from PySide6.QtWidgets import (
    QComboBox,
    QDialog,
    QDialogButtonBox,
    QFileDialog,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QListWidget,
    QMainWindow,
    QMessageBox,
    QSplitter,
    QStatusBar,
    QTabWidget,
    QTextEdit,
    QToolBar,
    QVBoxLayout,
    QWidget,
)

from ..ai import AIProvider, AIRequest, AIRequestType, get_ai_assistant
from ..core.interpreter import Language
from .canvas import TurtleCanvas
from .collaboration_client import CollaborationClient
from .editor import CodeEditor
from .output import OutputPanel
from .themes import ThemeManager
from .variable_inspector import VariableInspector

# pylint: enable=no-name-in-module


class MainWindow(QMainWindow):
    """Main IDE window with editor, output, and canvas."""

    # This class manages a rich UI with many widgets and stateful
    # attributes; allow larger limits for instance attributes and public
    # methods to avoid excessive linter noise.
    # pylint: disable=too-many-instance-attributes,too-many-public-methods

    def __init__(self):
        super().__init__()

        # Settings for persistence
        self.settings = QSettings("TimeWarp", "IDE")

        # Theme manager
        self.theme_manager = ThemeManager()

        # AI assistant
        self.ai_assistant = get_ai_assistant()

        # Collaboration client
        self.collaboration_client = CollaborationClient()
        self.setup_collaboration_callbacks()

        # Current file tracking (will be per tab)
        self.tab_files = {}  # tab_index -> filename
        self.tab_modified = {}  # tab_index -> bool
        self.tab_languages = {}  # tab_index -> Language

        # Dialog UI elements (initialized in respective methods)
        self.server_input = None
        self.username_input = None
        self.session_list = None
        self.session_name_input = None
        self.session_desc_input = None

        # Debugging UI removed — no per-window breakpoint state

        # Debugging UI removed — debug_paused connections are not used

        # Setup UI
        self.setup_ui()
        self.create_menus()
        self.create_toolbar()
        self.create_statusbar()

        # Restore previous state
        self.restore_state()

        # Apply theme to current editor
        current_editor = self.get_current_editor()
        if current_editor:
            theme_name = self.settings.value("theme", "Dracula")
            self.theme_manager.apply_theme(
                theme_name,
                editor=current_editor,
                output=self.output,
                canvas=self.canvas,
                highlighter=current_editor.highlighter,
            )

        # Ensure output starts cleared at application runtime
        try:
            self.output.clear()
        except AttributeError:  # pragma: no cover - defensive
            # Defensive: some tests or environments may not initialize output
            # exactly the same as runtime; only ignore missing attribute.
            pass

    def setup_collaboration_callbacks(self):
        """Setup collaboration client callbacks."""
        client = self.collaboration_client
        client.on_connected = self.on_collaboration_connected
        client.on_disconnected = self.on_collaboration_disconnected
        client.on_session_joined = self.on_session_joined
        client.on_session_left = self.on_session_left
        client.on_operation_received = self.on_operation_received
        client.on_cursor_update = self.on_cursor_update
        client.on_user_joined = self.on_user_joined
        client.on_user_left = self.on_user_left

    def get_current_editor(self):
        """Get the currently active editor."""
        current_index = self.editor_tabs.currentIndex()
        if current_index >= 0:
            return self.editor_tabs.widget(current_index)
        return None

    def get_current_tab_info(self):
        """Get info for current tab."""
        current_index = self.editor_tabs.currentIndex()
        if current_index >= 0:
            lang = self.tab_languages.get(current_index, Language.BASIC)
            return {
                "file": self.tab_files.get(current_index),
                "modified": self.tab_modified.get(current_index, False),
                "language": lang,
            }
        return {"file": None, "modified": False, "language": Language.BASIC}

    def set_current_tab_info(self, file=None, modified=None, language=None):
        """Set info for current tab."""
        current_index = self.editor_tabs.currentIndex()
        if current_index >= 0:
            if file is not None:
                self.tab_files[current_index] = file
            if modified is not None:
                self.tab_modified[current_index] = modified
            if language is not None:
                self.tab_languages[current_index] = language
            self.update_title()

    def create_new_tab(
        self,
        title: str = "Untitled",
        content: str = "",
        language: Language = Language.BASIC,
    ):
        """Create a new editor tab."""
        editor = CodeEditor(self)
        editor.setPlainText(content)
        editor.set_language(language)
        editor.textChanged.connect(lambda: self.on_text_changed(editor))

        tab_index = self.editor_tabs.addTab(editor, title)
        self.editor_tabs.setCurrentIndex(tab_index)

        # Initialize tab info
        self.tab_files[tab_index] = None
        self.tab_modified[tab_index] = False
        self.tab_languages[tab_index] = language

        return tab_index

    # -- small helpers used by menu actions (keeps lambdas short) --
    def _editor_undo(self, _checked: bool = False):
        ed = self.get_current_editor()
        if ed:
            ed.undo()

    def _editor_redo(self, _checked: bool = False):
        ed = self.get_current_editor()
        if ed:
            ed.redo()

    def _editor_cut(self, _checked: bool = False):
        ed = self.get_current_editor()
        if ed:
            ed.cut()

    def _editor_copy(self, _checked: bool = False):
        ed = self.get_current_editor()
        if ed:
            ed.copy()

    def _editor_paste(self, _checked: bool = False):
        ed = self.get_current_editor()
        if ed:
            ed.paste()

    def close_tab(self, index):
        """Close a tab."""
        if self.check_save_changes_for_tab(index):
            # Build new tab metadata maps for all tabs except the one
            # being closed. Doing this BEFORE removing the tab avoids
            # losing information when indices shift.
            new_tab_files = {}
            new_tab_modified = {}
            new_tab_languages = {}

            new_idx = 0
            for i in range(self.editor_tabs.count()):
                if i == index:
                    continue
                new_tab_files[new_idx] = self.tab_files.get(i)
                new_tab_modified[new_idx] = self.tab_modified.get(i, False)
                new_tab_languages[new_idx] = self.tab_languages.get(i, Language.BASIC)
                new_idx += 1

            # Remove the tab widget
            self.editor_tabs.removeTab(index)

            # Replace the internal maps with the reindexed versions
            self.tab_files = new_tab_files
            self.tab_modified = new_tab_modified
            self.tab_languages = new_tab_languages

            if new_idx == 0:
                # Ensure we always have at least one editor open
                self.new_file()

    def on_tab_changed(self, index):
        """Handle tab change."""
        if index >= 0:
            language = self.tab_languages.get(index, Language.BASIC)

            # Update language combo (if it exists)
            if hasattr(self, "language_combo"):
                for i in range(self.language_combo.count()):
                    if self.language_combo.itemData(i) == language:
                        self.language_combo.setCurrentIndex(i)
                        break

            # Update output language (if it exists)
            if hasattr(self, "output"):
                self.output.set_language(language)

            # Update title
            self.update_title()

    def check_save_changes_for_tab(self, tab_index):
        """Check if tab has unsaved changes and prompt to save."""
        if self.tab_modified.get(tab_index, False):
            filename = self.tab_files.get(tab_index, "Untitled")
            reply = QMessageBox.question(
                self,
                "Unsaved Changes",
                f"'{filename}' has been modified.\n"
                "Do you want to save your changes?",
                QMessageBox.Save | QMessageBox.Discard | QMessageBox.Cancel,
                QMessageBox.Save,
            )
            if reply == QMessageBox.Save:
                return self.save_tab(tab_index)
            if reply == QMessageBox.Cancel:
                return False
        return True

    def setup_ui(self):
        """Setup main UI layout."""
        self.setWindowTitle("Time Warp IDE - Python Edition")
        self.setMinimumSize(1200, 800)

        # Central widget with splitter
        central = QWidget()
        self.setCentralWidget(central)
        layout = QVBoxLayout(central)
        layout.setContentsMargins(0, 0, 0, 0)

        # Main splitter (horizontal)
        splitter = QSplitter(Qt.Horizontal)

        # Left side: Editor tabs
        self.editor_tabs = QTabWidget()
        self.editor_tabs.setTabsClosable(True)
        self.editor_tabs.tabCloseRequested.connect(self.close_tab)
        self.editor_tabs.currentChanged.connect(self.on_tab_changed)

        # Create initial tab
        self.new_file()

        splitter.addWidget(self.editor_tabs)

        # Right side: Tabs for Output and Canvas
        self.right_tabs = QTabWidget()

        # Output panel
        self.output = OutputPanel(self)
        self.right_tabs.addTab(self.output, "Output")

        # Turtle canvas
        self.canvas = TurtleCanvas(self)
        self.right_tabs.addTab(self.canvas, "Graphics")

        # Variable inspector
        self.variable_inspector = VariableInspector(self)
        self.right_tabs.addTab(self.variable_inspector, "Variables")

        splitter.addWidget(self.right_tabs)

        # Set initial splitter sizes (60% editor, 40% output)
        splitter.setSizes([720, 480])

        layout.addWidget(splitter)

    def create_menus(self):
        """Create menu bar."""
        # This method builds many actions and submenu entries; the local
        # variable count is high by design. Keep readability and disable
        # the excessive locals/statements checks for this method.
        # pylint: disable=too-many-locals,too-many-statements
        menubar = self.menuBar()

        # File menu
        file_menu = menubar.addMenu("&File")

        new_action = QAction("&New", self)
        new_action.setShortcut(QKeySequence.New)
        new_action.triggered.connect(self.new_file)
        file_menu.addAction(new_action)

        open_action = QAction("&Open...", self)
        open_action.setShortcut(QKeySequence.Open)
        open_action.triggered.connect(self.open_file)
        file_menu.addAction(open_action)

        save_action = QAction("&Save", self)
        save_action.setShortcut(QKeySequence.Save)
        save_action.triggered.connect(self.save_file)
        file_menu.addAction(save_action)

        # Debugging UI removed entirely — this distribution exposes Run/Stop
        # only. No debug menu is created.

        save_as_action = QAction("Save &As...", self)
        save_as_action.setShortcut(QKeySequence.SaveAs)
        save_as_action.triggered.connect(self.save_file_as)
        file_menu.addAction(save_as_action)

        file_menu.addSeparator()

        # Recent files submenu
        self.recent_menu = file_menu.addMenu("Recent Files")
        self.update_recent_files_menu()

        file_menu.addSeparator()

        exit_action = QAction("E&xit", self)
        exit_action.setShortcut(QKeySequence.Quit)
        exit_action.triggered.connect(self.close)
        file_menu.addAction(exit_action)

        # Edit menu
        edit_menu = menubar.addMenu("&Edit")

        undo_action = QAction("&Undo", self)
        undo_action.setShortcut(QKeySequence.Undo)
        undo_action.triggered.connect(self._editor_undo)
        edit_menu.addAction(undo_action)

        redo_action = QAction("&Redo", self)
        redo_action.setShortcut(QKeySequence.Redo)
        redo_action.triggered.connect(self._editor_redo)
        edit_menu.addAction(redo_action)

        edit_menu.addSeparator()

        cut_action = QAction("Cu&t", self)
        cut_action.setShortcut(QKeySequence.Cut)
        cut_action.triggered.connect(self._editor_cut)
        edit_menu.addAction(cut_action)

        copy_action = QAction("&Copy", self)
        copy_action.setShortcut(QKeySequence.Copy)
        copy_action.triggered.connect(self._editor_copy)
        edit_menu.addAction(copy_action)

        paste_action = QAction("&Paste", self)
        paste_action.setShortcut(QKeySequence.Paste)
        paste_action.triggered.connect(self._editor_paste)
        edit_menu.addAction(paste_action)

        edit_menu.addSeparator()

        find_action = QAction("&Find...", self)
        find_action.setShortcut(QKeySequence.Find)
        find_action.triggered.connect(
            lambda: (
                self.get_current_editor().show_find_dialog()
                if self.get_current_editor()
                else None
            )
        )
        edit_menu.addAction(find_action)

        # Run menu (no debugging items)
        run_menu = menubar.addMenu("&Run")

        self.run_action = QAction("&Run Program", self)
        self.run_action.setShortcut("F5")
        self.run_action.triggered.connect(self.run_program)
        run_menu.addAction(self.run_action)

        self.stop_action = QAction("&Stop", self)
        self.stop_action.setShortcut("Shift+F5")
        self.stop_action.setEnabled(False)
        self.stop_action.triggered.connect(self.stop_program)
        run_menu.addAction(self.stop_action)

        run_menu.addSeparator()

        clear_output_action = QAction("Clear &Output", self)
        clear_output_action.triggered.connect(self.output.clear)
        run_menu.addAction(clear_output_action)

        clear_canvas_action = QAction("Clear &Canvas", self)
        clear_canvas_action.triggered.connect(self.canvas.clear)
        run_menu.addAction(clear_canvas_action)

        # View menu
        view_menu = menubar.addMenu("&View")

        # Zoom in / out helpers
        zoom_in_action = QAction("Zoom &In", self)
        zoom_in_action.setShortcut(QKeySequence.ZoomIn)
        zoom_in_action.triggered.connect(
            lambda: (
                self.get_current_editor().zoom_in()
                if self.get_current_editor()
                else None
            )
        )
        view_menu.addAction(zoom_in_action)

        zoom_out_action = QAction("Zoom &Out", self)
        zoom_out_action.setShortcut(QKeySequence.ZoomOut)
        zoom_out_action.triggered.connect(
            lambda: (
                self.get_current_editor().zoom_out()
                if self.get_current_editor()
                else None
            )
        )
        view_menu.addAction(zoom_out_action)

        # Debugging UI removed — this distribution exposes Run/Stop only.

        # Help menu
        help_menu = menubar.addMenu("&Help")

        examples_action = QAction("&Example Programs...", self)
        examples_action.triggered.connect(self.show_examples)
        help_menu.addAction(examples_action)

        help_menu.addSeparator()

        about_action = QAction("&About Time Warp IDE", self)
        about_action.triggered.connect(self.show_about)
        help_menu.addAction(about_action)

        # AI features are disabled in this distribution. The menu and
        # related UI actions are intentionally omitted so users will not
        # see AI assistant functionality.

        # Collaboration features are disabled in this distribution. The
        # Collaboration menu is omitted to avoid exposing disabled
        # real-time collaboration UI.

    def create_toolbar(self):
        """Create toolbar."""
        toolbar = QToolBar("Main Toolbar")
        toolbar.setObjectName("MainToolbar")  # Avoid Qt warning
        toolbar.setMovable(False)
        self.addToolBar(toolbar)

        # Add common actions
        toolbar.addAction("New", self.new_file)
        toolbar.addAction("Open", self.open_file)
        toolbar.addAction("Save", self.save_file)
        toolbar.addSeparator()
        toolbar.addAction("Run (F5)", self.run_program)
        toolbar.addSeparator()
        # Only a single Stop action is kept in the toolbar
        toolbar.addAction("Stop", self.stop_program)
        toolbar.addSeparator()
        toolbar.addAction("Clear Output", self.output.clear)
        toolbar.addAction("Clear Canvas", self.canvas.clear)

        # Language selector
        toolbar.addSeparator()
        self.language_combo = QComboBox()

        for lang in [
            Language.BASIC,
            Language.PILOT,
            Language.LOGO,
        ]:
            self.language_combo.addItem(lang.friendly_name(), lang)
        self.language_combo.currentIndexChanged.connect(self.on_language_changed)

    def create_statusbar(self):
        """Create status bar."""
        self.statusbar = QStatusBar()
        self.setStatusBar(self.statusbar)
        self.statusbar.showMessage("Ready")

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
            "Time Warp Files (*.pilot *.bas *.logo);;"
            "PILOT Files (*.pilot);;"
            "BASIC Files (*.bas);;"
            "Logo Files (*.logo);;"
            "All Files (*.*)",
            options=QFileDialog.DontUseNativeDialog,
        )

        if filename:
            self.settings.setValue("last_dir", str(Path(filename).parent))
            self.load_file(filename)

    def load_file(self, filename):
        """Load file into current tab."""
        try:
            with open(filename, "r", encoding="utf-8") as f:
                content = f.read()

            # Detect language from file extension
            ext = Path(filename).suffix
            language = Language.from_extension(ext)

            # Create new tab or update current
            current_editor = self.get_current_editor()
            if (
                current_editor
                and not self.get_current_tab_info()["modified"]
                and not self.get_current_tab_info()["file"]
            ):
                # Reuse empty untitled tab
                current_editor.setPlainText(content)
                current_editor.set_language(language)
                tab_title = Path(filename).name
                current_index = self.editor_tabs.currentIndex()
                self.editor_tabs.setTabText(current_index, tab_title)
                self.set_current_tab_info(
                    file=filename, modified=False, language=language
                )
            else:
                # Create new tab
                tab_title = Path(filename).name
                self.create_new_tab(tab_title, content, language)
                self.set_current_tab_info(
                    file=filename, modified=False, language=language
                )

            # Update combo box
            for i in range(self.language_combo.count()):
                if self.language_combo.itemData(i) == language:
                    self.language_combo.setCurrentIndex(i)
                    break

            # Update output language
            self.output.set_language(language)

            self.add_recent_file(filename)
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

        filename = self.tab_files.get(current_index)
        if not filename:
            # No filename yet — prompt save-as flow
            return self.save_tab_as(current_index)

        # Write to the existing file
        try:
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

        editor = self.editor_tabs.widget(tab_index)
        if editor is None:
            return False

        filename = self.tab_files.get(tab_index)
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

        editor = self.editor_tabs.widget(tab_index)
        if editor is None:
            return False

        last_dir = self.settings.value("last_dir", str(Path.home()))

        filename, _ = QFileDialog.getSaveFileName(
            self,
            "Save File As",
            last_dir,
            "Time Warp Files (*.pilot *.bas *.logo);;All Files (*.*)",
            options=QFileDialog.DontUseNativeDialog,
        )

        if not filename:
            return False

        # Ensure the directory is stored for next dialog
        self.settings.setValue("last_dir", str(Path(filename).parent))

        try:
            with open(filename, "w", encoding="utf-8") as f:
                f.write(editor.toPlainText())

            # Update tab metadata and title
            self.tab_files[tab_index] = filename
            self.tab_modified[tab_index] = False
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

    def check_execution_complete(self):
        """Check if execution is complete."""
        if self.output.is_running():
            QTimer.singleShot(100, self.check_execution_complete)
        else:
            self.run_action.setEnabled(True)
            # debug_action removed
            self.stop_action.setEnabled(False)
            # stepping and resume controls removed
            # Disconnect any lingering debug pause handlers to avoid duplicates
            # debug pause handling removed
            self.statusbar.showMessage("Execution complete")
            # If graphics were drawn, switch to Graphics tab for convenience
            try:
                if getattr(self.canvas, "lines", None):
                    if len(self.canvas.lines) > 0:
                        self.right_tabs.setCurrentWidget(self.canvas)
            except (AttributeError, TypeError):
                # Non-fatal; ignore any unexpected attribute/sequence issues
                pass

    def run_program(self):
        """Run the current editor contents in the output panel."""
        editor = self.get_current_editor()
        if not editor:
            return

        current_info = self.get_current_tab_info()
        language = current_info["language"]

        # Ensure output uses the current language and is visible
        self.output.set_language(language)
        try:
            self.right_tabs.setCurrentWidget(self.output)
        except AttributeError:
            # Non-fatal: if right_tabs not yet created in tests, ignore
            # Only expect attribute errors from missing widgets; don't
            # swallow unrelated exceptions.
            pass

        code = editor.toPlainText()

        # Update UI state
        self.run_action.setEnabled(False)
        self.stop_action.setEnabled(True)
        self.statusbar.showMessage("Running program")

        # Start execution (no debug controls in this build)
        self.output.run_program(code, self.canvas, debug_mode=False)

    def stop_program(self):
        """Stop running program."""
        self.output.stop_execution()
        self.run_action.setEnabled(True)
        self.stop_action.setEnabled(False)
        self.statusbar.showMessage("Stopped")
        # debug pause handling removed

    def on_text_changed(self, editor=None):
        """Handle text changes."""
        if editor is None:
            editor = self.get_current_editor()
        if editor:
            # Find which tab this editor belongs to
            for i in range(self.editor_tabs.count()):
                if self.editor_tabs.widget(i) == editor:
                    self.set_current_tab_info(modified=True)
                    break

    def on_language_changed(self):
        """Handle language selection change."""
        current_data = self.language_combo.currentData()
        if current_data:
            current_editor = self.get_current_editor()
            if current_editor:
                current_editor.set_language(current_data)
            # Update interpreter language if needed
            self.output.set_language(current_data)
            # Update tab info
            self.set_current_tab_info(language=current_data)

    def update_title(self):
        """Update window title."""
        title = "Time Warp IDE"

        current_info = self.get_current_tab_info()
        if current_info["file"]:
            title += f" - {Path(current_info['file']).name}"
        else:
            title += " - Untitled"

        if current_info["modified"]:
            title += " *"

        self.setWindowTitle(title)

    def check_save_changes(self):
        """Check if current tab has unsaved changes."""
        current_info = self.get_current_tab_info()
        if not current_info["modified"]:
            return True

        reply = QMessageBox.question(
            self,
            "Unsaved Changes",
            "Do you want to save your changes?",
            QMessageBox.Save | QMessageBox.Discard | QMessageBox.Cancel,
            QMessageBox.Save,
        )

        if reply == QMessageBox.Save:
            self.save_file()
            return True
        if reply == QMessageBox.Discard:
            return True
        return False

    def change_theme(self, theme_name):
        """Change IDE theme."""
        # Apply theme to all editors
        for i in range(self.editor_tabs.count()):
            editor = self.editor_tabs.widget(i)
            if editor:
                self.theme_manager.apply_theme(
                    theme_name,
                    editor=editor,
                    output=self.output,
                    canvas=self.canvas,
                    highlighter=editor.highlighter,
                )

        # Save theme preference
        self.settings.setValue("theme", theme_name)
        self.settings.setValue("theme", theme_name)
        self.statusbar.showMessage(f"Theme changed to: {theme_name}")

    def add_recent_file(self, filename):
        """Add file to recent files list."""
        recent = self.settings.value("recent_files", [])
        if not isinstance(recent, list):
            recent = []

        if filename in recent:
            recent.remove(filename)
        recent.insert(0, filename)
        recent = recent[:10]  # Keep last 10

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

    def show_examples(self):
        """Show examples dialog and load selected example."""
        examples_dir = Path(__file__).parent.parent.parent / "examples"

        if not examples_dir.exists():
            QMessageBox.information(self, "Examples", "Examples dir not found")
        filename, _ = QFileDialog.getOpenFileName(
            self,
            "Open Example",
            str(examples_dir),
            "Time Warp Files (*.pilot *.bas *.logo);;All Files (*.*)",
        )

        if filename:
            self.load_file(filename)

    def show_about(self):
        """Show about dialog."""
        QMessageBox.about(
            self,
            "About Time Warp IDE",
            "<h2>Time Warp IDE - Python Edition</h2>"
            "<p>Version 4.0.0 — Official PySide6 release</p>"
            "<p>Educational programming environment supporting:</p>"
            "<ul>"
            "<li>PILOT - Interactive teaching language</li>"
            "<li>BASIC - Classic BASIC with line numbers</li>"
            "<li>Logo - Turtle graphics for visual learning</li>"
            "</ul>"
            "<p>Powered by unified Time Warp interpreter with optional "
            "Pascal, Prolog, C experiments.</p>"
            "<p><b>Author:</b> James Temple</p>"
            '<p><a href="https://github.com/James-HoneyBadger/Time_Warp">'
            "github.com/James-HoneyBadger/Time_Warp</a><br>"
            '<a href="https://github.com/James-HoneyBadger/Time_Warp/'
            'tree/main/Docs">'
            "Documentation</a></p>",
        )

    def restore_state(self):
        """Restore window state from settings."""
        geometry = self.settings.value("geometry")
        if geometry:
            self.restoreGeometry(geometry)

        state = self.settings.value("windowState")
        if state:
            self.restoreState(state)

    def save_state(self):
        """Save window state to settings."""
        self.settings.setValue("geometry", self.saveGeometry())
        self.settings.setValue("windowState", self.saveState())

    def closeEvent(self, event):  # pylint: disable=invalid-name
        """Handle window close."""
        if self.check_save_changes():
            self.save_state()
            event.accept()
        else:
            event.ignore()

    # Breakpoint toggle removed with debugging features

    # Full debugging support removed from the UI; use Run/Stop only.

    # Debug pause handling removed

    # Step and resume debugging removed — stepping is not supported by the
    # UI in this build. Use stop_program() to terminate a running session.

    def set_ai_provider(self, provider: AIProvider):
        """Set the active AI provider."""
        if self.ai_assistant.set_active_provider(provider):
            self.statusbar.showMessage(f"AI provider set to: {provider.value}")
        else:
            QMessageBox.warning(
                self,
                "AI Provider Error",
                f"Could not set AI provider to {provider.value}.\n"
                "Please check your configuration and try again.",
            )

    def ai_complete_code(self):
        """Request AI code completion."""
        current_editor = self.get_current_editor()
        if not current_editor:
            return

        cursor = current_editor.textCursor()
        code_context = current_editor.toPlainText()
        cursor_position = cursor.position()

        current_info = self.get_current_tab_info()
        language = current_info["language"]

        request = AIRequest(
            request_type=AIRequestType.CODE_COMPLETION,
            code_context=code_context,
            language=language.friendly_name().lower(),
            cursor_position=cursor_position,
        )

        self.statusbar.showMessage("Requesting AI code completion...")

        # Run AI request in background thread
        class AIWorker(QThread):
            """Worker thread for AI requests."""

            # Small helper thread class used only to perform the request.
            # Keep this compact and allow single-public-method pattern.
            # pylint: disable=too-few-public-methods

            finished = Signal(str)

            def __init__(self, ai_assistant, request):
                super().__init__()
                self.ai_assistant = ai_assistant
                self.request = request

            def run(self):
                """Run the AI completion request and emit result."""
                response = self.ai_assistant.generate_completion(self.request)
                if response.success:
                    self.finished.emit(response.content)
                else:
                    self.finished.emit(f"AI Error: {response.content}")

        worker = AIWorker(self.ai_assistant, request)
        worker.finished.connect(self.on_ai_completion_finished)
        worker.start()

    def on_ai_completion_finished(self, completion: str):
        """Handle AI completion response."""
        if completion.startswith("AI Error:"):
            QMessageBox.warning(self, "AI Completion Error", completion)
            self.statusbar.showMessage("AI completion failed")
        else:
            current_editor = self.get_current_editor()
            if current_editor:
                cursor = current_editor.textCursor()
                cursor.insertText(completion)
                self.statusbar.showMessage("AI completion inserted")

    def ai_explain_error(self):
        """Request AI error explanation."""
        # Get error from output panel
        error_text = self.output.get_last_error()
        if not error_text:
            QMessageBox.information(
                self,
                "No Error Found",
                "No recent error found in the output panel.\n"
                "Run your program first to generate an error.",
            )
            return

        current_editor = self.get_current_editor()
        code_context = ""
        if current_editor:
            code_context = current_editor.toPlainText()

        current_info = self.get_current_tab_info()
        language = current_info["language"]

        request = AIRequest(
            request_type=AIRequestType.ERROR_EXPLANATION,
            code_context=code_context,
            language=language.friendly_name().lower(),
            error_message=error_text,
        )

        self.statusbar.showMessage("Requesting AI error explanation...")

        # Run AI request in background thread
        class AIWorker(QThread):
            """Worker thread for AI error explanation."""

            # Small helper thread; permit single-public-method implementation.
            # pylint: disable=too-few-public-methods

            finished = Signal(str)

            def __init__(self, ai_assistant, request):
                super().__init__()
                self.ai_assistant = ai_assistant
                self.request = request

            def run(self):
                """Run the AI explanation request and emit result."""
                response = self.ai_assistant.explain_error(self.request)
                if response.success:
                    self.finished.emit(response.content)
                else:
                    self.finished.emit(f"AI Error: {response.content}")

        worker = AIWorker(self.ai_assistant, request)
        worker.finished.connect(self.on_ai_explanation_finished)
        worker.start()

    def on_ai_explanation_finished(self, explanation: str):
        """Handle AI error explanation response."""
        if explanation.startswith("AI Error:"):
            msg = "AI Error Explanation Failed"
            QMessageBox.warning(self, msg, explanation)
            self.statusbar.showMessage("AI error explanation failed")
        else:
            # Show explanation in a dialog
            dialog = QDialog(self)
            dialog.setWindowTitle("AI Error Explanation")
            dialog.setMinimumSize(600, 400)

            layout = QVBoxLayout(dialog)

            text_edit = QTextEdit()
            text_edit.setPlainText(explanation)
            text_edit.setReadOnly(True)
            layout.addWidget(text_edit)

            buttons = QDialogButtonBox(QDialogButtonBox.Ok)
            buttons.accepted.connect(dialog.accept)
            layout.addWidget(buttons)

            dialog.exec()
            self.statusbar.showMessage("AI error explanation provided")

    def ai_review_code(self):
        """Request AI code review."""
        current_editor = self.get_current_editor()
        if not current_editor:
            return

        code_context = current_editor.toPlainText()
        if not code_context.strip():
            QMessageBox.information(
                self, "No Code to Review", "Please enter some code to review."
            )
            return

        current_info = self.get_current_tab_info()
        language = current_info["language"]

        request = AIRequest(
            request_type=AIRequestType.CODE_REVIEW,
            code_context=code_context,
            language=language.friendly_name().lower(),
        )

        self.statusbar.showMessage("Requesting AI code review...")

        # Run AI request in background thread
        class AIWorker(QThread):
            """Worker thread for AI code review."""

            # Small helper thread; permit single-public-method implementation.
            # pylint: disable=too-few-public-methods

            finished = Signal(str)

            def __init__(self, ai_assistant, request):
                super().__init__()
                self.ai_assistant = ai_assistant
                self.request = request

            def run(self):
                """Run the AI code review request and emit result."""
                response = self.ai_assistant.review_code(self.request)
                if response.success:
                    self.finished.emit(response.content)
                else:
                    self.finished.emit(f"AI Error: {response.content}")

        worker = AIWorker(self.ai_assistant, request)
        worker.finished.connect(self.on_ai_review_finished)
        worker.start()

    def on_ai_review_finished(self, review: str):
        """Handle AI code review response."""
        if review.startswith("AI Error:"):
            QMessageBox.warning(self, "AI Code Review Failed", review)
            self.statusbar.showMessage("AI code review failed")
        else:
            # Show review in a dialog
            dialog = QDialog(self)
            dialog.setWindowTitle("AI Code Review")
            dialog.setMinimumSize(600, 400)

            layout = QVBoxLayout(dialog)

            text_edit = QTextEdit()
            text_edit.setPlainText(review)
            text_edit.setReadOnly(True)
            layout.addWidget(text_edit)

            buttons = QDialogButtonBox(QDialogButtonBox.Ok)
            buttons.accepted.connect(dialog.accept)
            layout.addWidget(buttons)

            dialog.exec()
            self.statusbar.showMessage("AI code review completed")

    def ai_learning_tips(self):
        """Request AI learning suggestions."""
        current_editor = self.get_current_editor()
        code_context = ""
        if current_editor:
            code_context = current_editor.toPlainText()

        current_info = self.get_current_tab_info()
        language = current_info["language"]

        request = AIRequest(
            request_type=AIRequestType.LEARNING_SUGGESTION,
            code_context=code_context,
            language=language.friendly_name().lower(),
            user_level="beginner",  # Could be made configurable
        )

        self.statusbar.showMessage("Requesting AI learning tips...")

        # Run AI request in background thread
        class AIWorker(QThread):
            """Worker thread for AI learning suggestions."""

            # Small helper thread; allow single-public-method pattern.
            # pylint: disable=too-few-public-methods

            finished = Signal(str)

            def __init__(self, ai_assistant, request):
                super().__init__()
                self.ai_assistant = ai_assistant
                self.request = request

            def run(self):
                """Run the AI learning request and emit result."""
                req = self.request
                response = self.ai_assistant.get_learning_suggestion(req)
                if response.success:
                    self.finished.emit(response.content)
                else:
                    self.finished.emit(f"AI Error: {response.content}")

        worker = AIWorker(self.ai_assistant, request)
        worker.finished.connect(self.on_ai_learning_finished)
        worker.start()

    def on_ai_learning_finished(self, tips: str):
        """Handle AI learning tips response."""
        if tips.startswith("AI Error:"):
            QMessageBox.warning(self, "AI Learning Tips Failed", tips)
            self.statusbar.showMessage("AI learning tips failed")
        else:
            # Show tips in a dialog
            dialog = QDialog(self)
            dialog.setWindowTitle("AI Learning Tips")
            dialog.setMinimumSize(600, 400)

            layout = QVBoxLayout(dialog)

            text_edit = QTextEdit()
            text_edit.setPlainText(tips)
            text_edit.setReadOnly(True)
            layout.addWidget(text_edit)

            buttons = QDialogButtonBox(QDialogButtonBox.Ok)
            buttons.accepted.connect(dialog.accept)
            layout.addWidget(buttons)

            dialog.exec()
            self.statusbar.showMessage("AI learning tips provided")

    def collab_connect(self):
        """Connect to collaboration server."""
        if not self.collaboration_client:
            QMessageBox.warning(
                self,
                "Collaboration Error",
                "Collaboration client not available.",
            )
            return

        # Show connection dialog
        dialog = QDialog(self)
        dialog.setWindowTitle("Connect to Collaboration Server")
        dialog.setModal(True)

        layout = QVBoxLayout(dialog)

        # Server address input
        server_layout = QHBoxLayout()
        server_layout.addWidget(QLabel("Server:"))
        self.server_input = QLineEdit("localhost:8765")
        server_layout.addWidget(self.server_input)
        layout.addLayout(server_layout)

        # Username input
        user_layout = QHBoxLayout()
        user_layout.addWidget(QLabel("Username:"))
        self.username_input = QLineEdit()
        # Try to get username from system
        try:
            self.username_input.setText(getpass.getuser())
        except (OSError, ImportError):
            self.username_input.setText("Anonymous")
        user_layout.addWidget(self.username_input)
        layout.addLayout(user_layout)

        # Buttons
        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttons.accepted.connect(dialog.accept)
        buttons.rejected.connect(dialog.reject)
        layout.addWidget(buttons)

        if dialog.exec() == QDialog.Accepted:
            server = self.server_input.text().strip()
            username = self.username_input.text().strip()

            if not server or not username:
                QMessageBox.warning(
                    self, "Invalid Input", "Enter server address and username."
                )
                return

            msg = f"Connecting to {server} as {username}..."
            self.statusbar.showMessage(msg)

            # Connect in background thread
            def connect_callback(success, message):
                if success:
                    self.collab_disconnect_action.setEnabled(True)
                    self.statusbar.showMessage(f"Connected to {server}")
                    QMessageBox.information(
                        self,
                        "Connected",
                        f"Successfully connected to collaboration server as "
                        f"{username}.",
                    )
                else:
                    self.statusbar.showMessage("Connection failed")
                    QMessageBox.warning(
                        self,
                        "Connection Failed",
                        f"Could not connect to server:\n{message}",
                    )

            self.collaboration_client.connect(
                server,
                username,
                connect_callback,
            )

    def collab_disconnect(self):
        """Disconnect from collaboration server."""
        if self.collaboration_client:
            self.collaboration_client.disconnect()
            self.collab_disconnect_action.setEnabled(False)
            self.statusbar.showMessage("Disconnected from collaboration server")

    def collab_join_session(self):
        """Join a collaboration session."""
        if (
            not self.collaboration_client
            or not self.collaboration_client.is_connected()
        ):
            QMessageBox.warning(
                self,
                "Not Connected",
                "Please connect to a collaboration server first.",
            )
            return

        # Show session selection dialog
        dialog = QDialog(self)
        dialog.setWindowTitle("Join Collaboration Session")
        dialog.setModal(True)

        layout = QVBoxLayout(dialog)

        layout.addWidget(QLabel("Available Sessions:"))

        # Session list (would be populated from server)
        self.session_list = QListWidget()
        # For now, add some example sessions
        self.session_list.addItem("Session 1 - Python Project")
        self.session_list.addItem("Session 2 - Logo Graphics")
        self.session_list.addItem("Session 3 - BASIC Tutorial")
        layout.addWidget(self.session_list)

        # Buttons
        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttons.accepted.connect(dialog.accept)
        buttons.rejected.connect(dialog.reject)
        layout.addWidget(buttons)

        if dialog.exec() == QDialog.Accepted:
            selected_items = self.session_list.selectedItems()
            if not selected_items:
                QMessageBox.warning(
                    self, "No Selection", "Please select a session to join."
                )
                return

            session_name = selected_items[0].text().split(" - ")[0]
            self.statusbar.showMessage(f"Joining session {session_name}...")

            # Join session (placeholder - would call client method)
            def join_callback(success, message):
                if success:
                    msg = f"Joined session {session_name}"
                    self.statusbar.showMessage(msg)
                    QMessageBox.information(
                        self,
                        "Joined Session",
                        f"Successfully joined {session_name}.",
                    )
                else:
                    self.statusbar.showMessage("Failed to join session")
                    QMessageBox.warning(
                        self,
                        "Join Failed",
                        f"Could not join session:\n{message}",
                    )

            # Placeholder - would call client.join_session()
            join_callback(True, "Session joined successfully")

    def collab_create_session(self):
        """Create a new collaboration session."""
        if (
            not self.collaboration_client
            or not self.collaboration_client.is_connected()
        ):
            QMessageBox.warning(
                self,
                "Not Connected",
                "Please connect to a collaboration server first.",
            )
            return

        # Show session creation dialog
        dialog = QDialog(self)
        dialog.setWindowTitle("Create Collaboration Session")
        dialog.setModal(True)

        layout = QVBoxLayout(dialog)

        # Session name input
        name_layout = QHBoxLayout()
        name_layout.addWidget(QLabel("Session Name:"))
        self.session_name_input = QLineEdit("My Project Session")
        name_layout.addWidget(self.session_name_input)
        layout.addLayout(name_layout)

        # Session description
        desc_layout = QHBoxLayout()
        desc_layout.addWidget(QLabel("Description:"))
        self.session_desc_input = QLineEdit("Collaborative coding session")
        desc_layout.addWidget(self.session_desc_input)
        layout.addLayout(desc_layout)

        # Buttons
        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttons.accepted.connect(dialog.accept)
        buttons.rejected.connect(dialog.reject)
        layout.addWidget(buttons)

        if dialog.exec() == QDialog.Accepted:
            session_name = self.session_name_input.text().strip()
            # session_desc unused placeholder

            if not session_name:
                QMessageBox.warning(
                    self, "Invalid Input", "Please enter a session name."
                )
                return

            self.statusbar.showMessage(f"Creating session '{session_name}'...")

            # Create session (placeholder - would call client method)
            def create_callback(success, message):
                if success:
                    msg = f"Created session '{session_name}'"
                    self.statusbar.showMessage(msg)
                    QMessageBox.information(
                        self,
                        "Session Created",
                        f"Successfully created session '{session_name}'.",
                    )
                else:
                    self.statusbar.showMessage("Failed to create session")
                    QMessageBox.warning(
                        self,
                        "Create Failed",
                        f"Could not create session:\n{message}",
                    )

            # Placeholder - actual implementation would call
            # client.create_session()
            create_callback(True, "Session created successfully")

    def collab_show_users(self):
        """Show users in current session."""
        if (
            not self.collaboration_client
            or not self.collaboration_client.is_connected()
        ):
            QMessageBox.warning(
                self,
                "Not Connected",
                "Please connect to a collaboration server first.",
            )
            return

        # Show users dialog
        dialog = QDialog(self)
        dialog.setWindowTitle("Session Users")
        dialog.setModal(True)

        layout = QVBoxLayout(dialog)

        layout.addWidget(QLabel("Current Session Users:"))

        # User list (would be populated from client)
        user_list = QListWidget()
        # For now, add example users
        user_list.addItem("👑 Alice (Host)")
        user_list.addItem("👤 Bob")
        user_list.addItem("👤 Charlie")
        user_list.addItem("👤 You")
        layout.addWidget(user_list)

        # Buttons
        buttons = QDialogButtonBox(QDialogButtonBox.Ok)
        buttons.accepted.connect(dialog.accept)
        layout.addWidget(buttons)

        dialog.exec()

    def collab_share_project(self):
        """Share current project."""
        if (
            not self.collaboration_client
            or not self.collaboration_client.is_connected()
        ):
            QMessageBox.warning(
                self,
                "Not Connected",
                "Please connect to a collaboration server first.",
            )
            return

        current_info = self.get_current_tab_info()
        if not current_info["file"]:
            QMessageBox.warning(
                self,
                "No File Open",
                "Please open a file to share.",
            )
            return

        filename = current_info["file"]
        self.statusbar.showMessage(f"Sharing project: {Path(filename).name}")

        # Share project (placeholder - would call client method)
        def share_callback(success, message):
            if success:
                self.statusbar.showMessage("Project shared successfully")
                QMessageBox.information(
                    self,
                    "Project Shared",
                    "Your project has been shared with the session.",
                )
            else:
                self.statusbar.showMessage("Failed to share project")
                QMessageBox.warning(
                    self,
                    "Share Failed",
                    f"Could not share project:\n{message}",
                )

        # Placeholder - actual implementation would call client.share_project()
        share_callback(True, "Project shared successfully")

    def on_collaboration_connected(self):
        """Handle collaboration connection."""
        self.collab_disconnect_action.setEnabled(True)
        self.statusbar.showMessage("Connected to collaboration server")

    def on_collaboration_disconnected(self):
        """Handle collaboration disconnection."""
        self.collab_disconnect_action.setEnabled(False)
        self.statusbar.showMessage("Disconnected from collaboration server")

    def on_session_joined(self, _session_id, session_name):
        """Handle joining a session."""
        self.statusbar.showMessage(f"Joined session: {session_name}")

    def on_session_left(self, _session_id):
        """Handle leaving a session."""
        self.statusbar.showMessage("Left collaboration session")

    def on_operation_received(self, _operation):
        """Handle incoming collaborative operation."""
        # Apply operation to current editor
        current_editor = self.get_current_editor()
        if current_editor:
            # Apply the operation (placeholder - would use
            # operational transform)
            pass

    def on_cursor_update(self, _user_id, _position):
        """Handle cursor position update from other users."""
        # Update cursor display for other users

    def on_user_joined(self, _user_id, username):
        """Handle user joining session."""
        self.statusbar.showMessage(f"User joined: {username}")

    def on_user_left(self, _user_id, username):
        """Handle user leaving session."""
        self.statusbar.showMessage(f"User left: {username}")
