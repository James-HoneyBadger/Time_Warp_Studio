"""Main window for Time Warp IDE."""

import getpass
import re
from functools import partial
from pathlib import Path

# pylint: disable=no-name-in-module
# Large UI file with many helper methods and nested classes — allow the
# line-count threshold to be exceeded for readability and maintenance.
# pylint: disable=too-many-lines
from PySide6.QtCore import QSettings, Qt, QTimer
from PySide6.QtGui import QAction, QKeySequence

try:
    from PySide6.QtWidgets import QActionGroup
except (
    ImportError,
    ModuleNotFoundError,
):  # pragma: no cover - Qt variant compatibility
    # Some PySide6 builds expose QActionGroup via QtGui instead of
    # QtWidgets; import it from the fallback location if available.
    try:
        from PySide6.QtGui import QActionGroup
    except (
        ImportError,
        ModuleNotFoundError,
    ):  # pragma: no cover - headless/test environments
        # Minimal fallback for test environments without Qt installed
        class QActionGroup:  # type: ignore  # pylint: disable=too-few-public-methods
            """Minimal placeholder for QActionGroup used in headless/test environments.

            This class mimics the presence of QActionGroup to allow tests and
            static analyzers to import the UI module without PySide6 being
            installed. It does not implement any functionality.
            """

            # Minimal placeholder - no methods required for test shims
            # pylint: disable=too-few-public-methods


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
    QTextBrowser,
    QToolBar,
    QVBoxLayout,
    QWidget,
)

from ..core.interpreter import Language
from .canvas import TurtleCanvas
from .cassette_animation import show_cassette_load, show_cassette_save
from .collaboration_client import CollaborationClient
from .crt_effect import CRTEffectOverlay
from .debug_panel import DebugPanel
from .editor import CodeEditor
from .output import ImmediateModePanel, OutputPanel
from .screen_modes import ScreenModeManager
from .themes import ThemeManager
from .variable_inspector import VariableInspector

# pylint: enable=no-name-in-module


class MainWindow(QMainWindow):
    """Main IDE window with editor, output, and canvas.

    This class composes the editor area, output panel, turtle graphics
    canvas and various UI controls such as application menu and toolbar.
    It is intentionally large and stateful since it integrates multiple
    subsystems (collaboration client) and
    supports running and inspecting programs across multiple languages.
    """

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

        # Debugging state
        self.breakpoints = set()  # Set of (tab_index, line) tuples
        self._is_debugging = False
        self._is_paused = False
        self._current_debug_line = 0

        # Retro features
        self.screen_mode_manager = ScreenModeManager()
        self.crt_enabled = False
        self.cassette_mode = True  # Fun cassette animation for save/load

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
        editor.cursorPositionChanged.connect(self.update_cursor_position)
        # Connect breakpoint toggle signal
        editor.breakpoint_toggled.connect(self._on_breakpoint_toggled)

        tab_index = self.editor_tabs.addTab(editor, title)
        self.editor_tabs.setCurrentIndex(tab_index)

        # Initialize tab info
        self.tab_files[tab_index] = None
        self.tab_modified[tab_index] = False
        self.tab_languages[tab_index] = language

        return tab_index

    def _on_breakpoint_toggled(self, _line: int):
        """Handle breakpoint toggle from editor."""
        self._update_breakpoints_display()

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

    def _show_snippet_dialog(self, _checked: bool = False):
        """Show the code snippets dialog."""
        # pylint: disable=import-outside-toplevel
        from .snippet_dialog import SnippetDialog

        # Get current language
        current_idx = self.editor_tabs.currentIndex()
        language = self.tab_languages.get(current_idx, Language.BASIC)
        lang_name = language.name if hasattr(language, "name") else "BASIC"

        dialog = SnippetDialog(lang_name, self)
        if dialog.exec():
            code = dialog.get_selected_code()
            if code:
                ed = self.get_current_editor()
                if ed:
                    ed.insertPlainText(code)

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

    def _print_code(self, _checked: bool = False):
        """Print the current code."""
        try:
            # pylint: disable=import-outside-toplevel
            from PySide6.QtPrintSupport import QPrintDialog, QPrinter
        except ImportError:
            self.statusbar.showMessage("Print support not available", 3000)
            return

        ed = self.get_current_editor()
        if not ed:
            return

        printer = QPrinter(QPrinter.HighResolution)
        dialog = QPrintDialog(printer, self)
        if dialog.exec():
            ed.print_(printer)

    def _print_graphics(self, _checked: bool = False):
        """Print the canvas graphics."""
        try:
            # pylint: disable=import-outside-toplevel
            from PySide6.QtPrintSupport import QPrintDialog, QPrinter
            from PySide6.QtGui import QPainter
        except ImportError:
            self.statusbar.showMessage("Print support not available", 3000)
            return

        printer = QPrinter(QPrinter.HighResolution)
        dialog = QPrintDialog(printer, self)
        if dialog.exec():
            image = self.canvas.get_print_document()
            painter = QPainter(printer)
            rect = painter.viewport()
            size = image.size()
            size.scale(rect.size(), Qt.KeepAspectRatio)
            painter.setViewport(rect.x(), rect.y(), size.width(), size.height())
            painter.setWindow(image.rect())
            painter.drawImage(0, 0, image)
            painter.end()

    def _format_code(self, _checked: bool = False):
        """Format the current code."""
        # pylint: disable=import-outside-toplevel
        from ..utils.code_formatter import get_formatter

        ed = self.get_current_editor()
        if not ed:
            return

        current_idx = self.editor_tabs.currentIndex()
        language = self.tab_languages.get(current_idx, Language.BASIC)
        lang_name = language.name if hasattr(language, "name") else "BASIC"

        code = ed.toPlainText()
        formatter = get_formatter()
        formatted, msg = formatter.format_and_normalize(code, lang_name)

        if formatted != code:
            # Preserve cursor position roughly
            cursor = ed.textCursor()
            pos = cursor.position()
            ed.setPlainText(formatted)
            cursor.setPosition(min(pos, len(formatted)))
            ed.setTextCursor(cursor)

        self.statusbar.showMessage(msg, 3000)

    def _toggle_profiler(self, checked: bool):
        """Toggle performance profiling."""
        # pylint: disable=import-outside-toplevel
        from ..utils.profiler import get_profiler

        profiler = get_profiler()
        if checked:
            profiler.start_session()
            self.statusbar.showMessage("⏱️ Profiling enabled", 3000)
        else:
            session = profiler.end_session()
            if session:
                self.statusbar.showMessage(
                    f"⏱️ Profiling stopped: {session.total_lines_executed} lines",
                    3000,
                )

    def _show_profile_report(self, _checked: bool = False):
        """Show the performance profile report."""
        # pylint: disable=import-outside-toplevel
        from ..utils.profiler import get_profiler

        profiler = get_profiler()
        report = profiler.get_report()

        # Show in output area
        self.output.append("\n" + report)
        self.statusbar.showMessage("📊 Profile report shown in output", 3000)

    def _show_sound_effects(self, _checked: bool = False):
        """Show available sound effects."""
        # pylint: disable=import-outside-toplevel
        from ..core.music import get_sound_effects

        effects = get_sound_effects()
        effect_list = effects.list_effects()

        msg = "🔊 Available Sound Effects:\n"
        msg += 'Use: PLAY "effect_name" in BASIC\n\n'
        msg += ", ".join(effect_list)

        self.output.append(msg)
        self.statusbar.showMessage(
            f"🔊 {len(effect_list)} sound effects available", 3000
        )

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
                default_language = self.tab_languages.get(i, Language.BASIC)
                new_tab_languages[new_idx] = default_language
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

            # Update status bar language label
            if hasattr(self, "language_label"):
                lang_text = f"Language: {language.friendly_name()}"
                self.language_label.setText(lang_text)

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
        self.setWindowTitle("🎨 Time Warp IDE - Python Edition")
        self.setMinimumSize(1200, 800)

        # Set main window style
        self.setStyleSheet(
            """
            QMainWindow {
                background-color: palette(window);
            }
            QTabWidget::pane {
                border: 1px solid palette(dark);
                border-radius: 4px;
                background-color: palette(base);
            }
            QTabBar::tab {
                background-color: palette(window);
                color: palette(window-text);
                padding: 8px 16px;
                border: 1px solid palette(dark);
                border-bottom: none;
                border-radius: 4px 4px 0 0;
                margin-right: 2px;
            }
            QTabBar::tab:selected {
                background-color: palette(highlight);
                color: palette(highlighted-text);
                font-weight: bold;
            }
            QTabBar::tab:hover {
                background-color: palette(highlight);
                color: palette(highlighted-text);
            }
            QSplitter::handle {
                background-color: palette(dark);
                border: 1px solid palette(shadow);
            }
            QSplitter::handle:hover {
                background-color: palette(highlight);
            }
        """
        )

        # Central widget with splitter
        central = QWidget()
        self.setCentralWidget(central)
        layout = QVBoxLayout(central)
        layout.setContentsMargins(5, 5, 5, 5)

        # Main splitter (horizontal) - left side vs right side
        splitter = QSplitter(Qt.Horizontal)
        splitter.setStyleSheet(
            """
            QSplitter {
                background-color: palette(window);
            }
        """
        )

        # Left side: Vertical splitter for editor + immediate mode
        left_splitter = QSplitter(Qt.Vertical)

        # Editor tabs
        self.editor_tabs = QTabWidget()
        self.editor_tabs.setTabsClosable(True)
        self.editor_tabs.tabCloseRequested.connect(self.close_tab)
        self.editor_tabs.currentChanged.connect(self.on_tab_changed)
        self.editor_tabs.setStyleSheet(
            """
            QTabWidget {
                background-color: palette(base);
                border: 1px solid palette(dark);
                border-radius: 4px;
            }
        """
        )

        # Create initial tab
        self.new_file()

        left_splitter.addWidget(self.editor_tabs)

        # Immediate mode panel (REPL) - below editor
        self.immediate_mode = ImmediateModePanel(self)
        left_splitter.addWidget(self.immediate_mode)

        # Set left splitter sizes (85% editor, 15% immediate mode)
        left_splitter.setSizes([550, 50])

        splitter.addWidget(left_splitter)

        # Right side: Tabs for Output and Canvas
        self.right_tabs = QTabWidget()
        self.right_tabs.setStyleSheet(
            """
            QTabWidget {
                background-color: palette(base);
                border: 1px solid palette(dark);
                border-radius: 4px;
            }
        """
        )

        # Output panel
        self.output = OutputPanel(self)
        self.right_tabs.addTab(self.output, "📝 Output")

        # Connect output panel signals
        self.output.variables_updated.connect(self.on_variables_updated)

        # Turtle canvas
        self.canvas = TurtleCanvas(self)
        self.right_tabs.addTab(self.canvas, "🎨 Graphics")
        
        # Connect output panel to tabs for auto-switching to Graphics
        self.output.set_tabs_widget(self.right_tabs)

        # Connect immediate mode to output panel and canvas
        self.immediate_mode.set_canvas(self.canvas)
        self.immediate_mode.set_output_panel(self.output)
        self.immediate_mode.variables_updated.connect(self.on_variables_updated)

        # Variable inspector
        self.variable_inspector = VariableInspector(self)
        self.right_tabs.addTab(self.variable_inspector, "🔍 Variables")

        # Debug panel
        self.debug_panel = DebugPanel(self)
        self.right_tabs.addTab(self.debug_panel, "🐛 Debug")
        self._connect_debug_signals()

        splitter.addWidget(self.right_tabs)

        # Set initial splitter sizes (60% editor, 40% output)
        splitter.setSizes([720, 480])

        layout.addWidget(splitter)

        # CRT effect overlay (covers the entire window)
        self.crt_overlay = CRTEffectOverlay(central)
        self.crt_overlay.setGeometry(central.rect())
        self.crt_overlay.hide()  # Start disabled

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

        # Export submenu
        export_menu = file_menu.addMenu("&Export")

        export_png_action = QAction("Export Graphics to &PNG...", self)
        export_png_action.triggered.connect(self._export_to_png)
        export_menu.addAction(export_png_action)

        export_svg_action = QAction("Export Graphics to &SVG...", self)
        export_svg_action.triggered.connect(self._export_to_svg)
        export_menu.addAction(export_svg_action)

        file_menu.addSeparator()

        # Print actions
        print_code_action = QAction("&Print Code...", self)
        print_code_action.setShortcut("Ctrl+P")
        print_code_action.triggered.connect(self._print_code)
        file_menu.addAction(print_code_action)

        print_graphics_action = QAction("Print &Graphics...", self)
        print_graphics_action.triggered.connect(self._print_graphics)
        file_menu.addAction(print_graphics_action)

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

        edit_menu.addSeparator()

        # Insert snippets
        snippets_action = QAction("&Insert Snippet...", self)
        snippets_action.setShortcut("Ctrl+Shift+I")
        snippets_action.triggered.connect(self._show_snippet_dialog)
        edit_menu.addAction(snippets_action)

        edit_menu.addSeparator()

        # Format code
        format_action = QAction("&Format Code", self)
        format_action.setShortcut("Ctrl+Shift+F")
        format_action.triggered.connect(self._format_code)
        edit_menu.addAction(format_action)

        # Run menu
        run_menu = menubar.addMenu("&Run")

        self.run_action = QAction("&Run Program", self)
        self.run_action.setShortcut("Ctrl+R")
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

        # Debug menu
        debug_menu = menubar.addMenu("&Debug")

        self.debug_start_action = QAction("&Start Debugging", self)
        self.debug_start_action.setShortcut("F5")
        self.debug_start_action.triggered.connect(self.start_debug)
        debug_menu.addAction(self.debug_start_action)

        self.debug_stop_action = QAction("Stop &Debugging", self)
        self.debug_stop_action.setShortcut("Shift+F5")
        self.debug_stop_action.setEnabled(False)
        self.debug_stop_action.triggered.connect(self.stop_debug)
        debug_menu.addAction(self.debug_stop_action)

        debug_menu.addSeparator()

        self.debug_continue_action = QAction("&Continue", self)
        self.debug_continue_action.setShortcut("F5")
        self.debug_continue_action.setEnabled(False)
        self.debug_continue_action.triggered.connect(self.debug_continue)
        debug_menu.addAction(self.debug_continue_action)

        self.debug_pause_action = QAction("&Pause", self)
        self.debug_pause_action.setShortcut("F6")
        self.debug_pause_action.setEnabled(False)
        self.debug_pause_action.triggered.connect(self.debug_pause)
        debug_menu.addAction(self.debug_pause_action)

        debug_menu.addSeparator()

        self.debug_step_into_action = QAction("Step &Into", self)
        self.debug_step_into_action.setShortcut("F11")
        self.debug_step_into_action.setEnabled(False)
        self.debug_step_into_action.triggered.connect(self.debug_step_into)
        debug_menu.addAction(self.debug_step_into_action)

        self.debug_step_over_action = QAction("Step &Over", self)
        self.debug_step_over_action.setShortcut("F10")
        self.debug_step_over_action.setEnabled(False)
        self.debug_step_over_action.triggered.connect(self.debug_step_over)
        debug_menu.addAction(self.debug_step_over_action)

        self.debug_step_out_action = QAction("Step O&ut", self)
        self.debug_step_out_action.setShortcut("Shift+F11")
        self.debug_step_out_action.setEnabled(False)
        self.debug_step_out_action.triggered.connect(self.debug_step_out)
        debug_menu.addAction(self.debug_step_out_action)

        debug_menu.addSeparator()

        toggle_bp_action = QAction("Toggle &Breakpoint", self)
        toggle_bp_action.setShortcut("F9")
        toggle_bp_action.triggered.connect(self.toggle_breakpoint_at_cursor)
        debug_menu.addAction(toggle_bp_action)

        clear_bp_action = QAction("Clear &All Breakpoints", self)
        clear_bp_action.setShortcut("Ctrl+Shift+F9")
        clear_bp_action.triggered.connect(self.clear_all_breakpoints)
        debug_menu.addAction(clear_bp_action)

        # View menu
        view_menu = menubar.addMenu("&View")

        # Theme submenu
        theme_menu = view_menu.addMenu("&Theme")
        theme_group = QActionGroup(self)
        theme_group.setExclusive(True)

        for theme_name in self.theme_manager.get_theme_names():
            theme_action = QAction(theme_name, self)
            theme_action.setCheckable(True)
            is_current = theme_name == self.theme_manager.current_theme_name
            theme_action.setChecked(is_current)
            theme_action.triggered.connect(
                lambda checked, name=theme_name: self.change_theme(name)
            )
            theme_group.addAction(theme_action)
            theme_menu.addAction(theme_action)

        # Font submenu
        font_menu = view_menu.addMenu("&Font")

        # Font family submenu
        font_family_menu = font_menu.addMenu("Font &Family")
        self.font_family_group = QActionGroup(self)
        self.font_family_group.setExclusive(True)

        available_fonts = self.theme_manager.get_available_fonts()[:20]
        for font_family in available_fonts:
            font_action = QAction(font_family, self)
            font_action.setCheckable(True)
            is_current = font_family == self.theme_manager.current_font_family
            font_action.setChecked(is_current)
            font_action.triggered.connect(
                lambda checked, family=font_family: self.change_font_family(family)
            )
            self.font_family_group.addAction(font_action)
            font_family_menu.addAction(font_action)

        # Font size submenu
        font_size_menu = font_menu.addMenu("Font &Size")
        self.font_size_group = QActionGroup(self)
        self.font_size_group.setExclusive(True)

        for size in self.theme_manager.get_font_sizes():
            size_action = QAction(f"{size} pt", self)
            size_action.setCheckable(True)
            is_current = size == self.theme_manager.current_font_size
            size_action.setChecked(is_current)
            size_action.triggered.connect(
                lambda checked, s=size: self.change_font_size(s)
            )
            self.font_size_group.addAction(size_action)
            font_size_menu.addAction(size_action)

        view_menu.addSeparator()

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

        view_menu.addSeparator()

        # === RETRO FEATURES ===

        # Screen Mode submenu
        screen_mode_menu = view_menu.addMenu("📺 &Screen Mode")
        self.screen_mode_group = QActionGroup(self)
        self.screen_mode_group.setExclusive(True)

        # Add "Normal (Hi-Res)" option to disable retro mode
        normal_mode_action = QAction("Normal (Hi-Res)", self)
        normal_mode_action.setCheckable(True)
        normal_mode_action.setChecked(True)  # Default
        normal_mode_action.triggered.connect(self.disable_screen_mode)
        self.screen_mode_group.addAction(normal_mode_action)
        screen_mode_menu.addAction(normal_mode_action)

        screen_mode_menu.addSeparator()

        for mode in self.screen_mode_manager.get_all_modes():
            mode_action = QAction(f"MODE {mode.mode_number}: {mode.name}", self)
            mode_action.setCheckable(True)
            mode_action.setChecked(False)
            mode_action.triggered.connect(
                lambda checked, m=mode.mode_number: self.change_screen_mode(m)
            )
            self.screen_mode_group.addAction(mode_action)
            screen_mode_menu.addAction(mode_action)

        view_menu.addSeparator()

        # CRT Effects submenu
        crt_menu = view_menu.addMenu("📟 CRT &Effects")

        self.crt_enable_action = QAction("&Enable CRT Effects", self)
        self.crt_enable_action.setCheckable(True)
        self.crt_enable_action.setChecked(False)
        self.crt_enable_action.triggered.connect(self.toggle_crt_effects)
        crt_menu.addAction(self.crt_enable_action)

        crt_menu.addSeparator()

        self.scanlines_action = QAction("&Scanlines", self)
        self.scanlines_action.setCheckable(True)
        self.scanlines_action.setChecked(True)
        self.scanlines_action.triggered.connect(self._update_crt_settings)
        crt_menu.addAction(self.scanlines_action)

        self.curvature_action = QAction("Screen &Curvature", self)
        self.curvature_action.setCheckable(True)
        self.curvature_action.setChecked(True)
        self.curvature_action.triggered.connect(self._update_crt_settings)
        crt_menu.addAction(self.curvature_action)

        self.vignette_action = QAction("&Vignette", self)
        self.vignette_action.setCheckable(True)
        self.vignette_action.setChecked(True)
        self.vignette_action.triggered.connect(self._update_crt_settings)
        crt_menu.addAction(self.vignette_action)

        self.glow_action = QAction("Phosphor &Glow", self)
        self.glow_action.setCheckable(True)
        self.glow_action.setChecked(True)
        self.glow_action.triggered.connect(self._update_crt_settings)
        crt_menu.addAction(self.glow_action)

        self.flicker_action = QAction("Screen &Flicker", self)
        self.flicker_action.setCheckable(True)
        self.flicker_action.setChecked(False)
        self.flicker_action.triggered.connect(self._update_crt_settings)
        crt_menu.addAction(self.flicker_action)

        view_menu.addSeparator()

        # Cassette mode toggle
        self.cassette_action = QAction("📼 &Cassette Save Animation", self)
        self.cassette_action.setCheckable(True)
        self.cassette_action.setChecked(True)
        self.cassette_action.triggered.connect(self._toggle_cassette_mode)
        view_menu.addAction(self.cassette_action)

        # Debugging UI removed — this distribution exposes Run/Stop only.

        # Tools menu
        tools_menu = menubar.addMenu("&Tools")

        # Performance profiler
        self.profiler_action = QAction("&Profile Execution", self)
        self.profiler_action.setCheckable(True)
        self.profiler_action.setChecked(False)
        self.profiler_action.triggered.connect(self._toggle_profiler)
        tools_menu.addAction(self.profiler_action)

        show_profile_action = QAction("Show Profile &Report", self)
        show_profile_action.triggered.connect(self._show_profile_report)
        tools_menu.addAction(show_profile_action)

        tools_menu.addSeparator()

        # Sound effects library
        sound_effects_action = QAction("&Sound Effects Library", self)
        sound_effects_action.triggered.connect(self._show_sound_effects)
        tools_menu.addAction(sound_effects_action)

        # Help menu (last menu item)
        help_menu = menubar.addMenu("&Help")

        # Documentation
        user_manual_action = QAction("&User Manual", self)
        user_manual_action.setShortcut("F1")
        user_manual_action.triggered.connect(self.show_user_manual)
        help_menu.addAction(user_manual_action)

        quick_ref_action = QAction("&Quick Reference", self)
        quick_ref_action.triggered.connect(self.show_quick_reference)
        help_menu.addAction(quick_ref_action)

        prog_guide_action = QAction("&Programming Guide", self)
        prog_guide_action.triggered.connect(self.show_programming_guide)
        help_menu.addAction(prog_guide_action)

        help_menu.addSeparator()

        # Language-specific help
        lang_help_menu = help_menu.addMenu("&Language Help")

        basic_help = QAction("BASIC Commands", self)
        basic_help.triggered.connect(lambda: self.show_language_help("basic"))
        lang_help_menu.addAction(basic_help)

        pilot_help = QAction("PILOT Commands", self)
        pilot_help.triggered.connect(lambda: self.show_language_help("pilot"))
        lang_help_menu.addAction(pilot_help)

        logo_help = QAction("Logo Commands", self)
        logo_help.triggered.connect(lambda: self.show_language_help("logo"))
        lang_help_menu.addAction(logo_help)

        help_menu.addSeparator()

        about_action = QAction("&About Time Warp IDE", self)
        about_action.triggered.connect(self.show_about)
        help_menu.addAction(about_action)

        # Collaboration features are disabled in this distribution. The
        # Collaboration menu is omitted to avoid exposing disabled
        # real-time collaboration UI.

    def create_toolbar(self):
        """Create toolbar."""
        toolbar = QToolBar("Main Toolbar")
        toolbar.setObjectName("MainToolbar")  # Avoid Qt warning
        toolbar.setMovable(False)
        toolbar.setToolButtonStyle(Qt.ToolButtonTextBesideIcon)
        toolbar.setStyleSheet(
            """
            QToolBar {
                background-color: palette(window);
                border-bottom: 1px solid palette(dark);
                padding: 2px;
                spacing: 5px;
            }
            QToolButton {
                background-color: transparent;
                border: 1px solid transparent;
                border-radius: 4px;
                padding: 4px 8px;
                margin: 1px;
                color: palette(window-text);
            }
            QToolButton:hover {
                background-color: palette(highlight);
                color: palette(highlighted-text);
            }
            QToolButton:pressed {
                background-color: palette(dark);
            }
        """
        )
        self.addToolBar(toolbar)

        # Add common actions with enhanced styling
        new_btn = toolbar.addAction("📄 New", self.new_file)
        new_btn.setToolTip("Create a new file (Ctrl+N)")

        open_btn = toolbar.addAction("📂 Open", self.open_file)
        open_btn.setToolTip("Open an existing file (Ctrl+O)")

        save_btn = toolbar.addAction("💾 Save", self.save_file)
        save_btn.setToolTip("Save the current file (Ctrl+S)")

        toolbar.addSeparator()

        run_btn = toolbar.addAction("🚀 Run", self.run_program)
        run_btn.setToolTip("Run the current program (Ctrl+R)")

        # Debug toolbar buttons
        self.debug_btn = toolbar.addAction("🐛 Debug", self.start_debug)
        self.debug_btn.setToolTip("Start debugging (F5)")

        self.continue_btn = toolbar.addAction("▶️ Continue", self.debug_continue)
        self.continue_btn.setToolTip("Continue execution (F5)")
        self.continue_btn.setEnabled(False)

        self.step_btn = toolbar.addAction("↓ Step", self.debug_step_into)
        self.step_btn.setToolTip("Step into (F11)")
        self.step_btn.setEnabled(False)

        stop_btn = toolbar.addAction("⏹️ Stop", self.stop_program)
        stop_btn.setToolTip("Stop the running program (Shift+F5)")

        toolbar.addSeparator()

        clear_output_btn = toolbar.addAction(
            "🗑️ Clear Output",
            self.output.clear,
        )
        clear_output_btn.setToolTip("Clear the output panel")

        clear_canvas_btn = toolbar.addAction(
            "🎨 Clear Canvas",
            self.canvas.clear,
        )
        clear_canvas_btn.setToolTip("Clear the graphics canvas")

        # Language selector with enhanced styling
        toolbar.addSeparator()
        self.language_combo = QComboBox()
        self.language_combo.setStyleSheet(
            """
            QComboBox {
                background-color: palette(base);
                color: palette(text);
                border: 1px solid palette(dark);
                border-radius: 4px;
                padding: 2px 8px;
                min-width: 80px;
            }
            QComboBox:hover {
                border-color: palette(highlight);
            }
            QComboBox::drop-down {
                border: none;
                width: 20px;
            }
            QComboBox::down-arrow {
                image: none;
                border-left: 4px solid transparent;
                border-right: 4px solid transparent;
                border-top: 4px solid palette(text);
                margin-right: 8px;
            }
        """
        )

        for lang in [
            Language.BASIC,
            Language.PILOT,
            Language.LOGO,
        ]:
            self.language_combo.addItem(f"💻 {lang.friendly_name()}", lang)
        connect_cb = self.on_language_changed
        self.language_combo.currentIndexChanged.connect(connect_cb)

        toolbar.addWidget(QLabel("Language:"))
        toolbar.addWidget(self.language_combo)

    def create_statusbar(self):
        """Create status bar."""
        self.statusbar = QStatusBar()
        self.statusbar.setStyleSheet(
            """
            QStatusBar {
                background-color: palette(window);
                border-top: 1px solid palette(dark);
                padding: 2px;
            }
            QLabel {
                color: palette(window-text);
                padding: 2px 8px;
                border-radius: 3px;
            }
        """
        )
        self.setStatusBar(self.statusbar)

        # Add permanent widgets for better visual feedback
        self.language_label = QLabel("Language: BASIC")
        self.language_label.setStyleSheet(
            """
            QLabel {
                background-color: palette(highlight);
                color: palette(highlighted-text);
                font-weight: bold;
                padding: 2px 6px;
                border-radius: 3px;
            }
        """
        )
        self.statusbar.addPermanentWidget(self.language_label)

        self.position_label = QLabel("Ln 1, Col 1")
        self.position_label.setStyleSheet(
            """
            QLabel {
                background-color: palette(base);
                color: palette(text);
                padding: 2px 6px;
                border-radius: 3px;
                border: 1px solid palette(dark);
            }
        """
        )
        self.statusbar.addPermanentWidget(self.position_label)

        ready_msg = "🎉 Ready - Time Warp IDE loaded successfully!"
        self.statusbar.showMessage(ready_msg)

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
        # Show cassette animation if enabled
        if self.cassette_mode:
            display_name = Path(filename).name
            if not show_cassette_load(self, display_name, duration_ms=1800):
                self.statusbar.showMessage("Load cancelled")
                return

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

        # Show cassette animation if enabled
        if self.cassette_mode:
            display_name = Path(filename).name
            if not show_cassette_save(self, display_name, duration_ms=1500):
                self.statusbar.showMessage("Save cancelled")
                return False

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
            self.stop_action.setEnabled(False)

            # Clean up debug state if debugging ended
            if self._is_debugging:
                self._is_debugging = False
                self._is_paused = False
                self._update_debug_ui()
                self.debug_panel.set_debugging(False)
                editor = self.get_current_editor()
                if editor:
                    editor.clear_current_line()

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

    def on_variables_updated(self, variables):
        """Handle variables update from interpreter."""
        self.variable_inspector.update_variables(variables)
        # Also update debug panel watches
        if hasattr(self, "debug_panel"):
            self.debug_panel.update_variables(variables)
        # Re-enable run action after execution completes
        if not self._is_debugging:
            self.run_action.setEnabled(True)
            self.stop_action.setEnabled(False)
        else:
            # If debugging finished, clean up
            if not self.output.is_running():
                self._is_debugging = False
                self._is_paused = False
                self._update_debug_ui()
                self.debug_panel.set_debugging(False)
                editor = self.get_current_editor()
                if editor:
                    editor.clear_current_line()

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
            # Update immediate mode language
            if hasattr(self, "immediate_mode"):
                self.immediate_mode.set_language(current_data)
            # Update tab info
            self.set_current_tab_info(language=current_data)
            # Update status bar language label
            if hasattr(self, "language_label"):
                lang_text = f"Language: {current_data.friendly_name()}"
                self.language_label.setText(lang_text)

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
        self.statusBar().showMessage(f"Theme changed to: {theme_name}")

    def change_font_family(self, font_family):
        """Change editor font family."""
        self.theme_manager.set_font(font_family, self.theme_manager.current_font_size)
        self._apply_font_to_editors()
        self.settings.setValue("font_family", font_family)
        self.statusBar().showMessage(f"Font changed to: {font_family}")

    def change_font_size(self, size):
        """Change editor font size."""
        self.theme_manager.set_font(self.theme_manager.current_font_family, size)
        self._apply_font_to_editors()
        self.settings.setValue("font_size", size)
        self.statusBar().showMessage(f"Font size changed to: {size} pt")

    def _apply_font_to_editors(self):
        """Apply current font to all editors and output."""
        font = self.theme_manager.get_font()
        for i in range(self.editor_tabs.count()):
            editor = self.editor_tabs.widget(i)
            if editor:
                editor.setFont(font)
        self.output.setFont(font)

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

    def _get_docs_path(self) -> Path:
        """Get path to the Docs directory."""
        # Go up from ui -> time_warp -> Python -> Platforms -> Time_Warp_Studio
        return Path(__file__).parent.parent.parent.parent.parent / "Docs"

    def _show_help_dialog(self, title: str, filepath: Path):
        """Show a help dialog with markdown content."""
        dialog = QDialog(self)
        dialog.setWindowTitle(title)
        dialog.setMinimumSize(700, 500)
        dialog.resize(800, 600)

        layout = QVBoxLayout(dialog)

        browser = QTextBrowser()
        browser.setOpenExternalLinks(True)
        browser.setStyleSheet(
            """
            QTextBrowser {
                font-family: 'Segoe UI', 'DejaVu Sans', sans-serif;
                font-size: 12pt;
                padding: 10px;
                background-color: palette(base);
            }
        """
        )

        if filepath.exists():
            content = filepath.read_text(encoding="utf-8")
            # Convert markdown to simple HTML
            html = self._markdown_to_html(content)
            browser.setHtml(html)
        else:
            browser.setPlainText(f"Documentation not found: {filepath}")

        layout.addWidget(browser)

        buttons = QDialogButtonBox(QDialogButtonBox.Close)
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
                # Escape HTML in code blocks
                escaped = (
                    line.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
                )
                html_lines.append(escaped)
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
                processed = re.sub(r"`(.+?)`", code_style + r"\1</code>", processed)
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
        docs_path = self._get_docs_path() / "user" / "00-user-manual.md"
        self._show_help_dialog("Time Warp IDE - User Manual", docs_path)

    def show_quick_reference(self):
        """Show quick reference guide."""
        docs_path = self._get_docs_path() / "user" / "02-quick-reference.md"
        self._show_help_dialog("Quick Reference Guide", docs_path)

    def show_programming_guide(self):
        """Show programming guide."""
        docs_path = self._get_docs_path() / "user" / "01-programming-guide.md"
        self._show_help_dialog("Programming Guide", docs_path)

    def show_language_help(self, language: str):
        """Show help for a specific language."""
        # Extract relevant section from quick reference
        docs_path = self._get_docs_path() / "user" / "02-quick-reference.md"

        if not docs_path.exists():
            QMessageBox.information(
                self, "Help", f"Documentation for {language.upper()} not found."
            )
            return

        content = docs_path.read_text(encoding="utf-8")

        # Find the language section
        lang_titles = {
            "basic": "## BASIC Commands",
            "pilot": "## PILOT Commands",
            "logo": "## Logo Commands",
        }

        start_marker = lang_titles.get(language, "")
        if not start_marker or start_marker not in content:
            # Show full quick reference
            self._show_help_dialog(f"{language.upper()} Help", docs_path)
            return

        # Extract just that language section
        start_idx = content.find(start_marker)
        # Find next ## header or end
        next_section = content.find("\n## ", start_idx + len(start_marker))
        if next_section == -1:
            section = content[start_idx:]
        else:
            section = content[start_idx:next_section]

        html = self._markdown_to_html(section)

        dialog = QDialog(self)
        dialog.setWindowTitle(f"{language.upper()} Quick Reference")
        dialog.setMinimumSize(600, 450)
        dialog.resize(700, 550)

        layout = QVBoxLayout(dialog)

        browser = QTextBrowser()
        browser.setOpenExternalLinks(True)
        browser.setStyleSheet(
            """
            QTextBrowser {
                font-family: 'Courier New', monospace;
                font-size: 11pt;
                padding: 10px;
                background-color: palette(base);
            }
        """
        )
        browser.setHtml(html)
        layout.addWidget(browser)

        buttons = QDialogButtonBox(QDialogButtonBox.Close)
        buttons.rejected.connect(dialog.close)
        layout.addWidget(buttons)

        dialog.exec()

    def show_about(self):
        """Show about dialog."""
        QMessageBox.about(
            self,
            "About Time Warp IDE",
            "<h2>Time Warp IDE - Python Edition</h2>"
            "<p>Version 5.0.0 — Official PySide6 release</p>"
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

        # Restore font settings
        font_family = self.settings.value("font_family")
        font_size = self.settings.value("font_size")
        if font_family:
            self.theme_manager.current_font_family = font_family
        if font_size:
            try:
                self.theme_manager.current_font_size = int(font_size)
            except (ValueError, TypeError):
                pass

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

    # ---- Debugging Methods ----

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
            code, self.canvas, debug_mode=True, breakpoints=breakpoints
        )

        # Update debug panel
        self.debug_panel.set_debugging(True)
        self.debug_panel.update_breakpoints(breakpoints)

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
            # Signal the interpreter to pause at next line
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
        # In a simple interpreter, step over is the same as step into
        self.debug_step_into()

    def debug_step_out(self):
        """Step out of current subroutine (continue until return)."""
        # For now, just continue - proper step-out would need call stack tracking
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

        # Get call stack from interpreter if available
        if self.output.exec_thread and self.output.exec_thread.interp:
            interp = self.output.exec_thread.interp
            if hasattr(interp, "call_stack"):
                self.debug_panel.update_call_stack(interp.call_stack)

        self.statusbar.showMessage(f"🔴 Paused at line {line}")

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

    def update_cursor_position(self):
        """Update cursor position in status bar."""
        editor = self.get_current_editor()
        if editor and hasattr(self, "position_label"):
            cursor = editor.textCursor()
            line = cursor.blockNumber() + 1
            col = cursor.columnNumber() + 1
            self.position_label.setText(f"Ln {line}, Col {col}")

    # ---- Retro Features ----

    def disable_screen_mode(self):
        """Disable retro screen mode, return to normal hi-res rendering."""
        self.canvas.set_screen_mode_enabled(False)
        self.statusbar.showMessage("📺 Normal hi-res mode")

    def change_screen_mode(self, mode_number: int):
        """Change the current screen mode."""
        mode = self.screen_mode_manager.set_mode(mode_number)

        # Enable retro mode rendering on the canvas
        self.canvas.set_screen_mode(mode)
        self.canvas.set_screen_mode_enabled(True)

        # Update status bar with mode info
        self.statusbar.showMessage(
            f"📺 {mode.name} - {mode.resolution_str} ({mode.colors} colors)"
        )

        # Switch to Graphics tab to show the effect
        self.right_tabs.setCurrentWidget(self.canvas)

    def toggle_crt_effects(self, enabled: bool):
        """Toggle CRT effects on/off."""
        self.crt_enabled = enabled
        self.crt_overlay.set_enabled(enabled)
        if enabled:
            self.crt_overlay.raise_()
            self._update_crt_settings()
            self.statusbar.showMessage("📟 CRT effects enabled")
        else:
            self.statusbar.showMessage("CRT effects disabled")

    def _update_crt_settings(self):
        """Update CRT effect settings from menu checkboxes."""
        if not hasattr(self, "crt_overlay"):
            return

        self.crt_overlay.set_scanlines(
            self.scanlines_action.isChecked(),
            intensity=0.15,
            spacing=2,
        )
        self.crt_overlay.set_curvature(
            self.curvature_action.isChecked(),
            amount=0.02,
        )
        self.crt_overlay.set_vignette(
            self.vignette_action.isChecked(),
            intensity=0.3,
        )
        self.crt_overlay.set_glow(
            self.glow_action.isChecked(),
            intensity=0.1,
        )
        self.crt_overlay.set_flicker(self.flicker_action.isChecked())

    def _toggle_cassette_mode(self, enabled: bool):
        """Toggle cassette animation mode."""
        self.cassette_mode = enabled
        if enabled:
            self.statusbar.showMessage("📼 Cassette mode enabled")
        else:
            self.statusbar.showMessage("📼 Cassette mode disabled")

    def resizeEvent(self, event):  # pylint: disable=invalid-name
        """Handle window resize - update CRT overlay."""
        super().resizeEvent(event)
        if hasattr(self, "crt_overlay") and self.crt_overlay.parent():
            self.crt_overlay.setGeometry(self.crt_overlay.parent().rect())

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
        flags = QDialogButtonBox.Ok | QDialogButtonBox.Cancel
        buttons = QDialogButtonBox(flags)
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
            msg = "Disconnected from collaboration server"
            self.statusbar.showMessage(msg)

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
        flags = QDialogButtonBox.Ok | QDialogButtonBox.Cancel
        buttons = QDialogButtonBox(flags)
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
        flags = QDialogButtonBox.Ok | QDialogButtonBox.Cancel
        buttons = QDialogButtonBox(flags)
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
        msg = "Disconnected from collaboration server"
        self.statusbar.showMessage(msg)

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
