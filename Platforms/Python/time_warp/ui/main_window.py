"""Main window for Time Warp Studio.

.. note:: Large chunks of functionality have been moved to mixin classes
   under :mod:`time_warp.ui.mixins` to keep each file focused.  See:

   * :class:`~.mixins.CollaborationMixin`
   * :class:`~.mixins.ClassroomMixin`
   * :class:`~.mixins.DebugMixin`
   * :class:`~.mixins.ExportMixin`
   * :class:`~.mixins.FileOperationsMixin`
   * :class:`~.mixins.HelpDocsMixin`
"""

import logging
import time
from pathlib import Path

# pylint: disable=no-name-in-module
from PySide6.QtCore import QSettings, QSize, Qt, QTimer
from PySide6.QtGui import (
    QAction,
    QActionGroup,
    QColor,
    QFont,
    QKeySequence,
    QTextCharFormat,
    QTextCursor,
)
from PySide6.QtWidgets import (
    QComboBox,
    QDialog,
    QDialogButtonBox,
    QHBoxLayout,
    QLabel,
    QListWidget,
    QMainWindow,
    QMenu,
    QMessageBox,
    QPlainTextEdit,
    QSlider,
    QSplitter,
    QStatusBar,
    QTabWidget,
    QTextBrowser,
    QTextEdit,
    QToolBar,
    QToolButton,
    QVBoxLayout,
    QWidget,
)

from ..core.interpreter import Language
from .tab_state import TabState
from .canvas import TurtleCanvas
from .coach_marks import CoachMarkManager
from .collaboration_client import CollaborationClient
from .command_palette import CommandPalette
from .crt_effect import CRTEffectOverlay
from .debug_panel import DebugPanel
from .editor import CodeEditor
from .feature_integration import FeatureIntegrationManager
from .mixins import (
    ClassroomMixin,
    CollaborationMixin,
    DebugMixin,
    ExportMixin,
    FileOperationsMixin,
    HelpDocsMixin,
)
from .output import ImmediateModePanel, OutputPanelContainer
from .screen_modes import ScreenModeManager
from .themes import ThemeManager
from .variable_inspector import VariableInspector
from ..features.classroom_mode import ClassroomMode
from ..features.autosave_manager import AutosaveManager
from .focus_mode import FocusModeManager
from .onboarding import OnboardingDialog, OnboardingManager
from ..features.examples_browser import ExamplesBrowser
from ..utils.error_hints import get_enhanced_error_message

# Fix: Import CustomUILayouts for custom UI layout management
from .custom_layouts import CustomUILayouts

# pylint: enable=no-name-in-module

logger = logging.getLogger(__name__)


class ResizableTabWidget(QTabWidget):
    """QTabWidget whose minimum/size hints reflect only the current page.

    Qt's default QTabWidget.minimumSizeHint() returns the *maximum* of all
    tab page hints, forcing the window to stay huge even when large pages are
    hidden.  This subclass returns a small fixed minimum so the splitter and
    window can shrink freely; the current page drives the preferred size hint.
    """

    _TAB_BAR_MARGIN = 8  # extra pixels above/below tab bar text

    def minimumSizeHint(self) -> QSize:  # type: ignore[override]
        return QSize(100, 100)

    def sizeHint(self) -> QSize:  # type: ignore[override]
        current = self.currentWidget()
        if current is None:
            return QSize(400, 300)
        bar_h = self.tabBar().sizeHint().height() + self._TAB_BAR_MARGIN
        sh = current.sizeHint()
        w = max(sh.width(), 100) if sh.width() > 0 else 400
        h = (max(sh.height(), 100) if sh.height() > 0 else 300) + bar_h
        return QSize(w, h)


class MainWindow(
    CollaborationMixin,
    ClassroomMixin,
    DebugMixin,
    ExportMixin,
    FileOperationsMixin,
    HelpDocsMixin,
    QMainWindow,
):
    """Main IDE window with editor, output, and canvas.

    This class composes the editor area, output panel, turtle graphics
    canvas and various UI controls such as application menu and toolbar.

    Mixins provide focused groups of functionality:

    * **CollaborationMixin** – real-time collaboration server UI
    * **ClassroomMixin** – presentation mode, bundles, lesson export
    * **DebugMixin** – debugger start/stop/step, breakpoints, timeline
    * **ExportMixin** – PNG/SVG/HTML/PDF export, printing
    * **FileOperationsMixin** – new/open/save/recent-files
    * **HelpDocsMixin** – help dialogs, markdown→HTML, about dialog
    """

    # This class manages a rich UI with many widgets and stateful
    # attributes; allow larger limits for instance attributes and public
    # methods to avoid excessive linter noise.
    # pylint: disable=too-many-instance-attributes,too-many-public-methods

    MAX_SAFE_LINES = 10000

    def __init__(self):
        super().__init__()

        from ..core.config import QSETTINGS_APP, QSETTINGS_ORG

        # Settings for persistence
        self.settings = QSettings(QSETTINGS_ORG, QSETTINGS_APP)

        # Theme manager
        self.theme_manager = ThemeManager()

        # Collaboration client
        self.collaboration_client = CollaborationClient()
        self.collab_disconnect_action = QAction("Disconnect", self)
        self.collab_disconnect_action.setEnabled(False)
        self.setup_collaboration_callbacks()

        # Classroom mode
        self.classroom_mode = ClassroomMode()
        self._classroom_lock_theme = False
        self._pre_presentation_font_size = None
        self._pre_presentation_menu_visible = None
        self._pre_presentation_was_fullscreen = None

        # Current file tracking — single dict of TabState per tab index.
        # Use self._ts(idx) to get-or-create a TabState for any index.
        self._tab_states: dict = {}

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
        self._debug_step_granularity = "line"
        self._last_debug_timeline = None

        # Retro features
        self.screen_mode_manager = ScreenModeManager()
        self.crt_enabled = False

        # Autosave and version history
        self._autosave_manager = AutosaveManager(autosave_interval=300)

        # Focus mode
        self._focus_mode = FocusModeManager(self)

        # Onboarding (first-run wizard)
        self._onboarding_manager = OnboardingManager()

        # Command palette (Ctrl+Shift+P)
        self._command_palette: CommandPalette | None = None

        # Coach mark onboarding tour
        self._coach_marks: CoachMarkManager | None = None

        # Run history (last 10 runs: list of dicts with 'code','language','timestamp')
        self._run_history: list = []

        # Feature integration manager
        self.feature_manager = FeatureIntegrationManager(self)

        # Initialize custom UI layouts manager
        self.ui_layout_manager = CustomUILayouts()

        # Setup UI
        self.setup_ui()
        self.create_toolbar()
        self.create_menus()
        self.create_statusbar()

        # Setup feature panels and integration
        self.feature_manager.setup_features()
        self.output.execution_complete.connect(self.on_execution_complete)
        self._connect_lesson_signals()
        self._connect_lesson_authoring_signals()
        self._connect_learning_hub_signals()
        self._connect_turtle_inspector_signals()
        self._connect_classroom_signals()
        self._connect_reference_signals()

        # Initialise coach-mark tour (shown on first launch after a delay)
        self._coach_marks = CoachMarkManager(self)

        # Set a sensible default size before restoring saved geometry.
        # restoreGeometry() will override this when a previous session exists.
        self.resize(1400, 900)

        # Restore previous state
        self.restore_state()

        # Show first-run onboarding after UI settles
        QTimer.singleShot(600, self._maybe_show_onboarding)

        # Apply theme to current editor
        current_editor = self.get_current_editor()
        if current_editor and hasattr(current_editor, "highlighter"):
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

    def get_current_editor(self):
        """Get the currently active editor."""
        current_index = self.editor_tabs.currentIndex()
        if current_index >= 0:
            return self.editor_tabs.widget(current_index)
        return None

    def _ts(self, idx: int) -> TabState:
        """Get or create the TabState for *idx*."""
        if idx not in self._tab_states:
            self._tab_states[idx] = TabState()
        return self._tab_states[idx]

    def get_current_tab_info(self):
        """Get info for current tab."""
        current_index = self.editor_tabs.currentIndex()
        if current_index >= 0:
            s = self._ts(current_index)
            return {"file": s.file, "modified": s.modified, "language": s.language}
        return {"file": None, "modified": False, "language": Language.BASIC}

    def set_current_tab_info(self, file=None, modified=None, language=None):
        """Set info for current tab."""
        current_index = self.editor_tabs.currentIndex()
        if current_index >= 0:
            s = self._ts(current_index)
            if file is not None:
                s.file = file
            if modified is not None:
                old_modified = s.modified
                s.modified = modified
                # Update tab title dot indicator when modified state changes
                if modified != old_modified:
                    self._update_tab_title_indicator(current_index, modified)
            if language is not None:
                s.language = language
            self.update_title()

    def _update_tab_title_indicator(self, index: int, modified: bool):
        """Add or remove the unsaved dot (●) from the tab title at *index*."""
        if not hasattr(self, "editor_tabs"):
            return
        if index < 0 or index >= self.editor_tabs.count():
            return
        title = self.editor_tabs.tabText(index)
        # Strip any existing indicator
        clean = title.rstrip(" ●")
        new_title = f"{clean} ●" if modified else clean
        self.editor_tabs.setTabText(index, new_title)
        # Colour the tab text red when unsaved
        tab_bar = self.editor_tabs.tabBar()
        if modified:
            tab_bar.setTabTextColor(index, QColor(255, 120, 80))
        else:
            tab_bar.setTabTextColor(index, QColor())  # reset to default

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
        self._tab_states[tab_index] = TabState(file=None, modified=False, language=language)

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
        language = self._ts(current_idx).language
        lang_name = language.name if hasattr(language, "name") else "BASIC"

        dialog = SnippetDialog(lang_name, self)
        if dialog.exec():
            code = dialog.get_selected_code()
            if code:
                ed = self.get_current_editor()
                if ed:
                    ed.insertPlainText(code)

    # -- Export / print methods live in ExportMixin --

    # ---- Focus mode ----

    def _toggle_focus_mode(self, _checked: bool = False):
        """Toggle distraction-free focus mode."""
        self._focus_mode.toggle_focus_mode()
        self._focus_mode_action.setChecked(self._focus_mode.is_focus_mode)

    # ---- Onboarding ----

    def _maybe_show_onboarding(self):
        """Show first-run onboarding wizard if not yet completed."""
        if not self._onboarding_manager.should_show_onboarding():
            return

        try:
            dialog = OnboardingDialog(self)
            dialog.step_completed.connect(self._onboarding_manager.mark_step_completed)
            dialog.exec()
            self._onboarding_manager.record_result(
                dialog.completion_state,
                skip_requested=dialog.should_skip_onboarding(),
            )
        except Exception as exc:  # noqa: BLE001
            logger.warning("Onboarding unavailable for this session: %s", exc)

    # ---- Welcome tab ----

    def _maybe_show_welcome_tab(self):
        """Insert a read-only Welcome tab on first run (or when preference set).

        The tab is inserted at position 0 so it is visible immediately.
        Closing it or creating any new file removes it permanently for the
        current session.
        """
        # Only show when no file was opened via CLI args and
        # the user hasn't disabled the welcome screen.
        show = self.settings.value("show_welcome", True)
        if show in (False, "false", "0", 0):
            return

        welcome = QTextBrowser()
        welcome.setReadOnly(True)
        welcome.setOpenExternalLinks(False)
        welcome.setHtml(self._welcome_html())
        welcome.setStyleSheet("QTextBrowser { background: palette(base); color: palette(text); border: none; }")

        idx = self.editor_tabs.insertTab(0, welcome, "🏠 Welcome")
        self.editor_tabs.setCurrentIndex(idx)
        # Mark the tab so close_tab can clean up without prompting
        self._tab_states[idx] = TabState(file=None, modified=False)

    def _welcome_html(self) -> str:
        """Return the HTML content for the Welcome tab."""
        langs = ", ".join([
            "BASIC", "Logo", "Python", "Pascal", "C", "Forth", "PILOT",
            "Prolog", "Lua", "Scheme", "JavaScript",
            "REXX", "Smalltalk", "HyperTalk", "Haskell",
            "Brainfuck",
            "Ruby", "Erlang", "Rust",
        ])
        return f"""
<html><body style="font-family: 'Segoe UI', sans-serif; margin: 24px; line-height: 1.6;">
<h1 style="color: #bd93f9;">&#127775; Welcome to Time Warp Studio</h1>
<p style="font-size: 14px; color: #f8f8f2;">
  An educational multi-language programming environment for exploring
  <b>21 programming languages</b> side-by-side with live turtle graphics.
</p>
<hr style="border: 1px solid #44475a;"/>

<h2 style="color: #8be9fd;">&#128640; Quick Start</h2>
<ul style="font-size: 13px;">
  <li><b>Ctrl+N</b> — New file &nbsp;|&nbsp; <b>Ctrl+O</b> — Open file</li>
  <li>Pick a language from the toolbar combo, type your code, press <b>Ctrl+R</b>.</li>
  <li>Open <b>🎓 Learning Hub</b> from the toolbar for challenges, remixing, and tutor help.</li>
  <li>Browse ready-made examples via <b>Help → Browse Examples…</b></li>
  <li>Press <b>Ctrl+?</b> to show all keyboard shortcuts.</li>
  <li>Press <b>F1</b> while editing for context-sensitive language help.</li>
</ul>

<h2 style="color: #8be9fd;">&#128187; Supported Languages</h2>
<p style="font-size: 13px; color: #f8f8f2;">{langs}</p>

<h2 style="color: #8be9fd;">&#128736; Features</h2>
<ul style="font-size: 13px;">
  <li>Turtle graphics canvas with zoom, pan, animation playback, and PNG export</li>
  <li>Step-through debugger with breakpoints and variable inspection</li>
  <li>28 editor themes, CRT retro effects, and focus mode</li>
  <li>SQL workbench with transactions and full DDL support</li>
</ul>

<p style="font-size: 11px; color: #6272a4; margin-top: 32px;">
  Close this tab to start coding, or press <b>Ctrl+N</b> to open a blank editor.
  To disable this screen go to <i>Settings → Show Welcome Tab</i>.
</p>
</body></html>"""

    # ---- Examples browser ----

    def _show_examples_browser(self, _checked: bool = False):
        """Show the examples browser dialog."""
        from ..core.config import EXAMPLES_DIR

        examples_dir = EXAMPLES_DIR
        browser = ExamplesBrowser(examples_dir)
        browser.scan_examples()

        dlg = QDialog(self)
        dlg.setWindowTitle("\ud83d\udcda Examples Browser")
        dlg.setMinimumSize(700, 500)
        layout = QVBoxLayout(dlg)

        from PySide6.QtWidgets import QTreeWidget, QTreeWidgetItem, QSplitter, QTextBrowser  # type: ignore[attr-defined]  # noqa: F401

        splitter = QSplitter()

        tree = QTreeWidget()
        tree.setHeaderLabels(["Example", "Difficulty"])
        tree.setMinimumWidth(260)

        # Group by language
        lang_nodes: dict = {}
        for ex in browser.examples:
            lang_name = ex.language.friendly_name()
            if lang_name not in lang_nodes:
                parent = QTreeWidgetItem(tree, [lang_name, ""])
                parent.setExpanded(True)
                lang_nodes[lang_name] = parent
            item = QTreeWidgetItem(
                lang_nodes[lang_name], [ex.title, ex.difficulty.value]
            )
            item.setData(0, Qt.UserRole, ex)
        splitter.addWidget(tree)

        preview = QTextBrowser()
        preview.setFontFamily("Courier New")
        splitter.addWidget(preview)
        splitter.setSizes([260, 440])
        layout.addWidget(splitter)

        from PySide6.QtWidgets import (
            QDialogButtonBox,
        )  # noqa: F811  # type: ignore[attr-defined]

        btns = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Open
            | QDialogButtonBox.StandardButton.Cancel
        )
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        layout.addWidget(btns)

        def on_item_clicked(item, _column):
            ex = item.data(0, Qt.UserRole)
            if ex:
                preview.setPlainText(ex.code)

        tree.itemClicked.connect(on_item_clicked)

        if dlg.exec() == QDialog.Accepted:
            selected = tree.currentItem()
            if selected:
                ex = selected.data(0, Qt.UserRole)
                if ex:
                    self.create_new_tab()
                    editor = self.get_current_editor()
                    if editor:
                        editor.setPlainText(ex.code)
                        lang = ex.language
                        editor.set_language(lang)
                        idx = self.editor_tabs.currentIndex()
                        self._ts(idx).language = lang
                        self.editor_tabs.setTabText(idx, ex.title)
                        self.statusbar.showMessage(f"Opened example: {ex.title}", 3000)

    # ---- Language comparator ----

    def _show_language_comparator(self, _checked: bool = False):
        """Show side-by-side language comparison dialog."""
        from PySide6.QtWidgets import (  # type: ignore[attr-defined]
            QPushButton as _QPB,
            QPlainTextEdit as _PTE,
            QDialogButtonBox as _DBB,
        )

        dlg = QDialog(self)
        dlg.setWindowTitle("\u2696\ufe0f Language Comparator")
        dlg.setMinimumSize(800, 540)
        layout = QVBoxLayout(dlg)

        # Two-column code editors (plain text)
        top = QHBoxLayout()
        lang_options = [lang.friendly_name() for lang in Language]

        left_vbox = QVBoxLayout()
        lang1_combo = QComboBox()
        lang1_combo.addItems(lang_options)
        lang1_combo.setCurrentText("BASIC")
        left_vbox.addWidget(lang1_combo)
        code1_edit = _PTE()
        code1_edit.setPlaceholderText("Enter code for language 1\u2026")
        left_vbox.addWidget(code1_edit)
        top.addLayout(left_vbox)

        right_vbox = QVBoxLayout()
        lang2_combo = QComboBox()
        lang2_combo.addItems(lang_options)
        lang2_combo.setCurrentText("Python")
        right_vbox.addWidget(lang2_combo)
        code2_edit = _PTE()
        code2_edit.setPlaceholderText("Enter code for language 2\u2026")
        right_vbox.addWidget(code2_edit)
        top.addLayout(right_vbox)
        layout.addLayout(top)

        run_btn = _QPB("\u25b6 Compare")
        layout.addWidget(run_btn)

        result_view = QTextBrowser()
        result_view.setFontFamily("Courier New")
        layout.addWidget(result_view)

        close_btns = _DBB(_DBB.StandardButton.Close)
        close_btns.rejected.connect(dlg.reject)
        layout.addWidget(close_btns)

        def _run_comparison():
            l1 = Language.__members__.get(
                lang1_combo.currentText().upper(), Language.BASIC
            )
            l2 = Language.__members__.get(
                lang2_combo.currentText().upper(), Language.BASIC
            )
            try:
                from ..core.interpreter import Interpreter as _Interp
                from ..graphics.turtle_state import TurtleState as _TS

                def _run_one(lang, code):
                    interp = _Interp()
                    interp.load_program(code, lang)
                    out = interp.execute(_TS())
                    return "\n".join(out) if out else "(no output)"

                out1 = _run_one(l1, code1_edit.toPlainText())
                out2 = _run_one(l2, code2_edit.toPlainText())

                sep = "=" * 50
                result_view.setPlainText(
                    f"{sep}\n{l1.friendly_name()} OUTPUT\n{sep}\n{out1}\n\n"
                    f"{sep}\n{l2.friendly_name()} OUTPUT\n{sep}\n{out2}"
                )
            except Exception as exc:  # pylint: disable=broad-except
                result_view.setPlainText(f"\u274c Comparison failed: {exc}")

        run_btn.clicked.connect(_run_comparison)
        dlg.exec()

    def _format_code(self, _checked: bool = False):
        """Format the current code."""
        # pylint: disable=import-outside-toplevel
        from ..utils.code_formatter import get_formatter

        ed = self.get_current_editor()
        if not ed:
            return

        current_idx = self.editor_tabs.currentIndex()
        language = self._ts(current_idx).language
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
                msg = f"⏱️ Profiling stopped: " f"{session.total_lines_executed} lines"
                self.statusbar.showMessage(msg, 3000)

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
        from ..features.music import get_sound_effects

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
            new_states = {}

            new_idx = 0
            for i in range(self.editor_tabs.count()):
                if i == index:
                    continue
                new_states[new_idx] = self._tab_states.get(i, TabState())
                new_idx += 1

            # Remove the tab widget. Block signals so the currentChanged
            # signal doesn't fire while _tab_states is being rebuilt,
            # which would cause on_tab_changed to read a stale index.
            self.editor_tabs.blockSignals(True)
            self.editor_tabs.removeTab(index)
            self.editor_tabs.blockSignals(False)

            # Replace the internal map with the reindexed version
            self._tab_states = new_states

            if new_idx == 0:
                # Ensure we always have at least one editor open
                self.new_file()

    def on_tab_changed(self, index):
        """Handle tab change."""
        if index >= 0:
            # Guard against stale index arriving after a tab was closed
            if index not in self._tab_states:
                return
            language = self._ts(index).language

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
                self.language_label.setText(self._lang_badge_text(language))

            # Update title
            self.update_title()

    def on_language_changed(self, index=None):
        """Handle language selection change."""
        if index is None and hasattr(self, "language_combo"):
            index = self.language_combo.currentIndex()

        if index is None or index < 0 or not hasattr(self, "language_combo"):
            return

        # Get selected language from combo data
        language = self.language_combo.itemData(index)
        if not language:
            return

        # Update current tab's language
        current_index = self.editor_tabs.currentIndex()
        if current_index >= 0:
            # Update editor syntax highlighting
            editor = self.editor_tabs.currentWidget()
            if isinstance(editor, CodeEditor):
                editor.set_language(language)

            # Update output panel context
            if hasattr(self, "output"):
                self.output.set_language(language)

            # Update immediate mode language
            if hasattr(self, "immediate_mode"):
                self.immediate_mode.set_language(language)

            # Update tab info (handles tab_languages and title update)
            self.set_current_tab_info(language=language)

            # Update status bar
            if hasattr(self, "language_label"):
                self.language_label.setText(self._lang_badge_text(language))

    def check_save_changes_for_tab(self, tab_index):
        """Check if tab has unsaved changes and prompt to save."""
        if self._ts(tab_index).modified:
            filename = self._ts(tab_index).file or "Untitled"
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
        self.setWindowTitle("🎨 Time Warp Studio v10.0.0")
        self.setMinimumSize(900, 600)

        # Set main window style
        self.setStyleSheet("""
            QMainWindow {
                background-color: palette(window);
            }

            /* Tab styling */
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
                min-height: 24px;
            }
            QTabBar::tab:selected {
                background-color: palette(highlight);
                color: palette(highlighted-text);
                font-weight: bold;
            }
            QTabBar::tab:hover:!selected {
                background-color: rgba(0, 0, 0, 0.05);
            }

            /* Splitter styling */
            QSplitter::handle {
                background-color: palette(dark);
                border: none;
                width: 6px;
                height: 6px;
            }
            QSplitter::handle:hover {
                background-color: palette(highlight);
            }

            /* Button styling for all buttons */
            QPushButton {
                background-color: palette(button);
                color: palette(button-text);
                border: 2px solid palette(dark);
                border-radius: 6px;
                padding: 6px 14px;
                font-weight: bold;
                min-height: 28px;
                min-width: 80px;
            }
            QPushButton:hover {
                background-color: palette(highlight);
                color: palette(highlighted-text);
                border: 2px solid palette(highlight);
            }
            QPushButton:pressed {
                background-color: palette(dark);
                border: 2px solid palette(highlight);
            }
            QPushButton:disabled {
                color: palette(dark);
                background-color: palette(window);
                border: 2px solid palette(dark);
            }

            /* Dialog button styling */
            QDialogButtonBox QPushButton {
                min-width: 70px;
            }
        """)

        # Central widget with splitter
        central = QWidget()
        self.setCentralWidget(central)
        layout = QVBoxLayout(central)
        layout.setContentsMargins(5, 5, 5, 5)

        # Main splitter (horizontal) - left side vs right side
        splitter = QSplitter(Qt.Horizontal)
        splitter.setStyleSheet("""
            QSplitter {
                background-color: palette(window);
            }
        """)
        # Prevent either side from being collapsed to zero during resize
        splitter.setChildrenCollapsible(False)
        # Wider grab handle so it's easy to drag with a mouse
        splitter.setHandleWidth(6)

        # Left side: Vertical splitter for editor + immediate mode
        left_splitter = QSplitter(Qt.Vertical)
        # Prevent the REPL bar from being dragged out of sight
        left_splitter.setChildrenCollapsible(False)
        left_splitter.setHandleWidth(6)

        # Editor tabs
        self.editor_tabs = ResizableTabWidget()
        self.editor_tabs.setTabsClosable(True)
        self.editor_tabs.tabCloseRequested.connect(self.close_tab)
        self.editor_tabs.currentChanged.connect(self.on_tab_changed)
        self.editor_tabs.setAccessibleName("Editor Tabs")
        self.editor_tabs.setAccessibleDescription("Code editor tab group")
        self.editor_tabs.setStyleSheet("""
            QTabWidget {
                background-color: palette(base);
                border: 1px solid palette(dark);
                border-radius: 4px;
            }
        """)

        # Create initial tab + welcome page
        self.new_file()
        self._maybe_show_welcome_tab()

        left_splitter.addWidget(self.editor_tabs)
        # Editor expands, REPL bar stays fixed-ish
        left_splitter.setStretchFactor(0, 1)

        # Immediate mode panel (REPL) - below editor
        self.immediate_mode = ImmediateModePanel(self)
        self.immediate_mode.setAccessibleName("REPL Command Line")
        self.immediate_mode.setAccessibleDescription("Interactive command prompt for immediate code execution")
        # Hard floor so the REPL bar can never be dragged to nothing
        self.immediate_mode.setMinimumHeight(44)
        left_splitter.addWidget(self.immediate_mode)
        # REPL bar does not steal space on resize
        left_splitter.setStretchFactor(1, 0)

        # Set left splitter sizes (85% editor, 15% immediate mode)
        left_splitter.setSizes([550, 50])

        splitter.addWidget(left_splitter)

        # Right side: Tabs for Output and Canvas
        self.right_tabs = ResizableTabWidget()
        self.right_tabs.setStyleSheet("""
            QTabWidget {
                background-color: palette(base);
                border: 1px solid palette(dark);
                border-radius: 4px;
            }
        """)

        # Output panel
        _output_container = OutputPanelContainer(self)
        self.output = _output_container.output_panel
        self.output.setAccessibleName("Program Output")
        self.output.setAccessibleDescription("Displays program execution output")
        # Connect output panel signals
        self.output.variables_updated.connect(self.on_variables_updated)
        self.output.execution_stats.connect(self.on_execution_stats)
        self.output.error_occurred.connect(self.on_execution_error)

        # Turtle canvas
        self.canvas = TurtleCanvas(self)
        self.canvas.setAccessibleName("Turtle Graphics Canvas")
        self.canvas.setAccessibleDescription("Drawing surface for turtle graphics output")

        # Combine Output and Graphics in a persistent vertical split so both
        # panels are always visible — no tab-flipping when a program draws.
        self.output_canvas_pane = QSplitter(Qt.Vertical)
        self.output_canvas_pane.setChildrenCollapsible(False)
        self.output_canvas_pane.setHandleWidth(5)

        # Tabbed output area: Console / Errors / Turtle Log
        self.output_sub_tabs = QTabWidget()
        self.output_sub_tabs.setTabPosition(QTabWidget.TabPosition.South)
        self.output_sub_tabs.setDocumentMode(True)
        self.output_sub_tabs.setStyleSheet("""
            QTabBar::tab { padding: 4px 10px; min-height: 20px; }
        """)
        self.output_sub_tabs.addTab(_output_container, "📝 Console")

        # Errors sub-tab
        self.errors_log = QPlainTextEdit()
        self.errors_log.setReadOnly(True)
        self.errors_log.setFont(QFont("Courier New", 10))
        self.errors_log.setAccessibleName("Error Log")
        self.errors_log.setPlaceholderText("Errors and warnings appear here…")
        self.output_sub_tabs.addTab(self.errors_log, "❌ Errors")

        # Turtle log sub-tab
        self.turtle_log = QPlainTextEdit()
        self.turtle_log.setReadOnly(True)
        self.turtle_log.setFont(QFont("Courier New", 10))
        self.turtle_log.setAccessibleName("Turtle Command Log")
        self.turtle_log.setPlaceholderText("Turtle 🐢 commands appear here…")
        self.output_sub_tabs.addTab(self.turtle_log, "🐢 Turtle Log")

        self.output_canvas_pane.addWidget(self.output_sub_tabs)
        self.output_canvas_pane.addWidget(self.canvas)
        self.output_canvas_pane.setSizes([200, 300])

        # Route streamed output to the appropriate sub-tabs
        self.output.output_streamed.connect(self._route_output_to_subtabs)

        self.right_tabs.addTab(self.output_canvas_pane, "📝 Output  🎨 Graphics")

        # Connect immediate mode to output panel and canvas
        self.immediate_mode.set_canvas(self.canvas)
        self.immediate_mode.set_output_panel(self.output)
        self.immediate_mode.variables_updated.connect(self.on_variables_updated)

        # Variable inspector
        self.variable_inspector = VariableInspector(self)
        self.variable_inspector.setAccessibleName("Variable Inspector")
        self.right_tabs.addTab(self.variable_inspector, "🔍 Variables")

        # Debug panel
        self.debug_panel = DebugPanel(self)
        self.debug_panel.setAccessibleName("Debug Panel")
        self.right_tabs.addTab(self.debug_panel, "🐛 Debug")
        self._connect_debug_signals()


        splitter.addWidget(self.right_tabs)
        # Left (editor) gets 3 parts, right (output/canvas) gets 2 parts on resize
        splitter.setStretchFactor(0, 3)
        splitter.setStretchFactor(1, 2)

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

        open_btn = self.toolbar.addAction("📂 Open", self.open_file)
        open_btn.setToolTip("Open an existing file (Ctrl+O)")
        open_btn.setStatusTip("Open a file from disk")

        learn_btn = self.toolbar.addAction(
            "🎓 Learn",
            lambda: self.feature_manager.toggle_feature_panel("learning_hub", visible=True),
        )
        learn_btn.setToolTip("Open the Learning Hub")
        learn_btn.setStatusTip("Launch lessons, challenges, and tutor tools")

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

        export_html_action = QAction("Export Session as &HTML...", self)
        export_html_action.setShortcut("Ctrl+Shift+H")
        export_html_action.triggered.connect(self._export_session_html)
        export_menu.addAction(export_html_action)

        file_menu.addSeparator()

        # Print actions
        print_code_action = QAction("&Print Code...", self)
        print_code_action.setShortcut("Ctrl+P")
        print_code_action.triggered.connect(self._print_code)
        file_menu.addAction(print_code_action)

        print_graphics_action = QAction("Print &Graphics...", self)
        print_graphics_action.triggered.connect(self._print_graphics)
        file_menu.addAction(print_graphics_action)

        export_pdf_action = QAction("Export Code as &PDF...", self)
        export_pdf_action.triggered.connect(self._export_code_pdf)
        file_menu.addAction(export_pdf_action)

        version_history_action = QAction("&Version History...", self)
        version_history_action.setShortcut("Ctrl+H")
        version_history_action.triggered.connect(self._show_version_history)
        file_menu.addAction(version_history_action)

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
        self.stop_action.setShortcut("Ctrl+Shift+F5")
        self.stop_action.setEnabled(False)
        self.stop_action.triggered.connect(self.stop_program)
        run_menu.addAction(self.stop_action)

        self.run_selection_action = QAction("Run &Selection", self)
        self.run_selection_action.setShortcut("Ctrl+Shift+R")
        self.run_selection_action.setToolTip(
            "Run the currently selected text (Ctrl+Shift+R)"
        )
        self.run_selection_action.triggered.connect(self.run_selection)
        run_menu.addAction(self.run_selection_action)

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
        self.debug_continue_action.setShortcut("Ctrl+F5")
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

        # Theme preview dialog
        theme_preview_action = QAction("🎨 &Preview Themes…", self)
        theme_preview_action.setStatusTip("Browse and preview all available themes")
        theme_preview_action.triggered.connect(self._show_theme_preview_dialog)
        theme_menu.addAction(theme_preview_action)
        theme_menu.addSeparator()

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
                lambda checked, family=font_family: (self.change_font_family(family))
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

        crt_menu.addSeparator()
        crt_settings_action = QAction("⚙️ CRT &Settings…", self)
        crt_settings_action.triggered.connect(self._show_crt_settings)
        crt_menu.addAction(crt_settings_action)

        view_menu.addSeparator()

        # Focus mode
        self._focus_mode_action = QAction("🎯 &Focus Mode", self)
        self._focus_mode_action.setShortcut("Ctrl+Shift+F11")
        self._focus_mode_action.setCheckable(True)
        self._focus_mode_action.triggered.connect(self._toggle_focus_mode)
        view_menu.addAction(self._focus_mode_action)

        view_menu.addSeparator()

        # Command palette
        cmd_palette_action = QAction("🎩 &Command Palette", self)
        cmd_palette_action.setShortcut("Ctrl+Shift+P")
        cmd_palette_action.setStatusTip("Open command palette to find any action")
        cmd_palette_action.triggered.connect(self._show_command_palette)
        view_menu.addAction(cmd_palette_action)

        # Language picker shortcut
        lang_picker_action = QAction("💻 &Language Picker", self)
        lang_picker_action.setShortcut("Ctrl+Shift+L")
        lang_picker_action.setStatusTip("Switch the current editor language")
        lang_picker_action.triggered.connect(self._show_language_picker)
        view_menu.addAction(lang_picker_action)

        view_menu.addSeparator()

        # Split editor panes
        split_action = QAction("⬜ Split &Editor Right", self)
        split_action.setShortcut("Ctrl+\\")
        split_action.setStatusTip("Open a second editor pane side-by-side")
        split_action.triggered.connect(self._split_editor_right)
        view_menu.addAction(split_action)

        # Minimap toggle
        self._minimap_action = QAction("🗺 Show &Minimap", self)
        self._minimap_action.setCheckable(True)
        self._minimap_action.setChecked(False)
        self._minimap_action.setStatusTip(
            "Toggle the code minimap on the right edge of the editor"
        )
        self._minimap_action.triggered.connect(self._toggle_minimap)
        view_menu.addAction(self._minimap_action)

        # Debugging UI removed — this distribution exposes Run/Stop only.

        # Tools menu
        tools_menu = menubar.addMenu("&Tools")

        tools_menu.addSeparator()

        compare_action = QAction("⚖️ &Compare Languages...", self)
        compare_action.triggered.connect(self._show_language_comparator)
        tools_menu.addAction(compare_action)

        tools_menu.addSeparator()

        format_tool_action = QAction("🔧 &Format Code", self)
        format_tool_action.setShortcut("Ctrl+Alt+F")
        format_tool_action.triggered.connect(self._format_code)
        tools_menu.addAction(format_tool_action)

        # Help menu (last menu item)
        help_menu = menubar.addMenu("&Help")

        # Documentation
        user_manual_action = QAction("&User Manual", self)
        user_manual_action.triggered.connect(self.show_user_manual)
        help_menu.addAction(user_manual_action)

        # F1 → context-sensitive language help
        f1_action = QAction("&Context Help (F1)", self)
        f1_action.setShortcut("F1")
        f1_action.triggered.connect(self._show_contextual_help)
        help_menu.addAction(f1_action)

        quick_ref_action = QAction("&Quick Reference", self)
        quick_ref_action.triggered.connect(self.show_quick_reference)
        help_menu.addAction(quick_ref_action)

        doc_index_action = QAction("Documentation &Index", self)
        doc_index_action.triggered.connect(self.show_doc_index)
        help_menu.addAction(doc_index_action)

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

        lang_help_menu.addSeparator()

        # All remaining languages (alphabetical)
        for lang_name, lang_key in [
            ("Brainfuck", "brainfuck"),
            ("C", "c"),
            ("Erlang", "erlang"),
            ("Forth", "forth"),
            ("Haskell", "haskell"),
            ("HyperTalk", "hypertalk"),
            ("JavaScript", "javascript"),
            ("Lua", "lua"),
            ("Pascal", "pascal"),
            ("Prolog", "prolog"),
            ("Python", "python"),
            ("REXX", "rexx"),
            ("Ruby", "ruby"),
            ("Rust", "rust"),
            ("Scheme", "scheme"),
            ("Smalltalk", "smalltalk"),
        ]:
            action = QAction(f"{lang_name} Reference", self)
            action.triggered.connect(
                lambda checked=False, k=lang_key: self.show_language_help(k)
            )
            lang_help_menu.addAction(action)

        help_menu.addSeparator()

        examples_action = QAction("📚 Browse &Examples...", self)
        examples_action.triggered.connect(self._show_examples_browser)
        help_menu.addAction(examples_action)

        help_menu.addSeparator()

        about_action = QAction("&About Time Warp Studio", self)
        about_action.triggered.connect(self.show_about)
        help_menu.addAction(about_action)

        help_menu.addSeparator()

        tour_action = QAction("🎓 &Start Guided Tour", self)
        tour_action.setStatusTip("Replay the interactive coach-mark walkthrough")
        tour_action.triggered.connect(self._start_coach_marks_tour)
        help_menu.addAction(tour_action)

        help_menu.addSeparator()

        shortcuts_action = QAction("⌨️ &Keyboard Shortcuts…", self)
        shortcuts_action.setShortcut("Ctrl+?")
        shortcuts_action.setStatusTip("Show all keyboard shortcuts in a searchable table")
        shortcuts_action.triggered.connect(self._show_keyboard_shortcuts)
        help_menu.addAction(shortcuts_action)

        # Collaboration features are disabled in this distribution. The
        # Collaboration menu is omitted to avoid exposing disabled
        # real-time collaboration UI.

    def create_toolbar(self):
        """Create toolbar with enhanced button styling."""
        self.toolbar = QToolBar("Main Toolbar")
        self.toolbar.setObjectName("MainToolbar")  # Avoid Qt warning
        self.toolbar.setMovable(False)
        self.toolbar.setToolButtonStyle(Qt.ToolButtonTextBesideIcon)
        self.toolbar.setIconSize(self.toolbar.iconSize())  # Use default icon size
        # Toolbar height for better button appearance
        self.toolbar.setMinimumHeight(44)
        self.toolbar.setStyleSheet("""
            QToolBar {
                background-color: palette(window);
                border-bottom: 2px solid palette(highlight);
                padding: 4px;
                spacing: 8px;
            }
            QToolButton {
                background-color: transparent;
                border: 1px solid transparent;
                border-radius: 5px;
                padding: 6px 10px;
                margin: 2px;
                color: palette(window-text);
                font-weight: bold;
                min-width: 70px;
                min-height: 28px;
            }
            QToolButton:hover {
                background-color: palette(highlight);
                color: palette(highlighted-text);
                border: 1px solid palette(highlight);
                border-radius: 5px;
            }
            QToolButton:pressed {
                background-color: palette(dark);
                border: 1px solid palette(highlight);
            }
            QToolButton:disabled {
                color: palette(dark);
                background-color: transparent;
            }
        """)
        self.addToolBar(self.toolbar)

        # Add common actions with enhanced styling and better organization
        # File operations group
        new_btn = self.toolbar.addAction("📄 New", self.new_file)
        new_btn.setToolTip("Create a new file (Ctrl+N)")
        new_btn.setStatusTip("Create a new editor tab")

        open_btn = self.toolbar.addAction("📂 Open", self.open_file)
        open_btn.setToolTip("Open an existing file (Ctrl+O)")
        open_btn.setStatusTip("Open a file from disk")

        save_btn = self.toolbar.addAction("💾 Save", self.save_file)
        save_btn.setToolTip("Save the current file (Ctrl+S)")
        save_btn.setStatusTip("Save changes to disk")

        self.toolbar.addSeparator()

        # Language selector
        self.toolbar.addWidget(QLabel(" Language: "))
        self.language_combo = QComboBox()
        tooltip_text = "Select programming language for current tab"
        self.language_combo.setToolTip(tooltip_text)
        self.language_combo.setStatusTip(
            "Change the syntax highlighting and execution engine"
        )
        self.language_combo.setStyleSheet("""
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
        """)

        # Populate combo
        for lang in Language:
            self.language_combo.addItem(f"💻 {lang.friendly_name()}", lang)

        self.language_combo.currentIndexChanged.connect(self.on_language_changed)
        self.toolbar.addWidget(self.language_combo)

        self.toolbar.addSeparator()

        # Execution controls group
        # Run button with history dropdown
        self._run_tool_btn = QToolButton()
        self._run_tool_btn.setText("🚀 Run")
        self._run_tool_btn.setToolTip(
            "Run the current program (Ctrl+R)\nClick ▼ for run history"
        )
        self._run_tool_btn.setStatusTip("Execute the code in the current editor")
        self._run_tool_btn.setPopupMode(QToolButton.ToolButtonPopupMode.MenuButtonPopup)
        self._run_tool_btn.setStyleSheet("""
            QToolButton {
                background-color: transparent;
                border: 1px solid transparent;
                border-radius: 5px;
                padding: 6px 10px;
                margin: 2px;
                color: palette(window-text);
                font-weight: bold;
                min-width: 70px;
                min-height: 28px;
            }
            QToolButton:hover {
                background-color: palette(highlight);
                color: palette(highlighted-text);
                border: 1px solid palette(highlight);
                border-radius: 5px;
            }
            QToolButton::menu-button {
                border-left: 1px solid palette(dark);
                width: 14px;
            }
        """)
        self._run_history_menu = QMenu(self)
        self._run_tool_btn.setMenu(self._run_history_menu)
        self._run_tool_btn.clicked.connect(self.run_program)
        # Populate with persisted history
        self._populate_run_history_menu()
        self.toolbar.addWidget(self._run_tool_btn)

        # Debug toolbar buttons
        self.debug_btn = self.toolbar.addAction("🐛 Debug", self.start_debug)
        self.debug_btn.setToolTip("Start debugging (F5)")
        self.debug_btn.setStatusTip("Start debugging with breakpoints")

        self.continue_btn = self.toolbar.addAction("▶️ Continue", self.debug_continue)
        self.continue_btn.setToolTip("Continue execution (F5)")
        self.continue_btn.setStatusTip("Resume execution from breakpoint")
        self.continue_btn.setEnabled(False)

        self.step_btn = self.toolbar.addAction("↓ Step", self.debug_step_into)
        self.step_btn.setToolTip("Step into (F11)")
        self.step_btn.setStatusTip("Execute one line and enter functions")
        self.step_btn.setEnabled(False)

        stop_btn = self.toolbar.addAction("⏹️ Stop", self.stop_program)
        stop_btn.setToolTip("Stop the running program (Shift+F5)")
        stop_btn.setStatusTip("Interrupt execution")

        snippet_btn = self.toolbar.addAction("📋 Snippets", self._show_snippet_dialog)
        snippet_btn.setToolTip("Browse and insert code snippets (Ctrl+Shift+I)")
        snippet_btn.setStatusTip("Open the per-language snippet library")

        self.toolbar.addSeparator()

        # Canvas/Output controls group
        clear_output_btn = self.toolbar.addAction(
            "🗑️ Clear",
            self._clear_all_output,
        )
        clear_output_btn.setToolTip("Clear the output panel")
        clear_output_btn.setStatusTip("Clear all output text")

        clear_canvas_btn = self.toolbar.addAction(
            "🎨 Canvas",
            self.canvas.clear,
        )
        clear_canvas_btn.setToolTip("Clear the graphics canvas")
        clear_canvas_btn.setStatusTip("Clear all graphics drawings")

        self.toolbar.addSeparator()

        # Database / SQL group
        sql_btn = self.toolbar.addAction("🗄 SQL", self._show_sql_workbench)
        sql_btn.setToolTip("Open SQL Workbench (Ctrl+Shift+Q)")
        sql_btn.setStatusTip("Open the embedded SQL Server 2000 workbench")

        self.toolbar.addSeparator()

        # Accessibility preset — one click applies large font + high-contrast theme
        access_btn = self.toolbar.addAction("♿ A+", self._apply_accessibility_preset)
        access_btn.setToolTip(
            "Accessibility Preset\n"
            "Applies 18pt font, high-contrast theme, and disables CRT effects"
        )
        access_btn.setStatusTip("Apply large-print accessible theme")

        self.toolbar.addSeparator()

        # ── Speed throttle ────────────────────────────────────────────────
        # A compact slider that adds a per-output-line delay so educational
        # demos can be slowed down to make each step visible.
        self.toolbar.addWidget(QLabel(" 🐢 Speed: "))
        self._speed_slider = QSlider(Qt.Horizontal)
        self._speed_slider.setRange(0, 500)
        self._speed_slider.setValue(0)
        self._speed_slider.setFixedWidth(100)
        self._speed_slider.setToolTip(
            "Execution speed throttle\n"
            "0 = full speed  |  500 = 500 ms per output line\n"
            "Drag right to slow the program down"
        )
        self._speed_slider.setStatusTip(
            "Slow execution down — useful for step-by-step educational demos"
        )
        self._speed_label = QLabel("0 ms")
        self._speed_label.setFixedWidth(48)

        def _on_speed_changed(val: int) -> None:
            self._speed_label.setText(f"{val} ms")
            self.output.set_step_delay(val)

        self._speed_slider.valueChanged.connect(_on_speed_changed)
        self.toolbar.addWidget(self._speed_slider)
        self.toolbar.addWidget(self._speed_label)

    # Language name → status-bar emoji
    _LANG_EMOJI: dict = {
        "Logo": "🐢",
        "BASIC": "🔢",
        "PILOT": "✈️",
        "Pascal": "🏗️",
        "Forth": "📚",
        "C": "⚙️",
        "Prolog": "🧠",
        "Python": "🐍",
        "Haskell": "λ",
        "Lua": "🌙",
        "Scheme": "λ",
        "Brainfuck": "🧨",
        "JavaScript": "🌐",
        "REXX": "📜",
        "Smalltalk": "💬",
        "HyperTalk": "💡",
    }

    def _lang_badge_text(self, language) -> str:
        """Return a compact emoji + name string for the status bar badge."""
        name = (
            language.friendly_name()
            if hasattr(language, "friendly_name")
            else str(language)
        )
        emoji = self._LANG_EMOJI.get(name, "📝")
        return f"{emoji} {name}"

    def create_statusbar(self):
        """Create status bar."""
        self.statusbar = QStatusBar()
        self.statusbar.setStyleSheet("""
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
        """)
        self.setStatusBar(self.statusbar)

        # Add permanent widgets for better visual feedback
        self.language_label = QLabel("🔢 BASIC")
        self.language_label.setStyleSheet("""
            QLabel {
                background-color: palette(highlight);
                color: palette(highlighted-text);
                font-weight: bold;
                padding: 2px 6px;
                border-radius: 3px;
            }
        """)
        self.statusbar.addPermanentWidget(self.language_label)

        self.position_label = QLabel("Ln 1, Col 1")
        self.position_label.setStyleSheet("""
            QLabel {
                background-color: palette(base);
                color: palette(text);
                padding: 2px 6px;
                border-radius: 3px;
                border: 1px solid palette(dark);
            }
        """)
        self.statusbar.addPermanentWidget(self.position_label)

        # Breadcrumb label: language > context hint
        self.breadcrumb_label = QLabel("")
        self.breadcrumb_label.setStyleSheet("""
            QLabel {
                color: palette(shadow);
                padding: 2px 6px;
                font-size: 11px;
                font-style: italic;
            }
        """)
        self.statusbar.addWidget(self.breadcrumb_label)

        self.sql_status_label = QLabel("🗄 SQL: offline")
        self.sql_status_label.setToolTip("SQL Server 2000 — click to open workbench")
        self.sql_status_label.setStyleSheet("""
            QLabel {
                background-color: palette(base);
                color: palette(text);
                padding: 2px 6px;
                border-radius: 3px;
                border: 1px solid palette(dark);
                font-size: 11px;
            }
        """)
        self.sql_status_label.mousePressEvent = lambda _: self._show_sql_workbench()
        self.statusbar.addPermanentWidget(self.sql_status_label)

        ready_msg = "🎉 Ready - Time Warp Studio loaded successfully!"
        self.statusbar.showMessage(ready_msg)

    # -- File I/O methods (new_file, open_file, load_file, save_file, etc.)
    # -- live in FileOperationsMixin --

    def check_execution_complete(self):
        """Check if execution is complete."""
        if self.output.is_running():
            QTimer.singleShot(100, self.check_execution_complete)
        else:
            self.run_action.setEnabled(True)
            self.stop_action.setEnabled(False)

            # Update tab run indicator for the current tab
            current_idx = self.editor_tabs.currentIndex()
            self._set_tab_run_indicator(current_idx, "done")

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

    def _set_tab_run_indicator(self, idx: int, state: str) -> None:
        """Update the tab title badge to reflect execution state.

        Args:
            idx:   Tab index.
            state: ``"running"`` | ``"done"`` | ``"error"`` | ``"clear"``.
        """
        if not hasattr(self, "editor_tabs") or idx < 0:
            return
        if idx >= self.editor_tabs.count():
            return
        s = self._ts(idx)
        title = self.editor_tabs.tabText(idx)
        # Strip any existing indicator prefix (emoji up to first space)
        for prefix in ("⚙️ ", "✅ ", "❌ "):
            if title.startswith(prefix):
                title = title[len(prefix):]
                break
        if state == "running":
            s.running = True
            new_title = f"⚙️ {title}"
        elif state == "done":
            s.running = False
            new_title = f"✅ {title}"
        elif state == "error":
            s.running = False
            new_title = f"❌ {title}"
        else:
            s.running = False
            new_title = title
        self.editor_tabs.setTabText(idx, new_title)
        # Clear the done/error indicator after 4 s to avoid clutter
        if state in ("done", "error"):
            QTimer.singleShot(4000, lambda: self._set_tab_run_indicator(idx, "clear"))

    def run_selection(self):
        """Run only the currently selected text in the editor."""
        editor = self.get_current_editor()
        if not editor:
            return

        if self.output.is_running():
            self.statusbar.showMessage("Program already running")
            return

        cursor = editor.textCursor()
        # QPlainTextEdit uses U+2029 (paragraph separator) in selectedText();
        # replace with standard newlines for the interpreter.
        selected = cursor.selectedText().replace("\u2029", "\n").strip()
        if not selected:
            self.statusbar.showMessage("No selection to run")
            return

        current_info = self.get_current_tab_info()
        language = current_info["language"]
        self.output.set_language(language)
        try:
            self.right_tabs.setCurrentWidget(self.output_canvas_pane)
        except AttributeError:
            pass

        self.run_action.setEnabled(False)
        self.stop_action.setEnabled(True)
        self.statusbar.showMessage("Running selection")
        self.output.run_program(selected, self.canvas, debug_mode=False)

    def run_program(self):
        """Run the current editor contents in the output panel."""
        editor = self.get_current_editor()
        if not editor:
            return

        if self.output.is_running():
            self.statusbar.showMessage("Program already running")
            return

        current_info = self.get_current_tab_info()
        language = current_info["language"]

        # Ensure output uses the current language and is visible
        self.output.set_language(language)
        try:
            self.right_tabs.setCurrentWidget(self.output_canvas_pane)
        except AttributeError:
            # Non-fatal: if right_tabs not yet created in tests, ignore
            pass

        code = editor.toPlainText()

        line_count = len(code.splitlines())
        if line_count > self.MAX_SAFE_LINES:
            choice = QMessageBox.warning(
                self,
                "Large Program Warning",
                (
                    "This program is very large and may slow down the IDE. "
                    "Run anyway?"
                ),
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.No,
            )
            if choice != QMessageBox.StandardButton.Yes:
                self.statusbar.showMessage("Execution cancelled")
                return

        if not self._validate_before_run(code, language):
            self.statusbar.showMessage("Execution cancelled")
            return

        # Update UI state
        self.run_action.setEnabled(False)
        self.stop_action.setEnabled(True)
        self.statusbar.showMessage("Running program")

        # Show run indicator on current tab
        current_idx = self.editor_tabs.currentIndex()
        self._set_tab_run_indicator(current_idx, "running")

        # Clear any error highlights from the previous run
        editor = self.get_current_editor()
        if editor:
            editor.clear_error_lines()

        # Save to run history
        self._save_run_history(code, language)

        # Start execution (no debug controls in this build)
        self.output.run_program(code, self.canvas, debug_mode=False)

    def _validate_before_run(self, code: str, language: Language) -> bool:
        """Validate code with the Syntax Validator panel before execution."""
        panel = self.feature_manager.get_feature_panel("syntax_validator")
        if panel and hasattr(panel, "validate_external"):
            issues = panel.validate_external(code, language)
            if issues:
                # Build a readable list of errors for the dialog
                details = []
                for issue in issues[:20]:  # Cap at 20 to keep dialog manageable
                    sev = issue.severity.value if hasattr(issue.severity, "value") else str(issue.severity)
                    details.append(f"Line {issue.line} [{sev}]: {issue.message}")
                if len(issues) > 20:
                    details.append(f"... and {len(issues) - 20} more issues")
                detail_text = "\n".join(details)

                msg = QMessageBox(self)
                msg.setIcon(QMessageBox.Icon.Warning)
                msg.setWindowTitle("Syntax Issues Detected")
                msg.setText(f"Syntax Validator found {len(issues)} issue(s):")
                msg.setDetailedText(detail_text)
                msg.setInformativeText("Run anyway?")
                msg.setStandardButtons(
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                )
                msg.setDefaultButton(QMessageBox.StandardButton.No)
                choice = msg.exec()

                # Show the validator panel so the user can review all issues
                dock = self.feature_manager.dock_widgets.get("syntax_validator")
                if dock:
                    dock.setVisible(True)
                    dock.raise_()

                return choice == QMessageBox.StandardButton.Yes
        return True

    def on_execution_stats(self, stats: dict):
        """Handle execution stats from output panel."""
        language = stats.get("language")
        if isinstance(language, Language):
            stats["language"] = language.friendly_name()

        profiler_panel = self.feature_manager.get_feature_panel("performance_profiler")
        if profiler_panel and hasattr(profiler_panel, "update_from_stats"):
            profiler_panel.update_from_stats(stats)

        replay_panel = self.feature_manager.get_feature_panel("execution_replay")
        if replay_panel and hasattr(replay_panel, "record_execution"):
            replay_panel.record_execution(stats)

        analytics_panel = self.feature_manager.get_feature_panel("learning_analytics")
        if analytics_panel and hasattr(analytics_panel, "record_execution"):
            analytics_panel.record_execution(stats)

    def on_execution_complete(self):
        """Handle completion from the output panel."""
        self.check_execution_complete()

        lesson_panel = self.feature_manager.get_feature_panel("lesson_mode")
        if lesson_panel and hasattr(lesson_panel, "handle_execution_output"):
            lesson_panel.handle_execution_output(self.output.toPlainText())

        self._maybe_record_example_progress()

    def on_execution_error(self, error: str):
        """Handle execution errors for AI assistance and editor navigation."""
        # Parse line number from error message (e.g. 'line 5', 'Line 5:', 'at line 5')
        import re as _re

        line_match = _re.search(r"(?:line|Line)\s+(\d+)", error)
        if line_match:
            err_line = int(line_match.group(1))
            editor = self.get_current_editor()
            if editor:
                editor.clear_error_lines()
                editor.set_error_line(err_line)
                editor.goto_line(err_line)
                # Apply red wave underline to the error line
                self._underline_error_lines_in_editor(editor, {err_line})

        # Append friendly error hint from error_hints module
        try:
            context = self._get_current_line_context()
            hint = get_enhanced_error_message(error, context)
            if hint and hint.strip() and hint.strip() != error.strip():
                self.output.append(f"\n💡 Hint: {hint}")
        except Exception:  # pylint: disable=broad-except
            logger.debug("Error generating enhanced error hint", exc_info=True)

        ai_panel = self.feature_manager.get_feature_panel("ai_assistant")
        if ai_panel and hasattr(ai_panel, "set_error_context"):
            ai_panel.set_error_context(error)

        explainer_panel = self.feature_manager.get_feature_panel("error_explainer")
        if explainer_panel and hasattr(explainer_panel, "set_error_context"):
            context = self._get_current_line_context()
            explainer_panel.set_error_context(error, context)

        # Show error indicator on the active tab
        current_idx = self.editor_tabs.currentIndex()
        self._set_tab_run_indicator(current_idx, "error")

    def _get_current_line_context(self) -> str:
        """Get the current editor line for error context."""
        editor = self.get_current_editor()
        if not editor:
            return ""
        cursor = editor.textCursor()
        cursor.select(QTextCursor.SelectionType.LineUnderCursor)
        return cursor.selectedText()

    def _connect_lesson_signals(self):
        """Connect lesson panel signals to the main window."""
        panel = self.feature_manager.get_feature_panel("lesson_mode")
        if not panel:
            return
        if hasattr(panel, "lesson_started"):
            panel.lesson_started.connect(self._apply_lesson_code)
        if hasattr(panel, "lesson_checkpoint_ready"):
            panel.lesson_checkpoint_ready.connect(self._apply_lesson_code)
        if hasattr(panel, "export_markdown_requested"):
            panel.export_markdown_requested.connect(
                lambda: self.export_lesson_session("markdown")
            )
        if hasattr(panel, "export_pdf_requested"):
            panel.export_pdf_requested.connect(
                lambda: self.export_lesson_session("pdf")
            )

    def _connect_lesson_authoring_signals(self):
        """Connect custom lesson authoring into Lesson Mode."""
        panel = self.feature_manager.get_feature_panel("lesson_authoring")
        if not panel or not hasattr(panel, "lesson_created"):
            return
        panel.lesson_created.connect(self._register_custom_lesson)

    def _connect_learning_hub_signals(self):
        """Connect the central learning hub to existing IDE features."""
        panel = self.feature_manager.get_feature_panel("learning_hub")
        if not panel:
            return
        if hasattr(panel, "open_feature_requested"):
            panel.open_feature_requested.connect(self._open_learning_resource)
        if hasattr(panel, "challenge_requested"):
            panel.challenge_requested.connect(self._load_challenge_from_hub)
        if hasattr(panel, "remix_requested"):
            panel.remix_requested.connect(self._remix_current_tab)
        if hasattr(panel, "tutor_requested"):
            panel.tutor_requested.connect(self._launch_ai_tutor_on_current_code)
        if hasattr(panel, "export_markdown_requested"):
            panel.export_markdown_requested.connect(
                lambda: self.export_lesson_session("markdown")
            )
        if hasattr(panel, "export_bundle_requested"):
            panel.export_bundle_requested.connect(self.export_classroom_bundle)

    def _register_custom_lesson(self, lesson_data: dict):
        """Register a custom-authored lesson with the lesson panel."""
        panel = self.feature_manager.get_feature_panel("lesson_mode")
        if not panel or not hasattr(panel, "add_custom_lesson"):
            self.statusbar.showMessage("Lesson mode not available", 3000)
            return
        try:
            lesson = panel.add_custom_lesson(lesson_data)
        except ValueError as exc:
            self.statusbar.showMessage(f"Could not add lesson: {exc}", 4000)
            return

        self.feature_manager.toggle_feature_panel("lesson_mode", visible=True)
        self.statusbar.showMessage(f"Custom lesson ready: {lesson.title}", 3000)

    def _open_learning_resource(self, feature_id: str):
        """Open a hub-selected panel or built-in help view."""
        if feature_id == "quick_reference":
            self._show_quick_reference()
            return
        self.feature_manager.toggle_feature_panel(feature_id, visible=True)

    def _load_challenge_from_hub(self, language_name: str, starter_code: str):
        """Create a new tab from a featured learning challenge."""
        language = getattr(Language, language_name.upper(), Language.BASIC)
        title = f"{language_name.title()} Challenge"
        self.create_new_tab(title=title, content=starter_code, language=language)
        self.feature_manager.toggle_feature_panel("lesson_mode", visible=True)
        self.statusbar.showMessage(f"Challenge loaded: {title}", 3000)

    def _remix_current_tab(self):
        """Duplicate the current tab so learners can safely experiment."""
        editor = self.get_current_editor()
        current_index = self.editor_tabs.currentIndex()
        if editor is None or current_index < 0:
            self.create_new_tab(title="Remix", content="", language=Language.BASIC)
            self.statusbar.showMessage("Opened a blank remix tab", 3000)
            return

        language = self._ts(current_index).language
        title = self.editor_tabs.tabText(current_index).replace(" ●", "") or "Untitled"
        self.create_new_tab(
            title=f"Remix - {title}",
            content=editor.toPlainText(),
            language=language,
        )
        self.set_current_tab_info(modified=True, language=language)
        self.statusbar.showMessage("Created a remix copy of the current tab", 3000)

    def _launch_ai_tutor_on_current_code(self):
        """Open the AI tutor with the active code context."""
        panel = self.feature_manager.get_feature_panel("ai_assistant")
        self.feature_manager.toggle_feature_panel("ai_assistant", visible=True)
        editor = self.get_current_editor()
        current_index = self.editor_tabs.currentIndex()
        language = Language.BASIC
        code = ""
        if current_index >= 0:
            language = self._ts(current_index).language
        if editor is not None:
            code = editor.toPlainText()
        if panel and hasattr(panel, "set_code_context"):
            panel.set_code_context(language.name if hasattr(language, "name") else "BASIC", code)
        self.statusbar.showMessage("AI tutor opened for the current tab", 3000)

    def _connect_turtle_inspector_signals(self):
        """Connect turtle inspector panel to output events."""
        panel = self.feature_manager.get_feature_panel("turtle_inspector")
        if not panel:
            return
        if hasattr(panel, "add_snapshot"):
            self.output.turtle_state_changed.connect(panel.add_snapshot)
        if hasattr(panel, "clear_timeline"):
            self.output.turtle_state_reset.connect(panel.clear_timeline)
        if hasattr(panel, "snapshot_selected"):
            panel.snapshot_selected.connect(self._apply_turtle_snapshot)

    def _connect_reference_signals(self):
        """Connect reference search panel signals."""
        panel = self.feature_manager.get_feature_panel("reference_search")
        if not panel:
            return
        if hasattr(panel, "open_reference_requested"):
            panel.open_reference_requested.connect(self.load_file)

    def _connect_classroom_signals(self):
        """Connect classroom mode panel signals."""
        panel = self.feature_manager.get_feature_panel("classroom_mode")
        if not panel:
            return
        if hasattr(panel, "start_presentation"):
            panel.start_presentation.connect(self.start_presentation_mode)
        if hasattr(panel, "stop_presentation"):
            panel.stop_presentation.connect(self.stop_presentation_mode)
        if hasattr(panel, "export_bundle_requested"):
            panel.export_bundle_requested.connect(self.export_classroom_bundle)
        if hasattr(panel, "import_bundle_requested"):
            panel.import_bundle_requested.connect(self.import_classroom_bundle)
        if hasattr(panel, "broadcast_code_requested"):
            panel.broadcast_code_requested.connect(self.broadcast_sample_code)
        if hasattr(panel, "collect_outputs_requested"):
            panel.collect_outputs_requested.connect(self.collect_student_outputs)

    def _apply_turtle_snapshot(self, turtle_state):
        """Apply a turtle snapshot to the canvas for replay."""
        if turtle_state and hasattr(self, "canvas"):
            self.canvas.set_turtle_state(turtle_state)

    # -- Presentation, classroom, lesson, and bundle methods live in
    # -- ClassroomMixin --

    def _maybe_record_example_progress(self):
        """Record example progress if current file is an example."""
        panel = self.feature_manager.get_feature_panel("achievements")
        if not panel or not hasattr(panel, "record_example_run"):
            return
        current_info = self.get_current_tab_info()
        file_path = current_info.get("file")
        if not file_path:
            return
        path = Path(file_path).resolve()
        root = self._find_repo_root()
        if not root:
            return
        try:
            rel_path = str(path.relative_to(root))
        except ValueError:
            return
        if rel_path.startswith("Examples/"):
            panel.record_example_run(rel_path)

    def _find_repo_root(self) -> Path | None:
        """Locate repository root for relative paths."""
        start = Path(__file__).resolve()
        for parent in [start] + list(start.parents):
            if (parent / "Examples").exists() and (parent / "docs").exists():
                return parent
        return None

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

    def update_title(self):
        """Update window title."""
        title = "Time Warp Studio v10.0.0"

        current_info = self.get_current_tab_info()
        if current_info["file"]:
            title += f" - {Path(current_info['file']).name}"
        else:
            title += " - Untitled"

        if current_info["modified"]:
            title += " *"

        self.setWindowTitle(title)

    def check_save_changes(self):
        """Check all tabs for unsaved changes before closing."""
        for i in range(self.editor_tabs.count()):
            modified = self._ts(i).modified
            if not modified:
                continue
            # Switch to the tab so the user can see what file we're asking about
            self.editor_tabs.setCurrentIndex(i)
            tab_name = self.editor_tabs.tabText(i).rstrip(" *")
            reply = QMessageBox.question(
                self,
                "Unsaved Changes",
                f"Do you want to save changes to {tab_name}?",
                QMessageBox.Save | QMessageBox.Discard | QMessageBox.Cancel,
                QMessageBox.Save,
            )
            if reply == QMessageBox.Save:
                self.save_file()
            elif reply == QMessageBox.Cancel:
                return False
            # Discard: continue to next tab
        return True

    def change_theme(self, theme_name):
        """Change IDE theme."""
        if self._classroom_lock_theme:
            self.statusBar().showMessage("Theme locked in classroom mode")
            return
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
        if self._classroom_lock_theme:
            self.statusBar().showMessage("Settings locked in classroom mode")
            return
        size = self.theme_manager.current_font_size
        self.theme_manager.set_font(font_family, size)
        self._apply_font_to_editors()
        self.settings.setValue("font_family", font_family)
        self.statusBar().showMessage(f"Font changed to: {font_family}")

    def change_font_size(self, size):
        """Change editor font size."""
        if self._classroom_lock_theme:
            self.statusBar().showMessage("Settings locked in classroom mode")
            return
        family = self.theme_manager.current_font_family
        self.theme_manager.set_font(family, size)
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

    # -- add_recent_file / update_recent_files_menu live in FileOperationsMixin --
    # -- Help / docs / about methods live in HelpDocsMixin --

    def _show_contextual_help(self):
        """F1 — show language-specific help for the currently active language."""
        current_idx = self.tab_widget.currentIndex()
        lang = self._ts(current_idx).language
        # Map Language enum to help key used by show_language_help
        lang_key = lang.name.lower().replace("_", "") if lang else None
        # Some names differ from help keys; build explicit overrides
        _overrides = {
            "c": "c",
            "c_lang": "c",
            "javascript": "javascript",
            "hypertalk": "hypertalk",
            "brainfuck": "brainfuck",
        }
        key = _overrides.get(lang_key, lang_key) if lang_key else None
        if key and hasattr(self, "show_language_help"):
            try:
                self.show_language_help(key)
                return
            except Exception:
                pass
        # Fallback to user manual
        self.show_user_manual()

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

    # -- Debugging methods live in DebugMixin --

    def update_cursor_position(self):
        """Update cursor position and breadcrumb in status bar."""
        editor = self.get_current_editor()
        if editor and hasattr(self, "position_label"):
            cursor = editor.textCursor()
            line = cursor.blockNumber() + 1
            col = cursor.columnNumber() + 1
            self.position_label.setText(f"Ln {line}, Col {col}")

            # Update breadcrumb: show language context around cursor
            if hasattr(self, "breadcrumb_label"):
                current_idx = self.editor_tabs.currentIndex()
                lang = self._ts(current_idx).language
                lang_name = (
                    lang.friendly_name()
                    if hasattr(lang, "friendly_name")
                    else str(lang)
                )
                # Find nearest function/procedure name above cursor
                context = self._get_cursor_context(editor, cursor)
                if context:
                    self.breadcrumb_label.setText(
                        f"{lang_name}  ›  {context}  ›  Ln {line}"
                    )
                else:
                    self.breadcrumb_label.setText(f"{lang_name}  ›  Ln {line}")

    def _get_cursor_context(self, editor, cursor) -> str:
        """Return the name of the nearest function/procedure above the cursor."""
        import re as _re

        patterns = [
            r"^\s*(?:def|function|sub|procedure|to|subroutine)\s+([A-Za-z_][A-Za-z0-9_]*)",
        ]
        block = cursor.block()
        # Walk backwards for up to 80 lines
        for _ in range(80):
            if not block.isValid():
                break
            text = block.text()
            for pat in patterns:
                m = _re.match(pat, text, _re.IGNORECASE)
                if m:
                    return m.group(1)
            block = block.previous()
        return ""

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

        # Show the output/graphics pane
        self.right_tabs.setCurrentWidget(self.output_canvas_pane)

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

    def _show_crt_settings(self):
        """Open a CRT settings dialog with intensity sliders."""
        from PySide6.QtWidgets import (
            QDialog, QDialogButtonBox, QFormLayout, QSlider,
        )
        from PySide6.QtCore import Qt

        dlg = QDialog(self)
        dlg.setWindowTitle("⚙️ CRT Effect Settings")
        dlg.setMinimumWidth(360)
        form = QFormLayout(dlg)

        def _slider(lo, hi, val):
            s = QSlider(Qt.Orientation.Horizontal)
            s.setRange(lo, hi)
            s.setValue(val)
            return s

        sl_scan = _slider(0, 100, int(getattr(self.crt_overlay, "_scanline_intensity", 0.15) * 100))
        sl_glow = _slider(0, 100, int(getattr(self.crt_overlay, "_glow_intensity", 0.10) * 100))
        sl_curv = _slider(0, 100, int(getattr(self.crt_overlay, "_curvature_amount", 0.02) * 1000))
        sl_vign = _slider(0, 100, int(getattr(self.crt_overlay, "_vignette_intensity", 0.30) * 100))

        form.addRow("Scanline Intensity (%):", sl_scan)
        form.addRow("Phosphor Glow (%):", sl_glow)
        form.addRow("Curvature (‰):", sl_curv)
        form.addRow("Vignette (%):", sl_vign)

        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        form.addRow(buttons)

        def _apply():
            self.crt_overlay.set_scanlines(
                self.scanlines_action.isChecked(),
                intensity=sl_scan.value() / 100.0,
                spacing=2,
            )
            self.crt_overlay.set_glow(
                self.glow_action.isChecked(),
                intensity=sl_glow.value() / 100.0,
            )
            self.crt_overlay.set_curvature(
                self.curvature_action.isChecked(),
                amount=sl_curv.value() / 1000.0,
            )
            self.crt_overlay.set_vignette(
                self.vignette_action.isChecked(),
                intensity=sl_vign.value() / 100.0,
            )

        buttons.accepted.connect(lambda: (_apply(), dlg.accept()))
        buttons.rejected.connect(dlg.reject)
        dlg.exec()

    def resizeEvent(self, event):  # pylint: disable=invalid-name
        """Handle window resize - update CRT overlay."""
        super().resizeEvent(event)
        if hasattr(self, "crt_overlay") and self.crt_overlay.parent():
            self.crt_overlay.setGeometry(self.crt_overlay.parent().rect())

    # -- Collaboration methods (collab_connect … on_user_left) live in
    # -- CollaborationMixin --

    # ===================================================================
    # GUI Enhancement: tabbed output routing
    # ===================================================================

    def _route_output_to_subtabs(self, text: str, output_type: str):
        """Route streamed output lines to the Errors and Turtle Log sub-tabs."""
        is_error = output_type == "error" or text.lstrip().startswith("❌")
        is_turtle = "🐢" in text

        if is_error and hasattr(self, "errors_log"):
            cursor = self.errors_log.textCursor()
            cursor.movePosition(QTextCursor.MoveOperation.End)
            cursor.insertText(text + "\n")
            self.errors_log.setTextCursor(cursor)

        if is_turtle and hasattr(self, "turtle_log"):
            cursor = self.turtle_log.textCursor()
            cursor.movePosition(QTextCursor.MoveOperation.End)
            cursor.insertText(text + "\n")
            self.turtle_log.setTextCursor(cursor)

    def _clear_all_output(self):
        """Clear the output panel and all sub-tabs."""
        self.output.clear()
        if hasattr(self, "errors_log"):
            self.errors_log.clear()
        if hasattr(self, "turtle_log"):
            self.turtle_log.clear()

    # ===================================================================
    # GUI Enhancement: command palette
    # ===================================================================

    def _show_command_palette(self, _checked: bool = False):
        """Open (or re-open) the command palette."""
        if self._command_palette is None:
            self._command_palette = CommandPalette(self)
        self._command_palette.show_palette(self)

    # ===================================================================
    # GUI Enhancement: language picker (Ctrl+Shift+L)
    # ===================================================================

    def _show_language_picker(self, _checked: bool = False):
        """Show a quick language selection dialog."""
        dlg = QDialog(self)
        dlg.setWindowTitle("Select Language")
        dlg.setMinimumWidth(300)
        layout = QVBoxLayout(dlg)
        layout.addWidget(QLabel("Choose a language for the current editor:"))

        combo = QComboBox()
        for lang in Language:
            combo.addItem(f"💻 {lang.friendly_name()}", lang)

        # Select current language
        current_idx = self.editor_tabs.currentIndex()
        current_lang = self._ts(current_idx).language
        for i in range(combo.count()):
            if combo.itemData(i) == current_lang:
                combo.setCurrentIndex(i)
                break

        layout.addWidget(combo)
        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        buttons.accepted.connect(dlg.accept)
        buttons.rejected.connect(dlg.reject)
        layout.addWidget(buttons)

        if dlg.exec() == QDialog.DialogCode.Accepted:
            selected = combo.currentData()
            if selected:
                # Update the language combo on the toolbar
                for i in range(self.language_combo.count()):
                    if self.language_combo.itemData(i) == selected:
                        self.language_combo.setCurrentIndex(i)
                        break

    # ===================================================================
    # GUI Enhancement: SQL workbench launcher
    # ===================================================================

    def _show_sql_workbench(self, _checked: bool = False):
        """Open the SQL workbench by switching the language to SQL and focusing the editor."""
        from .dialogs import show_info_dialog  # noqa: F401 – imported lazily
        # Switch editor language to SQL so the user can write SQL immediately
        for i in range(self.language_combo.count()):
            if str(self.language_combo.itemData(i)).upper() == "SQL":
                self.language_combo.setCurrentIndex(i)
                break
        self.editor.setFocus()
        self.statusbar.showMessage("🗄 SQL Workbench ready – write SQL and press F5 to run")

    # ===================================================================
    # GUI Enhancement: accessibility preset
    # ===================================================================

    def _apply_accessibility_preset(self, _checked: bool = False):
        """Apply one-click accessibility: 18pt font, light theme, no CRT."""
        # Pick a high-contrast-friendly theme
        accessible_themes = ["Solarized Light", "Spring", "Candy", "Forest"]
        available = self.theme_manager.get_theme_names()
        target_theme = next(
            (t for t in accessible_themes if t in available),
            available[0] if available else None,
        )
        if target_theme:
            self.change_theme(target_theme)

        # Set large font
        self.change_font_size(18)

        # Disable CRT effects
        if hasattr(self, "crt_enable_action") and self.crt_enable_action.isChecked():
            self.crt_enable_action.setChecked(False)
            self.toggle_crt_effects(False)

        self.statusbar.showMessage(
            "♿ Accessibility preset applied (18pt font, light theme, no CRT)"
        )

    # ===================================================================
    # GUI Enhancement: run history dropdown
    # ===================================================================

    def _populate_run_history_menu(self):
        """Populate the run history dropdown from QSettings + in-memory list."""
        if not hasattr(self, "_run_history_menu") or self._run_history_menu is None:
            return
        menu = self._run_history_menu
        menu.clear()

        # Load persisted history
        stored = self.settings.value("run_history", []) or []
        if isinstance(stored, str):
            stored = []  # corrupt entry

        # Merge persisted with current-session list
        combined = []
        seen_snippets: set = set()
        for entry in stored:
            if isinstance(entry, dict):
                snippet = entry.get("snippet", "")
                if snippet and snippet not in seen_snippets:
                    seen_snippets.add(snippet)
                    combined.append(entry)
        for entry in self._run_history:
            snippet = entry.get("snippet", "")
            if snippet and snippet not in seen_snippets:
                seen_snippets.add(snippet)
                combined.insert(0, entry)

        if not combined:
            no_history = menu.addAction("(No run history yet)")
            no_history.setEnabled(False)
            return

        for entry in combined[:10]:
            lang_name = entry.get("language", "BASIC")
            snippet = entry.get("snippet", "")
            label = f"[{lang_name}]  {snippet[:50]}{'…' if len(snippet) > 50 else ''}"
            action = menu.addAction(label)
            action.setData(entry)
            action.triggered.connect(
                lambda checked=False, e=entry: self._load_run_from_history(e)
            )

        menu.addSeparator()
        clear_action = menu.addAction("🗑 Clear History")
        clear_action.triggered.connect(self._clear_run_history)

    def _save_run_history(self, code: str, language: Language):
        """Save a run to the history list and persist to QSettings."""
        lang_name = (
            language.friendly_name()
            if hasattr(language, "friendly_name")
            else str(language)
        )
        first_line = code.split("\n", 1)[0].strip()
        entry = {
            "snippet": first_line or code[:60],
            "language": lang_name,
            "code": code,
            "timestamp": time.time(),
        }
        self._run_history.insert(0, entry)
        self._run_history = self._run_history[:10]

        # Persist
        existing = self.settings.value("run_history", []) or []
        if not isinstance(existing, list):
            existing = []
        existing.insert(0, entry)
        self.settings.setValue("run_history", existing[:20])

        self._populate_run_history_menu()

    def _load_run_from_history(self, entry: dict):
        """Load code from a run history entry into a new tab."""
        code = entry.get("code", "")
        lang_name = entry.get("language", "BASIC")
        if not code:
            return

        # Find the language enum
        language = Language.BASIC
        for lang in Language:
            if hasattr(lang, "friendly_name") and lang.friendly_name() == lang_name:
                language = lang
                break

        self.create_new_tab(f"[history] {lang_name}", code, language)
        self.set_current_tab_info(file=None, modified=True, language=language)

        # Update combo
        for i in range(self.language_combo.count()):
            if self.language_combo.itemData(i) == language:
                self.language_combo.setCurrentIndex(i)
                break

        self.statusbar.showMessage(f"Loaded {lang_name} run from history")

    def _clear_run_history(self):
        """Clear all run history."""
        self._run_history.clear()
        self.settings.remove("run_history")
        self._populate_run_history_menu()
        self.statusbar.showMessage("Run history cleared")

    # ===================================================================
    # GUI Enhancement: theme preview dialog
    # ===================================================================

    def _show_theme_preview_dialog(self, _checked: bool = False):
        """Display a dialog showing a live preview of each theme."""
        dlg = QDialog(self)
        dlg.setWindowTitle("🎨 Theme Preview")
        dlg.setMinimumSize(700, 400)
        layout = QHBoxLayout(dlg)

        # Left: theme list
        theme_list = QListWidget()
        theme_list.setMaximumWidth(180)
        for name in self.theme_manager.get_theme_names():
            theme_list.addItem(name)
        layout.addWidget(theme_list)

        # Right: code preview
        preview = QPlainTextEdit()
        preview.setReadOnly(True)
        preview.setFont(QFont("Courier New", 11))
        _SAMPLE = (
            "REM Time Warp Studio — Theme Preview\n"
            '10 LET greeting$ = "Hello, World!"\n'
            "20 FOR I = 1 TO 5\n"
            '30   PRINT I; ") "; greeting$\n'
            "40 NEXT I\n"
            "50 END\n"
        )
        preview.setPlainText(_SAMPLE)
        layout.addWidget(preview)

        def on_select(item):
            name = item.text()
            self.theme_manager.apply_theme(
                name,
                editor=preview,
                output=None,
                canvas=None,
                highlighter=None,
            )

        def on_apply():
            items = theme_list.selectedItems()
            if items:
                self.change_theme(items[0].text())
            dlg.accept()

        theme_list.currentItemChanged.connect(on_select)

        # Pre-select current theme
        for i in range(theme_list.count()):
            if theme_list.item(i).text() == self.theme_manager.current_theme_name:
                theme_list.setCurrentRow(i)
                break

        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Apply
            | QDialogButtonBox.StandardButton.Close
        )
        buttons.button(QDialogButtonBox.StandardButton.Apply).clicked.connect(on_apply)
        buttons.rejected.connect(dlg.reject)

        outer = QVBoxLayout()
        outer.addLayout(layout)
        outer.addWidget(buttons)
        dlg.setLayout(outer)
        dlg.exec()

    # ===================================================================
    # GUI Enhancement: split editor panes
    # ===================================================================

    def _split_editor_right(self, _checked: bool = False):
        """Open the current tab's content in a second side-by-side editor pane."""
        current_editor = self.get_current_editor()
        if not current_editor:
            return

        current_idx = self.editor_tabs.currentIndex()
        code = current_editor.toPlainText()
        language = self._ts(current_idx).language
        title = f"[split] {self.editor_tabs.tabText(current_idx)}"

        self.create_new_tab(title, code, language)
        self.set_current_tab_info(file=None, modified=False, language=language)
        self.statusbar.showMessage("Opened split editor pane — edits are independent")

    # ===================================================================
    # GUI Enhancement: minimap toggle
    # ===================================================================

    def _toggle_minimap(self, checked: bool = False):
        """Toggle the minimap on all open editors."""
        for i in range(self.editor_tabs.count()):
            editor = self.editor_tabs.widget(i)
            if editor is not None and hasattr(editor, "enable_minimap"):
                editor.enable_minimap(checked)
        state = "shown" if checked else "hidden"
        self.statusbar.showMessage(f"Minimap {state}")

    # ===================================================================
    # GUI Enhancement: inline error wave-underlines
    # ===================================================================

    def _underline_error_lines_in_editor(self, editor, line_numbers: set):
        """Apply red wavy underlines to the specified line numbers in *editor*."""
        if not line_numbers or editor is None:
            return
        doc = editor.document()
        extra_selections = list(editor.extraSelections())

        for line_num in line_numbers:
            block = doc.findBlockByLineNumber(line_num - 1)
            if not block.isValid():
                continue
            cursor = QTextCursor(block)
            cursor.movePosition(
                QTextCursor.MoveOperation.EndOfLine, QTextCursor.MoveMode.KeepAnchor
            )

            fmt = QTextCharFormat()
            fmt.setUnderlineStyle(QTextCharFormat.UnderlineStyle.WaveUnderline)
            fmt.setUnderlineColor(QColor(255, 60, 60))

            sel = QTextEdit.ExtraSelection()
            sel.cursor = cursor
            sel.format = fmt
            extra_selections.append(sel)  # type: ignore[attr-defined]

        editor.setExtraSelections(extra_selections)

    # ===================================================================
    # GUI Enhancement: coach marks
    # ===================================================================

    def _start_coach_marks_tour(self, _checked: bool = False):
        """Force-start the interactive guided tour."""
        if self._coach_marks:
            self._coach_marks.start(force=True)
        else:
            self._coach_marks = CoachMarkManager(self)
            self._coach_marks.start(force=True)

    # ===================================================================
    # GUI Enhancement: keyboard shortcut overlay (Ctrl+?)
    # ===================================================================

    _SHORTCUTS_TABLE = [
        # (Category, Shortcut, Description)
        ("File", "Ctrl+N", "New file"),
        ("File", "Ctrl+O", "Open file"),
        ("File", "Ctrl+S", "Save file"),
        ("File", "Ctrl+Shift+S", "Save As…"),
        ("File", "Ctrl+H", "Version history"),
        ("File", "Ctrl+P", "Print code"),
        ("Edit", "Ctrl+Z", "Undo"),
        ("Edit", "Ctrl+Y / Ctrl+Shift+Z", "Redo"),
        ("Edit", "Ctrl+X", "Cut"),
        ("Edit", "Ctrl+C", "Copy"),
        ("Edit", "Ctrl+V", "Paste"),
        ("Edit", "Ctrl+F", "Find…"),
        ("Edit", "Ctrl+Shift+F", "Format code"),
        ("Edit", "Ctrl+Shift+I", "Insert snippet…"),
        ("Run", "Ctrl+R", "Run program"),
        ("Run", "Ctrl+Shift+R", "Run selection"),
        ("Run", "Ctrl+Shift+F5", "Stop execution"),
        ("Debug", "F5", "Start debugging"),
        ("Debug", "Shift+F5", "Stop debugging"),
        ("Debug", "Ctrl+F5", "Continue"),
        ("Debug", "F10", "Step over"),
        ("Debug", "F11", "Step into"),
        ("Debug", "Shift+F11", "Step out"),
        ("Debug", "F9", "Toggle breakpoint"),
        ("Debug", "Ctrl+Shift+F9", "Clear all breakpoints"),
        ("View", "Ctrl+Shift+P", "Command palette"),
        ("View", "Ctrl+Shift+L", "Language picker"),
        ("View", "Ctrl+\\", "Split editor right"),
        ("View", "Ctrl++", "Zoom in"),
        ("View", "Ctrl+-", "Zoom out"),
        ("View", "Ctrl+Shift+F11", "Focus / distraction-free mode"),
        ("Tools", "Ctrl+Shift+Q", "SQL Workbench"),
        ("Tools", "Ctrl+Shift+D", "Database Manager"),
        ("Help", "F1", "Context-sensitive language help"),
        ("Help", "Ctrl+?", "Show keyboard shortcuts (this dialog)"),
        ("Help", "Ctrl+Shift+H", "Export session as HTML"),
    ]

    def _show_keyboard_shortcuts(self, _checked: bool = False):
        """Open a searchable keyboard-shortcut reference dialog."""
        from PySide6.QtWidgets import (  # type: ignore[attr-defined]
            QDialog,
            QLineEdit,
            QTableWidget,
            QTableWidgetItem,
            QVBoxLayout,
            QDialogButtonBox,
            QHeaderView,
        )

        dlg = QDialog(self)
        dlg.setWindowTitle("⌨️ Keyboard Shortcuts")
        dlg.setMinimumSize(600, 450)
        layout = QVBoxLayout(dlg)

        search = QLineEdit()
        search.setPlaceholderText("Filter shortcuts…")
        layout.addWidget(search)

        table = QTableWidget()
        table.setColumnCount(3)
        table.setHorizontalHeaderLabels(["Category", "Shortcut", "Action"])
        table.horizontalHeader().setSectionResizeMode(2, QHeaderView.ResizeMode.Stretch)
        table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        table.setAlternatingRowColors(True)

        def _populate(query: str = ""):
            table.setRowCount(0)
            query_lower = query.lower()
            for cat, shortcut, desc in self._SHORTCUTS_TABLE:
                if query_lower and query_lower not in (cat + shortcut + desc).lower():
                    continue
                row = table.rowCount()
                table.insertRow(row)
                table.setItem(row, 0, QTableWidgetItem(cat))
                table.setItem(row, 1, QTableWidgetItem(shortcut))
                table.setItem(row, 2, QTableWidgetItem(desc))

        _populate()
        search.textChanged.connect(_populate)
        layout.addWidget(table)

        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Close)
        btns.rejected.connect(dlg.accept)
        layout.addWidget(btns)
        dlg.exec()

    # Placeholder for live collaboration feature
    class CollaborationManager:
        """Manages real-time collaboration sessions."""

        def __init__(self):
            self.active_sessions = {}

        def start_session(self, session_id: str, user: str):
            self.active_sessions[session_id] = [user]
            print(f"Collaboration session {session_id} started by {user}.")

        def join_session(self, session_id: str, user: str):
            if session_id in self.active_sessions:
                self.active_sessions[session_id].append(user)
                print(f"{user} joined session {session_id}.")
            else:
                print(f"Session {session_id} does not exist.")

        def end_session(self, session_id: str):
            if session_id in self.active_sessions:
                del self.active_sessions[session_id]
                print(f"Collaboration session {session_id} ended.")

    # Example integration point
    # collaboration_manager = CollaborationManager()
    # collaboration_manager.start_session("12345", "Alice")
