"""Main window for Time Warp IDE."""

from PySide6.QtWidgets import (
    QMainWindow, QWidget, QVBoxLayout,
    QTabWidget, QFileDialog, QMessageBox,
    QStatusBar, QToolBar, QSplitter
)
from PySide6.QtCore import Qt, QSettings, QTimer
from PySide6.QtGui import QAction, QKeySequence
from pathlib import Path

from .editor import CodeEditor
from .output import OutputPanel
from .canvas import TurtleCanvas
from .themes import ThemeManager


class MainWindow(QMainWindow):
    """Main IDE window with editor, output, and canvas."""
    
    def __init__(self):
        super().__init__()
        
        # Settings for persistence
        self.settings = QSettings('TimeWarp', 'IDE')
        
        # Theme manager
        self.theme_manager = ThemeManager()
        
        # Current file
        self.current_file = None
        self.is_modified = False
        
        # Setup UI
        self.setup_ui()
        self.create_menus()
        self.create_toolbar()
        self.create_statusbar()
        
        # Restore previous state
        self.restore_state()
        
        # Apply theme
        self.theme_manager.apply_theme(
            self.settings.value('theme', 'Dracula')
        )

        # Ensure output starts cleared at application runtime
        try:
            self.output.clear()
        except Exception:
            pass
        
    def setup_ui(self):
        """Setup main UI layout."""
        self.setWindowTitle('Time Warp IDE - Python Edition')
        self.setMinimumSize(1200, 800)
        
        # Central widget with splitter
        central = QWidget()
        self.setCentralWidget(central)
        layout = QVBoxLayout(central)
        layout.setContentsMargins(0, 0, 0, 0)
        
        # Main splitter (horizontal)
        splitter = QSplitter(Qt.Horizontal)
        
        # Left side: Editor
        self.editor = CodeEditor(self)
        self.editor.textChanged.connect(self.on_text_changed)
        splitter.addWidget(self.editor)
        
        # Right side: Tabs for Output and Canvas
        self.right_tabs = QTabWidget()
        
        # Output panel
        self.output = OutputPanel(self)
        self.right_tabs.addTab(self.output, "Output")
        
        # Turtle canvas
        self.canvas = TurtleCanvas(self)
        self.right_tabs.addTab(self.canvas, "Graphics")
        
        splitter.addWidget(self.right_tabs)
        
        # Set initial splitter sizes (60% editor, 40% output)
        splitter.setSizes([720, 480])
        
        layout.addWidget(splitter)
        
    def create_menus(self):
        """Create menu bar."""
        menubar = self.menuBar()
        
        # File menu
        file_menu = menubar.addMenu('&File')
        
        new_action = QAction('&New', self)
        new_action.setShortcut(QKeySequence.New)
        new_action.triggered.connect(self.new_file)
        file_menu.addAction(new_action)
        
        open_action = QAction('&Open...', self)
        open_action.setShortcut(QKeySequence.Open)
        open_action.triggered.connect(self.open_file)
        file_menu.addAction(open_action)
        
        save_action = QAction('&Save', self)
        save_action.setShortcut(QKeySequence.Save)
        save_action.triggered.connect(self.save_file)
        file_menu.addAction(save_action)
        
        save_as_action = QAction('Save &As...', self)
        save_as_action.setShortcut(QKeySequence.SaveAs)
        save_as_action.triggered.connect(self.save_file_as)
        file_menu.addAction(save_as_action)
        
        file_menu.addSeparator()
        
        # Recent files submenu
        self.recent_menu = file_menu.addMenu('Recent Files')
        self.update_recent_files_menu()
        
        file_menu.addSeparator()
        
        exit_action = QAction('E&xit', self)
        exit_action.setShortcut(QKeySequence.Quit)
        exit_action.triggered.connect(self.close)
        file_menu.addAction(exit_action)
        
        # Edit menu
        edit_menu = menubar.addMenu('&Edit')
        
        undo_action = QAction('&Undo', self)
        undo_action.setShortcut(QKeySequence.Undo)
        undo_action.triggered.connect(self.editor.undo)
        edit_menu.addAction(undo_action)
        
        redo_action = QAction('&Redo', self)
        redo_action.setShortcut(QKeySequence.Redo)
        redo_action.triggered.connect(self.editor.redo)
        edit_menu.addAction(redo_action)
        
        edit_menu.addSeparator()
        
        cut_action = QAction('Cu&t', self)
        cut_action.setShortcut(QKeySequence.Cut)
        cut_action.triggered.connect(self.editor.cut)
        edit_menu.addAction(cut_action)
        
        copy_action = QAction('&Copy', self)
        copy_action.setShortcut(QKeySequence.Copy)
        copy_action.triggered.connect(self.editor.copy)
        edit_menu.addAction(copy_action)
        
        paste_action = QAction('&Paste', self)
        paste_action.setShortcut(QKeySequence.Paste)
        paste_action.triggered.connect(self.editor.paste)
        edit_menu.addAction(paste_action)
        
        edit_menu.addSeparator()
        
        find_action = QAction('&Find...', self)
        find_action.setShortcut(QKeySequence.Find)
        find_action.triggered.connect(self.editor.show_find_dialog)
        edit_menu.addAction(find_action)
        
        # Run menu
        run_menu = menubar.addMenu('&Run')
        
        self.run_action = QAction('&Run Program', self)
        self.run_action.setShortcut('F5')
        self.run_action.triggered.connect(self.run_program)
        run_menu.addAction(self.run_action)
        
        self.stop_action = QAction('&Stop', self)
        self.stop_action.setShortcut('Shift+F5')
        self.stop_action.setEnabled(False)
        self.stop_action.triggered.connect(self.stop_program)
        run_menu.addAction(self.stop_action)
        
        run_menu.addSeparator()
        
        clear_output_action = QAction('Clear &Output', self)
        clear_output_action.triggered.connect(self.output.clear)
        run_menu.addAction(clear_output_action)
        
        clear_canvas_action = QAction('Clear &Canvas', self)
        clear_canvas_action.triggered.connect(self.canvas.clear)
        run_menu.addAction(clear_canvas_action)
        
        # View menu
        view_menu = menubar.addMenu('&View')
        
        # Theme submenu
        theme_menu = view_menu.addMenu('&Theme')
        for theme_name in self.theme_manager.get_theme_names():
            action = QAction(theme_name, self)
            action.triggered.connect(
                lambda checked, t=theme_name: self.change_theme(t)
            )
            theme_menu.addAction(action)
        
        view_menu.addSeparator()
        
        zoom_in_action = QAction('Zoom &In', self)
        zoom_in_action.setShortcut(QKeySequence.ZoomIn)
        zoom_in_action.triggered.connect(self.editor.zoom_in)
        view_menu.addAction(zoom_in_action)
        
        zoom_out_action = QAction('Zoom &Out', self)
        zoom_out_action.setShortcut(QKeySequence.ZoomOut)
        zoom_out_action.triggered.connect(self.editor.zoom_out)
        view_menu.addAction(zoom_out_action)
        
        # Help menu
        help_menu = menubar.addMenu('&Help')
        
        examples_action = QAction('&Example Programs...', self)
        examples_action.triggered.connect(self.show_examples)
        help_menu.addAction(examples_action)
        
        help_menu.addSeparator()
        
        about_action = QAction('&About Time Warp IDE', self)
        about_action.triggered.connect(self.show_about)
        help_menu.addAction(about_action)
        
    def create_toolbar(self):
        """Create toolbar."""
        toolbar = QToolBar('Main Toolbar')
        toolbar.setObjectName('MainToolbar')  # Set object name to avoid Qt warning
        toolbar.setMovable(False)
        self.addToolBar(toolbar)
        
        # Add common actions
        toolbar.addAction('New', self.new_file)
        toolbar.addAction('Open', self.open_file)
        toolbar.addAction('Save', self.save_file)
        toolbar.addSeparator()
        toolbar.addAction('Run (F5)', self.run_program)
        toolbar.addAction('Stop', self.stop_program)
        toolbar.addSeparator()
        toolbar.addAction('Clear Output', self.output.clear)
        toolbar.addAction('Clear Canvas', self.canvas.clear)
        
    def create_statusbar(self):
        """Create status bar."""
        self.statusbar = QStatusBar()
        self.setStatusBar(self.statusbar)
        self.statusbar.showMessage('Ready')
        
    def new_file(self):
        """Create new file."""
        if not self.check_save_changes():
            return
        
        self.editor.clear()
        self.current_file = None
        self.is_modified = False
        self.update_title()
        self.statusbar.showMessage('New file created')
        
    def open_file(self):
        """Open file dialog."""
        if not self.check_save_changes():
            return
        
        filename, _ = QFileDialog.getOpenFileName(
            self,
            'Open File',
            str(Path.home()),
            'Time Warp Files (*.pilot *.bas *.logo *.tc);;'
            'PILOT Files (*.pilot);;'
            'BASIC Files (*.bas);;'
            'Logo Files (*.logo);;'
            'TempleCode Files (*.tc);;'
            'All Files (*.*)'
        )
        
        if filename:
            self.load_file(filename)
            
    def load_file(self, filename):
        """Load file into editor."""
        try:
            with open(filename, 'r', encoding='utf-8') as f:
                content = f.read()
            
            self.editor.setPlainText(content)
            self.current_file = filename
            self.is_modified = False
            self.update_title()
            self.add_recent_file(filename)
            self.statusbar.showMessage(f'Loaded: {filename}')
            
        except Exception as e:
            QMessageBox.critical(
                self,
                'Error Loading File',
                f'Could not load file:\n{e}'
            )
            
    def save_file(self):
        """Save current file."""
        if self.current_file:
            self.save_to_file(self.current_file)
        else:
            self.save_file_as()
            
    def save_file_as(self):
        """Save file as dialog."""
        filename, _ = QFileDialog.getSaveFileName(
            self,
            'Save File As',
            str(Path.home()),
            'Time Warp Files (*.pilot *.bas *.logo *.tc);;'
            'PILOT Files (*.pilot);;'
            'BASIC Files (*.bas);;'
            'Logo Files (*.logo);;'
            'TempleCode Files (*.tc);;'
            'All Files (*.*)'
        )
        
        if filename:
            self.save_to_file(filename)
            
    def save_to_file(self, filename):
        """Save content to file."""
        try:
            with open(filename, 'w', encoding='utf-8') as f:
                f.write(self.editor.toPlainText())
            
            self.current_file = filename
            self.is_modified = False
            self.update_title()
            self.add_recent_file(filename)
            self.statusbar.showMessage(f'Saved: {filename}')
            
        except Exception as e:
            QMessageBox.critical(
                self,
                'Error Saving File',
                f'Could not save file:\n{e}'
            )
            
    def run_program(self):
        """Run current program."""
        code = self.editor.toPlainText()
        
        if not code.strip():
            self.statusbar.showMessage('Nothing to run')
            return
        
        # Clear previous output and canvas
        self.output.clear()
        self.canvas.clear()
        
        # Switch to output tab
        self.right_tabs.setCurrentIndex(0)
        
        # Disable run, enable stop
        self.run_action.setEnabled(False)
        self.stop_action.setEnabled(True)
        self.statusbar.showMessage('Running...')
        
        # Run in background thread
        self.output.run_program(code, self.canvas)
        
        # Re-enable run button after execution
        QTimer.singleShot(100, self.check_execution_complete)
        
    def check_execution_complete(self):
        """Check if execution is complete."""
        if self.output.is_running():
            QTimer.singleShot(100, self.check_execution_complete)
        else:
            self.run_action.setEnabled(True)
            self.stop_action.setEnabled(False)
            self.statusbar.showMessage('Execution complete')
            # If graphics were drawn, switch to Graphics tab for convenience
            try:
                if getattr(self.canvas, 'lines', None):
                    if len(self.canvas.lines) > 0:
                        self.right_tabs.setCurrentWidget(self.canvas)
            except Exception:
                # Non-fatal; ignore any unexpected attribute issues
                pass
            
    def stop_program(self):
        """Stop running program."""
        self.output.stop_execution()
        self.run_action.setEnabled(True)
        self.stop_action.setEnabled(False)
        self.statusbar.showMessage('Stopped')
        
    def on_text_changed(self):
        """Handle text changes."""
        self.is_modified = True
        self.update_title()
        
    def update_title(self):
        """Update window title."""
        title = 'Time Warp IDE'
        
        if self.current_file:
            title += f' - {Path(self.current_file).name}'
        else:
            title += ' - Untitled'
            
        if self.is_modified:
            title += ' *'
            
        self.setWindowTitle(title)
        
    def check_save_changes(self):
        """Check if unsaved changes need to be saved."""
        if not self.is_modified:
            return True
        
        reply = QMessageBox.question(
            self,
            'Unsaved Changes',
            'Do you want to save your changes?',
            QMessageBox.Save | QMessageBox.Discard | QMessageBox.Cancel,
            QMessageBox.Save
        )
        
        if reply == QMessageBox.Save:
            self.save_file()
            return True
        elif reply == QMessageBox.Discard:
            return True
        else:
            return False
            
    def change_theme(self, theme_name):
        """Change IDE theme."""
        self.theme_manager.apply_theme(theme_name)
        self.settings.setValue('theme', theme_name)
        self.statusbar.showMessage(f'Theme changed to: {theme_name}')
        
    def add_recent_file(self, filename):
        """Add file to recent files list."""
        recent = self.settings.value('recent_files', [])
        if not isinstance(recent, list):
            recent = []
        
        if filename in recent:
            recent.remove(filename)
        recent.insert(0, filename)
        recent = recent[:10]  # Keep last 10
        
        self.settings.setValue('recent_files', recent)
        self.update_recent_files_menu()
        
    def update_recent_files_menu(self):
        """Update recent files menu."""
        self.recent_menu.clear()
        
        recent = self.settings.value('recent_files', [])
        if not isinstance(recent, list):
            recent = []
        
        if not recent:
            action = QAction('No recent files', self)
            action.setEnabled(False)
            self.recent_menu.addAction(action)
        else:
            for filename in recent:
                action = QAction(Path(filename).name, self)
                action.triggered.connect(
                    lambda checked, f=filename: self.load_file(f)
                )
                self.recent_menu.addAction(action)
                
    def show_examples(self):
        """Show examples dialog."""
        examples_dir = Path(__file__).parent.parent.parent / 'examples'
        
        if not examples_dir.exists():
            QMessageBox.information(
                self,
                'Examples',
                'Examples directory not found.'
            )
            return
        
        filename, _ = QFileDialog.getOpenFileName(
            self,
            'Open Example',
            str(examples_dir),
            'Time Warp Files (*.pilot *.bas *.logo);;All Files (*.*)'
        )
        
        if filename:
            self.load_file(filename)
            
    def show_about(self):
        """Show about dialog."""
        QMessageBox.about(
            self,
            'About Time Warp IDE',
            '<h2>Time Warp IDE - Python Edition</h2>'
            '<p>Version 2.0.0</p>'
            '<p>Educational programming environment supporting:</p>'
            '<ul>'
            '<li>PILOT - Interactive teaching language</li>'
            '<li>BASIC - Classic BASIC with line numbers</li>'
            '<li>Logo - Turtle graphics for visual learning</li>'
            '</ul>'
            '<p>Ported from Rust implementation</p>'
            '<p><b>Author:</b> James Temple</p>'
            '<p><a href="https://github.com/James-HoneyBadger/Time_Warp">'
            'github.com/James-HoneyBadger/Time_Warp</a></p>'
        )
        
    def restore_state(self):
        """Restore window state from settings."""
        geometry = self.settings.value('geometry')
        if geometry:
            self.restoreGeometry(geometry)
            
        state = self.settings.value('windowState')
        if state:
            self.restoreState(state)
            
    def save_state(self):
        """Save window state to settings."""
        self.settings.setValue('geometry', self.saveGeometry())
        self.settings.setValue('windowState', self.saveState())
        
    def closeEvent(self, event):
        """Handle window close."""
        if self.check_save_changes():
            self.save_state()
            event.accept()
        else:
            event.ignore()
