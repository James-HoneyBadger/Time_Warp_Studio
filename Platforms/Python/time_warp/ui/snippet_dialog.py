# pylint: disable=too-few-public-methods
"""
Snippet dialog for Time Warp IDE.
Provides a dialog for browsing and inserting code snippets.
"""

# pylint: disable=no-name-in-module
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QFont
from PySide6.QtWidgets import (
    QComboBox,
    QDialog,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QPushButton,
    QSplitter,
    QTextEdit,
    QVBoxLayout,
    QWidget,
)

from .snippets import CodeSnippet, get_snippet_library


class SnippetDialog(QDialog):
    """Dialog for browsing and inserting code snippets."""

    snippet_selected = Signal(str)  # Emits the selected snippet code

    def __init__(self, language: str = "BASIC", parent=None):
        super().__init__(parent)
        self.language = language.upper()
        self.library = get_snippet_library()
        self.current_snippet: CodeSnippet | None = None

        self.setWindowTitle(f"Code Snippets - {self.language}")
        self.setMinimumSize(700, 500)
        self._setup_ui()
        self._load_snippets()

    def _setup_ui(self):
        """Set up the dialog UI."""
        layout = QVBoxLayout(self)

        # Top bar - language and search
        top_bar = QHBoxLayout()

        # Language selector
        top_bar.addWidget(QLabel("Language:"))
        self.language_combo = QComboBox()
        self.language_combo.addItems(["BASIC", "PILOT", "LOGO"])
        self.language_combo.setCurrentText(self.language)
        self.language_combo.currentTextChanged.connect(
            self._on_language_changed
        )
        top_bar.addWidget(self.language_combo)

        top_bar.addSpacing(20)

        # Category filter
        top_bar.addWidget(QLabel("Category:"))
        self.category_combo = QComboBox()
        self.category_combo.addItem("All")
        self.category_combo.currentTextChanged.connect(
            self._on_category_changed
        )
        top_bar.addWidget(self.category_combo)

        top_bar.addSpacing(20)

        # Search box
        top_bar.addWidget(QLabel("Search:"))
        self.search_edit = QLineEdit()
        self.search_edit.setPlaceholderText("Search snippets...")
        self.search_edit.textChanged.connect(self._on_search_changed)
        top_bar.addWidget(self.search_edit, 1)

        layout.addLayout(top_bar)

        # Main content - splitter with list and preview
        splitter = QSplitter(Qt.Horizontal)

        # Snippet list
        list_widget = QWidget()
        list_layout = QVBoxLayout(list_widget)
        list_layout.setContentsMargins(0, 0, 0, 0)

        list_layout.addWidget(QLabel("Snippets:"))
        self.snippet_list = QListWidget()
        self.snippet_list.currentItemChanged.connect(self._on_snippet_selected)
        self.snippet_list.itemDoubleClicked.connect(self._on_insert)
        list_layout.addWidget(self.snippet_list)

        splitter.addWidget(list_widget)

        # Preview pane
        preview_widget = QWidget()
        preview_layout = QVBoxLayout(preview_widget)
        preview_layout.setContentsMargins(0, 0, 0, 0)

        preview_layout.addWidget(QLabel("Preview:"))
        self.preview_edit = QTextEdit()
        self.preview_edit.setReadOnly(True)
        self.preview_edit.setFont(self._get_monospace_font())
        preview_layout.addWidget(self.preview_edit)

        # Description
        self.description_label = QLabel("")
        self.description_label.setWordWrap(True)
        self.description_label.setStyleSheet(
            "color: #888; font-style: italic;"
        )
        preview_layout.addWidget(self.description_label)

        splitter.addWidget(preview_widget)

        # Set initial splitter sizes
        splitter.setSizes([250, 450])

        layout.addWidget(splitter, 1)

        # Bottom buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        self.insert_button = QPushButton("Insert")
        self.insert_button.setEnabled(False)
        self.insert_button.clicked.connect(self._on_insert)
        button_layout.addWidget(self.insert_button)

        cancel_button = QPushButton("Cancel")
        cancel_button.clicked.connect(self.reject)
        button_layout.addWidget(cancel_button)

        layout.addLayout(button_layout)

    def _get_monospace_font(self):
        """Get a monospace font."""
        font = QFont("Courier New", 10)
        font.setStyleHint(QFont.Monospace)
        return font

    def _load_snippets(self):
        """Load snippets for the current language."""
        # Update categories
        self.category_combo.blockSignals(True)
        self.category_combo.clear()
        self.category_combo.addItem("All")
        for category in self.library.get_categories(self.language):
            self.category_combo.addItem(category)
        self.category_combo.blockSignals(False)

        # Load all snippets
        self._filter_snippets()

    def _filter_snippets(self):
        """Filter snippets based on category and search."""
        self.snippet_list.clear()

        category = self.category_combo.currentText()
        search_text = self.search_edit.text()

        if search_text:
            snippets = self.library.search(self.language, search_text)
        elif category == "All":
            snippets = self.library.get_snippets(self.language)
        else:
            snippets = self.library.get_by_category(self.language, category)

        for snippet in snippets:
            item = QListWidgetItem(f"📝 {snippet.name}")
            item.setData(Qt.UserRole, snippet)
            self.snippet_list.addItem(item)

        # Clear preview
        self.preview_edit.clear()
        self.description_label.clear()
        self.current_snippet = None
        self.insert_button.setEnabled(False)

    def _on_language_changed(self, language: str):
        """Handle language selection change."""
        self.language = language.upper()
        self.setWindowTitle(f"Code Snippets - {self.language}")
        self._load_snippets()

    def _on_category_changed(self, _category: str):
        """Handle category selection change."""
        self._filter_snippets()

    def _on_search_changed(self, _text: str):
        """Handle search text change."""
        self._filter_snippets()

    def _on_snippet_selected(self, current: QListWidgetItem, _previous):
        """Handle snippet selection."""
        if current is None:
            self.preview_edit.clear()
            self.description_label.clear()
            self.current_snippet = None
            self.insert_button.setEnabled(False)
            return

        snippet = current.data(Qt.UserRole)
        if isinstance(snippet, CodeSnippet):
            self.current_snippet = snippet
            self.preview_edit.setPlainText(snippet.code)
            self.description_label.setText(snippet.description)
            self.insert_button.setEnabled(True)

    def _on_insert(self):
        """Handle insert button click."""
        if self.current_snippet:
            self.snippet_selected.emit(self.current_snippet.code)
            self.accept()

    def get_selected_code(self) -> str | None:
        """Get the selected snippet code."""
        if self.current_snippet:
            return self.current_snippet.code
        return None
