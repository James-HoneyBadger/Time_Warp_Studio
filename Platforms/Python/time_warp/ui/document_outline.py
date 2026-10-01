"""Document outline widget for navigating Logo procedures and BASIC subroutines."""

import re
from typing import List, Tuple, Optional

from PySide6.QtCore import Qt, Signal, QAbstractItemModel
from PySide6.QtWidgets import (
    QTreeView,
    QWidget,
    QVBoxLayout,
    QLineEdit,
    QLabel,
)
from PySide6.QtGui import QIcon, QColor


class OutlineItem:
    """Represents an outline item (procedure, function, etc.)."""

    def __init__(self, name: str, line_number: int, item_type: str = "procedure"):
        self.name = name
        self.line_number = line_number  # 1-based
        self.item_type = item_type  # "procedure", "subroutine", "function", etc.
        self.children: List['OutlineItem'] = []
        self.parent: Optional['OutlineItem'] = None

    def __repr__(self):
        return f"OutlineItem({self.name}, line {self.line_number}, type={self.item_type})"


class OutlineModel(QAbstractItemModel):
    """Tree model for document outline."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.root = OutlineItem("root", 0)
        self.items: List[OutlineItem] = []

    def set_items(self, items: List[OutlineItem]):
        """Set the outline items."""
        self.beginResetModel()
        self.root = OutlineItem("root", 0)
        self.items = items
        for item in items:
            item.parent = self.root
        self.endResetModel()

    def rowCount(self, parent=None):
        if not parent or not parent.isValid():
            return len(self.items)
        return 0

    def columnCount(self, parent=None):
        return 1

    def data(self, index, role):
        if not index.isValid():
            return None

        item = self.items[index.row()]

        if role == Qt.DisplayRole:
            return f"{item.name} (line {item.line_number})"
        elif role == Qt.UserRole:
            return item

        return None

    def index(self, row, column, parent=None):
        if not parent or not parent.isValid():
            if 0 <= row < len(self.items):
                return self.createIndex(row, column, self.items[row])
        return self.createIndex(-1, -1)


class DocumentOutline(QWidget):
    """Widget showing document structure (procedures, functions, subroutines)."""

    item_clicked = Signal(int)  # Emitted with line number when item is clicked

    def __init__(self, parent=None):
        super().__init__(parent)
        self._editor = None
        self._current_language = None
        self._setup_ui()

    def _setup_ui(self):
        """Setup the outline UI."""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(4)

        # Title label
        title = QLabel("📋 Document Outline")
        layout.addWidget(title)

        # Search/filter field
        self._filter_input = QLineEdit()
        self._filter_input.setPlaceholderText("Filter (type to search)…")
        self._filter_input.textChanged.connect(self._apply_filter)
        layout.addWidget(self._filter_input)

        # Tree view
        self._tree = QTreeView()
        self._tree.setHeaderHidden(True)
        self._tree.setAnimated(True)
        self._model = OutlineModel(self)
        self._tree.setModel(self._model)
        self._tree.clicked.connect(self._on_item_clicked)
        layout.addWidget(self._tree)

    def set_editor(self, editor):
        """Set the editor to extract outline from."""
        self._editor = editor
        # Connect text changes to rebuild outline
        if editor:
            editor.document().contentsChanged.connect(self.rebuild_outline)

    def set_language(self, language):
        """Set the current language."""
        self._current_language = language
        self.rebuild_outline()

    def rebuild_outline(self):
        """Parse the document and rebuild the outline."""
        if not self._editor:
            return

        text = self._editor.toPlainText()
        items = self._extract_outline(text)
        self._model.set_items(items)
        self._tree.expandAll()

    def _extract_outline(self, text: str) -> List[OutlineItem]:
        """Extract procedures/functions from document based on language."""
        items = []

        if not self._current_language:
            return items

        lang_name = (
            self._current_language.name
            if hasattr(self._current_language, 'name')
            else str(self._current_language)
        ).upper()

        lines = text.split('\n')

        if lang_name == 'LOGO':
            items = self._extract_logo_outline(lines)
        elif lang_name == 'BASIC':
            items = self._extract_basic_outline(lines)
        elif lang_name == 'PASCAL':
            items = self._extract_pascal_outline(lines)
        elif lang_name == 'PROLOG':
            items = self._extract_prolog_outline(lines)
        elif lang_name == 'FORTH':
            items = self._extract_forth_outline(lines)
        elif lang_name == 'C':
            items = self._extract_c_outline(lines)

        # Sort by line number
        items.sort(key=lambda x: x.line_number)
        return items

    def _extract_logo_outline(self, lines: List[str]) -> List[OutlineItem]:
        """Extract Logo procedures (TO ... END)."""
        items = []
        for i, line in enumerate(lines, 1):
            match = re.match(r'^\s*TO\s+(\w+)', line, re.IGNORECASE)
            if match:
                proc_name = match.group(1)
                items.append(OutlineItem(proc_name, i, "procedure"))
        return items

    def _extract_basic_outline(self, lines: List[str]) -> List[OutlineItem]:
        """Extract BASIC subroutines (SUB/END SUB, FUNCTION/END FUNCTION)."""
        items = []
        for i, line in enumerate(lines, 1):
            # Look for SUB declarations
            match = re.match(r'^\s*SUB\s+(\w+)', line, re.IGNORECASE)
            if match:
                sub_name = match.group(1)
                items.append(OutlineItem(sub_name, i, "subroutine"))
                continue

            # Look for FUNCTION declarations
            match = re.match(r'^\s*FUNCTION\s+(\w+)', line, re.IGNORECASE)
            if match:
                func_name = match.group(1)
                items.append(OutlineItem(func_name, i, "function"))

        return items

    def _extract_pascal_outline(self, lines: List[str]) -> List[OutlineItem]:
        """Extract Pascal procedures and functions."""
        items = []
        for i, line in enumerate(lines, 1):
            match = re.match(r'^\s*PROCEDURE\s+(\w+)', line, re.IGNORECASE)
            if match:
                items.append(OutlineItem(match.group(1), i, "procedure"))

            match = re.match(r'^\s*FUNCTION\s+(\w+)', line, re.IGNORECASE)
            if match:
                items.append(OutlineItem(match.group(1), i, "function"))

        return items

    def _extract_prolog_outline(self, lines: List[str]) -> List[OutlineItem]:
        """Extract Prolog clauses (facts and rules)."""
        items = []
        for i, line in enumerate(lines, 1):
            # Match predicate definitions (head before :- or .)
            match = re.match(r'^\s*(\w+)\s*(?:\(|:-|\.)', line)
            if match:
                pred_name = match.group(1)
                is_rule = ':-' in line
                item_type = "rule" if is_rule else "fact"
                items.append(OutlineItem(pred_name, i, item_type))

        return items

    def _extract_forth_outline(self, lines: List[str]) -> List[OutlineItem]:
        """Extract Forth words (: definition ... ;)."""
        items = []
        for i, line in enumerate(lines, 1):
            match = re.match(r'^\s*:\s+(\w+)\s', line)
            if match:
                word_name = match.group(1)
                items.append(OutlineItem(word_name, i, "word"))

        return items

    def _extract_c_outline(self, lines: List[str]) -> List[OutlineItem]:
        """Extract C functions."""
        items = []
        for i, line in enumerate(lines, 1):
            # Very basic: look for function patterns
            match = re.match(r'^\s*\w+\s+(\w+)\s*\(', line)
            if match and not any(kw in line for kw in ['if', 'while', 'for', 'switch']):
                func_name = match.group(1)
                items.append(OutlineItem(func_name, i, "function"))

        return items

    def _apply_filter(self):
        """Filter outline items based on search text."""
        filter_text = self._filter_input.text().lower()

        # For simplicity, just rebuild with filter
        # In a production UI, we'd implement proper filtering
        if not filter_text:
            self.rebuild_outline()
            return

        # Filter the current model items
        if not self._editor:
            return

        text = self._editor.toPlainText()
        all_items = self._extract_outline(text)

        # Filter items by name
        filtered = [
            item for item in all_items
            if filter_text in item.name.lower()
        ]

        self._model.set_items(filtered)
        self._tree.expandAll()

    def _on_item_clicked(self, index):
        """Handle outline item click."""
        item = self._model.data(index, Qt.UserRole)
        if item and item.line_number > 0:
            self.item_clicked.emit(item.line_number)
