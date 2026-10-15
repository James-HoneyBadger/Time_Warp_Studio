"""Document outline widget for navigating Logo procedures and BASIC subroutines."""

from __future__ import annotations

import re

from PySide6.QtCore import QAbstractItemModel, QModelIndex, Qt, Signal
from PySide6.QtWidgets import (
    QLabel,
    QLineEdit,
    QTreeView,
    QVBoxLayout,
    QWidget,
)

_INVALID_INDEX = QModelIndex()


class OutlineItem:
    """Represents an outline item (procedure, function, etc.)."""

    def __init__(self, name: str, line_number: int, item_type: str = "procedure"):
        self.name = name
        self.line_number = line_number  # 1-based
        self.item_type = item_type  # "procedure", "subroutine", "function", etc.
        self.children: list[OutlineItem] = []
        self.parent: OutlineItem | None = None

    def __repr__(self):
        return f"OutlineItem({self.name}, line {self.line_number}, type={self.item_type})"


class OutlineModel(QAbstractItemModel):
    """Tree model for document outline."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.root = OutlineItem("root", 0)
        self.items: list[OutlineItem] = []

    def set_items(self, items: list[OutlineItem]):
        """Set the outline items."""
        self.beginResetModel()
        self.root = OutlineItem("root", 0)
        self.items = items
        for item in items:
            item.parent = self.root
        self.endResetModel()

    def rowCount(self, parent=_INVALID_INDEX):  # type: ignore[override]
        if not parent or not parent.isValid():
            return len(self.items)
        return 0

    def columnCount(self, parent=_INVALID_INDEX):  # type: ignore[override]
        return 1

    def parent(self, index=_INVALID_INDEX):  # type: ignore[override]
        """Return no parent because the outline contains only top-level items."""
        return QModelIndex()

    def data(self, index, role=int(Qt.ItemDataRole.DisplayRole)):  # type: ignore[override]
        if not index.isValid():
            return None

        item = index.internalPointer()
        if not isinstance(item, OutlineItem):
            return None

        if role == Qt.ItemDataRole.DisplayRole:
            badges = {
                "procedure": "◆",
                "subroutine": "▸",
                "function": "ƒ",
                "class": "❖",
                "rule": "⊢",
                "fact": "•",
                "word": ":",
                "label": "★",
            }
            badge = badges.get(item.item_type, "•")
            return f"{badge} {item.name}  [Ln {item.line_number}]"
        if role == Qt.ItemDataRole.UserRole:
            return item

        return None

    def index(
        self,
        row: int,
        column: int,
        parent=_INVALID_INDEX,
    ) -> QModelIndex:  # type: ignore[override]
        if (not parent or not parent.isValid()) and column == 0 and 0 <= row < len(self.items):
            return self.createIndex(row, column, self.items[row])
        return QModelIndex()


class DocumentOutline(QWidget):
    """Widget showing document structure (procedures, functions, subroutines)."""

    item_clicked = Signal(int)  # Emitted with line number when item is clicked

    def __init__(self, parent=None):
        super().__init__(parent)
        self._editor = None
        self._current_language = None
        self._connected_editor = None
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
        if self._connected_editor is not None:
            try:
                self._connected_editor.document().contentsChanged.disconnect(
                    self.rebuild_outline
                )
            except (RuntimeError, TypeError):
                pass

        self._editor = editor
        self._connected_editor = editor
        if editor:
            editor.document().contentsChanged.connect(self.rebuild_outline)
            self.rebuild_outline()
        else:
            self._model.set_items([])

    def set_language(self, language):
        """Set the current language."""
        self._current_language = language
        self.rebuild_outline()

    def rebuild_outline(self):
        """Parse the document and rebuild the outline."""
        if not self._editor:
            self._model.set_items([])
            return

        text = self._editor.toPlainText()
        items = self._extract_outline(text)
        self._model.set_items(items)
        self._tree.expandAll()

    def _extract_outline(self, text: str) -> list[OutlineItem]:
        """Extract procedures/functions from document based on language."""
        items: list[OutlineItem] = []

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
        elif lang_name in ('PYTHON', 'PYTHON_LANG'):
            items = self._extract_python_outline(lines)
        elif lang_name == 'PILOT':
            items = self._extract_pilot_outline(lines)

        # Sort by line number
        items.sort(key=lambda x: x.line_number)
        return items

    def _extract_python_outline(self, lines: list[str]) -> list[OutlineItem]:
        """Extract Python classes and functions."""
        items = []
        for i, line in enumerate(lines, 1):
            m_cls = re.match(r'^\s*class\s+([A-Za-z_][A-Za-z0-9_]*)', line)
            if m_cls:
                items.append(OutlineItem(m_cls.group(1), i, "class"))
                continue
            m_def = re.match(r'^\s*(?:async\s+)?def\s+([A-Za-z_][A-Za-z0-9_]*)', line)
            if m_def:
                items.append(OutlineItem(m_def.group(1), i, "function"))
        return items

    def _extract_pilot_outline(self, lines: list[str]) -> list[OutlineItem]:
        """Extract PILOT *labels."""
        items = []
        for i, line in enumerate(lines, 1):
            m_lbl = re.match(r'^\s*\*([A-Za-z_][A-Za-z0-9_]*)', line)
            if m_lbl:
                items.append(OutlineItem(f"*{m_lbl.group(1)}", i, "label"))
        return items

    def _extract_logo_outline(self, lines: list[str]) -> list[OutlineItem]:
        """Extract Logo procedures (TO ... END)."""
        items = []
        for i, line in enumerate(lines, 1):
            match = re.match(r'^\s*TO\s+(\w+)', line, re.IGNORECASE)
            if match:
                proc_name = match.group(1)
                items.append(OutlineItem(proc_name, i, "procedure"))
        return items

    def _extract_basic_outline(self, lines: list[str]) -> list[OutlineItem]:
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

    def _extract_pascal_outline(self, lines: list[str]) -> list[OutlineItem]:
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

    def _extract_prolog_outline(self, lines: list[str]) -> list[OutlineItem]:
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

    def _extract_forth_outline(self, lines: list[str]) -> list[OutlineItem]:
        """Extract Forth words (: definition ... ;)."""
        items = []
        for i, line in enumerate(lines, 1):
            match = re.match(r'^\s*:\s+(\w+)\s', line)
            if match:
                word_name = match.group(1)
                items.append(OutlineItem(word_name, i, "word"))

        return items

    def _extract_c_outline(self, lines: list[str]) -> list[OutlineItem]:
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
        item = index.data(Qt.UserRole)
        if isinstance(item, OutlineItem) and item.line_number > 0:
            self.item_clicked.emit(item.line_number)
