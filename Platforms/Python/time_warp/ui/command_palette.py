"""Command Palette widget for Time Warp Studio.

Provides a keyboard-searchable list of all QActions registered in the main
window, triggered by Ctrl+Shift+P.
"""

# pylint: disable=no-name-in-module

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QDialog,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QVBoxLayout,
)


class CommandPalette(QDialog):
    """Floating command palette that filters and triggers QActions.

    Usage::

        palette = CommandPalette(main_window)
        palette.show_palette()
    """

    def __init__(self, parent=None):
        super().__init__(parent, Qt.Dialog | Qt.FramelessWindowHint)
        self.setWindowTitle("Command Palette")
        self._actions: list = []  # list of (display_text, shortcut_text, QAction)
        self._setup_ui()

    # ------------------------------------------------------------------
    # UI setup
    # ------------------------------------------------------------------

    def _setup_ui(self):
        """Build the palette UI."""
        self.setMinimumWidth(500)
        self.setMaximumWidth(700)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(8, 8, 8, 8)
        layout.setSpacing(6)

        # Search field
        search_layout = QHBoxLayout()
        search_layout.addWidget(QLabel("🔍"))
        self.search_input = QLineEdit()
        self.search_input.setPlaceholderText("Type to filter commands…")
        self.search_input.textChanged.connect(self._on_search)
        self.search_input.returnPressed.connect(self._trigger_selected)
        search_layout.addWidget(self.search_input)
        layout.addLayout(search_layout)

        # Results list
        self.results_list = QListWidget()
        self.results_list.setFrameShape(QListWidget.NoFrame)
        self.results_list.itemActivated.connect(self._on_item_activated)
        layout.addWidget(self.results_list)

        hint = QLabel("↑↓ navigate  •  Enter to run  •  Esc to close")
        hint.setAlignment(Qt.AlignCenter)
        hint.setStyleSheet("color: palette(dark); font-size: 10px;")
        layout.addWidget(hint)

        self.setStyleSheet("""
            QDialog {
                background-color: palette(window);
                border: 2px solid palette(highlight);
                border-radius: 8px;
            }
            QLineEdit {
                background-color: palette(base);
                color: palette(text);
                border: 1px solid palette(dark);
                border-radius: 4px;
                padding: 6px 8px;
                font-size: 14px;
            }
            QListWidget {
                background-color: palette(base);
                color: palette(text);
                border: 1px solid palette(dark);
                border-radius: 4px;
                font-size: 12px;
            }
            QListWidget::item {
                padding: 6px 10px;
                border-radius: 3px;
            }
            QListWidget::item:selected {
                background-color: palette(highlight);
                color: palette(highlighted-text);
            }
            QListWidget::item:hover {
                background-color: rgba(0, 0, 0, 0.05);
            }
        """)

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def collect_actions(self, main_window):
        """Collect all QActions from main_window menus and toolbars."""
        self._actions.clear()
        seen_texts: set = set()

        def _harvest(actions):
            for action in actions:
                if action.menu():
                    # recurse into sub-menus
                    _harvest(action.menu().actions())
                elif action.text():
                    text = action.text().replace("&", "")
                    if text in seen_texts or not action.isEnabled():
                        continue
                    seen_texts.add(text)
                    sc = action.shortcut().toString()
                    self._actions.append((text, sc, action))

        # Harvest from menu bar
        for action in main_window.menuBar().actions():
            if action.menu():
                _harvest(action.menu().actions())

        # Sort alphabetically
        self._actions.sort(key=lambda t: t[0].lower())

    def show_palette(self, main_window=None):
        """Populate and show the palette centred on the main window."""
        if main_window is not None:
            self.collect_actions(main_window)

        self.search_input.clear()
        self._populate_list(self._actions)

        # Centre over parent
        if self.parent():
            parent_rect = self.parent().geometry()
            x = parent_rect.center().x() - self.sizeHint().width() // 2
            y = parent_rect.top() + 60
            self.move(x, y)

        self.show()
        self.raise_()
        self.activateWindow()
        self.search_input.setFocus()

    # ------------------------------------------------------------------
    # Internals
    # ------------------------------------------------------------------

    def _populate_list(self, candidates):
        """Fill the list widget with (text, shortcut, action) tuples."""
        self.results_list.clear()
        for text, sc, action in candidates:
            label = text
            if sc:
                label = f"{text}  [{sc}]"
            item = QListWidgetItem(label)
            item.setData(Qt.UserRole, action)
            self.results_list.addItem(item)

        if self.results_list.count():
            self.results_list.setCurrentRow(0)

    def _on_search(self, query: str):
        """Filter actions by query string."""
        q = query.lower()
        if not q:
            filtered = self._actions
        else:
            filtered = [(t, sc, a) for t, sc, a in self._actions if q in t.lower()]
        self._populate_list(filtered)

    def _trigger_selected(self):
        """Trigger the currently selected action and close."""
        item = self.results_list.currentItem()
        if item:
            action = item.data(Qt.UserRole)
            if action and action.isEnabled():
                self.close()
                action.trigger()

    def _on_item_activated(self, item):
        """Handle double-click / Enter on a list item."""
        action = item.data(Qt.UserRole)
        if action and action.isEnabled():
            self.close()
            action.trigger()

    # ------------------------------------------------------------------
    # Key navigation
    # ------------------------------------------------------------------

    def keyPressEvent(self, event):
        """Handle arrow keys for list navigation and Esc to close."""
        key = event.key()
        if key == Qt.Key_Escape:
            self.close()
        elif key == Qt.Key_Down:
            row = self.results_list.currentRow()
            if row < self.results_list.count() - 1:
                self.results_list.setCurrentRow(row + 1)
        elif key == Qt.Key_Up:
            row = self.results_list.currentRow()
            if row > 0:
                self.results_list.setCurrentRow(row - 1)
        else:
            super().keyPressEvent(event)
