"""Borland Turbo-style Function-Key Bar (F1-F10) and Workspace Deck selector.

Shows the Normal / Shift / Ctrl function-key layers (the bar follows the held
modifier) and offers one-click Workspace Deck switching.
"""

from __future__ import annotations

from typing import ClassVar

from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QFont
from PySide6.QtWidgets import (
    QFrame,
    QHBoxLayout,
    QLabel,
    QPushButton,
    QSizePolicy,
    QWidget,
)


class FunctionKeyBar(QFrame):
    """Interactive F1–F10 bar with Normal / Shift / Ctrl layers and deck switches.

    Every non-empty slot names the exact key sequence MainWindow binds to the
    matching action, so the bar can never advertise a key that does nothing.
    """

    action_triggered = Signal(str)
    deck_selected = Signal(str)

    # Per layer: 10 slots of (key_sequence, action_id, caption); None = unused key.
    LAYERS: ClassVar[dict[str, list[tuple[str, str, str] | None]]] = {
        "normal": [
            ("F1", "help", "Help"),
            ("F2", "save", "Save"),
            ("F3", "open", "Open"),
            ("F4", "outline", "Outline"),
            ("F5", "zoom", "Zoom"),
            ("F6", "next_pane", "Next"),
            ("F7", "step_into", "Trace"),
            ("F8", "step_over", "Step"),
            ("F9", "run", "Run"),
            ("F10", "palette", "Menu"),
        ],
        "shift": [
            ("Shift+F1", "shortcuts", "Keys"),
            ("Shift+F2", "save_as", "SaveAs"),
            ("Shift+F3", "examples", "Demos"),
            ("Shift+F4", "find_in_files", "Grep"),
            ("Shift+F5", "split_editor", "Split"),
            ("Shift+F6", "toggle_hud", "HUD"),
            ("Shift+F7", "toggle_crt", "CRT"),
            ("Shift+F8", "step_out", "Out"),
            ("Shift+F9", "run_selection", "RunSel"),
            ("Shift+F10", "cycle_theme", "Theme"),
        ],
        "ctrl": [
            ("Ctrl+F1", "inspect", "Inspect"),
            ("Ctrl+F2", "reset", "Reset"),
            ("Ctrl+F3", "lessons", "Lessons"),
            None,
            ("Ctrl+F5", "filter_errors", "Errors"),
            None,
            ("Ctrl+F7", "pause", "Pause"),
            ("Ctrl+F8", "breakpoint", "Break"),
            ("Ctrl+F9", "debug", "Debug"),
            None,
        ],
    }

    _LAYER_BADGES: ClassVar[dict[str, str]] = {
        "normal": "FN",
        "shift": "SHIFT",
        "ctrl": "CTRL",
    }

    _DECKS: ClassVar[list[tuple[str, str, str]]] = [
        ("1:CODE", "code_focus", "Alt+1 — Code Focus Deck (Editor + Outline)"),
        ("2:SPLIT", "split_lab", "Alt+2 — Split Lab Deck (Dual Editor Panes)"),
        ("3:GFX", "graphics_studio", "Alt+3 — Graphics Studio Deck (Canvas Focus)"),
        ("4:DEBUG", "debugger_deck", "Alt+4 — Debugger & Telemetry HUD Deck"),
    ]

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setObjectName("FunctionKeyBar")
        self._current_layer: str = "normal"
        self._execution_state: str = "ready"
        self._active_deck: str = "code_focus"
        self._fkey_buttons: list[QPushButton] = []
        self._deck_buttons: dict[str, QPushButton] = {}
        self._setup_ui()
        self.set_layer("normal")

    def _setup_ui(self) -> None:
        self.setFrameShape(QFrame.Shape.StyledPanel)
        self.setFixedHeight(30)
        layout = QHBoxLayout(self)
        layout.setContentsMargins(6, 2, 6, 2)
        layout.setSpacing(3)

        # Layer cycle badge (Normal / Shift / Alt)
        self._layer_btn = QPushButton("FN")
        self._layer_btn.setFixedWidth(42)
        self._layer_btn.setCursor(Qt.CursorShape.PointingHandCursor)
        self._layer_btn.setToolTip(
            "Cycle Function-Key Bar layer (Normal / Shift / Ctrl)"
        )
        self._layer_btn.setAccessibleName("Cycle Function Key Layer")
        self._layer_btn.clicked.connect(self.cycle_layer)
        layout.addWidget(self._layer_btn)

        # 10 function-key buttons
        mono = QFont("Monospace", 9)
        mono.setStyleHint(QFont.StyleHint.Monospace)
        mono.setBold(True)

        for idx in range(10):
            btn = QPushButton()
            btn.setFont(mono)
            btn.setCursor(Qt.CursorShape.PointingHandCursor)
            btn.setSizePolicy(
                QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Preferred
            )
            btn.setMinimumWidth(68)
            btn.clicked.connect(
                lambda _checked=False, i=idx: self._on_slot_clicked(i)
            )
            self._fkey_buttons.append(btn)
            layout.addWidget(btn)

        # Separator label
        sep = QLabel("│")
        sep.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(sep)

        # Workspace Deck quick-switch buttons
        for label, deck_id, tooltip in self._DECKS:
            dbtn = QPushButton(label)
            dbtn.setFont(mono)
            dbtn.setCheckable(True)
            dbtn.setCursor(Qt.CursorShape.PointingHandCursor)
            dbtn.setToolTip(tooltip)
            dbtn.setAccessibleName(tooltip)
            dbtn.setMinimumWidth(58)
            dbtn.clicked.connect(
                lambda _checked=False, did=deck_id: self._on_deck_clicked(did)
            )
            self._deck_buttons[deck_id] = dbtn
            layout.addWidget(dbtn)

        self._update_deck_state()

    def _update_deck_state(self) -> None:
        for did, btn in self._deck_buttons.items():
            btn.setChecked(did == self._active_deck)

    def current_layer(self) -> str:
        """Return active modifier layer ('normal', 'shift', 'ctrl')."""
        return self._current_layer

    @classmethod
    def bindings(cls) -> list[tuple[str, str, str]]:
        """Return every (key_sequence, action_id, caption) across all layers."""
        return [slot for layer in cls.LAYERS.values() for slot in layer if slot]

    def set_layer(self, layer: str) -> None:
        """Switch displayed F-key bindings to *layer*."""
        if layer not in self.LAYERS:
            layer = "normal"
        self._current_layer = layer
        self._layer_btn.setText(self._LAYER_BADGES[layer])
        prefix = {"normal": "", "shift": "⇧", "ctrl": "^"}[layer]

        for idx, slot in enumerate(self.LAYERS[layer]):
            btn = self._fkey_buttons[idx]
            fkey = f"{prefix}F{idx + 1}"
            if slot is None:
                btn.setText(fkey)
                btn.setProperty("action_id", "")
                btn.setEnabled(False)
                btn.setToolTip("")
                btn.setAccessibleName(f"{fkey} unassigned")
                continue
            key_seq, action_id, caption = slot
            btn.setEnabled(True)
            btn.setText(f"{fkey} {caption}")
            btn.setProperty("action_id", action_id)
            btn.setToolTip(f"{key_seq}: {caption}")
            btn.setAccessibleName(f"{key_seq} {caption}")

    def cycle_layer(self) -> None:
        """Cycle through normal -> shift -> ctrl -> normal."""
        order = list(self.LAYERS)
        next_idx = (order.index(self._current_layer) + 1) % len(order)
        self.set_layer(order[next_idx])

    def set_execution_state(self, state: str) -> None:
        """Record the run state ('ready', 'running', 'stopped', 'error')."""
        self._execution_state = state

    def set_active_deck(self, deck_id: str) -> None:
        """Highlight the active Workspace Deck button."""
        self._active_deck = deck_id
        self._update_deck_state()

    def trigger_slot(self, slot_index: int) -> None:
        """Programmatically trigger slot 0..9."""
        self._on_slot_clicked(slot_index)

    def _on_slot_clicked(self, slot_index: int) -> None:
        if not (0 <= slot_index < len(self._fkey_buttons)):
            return
        action_id = self._fkey_buttons[slot_index].property("action_id")
        if isinstance(action_id, str) and action_id:
            self.action_triggered.emit(action_id)

    def _on_deck_clicked(self, deck_id: str) -> None:
        self.set_active_deck(deck_id)
        self.deck_selected.emit(deck_id)

    def apply_theme(self, theme) -> None:
        """Style the Function-Key Bar using semantic workstation tokens."""
        fkey_bg = getattr(theme, "fkey_bg", getattr(theme, "menu_bg", "#191a24")) or "#191a24"
        fkey_badge = getattr(theme, "fkey_badge", getattr(theme, "keyword", "#00e5ff"))
        fg = getattr(theme, "menu_fg", None) or getattr(theme, "foreground", "#f8f8f2")
        border = getattr(theme, "border_color", None) or getattr(theme, "line_number_bg", "#303646")
        sel_bg = getattr(theme, "selection_bg", "#264f78")
        sel_fg = getattr(theme, "selection_fg", "#ffffff")
        accent = getattr(theme, "hud_accent", "#50fa7b")

        self.setStyleSheet(f"""
            QFrame#FunctionKeyBar {{
                background-color: {fkey_bg};
                border-top: 1px solid {fkey_badge};
                border-bottom: 1px solid {border};
            }}
            QPushButton {{
                background-color: {fkey_bg};
                color: {fg};
                border: 1px solid {border};
                border-radius: 2px;
                padding: 2px 4px;
                font-size: 11px;
                font-weight: bold;
            }}
            QPushButton:hover {{
                background-color: {sel_bg};
                color: {sel_fg};
                border-color: {fkey_badge};
            }}
            QPushButton:checked {{
                background-color: {fkey_badge};
                color: {fkey_bg};
                border-color: {accent};
            }}
        """)
