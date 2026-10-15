"""
One-Key Workspace Decks.

- Deck 1 (`code_focus`): Maximized editor + Document Outline + compact output
- Deck 2 (`split_lab`): Dual side-by-side editor panes + output
- Deck 3 (`graphics_studio`): Maximized turtle canvas + compact code strip
- Deck 4 (`debugger_deck`): Editor + Live Telemetry & Memory HUD
"""

from __future__ import annotations

from typing import Any, ClassVar


class CustomUILayouts:
    """Manages Workspace Decks and customizable UI layouts."""

    DECK_PRESETS: ClassVar[dict[str, dict[str, Any]]] = {
        "code_focus": {
            "name": "Code Focus",
            "shortcut": "Alt+1",
            "main_split": [960, 440],
            "right_split": [180, 620],
            "split_editor": False,
            "right_tab": "output",
            "show_outline": True,
        },
        "split_lab": {
            "name": "Split Lab",
            "shortcut": "Alt+2",
            "main_split": [980, 420],
            "right_split": [240, 560],
            "split_editor": True,
            "right_tab": "output",
            "show_outline": False,
        },
        "graphics_studio": {
            "name": "Graphics Studio",
            "shortcut": "Alt+3",
            "main_split": [480, 920],
            "right_split": [680, 160],
            "split_editor": False,
            "right_tab": "output",
            "show_outline": False,
        },
        "debugger_deck": {
            "name": "Debugger & Telemetry Deck",
            "shortcut": "Alt+4",
            "main_split": [720, 680],
            "right_split": [280, 520],
            "split_editor": False,
            "right_tab": "telemetry",
            "show_outline": True,
        },
    }

    def __init__(self) -> None:
        self.active_deck: str = "code_focus"

    def apply_deck(self, main_window: Any, deck_id: str) -> bool:
        """Apply one of the 4 Workspace Decks to *main_window*."""
        preset = self.DECK_PRESETS.get(deck_id)
        if not preset:
            return False

        self.active_deck = deck_id

        # 1. Split editor pane state
        want_split = bool(preset.get("split_editor", False))
        sec_tabs = getattr(main_window, "_editor_tabs_b", None)
        if (
            want_split
            and hasattr(main_window, "open_split_editor")
            and sec_tabs is not None
            and sec_tabs.isHidden()
        ):
            main_window.open_split_editor()
        elif (
            not want_split
            and hasattr(main_window, "close_split_editor")
            and sec_tabs is not None
            and not sec_tabs.isHidden()
        ):
            main_window.close_split_editor()

        # 2. Main horizontal splitter & right vertical splitter proportions
        main_sizes = preset.get("main_split")
        if isinstance(main_sizes, list) and hasattr(main_window, "main_splitter"):
            main_window.main_splitter.setSizes([int(x) for x in main_sizes])

        right_sizes = preset.get("right_split")
        if isinstance(right_sizes, list) and hasattr(main_window, "right_splitter"):
            main_window.right_splitter.setSizes([int(x) for x in right_sizes])

        # 3. Select lower-right tab (Output, Debug, Telemetry HUD, Terminal)
        target_tab = str(preset.get("right_tab", "output")).lower()
        if hasattr(main_window, "right_tabs"):
            for idx in range(main_window.right_tabs.count()):
                tab_label = main_window.right_tabs.tabText(idx).lower()
                if target_tab in tab_label:
                    main_window.right_tabs.setCurrentIndex(idx)
                    break

        # 4. Document Outline dock
        outline_dock = getattr(main_window, "outline_dock", None)
        if outline_dock is not None:
            outline_dock.setVisible(bool(preset.get("show_outline", False)))

        # 5. Sync Function-Key Bar active deck highlight
        if hasattr(main_window, "function_key_bar") and main_window.function_key_bar is not None:
            main_window.function_key_bar.set_active_deck(deck_id)

        if hasattr(main_window, "statusbar") and main_window.statusbar is not None:
            deck_title = preset.get("name", deck_id)
            main_window.statusbar.showMessage(
                f"🖥️ Workspace Deck: {deck_title} ({preset.get('shortcut', '')})", 3000
            )
        return True
