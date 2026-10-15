"""Turbo keymap: every Function-Key Bar slot is backed by a real, unique shortcut."""

from __future__ import annotations

import collections

from PySide6.QtGui import QAction, QKeySequence

from time_warp.ui.function_key_bar import FunctionKeyBar
from time_warp.ui.main_window import MainWindow


def test_every_bar_key_is_bound_to_its_action(qapp):
    win = MainWindow()
    for key_seq, action_id, _caption in FunctionKeyBar.bindings():
        action = win._fkey_actions[action_id]
        assert QKeySequence(key_seq) in action.shortcuts(), (key_seq, action_id)


def test_no_shortcut_is_bound_twice(qapp):
    win = MainWindow()
    owners = collections.defaultdict(list)
    for action in win.findChildren(QAction):
        for ks in action.shortcuts():
            if not ks.isEmpty():
                owners[ks.toString()].append(action.text())
    dups = {k: v for k, v in owners.items() if len(v) > 1}
    assert not dups, dups


def test_turbo_core_keys(qapp):
    win = MainWindow()
    expected = {
        "run": "F9",
        "debug": "Ctrl+F9",
        "step_into": "F7",
        "step_over": "F8",
        "step_out": "Shift+F8",
        "breakpoint": "Ctrl+F8",
        "reset": "Ctrl+F2",
        "save": "F2",
        "open": "F3",
        "palette": "F10",
    }
    for action_id, key in expected.items():
        assert QKeySequence(key) in win._fkey_actions[action_id].shortcuts()


def test_shortcut_table_reflects_live_actions(qapp):
    win = MainWindow()
    rows = win.shortcut_table()
    keys = " ".join(r[1] for r in rows)
    for key in ("F9", "Ctrl+F9", "F7", "Ctrl+F8", "Ctrl+Alt+L", "Alt+X"):
        assert key in keys
    assert "F5" not in [r[1] for r in rows if r[2].startswith("Start")]


def test_clicking_bar_slot_triggers_action(qapp):
    win = MainWindow()
    fired = []
    win._fkey_actions["cycle_theme"].triggered.connect(lambda *_: fired.append(1))
    win.function_key_bar.set_layer("shift")
    win.function_key_bar.trigger_slot(9)
    assert fired == [1]
