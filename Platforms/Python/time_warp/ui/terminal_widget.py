"""Embedded shell terminal widget.

Provides a lightweight interactive terminal tab using ``QProcess``.
ANSI colour codes are stripped so output is always readable regardless
of the active theme.  Command history (up/down arrows) is supported.
"""

from __future__ import annotations

import os
import re
from collections import deque
from pathlib import Path

from PySide6.QtCore import QProcess, Qt
from PySide6.QtGui import QFont, QKeyEvent, QTextCursor
from PySide6.QtWidgets import (
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QPlainTextEdit,
    QSizePolicy,
    QVBoxLayout,
    QWidget,
)

# Matches all ANSI/VT100 escape sequences (colours, cursor movement, etc.)
_ANSI_RE = re.compile(r"\x1B\[[0-?]*[ -/]*[@-~]")


def _strip_ansi(text: str) -> str:
    return _ANSI_RE.sub("", text)


class _HistoryLineEdit(QLineEdit):
    """QLineEdit with up/down command-history navigation."""

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._history: deque[str] = deque(maxlen=200)
        self._hist_idx: int = -1  # -1 = "current (unsaved) input"
        self._saved_input: str = ""
        self._history_path: Path | None = None

    def add_to_history(self, cmd: str) -> None:
        if cmd and (not self._history or self._history[-1] != cmd):
            self._history.append(cmd)
            self._persist_command(cmd)
        self._hist_idx = -1

    def load_history_file(self, path: Path) -> None:
        """Load command history from *path* (one command per line)."""
        self._history_path = path
        if path.exists():
            try:
                lines = path.read_text(encoding="utf-8").splitlines()
                for line in lines:
                    line = line.strip()
                    if line:
                        self._history.append(line)
            except OSError:
                pass

    def _persist_command(self, cmd: str) -> None:
        """Append *cmd* to the history file (create if needed)."""
        if self._history_path is None:
            return
        try:
            self._history_path.parent.mkdir(parents=True, exist_ok=True)
            with self._history_path.open("a", encoding="utf-8") as fh:
                fh.write(cmd + "\n")
        except OSError:
            pass

    def keyPressEvent(self, event: QKeyEvent) -> None:  # type: ignore[override]
        if event.key() == Qt.Key.Key_Up:
            if not self._history:
                return
            if self._hist_idx == -1:
                self._saved_input = self.text()
                self._hist_idx = len(self._history) - 1
            elif self._hist_idx > 0:
                self._hist_idx -= 1
            self.setText(self._history[self._hist_idx])
        elif event.key() == Qt.Key.Key_Down:
            if self._hist_idx == -1:
                return
            self._hist_idx += 1
            if self._hist_idx >= len(self._history):
                self._hist_idx = -1
                self.setText(self._saved_input)
            else:
                self.setText(self._history[self._hist_idx])
        else:
            super().keyPressEvent(event)


class TerminalWidget(QWidget):
    """Embedded interactive terminal using QProcess.

    Each command is run as a fresh ``/bin/sh -c`` invocation so state
    (current directory, environment) is **not** shared between commands.
    ``cd`` changes the widget's internal ``_cwd`` so subsequent commands
    run in the new directory.
    """

    def __init__(self, cwd: str | None = None, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._cwd: str = cwd or os.path.expanduser("~")
        self._process: QProcess | None = None

        self._build_ui()
        # Load persistent history after UI is built
        try:
            from ..core.config import APP_DATA_DIR
            self._input.load_history_file(APP_DATA_DIR / "terminal_history")
        except Exception:  # noqa: BLE001 — optional feature
            pass

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self) -> None:
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(2)

        # Output display
        self.output = QPlainTextEdit()
        self.output.setReadOnly(True)
        self.output.setFont(QFont("Courier New", 10))
        self.output.setMaximumBlockCount(5000)
        self.output.setSizePolicy(
            QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding
        )
        layout.addWidget(self.output)

        # Input row: [cwd label] [command entry]
        input_row = QHBoxLayout()
        input_row.setContentsMargins(4, 0, 4, 4)
        input_row.setSpacing(6)

        self._cwd_label = QLabel(self._short_cwd())
        self._cwd_label.setFont(QFont("Courier New", 10))
        self._cwd_label.setObjectName("TerminalPrompt")
        input_row.addWidget(self._cwd_label)

        self._input = _HistoryLineEdit()
        self._input.setFont(QFont("Courier New", 10))
        self._input.setPlaceholderText("Enter a shell command…")
        self._input.returnPressed.connect(self._run_command)
        input_row.addWidget(self._input, stretch=1)

        layout.addLayout(input_row)

        self._append(
            f"Time Warp Studio — Integrated Terminal\n"
            f"Working directory: {self._cwd}\n"
            f"Type 'help' for built-in commands.\n"
        )

    # ------------------------------------------------------------------
    # Command handling
    # ------------------------------------------------------------------

    def _run_command(self) -> None:
        cmd = self._input.text().strip()
        self._input.clear()
        if not cmd:
            return

        self._input.add_to_history(cmd)
        self._append(f"$ {cmd}\n", bold=True)

        # Built-in: cd
        if cmd.startswith("cd"):
            parts = cmd.split(None, 1)
            target = parts[1] if len(parts) > 1 else os.path.expanduser("~")
            target = os.path.expanduser(target)
            if not os.path.isabs(target):
                target = os.path.join(self._cwd, target)
            target = os.path.normpath(target)
            if os.path.isdir(target):
                self._cwd = target
                self._update_prompt()
                self._append(f"→ {self._cwd}\n")
            else:
                self._append(f"cd: {target}: No such file or directory\n")
            return

        # Built-in: clear / cls
        if cmd in ("clear", "cls"):
            self.output.clear()
            return

        # Built-in: help
        if cmd == "help":
            self._append(
                "Built-in commands:\n"
                "  cd <dir>   — change directory\n"
                "  clear/cls  — clear output\n"
                "  help       — show this message\n"
                "All other commands are passed to /bin/sh -c\n"
            )
            return

        # External command via QProcess
        self._process = QProcess(self)
        self._process.setWorkingDirectory(self._cwd)
        env = self._process.processEnvironment()
        # Ensure PATH is populated from the current process
        for key in ("PATH", "HOME", "USER", "SHELL", "LANG", "TERM"):
            val = os.environ.get(key)
            if val:
                env.insert(key, val)
        self._process.setProcessEnvironment(env)
        self._process.setProcessChannelMode(
            QProcess.ProcessChannelMode.MergedChannels
        )
        self._process.readyRead.connect(self._on_output)
        self._process.finished.connect(self._on_finished)
        self._input.setEnabled(False)
        self._process.start("/bin/sh", ["-c", cmd])

    def _on_output(self) -> None:
        if self._process is None:
            return
        raw = bytes(self._process.readAll().data()).decode("utf-8", errors="replace")  # type: ignore[arg-type]
        self._append(_strip_ansi(raw))

    def _on_finished(self, exit_code: int, _exit_status: object) -> None:
        if exit_code != 0:
            self._append(f"[exited with code {exit_code}]\n")
        self._process = None
        self._input.setEnabled(True)
        self._input.setFocus()
        self._update_prompt()

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _append(self, text: str, bold: bool = False) -> None:
        cursor = self.output.textCursor()
        cursor.movePosition(QTextCursor.MoveOperation.End)
        if bold:
            fmt = cursor.charFormat()
            fmt.setFontWeight(700)
            cursor.setCharFormat(fmt)
            cursor.insertText(text)
            fmt.setFontWeight(400)
            cursor.setCharFormat(fmt)
        else:
            cursor.insertText(text)
        self.output.setTextCursor(cursor)
        self.output.ensureCursorVisible()

    def _short_cwd(self) -> str:
        home = os.path.expanduser("~")
        path = self._cwd
        if path.startswith(home):
            path = "~" + path[len(home):]
        return path + " $"

    def _update_prompt(self) -> None:
        self._cwd_label.setText(self._short_cwd())

    def focus_input(self) -> None:
        """Focus the command input; call this when the tab becomes visible."""
        self._input.setFocus()

    def set_cwd(self, path: str) -> None:
        """Set working directory (called by the IDE when a file is opened)."""
        if os.path.isdir(path):
            self._cwd = path
            self._update_prompt()
