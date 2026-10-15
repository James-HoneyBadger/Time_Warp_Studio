"""Live Telemetry & Memory/Stack HUD for Time Warp Studio.

Provides an early-1990s hardware logic-analyzer / Turbo Debugger style live
inspection panel displaying:
- Execution speed (steps/sec Hz), elapsed time, active line, and budget meter
- Brainfuck 16-cell memory tape window with active data pointer highlight
- Forth Data Stack, Return Stack, and 16-byte hex/ASCII memory dump
- Turtle vector telemetry (X, Y, Heading, Pen state, Segments, Shapes)
- Active variables / call-stack summary across all 9 languages
"""

from __future__ import annotations

from typing import Any

from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QFont
from PySide6.QtWidgets import (
    QFrame,
    QGridLayout,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QPlainTextEdit,
    QProgressBar,
    QPushButton,
    QVBoxLayout,
    QWidget,
)


class TelemetryHUD(QWidget):
    """Hardware-style live telemetry, stack/tape memory, and turtle vector HUD."""

    memory_poked = Signal(int, int)  # (address, byte_value)

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setObjectName("TelemetryHUD")
        self._last_snapshot: dict[str, Any] = {}
        self._setup_ui()

    def _setup_ui(self) -> None:
        layout = QVBoxLayout(self)
        layout.setContentsMargins(6, 6, 6, 6)
        layout.setSpacing(6)

        mono = QFont("Monospace", 10)
        mono.setStyleHint(QFont.StyleHint.Monospace)

        # Header banner
        hdr_frame = QFrame()
        hdr_layout = QHBoxLayout(hdr_frame)
        hdr_layout.setContentsMargins(6, 4, 6, 4)
        self._title_label = QLabel("⚡ LIVE TELEMETRY & MEMORY HUD")
        self._title_label.setStyleSheet("font-weight: bold;")
        self._state_badge = QLabel("IDLE")
        self._state_badge.setAlignment(
            Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter
        )
        hdr_layout.addWidget(self._title_label)
        hdr_layout.addStretch()
        hdr_layout.addWidget(self._state_badge)
        layout.addWidget(hdr_frame)

        # Metrics grid (Speed Hz, Steps, Elapsed, Line/Depth, Turtle Vector)
        grid_frame = QFrame()
        grid = QGridLayout(grid_frame)
        grid.setContentsMargins(6, 4, 6, 4)
        grid.setHorizontalSpacing(12)
        grid.setVerticalSpacing(4)

        self._lang_val = QLabel("BASIC")
        self._speed_val = QLabel("0.0 Hz")
        self._steps_val = QLabel("0 / 250,000")
        self._elapsed_val = QLabel("0.0 ms")
        self._line_val = QLabel("Ln 1  Depth 0")
        self._turtle_val = QLabel("X:0.0 Y:0.0 ∠0.0° PEN:DN Seg:0")

        for lbl in (
            self._lang_val,
            self._speed_val,
            self._steps_val,
            self._elapsed_val,
            self._line_val,
            self._turtle_val,
        ):
            lbl.setFont(mono)

        grid.addWidget(QLabel("ENGINE:"), 0, 0)
        grid.addWidget(self._lang_val, 0, 1)
        grid.addWidget(QLabel("RATE:"), 0, 2)
        grid.addWidget(self._speed_val, 0, 3)

        grid.addWidget(QLabel("STEPS:"), 1, 0)
        grid.addWidget(self._steps_val, 1, 1)
        grid.addWidget(QLabel("TIME:"), 1, 2)
        grid.addWidget(self._elapsed_val, 1, 3)

        grid.addWidget(QLabel("CURSOR:"), 2, 0)
        grid.addWidget(self._line_val, 2, 1)
        grid.addWidget(QLabel("TURTLE:"), 2, 2)
        grid.addWidget(self._turtle_val, 2, 3)

        layout.addWidget(grid_frame)

        # Execution budget bar
        self._budget_bar = QProgressBar()
        self._budget_bar.setRange(0, 100)
        self._budget_bar.setValue(0)
        self._budget_bar.setFormat("Execution Budget: %p%")
        self._budget_bar.setFixedHeight(16)
        layout.addWidget(self._budget_bar)

        # Memory / Tape / Stack / Variables inspector readout
        self._memory_view = QPlainTextEdit()
        self._memory_view.setReadOnly(True)
        self._memory_view.setFont(mono)
        self._memory_view.setPlainText(
            "MEMORY / STACK / TAPE MONITOR READY.\n"
            "Run or step a program (F9 / F7) to inspect live state."
        )
        layout.addWidget(self._memory_view, 1)

        # Interactive Live Hex/RAM Poke Bar
        poke_frame = QFrame()
        poke_layout = QHBoxLayout(poke_frame)
        poke_layout.setContentsMargins(6, 3, 6, 3)
        poke_layout.setSpacing(6)
        poke_layout.addWidget(QLabel("HEX POKE:"))
        self._poke_addr_input = QLineEdit()
        self._poke_addr_input.setPlaceholderText("Addr (e.g. 0 or $04)")
        self._poke_addr_input.setFixedWidth(115)
        self._poke_val_input = QLineEdit()
        self._poke_val_input.setPlaceholderText("Byte (0..255 or $FF)")
        self._poke_val_input.setFixedWidth(125)
        self._poke_btn = QPushButton("POKE")
        self._poke_btn.setToolTip("Write byte value directly into live RAM / Tape cell")
        self._poke_btn.clicked.connect(self._on_poke_clicked)
        poke_layout.addWidget(self._poke_addr_input)
        poke_layout.addWidget(self._poke_val_input)
        poke_layout.addWidget(self._poke_btn)
        poke_layout.addStretch()
        layout.addWidget(poke_frame)

    def _on_poke_clicked(self) -> None:
        raw_addr = self._poke_addr_input.text().strip()
        raw_val = self._poke_val_input.text().strip()
        if not raw_addr or not raw_val:
            return
        try:
            addr = (
                int(raw_addr[1:], 16)
                if raw_addr.startswith("$")
                else int(raw_addr, 0)
            )
            val = (
                int(raw_val[1:], 16)
                if raw_val.startswith("$")
                else int(raw_val, 0)
            ) & 0xFF
        except ValueError:
            return
        tape = self._last_snapshot.get("brainfuck_tape_window")
        if isinstance(tape, list) and 0 <= addr < len(tape):
            tape[addr] = val
        ram = self._last_snapshot.get("basic_ram_window")
        if isinstance(ram, list) and 0 <= addr < len(ram):
            ram[addr] = val
        self.update_from_snapshot(self._last_snapshot)
        self.memory_poked.emit(addr, val)

    @property
    def last_snapshot(self) -> dict[str, Any]:
        """Return the most recent telemetry snapshot."""
        return dict(self._last_snapshot)

    def update_from_snapshot(self, snapshot: dict[str, Any]) -> None:
        """Refresh HUD counters, budget gauge, and memory/stack view."""
        self._last_snapshot = dict(snapshot)

        lang = str(snapshot.get("language", "BASIC"))
        running = bool(snapshot.get("running", False))
        steps_used = int(snapshot.get("steps_used", 0))
        max_steps = max(1, int(snapshot.get("max_steps", 250_000)))
        elapsed_ms = float(snapshot.get("elapsed_ms", 0.0))
        steps_per_sec = float(snapshot.get("steps_per_sec", 0.0))
        current_line = int(snapshot.get("current_line", 1))
        recursion_depth = int(snapshot.get("recursion_depth", 0))

        self._lang_val.setText(lang)
        self._state_badge.setText("● RUNNING" if running else "■ READY")
        self._speed_val.setText(f"{steps_per_sec:,.1f} steps/s")
        self._steps_val.setText(f"{steps_used:,} / {max_steps:,}")
        self._elapsed_val.setText(f"{elapsed_ms:,.1f} ms")
        self._line_val.setText(f"Ln {current_line}  Depth {recursion_depth}")

        pct = min(100, int((steps_used * 100) / max_steps))
        self._budget_bar.setValue(pct)

        t_info = snapshot.get("turtle", {}) or {}
        tx = t_info.get("x", 0.0)
        ty = t_info.get("y", 0.0)
        th = t_info.get("heading", 0.0)
        pen = "DN" if t_info.get("pen_down", True) else "UP"
        lines_cnt = t_info.get("lines", 0)
        shapes_cnt = t_info.get("shapes", 0)
        self._turtle_val.setText(
            f"X:{tx} Y:{ty} ∠{th}° PEN:{pen} Seg:{lines_cnt} Shp:{shapes_cnt}"
        )

        # Build detailed memory/stack/variable text
        sections: list[str] = []

        # 1. Brainfuck Tape Window
        bf_tape = snapshot.get("brainfuck_tape_window", [])
        bf_dp = int(snapshot.get("brainfuck_dp", 0))
        if lang.upper() == "BRAINFUCK" or any(bf_tape):
            start_idx = max(0, bf_dp - 4)
            idx_row = " ".join(f"{start_idx + i:04d}" for i in range(len(bf_tape)))
            hex_row = " ".join(
                f"[{v:02X}]" if (start_idx + i) == bf_dp else f" {v:02X} "
                for i, v in enumerate(bf_tape)
            )
            asc_row = " ".join(
                f"  {chr(v) if 32 <= v <= 126 else '.'} " for v in bf_tape
            )
            sections.append(
                f"── BRAINFUCK TAPE (DP={bf_dp}) ──\n"
                f"ADDR: {idx_row}\n"
                f"HEX : {hex_row}\n"
                f"CHAR: {asc_row}"
            )

        # 2. Forth Stacks & Memory Window
        f_stack = snapshot.get("forth_stack", [])
        f_rstack = snapshot.get("forth_rstack", [])
        f_mem = snapshot.get("forth_memory_window", [])
        if lang.upper() == "FORTH" or f_stack or f_rstack:
            s_str = " ".join(str(x) for x in f_stack) if f_stack else "<empty>"
            r_str = " ".join(str(x) for x in f_rstack) if f_rstack else "<empty>"
            hex_dump = " ".join(f"{b:02X}" for b in f_mem[:16])
            sections.append(
                f"── FORTH STACK & MEMORY ──\n"
                f"DATA STACK ({len(f_stack)}): {s_str}\n"
                f"RET  STACK ({len(f_rstack)}): {r_str}\n"
                f"MEM[0000..000F]: {hex_dump}"
            )

        # 3. AdLib / Chiptune Audio Oscilloscope
        audio_osc = snapshot.get("audio_oscilloscope", "")
        if audio_osc:
            sections.append(f"── ADLIB / CHIPTUNE OSCILLOSCOPE ──\n[{audio_osc}]")

        # 4. BASIC / System RAM Window
        basic_ram = snapshot.get("basic_ram_window", [])
        if any(basic_ram):
            ram_hex = " ".join(f"{b:02X}" for b in basic_ram[:16])
            sections.append(f"── RAM[0000..000F] ──\n{ram_hex}")

        # 5. Call Stack
        call_stack = snapshot.get("call_stack", [])
        if call_stack:
            frames_str = " -> ".join(f"{name}@L{line}" for line, name in call_stack)
            sections.append(f"── CALL STACK ──\n{frames_str}")

        # 4. Variables Snapshot
        variables = snapshot.get("variables", {}) or {}
        user_vars = [
            (k, v)
            for k, v in sorted(variables.items())
            if not str(k).startswith("__")
        ]
        if user_vars:
            var_lines = [f"  {k:<16} = {v!r}" for k, v in user_vars[:24]]
            sections.append("── ACTIVE VARIABLES ──\n" + "\n".join(var_lines))
        elif not sections:
            sections.append("── ACTIVE VARIABLES ──\n  <no user variables bound>")

        self._memory_view.setPlainText("\n\n".join(sections))

    def apply_theme(self, theme) -> None:
        """Apply Neo-90s workstation theme colors to the HUD."""
        bg = getattr(theme, "editor_bg", "#12141b")
        fg = getattr(theme, "editor_fg", "#e6edf3")
        hdr_bg = getattr(theme, "panel_header_bg", "#202534")
        hdr_fg = getattr(theme, "panel_header_fg", "#00e5ff")
        border = getattr(theme, "border_color", "#303646")
        accent = getattr(theme, "hud_accent", "#50fa7b")

        self.setStyleSheet(f"""
            QWidget#TelemetryHUD {{
                background-color: {bg};
                color: {fg};
            }}
            QFrame {{
                background-color: {hdr_bg};
                border: 1px solid {border};
                border-radius: 2px;
            }}
            QLabel {{
                color: {fg};
                border: none;
                background: transparent;
            }}
            QProgressBar {{
                border: 1px solid {border};
                background-color: {bg};
                color: {hdr_fg};
                text-align: center;
                font-weight: bold;
            }}
            QProgressBar::chunk {{
                background-color: {accent};
            }}
            QPlainTextEdit {{
                background-color: {bg};
                color: {accent};
                border: 1px solid {border};
            }}
        """)
