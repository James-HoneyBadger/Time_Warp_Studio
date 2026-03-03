"""
CICS IDE Panel for Time Warp Studio.

Embeds the IBM 3278 terminal emulator alongside a code editor,
providing a full interactive CICS/COBOL development environment.

Layout:
  Left:  Code editor (CICS pseudo-COBOL) + run controls
  Right: IBM 3278 terminal (full 24×80 with proper colours, cursor, tabs)
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Optional

from PySide6.QtCore import QSize, Qt, QThread, Signal, Slot
from PySide6.QtGui import QFont
from PySide6.QtWidgets import (
    QComboBox, QGroupBox, QHBoxLayout, QLabel,
    QPlainTextEdit, QPushButton, QScrollArea, QSplitter,
    QVBoxLayout, QWidget,
)

from .terminal_3278 import (
    Attr, Color3270, Screen3270, Terminal3278, Terminal3278Window,
)

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter


def _mono(size: int = 10) -> QFont:
    f = QFont("Courier New", size)
    f.setStyleHint(QFont.TypeWriter)
    f.setFixedPitch(True)
    return f


# ---------------------------------------------------------------------------
# Background execution thread for CICS
# ---------------------------------------------------------------------------

class _CICSRunThread(QThread):
    output_ready = Signal(str)
    screen_ops_ready = Signal(list)   # list of (op, args) screen commands

    def __init__(self, interpreter, source: str):
        super().__init__()
        self._interpreter = interpreter
        self._source = source

    def run(self):
        try:
            from ..core.interpreter import Language
            from ..languages.cics import execute_cics, extract_screen_ops
            from ..graphics.turtle_state import TurtleState
            turtle = TurtleState()
            # execute_cics returns text output
            out = execute_cics(self._interpreter, self._source, turtle)
            self.output_ready.emit(out if isinstance(out, str) else "\n".join(out))
            # Also try to get structured screen ops if available
            if hasattr(self._interpreter, "_cics_screen_ops"):
                self.screen_ops_ready.emit(self._interpreter._cics_screen_ops)
                self._interpreter._cics_screen_ops = []
        except Exception as e:
            self.output_ready.emit(f"❌ CICS error: {e}")


# ---------------------------------------------------------------------------
# CICS IDE Panel
# ---------------------------------------------------------------------------

class CICSPanel(QWidget):
    """
    Full CICS development panel:
     - Left: source editor with CICS/COBOL syntax, run controls, output log
     - Right: IBM 3278 Model 2 terminal with phosphor selection and full keyboard
    """

    def __init__(self, interpreter=None, parent=None):
        super().__init__(parent)
        self._interpreter = interpreter
        self._thread: Optional[_CICSRunThread] = None
        self._build()

    def minimumSizeHint(self) -> QSize:  # type: ignore[override]
        """Return a small hint so QTabWidget doesn't enforce a huge window."""
        return QSize(100, 100)

    def _build(self):
        outer = QHBoxLayout(self)
        outer.setContentsMargins(2, 2, 2, 2)

        splitter = QSplitter(Qt.Horizontal)
        outer.addWidget(splitter)

        # ---- Left: editor + controls ----
        left_widget = QWidget()
        left_lay = QVBoxLayout(left_widget)
        left_lay.setContentsMargins(2, 2, 2, 2)

        # Toolbar row
        ctrl_row = QHBoxLayout()
        run_btn = QPushButton("▶  Run (F5)")
        run_btn.setShortcut("F5")
        run_btn.setStyleSheet("font-weight:bold; background:#1a3a1a; color:#0f0; "
                              "border:1px solid #0a0; padding:4px 10px;")
        run_btn.clicked.connect(self._run)
        ctrl_row.addWidget(run_btn)

        clear_btn = QPushButton("🗑 Clear")
        clear_btn.clicked.connect(self._clear_all)
        ctrl_row.addWidget(clear_btn)

        self._template_combo = QComboBox()
        self._template_combo.addItems([
            "— Template —",
            "Hello World",
            "Customer Inquiry",
            "Account Browse",
            "Menu Screen",
            "File Read Loop",
            "VSAM Update",
            "Multi-Map App",
        ])
        self._template_combo.currentIndexChanged.connect(self._load_template)
        ctrl_row.addWidget(self._template_combo)
        ctrl_row.addStretch()
        left_lay.addLayout(ctrl_row)

        # Code editor
        self._editor = QPlainTextEdit()
        self._editor.setFont(_mono(11))
        self._editor.setLineWrapMode(QPlainTextEdit.NoWrap)
        self._editor.setPlaceholderText(
            "       IDENTIFICATION DIVISION.\n"
            "       PROGRAM-ID. MYPROG.\n"
            "       PROCEDURE DIVISION.\n"
            "           EXEC CICS\n"
            "               SEND TEXT FROM('HELLO CICS WORLD')\n"
            "               LENGTH(16)\n"
            "           END-EXEC\n"
            "           EXEC CICS RETURN END-EXEC."
        )
        left_lay.addWidget(self._editor, 3)

        # Output log
        out_group = QGroupBox("Execution Log / Spool Output")
        out_lay = QVBoxLayout(out_group)
        self._output = QPlainTextEdit()
        self._output.setReadOnly(True)
        self._output.setFont(_mono(9))
        self._output.setMaximumHeight(180)
        out_lay.addWidget(self._output)
        left_lay.addWidget(out_group, 1)

        splitter.addWidget(left_widget)

        # ---- Right: 3278 terminal ----
        right_widget = QWidget()
        right_lay = QVBoxLayout(right_widget)
        right_lay.setContentsMargins(2, 2, 2, 2)

        # Phosphor + font controls
        phos_row = QHBoxLayout()
        phos_row.addWidget(QLabel("Phosphor:"))
        self._phos_combo = QComboBox()
        self._phos_combo.addItems(["🟢 Green", "🟡 Amber", "📄 Paper"])
        self._phos_combo.currentIndexChanged.connect(self._change_phosphor)
        phos_row.addWidget(self._phos_combo)
        font_minus = QPushButton("A-")
        font_minus.setFixedWidth(28)
        font_minus.clicked.connect(
            lambda: self._terminal.set_font_size(
                max(8, self._terminal._font.pointSize() - 1)))
        phos_row.addWidget(font_minus)
        font_plus = QPushButton("A+")
        font_plus.setFixedWidth(28)
        font_plus.clicked.connect(
            lambda: self._terminal.set_font_size(
                min(22, self._terminal._font.pointSize() + 1)))
        phos_row.addWidget(font_plus)
        phos_row.addStretch()

        # Key hint
        hint = QLabel(" Tab=next field  Shift+Tab=prev  F1-F24=PF keys  "
                       "Esc=CLEAR  Enter=ENTER  Alt+1/2/3=PA1/2/3")
        hint.setStyleSheet("color:#888; font-size:9px;")
        phos_row.addWidget(hint)
        right_lay.addLayout(phos_row)

        # The actual 3278 terminal widget
        self._terminal = Terminal3278(
            self, phosphor=Color3270.GREEN_PHOSPHOR
        )
        self._terminal.aid_key.connect(self._on_aid)

        # Wrap the terminal in a QScrollArea so its large fixed minimum size
        # (80 cols × char_width ≈ 800+ px) doesn't propagate up and force
        # the whole window to stay full-size.
        term_scroll = QScrollArea()
        term_scroll.setWidget(self._terminal)
        term_scroll.setWidgetResizable(False)  # terminal keeps its own size
        term_scroll.setMinimumSize(200, 200)   # override sizeHint propagation
        term_scroll.setStyleSheet("QScrollArea { background: #000; border: none; }")
        right_lay.addWidget(term_scroll)

        splitter.addWidget(right_widget)
        splitter.setSizes([380, 500])

    # ---- Controls ----

    def _run(self):
        source = self._editor.toPlainText().strip()
        if not source:
            return
        self._terminal.lock(True)
        self._terminal.clear_screen()
        self._terminal.set_status("⌛ EXECUTING...")
        self._output.clear()
        self._thread = _CICSRunThread(self._interpreter, source)
        self._thread.output_ready.connect(self._on_output)
        self._thread.screen_ops_ready.connect(self._on_screen_ops)
        self._thread.finished.connect(self._on_done)
        self._thread.start()

    def _on_output(self, text: str):
        self._output.setPlainText(text)
        # Parse text output and render on terminal
        self._render_text_on_terminal(text)

    def _on_screen_ops(self, ops: list):
        """Apply structured terminal screen operations (future)."""
        for op, *args in ops:
            if op == "write":
                row, col, txt, attr = args
                self._terminal.write_text(row, col, txt, attr)
            elif op == "field":
                row, col, attr, width = args
                self._terminal.define_field(row, col, attr, width)
            elif op == "cursor":
                row, col = args
                self._terminal.set_cursor(row, col)
            elif op == "clear":
                self._terminal.clear_screen()
            elif op == "status":
                self._terminal.set_status(args[0])

    def _on_done(self):
        self._terminal.lock(False)
        self._terminal.set_status("READY")

    def _render_text_on_terminal(self, text: str):
        """
        Render text output from the CICS executor onto the 3278 screen.
        Lines prefixed '=== ' are headers (bright),
        lines prefixed '  ' are field labels, others are normal text.
        """
        self._terminal.clear_screen()
        lines = text.splitlines()
        row = 0
        for raw in lines:
            if row >= 24:
                break
            if raw.startswith("===") or raw.startswith("---"):
                self._terminal.write_text(
                    row, 0, raw[:80], Attr.PROT_BRIGHT, "turquoise")
            elif raw.startswith("❌") or "ERROR" in raw.upper():
                self._terminal.write_text(
                    row, 0, raw[:80], Attr.PROT_BRIGHT, "red")
            elif raw.startswith("ℹ") or raw.startswith("  CICS"):
                self._terminal.write_text(
                    row, 0, raw[:80], Attr.PROT_NORMAL, "yellow")
            elif raw.startswith("🚀"):
                self._terminal.write_text(
                    row, 0, raw[:80], Attr.PROT_BRIGHT, "green")
            elif raw:
                self._terminal.write_text(
                    row, 0, raw[:80], Attr.PROT_NORMAL)
            row += 1

        # Place cursor at first unprotected field if any, else row 0
        first = self._terminal.screen.next_unprotected(0)
        self._terminal.screen.cursor = first
        self._terminal.update()

    def _clear_all(self):
        self._editor.clear()
        self._output.clear()
        self._terminal.clear_screen()
        self._terminal._show_welcome()

    def _change_phosphor(self, idx: int):
        modes = [Color3270.GREEN_PHOSPHOR,
                 Color3270.AMBER_PHOSPHOR,
                 Color3270.WHITE_PAPER]
        if idx < len(modes):
            self._terminal.set_phosphor(modes[idx])

    def _on_aid(self, key: str, fields: list):
        """User pressed an AID key on the terminal – pass to CICS executor."""
        # Build a synthetic EXEC CICS RECEIVE response
        field_info = "\n".join(
            f"  Field[{i}]: {v}" for i, (_, _, v) in enumerate(fields)
        )
        msg = f"AID: {key}\n{field_info}" if fields else f"AID: {key}"
        self._output.appendPlainText(f"\n▶ {msg}")
        self._terminal.lock(False)
        self._terminal.set_status(f"AID {key} received")

    def _load_template(self, idx: int):
        if idx == 0:
            return
        templates = {
            1: _TMPL_HELLO,
            2: _TMPL_INQUIRY,
            3: _TMPL_BROWSE,
            4: _TMPL_MENU,
            5: _TMPL_FILE_READ,
            6: _TMPL_VSAM_UPDATE,
            7: _TMPL_MULTI_MAP,
        }
        tmpl = templates.get(idx, "")
        if tmpl:
            self._editor.setPlainText(tmpl)
        self._template_combo.setCurrentIndex(0)

    def set_source(self, source: str):
        self._editor.setPlainText(source)

    def apply_theme(self, theme_name: str):
        dark = theme_name.lower() not in ("spring", "candy", "paper")
        bg = "#0a0f0a" if dark else "#f0f5f0"
        self.setStyleSheet(f"background:{bg};")
        if dark:
            self._editor.setStyleSheet(
                "background:#0d1a0d; color:#33ff33; "
                "selection-background-color:#005500;"
            )
            self._output.setStyleSheet(
                "background:#0a0f0a; color:#00cc00;"
            )
        else:
            self._editor.setStyleSheet("")
            self._output.setStyleSheet("")


# ---------------------------------------------------------------------------
# CICS Program Templates
# ---------------------------------------------------------------------------

_TMPL_HELLO = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOCICS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MSG            PIC X(40) VALUE 'HELLO FROM TIME WARP CICS!'.
       01  WS-DATE           PIC 9(8).
       01  WS-TIME           PIC 9(7).
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           EXEC CICS
               ASKTIME ABSTIME(WS-TIME)
           END-EXEC
           EXEC CICS
               FORMATTIME ABSTIME(WS-TIME)
                          DDMMYYYY(WS-DATE)
           END-EXEC
           EXEC CICS
               SEND TEXT FROM(WS-MSG)
                         LENGTH(40)
                         ERASE
           END-EXEC
           EXEC CICS
               RETURN
           END-EXEC.
       STOP RUN.
"""

_TMPL_INQUIRY = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTINQ.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CUST-REC.
           05  CUST-ID       PIC 9(6).
           05  CUST-NAME     PIC X(30).
           05  CUST-BALANCE  PIC 9(9)V99.
       01  WS-CUST-KEY       PIC 9(6) VALUE 000001.
       01  WS-MSG            PIC X(80).
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           EXEC CICS
               READ FILE('CUSTFILE')
                    INTO(CUST-REC)
                    RIDFLD(WS-CUST-KEY)
               RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'CUSTOMER FOUND:' TO WS-MSG
           ELSE
               MOVE 'CUSTOMER NOT FOUND' TO WS-MSG
           END-IF
           EXEC CICS
               SEND TEXT FROM(WS-MSG) LENGTH(80) ERASE
           END-EXEC
           EXEC CICS RETURN END-EXEC.
"""

_TMPL_BROWSE = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTBROW.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ACCT-REC.
           05  ACCT-NUM      PIC 9(8).
           05  ACCT-TYPE     PIC X(10).
           05  ACCT-BAL      PIC S9(11)V99.
       01  WS-KEY            PIC 9(8) VALUE 00000001.
       01  WS-COUNT          PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           EXEC CICS
               STARTBR FILE('ACCTFILE')
                       RIDFLD(WS-KEY)
           END-EXEC
           PERFORM BROWSE-LOOP UNTIL WS-COUNT > 10.
           EXEC CICS
               ENDBR FILE('ACCTFILE')
           END-EXEC
           EXEC CICS RETURN END-EXEC.
       BROWSE-LOOP.
           EXEC CICS
               READNEXT FILE('ACCTFILE')
                        INTO(ACCT-REC)
                        RIDFLD(WS-KEY)
               RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 99 TO WS-COUNT
           ELSE
               ADD 1 TO WS-COUNT
               DISPLAY ACCT-NUM ACCT-TYPE ACCT-BAL
           END-IF.
"""

_TMPL_MENU = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINMENU.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SELECTION      PIC 9(1) VALUE 0.
       01  WS-MSG            PIC X(40).
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           EXEC CICS
               SEND TEXT FROM('=== TIME WARP MAIN MENU ===') LENGTH(28)
                         ERASE
           END-EXEC
           EXEC CICS
               SEND TEXT FROM('  1. CUSTOMER INQUIRY') LENGTH(21)
           END-EXEC
           EXEC CICS
               SEND TEXT FROM('  2. ACCOUNT BROWSE')   LENGTH(19)
           END-EXEC
           EXEC CICS
               SEND TEXT FROM('  3. TRANSACTION ENTRY') LENGTH(22)
           END-EXEC
           EXEC CICS
               SEND TEXT FROM('  PF3=EXIT') LENGTH(10)
           END-EXEC
           EXEC CICS
               RECEIVE INTO(WS-SELECTION) LENGTH(1)
           END-EXEC
           EVALUATE WS-SELECTION
               WHEN 1
                   EXEC CICS XCTL PROGRAM('CUSTINQ') END-EXEC
               WHEN 2
                   EXEC CICS XCTL PROGRAM('ACCTBROW') END-EXEC
               WHEN 3
                   EXEC CICS XCTL PROGRAM('TRANENT') END-EXEC
               WHEN OTHER
                   MOVE 'INVALID SELECTION' TO WS-MSG
                   EXEC CICS
                       SEND TEXT FROM(WS-MSG) LENGTH(18)
                   END-EXEC
           END-EVALUATE
           EXEC CICS RETURN TRANSID('MENU') END-EXEC.
"""

_TMPL_FILE_READ = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILERDEMO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  EMPLOYEE-REC.
           05  EMP-ID        PIC 9(6).
           05  EMP-LAST      PIC X(20).
           05  EMP-FIRST     PIC X(15).
           05  EMP-DEPT      PIC X(10).
           05  EMP-SALARY    PIC 9(7)V99.
       01  WS-KEY            PIC 9(6).
       01  WS-RESP           PIC 9(4).
       01  WS-LINE           PIC X(80).
       PROCEDURE DIVISION.
           MOVE 000001 TO WS-KEY
           EXEC CICS
               READ FILE('EMPFILE')
                    INTO(EMPLOYEE-REC)
                    RIDFLD(WS-KEY)
               RESP(WS-RESP)
           END-EXEC
           EXEC CICS
               HANDLE CONDITION
                   NOTFND(EMP-NOTFOUND)
                   ERROR(EMP-ERROR)
           END-EXEC
           STRING 'EMP: ' EMP-LAST ' ' EMP-FIRST
                  ' DEPT: ' EMP-DEPT
               DELIMITED SIZE INTO WS-LINE
           EXEC CICS
               SEND TEXT FROM(WS-LINE) LENGTH(80) ERASE
           END-EXEC
           GO TO DONE.
       EMP-NOTFOUND.
           EXEC CICS
               SEND TEXT FROM('EMPLOYEE NOT FOUND') LENGTH(18) ERASE
           END-EXEC.
       EMP-ERROR.
           EXEC CICS
               SEND TEXT FROM('FILE ERROR OCCURRED') LENGTH(19) ERASE
           END-EXEC.
       DONE.
           EXEC CICS RETURN END-EXEC.
"""

_TMPL_VSAM_UPDATE = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSAMUPD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ACCT-REC.
           05  ACCT-NUM      PIC 9(8).
           05  ACCT-BAL      PIC S9(11)V99.
           05  ACCT-LAST-UPD PIC 9(8).
       01  WS-KEY            PIC 9(8) VALUE 00000001.
       01  WS-AMOUNT         PIC S9(9)V99 VALUE +500.00.
       PROCEDURE DIVISION.
           EXEC CICS
               READ FILE('ACCTFILE')
                    INTO(ACCT-REC)
                    RIDFLD(WS-KEY)
                    UPDATE
           END-EXEC
           ADD WS-AMOUNT TO ACCT-BAL
           EXEC CICS
               REWRITE FILE('ACCTFILE')
                       FROM(ACCT-REC)
           END-EXEC
           EXEC CICS
               SYNCPOINT
           END-EXEC
           EXEC CICS
               SEND TEXT FROM('UPDATE COMPLETE') LENGTH(15) ERASE
           END-EXEC
           EXEC CICS RETURN END-EXEC.
"""

_TMPL_MULTI_MAP = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTMAP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COMMAREA.
           05  CA-TRAN-STATE  PIC X(1) VALUE 'I'.
           05  CA-CUST-ID     PIC 9(6).
           05  CA-ERROR-MSG   PIC X(40).
       01  WS-RESP            PIC 9(4).
       PROCEDURE DIVISION.
           EVALUATE CA-TRAN-STATE
               WHEN 'I'
                   PERFORM SEND-INITIAL-MAP
               WHEN 'P'
                   PERFORM PROCESS-INPUT
               WHEN OTHER
                   EXEC CICS ABEND ABCODE('STAT') END-EXEC
           END-EVALUATE
           EXEC CICS
               RETURN TRANSID('MMAP')
                      COMMAREA(WS-COMMAREA)
                      LENGTH(47)
           END-EXEC.
       SEND-INITIAL-MAP.
           EXEC CICS
               SEND MAP('MAINMAP')
                    MAPSET('MMAPMSET')
                    ERASE
                    CURSOR
           END-EXEC.
       PROCESS-INPUT.
           EXEC CICS
               RECEIVE MAP('MAINMAP')
                       MAPSET('MMAPMSET')
               RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               PERFORM VALIDATE-AND-PROCESS
           ELSE
               MOVE 'INVALID INPUT - PLEASE RE-ENTER' TO CA-ERROR-MSG
               MOVE 'I' TO CA-TRAN-STATE
           END-IF.
       VALIDATE-AND-PROCESS.
           IF CA-CUST-ID = ZEROS
               MOVE 'CUSTOMER ID REQUIRED' TO CA-ERROR-MSG
           ELSE
               MOVE 'P' TO CA-TRAN-STATE
               EXEC CICS
                   LINK PROGRAM('CUSTPROC')
                        COMMAREA(WS-COMMAREA)
               END-EXEC
           END-IF.
"""
