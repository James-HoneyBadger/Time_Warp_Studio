"""
IBM 3278 Terminal Emulator for Time Warp Studio CICS.

Faithful emulation of the IBM 3278 Model 2 (24x80) terminal:
- Complete 3270 data stream attribute model
- Protected/unprotected/numeric fields
- Full field-tab navigation (Tab / Shift+Tab)
- Block hardware cursor with blink
- IBM OIA (Operator Information Area) status line
- Colour model: 3279-style extended colour (7 standard 3270 colours)
- PF1-PF24, PA1-PA3, CLEAR, ENTER, Home, ErEOF, Insert, Delete, Reset
- IBM Courier / 3270-style monospace font
- Amber & green phosphor swap, and "white paper" mode
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Callable, List, Optional, Tuple

from PySide6.QtCore import (
    QRect,
    QSize,
    Qt,
    QTimer,
    Signal,
)
from PySide6.QtGui import (
    QColor,
    QFont,
    QFontDatabase,
    QFontMetrics,
    QKeyEvent,
    QPainter,
    QPen,
    QResizeEvent,
)
from PySide6.QtWidgets import (
    QLabel,
    QMainWindow,
    QStatusBar,
    QToolBar,
    QVBoxLayout,
    QWidget,
)

# ---------------------------------------------------------------------------
# Screen geometry constants
# ---------------------------------------------------------------------------
ROWS = 24  # visible rows (row 25 = OIA)
COLS = 80  # columns
OIA_ROW = 24  # 0-based OIA row index (25th row)
TOTAL_ROWS = 25  # including OIA
BUFFER_SIZE = ROWS * COLS  # address space 0-1919

# ---------------------------------------------------------------------------
# 3270 Attribute byte model (EBCDIC attribute character layout)
# ---------------------------------------------------------------------------
# Attribute bits (in high nibble of the single attribute character):
#   Bit 7-6 : 00 – field start marker (SFE)
#   Bit 5   : Protected
#   Bit 4   : Numeric
#   Bit 3-2 : Display intensity  00=normal 01=bright 10=zero-suppress 11=dark
#   Bit 1   : MDT (Modified Data Tag) – set when operator modifies field


class Attr:
    """3270 field attribute constants."""

    # Protection
    UNPROTECTED = 0x00
    PROTECTED = 0x20
    # Numeric
    ALPHA = 0x00
    NUMERIC = 0x10
    # Intensity
    NORMAL = 0x00
    BRIGHT = 0x08
    ZERO = 0x04  # display as blanks
    DARK = 0x0C  # non-display
    # MDT
    MDT = 0x01

    PROT_NORMAL = PROTECTED | NORMAL
    PROT_BRIGHT = PROTECTED | BRIGHT
    PROT_DARK = PROTECTED | DARK
    UNPROT_NORMAL = UNPROTECTED | NORMAL
    UNPROT_BRIGHT = UNPROTECTED | BRIGHT
    UNPROT_DARK = UNPROTECTED | DARK  # non-display (password fields)
    UNPROT_NUM = UNPROTECTED | NUMERIC
    SKIP = PROTECTED | DARK  # autoskip

    @staticmethod
    def is_protected(a: int) -> bool:
        return bool(a & Attr.PROTECTED)

    @staticmethod
    def is_numeric(a: int) -> bool:
        return bool(a & Attr.NUMERIC)

    @staticmethod
    def is_dark(a: int) -> bool:
        return (a & 0x0C) in (Attr.DARK, Attr.ZERO)

    @staticmethod
    def is_mdt(a: int) -> bool:
        return bool(a & Attr.MDT)

    @staticmethod
    def set_mdt(a: int) -> int:
        return a | Attr.MDT

    @staticmethod
    def clear_mdt(a: int) -> int:
        return a & ~Attr.MDT


# ---------------------------------------------------------------------------
# Colour palette – IBM 3279 extended colour (7 colours)
# ---------------------------------------------------------------------------
class Color3270:
    """IBM 3279 standard field colours (mapped into Qt colours)."""

    # Phosphor modes
    GREEN_PHOSPHOR = "green"
    AMBER_PHOSPHOR = "amber"
    WHITE_PAPER = "paper"

    _green = {
        "bg": QColor(0x00, 0x10, 0x00),
        "fg_normal": QColor(0x33, 0xCC, 0x33),
        "fg_bright": QColor(0x88, 0xFF, 0x88),
        "fg_dark": QColor(0x00, 0x40, 0x00),
        "cursor": QColor(0x33, 0xFF, 0x33),
        "oia_bg": QColor(0x00, 0x20, 0x00),
        "oia_text": QColor(0x00, 0xBB, 0x00),
        # field colours
        "blue": QColor(0x77, 0xBB, 0xFF),
        "red": QColor(0xFF, 0x55, 0x55),
        "pink": QColor(0xFF, 0x99, 0xFF),
        "green": QColor(0x33, 0xCC, 0x33),
        "turquoise": QColor(0x55, 0xFF, 0xFF),
        "yellow": QColor(0xFF, 0xFF, 0x55),
        "white": QColor(0xFF, 0xFF, 0xFF),
        "neutral": QColor(0x33, 0xCC, 0x33),
    }
    _amber = {
        "bg": QColor(0x10, 0x08, 0x00),
        "fg_normal": QColor(0xCC, 0x88, 0x00),
        "fg_bright": QColor(0xFF, 0xCC, 0x44),
        "fg_dark": QColor(0x44, 0x22, 0x00),
        "cursor": QColor(0xFF, 0xAA, 0x00),
        "oia_bg": QColor(0x20, 0x10, 0x00),
        "oia_text": QColor(0xCC, 0x77, 0x00),
        "blue": QColor(0x77, 0xBB, 0xFF),
        "red": QColor(0xFF, 0x55, 0x55),
        "pink": QColor(0xFF, 0x99, 0xFF),
        "green": QColor(0x33, 0xCC, 0x33),
        "turquoise": QColor(0x55, 0xFF, 0xFF),
        "yellow": QColor(0xFF, 0xFF, 0x55),
        "white": QColor(0xFF, 0xFF, 0xFF),
        "neutral": QColor(0xCC, 0x88, 0x00),
    }
    _paper = {
        "bg": QColor(0xF5, 0xF5, 0xE8),
        "fg_normal": QColor(0x00, 0x00, 0x00),
        "fg_bright": QColor(0x00, 0x00, 0x88),
        "fg_dark": QColor(0xCC, 0xCC, 0xCC),
        "cursor": QColor(0x00, 0x00, 0xFF),
        "oia_bg": QColor(0xDD, 0xDD, 0xCC),
        "oia_text": QColor(0x00, 0x00, 0x00),
        "blue": QColor(0x00, 0x00, 0xBB),
        "red": QColor(0xCC, 0x00, 0x00),
        "pink": QColor(0xAA, 0x00, 0xAA),
        "green": QColor(0x00, 0x88, 0x00),
        "turquoise": QColor(0x00, 0x88, 0x88),
        "yellow": QColor(0x88, 0x88, 0x00),
        "white": QColor(0x44, 0x44, 0x44),
        "neutral": QColor(0x00, 0x00, 0x00),
    }
    _palettes = {
        GREEN_PHOSPHOR: _green,
        AMBER_PHOSPHOR: _amber,
        WHITE_PAPER: _paper,
    }

    @classmethod
    def palette(cls, mode: str) -> dict:
        return cls._palettes.get(mode, cls._green)


# ---------------------------------------------------------------------------
# Screen Cell
# ---------------------------------------------------------------------------
@dataclass
class Cell:
    char: str = " "  # displayed character (space for attribute/null)
    attr: int = Attr.UNPROT_NORMAL
    is_attr: bool = False  # True = this cell IS an attribute byte
    ext_color: Optional[str] = None  # extended colour override (3279)


# ---------------------------------------------------------------------------
# 3278 Screen Buffer
# ---------------------------------------------------------------------------
class Screen3270:
    """Logical 3270 screen: 24×80 cells + field model."""

    def __init__(self):
        self.cells: List[Cell] = [Cell() for _ in range(BUFFER_SIZE)]
        self.cursor: int = 0  # linear buffer address
        self._reset()

    def _reset(self):
        """Clear to defaults."""
        for i, c in enumerate(self.cells):
            c.char = " "
            c.attr = Attr.UNPROT_NORMAL
            c.is_attr = False
            c.ext_color = None
        self.cursor = 0

    def clear(self):
        self._reset()

    # -- Address helpers --
    @staticmethod
    def addr(row: int, col: int) -> int:
        """Convert (0-based row, col) to linear address."""
        return row * COLS + col

    @staticmethod
    def row_col(addr: int) -> Tuple[int, int]:
        return divmod(addr, COLS)

    # -- Field model --
    def _attr_at(self, addr: int) -> int:
        """Walk backward from addr to find governing attribute."""
        n = BUFFER_SIZE
        for i in range(n):
            pos = (addr - i) % n
            if self.cells[pos].is_attr:
                return self.cells[pos].attr
        return Attr.UNPROT_NORMAL

    def set_attribute(self, addr: int, attr: int):
        c = self.cells[addr % BUFFER_SIZE]
        c.is_attr = True
        c.attr = attr
        c.char = " "

    def write_char(self, addr: int, ch: str, attr: int | None = None):
        pos = addr % BUFFER_SIZE
        c = self.cells[pos]
        c.is_attr = False
        c.char = ch if ch else " "
        if attr is not None:
            c.attr = attr
        # Set MDT on governing field when writing
        if not Attr.is_protected(self._attr_at(pos)):
            governing = self._find_attr_addr(pos)
            if governing is not None:
                self.cells[governing].attr = Attr.set_mdt(self.cells[governing].attr)

    def _find_attr_addr(self, addr: int) -> Optional[int]:
        n = BUFFER_SIZE
        for i in range(1, n + 1):
            pos = (addr - i) % n
            if self.cells[pos].is_attr:
                return pos
        return None

    def erase_eof(self, addr: int):
        """Erase from addr to end of field."""
        n = BUFFER_SIZE
        for i in range(n):
            pos = (addr + i) % n
            if self.cells[pos].is_attr:
                break
            self.cells[pos].char = " "

    def next_unprotected(self, addr: int) -> int:
        """Return address of first char of next unprotected field."""
        n = BUFFER_SIZE
        for i in range(1, n + 1):
            pos = (addr + i) % n
            c = self.cells[pos]
            if c.is_attr:
                if not Attr.is_protected(c.attr) and not Attr.is_dark(c.attr):
                    # Return the character position right after this attr
                    return (pos + 1) % n
        return addr  # no unprotected field found

    def prev_unprotected(self, addr: int) -> int:
        """Return address of first char of previous unprotected field."""
        n = BUFFER_SIZE
        for i in range(1, n + 1):
            pos = (addr - i) % n
            c = self.cells[pos]
            if c.is_attr and not Attr.is_protected(c.attr) and not Attr.is_dark(c.attr):
                return (pos + 1) % n
        return addr

    def get_field_data(self) -> List[Tuple[int, int, str]]:
        """Return list of (attr_addr, field_start_addr, value) for MDT fields."""
        result = []
        for i, c in enumerate(self.cells):
            if c.is_attr and not Attr.is_protected(c.attr) and Attr.is_mdt(c.attr):
                start = (i + 1) % BUFFER_SIZE
                data = []
                for j in range(1, BUFFER_SIZE):
                    pos = (start + j - 1) % BUFFER_SIZE
                    if self.cells[pos].is_attr:
                        break
                    data.append(self.cells[pos].char)
                result.append((i, start, "".join(data).rstrip()))
        return result

    def write_string(
        self,
        row: int,
        col: int,
        text: str,
        attr: int = Attr.PROT_BRIGHT,
        color: str | None = None,
    ):
        """Convenience: write a string starting at (row, col)."""
        addr = self.addr(row, col)
        for ch in text:
            if addr >= BUFFER_SIZE:
                break
            c = self.cells[addr]
            c.is_attr = False
            c.char = ch
            c.attr = attr
            c.ext_color = color
            addr += 1

    def place_field(
        self, row: int, col: int, attr: int, width: int = 0, color: str | None = None
    ):
        """Place an attribute byte and blank the field following it."""
        a = self.addr(row, col)
        self.set_attribute(a, attr)
        self.cells[a].ext_color = color
        for i in range(1, width + 1):
            pos = (a + i) % BUFFER_SIZE
            self.cells[pos].char = " "
            self.cells[pos].is_attr = False
            self.cells[pos].ext_color = color

    def read_field(self, row: int, col: int) -> str:
        """Read text from field starting at (row, col) up to next attr."""
        addr = self.addr(row, col)
        data = []
        for i in range(BUFFER_SIZE):
            pos = (addr + i) % BUFFER_SIZE
            if self.cells[pos].is_attr:
                break
            data.append(self.cells[pos].char)
        return "".join(data).rstrip()


# ---------------------------------------------------------------------------
# OIA (Operator Information Area) - row 25 of the terminal
# ---------------------------------------------------------------------------
class OIA:
    """Manages the 25th line status display."""

    def __init__(self):
        self.inhibited = False
        self.insert_mode = False
        self.message = ""
        self.system_lock = False
        self.connection = "A"  # connection indicator
        self.lu_name = "TWRP01  "  # Logical Unit name

    def render(self) -> str:
        """Produce an 80-char OIA string."""
        line = [" "] * 80
        # Column 0: connection indicator
        line[0] = self.connection
        # LU name columns 2-9
        for i, ch in enumerate(self.lu_name[:8]):
            line[2 + i] = ch
        # Status message centre
        msg = self.message[:30] if self.message else ""
        start = (80 - len(msg)) // 2
        for i, ch in enumerate(msg):
            line[start + i] = ch
        # Insert mode indicator column 65
        if self.insert_mode:
            for i, ch in enumerate("INSERT"):
                if 65 + i < 80:
                    line[65 + i] = ch
        # System lock
        if self.system_lock or self.inhibited:
            for i, ch in enumerate("X SYSTEM"):
                if 70 + i < 80:
                    line[70 + i] = ch
        # Cursor position far right
        return "".join(line)


# ---------------------------------------------------------------------------
# Main 3278 Terminal Widget
# ---------------------------------------------------------------------------
class Terminal3278(QWidget):
    """
    IBM 3278 Model 2 (24×80) terminal emulator widget.

    Signals:
        aid_key(key_name, field_data)  – emitted when operator presses an AID key
        status_changed(msg)             – OIA message changed
    """

    aid_key = Signal(str, list)  # (key, [(attr_addr, field_addr, value)…])
    status_changed = Signal(str)

    def __init__(self, parent=None, phosphor: str = Color3270.GREEN_PHOSPHOR):
        super().__init__(parent)
        self.screen = Screen3270()
        self.oia = OIA()
        self.phosphor = phosphor
        self.pal = Color3270.palette(phosphor)
        self._insert_mode = False
        self._cursor_blink_on = True
        self._char_w = 0
        self._char_h = 0
        self._font: QFont | None = None
        self._key_callbacks: dict[str, Callable] = {}

        self.setFocusPolicy(Qt.StrongFocus)
        self.setAttribute(Qt.WA_OpaquePaintEvent)
        self._init_font()

        # Cursor blink timer
        self._blink_timer = QTimer(self)
        self._blink_timer.timeout.connect(self._blink)
        self._blink_timer.start(530)

        self._show_welcome()

    # -- Font initialization --
    def _init_font(self):
        """Try IBM 3270 fonts, fall back to Courier."""
        from PySide6.QtWidgets import QApplication

        preferred = [
            "IBM 3270",
            "IBM 3270 Semi-Narrow",
            "3270",
            "Courier New",
            "Courier",
            "Lucida Console",
            "DejaVu Sans Mono",
            "Monospace",
        ]
        families = QFontDatabase.families() if QApplication.instance() else []
        chosen = "Courier New"
        for name in preferred:
            if any(name.lower() in f.lower() for f in families):
                chosen = name
                break
        self._font = QFont(chosen, 14)
        self._font.setStyleHint(QFont.TypeWriter)
        self._font.setFixedPitch(True)
        self._font.setBold(False)
        fm = QFontMetrics(self._font)
        self._char_w = fm.horizontalAdvance("M")
        self._char_h = fm.height()
        # Tell Qt the natural (minimum) size for the 80×25 grid
        self.setMinimumSize(
            self._char_w * COLS + 4,
            self._char_h * TOTAL_ROWS + 4,
        )
        # Resize the widget to exactly the 80×25 grid so the scroll area
        # knows the scrollable extent.
        self.resize(
            self._char_w * COLS + 4,
            self._char_h * TOTAL_ROWS + 4,
        )

    def set_font_size(self, pts: int):
        """Dynamically resize the terminal font."""
        self._font.setPointSize(pts)
        fm = QFontMetrics(self._font)
        self._char_w = fm.horizontalAdvance("M")
        self._char_h = fm.height()
        new_w = self._char_w * COLS + 4
        new_h = self._char_h * TOTAL_ROWS + 4
        self.setMinimumSize(new_w, new_h)
        # Resize so the parent QScrollArea updates its scrollable extent
        self.resize(new_w, new_h)
        self.update()

    def set_phosphor(self, mode: str):
        self.phosphor = mode
        self.pal = Color3270.palette(mode)
        self.update()

    # -- Internal helpers --
    def _blink(self):
        self._cursor_blink_on = not self._cursor_blink_on
        self.update()

    def _pos_to_addr(self, x: int, y: int) -> Optional[Tuple[int, int]]:
        """Convert pixel (x,y) to (row, col), or None if OIA."""
        col = (x - 2) // self._char_w
        row = (y - 2) // self._char_h
        if row < 0 or row >= ROWS or col < 0 or col >= COLS:
            return None
        return row, col

    def _show_welcome(self):
        """Paint the initial CICS logon screen."""
        s = self.screen
        s.clear()
        # Header
        s.write_string(0, 0, "=" * 80, Attr.PROT_BRIGHT, "turquoise")
        title = "IBM 3278 MODEL 2 TERMINAL - TIME WARP STUDIO"
        s.write_string(0, (80 - len(title)) // 2, title, Attr.PROT_BRIGHT, "turquoise")
        s.write_string(1, 0, " " * 80, Attr.PROT_NORMAL)
        subtitle = "CICS/TS  V5.6   TRANSACTION PROCESSING FACILITY"
        s.write_string(
            1, (80 - len(subtitle)) // 2, subtitle, Attr.PROT_BRIGHT, "yellow"
        )
        s.write_string(2, 0, "=" * 80, Attr.PROT_BRIGHT, "turquoise")
        # Logon fields
        s.write_string(5, 2, "USERID   :", Attr.PROT_BRIGHT, "green")
        s.place_field(5, 12, Attr.UNPROT_NORMAL, 8, "turquoise")
        s.write_string(7, 2, "PASSWORD :", Attr.PROT_BRIGHT, "green")
        s.place_field(7, 12, Attr.UNPROT_DARK, 8, "turquoise")
        s.write_string(9, 2, "TERMINAL :", Attr.PROT_NORMAL, "yellow")
        s.write_string(9, 13, "TW3278  ", Attr.PROT_NORMAL, "yellow")
        s.write_string(11, 2, "SYSTEM   :", Attr.PROT_NORMAL, "yellow")
        s.write_string(11, 13, "TIMEWRP01", Attr.PROT_NORMAL, "yellow")
        s.write_string(
            14, 2, "PF3=LOGOFF   PF9=MSGS   ENTER=LOGON", Attr.PROT_DIM, "blue"
        )
        s.write_string(23, 0, "=" * 80, Attr.PROT_BRIGHT, "turquoise")
        # Place cursor at userid field
        self.screen.cursor = Screen3270.addr(5, 13)
        self.oia.message = "ENTER USERID AND PASSWORD"
        self.update()

    # -- Screen management API (called by CICS executor) --
    def clear_screen(self):
        self.screen.clear()
        self.oia.message = ""
        self.update()

    def write_text(
        self,
        row: int,
        col: int,
        text: str,
        attr: int = Attr.PROT_BRIGHT,
        color: str | None = None,
    ):
        self.screen.write_string(row, col, text, attr, color)
        self.update()

    def define_field(
        self, row: int, col: int, attr: int, width: int, color: str | None = None
    ):
        self.screen.place_field(row, col, attr, width, color)
        self.update()

    def set_cursor(self, row: int, col: int):
        self.screen.cursor = Screen3270.addr(row, col)
        self._cursor_blink_on = True
        self.update()

    def get_field_value(self, row: int, col: int) -> str:
        return self.screen.read_field(row, col)

    def set_status(self, msg: str):
        self.oia.message = msg
        self.status_changed.emit(msg)
        self.update()

    def lock(self, locked: bool = True):
        self.oia.system_lock = locked
        self.update()

    def render_map(self, bms_output: str):
        """
        Parse the BMS map output format produced by CICSEnvironment.render_bms_map()
        and display it on screen using proper 3270 attributes.
        Lines prefixed '@FIELD:row,col,attr,length:' define input fields.
        Plain lines are text.
        """
        self.clear_screen()
        for line_no, raw in enumerate(bms_output.splitlines()):
            if line_no >= ROWS:
                break
            if raw.startswith("@FIELD:"):
                # @FIELD:row,col,attr,length:initial_text
                m = re.match(r"@FIELD:(\d+),(\d+),(\w+),(\d+):(.*)", raw)
                if m:
                    row, col = int(m.group(1)), int(m.group(2))
                    attr_name = m.group(3)
                    length = int(m.group(4))
                    initial = m.group(5)
                    attr = getattr(Attr, attr_name, Attr.UNPROT_NORMAL)
                    self.screen.place_field(row, col, attr, length)
                    if initial.strip():
                        self.screen.write_string(
                            row, col + 1, initial, Attr.UNPROT_NORMAL
                        )
            else:
                if raw:
                    self.screen.write_string(line_no, 0, raw[:COLS], Attr.PROT_BRIGHT)
        self.update()

    # -- Painting --
    def paintEvent(self, event):
        p = QPainter(self)
        pal = self.pal
        cw, ch = self._char_w, self._char_h
        p.setFont(self._font)
        p.fillRect(self.rect(), pal["bg"])

        screen = self.screen
        cursor_addr = screen.cursor

        for row in range(ROWS):
            y = 2 + row * ch
            for col in range(COLS):
                addr = row * COLS + col
                cell = screen.cells[addr]

                # Determine governing attribute
                if cell.is_attr:
                    continue  # attribute cells are invisible

                attr = screen._attr_at(addr)
                # Pick colour
                ext = cell.ext_color
                if ext and ext in pal:
                    fg = pal[ext]
                elif Attr.is_protected(attr):
                    if attr & Attr.BRIGHT:
                        fg = pal["blue"]
                    else:
                        fg = pal["fg_normal"]
                else:
                    if attr & Attr.NUMERIC:
                        fg = pal["turquoise"]
                    elif attr & Attr.BRIGHT:
                        fg = pal["green"]
                    else:
                        fg = pal["fg_normal"]

                if Attr.is_dark(attr) and not cell.is_attr:
                    # Non-display field – show bg only
                    ch_draw = " "
                    fg = pal["bg"]
                else:
                    ch_draw = cell.char

                x = 2 + col * cw

                # Cursor rendering (block cursor)
                is_cursor = addr == cursor_addr
                if is_cursor and self._cursor_blink_on:
                    p.fillRect(QRect(x, y, cw, ch), pal["cursor"])
                    p.setPen(pal["bg"])
                else:
                    p.setPen(fg)

                p.drawText(QRect(x, y, cw, ch), Qt.AlignLeft | Qt.AlignVCenter, ch_draw)

        # OIA row
        oia_y = 2 + ROWS * ch
        p.fillRect(QRect(0, oia_y - 2, self.width(), ch + 4), pal["oia_bg"])
        oia_text = self.oia.render()
        p.setPen(pal["oia_text"])
        p.drawText(
            QRect(2, oia_y, COLS * cw, ch), Qt.AlignLeft | Qt.AlignVCenter, oia_text
        )

        # Separator line
        p.setPen(QPen(pal["fg_normal"], 1))
        p.drawLine(0, oia_y - 3, self.width(), oia_y - 3)

    # -- Keyboard handling --
    def keyPressEvent(self, event: QKeyEvent):
        key = event.key()
        mod = event.modifiers()
        s = self.screen

        if self.oia.system_lock:
            self._handle_reset(event)
            return

        # --- AID keys ---
        if key == Qt.Key_Return or key == Qt.Key_Enter:
            self._fire_aid("ENTER")
            return
        if key == Qt.Key_Escape:
            self._fire_aid("CLEAR")
            return
        # PF keys via F1-F12 (and Shift+F1..F12 for PF13-PF24)
        if Qt.Key_F1 <= key <= Qt.Key_F12:
            fn = key - Qt.Key_F1 + 1
            if mod & Qt.ShiftModifier:
                fn += 12
            self._fire_aid(f"PF{fn}")
            return
        # PA keys via Alt+1, Alt+2, Alt+3
        if mod & Qt.AltModifier and key in (Qt.Key_1, Qt.Key_2, Qt.Key_3):
            pa = key - Qt.Key_0
            self._fire_aid(f"PA{pa}")
            return

        # --- Navigation ---
        if key == Qt.Key_Tab:
            new = s.next_unprotected(s.cursor)
            s.cursor = new
            self.update()
            return
        if key == Qt.Key_Backtab:
            new = s.prev_unprotected(s.cursor)
            s.cursor = new
            self.update()
            return
        if key == Qt.Key_Home:
            s.cursor = s.next_unprotected(0)
            self.update()
            return
        if key == Qt.Key_Up:
            row, col = Screen3270.row_col(s.cursor)
            s.cursor = Screen3270.addr(max(0, row - 1), col)
            self.update()
            return
        if key == Qt.Key_Down:
            row, col = Screen3270.row_col(s.cursor)
            s.cursor = Screen3270.addr(min(ROWS - 1, row + 1), col)
            self.update()
            return
        if key == Qt.Key_Left:
            s.cursor = (s.cursor - 1) % BUFFER_SIZE
            self.update()
            return
        if key == Qt.Key_Right:
            s.cursor = (s.cursor + 1) % BUFFER_SIZE
            self.update()
            return
        if key == Qt.Key_End:
            # Erase from cursor to end of field (ErEOF)
            attr = s._attr_at(s.cursor)
            if not Attr.is_protected(attr):
                s.erase_eof(s.cursor)
            self.update()
            return
        if key == Qt.Key_Insert:
            self._insert_mode = not self._insert_mode
            self.oia.insert_mode = self._insert_mode
            self.update()
            return
        if key == Qt.Key_Delete:
            attr = s._attr_at(s.cursor)
            if not Attr.is_protected(attr):
                self._delete_char(s.cursor)
            self.update()
            return
        if key == Qt.Key_Backspace:
            attr = s._attr_at(s.cursor)
            if not Attr.is_protected(attr) and s.cursor > 0:
                prev = (s.cursor - 1) % BUFFER_SIZE
                if not s.cells[prev].is_attr:
                    s.cursor = prev
                    self._delete_char(s.cursor)
            self.update()
            return

        # --- Character input ---
        text = event.text()
        if text and text.isprintable():
            attr = s._attr_at(s.cursor)
            if Attr.is_protected(attr):
                self.oia.message = "PROTECTED FIELD"
                self.oia.inhibited = True
                self.update()
                return
            self.oia.inhibited = False
            self.oia.message = ""
            if self._insert_mode:
                self._insert_char(s.cursor, text[0])
            else:
                s.write_char(s.cursor, text[0])
                s.cursor = (s.cursor + 1) % BUFFER_SIZE
                # Skip to next field if at attr byte
                if s.cells[s.cursor].is_attr:
                    s.cursor = (s.cursor + 1) % BUFFER_SIZE
            self._cursor_blink_on = True
            self.update()
            return

        super().keyPressEvent(event)

    def _insert_char(self, addr: int, ch: str):
        """Insert character at addr, shifting field right (drop last)."""
        s = self.screen
        # find end of field
        end = addr
        for i in range(BUFFER_SIZE):
            pos = (addr + i) % BUFFER_SIZE
            if s.cells[pos].is_attr:
                end = (pos - 1) % BUFFER_SIZE
                break
        # shift characters right
        for i in range(Screen3270.row_col(end)[1], Screen3270.row_col(addr)[1], -1):
            row, _ = Screen3270.row_col(addr)
            src = Screen3270.addr(row, i - 1)
            dst = Screen3270.addr(row, i)
            s.cells[dst].char = s.cells[src].char
        s.cells[addr].char = ch
        s.cursor = (addr + 1) % BUFFER_SIZE

    def _delete_char(self, addr: int):
        """Delete character at addr, shifting field left."""
        s = self.screen
        for i in range(BUFFER_SIZE - 1):
            pos = (addr + i) % BUFFER_SIZE
            nxt = (addr + i + 1) % BUFFER_SIZE
            if s.cells[nxt].is_attr:
                s.cells[pos].char = " "
                break
            s.cells[pos].char = s.cells[nxt].char

    def _handle_reset(self, event: QKeyEvent):
        """RESET key – unlock keyboard."""
        self.oia.system_lock = False
        self.oia.inhibited = False
        self.oia.message = ""
        self.update()

    def _fire_aid(self, key: str):
        """Collect modified field data and emit aid_key signal."""
        fields = self.screen.get_field_data()
        self.aid_key.emit(key, fields)
        self.oia.message = f"PROCESSING... ({key})"
        self.oia.system_lock = True
        self.update()

    def mousePressEvent(self, event):
        """Click-to-position cursor."""
        result = self._pos_to_addr(event.x(), event.y())
        if result:
            row, col = result
            addr = Screen3270.addr(row, col)
            cell = self.screen.cells[addr]
            if not cell.is_attr:
                self.screen.cursor = addr
                self._cursor_blink_on = True
                self.update()
        self.setFocus()

    def sizeHint(self) -> QSize:
        return QSize(self._char_w * COLS + 4, self._char_h * TOTAL_ROWS + 4)

    def resizeEvent(self, event: QResizeEvent):
        super().resizeEvent(event)


# ---------------------------------------------------------------------------
# Attr addendum – PROT_DIM missing in class above
# ---------------------------------------------------------------------------
Attr.PROT_DIM = Attr.PROTECTED | 0x00  # alias for normal protected


# ---------------------------------------------------------------------------
# 3278 Terminal Window (standalone dialog wrapping the widget)
# ---------------------------------------------------------------------------
class Terminal3278Window(QMainWindow):
    """
    Full application window containing the 3278 terminal widget,
    a toolbar for phosphor switching + font size, and an OIA status bar.
    """

    def __init__(self, parent=None, title: str = "IBM 3278 Terminal — CICS"):
        super().__init__(parent)
        self.setWindowTitle(title)
        self.terminal = Terminal3278(self)

        # Central widget
        cw = QWidget()
        layout = QVBoxLayout(cw)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)
        layout.addWidget(self.terminal)
        self.setCentralWidget(cw)

        self._build_toolbar()
        self._build_statusbar()

        self.terminal.aid_key.connect(self._on_aid)
        self.terminal.status_changed.connect(self._on_status)
        self.resize(self.terminal.sizeHint())

    def _build_toolbar(self):
        tb = QToolBar("3278 Controls")
        tb.setMovable(False)
        tb.setStyleSheet(
            "QToolBar { background: #111; border: none; } "
            "QToolButton { color: #0f0; background: #111; "
            "border: 1px solid #040; padding: 3px 6px; margin: 1px; }"
            "QToolButton:hover { background: #030; }"
        )
        self.addToolBar(tb)

        tb.addAction(
            "🟢 Green", lambda: self.terminal.set_phosphor(Color3270.GREEN_PHOSPHOR)
        )
        tb.addAction(
            "🟡 Amber", lambda: self.terminal.set_phosphor(Color3270.AMBER_PHOSPHOR)
        )
        tb.addAction(
            "📄 Paper", lambda: self.terminal.set_phosphor(Color3270.WHITE_PAPER)
        )
        tb.addSeparator()
        tb.addAction(
            "Font-",
            lambda: self.terminal.set_font_size(
                max(8, self.terminal._font.pointSize() - 1)
            ),
        )
        tb.addAction(
            "Font+",
            lambda: self.terminal.set_font_size(
                min(24, self.terminal._font.pointSize() + 1)
            ),
        )
        tb.addSeparator()
        tb.addAction("CLEAR", lambda: self.terminal._fire_aid("CLEAR"))
        tb.addAction("ENTER", lambda: self.terminal._fire_aid("ENTER"))
        # PF key quick-buttons
        for pf in (1, 2, 3, 4, 9, 12):
            tb.addAction(
                f"PF{pf}", (lambda n=pf: lambda: self.terminal._fire_aid(f"PF{n}"))()
            )
        tb.addSeparator()
        tb.addAction("PA1", lambda: self.terminal._fire_aid("PA1"))
        tb.addAction("PA2", lambda: self.terminal._fire_aid("PA2"))

    def _build_statusbar(self):
        sb = QStatusBar()
        sb.setStyleSheet(
            "QStatusBar { background: #050; color: #0f0; "
            "border-top: 1px solid #030; font-family: Courier New; }"
        )
        self.setStatusBar(sb)
        self._sb_label = QLabel("READY")
        self._sb_label.setStyleSheet("color: #0f0; font-family: Courier New;")
        sb.addWidget(self._sb_label)

    def _on_aid(self, key: str, fields: list):
        self._sb_label.setText(f"AID: {key}  Fields: {len(fields)}")

    def _on_status(self, msg: str):
        self._sb_label.setText(msg)

    def unlock(self):
        """Called by CICS executor when processing is complete."""
        self.terminal.lock(False)
        self.terminal.oia.message = ""
        self.terminal.update()
