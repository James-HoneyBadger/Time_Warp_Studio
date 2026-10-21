"""CHIP-8 language executor for Time Warp Studio.

CHIP-8 is Joseph Weisbecker's bytecode interpreter from 1977 — the machine the
first home computers were taught with. It is not a text language: a program is a
sequence of two-byte opcodes, and the display is a framebuffer of lit and unlit
pixels -- 64x32 in the original mode and 128x64 in SuperChip hi-res. So this
executor is two things in one file: a small **assembler** that turns mnemonics
into opcodes, and the **machine** that runs them.

Executed whole-program: the listing is assembled in full and then run, because an
opcode stream cannot be interpreted one line at a time.

    ; a comment
    start:  LD   V0, 0x41
            LD   I, hexfont
            LD   F, V0
            DRW  V0, V1, 5
            HALT
    hexfont: DW  0x0000

Directives: ``DB`` (one byte), ``DW`` (one word), ``HALT`` (stop), and
``; keys <digits>`` to present keypad state for the run.

Numbers are decimal unless marked, and marked either way round because both
spellings appear in listings: ``0x41`` and ``41h`` are both 65, and a bare ``41``
is decimal 41.

The instruction set is the complete original -- all 35 opcodes, including the
arithmetic group, the random and timing registers, the font and BCD conversions,
and memory block load/store -- plus the SuperChip 1.1 extensions a hi-res ROM
needs: 00FE/00FF for the screen mode, 00Cn/00FB/00FC to scroll, 00FD to exit,
DXY0's 16x16 sprite, FX30's large font and the FX75/FX85 flag registers. The
MegaChip plane and audio extensions are not implemented.

Where the original is ambiguous, this follows the 1977 interpreter rather than a
later emulator, because that is the machine the books describe:

  * **``SHR``/``SHL`` shift ``Vx`` in place.** The later convention of shifting
    ``Vy`` and storing into ``Vx`` is not used here; the y operand is accepted and
    ignored, so both spellings assemble.
  * **Sprites wrap, and a collision sets ``VF``.** Drawing a lit pixel onto a lit
    pixel clears it and reports the collision.
  * **``LD [I], Vx`` and ``LD Vx, [I]`` leave ``I`` one past the block.** The
    1977 interpreter increments I; CHIP-48 and SCHIP leave it where it was. This
    machine is the 1977 one, as it is for the shifts and the sprite wrap.
  * **``SYS`` (``0NNN``) is ignored.** It addresses an RCA 1802 routine the
    interpreter cannot run; refusing the opcode would make a ROM that contains
    one unrunnable, so every interpreter since the original ignores it.

Two limits are this implementation's, and both are reported rather than silently
applied:

  * **The hex keypad is presented by directive, not by typing.** A program runs to
    completion inside one executor call, and the engine has no way to hand input
    to something that never yields control. ``; keys 13`` marks keys 1 and 3 as
    held down. ``LD Vx, K`` therefore reads the first held key, or 0.
  * **Frames are simulated.** Ten opcodes make a frame and the timers tick once
    per frame, with no real clock, so a program runs as fast as the budget allows
    and a fixed number of frames is the limit.

The display is drawn in the canvas's own coordinates with filled rectangles, and
its two colours come from ``set_palette_color``. ``set_svga_mode`` says the
machine's resolution; the native canvas honours it.

A source containing a NUL byte is a **binary ROM** rather than a listing: it is
copied to 0x200 unchanged, up to the 3584 bytes that fit before the end of
memory. The reference engine and the C++ IDE agree on that rule because they share
the same assembly entry point's contract.
"""

from __future__ import annotations

import random
import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


class Chip8Error(Exception):
    """Raised while assembling or running; reported with the ❌ prefix."""


#: The mnemonics this assembler accepts, for the command inventory.
CHIP8_COMMANDS = {
    "CLS",
    "RET",
    "HALT",
    "SYS",
    "JP",
    "CALL",
    "SE",
    "SNE",
    "LD",
    "ADD",
    "OR",
    "AND",
    "XOR",
    "SUB",
    "SHR",
    "SUBN",
    "SHL",
    "RND",
    "DRW",
    "SKP",
    "SKNP",
    "DB",
    "DW",
}

#: Screen geometry and the scale a CHIP-8 pixel is drawn at. Hi-res is exactly
#: twice the original in each direction, so the canvas keeps its 2:1 shape and a
#: hi-res pixel is half the size.
SCREEN_WIDTH = 64
SCREEN_HEIGHT = 32
HIRES_WIDTH = 128
HIRES_HEIGHT = 64
PIXEL_SCALE = 8

#: Simulation budget. Ten opcodes to a frame and the timers ticking once per
#: frame means 600 frames is about ten seconds of 60 Hz, with no real clock
#: involved.
CYCLES_PER_FRAME = 10
MAX_FRAMES = 600
MAX_STEPS = 1_000_000

#: Palette slots, so the display's colours are named rather than splashed about.
_OFF_INDEX = 1
_ON_INDEX = 2

#: The 4x5 hex font the machine expects to find in memory at 0x50, in the order
#: 0-F. This is the standard set every CHIP-8 program is written against.
_FONT = (
    0xF0,
    0x90,
    0x90,
    0x90,
    0xF0,  # 0
    0x20,
    0x60,
    0x20,
    0x20,
    0x70,  # 1
    0xF0,
    0x10,
    0xF0,
    0x80,
    0xF0,  # 2
    0xF0,
    0x10,
    0xF0,
    0x10,
    0xF0,  # 3
    0x90,
    0x90,
    0xF0,
    0x10,
    0x10,  # 4
    0xF0,
    0x80,
    0xF0,
    0x10,
    0xF0,  # 5
    0xF0,
    0x80,
    0xF0,
    0x90,
    0xF0,  # 6
    0xF0,
    0x10,
    0x20,
    0x40,
    0x40,  # 7
    0xF0,
    0x90,
    0xF0,
    0x90,
    0xF0,  # 8
    0xF0,
    0x90,
    0xF0,
    0x10,
    0xF0,  # 9
    0xF0,
    0x90,
    0xF0,
    0x90,
    0x90,  # A
    0xE0,
    0x90,
    0xE0,
    0x90,
    0xE0,  # B
    0xF0,
    0x80,
    0x80,
    0x80,
    0xF0,  # C
    0xE0,
    0x90,
    0x90,
    0x90,
    0xE0,  # D
    0xF0,
    0x80,
    0xF0,
    0x80,
    0xF0,  # E
    0xF0,
    0x80,
    0xF0,
    0x80,
    0x80,  # F
)

_FONT_ADDRESS = 0x50
_LARGE_FONT_ADDRESS = _FONT_ADDRESS + 16 * 5
_PROGRAM_ADDRESS = 0x200
_MEMORY_SIZE = 4096


def _large_font() -> bytes:
    """The 8x10 digits FX30 points at, made by doubling the 4x5 set.

    SuperChip's large font is ten bytes per digit: each source row becomes two
    rows and each source column two columns. Deriving it from the small font
    rather than embedding a second table is what keeps the two from disagreeing
    about what a digit looks like.
    """
    out = bytearray()
    for digit in range(10):
        for row in _FONT[digit * 5 : (digit + 1) * 5]:
            doubled = 0
            for column in range(4):
                if row & (0x80 >> column):
                    doubled |= 0xC0 >> (column * 2)
            out.append(doubled)
            out.append(doubled)
    return bytes(out)


_LARGE_FONT = _large_font()

_REGISTER_RE = re.compile(r"[Vv]([0-9A-Fa-f])")
_LABEL_RE = re.compile(r"^\s*([A-Za-z_]\w*)\s*:")
_OPERAND_RE = re.compile(r"[,\s]+")


def _byte(text: str) -> int:
    value = _number(text)
    if not 0 <= value <= 0xFF:
        raise Chip8Error(f"{text} is not a byte")
    return value


def _word(text: str) -> int:
    value = _number(text)
    if not 0 <= value <= 0xFFFF:
        raise Chip8Error(f"{text} is not a 16-bit value")
    return value


def _number(text: str) -> int:
    """A literal, decimal unless it says otherwise.

    Both hex spellings are accepted because both appear in CHIP-8 listings: the
    C one (``0x41``) and the assembler one (``41h``). A bare number is decimal,
    so ``LD V0, 41`` is 0x29 — which is why the spellings exist.
    """
    stripped = text.strip()
    body = stripped[:-1] if stripped.lower().endswith("h") else stripped
    base = 16 if body != stripped or stripped.lower().startswith("0x") else 10
    try:
        return int(body, base)
    except ValueError as exc:
        raise Chip8Error(f"{text!r} is not a number") from exc


def _register(text: str) -> int:
    match = _REGISTER_RE.fullmatch(text.strip())
    if match is None:
        raise Chip8Error(f"{text!r} is not a register (V0-VF)")
    return int(match.group(1), 16)


def _code_of(line: str) -> str:
    """Strip a comment. ``;`` starts one, as it does in the listings people read."""
    index = line.find(";")
    return (line if index < 0 else line[:index]).strip()


# ---------------------------------------------------------------------------
# Assembler
# ---------------------------------------------------------------------------


def assemble(source: str) -> tuple[bytearray, dict[str, int], int]:
    """Assemble *source* into memory.

    Returns ``(memory, labels, entry_address)``. Two passes, because a jump
    forward to a label cannot be encoded until the labels are known.
    """
    memory = bytearray(_MEMORY_SIZE)
    memory[_FONT_ADDRESS : _FONT_ADDRESS + len(_FONT)] = bytes(_FONT)
    memory[_LARGE_FONT_ADDRESS : _LARGE_FONT_ADDRESS + len(_LARGE_FONT)] = _LARGE_FONT

    # A NUL byte is not a thing a listing contains and is in every real ROM, so
    # it decides which of the two loaders runs.
    if "\x00" in source:
        if len(source) > _MEMORY_SIZE - _PROGRAM_ADDRESS:
            raise Chip8Error(
                f"the ROM is {len(source)} bytes; memory from 0x200 holds "
                f"{_MEMORY_SIZE - _PROGRAM_ADDRESS}"
            )
        memory[_PROGRAM_ADDRESS : _PROGRAM_ADDRESS + len(source)] = bytes(
            ord(character) & 0xFF for character in source
        )
        return memory, {}, _PROGRAM_ADDRESS

    lines = source.splitlines()
    labels: dict[str, int] = {}

    # Pass one: find every label and where it lands.
    address = _PROGRAM_ADDRESS
    for number, line in enumerate(lines, start=1):
        text = _code_of(line)
        if not text:
            continue
        label = _LABEL_RE.match(text)
        if label:
            name = label.group(1)
            if name in labels:
                raise Chip8Error(f"line {number}: label {name} is defined twice")
            labels[name] = address
            text = text[label.end() :].strip()
            if not text:
                continue
        address += _size_of(text)

    if address > _MEMORY_SIZE:
        raise Chip8Error(
            f"the program needs {address - _PROGRAM_ADDRESS} bytes and memory "
            f"ends at 0x{_MEMORY_SIZE:04X}"
        )

    # Pass two: emit.
    address = _PROGRAM_ADDRESS
    for number, line in enumerate(lines, start=1):
        text = _code_of(line)
        if not text:
            continue
        label = _LABEL_RE.match(text)
        if label:
            text = text[label.end() :].strip()
            if not text:
                continue
        emitted = _encode(text, labels, number)
        memory[address : address + len(emitted)] = bytes(emitted)
        address += len(emitted)

    return memory, labels, _PROGRAM_ADDRESS


def _size_of(text: str) -> int:
    """How many bytes a statement emits, without encoding it."""
    head = text.split(maxsplit=1)[0].upper()
    if head == "DB":
        return 1
    return 2


def _encode(text: str, labels: dict[str, int], number: int) -> list[int]:
    """One assembled statement, as bytes."""
    head, _, rest = text.partition(" ")
    mnemonic = head.upper()
    operands = [part for part in _OPERAND_RE.split(rest.strip()) if part]

    def target(text: str) -> int:
        """An address operand: a label, or a number."""
        if text in labels:
            return labels[text]
        return _word(text)

    def fail(expectation: str) -> "Chip8Error":
        return Chip8Error(f"line {number}: {mnemonic} {expectation}")

    if mnemonic == "DB":
        if len(operands) != 1:
            raise fail("takes one byte")
        return [_byte(operands[0])]
    if mnemonic == "DW":
        if len(operands) != 1:
            raise fail("takes one word")
        value = _word(operands[0])
        return [value >> 8, value & 0xFF]

    if mnemonic == "CLS":
        return [0x00, 0xE0]
    if mnemonic == "RET":
        return [0x00, 0xEE]
    if mnemonic == "SYS":
        # 0NNN calls an RCA 1802 routine. The machine ignores it; the assembler
        # can still spell it so a listing that contains one assembles.
        if len(operands) != 1:
            raise fail("takes an address")
        value = target(operands[0])
        return [value >> 8, value & 0xFF]
    if mnemonic == "HALT":
        # 0x0000 is unused by the original set, so a program can stop instead of
        # spinning on a jump-to-self to mark the end.
        return [0x00, 0x00]

    if mnemonic == "JP" and len(operands) == 1:
        value = target(operands[0])
        return [0x10 | (value >> 8), value & 0xFF]
    if mnemonic == "JP" and len(operands) == 2:
        # Bnnn carries the *address* in its twelve bits; V0 is added at run time
        # rather than encoded. ORing the register number into the high nibble
        # instead produced B006 for a target of 0x206, which jumped to 6.
        if _register(operands[0]) != 0:
            raise fail("uses V0 only; Bnnn adds V0")
        value = target(operands[1])
        return [0xB0 | (value >> 8), value & 0xFF]
    if mnemonic == "CALL":
        if len(operands) != 1:
            raise fail("takes an address")
        value = target(operands[0])
        return [0x20 | (value >> 8), value & 0xFF]

    if mnemonic in ("SE", "SNE") and len(operands) == 2:
        x = _register(operands[0])
        if _REGISTER_RE.fullmatch(operands[1].strip()):
            y = _register(operands[1])
            base = 0x50 if mnemonic == "SE" else 0x90
            return [(base | x), (y << 4)]
        base = 0x30 if mnemonic == "SE" else 0x40
        return [base | x, _byte(operands[1])]

    if mnemonic == "LD" and len(operands) == 2:
        left, right = operands[0].strip().upper(), operands[1].strip().upper()
        if left == "I":
            value = target(operands[1])
            return [0xA0 | (value >> 8), value & 0xFF]
        # The conversions and the timers name the *destination* on the left, so
        # `LD F, V0` reads "point I at the glyph for V0" — the register is the
        # operand on the right, not the one being loaded.
        if left in ("DT", "ST"):
            x = _register(operands[1])
            return [0xF0 | x, 0x15 if left == "DT" else 0x18]
        if left == "F":
            x = _register(operands[1])
            return [0xF0 | x, 0x29]
        if left == "B":
            x = _register(operands[1])
            return [0xF0 | x, 0x33]
        if left == "[I]":
            x = _register(operands[1])
            return [0xF0 | x, 0x55]
        if right == "[I]":
            x = _register(operands[0])
            return [0xF0 | x, 0x65]
        if right == "DT":
            x = _register(operands[0])
            return [0xF0 | x, 0x07]
        if right == "K":
            x = _register(operands[0])
            return [0xF0 | x, 0x0A]
        x = _register(operands[0])
        if _REGISTER_RE.fullmatch(operands[1].strip()):
            return [0x80 | x, (_register(operands[1]) << 4)]
        return [0x60 | x, _byte(operands[1])]

    if mnemonic == "ADD" and len(operands) == 2:
        left, right = operands
        if left.strip().upper() == "I":
            x = _register(right)
            return [0xF0 | x, 0x1E]
        x = _register(left)
        if _REGISTER_RE.fullmatch(right.strip()):
            return [0x80 | x, (_register(right) << 4) | 0x4]
        return [0x70 | x, _byte(right)]

    if mnemonic in ("OR", "AND", "XOR", "SUB", "SUBN") and len(operands) == 2:
        x, y = _register(operands[0]), _register(operands[1])
        low = {"OR": 0x1, "AND": 0x2, "XOR": 0x3, "SUB": 0x5, "SUBN": 0x7}[mnemonic]
        return [0x80 | x, (y << 4) | low]

    if mnemonic in ("SHR", "SHL") and operands:
        # The y operand is accepted and ignored: the 1977 machine shifts Vx in
        # place, and a listing written for a later emulator still assembles.
        x = _register(operands[0])
        return [0x80 | x, 0x06 if mnemonic == "SHR" else 0x0E]

    if mnemonic == "RND" and len(operands) == 2:
        x = _register(operands[0])
        return [0xC0 | x, _byte(operands[1])]

    if mnemonic == "DRW" and len(operands) == 3:
        x, y = _register(operands[0]), _register(operands[1])
        rows = _byte(operands[2])
        if rows > 15:
            raise fail("draws at most 15 rows")
        return [0xD0 | x, (y << 4) | rows]

    if mnemonic in ("SKP", "SKNP") and len(operands) == 1:
        x = _register(operands[0])
        return [0xE0 | x, 0x9E if mnemonic == "SKP" else 0xA1]

    raise fail("is not an instruction form this assembler knows")


# ---------------------------------------------------------------------------
# Machine
# ---------------------------------------------------------------------------


class Machine:
    """The CHIP-8 interpreter: registers, memory, timers and framebuffer."""

    def __init__(self, memory: bytearray, entry: int, keys: int = 0) -> None:
        self.memory = memory
        self.v = [0] * 16
        self.i = 0
        self.pc = entry
        self.stack: list[int] = []
        self.delay_timer = 0
        self.sound_timer = 0
        self.keys = keys
        self.hi_res = False
        #: The eight (sixteen here) RPL flags FX75 stores and FX85 loads.
        self.rpl = bytearray(16)
        self.framebuffer = bytearray(self.width * self.height)
        self.halted = False
        self.cycles = 0
        self.collisions = 0

    @property
    def width(self) -> int:
        """The framebuffer's width: 128 in hi-res, 64 otherwise."""
        return HIRES_WIDTH if self.hi_res else SCREEN_WIDTH

    @property
    def height(self) -> int:
        """The framebuffer's height: 64 in hi-res, 32 otherwise."""
        return HIRES_HEIGHT if self.hi_res else SCREEN_HEIGHT

    # -- helpers ---------------------------------------------------------
    def _fetch(self) -> int:
        if self.pc + 1 >= _MEMORY_SIZE:
            raise Chip8Error("the program counter ran past the end of memory")
        opcode = (self.memory[self.pc] << 8) | self.memory[self.pc + 1]
        self.pc += 2
        return opcode

    def _skip(self) -> None:
        self.pc += 2

    def _pixel(self, x: int, y: int) -> bool:
        return (
            x < self.width
            and y < self.height
            and bool(self.framebuffer[y * self.width + x])
        )

    def _set_pixel(self, x: int, y: int, on: bool) -> None:
        if x < self.width and y < self.height:
            self.framebuffer[y * self.width + x] = 1 if on else 0

    def _first_key(self) -> int:
        for index in range(16):
            if self.keys & (1 << index):
                return index
        return 0

    # -- the interpreter -------------------------------------------------
    def step(self) -> None:
        """Run one opcode."""
        self.cycles += 1
        opcode = self._fetch()
        high = opcode >> 12
        x = (opcode >> 8) & 0xF
        y = (opcode >> 4) & 0xF
        low_byte = opcode & 0xFF
        low_nibble = opcode & 0xF

        if opcode == 0x0000:
            self.halted = True
            return
        if opcode == 0x00E0:
            self.framebuffer = bytearray(self.width * self.height)
            return
        if opcode == 0x00EE:
            if not self.stack:
                raise Chip8Error("RET with an empty call stack")
            self.pc = self.stack.pop()
            return
        if opcode == 0x00FD:
            # SuperChip's "exit interpreter": HALT in one opcode.
            self.halted = True
            return
        if opcode == 0x00FE or opcode == 0x00FF:
            self._set_mode(opcode == 0x00FF)
            return
        if (opcode & 0xFFF0) == 0x00C0:
            self._scroll_down(opcode & 0xF)
            return
        if opcode == 0x00FB:
            self._scroll_left(-4)
            return
        if opcode == 0x00FC:
            self._scroll_left(4)
            return
        if high == 0x0:
            # SYS: an RCA 1802 routine, which this machine cannot execute. Every
            # interpreter since the original ignores it.
            return

        if high == 0x1:
            self.pc = opcode & 0xFFF
        elif high == 0x2:
            self.stack.append(self.pc)
            self.pc = opcode & 0xFFF
        elif high == 0x3:
            if self.v[x] == low_byte:
                self._skip()
        elif high == 0x4:
            if self.v[x] != low_byte:
                self._skip()
        elif high == 0x5:
            if self.v[x] == self.v[y]:
                self._skip()
        elif high == 0x6:
            self.v[x] = low_byte
        elif high == 0x7:
            self.v[x] = (self.v[x] + low_byte) & 0xFF
        elif high == 0x8:
            self._arithmetic(x, y, low_nibble)
        elif high == 0x9:
            if self.v[x] != self.v[y]:
                self._skip()
        elif high == 0xA:
            self.i = opcode & 0xFFF
        elif high == 0xB:
            self.pc = (opcode & 0xFFF) + self.v[0]
        elif high == 0xC:
            self.v[x] = random.randint(0, 255) & low_byte
        elif high == 0xD:
            self._draw(x, y, low_nibble)
        elif high == 0xE:
            if low_byte == 0x9E and self.keys & (1 << self.v[x]):
                self._skip()
            elif low_byte == 0xA1 and not self.keys & (1 << self.v[x]):
                self._skip()
        elif high == 0xF:
            self._misc(x, low_byte)
        else:
            raise Chip8Error(f"unknown opcode 0x{opcode:04X} at 0x{self.pc - 2:03X}")

    def _set_mode(self, hi_res: bool) -> None:
        """Switch screen mode and clear: the two modes share no pixels.

        The original machine has no instruction for this; SuperChip's 00FE and
        00FF do, and a ROM that changes mode expects a blank screen rather than
        the old drawing stretched or cropped.
        """
        self.hi_res = hi_res
        self.framebuffer = bytearray(self.width * self.height)

    def _scroll_down(self, rows: int) -> None:
        rows = min(rows, self.height)
        if rows <= 0:
            return
        width = self.width
        for y in range(self.height - 1, rows - 1, -1):
            source = (y - rows) * width
            target = y * width
            self.framebuffer[target : target + width] = self.framebuffer[
                source : source + width
            ]
        self.framebuffer[: rows * width] = bytes(rows * width)

    def _scroll_left(self, amount: int) -> None:
        """Shift every row by *amount*: positive is left, negative is right."""
        width = self.width
        shift = min(abs(amount), width)
        blank = bytes(shift)
        for y in range(self.height):
            row = y * width
            if amount > 0:
                self.framebuffer[row : row + width - shift] = self.framebuffer[
                    row + shift : row + width
                ]
                self.framebuffer[row + width - shift : row + width] = blank
            else:
                self.framebuffer[row + shift : row + width] = self.framebuffer[
                    row : row + width - shift
                ]
                self.framebuffer[row : row + shift] = blank

    def _arithmetic(self, x: int, y: int, low: int) -> None:
        if low == 0x0:
            self.v[x] = self.v[y]
        elif low == 0x1:
            self.v[x] |= self.v[y]
        elif low == 0x2:
            self.v[x] &= self.v[y]
        elif low == 0x3:
            self.v[x] ^= self.v[y]
        elif low == 0x4:
            total = self.v[x] + self.v[y]
            self.v[x] = total & 0xFF
            self.v[0xF] = 1 if total > 0xFF else 0
        elif low == 0x5:
            self.v[0xF] = 1 if self.v[x] >= self.v[y] else 0
            self.v[x] = (self.v[x] - self.v[y]) & 0xFF
        elif low == 0x6:
            # Vx shifted in place, as the 1977 interpreter does.
            self.v[0xF] = self.v[x] & 0x1
            self.v[x] >>= 1
        elif low == 0x7:
            self.v[0xF] = 1 if self.v[y] >= self.v[x] else 0
            self.v[x] = (self.v[y] - self.v[x]) & 0xFF
        elif low == 0xE:
            self.v[0xF] = 1 if self.v[x] & 0x80 else 0
            self.v[x] = (self.v[x] << 1) & 0xFF
        else:
            raise Chip8Error(f"unknown arithmetic opcode 8xy{low:X}")

    def _draw(self, x: int, y: int, rows: int) -> None:
        """XOR a sprite at (Vx, Vy); VF reports a collision.

        ``rows == 0`` is SuperChip's large sprite: 16 rows of 16 bits in hi-res.
        In the original mode the same opcode is an 8x16 sprite with each column
        doubled, which is what SCHIP draws there and what a lores ROM expects.
        """
        start_x = self.v[x] % self.width
        start_y = self.v[y] % self.height
        wide = 8
        doubled = False
        if rows == 0:
            rows = 16
            if self.hi_res:
                wide = 16
            else:
                doubled = True
        self.v[0xF] = 0
        for row in range(rows):
            if wide == 16:
                high_byte = self.memory[(self.i + row * 2) % _MEMORY_SIZE]
                low_byte_ = self.memory[(self.i + row * 2 + 1) % _MEMORY_SIZE]
                bits = (high_byte << 8) | low_byte_
            else:
                bits = self.memory[(self.i + row) % _MEMORY_SIZE]
            for source_column in range(wide):
                mask = 0x8000 >> source_column if wide == 16 else 0x80 >> source_column
                if not bits & mask:
                    continue
                for repeat in range(2 if doubled else 1):
                    column = source_column * (2 if doubled else 1) + repeat
                    pixel_x = (start_x + column) % self.width
                    pixel_y = (start_y + row) % self.height
                    if self._pixel(pixel_x, pixel_y):
                        self.v[0xF] = 1
                        self.collisions += 1
                    self._set_pixel(pixel_x, pixel_y, not self._pixel(pixel_x, pixel_y))

    def _misc(self, x: int, low: int) -> None:
        if low == 0x07:
            self.v[x] = self.delay_timer
        elif low == 0x0A:
            # Blocking on a real key press cannot work here: see the module
            # docstring. The first held key, or 0.
            self.v[x] = self._first_key()
        elif low == 0x15:
            self.delay_timer = self.v[x]
        elif low == 0x18:
            self.sound_timer = self.v[x]
        elif low == 0x1E:
            self.i = (self.i + self.v[x]) & 0xFFF
        elif low == 0x29:
            self.i = _FONT_ADDRESS + (self.v[x] & 0xF) * 5
        elif low == 0x30:
            self.i = _LARGE_FONT_ADDRESS + (self.v[x] & 0xF) * 10
        elif low == 0x33:
            self.memory[self.i % _MEMORY_SIZE] = self.v[x] // 100
            self.memory[(self.i + 1) % _MEMORY_SIZE] = (self.v[x] // 10) % 10
            self.memory[(self.i + 2) % _MEMORY_SIZE] = self.v[x] % 10
        elif low == 0x55:
            for index in range(x + 1):
                self.memory[(self.i + index) % _MEMORY_SIZE] = self.v[index]
            # The 1977 interpreter leaves I one past the block.
            self.i = (self.i + x + 1) & 0xFFF
        elif low == 0x65:
            for index in range(x + 1):
                self.v[index] = self.memory[(self.i + index) % _MEMORY_SIZE]
            self.i = (self.i + x + 1) & 0xFFF
        elif low == 0x75:
            for index in range(min(x + 1, len(self.rpl))):
                self.rpl[index] = self.v[index]
        elif low == 0x85:
            for index in range(min(x + 1, len(self.rpl))):
                self.v[index] = self.rpl[index]
        else:
            raise Chip8Error(f"unknown opcode Fx{low:02X}")

    def tick_timers(self) -> None:
        """Advance both timers by one frame, which is 60 Hz on the real machine."""
        if self.delay_timer:
            self.delay_timer -= 1
        if self.sound_timer:
            self.sound_timer -= 1


# ---------------------------------------------------------------------------
# Display
# ---------------------------------------------------------------------------


class _Display:
    """Draws changed pixels onto the turtle canvas.

    A diff rather than a full repaint: a CHIP-8 frame touches a handful of the
    2048 pixels, and emitting all of them every frame would put a quarter of a
    million rectangles into the bridge for a ten-second program.
    """

    def __init__(self, turtle: "TurtleState") -> None:
        self.turtle = turtle
        turtle.set_svga_mode(SCREEN_WIDTH * PIXEL_SCALE, SCREEN_HEIGHT * PIXEL_SCALE)
        # CHIP-8 is a palette machine, so the two colours are palette entries.
        turtle.set_palette_color(_OFF_INDEX, 16, 16, 20)
        turtle.set_palette_color(_ON_INDEX, 140, 255, 140)
        self.off_colour = turtle.resolve_color(_OFF_INDEX)
        self.on_colour = turtle.resolve_color(_ON_INDEX)
        self.width = SCREEN_WIDTH
        self.height = SCREEN_HEIGHT
        self.previous = bytearray(self.width * self.height)

    def _rect(self, column: int, row: int, colour: Any) -> None:
        """One CHIP-8 pixel as a filled square, in canvas coordinates.

        CHIP-8 counts down from the top-left; the turtle canvas counts up from
        the middle, which is where the sign flip on y comes from.
        """
        width = SCREEN_WIDTH * PIXEL_SCALE
        height = SCREEN_HEIGHT * PIXEL_SCALE
        # A hi-res pixel is half the size: both modes fill the same canvas.
        scale = (SCREEN_WIDTH * PIXEL_SCALE) // self.width
        left = column * scale - width / 2
        top = height / 2 - row * scale
        self.turtle.draw_rect(left, top, left + scale, top - scale, fill_color=colour)

    def clear(self) -> None:
        """Paint the whole screen in the off colour."""
        width = SCREEN_WIDTH * PIXEL_SCALE
        height = SCREEN_HEIGHT * PIXEL_SCALE
        self.turtle.draw_rect(
            -width / 2, height / 2, width / 2, -height / 2, fill_color=self.off_colour
        )
        self.previous = bytearray(self.width * self.height)

    def paint(self, framebuffer: bytearray, width: int, height: int) -> None:
        """Emit only the pixels that changed since the last frame.

        A mode change repaints the whole screen: the previous frame's pixels are
        not comparable with the new grid even when the byte count matches.
        """
        if (width, height) != (self.width, self.height):
            self.width = width
            self.height = height
            self.previous = bytearray(width * height)
            self.clear()
        for index, value in enumerate(framebuffer):
            if value == self.previous[index]:
                continue
            self.previous[index] = value
            colour = self.on_colour if value else self.off_colour
            self._rect(index % self.width, index // self.width, colour)


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def _keypad(source: str) -> int:
    """Read the ``; keys`` directive: the keys held down for this run."""
    keys = 0
    for line in source.splitlines():
        stripped = line.strip()
        if not stripped.lower().startswith("; keys"):
            continue
        for character in stripped[len("; keys") :].strip():
            if character.upper() in "0123456789ABCDEF":
                keys |= 1 << int(character, 16)
    return keys


def execute_chip8(  # pylint: disable=unused-argument
    interpreter: "Interpreter | None",
    source: str,
    turtle: "TurtleState",
) -> str:
    """Assemble and run a CHIP-8 listing.

    ``interpreter`` is accepted to match the executor signature and is not used:
    CHIP-8 has no variables to read or write, so the machine is the whole state.
    The tests pass ``None`` to say so.
    """
    try:
        memory, _labels, entry = assemble(source)
    except Chip8Error as exc:
        return f"❌ CHIP-8: {exc}\n"

    machine = Machine(memory, entry, keys=_keypad(source))
    display = _Display(turtle)
    display.clear()

    frames = 0
    try:
        while not machine.halted:
            if machine.cycles >= MAX_STEPS:
                return (
                    "❌ CHIP-8: exceeded the cycle limit "
                    f"({MAX_STEPS} opcodes) without reaching HALT\n"
                )
            for _ in range(CYCLES_PER_FRAME):
                if machine.halted:
                    break
                machine.step()
            machine.tick_timers()
            display.paint(machine.framebuffer, machine.width, machine.height)
            frames += 1
            if frames >= MAX_FRAMES:
                # Not an error: a loop that never halts is a legitimate CHIP-8
                # program (an animation, or a game waiting for a key). Saying so
                # is the difference between "it finished" and "it stopped".
                return (
                    f"ℹ️ CHIP-8: stopped after {MAX_FRAMES} frames "
                    f"({machine.cycles} opcodes); the program had not halted\n"
                )
    except Chip8Error as exc:
        return f"❌ CHIP-8: {exc}\n"

    return ""
