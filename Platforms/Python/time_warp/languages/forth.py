from __future__ import annotations

import math as _fmath
from typing import TYPE_CHECKING, Callable, Dict, List, Optional, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


class ForthExecutor:
    def __init__(self, interpreter: "Interpreter"):
        self.interpreter = interpreter
        self.stack: List[Any] = []
        self.return_stack: List[Any] = []
        self.fstack: List[float] = []          # floating-point stack
        self.dictionary: Dict[str, Callable] = {}
        self.memory: List[int] = [0] * 65536   # 64K cells
        self.here: int = 0                     # next free memory address
        self.compiling = False
        self.new_word_name = ""
        self.new_word_definition: List[str] = []
        self.output_buffer = ""
        self.turtle: Optional["TurtleState"] = None
        self.base: int = 10                    # numeric base
        self.constants: Dict[str, Any] = {}
        self.values: Dict[str, Any] = {}       # VALUE words
        self.deferred: Dict[str, str] = {}     # DEFER words (name -> word name)

        self._init_dictionary()

    def _init_dictionary(self):
        d = self.dictionary

        # ── Stack manipulation ────────────────────────────────────────────
        d["DUP"] = self._dup
        d["DROP"] = self._drop
        d["SWAP"] = self._swap
        d["OVER"] = self._over
        d["ROT"] = self._rot
        d["-ROT"] = self._neg_rot
        d["NIP"] = self._nip
        d["TUCK"] = self._tuck
        d["2DUP"] = self._2dup
        d["2DROP"] = self._2drop
        d["2SWAP"] = self._2swap
        d["2OVER"] = self._2over
        d["?DUP"] = self._qdup
        d["DEPTH"] = self._depth
        d["PICK"] = self._pick
        d["ROLL"] = self._roll

        # ── Return stack ──────────────────────────────────────────────────
        d[">R"] = self._to_r
        d["R>"] = self._from_r
        d["R@"] = self._r_fetch
        d["RDROP"] = self._rdrop
        d["2>R"] = self._2_to_r
        d["2R>"] = self._2_from_r

        # ── Output ────────────────────────────────────────────────────────
        d["."] = self._dot
        d[".S"] = self._dot_s
        d["CR"] = self._cr
        d["EMIT"] = self._emit
        d["SPACE"] = self._space
        d["SPACES"] = self._spaces
        d["TYPE"] = self._type
        d["U."] = self._udot
        d[".R"] = self._dot_r
        d["?"] = self._question

        # ── Arithmetic ────────────────────────────────────────────────────
        d["+"] = self._add
        d["-"] = self._sub
        d["*"] = self._mul
        d["/"] = self._div
        d["MOD"] = self._mod
        d["/MOD"] = self._divmod
        d["*/"] = self._muldiv
        d["*/MOD"] = self._muldivmod
        d["ABS"] = self._abs
        d["NEGATE"] = self._negate
        d["MAX"] = self._max
        d["MIN"] = self._min
        d["1+"] = self._1plus
        d["1-"] = self._1minus
        d["2+"] = self._2plus
        d["2-"] = self._2minus
        d["2*"] = self._2mul
        d["2/"] = self._2div
        d["LSHIFT"] = self._lshift
        d["RSHIFT"] = self._rshift
        d["M+"] = self._madd

        # ── Comparisons ───────────────────────────────────────────────────
        d["="] = self._eq
        d["<>"] = self._ne
        d["<"] = self._lt
        d[">"] = self._gt
        d["<="] = self._le
        d[">="] = self._ge
        d["0="] = self._0eq
        d["0<"] = self._0lt
        d["0>"] = self._0gt
        d["0<>"] = self._0ne
        d["U<"] = self._ult
        d["U>"] = self._ugt

        # ── Logic / bitwise ───────────────────────────────────────────────
        d["AND"] = self._and
        d["OR"] = self._or
        d["XOR"] = self._xor
        d["INVERT"] = self._invert
        d["TRUE"] = self._true
        d["FALSE"] = self._false
        d["NOT"] = self._not

        # ── Memory ────────────────────────────────────────────────────────
        d["@"] = self._fetch
        d["!"] = self._store
        d["C@"] = self._cfetch
        d["C!"] = self._cstore
        d["2@"] = self._2fetch
        d["2!"] = self._2store
        d["+!"] = self._plus_store
        d["?"] = self._question

        # ── Floating-point ────────────────────────────────────────────────
        d["F+"] = self._fadd
        d["F-"] = self._fsub
        d["F*"] = self._fmul
        d["F/"] = self._fdiv
        d["FDUP"] = self._fdup
        d["FDROP"] = self._fdrop
        d["FSWAP"] = self._fswap
        d["FOVER"] = self._fover
        d["FNEGATE"] = self._fnegate
        d["FABS"] = self._fabs
        d["FMAX"] = self._fmax
        d["FMIN"] = self._fmin
        d["FSQRT"] = self._fsqrt
        d["FSIN"] = self._fsin
        d["FCOS"] = self._fcos
        d["FTAN"] = self._ftan
        d["FASIN"] = self._fasin
        d["FACOS"] = self._facos
        d["FATAN"] = self._fatan
        d["FATAN2"] = self._fatan2
        d["FEXP"] = self._fexp
        d["FLN"] = self._fln
        d["FLOG"] = self._flog
        d["FLOOR"] = self._ffloor
        d["FROUND"] = self._fround
        d["FTRUNCATE"] = self._ftrunc
        d["F="] = self._feq
        d["F<"] = self._flt
        d["F>"] = self._fgt
        d["F."] = self._fdot
        d["F0="] = self._f0eq
        d["F0<"] = self._f0lt
        d["S>F"] = self._s_to_f
        d["F>S"] = self._f_to_s
        d["D>F"] = self._d_to_f
        d["PI"] = self._fpi
        d["FLIT"] = self._flit

        # ── I/O ───────────────────────────────────────────────────────────
        d["KEY"] = self._key
        d["ACCEPT"] = self._accept
        d["WORD"] = self._word

        # ── System ────────────────────────────────────────────────────────
        d["BASE"] = self._push_base_addr
        d["DECIMAL"] = self._decimal
        d["HEX"] = self._hex
        d["BINARY"] = self._binary
        d["OCTAL"] = self._octal
        d["WORDS"] = self._words
        d["BYE"] = self._bye
        d["INCLUDE"] = self._include  # stub
        d["REQUIRE"] = self._include  # stub
        d["MS"] = self._ms           # millisecond delay stub
        d["HERE"] = self._here_word
        d["ALLOT"] = self._allot_word

        # ── String ────────────────────────────────────────────────────────
        d["COUNT"] = self._count
        d["COMPARE"] = self._compare
        d["MOVE"] = self._move_mem
        d["FILL"] = self._fill
        d["CMOVE"] = self._cmove
        d["CMOVE>"] = self._cmove_up
        d["CHAR"] = self._char_word   # handled in token loop but alias too
        d["+PLACE"] = self._plus_place

        # ── Graphics (Time Warp extensions) ───────────────────────────────
        d["FD"] = self._fd
        d["FORWARD"] = self._fd
        d["BK"] = self._bk
        d["BACK"] = self._bk
        d["RT"] = self._rt
        d["LT"] = self._lt_turn
        d["PU"] = self._pu
        d["PENUP"] = self._pu
        d["PD"] = self._pd
        d["PENDOWN"] = self._pd
        d["HOME"] = self._home
        d["CLEAN"] = self._clean
        d["CLEARSCREEN"] = self._clean
        d["CS"] = self._clean
        d["PEN"] = self._pen
        d["SETPENCOLOR"] = self._pen
        d["XCOR"] = self._xcor
        d["YCOR"] = self._ycor
        d["HEADING"] = self._heading
        d["COLOR"] = self._color
        d["CIRCLE"] = self._circle
        d["ARC"] = self._arc
        d["FILL"] = self._fill_gfx
        d["DOT"] = self._dot_gfx
        d["LABEL"] = self._label

    # ── Stack manipulation ──────────────────────────────────────────────

    def _dup(self):
        if self.stack: self.stack.append(self.stack[-1])

    def _drop(self):
        if self.stack: self.stack.pop()

    def _swap(self):
        if len(self.stack) >= 2:
            a, b = self.stack.pop(), self.stack.pop()
            self.stack += [a, b]

    def _over(self):
        if len(self.stack) >= 2: self.stack.append(self.stack[-2])

    def _rot(self):
        if len(self.stack) >= 3:
            c, b, a = self.stack.pop(), self.stack.pop(), self.stack.pop()
            self.stack += [b, c, a]

    def _neg_rot(self):
        if len(self.stack) >= 3:
            c, b, a = self.stack.pop(), self.stack.pop(), self.stack.pop()
            self.stack += [c, a, b]

    def _nip(self):
        if len(self.stack) >= 2:
            a = self.stack.pop(); self.stack.pop(); self.stack.append(a)

    def _tuck(self):
        if len(self.stack) >= 2:
            a, b = self.stack.pop(), self.stack.pop()
            self.stack += [a, b, a]

    def _2dup(self):
        if len(self.stack) >= 2: self.stack += [self.stack[-2], self.stack[-1]]

    def _2drop(self):
        if len(self.stack) >= 2: self.stack.pop(); self.stack.pop()

    def _2swap(self):
        if len(self.stack) >= 4:
            d, c, b, a = self.stack.pop(), self.stack.pop(), self.stack.pop(), self.stack.pop()
            self.stack += [c, d, a, b]

    def _2over(self):
        if len(self.stack) >= 4: self.stack += [self.stack[-4], self.stack[-3]]

    def _qdup(self):
        if self.stack and self.stack[-1] != 0: self.stack.append(self.stack[-1])

    def _depth(self):
        self.stack.append(len(self.stack))

    def _pick(self):
        if self.stack:
            n = self.stack.pop()
            if 0 <= n < len(self.stack): self.stack.append(self.stack[-(n + 1)])

    def _roll(self):
        if self.stack:
            n = self.stack.pop()
            if 0 < n < len(self.stack):
                v = self.stack[-(n + 1)]
                del self.stack[-(n + 1)]
                self.stack.append(v)

    # ── Return stack ───────────────────────────────────────────────────

    def _to_r(self):
        if self.stack: self.return_stack.append(self.stack.pop())

    def _from_r(self):
        if self.return_stack: self.stack.append(self.return_stack.pop())

    def _r_fetch(self):
        if self.return_stack: self.stack.append(self.return_stack[-1])

    def _rdrop(self):
        if self.return_stack: self.return_stack.pop()

    def _2_to_r(self):
        if len(self.stack) >= 2:
            b, a = self.stack.pop(), self.stack.pop()
            self.return_stack += [a, b]

    def _2_from_r(self):
        if len(self.return_stack) >= 2:
            b, a = self.return_stack.pop(), self.return_stack.pop()
            self.stack += [a, b]

    # ── Output ─────────────────────────────────────────────────────────

    def _dot(self):
        if self.stack:
            v = self.stack.pop()
            self.output_buffer += (f"{v:0X} " if self.base == 16 else
                                   f"{v:0b} " if self.base == 2  else
                                   f"{v:0o} " if self.base == 8  else
                                   f"{v} ")

    def _dot_s(self):
        self.output_buffer += f"<{len(self.stack)}> " + " ".join(map(str, self.stack)) + " "

    def _cr(self):
        self.interpreter.log_output(self.output_buffer)
        self.output_buffer = ""

    def _emit(self):
        if self.stack: self.output_buffer += chr(self.stack.pop() & 0xFF)

    def _space(self):
        self.output_buffer += " "

    def _spaces(self):
        if self.stack: self.output_buffer += " " * max(0, int(self.stack.pop()))

    def _type(self):
        # ( caddr u -- ) print u characters from c-addr (here: addr is index)
        if len(self.stack) >= 2:
            u = self.stack.pop(); addr = self.stack.pop()
            chars = []
            for off in range(u):
                a = addr + off
                if 0 <= a < len(self.memory): chars.append(chr(self.memory[a] & 0xFF))
            self.output_buffer += "".join(chars)

    def _udot(self):
        if self.stack: self.output_buffer += f"{self.stack.pop() & 0xFFFFFFFF} "

    def _dot_r(self):
        if len(self.stack) >= 2:
            w = self.stack.pop(); n = self.stack.pop()
            s = str(n); self.output_buffer += s.rjust(w)

    def _question(self):
        if self.stack:
            addr = self.stack.pop()
            if 0 <= addr < len(self.memory):
                self.output_buffer += f"{self.memory[addr]} "

    # ── Arithmetic ─────────────────────────────────────────────────────

    def _add(self):
        if len(self.stack) >= 2:
            self.stack.append(self.stack.pop() + self.stack.pop())
        else:
            self.interpreter.log_output("❌ Stack underflow")

    def _sub(self):
        if len(self.stack) >= 2:
            b, a = self.stack.pop(), self.stack.pop(); self.stack.append(a - b)
        else:
            self.interpreter.log_output("❌ Stack underflow")

    def _mul(self):
        if len(self.stack) >= 2:
            self.stack.append(self.stack.pop() * self.stack.pop())
        else:
            self.interpreter.log_output("❌ Stack underflow")

    def _div(self):
        if len(self.stack) >= 2:
            b = self.stack.pop(); a = self.stack.pop()
            self.stack.append(0 if b == 0 else int(a / b))

    def _mod(self):
        if len(self.stack) >= 2:
            b = self.stack.pop(); a = self.stack.pop()
            self.stack.append(0 if b == 0 else a % b)

    def _divmod(self):
        if len(self.stack) >= 2:
            b = self.stack.pop(); a = self.stack.pop()
            if b == 0: self.stack += [0, 0]
            else: self.stack += [a % b, int(a / b)]

    def _muldiv(self):
        if len(self.stack) >= 3:
            c, b, a = self.stack.pop(), self.stack.pop(), self.stack.pop()
            self.stack.append(a * b // c if c else 0)

    def _muldivmod(self):
        if len(self.stack) >= 3:
            c, b, a = self.stack.pop(), self.stack.pop(), self.stack.pop()
            p = a * b; self.stack += [p % c, p // c] if c else [0, 0]

    def _abs(self):
        if self.stack: self.stack.append(abs(self.stack.pop()))

    def _negate(self):
        if self.stack: self.stack.append(-self.stack.pop())

    def _max(self):
        if len(self.stack) >= 2: b, a = self.stack.pop(), self.stack.pop(); self.stack.append(max(a, b))

    def _min(self):
        if len(self.stack) >= 2: b, a = self.stack.pop(), self.stack.pop(); self.stack.append(min(a, b))

    def _1plus(self):
        if self.stack: self.stack[-1] += 1

    def _1minus(self):
        if self.stack: self.stack[-1] -= 1

    def _2plus(self):
        if self.stack: self.stack[-1] += 2

    def _2minus(self):
        if self.stack: self.stack[-1] -= 2

    def _2mul(self):
        if self.stack: self.stack[-1] *= 2

    def _2div(self):
        if self.stack: self.stack[-1] //= 2

    def _lshift(self):
        if len(self.stack) >= 2: n, u = self.stack.pop(), self.stack.pop(); self.stack.append(u << n)

    def _rshift(self):
        if len(self.stack) >= 2: n, u = self.stack.pop(), self.stack.pop(); self.stack.append(u >> n)

    def _madd(self):
        if len(self.stack) >= 2: n, d = self.stack.pop(), self.stack.pop(); self.stack.append(d + n)

    # ── Comparisons ────────────────────────────────────────────────────

    def _eq(self):
        if len(self.stack) >= 2: self.stack.append(-1 if self.stack.pop() == self.stack.pop() else 0)

    def _ne(self):
        if len(self.stack) >= 2: self.stack.append(0 if self.stack.pop() == self.stack.pop() else -1)

    def _lt(self):
        if len(self.stack) >= 2: b, a = self.stack.pop(), self.stack.pop(); self.stack.append(-1 if a < b else 0)

    def _gt(self):
        if len(self.stack) >= 2: b, a = self.stack.pop(), self.stack.pop(); self.stack.append(-1 if a > b else 0)

    def _le(self):
        if len(self.stack) >= 2: b, a = self.stack.pop(), self.stack.pop(); self.stack.append(-1 if a <= b else 0)

    def _ge(self):
        if len(self.stack) >= 2: b, a = self.stack.pop(), self.stack.pop(); self.stack.append(-1 if a >= b else 0)

    def _0eq(self):
        if self.stack: self.stack.append(-1 if self.stack.pop() == 0 else 0)

    def _0lt(self):
        if self.stack: self.stack.append(-1 if self.stack.pop() < 0 else 0)

    def _0gt(self):
        if self.stack: self.stack.append(-1 if self.stack.pop() > 0 else 0)

    def _0ne(self):
        if self.stack: self.stack.append(-1 if self.stack.pop() != 0 else 0)

    def _ult(self):
        if len(self.stack) >= 2: b, a = self.stack.pop(), self.stack.pop(); self.stack.append(-1 if (a & 0xFFFFFFFF) < (b & 0xFFFFFFFF) else 0)

    def _ugt(self):
        if len(self.stack) >= 2: b, a = self.stack.pop(), self.stack.pop(); self.stack.append(-1 if (a & 0xFFFFFFFF) > (b & 0xFFFFFFFF) else 0)

    # ── Logic / bitwise ────────────────────────────────────────────────

    def _and(self):
        if len(self.stack) >= 2: self.stack.append(self.stack.pop() & self.stack.pop())

    def _or(self):
        if len(self.stack) >= 2: self.stack.append(self.stack.pop() | self.stack.pop())

    def _xor(self):
        if len(self.stack) >= 2: self.stack.append(self.stack.pop() ^ self.stack.pop())

    def _invert(self):
        if self.stack: self.stack.append(~self.stack.pop())

    def _true(self):
        self.stack.append(-1)

    def _false(self):
        self.stack.append(0)

    def _not(self):
        if self.stack: self.stack.append(-1 if self.stack.pop() == 0 else 0)

    # ── Memory ─────────────────────────────────────────────────────────

    def _fetch(self):
        if self.stack:
            addr = self.stack.pop()
            self.stack.append(self.memory[addr] if 0 <= addr < len(self.memory) else 0)

    def _store(self):
        if len(self.stack) >= 2:
            addr = self.stack.pop(); val = self.stack.pop()
            if 0 <= addr < len(self.memory): self.memory[addr] = val

    def _cfetch(self):
        if self.stack:
            addr = self.stack.pop()
            self.stack.append(self.memory[addr] & 0xFF if 0 <= addr < len(self.memory) else 0)

    def _cstore(self):
        if len(self.stack) >= 2:
            addr = self.stack.pop(); val = self.stack.pop()
            if 0 <= addr < len(self.memory): self.memory[addr] = val & 0xFF

    def _2fetch(self):
        if self.stack:
            addr = self.stack.pop()
            if 0 <= addr + 1 < len(self.memory):
                self.stack += [self.memory[addr], self.memory[addr + 1]]

    def _2store(self):
        if len(self.stack) >= 3:
            addr = self.stack.pop(); hi = self.stack.pop(); lo = self.stack.pop()
            if 0 <= addr + 1 < len(self.memory):
                self.memory[addr] = lo; self.memory[addr + 1] = hi

    def _plus_store(self):
        if len(self.stack) >= 2:
            addr = self.stack.pop(); n = self.stack.pop()
            if 0 <= addr < len(self.memory): self.memory[addr] += n

    def _variable(self):
        pass  # handled in execute_tokens

    # ── Floating-point ─────────────────────────────────────────────────

    def _fadd(self):
        if len(self.fstack) >= 2: self.fstack.append(self.fstack.pop() + self.fstack.pop())

    def _fsub(self):
        if len(self.fstack) >= 2: b, a = self.fstack.pop(), self.fstack.pop(); self.fstack.append(a - b)

    def _fmul(self):
        if len(self.fstack) >= 2: self.fstack.append(self.fstack.pop() * self.fstack.pop())

    def _fdiv(self):
        if len(self.fstack) >= 2:
            b, a = self.fstack.pop(), self.fstack.pop()
            self.fstack.append(a / b if b else float("nan"))

    def _fdup(self):
        if self.fstack: self.fstack.append(self.fstack[-1])

    def _fdrop(self):
        if self.fstack: self.fstack.pop()

    def _fswap(self):
        if len(self.fstack) >= 2: a, b = self.fstack.pop(), self.fstack.pop(); self.fstack += [a, b]

    def _fover(self):
        if len(self.fstack) >= 2: self.fstack.append(self.fstack[-2])

    def _fnegate(self):
        if self.fstack: self.fstack.append(-self.fstack.pop())

    def _fabs(self):
        if self.fstack: self.fstack.append(abs(self.fstack.pop()))

    def _fmax(self):
        if len(self.fstack) >= 2: b, a = self.fstack.pop(), self.fstack.pop(); self.fstack.append(max(a, b))

    def _fmin(self):
        if len(self.fstack) >= 2: b, a = self.fstack.pop(), self.fstack.pop(); self.fstack.append(min(a, b))

    def _fsqrt(self):
        if self.fstack: self.fstack.append(_fmath.sqrt(abs(self.fstack.pop())))

    def _fsin(self):
        if self.fstack: self.fstack.append(_fmath.sin(self.fstack.pop()))

    def _fcos(self):
        if self.fstack: self.fstack.append(_fmath.cos(self.fstack.pop()))

    def _ftan(self):
        if self.fstack: self.fstack.append(_fmath.tan(self.fstack.pop()))

    def _fasin(self):
        if self.fstack: self.fstack.append(_fmath.asin(self.fstack.pop()))

    def _facos(self):
        if self.fstack: self.fstack.append(_fmath.acos(self.fstack.pop()))

    def _fatan(self):
        if self.fstack: self.fstack.append(_fmath.atan(self.fstack.pop()))

    def _fatan2(self):
        if len(self.fstack) >= 2: b, a = self.fstack.pop(), self.fstack.pop(); self.fstack.append(_fmath.atan2(a, b))

    def _fexp(self):
        if self.fstack: self.fstack.append(_fmath.exp(self.fstack.pop()))

    def _fln(self):
        if self.fstack: v = self.fstack.pop(); self.fstack.append(_fmath.log(v) if v > 0 else float("nan"))

    def _flog(self):
        if self.fstack: v = self.fstack.pop(); self.fstack.append(_fmath.log10(v) if v > 0 else float("nan"))

    def _ffloor(self):
        if self.fstack: self.fstack.append(float(_fmath.floor(self.fstack.pop())))

    def _fround(self):
        if self.fstack: self.fstack.append(float(round(self.fstack.pop())))

    def _ftrunc(self):
        if self.fstack: self.fstack.append(float(_fmath.trunc(self.fstack.pop())))

    def _feq(self):
        if len(self.fstack) >= 2: b, a = self.fstack.pop(), self.fstack.pop(); self.stack.append(-1 if a == b else 0)

    def _flt(self):
        if len(self.fstack) >= 2: b, a = self.fstack.pop(), self.fstack.pop(); self.stack.append(-1 if a < b else 0)

    def _fgt(self):
        if len(self.fstack) >= 2: b, a = self.fstack.pop(), self.fstack.pop(); self.stack.append(-1 if a > b else 0)

    def _fdot(self):
        if self.fstack: self.output_buffer += f"{self.fstack.pop():.6G} "

    def _f0eq(self):
        if self.fstack: self.stack.append(-1 if self.fstack.pop() == 0.0 else 0)

    def _f0lt(self):
        if self.fstack: self.stack.append(-1 if self.fstack.pop() < 0.0 else 0)

    def _s_to_f(self):
        if self.stack: self.fstack.append(float(self.stack.pop()))

    def _f_to_s(self):
        if self.fstack: self.stack.append(int(self.fstack.pop()))

    def _d_to_f(self):
        if len(self.stack) >= 2:
            hi, lo = self.stack.pop(), self.stack.pop()
            self.fstack.append(float((hi << 32) | (lo & 0xFFFFFFFF)))

    def _fpi(self):
        self.fstack.append(_fmath.pi)

    def _flit(self):
        # literal float on f-stack — value pushed by token loop
        pass

    # ── I/O ────────────────────────────────────────────────────────────

    def _key(self):
        raw = ""
        if hasattr(self.interpreter, "request_input"):
            raw = self.interpreter.request_input("key: ") or ""
        self.stack.append(ord(raw[0]) if raw else 0)

    def _accept(self):
        # ( caddr +n1 -- +n2 ) read up to n1 chars into caddr
        if len(self.stack) >= 2:
            n1 = self.stack.pop(); addr = self.stack.pop()
            raw = ""
            if hasattr(self.interpreter, "request_input"):
                raw = self.interpreter.request_input("") or ""
            raw = raw[:n1]
            for i, c in enumerate(raw):
                if 0 <= addr + i < len(self.memory):
                    self.memory[addr + i] = ord(c)
            self.stack.append(len(raw))

    def _word(self):
        # stub: push empty string address
        self.stack.append(0)

    # ── System ─────────────────────────────────────────────────────────

    def _push_base_addr(self):
        # Push address of BASE so it can be stored via !
        # we store base at memory address 0xFFFF
        self.memory[0xFFFF] = self.base
        self.stack.append(0xFFFF)

    def _decimal(self):
        self.base = 10

    def _hex(self):
        self.base = 16

    def _binary(self):
        self.base = 2

    def _octal(self):
        self.base = 8

    def _words(self):
        words = sorted(self.dictionary.keys())
        chunks = [words[i:i+8] for i in range(0, len(words), 8)]
        for chunk in chunks:
            self.interpreter.log_output("  ".join(f"{w:<12}" for w in chunk))

    def _bye(self):
        raise StopIteration

    def _include(self):
        pass

    def _ms(self):
        if self.stack: self.stack.pop()  # discard milliseconds, no actual delay

    def _here_word(self):
        self.stack.append(self.here)

    def _allot_word(self):
        if self.stack:
            n = self.stack.pop()
            self.here += int(n)

    # ── String words ───────────────────────────────────────────────────

    def _count(self):
        # ( caddr -- caddr+1 u ) counted string
        if self.stack:
            addr = self.stack.pop()
            length = self.memory[addr] if 0 <= addr < len(self.memory) else 0
            self.stack += [addr + 1, length]

    def _compare(self):
        # ( caddr1 u1 caddr2 u2 -- n )
        if len(self.stack) >= 4:
            u2, a2, u1, a1 = self.stack.pop(), self.stack.pop(), self.stack.pop(), self.stack.pop()
            s1 = "".join(chr(self.memory[a1 + i] & 0xFF) for i in range(u1) if a1+i < len(self.memory))
            s2 = "".join(chr(self.memory[a2 + i] & 0xFF) for i in range(u2) if a2+i < len(self.memory))
            self.stack.append(0 if s1 == s2 else (-1 if s1 < s2 else 1))

    def _move_mem(self):
        if len(self.stack) >= 3:
            u, dst, src = self.stack.pop(), self.stack.pop(), self.stack.pop()
            for i in range(u):
                if 0 <= src + i < len(self.memory) and 0 <= dst + i < len(self.memory):
                    self.memory[dst + i] = self.memory[src + i]

    def _fill(self):
        if len(self.stack) >= 3:
            ch, u, addr = self.stack.pop(), self.stack.pop(), self.stack.pop()
            for i in range(u):
                if 0 <= addr + i < len(self.memory): self.memory[addr + i] = ch & 0xFF

    def _cmove(self):
        if len(self.stack) >= 3:
            u, dst, src = self.stack.pop(), self.stack.pop(), self.stack.pop()
            for i in range(u):
                if 0 <= src + i < len(self.memory) and 0 <= dst + i < len(self.memory):
                    self.memory[dst + i] = self.memory[src + i]

    def _cmove_up(self):
        if len(self.stack) >= 3:
            u, dst, src = self.stack.pop(), self.stack.pop(), self.stack.pop()
            for i in range(u - 1, -1, -1):
                if 0 <= src + i < len(self.memory) and 0 <= dst + i < len(self.memory):
                    self.memory[dst + i] = self.memory[src + i]

    def _char_word(self):
        pass  # handled in token loop

    def _plus_place(self):
        pass

    # ── Graphics ───────────────────────────────────────────────────────

    def _fd(self):
        if self.stack and self.turtle: self.turtle.forward(self.stack.pop())

    def _bk(self):
        if self.stack and self.turtle: self.turtle.back(self.stack.pop())

    def _rt(self):
        if self.stack and self.turtle: self.turtle.right(self.stack.pop())

    def _lt_turn(self):
        if self.stack and self.turtle: self.turtle.left(self.stack.pop())

    def _pu(self):
        if self.turtle: self.turtle.penup()

    def _pd(self):
        if self.turtle: self.turtle.pendown()

    def _home(self):
        if self.turtle: self.turtle.home()

    def _clean(self):
        if self.turtle: self.turtle.clear()

    def _pen(self):
        if self.stack and self.turtle:
            color_idx = int(self.stack.pop())
            color_map = {0:"BLACK",1:"RED",2:"GREEN",3:"BLUE",4:"YELLOW",
                         5:"CYAN",6:"MAGENTA",7:"WHITE",8:"ORANGE",9:"PURPLE",
                         10:"BROWN",11:"PINK",12:"GRAY",13:"LIME",14:"NAVY",15:"TEAL"}
            self.turtle.pencolor(color_map.get(color_idx, "WHITE"))

    def _xcor(self):
        if self.turtle:
            x = getattr(self.turtle, "x", 0)
            self.stack.append(int(x))

    def _ycor(self):
        if self.turtle:
            y = getattr(self.turtle, "y", 0)
            self.stack.append(int(y))

    def _heading(self):
        if self.turtle:
            h = getattr(self.turtle, "heading", 0)
            self.stack.append(int(h))

    def _color(self):
        if self.stack and self.turtle:
            c = self.stack.pop()
            if isinstance(c, str): self.turtle.pencolor(c)
            else: self._pen()

    def _circle(self):
        if self.stack and self.turtle:
            r = self.stack.pop()
            if hasattr(self.turtle, "circle"): self.turtle.circle(r)
            else:
                for _ in range(36):
                    self.turtle.forward(r * _fmath.pi * 2 / 36)
                    self.turtle.right(10)

    def _arc(self):
        if len(self.stack) >= 2 and self.turtle:
            ang, r = self.stack.pop(), self.stack.pop()
            steps = max(1, abs(ang) // 5)
            for _ in range(steps):
                self.turtle.forward(r * _fmath.pi * 2 * ang / 360 / steps)
                self.turtle.right(ang / steps)

    def _fill_gfx(self):
        if self.stack and self.turtle: self.stack.pop()  # stub

    def _dot_gfx(self):
        if len(self.stack) >= 2 and self.turtle:
            x = self.stack.pop(); y = self.stack.pop()
            if hasattr(self.turtle, "goto"): self.turtle.goto(y, x)

    def _label(self):
        if self.stack: self.output_buffer += str(self.stack.pop())

    # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    # execute_tokens — main interpreter loop
    # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    def execute_tokens(self, tokens: List[str]):
        i = 0
        while i < len(tokens):
            token = tokens[i]
            token_upper = token.upper()

            # ── Compilation mode ─────────────────────────────────────────
            if self.compiling:
                if token == ";":
                    self.compiling = False
                    definition = list(self.new_word_definition)
                    def new_word_func(d=tuple(definition)):
                        self.execute_tokens(list(d))
                    self.dictionary[self.new_word_name] = new_word_func
                    self.interpreter.log_output(f"✅ Defined {self.new_word_name}")
                else:
                    self.new_word_definition.append(token)
                i += 1
                continue

            # ── Colon definition ─────────────────────────────────────────
            if token_upper == ":":
                self.compiling = True
                if i + 1 < len(tokens):
                    self.new_word_name = tokens[i + 1].upper()
                    self.new_word_definition = []
                    i += 2
                else:
                    self.interpreter.log_output("❌ : requires name")
                    i += 1
                continue

            # ── CONSTANT name ─────────────────────────────────────────────
            if token_upper == "CONSTANT":
                if i + 1 < len(tokens) and self.stack:
                    name = tokens[i + 1].upper()
                    val = self.stack.pop()
                    def const_fn(v=val): self.stack.append(v)
                    self.dictionary[name] = const_fn
                    i += 2
                else:
                    self.interpreter.log_output("❌ CONSTANT requires name and value")
                    i += 1
                continue

            # ── VALUE name ────────────────────────────────────────────────
            if token_upper == "VALUE":
                if i + 1 < len(tokens) and self.stack:
                    name = tokens[i + 1].upper()
                    self.values[name] = self.stack.pop()
                    def val_fn(n=name): self.stack.append(self.values[n])
                    self.dictionary[name] = val_fn
                    i += 2
                else:
                    self.interpreter.log_output("❌ VALUE requires name and value")
                    i += 1
                continue

            # ── TO name (update VALUE) ────────────────────────────────────
            if token_upper == "TO":
                if i + 1 < len(tokens) and self.stack:
                    name = tokens[i + 1].upper()
                    self.values[name] = self.stack.pop()
                    i += 2
                else:
                    i += 1
                continue

            # ── DEFER name ────────────────────────────────────────────────
            if token_upper == "DEFER":
                if i + 1 < len(tokens):
                    def_name = tokens[i + 1].upper()
                    self.deferred[def_name] = None
                    def deferred_fn(n=def_name):
                        target = self.deferred.get(n)
                        if target and target in self.dictionary:
                            self.dictionary[target]()
                        else:
                            self.interpreter.log_output(f"❌ {n} is not yet assigned (IS not called)")
                    self.dictionary[def_name] = deferred_fn
                    i += 2
                else:
                    self.interpreter.log_output("❌ DEFER requires name")
                    i += 1
                continue

            # ── IS name (assign deferred word) ─────────────────────────────
            if token_upper == "IS":
                if i + 1 < len(tokens):
                    def_name = tokens[i + 1].upper()
                    # The TOS should be the execution token — in our model
                    # we accept a quoted word name on the stack as a string,
                    # but more commonly IS follows a word name directly.
                    # Support: ' WORD IS DEFERRED or string token on stack.
                    if self.stack and isinstance(self.stack[-1], str):
                        target = self.stack.pop().upper()
                    else:
                        # Try to use an xt (integer address) — fall back to
                        # the previous token as the word to bind.
                        target = None
                    if target is not None:
                        self.deferred[def_name] = target
                        def deferred_fn(n=def_name):
                            t = self.deferred.get(n)
                            if t and t in self.dictionary:
                                self.dictionary[t]()
                        self.dictionary[def_name] = deferred_fn
                    i += 2
                else:
                    i += 1
                continue

            # ── VARIABLE name ─────────────────────────────────────────────
            if token_upper == "VARIABLE":
                if i + 1 < len(tokens):
                    var_name = tokens[i + 1].upper()
                    addr = self.here
                    self.here += 1
                    if addr < len(self.memory): self.memory[addr] = 0
                    def var_func(a=addr): self.stack.append(a)
                    self.dictionary[var_name] = var_func
                    i += 2
                else:
                    self.interpreter.log_output("❌ VARIABLE requires name")
                    i += 1
                continue

            # ── ALLOT ─────────────────────────────────────────────────────
            if token_upper == "ALLOT":
                if self.stack:
                    n = self.stack.pop()
                    self.here += n
                i += 1
                continue

            # ── CELLS (n -- n*cell-size, cell=1 in our model) ────────────
            if token_upper == "CELLS":
                i += 1; continue  # each cell is 1 unit in our model

            # ── CREATE name ───────────────────────────────────────────────
            if token_upper == "CREATE":
                if i + 1 < len(tokens):
                    cname = tokens[i + 1].upper()
                    addr = self.here
                    def create_fn(a=addr): self.stack.append(a)
                    self.dictionary[cname] = create_fn
                    i += 2
                else:
                    i += 1
                continue

            # ── CHAR c  ( -- n ) ──────────────────────────────────────────
            if token_upper == "CHAR":
                if i + 1 < len(tokens):
                    s = tokens[i + 1]
                    self.stack.append(ord(s[0]) if s else 0)
                    i += 2
                else:
                    i += 1
                continue

            # ── [ CHAR ] c ────────────────────────────────────────────────
            if token_upper == "[CHAR]":
                if i + 1 < len(tokens):
                    s = tokens[i + 1]
                    self.new_word_definition.append(str(ord(s[0]) if s else 0))
                    i += 2
                else:
                    i += 1
                continue

            # ── IF / ELSE / THEN ──────────────────────────────────────────
            if token_upper == "IF":
                if not self.stack:
                    self.interpreter.log_output("❌ Stack underflow for IF")
                    i += 1; continue
                cond = self.stack.pop()
                if cond == 0:
                    depth = 1; advance = i + 1
                    while advance < len(tokens):
                        t = tokens[advance].upper()
                        if t == "IF": depth += 1
                        elif t == "THEN":
                            depth -= 1
                            if depth == 0: break
                        elif t == "ELSE" and depth == 1: break
                        advance += 1
                    i = advance
                i += 1; continue

            if token_upper == "ELSE":
                depth = 1; advance = i + 1
                while advance < len(tokens):
                    t = tokens[advance].upper()
                    if t == "IF": depth += 1
                    elif t == "THEN":
                        depth -= 1
                        if depth == 0: break
                    advance += 1
                i = advance + 1; continue

            if token_upper == "THEN":
                i += 1; continue

            # ── DO / ?DO / LOOP / +LOOP / LEAVE / I / J ──────────────────
            if token_upper == "DO":
                if len(self.stack) < 2:
                    self.interpreter.log_output("❌ Stack underflow for DO")
                    i += 1; continue
                start = self.stack.pop(); limit = self.stack.pop()
                self.return_stack += [limit, start, i]
                i += 1; continue

            if token_upper == "?DO":
                if len(self.stack) < 2:
                    i += 1; continue
                start = self.stack.pop(); limit = self.stack.pop()
                if start == limit:
                    # skip to LOOP/+LOOP
                    depth2 = 1; advance = i + 1
                    while advance < len(tokens):
                        t = tokens[advance].upper()
                        if t in ("DO", "?DO"): depth2 += 1
                        elif t in ("LOOP", "+LOOP"):
                            depth2 -= 1
                            if depth2 == 0: break
                        advance += 1
                    i = advance + 1
                else:
                    self.return_stack += [limit, start, i]
                    i += 1
                continue

            if token_upper == "LOOP":
                if len(self.return_stack) < 3:
                    self.interpreter.log_output("❌ Return stack underflow for LOOP")
                    i += 1; continue
                loop_start = self.return_stack.pop()
                index = self.return_stack.pop()
                limit = self.return_stack.pop()
                index += 1
                if index < limit:
                    self.return_stack += [limit, index, loop_start]
                    i = loop_start + 1
                else:
                    i += 1
                continue

            if token_upper == "+LOOP":
                if len(self.return_stack) < 3 or not self.stack:
                    i += 1; continue
                step = self.stack.pop()
                loop_start = self.return_stack.pop()
                index = self.return_stack.pop()
                limit = self.return_stack.pop()
                index += step
                cont = index < limit if step > 0 else index > limit
                if cont:
                    self.return_stack += [limit, index, loop_start]
                    i = loop_start + 1
                else:
                    i += 1
                continue

            if token_upper == "LEAVE":
                # Skip to matching LOOP/+LOOP
                if len(self.return_stack) >= 3:
                    self.return_stack.pop(); self.return_stack.pop(); self.return_stack.pop()
                depth3 = 1; advance = i + 1
                while advance < len(tokens):
                    t = tokens[advance].upper()
                    if t in ("DO", "?DO"): depth3 += 1
                    elif t in ("LOOP", "+LOOP"):
                        depth3 -= 1
                        if depth3 == 0: break
                    advance += 1
                i = advance + 1; continue

            if token_upper == "I":
                if len(self.return_stack) >= 2: self.stack.append(self.return_stack[-2])
                else: self.interpreter.log_output("❌ I outside DO loop")
                i += 1; continue

            if token_upper == "J":
                if len(self.return_stack) >= 5: self.stack.append(self.return_stack[-5])
                else: self.interpreter.log_output("❌ J: no outer loop")
                i += 1; continue

            # ── BEGIN / UNTIL / WHILE / REPEAT / AGAIN ────────────────────
            if token_upper == "BEGIN":
                self.return_stack.append(i)  # save BEGIN position
                i += 1; continue

            if token_upper == "AGAIN":
                if self.return_stack:
                    i = self.return_stack[-1] + 1
                else: i += 1
                continue

            if token_upper == "UNTIL":
                if not self.stack or not self.return_stack:
                    i += 1; continue
                cond = self.stack.pop()
                if cond == 0:
                    i = self.return_stack[-1] + 1  # loop back
                else:
                    self.return_stack.pop()          # exit loop
                    i += 1
                continue

            if token_upper == "WHILE":
                if not self.stack:
                    i += 1; continue
                cond = self.stack.pop()
                if cond == 0:
                    # Exit: skip to REPEAT
                    depth4 = 1; advance = i + 1
                    while advance < len(tokens):
                        t = tokens[advance].upper()
                        if t == "BEGIN": depth4 += 1
                        elif t == "REPEAT":
                            depth4 -= 1
                            if depth4 == 0: break
                        advance += 1
                    if self.return_stack: self.return_stack.pop()
                    i = advance + 1
                else:
                    i += 1
                continue

            if token_upper == "REPEAT":
                if self.return_stack:
                    i = self.return_stack[-1] + 1  # back to BEGIN
                else: i += 1
                continue

            # ── EXIT (leave current word) ─────────────────────────────────
            if token_upper == "EXIT":
                return

            # ── String literal ." ... " ───────────────────────────────────
            if token.startswith('."') or token_upper.startswith('."'):
                inner = token[2:]
                if inner.endswith('"'):
                    self.output_buffer += inner[:-1]
                else:
                    self.output_buffer += inner + " "
                i += 1; continue

            # ── S" ... " (push addr, len) ─────────────────────────────────
            if token_upper.startswith('S"'):
                inner = token[2:]
                if inner.endswith('"'): inner = inner[:-1]
                addr = self.here
                for c in inner:
                    if self.here < len(self.memory):
                        self.memory[self.here] = ord(c); self.here += 1
                self.stack += [addr, len(inner)]
                i += 1; continue

            # ── Floating-point literal (e.g. 3.14  -1.5e10) ──────────────
            try:
                if "." in token or "e" in token.lower():
                    fval = float(token)
                    self.fstack.append(fval)
                    i += 1; continue
            except ValueError:
                pass

            # ── Dictionary lookup ─────────────────────────────────────────
            if token_upper in self.dictionary:
                self.dictionary[token_upper]()
                i += 1; continue

            # ── Integer literal ───────────────────────────────────────────
            try:
                if self.base == 16:
                    val = int(token, 16)
                elif self.base == 2:
                    val = int(token, 2)
                elif self.base == 8:
                    val = int(token, 8)
                else:
                    val = int(token)
                self.stack.append(val)
                i += 1; continue
            except ValueError:
                pass

            # ── Hex literal with $ prefix ─────────────────────────────────
            if token.startswith("$"):
                try:
                    self.stack.append(int(token[1:], 16)); i += 1; continue
                except ValueError: pass

            # ── Binary literal with % prefix ─────────────────────────────
            if token.startswith("%"):
                try:
                    self.stack.append(int(token[1:], 2)); i += 1; continue
                except ValueError: pass

            self.interpreter.log_output(f"❌ Unknown word: {token}")
            i += 1

    # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    # execute_line — tokenizer entry
    # ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    def execute_line(self, line: str, turtle=None):
        self.turtle = turtle
        tokens: List[str] = []
        parts = line.split()
        j = 0
        while j < len(parts):
            t = parts[j]
            tu = t.upper()
            if tu in ('."', 'S"', 'C"'):
                # collect until closing "
                s = t
                j += 1
                while j < len(parts):
                    s += " " + parts[j]
                    if parts[j].endswith('"'):
                        break
                    j += 1
                tokens.append(s)
            elif t == "\\":
                break  # line comment
            elif t == "(":
                j += 1
                while j < len(parts) and parts[j] != ")":
                    j += 1
            else:
                tokens.append(t)
            j += 1
        self.execute_tokens(tokens)
        if self.output_buffer:
            self.interpreter.log_output(self.output_buffer)
            self.output_buffer = ""


# ── Global instance ─────────────────────────────────────────────────────────

_forth_executor = None


def reset_forth():
    """Reset the global Forth executor so state doesn't leak between runs."""
    global _forth_executor
    _forth_executor = None


def execute_forth(interpreter: "Interpreter", command: str, _turtle=None) -> str:
    global _forth_executor
    if _forth_executor is None or _forth_executor.interpreter != interpreter:
        _forth_executor = ForthExecutor(interpreter)
    _forth_executor.execute_line(command, _turtle)
    return ""
