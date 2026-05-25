"""COBOL language executor for Time Warp Studio.

Educational COBOL interpreter — whole-program execution.
Implements a free-format teaching subset of ANSI COBOL-85:
  - IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE divisions
  - WORKING-STORAGE SECTION variable declarations (PIC clause)
  - DISPLAY (output), MOVE, COMPUTE, ADD/SUBTRACT/MULTIPLY/DIVIDE
  - PERFORM (paragraph call), PERFORM VARYING (loop), PERFORM UNTIL
  - IF/ELSE/END-IF, EVALUATE/WHEN/OTHER/END-EVALUATE
  - STOP RUN / STOP
  - STRING, UNSTRING (basic)
  - INSPECT (TALLYING / REPLACING)
  - ACCEPT (from input)
  - GO TO (paragraph jump)
  - OCCURS (table/array handling with subscripted references)
"""

from __future__ import annotations

import math
import re
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_cobol(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a complete COBOL program and return all output as a string."""
    env = CobolEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------


class _CobolStop(Exception):
    """STOP RUN raised to terminate cleanly."""


class _CobolGoTo(Exception):
    """GO TO paragraph jump."""

    def __init__(self, para: str) -> None:
        self.para = para


class _CobolPerformReturn(Exception):
    """Internal sentinel to return from a PERFORM."""


# ---------------------------------------------------------------------------
# PIC clause helpers
# ---------------------------------------------------------------------------

_PIC_RE = re.compile(
    r"PIC(?:TURE)?\s+IS\s+([^\s]+)|PIC(?:TURE)?\s+([^\s]+)",
    re.IGNORECASE,
)
_PIC_CLAUSE_RE = re.compile(
    r"PIC(?:TURE)?(?:\s+IS)?\s+([^\s.]+)",
    re.IGNORECASE,
)


def _pic_default(pic: str) -> Any:
    """Return the zero value for a PIC clause."""
    pic = pic.upper().strip(" .")
    # Numeric PIC: 9, S9, 9V99, etc.
    if re.search(r"[9SVZ]", pic):
        return 0
    # Alphanumeric PIC: X
    return ""


def _pic_is_numeric(pic: str) -> bool:
    """Return True if the PIC clause represents a numeric type."""
    return bool(re.search(r"[9SV]", pic.upper()))


def _pic_size(pic: str) -> int:
    """Return the total character size represented by a PIC clause."""
    pic = pic.upper().strip()
    # Expand repetition notation: X(10) → 10 chars
    total = 0
    for m in re.finditer(r"([A-Z9V])\((\d+)\)|([A-Z9V])", pic):
        if m.group(1):
            total += int(m.group(2))
        else:
            total += 1
    return max(total, 1)


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------


class CobolEnvironment:
    """Full execution environment for a COBOL program."""

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState") -> None:
        self._interp = interpreter
        self._turtle = turtle
        self._vars: Dict[str, Any] = {}  # variable name → current value
        self._pics: Dict[str, str] = {}  # variable name → PIC clause
        self._occurs: Dict[str, int] = {}  # table name → number of occurrences
        self._paragraphs: Dict[str, List[str]] = {}  # paragraph name → lines
        self._output: List[str] = []
        self._para_order: List[str] = []  # paragraph execution order

    # ------------------------------------------------------------------
    # Top-level run
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        try:
            self._parse(source)
            self._execute()
        except _CobolStop:
            pass
        except Exception as e:  # noqa: BLE001
            self._output.append(f"❌ COBOL error: {e}\n")
        return "".join(self._output)

    # ------------------------------------------------------------------
    # Parsing
    # ------------------------------------------------------------------

    def _parse(self, source: str) -> None:
        """Split source into divisions/sections and populate internal state."""
        lines = source.splitlines()

        # Normalise: strip inline comments (* in column 1 or after COBOL *)
        cleaned: List[str] = []
        for raw in lines:
            stripped = raw.strip()
            # Full-line comment: starts with * or /
            if stripped.startswith("*") or stripped.startswith("/"):
                continue
            # Remove inline comment  >>  source >> *> comment text
            stripped = re.sub(r"\*>.*$", "", stripped).strip()
            if stripped:
                cleaned.append(stripped)

        # Locate division boundaries (case-insensitive)
        _DIV = re.compile(
            r"^(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION", re.I
        )
        _SECT = re.compile(
            r"^(WORKING-STORAGE|FILE|LINKAGE|LOCAL-STORAGE)\s+SECTION", re.I
        )
        _PARA = re.compile(r"^([A-Z0-9][A-Z0-9\-]*)\s*\.\s*$", re.I)

        current_division = ""
        current_section = ""
        current_para: Optional[str] = None
        para_lines: List[str] = []

        for line in cleaned:
            # Division header
            dm = _DIV.match(line)
            if dm:
                # Flush current paragraph
                if current_para is not None:
                    self._paragraphs[current_para] = para_lines[:]
                    if current_para not in self._para_order:
                        self._para_order.append(current_para)
                    current_para = None
                    para_lines = []
                current_division = dm.group(1).upper()
                current_section = ""
                continue

            # Section header (DATA division)
            sm = _SECT.match(line)
            if sm:
                current_section = sm.group(1).upper()
                continue

            # DATA DIVISION — parse WORKING-STORAGE declarations
            if current_division == "DATA" and "WORKING-STORAGE" in current_section:
                self._parse_data_line(line)
                continue

            # PROCEDURE DIVISION
            if current_division == "PROCEDURE":
                # Paragraph label: word followed by period on its own line
                pm = _PARA.match(line)
                if pm:
                    if current_para is not None:
                        self._paragraphs[current_para] = para_lines[:]
                        if current_para not in self._para_order:
                            self._para_order.append(current_para)
                    current_para = pm.group(1).upper()
                    para_lines = []
                else:
                    if current_para is not None:
                        # Strip trailing period from statement
                        para_lines.append(line.rstrip(".").strip())
                    else:
                        # Lines before first paragraph go into implicit MAIN
                        if "__MAIN__" not in self._paragraphs:
                            self._paragraphs["__MAIN__"] = []
                            self._para_order.insert(0, "__MAIN__")
                        self._paragraphs["__MAIN__"].append(line.rstrip(".").strip())

        # Flush last paragraph
        if current_para is not None:
            self._paragraphs[current_para] = para_lines[:]
            if current_para not in self._para_order:
                self._para_order.append(current_para)

    def _parse_data_line(self, line: str) -> None:
        """Parse a WORKING-STORAGE variable declaration."""
        # Level-number  name  PIC ...  [VALUE ...]
        m = re.match(
            r"^(\d+)\s+([A-Z0-9][A-Z0-9\-]*)\s+(.*)",
            line,
            re.IGNORECASE,
        )
        if not m:
            return
        _level, name, rest = m.group(1), m.group(2).upper(), m.group(3)

        # OCCURS clause — table declaration
        occ_m = re.search(r"\bOCCURS\s+(\d+)(?:\s+TIMES)?\b", rest, re.IGNORECASE)
        if occ_m:
            size = int(occ_m.group(1))
            self._occurs[name] = size
            pic_m2 = _PIC_CLAUSE_RE.search(rest)
            pic2 = pic_m2.group(1) if pic_m2 else "9"
            self._pics[name] = pic2
            default = _pic_default(pic2)
            # Optional VALUE clause for tables
            val_m2 = re.search(r"\bVALUE\s+(?:IS\s+)?([^\s.]+)", rest, re.IGNORECASE)
            if val_m2:
                raw2 = val_m2.group(1).strip("\"'")
                if _pic_is_numeric(pic2):
                    try:
                        default = float(raw2) if "." in raw2 else int(raw2)
                    except ValueError:
                        pass
                else:
                    default = raw2
            self._vars[name] = [default] * size
            return

        # Skip level 77/01 group items with no PIC (they're group containers)
        pic_m = _PIC_CLAUSE_RE.search(rest)
        if not pic_m:
            # Group item — register with default numeric 0
            if name not in self._vars:
                self._vars[name] = 0
            return
        pic = pic_m.group(1)
        self._pics[name] = pic
        # Optional VALUE clause
        val_m = re.search(r"\bVALUE\s+(?:IS\s+)?([^\s.]+)", rest, re.IGNORECASE)
        if val_m:
            raw_val = val_m.group(1).strip("\"'")
            if _pic_is_numeric(pic):
                try:
                    self._vars[name] = (
                        float(raw_val) if "." in raw_val else int(raw_val)
                    )
                except ValueError:
                    self._vars[name] = 0
            else:
                self._vars[name] = raw_val
        else:
            self._vars[name] = _pic_default(pic)

    # ------------------------------------------------------------------
    # Execution
    # ------------------------------------------------------------------

    def _execute(self) -> None:
        """Run paragraphs in order, starting from the first PROCEDURE paragraph."""
        if not self._para_order:
            self._output.append("ℹ️ No PROCEDURE DIVISION found\n")
            return

        # Execute paragraphs in declaration order until STOP RUN
        i = 0
        while i < len(self._para_order):
            para_name = self._para_order[i]
            try:
                self._run_paragraph(para_name)
            except _CobolGoTo as gt:
                target = gt.para
                if target in self._para_order:
                    i = self._para_order.index(target)
                    continue
                else:
                    self._output.append(f"❌ Undefined paragraph: {target}\n")
                    break
            i += 1

    def _run_paragraph(self, name: str) -> None:
        """Execute all statements in a paragraph."""
        stmts = self._paragraphs.get(name, [])
        idx = 0
        while idx < len(stmts):
            line = stmts[idx]
            if not line:
                idx += 1
                continue
            result = self._exec_stmt(line, stmts, idx)
            if result is None:
                idx += 1
            else:
                idx = result

    def _exec_stmt(self, line: str, block: List[str], idx: int) -> Optional[int]:
        """Execute a single statement. Return new index or None to advance by 1."""
        up = line.upper().strip()

        # STOP RUN
        if up in ("STOP RUN", "STOP"):
            raise _CobolStop

        # DISPLAY
        if up.startswith("DISPLAY "):
            self._stmt_display(line[8:].strip())
            return None

        # MOVE
        if up.startswith("MOVE "):
            self._stmt_move(line[5:].strip())
            return None

        # COMPUTE
        if up.startswith("COMPUTE "):
            self._stmt_compute(line[8:].strip())
            return None

        # ADD
        if up.startswith("ADD "):
            self._stmt_add(line[4:].strip())
            return None

        # SUBTRACT
        if up.startswith("SUBTRACT "):
            self._stmt_subtract(line[9:].strip())
            return None

        # MULTIPLY
        if up.startswith("MULTIPLY "):
            self._stmt_multiply(line[9:].strip())
            return None

        # DIVIDE
        if up.startswith("DIVIDE "):
            self._stmt_divide(line[7:].strip())
            return None

        # PERFORM ... VARYING
        m = re.match(
            r"^PERFORM\s+(\S+)\s+VARYING\s+(\S+)\s+FROM\s+(.+?)\s+BY\s+(.+?)\s+UNTIL\s+(.+)$",
            up,
        )
        if m:
            self._stmt_perform_varying(
                m.group(1),
                m.group(2),
                m.group(3),
                m.group(4),
                m.group(5),
                line,
            )
            return None

        # PERFORM UNTIL (inline or paragraph)
        m2 = re.match(r"^PERFORM\s+(\S+)\s+UNTIL\s+(.+)$", up)
        if m2:
            self._stmt_perform_until(m2.group(1), m2.group(2))
            return None

        # PERFORM TIMES
        m3 = re.match(r"^PERFORM\s+(\S+)\s+(\S+)\s+TIMES$", up)
        if m3:
            times = self._eval(m3.group(2))
            for _ in range(int(times)):
                self._run_paragraph(m3.group(1))
            return None

        # PERFORM (plain paragraph call)
        m4 = re.match(r"^PERFORM\s+(\S+)$", up)
        if m4:
            self._run_paragraph(m4.group(1))
            return None

        # IF / ELSE / END-IF
        if up.startswith("IF "):
            new_idx = self._stmt_if(line, block, idx)
            return new_idx

        # EVALUATE / WHEN / END-EVALUATE
        if up.startswith("EVALUATE "):
            new_idx = self._stmt_evaluate(line, block, idx)
            return new_idx

        # GO TO
        if up.startswith("GO TO ") or up.startswith("GOTO "):
            target = re.split(r"\s+", up, 2)[2].strip()
            raise _CobolGoTo(target)

        # ACCEPT
        if up.startswith("ACCEPT "):
            var = up[7:].strip()
            self._stmt_accept(var)
            return None

        # STRING ... INTO
        if up.startswith("STRING "):
            self._stmt_string(line[7:].strip())
            return None

        # INSPECT
        if up.startswith("INSPECT "):
            self._stmt_inspect(line[8:].strip())
            return None

        # INITIALIZE
        if up.startswith("INITIALIZE "):
            var = up[11:].strip()
            self._set_var(var, self._get_pic_default(var))
            return None

        # END-IF / END-EVALUATE / END-PERFORM — handled by their parents
        if up in ("END-IF", "END-EVALUATE", "END-PERFORM", "ELSE"):
            return None

        # Unknown — silently skip to allow future expansion
        return None

    # ------------------------------------------------------------------
    # Statement implementations
    # ------------------------------------------------------------------

    def _stmt_display(self, rest: str) -> None:
        """DISPLAY [value ...] [NO ADVANCING]"""
        advancing = True
        rest_up = rest.upper()
        if rest_up.endswith(" NO ADVANCING") or rest_up.endswith(" WITH NO ADVANCING"):
            advancing = False
            rest = re.sub(
                r"\s+(?:WITH\s+)?NO\s+ADVANCING\s*$", "", rest, flags=re.IGNORECASE
            )

        parts = self._split_display_args(rest)
        text = " ".join(str(self._eval(p)) for p in parts)
        if advancing:
            self._output.append(text + "\n")
        else:
            self._output.append(text)

    def _split_display_args(self, rest: str) -> List[str]:
        """Split DISPLAY arguments (space-separated, respecting quoted strings and parens)."""
        parts: List[str] = []
        current: List[str] = []
        in_quote = False
        q_char = ""
        depth = 0
        for ch in rest:
            if in_quote:
                current.append(ch)
                if ch == q_char:
                    in_quote = False
            elif ch in ('"', "'"):
                in_quote = True
                q_char = ch
                current.append(ch)
            elif ch == "(":
                depth += 1
                current.append(ch)
            elif ch == ")":
                depth -= 1
                current.append(ch)
            elif ch == " " and depth == 0 and current:
                parts.append("".join(current))
                current = []
            else:
                current.append(ch)
        if current:
            parts.append("".join(current))
        return [p for p in parts if p]

    def _stmt_move(self, rest: str) -> None:
        """MOVE src TO dest [dest2 ...]"""
        m = re.match(r"(.+?)\s+TO\s+(.+)$", rest, re.IGNORECASE)
        if not m:
            return
        src_str, dests_str = m.group(1).strip(), m.group(2).strip()
        val = self._eval(src_str)
        # Split destinations respecting parentheses (for subscripted refs)
        for dest in self._split_dest_list(dests_str):
            if dest:
                self._set_var(dest, val)

    def _split_dest_list(self, s: str) -> List[str]:
        """Split a space-separated destination list, keeping NAME(IDX) intact."""
        parts: List[str] = []
        current: List[str] = []
        depth = 0
        for ch in s:
            if ch == "(":
                depth += 1
                current.append(ch)
            elif ch == ")":
                depth -= 1
                current.append(ch)
            elif ch == " " and depth == 0:
                if current:
                    parts.append("".join(current))
                    current = []
            else:
                current.append(ch)
        if current:
            parts.append("".join(current))
        return [p for p in parts if p]

    def _stmt_compute(self, rest: str) -> None:
        """COMPUTE var [ROUNDED] = expr"""
        m = re.match(
            r"(\S+(?:\([^)]+\))?)(?:\s+ROUNDED)?\s*=\s*(.+)$", rest, re.IGNORECASE
        )
        if not m:
            return
        var, expr = m.group(1), m.group(2).strip()
        result = self._eval_arith(expr)
        self._set_var(var, result)

    def _stmt_add(self, rest: str) -> None:
        """ADD a [b ...] TO dest [GIVING result]"""
        giving_m = re.match(r"(.+?)\s+GIVING\s+(\S+)$", rest, re.IGNORECASE)
        if giving_m:
            operands, result_var = giving_m.group(1), giving_m.group(2).upper()
            # Split on TO
            to_m = re.match(r"(.+?)\s+TO\s+(.+)$", operands, re.IGNORECASE)
            if to_m:
                vals = [
                    self._eval(t)
                    for t in re.split(r"\s+", to_m.group(1) + " " + to_m.group(2))
                    if t
                ]
            else:
                vals = [self._eval(t) for t in re.split(r"\s+", operands) if t]
            self._set_var(result_var, sum(float(v) for v in vals))
            return
        to_m = re.match(r"(.+?)\s+TO\s+(\S+(?:\([^)]+\))?)$", rest, re.IGNORECASE)
        if to_m:
            addend = self._eval(to_m.group(1))
            var = to_m.group(2)
            self._set_var(var, float(self._eval(var) or 0) + float(addend))

    def _stmt_subtract(self, rest: str) -> None:
        """SUBTRACT a FROM b [GIVING result]"""
        giving_m = re.match(
            r"(.+?)\s+FROM\s+(.+?)\s+GIVING\s+(\S+)$", rest, re.IGNORECASE
        )
        if giving_m:
            a = self._eval(giving_m.group(1))
            b = self._eval(giving_m.group(2))
            self._set_var(giving_m.group(3).upper(), float(b) - float(a))
            return
        from_m = re.match(r"(.+?)\s+FROM\s+(\S+(?:\([^)]+\))?)$", rest, re.IGNORECASE)
        if from_m:
            a = self._eval(from_m.group(1))
            var = from_m.group(2)
            self._set_var(var, float(self._eval(var) or 0) - float(a))

    def _stmt_multiply(self, rest: str) -> None:
        """MULTIPLY a BY b [GIVING result]"""
        giving_m = re.match(
            r"(.+?)\s+BY\s+(.+?)\s+GIVING\s+(\S+)$", rest, re.IGNORECASE
        )
        if giving_m:
            a, b = self._eval(giving_m.group(1)), self._eval(giving_m.group(2))
            self._set_var(giving_m.group(3).upper(), float(a) * float(b))
            return
        by_m = re.match(r"(.+?)\s+BY\s+(\S+)$", rest, re.IGNORECASE)
        if by_m:
            a = self._eval(by_m.group(1))
            var = by_m.group(2).upper()
            self._set_var(var, float(self._vars.get(var, 0)) * float(a))

    def _stmt_divide(self, rest: str) -> None:
        """DIVIDE a INTO/BY b [GIVING result] [REMAINDER rem]"""
        # DIVIDE a INTO b GIVING c REMAINDER r
        full_m = re.match(
            r"(.+?)\s+INTO\s+(.+?)\s+GIVING\s+(\S+)(?:\s+REMAINDER\s+(\S+))?$",
            rest,
            re.IGNORECASE,
        )
        if full_m:
            a, b = self._eval(full_m.group(1)), self._eval(full_m.group(2))
            if float(a) == 0:
                self._output.append("❌ COBOL: division by zero\n")
                return
            q, r = divmod(float(b), float(a))
            self._set_var(full_m.group(3).upper(), q)
            if full_m.group(4):
                self._set_var(full_m.group(4).upper(), r)
            return
        giving_m = re.match(
            r"(.+?)\s+BY\s+(.+?)\s+GIVING\s+(\S+)$", rest, re.IGNORECASE
        )
        if giving_m:
            a, b = self._eval(giving_m.group(1)), self._eval(giving_m.group(2))
            if float(b) == 0:
                self._output.append("❌ COBOL: division by zero\n")
                return
            self._set_var(giving_m.group(3).upper(), float(a) / float(b))
            return
        into_m = re.match(r"(.+?)\s+INTO\s+(\S+)$", rest, re.IGNORECASE)
        if into_m:
            a = self._eval(into_m.group(1))
            var = into_m.group(2).upper()
            if float(a) == 0:
                self._output.append("❌ COBOL: division by zero\n")
                return
            self._set_var(var, float(self._vars.get(var, 0)) / float(a))

    def _stmt_perform_varying(
        self, para: str, var: str, from_: str, by_: str, until_: str, _line: str
    ) -> None:
        """PERFORM para VARYING var FROM from BY by UNTIL cond"""
        val = float(self._eval(from_))
        step = float(self._eval(by_))
        self._set_var(var, val)
        while not self._eval_cond(until_):
            self._run_paragraph(para)
            val += step
            self._set_var(var, val)

    def _stmt_perform_until(self, para: str, until_: str) -> None:
        """PERFORM para UNTIL cond"""
        limit = 100_000
        for _ in range(limit):
            if self._eval_cond(until_):
                break
            self._run_paragraph(para)
        else:
            self._output.append("❌ COBOL: PERFORM UNTIL exceeded loop limit\n")

    def _stmt_if(self, line: str, block: List[str], idx: int) -> int:
        """IF cond [ELSE ...] END-IF — returns new block index."""
        cond_str = re.sub(r"^IF\s+", "", line, flags=re.IGNORECASE).strip()
        cond = self._eval_cond(cond_str)

        # Collect THEN, ELSE, END-IF from subsequent lines
        then_lines: List[str] = []
        else_lines: List[str] = []
        in_else = False
        depth = 1
        j = idx + 1
        while j < len(block):
            s = block[j].upper().strip()
            if re.match(r"^IF\s+", s):
                depth += 1
            if s == "END-IF":
                depth -= 1
                if depth == 0:
                    j += 1
                    break
            if depth == 1 and s == "ELSE":
                in_else = True
                j += 1
                continue
            if in_else:
                else_lines.append(block[j])
            else:
                then_lines.append(block[j])
            j += 1

        target_lines = then_lines if cond else else_lines
        for stmt in target_lines:
            if stmt.strip():
                self._exec_stmt(stmt, target_lines, target_lines.index(stmt))
        return j

    def _stmt_evaluate(self, line: str, block: List[str], idx: int) -> int:
        """EVALUATE subject WHEN v1 ... WHEN OTHER ... END-EVALUATE"""
        subject_str = re.sub(r"^EVALUATE\s+", "", line, flags=re.IGNORECASE).strip()
        subject_up = subject_str.upper().strip()
        if subject_up == "TRUE":
            subject: Any = True
        elif subject_up == "FALSE":
            subject = False
        else:
            subject = self._eval(subject_str)

        # Collect WHEN blocks up to END-EVALUATE
        when_blocks: List[Tuple[Optional[str], List[str]]] = []
        current_when: Optional[str] = None
        current_stmts: List[str] = []
        j = idx + 1
        while j < len(block):
            s = block[j].upper().strip()
            if s == "END-EVALUATE":
                if current_when is not None:
                    when_blocks.append((current_when, current_stmts[:]))
                j += 1
                break
            if s.startswith("WHEN "):
                if current_when is not None:
                    when_blocks.append((current_when, current_stmts[:]))
                current_when = block[j][5:].strip()
                current_stmts = []
            else:
                if current_when is not None:
                    current_stmts.append(block[j])
            j += 1
        else:
            # Loop exhausted without END-EVALUATE (e.g. parsed as paragraph label)
            if current_when is not None:
                when_blocks.append((current_when, current_stmts[:]))

        # Execute first matching WHEN
        for when_val, stmts in when_blocks:
            if when_val is None:
                continue
            when_up = when_val.upper()
            matched = False
            if when_up in ("OTHER", "ANY"):
                matched = True
            elif isinstance(subject, bool):
                # EVALUATE TRUE/FALSE: when-clauses are boolean conditions
                cond_result = bool(self._eval_cond(when_val))
                matched = cond_result if subject else not cond_result
            else:
                # Support ranges: 1 THRU 5
                thru_m = re.match(r"(.+?)\s+THRU\s+(.+)$", when_val, re.IGNORECASE)
                if thru_m:
                    lo = self._eval(thru_m.group(1))
                    hi = self._eval(thru_m.group(2))
                    matched = float(lo) <= float(subject) <= float(hi)
                else:
                    when_as_val = self._eval(when_val)
                    try:
                        matched = float(str(when_as_val)) == float(str(subject))
                    except (ValueError, TypeError):
                        matched = str(when_as_val) == str(subject)
            if matched:
                for stmt in stmts:
                    if stmt.strip():
                        self._exec_stmt(stmt, stmts, stmts.index(stmt))
                break
        return j

    def _stmt_accept(self, var: str) -> None:
        """ACCEPT var — get input from interpreter pending input."""
        var = var.upper()
        val = getattr(self._interp, "pending_input", None)
        if val is not None:
            self._interp.pending_input = None  # type: ignore[attr-defined]
        else:
            val = "0"
        if _pic_is_numeric(self._pics.get(var, "9")):
            try:
                self._vars[var] = int(val) if "." not in val else float(val)
            except ValueError:
                self._vars[var] = 0
        else:
            self._vars[var] = str(val)

    def _stmt_string(self, rest: str) -> None:
        """STRING src1 [DELIMITED BY x] [src2 ...] INTO dest"""
        into_m = re.match(r"(.+?)\s+INTO\s+(\S+)$", rest, re.IGNORECASE)
        if not into_m:
            return
        sources_str, dest = into_m.group(1), into_m.group(2).upper()
        # Split on DELIMITED BY SPACE / SIZE / literal
        parts_raw = re.split(
            r"\s+DELIMITED\s+BY\s+\S+", sources_str, flags=re.IGNORECASE
        )
        result = "".join(str(self._eval(p.strip())) for p in parts_raw if p.strip())
        self._set_var(dest, result)

    def _stmt_inspect(self, rest: str) -> None:
        """INSPECT var TALLYING/REPLACING (basic)."""
        m = re.match(
            r"(\S+)\s+REPLACING\s+ALL\s+(.+?)\s+BY\s+(.+)$", rest, re.IGNORECASE
        )
        if m:
            var, old, new = (
                m.group(1).upper(),
                self._eval(m.group(2)),
                self._eval(m.group(3)),
            )
            self._set_var(var, str(self._vars.get(var, "")).replace(str(old), str(new)))
        # TALLYING is parsed but result storage is non-trivial; skip for now.

    # ------------------------------------------------------------------
    # Evaluation helpers
    # ------------------------------------------------------------------

    def _resolve_subscript(self, token: str) -> Tuple[Optional[str], Optional[int]]:
        """If *token* is NAME(IDX), return (name, 0-based index). Else (None, None)."""
        m = re.match(
            r"^([A-Z0-9][A-Z0-9\-]*)\s*\((.+)\)$", token.strip(), re.IGNORECASE
        )
        if m:
            name = m.group(1).upper()
            if name in self._occurs:
                idx_val = self._eval(m.group(2).strip())
                try:
                    return name, int(float(idx_val)) - 1  # COBOL is 1-based
                except (TypeError, ValueError):
                    return name, 0
        return None, None

    def _eval(self, token: str) -> Any:
        """Evaluate a COBOL token — literal, variable, or simple arithmetic."""
        token = token.strip().rstrip(".")
        # Quoted string literal
        if (token.startswith('"') and token.endswith('"')) or (
            token.startswith("'") and token.endswith("'")
        ):
            return token[1:-1]
        # Numeric literal (possibly negative)
        try:
            return float(token) if "." in token else int(token)
        except ValueError:
            pass
        # Subscripted table reference: WS-ITEM(I)
        tname, tidx = self._resolve_subscript(token)
        if tname is not None:
            tbl = self._vars.get(tname, [])
            if isinstance(tbl, list) and 0 <= tidx < len(tbl):
                return tbl[tidx]
            return 0
        # Variable lookup
        up = token.upper()
        if up in self._vars:
            val = self._vars[up]
            return val
        # SPECIAL-NAMES
        if up == "ZERO" or up == "ZEROS" or up == "ZEROES":
            return 0
        if up == "SPACE" or up == "SPACES":
            return " "
        if up == "HIGH-VALUE" or up == "HIGH-VALUES":
            return "\xff"
        if up == "LOW-VALUE" or up == "LOW-VALUES":
            return "\x00"
        # Inline arithmetic expression (COMPUTE-style)
        try:
            return self._eval_arith(token)
        except Exception:  # noqa: BLE001
            return token  # fall through as string

    def _eval_arith(self, expr: str) -> float:
        """Evaluate a COBOL arithmetic expression, substituting variable values."""

        # Pre-substitute subscripted table references: WS-ITEM(I) → value
        def _sub_subscript(m: re.Match) -> str:
            name = m.group(1).upper()
            if name in self._occurs:
                idx_str = m.group(2).strip()
                idx_val = self._eval(idx_str)
                try:
                    idx0 = int(float(idx_val)) - 1
                except (TypeError, ValueError):
                    idx0 = 0
                tbl = self._vars.get(name, [])
                if isinstance(tbl, list) and 0 <= idx0 < len(tbl):
                    try:
                        return str(float(tbl[idx0]))
                    except (TypeError, ValueError):
                        return "0"
            return m.group(0)

        expr = re.sub(
            r"([A-Z][A-Z0-9\-]*)\s*\(([^)]+)\)",
            _sub_subscript,
            expr,
            flags=re.IGNORECASE,
        )

        # Replace variable names with their numeric values
        def _sub(m: re.Match) -> str:
            name = m.group(0).upper()
            val = self._vars.get(name)
            if val is not None and not isinstance(val, list):
                try:
                    return str(float(val))
                except (TypeError, ValueError):
                    pass
            return m.group(0)

        # Replace ** with ^ then handle
        e = re.sub(r"\*\*", "**", expr)
        e = re.sub(r"\b[A-Z][A-Z0-9\-]*\b", _sub, e, flags=re.IGNORECASE)
        # Replace COBOL function calls: FUNCTION SQRT(x)
        e = re.sub(
            r"\bFUNCTION\s+SQRT\s*\(([^)]+)\)", r"math.sqrt(\1)", e, flags=re.IGNORECASE
        )
        e = re.sub(
            r"\bFUNCTION\s+ABS\s*\(([^)]+)\)", r"abs(\1)", e, flags=re.IGNORECASE
        )
        e = re.sub(
            r"\bFUNCTION\s+INTEGER\s*\(([^)]+)\)", r"int(\1)", e, flags=re.IGNORECASE
        )
        try:
            # Restrict evaluation to safe math operations
            result = eval(
                e,
                {
                    "__builtins__": {},
                    "math": math,
                    "abs": abs,
                    "int": int,
                    "float": float,
                },
            )  # noqa: S307,PGH001
            return float(result)
        except Exception:  # noqa: BLE001
            try:
                return float(expr)
            except ValueError:
                return 0.0

    def _eval_cond(self, cond: str) -> bool:
        """Evaluate a COBOL conditional expression."""
        cond = cond.strip()
        if not cond:
            return False

        # AND / OR (top-level split, respecting nesting)
        for op in (" AND ", " OR "):
            idx = self._find_op(cond.upper(), op)
            if idx >= 0:
                left = self._eval_cond(cond[:idx])
                right = self._eval_cond(cond[idx + len(op) :])
                if op.strip() == "AND":
                    return left and right
                return left or right

        # NOT
        if cond.upper().startswith("NOT "):
            return not self._eval_cond(cond[4:])

        # IS [NOT] NUMERIC / ALPHABETIC
        m = re.match(r"(\S+)\s+IS\s+(NOT\s+)?NUMERIC$", cond, re.IGNORECASE)
        if m:
            v = self._eval(m.group(1))
            try:
                float(str(v))
                result = True
            except ValueError:
                result = False
            return result if not m.group(2) else not result

        # Comparisons: = < > <= >= <> NOT EQUAL GREATER LESS
        _CMP_RE = re.compile(
            r"^(.+?)\s*(>=|<=|<>|>|<|=|NOT\s*=|NOT\s+EQUAL|EQUAL|GREATER|LESS)\s*(?:TO\s+|THAN\s+)?(.+)$",
            re.IGNORECASE,
        )
        cm = _CMP_RE.match(cond)
        if cm:
            left_v = self._eval(cm.group(1).strip())
            op = cm.group(2).upper().strip()
            right_v = self._eval(cm.group(3).strip())
            try:
                lf, rf = float(left_v), float(right_v)  # type: ignore[arg-type]
                return self._compare(lf, op, rf)
            except (TypeError, ValueError):
                ls, rs = str(left_v), str(right_v)
                return self._compare(ls, op, rs)

        # Boolean variable / condition name
        val = self._eval(cond)
        return bool(val) and val not in (0, "0", "", "FALSE", False)

    def _compare(self, a: Any, op: str, b: Any) -> bool:
        if op in ("=", "EQUAL"):
            return a == b
        if op in ("<>", "NOT EQUAL", "NOT="):
            return a != b
        if op in (">", "GREATER"):
            return a > b
        if op in ("<", "LESS"):
            return a < b
        if op == ">=":
            return a >= b
        if op == "<=":
            return a <= b
        return False

    @staticmethod
    def _find_op(s: str, op: str) -> int:
        """Find *op* in *s* at depth 0 (not inside parentheses)."""
        depth = 0
        i = 0
        while i <= len(s) - len(op):
            c = s[i]
            if c == "(":
                depth += 1
            elif c == ")":
                depth -= 1
            elif depth == 0 and s[i : i + len(op)] == op:
                return i
            i += 1
        return -1

    def _set_var(self, name: str, value: Any) -> None:
        """Set a variable (or table element), coercing to the declared PIC type."""
        # Subscripted assignment: WS-ITEM(I)
        tname, tidx = self._resolve_subscript(name)
        if tname is not None:
            tbl = self._vars.get(tname)
            if isinstance(tbl, list) and 0 <= tidx < len(tbl):
                pic = self._pics.get(tname, "")
                if pic and _pic_is_numeric(pic):
                    try:
                        fval = float(value)
                        tbl[tidx] = int(fval) if fval == int(fval) else fval
                    except (TypeError, ValueError):
                        tbl[tidx] = 0
                else:
                    tbl[tidx] = str(value)
            return
        name = name.upper()
        pic = self._pics.get(name, "")
        if pic and _pic_is_numeric(pic):
            try:
                fval = float(value)
                self._vars[name] = int(fval) if fval == int(fval) else fval
            except (TypeError, ValueError):
                self._vars[name] = 0
        else:
            self._vars[name] = str(value)

    def _get_pic_default(self, name: str) -> Any:
        pic = self._pics.get(name.upper(), "")
        return _pic_default(pic) if pic else 0
