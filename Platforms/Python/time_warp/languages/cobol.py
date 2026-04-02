"""COBOL language executor for Time Warp Studio.

Educational COBOL interpreter — whole-program execution.
Supports a teaching subset of COBOL-85:
  - IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE DIVISION headers
  - DISPLAY, MOVE, ADD/SUBTRACT/MULTIPLY/DIVIDE, COMPUTE
  - IF/ELSE/END-IF, PERFORM (simple and VARYING), STOP RUN
  - Working-storage variables with PIC clauses
"""

from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_cobol(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a complete COBOL program and return all output."""
    env = CobolEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------


class CobolEnvironment:
    # COBOL end-scope markers — must NOT be treated as paragraph names
    _END_MARKERS = frozenset(
        {
            "END-PERFORM",
            "END-IF",
            "END-EVALUATE",
            "END-READ",
            "END-WRITE",
            "END-SEARCH",
            "END-COMPUTE",
            "END-ADD",
            "END-SUBTRACT",
            "END-MULTIPLY",
            "END-DIVIDE",
            "END-STRING",
            "END-UNSTRING",
            "END-CALL",
            "END-START",
            "END-DELETE",
            "END-REWRITE",
            "END-RETURN",
            "END-ACCEPT",
            "END-DISPLAY",
            "STOP",
        }
    )

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []
        self._vars: dict[str, Any] = {}
        self._paragraphs: dict[str, list[str]] = {}

    def _emit(self, text: str):
        self._output.append(str(text))

    def run(self, source: str) -> str:
        try:
            lines = self._preprocess(source)
            self._parse_divisions(lines)
            self._run_procedure()
        except CobolStop:
            pass
        except CobolError as e:
            self._emit(f"❌ COBOL error: {e}")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}")
        return "\n".join(self._output)

    # ------------------------------------------------------------------
    # Preprocessing
    # ------------------------------------------------------------------

    def _preprocess(self, source: str) -> list[str]:
        """Strip comments, handle continuation, normalise case."""
        raw_lines = source.splitlines()
        fixed = self._detect_fixed_format(raw_lines)
        lines = []
        for raw in raw_lines:
            if fixed:
                # Fixed-format: col 7 is continuation marker, 1-6 are sequence/area
                if len(raw) >= 7 and raw[6] == "*":
                    continue  # comment line
                # Take cols 7-72 (0-indexed 6-71), strip trailing
                if len(raw) >= 7:
                    stmt = raw[6:72].rstrip()
                else:
                    stmt = raw.rstrip()
            else:
                # Free-format: just strip whitespace
                stmt = raw.strip()
                if stmt.startswith("*"):
                    continue  # comment line
            if stmt:
                lines.append(self._upper_preserve_strings(stmt))
        return lines

    def _detect_fixed_format(self, raw_lines: list[str]) -> bool:
        """Return True if source uses COBOL fixed-format (cols 1-6 sequence area)."""
        for line in raw_lines:
            if len(line) < 7:
                continue
            prefix = line[:6]
            # Fixed format: cols 1-6 must be blank or sequence numbers
            if prefix.strip() == "" or prefix.replace(" ", "").isdigit():
                indicator = line[6]
                if indicator in ("*", "-", "/"):
                    return True
        return False

    @staticmethod
    def _upper_preserve_strings(line: str) -> str:
        """Uppercase COBOL keywords but preserve quoted string content."""
        from .lang_utils import upper_preserve_strings
        return upper_preserve_strings(line)

    # ------------------------------------------------------------------
    # Division parser
    # ------------------------------------------------------------------

    def _parse_divisions(self, lines: list[str]):
        # Check if any DIVISION header exists
        has_divs = any("DIVISION" in line for line in lines)
        if not has_divs:
            self._parse_implicit(lines)
            return

        division = None
        para_name = None
        para_lines: list[str] = []

        for line in lines:
            # Division headers
            if "DIVISION" in line:
                if para_name:
                    self._paragraphs[para_name] = para_lines[:]
                para_name = None
                para_lines = []
                if "IDENTIFICATION" in line:
                    division = "ID"
                elif "ENVIRONMENT" in line:
                    division = "ENV"
                elif "DATA" in line:
                    division = "DATA"
                elif "PROCEDURE" in line:
                    division = "PROC"
                continue

            # Skip section headers
            if re.match(
                r"^(?:WORKING-STORAGE|FILE|LINKAGE|LOCAL-STORAGE)\s+SECTION", line, re.I
            ):
                if division != "DATA":
                    division = "DATA"
                continue

            if division == "DATA":
                self._parse_data_line(line)

            elif division == "PROC":
                # Paragraph name: line ending in a period, standalone word
                m = re.match(r"^([A-Z0-9][\w-]*)\s*\.\s*$", line)
                if m and m.group(1) not in self._END_MARKERS:
                    if para_name:
                        self._paragraphs[para_name] = para_lines[:]
                    para_name = m.group(1)
                    para_lines = []
                else:
                    if para_name is None:
                        para_name = "__MAIN__"
                    para_lines.append(line)

        if para_name:
            self._paragraphs[para_name] = para_lines

    def _parse_implicit(self, lines: list[str]):
        """Handle free-form code without DIVISION headers."""
        proc_lines = []
        for line in lines:
            stripped = line.strip()
            if not stripped:
                continue
            # Skip section headers
            if re.match(
                r"^(?:WORKING-STORAGE|FILE|LINKAGE|LOCAL-STORAGE)\s+SECTION",
                stripped,
                re.I,
            ):
                continue
            # Data declarations
            if re.match(r"^(?:0?1|77|05)\s+[\w-]+\s+PIC\s+", stripped):
                self._parse_data_line(stripped)
            else:
                proc_lines.append(line)
        # Parse procedure lines for paragraphs
        para_name = "__MAIN__"
        para_lines: list[str] = []
        for line in proc_lines:
            m = re.match(r"^([A-Z0-9][\w-]*)\s*\.\s*$", line)
            if m and m.group(1) not in self._END_MARKERS:
                if para_lines:
                    self._paragraphs[para_name] = para_lines[:]
                para_name = m.group(1)
                para_lines = []
            else:
                para_lines.append(line)
        if para_lines:
            self._paragraphs[para_name] = para_lines

    def _parse_data_line(self, line: str):
        """Parse working-storage variable declarations."""
        # Level 01/77  VAR-NAME  PIC X(10) VALUE SPACES.
        m = re.match(
            r"^(?:0?1|77|05)\s+([\w-]+)\s+PIC\s+([\w()\d]+)(?:\s+VALUE\s+(.+?))?\.?\s*$",
            line,
        )
        if m:
            name = m.group(1)
            pic = m.group(2)
            value_str = (m.group(3) or "").strip().rstrip(".")
            self._vars[name] = self._pic_default(pic, value_str)

    def _pic_default(self, pic: str, value_str: str) -> Any:
        is_numeric = re.match(r"^[9S()\d+Vv.-]+$", pic) is not None
        if value_str.upper() in ("SPACES", "SPACE", ""):
            return "" if not is_numeric else 0
        if value_str.upper() in ("ZEROS", "ZEROES", "ZERO"):
            return 0 if is_numeric else "0"
        if value_str.startswith('"') or value_str.startswith("'"):
            return value_str.strip("\"'")
        try:
            return int(value_str) if is_numeric else value_str.strip("\"'")
        except ValueError:
            try:
                return float(value_str)
            except ValueError:
                return value_str.strip("\"'")

    # ------------------------------------------------------------------
    # Execution
    # ------------------------------------------------------------------

    def _run_procedure(self):
        # Execute paragraphs in order
        executed_names = list(self._paragraphs.keys())
        performed = set()
        i = 0
        while i < len(executed_names):
            name = executed_names[i]
            self._exec_paragraph(name, performed)
            i += 1

    def _exec_paragraph(self, name: str, call_stack_guard: set):
        if name not in self._paragraphs:
            raise CobolError(f"Paragraph not found: {name}")
        lines = self._paragraphs[name]
        self._exec_stmts(lines)

    def _exec_stmts(self, lines: list[str]):
        i = 0
        while i < len(lines):
            line = lines[i].strip()
            if not line:
                i += 1
                continue
            # Merge EXEC SQL ... END-EXEC blocks (may span multiple lines)
            if re.match(r"^EXEC\s+SQL\b", line, re.I):
                buf = [line]
                i += 1
                while i < len(lines):
                    seg = lines[i].strip()
                    buf.append(seg)
                    i += 1
                    if re.search(r"END-EXEC", seg, re.I):
                        break
                self._exec_stmt(" ".join(buf))
                continue
            # Gather multi-line statement (ends at period or COBOL keyword)
            stmt, consumed = self._gather_stmt(lines, i)
            self._exec_stmt(stmt.strip())
            i += consumed

    def _gather_stmt(self, lines: list[str], start: int) -> tuple[str, int]:
        """Gather a logical statement ending in '.'"""
        parts = []
        i = start
        while i < len(lines):
            seg = lines[i].strip()
            if seg.endswith("."):
                parts.append(seg.rstrip("."))
                return " ".join(p for p in parts if p), i - start + 1
            parts.append(seg)
            i += 1
        return " ".join(p for p in parts if p), len(lines) - start

    def _exec_stmt(self, stmt: str):
        if not stmt:
            return
        stmt = stmt.strip().rstrip(".")

        # STOP RUN
        if re.match(r"^STOP\s+RUN$", stmt):
            raise CobolStop()

        # DISPLAY
        m = re.match(r"^DISPLAY\s+(.+?)(?:\s+WITH\s+NO\s+ADVANCING)?$", stmt)
        if m:
            self._emit(self._resolve_str(m.group(1).strip()))
            return

        # MOVE
        m = re.match(r"^MOVE\s+(.+?)\s+TO\s+(.+)$", stmt)
        if m:
            val = self._eval(m.group(1).strip())
            targets = m.group(2).strip().split()
            for t in targets:
                self._vars[t] = val
            return

        # ADD x TO y [GIVING z]
        m = re.match(
            r"^ADD\s+(.+?)\s+TO\s+(\w[\w-]*?)(?:\s+GIVING\s+(\w[\w-]*))?$", stmt
        )
        if m:
            a = self._eval(m.group(1).strip())
            b = self._eval(m.group(2).strip())
            result = _to_num(a) + _to_num(b)
            dest = m.group(3).strip() if m.group(3) else m.group(2).strip()
            self._vars[dest] = result
            return

        # SUBTRACT x FROM y [GIVING z]
        m = re.match(
            r"^SUBTRACT\s+(.+?)\s+FROM\s+(\w[\w-]*?)(?:\s+GIVING\s+(\w[\w-]*))?$", stmt
        )
        if m:
            a = _to_num(self._eval(m.group(1).strip()))
            b = _to_num(self._eval(m.group(2).strip()))
            result = b - a
            dest = m.group(3).strip() if m.group(3) else m.group(2).strip()
            self._vars[dest] = result
            return

        # MULTIPLY x BY y [GIVING z]
        m = re.match(
            r"^MULTIPLY\s+(.+?)\s+BY\s+(\w[\w-]*?)(?:\s+GIVING\s+(\w[\w-]*))?$", stmt
        )
        if m:
            a = _to_num(self._eval(m.group(1).strip()))
            b = _to_num(self._eval(m.group(2).strip()))
            result = a * b
            dest = m.group(3).strip() if m.group(3) else m.group(2).strip()
            self._vars[dest] = result
            return

        # DIVIDE x INTO y [GIVING z]
        m = re.match(
            r"^DIVIDE\s+(.+?)\s+INTO\s+(\w[\w-]*?)(?:\s+GIVING\s+(\w[\w-]*))?$", stmt
        )
        if m:
            a = _to_num(self._eval(m.group(1).strip()))
            b = _to_num(self._eval(m.group(2).strip()))
            result = b / a if a else 0
            dest = m.group(3).strip() if m.group(3) else m.group(2).strip()
            self._vars[dest] = result
            return

        # COMPUTE
        m = re.match(r"^COMPUTE\s+([\w-]+)\s*=\s*(.+)$", stmt)
        if m:
            val = self._arith(m.group(2).strip())
            self._vars[m.group(1).strip()] = val
            return

        # PERFORM ... END-PERFORM (inline blocks)
        if re.match(r"^PERFORM\b", stmt) and "END-PERFORM" in stmt:
            self._exec_perform_block(stmt)
            return

        # PERFORM VARYING
        m = re.match(
            r"^PERFORM\s+VARYING\s+(\w[\w-]*)\s+FROM\s+(.+?)\s+BY\s+(.+?)\s+UNTIL\s+(.+)$",
            stmt,
        )
        if m:
            var = m.group(1)
            frm = _to_num(self._eval(m.group(2).strip()))
            by = _to_num(self._eval(m.group(3).strip()))
            until_expr = m.group(4).strip()
            self._vars[var] = frm
            limit = 10000
            count = 0
            # The body must follow — this simple impl just loops the counter
            while not self._eval_cond(until_expr):
                if count >= limit:
                    break
                self._vars[var] = _to_num(self._vars.get(var, 0)) + by
                count += 1
            return

        # PERFORM paragraph-name
        m = re.match(r"^PERFORM\s+([\w-]+)$", stmt)
        if m:
            self._exec_paragraph(m.group(1), set())
            return

        # PERFORM paragraph THROUGH/THRU paragraph
        m = re.match(r"^PERFORM\s+([\w-]+)\s+(?:THROUGH|THRU)\s+([\w-]+)$", stmt)
        if m:
            self._exec_paragraph(m.group(1), set())
            return

        # IF ... THEN ... [ELSE ...] END-IF
        if stmt.startswith("IF ") or stmt.startswith("IF\t"):
            self._exec_if_inline(stmt)
            return

        # ACCEPT
        m = re.match(r"^ACCEPT\s+([\w-]+)$", stmt)
        if m:
            val = (
                self.interpreter.request_input(f"Enter {m.group(1)}: ")
                if hasattr(self.interpreter, "request_input")
                else ""
            )
            self._vars[m.group(1)] = val
            return

        # CONTINUE (no-op)
        if re.match(r"^CONTINUE$", stmt, re.I):
            return

        # GOBACK  (treated as STOP RUN)
        if re.match(r"^GOBACK$", stmt, re.I):
            raise CobolStop()

        # NEXT SENTENCE
        if re.match(r"^NEXT\s+SENTENCE$", stmt, re.I):
            return

        # EVALUATE (CASE / SELECT)
        if re.match(r"^EVALUATE\b", stmt, re.I):
            body = re.sub(r"\s*END-EVALUATE\s*$", "", stmt, flags=re.I).strip()
            m2 = re.match(r"EVALUATE\s+(.+?)\s+WHEN\b(.*)", body, re.I | re.DOTALL)
            if m2:
                subject = self._eval(m2.group(1).strip())
                when_rest = "WHEN" + m2.group(2)
                clauses = re.split(r"\bWHEN\b", when_rest, flags=re.I)
                matched = False
                for clause in clauses:
                    clause = clause.strip()
                    if not clause:
                        continue
                    if re.match(r"^OTHER\s*", clause, re.I):
                        action = re.sub(r"^OTHER\s*", "", clause, flags=re.I).strip()
                        if not matched:
                            self._exec_stmt(action)
                        break
                    m3 = re.match(r"(.+?)\s+(.+)$", clause, re.DOTALL)
                    if m3 and not matched:
                        val_str = m3.group(1).strip()
                        action = m3.group(2).strip()
                        if re.search(r"\bTHROUGH\b|\bTHRU\b", val_str, re.I):
                            parts_ = re.split(
                                r"\bTHROUGH\b|\bTHRU\b", val_str, maxsplit=1, flags=re.I
                            )
                            low = _to_num(self._eval(parts_[0].strip()))
                            high = _to_num(self._eval(parts_[1].strip()))
                            test = low <= _to_num(subject) <= high
                        else:
                            test = self._eval(val_str) == subject
                        if test:
                            self._exec_stmt(action)
                            matched = True
            return

        # SET var TO value
        m = re.match(r"^SET\s+([\w-]+)\s+TO\s+(.+)$", stmt, re.I)
        if m:
            var = m.group(1).upper()
            val = self._eval(m.group(2).strip())
            self._vars[var] = val
            return

        # INITIALIZE vars
        m = re.match(r"^INITIALIZE\s+(.+)$", stmt, re.I)
        if m:
            for t in m.group(1).split():
                t = t.upper()
                current = self._vars.get(t)
                if isinstance(current, str):
                    self._vars[t] = " " * len(current) if current else ""
                else:
                    self._vars[t] = 0
            return

        # STRING ... DELIMITED BY ... INTO var
        m = re.match(r"^STRING\s+(.+?)\s+INTO\s+([\w-]+)$", stmt, re.I)
        if m:
            src_part = m.group(1)
            dest_var = m.group(2).upper()
            # Extract pieces before DELIMITED tokens
            pieces = re.split(
                r"\s+DELIMITED\s+(?:BY\s+)?(?:SIZE|SPACE|SPACES|['\"][^'\"]*['\"]|[\w-]+)\s*",
                src_part,
                flags=re.I,
            )
            result_str = "".join(
                self._resolve_str(p.strip()) for p in pieces if p.strip()
            )
            self._vars[dest_var] = result_str
            return

        # UNSTRING src DELIMITED BY delim INTO v1 [, v2 ...]
        m = re.match(
            r"^UNSTRING\s+([\w-]+)\s+DELIMITED\s+(?:BY\s+)?(.+?)\s+INTO\s+(.+)$",
            stmt,
            re.I,
        )
        if m:
            src_var = m.group(1).upper()
            delim_raw = m.group(2).strip()
            # Multiple delimiters: split on " OR "
            delims = [
                self._resolve_str(d.strip())
                for d in re.split(r"\s+OR\s+", delim_raw, flags=re.I)
            ]
            targets_str = m.group(3)
            targets = [t.strip() for t in re.split(r",\s*", targets_str) if t.strip()]
            src_val = str(self._vars.get(src_var, ""))
            # Use first delimiter for split (multi-delimiter approximation)
            delim = delims[0] if delims else " "
            tokens = src_val.split(delim) if delim else list(src_val)
            for i, tgt in enumerate(targets):
                tgt_name = re.sub(r"\s.*", "", tgt).upper()
                self._vars[tgt_name] = tokens[i] if i < len(tokens) else ""
            return

        # INSPECT TALLYING
        m = re.match(
            r"^INSPECT\s+([\w-]+)\s+TALLYING\s+([\w-]+)\s+FOR\s+(ALL|LEADING|TRAILING)\s+(.+)$",
            stmt,
            re.I,
        )
        if m:
            var = m.group(1).upper()
            counter = m.group(2).upper()
            mode = m.group(3).upper()
            target = self._resolve_str(m.group(4).strip())
            src = str(self._vars.get(var, ""))
            if mode == "ALL":
                count = src.count(target)
            elif mode == "LEADING":
                count = 0
                for ch in src:
                    if ch == target[0:1]:
                        count += 1
                    else:
                        break
            elif mode == "TRAILING":
                count = 0
                for ch in reversed(src):
                    if ch == target[-1:]:
                        count += 1
                    else:
                        break
            else:
                count = 0
            self._vars[counter] = _to_num(self._vars.get(counter, 0)) + count
            return

        # INSPECT REPLACING
        m = re.match(
            r"^INSPECT\s+([\w-]+)\s+REPLACING\s+(ALL|LEADING|FIRST)\s+(.+?)\s+BY\s+(.+)$",
            stmt,
            re.I,
        )
        if m:
            var = m.group(1).upper()
            mode = m.group(2).upper()
            find = self._resolve_str(m.group(3).strip())
            repl = self._resolve_str(m.group(4).strip())
            src = str(self._vars.get(var, ""))
            if mode in ("ALL", "LEADING"):
                self._vars[var] = src.replace(find, repl)
            elif mode == "FIRST":
                self._vars[var] = src.replace(find, repl, 1)
            return

        # INSPECT CONVERTING
        m = re.match(
            r"^INSPECT\s+([\w-]+)\s+CONVERTING\s+(.+?)\s+TO\s+(.+)$", stmt, re.I
        )
        if m:
            var = m.group(1).upper()
            from_str = self._resolve_str(m.group(2).strip())
            to_str = self._resolve_str(m.group(3).strip())
            src = str(self._vars.get(var, ""))
            tbl = str.maketrans(from_str, to_str[: len(from_str)])
            self._vars[var] = src.translate(tbl)
            return

        # PERFORM n TIMES (para)
        m = re.match(r"^PERFORM\s+([\w-]+)\s+(\d+)\s+TIMES$", stmt, re.I)
        if m:
            para = m.group(1)
            n = int(m.group(2))
            for _ in range(min(n, 10000)):
                self._exec_paragraph(para, set())
            return

        # PERFORM UNTIL (inline body)
        m = re.match(
            r"^PERFORM\s+UNTIL\s+(.+?)\s+(PERFORM\s+.+|DISPLAY\s+.+|MOVE\s+.+|ADD\s+.+)$",
            stmt,
            re.I,
        )
        if m:
            until_cond = m.group(1).strip()
            body_stmt = m.group(2).strip()
            limit = 10000
            count = 0
            while not self._eval_cond(until_cond) and count < limit:
                self._exec_stmt(body_stmt)
                count += 1
            return

        # ADD ... GIVING
        m = re.match(
            r"^ADD\s+(.+?)\s+(?:AND\s+(.+?)\s+)?GIVING\s+([\w-]+)$", stmt, re.I
        )
        if m:
            a = _to_num(self._eval(m.group(1).strip()))
            b = _to_num(self._eval(m.group(2).strip())) if m.group(2) else 0
            self._vars[m.group(3).strip().upper()] = a + b
            return

        # SUBTRACT ... FROM ... GIVING
        m = re.match(
            r"^SUBTRACT\s+(.+?)\s+FROM\s+(.+?)\s+GIVING\s+([\w-]+)$", stmt, re.I
        )
        if m:
            a = _to_num(self._eval(m.group(1).strip()))
            b = _to_num(self._eval(m.group(2).strip()))
            self._vars[m.group(3).strip().upper()] = b - a
            return

        # DIVIDE ... INTO/BY ... GIVING ... [REMAINDER ...]
        m = re.match(
            r"^DIVIDE\s+(.+?)\s+(INTO|BY)\s+(.+?)\s+GIVING\s+([\w-]+)(?:\s+REMAINDER\s+([\w-]+))?$",
            stmt,
            re.I,
        )
        if m:
            a = _to_num(self._eval(m.group(1).strip()))
            keyword = m.group(2).upper()
            b = _to_num(self._eval(m.group(3).strip()))
            if keyword == "BY":
                # DIVIDE A BY B GIVING C  →  C = A / B
                divisor, dividend = b, a
            else:
                # DIVIDE A INTO B GIVING C  →  C = B / A
                divisor, dividend = a, b
            result = int(dividend / divisor) if divisor else 0
            rem = dividend % divisor if divisor else 0
            self._vars[m.group(4).strip().upper()] = result
            if m.group(5):
                self._vars[m.group(5).strip().upper()] = rem
            return

        # MOVE CORRESPONDING
        m = re.match(r"^MOVE\s+CORRESPONDING\s+([\w-]+)\s+TO\s+([\w-]+)$", stmt, re.I)
        if m:
            src_prefix = m.group(1).upper()
            dst_prefix = m.group(2).upper()
            for key, val in list(self._vars.items()):
                if key.startswith(src_prefix + "-") or key.startswith(src_prefix + "."):
                    suffix = key[len(src_prefix) :]
                    dst_key = dst_prefix + suffix
                    self._vars[dst_key] = val
            return

        # ADD CORRESPONDING
        m = re.match(r"^ADD\s+CORRESPONDING\s+([\w-]+)\s+TO\s+([\w-]+)$", stmt, re.I)
        if m:
            src_prefix = m.group(1).upper()
            dst_prefix = m.group(2).upper()
            for key, val in list(self._vars.items()):
                if key.startswith(src_prefix + "-") or key.startswith(src_prefix + "."):
                    suffix = key[len(src_prefix) :]
                    dst_key = dst_prefix + suffix
                    if dst_key in self._vars:
                        self._vars[dst_key] = _to_num(self._vars[dst_key]) + _to_num(
                            val
                        )
            return

        # CALL external program
        m = re.match(r"^CALL\s+['\"]?([\w-]+)['\"]?\s*(?:USING\s+(.+))?$", stmt, re.I)
        if m:
            prog = m.group(1).upper()
            self._emit(f"ℹ️  CALL {prog} — external call simulated")
            return

        # OPEN file
        m = re.match(r"^OPEN\s+(INPUT|OUTPUT|I-O|EXTEND)\s+(.+)$", stmt, re.I)
        if m:
            mode = m.group(1).upper()
            for fname in m.group(2).split():
                fname = fname.upper()
                if not hasattr(self, "_files"):
                    self._files: dict = {}
                self._files[fname] = {"mode": mode, "records": [], "pos": 0}
            self._vars["WS-FILE-STATUS"] = "00"
            return

        # CLOSE file(s)
        m = re.match(r"^CLOSE\s+(.+)$", stmt, re.I)
        if m and not re.search(r"\bINPUT\b|\bOUTPUT\b|\bI-O\b|\bEXTEND\b", stmt, re.I):
            for fname in m.group(1).split():
                fname = fname.upper()
                if hasattr(self, "_files"):
                    self._files.pop(fname, None)
            self._vars["WS-FILE-STATUS"] = "00"
            return

        # WRITE record [FROM var]
        m = re.match(r"^WRITE\s+([\w-]+)\s*(?:FROM\s+([\w-]+))?", stmt, re.I)
        if m:
            rec_var = m.group(1).upper()
            src_var = m.group(2).upper() if m.group(2) else rec_var
            val = self._vars.get(src_var, self._vars.get(rec_var, ""))
            if hasattr(self, "_files"):
                for f in self._files.values():
                    if f["mode"] in ("OUTPUT", "I-O", "EXTEND"):
                        f["records"].append(str(val))
                        break
            self._vars["WS-FILE-STATUS"] = "00"
            return

        # READ file [INTO var] [AT END stmt]
        m = re.match(
            r"^READ\s+([\w-]+)\s*(?:INTO\s+([\w-]+))?\s*(?:AT\s+END\s+(.+))?$",
            stmt,
            re.I,
        )
        if m:
            fname = m.group(1).upper()
            into_var = m.group(2).upper() if m.group(2) else None
            at_end_stmt = (m.group(3) or "").strip()
            if hasattr(self, "_files") and fname in self._files:
                f = self._files[fname]
                if f["pos"] < len(f["records"]):
                    rec = f["records"][f["pos"]]
                    f["pos"] += 1
                    if into_var:
                        self._vars[into_var] = rec
                    self._vars["WS-FILE-STATUS"] = "00"
                else:
                    self._vars["WS-FILE-STATUS"] = "10"
                    if at_end_stmt:
                        self._exec_stmt(at_end_stmt)
            else:
                self._vars["WS-FILE-STATUS"] = "35"
                if at_end_stmt:
                    self._exec_stmt(at_end_stmt)
            return

        # REWRITE record [FROM var]
        m = re.match(r"^REWRITE\s+([\w-]+)\s*(?:FROM\s+([\w-]+))?$", stmt, re.I)
        if m:
            rec_var = m.group(1).upper()
            src_var = m.group(2).upper() if m.group(2) else rec_var
            val = self._vars.get(src_var, "")
            if hasattr(self, "_files"):
                for f in self._files.values():
                    pos = f.get("pos", 1)
                    if 0 < pos <= len(f["records"]):
                        f["records"][pos - 1] = str(val)
                        break
            self._vars["WS-FILE-STATUS"] = "00"
            return

        # DELETE file RECORD
        m = re.match(r"^DELETE\s+([\w-]+)\s+RECORD$", stmt, re.I)
        if m:
            fname = m.group(1).upper()
            if hasattr(self, "_files") and fname in self._files:
                f = self._files[fname]
                pos = f.get("pos", 1)
                if 0 < pos <= len(f["records"]):
                    del f["records"][pos - 1]
            self._vars["WS-FILE-STATUS"] = "00"
            return

        # SORT table [ON ASCENDING/DESCENDING KEY field]
        m = re.match(
            r"^SORT\s+([\w-]+)\s+ON\s+(ASCENDING|DESCENDING)\s+KEY\s+([\w-]+)",
            stmt,
            re.I,
        )
        if m:
            table_var = m.group(1).upper()
            order = m.group(2).upper()
            reverse = order == "DESCENDING"
            prefix = table_var + "("
            entries = {k: v for k, v in self._vars.items() if k.startswith(prefix)}
            if entries:
                sorted_items = sorted(
                    entries.items(), key=lambda kv: str(kv[1]), reverse=reverse
                )
                for k, v in sorted_items:
                    self._vars[k] = v
            return

        # SEARCH table VARYING idx WHEN cond stmt
        m = re.match(
            r"^SEARCH\s+([\w-]+)\s+(?:VARYING\s+([\w-]+)\s+)?WHEN\s+(.+?)\s+(PERFORM\s+[\w-]+|DISPLAY\s+.+)$",
            stmt,
            re.I,
        )
        if m:
            idx_var = (m.group(2) or m.group(1) + "-IDX").upper()
            cond = m.group(3).strip()
            action = m.group(4).strip()
            self._vars[idx_var] = 1
            for i in range(1, 1001):
                self._vars[idx_var] = i
                if self._eval_cond(cond):
                    self._exec_stmt(action)
                    break
            return

        # ON SIZE ERROR wrapper (strip and execute inner)
        m = re.match(r"^(.+?)\s+ON\s+SIZE\s+ERROR\s+(.+)$", stmt, re.I)
        if m:
            inner = m.group(1).strip()
            try:
                self._exec_stmt(inner)
            except Exception:
                self._exec_stmt(m.group(2).strip())
            return

        # WITH ROUNDED (strip and execute inner)
        m = re.match(r"^(.+?)\s+ROUNDED$", stmt, re.I)
        if m:
            self._exec_stmt(m.group(1).strip())
            return

        # EXEC SQL ... END-EXEC
        if re.match(r"^EXEC\s+SQL\b", stmt, re.I):
            self._exec_embedded_sql(stmt)
            return

    def _exec_embedded_sql(self, stmt: str) -> None:
        """Handle COBOL embedded SQL: EXEC SQL <sql> END-EXEC."""
        inner = re.sub(r"^EXEC\s+SQL\b", "", stmt, flags=re.I)
        # Strip END-EXEC (and anything following it) — it may not be at end-of-string
        # if the multi-line merger included trailing COBOL statements
        inner = re.split(r"\bEND-EXEC\b", inner, maxsplit=1, flags=re.I)[0].strip()
        if not inner:
            return

        # Cursor operations — silently simulate (check BEFORE substitution)
        sql_upper_strip = inner.strip().upper()
        if re.match(r"^DECLARE\b", sql_upper_strip):
            self._emit(f"ℹ️  SQL: {inner.strip()[:60]}")
            return
        if re.match(r"^OPEN\b", sql_upper_strip):
            self._emit("ℹ️  SQL cursor opened")
            return
        if re.match(r"^CLOSE\b", sql_upper_strip):
            self._emit("ℹ️  SQL cursor closed")
            return
        if re.match(r"^FETCH\b", sql_upper_strip):
            self._emit("ℹ️  SQL: fetch complete")
            return
        # Skip INCLUDE / BEGIN DECLARE SECTION / END DECLARE SECTION
        if re.match(
            r"^(INCLUDE|BEGIN\s+DECLARE\s+SECTION|END\s+DECLARE\s+SECTION)\b",
            sql_upper_strip,
        ):
            return

        # SELECT ... INTO :var1[, :var2, ...]  — identify and strip INTO clause
        # (must be done on inner BEFORE host var substitution changes :vars to literals)
        into_m = re.search(r"\bINTO\s+(:\w[\w-]*\s*,\s*)*:\w[\w-]*", inner, re.I)
        into_var = None
        if into_m:
            into_vars = re.findall(r":\w[\w-]*", into_m.group(0))
            into_var = into_vars[0].lstrip(":").upper() if into_vars else None
            # Remove the INTO clause from inner before substitution
            inner = re.sub(
                r"\bINTO\s+(?:\s*:\w[\w-]*\s*,)*\s*:\w[\w-]*", "", inner, flags=re.I
            ).strip()

        # Substitute :cobol-var host variables  → bare value
        def _subst_host(m_: re.Match) -> str:
            var = m_.group(1).upper()
            val = self._vars.get(var, "")
            if isinstance(val, str):
                return f"'{val}'"
            return str(val)

        sql_with_vals = re.sub(r":(\w[\w-]*)", _subst_host, inner)

        # Normalise DB2/mainframe SQL to SQLite equivalents
        # CURRENT DATE → date('now')
        sql_with_vals = re.sub(
            r"\bCURRENT\s+DATE\b", "date('now')", sql_with_vals, flags=re.I
        )
        # CURRENT TIMESTAMP → datetime('now')
        sql_with_vals = re.sub(
            r"\bCURRENT\s+TIMESTAMP\b", "datetime('now')", sql_with_vals, flags=re.I
        )
        # FETCH FIRST n ROW(S) ONLY → LIMIT n
        sql_with_vals = re.sub(
            r"\bFETCH\s+FIRST\s+(\d+)\s+ROW(?:S)?\s+ONLY\b",
            r"LIMIT \1",
            sql_with_vals,
            flags=re.I,
        )
        # WITH UR / WITH CS / WITH RS  — isolation hints, strip
        sql_with_vals = re.sub(
            r"\bWITH\s+(?:UR|CS|RS|RR)\b", "", sql_with_vals, flags=re.I
        )

        # Run through the SQL engine
        try:
            from ..core.sql_engine import SQLSession

            session = getattr(self.interpreter, "sql_session", None)
            if session is None:
                session = SQLSession()
                self.interpreter.sql_session = session
            result = session.run_statement(sql_with_vals)
            self._emit(result)
            # If INTO target given and result contains a value, store it
            if into_var and result.strip():
                lines = [
                    line
                    for line in result.strip().splitlines()
                    if line.strip() and not line.startswith("-")
                ]
                if len(lines) >= 2:  # header + row
                    self._vars[into_var] = (
                        lines[1].strip().split()[0] if lines[1].strip() else ""
                    )
        except Exception as e:
            err_str = str(e)
            # "no such table" is a demo-mode limitation; emit as info, not error
            if "no such table" in err_str.lower():
                tbl_m = re.search(r"no such table:\s*(\S+)", err_str, re.I)
                tbl = tbl_m.group(1) if tbl_m else "?"
                self._emit(
                    f"ℹ️  SQL: table '{tbl}' not found (demo mode — table not pre-created)"
                )
            else:
                self._emit(f"❌ EXEC SQL error: {e}")

    # COBOL verbs used to detect where condition ends and body begins
    _VERBS_RE = re.compile(
        r"\b(?:DISPLAY|MOVE|ADD|SUBTRACT|MULTIPLY|DIVIDE|COMPUTE|PERFORM|"
        r"SET|STOP|GOBACK|ACCEPT|CALL|WRITE|READ|STRING|UNSTRING|"
        r"INITIALIZE|INSPECT|EVALUATE|IF|CONTINUE|NEXT)\b",
        re.I,
    )

    def _exec_if_inline(self, stmt: str):
        """Parse: IF cond [THEN] stmts [ELSE stmts] [END-IF]"""
        # Remove END-IF
        stmt = re.sub(r"\s*END-IF\s*$", "", stmt).strip()
        # Remove IF prefix and optional THEN
        body = re.sub(r"^IF\s+", "", stmt).strip()
        body = re.sub(r"\bTHEN\b\s*", "", body, count=1).strip()
        # Split on ELSE
        else_parts = re.split(r"\bELSE\b", body, maxsplit=1)
        then_full = else_parts[0].strip()
        else_body = else_parts[1].strip() if len(else_parts) > 1 else ""
        # Find where condition ends (first COBOL verb)
        vm = self._VERBS_RE.search(then_full)
        if vm:
            cond = then_full[: vm.start()].strip()
            then_body = then_full[vm.start() :].strip()
        else:
            cond = then_full
            then_body = ""
        if self._eval_cond(cond):
            if then_body:
                self._exec_stmt(then_body)
        elif else_body:
            self._exec_stmt(else_body)

    def _exec_perform_block(self, stmt: str):
        """Handle inline PERFORM ... END-PERFORM blocks."""
        body_full = re.sub(r"\s*END-PERFORM$", "", stmt).strip()
        # PERFORM N TIMES body
        m = re.match(r"^PERFORM\s+(\d+)\s+TIMES\s+(.+)$", body_full, re.I)
        if m:
            n = int(m.group(1))
            body = m.group(2).strip()
            for _ in range(min(n, 10000)):
                self._exec_stmt(body)
            return
        # PERFORM VARYING var FROM x BY y UNTIL cond body
        vm = self._VERBS_RE.search(
            body_full,
            (
                re.match(
                    r"^PERFORM\s+VARYING\s+[\w-]+\s+FROM\s+.+?\s+BY\s+.+?\s+UNTIL\s+",
                    body_full,
                    re.I,
                ).end()
                if re.match(
                    r"^PERFORM\s+VARYING\s+[\w-]+\s+FROM\s+.+?\s+BY\s+.+?\s+UNTIL\s+",
                    body_full,
                    re.I,
                )
                else 0
            ),
        )
        hdr = re.match(
            r"^PERFORM\s+VARYING\s+([\w-]+)\s+FROM\s+(.+?)\s+BY\s+(.+?)\s+UNTIL\s+(.+)",
            body_full,
            re.I,
        )
        if hdr and vm:
            var = hdr.group(1)
            frm = _to_num(self._eval(hdr.group(2).strip()))
            by = _to_num(self._eval(hdr.group(3).strip()))
            # Split condition from body at first verb
            until_and_body = hdr.group(4)
            vm2 = self._VERBS_RE.search(until_and_body)
            if vm2:
                until_expr = until_and_body[: vm2.start()].strip()
                body = until_and_body[vm2.start() :].strip()
            else:
                until_expr = until_and_body.strip()
                body = ""
            self._vars[var] = frm
            for _ in range(10000):
                if self._eval_cond(until_expr):
                    break
                if body:
                    self._exec_stmt(body)
                self._vars[var] = _to_num(self._vars.get(var, 0)) + by
            return
        # PERFORM UNTIL cond body
        hdr2 = re.match(r"^PERFORM\s+UNTIL\s+(.+)", body_full, re.I)
        if hdr2:
            rest = hdr2.group(1)
            vm3 = self._VERBS_RE.search(rest)
            if vm3:
                until_expr = rest[: vm3.start()].strip()
                body = rest[vm3.start() :].strip()
                for _ in range(10000):
                    if self._eval_cond(until_expr):
                        break
                    self._exec_stmt(body)
            return

    # ------------------------------------------------------------------
    # Evaluation helpers
    # ------------------------------------------------------------------

    def _resolve_str(self, expr: str) -> str:
        """Resolve DISPLAY argument (string literal or variable)."""
        if expr.startswith('"') or expr.startswith("'"):
            return expr.strip("\"'")
        # Concatenation: "hello" " " NAME
        parts = re.findall(r'"[^"]*"|\'[^\']*\'|[\w-]+', expr)
        result = []
        for p in parts:
            if p.startswith('"') or p.startswith("'"):
                result.append(p.strip("\"'"))
            else:
                result.append(str(self._vars.get(p, p)))
        return "".join(result)

    def _eval(self, expr: str) -> Any:
        expr = expr.strip()
        if expr.startswith('"') or expr.startswith("'"):
            return expr.strip("\"'")
        if expr.upper() == "SPACES" or expr.upper() == "SPACE":
            return " "
        if expr.upper() in ("ZEROS", "ZEROES", "ZERO"):
            return 0
        try:
            if "." in expr:
                return float(expr)
            return int(expr)
        except ValueError:
            return self._vars.get(expr, expr)

    def _arith(self, expr: str) -> Any:
        """Evaluate simple arithmetic expression safely."""
        from ..utils.expression_evaluator import ExpressionEvaluator

        # Build variable dict with numeric values for the evaluator
        num_vars: dict[str, float] = {}
        for k, v in self._vars.items():
            try:
                num_vars[k] = float(v)
            except (TypeError, ValueError):
                pass
        # Also handle hyphenated COBOL names by replacing with underscores
        cleaned = re.sub(r"([A-Za-z][\w-]*)", lambda m: m.group(0).replace("-", "_"), expr)
        clean_vars = {k.replace("-", "_"): v for k, v in num_vars.items()}

        evaluator = ExpressionEvaluator(variables=clean_vars)
        try:
            return evaluator.evaluate(cleaned)
        except (ValueError, ZeroDivisionError):
            return 0

    def _eval_cond(self, cond: str) -> bool:
        """Evaluate a simple COBOL condition."""
        m = re.match(
            r"^(.+?)\s*(=|NOT EQUAL|EQUAL TO|NOT EQUAL TO|>|<|>=|<=)\s*(.+)$",
            cond,
            re.IGNORECASE,
        )
        if m:
            lhs = _to_num_or_str(self._eval(m.group(1).strip()))
            op = m.group(2).strip().upper()
            rhs = _to_num_or_str(self._eval(m.group(3).strip()))
            if op in ("=", "EQUAL TO"):
                return lhs == rhs
            if op in ("NOT EQUAL", "NOT EQUAL TO"):
                return lhs != rhs
            if op == ">":
                return lhs > rhs
            if op == "<":
                return lhs < rhs
            if op == ">=":
                return lhs >= rhs
            if op == "<=":
                return lhs <= rhs
        return False


# ---------------------------------------------------------------------------
# Exceptions and helpers
# ---------------------------------------------------------------------------


class CobolStop(Exception):
    pass


class CobolError(Exception):
    pass


def _to_num(val: Any) -> float:
    try:
        return float(val)
    except (TypeError, ValueError):
        return 0.0


def _to_num_or_str(val: Any):
    try:
        return float(val)
    except (TypeError, ValueError):
        return str(val)
