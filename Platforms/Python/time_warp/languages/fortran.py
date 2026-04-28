"""
FORTRAN executor for Time Warp Studio.

Supports a subset of FORTRAN 77 / Fortran 90 sufficient for educational use:
  - Fixed-form (column-7+) and free-form source
  - PROGRAM/END/STOP
  - WRITE(*,*) / PRINT *
  - INTEGER / REAL / CHARACTER*N declarations (fixed and free-form ::)
  - PARAMETER
  - Assignment
  - Arithmetic: +, -, *, /, ** (exponentiation)
  - IF / THEN / ELSE / ENDIF (and END IF)
  - DO loop with labeled CONTINUE and free-form END DO
  - GOTO with numeric labels
  - SUBROUTINE / CALL / RETURN
  - C-comment lines (fixed-form) and ! inline comments (free-form)
"""
from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

# ---------------------------------------------------------------------------
# Expression evaluator
# ---------------------------------------------------------------------------

_FORT_OPS = {".GT.": ">", ".LT.": "<", ".EQ.": "==",
             ".NE.": "!=", ".GE.": ">=", ".LE.": "<=",
             ".AND.": "and", ".OR.": "or", ".NOT.": "not"}


def _to_py_expr(expr: str) -> str:
    """Convert FORTRAN expression to Python-evaluable string."""
    result = expr
    for fort_op, py_op in _FORT_OPS.items():
        result = re.sub(re.escape(fort_op), py_op, result, flags=re.IGNORECASE)
    # ** stays as ** in Python
    return result


def _eval_expr(expr: str, env: Dict[str, Any]) -> Any:
    """Evaluate a FORTRAN expression in the current variable environment."""
    py_expr = _to_py_expr(expr.strip())
    # Strip surrounding quotes -> string literal
    if (py_expr.startswith("'") and py_expr.endswith("'")) or \
       (py_expr.startswith('"') and py_expr.endswith('"')):
        return py_expr[1:-1]
    safe_env: Dict[str, Any] = {"__builtins__": {}, "abs": abs, "int": int, "float": float}
    # Populate variables (both original case and upper for FORTRAN case-insensitivity)
    safe_env.update({k.upper(): v for k, v in env.items()})
    safe_env.update(env)
    try:
        return eval(py_expr, safe_env)  # noqa: S307
    except Exception:
        return py_expr  # return as-is if evaluation fails


# ---------------------------------------------------------------------------
# Line pre-processor
# ---------------------------------------------------------------------------

def _upper_outside_strings(s: str) -> str:
    """Uppercase characters outside quoted string literals, preserving their content."""
    result = []
    in_q, q_ch = False, ""
    for ch in s:
        if ch in ("'", '"') and not in_q:
            in_q, q_ch = True, ch
            result.append(ch)
        elif ch == q_ch and in_q:
            in_q = False
            result.append(ch)
        elif in_q:
            result.append(ch)
        else:
            result.append(ch.upper())
    return "".join(result)


def _strip_comment(line: str, is_free: bool) -> str:
    """Remove trailing ! comment (free-form) or full-line C comment."""
    if is_free:
        # Remove ! and everything after (outside strings)
        in_q, q_ch = False, ""
        for i, ch in enumerate(line):
            if ch in ("'", '"') and not in_q:
                in_q, q_ch = True, ch
            elif ch == q_ch and in_q:
                in_q = False
            elif ch == "!" and not in_q:
                return line[:i].rstrip()
        return line
    # Fixed-form: columns 1-5 can be numeric labels, col 6 is continuation
    return line


def _is_c_comment(line: str) -> bool:
    """True for fixed-form comment lines (C/c/* in column 1)."""
    return bool(line) and line[0].upper() in ("C", "*")


def _get_label_and_stmt(line: str, is_free: bool) -> Tuple[Optional[str], str]:
    """Return (numeric_label_or_None, statement_text)."""
    if not is_free:
        # Fixed-form: cols 1-5 for labels, col 7+ for statement
        label_part = line[:5].strip() if len(line) >= 5 else ""
        stmt = line[6:].strip() if len(line) > 6 else line.strip()
        if re.match(r"^\d+$", label_part):
            return label_part, stmt
        return None, line.strip()
    # Free-form: optional numeric label at start
    m = re.match(r"^(\d+)\s+(.*)", line.strip())
    if m:
        return m.group(1), m.group(2)
    return None, line.strip()


# ---------------------------------------------------------------------------
# Interpreter state
# ---------------------------------------------------------------------------

class _FortranInterp:
    MAX_STEPS = 100_000

    def __init__(self, input_cb):
        self.env: Dict[str, Any] = {}
        self.output: List[str] = []
        self.input_cb = input_cb

    # ------------------------------------------------------------------
    def run(self, source: str) -> None:
        # Detect free-form vs fixed-form
        is_free = self._detect_free_form(source)
        lines = source.splitlines()

        # Build flat list of (label, stmt) skipping comments/empty
        stmts: List[Tuple[Optional[str], str]] = []
        for raw in lines:
            stripped = raw.rstrip()
            if not stripped:
                continue
            if not is_free and _is_c_comment(stripped):
                continue
            cleaned = _strip_comment(stripped, is_free)
            label, stmt = _get_label_and_stmt(cleaned, is_free)
            if stmt:
                stmts.append((label, _upper_outside_strings(stmt) if self._is_keyword_line(stmt) else stmt))

        # Collect subroutines
        subroutines: Dict[str, int] = {}
        for i, (lbl, stmt) in enumerate(stmts):
            if stmt.strip().upper().startswith("SUBROUTINE "):
                name = stmt.strip()[11:].strip().split("(")[0].strip().upper()
                subroutines[name] = i

        # Build label -> index map
        label_map: Dict[str, int] = {}
        for i, (lbl, _) in enumerate(stmts):
            if lbl:
                label_map[lbl] = i

        self._exec_block(stmts, 0, label_map, subroutines, ret_stack=[])

    def _detect_free_form(self, source: str) -> bool:
        """Heuristic: free-form if any line uses :: or END DO or END PROGRAM."""
        return bool(re.search(r"::|END\s+DO|END\s+PROGRAM", source, re.IGNORECASE))

    def _is_keyword_line(self, stmt: str) -> bool:
        """Lines that should be uppercased for keyword matching."""
        kw = stmt.strip().upper()
        return any(kw.startswith(p) for p in (
            "PROGRAM", "END", "STOP", "WRITE", "PRINT", "INTEGER", "REAL",
            "CHARACTER", "PARAMETER", "IF", "ELSE", "ENDIF", "END IF",
            "DO", "CONTINUE", "GOTO", "CALL", "RETURN", "SUBROUTINE",
        ))

    def _exec_block(
        self,
        stmts: List[Tuple[Optional[str], str]],
        start: int,
        label_map: Dict[str, int],
        subroutines: Dict[str, int],
        ret_stack: List[int],
        end_markers: Optional[tuple] = None,
    ) -> int:
        """Execute statements from `start`. Returns index after the block ends."""
        ip = start
        steps = 0
        while ip < len(stmts) and steps < self.MAX_STEPS:
            steps += 1
            lbl, stmt = stmts[ip]
            stmt_upper = stmt.strip().upper()

            if end_markers and any(stmt_upper.startswith(m) for m in end_markers):
                return ip

            if stmt_upper in ("STOP", "END", "END PROGRAM") or stmt_upper.startswith("END PROGRAM"):
                return ip

            # PROGRAM heading
            if stmt_upper.startswith("PROGRAM "):
                ip += 1
                continue

            # Declarations
            if self._try_declaration(stmt_upper, stmt, ip, stmts):
                ip += 1
                continue

            # PARAMETER
            if stmt_upper.startswith("PARAMETER"):
                self._exec_parameter(stmt)
                ip += 1
                continue

            # WRITE / PRINT
            if stmt_upper.startswith("WRITE") or stmt_upper.startswith("PRINT"):
                self._exec_write(stmt)
                ip += 1
                continue

            # IF / THEN
            if stmt_upper.startswith("IF ") or stmt_upper.startswith("IF("):
                ip = self._exec_if(stmt, stmts, ip, label_map, subroutines, ret_stack)
                continue

            # ELSE / ENDIF
            if stmt_upper.startswith("ELSE") or stmt_upper.startswith("END IF") or stmt_upper == "ENDIF":
                return ip  # Let parent IF handle it

            # DO loop
            if stmt_upper.startswith("DO "):
                ip = self._exec_do(stmt, stmts, ip, label_map, subroutines, ret_stack)
                continue

            # CONTINUE (label target consumed by DO)
            if stmt_upper == "CONTINUE":
                ip += 1
                continue

            # END DO
            if stmt_upper in ("END DO", "ENDDO"):
                return ip

            # GOTO
            if stmt_upper.startswith("GOTO") or stmt_upper.startswith("GO TO"):
                target = re.sub(r"GO\s*TO\s*", "", stmt_upper).strip()
                if target in label_map:
                    ip = label_map[target]
                    continue
                ip += 1
                continue

            # CALL subroutine
            if stmt_upper.startswith("CALL "):
                name = stmt_upper[5:].strip().split("(")[0].strip()
                if name in subroutines:
                    sub_ip = subroutines[name] + 1  # skip SUBROUTINE line
                    self._exec_block(stmts, sub_ip, label_map, subroutines,
                                     ret_stack, end_markers=("END", "RETURN"))
                ip += 1
                continue

            # RETURN
            if stmt_upper == "RETURN":
                return ip

            # SUBROUTINE definition (skip past it if not called)
            if stmt_upper.startswith("SUBROUTINE "):
                # Skip to END
                ip += 1
                while ip < len(stmts):
                    s = stmts[ip][1].strip().upper()
                    ip += 1
                    if s in ("END", "RETURN") or s.startswith("END "):
                        break
                continue

            # Assignment
            if "=" in stmt and not stmt_upper.startswith("IF"):
                self._exec_assign(stmt)
                ip += 1
                continue

            ip += 1

        return ip

    # ------------------------------------------------------------------
    def _try_declaration(self, stmt_upper: str, stmt: str, ip: int,
                          stmts: List[Tuple[Optional[str], str]]) -> bool:
        """Handle variable declarations. Returns True if handled."""
        for kw in ("INTEGER", "REAL", "LOGICAL", "DOUBLE PRECISION"):
            if stmt_upper.startswith(kw):
                # Strip keyword and optional ::
                rest = re.sub(r"^" + kw + r"\s*(::\s*)?", "", stmt, flags=re.IGNORECASE).strip()
                for decl in rest.split(","):
                    decl = decl.strip()
                    if "=" in decl:
                        # Inline initialisation: REAL :: X = 3.14
                        parts = decl.split("=", 1)
                        var = parts[0].strip().upper()
                        val = _eval_expr(parts[1].strip(), self.env)
                        self.env[var] = val
                    else:
                        var = re.split(r"[\s(]", decl)[0].upper()
                        if var and not var.startswith("("):
                            self.env.setdefault(var, 0)
                return True
        if stmt_upper.startswith("CHARACTER"):
            rest = re.sub(r"^CHARACTER\s*(\*\s*\d+\s*)?(::\s*)?", "", stmt,
                          flags=re.IGNORECASE).strip()
            for decl in rest.split(","):
                decl = decl.strip()
                if "=" in decl:
                    parts = decl.split("=", 1)
                    var = parts[0].strip().upper()
                    val = _eval_expr(parts[1].strip(), self.env)
                    self.env[var] = str(val).strip("'\"")
                else:
                    var = re.split(r"[\s(]", decl)[0].upper()
                    if var:
                        self.env.setdefault(var, "")
            return True
        return False

    def _exec_parameter(self, stmt: str) -> None:
        """PARAMETER (N = value, ...)"""
        inner = re.search(r"\((.+)\)", stmt)
        if inner:
            for item in inner.group(1).split(","):
                if "=" in item:
                    k, v = item.split("=", 1)
                    self.env[k.strip().upper()] = _eval_expr(v.strip(), self.env)

    def _exec_write(self, stmt: str) -> None:
        """WRITE(*,*) expr or PRINT *, expr"""
        # Normalise to extract the output list
        stmt_up = stmt.strip().upper()
        if stmt_up.startswith("PRINT"):
            # PRINT *, items
            rest = re.sub(r"^PRINT\s*\*\s*,?\s*", "", stmt, flags=re.IGNORECASE).strip()
        else:
            # WRITE(*,*) items
            m = re.match(r"WRITE\s*\(\s*\*\s*,\s*\*\s*\)\s*(.*)", stmt, re.IGNORECASE)
            rest = m.group(1).strip() if m else ""

        if not rest:
            self.output.append("")
            return

        parts = self._split_write_items(rest)
        out_parts = []
        for part in parts:
            part = part.strip()
            if (part.startswith("'") and part.endswith("'")) or \
               (part.startswith('"') and part.endswith('"')):
                out_parts.append(part[1:-1])
            else:
                val = _eval_expr(part, self.env)
                out_parts.append(str(val))
        self.output.append(" ".join(out_parts))

    def _split_write_items(self, s: str) -> List[str]:
        """Split comma-separated items respecting quoted strings."""
        items, cur, in_q, q_ch = [], "", False, ""
        for ch in s:
            if ch in ("'", '"') and not in_q:
                in_q, q_ch, cur = True, ch, cur + ch
            elif ch == q_ch and in_q:
                in_q, cur = False, cur + ch
            elif ch == "," and not in_q:
                items.append(cur)
                cur = ""
            else:
                cur += ch
        if cur:
            items.append(cur)
        return items

    def _exec_assign(self, stmt: str) -> None:
        parts = stmt.split("=", 1)
        var = parts[0].strip().upper()
        val = _eval_expr(parts[1].strip(), self.env)
        self.env[var] = val

    def _exec_if(self, stmt: str, stmts, ip: int, label_map, subroutines, ret_stack) -> int:
        """IF (cond) THEN ... [ELSE ...] END IF"""
        # Match condition
        m = re.match(r"IF\s*\((.+)\)\s*THEN\s*$", stmt.strip(), re.IGNORECASE)
        if not m:
            # Arithmetic IF: IF (cond) stmt
            m2 = re.match(r"IF\s*\((.+)\)\s*(.+)", stmt.strip(), re.IGNORECASE)
            if m2:
                cond_val = _eval_expr(m2.group(1), self.env)
                if cond_val:
                    inner_stmt = m2.group(2).strip()
                    self._exec_block([(None, inner_stmt)], 0, {}, {}, [])
            return ip + 1

        cond_val = _eval_expr(m.group(1), self.env)
        ip += 1  # move past IF line

        # Find matching ELSE / ENDIF
        then_block: List[Tuple[Optional[str], str]] = []
        else_block: List[Tuple[Optional[str], str]] = []
        depth, in_else = 0, False
        while ip < len(stmts):
            lbl, s = stmts[ip]
            su = s.strip().upper()
            if su.startswith("IF") and "THEN" in su.upper():
                depth += 1
            if depth == 0:
                if su in ("ELSE",):
                    in_else = True
                    ip += 1
                    continue
                if su in ("ENDIF", "END IF"):
                    ip += 1
                    break
            if depth > 0 and su in ("ENDIF", "END IF"):
                depth -= 1
            if in_else:
                else_block.append((lbl, s))
            else:
                then_block.append((lbl, s))
            ip += 1

        block = then_block if cond_val else else_block
        if block:
            sub_label = {l: i for i, (l, _) in enumerate(block) if l}
            self._exec_block(block, 0, sub_label, subroutines, ret_stack)
        return ip

    def _exec_do(self, stmt: str, stmts, ip: int, label_map, subroutines, ret_stack) -> int:
        """DO [label] var = start, end [, step]"""
        stmt_upper = stmt.strip().upper()
        # Fixed-form: DO 10 I = 1, 5
        m_fixed = re.match(r"DO\s+(\d+)\s+(\w+)\s*=\s*(.+),\s*(.+?)(?:,\s*(.+))?$",
                           stmt_upper, re.IGNORECASE)
        # Free-form: DO I = 1, 5
        m_free = re.match(r"DO\s+(\w+)\s*=\s*(.+),\s*(.+?)(?:,\s*(.+))?$",
                          stmt_upper, re.IGNORECASE)

        if m_fixed:
            term_label = m_fixed.group(1)
            var = m_fixed.group(2)
            start = int(_eval_expr(m_fixed.group(3), self.env))
            end_val = int(_eval_expr(m_fixed.group(4), self.env))
            step = int(_eval_expr(m_fixed.group(5), self.env)) if m_fixed.group(5) else 1
            use_free = False
        elif m_free and not re.match(r"DO\s+\d+\s+", stmt_upper):
            term_label = None
            var = m_free.group(1)
            start = int(_eval_expr(m_free.group(2), self.env))
            end_val = int(_eval_expr(m_free.group(3), self.env))
            step = int(_eval_expr(m_free.group(4), self.env)) if m_free.group(4) else 1
            use_free = True
        else:
            return ip + 1

        # Collect loop body
        body: List[Tuple[Optional[str], str]] = []
        ip += 1
        while ip < len(stmts):
            lbl, s = stmts[ip]
            su = s.strip().upper()
            if use_free and su in ("END DO", "ENDDO"):
                ip += 1
                break
            if not use_free and term_label and lbl == term_label:
                body.append((lbl, s))  # include the CONTINUE line
                ip += 1
                break
            body.append((lbl, s))
            ip += 1

        # Execute loop
        i = start
        while (step > 0 and i <= end_val) or (step < 0 and i >= end_val):
            self.env[var] = i
            sub_label = {l: idx for idx, (l, _) in enumerate(body) if l}
            self._exec_block(body, 0, sub_label, subroutines, ret_stack)
            i += step

        return ip


# ---------------------------------------------------------------------------
# Public executor
# ---------------------------------------------------------------------------

def execute_fortran(interpreter: "Interpreter", source: str, turtle: "TurtleState") -> str:
    """Execute a FORTRAN program and return output text."""
    if not source.strip():
        return ""
    interp = _FortranInterp(input_cb=getattr(interpreter, "input_callback", None))
    try:
        interp.run(source)
    except Exception as exc:  # noqa: BLE001
        interp.output.append(f"❌ Runtime error: {exc}")
    return "\n".join(interp.output) + ("\n" if interp.output else "")
