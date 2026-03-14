"""FORTRAN 77 language executor for Time Warp Studio.

Educational FORTRAN 77 interpreter — whole-program execution.
Fixed-format source: columns 1-5 label, 6 continuation (*), 7-72 statement.
Supports:
  PROGRAM name / END
  INTEGER, REAL, CHARACTER*n, LOGICAL declarations
  WRITE(*,*) / PRINT *, list
  READ(*,*) var-list
  IF (cond) THEN / ELSE / ENDIF
  IF (cond) statement  (arithmetic/logical IF)
  DO label var = start, end [, step]  / label CONTINUE
  GOTO label
  STOP
  SUBROUTINE / CALL / RETURN
  Assignment statements
  Arithmetic: **, *, /, +, -
"""

from __future__ import annotations

import math
import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

# Intrinsic function names — exempt from IMPLICIT NONE undeclared checks
_FORTRAN_INTRINSICS = {
    "ABS", "IABS", "DABS", "CABS",
    "SQRT", "DSQRT", "CSQRT",
    "INT", "IFIX", "IDINT", "AINT", "ANINT", "NINT", "DINT",
    "REAL", "FLOAT", "SNGL", "DBLE", "DFLOAT", "CMPLX", "DCMPLX",
    "SIN", "DSIN", "COS", "DCOS", "TAN", "DTAN",
    "ASIN", "DASIN", "ACOS", "DACOS", "ATAN", "DATAN", "ATAN2", "DATAN2",
    "SIND", "COSD", "TAND",
    "SINH", "DSINH", "COSH", "DCOSH", "TANH", "DTANH",
    "EXP", "DEXP", "CEXP", "LOG", "ALOG", "DLOG", "LOG10", "ALOG10", "DLOG10", "LOG2",
    "MAX", "MAX0", "MAX1", "AMAX0", "AMAX1", "DMAX1",
    "MIN", "MIN0", "MIN1", "AMIN0", "AMIN1", "DMIN1",
    "MOD", "AMOD", "DMOD", "MODULO",
    "SIGN", "DSIGN", "ISIGN",
    "DIM", "DDIM", "IDIM",
    "DPROD",
    "CEILING", "FLOOR",
    "NOT", "IAND", "IOR", "IEOR", "ISHFT", "IBITS", "BTEST", "IBSET", "IBCLR",
    "LEN", "LEN_TRIM", "TRIM", "ADJUSTL", "ADJUSTR", "INDEX", "SCAN", "VERIFY",
    "CHAR", "ICHAR", "IACHAR", "ACHAR",
    "LGE", "LGT", "LLE", "LLT",
    "BIT_SIZE", "HUGE", "TINY", "EPSILON", "KIND",
    "SELECTED_INT_KIND", "SELECTED_REAL_KIND",
    "ALLOCATED", "PRESENT", "UBOUND", "LBOUND", "SIZE", "SHAPE",
    "CONJG", "AIMAG", "DIMAG",
    "WRITE", "READ", "PRINT",
    # Fortran comparison operators (appear as identifier-like tokens)
    "EQ", "NE", "LT", "LE", "GT", "GE", "AND", "OR", "NOT",
    "TRUE", "FALSE",
}


def execute_fortran(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a FORTRAN 77 program and return all output."""
    env = FortranEnvironment(interpreter, turtle)
    return env.run(source)


class FortranEnvironment:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []
        self._vars: dict[str, Any] = {}
        self._labels: dict[str, int] = {}  # label -> line index
        self._lines: list[tuple[str, str]] = []  # [(label, stmt)]
        self._subroutines: dict[str, int] = {}
        self._functions: dict[str, int] = {}
        self._call_stack: list[int] = []
        self._do_stack: list[tuple[str, float, float, float, str]] = []
        # (var, end, step, loop_label, back_label)
        self._common_blocks: dict[str, list[str]] = {}
        self._named_constants: dict[str, Any] = {}
        self._format_stmts: dict[str, str] = {}
        self._file_units: dict[int, dict] = {}
        self._implicit_none = False
        self._declared_vars: set[str] = set()  # explicit declarations for IMPLICIT NONE
        self._func_return_types: dict[str, str] = {}  # function name -> return type

    def _emit(self, text: str):
        self._output.append(str(text))

    def run(self, source: str) -> str:
        try:
            self._parse(source)
            self._exec_from(0)
        except FortranStop:
            pass
        except FortranError as e:
            self._emit(f"❌ FORTRAN error: {e}")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}")
        return "\n".join(self._output)

    # ------------------------------------------------------------------
    # Parser
    # ------------------------------------------------------------------

    @staticmethod
    def _upper_preserve_strings(line: str) -> str:
        """Uppercase Fortran keywords but preserve quoted string literals."""
        from .lang_utils import upper_preserve_strings
        return upper_preserve_strings(line)

    def _is_free_form(self, source: str) -> bool:
        """Detect whether source is Fortran 90 free-form (vs fixed-format).

        Heuristics: presence of ``::`` declarations, ``END DO``, ``PROGRAM``
        at column 1, or lines where columns 1-6 contain non-label alphabetic
        content all suggest free-form.
        """
        for raw in source.splitlines():
            stripped = raw.strip()
            if not stripped or stripped.startswith("!"):
                continue
            up = stripped.upper()
            # "PROGRAM X" or "MODULE X" starting at col 1 = free-form
            if up.startswith("PROGRAM ") or up.startswith("MODULE "):
                return True
            # :: in a declaration is Fortran 90
            if "::" in up and re.match(
                r"^(INTEGER|REAL|CHARACTER|LOGICAL|DOUBLE\s+PRECISION)\b", up
            ):
                return True
            # END DO / END PROGRAM / END IF are Fortran 90 structured constructs
            if re.match(r"^END\s+(DO|PROGRAM|FUNCTION|SUBROUTINE|MODULE)\b", up):
                return True
        return False

    def _parse(self, source: str):
        if self._is_free_form(source):
            return self._parse_free_form(source)
        return self._parse_fixed_format(source)

    def _parse_free_form(self, source: str):
        """Parse Fortran 90 free-form source."""
        for raw in source.splitlines():
            # FORTRAN 77 full-line comments: C or * in column 1
            if raw and raw[0] in ("C", "c", "*"):
                continue
            # Strip inline comments (outside strings)
            stripped = raw.strip()
            if not stripped or stripped.startswith("!"):
                continue
            # Remove trailing comment
            in_str = False
            qchar = ""
            clean = []
            for ch in stripped:
                if in_str:
                    clean.append(ch)
                    if ch == qchar:
                        in_str = False
                elif ch in ("'", '"'):
                    in_str = True
                    qchar = ch
                    clean.append(ch)
                elif ch == "!":
                    break
                else:
                    clean.append(ch)
            stmt = self._upper_preserve_strings("".join(clean).strip())
            if not stmt:
                continue

            # Extract optional numeric label
            label = ""
            lm = re.match(r"^(\d+)\s+", stmt)
            if lm:
                label = lm.group(1)
                stmt = stmt[lm.end() :].strip()

            if stmt.startswith("SUBROUTINE "):
                sub_m = re.match(r"SUBROUTINE\s+(\w+)", stmt)
                if sub_m:
                    self._subroutines[sub_m.group(1)] = len(self._lines)

            func_m = re.match(
                r"^(?:(?:INTEGER|REAL|DOUBLE\s+PRECISION|CHARACTER|LOGICAL)\s+)?FUNCTION\s+(\w+)\s*\(",
                stmt,
            )
            if func_m:
                self._functions[func_m.group(1)] = len(self._lines)
                # Record the return type for typed functions
                type_m = re.match(
                    r"^(INTEGER|REAL|DOUBLE\s+PRECISION|CHARACTER|LOGICAL)\s+FUNCTION",
                    stmt,
                )
                if type_m:
                    self._func_return_types[func_m.group(1).upper()] = type_m.group(
                        1
                    ).upper()

            fmt_m = re.match(r"^FORMAT\s*\((.+)\)\s*$", stmt)
            if fmt_m and label:
                self._format_stmts[label] = fmt_m.group(1)

            self._lines.append((label, stmt))
            if label:
                self._labels[label] = len(self._lines) - 1

    def _parse_fixed_format(self, source: str):
        raw_lines = source.splitlines()
        continuation_buf: list[str] = []
        idx = 0

        for raw in raw_lines:
            # Fixed format: pad to at least 7 chars
            padded = raw.ljust(7)
            col1 = padded[0]
            col6 = padded[5]
            stmt_text = padded[6:72].rstrip() if len(padded) >= 7 else ""

            # Comment
            if col1 in ("C", "c", "!", "*") or (col1 == " " and raw.startswith("!")):
                continue

            # Continuation
            if col6 not in (" ", "0", "") and continuation_buf:
                continuation_buf[-1] = continuation_buf[-1][1] + " " + stmt_text
                continue

            label = raw[:5].strip()
            stmt = self._upper_preserve_strings(stmt_text)

            if continuation_buf:
                # Flush previous
                prev_label, prev_stmt = (
                    continuation_buf[-1]
                    if isinstance(continuation_buf[-1], tuple)
                    else ("", continuation_buf[-1])
                )
                self._lines.append((prev_label, prev_stmt))
                if prev_label:
                    self._labels[prev_label] = len(self._lines) - 1
                continuation_buf.clear()

            if stmt.startswith("SUBROUTINE "):
                sub_m = re.match(r"SUBROUTINE\s+(\w+)", stmt)
                if sub_m:
                    self._subroutines[sub_m.group(1)] = len(self._lines)

            # Register FUNCTION definitions
            func_m = re.match(
                r"^(?:(?:INTEGER|REAL|DOUBLE\s+PRECISION|CHARACTER|LOGICAL)\s+)?FUNCTION\s+(\w+)\s*\(",
                stmt,
            )
            if func_m:
                self._functions[func_m.group(1)] = len(self._lines)

            # Register FORMAT statements
            fmt_m = re.match(r"^FORMAT\s*\((.+)\)\s*$", stmt)
            if fmt_m and label:
                self._format_stmts[label] = fmt_m.group(1)

            self._lines.append((label, stmt))
            if label:
                self._labels[label] = len(self._lines) - 1
            idx += 1

    # ------------------------------------------------------------------
    # Executor
    # ------------------------------------------------------------------

    def _exec_from(self, start: int):
        ip = start
        max_steps = 500_000
        steps = 0
        while ip < len(self._lines):
            steps += 1
            if steps > max_steps:
                self._emit("❌ FORTRAN: execution limit exceeded")
                break
            label, stmt = self._lines[ip]
            # Check if this label is a DO loop terminal
            if label and self._do_stack:
                var, end_val, step_val, loop_lbl, back_ip = self._do_stack[-1]
                if loop_lbl == label:
                    # Execute the terminal statement (usually CONTINUE), then check loop
                    self._exec_stmt(stmt, ip)
                    new_val = _to_num(self._vars.get(var, 0)) + step_val
                    self._vars[var] = new_val
                    if (step_val >= 0 and new_val <= end_val) or (
                        step_val < 0 and new_val >= end_val
                    ):
                        ip = back_ip  # loop back
                    else:
                        self._do_stack.pop()
                        ip += 1
                    continue
            next_ip = self._exec_stmt(stmt, ip)
            if next_ip is None:
                ip += 1
            else:
                ip = next_ip

    def _exec_stmt(self, stmt: str, ip: int) -> int | None:
        stmt = stmt.strip()
        if not stmt:
            return None

        # PROGRAM / END / STOP
        if re.match(r"^PROGRAM\b", stmt):
            return None
        if re.match(r"^END\s*(?:PROGRAM|FUNCTION|SUBROUTINE)\b", stmt) or stmt == "END":
            raise FortranStop()
        if stmt == "STOP" or re.match(r"^STOP\b", stmt):
            raise FortranStop()
        if stmt == "RETURN":
            if self._call_stack:
                return self._call_stack.pop()
            raise FortranStop()
        if stmt == "CONTINUE":
            return None
        if stmt in ("CYCLE", "ITERATE"):
            # Jump back to DO loop start
            if self._do_stack:
                var, end_val, step_val, loop_lbl, back_ip = self._do_stack[-1]
                self._vars[var] = _to_num(self._vars.get(var, 0)) + step_val
                return back_ip
            return None
        if stmt == "EXIT":
            if self._do_stack:
                var, end_val, step_val, loop_lbl, back_ip = self._do_stack.pop()
                if loop_lbl and loop_lbl in self._labels:
                    return self._labels[loop_lbl] + 1
            return None

        # IMPLICIT NONE / IMPLICIT type(range)
        if re.match(r"^IMPLICIT\b", stmt):
            if "NONE" in stmt:
                self._implicit_none = True
            return None

        # PARAMETER (name = val, ...)
        m = re.match(r"^PARAMETER\s*\((.+)\)$", stmt)
        if m:
            for item in m.group(1).split(","):
                item = item.strip()
                pm = re.match(r"^(\w+)\s*=\s*(.+)$", item)
                if pm:
                    val = self._eval_expr(pm.group(2).strip())
                    self._named_constants[pm.group(1).upper()] = val
                    self._vars[pm.group(1).upper()] = val
            return None

        # DATA var / val, .../ [var2 / val2 /...]
        m = re.match(r"^DATA\s+(.+)$", stmt)
        if m:
            raw = m.group(1).strip()
            pairs = re.findall(r"([\w,\s()\*]+?)\s*/\s*(.+?)\s*/", raw)
            for var_part, val_part in pairs:
                vars_list = [v.strip() for v in var_part.split(",") if v.strip()]
                vals_list = [v.strip() for v in val_part.split(",") if v.strip()]
                # Handle repeat notation: n*val
                expanded_vals: list[Any] = []
                for v in vals_list:
                    rm = re.match(r"^(\d+)\*(.+)$", v)
                    if rm:
                        expanded_vals.extend(
                            [self._eval_expr(rm.group(2))] * int(rm.group(1))
                        )
                    else:
                        expanded_vals.append(self._eval_expr(v))
                for i, var in enumerate(vars_list):
                    if i < len(expanded_vals):
                        self._vars[var.upper()] = expanded_vals[i]
            return None

        # COMMON /name/ var1, var2, ...
        m = re.match(r"^COMMON\s*/(\w*)/\s*(.+)$", stmt)
        if m:
            blk = m.group(1) or "BLANK"
            names = [n.strip().upper() for n in m.group(2).split(",") if n.strip()]
            self._common_blocks[blk] = names
            for name in names:
                if name not in self._vars:
                    self._vars[name] = 0
            return None

        # EQUIVALENCE
        m = re.match(r"^EQUIVALENCE\s+\((.+)\)$", stmt)
        if m:
            names = [n.strip().upper() for n in m.group(1).split(",") if n.strip()]
            if names:
                # Alias them to same value
                base = self._vars.get(names[0], 0)
                for name in names:
                    self._vars[name] = base
            return None

        # FORMAT statement — already parsed, skip
        if re.match(r"^FORMAT\s*\(", stmt):
            return None

        # EXTERNAL name1, name2, ... — declare external function names
        m = re.match(r"^EXTERNAL\s+(.+)$", stmt)
        if m:
            for name in m.group(1).split(","):
                name = name.strip().upper()
                if name:
                    self._declared_vars.add(name)
                    self._functions[name] = -1  # mark as known function
            return None

        # DIMENSION array declarations
        m = re.match(r"^DIMENSION\s+(.+)$", stmt)
        if m:
            for item in m.group(1).split(","):
                am = re.match(r"^(\w+)\s*\(", item.strip())
                if am:
                    name = am.group(1).upper()
                    self._declared_vars.add(name)
                    if name not in self._vars:
                        self._vars[name] = []
            return None

        # SAVE — preserve local variables across calls (no-op for interpreter)
        if re.match(r"^SAVE\b", stmt):
            return None

        # INTRINSIC — declare intrinsic function usage (no-op)
        if re.match(r"^INTRINSIC\b", stmt):
            return None

        # FUNCTION header — skip body
        if re.match(
            r"^(?:(?:INTEGER|REAL|DOUBLE\s+PRECISION|CHARACTER|LOGICAL)\s+)?FUNCTION\b",
            stmt,
        ):
            return None

        # SUBROUTINE header — skip body until RETURN/END
        if re.match(r"^SUBROUTINE\b", stmt):
            return None

        # Variable type declarations
        if re.match(
            r"^(INTEGER|REAL|CHARACTER|LOGICAL|DOUBLE\s+PRECISION|COMPLEX|BYTE)\b", stmt
        ):
            self._parse_declaration(stmt)
            return None

        # OPEN (UNIT=n, FILE=f, STATUS=s, ...)
        m = re.match(r"^OPEN\s*\((.+)\)$", stmt)
        if m:
            args = m.group(1)
            unit_m = re.search(r"\bUNIT\s*=\s*(\d+)", args, re.I)
            unit = int(unit_m.group(1)) if unit_m else 10
            status_m = re.search(r"\bSTATUS\s*=\s*['\"]?(\w+)['\"]?", args, re.I)
            mode = status_m.group(1).upper() if status_m else "UNKNOWN"
            self._file_units[unit] = {"mode": mode, "records": [], "pos": 0}
            return None

        # CLOSE (UNIT=n)
        m = re.match(r"^CLOSE\s*\((.+)\)$", stmt)
        if m:
            unit_m = re.search(r"\bUNIT\s*=\s*(\d+)|\b(\d+)\b", m.group(1))
            if unit_m:
                unit = int(unit_m.group(1) or unit_m.group(2))
                self._file_units.pop(unit, None)
            return None

        # WRITE (unit, fmt) items  — handle file units and formatted output
        m = re.match(r"^WRITE\s*\(\s*(\w+|\*)\s*,\s*([^)]+)\)\s*(.*)", stmt)
        if m:
            unit_str = m.group(1).strip()
            fmt_str = m.group(2).strip()
            items_str = m.group(3).strip()
            if unit_str == "*":
                # stdout — use standard formatting
                self._do_write_fmt(items_str, fmt_str)
            else:
                try:
                    unit = int(unit_str)
                    line = self._format_items(items_str, fmt_str)
                    if unit in self._file_units:
                        self._file_units[unit]["records"].append(line)
                    else:
                        self._emit(line)
                except ValueError:
                    self._do_write_fmt(items_str, fmt_str)
            return None

        # PRINT *, items
        m = re.match(r"^PRINT\s*\*\s*,\s*(.*)", stmt)
        if m:
            self._do_write(m.group(1).strip())
            return None

        # PRINT fmt, items
        m = re.match(r"^PRINT\s+(['\d]\S*)\s*,\s*(.*)", stmt)
        if m:
            self._do_write_fmt(m.group(2).strip(), m.group(1).strip())
            return None

        # WRITE(*,*) list  or  WRITE(*,'(...)') list
        m = re.match(
            r"^(?:WRITE\s*\(\s*\*\s*,\s*[*'\"(][^)]*\)\s*|PRINT\s*\*\s*,\s*)(.*)", stmt
        )
        if m:
            self._do_write(m.group(1).strip())
            return None

        # READ (unit, fmt) vars
        m = re.match(r"^READ\s*\(\s*(\w+|\*)\s*,\s*[^)]*\)\s*(.*)", stmt)
        if m:
            self._do_read(m.group(2).strip())
            return None

        # READ(*,*) var-list
        m = re.match(r"^READ\s*\(\s*\*\s*,\s*\*\s*\)\s*(.*)", stmt)
        if m:
            self._do_read(m.group(1).strip())
            return None

        # DO WHILE (cond) — Fortran 90
        m = re.match(r"^DO\s+WHILE\s*\((.+)\)$", stmt)
        if m:
            cond = m.group(1).strip()
            # Push a "WHILE" sentinel on do_stack with end_lbl = "ENDDO"
            self._do_stack.append(("__WHILE__", 0, 0, "DOWHILE", ip + 1))
            if not _f_truthy(self._eval_expr(cond)):
                # Skip to matching END DO / ENDDO
                return self._skip_to_enddo(ip)
            return None

        # DO var = start, end [, step]  (new-style without label)
        m = re.match(r"^DO\s+(\w+)\s*=\s*(.+?),\s*(.+?)(?:,\s*(.+))?$", stmt)
        if m and not re.match(r"^DO\s+\d", stmt):
            var = m.group(1)
            start_val = float(self._eval_expr(m.group(2).strip()))
            end_val = float(self._eval_expr(m.group(3).strip()))
            step_val = float(self._eval_expr(m.group(4).strip())) if m.group(4) else 1.0
            self._vars[var] = start_val
            self._do_stack.append((var, end_val, step_val, "", ip + 1))
            if (step_val > 0 and start_val > end_val) or (
                step_val < 0 and start_val < end_val
            ):
                return self._skip_to_enddo(ip)
            return None

        # END DO / ENDDO (Fortran 90 style)
        if stmt in ("END DO", "ENDDO"):
            if self._do_stack:
                var, end_val, step_val, loop_lbl, back_ip = self._do_stack[-1]
                if var == "__WHILE__":
                    # Re-evaluate WHILE condition — need to find DO WHILE line
                    self._do_stack.pop()
                    # Re-run from back_ip
                    return back_ip - 1  # -1 because executor will +1
                new_val = _to_num(self._vars.get(var, 0)) + step_val
                self._vars[var] = new_val
                if (step_val >= 0 and new_val <= end_val) or (
                    step_val < 0 and new_val >= end_val
                ):
                    return back_ip  # loop back
                self._do_stack.pop()
            return None

        # GOTO label
        m = re.match(r"^GOTO\s+(\d+)", stmt)
        if m:
            lbl = m.group(1)
            if lbl not in self._labels:
                raise FortranError(f"Undefined label: {lbl}")
            return self._labels[lbl]

        # IF (cond) THEN
        m = re.match(r"^IF\s*\((.+)\)\s*THEN\s*$", stmt)
        if m:
            cond = self._eval_expr(m.group(1).strip())
            if _f_truthy(cond):
                return None  # execute THEN block
            else:
                # Skip to ELSE or ENDIF
                return self._skip_to_endif_or_else(ip)

        # IF (cond) stmt (inline – arithmetic or logical IF)
        m = re.match(r"^IF\s*\((.+?)\)\s+([^,].*)$", stmt)
        if m:
            cond = self._eval_expr(m.group(1).strip())
            if _f_truthy(cond):
                self._exec_stmt(m.group(2).strip(), ip)
            return None

        # ELSE IF (cond) THEN
        m = re.match(r"^ELSE\s*IF\s*\((.+)\)\s*THEN\s*$", stmt)
        if m:
            cond = self._eval_expr(m.group(1).strip())
            if _f_truthy(cond):
                return None  # execute this ELSE IF block
            else:
                return self._skip_to_endif_or_else(ip)

        # ELSE
        if stmt == "ELSE":
            # Skip to ENDIF
            return self._skip_to_endif(ip)

        # ENDIF
        if stmt == "ENDIF" or stmt == "END IF":
            return None

        # DO label var = start, end [, step]
        m = re.match(r"^DO\s+(\d+)\s+(\w+)\s*=\s*(.+?),\s*(.+?)(?:,\s*(.+))?$", stmt)
        if m:
            end_lbl = m.group(1)
            var = m.group(2)
            start_val = float(self._eval_expr(m.group(3).strip()))
            end_val = float(self._eval_expr(m.group(4).strip()))
            step_val = float(self._eval_expr(m.group(5).strip())) if m.group(5) else 1.0
            self._vars[var] = start_val
            self._do_stack.append((var, end_val, step_val, end_lbl, ip + 1))
            return None

        # CALL subroutine
        m = re.match(r"^CALL\s+(\w+)\s*(?:\((.+)\))?$", stmt)
        if m:
            sub_name = m.group(1).upper()
            if sub_name in self._subroutines:
                self._call_stack.append(ip + 1)
                return self._subroutines[sub_name] + 1  # skip SUBROUTINE header
            self._emit(f"ℹ️  CALL {sub_name} — external subroutine simulated")
            return None

        # Array subscript assignment: ARR(i) = expr
        m = re.match(r"^(\w+)\s*\((.+?)\)\s*=\s*(.+)$", stmt)
        if m:
            arr_name = m.group(1).upper()
            if arr_name in self._vars and isinstance(self._vars[arr_name], list):
                try:
                    idx = int(self._eval_expr(m.group(2).strip())) - 1
                    val = self._eval_expr(m.group(3).strip())
                    if 0 <= idx < len(self._vars[arr_name]):
                        self._vars[arr_name][idx] = val
                except Exception:
                    pass
                return None

        # Scalar assignment
        m = re.match(r"^(\w+)\s*=\s*(.+)$", stmt)
        if m:
            try:
                val = self._eval_expr(m.group(2).strip())
                self._vars[m.group(1).upper()] = val
            except Exception:
                pass
            return None

        return None

    def _skip_to_endif_or_else(self, start: int) -> int:
        depth = 1
        i = start + 1
        while i < len(self._lines):
            _, s = self._lines[i]
            if re.match(r"^IF\s*\(.+\)\s*THEN", s):
                depth += 1
            elif s == "ENDIF" or s == "END IF":
                depth -= 1
                if depth == 0:
                    return i + 1
            elif depth == 1:
                if s == "ELSE":
                    return i + 1  # skip ELSE keyword, execute body
                elif re.match(r"^ELSE\s*IF\s*\(.+\)\s*THEN", s):
                    return i  # evaluate ELSE IF condition
            i += 1
        return i

    def _skip_to_endif(self, start: int) -> int:
        depth = 1
        i = start + 1
        while i < len(self._lines):
            _, s = self._lines[i]
            if re.match(r"^IF\s*\(.+\)\s*THEN", s):
                depth += 1
            elif s == "ENDIF" or s == "END IF":
                depth -= 1
                if depth == 0:
                    return i + 1
            i += 1
        return i

    def _parse_declaration(self, stmt: str):
        # INTEGER I, J, K(10)  or  REAL X, Y  or  CHARACTER*20 NAME
        # Fortran 90: INTEGER :: I = 0, J  or  REAL :: X = 1.5
        # Also handles DOUBLE PRECISION, COMPLEX, CHARACTER*(n)

        # Strip optional :: separator (Fortran 90 style)
        decl_body = stmt
        if "::" in decl_body:
            # "INTEGER :: I = 0" → just the vars part after ::
            decl_body = (
                stmt.split("::", 1)[0].strip() + " " + stmt.split("::", 1)[1].strip()
            )

        char_m = re.match(r"^CHARACTER\s*(?:\*\s*(\d+|\*))?\s+(.+)$", decl_body)
        if char_m:
            width = (
                int(char_m.group(1))
                if (char_m.group(1) and char_m.group(1) != "*")
                else 1
            )
            for item in char_m.group(2).split(","):
                item = item.strip()
                # Strip initializer (= value)
                if "=" in item:
                    item = item.split("=")[0].strip()
                am = re.match(r"^(\w+)\s*\((\d+)\)$", item)
                if am:
                    self._vars[am.group(1).upper()] = [
                        "" for _ in range(int(am.group(2)))
                    ]
                    self._declared_vars.add(am.group(1).upper())
                elif item:
                    self._vars[item.upper()] = " " * width
                    self._declared_vars.add(item.upper())
            return
        m = re.match(
            r"^(?:INTEGER|REAL|LOGICAL|DOUBLE\s+PRECISION|COMPLEX|BYTE)\s+(.+)$",
            decl_body,
        )
        if m:
            is_int = decl_body.startswith("INTEGER") or decl_body.startswith("BYTE")
            for item in m.group(1).split(","):
                item = item.strip()
                # Handle initializer: I = 0
                init_val = None
                if "=" in item:
                    parts = item.split("=", 1)
                    item = parts[0].strip()
                    try:
                        init_val = self._eval_expr(parts[1].strip())
                    except Exception:
                        init_val = 0 if is_int else 0.0
                am = re.match(r"^(\w+)\s*\((.+)\)$", item)
                if am:
                    dims = [
                        int(float(self._eval_expr(d.strip())))
                        for d in am.group(2).split(",")
                    ]
                    total = 1
                    for d in dims:
                        total *= d
                    self._vars[am.group(1).upper()] = [0] * total
                    self._declared_vars.add(am.group(1).upper())
                elif item:
                    self._vars[item.upper()] = (
                        init_val if init_val is not None else (0 if is_int else 0.0)
                    )
                    self._declared_vars.add(item.upper())

    def _skip_to_enddo(self, start: int) -> int:
        """Skip DO body until END DO / ENDDO / matching labeled DO end."""
        depth = 1
        i = start + 1
        while i < len(self._lines):
            _, s = self._lines[i]
            if re.match(r"^DO\b", s):
                depth += 1
            elif s in ("END DO", "ENDDO"):
                depth -= 1
                if depth == 0:
                    return i + 1
            i += 1
        return i

    def _do_write(self, args_str: str):
        items = [a.strip() for a in self._split_args(args_str)]
        parts = []
        for item in items:
            if item.startswith("'") or item.startswith('"'):
                parts.append(item.strip("'\""))
            else:
                val = self._eval_expr(item)
                parts.append(str(val))
        self._emit(" ".join(parts))

    def _do_write_fmt(self, args_str: str, fmt: str):
        """Write with optional FORMAT descriptor."""
        items_vals = []
        if args_str:
            for item in self._split_args(args_str):
                item = item.strip()
                if item.startswith("'") or item.startswith('"'):
                    items_vals.append(item.strip("'\""))
                else:
                    items_vals.append(self._eval_expr(item))
        # Apply format
        result = self._apply_format(fmt, items_vals)
        self._emit(result)

    def _apply_format(self, fmt: str, vals: list) -> str:
        """Apply a FORTRAN FORMAT descriptor string to values."""
        # Resolve label reference
        if fmt and fmt.isdigit() and fmt in self._format_stmts:
            fmt = self._format_stmts[fmt]
        # Strip outer parens if wrapping format is *(...)
        fmt = re.sub(r"^\*\s*,\s*", "", fmt).strip("()")
        if not fmt or fmt == "*":
            return " ".join(str(v) for v in vals)
        parts = []
        val_idx = 0
        # Parse format descriptors
        descriptors = re.findall(
            r"'[^']*'|\"[^\"]*\"|/|\d*[IFEGALifeglX](?:\.\d+)?|\d+X", fmt
        )
        for desc in descriptors:
            desc = desc.strip()
            if not desc:
                continue
            if desc.startswith("'") or desc.startswith('"'):
                parts.append(desc.strip("'\""))
            elif desc.upper() == "/" or desc == "/":
                parts.append("\n")
            elif re.match(r"^\d*X$", desc, re.I):
                n = int(desc[:-1]) if desc[:-1].isdigit() else 1
                parts.append(" " * n)
            elif val_idx < len(vals):
                val = vals[val_idx]
                val_idx += 1
                dm = re.match(r"^(\d*)([IFEGALifegl])(?:\.(\d+))?$", desc)
                if dm:
                    width = int(dm.group(1)) if dm.group(1) else 0
                    spec = dm.group(2).upper()
                    prec = int(dm.group(3)) if dm.group(3) else 6
                    try:
                        if spec == "I":
                            s = str(int(float(val)))
                        elif spec == "F":
                            s = f"{float(val):.{prec}f}"
                        elif spec in ("E", "G"):
                            s = f"{float(val):.{prec}E}"
                        elif spec == "A":
                            s = str(val)
                        elif spec == "L":
                            s = "T" if val else "F"
                        else:
                            s = str(val)
                    except Exception:
                        s = str(val)
                    if width:
                        s = s.rjust(width)
                    parts.append(s)
                else:
                    parts.append(str(val))
            else:
                parts.append("")
        return "".join(parts)

    def _format_items(self, args_str: str, fmt: str) -> str:
        """Format items using FORMAT descriptor and return as string."""
        vals = []
        for item in self._split_args(args_str):
            item = item.strip()
            if item.startswith("'") or item.startswith('"'):
                vals.append(item.strip("'\""))
            elif item:
                vals.append(self._eval_expr(item))
        return self._apply_format(fmt, vals)

    def _split_args(self, args_str: str) -> list[str]:
        """Split comma-separated args respecting parentheses and quotes."""
        result = []
        depth = 0
        current = []
        in_quote = False
        quote_char = ""
        for ch in args_str:
            if in_quote:
                current.append(ch)
                if ch == quote_char:
                    in_quote = False
            elif ch in ("'", '"'):
                in_quote = True
                quote_char = ch
                current.append(ch)
            elif ch == "(":
                depth += 1
                current.append(ch)
            elif ch == ")":
                depth -= 1
                current.append(ch)
            elif ch == "," and depth == 0:
                result.append("".join(current))
                current = []
            else:
                current.append(ch)
        if current:
            result.append("".join(current))
        return result

    def _do_read(self, args_str: str):
        for var in self._split_args(args_str):
            var = var.strip()
            if var:
                raw = ""
                if hasattr(self.interpreter, "request_input"):
                    raw = self.interpreter.request_input(f"READ {var}: ") or "0"
                try:
                    self._vars[var.upper()] = float(raw)
                except ValueError:
                    self._vars[var.upper()] = raw

    def _eval_expr(self, expr: str) -> Any:
        expr = expr.strip()
        # String literal
        if expr.startswith("'") or expr.startswith('"'):
            return expr.strip("'\"")
        # Boolean
        if expr in (".TRUE.", ".T."):
            return True
        if expr in (".FALSE.", ".F."):
            return False
        # Number
        try:
            return int(expr)
        except ValueError:
            pass
        try:
            return float(expr.replace("D", "E").replace("d", "e"))
        except ValueError:
            pass
        # Parenthesised
        if expr.startswith("(") and expr.endswith(")"):
            return self._eval_expr(expr[1:-1])
        # Intrinsic functions
        m = re.match(r"^(\w+)\s*\((.+)\)$", expr)
        if m:
            fn = m.group(1).upper()
            raw_args = m.group(2).strip()
            args_list = self._split_args(raw_args)
            args_evaled = [self._eval_expr(a.strip()) for a in args_list if a.strip()]
            arg = args_evaled[0] if args_evaled else 0
            # Single-arg numeric intrinsics
            single_fns: dict = {
                "SQRT": math.sqrt,
                "ABS": abs,
                "IABS": lambda x: abs(int(x)),
                "INT": int,
                "IFIX": int,
                "IDINT": int,
                "AINT": math.trunc,
                "ANINT": round,
                "NINT": round,
                "REAL": float,
                "FLOAT": float,
                "DBLE": float,
                "SIN": math.sin,
                "COS": math.cos,
                "TAN": math.tan,
                "ASIN": math.asin,
                "ACOS": math.acos,
                "ATAN": math.atan,
                "SIND": lambda x: math.sin(math.radians(x)),
                "COSD": lambda x: math.cos(math.radians(x)),
                "TAND": lambda x: math.tan(math.radians(x)),
                "SINH": math.sinh,
                "COSH": math.cosh,
                "TANH": math.tanh,
                "EXP": math.exp,
                "LOG": math.log,
                "ALOG": math.log,
                "LOG10": math.log10,
                "ALOG10": math.log10,
                "LOG2": math.log2,
                "CEILING": math.ceil,
                "FLOOR": math.floor,
                "SIGN": lambda x: (1.0 if x >= 0 else -1.0),
                "NOT": lambda x: ~int(x),
                "LEN": lambda x: len(str(x)),
                "LEN_TRIM": lambda x: len(str(x).rstrip()),
                "TRIM": lambda x: str(x).rstrip(),
                "ADJUSTL": lambda x: str(x).lstrip(),
                "ADJUSTR": lambda x: str(x).rjust(len(str(x))),
                "CHAR": lambda x: chr(int(x)),
                "ICHAR": lambda x: ord(str(x)[0]) if str(x) else 0,
                "IACHAR": lambda x: ord(str(x)[0]) if str(x) else 0,
                "ACHAR": lambda x: chr(int(x)),
                "BIT_SIZE": lambda x: 32,
                "HUGE": lambda _: 2147483647,
                "TINY": lambda _: 1.175494e-38,
                "EPSILON": lambda _: 1.192093e-07,
                "KIND": lambda x: 4,
                "SELECTED_INT_KIND": lambda x: 4,
                "SELECTED_REAL_KIND": lambda x: 4,
                "ALLOCATED": lambda x: 1,
                "PRESENT": lambda x: 1,
                "UBOUND": lambda x: len(x) if isinstance(x, list) else 1,
                "LBOUND": lambda _: 1,
                "SIZE": lambda x: len(x) if isinstance(x, list) else 1,
                "SHAPE": lambda x: [len(x)] if isinstance(x, list) else [1],
            }
            if fn in single_fns:
                try:
                    return single_fns[fn](arg)
                except Exception:
                    return 0
            # Two-arg intrinsics
            if len(args_evaled) >= 2:
                arg2 = args_evaled[1]
                two_fns: dict = {
                    "MOD": lambda a, b: a % b if b else 0,
                    "MODULO": lambda a, b: a % b if b else 0,
                    "MAX": max,
                    "MAX0": max,
                    "MAX1": max,
                    "AMAX0": max,
                    "AMAX1": max,
                    "MIN": min,
                    "MIN0": min,
                    "MIN1": min,
                    "AMIN0": min,
                    "AMIN1": min,
                    "ATAN2": math.atan2,
                    "DIM": lambda a, b: max(0.0, a - b),
                    "DPROD": lambda a, b: float(a) * float(b),
                    "SIGN": lambda a, b: abs(float(a))
                    * (1.0 if float(b) >= 0 else -1.0),
                    "IBITS": lambda a, b: (int(a) >> int(b)),
                    "ISHFT": lambda a, b: (
                        int(a) << int(b) if int(b) >= 0 else int(a) >> (-int(b))
                    ),
                    "IOR": lambda a, b: int(a) | int(b),
                    "IAND": lambda a, b: int(a) & int(b),
                    "IEOR": lambda a, b: int(a) ^ int(b),
                    "INDEX": lambda a, b: str(a).find(str(b)) + 1,
                    "SCAN": lambda a, b: next(
                        (i + 1 for i, c in enumerate(str(a)) if c in str(b)), 0
                    ),
                    "VERIFY": lambda a, b: next(
                        (i + 1 for i, c in enumerate(str(a)) if c not in str(b)), 0
                    ),
                }
                if fn in two_fns:
                    try:
                        return two_fns[fn](arg, arg2)
                    except Exception:
                        return 0
            # Variable-arg: MAX/MIN with multiple args
            if fn in ("MAX", "AMAX1", "MAX0") and len(args_evaled) > 2:
                return max(args_evaled)
            if fn in ("MIN", "AMIN1", "MIN0") and len(args_evaled) > 2:
                return min(args_evaled)
            # CMPLX, TRANSFER, RESHAPE — return first arg
            if fn in ("CMPLX", "TRANSFER", "RESHAPE", "SPREAD", "TRANSPOSE", "MATMUL"):
                return arg
            # String join functions
            if fn in ("REPEAT",) and len(args_evaled) >= 2:
                return str(arg) * int(args_evaled[1])
            if fn == "MERGE" and len(args_evaled) >= 3:
                return args_evaled[0] if _f_truthy(args_evaled[2]) else args_evaled[1]
            # COUNT of logical array
            if fn == "COUNT" and isinstance(arg, list):
                return sum(1 for x in arg if _f_truthy(x))
            if fn == "SUM" and isinstance(arg, list):
                return sum(float(x) for x in arg)
            if fn == "PRODUCT" and isinstance(arg, list):
                result_p = 1.0
                for x in arg:
                    result_p *= float(x)
                return result_p
            # User-defined function call
            if fn in self._functions:
                func_start = self._functions[fn]
                # Push args as parameter variables (positional)
                saved_vars = dict(self._vars)
                # Parse the FUNCTION header to get param names
                _, func_header = self._lines[func_start]
                param_m = re.match(
                    r"^(?:(?:INTEGER|REAL|DOUBLE\s+PRECISION|CHARACTER|LOGICAL)\s+)?FUNCTION\s+\w+\s*\(([^)]*)\)",
                    func_header,
                )
                if param_m:
                    params = [
                        p.strip().upper()
                        for p in param_m.group(1).split(",")
                        if p.strip()
                    ]
                    for pname, pval in zip(params, args_evaled):
                        self._vars[pname] = pval
                # Initialize result variable (same name as function)
                self._vars[fn] = 0
                self._call_stack.append(-1)  # -1 = return to caller
                try:
                    self._exec_from(func_start + 1)
                except Exception:
                    pass
                if self._call_stack and self._call_stack[-1] == -1:
                    self._call_stack.pop()
                result_val = self._vars.get(fn, 0)
                # Coerce return type if declared
                ret_type = self._func_return_types.get(fn)
                if ret_type == "INTEGER":
                    try:
                        result_val = int(float(result_val))
                    except (ValueError, TypeError):
                        result_val = 0
                elif ret_type in ("REAL", "DOUBLE PRECISION"):
                    try:
                        result_val = float(result_val)
                    except (ValueError, TypeError):
                        result_val = 0.0
                self._vars = saved_vars
                return result_val
        # Fallback: variable look-up with subscript support
        # Subscripted array: NAME(i1[,i2])
        ma = re.match(r"^(\w+)\s*\((.+)\)$", expr)
        if ma:
            arr_name = ma.group(1).upper()
            idx_expr = ma.group(2).strip()
            arr = self._vars.get(arr_name)
            if isinstance(arr, list):
                try:
                    idx = int(self._eval_expr(idx_expr)) - 1  # 1-based
                    return arr[idx] if 0 <= idx < len(arr) else 0
                except Exception:
                    pass
        # STRING CONCATENATION //
        if "//" in expr and not re.match(r"^['\"]", expr):
            parts = expr.split("//")
            return "".join(str(self._eval_expr(p.strip())) for p in parts)
        # Simple variable lookup — avoids eval issues with string values
        if re.match(r"^\w+$", expr):
            upper = expr.upper()
            if upper in self._named_constants:
                return self._named_constants[upper]
            if upper in self._vars:
                return self._vars[upper]
            # IMPLICIT NONE enforcement for simple token lookups
            if (
                self._implicit_none
                and not upper.lstrip("-").isdigit()
                and upper not in self._functions
                and upper not in self._subroutines
                and upper not in self._declared_vars
                and upper not in _FORTRAN_INTRINSICS
                and upper not in (".TRUE.", ".FALSE.")
                and not upper.endswith("D0")
            ):
                self._emit(f"❌ IMPLICIT NONE: variable '{expr}' used but not declared")

        # Build Python expression substituting variables
        def replace_var(m_):
            name = m_.group(0)
            upper = name.upper()
            if upper in (".TRUE.", ".FALSE."):
                return "True" if upper == ".TRUE." else "False"
            # Check named constants first
            if upper in self._named_constants:
                return str(self._named_constants[upper])
            # IMPLICIT NONE enforcement
            if (
                self._implicit_none
                and upper not in self._vars
                and upper not in self._named_constants
                and upper not in self._functions
                and upper not in self._subroutines
                and upper not in self._declared_vars
                and upper not in _FORTRAN_INTRINSICS
                and not upper.lstrip("-").replace(".", "").isdigit()
                and not upper.endswith("D0")
            ):
                self._emit(f"❌ IMPLICIT NONE: '{name}' used but not declared")
            val = self._vars.get(upper, self._vars.get(name))
            if val is not None and not isinstance(val, list):
                return str(val)
            return name

        pyexpr = re.sub(r"\b\w+\b", replace_var, expr)
        pyexpr = pyexpr.replace("**", "**")
        pyexpr = re.sub(r"\.EQ\.", "==", pyexpr)
        pyexpr = re.sub(r"\.NE\.", "!=", pyexpr)
        pyexpr = re.sub(r"\.LT\.", "<", pyexpr)
        pyexpr = re.sub(r"\.GT\.", ">", pyexpr)
        pyexpr = re.sub(r"\.LE\.", "<=", pyexpr)
        pyexpr = re.sub(r"\.GE\.", ">=", pyexpr)
        pyexpr = re.sub(r"\.AND\.", " and ", pyexpr)
        pyexpr = re.sub(r"\.OR\.", " or ", pyexpr)
        pyexpr = re.sub(r"\.NOT\.", " not ", pyexpr)
        pyexpr = re.sub(r"\.EQV\.", "==", pyexpr)
        pyexpr = re.sub(r"\.NEQV\.", "!=", pyexpr)
        try:
            return eval(
                pyexpr,
                {
                    "__builtins__": {},
                    "abs": abs,
                    "int": int,
                    "float": float,  # noqa: S307
                    "round": round,
                    "math": math,
                },
            )
        except Exception:
            return 0


def _f_truthy(val: Any) -> bool:
    if isinstance(val, bool):
        return val
    if isinstance(val, (int, float)):
        return val != 0
    return bool(val)


def _to_num(val: Any) -> float:
    try:
        return float(val)
    except (TypeError, ValueError):
        return 0.0


class FortranStop(Exception):
    pass


class FortranError(Exception):
    pass
