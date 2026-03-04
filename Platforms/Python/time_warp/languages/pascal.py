"""
Minimal Pascal-like executor (Turbo Pascal era flavor)

Supported subset:
- var declarations: `var x: integer;`, multiple names allowed
- assignment: `x := expr;`
- write/writeln: `write(expr, ...)`, `writeln(expr, ...)`
- readln: `readln(x)` (single variable)
- begin/end/program/uses: accepted as no-ops
"""

from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any, Dict, List, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter


_VAR_RE = re.compile(
    r"^\s*(?:var\s+)?(.+):\s*(integer|longint|real|string|char|boolean|byte|word|shortint|double)\s*;?\s*$",
    re.IGNORECASE,
)
_VAR_ARRAY_RE = re.compile(
    (
        r"^\s*(?:var\s+)?(.+):\s*array\s*\[\s*(\d+)\.\.(\d+)\s*\]\s*of\s*"
        r"(integer|longint|real|string|char|boolean|byte|word|shortint|double)\s*;?\s*$"
    ),
    re.IGNORECASE,
)
# 2D array declarations and custom type declarations — treated as simple
# variable setup (flatten 2D to 1D, custom types default to real)
_VAR_ARRAY_2D_RE = re.compile(
    r"^\s*(?:var\s+)?(.+):\s*array\s*\[.+\]\s*of\s*\w+\s*;?\s*$",
    re.IGNORECASE,
)
# Type definition line: e.g. "TIntArray = array[1..MAX] of Integer;"
_TYPE_DEF_RE = re.compile(
    r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(array|record|class|set|packed)\b",
    re.IGNORECASE,
)
# Catch-all variable declaration with unknown/custom type
_VAR_CUSTOM_TYPE_RE = re.compile(
    r"^\s*(?:var\s+)?([A-Za-z_][A-Za-z0-9_]*(?:\s*,\s*[A-Za-z_][A-Za-z0-9_]*)*):\s*([A-Za-z_][A-Za-z0-9_]*)\s*;?\s*$",
    re.IGNORECASE,
)
_VAR_KEYWORD_RE = re.compile(r"^\s*var\s*$", re.IGNORECASE)
_CONST_RE = re.compile(r"^\s*const\s*$", re.IGNORECASE)
_CONST_DEF_RE = re.compile(
    r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.+);?\s*$", re.IGNORECASE
)

_ASSIGN_RE = re.compile(r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*:=\s*(.+);?\s*$")
_ASSIGN_ARRAY_RE = re.compile(
    r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*\[(.+)\]\s*:=\s*(.+);?\s*$"
)
# Field assignment: var.field := expr
_ASSIGN_FIELD_RE = re.compile(
    r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*\.\s*([A-Za-z_][A-Za-z0-9_]*)\s*:=\s*(.+);?\s*$"
)

_WRITE_RE = re.compile(
    r"^\s*(writeln|write)\s*\((.*)\)\s*;?\s*$",
    re.IGNORECASE,
)
_BARE_WRITELN_RE = re.compile(
    r"^\s*(writeln|write)\s*;?\s*$",
    re.IGNORECASE,
)
_READLN_RE = re.compile(
    r"^\s*readln\s*\((.*)\)\s*;?\s*$",
    re.IGNORECASE,
)
_CLRSCR_RE = re.compile(r"^\s*clrscr\s*;?\s*$", re.IGNORECASE)
_DELAY_RE = re.compile(r"^\s*delay\s*\((.+)\)\s*;?\s*$", re.IGNORECASE)
_TEXTCOLOR_RE = re.compile(r"^\s*textcolor\s*\((.+)\)\s*;?\s*$", re.IGNORECASE)
_GOTOXY_RE = re.compile(r"^\s*gotoxy\s*\((.+),(.+)\)\s*;?\s*$", re.IGNORECASE)
_RANDOMIZE_RE = re.compile(r"^\s*randomize\s*;?\s*$", re.IGNORECASE)

_IF_THEN_RE = re.compile(
    r"^\s*if\s+(.+)\s+then\s*;?\s*$",
    re.IGNORECASE,
)
_ELSE_RE = re.compile(
    r"^\s*else\s*;?\s*$",
    re.IGNORECASE,
)
_WHILE_DO_RE = re.compile(
    r"^\s*while\s+(.+)\s+do\s*;?\s*$",
    re.IGNORECASE,
)
_REPEAT_RE = re.compile(r"^\s*repeat\s*$", re.IGNORECASE)
_UNTIL_RE = re.compile(r"^\s*until\s+(.+)\s*;?\s*$", re.IGNORECASE)
_FOR_RE = re.compile(
    (
        r"^\s*for\s+([A-Za-z_][A-Za-z0-9_]*)\s*:=\s*(.+)\s+"
        + r"(to|downto)\s+(.+)\s+do\s*$"
    ),
    re.IGNORECASE,
)
_CASE_RE = re.compile(r"^\s*case\s+(.+)\s+of\s*$", re.IGNORECASE)
_CASE_LABEL_RE = re.compile(r"^\s*([0-9]+|'.*?'|\".*?\")\s*:\s*$")
# Also match inline case body: "1: writeln('x');"
_CASE_LABEL_INLINE_RE = re.compile(r"^\s*([0-9]+|'.*?'|\".*?\")\s*:\s*(.+)$")
_CASE_ELSE_RE = re.compile(r"^\s*else\s*:\s*$", re.IGNORECASE)
_PROC_RE = re.compile(
    r"^\s*procedure\s+([A-Za-z_][A-Za-z0-9_]*)\s*(?:\((.*)\))?\s*;\s*$",
    re.IGNORECASE,
)
_FUNC_RE = re.compile(
    r"^\s*function\s+([A-Za-z_][A-Za-z0-9_]*)\s*(?:\((.*)\))?\s*"
    r"(?::\s*(integer|longint|real|string|char|boolean|byte|word|shortint|double))?\s*;\s*$",
    re.IGNORECASE,
)
_CALL_RE = re.compile(r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*(?:\((.*)\))?\s*;\s*$")


def _ensure_pascal_stack(interpreter: "Interpreter"):
    if not hasattr(interpreter, "pascal_block_stack"):
        interpreter.pascal_block_stack = []


def _call_func_inline(
    interpreter: "Interpreter", fname: str, args_str: str, info: dict
) -> Any:
    """Execute a user-defined Pascal function inline and return its result."""
    params = info.get("params", [])
    args = _split_args(args_str) if args_str.strip() else []

    # Backup and bind parameters
    backups: list[dict] = []
    for idx, p in enumerate(params):
        pname = p["name"]
        suf = p["suffix"]
        var_key = pname + suf
        existed = False
        old_val: Any = None
        if suf == "$":
            if var_key in interpreter.string_variables:
                existed = True
                old_val = interpreter.string_variables[var_key]
        else:
            if pname in interpreter.variables:
                existed = True
                old_val = interpreter.variables[pname]
        backups.append(
            {"key": var_key, "is_str": suf == "$", "existed": existed, "old": old_val}
        )

        # Bind parameter value
        val: Any = "" if suf == "$" else 0
        if idx < len(args):
            a = args[idx].strip()
            if suf == "$":
                val = _eval_str(interpreter, a)
            else:
                val = _pascal_eval_expr(interpreter, a)
        interpreter.set_typed_variable(var_key, val)
        if not hasattr(interpreter, "pascal_types"):
            interpreter.pascal_types = {}
        interpreter.pascal_types[pname] = suf

    # Backup the function name variable
    fsuf = getattr(interpreter, "pascal_types", {}).get(fname, "#")
    fvar_key = fname + fsuf
    f_existed = False
    f_old: Any = None
    if fsuf == "$":
        if fvar_key in interpreter.string_variables:
            f_existed = True
            f_old = interpreter.string_variables[fvar_key]
    else:
        if fname in interpreter.variables:
            f_existed = True
            f_old = interpreter.variables[fname]

    # Execute function body lines inline
    saved_line = interpreter.current_line
    for line_idx in range(info["start"], info["end"]):
        _, line_text = interpreter.program_lines[line_idx]
        if line_text.strip():
            execute_pascal(interpreter, line_text.strip(), None)
    interpreter.current_line = saved_line

    # Read return value: function assigns to its own name
    if fsuf == "$":
        result = interpreter.string_variables.get(fvar_key, "")
    else:
        result = interpreter.get_numeric_value(fname)
        if result is None:
            result = 0

    # Restore backups
    for b in backups:
        if b["is_str"]:
            if b["existed"]:
                interpreter.string_variables[b["key"]] = b["old"]
            else:
                interpreter.string_variables.pop(b["key"], None)
        else:
            vname = b["key"][:-1] if b["key"].endswith(("%", "#", "$")) else b["key"]
            if b["existed"]:
                interpreter.variables[vname] = b["old"]
            else:
                interpreter.variables.pop(vname, None)

    # Restore function name variable
    if fsuf == "$":
        if f_existed:
            interpreter.string_variables[fvar_key] = f_old
        else:
            interpreter.string_variables.pop(fvar_key, None)
    else:
        if f_existed:
            interpreter.variables[fname] = f_old
        else:
            interpreter.variables.pop(fname, None)

    return result


def _pascal_eval_expr(interpreter: "Interpreter", expr: str) -> Any:
    """Evaluate a Pascal expression, expanding Pascal built-in functions first."""
    import math as _math

    expr = expr.strip()

    # String functions that return strings – handle specially
    # COPY(s, pos, len)
    m = re.match(r"^COPY\s*\((.+),\s*(.+),\s*(.+)\)$", expr, re.IGNORECASE)
    if m:
        s_e, pos_e, len_e = m.group(1).strip(), m.group(2).strip(), m.group(3).strip()
        s = _eval_str(interpreter, s_e)
        try:
            pos = int(interpreter.evaluate_expression(pos_e)) - 1
            ln = int(interpreter.evaluate_expression(len_e))
        except Exception:
            pos, ln = 0, 0
        return s[pos : pos + ln]

    # POS(sub, s) — returns integer
    m = re.match(r"^POS\s*\((.+),\s*(.+)\)$", expr, re.IGNORECASE)
    if m:
        sub = _eval_str(interpreter, m.group(1).strip())
        s = _eval_str(interpreter, m.group(2).strip())
        idx = s.find(sub)
        return idx + 1 if idx >= 0 else 0

    # LENGTH(s)
    m = re.match(r"^LENGTH\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        s = _eval_str(interpreter, m.group(1).strip())
        return len(s)

    # UPCASE(s)
    m = re.match(r"^UPCASE\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        return _eval_str(interpreter, m.group(1).strip()).upper()

    # UPPERCASE(s)
    m = re.match(r"^UPPERCASE\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        return _eval_str(interpreter, m.group(1).strip()).upper()

    # LOWERCASE(s)
    m = re.match(r"^LOWERCASE\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        return _eval_str(interpreter, m.group(1).strip()).lower()

    # TRIM(s)
    m = re.match(r"^TRIM\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        return _eval_str(interpreter, m.group(1).strip()).strip()

    # CONCAT(s1, s2, ...)
    m = re.match(r"^CONCAT\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        return "".join(
            _eval_str(interpreter, a.strip()) for a in _split_args(m.group(1))
        )

    # STRINGOFCHAR(c, n)
    m = re.match(r"^STRINGOFCHAR\s*\((.+),\s*(.+)\)$", expr, re.IGNORECASE)
    if m:
        c = _eval_str(interpreter, m.group(1).strip())
        try:
            n = int(interpreter.evaluate_expression(m.group(2).strip()))
        except Exception:
            n = 0
        return c[0] * n if c else ""

    # ORD(c)
    m = re.match(r"^ORD\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        s = _eval_str(interpreter, m.group(1).strip())
        return ord(s[0]) if s else 0

    # CHR(n)
    m = re.match(r"^CHR\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        try:
            return chr(int(interpreter.evaluate_expression(m.group(1).strip())))
        except Exception:
            return "\x00"

    # SUCC(n) / PRED(n)
    m = re.match(r"^(SUCC|PRED)\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        op = m.group(1).upper()
        try:
            v = interpreter.evaluate_expression(m.group(2).strip())
            return v + 1 if op == "SUCC" else v - 1
        except Exception:
            return 0

    # ODD(n)
    m = re.match(r"^ODD\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        try:
            return int(interpreter.evaluate_expression(m.group(1).strip())) % 2 != 0
        except Exception:
            return False

    # RANDOM(n)
    m = re.match(r"^RANDOM\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        import random as _rand

        try:
            n = int(interpreter.evaluate_expression(m.group(1).strip()))
            return _rand.randint(0, n - 1)
        except Exception:
            return 0

    # RANDOM (no args)
    if re.match(r"^RANDOM\s*$", expr, re.IGNORECASE):
        import random as _rand

        return _rand.random()

    # SQR(x)
    m = re.match(r"^SQR\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        try:
            v = float(interpreter.evaluate_expression(m.group(1).strip()))
            return v * v
        except Exception:
            return 0

    # FRAC(x)
    m = re.match(r"^FRAC\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        try:
            return _math.modf(
                float(interpreter.evaluate_expression(m.group(1).strip()))
            )[0]
        except Exception:
            return 0.0

    # INTFUNC / INT as function
    m = re.match(r"^INT\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        try:
            return float(int(interpreter.evaluate_expression(m.group(1).strip())))
        except Exception:
            return 0.0

    # TRUNC(x)
    m = re.match(r"^TRUNC\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        try:
            return int(interpreter.evaluate_expression(m.group(1).strip()))
        except Exception:
            return 0

    # ROUND(x)
    m = re.match(r"^ROUND\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        try:
            return round(float(interpreter.evaluate_expression(m.group(1).strip())))
        except Exception:
            return 0

    # ABS(x)
    m = re.match(r"^ABS\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        try:
            return abs(interpreter.evaluate_expression(m.group(1).strip()))
        except Exception:
            return 0

    # SQRT(x), SIN(x), COS(x), ARCTAN(x), EXP(x), LN(x)
    m = re.match(r"^(SQRT|SIN|COS|ARCTAN|EXP|LN)\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        fn = m.group(1).upper()
        try:
            v = float(interpreter.evaluate_expression(m.group(2).strip()))
            funcs = {
                "SQRT": _math.sqrt,
                "SIN": _math.sin,
                "COS": _math.cos,
                "ARCTAN": _math.atan,
                "EXP": _math.exp,
                "LN": _math.log,
            }
            return funcs[fn](v)
        except Exception:
            return 0.0

    # MAX(a,b) / MIN(a,b)
    m = re.match(r"^(MAX|MIN)\s*\((.+),\s*(.+)\)$", expr, re.IGNORECASE)
    if m:
        fn = m.group(1).upper()
        try:
            a = interpreter.evaluate_expression(m.group(2).strip())
            b = interpreter.evaluate_expression(m.group(3).strip())
            return max(a, b) if fn == "MAX" else min(a, b)
        except Exception:
            return 0

    # User-defined function call: NAME(args)
    m = re.match(r"^([A-Za-z_][A-Za-z0-9_]*)\s*\((.+)\)$", expr, re.IGNORECASE)
    if m and hasattr(interpreter, "pascal_procs"):
        fname = m.group(1).upper()
        info = interpreter.pascal_procs.get(fname)
        if info and info.get("is_func"):
            return _call_func_inline(interpreter, fname, m.group(2).strip(), info)

    # ── SysUtils / CRT built-in functions ────────────────────────────────────
    # INTTOSTR(n)
    m = re.match(r"^INTTOSTR\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        try:
            return str(int(interpreter.evaluate_expression(m.group(1).strip())))
        except Exception:
            return "0"

    # STRTOINT(s) / STRTOINTDEF(s, default)
    m = re.match(r"^STRTOINTDEF\s*\((.+),\s*(.+)\)$", expr, re.IGNORECASE)
    if m:
        try:
            return int(interpreter.evaluate_expression(m.group(1).strip()))
        except Exception:
            try:
                return int(interpreter.evaluate_expression(m.group(2).strip()))
            except Exception:
                return 0

    m = re.match(r"^STRTOINT\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        s = _eval_str(interpreter, m.group(1).strip())
        try:
            return int(s)
        except ValueError:
            return 0

    # STRTOFLOAT(s)
    m = re.match(r"^STRTOFLOAT\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        s = _eval_str(interpreter, m.group(1).strip())
        try:
            return float(s)
        except ValueError:
            return 0.0

    # FLOATTOSTR(f)
    m = re.match(r"^FLOATTOSTR\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        try:
            v = float(interpreter.evaluate_expression(m.group(1).strip()))
            return str(v)
        except Exception:
            return "0"

    # FORMAT(fmt, [args]) — simplified: replace %d/%s/%f placeholders
    m = re.match(r"^FORMAT\s*\((.+)\)$", expr, re.IGNORECASE)
    if m:
        inner = m.group(1).strip()
        arg_list = _split_args(inner)
        if arg_list:
            fmt_str = _eval_str(interpreter, arg_list[0])
            vals = []
            for a in arg_list[1:]:
                # strip [...] array syntax if present
                a = a.strip().strip("[]")
                if a:
                    try:
                        vals.append(interpreter.evaluate_expression(a))
                    except Exception:
                        vals.append(a)
            try:
                # Replace Pascal-style %d/%s/%f with Python equivalents
                result = re.sub(r"%\d*(?:\.\d+)?[dsfg]", "{}", fmt_str)
                return result.format(*vals)
            except Exception:
                return fmt_str
        return ""

    # Fallthrough – translate Pascal operators to Python and evaluate
    # Resolve string variables first
    up = expr.strip().upper()
    if up + "$" in interpreter.string_variables:
        return interpreter.string_variables[up + "$"]
    if up in interpreter.variables:
        return interpreter.variables[up]

    # Record field access: var.field
    field_m = re.match(
        r"^([A-Za-z_][A-Za-z0-9_]*)\s*\.\s*([A-Za-z_][A-Za-z0-9_]*)$",
        expr,
        re.IGNORECASE,
    )
    if field_m:
        rec_name = field_m.group(1).upper()
        field_name = field_m.group(2).upper()
        rec = interpreter.variables.get(rec_name)
        if isinstance(rec, dict):
            return rec.get(field_name, 0)

    # Translate Pascal operators to Python equivalents
    pyexpr = expr

    # Expand record field accesses like p.x → actual value before further processing
    def _field_replacer(m_field):
        rec_name = m_field.group(1).upper()
        field_name = m_field.group(2).upper()
        rec = interpreter.variables.get(rec_name)
        if isinstance(rec, dict) and field_name in rec:
            v = rec[field_name]
            return str(int(v)) if isinstance(v, float) and v == int(v) else str(v)
        return m_field.group(0)

    pyexpr = re.sub(
        r"\b([A-Za-z_][A-Za-z0-9_]*)\s*\.\s*([A-Za-z_][A-Za-z0-9_]*)\b",
        _field_replacer,
        pyexpr,
    )
    # Replace Pascal operators (word-boundary aware, case-insensitive)
    pyexpr = re.sub(r"\bdiv\b", "//", pyexpr, flags=re.IGNORECASE)
    pyexpr = re.sub(r"\bmod\b", "%", pyexpr, flags=re.IGNORECASE)
    pyexpr = re.sub(r"\band\b", " and ", pyexpr, flags=re.IGNORECASE)
    pyexpr = re.sub(r"\bor\b", " or ", pyexpr, flags=re.IGNORECASE)
    pyexpr = re.sub(r"\bnot\b", " not ", pyexpr, flags=re.IGNORECASE)
    pyexpr = pyexpr.replace("<>", "!=")
    # Single '=' that isn't '<=' or '>=' or '<>' or '!=' → '=='
    pyexpr = re.sub(r"(?<![<>!=])=(?!=)", "==", pyexpr)

    # Replace Pascal variable names with their values in the expression
    def _var_replacer(m_inner):
        vname = m_inner.group(0).upper()
        if vname + "$" in interpreter.string_variables:
            sval = interpreter.string_variables[vname + "$"]
            return repr(sval)
        if vname in interpreter.variables:
            v = interpreter.variables[vname]
            return str(int(v)) if v == int(v) else str(v)
        return m_inner.group(0)

    pyexpr = re.sub(r"\b([A-Za-z_][A-Za-z0-9_]*)\b", _var_replacer, pyexpr)
    # Clean up stray Python keywords that leaked through replacement
    for kw in ("and", "or", "not", "True", "False", "None"):
        pyexpr = pyexpr.replace(repr(kw), kw)

    try:
        return eval(pyexpr)  # noqa: S307
    except Exception:
        pass

    return interpreter.evaluate_expression(expr)


def _eval_str(interpreter: "Interpreter", expr: str) -> str:
    """Evaluate a Pascal expression that should be a string."""
    if expr.startswith("'") or expr.startswith('"'):
        return _unquote(expr)
    up = expr.upper()
    if up + "$" in interpreter.string_variables:
        return interpreter.string_variables[up + "$"]
    # Try Pascal built-ins
    result = _pascal_eval_expr(interpreter, expr)
    if isinstance(result, str):
        return result
    return str(result) if result is not None else ""


def _is_begin(line: str) -> bool:
    u = line.strip().upper()
    return u == "BEGIN"


def _is_end(line: str) -> bool:
    return bool(re.match(r"^\s*END\s*[.;]?\s*$", line, re.IGNORECASE))


def _find_next_statement(
    interpreter: "Interpreter",
    start_idx: int,
) -> int | None:
    """Find the index of the next non-empty, non-comment line after *start_idx*."""
    j = start_idx + 1
    while j < len(interpreter.program_lines):
        s = interpreter.program_lines[j][1].strip()
        if not s:
            j += 1
            continue
        if s.startswith("//") or s.startswith("{") or s.startswith("(*"):
            j += 1
            continue
        return j
    return None


def _find_begin_forward(
    interpreter: "Interpreter",
    start_idx: int,
) -> int | None:
    j = start_idx + 1
    while j < len(interpreter.program_lines):
        s = interpreter.program_lines[j][1]
        if not s.strip():
            j += 1
            continue
        # Skip comments
        if (
            s.strip().startswith("//")
            or s.strip().startswith("{")
            or s.strip().startswith("(*")
        ):
            j += 1
            continue
        # Skip variable declarations
        if _VAR_KEYWORD_RE.match(s):
            j += 1
            continue
        if _VAR_RE.match(s):
            j += 1
            continue
        if _VAR_ARRAY_RE.match(s):
            j += 1
            continue
        if _CONST_RE.match(s):
            j += 1
            continue
        if _CONST_DEF_RE.match(s):
            j += 1
            continue
        if _is_begin(s):
            return j
        break
    return None


def _find_end_for_begin(interpreter: "Interpreter", begin_idx: int) -> int:
    depth = 0
    j = begin_idx
    while j < len(interpreter.program_lines):
        s = interpreter.program_lines[j][1]
        if _is_begin(s):
            depth += 1
        if _is_end(s):
            depth -= 1
            if depth == 0:
                return j
        j += 1
    return begin_idx


def _parse_case_blocks(interpreter: "Interpreter", case_idx: int):
    lines = interpreter.program_lines
    j = case_idx + 1
    blocks: List[Tuple[str, int, int]] = []  # (label|"__ELSE__", begin_inner, end_idx)
    current_label = None
    current_begin = None
    while j < len(lines):
        s = lines[j][1]
        if not s.strip():
            j += 1
            continue
        if _is_end(s):
            if current_label is not None and current_begin is None:
                blocks.append((current_label, j, j))
            return blocks, j
        if _CASE_ELSE_RE.match(s):
            current_label = "__ELSE__"
            current_begin = None
            j += 1
            continue
        # Check for inline case body: "1: writeln('x');"
        mlabel_inline = _CASE_LABEL_INLINE_RE.match(s)
        mlabel = _CASE_LABEL_RE.match(s) if not mlabel_inline else None
        if mlabel_inline:
            if current_label is not None and current_begin is None:
                blocks.append((current_label, j, j))
            label_text = mlabel_inline.group(1)
            lbl = label_text[1:-1] if label_text.startswith(("'", '"')) else label_text
            # Store inline body text for later execution
            blocks.append((lbl, j, j))
            current_label = None
            current_begin = None
            j += 1
            continue
        if mlabel:
            if current_label is not None and current_begin is None:
                blocks.append((current_label, j, j))
            label_text = mlabel.group(1)
            if label_text.startswith("'") or label_text.startswith('"'):
                lbl = label_text[1:-1]
            else:
                lbl = label_text
            current_label = lbl
            current_begin = None
            j += 1
            continue
        if _is_begin(s) and current_label is not None and current_begin is None:
            begin_idx = j
            end_idx = _find_end_for_begin(interpreter, begin_idx)
            blocks.append((current_label, begin_idx + 1, end_idx))
            current_label = None
            current_begin = None
            j = end_idx + 1
            continue
        if current_label is not None and current_begin is None and s.strip():
            blocks.append((current_label, j, j))
            current_label = None
            current_begin = None
            j += 1
            continue
        j += 1
    return blocks, case_idx


def _split_args(arg_str: str) -> List[str]:
    """Split comma-separated arguments, respecting parentheses and quotes."""
    parts: List[str] = []
    buf: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    for ch in arg_str:
        if ch == "'" and not in_double:
            in_single = not in_single
            buf.append(ch)
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            buf.append(ch)
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
            elif ch == "," and depth == 0:
                parts.append("".join(buf).strip())
                buf = []
                continue
        buf.append(ch)
    if buf:
        parts.append("".join(buf).strip())
    return parts


def _unquote(s: str) -> str:
    s = s.strip()
    if len(s) >= 2 and s[0] in ('"', "'") and s[-1] == s[0]:
        return s[1:-1]
    return s


def _suffix_for_type(t: str) -> str:
    t = t.lower()
    if t in ("integer", "longint", "byte", "word", "shortint"):
        return "%"
    if t in ("real", "double"):
        return "#"
    if t in ("string", "char"):
        return "$"
    if t == "boolean":
        return "%"
    return "#"


def _parse_param_list(params: str | None) -> List[Dict[str, Any]]:
    res: List[Dict[str, Any]] = []
    if not params:
        return res
    groups = [g.strip() for g in params.split(";") if g.strip()]
    for g in groups:
        if ":" not in g:
            continue
        names_part, type_part = g.split(":", 1)
        t = type_part.strip().lower()
        suf = _suffix_for_type(t)
        byref = False
        # Detect 'var' parameters
        np = names_part.strip()
        if np.lower().startswith("var "):
            byref = True
            np = np[4:].strip()
        for nm in np.split(","):
            name = nm.strip().upper()
            if not name:
                continue
            res.append(
                {
                    "name": name,
                    "type": t,
                    "suffix": suf,
                    "byref": byref,
                }
            )
    return res


def _handle_procedure_def(
    interpreter: "Interpreter", name: str, params: List[Dict[str, str]]
):
    """Register a procedure definition and skip its body."""
    begin_idx = _find_begin_forward(interpreter, interpreter.current_line)
    if begin_idx is None:
        return "❌ Error: PROCEDURE requires BEGIN ... END block"
    end_idx = _find_end_for_begin(interpreter, begin_idx)
    if not hasattr(interpreter, "pascal_procs"):
        interpreter.pascal_procs = {}
    interpreter.pascal_procs[name] = {
        "start": begin_idx + 1,
        "end": end_idx,
        "is_func": False,
        "params": params,
    }
    interpreter.current_line = end_idx
    return ""


def _handle_function_def(
    interpreter: "Interpreter",
    name: str,
    params: List[Dict[str, str]],
    rtype: str,
):
    """
    Register a function definition, its return type,
    and skip its body.
    """
    begin_idx = _find_begin_forward(interpreter, interpreter.current_line)
    if begin_idx is None:
        return "❌ Error: FUNCTION requires BEGIN ... END block"
    end_idx = _find_end_for_begin(interpreter, begin_idx)
    if not hasattr(interpreter, "pascal_procs"):
        interpreter.pascal_procs = {}
    interpreter.pascal_procs[name] = {
        "start": begin_idx + 1,
        "end": end_idx,
        "is_func": True,
        "params": params,
    }
    # Track return type for function identifier variable
    if not hasattr(interpreter, "pascal_types"):
        interpreter.pascal_types = {}
    interpreter.pascal_types[name] = _suffix_for_type(rtype)
    interpreter.current_line = end_idx
    return ""


def _handle_proc_call(
    interpreter: "Interpreter",
    name: str,
    arg_str: str,
) -> str:
    """
    Invoke a procedure/function call with parameter binding
    and by-ref support.
    """
    if not hasattr(interpreter, "pascal_procs"):
        return ""
    info = interpreter.pascal_procs.get(name)
    if not info:
        return ""

    backups = []
    aliases: Dict[str, str] = {}
    params = info.get("params", [])
    args: List[str] = []
    if arg_str.strip():
        args = _split_args(arg_str)
    if len(args) != len(params):
        if len(params) == 0 and len(args) == 0:
            pass
        else:
            return "❌ Error: parameter count mismatch"
    for idx, p in enumerate(params):
        pname = p["name"]
        suf = p["suffix"]
        var_key = pname + suf
        # Ensure parameter type is known for assignments
        if not hasattr(interpreter, "pascal_types"):
            interpreter.pascal_types = {}
        interpreter.pascal_types[pname] = suf
        byref = bool(p.get("byref"))
        if byref:
            if idx >= len(args):
                return "❌ Error: parameter count mismatch"
            a = args[idx].strip()
            if not re.match(r"^[A-Za-z_][A-Za-z0-9_]*$", a):
                return "❌ Error: var parameter requires variable"
            target_up = a.upper()
            target_key = target_up + suf
            # Initialize local mirror but do NOT backup target (allow mutation)
            if suf == "$":
                cur: Any = interpreter.string_variables.get(target_key, "")
            else:
                cur = interpreter.get_numeric_value(target_up) or 0
            # Backup only local param if it existed
            local_existed = False
            local_old: Any | None = None
            if suf == "$":
                if var_key in interpreter.string_variables:
                    local_existed = True
                    local_old = interpreter.string_variables[var_key]
            else:
                if pname in interpreter.variables:
                    local_existed = True
                    local_old = interpreter.variables[pname]
            backups.append(
                {
                    "key": var_key,
                    "is_str": suf == "$",
                    "existed": local_existed,
                    "old": local_old,
                }
            )
            interpreter.set_typed_variable(var_key, cur)
            aliases[var_key] = target_key
        else:
            existed = False
            old_val: Any | None = None
            if suf == "$":
                if var_key in interpreter.string_variables:
                    existed = True
                    old_val = interpreter.string_variables[var_key]
            else:
                if pname in interpreter.variables:
                    existed = True
                    old_val = interpreter.variables[pname]
            backups.append(
                {
                    "key": var_key,
                    "is_str": suf == "$",
                    "existed": existed,
                    "old": old_val,
                }
            )
            val: Any = "" if suf == "$" else 0
            if idx < len(args):
                a = args[idx]
                if suf == "$":
                    val = _unquote(a)
                else:
                    try:
                        val = interpreter.evaluate_expression(a)
                    except (ValueError, TypeError, ZeroDivisionError):
                        val = 0
            interpreter.set_typed_variable(var_key, val)

    if not hasattr(interpreter, "pascal_call_stack"):
        interpreter.pascal_call_stack = []
    interpreter.pascal_call_stack.append(
        {
            "return_to": interpreter.current_line + 1,
            "end": info["end"],
            "backups": backups,
            "aliases": aliases,
        }
    )
    interpreter.current_line = info["start"] - 1
    return ""


def _handle_case_statement(interpreter: "Interpreter", expr: str) -> str:
    """Evaluate a CASE statement and position execution accordingly."""
    header_idx = interpreter.current_line
    blocks, end_idx = _parse_case_blocks(interpreter, header_idx)
    try:
        sel_val = _pascal_eval_expr(interpreter, expr)
    except (ValueError, TypeError, ZeroDivisionError):
        sel_val = 0
    target = None
    for label, bstart, bend in blocks:
        if label == "__ELSE__":
            continue
        try:
            if float(label) == float(sel_val):
                target = (bstart, bend)
                break
        except (ValueError, TypeError, ZeroDivisionError):
            if str(label) == str(sel_val):
                target = (bstart, bend)
                break
    if target is None:
        for label, bstart, bend in blocks:
            if label == "__ELSE__":
                target = (bstart, bend)
                break
    if target is None:
        interpreter.current_line = end_idx
        return ""
    start, bend = target
    single = start == bend
    interpreter.pascal_block_stack.append(
        {"type": "case", "end": bend, "case_end": end_idx, "single": single}
    )
    interpreter.current_line = start - 1
    return ""


def _handle_if_then(interpreter: "Interpreter", cond_expr: str) -> str:
    """Handle IF ... THEN [ELSE] with BEGIN...END blocks or single statements."""
    header_idx = interpreter.current_line
    begin_idx = _find_begin_forward(interpreter, header_idx)

    if begin_idx is not None:
        # Block body: BEGIN ... END
        then_start = begin_idx + 1
        then_end = _find_end_for_begin(interpreter, begin_idx)
    else:
        # Single-statement body
        stmt_idx = _find_next_statement(interpreter, header_idx)
        if stmt_idx is None:
            return "❌ Error: IF requires a statement body"
        then_start = stmt_idx
        then_end = stmt_idx

    # Check optional ELSE after then block
    j = then_end + 1
    lines = interpreter.program_lines
    while j < len(lines) and not lines[j][1].strip():
        j += 1
    else_start = None
    else_end = None
    if j < len(lines) and _ELSE_RE.match(lines[j][1]):
        else_begin_idx = _find_begin_forward(interpreter, j)
        if else_begin_idx is not None:
            else_start = else_begin_idx + 1
            else_end = _find_end_for_begin(interpreter, else_begin_idx)
        else:
            # Single-statement else
            else_stmt = _find_next_statement(interpreter, j)
            if else_stmt is not None:
                else_start = else_stmt
                else_end = else_stmt

    try:
        cond_v = _pascal_eval_expr(interpreter, cond_expr)
    except (ValueError, TypeError, ZeroDivisionError):
        cond_v = 0

    is_single_then = begin_idx is None
    # Determine the last line of the whole IF construct
    last_line = else_end if else_end is not None else then_end

    if cond_v:
        if is_single_then and else_end is not None:
            # Push a marker so that after executing then_start, we skip ELSE
            interpreter.pascal_block_stack.append(
                {
                    "type": "if_single",
                    "end": then_end,
                    "skip_to": (else_end or then_end) + 1,
                }
            )
        elif not is_single_then and else_end is not None:
            interpreter.pascal_block_stack.append(
                {
                    "type": "if",
                    "end": then_end,
                    "skip_else_to": (else_end or then_end) + 1,
                }
            )
        interpreter.current_line = then_start - 1
    else:
        if else_start is not None:
            if is_single_then or (begin_idx is None):
                # For single-statement else, no special handling needed
                # unless there was a single then body that needs skipping
                pass
            interpreter.current_line = else_start - 1
        else:
            # Skip past the entire then body (main loop won't auto-increment
            # when line_changed is True, so point one past the body)
            interpreter.current_line = last_line + 1
    return ""


def _handle_while_do(interpreter: "Interpreter", cond_expr: str) -> str:
    """Handle WHILE ... DO with BEGIN...END block or single statement."""
    header_idx = interpreter.current_line
    begin_idx = _find_begin_forward(interpreter, header_idx)
    if begin_idx is not None:
        body_start = begin_idx + 1
        body_end = _find_end_for_begin(interpreter, begin_idx)
    else:
        stmt_idx = _find_next_statement(interpreter, header_idx)
        if stmt_idx is None:
            return "❌ Error: WHILE requires a statement body"
        body_start = stmt_idx
        body_end = stmt_idx
    try:
        cond_v = _pascal_eval_expr(interpreter, cond_expr)
    except (ValueError, TypeError, ZeroDivisionError):
        cond_v = 0
    if not cond_v:
        interpreter.current_line = body_end
        return ""
    is_single = begin_idx is None
    interpreter.pascal_block_stack.append(
        {
            "type": "while",
            "start": body_start,
            "end": body_end,
            "cond": cond_expr,
            "single": is_single,
        }
    )
    interpreter.current_line = body_start - 1
    return ""


def _handle_repeat(interpreter: "Interpreter") -> str:
    """Handle REPEAT block start until its matching UNTIL line."""
    j = interpreter.current_line + 1
    while j < len(interpreter.program_lines):
        s = interpreter.program_lines[j][1]
        if _UNTIL_RE.match(s):
            break
        j += 1
    if j >= len(interpreter.program_lines):
        return "❌ Error: REPEAT requires UNTIL <cond>"
    interpreter.pascal_block_stack.append(
        {
            "type": "repeat",
            "start": interpreter.current_line + 1,
            "until": j,
        }
    )
    return ""


def _handle_until(interpreter: "Interpreter", cond_expr: str) -> str:
    """Handle UNTIL <cond> to close a REPEAT block or loop back."""
    if not getattr(interpreter, "pascal_block_stack", []):
        return ""
    top = interpreter.pascal_block_stack[-1]
    if top.get("type") != "repeat":
        return ""
    cond_expr = cond_expr.rstrip(";").strip()
    try:
        cond_v = _pascal_eval_expr(interpreter, cond_expr)
    except (ValueError, TypeError, ZeroDivisionError):
        cond_v = 0
    if cond_v:
        interpreter.pascal_block_stack.pop()
    else:
        _start = top.get("start", interpreter.current_line)
        interpreter.current_line = _start - 1
    return ""


def _handle_for(
    interpreter: "Interpreter",
    name: str,
    expr_start: str,
    dirw: str,
    expr_end: str,
    turtle: Any = None,
) -> str:
    """
    Handle FOR ... TO/DOWNTO ... DO with a required BEGIN ... END block.
    """
    up = name.upper()
    try:
        start_val = _pascal_eval_expr(interpreter, expr_start)
    except (ValueError, TypeError, ZeroDivisionError):
        start_val = 0
    try:
        limit_val = _pascal_eval_expr(interpreter, expr_end)
    except (ValueError, TypeError, ZeroDivisionError):
        limit_val = 0
    # Init variable; prefer declared type
    suf = None
    if hasattr(interpreter, "pascal_types"):
        suf = interpreter.pascal_types.get(up)
    interpreter.set_typed_variable(up + (suf or "#"), start_val)
    begin_idx = _find_begin_forward(interpreter, interpreter.current_line)
    if begin_idx is not None:
        end_idx = _find_end_for_begin(interpreter, begin_idx)
        body_start = begin_idx + 1
        body_end = end_idx
    else:
        # Single-statement body (no BEGIN ... END)
        stmt_idx = _find_next_statement(interpreter, interpreter.current_line)
        if stmt_idx is None:
            return "❌ Error: FOR requires a statement body"
        body_start = stmt_idx
        body_end = stmt_idx  # single line
    step = 1 if dirw.lower() == "to" else -1
    # Check initial condition
    cur = interpreter.get_numeric_value(up) or 0
    ok = cur <= limit_val if step > 0 else cur >= limit_val
    if not ok:
        if begin_idx is None:
            interpreter.current_line = body_end + 1
        else:
            interpreter.current_line = body_end
        return ""

    is_single = begin_idx is None

    if is_single:
        # Execute single-statement body inline to avoid end-of-program issues
        stmt_text = interpreter.program_lines[body_start][1]
        results: List[str] = []
        loop_count = 0
        while ok and loop_count < 100000:
            result = execute_pascal(interpreter, stmt_text, turtle)
            if result:
                results.append(result)
            cur += step
            interpreter.set_typed_variable(up + (suf or "#"), cur)
            ok = cur <= limit_val if step > 0 else cur >= limit_val
            loop_count += 1
        interpreter.current_line = body_end + 1
        return "".join(results)

    interpreter.pascal_block_stack.append(
        {
            "type": "for",
            "start": body_start,
            "end": body_end,
            "var": up,
            "limit": limit_val,
            "step": step,
            "single": False,
        }
    )
    interpreter.current_line = body_start - 1
    return ""


def execute_pascal(interpreter: "Interpreter", command: str, turtle) -> str:
    """Execute Pascal language command."""
    # Track multi-line block comments {  ...  } and (*  ...  *)
    if not hasattr(interpreter, "_pascal_in_block_comment"):
        interpreter._pascal_in_block_comment = False

    raw = command.strip()

    # If currently inside a block comment, look for the closing delimiter
    if interpreter._pascal_in_block_comment:
        if "}" in raw or "*)" in raw:
            interpreter._pascal_in_block_comment = False
        return ""

    # Check if this line opens a block comment that spans multiple lines
    # (i.e., has an opening delimiter but no matching closing delimiter on same line)
    has_brace_open = "{" in raw
    has_brace_close = "}" in raw
    has_paren_open = "(*" in raw
    has_paren_close = "*)" in raw
    if (has_brace_open and not has_brace_close) or (
        has_paren_open and not has_paren_close
    ):
        interpreter._pascal_in_block_comment = True
        return ""

    # Strip inline Pascal comments {...} and (* ... *) for control-flow parsing
    cmd = re.sub(r"\{.*?\}|\(\*.*?\*\)", "", command).strip()
    if not cmd:
        return ""

    # Handle inline CASE label bodies: "2: writeln('two');" → execute body
    m_case_inline = _CASE_LABEL_INLINE_RE.match(cmd)
    if m_case_inline and getattr(interpreter, "pascal_block_stack", []):
        top = interpreter.pascal_block_stack[-1]
        if top.get("type") == "case":
            body = m_case_inline.group(2).strip()
            return execute_pascal(interpreter, body, turtle)

    _ensure_pascal_stack(interpreter)

    # Handle end of single-line CASE branch by skipping to
    # CASE end on next line
    if getattr(interpreter, "pascal_block_stack", []):
        top = interpreter.pascal_block_stack[-1]
        if (
            top.get("type") == "case"
            and top.get("single", False)
            and interpreter.current_line == top.get("end", -1) + 1
        ):
            _case_end = top.get("case_end", interpreter.current_line)
            interpreter.current_line = _case_end
            interpreter.pascal_block_stack.pop()
            return ""

    # Handle end of single-statement IF/FOR/WHILE body
    if getattr(interpreter, "pascal_block_stack", []):
        top = interpreter.pascal_block_stack[-1]
        ttype = top.get("type", "")
        # After the single body line, check if we've passed "end"
        if ttype == "if_single" and interpreter.current_line > top.get("end", -1):
            skip_to = top.get("skip_to", interpreter.current_line)
            interpreter.pascal_block_stack.pop()
            interpreter.current_line = skip_to - 1
            return ""
        if (
            ttype == "for"
            and top.get("single", False)
            and interpreter.current_line > top.get("end", -1)
        ):
            # Increment and check loop variable
            var_name = top["var"]
            step = top["step"]
            limit = top["limit"]
            cur = (interpreter.get_numeric_value(var_name) or 0) + step
            suf = None
            if hasattr(interpreter, "pascal_types"):
                suf = interpreter.pascal_types.get(var_name)
            interpreter.set_typed_variable(var_name + (suf or "#"), cur)
            ok = cur <= limit if step > 0 else cur >= limit
            if ok:
                interpreter.current_line = top["start"] - 1
            else:
                interpreter.pascal_block_stack.pop()
            return ""
        if (
            ttype == "while"
            and top.get("single", False)
            and interpreter.current_line > top.get("end", -1)
        ):
            try:
                cond_v = _pascal_eval_expr(interpreter, top.get("cond", "0"))
            except (ValueError, TypeError, ZeroDivisionError):
                cond_v = 0
            if cond_v:
                interpreter.current_line = top["start"] - 1
            else:
                interpreter.pascal_block_stack.pop()
            return ""

    # Handle END of control-flow blocks
    if _is_end(cmd) and getattr(interpreter, "pascal_block_stack", []):
        top = interpreter.pascal_block_stack[-1]
        if top.get("end") == interpreter.current_line:
            if top.get("type") == "while":
                try:
                    _cond = top.get("cond", "0")
                    cond_v = _pascal_eval_expr(interpreter, _cond)
                except (ValueError, TypeError, ZeroDivisionError):
                    cond_v = 0
                if cond_v:
                    _start = int(top.get("start") or interpreter.current_line)
                    interpreter.current_line = _start - 1
                else:
                    interpreter.pascal_block_stack.pop()
                    return ""
            elif top.get("type") == "if" and top.get("skip_else_to") is not None:
                _skip_to = top.get("skip_else_to", 0)
                interpreter.current_line = int(_skip_to) - 1
                interpreter.pascal_block_stack.pop()
            elif top.get("type") == "for":
                # increment/decrement and re-check
                var = top.get("var")
                if not isinstance(var, str):
                    interpreter.pascal_block_stack.pop()
                    return ""
                step = top.get("step", 1)
                cur = interpreter.get_numeric_value(var) or 0
                interpreter.set_typed_variable(var, cur + step)
                # Check limit
                val: Any = interpreter.get_numeric_value(var) or 0
                limit = top.get("limit", 0)
                ok = val <= limit if step > 0 else val >= limit
                if ok:
                    _start = int(top.get("start") or interpreter.current_line)
                    interpreter.current_line = _start - 1
                else:
                    interpreter.pascal_block_stack.pop()
            elif top.get("type") == "case":
                # After finishing selected branch, skip to overall CASE end
                _case_end = top.get("case_end", interpreter.current_line)
                interpreter.current_line = _case_end
                interpreter.pascal_block_stack.pop()
        return ""

    # Procedure/function return: when reaching the end index
    # of a called proc, return
    if hasattr(interpreter, "pascal_call_stack") and interpreter.pascal_call_stack:
        frame = interpreter.pascal_call_stack[-1]
        if interpreter.current_line == frame.get("end"):
            backups = frame.get("backups") or []
            # Skip restoring values for by-ref parameters
            # (allow mutation to persist)
            byref_aliases = set(frame.get("aliases", {}).values())
            for b in backups:
                key = b["key"]
                if key in byref_aliases:
                    continue
                if b["is_str"]:
                    if b["existed"]:
                        interpreter.string_variables[key] = b["old"]
                    else:
                        interpreter.string_variables.pop(key, None)
                else:
                    varname = key[:-1] if key.endswith(("%", "#")) else key
                    if b["existed"]:
                        interpreter.variables[varname] = b["old"]
                    else:
                        interpreter.variables.pop(varname, None)
            interpreter.pascal_call_stack.pop()
            _return_to = frame.get("return_to", interpreter.current_line)
            interpreter.current_line = _return_to
            return ""

    # Procedure/function definitions (with optional params)
    m = _PROC_RE.match(cmd)
    if m:
        name = m.group(1).upper()
        params = _parse_param_list(m.group(2))
        return _handle_procedure_def(interpreter, name, params)

    m = _FUNC_RE.match(cmd)
    if m:
        name = m.group(1).upper()
        params = _parse_param_list(m.group(2))
        rtype = (m.group(3) or "real").lower()
        return _handle_function_def(interpreter, name, params, rtype)

    # Procedure/function calls: NAME; or NAME(args);
    m = _CALL_RE.match(cmd)
    if m and hasattr(interpreter, "pascal_procs"):
        name = m.group(1).upper()
        # Only treat as a procedure/function call if it's a known proc name;
        # otherwise allow other handlers (e.g., WRITE/WRITELN) to process.
        if name in interpreter.pascal_procs:
            arg_str = m.group(2) or ""
            return _handle_proc_call(interpreter, name, arg_str)

    upcmd = cmd.upper().rstrip(";").strip()
    if upcmd in ("BEGIN", "END", "END.", "PROGRAM", "USES", "VAR", "TYPE", "ELSE"):
        return ""
    if upcmd.startswith("USES ") or upcmd == "USES":
        # Parse the units list and activate stubs for known units
        units_str = upcmd[4:].strip().rstrip(";")
        units = [u.strip() for u in units_str.split(",") if u.strip()]
        if not hasattr(interpreter, "pascal_units"):
            interpreter.pascal_units = set()  # type: ignore[attr-defined]
        for unit in units:
            interpreter.pascal_units.add(unit)
        return ""
    if upcmd.startswith("PROGRAM "):
        return ""

    # IF ... THEN begin ... end [ELSE begin ... end]
    m = _IF_THEN_RE.match(cmd)
    if m:
        cond_expr = m.group(1).strip()
        return _handle_if_then(interpreter, cond_expr)

    # WHILE ... DO begin ... end
    m = _WHILE_DO_RE.match(cmd)
    if m:
        cond_expr = m.group(1).strip()
        return _handle_while_do(interpreter, cond_expr)

    # REPEAT ... UNTIL <cond>
    if _REPEAT_RE.match(cmd):
        return _handle_repeat(interpreter)

    # CASE <expr> OF ... END
    m = _CASE_RE.match(cmd)
    if m:
        expr = m.group(1).strip()
        return _handle_case_statement(interpreter, expr)

    m = _UNTIL_RE.match(cmd)
    if m and getattr(interpreter, "pascal_block_stack", []):
        return _handle_until(interpreter, m.group(1).strip())

    # FOR i := a TO/DOWNTO b DO begin ... end
    m = _FOR_RE.match(cmd)
    if m:
        name, expr_start, dirw, expr_end = m.groups()
        return _handle_for(interpreter, name, expr_start, dirw, expr_end, turtle)

    if _VAR_KEYWORD_RE.match(cmd):
        return ""

    # CRT / System Commands
    if _CLRSCR_RE.match(cmd):
        interpreter.output.append("CLS")
        return ""

    if _RANDOMIZE_RE.match(cmd):
        import random

        random.seed()
        return ""

    m = _DELAY_RE.match(cmd)
    if m:
        try:
            ms = int(interpreter.evaluate_expression(m.group(1)))
            import time

            time.sleep(ms / 1000.0)
        except Exception:  # noqa: BLE001  # pylint: disable=broad-exception-caught
            pass
        return ""

    m = _GOTOXY_RE.match(cmd)
    if m:
        try:
            x = int(interpreter.evaluate_expression(m.group(1).strip()))
            y = int(interpreter.evaluate_expression(m.group(2).strip()))
            interpreter.cursor_col = x
            interpreter.cursor_row = y
        except (ValueError, TypeError):
            pass
        return ""

    m = _TEXTCOLOR_RE.match(cmd)
    if m:
        # TextColor sets the console text color (informational in this context)
        try:
            color_val = int(interpreter.evaluate_expression(m.group(1).strip()))
            if turtle:
                color_map = {
                    0: "BLACK",
                    1: "BLUE",
                    2: "GREEN",
                    3: "CYAN",
                    4: "RED",
                    5: "MAGENTA",
                    6: "BROWN",
                    7: "GRAY",
                    8: "GRAY",
                    9: "BLUE",
                    10: "GREEN",
                    11: "CYAN",
                    12: "RED",
                    13: "MAGENTA",
                    14: "YELLOW",
                    15: "WHITE",
                }
                color_name = color_map.get(color_val, "WHITE")
                turtle.pencolor(color_name)
        except (ValueError, TypeError):
            pass
        return ""

    # Array Declaration
    m = _VAR_ARRAY_RE.match(cmd)
    if m:
        names, start_idx, end_idx, t = m.groups()
        start_idx = int(start_idx)
        end_idx = int(end_idx)
        size = end_idx + 1
        if size <= 0:
            return "❌ Error: Invalid array size"

        suf: str | None = _suffix_for_type(t)
        default_val = 0.0

        for raw in names.split(","):
            name = raw.strip().upper()
            interpreter.arrays[name] = [default_val] * size
        return ""

    # 2D array or complex array declarations — create a flat array as best effort
    m = _VAR_ARRAY_2D_RE.match(cmd)
    if m:
        names = m.group(1)
        for raw in names.split(","):
            name = raw.strip().upper()
            interpreter.arrays[name] = [0.0] * 100  # default flat array
        return ""

    # Type definitions — parse RECORD, ignore others
    if _TYPE_DEF_RE.match(cmd):
        m_td = _TYPE_DEF_RE.match(cmd)
        if m_td and m_td.group(2).upper() == "RECORD":
            rec_name = m_td.group(1).upper()
            # Scan ahead for field declarations until END
            if not hasattr(interpreter, "pascal_record_types"):
                interpreter.pascal_record_types = {}
            fields = {}
            scan_line = interpreter.current_line + 1
            while scan_line < len(interpreter.program_lines):
                _, scan_cmd = interpreter.program_lines[scan_line]
                sc = scan_cmd.strip().upper().rstrip(";")
                if (
                    sc in ("END", "END.", "END;")
                    or sc.startswith("END{")
                    or sc == "END"
                ):
                    interpreter.current_line = scan_line
                    break
                # Field declaration: name1, name2: Type;
                fm = re.match(
                    r"^\s*([A-Za-z_][A-Za-z0-9_,\s]*)\s*:\s*([A-Za-z_][A-Za-z0-9_]*)\s*;?\s*$",
                    scan_cmd,
                    re.IGNORECASE,
                )
                if fm:
                    fnames = [
                        n.strip().upper() for n in fm.group(1).split(",") if n.strip()
                    ]
                    ftype = fm.group(2).upper()
                    for fn in fnames:
                        fields[fn] = ftype
                scan_line += 1
            interpreter.pascal_record_types[rec_name] = fields
        return ""

    # Const Declaration
    if _CONST_RE.match(cmd):
        return ""

    # Type definitions: Name = array/record/... — treated as no-op (second guard)
    if _TYPE_DEF_RE.match(cmd):
        return ""

    m = _CONST_DEF_RE.match(cmd)
    if m:
        name, val_str = m.groups()
        name = name.upper()
        val_str = val_str.rstrip(";").strip()
        try:
            if val_str.startswith("'") or val_str.startswith('"'):
                val = _unquote(val_str)
                interpreter.string_variables[name + "$"] = val
            else:
                val = float(val_str)
                interpreter.variables[name] = val
        except Exception:  # noqa: BLE001  # pylint: disable=broad-exception-caught
            pass
        return ""

    # Array Assignment
    m = _ASSIGN_ARRAY_RE.match(cmd)
    if m:
        name, idx_expr, val_expr = m.groups()
        name = name.upper()
        idx_expr = idx_expr.replace("[", "(").replace("]", ")")
        try:
            idx = int(interpreter.evaluate_expression(idx_expr))
        except Exception:  # noqa: BLE001  # pylint: disable=broad-exception-caught
            return "❌ Error: Invalid array index"

        val_expr = val_expr.rstrip(";").strip()
        val_expr = val_expr.replace("[", "(").replace("]", ")")

        if name not in interpreter.arrays:
            return "❌ Error: Array not declared"

        arr = interpreter.arrays[name]
        if idx < 0 or idx >= len(arr):
            return "❌ Error: Array index out of bounds"

        try:
            val = interpreter.evaluate_expression(val_expr)
        except Exception:  # noqa: BLE001  # pylint: disable=broad-exception-caught
            val = 0.0

        arr[idx] = val
        return ""

    # Known-type variable declarations MUST come before custom-type catch-all
    m = _VAR_RE.match(cmd)
    if m:
        names, t = m.groups()
        suf = _suffix_for_type(t)
        for raw in names.split(","):
            name = raw.strip().upper()
            init_val = "" if suf == "$" else 0
            interpreter.set_typed_variable(name + suf, init_val)
            # Track Pascal declared type for input
            if not hasattr(interpreter, "pascal_types"):
                interpreter.pascal_types = {}
            interpreter.pascal_types[name] = suf
        return ""

    # Custom-type variable declarations — default to real (must come AFTER _VAR_RE)
    m = _VAR_CUSTOM_TYPE_RE.match(cmd)
    if m:
        names = m.group(1)
        type_name = m.group(2).upper() if m.group(2) else ""
        for raw in names.split(","):
            name = raw.strip().upper()
            # If type is a known record type, create a dict-based struct
            if (
                hasattr(interpreter, "pascal_record_types")
                and type_name in interpreter.pascal_record_types
            ):
                fields = interpreter.pascal_record_types[type_name]
                rec = {}
                for fname, ftype in fields.items():
                    ftype_up = ftype.upper()
                    if ftype_up in ("STRING", "CHAR"):
                        rec[fname] = ""
                    elif ftype_up in ("BOOLEAN"):
                        rec[fname] = False
                    else:
                        rec[fname] = 0.0
                interpreter.variables[name] = rec
                if not hasattr(interpreter, "pascal_types"):
                    interpreter.pascal_types = {}
                interpreter.pascal_types[name] = "#RECORD#"
            else:
                interpreter.variables[name] = 0.0
        return ""

    m = _ASSIGN_FIELD_RE.match(cmd)
    if m:
        var_name = m.group(1).upper()
        field_name = m.group(2).upper()
        expr = m.group(3).rstrip(";").strip()
        rec = interpreter.variables.get(var_name)
        if isinstance(rec, dict):
            try:
                val = _pascal_eval_expr(interpreter, expr)
            except (ValueError, TypeError, ZeroDivisionError):
                val = 0
            rec[field_name] = val
        return ""

    m = _ASSIGN_RE.match(cmd)
    if m:
        name, expr = m.groups()
        expr = expr.rstrip(";").strip()
        # Replace [] with () for array access in expression
        expr = expr.replace("[", "(").replace("]", ")")
        up = name.upper()
        suf = None
        if hasattr(interpreter, "pascal_types"):
            suf = interpreter.pascal_types.get(up)
        try:
            if suf == "$":
                # Could be a string function call result
                result = _pascal_eval_expr(interpreter, expr)
                if isinstance(result, str):
                    val = result
                else:
                    val = _unquote(expr)
            else:
                val = _pascal_eval_expr(interpreter, expr)
        except (ValueError, TypeError, ZeroDivisionError):
            val = "" if suf == "$" else 0
        # Handle by-reference parameter aliasing
        var_key = up + (suf or "#")
        if hasattr(interpreter, "pascal_call_stack") and interpreter.pascal_call_stack:
            top = interpreter.pascal_call_stack[-1]
            aliases = top.get("aliases", {})
            target_key = aliases.get(var_key)
            if not target_key:
                # Fallback: match by base name if suffix mapping not present
                for k, v in aliases.items():
                    if k.startswith(up):
                        var_key = k
                        target_key = v
                        break
            if target_key:
                interpreter.set_typed_variable(target_key, val)
                # Keep local param mirror updated for expressions
                interpreter.set_typed_variable(var_key, val)
                return ""
        if suf:
            interpreter.set_typed_variable(var_key, val)
        else:
            interpreter.set_typed_variable(var_key, val)
        return ""

    m = _WRITE_RE.match(cmd)
    if m:
        fn, arglist = m.groups()
        args = _split_args(arglist)
        out_parts: List[str] = []
        for a in args:
            if not a:
                continue
            if a.strip().startswith('"') or a.strip().startswith("'"):
                out_parts.append(_unquote(a))
            else:
                up = a.strip().upper()
                if up + "$" in interpreter.string_variables:
                    out_parts.append(interpreter.string_variables[up + "$"])
                elif up in interpreter.variables:
                    v = interpreter.variables[up]
                    if isinstance(v, float) and v == int(v):
                        out_parts.append(str(int(v)))
                    else:
                        out_parts.append(f"{v:g}")
                else:
                    try:
                        expr_fixed = a.replace("[", "(").replace("]", ")")
                        _val = _pascal_eval_expr(interpreter, expr_fixed)
                        if isinstance(_val, str):
                            out_parts.append(_val)
                        elif isinstance(_val, float) and _val == int(_val):
                            out_parts.append(str(int(_val)))
                        else:
                            out_parts.append(
                                f"{_val:g}"
                                if isinstance(_val, (int, float))
                                else str(_val)
                            )
                    except (
                        ValueError,
                        TypeError,
                        ZeroDivisionError,
                    ):  # noqa: BLE001
                        out_parts.append("0")
        text = "".join(out_parts)
        if fn.lower() == "writeln":
            text += "\n"
        return text

    # Bare WriteLn; / Write; (no parentheses)
    m = _BARE_WRITELN_RE.match(cmd)
    if m:
        fn = m.group(1)
        if fn.lower() == "writeln":
            return "\n"
        return ""

    m = _READLN_RE.match(cmd)
    if m:
        arg = m.group(1).strip()
        if not arg:
            return ""
        name = arg.upper()
        suf = None
        if hasattr(interpreter, "pascal_types"):
            suf = interpreter.pascal_types.get(name)
        if suf is None:
            suf = "#"
        interpreter.start_input_request(
            "",
            name + suf,
            is_numeric=(suf != "$"),
        )
        return ""

    # Comments
    if cmd.startswith("//") or cmd.startswith("(*") or cmd.startswith("{"):
        return ""

    # ── Built-in procedures ──────────────────────────────────────────────────
    upcmd_strip = upcmd.rstrip(";").strip()

    # Inc(var [, n])  /  Dec(var [, n])
    m = re.match(r"^(INC|DEC)\s*\((.+)\)\s*$", upcmd_strip, re.IGNORECASE)
    if m:
        op = m.group(1).upper()
        inner = _split_args(m.group(2))
        vname = inner[0].strip().upper()
        step = 1
        if len(inner) > 1:
            try:
                step = int(float(interpreter.evaluate_expression(inner[1].strip())))
            except Exception:
                step = 1
        cur = interpreter.get_numeric_value(vname) or 0
        interpreter.set_typed_variable(vname, cur + (step if op == "INC" else -step))
        return ""

    # Str(val, s)
    m = re.match(
        r"^STR\s*\((.+),\s*([A-Za-z_][A-Za-z0-9_]*)\)\s*$", upcmd_strip, re.IGNORECASE
    )
    if m:
        try:
            v = interpreter.evaluate_expression(m.group(1).strip())
            s = f"{v:g}" if isinstance(v, float) else str(int(v))
        except Exception:
            s = "0"
        interpreter.string_variables[m.group(2).upper() + "$"] = s
        return ""

    # Val(s, var, code)
    m = re.match(
        r"^VAL\s*\((.+),\s*([A-Za-z_][A-Za-z0-9_]*),\s*([A-Za-z_][A-Za-z0-9_]*)\)\s*$",
        upcmd_strip,
        re.IGNORECASE,
    )
    if m:
        s_expr, num_var, code_var = (
            m.group(1).strip(),
            m.group(2).upper(),
            m.group(3).upper(),
        )
        s = (
            _unquote(s_expr).strip()
            if s_expr.startswith(("'", '"'))
            else str(interpreter.string_variables.get(s_expr.upper() + "$", s_expr))
        )
        try:
            v = float(s.strip())
            interpreter.set_typed_variable(num_var, v)
            interpreter.set_typed_variable(code_var, 0)
        except ValueError:
            interpreter.set_typed_variable(code_var, 1)
        return ""

    # Insert(sub, dest, pos)
    m = re.match(
        r"^INSERT\s*\((.+),\s*([A-Za-z_][A-Za-z0-9_]*),\s*(.+)\)\s*$",
        upcmd_strip,
        re.IGNORECASE,
    )
    if m:
        sub = (
            _unquote(m.group(1).strip())
            if m.group(1).strip().startswith(("'", '"'))
            else str(
                interpreter.string_variables.get(
                    m.group(1).strip().upper() + "$", m.group(1).strip()
                )
            )
        )
        dest_var = m.group(2).upper()
        try:
            pos = int(interpreter.evaluate_expression(m.group(3).strip())) - 1
        except Exception:
            pos = 0
        dest = interpreter.string_variables.get(dest_var + "$", "")
        interpreter.string_variables[dest_var + "$"] = dest[:pos] + sub + dest[pos:]
        return ""

    # Delete(s, pos, len)
    m = re.match(
        r"^DELETE\s*\(([A-Za-z_][A-Za-z0-9_]*),\s*(.+),\s*(.+)\)\s*$",
        upcmd_strip,
        re.IGNORECASE,
    )
    if m:
        vname, pos_e, len_e = m.group(1).upper(), m.group(2).strip(), m.group(3).strip()
        try:
            pos = int(interpreter.evaluate_expression(pos_e)) - 1
            ln = int(interpreter.evaluate_expression(len_e))
        except Exception:
            return ""
        s = interpreter.string_variables.get(vname + "$", "")
        interpreter.string_variables[vname + "$"] = s[:pos] + s[pos + ln :]
        return ""

    # SetLength(s, n)
    m = re.match(
        r"^SETLENGTH\s*\(([A-Za-z_][A-Za-z0-9_]*),\s*(.+)\)\s*$",
        upcmd_strip,
        re.IGNORECASE,
    )
    if m:
        vname = m.group(1).upper()
        try:
            n = int(interpreter.evaluate_expression(m.group(2).strip()))
        except Exception:
            return ""
        s = interpreter.string_variables.get(vname + "$", "")
        if n > len(s):
            interpreter.string_variables[vname + "$"] = s.ljust(n)
        else:
            interpreter.string_variables[vname + "$"] = s[:n]
        return ""

    # Exit / Halt
    if upcmd_strip in ("EXIT", "HALT") or re.match(r"^HALT\s*\(.+\)$", upcmd_strip):
        raise StopIteration

    # AssignFile/Assign
    m = re.match(
        r"^(?:ASSIGNFILE|ASSIGN)\s*\(([A-Za-z_][A-Za-z0-9_]*),\s*(.+)\)\s*$",
        upcmd_strip,
        re.IGNORECASE,
    )
    if m:
        fvar, fname_expr = m.group(1).upper(), m.group(2).strip()
        fname = (
            _unquote(fname_expr)
            if fname_expr.startswith(("'", '"'))
            else str(
                interpreter.string_variables.get(fname_expr.upper() + "$", fname_expr)
            )
        )
        if not hasattr(interpreter, "_pascal_files"):
            interpreter._pascal_files = {}
        interpreter._pascal_files[fvar] = {"name": fname, "handle": None, "mode": None}
        return ""

    # Reset(f) / Reset(f, recsize)
    m = re.match(r"^RESET\s*\(([A-Za-z_][A-Za-z0-9_]*)[^)]*\)\s*$", upcmd_strip)
    if m:
        fvar = m.group(1).upper()
        pf = getattr(interpreter, "_pascal_files", {}).get(fvar)
        if pf:
            try:
                pf["handle"] = open(pf["name"], "r")
                pf["mode"] = "r"
            except OSError as e:
                return f"❌ {e}"
        return ""

    # Rewrite(f)
    m = re.match(r"^REWRITE\s*\(([A-Za-z_][A-Za-z0-9_]*)\)\s*$", upcmd_strip)
    if m:
        fvar = m.group(1).upper()
        pf = getattr(interpreter, "_pascal_files", {}).get(fvar)
        if pf:
            try:
                pf["handle"] = open(pf["name"], "w")
                pf["mode"] = "w"
            except OSError as e:
                return f"❌ {e}"
        return ""

    # Append(f)
    m = re.match(r"^APPEND\s*\(([A-Za-z_][A-Za-z0-9_]*)\)\s*$", upcmd_strip)
    if m:
        fvar = m.group(1).upper()
        pf = getattr(interpreter, "_pascal_files", {}).get(fvar)
        if pf:
            try:
                pf["handle"] = open(pf["name"], "a")
                pf["mode"] = "a"
            except OSError as e:
                return f"❌ {e}"
        return ""

    # CloseFile/Close(f)
    m = re.match(
        r"^(?:CLOSEFILE|CLOSE)\s*\(([A-Za-z_][A-Za-z0-9_]*)\)\s*$", upcmd_strip
    )
    if m:
        fvar = m.group(1).upper()
        pf = getattr(interpreter, "_pascal_files", {}).get(fvar)
        if pf and pf.get("handle"):
            try:
                pf["handle"].close()
            except Exception:
                pass
            pf["handle"] = None
        return ""

    # New(p) / Dispose(p) — stub (no pointer arithmetic)
    m = re.match(r"^(?:NEW|DISPOSE)\s*\(([A-Za-z_][A-Za-z0-9_]*)\)\s*$", upcmd_strip)
    if m:
        return ""

    # FillChar(dest, count, val)
    m = re.match(
        r"^FILLCHAR\s*\(([A-Za-z_][A-Za-z0-9_]*),\s*(.+),\s*(.+)\)\s*$",
        upcmd_strip,
        re.IGNORECASE,
    )
    if m:
        vname = m.group(1).upper()
        try:
            count = int(interpreter.evaluate_expression(m.group(2).strip()))
            val_e = m.group(3).strip()
            char = (
                chr(int(interpreter.evaluate_expression(val_e)))
                if not val_e.startswith(("'", '"'))
                else _unquote(val_e)[0]
            )
        except Exception:
            count, char = 0, "\x00"
        interpreter.string_variables[vname + "$"] = char * count
        return ""

    # ── Bare built-in function call used as statement (e.g. SetLength, Str)
    # already handled; catch remaining known identifiers
    m = re.match(
        r"^([A-Za-z_][A-Za-z0-9_]*)\s*(?:\((.*)?\))?\s*;?\s*$",
        upcmd_strip,
        re.IGNORECASE,
    )
    if m:
        fname = m.group(1).upper()
        fargs = m.group(2) or ""
        # Writeln/Write to file: Write(f, ...)
        if fname in ("WRITE", "WRITELN") and fargs:
            args = _split_args(fargs)
            if args and args[0].upper() in getattr(interpreter, "_pascal_files", {}):
                fvar = args[0].upper()
                pf = interpreter._pascal_files[fvar]
                parts = []
                for a in args[1:]:
                    a = a.strip()
                    if a.startswith(("'", '"')):
                        parts.append(_unquote(a))
                    else:
                        up = a.upper()
                        if up + "$" in interpreter.string_variables:
                            parts.append(interpreter.string_variables[up + "$"])
                        elif up in interpreter.variables:
                            parts.append(f"{interpreter.variables[up]:g}")
                        else:
                            try:
                                parts.append(str(interpreter.evaluate_expression(a)))
                            except Exception:
                                parts.append("")
                text = "".join(parts)
                if fname == "WRITELN":
                    text += "\n"
                if pf and pf.get("handle"):
                    pf["handle"].write(text)
                else:
                    return text
                return ""

    return f"❌ Error: Unknown Pascal command '{command.strip()}'"
