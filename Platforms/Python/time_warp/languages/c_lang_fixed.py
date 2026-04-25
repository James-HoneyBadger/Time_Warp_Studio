"""
Minimal C-like executor (Turbo C era flavor)

Supported subset:
- int/long/float/double declarations with optional initializer
- assignment with '=' and expression evaluation
- printf("...", args...);
- scanf("%d"|"%f"|"%s", &var);
- if/else with braces, while loops
- for(init; cond; post) { ... } implemented via while+post
- do { ... } while (cond);

Notes:
- Variable names are normalized to uppercase internally.
- Types are tracked via BASIC-typed helpers using suffixes internally.
"""

from __future__ import annotations

import math as _cmath
import random
import random as _crand
import re
import time as _ctime
from typing import TYPE_CHECKING, Any, Dict, List

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

_DECL_RE = re.compile(
    r"^\s*(int|long|float|double|char)\s+"
    r"([A-Za-z_][A-Za-z0-9_]*)"
    r"(\s*=\s*[^;]+)?\s*;?\s*$",
)
_ARRAY_DECL_RE = re.compile(
    r"^\s*(int|long|float|double|char)\s+"
    r"([A-Za-z_][A-Za-z0-9_]*)\s*\[(\d+)\]"
    r"(?:\s*=\s*\{([^}]*)\})?\s*;?\s*$",
)
_ASSIGN_RE = re.compile(r"^\s*([A-Za-z_][A-Za-z0-9_]*(?:\[[^\]]*\])?)\s*=\s*(.+);?\s*$")
_PRINTF_RE = re.compile(r"^\s*printf\s*\((.*)\)\s*;?\s*$", re.IGNORECASE)
_SCANF_RE = re.compile(r"^\s*scanf\s*\((.*)\)\s*;?\s*$", re.IGNORECASE)
_IF_RE = re.compile(r"^\s*if\s*\((.*)\)\s*\{?\s*$", re.IGNORECASE)
_ELSE_RE = re.compile(r"^\s*else\s*\{?\s*$", re.IGNORECASE)
_ELSE_ON_SAME_LINE_RE = re.compile(r".*\}\s*else\b.*", re.IGNORECASE)
_WHILE_RE = re.compile(r"^\s*while\s*\((.*)\)\s*\{?\s*$", re.IGNORECASE)
_FOR_RE = re.compile(
    r"^\s*for\s*\(([^;]*);([^;]*);([^)]*)\)\s*(.*)$",
    re.IGNORECASE,
)
_DO_RE = re.compile(r"^\s*do\s*\{?\s*$", re.IGNORECASE)
_WHILE_AFTER_DO_RE = re.compile(
    r"^\s*while\s*\((.*)\)\s*;?\s*$",
    re.IGNORECASE,
)
_CLOSE_BRACE_WITH_TRAILING_WHILE_RE = re.compile(
    r"^\s*\}\s*while\s*\((.*)\)\s*;?\s*$", re.IGNORECASE
)
_SWITCH_RE = re.compile(r"^\s*switch\s*\((.*)\)\s*\{?\s*$", re.IGNORECASE)
_CASE_RE = re.compile(r"^\s*case\s+(.+?)\s*:\s*(.*)?$", re.IGNORECASE)
_DEFAULT_RE = re.compile(r"^\s*default\s*:\s*(.*)?$", re.IGNORECASE)
_STRUCT_DEF_RE = re.compile(
    r"^\s*(?:typedef\s+)?struct\s+([A-Za-z_][A-Za-z0-9_]*)\s*\{\s*$", re.IGNORECASE
)
_STRUCT_END_RE = re.compile(
    r"^\s*\}\s*([A-Za-z_][A-Za-z0-9_]*)?\s*;?\s*$", re.IGNORECASE
)
_STRUCT_VAR_RE = re.compile(
    r"^\s*(?:struct\s+)?([A-Za-z_][A-Za-z0-9_]*)\s+([A-Za-z_][A-Za-z0-9_]*)\s*;?\s*$",
    re.IGNORECASE,
)
_TYPEDEF_RE = re.compile(
    r"^\s*typedef\s+(int|long|float|double|char|unsigned)\s+([A-Za-z_][A-Za-z0-9_]*)\s*;?\s*$",
    re.IGNORECASE,
)
_STRUCT_FIELD_ASSIGN_RE = re.compile(
    r"^\s*([A-Za-z_][A-Za-z0-9_]*)\.(\w+)\s*=\s*(.+);?\s*$"
)


def _split_case_stmts(text: str) -> List[str]:
    """Split semicolon-separated statements, respecting strings and parens."""
    parts: List[str] = []
    buf: List[str] = []
    in_str = False
    esc = False
    paren = 0
    for ch in text:
        if esc:
            buf.append(ch)
            esc = False
            continue
        if ch == "\\":
            buf.append(ch)
            esc = True
            continue
        if ch == '"':
            in_str = not in_str
            buf.append(ch)
            continue
        if in_str:
            buf.append(ch)
            continue
        if ch == "(":
            paren += 1
        elif ch == ")":
            paren -= 1
        if ch == ";" and paren == 0:
            s = "".join(buf).strip()
            if s:
                parts.append(s)
            buf = []
            continue
        buf.append(ch)
    s = "".join(buf).strip()
    if s:
        parts.append(s)
    return parts


def _split_args(arg_str: str) -> List[str]:
    parts: List[str] = []
    buf = []
    in_str = False
    esc = False
    paren_depth = 0
    for ch in arg_str:
        if in_str:
            buf.append(ch)
            if esc:
                esc = False
            elif ch == "\\":
                esc = True
            elif ch == '"':
                in_str = False
        else:
            if ch == "," and paren_depth == 0:
                parts.append("".join(buf).strip())
                buf = []
            else:
                buf.append(ch)
                if ch == '"':
                    in_str = True
                elif ch == "(":
                    paren_depth += 1
                elif ch == ")":
                    paren_depth = max(0, paren_depth - 1)
    if buf:
        parts.append("".join(buf).strip())
    return parts


def _unquote(s: str) -> str:
    s = s.strip()
    if len(s) >= 2 and s[0] == '"' and s[-1] == '"':
        return s[1:-1]
    if len(s) >= 2 and s[0] == "'" and s[-1] == "'":
        return s[1:-1]
    return s


def _suffix_for_type(t: str) -> str:
    t = t.lower()
    if t == "int":
        return "%"
    if t == "long":
        return "&"
    if t == "float":
        return "!"
    if t == "double":
        return "#"
    if t == "char":
        return "$"
    return "#"


def _exec_c_side_effect_expr(interpreter: "Interpreter", expr: str) -> bool:
    s = expr.strip().rstrip(";")
    if not s:
        return False
    # Handle i++, ++i, i--, --i
    m = re.match(
        r"^\s*(\+\+|--)?\s*([A-Za-z_][A-Za-z0-9_]*)\s*(\+\+|--)?\s*$",
        s,
    )
    if m:
        pre, name, post = m.groups()
        if pre in ("++", "--") or post in ("++", "--"):
            up = name.upper()
            suf = None
            if up + "%" in interpreter.int_variables:
                suf = "%"
            elif up + "&" in interpreter.long_variables:
                suf = "&"
            elif up + "!" in interpreter.single_variables:
                suf = "!"
            elif up + "#" in interpreter.double_variables:
                suf = "#"
            elif up + "$" in interpreter.string_variables:
                return False
            cur = interpreter.get_numeric_value(up) or 0
            delta = 1 if (pre == "++" or post == "++") else -1
            interpreter.set_typed_variable((up + (suf or "#")), cur + delta)
            return True
        # Simple assignment
        val: Any
    # Compound assignment: +=, -=, *=, /=, %=
    cm = re.match(
        r"^\s*([A-Za-z_][A-Za-z0-9_]*(?:\[[^\]]*\])?)\s*([+\-*/%])=\s*(.+)$", s
    )
    if cm:
        name = cm.group(1).strip().upper()
        op = cm.group(2)
        right_str = cm.group(3).strip()
        cur = interpreter.get_numeric_value(name) or 0
        try:
            norm = re.sub(
                r"([A-Za-z_][A-Za-z0-9_]*)\s*\[\s*(.*?)\s*\]", r"\1(\2)", right_str
            )
            rval = _c_eval_expr(interpreter, norm)
        except (ValueError, TypeError, ZeroDivisionError):
            rval = 0
        ops = {
            "+": lambda a, b: a + b,
            "-": lambda a, b: a - b,
            "*": lambda a, b: a * b,
            "/": lambda a, b: a // b if b else 0,
            "%": lambda a, b: a % b if b else 0,
        }
        new_val = ops[op](cur, rval)
        suf = None
        if name + "%" in interpreter.int_variables:
            suf = "%"
        elif name + "&" in interpreter.long_variables:
            suf = "&"
        elif name + "!" in interpreter.single_variables:
            suf = "!"
        elif name + "#" in interpreter.double_variables:
            suf = "#"
        interpreter.set_typed_variable(name + (suf or "#"), new_val)
        return True
    if "=" in s:
        left, right = s.split("=", 1)
        name = left.strip().upper()
        try:
            # normalize 'arr[index]' into 'arr(index)' for expression evaluator
            norm = re.sub(
                r"([A-Za-z_][A-Za-z0-9_]*)\s*\[\s*(.*?)\s*\]", r"\1(\2)", right
            )
            val = _c_eval_expr(interpreter, norm)
        except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
            val = 0
        suf = None
        if name + "%" in interpreter.int_variables:
            suf = "%"
        elif name + "&" in interpreter.long_variables:
            suf = "&"
        elif name + "!" in interpreter.single_variables:
            suf = "!"
        elif name + "#" in interpreter.double_variables:
            suf = "#"
        elif name + "$" in interpreter.string_variables:
            suf = "$"
        if suf == "$":
            interpreter.set_typed_variable(name + "$", _unquote(str(right)))
        else:
            interpreter.set_typed_variable(name + (suf or "#"), val)
        return True
    # Fallback: evaluate expression (stdlib calls / side effects)
    try:
        norm = re.sub(r"([A-Za-z_][A-Za-z0-9_]*)\s*\[\s*(.*?)\s*\]", r"\1(\2)", s)
        _c_eval_expr(interpreter, norm)
        return True
    except StopIteration:
        raise
    except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
        pass
    return False


def _try_eval_c_func(interpreter: "Interpreter", expr: str) -> Any:
    """Try to evaluate a C stdlib function call like strlen("hello").

    Returns the result value or None if this isn't a recognized function call.
    """
    m = re.match(r"([a-zA-Z_]\w*)\s*\((.*)\)$", expr.strip())
    if not m:
        return None
    func_name = m.group(1)
    # _C_STDLIB is defined later in this module
    stdlib = globals().get("_C_STDLIB", {})
    func = stdlib.get(func_name)
    if func is None:
        return None
    try:
        inner_args = _split_args(m.group(2))
        return func(inner_args, interpreter)
    except Exception:  # noqa: BLE001
        return None


def _printf(interpreter: "Interpreter", arglist: str) -> str:
    args = _split_args(arglist)
    if not args:
        return ""
    fmt = _unquote(args[0])
    values: List[Any] = []
    for expr in args[1:]:
        expr = expr.strip()
        if expr.startswith('"'):
            values.append(_unquote(expr))
        else:
            # Try evaluating C stdlib function calls first (e.g., strlen("hello"))
            func_val = _try_eval_c_func(interpreter, expr)
            if func_val is not None:
                values.append(func_val)
                continue
            # Use full C expression evaluator (handles arrays, stdlib, etc.)
            try:
                values.append(_c_eval_expr(interpreter, expr))
            except (
                ValueError,
                TypeError,
                ZeroDivisionError,
            ):  # noqa: BLE001
                values.append(0)

    out: List[str] = []
    vi = 0
    i = 0
    while i < len(fmt):
        ch = fmt[i]
        if ch == "%":
            # Try to match a format specifier
            # Regex: %[-+0 #]*[0-9]*(\.[0-9]+)?[hlL]?[diuoxXfFeEgGaAcspn%]
            # We'll use a simpler one for now: %[^a-zA-Z%]*[a-zA-Z%]
            m = re.match(
                r"^%([-+0 #]*[0-9]*(\.[0-9]+)?[hlL]?[diuoxXfFeEgGaAcspn%])",
                fmt[i:],
            )
            if m:
                spec_full = m.group(0)
                spec_type = spec_full[-1]

                if spec_type == "%":
                    out.append("%")
                    i += len(spec_full)
                    continue

                if vi < len(values):
                    val = values[vi]
                    vi += 1

                    # Convert value to appropriate type for formatting
                    try:
                        if spec_type == "c":
                            # %c expects an integer; convert to character
                            val = chr(int(float(val)))
                            out.append(val)
                            i += len(spec_full)
                            continue
                        elif spec_type in "diuoxX":
                            val = int(float(val))
                        elif spec_type in "fFeEgGaA":
                            val = float(val)
                        elif spec_type == "s":
                            val = str(val)

                        # Use Python's string formatting compatible with C
                        # Remove 'l' or 'L' length modifiers (not in Python)
                        py_spec = spec_full.replace("l", "").replace("L", "")
                        out.append(py_spec % val)
                    except (ValueError, TypeError):
                        out.append(spec_full)  # Fallback

                    i += len(spec_full)
                    continue
                else:
                    # Not enough arguments
                    out.append(spec_full)
                    i += len(spec_full)
                    continue
            else:
                # Just a %
                out.append("%")
                i += 1
                continue

        if ch == "\\" and i + 1 < len(fmt):
            nxt = fmt[i + 1]
            if nxt == "n":
                out.append("\n")
                i += 2
                continue
            if nxt == "t":
                out.append("\t")
                i += 2
                continue
        out.append(ch)
        i += 1
    return "".join(out)


def _scanf(interpreter: "Interpreter", arglist: str) -> str:
    args = _split_args(arglist)
    if len(args) < 2:
        return "❌ Error: scanf requires a format and one &var"
    fmt = _unquote(args[0])
    var = args[1].strip()
    if not var.startswith("&"):
        return "❌ Error: scanf requires &var destination"
    name = var[1:].strip().upper()
    # Determine the correct type suffix by checking what was declared
    tcode = None
    if name + "%" in interpreter.int_variables:
        tcode = "%"
    elif name + "&" in interpreter.long_variables:
        tcode = "&"
    elif name + "!" in interpreter.single_variables:
        tcode = "!"
    elif name + "#" in interpreter.double_variables:
        tcode = "#"
    elif name + "$" in interpreter.string_variables:
        tcode = "$"
    if tcode is None:
        # Fall back to format-based detection
        if "%d" in fmt or "%i" in fmt:
            tcode = "%"
        elif "%f" in fmt or "%g" in fmt:
            tcode = "#"
        elif "%s" in fmt:
            tcode = "$"
        else:
            tcode = "#"
    interpreter.start_input_request(
        "",
        name + tcode,
        is_numeric=(tcode != "$"),
    )
    return ""


def _ensure_c_stack(interpreter: "Interpreter"):
    if not hasattr(interpreter, "c_block_stack"):
        interpreter.c_block_stack = []


# ---------------------------------------------------------------------------
# C standard library built-in function table
# ---------------------------------------------------------------------------

_C_STDLIB: Dict[str, Any] = {
    # ── string.h ──────────────────────────────────────────────────────────
    "strlen": lambda args, interp: len(_c_str(args[0], interp)) if args else 0,
    "strcmp": lambda args, interp: (
        (
            0
            if _c_str(args[0], interp) == _c_str(args[1], interp)
            else (-1 if _c_str(args[0], interp) < _c_str(args[1], interp) else 1)
        )
        if len(args) >= 2
        else 0
    ),
    "strncmp": lambda args, interp: (
        (
            0
            if _c_str(args[0], interp)[: int(_c_num(args[2], interp))]
            == _c_str(args[1], interp)[: int(_c_num(args[2], interp))]
            else -1
        )
        if len(args) >= 3
        else 0
    ),
    "strcasecmp": lambda args, interp: (
        (
            0
            if _c_str(args[0], interp).lower() == _c_str(args[1], interp).lower()
            else -1
        )
        if len(args) >= 2
        else 0
    ),
    "strchr": lambda args, interp: (
        (_c_str(args[0], interp).find(chr(int(_c_num(args[1], interp)))) + 1 or 0)
        if len(args) >= 2
        else 0
    ),
    "strrchr": lambda args, interp: (
        (_c_str(args[0], interp).rfind(chr(int(_c_num(args[1], interp)))) + 1 or 0)
        if len(args) >= 2
        else 0
    ),
    "strstr": lambda args, interp: (
        (_c_str(args[0], interp).find(_c_str(args[1], interp)) + 1 or 0)
        if len(args) >= 2
        else 0
    ),
    "strpbrk": lambda args, interp: (
        next(
            (
                i + 1
                for i, c in enumerate(_c_str(args[0], interp))
                if c in _c_str(args[1], interp)
            ),
            0,
        )
        if len(args) >= 2
        else 0
    ),
    "strspn": lambda args, interp: (
        (lambda s, a: len(s) - len(s.lstrip("".join(set(a)))))(
            _c_str(args[0], interp), _c_str(args[1], interp)
        )
        if len(args) >= 2
        else 0
    ),
    "strcspn": lambda args, interp: (
        next(
            (
                i
                for i, c in enumerate(_c_str(args[0], interp))
                if c in _c_str(args[1], interp)
            ),
            len(_c_str(args[0], interp)),
        )
        if len(args) >= 2
        else 0
    ),
    "strtok": lambda args, interp: (
        _c_str(args[0], interp).split(_c_str(args[1], interp))[0]
        if len(args) >= 2 and _c_str(args[0], interp)
        else None
    ),
    "toupper": lambda args, interp: (
        ord(_c_str(args[0], interp).upper()[0])
        if args and _c_str(args[0], interp)
        else int(_c_num(args[0], interp)) - 32 if args else 0
    ),
    "tolower": lambda args, interp: (
        ord(_c_str(args[0], interp).lower()[0])
        if args and isinstance(_c_num(args[0], interp), str)
        else int(_c_num(args[0], interp)) + 32 if args else 0
    ),
    # ── ctype.h ───────────────────────────────────────────────────────────
    "isalpha": lambda args, interp: (
        1 if args and chr(int(_c_num(args[0], interp))).isalpha() else 0
    ),
    "isdigit": lambda args, interp: (
        1 if args and chr(int(_c_num(args[0], interp))).isdigit() else 0
    ),
    "isalnum": lambda args, interp: (
        1 if args and chr(int(_c_num(args[0], interp))).isalnum() else 0
    ),
    "isspace": lambda args, interp: (
        1 if args and chr(int(_c_num(args[0], interp))).isspace() else 0
    ),
    "isupper": lambda args, interp: (
        1 if args and chr(int(_c_num(args[0], interp))).isupper() else 0
    ),
    "islower": lambda args, interp: (
        1 if args and chr(int(_c_num(args[0], interp))).islower() else 0
    ),
    "ispunct": lambda args, interp: (
        1
        if args
        and chr(int(_c_num(args[0], interp))) in "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
        else 0
    ),
    "isprint": lambda args, interp: (
        1 if args and chr(int(_c_num(args[0], interp))).isprintable() else 0
    ),
    "iscntrl": lambda args, interp: (
        1 if args and ord(chr(int(_c_num(args[0], interp)))) < 32 else 0
    ),
    "isxdigit": lambda args, interp: (
        1
        if args and chr(int(_c_num(args[0], interp))) in "0123456789abcdefABCDEF"
        else 0
    ),
    # ── stdlib.h ──────────────────────────────────────────────────────────
    "atoi": lambda args, interp: (
        int(_c_str(args[0], interp).strip().split()[0].rstrip(",. ")) if args else 0
    ),
    "atof": lambda args, interp: (
        float(_c_str(args[0], interp).strip()) if args else 0.0
    ),
    "atol": lambda args, interp: int(_c_str(args[0], interp).strip()) if args else 0,
    "strtol": lambda args, interp: (
        int(
            _c_str(args[0], interp).strip(),
            int(_c_num(args[2], interp)) if len(args) > 2 else 10,
        )
        if args
        else 0
    ),
    "strtod": lambda args, interp: (
        float(_c_str(args[0], interp).strip()) if args else 0.0
    ),
    "abs": lambda args, interp: abs(int(_c_num(args[0], interp))) if args else 0,
    "labs": lambda args, interp: abs(int(_c_num(args[0], interp))) if args else 0,
    "rand": lambda args, interp: _crand.randint(0, 32767),
    "srand": lambda args, interp: (
        _crand.seed(int(_c_num(args[0], interp))) if args else _crand.seed()
    )
    or 0,
    "exit": lambda args, interp: (_ for _ in ()).throw(StopIteration),
    "getenv": lambda args, interp: "",
    "system": lambda args, interp: 0,
    "malloc": lambda args, interp: 1,
    "calloc": lambda args, interp: 1,
    "realloc": lambda args, interp: 1,
    "free": lambda args, interp: 0,
    "qsort": lambda args, interp: None,
    "bsearch": lambda args, interp: 0,
    "NULL": lambda args, interp: 0,
    # ── math.h ────────────────────────────────────────────────────────────
    "sqrt": lambda args, interp: (
        _cmath.sqrt(float(_c_num(args[0], interp))) if args else 0.0
    ),
    "cbrt": lambda args, interp: (
        _cmath.cbrt(float(_c_num(args[0], interp))) if args else 0.0
    ),
    "ceil": lambda args, interp: (
        float(_cmath.ceil(float(_c_num(args[0], interp)))) if args else 0.0
    ),
    "floor": lambda args, interp: (
        float(_cmath.floor(float(_c_num(args[0], interp)))) if args else 0.0
    ),
    "round": lambda args, interp: (
        float(round(float(_c_num(args[0], interp)))) if args else 0.0
    ),
    "trunc": lambda args, interp: (
        float(_cmath.trunc(float(_c_num(args[0], interp)))) if args else 0.0
    ),
    "fabs": lambda args, interp: abs(float(_c_num(args[0], interp))) if args else 0.0,
    "fmod": lambda args, interp: (
        _cmath.fmod(float(_c_num(args[0], interp)), float(_c_num(args[1], interp)))
        if len(args) >= 2
        else 0.0
    ),
    "pow": lambda args, interp: (
        _cmath.pow(float(_c_num(args[0], interp)), float(_c_num(args[1], interp)))
        if len(args) >= 2
        else 0.0
    ),
    "exp": lambda args, interp: (
        _cmath.exp(float(_c_num(args[0], interp))) if args else 1.0
    ),
    "exp2": lambda args, interp: 2.0 ** float(_c_num(args[0], interp)) if args else 1.0,
    "log": lambda args, interp: (
        _cmath.log(float(_c_num(args[0], interp))) if args else 0.0
    ),
    "log2": lambda args, interp: (
        _cmath.log2(float(_c_num(args[0], interp))) if args else 0.0
    ),
    "log10": lambda args, interp: (
        _cmath.log10(float(_c_num(args[0], interp))) if args else 0.0
    ),
    "sin": lambda args, interp: (
        _cmath.sin(float(_c_num(args[0], interp))) if args else 0.0
    ),
    "cos": lambda args, interp: (
        _cmath.cos(float(_c_num(args[0], interp))) if args else 1.0
    ),
    "tan": lambda args, interp: (
        _cmath.tan(float(_c_num(args[0], interp))) if args else 0.0
    ),
    "asin": lambda args, interp: (
        _cmath.asin(float(_c_num(args[0], interp))) if args else 0.0
    ),
    "acos": lambda args, interp: (
        _cmath.acos(float(_c_num(args[0], interp))) if args else 0.0
    ),
    "atan": lambda args, interp: (
        _cmath.atan(float(_c_num(args[0], interp))) if args else 0.0
    ),
    "atan2": lambda args, interp: (
        _cmath.atan2(float(_c_num(args[0], interp)), float(_c_num(args[1], interp)))
        if len(args) >= 2
        else 0.0
    ),
    "sinh": lambda args, interp: (
        _cmath.sinh(float(_c_num(args[0], interp))) if args else 0.0
    ),
    "cosh": lambda args, interp: (
        _cmath.cosh(float(_c_num(args[0], interp))) if args else 1.0
    ),
    "tanh": lambda args, interp: (
        _cmath.tanh(float(_c_num(args[0], interp))) if args else 0.0
    ),
    "hypot": lambda args, interp: (
        _cmath.hypot(float(_c_num(args[0], interp)), float(_c_num(args[1], interp)))
        if len(args) >= 2
        else 0.0
    ),
    "ldexp": lambda args, interp: (
        _cmath.ldexp(float(_c_num(args[0], interp)), int(_c_num(args[1], interp)))
        if len(args) >= 2
        else 0.0
    ),
    "frexp": lambda args, interp: (
        _cmath.frexp(float(_c_num(args[0], interp)))[0] if args else 0.0
    ),
    # ── time.h ────────────────────────────────────────────────────────────
    "time": lambda args, interp: int(_ctime.time()),
    "clock": lambda args, interp: int(_ctime.process_time() * 1000000),
    "difftime": lambda args, interp: (
        float(_c_num(args[0], interp)) - float(_c_num(args[1], interp))
        if len(args) >= 2
        else 0.0
    ),
    # ── stdio.h ───────────────────────────────────────────────────────────
    "putchar": lambda args, interp: (
        (interp.log_output(chr(int(_c_num(args[0], interp)))) if args else None)
        or int(_c_num(args[0], interp))
        if args
        else -1
    ),
    "getchar": lambda args, interp: (
        ord(interp.request_input("")[0]) if hasattr(interp, "request_input") else -1
    ),
    "puts": lambda args, interp: (
        interp.log_output(_c_str(args[0], interp) + "\n") if args else None
    )
    or 0,
    "fputs": lambda args, interp: 0,
    "fclose": lambda args, interp: 0,
    "feof": lambda args, interp: 0,
    "rewind": lambda args, interp: 0,
    "fflush": lambda args, interp: 0,
    # ── Misc C99/identifiers ──────────────────────────────────────────────
    "INT_MAX": lambda args, interp: 2147483647,
    "INT_MIN": lambda args, interp: -2147483648,
    "UINT_MAX": lambda args, interp: 4294967295,
    "LONG_MAX": lambda args, interp: 9223372036854775807,
    "FLT_MAX": lambda args, interp: 3.4028235e38,
    "DBL_MAX": lambda args, interp: 1.7976931348623157e308,
    "M_PI": lambda args, interp: _cmath.pi,
    "M_E": lambda args, interp: _cmath.e,
    "M_SQRT2": lambda args, interp: _cmath.sqrt(2),
    "M_LN2": lambda args, interp: _cmath.log(2),
    "M_LN10": lambda args, interp: _cmath.log(10),
}


def _c_str(expr_or_val: str, interpreter: "Interpreter") -> str:
    """Resolve a C expression to a string."""
    if isinstance(expr_or_val, str) and (
        expr_or_val.startswith('"') or expr_or_val.startswith("'")
    ):
        return _unquote(expr_or_val)
    if isinstance(expr_or_val, str):
        up = expr_or_val.strip().upper()
        if up + "$" in interpreter.string_variables:
            return interpreter.string_variables[up + "$"]
        try:
            v = interpreter.evaluate_expression(expr_or_val)
            return str(v)
        except Exception:
            return ""
    return str(expr_or_val)


def _c_num(expr_or_val: str, interpreter: "Interpreter") -> Any:
    """Resolve a C expression to a number."""
    if isinstance(expr_or_val, (int, float)):
        return expr_or_val
    if isinstance(expr_or_val, str):
        s = expr_or_val.strip()
        if s.startswith('"') or s.startswith("'"):
            sv = _unquote(s)
            return ord(sv[0]) if sv else 0
        up = s.upper()
        if up + "#" in interpreter.double_variables:
            return interpreter.double_variables[up + "#"]
        if up in interpreter.variables:
            return interpreter.variables[up]
        try:
            return interpreter.evaluate_expression(s)
        except Exception:
            return 0
    return 0


def _c_eval_expr(interpreter: "Interpreter", expr: str) -> Any:
    """Evaluate a C expression, handling stdlib function calls."""
    expr = expr.strip().rstrip(";")
    if not expr:
        return 0

    # Struct field access: var.field
    m_field = re.match(r"^([A-Za-z_][A-Za-z0-9_]*)\.([A-Za-z_][A-Za-z0-9_]*)$", expr)
    if m_field:
        var_name = m_field.group(1).upper()
        field_name = m_field.group(2).upper()
        rec = interpreter.variables.get(var_name)
        if isinstance(rec, dict) and field_name in rec:
            return rec[field_name]

    # Ternary operator: cond ? true_val : false_val
    # Find ? and : at depth 0
    ternary_q = -1
    ternary_c = -1
    depth = 0
    in_str = False
    for ti, tc in enumerate(expr):
        if tc == '"' and not in_str:
            in_str = True
        elif tc == '"' and in_str:
            in_str = False
        elif not in_str:
            if tc in "([":
                depth += 1
            elif tc in ")]":
                depth -= 1
            elif tc == "?" and depth == 0 and ternary_q < 0:
                ternary_q = ti
            elif tc == ":" and depth == 0 and ternary_q >= 0:
                ternary_c = ti
                break
    if ternary_q > 0 and ternary_c > ternary_q:
        cond_s = expr[:ternary_q].strip()
        true_s = expr[ternary_q + 1 : ternary_c].strip()
        false_s = expr[ternary_c + 1 :].strip()
        try:
            cond_val = _c_eval_expr(interpreter, cond_s)
        except Exception:
            cond_val = 0
        if cond_val:
            return _c_eval_expr(interpreter, true_s)
        else:
            return _c_eval_expr(interpreter, false_s)

    # sprintf(buf, fmt, ...) — used as value (returns formatted string)
    m = re.match(r"^sprintf\s*\(\s*([A-Za-z_]\w*)\s*,\s*(.+)\)$", expr, re.IGNORECASE)
    if m:
        buf_name = m.group(1).upper()
        rest_args = _split_args(m.group(2))
        result = _printf(interpreter, ", ".join(rest_args))
        interpreter.string_variables[buf_name + "$"] = result
        return len(result)

    # snprintf(buf, n, fmt, ...)
    m = re.match(
        r"^snprintf\s*\(\s*([A-Za-z_]\w*)\s*,\s*\d+\s*,\s*(.+)\)$", expr, re.IGNORECASE
    )
    if m:
        buf_name = m.group(1).upper()
        rest_args = _split_args(m.group(2))
        result = _printf(interpreter, ", ".join(rest_args))
        interpreter.string_variables[buf_name + "$"] = result
        return len(result)

    # strcpy(dest, src) / strncpy(dest, src, n)
    m = re.match(
        r"^str(?:n)?cpy\s*\(([A-Za-z_]\w*),\s*(.+?)(?:,\s*.+?)?\)$", expr, re.IGNORECASE
    )
    if m:
        dest, src = m.group(1).upper(), m.group(2).strip()
        interpreter.string_variables[dest + "$"] = _c_str(src, interpreter)
        return 1

    # strcat(dest, src) / strncat(dest, src, n)
    m = re.match(
        r"^str(?:n)?cat\s*\(([A-Za-z_]\w*),\s*(.+?)(?:,\s*.+?)?\)$", expr, re.IGNORECASE
    )
    if m:
        dest, src = m.group(1).upper(), m.group(2).strip()
        interpreter.string_variables[dest + "$"] = interpreter.string_variables.get(
            dest + "$", ""
        ) + _c_str(src, interpreter)
        return 1

    # strcpy as void statement
    m = re.match(r"^strcpy\s*\(([A-Za-z_]\w*)\s*,\s*(.+)\)$", expr, re.IGNORECASE)
    if m:
        dest, src = m.group(1).upper(), m.group(2).strip()
        interpreter.string_variables[dest + "$"] = _c_str(src, interpreter)
        return 1

    # memset(dest, c, n)
    m = re.match(
        r"^memset\s*\(([A-Za-z_]\w*)\s*,\s*(.+),\s*(.+)\)$", expr, re.IGNORECASE
    )
    if m:
        dest, c_e, n_e = m.group(1).upper(), m.group(2).strip(), m.group(3).strip()
        try:
            c = chr(int(_c_num(c_e, interpreter)))
            n = int(_c_num(n_e, interpreter))
            interpreter.string_variables[dest + "$"] = c * n
        except Exception:
            pass
        return 1

    # memcpy(dest, src, n) / memmove(dest, src, n)
    m = re.match(
        r"^mem(?:cpy|move)\s*\(([A-Za-z_]\w*)\s*,\s*(.+),\s*(.+)\)$",
        expr,
        re.IGNORECASE,
    )
    if m:
        dest, src = m.group(1).upper(), m.group(2).strip()
        interpreter.string_variables[dest + "$"] = _c_str(src, interpreter)
        return 1

    # Look up stdlib function
    m = re.match(r"^([A-Za-z_]\w*)\s*\(([^)]*)\)$", expr)
    if m:
        fname = m.group(1).lower()
        arg_str = m.group(2).strip()
        if fname in _C_STDLIB:
            try:
                args = _split_args(arg_str) if arg_str else []
                return _C_STDLIB[fname](args, interpreter)
            except StopIteration:
                raise
            except Exception:
                return 0
        # Check as uppercase constant
        fupper = m.group(1).upper()
        if fupper in _C_STDLIB:
            return _C_STDLIB[fupper]([], interpreter)

    # Check bare constant names (M_PI etc.)
    if expr in _C_STDLIB:
        return _C_STDLIB[expr]([], interpreter)
    if expr.upper() in _C_STDLIB:
        return _C_STDLIB[expr.upper()]([], interpreter)

    # Replace rand() before passing to generic evaluator
    expr_repl = expr
    while "rand(" in expr_repl.lower():
        rnum = str(_crand.randint(0, 32767))
        expr_repl = re.sub(
            r"rand\s*\([^)]*\)", rnum, expr_repl, count=1, flags=re.IGNORECASE
        )

    # Replace array indexing  arr[idx]  with the actual stored value
    def _resolve_arr(m_arr):
        aname = m_arr.group(1).upper()
        idx_expr = m_arr.group(2)
        try:
            idx = int(interpreter.evaluate_expression(idx_expr))
        except Exception:
            idx = 0
        a = getattr(interpreter, "arrays", {}).get(aname)
        if a is not None and 0 <= idx < len(a):
            v = a[idx]
            if isinstance(v, str):
                return f'"{v}"'
            return str(v)
        return "0"

    expr_repl = re.sub(
        r"([A-Za-z_][A-Za-z0-9_]*)\s*\[\s*([^\]]+?)\s*\]",
        _resolve_arr,
        expr_repl,
    )

    # Replace struct field accesses var.field with their values
    def _resolve_field(mf: re.Match) -> str:
        var_n = mf.group(1).upper()
        field_n = mf.group(2).upper()
        rec = interpreter.variables.get(var_n)
        if isinstance(rec, dict) and field_n in rec:
            v = rec[field_n]
            if isinstance(v, str):
                return f'"{v}"'
            return str(v)
        return mf.group(0)

    expr_repl = re.sub(
        r"([A-Za-z_][A-Za-z0-9_]*)\.([A-Za-z_][A-Za-z0-9_]*)",
        _resolve_field,
        expr_repl,
    )

    # Check if the expression is a bare variable name that maps to a string variable
    bare = expr_repl.strip()
    bare_up = bare.upper()
    if re.match(r"^[A-Za-z_][A-Za-z0-9_]*$", bare):
        if bare_up + "$" in interpreter.string_variables:
            return interpreter.string_variables[bare_up + "$"]
        if bare_up + "%" in interpreter.int_variables:
            return interpreter.int_variables[bare_up + "%"]
        if bare_up + "&" in interpreter.long_variables:
            return interpreter.long_variables[bare_up + "&"]
        if bare_up + "!" in interpreter.single_variables:
            return interpreter.single_variables[bare_up + "!"]
        if bare_up + "#" in interpreter.double_variables:
            return interpreter.double_variables[bare_up + "#"]

    # Delegate to the generic interpreter evaluator
    try:
        return interpreter.evaluate_expression(expr_repl)
    except (ValueError, TypeError, ZeroDivisionError):
        return 0


def _get_block_comment_flag(interpreter: "Interpreter") -> bool:
    """Safely read the C block-comment flag from interpreter state."""
    return getattr(interpreter, "_in_c_block_comment", False)


def _set_block_comment_flag(interpreter: "Interpreter", value: bool) -> None:
    """Safely set the C block-comment flag on interpreter state."""
    try:
        setattr(interpreter, "_in_c_block_comment", value)
    except AttributeError:
        # If interpreter forbids setting arbitrary attributes, ignore
        # gracefully
        pass


def _find_block_end(interpreter: "Interpreter", header_idx: int) -> int:
    lines = interpreter.program_lines
    depth = 0
    seen_open = False
    j = header_idx
    while j < len(lines):
        s = lines[j][1]
        # Process character by character to handle } else { correctly
        for char in s:
            if char == "{":
                seen_open = True
                depth += 1
            elif char == "}":
                if seen_open:
                    depth -= 1
                    if depth == 0:
                        return j
        j += 1
    return header_idx


def _first_inside_index(interpreter: "Interpreter", header_idx: int) -> int:
    lines = interpreter.program_lines
    j = header_idx
    while j < len(lines):
        s = lines[j][1].strip()
        if "{" in s:
            return min(j + 1, len(lines) - 1)
        j += 1
    return min(header_idx + 1, len(lines) - 1)


def _declare_variable(
    interpreter: "Interpreter",
    t: str,
    name: str,
    init: str | None,
):
    """Declare a C variable of type t with optional initializer expr."""
    up = name.upper()
    suf = _suffix_for_type(t)
    val: Any
    if init:
        expr = init.split("=", 1)[1]
        try:
            if suf == "$":
                val = _unquote(expr)
            else:
                val = _c_eval_expr(interpreter, expr)
        except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
            val = 0
    else:
        val = "" if suf == "$" else 0
    interpreter.set_typed_variable(up + suf, val)
    return ""


def _assign_variable(interpreter: "Interpreter", name: str, expr: str):
    """Assign expression to an existing (or inferred) C variable name."""
    expr = expr.rstrip(";").strip()
    # Handle array assignments like arr[index]
    arr_idx = None
    arr_name = name
    if "[" in name and "]" in name:
        base = name.split("[", 1)[0].strip()
        idx_expr = name[name.find("[") + 1 : name.rfind("]")]
        try:
            idx_val = int(interpreter.evaluate_expression(idx_expr))
        except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
            idx_val = 0
        arr_idx = int(idx_val)
        arr_name = base.strip()

    up = arr_name.upper()
    suf = None
    if up + "%" in interpreter.int_variables:
        suf = "%"
    elif up + "&" in interpreter.long_variables:
        suf = "&"
    elif up + "!" in interpreter.single_variables:
        suf = "!"
    elif up + "#" in interpreter.double_variables:
        suf = "#"
    elif up + "$" in interpreter.string_variables:
        suf = "$"
    # small helper: allow rand() calls embedded in expressions
    expr_repl = expr
    while "rand(" in expr_repl.lower():
        # replace the first occurrence with a random int (0..32767)
        rnum = str(random.randint(0, 32767))
        # naive replace for 'rand()' or 'rand(arg)' occurrences
        expr_repl = re.sub(
            r"rand\s*\([^)]*\)", rnum, expr_repl, count=1, flags=re.IGNORECASE
        )

    val: Any
    try:
        if suf == "$":
            # Could be result of a string function
            result = _c_eval_expr(interpreter, expr_repl)
            if isinstance(result, str):
                val = result
            else:
                val = _unquote(expr_repl)
        else:
            val = _c_eval_expr(interpreter, expr_repl)
    except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
        val = "" if suf == "$" else 0
    # Infer type if not declared yet
    if suf is None:
        if isinstance(val, str):
            suf_to_use = "$"
        else:
            suf_to_use = "#"
    else:
        suf_to_use = suf
    # If array assignment: ensure arrays store lists and set by index
    if arr_idx is not None:
        a = interpreter.arrays.get(up)
        if a is None:
            a = []
        # expand as necessary
        if arr_idx < 0:
            idx_to_use = 0
        else:
            idx_to_use = arr_idx
        if idx_to_use >= len(a):
            a.extend([0] * (idx_to_use + 1 - len(a)))
        a[idx_to_use] = val  # type: ignore[call-overload]
        interpreter.arrays[up] = a
        return ""

    interpreter.set_typed_variable(up + suf_to_use, val)
    return ""


def _handle_close_brace(interpreter: "Interpreter") -> str:
    """Handle end of C blocks for while/do/if when encountering '}' or '};'."""
    if not interpreter.c_block_stack:
        return ""
    top: Dict[str, Any] = interpreter.c_block_stack[-1]
    if top.get("end") != interpreter.current_line:
        return ""
    t = top.get("type")
    if t == "while":
        post_expr = top.get("post")
        if post_expr:
            _exec_c_side_effect_expr(interpreter, post_expr)
        try:
            cond_val = interpreter.evaluate_expression(top.get("cond", "0"))
        except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
            cond_val = 0
        if cond_val:
            _start = top.get("start", interpreter.current_line)
            interpreter.current_line = _start
        else:
            interpreter.c_block_stack.pop()
        return ""
    if t == "do":
        # Look ahead for trailing while(cond);
        lines = interpreter.program_lines
        j = interpreter.current_line + 1
        while j < len(lines) and not lines[j][1].strip():
            j += 1
        if j < len(lines):
            mw = _WHILE_AFTER_DO_RE.match(lines[j][1])
            if mw:
                cond_expr = mw.group(1).strip()
                try:
                    cond_val = interpreter.evaluate_expression(cond_expr)
                except (
                    ValueError,
                    TypeError,
                    ZeroDivisionError,
                ):  # noqa: BLE001
                    cond_val = 0
                if cond_val:
                    interpreter.current_line = (
                        top.get("start", interpreter.current_line) - 1
                    )
                else:
                    interpreter.c_block_stack.pop()
                    interpreter.current_line = j  # skip while line
                    return ""
        interpreter.c_block_stack.pop()
        return ""
    if t == "if":
        skip_to = top.get("else_skip_to")
        if skip_to is not None:
            interpreter.current_line = skip_to
        interpreter.c_block_stack.pop()
        return ""
    if t == "else":
        interpreter.c_block_stack.pop()
        return ""
    if t == "switch":
        interpreter.c_block_stack.pop()
        return ""
    return ""


def execute_c(interpreter: "Interpreter", command: str, turtle: "TurtleState") -> str:
    """Execute a single C-like command line."""
    _ensure_c_stack(interpreter)
    # Track multiline block-comment state on the interpreter instance
    if not hasattr(interpreter, "_in_c_block_comment"):
        _set_block_comment_flag(interpreter, False)

    cmd = command.strip()

    # If we are currently inside a /* ... */ comment block ignore until we see
    # */
    if _get_block_comment_flag(interpreter):
        if "*/" in cmd:
            # End block comment; ignore everything through this marker
            _set_block_comment_flag(interpreter, False)
        return ""

    # Handle start (or full single-line) block comment
    if cmd.startswith("/*"):
        if "*/" not in cmd:
            _set_block_comment_flag(interpreter, True)
        return ""

    # Ignore preprocessor directives (e.g. #include, #define)
    if cmd.startswith("#"):
        return ""

    # Accept function headers like 'int main() {' as valid no-op lines
    if re.match(
        r"^\s*(?:int|void|char|float|double)\s+[A-Za-z_]\w*\s*\([^)]*\)\s*\{?$",
        cmd,
    ):
        # If there's an opening brace here, let the brace handling code
        # deal with block depth
        return ""
    if not cmd:
        return ""

    # --- Multi-statement line splitting ---
    # If a line contains multiple statements separated by ';' (e.g. a[0]=1; a[1]=2;)
    # split them and execute each individually.  Skip for known compound statements.
    if (
        ";" in cmd
        and not _FOR_RE.match(cmd)
        and not _PRINTF_RE.match(cmd)
        and not _SCANF_RE.match(cmd)
        and not cmd.startswith("//")
        and not cmd.startswith("#")
    ):
        parts = _split_case_stmts(cmd)
        if len(parts) > 1:
            results: List[str] = []
            for part in parts:
                part = part.strip()
                if part:
                    r = execute_c(interpreter, part, turtle)
                    if r:
                        results.append(r)
            return "\n".join(results) if results else ""

    # Handle closing brace with a trailing while on same line
    # e.g. `} while (cond);`
    m = _CLOSE_BRACE_WITH_TRAILING_WHILE_RE.match(cmd)
    if m:
        if (
            interpreter.c_block_stack
            and interpreter.c_block_stack[-1].get("type") == "do"
        ):
            top: Dict[str, Any] = interpreter.c_block_stack[-1]
            if top.get("end") == interpreter.current_line:
                cond_expr = m.group(1).strip()
                try:
                    cond_val = interpreter.evaluate_expression(cond_expr)
                except (
                    ValueError,
                    TypeError,
                    ZeroDivisionError,
                ):  # noqa: BLE001
                    cond_val = 0
                if cond_val:
                    interpreter.current_line = (
                        top.get("start", interpreter.current_line) - 1
                    )
                else:
                    interpreter.c_block_stack.pop()
                return ""

    # Plain close brace(s): delegate to block handler
    if cmd in ("}", "};") or cmd.startswith("} else"):
        return _handle_close_brace(interpreter)

    # break/continue
    low = cmd.lower().rstrip(";")
    if low == "break":
        for i in range(len(interpreter.c_block_stack) - 1, -1, -1):
            fr: Dict[str, Any] = interpreter.c_block_stack[i]
            if fr.get("type") in ("while", "do", "switch"):
                _end = fr.get("end", interpreter.current_line)
                interpreter.current_line = _end
                if fr.get("type") == "switch":
                    interpreter.c_block_stack.pop(i)
                return ""
        return ""
    if low == "continue":
        for i in range(len(interpreter.c_block_stack) - 1, -1, -1):
            fr = interpreter.c_block_stack[i]
            if fr.get("type") == "while":
                _end = fr.get("end", interpreter.current_line)
                interpreter.current_line = _end
                return ""
        return ""

    # if (...) { ... } [else { ... }]
    m = _IF_RE.match(cmd)
    if m:
        cond_expr = m.group(1).strip()
        header_idx = interpreter.current_line
        end_idx = _find_block_end(interpreter, header_idx)
        lines = interpreter.program_lines
        else_start = None
        else_end = None
        if end_idx < len(lines) and _ELSE_ON_SAME_LINE_RE.match(lines[end_idx][1]):
            else_end = _find_block_end(interpreter, end_idx)
            else_start = _first_inside_index(interpreter, end_idx)
        else:
            j = end_idx + 1
            while j < len(lines) and not lines[j][1].strip():
                j += 1
            if j < len(lines) and _ELSE_RE.match(lines[j][1]):
                else_end = _find_block_end(interpreter, j)
                else_start = _first_inside_index(interpreter, j)
        try:
            cond_val = interpreter.evaluate_expression(cond_expr)
        except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
            cond_val = 0
        if cond_val:
            if else_end is not None:
                interpreter.c_block_stack.append(
                    {
                        "type": "if",
                        "end": end_idx,
                        "else_skip_to": else_end + 1,
                    }
                )
            start = _first_inside_index(interpreter, header_idx)
            interpreter.current_line = start
        else:
            if else_start is not None:
                interpreter.c_block_stack.append(
                    {
                        "type": "else",
                        "end": else_end,
                    }
                )
                interpreter.current_line = else_start
            else:
                interpreter.current_line = end_idx
        return ""

    # switch (expr) { case val: ... break; ... default: ... }
    m = _SWITCH_RE.match(cmd)
    if m:
        switch_expr_str = m.group(1).strip()
        header_idx = interpreter.current_line
        end_idx = _find_block_end(interpreter, header_idx)
        try:
            switch_val = interpreter.evaluate_expression(switch_expr_str)
        except (ValueError, TypeError, ZeroDivisionError):
            switch_val = 0
        # Scan forward to find the matching case or default
        lines = interpreter.program_lines
        target_line = None
        default_line = None
        for j in range(header_idx + 1, end_idx + 1):
            line_text = lines[j][1].strip() if j < len(lines) else ""
            cm = _CASE_RE.match(line_text)
            if cm:
                case_val_str = cm.group(1).strip().strip("'\"")
                try:
                    case_val: Any = interpreter.evaluate_expression(case_val_str)
                except (ValueError, TypeError, ZeroDivisionError):
                    case_val = case_val_str
                if case_val == switch_val or str(case_val) == str(switch_val):
                    target_line = j
                    break
            dm = _DEFAULT_RE.match(line_text)
            if dm:
                default_line = j
        if target_line is None:
            target_line = default_line
        if target_line is not None:
            interpreter.c_block_stack.append({"type": "switch", "end": end_idx})
            interpreter.current_line = target_line
        else:
            interpreter.current_line = end_idx
        return ""

    # case ...: / default: — inside a switch, just skip the label and run the rest
    m = _CASE_RE.match(cmd)
    if m:
        rest = m.group(2).strip() if m.group(2) else ""
        if rest:
            results = []
            for part in _split_case_stmts(rest):
                part = part.strip()
                if part:
                    r = execute_c(interpreter, part, turtle)
                    if r:
                        results.append(r)
            return "\n".join(results) if results else ""
        return ""

    m = _DEFAULT_RE.match(cmd)
    if m:
        rest = m.group(1).strip() if m.group(1) else ""
        if rest:
            results = []
            for part in _split_case_stmts(rest):
                part = part.strip()
                if part:
                    r = execute_c(interpreter, part, turtle)
                    if r:
                        results.append(r)
            return "\n".join(results) if results else ""
        return ""

    # while (...) { ... }
    m = _WHILE_RE.match(cmd)
    if m:
        cond_expr = m.group(1).strip()
        header_idx = interpreter.current_line
        end_idx = _find_block_end(interpreter, header_idx)
        try:
            cond_val = interpreter.evaluate_expression(cond_expr)
        except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
            cond_val = 0
        if not cond_val:
            interpreter.current_line = end_idx
            return ""
        start = _first_inside_index(interpreter, header_idx)
        interpreter.c_block_stack.append(
            {
                "type": "while",
                "end": end_idx,
                "start": start,
                "cond": cond_expr,
            }
        )
        interpreter.current_line = start
        return ""

    # for (init; cond; post) { ... }
    m = _FOR_RE.match(cmd)
    if m:
        init, cond_expr, post, rest = m.groups()
        header_idx = interpreter.current_line
        # Check for an inline single-statement body after the header, e.g.
        # for (int i = 0; i < n; ++i) arr[i] = ...;
        # rest was captured in regex (trailing statement or brace)
        if rest and not rest.startswith("{"):
            # Handle single-line for-loop by executing init, then looping over
            # body
            if init.strip():
                # allow declarations in init: e.g. 'int i = 0'
                d = _DECL_RE.match(init.strip())
                if d:
                    t, name, _init = d.groups()
                    _declare_variable(interpreter, t, name, _init)
                else:
                    _exec_c_side_effect_expr(interpreter, init)

            # Evaluate loop iteratively
            while True:
                try:
                    cond_val = (
                        interpreter.evaluate_expression(cond_expr.strip())
                        if cond_expr.strip()
                        else 0
                    )
                except (
                    ValueError,
                    TypeError,
                    ZeroDivisionError,
                ):  # noqa: BLE001
                    cond_val = 0
                if not cond_val:
                    break

                # Execute the inline body (strip trailing semicolon)
                body_cmd = rest.rstrip(";").strip()
                if body_cmd:
                    res = execute_c(interpreter, body_cmd, turtle)
                    if res:
                        # ensure printed values flow into interpreter output
                        try:
                            interpreter.log_output(res)
                        except (AttributeError, TypeError):  # noqa: B902
                            pass

                # post expression side-effects
                if post.strip():
                    _exec_c_side_effect_expr(interpreter, post)

            return ""
        if init.strip():
            d = _DECL_RE.match(init.strip())
            if d:
                t, name, _init = d.groups()
                _declare_variable(interpreter, t, name, _init)
            else:
                _exec_c_side_effect_expr(interpreter, init)
        end_idx = _find_block_end(interpreter, header_idx)
        try:
            _cond = cond_expr.strip() or "0"
            cond_val = interpreter.evaluate_expression(_cond)
        except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
            cond_val = 0
        if not cond_val:
            interpreter.current_line = end_idx
            return ""
        start = _first_inside_index(interpreter, header_idx)
        interpreter.c_block_stack.append(
            {
                "type": "while",
                "end": end_idx,
                "start": start,
                "cond": cond_expr.strip() or "0",
                "post": post.strip(),
            }
        )
        interpreter.current_line = start
        return ""

    # do { ... } while (cond);
    if _DO_RE.match(cmd):
        header_idx = interpreter.current_line
        end_idx = _find_block_end(interpreter, header_idx)
        start = _first_inside_index(interpreter, header_idx)
        interpreter.c_block_stack.append({"type": "do", "end": end_idx, "start": start})
        interpreter.current_line = start
        return ""

    # Array declarations  (int a[3];  or  int a[3]={10,20,30};)
    m = _ARRAY_DECL_RE.match(cmd)
    if m:
        atype = m.group(1)
        aname = m.group(2)
        asize_str = m.group(3)
        init_list = m.group(4)  # may be None
        asize = int(asize_str)
        suffix = _suffix_for_type(atype)
        default: Any = "" if suffix == "$" else 0
        if not hasattr(interpreter, "arrays"):
            interpreter.arrays = {}
        arr = [default] * asize
        if init_list is not None:
            # Parse initializer list: {10, 20, 30}
            values = [v.strip() for v in init_list.split(",") if v.strip()]
            for idx, val_str in enumerate(values):
                if idx >= asize:
                    break
                try:
                    arr[idx] = _c_eval_expr(interpreter, val_str)
                except (ValueError, TypeError, ZeroDivisionError):
                    arr[idx] = default
        interpreter.arrays[aname.upper()] = arr
        return ""

    # Declarations
    m = _DECL_RE.match(cmd)
    if m:
        t, name, init = m.groups()
        return _declare_variable(interpreter, t, name, init)

    # Assignment
    m = _ASSIGN_RE.match(cmd)
    if m:
        name, expr = m.groups()
        return _assign_variable(interpreter, name, expr)

    # printf
    m = _PRINTF_RE.match(cmd)
    if m:
        return _printf(interpreter, m.group(1))

    # sprintf(buf, fmt, ...) / snprintf(buf, n, fmt, ...)
    m = re.match(r"^\s*s(?:n)?printf\s*\((.+)\)\s*;?\s*$", cmd, re.IGNORECASE)
    if m:
        _c_eval_expr(interpreter, cmd.rstrip(";"))
        return ""

    # scanf
    m = _SCANF_RE.match(cmd)
    if m:
        return _scanf(interpreter, m.group(1))

    # Comments and braces
    # struct/union type definition: struct Point { int x; int y; }
    if _STRUCT_DEF_RE.match(cmd):
        m = _STRUCT_DEF_RE.match(cmd)
        struct_name = m.group(1).upper()
        if not hasattr(interpreter, "c_struct_defs"):
            interpreter.c_struct_defs = {}
        fields: Dict[str, str] = {}
        j = interpreter.current_line + 1
        lines = interpreter.program_lines
        while j < len(lines):
            s = lines[j][1].strip()
            # End of struct body
            if s.startswith("}"):
                interpreter.current_line = j
                break
            # Field declaration: int x; or int x, y;
            fm = re.match(
                r"^(int|long|float|double|char)\s+([^;]+);?\s*$", s, re.IGNORECASE
            )
            if fm:
                ftype, fname_list = fm.group(1), fm.group(2)
                for fn in fname_list.split(","):
                    fn = fn.strip().lstrip("*")
                    if fn:
                        fields[fn.upper()] = ftype.upper()
            j += 1
        interpreter.c_struct_defs[struct_name] = fields
        return ""

    # typedef scalar: typedef int MyInt;
    if _TYPEDEF_RE.match(cmd):
        m = _TYPEDEF_RE.match(cmd)
        if not hasattr(interpreter, "c_typedefs"):
            interpreter.c_typedefs = {}
        interpreter.c_typedefs[m.group(2).upper()] = m.group(1).upper()
        return ""

    # struct variable declaration: struct Point p; or Point p; (typedef)
    if _STRUCT_VAR_RE.match(cmd):
        m = _STRUCT_VAR_RE.match(cmd)
        type_name = m.group(1).upper()
        var_name = m.group(2).upper()
        # Avoid matching 'int x;' again
        if type_name not in ("INT", "LONG", "FLOAT", "DOUBLE", "CHAR", "VOID",
                              "UNSIGNED", "SIGNED", "SHORT", "CONST", "STATIC",
                              "RETURN", "BREAK", "CONTINUE", "ELSE", "CASE",
                              "DEFAULT", "SWITCH", "WHILE", "FOR", "DO", "IF"):
            struct_defs = getattr(interpreter, "c_struct_defs", {})
            if type_name in struct_defs:
                # Create a dict-based struct instance
                fields = struct_defs[type_name]
                rec: Dict[str, Any] = {}
                for fn, ft in fields.items():
                    rec[fn] = "" if ft in ("CHAR",) else 0.0
                interpreter.variables[var_name] = rec
                return ""
            # Check typedefs
            typedefs = getattr(interpreter, "c_typedefs", {})
            if type_name in typedefs:
                suf = _suffix_for_type(typedefs[type_name])
                interpreter.set_typed_variable(var_name + suf, 0)
                return ""

    # struct field assignment: p.x = expr;
    m = _STRUCT_FIELD_ASSIGN_RE.match(cmd)
    if m:
        var_name = m.group(1).upper()
        field_name = m.group(2).upper()
        expr = m.group(3).rstrip(";").strip()
        rec = interpreter.variables.get(var_name)
        if isinstance(rec, dict) and field_name in rec:
            try:
                val = _c_eval_expr(interpreter, expr)
            except (ValueError, TypeError, ZeroDivisionError):
                val = 0
            rec[field_name] = val
            return ""

    # Recognize C++ style line comments and braces/parentheses
    if cmd.startswith("//") or cmd in ("{", "(", ")"):
        return ""

    # Handle 'return' statements gracefully: end execution if inside main
    if cmd.lower().startswith("return"):
        # try to parse return expression and ignore — treat it as program
        # termination
        inner = cmd[len("return") :].strip().rstrip(";")
        if inner:
            # Evaluate expression (if numeric) and set variable _RETURN if
            # needed
            try:
                val = interpreter.evaluate_expression(inner)
                interpreter.set_typed_variable("_RETURN#", val)
            except (
                ValueError,
                TypeError,
                ZeroDivisionError,
                AttributeError,
            ):  # noqa: BLE001
                pass
        # stop the interpreter as 'return' indicates program exit
        try:
            interpreter.running = False
        except AttributeError:
            pass
        return ""

    # Handle standalone stdlib / user function calls: funcname(args);
    m = re.match(r"^([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*;?\s*$", cmd)
    if m:
        fname = m.group(1).lower()
        # Handle known void stdlib calls
        if fname in (
            "strcpy",
            "strncpy",
            "strcat",
            "strncat",
            "memset",
            "memcpy",
            "memmove",
            "srand",
            "exit",
            "free",
            "qsort",
            "sprintf",
            "snprintf",
            "printf",
            "puts",
            "fputs",
            "fclose",
            "fflush",
            "rewind",
            "memset",
        ):
            _c_eval_expr(interpreter, cmd.rstrip(";"))
            return ""
        if fname in _C_STDLIB:
            _c_eval_expr(interpreter, cmd.rstrip(";"))
            return ""

    # Try to execute as a side-effect expression (e.g. i++; func();)
    if _exec_c_side_effect_expr(interpreter, cmd):
        return ""

    return f"❌ Error: Unknown C command '{command.strip()}'"
