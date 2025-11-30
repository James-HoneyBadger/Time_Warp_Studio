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

import re
from typing import TYPE_CHECKING, Dict, List

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter


_DECL_RE = re.compile(
    r"^\s*(int|long|float|double)\s+"
    r"([A-Za-z_][A-Za-z0-9_]*)"
    r"(\s*=\s*[^;]+)?\s*;?\s*$",
)
_ASSIGN_RE = re.compile(r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.+);?\s*$")
_PRINTF_RE = re.compile(r"^\s*printf\s*\((.*)\)\s*;?\s*$", re.IGNORECASE)
_SCANF_RE = re.compile(r"^\s*scanf\s*\((.*)\)\s*;?\s*$", re.IGNORECASE)
_IF_RE = re.compile(r"^\s*if\s*\((.*)\)\s*\{?\s*$", re.IGNORECASE)
_ELSE_RE = re.compile(r"^\s*else\s*\{?\s*$", re.IGNORECASE)
_WHILE_RE = re.compile(r"^\s*while\s*\((.*)\)\s*\{?\s*$", re.IGNORECASE)
_FOR_RE = re.compile(
    r"^\s*for\s*\(([^;]*);([^;]*);([^)]*)\)\s*\{?\s*$",
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


def _split_args(arg_str: str) -> List[str]:
    parts: List[str] = []
    buf = []
    in_str = False
    esc = False
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
            if ch == ",":
                parts.append("".join(buf).strip())
                buf = []
            else:
                buf.append(ch)
                if ch == '"':
                    in_str = True
    if buf:
        parts.append("".join(buf).strip())
    return parts


def _unquote(s: str) -> str:
    s = s.strip()
    if len(s) >= 2 and s[0] == '"' and s[-1] == '"':
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
    return "#"


def _exec_c_side_effect_expr(interpreter: "Interpreter", expr: str):
    s = expr.strip().rstrip(";")
    if not s:
        return
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
                return
            cur = interpreter.get_numeric_value(up) or 0
            delta = 1 if (pre == "++" or post == "++") else -1
            interpreter.set_typed_variable((up + (suf or "#")), cur + delta)
            return
    # Simple assignment
    if "=" in s:
        left, right = s.split("=", 1)
        name = left.strip().upper()
        try:
            val = interpreter.evaluate_expression(right)
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
        return
    # Fallback: evaluate expression (no side effects captured)
    try:
        interpreter.evaluate_expression(s)
    except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
        pass


def _printf(interpreter: "Interpreter", arglist: str) -> str:
    args = _split_args(arglist)
    if not args:
        return ""
    fmt = _unquote(args[0])
    values = []
    for expr in args[1:]:
        expr = expr.strip()
        if expr.startswith('"'):
            values.append(_unquote(expr))
        else:
            up = expr.strip().upper()
            if up + "$" in interpreter.string_variables:
                values.append(interpreter.string_variables[up + "$"])
            elif up in interpreter.variables:
                values.append(interpreter.variables[up])
            else:
                try:
                    values.append(interpreter.evaluate_expression(expr))
                except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
                    values.append(0)

    out = []
    vi = 0
    i = 0
    while i < len(fmt):
        ch = fmt[i]
        if ch == "%" and i + 1 < len(fmt):
            spec = fmt[i + 1]
            if spec in ("d", "i") and vi < len(values):
                out.append(str(int(float(values[vi]))))
                vi += 1
                i += 2
                continue
            if spec in ("f", "g") and vi < len(values):
                out.append(f"{float(values[vi]):g}")
                vi += 1
                i += 2
                continue
            if spec == "s" and vi < len(values):
                out.append(str(values[vi]))
                vi += 1
                i += 2
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
    tcode = "#"
    if "%d" in fmt or "%i" in fmt:
        tcode = "%"
    elif "%f" in fmt or "%g" in fmt:
        tcode = "#"
    elif "%s" in fmt:
        tcode = "$"
    interpreter.start_input_request(
        "",
        name + tcode,
        is_numeric=(tcode != "$"),
    )
    return ""


def _ensure_c_stack(interpreter: "Interpreter"):
    if not hasattr(interpreter, "c_block_stack"):
        interpreter.c_block_stack: List[Dict] = []


def _find_block_end(interpreter: "Interpreter", header_idx: int) -> int:
    lines = interpreter.program_lines
    depth = 0
    seen_open = False
    j = header_idx
    while j < len(lines):
        s = lines[j][1]
        opens = s.count("{")
        closes = s.count("}")
        if opens:
            seen_open = True
        depth += opens
        depth -= closes
        if seen_open and depth == 0:
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
    if init:
        expr = init.split("=", 1)[1]
        try:
            if suf == "$":
                val = _unquote(expr)
            else:
                val = interpreter.evaluate_expression(expr)
        except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
            val = 0
    else:
        val = "" if suf == "$" else 0
    interpreter.set_typed_variable(up + suf, val)
    return ""


def _assign_variable(interpreter: "Interpreter", name: str, expr: str):
    """Assign expression to an existing (or inferred) C variable name."""
    expr = expr.rstrip(";").strip()
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
        suf = "$"
    try:
        if suf == "$":
            val = _unquote(expr)
        else:
            val = interpreter.evaluate_expression(expr)
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
    interpreter.set_typed_variable(up + suf_to_use, val)
    return ""


def _handle_close_brace(interpreter: "Interpreter") -> str:
    """Handle end of C blocks for while/do/if when encountering '}' or '};'."""
    if not interpreter.c_block_stack:
        return ""
    top = interpreter.c_block_stack[-1]
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
            interpreter.current_line = _start - 1
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
                except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
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
            interpreter.current_line = skip_to - 1
        interpreter.c_block_stack.pop()
        return ""
    return ""


def execute_c(interpreter: "Interpreter", command: str, _turtle=None) -> str:
    """Execute a single C-like command line."""
    _ensure_c_stack(interpreter)
    cmd = command.strip()
    if not cmd:
        return ""

    # Handle closing brace with a trailing while on same line
    # e.g. `} while (cond);`
    m = _CLOSE_BRACE_WITH_TRAILING_WHILE_RE.match(cmd)
    if m:
        if (
            interpreter.c_block_stack
            and interpreter.c_block_stack[-1].get("type") == "do"
        ):
            top = interpreter.c_block_stack[-1]
            if top.get("end") == interpreter.current_line:
                cond_expr = m.group(1).strip()
                try:
                    cond_val = interpreter.evaluate_expression(cond_expr)
                except (ValueError, TypeError, ZeroDivisionError):  # noqa: BLE001
                    cond_val = 0
                if cond_val:
                    interpreter.current_line = (
                        top.get("start", interpreter.current_line) - 1
                    )
                else:
                    interpreter.c_block_stack.pop()
                return ""

    # Plain close brace(s): delegate to block handler
    if cmd in ("}", "};"):
        return _handle_close_brace(interpreter)

    # break/continue
    low = cmd.lower().rstrip(";")
    if low == "break":
        for i in range(len(interpreter.c_block_stack) - 1, -1, -1):
            fr = interpreter.c_block_stack[i]
            if fr.get("type") in ("while", "do"):
                _end = fr.get("end", interpreter.current_line)
                interpreter.current_line = _end
                return ""
        return ""
    if low == "continue":
        for i in range(len(interpreter.c_block_stack) - 1, -1, -1):
            fr = interpreter.c_block_stack[i]
            if fr.get("type") == "while":
                _end = fr.get("end", interpreter.current_line)
                interpreter.current_line = _end - 1
                return ""
        return ""

    # if (...) { ... } [else { ... }]
    m = _IF_RE.match(cmd)
    if m:
        cond_expr = m.group(1).strip()
        header_idx = interpreter.current_line
        end_idx = _find_block_end(interpreter, header_idx)
        lines = interpreter.program_lines
        j = end_idx + 1
        while j < len(lines) and not lines[j][1].strip():
            j += 1
        else_start = None
        else_end = None
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
            interpreter.current_line = start - 1
        else:
            if else_start is not None:
                interpreter.current_line = else_start - 1
            else:
                interpreter.current_line = end_idx
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
        interpreter.current_line = start - 1
        return ""

    # for (init; cond; post) { ... }
    m = _FOR_RE.match(cmd)
    if m:
        init, cond_expr, post = m.groups()
        header_idx = interpreter.current_line
        if init.strip():
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
        interpreter.current_line = start - 1
        return ""

    # do { ... } while (cond);
    if _DO_RE.match(cmd):
        header_idx = interpreter.current_line
        end_idx = _find_block_end(interpreter, header_idx)
        start = _first_inside_index(interpreter, header_idx)
        interpreter.c_block_stack.append({"type": "do", "end": end_idx, "start": start})
        interpreter.current_line = start - 1
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

    # scanf
    m = _SCANF_RE.match(cmd)
    if m:
        return _scanf(interpreter, m.group(1))

    # Comments and braces
    if cmd.startswith("//") or cmd in ("{", "(", ")"):
        return ""

    return f"❌ Error: Unknown C command '{command.strip()}'"
