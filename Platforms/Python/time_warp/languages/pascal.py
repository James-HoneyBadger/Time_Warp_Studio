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
    r"^\s*(?:var\s+)?(.+):\s*(integer|longint|real|string)\s*;?\s*$",
    re.IGNORECASE,
)
_VAR_ARRAY_RE = re.compile(
    r"^\s*(?:var\s+)?(.+):\s*array\s*\[\s*(\d+)\.\.(\d+)\s*\]\s*of\s*(integer|longint|real|string)\s*;?\s*$",
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

_WRITE_RE = re.compile(
    r"^\s*(writeln|write)\s*\((.*)\)\s*;?\s*$",
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
_CASE_LABEL_RE = re.compile(r"^\s*([0-9]+|'.*?'|\"\".*?\"\")\s*:\s*$")
_CASE_ELSE_RE = re.compile(r"^\s*else\s*:\s*$", re.IGNORECASE)
_PROC_RE = re.compile(
    r"^\s*procedure\s+([A-Za-z_][A-Za-z0-9_]*)\s*(?:\((.*)\))?\s*;\s*$",
    re.IGNORECASE,
)
_FUNC_RE = re.compile(
    r"^\s*function\s+([A-Za-z_][A-Za-z0-9_]*)\s*(?:\((.*)\))?\s*"
    r"(?::\s*(integer|longint|real|string))?\s*;\s*$",
    re.IGNORECASE,
)
_CALL_RE = re.compile(r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*(?:\((.*)\))?\s*;\s*$")


def _ensure_pascal_stack(interpreter: "Interpreter"):
    if not hasattr(interpreter, "pascal_block_stack"):
        interpreter.pascal_block_stack = []


def _is_begin(line: str) -> bool:
    u = line.strip().upper()
    return u == "BEGIN"


def _is_end(line: str) -> bool:
    return bool(re.match(r"^\s*END\s*[.;]?\s*$", line, re.IGNORECASE))


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
        mlabel = _CASE_LABEL_RE.match(s)
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
    parts: List[str] = []
    buf = []
    in_str = False
    for ch in arg_str:
        if ch == '"' and not in_str:
            in_str = True
            buf.append(ch)
            continue
        if ch == '"' and in_str:
            in_str = False
            buf.append(ch)
            continue
        if ch == "," and not in_str:
            parts.append("".join(buf).strip())
            buf = []
        else:
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
    if t in ("integer", "longint"):
        return "%"
    if t == "real":
        return "#"
    if t == "string":
        return "$"
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
                cur = interpreter.string_variables.get(target_key, "")
            else:
                cur: Any = interpreter.get_numeric_value(target_up) or 0
            # Backup only local param if it existed
            local_existed = False
            local_old: Any = None
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
        sel_val = interpreter.evaluate_expression(expr)
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
    """Handle IF ... THEN [ELSE] with required BEGIN ... END blocks."""
    header_idx = interpreter.current_line
    begin_idx = _find_begin_forward(interpreter, header_idx)
    if begin_idx is None:
        return "❌ Error: IF requires BEGIN ... END block"
    then_end = _find_end_for_begin(interpreter, begin_idx)
    # Check optional ELSE after then block
    j = then_end + 1
    lines = interpreter.program_lines
    while j < len(lines) and not lines[j][1].strip():
        j += 1
    else_begin = None
    else_end = None
    if j < len(lines) and _ELSE_RE.match(lines[j][1]):
        else_begin_idx = _find_begin_forward(interpreter, j)
        if else_begin_idx is not None:
            else_begin = else_begin_idx + 1
            else_end = _find_end_for_begin(interpreter, else_begin_idx)
    try:
        cond_v = interpreter.evaluate_expression(cond_expr)
    except (ValueError, TypeError, ZeroDivisionError):
        cond_v = 0
    if cond_v:
        # If there is an else block, ensure we skip it after then-end
        if else_end is not None:
            interpreter.pascal_block_stack.append(
                {"type": "if", "end": then_end, "skip_else_to": else_end + 1}
            )
        interpreter.current_line = begin_idx + 1 - 1
    else:
        if else_begin is not None:
            interpreter.current_line = else_begin - 1
        else:
            interpreter.current_line = then_end
    return ""


def _handle_while_do(interpreter: "Interpreter", cond_expr: str) -> str:
    """Handle WHILE ... DO with a required BEGIN ... END block."""
    header_idx = interpreter.current_line
    begin_idx = _find_begin_forward(interpreter, header_idx)
    if begin_idx is None:
        return "❌ Error: WHILE requires BEGIN ... END block"
    end_idx = _find_end_for_begin(interpreter, begin_idx)
    try:
        cond_v = interpreter.evaluate_expression(cond_expr)
    except (ValueError, TypeError, ZeroDivisionError):
        cond_v = 0
    if not cond_v:
        interpreter.current_line = end_idx
        return ""
    interpreter.pascal_block_stack.append(
        {
            "type": "while",
            "start": begin_idx + 1,
            "end": end_idx,
            "cond": cond_expr,
        }
    )
    interpreter.current_line = begin_idx + 1 - 1
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
        cond_v = interpreter.evaluate_expression(cond_expr)
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
) -> str:
    """
    Handle FOR ... TO/DOWNTO ... DO with a required BEGIN ... END block.
    """
    up = name.upper()
    try:
        start_val = interpreter.evaluate_expression(expr_start)
    except (ValueError, TypeError, ZeroDivisionError):
        start_val = 0
    try:
        limit_val = interpreter.evaluate_expression(expr_end)
    except (ValueError, TypeError, ZeroDivisionError):
        limit_val = 0
    # Init variable; prefer declared type
    suf = None
    if hasattr(interpreter, "pascal_types"):
        suf = interpreter.pascal_types.get(up)
    interpreter.set_typed_variable(up + (suf or "#"), start_val)
    begin_idx = _find_begin_forward(interpreter, interpreter.current_line)
    if begin_idx is None:
        return "❌ Error: FOR requires BEGIN ... END block"
    end_idx = _find_end_for_begin(interpreter, begin_idx)
    step = 1 if dirw.lower() == "to" else -1
    # Check initial condition
    cur = interpreter.get_numeric_value(up) or 0
    ok = cur <= limit_val if step > 0 else cur >= limit_val
    if not ok:
        interpreter.current_line = end_idx
        return ""
    interpreter.pascal_block_stack.append(
        {
            "type": "for",
            "start": begin_idx + 1,
            "end": end_idx,
            "var": up,
            "limit": limit_val,
            "step": step,
        }
    )
    interpreter.current_line = begin_idx + 1 - 1
    return ""


def execute_pascal(interpreter: "Interpreter", command: str, _turtle) -> str:
    """Execute Pascal language command."""
    # Strip inline Pascal comments {...} and (* ... *) for control-flow parsing
    cmd = re.sub(r"\{.*?\}|\(\*.*?\*\)", "", command).strip()
    if not cmd:
        return ""

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

    # Handle END of control-flow blocks
    if _is_end(cmd) and getattr(interpreter, "pascal_block_stack", []):
        top = interpreter.pascal_block_stack[-1]
        if top.get("end") == interpreter.current_line:
            if top.get("type") == "while":
                try:
                    _cond = top.get("cond", "0")
                    cond_v = interpreter.evaluate_expression(_cond)
                except (ValueError, TypeError, ZeroDivisionError):
                    cond_v = 0
                if cond_v:
                    _start = int(top.get("start") or interpreter.current_line)
                    interpreter.current_line = _start - 1
                else:
                    interpreter.pascal_block_stack.pop()
                    return ""
            elif top.get("type") == "if" and top.get("skip_else_to") is not None:
                _skip_to = top.get("skip_else_to")
                interpreter.current_line = _skip_to - 1
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
                val = interpreter.get_numeric_value(var) or 0
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
        print(f"DEBUG: Check return. Line={
                interpreter.current_line}, End={
                frame.get('end')}")
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

    upcmd = cmd.upper()
    if (
        upcmd in ("BEGIN", "END", "END.", "PROGRAM", "USES")
        or upcmd.startswith("USES ")
        or upcmd.startswith("PROGRAM ")
    ):
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
        return _handle_for(interpreter, name, expr_start, dirw, expr_end)

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
        except BaseException:
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

        suf = _suffix_for_type(t)
        default_val = 0.0

        for raw in names.split(","):
            name = raw.strip().upper()
            interpreter.arrays[name] = [default_val] * size
        return ""

    # Const Declaration
    if _CONST_RE.match(cmd):
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
        except BaseException:
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
        except BaseException:
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
        except BaseException:
            val = 0.0

        arr[idx] = val
        return ""

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
        val: Any
        try:
            if suf == "$":
                val = _unquote(expr)
            else:
                val = interpreter.evaluate_expression(expr)
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
                    out_parts.append(f"{interpreter.variables[up]:g}")
                else:
                    try:
                        expr_fixed = a.replace("[", "(").replace("]", ")")
                        _val = interpreter.evaluate_expression(expr_fixed)
                        out_parts.append(f"{_val:g}")
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

    return f"❌ Error: Unknown Pascal command '{command.strip()}'"
