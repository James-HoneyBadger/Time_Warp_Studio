"""COMAL language executor for Time Warp Studio.

Educational COMAL-80 (COMmon Algorithmic Language) interpreter — executed
line-by-line.  COMAL blends BASIC's immediacy with Pascal-style structured
constructs.

Supported statements:
    PRINT [USING fmt:] expr [,|; expr …]
    INPUT [PROMPT "text":] variable
    var := expr              (``=`` is also accepted)
    IF cond THEN … [ELIF cond THEN …] [ELSE …] ENDIF
    IF cond THEN statement           single-line form
    FOR i := a TO b [STEP s] … NEXT [i]
    WHILE cond … ENDWHILE
    REPEAT … UNTIL cond
    LOOP … EXIT … ENDLOOP
    PROC name(params) … ENDPROC      callable with CALL name(args)
    FUNC name(params) … ENDFUNC      callable with CALL name(args)
    CALL name(args)
    RETURN [expr]
    DIM name(size)
    DATA v1, v2, … / READ var / RESTORE
    GOTO n / GOSUB n / RETURN
    STOP | END | EXIT
    REM …  and  // …  comments

Turtle graphics: MOVE n, TURN n, PENUP, PENDOWN, HOME, PLOT.
"""

from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


class ComalError(Exception):
    """Runtime error raised by the COMAL executor."""


_LOOP_KINDS = {"for", "while", "repeat", "loop"}


# ---------------------------------------------------------------------------
# State
# ---------------------------------------------------------------------------


def _state(interp: "Interpreter") -> dict:
    state = interp.__dict__.get("_comal_state")
    if state is None:
        state = {"stack": [], "call_stack": [], "data": [], "data_ptr": 0}
        interp.__dict__["_comal_state"] = state
    return state


# ---------------------------------------------------------------------------
# Source helpers
# ---------------------------------------------------------------------------

_STRING_RE = re.compile(r'"[^"]*"')


def _strip_comment(line: str) -> str:
    in_str = False
    i = 0
    while i < len(line):
        if line[i] == '"':
            in_str = not in_str
        elif not in_str and line[i : i + 2] == "//":
            return line[:i]
        i += 1
    return line


def _code_text(line: str) -> str:
    """Comment-stripped line with string contents blanked out."""
    return _STRING_RE.sub("  ", _strip_comment(line)).strip()


def _upper_code(interp: "Interpreter", idx: int) -> str:
    return _code_text(interp.program_lines[idx][1]).upper()


def _split_top(text: str, separators: str = ",") -> list[str]:
    """Split on separators that are outside quotes and parentheses."""
    parts: list[str] = []
    depth = 0
    in_str = False
    current: list[str] = []
    for ch in text:
        if in_str:
            current.append(ch)
            if ch == '"':
                in_str = False
            continue
        if ch == '"':
            in_str = True
            current.append(ch)
            continue
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        if ch in separators and depth == 0:
            parts.append("".join(current))
            current = []
            continue
        current.append(ch)
    parts.append("".join(current))
    return parts


def _find_end(interp: "Interpreter", start_idx: int, opener, closer) -> int:
    """Return the index of the terminator matching the block at *start_idx*."""
    depth = 1
    idx = start_idx + 1
    lines = interp.program_lines
    while idx < len(lines):
        code = _upper_code(interp, idx)
        if closer(code):
            depth -= 1
        elif opener(code):
            depth += 1
        if depth <= 0:
            return idx
        idx += 1
    raise ComalError("unterminated block")


def _is_if_open(code: str) -> bool:
    return bool(re.match(r"^IF\b", code)) and bool(re.search(r"\bTHEN\s*$", code))


def _is_for_open(code: str) -> bool:
    return bool(re.match(r"^FOR\b", code))


# ---------------------------------------------------------------------------
# Values / expressions
# ---------------------------------------------------------------------------


def _get_var(interp: "Interpreter", name: str) -> Any:
    key = name.upper()
    if key.endswith("$"):
        return interp.string_variables.get(key, "")
    if key in interp.variables:
        return interp.variables[key]
    return 0.0


def _set_var(interp: "Interpreter", name: str, value: Any) -> None:
    key = name.upper()
    if key.endswith("$"):
        interp.string_variables[key] = str(value)
        return
    try:
        number = float(value)
    except (TypeError, ValueError):
        number = 0.0
    interp.variables[key] = number


def _fmt(value: Any) -> str:
    if isinstance(value, bool):
        return "1" if value else "0"
    if isinstance(value, float):
        return str(int(value)) if value.is_integer() else repr(value)
    return str(value)


def _eval_num(interp: "Interpreter", expr: str) -> float:
    text = expr.strip()
    if not text:
        return 0.0
    if text.startswith('"') and text.endswith('"') and len(text) >= 2:
        return 0.0
    if re.fullmatch(r"[A-Za-z_]\w*", text) and text.upper() in interp.variables:
        return float(interp.variables[text.upper()])
    try:
        return interpreter_eval(interp, text)
    except Exception:  # noqa: BLE001  # pylint: disable=broad-except
        return 0.0


def interpreter_eval(interp: "Interpreter", expr: str) -> float:
    return float(interp.evaluate_expression(expr))


def _eval_value(interp: "Interpreter", expr: str) -> Any:
    """Evaluate an expression, returning a string for string operands."""
    text = expr.strip()
    if len(text) >= 2 and text.startswith('"') and text.endswith('"'):
        return text[1:-1]
    if text.endswith("$") or re.fullmatch(r"[A-Za-z_]\w*\$", text):
        return _get_var(interp, text)
    stringy = bool(re.search(r'[A-Za-z_]\w*\$|"', text))
    if stringy:
        return _eval_string(interp, text)
    return _eval_num(interp, text)


def _eval_string(interp: "Interpreter", expr: str) -> str:
    """Concatenate string literals and ``$`` variables (simple ``+``/``;``)."""
    try:
        from ..utils.string_evaluator import StringExpressionEvaluator
    except Exception:  # noqa: BLE001  # pylint: disable=broad-except
        return expr
    env = dict(interp.string_variables)
    try:
        evaluator = StringExpressionEvaluator(string_variables=env)
        result = evaluator.evaluate(expr)
        return str(result)
    except Exception:  # noqa: BLE001  # pylint: disable=broad-except
        return expr


def _eval_condition(interp: "Interpreter", cond: str) -> bool:
    text = cond.strip()
    # String comparison?
    for op in ("<>", "<=", ">=", "=", "<", ">"):
        if op in text:
            left, right = text.split(op, 1)
            lv, rv = left.strip(), right.strip()
            if '"' in text or "$" in text:
                ls = _as_string(interp, lv)
                rs = _as_string(interp, rv)
                if op in ("=",):
                    return ls == rs
                if op == "<>":
                    return ls != rs
                if op == "<":
                    return ls < rs
                if op == ">":
                    return ls > rs
                if op == "<=":
                    return ls <= rs
                if op == ">=":
                    return ls >= rs
    try:
        return bool(interp.evaluate_expression(text))
    except Exception:  # noqa: BLE001  # pylint: disable=broad-except
        return _truthy_str(_as_string(interp, text))


def _as_string(interp: "Interpreter", expr: str) -> str:
    text = expr.strip()
    if len(text) >= 2 and text.startswith('"') and text.endswith('"'):
        return text[1:-1]
    if re.fullmatch(r"[A-Za-z_]\w*\$", text):
        return str(_get_var(interp, text))
    return _fmt(_eval_value(interp, text))


def _truthy_str(text: str) -> bool:
    text = text.strip()
    if text == "":
        return False
    try:
        return float(text) != 0.0
    except ValueError:
        return True


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_comal(
    interp: "Interpreter", command: str, turtle: "TurtleState"
) -> str:
    """Execute one line of COMAL source."""
    raw = _strip_comment(command)
    if not raw.strip():
        return ""
    upper = raw.strip().upper()

    if upper.startswith("REM"):
        return ""

    output: list[str] = []
    try:
        _exec_statement(interp, raw.strip(), turtle, output)
    except ComalError as exc:
        output.append(f"❌ COMAL error: {exc}")
    if not output:
        return ""
    return "\n".join(output) + "\n"


def _exec_statement(
    interp: "Interpreter",
    text: str,
    turtle: "TurtleState",
    output: list[str],
) -> str | None:
    upper = text.upper()

    if upper in ("ENDIF",):
        _pop(interp)
        return None
    if upper.startswith("ELIF ") or upper == "ELIF":
        return _handle_elif(interp, text)
    if upper == "ELSE":
        return _handle_else(interp)
    if upper.startswith("NEXT"):
        return _handle_next(interp)
    if upper == "ENDWHILE":
        return _handle_endwhile(interp)
    if upper.startswith("UNTIL "):
        return _handle_until(interp, text)
    if upper == "ENDLOOP":
        return _handle_endloop(interp)
    if upper == "ENDPROC" or upper == "ENDFUNC":
        return _handle_endproc(interp)
    if upper == "EXIT":
        return _handle_exit(interp)

    if upper.startswith("IF "):
        return _handle_if(interp, text, turtle, output)
    if upper.startswith("FOR "):
        return _handle_for(interp, text)
    if upper.startswith("WHILE "):
        return _handle_while(interp, text)
    if upper == "REPEAT":
        _state(interp)["stack"].append({"kind": "repeat", "start": interp.current_line})
        return None
    if upper == "LOOP":
        _state(interp)["stack"].append({"kind": "loop", "start": interp.current_line})
        return None
    if upper.startswith("PROC ") or upper.startswith("FUNC "):
        return _handle_proc(interp)
    if upper.startswith("CALL "):
        return _handle_call(interp, text[5:].strip())
    if upper.startswith("RETURN"):
        return _handle_return(interp, text[6:].strip())
    if upper.startswith("GOTO "):
        return _handle_goto(interp, text[5:].strip())
    if upper.startswith("GOSUB "):
        return _handle_gosub(interp, text[6:].strip())
    if upper.startswith("DIM "):
        return _handle_dim(interp, text[4:].strip())
    if upper.startswith("DATA "):
        return _handle_data(interp, text[5:].strip())
    if upper.startswith("READ "):
        return _handle_read(interp, text[5:].strip())
    if upper == "RESTORE":
        _state(interp)["data_ptr"] = 0
        return None
    if upper.startswith("PRINT"):
        return _handle_print(interp, text[5:].strip(), output)
    if upper.startswith("INPUT"):
        return _handle_input(interp, text[5:].strip())
    if upper in ("STOP", "END"):
        interp.running = False
        return "__JUMP__"

    if _handle_turtle(interp, upper, text, turtle):
        return None

    assign = re.match(
        r"^([A-Za-z_]\w*\$?)\s*(?:\(\s*([^)]*)\s*\))?\s*:?=\s*(.+)$", text, re.S
    )
    if assign:
        name, index, value_expr = assign.group(1), assign.group(2), assign.group(3).strip()
        if index is not None:
            position = int(_eval_num(interp, index))
            array = interp.arrays.get(name.upper())
            if array is None:
                raise ComalError(f"array {name.upper()} not dimensioned")
            value = _eval_value(interp, value_expr)
            number = float(value) if not isinstance(value, str) or not value.endswith("$") else 0.0
            while len(array) <= position:
                array.append(0)
            array[position] = number
            return None
        _set_var(interp, name, _eval_value(interp, value_expr))
        return None

    raise ComalError(f"unrecognised statement: {text}")


def _pop(interp: "Interpreter") -> dict | None:
    stack = _state(interp)["stack"]
    return stack.pop() if stack else None


# ---------------------------------------------------------------------------
# Conditionals
# ---------------------------------------------------------------------------


def _jump(interp: "Interpreter", idx: int) -> str:
    interp.current_line = idx
    return "__JUMP__"


def _handle_if(
    interp: "Interpreter",
    text: str,
    turtle: "TurtleState",
    output: list[str],
) -> str | None:
    match = re.match(r"^IF\s+(.*?)\s+THEN\b(.*)$", text, re.I | re.S)
    if not match:
        raise ComalError("IF requires THEN")
    condition = match.group(1).strip()
    rest = match.group(2).strip()

    if rest:
        # Single-line IF [ELSE stmt]
        else_match = re.match(r"^(.*?)\s+ELSE\s+(.*)$", rest, re.I | re.S)
        if else_match:
            then_stmt, else_stmt = else_match.group(1).strip(), else_match.group(2).strip()
        else:
            then_stmt, else_stmt = rest, None
        if _eval_condition(interp, condition):
            _exec_statement(interp, then_stmt, turtle, output)
        elif else_stmt:
            _exec_statement(interp, else_stmt, turtle, output)
        return None

    start = interp.current_line
    end = _find_end(interp, start, _is_if_open, lambda c: c.startswith("ENDIF"))
    frame = {"kind": "if", "start": start, "taken": False}
    _state(interp)["stack"].append(frame)
    if _eval_condition(interp, condition):
        frame["taken"] = True
        return None
    return _next_branch(interp, start, end)


def _next_branch(interp: "Interpreter", start: int, end: int) -> str:
    """Find the next top-level ELIF/ELSE between *start* and *end*."""
    depth = 0
    for idx in range(start + 1, end):
        code = _upper_code(interp, idx)
        if _is_if_open(code):
            depth += 1
        elif code.startswith("ENDIF"):
            depth -= 1
        elif depth == 0 and (code.startswith("ELIF") or code == "ELSE"):
            return _jump(interp, idx)
    # No branch matched: drop the pending IF frame and skip the block.
    stack = _state(interp)["stack"]
    if stack and stack[-1].get("kind") == "if":
        stack.pop()
    return _jump(interp, end + 1)


def _handle_elif(interp: "Interpreter", text: str) -> str | None:
    frame = _state(interp)["stack"][-1] if _state(interp)["stack"] else None
    if frame is None or frame["kind"] != "if":
        raise ComalError("ELIF without IF")
    if frame["taken"]:
        end = _find_end(interp, frame["start"], _is_if_open, lambda c: c.startswith("ENDIF"))
        return _jump(interp, end + 1)
    match = re.match(r"^ELIF\s+(.*?)\s+THEN\s*$", text, re.I | re.S)
    if not match:
        raise ComalError("ELIF requires THEN")
    if _eval_condition(interp, match.group(1).strip()):
        frame["taken"] = True
        return None
    end = _find_end(interp, frame["start"], _is_if_open, lambda c: c.startswith("ENDIF"))
    return _next_branch(interp, interp.current_line, end)


def _handle_else(interp: "Interpreter") -> str | None:
    frame = _state(interp)["stack"][-1] if _state(interp)["stack"] else None
    if frame is None or frame["kind"] != "if":
        raise ComalError("ELSE without IF")
    if frame["taken"]:
        end = _find_end(interp, frame["start"], _is_if_open, lambda c: c.startswith("ENDIF"))
        return _jump(interp, end + 1)
    frame["taken"] = True
    return None


# ---------------------------------------------------------------------------
# Loops
# ---------------------------------------------------------------------------


def _handle_for(interp: "Interpreter", text: str) -> str | None:
    match = re.match(
        r"^FOR\s+([A-Za-z_]\w*)\s*:?=\s*(.+?)\s+TO\s+(.+?)(?:\s+STEP\s+(.+?))?\s*(?:DO)?$",
        text,
        re.I | re.S,
    )
    if not match:
        raise ComalError("malformed FOR statement")
    name = match.group(1)
    start = _eval_num(interp, match.group(2).strip())
    limit = _eval_num(interp, match.group(3).strip())
    step = _eval_num(interp, match.group(4).strip()) if match.group(4) else 1.0
    _set_var(interp, name, start)
    frame = {
        "kind": "for",
        "start": interp.current_line,
        "var": name.upper(),
        "limit": limit,
        "step": step,
    }
    if not _for_in_range(start, limit, step):
        end = _find_end(interp, interp.current_line, _is_for_open, lambda c: c.startswith("NEXT"))
        return _jump(interp, end + 1)
    _state(interp)["stack"].append(frame)
    return None


def _for_in_range(value: float, limit: float, step: float) -> bool:
    return value <= limit if step >= 0 else value >= limit


def _handle_next(interp: "Interpreter") -> str | None:
    # The FOR frame may sit below inner if/loop frames that already closed,
    # so locate the nearest FOR frame.
    stack = _state(interp)["stack"]
    for index in range(len(stack) - 1, -1, -1):
        if stack[index]["kind"] == "for":
            frame = stack[index]
            del stack[index + 1 :]
            value = float(_get_var(interp, frame["var"])) + frame["step"]
            _set_var(interp, frame["var"], value)
            if _for_in_range(value, frame["limit"], frame["step"]):
                return _jump(interp, frame["start"] + 1)
            del stack[index]
            return None
    # NEXT without FOR: treat as no-op (COMAL allows bare END-style NEXT).
    return None


def _handle_while(interp: "Interpreter", text: str) -> str | None:
    condition = text[5:].strip()
    if not _eval_condition(interp, condition):
        end = _find_end(
            interp, interp.current_line, lambda c: c.startswith("WHILE"), lambda c: c == "ENDWHILE"
        )
        return _jump(interp, end + 1)
    _state(interp)["stack"].append(
        {"kind": "while", "start": interp.current_line, "cond": condition}
    )
    return None


def _handle_endwhile(interp: "Interpreter") -> str | None:
    stack = _state(interp)["stack"]
    for index in range(len(stack) - 1, -1, -1):
        if stack[index]["kind"] == "while":
            frame = stack[index]
            del stack[index + 1 :]
            if _eval_condition(interp, frame["cond"]):
                return _jump(interp, frame["start"] + 1)
            del stack[index]
            return None
    raise ComalError("ENDWHILE without WHILE")


def _handle_until(interp: "Interpreter", text: str) -> str | None:
    stack = _state(interp)["stack"]
    for index in range(len(stack) - 1, -1, -1):
        if stack[index]["kind"] == "repeat":
            frame = stack[index]
            del stack[index + 1 :]
            if _eval_condition(interp, text[5:].strip()):
                del stack[index]
                return None
            return _jump(interp, frame["start"] + 1)
    raise ComalError("UNTIL without REPEAT")


def _handle_endloop(interp: "Interpreter") -> str | None:
    stack = _state(interp)["stack"]
    for index in range(len(stack) - 1, -1, -1):
        if stack[index]["kind"] == "loop":
            frame = stack[index]
            del stack[index + 1 :]
            return _jump(interp, frame["start"] + 1)
    raise ComalError("ENDLOOP without LOOP")


def _handle_exit(interp: "Interpreter") -> str | None:
    stack = _state(interp)["stack"]
    for index in range(len(stack) - 1, -1, -1):
        if stack[index]["kind"] in _LOOP_KINDS:
            frame = stack[index]
            del stack[index:]
            end = _loop_end(interp, frame)
            return _jump(interp, end + 1)
    interp.running = False
    return "__JUMP__"


def _loop_end(interp: "Interpreter", frame: dict) -> int:
    kind = frame["kind"]
    start = frame["start"]
    if kind == "for":
        return _find_end(interp, start, _is_for_open, lambda c: c.startswith("NEXT"))
    if kind == "while":
        return _find_end(interp, start, lambda c: c.startswith("WHILE"), lambda c: c == "ENDWHILE")
    if kind == "repeat":
        return _find_end(interp, start, lambda c: c.startswith("REPEAT"), lambda c: c.startswith("UNTIL"))
    return _find_end(interp, start, lambda c: c == "LOOP", lambda c: c == "ENDLOOP")


# ---------------------------------------------------------------------------
# Procedures / subroutines
# ---------------------------------------------------------------------------


def _proc_body_index(interp: "Interpreter", name: str) -> int | None:
    pattern = re.compile(rf"^(PROC|FUNC)\s+{re.escape(name)}\b", re.I)
    for idx, (_, text) in enumerate(interp.program_lines):
        if pattern.match(_code_text(text)):
            return idx
    return None


def _handle_proc(interp: "Interpreter") -> str | None:
    """A definition encountered at top level is skipped over."""
    code = _upper_code(interp, interp.current_line)
    closer = (lambda c: c == "ENDFUNC") if code.startswith("FUNC") else (lambda c: c == "ENDPROC")
    opener = (lambda c: c.startswith("FUNC")) if code.startswith("FUNC") else (lambda c: c.startswith("PROC"))
    end = _find_end(interp, interp.current_line, opener, closer)
    return _jump(interp, end + 1)


def _handle_call(interp: "Interpreter", call: str) -> str | None:
    match = re.match(r"^([A-Za-z_]\w*)\s*(?:\((.*)\))?$", call, re.S)
    if not match:
        raise ComalError("malformed CALL")
    name = match.group(1)
    args_text = match.group(2) or ""
    args = [_eval_value(interp, a.strip()) for a in _split_top(args_text) if a.strip()]

    body = _proc_body_index(interp, name)
    if body is None:
        raise ComalError(f"unknown procedure {name}")
    header = _code_text(interp.program_lines[body][1])
    params_text = re.search(r"\((.*)\)", header)
    params = []
    if params_text and params_text.group(1).strip():
        params = [p.strip().upper() for p in _split_top(params_text.group(1))]
    for position, param in enumerate(params):
        _set_var(interp, param, args[position] if position < len(args) else 0.0)

    _state(interp)["call_stack"].append(interp.current_line)
    return _jump(interp, body + 1)


def _handle_return(interp: "Interpreter", expr: str) -> str | None:
    if expr:
        _set_var(interp, "RESULT", _eval_value(interp, expr))
    stack = _state(interp)["call_stack"]
    if stack:
        return _jump(interp, stack.pop() + 1)
    interp.running = False
    return "__JUMP__"


def _handle_endproc(interp: "Interpreter") -> str | None:
    stack = _state(interp)["call_stack"]
    if stack:
        return _jump(interp, stack.pop() + 1)
    return None


def _handle_goto(interp: "Interpreter", target: str) -> str | None:
    number = int(_eval_num(interp, target))
    if number not in interp.line_number_map:
        raise ComalError(f"no such line {number}")
    return _jump(interp, interp.line_number_map[number])


def _handle_gosub(interp: "Interpreter", target: str) -> str | None:
    number = int(_eval_num(interp, target))
    if number not in interp.line_number_map:
        raise ComalError(f"no such line {number}")
    _state(interp)["call_stack"].append(interp.current_line)
    return _jump(interp, interp.line_number_map[number])


# ---------------------------------------------------------------------------
# Data / I-O
# ---------------------------------------------------------------------------


def _handle_dim(interp: "Interpreter", spec: str) -> str | None:
    for entry in _split_top(spec):
        match = re.match(r"^([A-Za-z_]\w*)\s*\(\s*(\d+)\s*\)$", entry.strip())
        if not match:
            raise ComalError(f"malformed DIM: {entry.strip()}")
        size = int(match.group(2))
        interp.arrays[match.group(1).upper()] = [0.0] * (size + 1)
    return None


def _handle_data(interp: "Interpreter", values: str) -> str | None:
    state = _state(interp)
    for entry in _split_top(values):
        entry = entry.strip()
        if not entry:
            continue
        if entry.startswith('"') and entry.endswith('"'):
            state["data"].append(entry[1:-1])
        else:
            state["data"].append(entry)
    return None


def _handle_read(interp: "Interpreter", names: str) -> str | None:
    state = _state(interp)
    for name in _split_top(names):
        name = name.strip()
        if not name:
            continue
        if state["data_ptr"] >= len(state["data"]):
            raise ComalError("out of DATA")
        raw = state["data"][state["data_ptr"]]
        state["data_ptr"] += 1
        if name.upper().endswith("$"):
            _set_var(interp, name, raw)
        else:
            _set_var(interp, name, _to_number(raw))
    return None


def _to_number(raw: str) -> float:
    try:
        return float(raw)
    except ValueError:
        return 0.0


def _split_print_parts(text: str) -> list[tuple[str, str]]:
    """Split PRINT arguments on top-level ``,``/``;`` keeping separators."""
    parts: list[tuple[str, str]] = []
    depth = 0
    in_str = False
    current: list[str] = []
    for ch in text:
        if in_str:
            current.append(ch)
            if ch == '"':
                in_str = False
            continue
        if ch == '"':
            in_str = True
            current.append(ch)
            continue
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        if ch in ",;" and depth == 0:
            parts.append(("".join(current).strip(), ch))
            current = []
            continue
        current.append(ch)
    tail = "".join(current).strip()
    if tail or not parts:
        parts.append((tail, ""))
    return parts


def _handle_print(interp: "Interpreter", args: str, output: list[str]) -> str | None:
    args = re.sub(r"^USING\s+.+?:", "", args, flags=re.I)
    if not args.strip():
        output.append("")
        return None
    parts = _split_print_parts(args)
    joined = ""
    for index, (token, sep) in enumerate(parts):
        if token:
            joined += _as_string(interp, token)
        if index < len(parts) - 1 and sep != ";":
            joined += " "
    output.append(joined)
    return None


def _handle_input(interp: "Interpreter", args: str) -> str | None:
    prompt = "? "
    target = args.strip()
    prompt_match = re.match(r'^(?:PROMPT\s+)?(".*?")\s*:?\s*(.*)$', args, re.I | re.S)
    if prompt_match:
        prompt = prompt_match.group(1)[1:-1] + " "
        target = prompt_match.group(2).strip()
    target = target.rstrip(":").strip()
    if not target:
        return None
    var_name = target.split()[0]
    if var_name.upper().endswith("$"):
        interp.start_input_request(prompt, var_name.upper(), False)
    else:
        interp.start_input_request(prompt, var_name, True)
    return "__JUMP__"


# ---------------------------------------------------------------------------
# Turtle graphics
# ---------------------------------------------------------------------------


def _handle_turtle(
    interp: "Interpreter", upper: str, text: str, turtle: "TurtleState"
) -> bool:
    """Handle a turtle graphics keyword; return True when handled."""
    if turtle is None:
        return False
    keyword = upper.split()[0] if upper.split() else ""

    def amount() -> float:
        return _eval_num(interp, text[len(keyword) :].strip())

    if keyword == "MOVE":
        turtle.forward(amount())
        return True
    if keyword == "TURN":
        turtle.right(amount())
        return True
    if keyword == "PENUP":
        turtle.penup()
        return True
    if keyword == "PENDOWN":
        turtle.pendown()
        return True
    if keyword == "HOME":
        turtle.home()
        return True
    if keyword == "PLOT":
        turtle.draw_point(0, 0)
        return True
    return False
