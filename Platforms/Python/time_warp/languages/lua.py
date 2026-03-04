"""Lua language executor for Time Warp Studio.

Educational Lua interpreter — whole-program execution.
Supports a teaching subset of Lua 5.x.
"""

from __future__ import annotations

import math
import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..core.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_lua(interpreter: "Interpreter", source: str, turtle: "TurtleState") -> str:
    """Execute a complete Lua program and return all output as a string."""
    env = LuaEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------


class LuaEnvironment:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []
        self._globals: dict[str, Any] = {}
        self._functions: dict[str, "LuaFunction"] = {}
        self._call_stack: list[dict[str, Any]] = []
        self._setup_stdlib()

    def _setup_stdlib(self):
        self._globals.update(
            {
                "math": LuaMathLib(),
                "string": LuaStringLib(),
                "table": LuaTableLib(),
                "io": LuaIOLib(self),
                "os": LuaOSLib(),
                "coroutine": LuaCoroutineLib(),
                "package": {"path": "."},
                "_VERSION": "Lua 5.4",
            }
        )

    def _emit(self, text: str):
        self._output.append(str(text))

    def _resolve(self, name: str) -> Any:
        if self._call_stack:
            frame = self._call_stack[-1]
            if name in frame:
                return frame[name]
        return self._globals.get(name)

    def _assign(self, name: str, value: Any, local: bool = False):
        if local and self._call_stack:
            self._call_stack[-1][name] = value
        else:
            self._globals[name] = value

    # ------------------------------------------------------------------
    # Entry point
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        try:
            # Basic syntax check for invalid tokens
            if re.search(r"@@|##|\$\$", source):
                self._emit("❌ Lua syntax error: unexpected symbol")
                return "\n".join(self._output)
            lines = source.splitlines()
            blocks = self._parse_blocks(lines)
            self._exec_block(blocks)
        except LuaError as e:
            self._emit(f"❌ Lua error: {e}")
        except StopIteration:
            pass
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}")
        return "\n".join(self._output)

    # ------------------------------------------------------------------
    # Block parser: groups lines into logical blocks
    # ------------------------------------------------------------------

    def _parse_blocks(self, lines: list[str]) -> list[str]:
        """Join continuation lines (very naive – returns lines as-is for now)."""
        return lines

    # ------------------------------------------------------------------
    # Execute a list of statement lines
    # ------------------------------------------------------------------

    def _exec_block(self, lines: list[str], local_env: dict | None = None):
        """Execute a flat list of lines, handling multi-line constructs."""
        i = 0
        while i < len(lines):
            line = lines[i].strip()
            if not line or line.startswith("--"):
                i += 1
                continue
            # Merge multi-line table constructors: accumulate until braces balance.
            # Only merge when no block-introducing keyword starts the line.
            _block_kw = re.match(
                r"^(if|for|while|repeat|function|local\s+function)\b", line
            )
            if not _block_kw:
                brace_depth = line.count("{") - line.count("}")
                if brace_depth > 0:
                    merged = [line]
                    i += 1
                    while i < len(lines) and brace_depth > 0:
                        nxt = lines[i].strip()
                        merged.append(nxt)
                        brace_depth += nxt.count("{") - nxt.count("}")
                        i += 1
                    line = " ".join(merged)
                    # Execute merged line and continue (do not fall into block dispatch)
                    self._exec_stmt(line)
                    continue
            # Multi-line if
            if re.match(r"^if\b", line):
                i = self._exec_if(lines, i)
            elif re.match(r"^elseif\b|^else\b|^end\b", line):
                i += 1  # consumed by _exec_if
            elif re.match(r"^for\b", line):
                i = self._exec_for(lines, i)
            elif re.match(r"^while\b", line):
                i = self._exec_while(lines, i)
            elif re.match(r"^repeat\b", line):
                i = self._exec_repeat(lines, i)
            elif re.match(r"^function\b", line):
                i = self._define_function(lines, i)
            elif re.match(r"^local\s+function\b", line):
                i = self._define_function(lines, i)
            elif re.match(r"^return\b", line):
                ret_str = line[6:].strip()
                if ret_str:
                    parts = _split_commas(ret_str)
                    if len(parts) > 1:
                        val = tuple(self._eval_expr(p.strip()) for p in parts)
                    else:
                        val = self._eval_expr(ret_str)
                else:
                    val = None
                raise LuaReturn(val)
            elif re.match(r"^break\b", line):
                raise LuaBreak()
            else:
                self._exec_stmt(line)
                i += 1

    def _exec_inline(self, stmt: str):
        """Execute a single inline statement that may be break/return."""
        stmt = stmt.strip()
        if stmt == "break":
            raise LuaBreak()
        if stmt.startswith("return"):
            ret_str = stmt[6:].strip()
            if ret_str:
                parts = _split_commas(ret_str)
                if len(parts) > 1:
                    val = tuple(self._eval_expr(p.strip()) for p in parts)
                else:
                    val = self._eval_expr(ret_str)
            else:
                val = None
            raise LuaReturn(val)
        self._exec_stmt(stmt)

    @staticmethod
    def _split_inline_stmts(text: str) -> list[str]:
        """Split inline statements at depth-0 ``end`` boundaries.

        Example: ``"if x then y end z"`` → ``["if x then y end", "z"]``
        """
        parts: list[str] = []
        depth = 0
        start = 0
        i = 0
        n = len(text)
        while i < n:
            # word-boundary-safe keyword check helper
            def _kw_at(kw: str) -> bool:
                end_pos = i + len(kw)
                if text[i:end_pos] != kw:
                    return False
                if i > 0 and (text[i - 1].isalnum() or text[i - 1] == "_"):
                    return False
                if end_pos < n and (text[end_pos].isalnum() or text[end_pos] == "_"):
                    return False
                return True

            for kw in ("if", "for", "while", "function", "repeat", "do"):
                if _kw_at(kw):
                    depth += 1
                    break
            if _kw_at("end"):
                depth -= 1
                if depth == 0:
                    part = text[start : i + 3].strip()
                    if part:
                        parts.append(part)
                    start = i + 3
            i += 1
        remaining = text[start:].strip()
        if remaining:
            parts.append(remaining)
        return parts if parts else [text.strip()]

    def _collect_block_to_end(
        self, lines: list[str], start: int
    ) -> tuple[list[str], int]:
        """Collect lines until matching 'end', handling nesting."""
        depth = 1
        i = start
        body: list[str] = []
        while i < len(lines):
            stripped = lines[i].strip()
            # Single-line constructs like "if COND then STMT end" don't change depth
            if re.match(r"^(if|for|while)\b", stripped) and re.search(
                r"\bend\s*$", stripped
            ):
                # Self-contained line — depth unchanged
                pass
            else:
                if re.match(r"^(if|for|while|do|function)\b", stripped) or re.match(
                    r"^local\s+function\b", stripped
                ):
                    depth += 1
                if re.match(r"^end\b", stripped):
                    depth -= 1
                    if depth == 0:
                        return body, i + 1
            body.append(lines[i])
            i += 1
        return body, i

    def _exec_if(self, lines: list[str], start: int) -> int:
        """Execute if/elseif/else/end block. Returns index after 'end'."""
        i = start
        executed = False
        while i < len(lines):
            line = lines[i].strip()
            m = re.match(r"^if\s+(.+?)\s+then\s*(.*)$", line)
            m_elseif = re.match(r"^elseif\s+(.+?)\s+then\s*(.*)$", line)
            m_else = re.match(r"^else\b", line)
            m_end = re.match(r"^end\b", line)
            if m and not executed:
                cond = self._eval_expr(m.group(1))
                inline = m.group(2).strip()
                is_single_line = inline.endswith(" end") or inline == "end"
                if self._lua_truthy(cond):
                    executed = True
                    # Single-line if: "if COND then STMT end"
                    if is_single_line:
                        body_text = (
                            inline[:-4].strip() if inline.endswith(" end") else ""
                        )
                        if body_text:
                            self._exec_inline(body_text)
                        i += 1
                        continue
                    elif inline:
                        self._exec_inline(inline)
                elif is_single_line:
                    # Condition false, single-line — skip entire line
                    i += 1
                    continue
                i += 1
                # Collect until matching elseif/else/end at same depth
                depth = 0
                block: list[str] = []
                while i < len(lines):
                    s = lines[i].strip()
                    if re.match(r"^(if|for|while|do|function)\b", s):
                        depth += 1
                    if depth == 0 and (re.match(r"^(elseif|else|end)\b", s)):
                        break
                    if re.match(r"^end\b", s):
                        depth -= 1
                    block.append(lines[i])
                    i += 1
                if executed:
                    self._exec_block(block)
            elif m_elseif and not executed:
                cond = self._eval_expr(m_elseif.group(1))
                inline_elif = (
                    m_elseif.group(2).strip() if m_elseif.lastindex >= 2 else ""
                )
                if cond and cond != 0 and cond is not None and cond is not False:
                    executed = True
                    if inline_elif:
                        self._exec_inline(inline_elif)
                i += 1
                depth = 0
                block = []
                while i < len(lines):
                    s = lines[i].strip()
                    if re.match(r"^(if|for|while|do|function)\b", s):
                        depth += 1
                    if depth == 0 and re.match(r"^(elseif|else|end)\b", s):
                        break
                    if re.match(r"^end\b", s):
                        depth -= 1
                    block.append(lines[i])
                    i += 1
                if executed:
                    self._exec_block(block)
            elif m_else:
                # Extract optional inline body: "else STMT" on the same line
                else_inline = lines[i].strip()[4:].strip()  # strip "else"
                i += 1
                depth = 0
                block = []
                while i < len(lines):
                    s = lines[i].strip()
                    if re.match(r"^(if|for|while|do|function)\b", s):
                        depth += 1
                    if depth == 0 and re.match(r"^end\b", s):
                        break
                    if re.match(r"^end\b", s):
                        depth -= 1
                    block.append(lines[i])
                    i += 1
                if not executed:
                    if else_inline:
                        self._exec_inline(else_inline)
                    else:
                        self._exec_block(block)
            elif m_end:
                return i + 1
            else:
                break
        return i

    def _exec_for(self, lines: list[str], start: int) -> int:
        line = lines[start].strip()

        # Numeric for: for i = start, limit [, step] do [inline_body end]
        m = re.match(
            r"^for\s+(\w+)\s*=\s*(.+?),\s*(.+?)(?:,\s*(.+?))?\s+do\s*(.*)$", line
        )
        # Generic for: for k, v in pairs(t) do [inline_body end]
        m2 = None if m else re.match(r"^for\s+(.+?)\s+in\s+(.+?)\s+do\s*(.*)", line)

        # Check for single-line form: "for ... do STMT end"
        inline_body = None
        tail_src = (
            (m.group(5) if m else m2.group(3) if m2 else "").strip()
            if (m or m2)
            else ""
        )
        if tail_src and re.search(r"\bend\s*$", tail_src):
            stmt = re.sub(r"\bend\s*$", "", tail_src).strip()
            if stmt:
                inline_body = self._split_inline_stmts(stmt)

        if inline_body is not None:
            body = inline_body
            end_i = start + 1
        else:
            i = start + 1
            body, end_i = self._collect_block_to_end(lines, i)
        if m:
            var = m.group(1)
            frm = self._eval_expr(m.group(2))
            to = self._eval_expr(m.group(3))
            step = self._eval_expr(m.group(4)) if m.group(4) else 1
            current = frm
            try:
                while (step > 0 and current <= to) or (step < 0 and current >= to):
                    self._assign(var, current)
                    try:
                        self._exec_block(body)
                    except LuaBreak:
                        break
                    current += step
            except LuaReturn as r:
                raise r
        else:
            # Generic for: for k, v in pairs(t) do
            if m2:
                vars_ = [v.strip() for v in m2.group(1).split(",")]
                iterator = self._eval_expr(m2.group(2))
                items: Any  # may be dict_items, list, or enumerate
                if isinstance(iterator, dict):
                    items = iterator.items()
                elif isinstance(iterator, list):
                    # ipairs/pairs return list of tuples — use directly
                    if iterator and isinstance(iterator[0], tuple):
                        items = iterator
                    else:
                        items = enumerate(iterator, 1)
                else:
                    items = []
                for k, v in items:
                    if len(vars_) >= 1:
                        self._assign(vars_[0], k)
                    if len(vars_) >= 2:
                        self._assign(vars_[1], v)
                    try:
                        self._exec_block(body)
                    except LuaBreak:
                        break
        return end_i

    def _exec_while(self, lines: list[str], start: int) -> int:
        line = lines[start].strip()
        m = re.match(r"^while\s+(.+?)\s+do", line)
        i = start + 1
        body, end_i = self._collect_block_to_end(lines, i)
        if m:
            limit = 10000
            count = 0
            while self._lua_truthy(self._eval_expr(m.group(1))):
                if count >= limit:
                    self._emit("❌ Lua: while loop limit exceeded")
                    break
                try:
                    self._exec_block(body)
                except LuaBreak:
                    break
                count += 1
        return end_i

    def _exec_repeat(self, lines: list[str], start: int) -> int:
        """repeat ... until cond"""
        i = start + 1
        body: list[str] = []
        while i < len(lines):
            s = lines[i].strip()
            if re.match(r"^until\b", s):
                cond_str = s[5:].strip()
                break
            body.append(lines[i])
            i += 1
        else:
            return i
        limit = 10000
        count = 0
        while True:
            if count >= limit:
                self._emit("❌ Lua: repeat loop limit exceeded")
                break
            try:
                self._exec_block(body)
            except LuaBreak:
                break
            count += 1
            if self._lua_truthy(self._eval_expr(cond_str)):
                break
        return i + 1

    def _define_function(self, lines: list[str], start: int) -> int:
        line = lines[start].strip()
        m = re.match(r"^(?:local\s+)?function\s+(\w+)\s*\((.*?)\)\s*(.*)", line)
        # Check for single-line function: function f(x) return x*2 end
        inline_body = None
        if m and m.group(3):
            tail = m.group(3).strip()
            if re.search(r"\bend\s*$", tail):
                stmt = re.sub(r"\bend\s*$", "", tail).strip()
                if stmt:
                    inline_body = self._split_inline_stmts(stmt)
        if inline_body is not None:
            body = inline_body
            end_i = start + 1
        else:
            i = start + 1
            body, end_i = self._collect_block_to_end(lines, i)
        if m:
            fname = m.group(1)
            params = [p.strip() for p in m.group(2).split(",") if p.strip()]
            self._globals[fname] = LuaFunction(fname, params, body, self)
        return end_i

    # ------------------------------------------------------------------
    # Statement executor
    # ------------------------------------------------------------------

    def _exec_stmt(self, stmt: str):
        stmt = stmt.strip()
        if not stmt or stmt.startswith("--"):
            return
        # Strip inline comments (-- ...) that are outside of string literals
        stmt = _strip_lua_comment(stmt)

        # print(...)
        m = re.match(r"^print\s*\((.+)\)$", stmt)
        if m:
            val = self._eval_call_args(m.group(1))
            self._emit("\t".join(_lua_tostring(v) for v in val))
            return

        # local var = expr  (multiple assignment: local a, b = 1, 2)
        m = re.match(r"^local\s+([\w,\s]+?)\s*=\s*(.+)$", stmt)
        if m:
            var_names = [v.strip() for v in m.group(1).split(",")]
            rhs_parts = _split_commas(m.group(2))
            vals: list[Any] = []
            for rp in rhs_parts:
                v = self._eval_expr(rp.strip())
                if isinstance(v, tuple):
                    vals.extend(v)
                else:
                    vals.append(v)
            for i_v, vn in enumerate(var_names):
                self._assign(
                    vn.strip(), vals[i_v] if i_v < len(vals) else None, local=True
                )
            return

        # var = expr (assignment -- but not ==)
        m = re.match(r"^([\w.]+(?:\[.+?\])?)\s*=\s*(?!=)(.+)$", stmt)
        if m:
            self._do_assignment(m.group(1), m.group(2))
            return

        # bare function call
        self._eval_expr(stmt)

    def _do_assignment(self, lhs: str, rhs: str):
        val = self._eval_expr(rhs)
        # table field: t.key or t[idx]
        m = re.match(r"^(\w+)\[(.+)\]$", lhs)
        if m:
            tbl = self._resolve(m.group(1))
            if isinstance(tbl, dict):
                tbl[self._eval_expr(m.group(2))] = val
            return
        m = re.match(r"^(\w+)\.(\w+)$", lhs)
        if m:
            tbl = self._resolve(m.group(1))
            if isinstance(tbl, dict):
                tbl[m.group(2)] = val
            return
        self._assign(lhs, val)

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval_expr(self, expr: str) -> Any:
        expr = expr.strip()
        if not expr:
            return None
        # Anonymous function expression: function(params) BODY end
        m = re.match(r"^function\s*\((.*?)\)\s*(.+)\s+end$", expr)
        if m:
            params = [p.strip() for p in m.group(1).split(",") if p.strip()]
            body_str = m.group(2).strip()
            body_lines = body_str.split("\n") if "\n" in body_str else [body_str]
            return LuaFunction("<anon>", params, body_lines, self)
        # nil / true / false
        if expr == "nil":
            return None
        if expr == "true":
            return True
        if expr == "false":
            return False
        # String literal
        m = re.match(r'^"((?:[^"\\]|\\.)*)"$', expr)
        if m:
            return (
                m.group(1).replace("\\n", "\n").replace("\\t", "\t").replace('\\"', '"')
            )
        m = re.match(r"^'((?:[^'\\]|\\.)*)'$", expr)
        if m:
            return (
                m.group(1).replace("\\n", "\n").replace("\\t", "\t").replace("\\'", "'")
            )
        # Long string [[...]]
        m = re.match(r"^\[\[(.+?)\]\]$", expr, re.DOTALL)
        if m:
            return m.group(1)
        # Number
        try:
            if "." in expr:
                return float(expr)
            return int(expr)
        except ValueError:
            pass
        # Unary not
        m = re.match(r"^not\s+(.+)$", expr)
        if m:
            return not self._lua_truthy(self._eval_expr(m.group(1)))
        # Unary minus
        m = re.match(r"^-\s*(\w.*)$", expr)
        if m:
            val = self._eval_expr(m.group(1))
            return -val if val is not None else 0
        # String length #
        if expr.startswith("#"):
            v = self._eval_expr(expr[1:].strip())
            if isinstance(v, str):
                return len(v)
            if isinstance(v, (list, dict)):
                return len(v)
            return 0
        # Binary operators (handle lowest precedence first)
        # We use a simple tokeniser for +, -, *, /, //, %, ^, .., comparisons, and/or
        result = self._parse_expr(expr)
        if result is not None:
            return result
        # Function call: name(args)
        m = re.match(r"^([\w.]+)\s*\((.*)?\)$", expr)
        if m:
            return self._call_function(m.group(1), m.group(2) or "")
        # Method call: obj:method(args)
        m = re.match(r"^(\w+):(\w+)\s*\((.*)?\)$", expr)
        if m:
            obj = self._resolve(m.group(1))
            method = m.group(2)
            if hasattr(obj, method):
                args = self._eval_call_args(m.group(3) or "")
                return getattr(obj, method)(*args)
        # Table access: t[k] or t.field
        m = re.match(r"^(\w+)\[(.+)\]$", expr)
        if m:
            tbl_val = self._resolve(m.group(1))
            key = self._eval_expr(m.group(2))
            if isinstance(tbl_val, (dict, list)):
                if isinstance(tbl_val, list) and isinstance(key, int):
                    return tbl_val[key - 1] if 1 <= key <= len(tbl_val) else None
                if isinstance(tbl_val, dict):
                    return tbl_val.get(key)
            return None
        m = re.match(r"^(\w+)\.(\w+)$", expr)
        if m:
            obj = self._resolve(m.group(1))
            if isinstance(obj, dict):
                return obj.get(m.group(2))
            return getattr(obj, m.group(2), None)
        # Table constructor {}
        if expr.startswith("{") and expr.endswith("}"):
            inner = expr[1:-1].strip()
            if not inner:
                return {}
            # key=value or just value list
            tbl: dict[Any, Any] = {}
            idx = 1
            for item in _split_commas(inner):
                item = item.strip()
                # [expr] = value  (Lua table constructor with arbitrary key)
                m_bk = re.match(r"^\[(.+)\]\s*=\s*(.+)$", item)
                if m_bk:
                    key = self._eval_expr(m_bk.group(1))
                    tbl[key] = self._eval_expr(m_bk.group(2))
                    continue
                # name = value
                m2 = re.match(r"^(\w+)\s*=\s*(.+)$", item)
                if m2:
                    tbl[m2.group(1)] = self._eval_expr(m2.group(2))
                else:
                    tbl[idx] = self._eval_expr(item)
                    idx += 1
            return tbl
        # Variable look-up
        return self._resolve(expr)

    def _parse_expr(self, expr: str) -> Any:
        """Try to parse binary-operator expressions."""
        for op in [
            " or ",
            " and ",
            "==",
            "~=",
            "<=",
            ">=",
            "<",
            ">",
            " .. ",
            "+",
            "-",
            "*",
            "//",
            "/",
            "%",
            "^",
        ]:
            idx = _find_operator(expr, op)
            if idx != -1:
                lhs = self._eval_expr(expr[:idx].strip())
                rhs = self._eval_expr(expr[idx + len(op) :].strip())
                return _apply_op(lhs, op.strip(), rhs)
        return None

    def _eval_call_args(self, args_str: str) -> list:
        if not args_str.strip():
            return []
        return [self._eval_expr(a.strip()) for a in _split_commas(args_str)]

    def _call_function(self, name: str, args_str: str) -> Any:
        # Built-in functions
        args = self._eval_call_args(args_str)
        builtins = {
            "tostring": lambda a: _lua_tostring(a[0]) if a else "nil",
            "tonumber": lambda a: _to_number(a[0]) if a else None,
            "type": lambda a: _lua_type(a[0]) if a else "nil",
            "ipairs": lambda a: (
                list(
                    enumerate(
                        a[0] if isinstance(a[0], list) else list(a[0].values()), 1
                    )
                )
                if a and isinstance(a[0], (list, dict))
                else []
            ),
            "pairs": lambda a: (
                list(a[0].items() if isinstance(a[0], dict) else enumerate(a[0], 1))
                if a
                else []
            ),
            "next": lambda a: (
                next(iter(a[0].items()))
                if a and isinstance(a[0], dict) and a[0]
                else None
            ),
            "rawget": lambda a: (
                a[0].get(a[1]) if len(a) >= 2 and isinstance(a[0], dict) else None
            ),
            "rawset": lambda a: (
                _lua_rawset(a[0], a[1], a[2])
                if len(a) >= 3 and isinstance(a[0], dict)
                else None
            ),
            "rawequal": lambda a: a[0] is a[1] if len(a) >= 2 else False,
            "rawlen": lambda a: len(a[0]) if a else 0,
            "unpack": lambda a: (
                a[0]
                if a and isinstance(a[0], list)
                else (list(a[0]) if a and isinstance(a[0], dict) else a)
            ),
            "table.unpack": lambda a: a[0] if a and isinstance(a[0], list) else a,
            "select": lambda a: (
                len(a) - 1
                if a and a[0] == "#"
                else (args[int(a[0])] if len(a) > 1 else None)
            ),
            "assert": lambda a: (
                a[0]
                if (a and a[0] is not None and a[0] is not False)
                else (_ for _ in ()).throw(
                    LuaError(str(a[1]) if len(a) > 1 else "assertion failed")
                )
            ),
            "error": lambda a: (_ for _ in ()).throw(
                LuaError(str(a[0]) if a else "error")
            ),
            "print": lambda a: self._emit("\t".join(_lua_tostring(v) for v in a))
            or None,
            "require": lambda a: self._emit(
                f"ℹ️ require('{a[0]}') not supported in sandbox"
            )
            or None,
            "pcall": lambda a: self._lua_pcall(a),
            "xpcall": lambda a: self._lua_xpcall(a),
            "load": lambda a: (
                (lambda src: LuaLoadFunction(str(a[0]), self))(a[0]) if a else None
            ),
            "loadstring": lambda a: LuaLoadFunction(str(a[0]), self) if a else None,
            "dofile": lambda a: self._emit("ℹ️ dofile not supported in sandbox")
            or None,
            "collectgarbage": lambda a: 0,
            "setmetatable": lambda a: a[0] if a else None,
            "getmetatable": lambda a: None,
            "setfenv": lambda a: None,
            "getfenv": lambda a: {},
            "module": lambda a: None,
            "newproxy": lambda a: {},
            "string.format": lambda a: (
                a[0] % tuple(a[1:]) if len(a) > 1 else str(a[0]) if a else ""
            ),
            "math.random": lambda a: self._resolve("math").random(
                a[0] if a else None, a[1] if len(a) > 1 else None
            ),
            "math.randomseed": lambda a: None,
            "math.huge": lambda a: float("inf"),
            "math.pi": lambda a: math.pi,
            "math.tointeger": lambda a: (
                int(a[0])
                if a and isinstance(a[0], (int, float)) and float(a[0]).is_integer()
                else None
            ),
            "math.type": lambda a: (
                "integer"
                if isinstance(a[0], int)
                else ("float" if isinstance(a[0], float) else "fail") if a else "fail"
            ),
        }
        if name in builtins:
            return builtins[name](args)
        # Standard library object method: math.sin etc.
        if "." in name:
            parts = name.split(".", 1)
            obj = self._resolve(parts[0])
            if obj is not None and hasattr(obj, parts[1]):
                return getattr(obj, parts[1])(*args)
        # User-defined function
        fn = self._resolve(name)
        if isinstance(fn, LuaFunction):
            return fn.call(args)
        if callable(fn):
            return fn(*args)
        return None

    def _lua_truthy(self, val: Any) -> bool:
        return val is not None and val is not False

    def _lua_pcall(self, args: list) -> list:
        if not args:
            return [False, "no function"]
        fn = args[0]
        fn_args = args[1:]
        try:
            if isinstance(fn, LuaFunction):
                result = fn.call(fn_args)
            elif callable(fn):
                result = fn(*fn_args)
            else:
                return [False, "attempt to call a non-function"]
            return [True, result]
        except LuaError as e:
            return [False, str(e)]
        except Exception as e:
            return [False, str(e)]

    def _lua_xpcall(self, args: list) -> list:
        if len(args) < 2:
            return [False, "no function"]
        fn, handler = args[0], args[1]
        fn_args = args[2:]
        try:
            if isinstance(fn, LuaFunction):
                result = fn.call(fn_args)
            elif callable(fn):
                result = fn(*fn_args)
            else:
                return [False, "not a function"]
            return [True, result]
        except Exception as e:
            msg = str(e)
            try:
                hmsg = handler(msg) if callable(handler) else msg
            except Exception:
                hmsg = msg
            return [False, hmsg]


# ---------------------------------------------------------------------------
# Standard library stubs
# ---------------------------------------------------------------------------


class LuaMathLib:
    pi = math.pi
    huge = float("inf")
    maxinteger = 2**63 - 1
    mininteger = -(2**63)

    def __getattr__(self, name: str):
        return getattr(math, name, None)

    def random(self, a=None, b=None):
        import random as _random

        if a is None:
            return _random.random()
        if b is None:
            return _random.randint(1, int(a))
        return _random.randint(int(a), int(b))

    def randomseed(self, seed=None):
        import random as _random

        _random.seed(seed)

    def type(self, x):
        if isinstance(x, int):
            return "integer"
        if isinstance(x, float):
            return "float"
        return "fail"

    def tointeger(self, x):
        if isinstance(x, int):
            return x
        if isinstance(x, float) and x.is_integer():
            return int(x)
        return None

    def fmod(self, x, y):
        return math.fmod(x, y)

    def modf(self, x):
        return math.modf(x)

    def max(self, *a):
        return max(a)

    def min(self, *a):
        return min(a)

    def abs(self, x):
        return abs(x)


class LuaStringLib:
    def len(self, s):
        return len(s)

    def sub(self, s, i, j=None):
        if j is None:
            return s[i - 1 :]
        return s[i - 1 : j]

    def upper(self, s):
        return s.upper()

    def lower(self, s):
        return s.lower()

    def rep(self, s, n):
        return s * int(n)

    def reverse(self, s):
        return s[::-1]

    def byte(self, s, i=1):
        return ord(s[i - 1]) if s else 0

    def char(self, *args):
        return "".join(chr(int(a)) for a in args)

    def find(self, s, pattern, init=1, plain=False):
        if plain:
            idx = s.find(pattern, init - 1)
            if idx == -1:
                return None
            return idx + 1, idx + len(pattern)
        m = re.search(pattern, s[init - 1 :])
        if m:
            return m.start() + init, m.end() + init - 1
        return None

    def format(self, fmt, *args):
        return fmt % args if args else fmt

    def gsub(self, s, pattern, repl, n=None):
        count = [0]

        def replacer(m):
            if n and count[0] >= n:
                return m.group(0)
            count[0] += 1
            if callable(repl):
                return str(repl(m.group(0)))
            return repl

        result = re.sub(pattern, replacer, s)
        return result, count[0]

    def match(self, s, pattern, init=1):
        m = re.match(pattern, s[init - 1 :])
        if not m:
            return None
        groups = m.groups()
        return groups[0] if len(groups) == 1 else (groups if groups else m.group(0))

    def gmatch(self, s, pattern):
        return re.findall(pattern, s)


class LuaTableLib:
    def insert(self, tbl, pos_or_val=None, val=None):
        if isinstance(tbl, list):
            if val is None:
                tbl.append(pos_or_val)
            else:
                tbl.insert(int(pos_or_val) - 1, val)
        elif isinstance(tbl, dict):
            if val is None:
                idx = max((k for k in tbl if isinstance(k, int)), default=0) + 1
                tbl[idx] = pos_or_val
            else:
                tbl[int(pos_or_val)] = val

    def remove(self, tbl, pos=None):
        if isinstance(tbl, list):
            if pos is None:
                return tbl.pop() if tbl else None
            return tbl.pop(int(pos) - 1)
        if isinstance(tbl, dict) and pos is not None:
            return tbl.pop(int(pos), None)

    def concat(self, tbl, sep="", i=1, j=None):
        if isinstance(tbl, list):
            start, end = int(i) - 1, j
            vals = [str(v) for v in (tbl[start:end] if end else tbl[start:])]
        elif isinstance(tbl, dict):
            keys = sorted(k for k in tbl if isinstance(k, int))
            vals = [str(tbl[k]) for k in keys]
        else:
            vals = [str(v) for v in tbl]
        return sep.join(vals)

    def sort(self, tbl, comp=None):
        if isinstance(tbl, list):
            if comp and callable(comp):
                import functools

                tbl.sort(key=functools.cmp_to_key(lambda a, b: comp(a, b)))
            else:
                tbl.sort()

    def move(self, a1, f, e, t, a2=None):
        target = a2 if a2 is not None else a1
        chunk = (a1 if isinstance(a1, list) else list(a1.values()))[int(f) - 1 : int(e)]
        for i, v in enumerate(chunk):
            if isinstance(target, list):
                idx = int(t) - 1 + i
                while idx >= len(target):
                    target.append(None)
                target[idx] = v
            else:
                target[int(t) + i] = v
        return target

    def unpack(self, tbl, i=1, j=None):
        if isinstance(tbl, list):
            return tbl[int(i) - 1 : j]
        return list(tbl.values()) if isinstance(tbl, dict) else []

    def pack(self, *args):
        t = list(args)
        return t


class LuaIOLib:
    def __init__(self, env: "LuaEnvironment"):
        self.env = env
        self._open_files: dict = {}
        self.stdout = self
        self.stdin = self
        self.stderr = self

    def write(self, *args):
        self.env._emit("".join(str(a) for a in args))

    def read(self, fmt="*l"):
        return (
            self.env.interpreter.request_input("lua> ")
            if hasattr(self.env.interpreter, "request_input")
            else ""
        )

    def lines(self, filename=None):
        if filename:
            try:
                with open(filename) as f:
                    return f.readlines()
            except OSError:
                return []
        return []

    def open(self, filename, mode="r"):
        try:
            f = open(filename, mode)
            self._open_files[filename] = f
            return LuaFileHandle(f)
        except OSError as e:
            return None, str(e)

    def close(self, *args):
        pass

    def popen(self, cmd, mode="r"):
        return self

    def tmpfile(self):
        return self

    def type(self, obj):
        if isinstance(obj, LuaFileHandle):
            return "file"
        return "file"


class LuaFileHandle:
    def __init__(self, f):
        self._f = f

    def read(self, fmt="*l"):
        if fmt in ("*l", "l"):
            return self._f.readline().rstrip("\n")
        if fmt in ("*n", "n"):
            try:
                return float(self._f.readline())
            except:
                return None
        if fmt in ("*a", "a"):
            return self._f.read()
        return self._f.readline()

    def write(self, *args):
        for a in args:
            self._f.write(str(a))
        return self

    def lines(self):
        return self._f.readlines()

    def close(self):
        self._f.close()

    def seek(self, whence="cur", offset=0):
        modes = {"set": 0, "cur": 1, "end": 2}
        return self._f.seek(offset, modes.get(whence, 1))


class LuaOSLib:
    def time(self, *a):
        import time as _time

        return int(_time.time())

    def clock(self):
        import time as _time

        return _time.process_time()

    def date(self, fmt="%c", t=None):
        import time as _time

        t = t or _time.time()
        if fmt.startswith("!"):
            fmt = fmt[1:]
        if fmt == "*t":
            lt = _time.localtime(t)
            return {
                "year": lt.tm_year,
                "month": lt.tm_mon,
                "day": lt.tm_mday,
                "hour": lt.tm_hour,
                "min": lt.tm_min,
                "sec": lt.tm_sec,
                "wday": lt.tm_wday + 1,
                "yday": lt.tm_yday,
                "isdst": lt.tm_isdst,
            }
        return _time.strftime(fmt, _time.localtime(t)) if fmt else _time.ctime(t)

    def difftime(self, t2, t1):
        return t2 - t1

    def exit(self, code=True):
        pass

    def getenv(self, name):
        import os as _os

        return _os.environ.get(name)

    def execute(self, cmd=None):
        return True, "exit", 0

    def tmpname(self):
        import tempfile

        return tempfile.mktemp()

    def rename(self, old, new):
        try:
            import os as _os

            _os.rename(old, new)
            return True
        except OSError as e:
            return None, str(e)

    def remove(self, name):
        try:
            import os as _os

            _os.remove(name)
            return True
        except OSError as e:
            return None, str(e)


class LuaCoroutineLib:
    """Simplified coroutine stub — full coroutines need Python greenlets/native async."""

    def create(self, f):
        return LuaCoroutine(f)

    def wrap(self, f):
        co = LuaCoroutine(f)

        def wrapper(*args):
            ok, val = co.resume(*args)
            if not ok:
                raise LuaError(val)
            return val

        return wrapper

    def resume(self, co, *args):
        if isinstance(co, LuaCoroutine):
            return co.resume(*args)
        return False, "cannot resume non-coroutine"

    def yield_(self, *args):
        raise LuaYield(args)

    def status(self, co):
        if isinstance(co, LuaCoroutine):
            return co.status
        return "dead"

    def isyieldable(self):
        return False

    def running(self):
        return None, True

    def __getattr__(self, name):
        if name == "yield":
            return self.yield_
        raise AttributeError(name)


class LuaCoroutine:
    def __init__(self, f):
        self.f = f
        self.status = "suspended"
        self._result = None

    def resume(self, *args):
        if self.status == "dead":
            return False, "cannot resume dead coroutine"
        self.status = "running"
        try:
            if callable(self.f):
                result = self.f(*args)
            else:
                result = None
            self.status = "dead"
            return True, result
        except LuaYield as y:
            self.status = "suspended"
            return True, y.values[0] if y.values else None
        except Exception as e:
            self.status = "dead"
            return False, str(e)


class LuaYield(Exception):
    def __init__(self, values):
        self.values = values


class LuaLoadFunction:
    """Result of load()/loadstring() — callable chunk."""

    def __init__(self, src: str, env: "LuaEnvironment"):
        self.src = src
        self.env = env

    def __call__(self, *args):
        try:
            return self.env.run(self.src)
        except Exception as e:
            return None, str(e)


# ---------------------------------------------------------------------------
# User-defined function
# ---------------------------------------------------------------------------


class LuaFunction:
    def __init__(
        self, name: str, params: list[str], body: list[str], env: "LuaEnvironment"
    ):
        self.name = name
        self.params = params
        self.body = body
        self.env = env

    def call(self, args: list) -> Any:
        frame: dict[str, Any] = {}
        for i, p in enumerate(self.params):
            frame[p] = args[i] if i < len(args) else None
        self.env._call_stack.append(frame)
        result = None
        try:
            self.env._exec_block(self.body)
        except LuaReturn as r:
            result = r.value
        finally:
            self.env._call_stack.pop()
        return result


# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------


class LuaError(Exception):
    pass


class LuaReturn(Exception):
    def __init__(self, value):
        self.value = value


class LuaBreak(Exception):
    pass


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _lua_rawset(d: dict, key: Any, value: Any) -> dict:
    """Set key in dict and return the dict (rawset semantics)."""
    d[key] = value
    return d


def _lua_tostring(v: Any) -> str:
    if v is None:
        return "nil"
    if v is True:
        return "true"
    if v is False:
        return "false"
    if isinstance(v, float) and v.is_integer():
        return str(int(v))
    return str(v)


def _strip_lua_comment(stmt: str) -> str:
    """Strip a trailing inline Lua comment (-- ...) that's outside string literals."""
    in_str = None
    i = 0
    while i < len(stmt):
        ch = stmt[i]
        if in_str:
            if ch == in_str and (i == 0 or stmt[i - 1] != "\\"):
                in_str = None
        elif ch in ('"', "'"):
            in_str = ch
        elif ch == "-" and i + 1 < len(stmt) and stmt[i + 1] == "-":
            return stmt[:i].rstrip()
        i += 1
    return stmt


def _split_commas(s: str) -> list[str]:
    """Split on commas, respecting parentheses, brackets and strings."""
    parts = []
    depth = 0
    current = []
    in_str = None
    for ch in s:
        if in_str:
            current.append(ch)
            if ch == in_str:
                in_str = None
        elif ch in ('"', "'"):
            in_str = ch
            current.append(ch)
        elif ch in "([{":
            depth += 1
            current.append(ch)
        elif ch in ")]}":
            depth -= 1
            current.append(ch)
        elif ch == "," and depth == 0:
            parts.append("".join(current))
            current = []
        else:
            current.append(ch)
    if current:
        parts.append("".join(current))
    return parts


def _find_operator(expr: str, op: str) -> int:
    """Find op in expr at depth 0, outside strings. Returns index or -1."""
    depth = 0
    in_str = None
    i = 0
    while i < len(expr):
        ch = expr[i]
        if in_str:
            if ch == in_str:
                in_str = None
        elif ch in ('"', "'"):
            in_str = ch
        elif ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        elif depth == 0 and expr[i : i + len(op)] == op:
            # Avoid matching -- comment prefix as subtraction
            if op == "-" and i > 0 and expr[i - 1] == "-":
                pass
            else:
                return i
        i += 1
    return -1


def _apply_op(lhs: Any, op: str, rhs: Any) -> Any:
    if op == "+":
        if isinstance(lhs, str) or isinstance(rhs, str):
            return str(lhs) + str(rhs)
        return lhs + rhs
    if op == "-":
        return lhs - rhs
    if op == "*":
        return lhs * rhs
    if op == "/":
        return lhs / rhs if rhs else 0
    if op == "//":
        return lhs // rhs if rhs else 0
    if op == "%":
        return lhs % rhs if rhs else 0
    if op == "^":
        return lhs**rhs
    if op == "..":
        return str(lhs) + str(rhs)
    if op == "==":
        return lhs == rhs
    if op == "~=":
        return lhs != rhs
    if op == "<":
        return lhs < rhs
    if op == ">":
        return lhs > rhs
    if op == "<=":
        return lhs <= rhs
    if op == ">=":
        return lhs >= rhs
    if op == "and":
        return rhs if lhs else lhs
    if op == "or":
        return lhs if lhs else rhs
    return None


def _to_number(v: Any) -> Any:
    try:
        if isinstance(v, str):
            return int(v) if "." not in v else float(v)
        return v
    except (ValueError, TypeError):
        return None


def _lua_type(v: Any) -> str:
    if v is None:
        return "nil"
    if isinstance(v, bool):
        return "boolean"
    if isinstance(v, int):
        return "number"
    if isinstance(v, float):
        return "number"
    if isinstance(v, str):
        return "string"
    if isinstance(v, dict):
        return "table"
    if callable(v):
        return "function"
    return "userdata"
