"""HyperTalk language executor for Time Warp Studio.

Educational HyperTalk interpreter — whole-program execution.
Inspired by Apple HyperCard's HyperTalk scripting language.

Supports:
  put value into var / put value after var / put value before var
  get expression (stores in "it")
  set property to value
  if expr then ... [else ...] end if
  repeat n times / repeat with var = i to n / repeat while/until
  end repeat
  on handlerName ... end handlerName
  answer text  -- display text (appended to output)
  say text     -- print
  ask prompt   -- input (stores in "it")
  find [whole|partial|string|chars|word] text [in container]
  go [to] card/next/prev/first/last  -- navigation (logged, no-op in desktop)
  global var   -- declare global
  return value
  exit to HyperCard / exit repeat / next repeat  -- flow control
  add n to var / subtract n from var
  multiply var by n / divide var by n
  do expression  -- runtime evaluation
  wait n seconds / wait n ticks
  sort container by expression
  visual effect name [speed]  -- display transition name
  Arithmetic: +, -, *, /, mod, div, ^, **
  Comparisons: =, <>, <, >, <=, >=, is, is not, contains, is in
  String: char i of x, word i of x, line i of x, item i of x
  Chunk ranges: char i to j of x, word i to j of x
  Built-in properties: the date, the time, the ticks, the seconds
  Functions: sin, cos, sqrt, abs, round, trunc, random, length, offset
             numToChar, charToNum, max, min, average, sum, the result
"""

from __future__ import annotations

import math
import random
import re
import time as _time
from datetime import datetime as _datetime
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


def execute_hypertalk(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a HyperTalk program and return all output."""
    env = HyperTalkEnvironment(interpreter, turtle)
    return env.run(source)


class HyperTalkEnvironment:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []
        self._vars: dict[str, Any] = {"_card_count": 1, "_current_card": 1}
        self._globals: dict[str, Any] = {}
        self._handlers: dict[str, list[str]] = {}
        self._handler_params: dict[str, list[str]] = {}
        self._it: Any = ""

    def _emit(self, text: str):
        self._output.append(str(text))

    def _set_var(self, name: str, value: Any):
        name = name.lower()
        if name in self._globals:
            self._globals[name] = value
        else:
            self._vars[name] = value

    def _get_var(self, name: str) -> Any:
        name = name.lower()
        if name == "it":
            return self._it
        if name in self._vars:
            return self._vars[name]
        if name in self._globals:
            return self._globals[name]
        return name  # unbound returns capitalised name (HyperTalk convention)

    def run(self, source: str) -> str:
        try:
            # Join line continuations (¬ or \ at end of line)
            source = re.sub(r'[ \t]*[¬\\][ \t]*\n[ \t]*', ' ', source)
            lines = source.splitlines()
            self._scan_handlers(lines)
            output_before = len(self._output)
            self._exec_lines(lines, 0, len(lines))
            # Auto-call entry-point handler if no top-level output was produced
            if len(self._output) == output_before:
                for ep in ("startup", "openstack", "opencard", "run", "main", "initialize"):
                    if ep in self._handlers:
                        self._exec_lines(self._handlers[ep], 0, len(self._handlers[ep]))
                        break
        except HTExit:
            pass
        except HTError as e:
            self._emit(f"❌ HyperTalk error: {e}")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}")
        return "\n".join(self._output)

    def _scan_handlers(self, lines: list[str]):
        i = 0
        while i < len(lines):
            m = re.match(r"^\s*(?:on|function)\s+(\w+)(.*)$", lines[i], re.IGNORECASE)
            if m:
                name = m.group(1).lower()
                params = [p.strip() for p in m.group(2).strip().split(",") if p.strip()]
                # Also split space-separated params
                if len(params) == 1 and " " in params[0]:
                    params = params[0].split()
                self._handler_params[name] = params
                body = []
                i += 1
                while i < len(lines) and not re.match(
                    r"^\s*end\s+" + re.escape(name), lines[i], re.IGNORECASE
                ):
                    body.append(lines[i])
                    i += 1
                self._handlers[name] = body
            i += 1

    def _exec_lines(self, lines: list[str], start: int, end: int) -> Any:
        i = start
        while i < end:
            line = lines[i].strip()
            if not line or line.startswith("--"):
                i += 1
                continue

            # Skip on/function...end handler blocks in main flow
            m = re.match(r"^(?:on|function)\s+(\w+)", line, re.IGNORECASE)
            if m:
                # Skip to matching "end name"
                name = m.group(1).lower()
                i += 1
                while i < end and not re.match(
                    r"^end\s+" + re.escape(name), lines[i].strip(), re.IGNORECASE
                ):
                    i += 1
                i += 1
                continue

            result = self._exec_stmt(line, lines, i, end)
            if isinstance(result, tuple):
                cmd, val = result
                if cmd == "GOTO":
                    i = val
                elif cmd == "RETURN":
                    return val
                elif cmd == "BREAK":
                    return ("BREAK", None)
                else:
                    i += 1
            else:
                i += 1
        return None

    def _exec_stmt(self, stmt: str, lines: list[str], ip: int, block_end: int) -> Any:
        stmt_lower = stmt.lower()

        # ELSE IF — strip "else" prefix (used in else-if chains)
        m = re.match(r"^else\s+(if\s+.+)$", stmt, re.IGNORECASE)
        if m:
            return self._exec_stmt(m.group(1), lines, ip, block_end)

        # CALL handlerName [, arg1, arg2, ...]
        m = re.match(r"^call\s+(\w+)(?:\s*,\s*(.+))?$", stmt, re.IGNORECASE)
        if m:
            name_c = m.group(1).lower()
            args_str_c = (m.group(2) or "").strip()
            if name_c in self._handlers:
                params_c = self._handler_params.get(name_c, [])
                arg_vals_c = self._split_args(args_str_c) if args_str_c else []
                saved_c = dict(self._vars)
                for pi, pname in enumerate(params_c):
                    hval: Any = self._eval(arg_vals_c[pi]) if pi < len(arg_vals_c) else None
                    self._set_var(pname, hval)
                result_c = self._exec_lines(self._handlers[name_c], 0, len(self._handlers[name_c]))
                self._vars = saved_c
                if result_c is not None:
                    self._it = result_c
            return None

        # GO TO CARD / navigation
        m = re.match(r"^go\s+(?:to\s+)?(.+)$", stmt, re.IGNORECASE)
        if m:
            dest = m.group(1).strip().lower()
            total = max(1, int(float(self._get_var("_card_count") or 1)))
            current = max(1, int(float(self._get_var("_current_card") or 1)))
            dm = re.match(r"^card\s+(.+)$", dest, re.IGNORECASE)
            if dm:
                try:
                    new_card = int(float(self._eval(dm.group(1).strip())))
                    self._set_var("_current_card", max(1, min(new_card, total)))
                except Exception:
                    pass
            elif re.match(r"^first\b", dest, re.IGNORECASE):
                self._set_var("_current_card", 1)
            elif re.match(r"^last\b", dest, re.IGNORECASE):
                self._set_var("_current_card", total)
            elif re.match(r"^next\b", dest, re.IGNORECASE):
                self._set_var("_current_card", min(current + 1, total))
            elif re.match(r"^prev(?:ious)?\b", dest, re.IGNORECASE):
                self._set_var("_current_card", max(current - 1, 1))
            return None

        # FIND text
        m = re.match(r"^find\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            self._emit(f"ℹ️ Find: {self._eval(m.group(1).strip())}")
            return None

        # FLASH n
        m = re.match(r"^flash\s+(\d+)$", stmt, re.IGNORECASE)
        if m:
            self._emit(f"✨ Flash {m.group(1)}")
            return None

        # DOMENU menuItem
        m = re.match(r"^doMenu\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            return None  # silently ignore menu operations

        # STUBS: select, print, delete, save, ask file
        if re.match(r"^(?:select|print|delete|save)\b", stmt_lower):
            return None
        if re.match(r"^ask\s+file\b", stmt_lower):
            self._it = ""
            return None

        # PUT value INTO/AFTER/BEFORE field "name"
        m = re.match(
            r'^put\s+(.+?)\s+(into|after|before)\s+field\s+["\']([^"\']+)["\']$',
            stmt, re.IGNORECASE
        )
        if m:
            val = str(self._eval(m.group(1).strip()))
            op = m.group(2).lower()
            fld = "_field_" + re.sub(r"\W+", "_", m.group(3).lower())
            cur = str(self._get_var(fld))
            if op == "into":
                self._set_var(fld, val)
            elif op == "after":
                self._set_var(fld, cur + val)
            elif op == "before":
                self._set_var(fld, val + cur)
            return None

        # PUT value INTO/AFTER/BEFORE var
        m = re.match(
            r"^put\s+(.+?)\s+(into|after|before)\s+(\w+)$", stmt, re.IGNORECASE
        )
        if m:
            val = str(self._eval(m.group(1).strip()))
            op = m.group(2).lower()
            var = m.group(3)
            if op == "into":
                self._set_var(var, val)
            elif op == "after":
                self._set_var(var, str(self._get_var(var)) + val)
            elif op == "before":
                self._set_var(var, val + str(self._get_var(var)))
            return None

        # PUT value (store to "it" and display in message box)
        m = re.match(r"^put\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            val = self._eval(m.group(1).strip())
            self._it = val
            self._emit(str(val))
            return None

        # GET expression
        m = re.match(r"^get\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            self._it = self._eval(m.group(1).strip())
            return None

        # SET var TO value
        m = re.match(r"^set\s+(\w+)\s+to\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            self._set_var(m.group(1), self._eval(m.group(2).strip()))
            return None

        # GLOBAL
        m = re.match(r"^global\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            for v in m.group(1).split(","):
                v = v.strip().lower()
                if v not in self._globals:
                    self._globals[v] = ""
            return None

        # ANSWER expr WITH btn1 [OR btn2 ...] — dialog with buttons
        m = re.match(r"^(?:answer|say)\s+(.+?)\s+with\s+(.+?)(?:\s+or\s+.+)?$", stmt, re.IGNORECASE)
        if m:
            self._emit(str(self._eval(m.group(1).strip())))
            self._it = str(self._eval(m.group(2).strip()))
            return None

        # ANSWER / SAY
        m = re.match(r"^(?:answer|say)\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            self._emit(str(self._eval(m.group(1).strip())))
            return None

        # ASK expr [WITH default]
        m = re.match(r"^ask\s+(.+?)\s+with\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            default = str(self._eval(m.group(2).strip()))
            if hasattr(self.interpreter, "request_input"):
                self._it = self.interpreter.request_input(str(self._eval(m.group(1).strip())))
            else:
                self._it = default
            return None

        # ASK (plain)
        m = re.match(r"^ask\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            if hasattr(self.interpreter, "request_input"):
                self._it = self.interpreter.request_input(str(self._eval(m.group(1).strip())))
            else:
                self._it = ""
            return None

        # FIND [whole | partial | string | chars] <text> [in <field>]
        # Sets 'it' to the found text (or empty string if not found).
        m = re.match(
            r"^find\s+(?:(?:whole|partial|string|chars|word)\s+)?"
            r"(.+?)(?:\s+in\s+(\w+))?$",
            stmt, re.IGNORECASE,
        )
        if m:
            needle = str(self._eval(m.group(1).strip())).strip('"\'')
            container_name = m.group(2)
            haystack = (
                str(self._get_var(container_name)) if container_name else
                "\n".join(str(s) for s in self._output)
            )
            idx = haystack.lower().find(needle.lower())
            if idx >= 0:
                self._it = haystack[idx: idx + len(needle)]
                self._emit(f"ℹ️ Found: {needle!r}")
            else:
                self._it = ""
                self._emit(f"ℹ️ Not found: {needle!r}")
            return None

        # GO TO CARD <name or number> / GO [BACK|NEXT|PREV|FIRST|LAST]
        m = re.match(r"^go(?:\s+to)?\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            dest = m.group(1).strip().lower()
            self._emit(f"ℹ️ Navigation: go to {dest}")
            return None

        # RETURN
        m = re.match(r"^return\s*(.*)", stmt, re.IGNORECASE)
        if m:
            retval = self._eval(m.group(1).strip()) if m.group(1).strip() else None
            return ("RETURN", retval)

        # EXIT
        if re.match(r"^exit\s+repeat\b", stmt_lower):
            return ("BREAK", None)
        if re.match(r"^next\s+repeat\b", stmt_lower):
            return ("CONTINUE", None)
        if re.match(r"^exit\b", stmt_lower):
            raise HTExit()

        # IF ... THEN [inline or block]
        m = re.match(r"^if\s+(.+?)\s+then\s*(.*)", stmt, re.IGNORECASE)
        if m:
            cond = self._eval_cond(m.group(1).strip())
            inline = m.group(2).strip()
            if inline:
                # Check for else on same line
                else_m = re.match(r"^(.+?)\s+else\s+(.+)$", inline, re.IGNORECASE)
                if else_m:
                    if cond:
                        r = self._exec_stmt(else_m.group(1).strip(), lines, ip, block_end)
                    else:
                        r = self._exec_stmt(else_m.group(2).strip(), lines, ip, block_end)
                    if isinstance(r, tuple) and r[0] in ("BREAK", "CONTINUE", "RETURN"):
                        return r
                else:
                    if cond:
                        r = self._exec_stmt(inline, lines, ip, block_end)
                        if isinstance(r, tuple) and r[0] in ("BREAK", "CONTINUE", "RETURN"):
                            return r
            else:
                # Block if
                then_end, else_range, after = self._scan_if(lines, ip + 1)
                if cond:
                    r = self._exec_lines(lines, ip + 1, then_end)
                    if isinstance(r, tuple) and r[0] in ("BREAK", "CONTINUE", "RETURN"):
                        return r
                elif else_range:
                    r = self._exec_lines(lines, else_range[0], else_range[1])
                    if isinstance(r, tuple) and r[0] in ("BREAK", "CONTINUE", "RETURN"):
                        return r
                return ("GOTO", after)
            return None

        # REPEAT n TIMES / WITH / WHILE / UNTIL / forever
        if re.match(r"^repeat\b", stmt_lower):
            return self._exec_repeat(stmt, lines, ip, block_end)

        # ADD n TO var
        m = re.match(r"^add\s+(.+?)\s+to\s+(\w+)$", stmt, re.IGNORECASE)
        if m:
            val = _to_num(self._eval(m.group(1).strip()))
            var = m.group(2)
            self._set_var(var, _to_num(self._get_var(var)) + val)
            return None

        # SUBTRACT n FROM var
        m = re.match(r"^subtract\s+(.+?)\s+from\s+(\w+)$", stmt, re.IGNORECASE)
        if m:
            val = _to_num(self._eval(m.group(1).strip()))
            var = m.group(2)
            self._set_var(var, _to_num(self._get_var(var)) - val)
            return None

        # MULTIPLY var BY n
        m = re.match(r"^multiply\s+(\w+)\s+by\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            var = m.group(1)
            val = _to_num(self._eval(m.group(2).strip()))
            self._set_var(var, _to_num(self._get_var(var)) * val)
            return None

        # DIVIDE var BY n
        m = re.match(r"^divide\s+(\w+)\s+by\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            var = m.group(1)
            val = _to_num(self._eval(m.group(2).strip()))
            if val == 0:
                self._emit("❌ Division by zero")
                return None
            self._set_var(var, _to_num(self._get_var(var)) / val)
            return None

        # DO expression (runtime evaluation of a string as HyperTalk)
        m = re.match(r"^do\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            code = str(self._eval(m.group(1).strip()))
            for do_line in code.splitlines():
                do_line = do_line.strip()
                if do_line:
                    self._exec_stmt(do_line, lines, ip, block_end)
            return None

        # WAIT n SECONDS/TICKS (informational only in educational context)
        m = re.match(r"^wait\s+(.+?)\s+(seconds?|ticks?)$", stmt, re.IGNORECASE)
        if m:
            val = _to_num(self._eval(m.group(1).strip()))
            unit = m.group(2).lower()
            if unit.startswith("tick"):
                val = val / 60  # 60 ticks per second
            self._emit(f"ℹ️ Waited {val:.1f} seconds")
            return None

        # VISUAL EFFECT (display effect name)
        m = re.match(r"^visual\s+effect\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            self._emit(f"🎨 Visual effect: {m.group(1).strip()}")
            return None

        # SORT container
        m = re.match(
            r"^sort\s+(?:the\s+)?(?:lines\s+of\s+)?(\w+)(?:\s+(ascending|descending|numeric|text))?$",
            stmt,
            re.IGNORECASE,
        )
        if m:
            var = m.group(1)
            mode = (m.group(2) or "text").lower()
            val = str(self._get_var(var))
            items = val.split("\n") if "\n" in val else val.split(",")
            items = [i.strip() for i in items if i.strip()]
            if mode == "numeric":
                items.sort(key=lambda x: _to_num(x))
            elif mode == "descending":
                items.sort(reverse=True)
            else:
                items.sort()
            sep = "\n" if "\n" in val else ","
            self._set_var(var, sep.join(items))
            return None

        # BEEP [n]
        m = re.match(r"^beep\s+(\d+)$", stmt, re.IGNORECASE)
        if m:
            for _ in range(min(int(m.group(1)), 5)):
                self._emit("🔔 Beep!")
            return None

        # BEEP (single)
        if stmt_lower.strip() == "beep":
            self._emit("🔔 Beep!")
            return None

        # Function call as statement: name() or name(args)
        m = re.match(r"^(\w+)\s*\((.*)\)\s*$", stmt)
        if m and not re.match(
            r"^(if|put|get|set|answer|say|ask|repeat|on|function|end|global|return|exit|call|beep|flash|find|go|do|wait|sort|visual|add|subtract|multiply|divide)\b",
            stmt_lower,
        ):
            fn_name_s = m.group(1).lower()
            fn_args_s = m.group(2).strip()
            if fn_name_s in self._handlers:
                params_s = self._handler_params.get(fn_name_s, [])
                arg_vals_s = self._split_args(fn_args_s) if fn_args_s else []
                saved_s = dict(self._vars)
                for pi, pname in enumerate(params_s):
                    hval = self._eval(arg_vals_s[pi]) if pi < len(arg_vals_s) else None
                    self._set_var(pname, hval)
                result_s = self._exec_lines(self._handlers[fn_name_s], 0, len(self._handlers[fn_name_s]))
                self._vars = saved_s
                if isinstance(result_s, tuple) and result_s[0] == "RETURN":
                    self._it = result_s[1]
                return None

        # Assignment: var = expr or var is expr
        m = re.match(r"^(\w+)\s*[=:]\s*(.+)$", stmt)
        if m and not re.match(
            r"^(if|put|get|set|answer|say|ask|repeat|on|end|global|return|exit)\b",
            stmt_lower,
        ):
            self._set_var(m.group(1), self._eval(m.group(2).strip()))
            return None

        # CALL handler [args] — synonym for direct handler invocation
        m = re.match(r"^call\s+(\w+)(?:\s+(.*))?$", stmt, re.IGNORECASE)
        if m:
            return self._exec_stmt(
                (m.group(1) + (" " + m.group(2) if m.group(2) else "")).strip(),
                lines, ip, block_end
            )

        # Call handler
        m = re.match(r"^(\w+)(?:\s+(.*)|\s*\(([^)]*)\))?$", stmt)
        if m:
            name = m.group(1).lower()
            # Support "foo(args)" or "foo args" syntax
            if m.group(3) is not None:
                args_str = m.group(3).strip()
            else:
                args_str = m.group(2) or ""
            if name in self._handlers:
                # Bind arguments to handler parameter names
                params = self._handler_params.get(name, [])
                arg_vals = (
                    [a.strip() for a in args_str.split(",")] if args_str.strip() else []
                )
                # If single arg with spaces and multiple params, split by space
                if len(arg_vals) == 1 and len(params) > 1 and " " in arg_vals[0]:
                    arg_vals = arg_vals[0].split()
                # Handlers share caller scope (HyperTalk convention); don't save/restore vars
                for pi, pname in enumerate(params):
                    hval = self._eval(arg_vals[pi]) if pi < len(arg_vals) else None
                    self._set_var(pname, hval)
                result_h = self._exec_lines(self._handlers[name], 0, len(self._handlers[name]))
                if result_h is not None:
                    self._it = result_h
                return None

        return None

    def _scan_if(self, lines: list[str], start: int) -> tuple[int, tuple | None, int]:
        """Return (then_end, else_range or None, after_endif)."""
        i = start
        then_end = start
        else_start = None
        else_end = None
        depth = 0
        while i < len(lines):
            s = lines[i].strip().lower()
            if re.match(r"^if\b", s):
                depth += 1
            elif re.match(r"^end if\b", s) or s == "end if":
                if depth == 0:
                    if else_start is not None:
                        else_end = i
                    else:
                        then_end = i
                    return (
                        then_end,
                        (else_start, else_end) if else_start else None,
                        i + 1,
                    )
                depth -= 1
            elif re.match(r"^else\b", s) and depth == 0 and else_start is None:
                then_end = i
                # else-if: start AT this line so it executes as "if ..."
                else_start = i if re.match(r"^else\s+if\b", s, re.IGNORECASE) else i + 1
            i += 1
        return (len(lines), None, len(lines))

    def _exec_repeat(self, stmt: str, lines: list[str], ip: int, block_end: int) -> Any:
        end_i = self._find_end_repeat(lines, ip + 1)
        body_start = ip + 1

        m = re.match(r"^repeat\s+(\d+)\s+times?$", stmt, re.IGNORECASE)
        if m:
            n = int(m.group(1))
            for _ in range(min(n, 100000)):
                r = self._exec_lines(lines, body_start, end_i)
                if isinstance(r, tuple) and r[0] == "BREAK":
                    break
                if isinstance(r, tuple) and r[0] == "CONTINUE":
                    continue
            return ("GOTO", end_i + 1)

        m = re.match(
            r"^repeat\s+with\s+(\w+)\s*=\s*(.+?)\s+to\s+(.+?)(?:\s+(?:by|step)\s+(.+))?$",
            stmt,
            re.IGNORECASE,
        )
        if m:
            var = m.group(1)
            frm = int(float(self._eval(m.group(2).strip())))
            to = int(float(self._eval(m.group(3).strip())))
            step = int(float(self._eval(m.group(4).strip()))) if m.group(4) else 1
            i = frm
            count = 0
            while (step > 0 and i <= to) or (step < 0 and i >= to):
                if count > 100000:
                    break
                self._set_var(var, i)
                r = self._exec_lines(lines, body_start, end_i)
                if isinstance(r, tuple) and r[0] == "BREAK":
                    break
                if isinstance(r, tuple) and r[0] == "CONTINUE":
                    i += step
                    count += 1
                    continue
                i += step
                count += 1
            return ("GOTO", end_i + 1)

        m = re.match(r"^repeat\s+while\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            cond_str = m.group(1).strip()
            count = 0
            while self._eval_cond(cond_str) and count < 100000:
                r = self._exec_lines(lines, body_start, end_i)
                if isinstance(r, tuple) and r[0] == "BREAK":
                    break
                if isinstance(r, tuple) and r[0] == "CONTINUE":
                    count += 1
                    continue
                count += 1
            return ("GOTO", end_i + 1)

        m = re.match(r"^repeat\s+until\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            cond_str = m.group(1).strip()
            count = 0
            while not self._eval_cond(cond_str) and count < 100000:
                r = self._exec_lines(lines, body_start, end_i)
                if isinstance(r, tuple) and r[0] == "BREAK":
                    break
                if isinstance(r, tuple) and r[0] == "CONTINUE":
                    count += 1
                    continue
                count += 1
            return ("GOTO", end_i + 1)

        # repeat forever
        if re.match(r"^repeat\s*$", stmt, re.IGNORECASE) or re.match(
            r"^repeat\s+forever$", stmt, re.IGNORECASE
        ):
            count = 0
            while count < 100000:
                r = self._exec_lines(lines, body_start, end_i)
                if isinstance(r, tuple) and r[0] == "BREAK":
                    break
                count += 1
            return ("GOTO", end_i + 1)

        return ("GOTO", end_i + 1)

    def _find_end_repeat(self, lines: list[str], start: int) -> int:
        depth = 0
        i = start
        while i < len(lines):
            s = lines[i].strip().lower()
            if re.match(r"^repeat\b", s):
                depth += 1
            elif re.match(r"^end\s+repeat\b", s):
                if depth == 0:
                    return i
                depth -= 1
            i += 1
        return i

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval(self, expr: str) -> Any:
        expr = expr.strip()
        if not expr:
            return ""
        # String literal (possibly followed by & concatenation)
        if expr.startswith('"') or expr.startswith("'"):
            q = expr[0]
            end_idx = expr.find(q, 1)
            if end_idx < 0:
                # Unclosed string — return everything after the opening quote
                return expr[1:]
            val = expr[1:end_idx]
            rest = expr[end_idx + 1:].strip()
            if not rest:
                return val  # simple string literal
            if rest.startswith("&&"):
                return val + " " + str(self._eval(rest[2:].strip()))
            if rest.startswith("&"):
                return val + str(self._eval(rest[1:].strip()))
            return val  # ignore other trailing content
        # Number
        try:
            return int(expr)
        except ValueError:
            pass
        try:
            return float(expr)
        except ValueError:
            pass
        # Boolean
        if expr.lower() == "true":
            return True
        if expr.lower() == "false":
            return False
        if expr.lower() == "empty":
            return ""
        if expr.lower() == "it":
            return self._it
        if expr.lower() == "return":
            return "\n"
        if expr.lower() == "space":
            return " "
        if expr.lower() == "tab":
            return "\t"
        if expr.lower() == "cr":
            return "\n"
        if expr.lower() == "quote":
            return '"'

        # field "fieldName" expression
        m = re.match(r'^field\s+["\']([^"\']+)["\']$', expr, re.IGNORECASE)
        if m:
            fld = "_field_" + re.sub(r"\W+", "_", m.group(1).lower())
            return self._get_var(fld)

        # month of / day of (date chunk extraction)
        m = re.match(r"^month\s+of\s+(.+)$", expr, re.IGNORECASE)
        if m:
            date_str = str(self._eval(m.group(1).strip()))
            try:
                return date_str.split("/")[0]
            except Exception:
                return "0"
        m = re.match(r"^day\s+of\s+(.+)$", expr, re.IGNORECASE)
        if m:
            date_str = str(self._eval(m.group(1).strip()))
            try:
                return date_str.split("/")[1]
            except Exception:
                return "0"

        # Built-in properties: the date, the time, the ticks, the seconds
        if expr.lower().startswith("the "):
            prop = expr[4:].strip().lower()
            if prop == "date":
                return _datetime.now().strftime("%m/%d/%Y")
            if prop in ("short date", "abbrev date", "abbreviated date"):
                return _datetime.now().strftime("%m/%d/%y")
            if prop in ("long date", "long date"):
                return _datetime.now().strftime("%A, %B %d, %Y")
            if prop == "time":
                return _datetime.now().strftime("%I:%M %p")
            if prop in ("long time",):
                return _datetime.now().strftime("%I:%M:%S %p")
            if prop == "seconds":
                return int(_time.time())
            if prop == "ticks":
                return int(_time.time() * 60)
            if prop == "result":
                return self._it
            if prop == "target":
                return "card button 1"
            if prop == "random":
                return random.randint(1, 100)
            # number of cards in the stack
            if re.match(r"^number\s+of\s+cards?\b", prop, re.IGNORECASE):
                return max(1, int(float(self._get_var("_card_count") or 1)))
            # number of this card
            if re.match(r"^number\s+of\s+this\s+card\b", prop, re.IGNORECASE):
                return max(1, int(float(self._get_var("_current_card") or 1)))
            # modified / modified of this stack -> False in simulation
            if "modified" in prop:
                return False
            m_of = re.match(
                r"number\s+of\s+(chars?|words?|lines?|items?)\s+(?:of|in)\s+(.+)",
                prop,
                re.IGNORECASE,
            )
            if m_of:
                kind = m_of.group(1).lower().rstrip("s")
                val = str(self._eval(m_of.group(2).strip()))
                if kind == "char":
                    return len(val)
                if kind == "word":
                    return len(val.split())
                if kind == "line":
                    return len(val.splitlines()) if val else 0
                if kind == "item":
                    return len(val.split(","))
                return 0

        # Property access: chars? i to j of str (variable or literal index)
        m = re.match(r"^chars?(?:acter)?s?\s+(\w+)\s+to\s+(\w+)\s+of\s+(.+)$", expr, re.IGNORECASE)
        if m:
            try:
                ci = int(float(self._eval(m.group(1)))) - 1
                cj = int(float(self._eval(m.group(2))))
            except (TypeError, ValueError):
                ci, cj = 0, 0
            s = str(self._eval(m.group(3).strip()))
            return s[max(0, ci):min(cj, len(s))]

        m = re.match(r"^word\s+(\w+)\s+to\s+(\w+)\s+of\s+(.+)$", expr, re.IGNORECASE)
        if m:
            try:
                ci = int(float(self._eval(m.group(1)))) - 1
                cj = int(float(self._eval(m.group(2))))
            except (TypeError, ValueError):
                ci, cj = 0, 0
            words = str(self._eval(m.group(3).strip())).split()
            return " ".join(words[max(0, ci):min(cj, len(words))])

        m = re.match(r"^chars?(?:acter)?s?\s+(\w+)\s+of\s+(.+)$", expr, re.IGNORECASE)
        if m:
            try:
                idx = int(float(self._eval(m.group(1)))) - 1
            except (TypeError, ValueError):
                idx = 0
            s = str(self._eval(m.group(2).strip()))
            return s[idx] if 0 <= idx < len(s) else ""

        m = re.match(r"^word\s+(\w+)\s+of\s+(.+)$", expr, re.IGNORECASE)
        if m:
            try:
                idx = int(float(self._eval(m.group(1)))) - 1
            except (TypeError, ValueError):
                idx = 0
            words = str(self._eval(m.group(2).strip())).split()
            return words[idx] if 0 <= idx < len(words) else ""

        m = re.match(r"^line\s+(\w+)\s+of\s+(.+)$", expr, re.IGNORECASE)
        if m:
            try:
                idx = int(float(self._eval(m.group(1)))) - 1
            except (TypeError, ValueError):
                idx = 0
            lines_ = str(self._eval(m.group(2).strip())).splitlines()
            return lines_[idx] if 0 <= idx < len(lines_) else ""

        m = re.match(r"^item\s+(\w+)\s+of\s+(.+)$", expr, re.IGNORECASE)
        if m:
            try:
                idx = int(float(self._eval(m.group(1)))) - 1
            except (TypeError, ValueError):
                idx = 0
            items = str(self._eval(m.group(2).strip())).split(",")
            return items[idx].strip() if 0 <= idx < len(items) else ""
        m = re.match(
            r"^the\s+(?:number\s+of\s+)?chars?\s+(?:in|of)\s+(.+)$", expr, re.IGNORECASE
        )
        if m:
            return len(str(self._eval(m.group(1).strip())))

        m = re.match(r"^the\s+length\s+(?:of|in)\s+(.+)$", expr, re.IGNORECASE)
        if m:
            return len(str(self._eval(m.group(1).strip())))

        m = re.match(
            r"^the\s+(?:number\s+of\s+)?words?\s+(?:in|of)\s+(.+)$", expr, re.IGNORECASE
        )
        if m:
            return len(str(self._eval(m.group(1).strip())).split())

        # Function call: name(args) or name() — verify parens are balanced
        m = re.match(r"^(\w+)\s*\(", expr)
        if m:
            call_args_start = m.end()  # index just after the opening '('
            depth = 1
            close_pos = -1
            for ci, c in enumerate(expr[call_args_start:], call_args_start):
                if c == "(":
                    depth += 1
                elif c == ")":
                    depth -= 1
                if depth == 0:
                    close_pos = ci
                    break
            if close_pos == len(expr) - 1:  # matching ')' is the last char → whole expr is a call
                args_content = expr[call_args_start:close_pos]
                result = self._call_fn(m.group(1).lower(), args_content)
                if result is not None:
                    return result

        # Concatenation: expr & expr (check && first to avoid splitting inside &&)
        if "&&" in expr:
            parts = expr.split("&&", 1)
            return (
                str(self._eval(parts[0].strip()))
                + " "
                + str(self._eval(parts[1].strip()))
            )
        if "&" in expr:
            parts = expr.split("&", 1)
            return str(self._eval(parts[0].strip())) + str(self._eval(parts[1].strip()))

        # Comparisons (handle before mod/div to get correct precedence)
        # e.g. "17 mod d = 0" → lhs="17 mod d", rhs="0"
        for cmp_op, cmp_fn in [
            (" >= ", lambda a, b: _to_num(a) >= _to_num(b)),
            (" <= ", lambda a, b: _to_num(a) <= _to_num(b)),
            (" <> ", lambda a, b: str(a) != str(b)),
            (" != ", lambda a, b: str(a) != str(b)),
            (" > ", lambda a, b: _to_num(a) > _to_num(b)),
            (" < ", lambda a, b: _to_num(a) < _to_num(b)),
            (" = ", lambda a, b: str(a) == str(b)),
        ]:
            idx = expr.find(cmp_op)
            if idx > 0:
                lhs_s = expr[:idx].strip()
                rhs_s = expr[idx + len(cmp_op):].strip()
                lhs = self._eval(lhs_s)
                rhs = self._eval(rhs_s)
                return cmp_fn(lhs, rhs)

        # Arithmetic using Python
        for op in [" is ", " is not ", " mod ", " div ", " contains ", " is in "]:
            if op in expr.lower():
                lhs_s, rhs_s = re.split(op, expr, maxsplit=1, flags=re.IGNORECASE)
                lhs = self._eval(lhs_s.strip())
                rhs = self._eval(rhs_s.strip())
                op_clean = op.strip().lower()
                if op_clean == "is":
                    return lhs == rhs
                if op_clean == "is not":
                    return lhs != rhs
                if op_clean == "mod":
                    return _to_num(lhs) % _to_num(rhs)
                if op_clean == "div":
                    return int(_to_num(lhs) / _to_num(rhs))
                if op_clean == "contains":
                    return str(rhs) in str(lhs)
                if op_clean == "is in":
                    return str(lhs) in str(rhs)

        # Python eval for arithmetic
        # First substitute function calls (e.g. random(25), sin(x)) with their values
        def sub_fn_call(m_: re.Match) -> str:
            fn_n = m_.group(1).lower()
            fn_a = m_.group(2)
            # Avoid substituting keywords used as identifiers
            if fn_n in ("if", "on", "function", "end", "repeat", "while", "until"):
                return m_.group(0)
            result_fn = self._call_fn(fn_n, fn_a)
            if result_fn is not None:
                return str(result_fn)
            return m_.group(0)

        pyexpr = re.sub(r"([A-Za-z_]\w*)\s*\(([^)]*)\)", sub_fn_call, expr)

        def sub_var(m_: re.Match) -> str:
            name = m_.group(0).lower()
            val = self._get_var(name)
            return str(val)

        pyexpr = re.sub(r"[A-Za-z_]\w*", sub_var, pyexpr)
        pyexpr = pyexpr.replace("^", "**")
        try:
            return eval(pyexpr, {"__builtins__": {}})  # noqa: S307
        except Exception:
            pass

        return self._get_var(expr)

    def _eval_cond(self, cond: str) -> bool:  # noqa: C901
        cond = cond.strip()

        # NOT prefix
        if re.match(r"^not\s+", cond, re.IGNORECASE):
            return not self._eval_cond(cond[4:].strip())

        # AND: split on first top-level 'and' (case-insensitive, whole word)
        m = re.search(r"\band\b", cond, re.IGNORECASE)
        if m:
            return self._eval_cond(cond[:m.start()]) and self._eval_cond(cond[m.end():])

        # OR: split on first top-level 'or' (case-insensitive, whole word)
        m = re.search(r"\bor\b", cond, re.IGNORECASE)
        if m:
            return self._eval_cond(cond[:m.start()]) or self._eval_cond(cond[m.end():])

        # Handle comparison operators before _eval can misparse compound expressions
        # Check multi-char operators first to avoid splitting > from >=
        for cmp_op, fn in [
            (">=", lambda a, b: _to_num(a) >= _to_num(b)),
            ("<=", lambda a, b: _to_num(a) <= _to_num(b)),
            ("<>", lambda a, b: str(a).lower() != str(b).lower()),
            ("!=", lambda a, b: str(a).lower() != str(b).lower()),
        ]:
            if cmp_op in cond:
                parts = cond.split(cmp_op, 1)
                if len(parts) == 2:
                    lhs = self._eval(parts[0].strip())
                    rhs = self._eval(parts[1].strip())
                    try:
                        return fn(lhs, rhs)
                    except Exception:
                        return str(lhs).lower() == str(rhs).lower()
        # < and > (not preceded or followed by another < > = !)
        for cmp_op, fn in [  # type: ignore[assignment]
            ("<", lambda a, b: _to_num(a) < _to_num(b)),
            (">", lambda a, b: _to_num(a) > _to_num(b)),
        ]:
            idx = cond.find(cmp_op)
            if idx >= 0:
                before = cond[idx - 1] if idx > 0 else ""
                after = cond[idx + 1] if idx + 1 < len(cond) else ""
                if before not in ("<", ">", "!") and after not in ("=", ">"):
                    lhs = self._eval(cond[:idx].strip())
                    rhs = self._eval(cond[idx + 1:].strip())
                    try:
                        return fn(lhs, rhs)  # type: ignore[operator]
                    except Exception:
                        return False
        # = (equality — in condition context = is always equality)
        if "=" in cond:
            idx = cond.index("=")
            before = cond[idx - 1] if idx > 0 else ""
            after = cond[idx + 1] if idx + 1 < len(cond) else ""
            if before not in ("<", ">", "!", "=") and after != "=":
                lhs = self._eval(cond[:idx].strip())
                rhs = self._eval(cond[idx + 1:].strip())
                if str(lhs).lower() == str(rhs).lower():
                    return True
                try:
                    return float(lhs) == float(rhs)  # type: ignore[arg-type]
                except (TypeError, ValueError):
                    return False
        val = self._eval(cond)
        if isinstance(val, bool):
            return val
        if isinstance(val, (int, float)):
            return val != 0
        if isinstance(val, str):
            return val.lower() not in ("", "false", "0")
        return bool(val)

    def _call_fn(self, name: str, args_str: str) -> Any:
        # Use smart split for user-defined handlers; simple split for builtins
        raw_parts = self._split_args(args_str) if args_str.strip() else []
        args = [self._eval(a) for a in raw_parts] if raw_parts else []
        try:
            a0 = float(args[0]) if args else 0
        except (TypeError, ValueError):
            a0 = 0
        fns: dict[str, Any] = {
            "sin": lambda: math.sin(math.radians(a0)),
            "cos": lambda: math.cos(math.radians(a0)),
            "tan": lambda: math.tan(math.radians(a0)),
            "sqrt": lambda: math.sqrt(a0),
            "abs": lambda: abs(a0),
            "round": lambda: round(a0),
            "trunc": lambda: int(a0),
            "floor": lambda: math.floor(a0),
            "ceil": lambda: math.ceil(a0),
            "ceiling": lambda: math.ceil(a0),
            "integer": lambda: int(a0),
            "length": lambda: len(str(args[0])),
            "random": lambda: random.randint(1, int(a0)) if a0 else random.random(),
            "offset": lambda: (
                str(args[1]).find(str(args[0])) + 1 if len(args) > 1 else 0
            ),
            "max": lambda: max(float(a) for a in args),
            "min": lambda: min(float(a) for a in args),
            "value": lambda: self._eval(str(args[0])),
            "numtochar": lambda: chr(int(a0)),
            "chartonum": lambda: ord(str(args[0])[0]) if args[0] else 0,
            "average": lambda: sum(float(a) for a in args) / len(args) if args else 0,
            "sum": lambda: sum(float(a) for a in args),
            "ln": lambda: math.log(a0),
            "log2": lambda: math.log2(a0),
            "log10": lambda: math.log10(a0),
            "exp": lambda: math.exp(a0),
            "exp1": lambda: math.exp(a0),
            "atan": lambda: math.degrees(math.atan(a0)),
            "annuity": lambda: (1 - (1 + float(args[0])) ** (-float(args[1]))) / float(args[0]) if len(args) > 1 else 0,
            "compound": lambda: (1 + float(args[0])) ** float(args[1]) if len(args) > 1 else 0,
        }
        fn = fns.get(name)
        if fn:
            try:
                return fn()
            except Exception:
                return 0

        # User-defined handler called as a function
        if name in self._handlers:
            params_h = self._handler_params.get(name, [])
            saved_h = dict(self._vars)
            for pi, pname in enumerate(params_h):
                hval: Any = args[pi] if pi < len(args) else None
                self._set_var(pname, hval)
            result_h = self._exec_lines(self._handlers[name], 0, len(self._handlers[name]))
            self._vars = saved_h
            if result_h is not None:
                return result_h
            return self._it

        return None

    def _split_args(self, args_str: str) -> list[str]:
        """Split argument string at top-level commas (not inside strings/parens)."""
        result: list[str] = []
        depth = 0
        in_str: str | None = None
        current: list[str] = []
        for c in args_str:
            if in_str:
                current.append(c)
                if c == in_str:
                    in_str = None
            elif c in ('"', "'"):
                in_str = c
                current.append(c)
            elif c in ("(", "["):
                depth += 1
                current.append(c)
            elif c in (")", "]"):
                depth -= 1
                current.append(c)
            elif c == "," and depth == 0:
                result.append("".join(current).strip())
                current = []
            else:
                current.append(c)
        if current:
            result.append("".join(current).strip())
        return [a for a in result if a]


def _to_num(val: Any) -> float:
    try:
        return float(val)
    except (TypeError, ValueError):
        return 0.0


class HTExit(Exception):
    pass


class HTError(Exception):
    pass
