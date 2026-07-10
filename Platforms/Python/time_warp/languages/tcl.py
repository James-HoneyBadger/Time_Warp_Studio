"""Tcl/Tk language executor for Time Warp Studio.

Educational Tcl interpreter — whole-program execution.
Implements a teaching subset of Tcl 8.x:
  - puts, set, unset, expr, incr, append, lappend
  - if/elseif/else, while, for, foreach, break, continue
  - proc (user-defined procedures with local scope), return
  - list operations: list, lindex, lset, llength, lrange, lsort, lreverse
  - string operations: string length/index/range/toupper/tolower/trim/compare/match
  - string format / scan
  - regexp / regsub
  - switch
  - array set/get/names/unset/exists
  - Turtle graphics via simple command dispatch (fd/bk/lt/rt/pu/pd etc.)
  - Arithmetic via expr: +,-,*,/,**,==,!=,<,>,<=,>=,&&,||,!
  - catch / error
"""

from __future__ import annotations

import math
import re
from typing import TYPE_CHECKING, Any, Dict, List, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_tcl(interpreter: "Interpreter", source: str, turtle: "TurtleState") -> str:
    """Execute a complete Tcl program and return all output as a string."""
    env = TclEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Control-flow exceptions
# ---------------------------------------------------------------------------


class _TclBreak(Exception):
    pass


class _TclContinue(Exception):
    pass


class _TclReturn(Exception):
    def __init__(self, value: str = "") -> None:
        self.value = value


class _TclError(Exception):
    pass


# ---------------------------------------------------------------------------
# Tokenizer
# ---------------------------------------------------------------------------


def _tokenize(line: str) -> List[str]:
    """Split a Tcl command line into tokens, handling braces, brackets, quotes."""
    tokens: List[str] = []
    i = 0
    n = len(line)
    while i < n:
        c = line[i]
        if c in " \t":
            i += 1
            continue
        if c == "{":
            # Brace-quoted word — no substitution
            depth = 0
            j = i
            while j < n:
                if line[j] == "{":
                    depth += 1
                elif line[j] == "}":
                    depth -= 1
                    if depth == 0:
                        break
                j += 1
            tokens.append(line[i + 1 : j])
            i = j + 1
        elif c == '"':
            # Double-quoted word — substitution happens later
            j = i + 1
            while j < n:
                if line[j] == "\\" and j + 1 < n:
                    j += 2
                    continue
                if line[j] == '"':
                    break
                j += 1
            tokens.append(line[i : j + 1])
            i = j + 1
        elif c == "[":
            # Command substitution bracket
            depth = 0
            j = i
            while j < n:
                if line[j] == "[":
                    depth += 1
                elif line[j] == "]":
                    depth -= 1
                    if depth == 0:
                        break
                j += 1
            tokens.append(line[i : j + 1])
            i = j + 1
        elif c == ";":
            # Statement separator — treat as end of current command
            break
        else:
            j = i
            while j < n and line[j] not in " \t;":
                j += 1
            tokens.append(line[j - 1 if j > i else i : j])  # wrong, fix:
            tokens[-1] = line[i:j]  # correct slice
            tokens.pop(-2) if len(tokens) > 1 and tokens[-2] == tokens[-1] else None
            tokens.pop()
            tokens.append(line[i:j])
            i = j
    return tokens


def _tcl_tokenize(line: str) -> List[str]:
    """Proper Tcl tokenizer. Returns list of token strings (brace content stripped)."""
    return [tok for tok, _ in _tcl_tokenize_ex(line)]


def _tcl_tokenize_ex(line: str) -> List[Tuple[str, bool]]:
    """Tcl tokenizer returning (token, brace_quoted) pairs.

    brace_quoted=True means the token came from {…} and should NOT be substituted.
    """
    tokens: List[Tuple[str, bool]] = []
    i = 0
    n = len(line)
    while i < n:
        c = line[i]
        # Skip whitespace
        if c in " \t":
            i += 1
            continue
        # Semi-colon ends command
        if c == ";":
            break
        # Brace group: {…} — no substitution
        if c == "{":
            depth = 1
            j = i + 1
            while j < n and depth > 0:
                if line[j] == "{":
                    depth += 1
                elif line[j] == "}":
                    depth -= 1
                j += 1
            tokens.append((line[i + 1 : j - 1], True))  # brace_quoted=True
            i = j
        # Quoted group: "…" — do substitution
        elif c == '"':
            j = i + 1
            while j < n:
                if line[j] == "\\" and j + 1 < n:
                    j += 2
                    continue
                if line[j] == '"':
                    break
                j += 1
            tokens.append((line[i : j + 1], False))  # keep quotes, subst=True
            i = j + 1
        # Command substitution: […]
        elif c == "[":
            depth = 1
            j = i + 1
            while j < n and depth > 0:
                if line[j] == "[":
                    depth += 1
                elif line[j] == "]":
                    depth -= 1
                j += 1
            tokens.append((line[i:j], False))  # keep brackets, subst=True
            i = j
        else:
            j = i
            while j < n and line[j] not in " \t;":
                j += 1
            tokens.append((line[i:j], False))
            i = j
    return tokens


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------


class TclEnvironment:
    """Full execution environment for a Tcl program."""

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState") -> None:
        self._interp = interpreter
        self._turtle = turtle
        self._output: List[str] = []
        self._global_vars: Dict[str, str] = {}
        self._arrays: Dict[str, Dict[str, str]] = {}
        self._procs: Dict[str, Tuple[List[str], str]] = {}  # name → (params, body)
        self._call_stack: List[Dict[str, str]] = []  # stack of local scopes

    # ------------------------------------------------------------------
    # Top level
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        try:
            self._exec_block(source)
        except _TclReturn:
            pass
        except _TclError as e:
            self._output.append(f"❌ Tcl error: {e}\n")
        except Exception as e:  # noqa: BLE001
            self._output.append(f"❌ Tcl runtime error: {e}\n")
        return "".join(self._output)

    def _exec_block(self, block: str) -> str:
        """Execute a block of Tcl commands (multi-line). Returns last value."""
        result = ""
        lines = self._split_commands(block)
        for line in lines:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            result = self._exec_command(line)
        return result

    def _split_commands(self, block: str) -> List[str]:
        """Split a block into individual commands (respecting braces and quotes)."""
        commands: List[str] = []
        brace_depth = 0
        bracket_depth = 0
        in_quote = False
        i = 0
        lines = block.split("\n")

        merged = "\n".join(lines)
        i = 0
        n = len(merged)
        start = 0
        while i < n:
            c = merged[i]
            if c == "\\" and i + 1 < n and merged[i + 1] == "\n":
                # Line continuation
                i += 2
                continue
            if in_quote:
                if c == "\\" and i + 1 < n:
                    i += 2
                    continue
                if c == '"':
                    in_quote = False
            elif c == '"':
                in_quote = True
            elif c == "{":
                brace_depth += 1
            elif c == "}":
                brace_depth -= 1
            elif c == "[":
                bracket_depth += 1
            elif c == "]":
                bracket_depth -= 1
            elif brace_depth == 0 and bracket_depth == 0 and c in ("\n", ";"):
                cmd = merged[start:i].strip()
                if cmd and not cmd.startswith("#"):
                    commands.append(cmd)
                start = i + 1
            i += 1

        # Flush remaining
        cmd = merged[start:].strip()
        if cmd and not cmd.startswith("#"):
            commands.append(cmd)

        merged_commands: List[str] = []
        for command in commands:
            stripped = command.lstrip()
            if stripped.startswith(("elseif", "else")) and merged_commands:
                merged_commands[-1] = merged_commands[-1] + " " + command
            else:
                merged_commands.append(command)
        return merged_commands

    def _exec_command(self, line: str) -> str:
        """Execute a single Tcl command and return its result."""
        # Tokenize first, then substitute each token based on quoting rules.
        # Brace-quoted tokens ({…}) are never substituted.
        # All other tokens (bare words, "quoted", [brackets]) get substitution.
        raw_tokens = _tcl_tokenize_ex(line)
        if not raw_tokens:
            return ""
        tokens: List[str] = []
        for tok, brace_quoted in raw_tokens:
            if brace_quoted:
                tokens.append(tok)  # no substitution
            else:
                tokens.append(self._substitute(tok))
        if not tokens:
            return ""
        cmd = tokens[0].lower()
        args = tokens[1:]
        return self._dispatch(cmd, args)

    # ------------------------------------------------------------------
    # Substitution
    # ------------------------------------------------------------------

    def _substitute(self, s: str) -> str:
        """Apply Tcl variable and command substitutions to a string."""
        # Remove surrounding quotes if double-quoted
        if s.startswith('"') and s.endswith('"') and len(s) >= 2:
            s = s[1:-1]
            s = self._do_subst(s)
        elif s.startswith("[") and s.endswith("]"):
            # Command substitution
            inner = s[1:-1]
            s = self._exec_block(inner)
        else:
            s = self._do_subst(s)
        return s

    def _do_subst(self, s: str) -> str:
        """Perform $var and [cmd] substitution inside a string."""

        # Command substitutions — iterate from innermost brackets outward
        def _cmd_sub(m: re.Match) -> str:
            return self._exec_block(m.group(1))

        # Keep resolving innermost [...] until none remain
        limit = 100
        while limit > 0 and "[" in s:
            new_s = re.sub(r"\[([^\[\]]+)\]", _cmd_sub, s)
            if new_s == s:
                break
            s = new_s
            limit -= 1

        # Variable substitution: ${name} or $name(index) or $name
        def _var_sub(m: re.Match) -> str:
            name = m.group(1) or m.group(2) or m.group(3)
            idx = m.group(4)
            if idx is not None:
                return self._get_array(name.upper(), idx)
            return self._get_var(name)

        s = re.sub(
            r"\$\{([^}]+)\}|\$([A-Za-z_][A-Za-z0-9_]*)\(([^)]*)\)|\$([A-Za-z_][A-Za-z0-9_]*)",
            lambda m: (
                self._get_var(m.group(1) or m.group(2) or m.group(4))
                if not m.group(3)
                else self._get_array(m.group(2), m.group(3))
            ),
            s,
        )
        # Escape sequences
        s = s.replace("\\n", "\n").replace("\\t", "\t").replace("\\\\", "\\")
        return s

    # ------------------------------------------------------------------
    # Variable management
    # ------------------------------------------------------------------

    def _scope(self) -> Dict[str, str]:
        """Return current local scope (or globals if no call stack)."""
        return self._call_stack[-1] if self._call_stack else self._global_vars

    def _get_var(self, name: str) -> str:
        scope = self._scope()
        if name in scope:
            return scope[name]
        if self._call_stack and name in self._global_vars:
            return self._global_vars[name]
        raise _TclError(f'can\'t read "{name}": no such variable')

    def _set_var(self, name: str, value: str) -> None:
        self._scope()[name] = str(value)

    def _get_array(self, name: str, index: str) -> str:
        return self._arrays.get(name, {}).get(index, "")

    def _set_array(self, name: str, index: str, value: str) -> None:
        if name not in self._arrays:
            self._arrays[name] = {}
        self._arrays[name][index] = str(value)

    # ------------------------------------------------------------------
    # Command dispatch
    # ------------------------------------------------------------------

    def _dispatch(self, cmd: str, args: List[str]) -> str:  # noqa: C901
        if cmd == "puts":
            return self._cmd_puts(args)
        if cmd == "set":
            return self._cmd_set(args)
        if cmd == "unset":
            for a in args:
                self._scope().pop(a, None)
            return ""
        if cmd == "expr":
            return self._cmd_expr(args)
        if cmd == "incr":
            return self._cmd_incr(args)
        if cmd == "append":
            return self._cmd_append(args)
        if cmd == "if":
            return self._cmd_if(args)
        if cmd == "while":
            return self._cmd_while(args)
        if cmd == "for":
            return self._cmd_for(args)
        if cmd == "foreach":
            return self._cmd_foreach(args)
        if cmd == "break":
            raise _TclBreak
        if cmd == "continue":
            raise _TclContinue
        if cmd == "return":
            raise _TclReturn(" ".join(args))
        if cmd == "proc":
            return self._cmd_proc(args)
        if cmd == "switch":
            return self._cmd_switch(args)
        if cmd == "list":
            return self._cmd_list(args)
        if cmd == "lindex":
            return self._cmd_lindex(args)
        if cmd == "llength":
            return str(len(self._tcl_list(args[0]))) if args else "0"
        if cmd == "lrange":
            return self._cmd_lrange(args)
        if cmd == "lappend":
            return self._cmd_lappend(args)
        if cmd == "lrepeat":
            return self._cmd_lrepeat(args)
        if cmd == "lassign":
            return self._cmd_lassign(args)
        if cmd == "lset":
            return self._cmd_lset(args)
        if cmd == "lsort":
            if not args:
                return ""
            sort_integer = "-integer" in args[:-1]
            sort_decreasing = "-decreasing" in args[:-1]
            lst = self._tcl_list(args[-1])
            if sort_integer:
                try:
                    sorted_lst = sorted(lst, key=lambda x: int(float(x)))
                except ValueError:
                    sorted_lst = sorted(lst)
            else:
                sorted_lst = sorted(lst)
            if sort_decreasing:
                sorted_lst = list(reversed(sorted_lst))
            return self._list_to_str(sorted_lst)
        if cmd == "lreverse":
            lst = self._tcl_list(args[0]) if args else []
            return self._list_to_str(list(reversed(lst)))
        if cmd == "string":
            return self._cmd_string(args)
        if cmd == "format":
            return self._cmd_format(args)
        if cmd == "regexp":
            return self._cmd_regexp(args)
        if cmd == "regsub":
            return self._cmd_regsub(args)
        if cmd == "catch":
            return self._cmd_catch(args)
        if cmd == "error":
            raise _TclError(args[0] if args else "")
        if cmd == "array":
            return self._cmd_array(args)
        if cmd == "info":
            return self._cmd_info(args)
        if cmd in ("namespace", "package"):
            return ""
        # Turtle graphics
        if cmd in ("forward", "fd"):
            d = float(args[0]) if args else 0
            self._turtle.forward(d)
            return ""
        if cmd in ("backward", "back", "bk"):
            d = float(args[0]) if args else 0
            self._turtle.backward(d)
            return ""
        if cmd in ("left", "lt"):
            a = float(args[0]) if args else 0
            self._turtle.left(a)
            return ""
        if cmd in ("right", "rt"):
            a = float(args[0]) if args else 0
            self._turtle.right(a)
            return ""
        if cmd in ("penup", "pu"):
            self._turtle.pen_up()
            return ""
        if cmd in ("pendown", "pd"):
            self._turtle.pen_down()
            return ""
        if cmd in ("setheading", "seth"):
            a = float(args[0]) if args else 0
            self._turtle.set_heading(a)
            return ""
        if cmd in ("setxy", "setpos"):
            if len(args) >= 2:
                self._turtle.set_pos(float(args[0]), float(args[1]))
            return ""
        # Try user-defined proc
        if cmd in self._procs:
            return self._call_proc(cmd, args)
        # Silently ignore unknown commands (for portability)
        return ""

    # ------------------------------------------------------------------
    # Command implementations
    # ------------------------------------------------------------------

    def _cmd_puts(self, args: List[str]) -> str:
        no_nl = False
        text_args = []
        i = 0
        while i < len(args):
            a = args[i]
            if a == "-nonewline":
                no_nl = True
            elif a in ("stdout", "stderr"):
                pass  # channel selection not implemented; always write to output
            else:
                text_args.append(a)
            i += 1
        text = " ".join(text_args)
        if no_nl:
            self._output.append(text)
        else:
            self._output.append(text + "\n")
        return ""

    def _cmd_set(self, args: List[str]) -> str:
        if len(args) == 1:
            return self._get_var(args[0])
        if len(args) >= 2:
            self._set_var(args[0], args[1])
            return args[1]
        return ""

    def _cmd_expr(self, args: List[str]) -> str:
        expr = " ".join(args)
        return str(self._eval_expr(expr))

    def _eval_expr(self, expr: str) -> Any:
        """Evaluate a Tcl expression."""
        # Strip outer braces/brackets
        expr = expr.strip()
        if expr.startswith("{") and expr.endswith("}"):
            expr = expr[1:-1]
        # Variable substitution
        expr = self._do_subst(expr)

        # Replace Tcl operators with Python
        expr = re.sub(r"\beq\b", "==", expr)
        expr = re.sub(r"\bne\b", "!=", expr)
        expr = re.sub(r"\band\b", " and ", expr, flags=re.IGNORECASE)
        expr = re.sub(r"\bor\b", " or ", expr, flags=re.IGNORECASE)
        expr = re.sub(r"\bnot\b", " not ", expr, flags=re.IGNORECASE)
        expr = expr.replace("&&", " and ").replace("||", " or ")
        expr = re.sub(r"!(?!=)", " not ", expr)
        expr = re.sub(
            r"\b(abs|round|int|float|sqrt|sin|cos|tan|ceil|floor|log|exp|pow)\b",
            lambda m: {
                "sqrt": "math.sqrt",
                "sin": "math.sin",
                "cos": "math.cos",
                "tan": "math.tan",
                "ceil": "math.ceil",
                "floor": "math.floor",
                "log": "math.log",
                "exp": "math.exp",
                "pow": "math.pow",
            }.get(m.group(1), m.group(1)),
            expr,
        )
        expr = re.sub(r"(?<![*/])/(?![*/])", "//", expr)

        try:
            result = eval(
                expr,
                {
                    "__builtins__": {},
                    "math": math,
                    "abs": abs,  # noqa: S307,PGH001
                    "round": round,
                    "int": int,
                    "float": float,
                    "str": str,
                    "len": len,
                    "True": True,
                    "False": False,
                },
            )
            if isinstance(result, bool):
                return 1 if result else 0
            return result
        except Exception:  # noqa: BLE001
            return 0

    def _cmd_incr(self, args: List[str]) -> str:
        name = args[0] if args else ""
        step = int(float(args[1])) if len(args) > 1 else 1
        try:
            val = int(float(self._get_var(name)))
        except (_TclError, ValueError):
            val = 0
        val += step
        self._set_var(name, str(val))
        return str(val)

    def _cmd_append(self, args: List[str]) -> str:
        name = args[0] if args else ""
        try:
            val = self._get_var(name)
        except _TclError:
            val = ""
        val += "".join(args[1:])
        self._set_var(name, val)
        return val

    def _cmd_if(self, args: List[str]) -> str:
        """if cond body [elseif cond body ...] [else body]"""
        i = 0
        while i < len(args):
            if args[i].lower() in ("else",):
                body = args[i + 1] if i + 1 < len(args) else ""
                return self._exec_block(body)
            if args[i].lower() in ("elseif",):
                cond = args[i + 1] if i + 1 < len(args) else "0"
                body = args[i + 2] if i + 2 < len(args) else ""
                if bool(self._eval_expr(cond)):
                    return self._exec_block(body)
                i += 3
                continue
            # First condition
            cond = args[i]
            # Sometimes "then" keyword follows
            body_idx = i + 1
            if body_idx < len(args) and args[body_idx].lower() == "then":
                body_idx += 1
            body = args[body_idx] if body_idx < len(args) else ""
            if bool(self._eval_expr(cond)):
                return self._exec_block(body)
            i = body_idx + 1
        return ""

    def _cmd_while(self, args: List[str]) -> str:
        cond, body = args[0], args[1] if len(args) > 1 else ""
        limit = 100_000
        for _ in range(limit):
            if not bool(self._eval_expr(cond)):
                break
            try:
                self._exec_block(body)
            except _TclBreak:
                break
            except _TclContinue:
                continue
        return ""

    def _cmd_for(self, args: List[str]) -> str:
        """for {init} cond {step} body"""
        if len(args) < 4:
            return ""
        init, cond, step, body = args[0], args[1], args[2], args[3]
        self._exec_block(init)
        limit = 100_000
        for _ in range(limit):
            if not bool(self._eval_expr(cond)):
                break
            try:
                self._exec_block(body)
            except _TclBreak:
                break
            except _TclContinue:
                pass
            self._exec_block(step)
        return ""

    def _cmd_foreach(self, args: List[str]) -> str:
        """foreach var list body"""
        if len(args) < 3:
            return ""
        var, lst_str, body = args[0], args[1], args[2]
        items = self._tcl_list(lst_str)
        for item in items:
            self._set_var(var, item)
            try:
                self._exec_block(body)
            except _TclBreak:
                break
            except _TclContinue:
                continue
        return ""

    def _cmd_proc(self, args: List[str]) -> str:
        """proc name {params} body"""
        name, params_str, body = (
            args[0],
            args[1] if len(args) > 1 else "",
            args[2] if len(args) > 2 else "",
        )
        params = self._tcl_list(params_str)
        self._procs[name.lower()] = (params, body)
        return ""

    def _call_proc(self, name: str, args: List[str]) -> str:
        params, body = self._procs[name.lower()]
        local: Dict[str, str] = {}
        for i, param in enumerate(params):
            if param == "args":
                local["args"] = self._list_to_str(args[i:])
                break
            local[param] = args[i] if i < len(args) else ""
        self._call_stack.append(local)
        result = ""
        try:
            result = self._exec_block(body)
        except _TclReturn as r:
            result = r.value
        finally:
            self._call_stack.pop()
        return result

    def _cmd_switch(self, args: List[str]) -> str:
        """switch ?options? string {pattern body ...}"""
        # Find the string and pattern block
        i = 0
        while i < len(args) and args[i].startswith("-"):
            i += 1
        if i >= len(args):
            return ""
        value = args[i]
        i += 1
        if i >= len(args):
            return ""
        patterns_block = args[i]
        pairs = self._tcl_list(patterns_block)
        j = 0
        while j + 1 < len(pairs):
            pat, body = pairs[j], pairs[j + 1]
            if pat == "default" or pat == value or re.fullmatch(pat, value):
                return self._exec_block(body)
            j += 2
        return ""

    def _cmd_list(self, args: List[str]) -> str:
        return self._list_to_str(args)

    def _cmd_lindex(self, args: List[str]) -> str:
        if len(args) < 2:
            return ""
        lst = self._tcl_list(args[0])
        try:
            idx = int(float(args[1]))
            return lst[idx] if 0 <= idx < len(lst) else ""
        except (ValueError, IndexError):
            return ""

    def _cmd_lrange(self, args: List[str]) -> str:
        if len(args) < 3:
            return ""
        lst = self._tcl_list(args[0])
        first = int(float(args[1]))
        last = int(float(args[2])) if args[2] != "end" else len(lst) - 1
        return self._list_to_str(lst[first : last + 1])

    def _cmd_lappend(self, args: List[str]) -> str:
        name = args[0]
        try:
            current = self._tcl_list(self._get_var(name))
        except _TclError:
            current = []
        current.extend(args[1:])
        val = self._list_to_str(current)
        self._set_var(name, val)
        return val

    def _cmd_lrepeat(self, args: List[str]) -> str:
        if len(args) < 2:
            return ""
        count = int(float(args[0]))
        value = args[1]
        return self._list_to_str([value for _ in range(max(count, 0))])

    def _cmd_lassign(self, args: List[str]) -> str:
        if len(args) < 2:
            return ""
        lst = self._tcl_list(args[0])
        names = args[1:]
        for i, name in enumerate(names):
            if name == "":
                continue
            self._set_var(name, lst[i] if i < len(lst) else "")
        return lst[len(names)] if len(lst) > len(names) else ""

    def _cmd_lset(self, args: List[str]) -> str:
        if len(args) < 3:
            return ""
        name = args[0]
        try:
            lst = self._tcl_list(self._get_var(name))
        except _TclError:
            lst = []
        try:
            idx = int(float(args[1]))
        except ValueError:
            return ""
        if idx < 0:
            idx += len(lst)
        if idx < 0:
            return ""
        while len(lst) <= idx:
            lst.append("")
        lst[idx] = args[2]
        val = self._list_to_str(lst)
        self._set_var(name, val)
        return val

    def _cmd_string(self, args: List[str]) -> str:
        if not args:
            return ""
        sub = args[0].lower()
        rest = args[1:]
        if sub == "length":
            return str(len(rest[0])) if rest else "0"
        if sub == "index":
            s, i = rest[0], int(rest[1]) if len(rest) > 1 else 0
            return s[i] if 0 <= i < len(s) else ""
        if sub == "range":
            s = rest[0]
            first = int(rest[1]) if len(rest) > 1 else 0
            last = int(rest[2]) if len(rest) > 2 else len(s) - 1
            return s[first : last + 1]
        if sub == "toupper":
            return rest[0].upper() if rest else ""
        if sub == "tolower":
            return rest[0].lower() if rest else ""
        if sub == "trim":
            chars = rest[1] if len(rest) > 1 else None
            return rest[0].strip(chars) if rest else ""
        if sub == "trimleft":
            chars = rest[1] if len(rest) > 1 else None
            return rest[0].lstrip(chars) if rest else ""
        if sub == "trimright":
            chars = rest[1] if len(rest) > 1 else None
            return rest[0].rstrip(chars) if rest else ""
        if sub == "compare":
            a, b = (rest[0] if rest else ""), (rest[1] if len(rest) > 1 else "")
            return "0" if a == b else ("-1" if a < b else "1")
        if sub == "equal":
            a, b = (rest[0] if rest else ""), (rest[1] if len(rest) > 1 else "")
            return "1" if a == b else "0"
        if sub == "match":
            pat, s = (rest[0] if rest else ""), (rest[1] if len(rest) > 1 else "")
            return (
                "1"
                if re.fullmatch(pat.replace("*", ".*").replace("?", "."), s)
                else "0"
            )
        if sub == "replace":
            s = rest[0]
            first, last = int(rest[1]), int(rest[2]) if len(rest) > 2 else len(s)
            replacement = rest[3] if len(rest) > 3 else ""
            return s[:first] + replacement + s[last + 1 :]
        if sub == "repeat":
            return rest[0] * int(rest[1]) if len(rest) >= 2 else ""
        if sub in ("first", "last"):
            needle, haystack = rest[0], (rest[1] if len(rest) > 1 else "")
            if sub == "first":
                return str(haystack.find(needle))
            return str(haystack.rfind(needle))
        if sub == "is":
            cls_ = rest[0].lower() if rest else ""
            val = rest[1] if len(rest) > 1 else ""
            if cls_ == "integer":
                try:
                    int(val)
                    return "1"
                except ValueError:
                    return "0"
            if cls_ == "double":
                try:
                    float(val)
                    return "1"
                except ValueError:
                    return "0"
            if cls_ == "alpha":
                return "1" if val.isalpha() else "0"
            if cls_ == "alnum":
                return "1" if val.isalnum() else "0"
            if cls_ == "space":
                return "1" if val.isspace() else "0"
        return ""

    def _cmd_format(self, args: List[str]) -> str:
        if not args:
            return ""
        fmt = args[0]
        vals = args[1:]
        try:
            # Convert Tcl format specifiers to Python
            result = fmt % tuple(
                int(v)
                if "%d" in fmt or "%i" in fmt or "%o" in fmt or "%x" in fmt
                else float(v)
                if "%" in fmt and any(c in fmt for c in "feEgG")
                else v
                for v in vals
            )
            return result
        except Exception:  # noqa: BLE001
            return fmt

    def _cmd_regexp(self, args: List[str]) -> str:
        if len(args) < 2:
            return "0"
        pat, s = args[0], args[1]
        m = re.search(pat, s)
        return "1" if m else "0"

    def _cmd_regsub(self, args: List[str]) -> str:
        if len(args) < 3:
            return ""
        pat, s, repl = args[0], args[1], args[2]
        return re.sub(pat, repl, s)

    def _cmd_catch(self, args: List[str]) -> str:
        if not args:
            return "0"
        body = args[0]
        var = args[1] if len(args) > 1 else None
        try:
            result = self._exec_block(body)
            if var:
                self._set_var(var, result)
            return "0"
        except _TclError as e:
            if var:
                self._set_var(var, str(e))
            return "1"
        except Exception as e:  # noqa: BLE001
            if var:
                self._set_var(var, str(e))
            return "1"

    def _cmd_array(self, args: List[str]) -> str:
        if not args:
            return ""
        sub = args[0].lower()
        name = args[1].upper() if len(args) > 1 else ""
        if sub == "set":
            pairs = self._tcl_list(args[2]) if len(args) > 2 else []
            for i in range(0, len(pairs) - 1, 2):
                self._set_array(name, pairs[i], pairs[i + 1])
            return ""
        if sub == "get":
            arr = self._arrays.get(name, {})
            return self._list_to_str([x for pair in arr.items() for x in pair])
        if sub == "names":
            return self._list_to_str(list(self._arrays.get(name, {}).keys()))
        if sub == "exists":
            return "1" if name in self._arrays else "0"
        if sub == "unset":
            self._arrays.pop(name, None)
        if sub == "size":
            return str(len(self._arrays.get(name, {})))
        return ""

    def _cmd_info(self, args: List[str]) -> str:
        if not args:
            return ""
        sub = args[0].lower()
        if sub == "exists":
            name = args[1] if len(args) > 1 else ""
            try:
                self._get_var(name)
                return "1"
            except _TclError:
                return "0"
        if sub == "procs":
            return self._list_to_str(list(self._procs.keys()))
        if sub == "vars":
            return self._list_to_str(list(self._scope().keys()))
        if sub == "tclversion":
            return "8.6"
        return ""

    # ------------------------------------------------------------------
    # List helpers
    # ------------------------------------------------------------------

    def _tcl_list(self, s: str) -> List[str]:
        """Parse a Tcl list string into Python list."""
        if not s:
            return []
        items: List[str] = []
        tokens = _tcl_tokenize(s)
        for t in tokens:
            t = t.strip()
            if t.startswith('"') and t.endswith('"'):
                items.append(t[1:-1])
            elif t:
                items.append(t)
        return items or s.split()

    @staticmethod
    def _list_to_str(items: List[str]) -> str:
        """Convert a Python list to a Tcl list string."""
        parts = []
        for item in items:
            if " " in item or not item:
                parts.append("{" + item + "}")
            else:
                parts.append(item)
        return " ".join(parts)
