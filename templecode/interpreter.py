from __future__ import annotations

import ast
import csv
import json
import math
import random
from datetime import datetime
import os
import threading
import time
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple


# ----------------------------- IO Abstractions ----------------------------- #


class IOBase:
    """Abstract I/O interface for the interpreter."""

    def write(self, text: str) -> None:  # pragma: no cover
        raise NotImplementedError

    def read(self, prompt: str = "") -> str:  # pragma: no cover
        raise NotImplementedError


class ConsoleIO(IOBase):
    def write(self, text: str) -> None:
        print(text, end="")

    def read(self, prompt: str = "") -> str:
        return input(prompt)


# ----------------------------- Turtle Abstractions ------------------------- #


class TurtleAPI:
    """Abstract turtle drawing API."""

    def clear(self) -> None:  # pragma: no cover
        pass

    def forward(self, n: float) -> None:  # pragma: no cover
        pass

    def left(self, a: float) -> None:  # pragma: no cover
        pass

    def right(self, a: float) -> None:  # pragma: no cover
        pass

    def penup(self) -> None:  # pragma: no cover
        pass

    def pendown(self) -> None:  # pragma: no cover
        pass

    def setxy(
        self, x: float, y: float
    ) -> None:  # pragma: no cover - drawing abstraction
        pass

    def color(self, r: int, g: int, b: int) -> None:  # pragma: no cover
        pass

    # Extended drawing API (optional for concrete turtles)
    def penwidth(self, w: float) -> None:  # pragma: no cover
        pass

    def fillcolor(self, r: int, g: int, b: int) -> None:  # pragma: no cover
        pass

    def background(self, r: int, g: int, b: int) -> None:  # pragma: no cover
        pass

    def text(self, x: float, y: float, s: str) -> None:  # pragma: no cover
        pass

    def rect(
        self, x: float, y: float, w: float, h: float, fill: bool = False
    ) -> None:  # pragma: no cover
        pass

    def circle(
        self, x: float, y: float, r: float, fill: bool = False
    ) -> None:  # pragma: no cover
        pass

    def home(self) -> None:  # pragma: no cover
        pass

    def setheading(self, a: float) -> None:  # pragma: no cover
        pass


class NullTurtle(TurtleAPI):
    pass


# ----------------------------- Safe Expression Eval ----------------------- #


def _fn_upper(s: Any) -> str:
    return str(s).upper()


def _fn_lower(s: Any) -> str:
    return str(s).lower()


def _fn_substr(s: Any, start: int, length: Optional[int] = None) -> str:
    st = int(start)
    text = str(s)
    if length is None:
        return text[st:]
    return text[st : st + int(length)]


def _fn_find(s: Any, sub: Any) -> int:
    return str(s).find(str(sub))


def _fn_replace(s: Any, old: Any, new: Any) -> str:
    return str(s).replace(str(old), str(new))


def _fn_split(s: Any, sep: Optional[Any] = None) -> List[str]:
    return str(s).split(None if sep is None else str(sep))


def _fn_join(items: List[Any], sep: Optional[Any] = None) -> str:
    return ("" if sep is None else str(sep)).join(str(x) for x in items)


def _fn_rand() -> float:
    return random.random()


def _fn_randint(a: float, b: float) -> int:
    return int(random.randint(int(a), int(b)))


def _fn_now() -> str:
    return datetime.now().isoformat(sep=" ", timespec="seconds")


def _fn_date() -> str:
    return datetime.now().date().isoformat()


def _fn_time() -> str:
    return datetime.now().time().strftime("%H:%M:%S")


def _fn_avg(values: List[float]) -> float:
    return sum(values) / len(values) if values else 0.0


_ALLOWED_FUNCS: Dict[str, Any] = {
    **{
        k: getattr(math, k)
        for k in (
            "sin",
            "cos",
            "tan",
            "asin",
            "acos",
            "atan",
            "sqrt",
            "log",
            "log10",
            "floor",
            "ceil",
            "fabs",
            "pow",
            "pi",
            "e",
            "tau",
            "degrees",
            "radians",
        )
        if hasattr(math, k)
    },
    "abs": abs,
    "min": min,
    "max": max,
    "len": len,
    "int": int,
    "float": float,
    "str": str,
    "round": round,
    # string & list helpers
    "upper": _fn_upper,
    "lower": _fn_lower,
    "substr": _fn_substr,
    "find": _fn_find,
    "replace": _fn_replace,
    "split": _fn_split,
    "join": _fn_join,
    # randomness/time
    "rand": _fn_rand,
    "randint": _fn_randint,
    "now": _fn_now,
    "date": _fn_date,
    "time": _fn_time,
    # aggregates
    "sum": sum,
    "avg": _fn_avg,
}


class SafeEval(ast.NodeTransformer):
    """Restrict Python AST to a safe subset for expressions."""

    ALLOWED_NODES = (
        ast.Expression,
        ast.BinOp,
        ast.UnaryOp,
        ast.Num,
        ast.Constant,
        ast.Name,
        ast.Load,
        ast.Dict,
        ast.Add,
        ast.Sub,
        ast.Mult,
        ast.Div,
        ast.Mod,
        ast.Pow,
        ast.USub,
        ast.UAdd,
        ast.Call,
        ast.Compare,
        ast.Eq,
        ast.NotEq,
        ast.Lt,
        ast.LtE,
        ast.Gt,
        ast.GtE,
        ast.BoolOp,
        ast.And,
        ast.Or,
        ast.IfExp,
        ast.Subscript,
        ast.Index,
        ast.Slice,
        ast.Tuple,
        ast.List,
    )

    def visit(self, node):  # type: ignore[override]
        if not isinstance(node, self.ALLOWED_NODES):
            raise ValueError(f"Disallowed expression: {type(node).__name__}")
        return super().visit(node)


def _eval_ast(node: ast.AST, env: Dict[str, Any]) -> Any:
    if isinstance(node, ast.Expression):
        return _eval_ast(node.body, env)
    if isinstance(node, ast.Constant):
        return node.value
    if isinstance(node, ast.Num):  # pragma: no cover
        return node.n
    if isinstance(node, ast.Name):
        if node.id in env:
            return env[node.id]
        if node.id in _ALLOWED_FUNCS:
            return _ALLOWED_FUNCS[node.id]
        raise ValueError(f"Unknown name: {node.id}")
    if isinstance(node, ast.BinOp):
        left = _eval_ast(node.left, env)
        right = _eval_ast(node.right, env)
        if isinstance(node.op, ast.Add):
            return left + right
        if isinstance(node.op, ast.Sub):
            return left - right
        if isinstance(node.op, ast.Mult):
            return left * right
        if isinstance(node.op, ast.Div):
            return left / right
        if isinstance(node.op, ast.Mod):
            return left % right
        if isinstance(node.op, ast.Pow):
            return left**right
        raise ValueError("Unsupported binary operator")
    if isinstance(node, ast.UnaryOp):
        opd = _eval_ast(node.operand, env)
        if isinstance(node.op, ast.UAdd):
            return +opd
        if isinstance(node.op, ast.USub):
            return -opd
        raise ValueError("Unsupported unary operator")
    if isinstance(node, ast.BoolOp):
        if isinstance(node.op, ast.And):
            val = True
            for v in node.values:
                val = val and bool(_eval_ast(v, env))
                if not val:
                    break
            return val
        if isinstance(node.op, ast.Or):
            val = False
            for v in node.values:
                val = val or bool(_eval_ast(v, env))
                if val:
                    break
            return val
    if isinstance(node, ast.Compare):
        left = _eval_ast(node.left, env)
        for op, comp in zip(node.ops, node.comparators):
            right = _eval_ast(comp, env)
            if isinstance(op, ast.Eq) and not (left == right):
                return False
            if isinstance(op, ast.NotEq) and not (left != right):
                return False
            if isinstance(op, ast.Lt) and not (left < right):
                return False
            if isinstance(op, ast.LtE) and not (left <= right):
                return False
            if isinstance(op, ast.Gt) and not (left > right):
                return False
            if isinstance(op, ast.GtE) and not (left >= right):
                return False
            left = right
        return True
    if isinstance(node, ast.IfExp):
        return _eval_ast(
            node.body if _eval_ast(node.test, env) else node.orelse,
            env,
        )
    if isinstance(node, ast.Tuple):
        return tuple(_eval_ast(elt, env) for elt in node.elts)
    if isinstance(node, ast.List):
        return [_eval_ast(elt, env) for elt in node.elts]
    if isinstance(node, ast.Dict):
        result: Dict[Any, Any] = {}
        for k_node, v_node in zip(node.keys, node.values):
            if k_node is None or v_node is None:
                raise ValueError("Dict unpacking is not allowed")
            key = _eval_ast(k_node, env)
            val = _eval_ast(v_node, env)
            result[key] = val
        return result
    if isinstance(node, ast.Subscript):
        base = _eval_ast(node.value, env)
        if isinstance(node.slice, ast.Slice):
            s = node.slice
            if s.lower:
                lower = _eval_ast(s.lower, env)
            else:
                lower = None
            if s.upper:
                upper = _eval_ast(s.upper, env)
            else:
                upper = None
            if s.step:
                step = _eval_ast(s.step, env)
            else:
                step = None
            return base[lower:upper:step]
        idx = _eval_ast(node.slice, env)
        return base[idx]
    if isinstance(node, ast.Call):
        func = _eval_ast(node.func, env)
        if func not in _ALLOWED_FUNCS.values():
            raise ValueError("Function not allowed")
        args = [_eval_ast(a, env) for a in node.args]
        return func(*args)
    raise ValueError(f"Unsupported expression: {type(node).__name__}")


def eval_expr(expr: str, variables: Dict[str, Any]) -> Any:
    tree = ast.parse(expr, mode="eval")
    SafeEval().visit(tree)
    return _eval_ast(tree, {**_ALLOWED_FUNCS, **variables})


# ----------------------------- Program Model ------------------------------ #


def _is_comment_or_blank(line: str) -> bool:
    s = line.strip()
    return not s or s.upper().startswith("REM") or s.startswith("#")


@dataclass
class ForLoop:
    var: str
    end: float
    step: float
    start_line: int  # line index to jump back to after NEXT


@dataclass
class WhileLoop:
    condition: str
    start_line: int


@dataclass
class RepeatLoop:
    remaining: int
    start_line: int


# ----------------------------- Interpreter -------------------------------- #


class TempleInterpreter:
    def __init__(
        self,
        io: Optional[IOBase] = None,
        turtle: Optional[TurtleAPI] = None,
        stop_flag: Optional[threading.Event] = None,
    ) -> None:
        self.io = io or ConsoleIO()
        self.turtle = turtle or NullTurtle()
        self.stop_flag = stop_flag or threading.Event()
        self.trace: bool = False
        self.db_conns: Dict[str, Any] = {}

        self.variables: Dict[str, Any] = {}
        # scope stack: index 0 is globals; append for procedure calls
        self.scopes: List[Dict[str, Any]] = [self.variables]
        self.lines: List[str] = []
        self.labels: Dict[str, int] = {}
        self.procs: Dict[str, Tuple[int, int, List[str]]] = {}
        self.pc: int = 0
        self.for_stack: List[ForLoop] = []
        self.while_stack: List[WhileLoop] = []
        self.repeat_stack: List[RepeatLoop] = []
        self.call_stack: List[
            Tuple[
                int,
                List[ForLoop],
                List[WhileLoop],
                List[RepeatLoop],
                Optional[str],
                Any,
                int,
            ]
        ] = []
        # call frame tuple:
        # (return_pc, saved_for, saved_while, saved_repeat,
        #  into_var, return_value, end_line)

    # ------------------------- Public API ------------------------- #

    def load_program(self, code: str) -> None:
        raw_lines = code.splitlines()
        self.lines = []
        self.labels = {}
        self.procs = {}

        i = 0
        while i < len(raw_lines):
            raw = raw_lines[i]
            line = raw.rstrip("\n")
            if _is_comment_or_blank(line):
                self.lines.append(line)
                i += 1
                continue

            # Numeric line number support: `10 PRINT "HI"`
            parts = line.lstrip().split(maxsplit=1)
            if parts and parts[0].isdigit():
                label = parts[0]
                body = parts[1] if len(parts) > 1 else ""
                self.labels[label] = len(self.lines)
                line = body

            # Label support: `start:`
            stripped = line.lstrip()
            if ":" in stripped:
                first, rest = stripped.split(":", 1)
                if first and all(ch.isalnum() or ch == "_" for ch in first):
                    self.labels[first] = len(self.lines)
                    line = rest

            upper = line.strip().upper()
            if upper.startswith("PROC "):
                # parse proc signature: PROC name [params...]
                toks = self._tokenize(line)
                if len(toks) < 2:
                    raise ValueError("PROC requires a name")
                name = toks[1]
                params: List[str] = []
                if len(toks) > 2:
                    # everything after name are params (split by spaces/commas)
                    params = [
                        p
                        for p in self._split_args(self._rest_of_line(toks[2:]))
                        if p.isidentifier()
                    ]
                # Append the PROC line itself first
                self.lines.append(line)
                start_index = len(self.lines)  # first line after PROC
                # find matching ENDPROC
                j = i + 1
                end_index = -1
                while j < len(raw_lines):
                    if raw_lines[j].strip().upper().startswith("ENDPROC"):
                        # ENDPROC will be appended next; its index is the
                        # current length of self.lines
                        end_index = len(self.lines)
                        break
                    # we still push intermediate lines so indices align
                    self.lines.append(raw_lines[j])
                    j += 1
                if end_index == -1:
                    raise ValueError(f"PROC {name} missing ENDPROC")
                # record proc metadata
                self.procs[name] = (start_index, end_index, params)
                # append the ENDPROC line itself
                self.lines.append(raw_lines[j])
                # advance i to after ENDPROC
                i = j + 1
                continue

            self.lines.append(line)
            i += 1

        # Normalize lines (keep blank/comment lines but they no-op)

    def run(self, code: str) -> None:
        self.reset_state()
        self.load_program(code)
        self.run_program()

    def reset_state(self) -> None:
        self.variables.clear()
        self.scopes = [self.variables]
        self.pc = 0
        self.for_stack.clear()
        self.while_stack.clear()
        self.repeat_stack.clear()
        self.call_stack.clear()
        # Close any open DB connections
        try:
            for _name, conn in list(self.db_conns.items()):
                try:
                    conn.close()
                except Exception:
                    pass
        finally:
            self.db_conns.clear()

    def run_program(self) -> None:
        self.pc = 0
        while self.pc < len(self.lines):
            if self.stop_flag.is_set():
                break
            raw = self.lines[self.pc]
            if self.trace and not _is_comment_or_blank(raw):
                self.io.write(f"[line {self.pc+1}] {raw}\n")
            self._exec_line(raw)
            self.pc += 1

    # ------------------------- Internals -------------------------- #

    def _exec_line(self, raw_line: str) -> None:
        line = raw_line.strip()
        if not line or line.upper().startswith("REM") or line.startswith("#"):
            return

        # Case-insensitive keywords; keep string literals intact
        tokens = self._tokenize(line)
        if not tokens:
            return

        cmd = tokens[0].upper()
        args = tokens[1:]

        # BASIC / PILOT style
        if cmd in ("PRINT", "TYPE"):
            expr = self._rest_of_line(args)
            val = self._eval(expr)
            self.io.write(str(val) + "\n")
            return

        if cmd in ("INPUT", "ACCEPT"):
            if not args:
                raise ValueError("INPUT requires a variable name")
            var = args[0]
            prompt = ""
            if len(args) > 1:
                prompt = self._eval(self._rest_of_line(args[1:]))
                prompt = str(prompt)
            inp = self.io.read(prompt)
            self.variables[var] = self._coerce_input(inp)
            return

        if cmd == "LET" or self._is_assignment(tokens):
            if cmd == "LET":
                rest = args
            else:
                rest = tokens
            var, expr = self._parse_assignment(rest)
            self.variables[var] = self._eval(expr)
            return

        if cmd == "IF":
            # forms:
            # IF expr THEN PRINT ...
            # IF expr THEN GOTO label
            # IF expr THEN label
            then_idx = self._find_token(args, "THEN")
            if then_idx == -1:
                raise ValueError("IF requires THEN")
            cond_expr = self._join(args[:then_idx])
            else_idx = self._find_token(args, "ELSE")
            if else_idx != -1 and else_idx < then_idx:
                else_idx = -1  # treat malformed ELSE before THEN as absent
            if else_idx == -1:
                then_part = args[then_idx + 1 :]
                else_part: List[str] = []
            else:
                then_part = args[then_idx + 1 : else_idx]
                else_part = args[else_idx + 1 :]
            cond = bool(self._eval(cond_expr))
            if cond:
                if then_part:
                    # If it's GOTO/JUMP or label name
                    subcmd = then_part[0].upper()
                    if subcmd in ("GOTO", "JUMP"):
                        label = self._eval(self._rest_of_line(then_part[1:]))
                        self._goto(str(label))
                        return
                    # Inline statement (e.g., PRINT ...)
                    self._exec_line(self._rest_of_line(then_part))
            else:
                if else_part:
                    self._exec_line(self._rest_of_line(else_part))
            return
        if cmd == "PROC":
            # Skip procedure body during normal execution
            # Find end index from metadata and jump there
            if len(tokens) >= 2 and tokens[1] in self.procs:
                _, end_line, _ = self.procs[tokens[1]]
                self.pc = end_line
                return
            # If unknown, skip to next ENDPROC
            idx = self.pc + 1
            while idx < len(self.lines) and not self.lines[
                idx
            ].strip().upper().startswith("ENDPROC"):
                idx += 1
            self.pc = idx
            return

        if cmd == "ENDPROC":
            # End of procedure: pop call frame and return
            if not self.call_stack:
                return  # stray ENDPROC ignored
            (
                return_pc,
                saved_for,
                saved_while,
                saved_repeat,
                into_var,
                ret_val,
                _end_line,
            ) = self.call_stack.pop()
            # Pop local scope
            if len(self.scopes) > 1:
                self.scopes.pop()
            # Restore loop stacks
            self.for_stack = saved_for
            self.while_stack = saved_while
            self.repeat_stack = saved_repeat
            # Assign INTO var if requested
            if into_var:
                self._set_var(into_var, ret_val)
            # Jump back to caller
            self.pc = return_pc
            return

        if cmd == "RETURN":
            # RETURN [expr]
            ret_val = None
            if args:
                ret_val = self._eval(self._rest_of_line(args))
            if not self.call_stack:
                return
            (
                return_pc,
                saved_for,
                saved_while,
                saved_repeat,
                into_var,
                _old_ret,
                _end_line,
            ) = self.call_stack.pop()
            if len(self.scopes) > 1:
                self.scopes.pop()
            self.for_stack = saved_for
            self.while_stack = saved_while
            self.repeat_stack = saved_repeat
            if into_var:
                self._set_var(into_var, ret_val)
            self.pc = return_pc
            return

        if cmd == "CALL":
            # CALL name [args...] [INTO var]
            if not args:
                raise ValueError("CALL requires a procedure name")
            name = args[0]
            if name not in self.procs:
                raise ValueError(f"Unknown procedure: {name}")
            # parse INTO if present
            into_idx = self._find_token(args, "INTO")
            into_target: Optional[str] = None
            arg_tokens: List[str]
            if into_idx != -1:
                if into_idx + 1 >= len(args):
                    raise ValueError("INTO requires a variable name")
                into_target = args[into_idx + 1]
                arg_tokens = args[1:into_idx]
            else:
                arg_tokens = args[1:]
            # split args respecting quotes/commas (only split on commas)
            arg_str = self._join(arg_tokens)
            if arg_str.strip():
                # Split only on commas outside of quotes
                arg_exprs = self._split_on_comma(arg_str)
            else:
                arg_exprs = []
            start_line, end_line, params = self.procs[name]
            if len(arg_exprs) != len(params):
                raise ValueError(
                    (
                        f"CALL {name} expects {len(params)} args, "
                        f"got {len(arg_exprs)}"
                    )
                )
            # Evaluate arguments now in caller scope
            values = [self._eval(e) for e in arg_exprs]
            # Push frame: save stacks and return pc
            frame = (
                self.pc,  # return to this line
                self.for_stack.copy(),
                self.while_stack.copy(),
                self.repeat_stack.copy(),
                into_target,
                None,
                end_line,
            )
            self.call_stack.append(frame)
            # New local scope with params
            local_scope: Dict[str, Any] = dict(zip(params, values))
            self.scopes.append(local_scope)
            # Reset loop stacks for procedure
            self.for_stack = []
            self.while_stack = []
            self.repeat_stack = []
            # Jump to procedure start (pc will +1 after return)
            self.pc = start_line - 1
            return

        if cmd in ("GOTO", "JUMP"):
            if not args:
                raise ValueError("GOTO requires a label")
            label = self._eval(self._rest_of_line(args))
            self._goto(str(label))
            return

        if cmd == "SLEEP":
            amount = float(self._eval(self._rest_of_line(args)))
            # Cooperative sleep respecting stop flag
            end = time.time() + amount
            while time.time() < end and not self.stop_flag.is_set():
                time.sleep(0.01)
            return

        # ---------------------- MySQL Database I/O ---------------------- #
        if cmd == "DBCONNECT":
            # DBCONNECT name, host, user, password, database[, port]
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) not in (5, 6):
                raise ValueError(
                    "DBCONNECT name, host, user, password, database[, port]"
                )
            name = str(parts[0])
            host = str(self._eval(parts[1]))
            user = str(self._eval(parts[2]))
            password = str(self._eval(parts[3]))
            database = str(self._eval(parts[4]))
            port = int(float(self._eval(parts[5]))) if len(parts) == 6 else 3306
            try:
                import mysql.connector as _mysql  # type: ignore
            except Exception as e:  # pragma: no cover - import error path
                raise ValueError(
                    "MySQL support requires 'mysql-connector-python'."
                ) from e
            conn = _mysql.connect(
                host=host,
                user=user,
                password=password,
                database=database,
                port=port,
            )
            self.db_conns[name] = conn
            return
        if cmd == "DBDISCONNECT":
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 1:
                raise ValueError("DBDISCONNECT name")
            name = parts[0]
            conn = self.db_conns.get(name)
            if conn is not None:
                try:
                    conn.close()
                finally:
                    self.db_conns.pop(name, None)
            return
        if cmd in ("DBEXEC", "DBEXECPARAM"):
            parts = self._split_args(self._rest_of_line(args))
            if cmd == "DBEXEC":
                if len(parts) != 2:
                    raise ValueError("DBEXEC name, sql")
                name, sql_expr = parts[0], parts[1]
                params_val: Any = None
            else:
                if len(parts) != 3:
                    raise ValueError("DBEXECPARAM name, sql, params")
                name, sql_expr, params_expr = parts[0], parts[1], parts[2]
                params_val = self._eval(params_expr)
            conn = self.db_conns.get(name)
            if conn is None:
                raise ValueError(f"Unknown DB connection: {name}")
            sql = str(self._eval(sql_expr))
            cur = conn.cursor()
            if params_val is None:
                cur.execute(sql)
            else:
                cur.execute(sql, params_val)
            conn.commit()
            cur.close()
            return
        if cmd in ("DBQUERY", "DBQUERYPARAM"):
            # DBQUERY name, sql [INTO var]
            # DBQUERYPARAM name, sql, params [INTO var]
            into_idx = self._find_token(args, "INTO")
            if into_idx != -1:
                head = args[:into_idx]
                target_var = args[into_idx + 1] if into_idx + 1 < len(args) else None
            else:
                head = args
                target_var = None
            parts = self._split_args(self._rest_of_line(head))
            if cmd == "DBQUERY":
                if len(parts) != 2:
                    raise ValueError("DBQUERY name, sql [INTO var]")
                name, sql_expr = parts[0], parts[1]
                params_val = None
            else:
                if len(parts) != 3:
                    raise ValueError("DBQUERYPARAM name, sql, params [INTO var]")
                name, sql_expr, params_expr = parts[0], parts[1], parts[2]
                params_val = self._eval(params_expr)
            conn = self.db_conns.get(name)
            if conn is None:
                raise ValueError(f"Unknown DB connection: {name}")
            sql = str(self._eval(sql_expr))
            cur = conn.cursor()
            if params_val is None:
                cur.execute(sql)
            else:
                cur.execute(sql, params_val)
            db_rows = cur.fetchall()
            cols = [d[0] for d in cur.description] if cur.description else []
            cur.close()
            # Convert to list of dicts if we have column names
            if cols:
                out = [
                    {cols[i]: row[i] for i in range(min(len(cols), len(row)))}
                    for row in db_rows
                ]
            else:
                out = list(db_rows)
            if target_var:
                self._set_var(target_var, out)
            return

        # Debugging and utilities
        if cmd == "TRACE":
            if not args:
                raise ValueError("TRACE ON|OFF")
            val = args[0].upper()
            if val == "ON":
                self.trace = True
            elif val == "OFF":
                self.trace = False
            else:
                raise ValueError("TRACE expects ON or OFF")
            return
        if cmd == "PAUSE":
            prompt = self._rest_of_line(args).strip()
            if not prompt:
                prompt = "Press Enter to continue..."
            _ = self.io.read(prompt)
            return
        if cmd == "DUMPVARS":
            env: Dict[str, Any] = {}
            for scope in self.scopes:
                env.update(scope)
            for k in sorted(env.keys()):
                self.io.write(f"{k} = {env[k]}\n")
            return
        if cmd == "ASSERT":
            parts = self._split_on_comma(self._rest_of_line(args))
            if not parts:
                raise ValueError("ASSERT requires an expression")
            ok = bool(self._eval(parts[0]))
            msg = parts[1] if len(parts) > 1 else "Assertion failed"
            if not ok:
                raise ValueError(str(msg))
            return

        # File and CSV I/O
        if cmd == "READFILE":
            # READFILE var, path
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 2:
                raise ValueError("READFILE var, path")
            var, path_expr = parts[0], parts[1]
            path = str(self._eval(path_expr))
            with open(path, "r", encoding="utf-8") as f:
                data = f.read()
            self._set_var(var, data)
            return
        if cmd == "APPENDFILE":
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 2:
                raise ValueError("APPENDFILE path, expr")
            path = str(self._eval(parts[0]))
            data = str(self._eval(parts[1]))
            with open(path, "a", encoding="utf-8") as f:
                f.write(data)
            return
        if cmd == "EXISTS":
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 2:
                raise ValueError("EXISTS var, path")
            var, path_expr = parts[0], parts[1]
            path = str(self._eval(path_expr))
            self._set_var(var, os.path.exists(path))
            return
        if cmd == "DIRLIST":
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 2:
                raise ValueError("DIRLIST var, path")
            var, path_expr = parts[0], parts[1]
            path = str(self._eval(path_expr))
            self._set_var(var, list(os.listdir(path)))
            return
        if cmd == "READJSON":
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 2:
                raise ValueError("READJSON var, path")
            var, path_expr = parts[0], parts[1]
            path = str(self._eval(path_expr))
            with open(path, "r", encoding="utf-8") as f:
                data = json.load(f)
            self._set_var(var, data)
            return
        if cmd == "WRITEJSON":
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 2:
                raise ValueError("WRITEJSON path, data")
            path = str(self._eval(parts[0]))
            data = self._eval(parts[1])
            with open(path, "w", encoding="utf-8") as f:
                json.dump(data, f, ensure_ascii=False, indent=2)
            return

        if cmd == "WRITEFILE":
            # WRITEFILE path, expr
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 2:
                raise ValueError("WRITEFILE path, expr")
            path = str(self._eval(parts[0]))
            data = str(self._eval(parts[1]))
            with open(path, "w", encoding="utf-8") as f:
                f.write(data)
            return

        if cmd == "CSVREAD":
            # CSVREAD var, path
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 2:
                raise ValueError("CSVREAD var, path")
            var, path_expr = parts[0], parts[1]
            path = str(self._eval(path_expr))
            rows: List[List[str]] = []
            with open(path, "r", encoding="utf-8", newline="") as f:
                reader = csv.reader(f)
                rows = [list(r) for r in reader]
            self._set_var(var, rows)
            return

        if cmd == "CSVWRITE":
            # CSVWRITE path, rows
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 2:
                raise ValueError("CSVWRITE path, rows")
            path = str(self._eval(parts[0]))
            rows_val = self._eval(parts[1])
            with open(path, "w", encoding="utf-8", newline="") as f:
                writer = csv.writer(f)
                for row in rows_val:
                    writer.writerow(list(row))
            return

        if cmd == "FOR":
            # FOR i = 1 TO 10 [STEP 2]
            eq = self._find_token(args, "=")
            to = self._find_token(args, "TO")
            if eq == -1 or to == -1 or eq == 0:
                raise ValueError("FOR syntax: FOR var = start TO end [STEP step]")
            var = args[0]
            start_expr = self._join(args[eq + 1 : to])
            start = float(self._eval(start_expr))
            step = 1.0
            end_expr: List[str]
            step_idx = self._find_token(args, "STEP")
            if step_idx != -1:
                end_expr = args[to + 1 : step_idx]
                rest_tokens = args[step_idx + 1 :]
                step = float(self._eval(self._rest_of_line(rest_tokens)))
            else:
                end_expr = args[to + 1 :]
            end_val = float(self._eval(self._join(end_expr)))
            self.variables[var] = start
            self.for_stack.append(
                ForLoop(var=var, end=end_val, step=step, start_line=self.pc)
            )
            return

        if cmd == "NEXT":
            if not self.for_stack:
                raise ValueError("NEXT without FOR")
            for_loop = self.for_stack[-1]
            base_val = float(self.variables.get(for_loop.var, 0))
            var_val = base_val + for_loop.step
            self.variables[for_loop.var] = var_val
            # Determine loop continuation depending on step sign
            if (for_loop.step >= 0 and var_val <= for_loop.end) or (
                for_loop.step < 0 and var_val >= for_loop.end
            ):
                # jump back to line after FOR (pc will +1)
                self.pc = for_loop.start_line
            else:
                self.for_stack.pop()
            return

        if cmd == "WHILE":
            cond_expr = self._rest_of_line(args)
            self.while_stack.append(WhileLoop(condition=cond_expr, start_line=self.pc))
            return

        if cmd == "ENDWHILE":
            if not self.while_stack:
                raise ValueError("ENDWHILE without WHILE")
            while_loop = self.while_stack[-1]
            if bool(self._eval(while_loop.condition)):
                self.pc = while_loop.start_line
            else:
                self.while_stack.pop()
            return

        if cmd == "REPEAT":
            n = int(float(self._eval(self._rest_of_line(args))))
            if n <= 0:
                return
            self.repeat_stack.append(RepeatLoop(remaining=n, start_line=self.pc))
            return

        if cmd == "ENDREPEAT":
            if not self.repeat_stack:
                raise ValueError("ENDREPEAT without REPEAT")
            repeat_loop = self.repeat_stack[-1]
            repeat_loop.remaining -= 1
            if repeat_loop.remaining > 0:
                self.pc = repeat_loop.start_line
            else:
                self.repeat_stack.pop()
            return

        # Logo-like turtle commands
        if cmd in ("FORWARD", "FD"):
            self.turtle.forward(float(self._eval(self._rest_of_line(args))))
            return
        if cmd in ("LEFT", "LT"):
            self.turtle.left(float(self._eval(self._rest_of_line(args))))
            return
        if cmd in ("RIGHT", "RT"):
            self.turtle.right(float(self._eval(self._rest_of_line(args))))
            return
        if cmd in ("PENUP", "PU"):
            self.turtle.penup()
            return
        if cmd in ("PENDOWN", "PD"):
            self.turtle.pendown()
            return
        if cmd in ("CLEAR", "CLS"):
            self.turtle.clear()
            return
        if cmd == "SETXY":
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 2:
                raise ValueError("SETXY requires two numbers")
            x = float(self._eval(parts[0]))
            y = float(self._eval(parts[1]))
            self.turtle.setxy(x, y)
            return
        if cmd == "COLOR":
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) == 1:
                # maybe a color name but for simplicity, ignore names and no-op
                return
            if len(parts) != 3:
                raise ValueError("COLOR r g b")
            r, g, b = (int(float(self._eval(p))) for p in parts)
            self.turtle.color(r, g, b)
            return
        if cmd == "PENWIDTH":
            w = float(self._eval(self._rest_of_line(args)))
            self.turtle.penwidth(w)
            return
        if cmd == "FILLCOLOR":
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 3:
                raise ValueError("FILLCOLOR r g b")
            r, g, b = (int(float(self._eval(p))) for p in parts)
            self.turtle.fillcolor(r, g, b)
            return
        if cmd == "BACKGROUND":
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 3:
                raise ValueError("BACKGROUND r g b")
            r, g, b = (int(float(self._eval(p))) for p in parts)
            self.turtle.background(r, g, b)
            return
        if cmd == "TEXT":
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 3:
                raise ValueError("TEXT x, y, text")
            x = float(self._eval(parts[0]))
            y = float(self._eval(parts[1]))
            s = str(self._eval(parts[2]))
            self.turtle.text(x, y, s)
            return
        if cmd == "RECT":
            # RECT x, y, w, h [FILL]
            tokens_up = [t.upper() for t in args]
            fill = "FILL" in tokens_up
            if fill:
                # remove FILL token for parsing numbers
                args = [t for t in args if t.upper() != "FILL"]
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 4:
                raise ValueError("RECT x, y, w, h [FILL]")
            x, y, w, h = (float(self._eval(p)) for p in parts)
            self.turtle.rect(x, y, w, h, fill=fill)
            return
        if cmd == "CIRCLE":
            # CIRCLE x, y, r [FILL]
            tokens_up = [t.upper() for t in args]
            fill = "FILL" in tokens_up
            if fill:
                args = [t for t in args if t.upper() != "FILL"]
            parts = self._split_args(self._rest_of_line(args))
            if len(parts) != 3:
                raise ValueError("CIRCLE x, y, r [FILL]")
            x = float(self._eval(parts[0]))
            y = float(self._eval(parts[1]))
            rad = float(self._eval(parts[2]))
            self.turtle.circle(x, y, rad, fill=fill)
            return
        if cmd == "HOME":
            self.turtle.home()
            return
        if cmd == "SETHEADING":
            a = float(self._eval(self._rest_of_line(args)))
            self.turtle.setheading(a)
            return

        # Inline evaluation: allow bare expressions that evaluate to a value
        try:
            _ = self._eval(self._rest_of_line(tokens))
            return
        except Exception as e:
            raise ValueError(f"Unknown statement: {raw_line}") from e

    # ------------------------- Helpers ---------------------------- #

    def _goto(self, label: str) -> None:
        if label not in self.labels:
            raise ValueError(f"Unknown label: {label}")
        self.pc = self.labels[label]

    def _rest_of_line(self, tokens: List[str]) -> str:
        return self._join(tokens)

    @staticmethod
    def _join(tokens: List[str]) -> str:
        return " ".join(tokens)

    @staticmethod
    def _split_args(s: str) -> List[str]:
        # split by spaces unless in quotes; allow comma as separator too
        out: List[str] = []
        buf = []
        in_string = False
        quote_char = ""
        for ch in s:
            if in_string:
                buf.append(ch)
                if ch == quote_char:
                    in_string = False
                continue
            if ch in ('"', "'"):
                in_string = True
                quote_char = ch
                buf.append(ch)
                continue
            if ch in [",", " "]:
                if buf:
                    out.append("".join(buf).strip())
                    buf = []
                continue
            buf.append(ch)
        if buf:
            out.append("".join(buf).strip())
        return [p for p in out if p != ""]

    @staticmethod
    def _split_on_comma(s: str) -> List[str]:
        """Split on commas, but not inside quotes."""
        out: List[str] = []
        buf = []
        in_string = False
        quote_char = ""
        for ch in s:
            if in_string:
                buf.append(ch)
                if ch == quote_char:
                    in_string = False
                continue
            if ch in ('"', "'"):
                in_string = True
                quote_char = ch
                buf.append(ch)
                continue
            if ch == ",":
                if buf:
                    out.append("".join(buf).strip())
                    buf = []
                continue
            buf.append(ch)
        if buf:
            out.append("".join(buf).strip())
        return [p for p in out if p != ""]

    @staticmethod
    def _find_token(tokens: List[str], target: str) -> int:
        target_up = target.upper()
        for i, t in enumerate(tokens):
            if t.upper() == target_up:
                return i
        return -1

    @staticmethod
    def _is_assignment(tokens: List[str]) -> bool:
        # form: name = expr
        return len(tokens) >= 3 and tokens[1] == "=" and tokens[0].isidentifier()

    @staticmethod
    def _parse_assignment(tokens: List[str]) -> Tuple[str, str]:
        # tokens like: [var, '=', expr...]
        if len(tokens) < 3 or tokens[1] != "=":
            raise ValueError("Assignment syntax: var = expr")
        var = tokens[0]
        expr = " ".join(tokens[2:])
        return var, expr

    def _coerce_input(self, value: str) -> Any:
        # try int, then float, else string
        try:
            if value.strip().isdigit() or (
                value.strip().startswith("-") and value.strip()[1:].isdigit()
            ):
                return int(value)
            return float(value)
        except ValueError:
            return value

    def _eval(self, expr: str) -> Any:
        # Build an environment from topmost scope down to globals
        env: Dict[str, Any] = {}
        for scope in self.scopes:
            env.update(scope)
        return eval_expr(expr, env)

    def _set_var(self, name: str, value: Any) -> None:
        # assign to nearest scope that has the name; otherwise top scope
        for idx in range(len(self.scopes) - 1, -1, -1):
            if name in self.scopes[idx]:
                self.scopes[idx][name] = value
                return
        # not found: assign in current (top) scope
        self.scopes[-1][name] = value

    @staticmethod
    def _tokenize(line: str) -> List[str]:
        # Split while preserving quoted strings as single tokens
        tokens: List[str] = []
        cur = []
        in_str = False
        q = ""
        i = 0
        while i < len(line):
            ch = line[i]
            if in_str:
                cur.append(ch)
                if ch == q:
                    in_str = False
                i += 1
                continue
            if ch in ('"', "'"):
                if cur:
                    tokens.append("".join(cur))
                    cur = []
                in_str = True
                q = ch
                cur.append(ch)
                i += 1
                continue
            if ch.isspace():
                if cur:
                    tokens.append("".join(cur))
                    cur = []
                i += 1
                continue
            # Handle multi-character operators
            if ch in ["<", ">", "!", "="]:
                if cur:
                    tokens.append("".join(cur))
                    cur = []
                # Check for <=, >=, !=, ==
                if i + 1 < len(line) and line[i + 1] == "=":
                    tokens.append(ch + "=")
                    i += 2
                    continue
                tokens.append(ch)
                i += 1
                continue
            if ch in [",", ":"]:
                if cur:
                    tokens.append("".join(cur))
                    cur = []
                tokens.append(ch)
                i += 1
                continue
            cur.append(ch)
            i += 1
        if cur:
            tokens.append("".join(cur))
        return tokens
