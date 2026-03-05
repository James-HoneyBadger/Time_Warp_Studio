"""Smalltalk language executor for Time Warp Studio.

Educational Smalltalk interpreter — whole-program execution.
Implements a teaching subset of Smalltalk-80/Pharo:
  Transcript showCr: / Transcript show:
  Variable declarations: | x y z |
  Assignment: x := expr
  Message sends (unary, binary, keyword)
  Simple classes and methods
  Blocks: [ ... ] value, whileTrue:, timesRepeat:
  Basic collections: OrderedCollection, Array
  Control: ifTrue:/ifFalse:, whileTrue:, 1 to: n do:
"""

from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


def execute_smalltalk(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a Smalltalk program and return all output."""
    env = SmalltalkEnvironment(interpreter, turtle)
    return env.run(source)


class SmalltalkEnvironment:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []
        self._globals: dict[str, Any] = {}
        self._classes: dict[str, "STClass"] = {}
        self._setup_globals()

    def _emit(self, text: str):
        self._output.append(str(text))

    def run(self, source: str) -> str:
        try:
            stmts = self._split_statements(source)
            for stmt in stmts:
                stmt = stmt.strip()
                if stmt:
                    self._eval_stmt(stmt, self._globals)
        except STError as e:
            self._emit(f"❌ Smalltalk error: {e}")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}")
        return "\n".join(self._output)

    def _setup_globals(self):
        output = self._output
        env = self

        class _Transcript:
            @staticmethod
            def showCr_(text):
                output.append(str(text))
                return text

            @staticmethod
            def show_(text):
                output.append(str(text))
                return text

            @staticmethod
            def print_(obj):
                output.append(repr(obj))
                return obj

            @staticmethod
            def nl():
                output.append("")
                return None

        self._globals["Transcript"] = _Transcript()
        self._globals["nil"] = None
        self._globals["true"] = True
        self._globals["false"] = False
        self._globals["OrderedCollection"] = STOrderedCollection
        self._globals["Array"] = list

    # ------------------------------------------------------------------
    # Statement splitter
    # ------------------------------------------------------------------

    def _split_statements(self, source: str) -> list[str]:
        """Split on periods (statement terminators), ignoring string contents."""
        stmts = []
        current = []
        depth_sq = 0  # [ ] depth
        depth_p = 0  # ( ) depth
        in_str = False
        in_comment = False
        i = 0
        while i < len(source):
            ch = source[i]
            if in_comment:
                if ch == '"':
                    in_comment = False
                i += 1
                continue
            if ch == '"':
                in_comment = True
                i += 1
                continue
            if ch == "'":
                current.append(ch)
                i += 1
                while i < len(source) and source[i] != "'":
                    current.append(source[i])
                    i += 1
                if i < len(source):
                    current.append(source[i])
                    i += 1
                continue
            if ch == "[":
                depth_sq += 1
            elif ch == "]":
                depth_sq -= 1
            elif ch == "(":
                depth_p += 1
            elif ch == ")":
                depth_p -= 1
            elif ch == "." and depth_sq == 0 and depth_p == 0:
                text = "".join(current).strip()
                if text:
                    stmts.append(text)
                current = []
                i += 1
                continue
            current.append(ch)
            i += 1
        text = "".join(current).strip()
        if text:
            stmts.append(text)
        return stmts

    # ------------------------------------------------------------------
    # Statement executor
    # ------------------------------------------------------------------

    def _eval_stmt(self, stmt: str, env: dict) -> Any:
        stmt = stmt.strip()
        if not stmt:
            return None

        # Variable declaration: | x y z |
        m = re.match(r"^\|\s*([\w\s]+)\s*\|(.*)$", stmt, re.DOTALL)
        if m:
            for var in m.group(1).split():
                env[var] = None
            rest = m.group(2).strip()
            if rest:
                return self._eval_stmt(rest, env)
            return None

        # Assignment: x := expr
        m = re.match(r"^(\w+)\s*:=\s*(.+)$", stmt, re.DOTALL)
        if m:
            val = self._eval_expr(m.group(2).strip(), env)
            var_name = m.group(1)
            # Class variables (start with uppercase) go to globals scope
            if var_name[0].isupper():
                self._globals[var_name] = val
            env[var_name] = val
            return val

        # Class definition: Object subclass: #Name instanceVariableNames: '...'
        m = re.match(r"^(\w+)\s+subclass:\s*#(\w+)\s*(.*)", stmt, re.DOTALL)
        if m:
            superclass_name = m.group(1)
            class_name = m.group(2)
            rest = m.group(3)
            ivar_m = re.search(r"instanceVariableNames:\s*'([^']*)'", rest)
            ivars = ivar_m.group(1).split() if ivar_m else []
            cls = STClass(class_name, ivars, self)
            self._classes[class_name] = cls
            env[class_name] = cls
            return cls

        # Method definition: ClassName >> #methodName [ body ]
        m = re.match(r"^(\w+)\s*>>\s*#?(\w+:?)\s*\[(.+)\]$", stmt, re.DOTALL)
        if m:
            class_name = m.group(1)
            method_name = m.group(2)
            body = m.group(3).strip()
            if class_name in self._classes:
                self._classes[class_name].add_method(method_name, body)
            return None

        return self._eval_expr(stmt, env)

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval_expr(self, expr: str, env: dict) -> Any:
        expr = expr.strip()
        if not expr:
            return None

        # Nil / true / false
        if expr == "nil":
            return None
        if expr == "true":
            return True
        if expr == "false":
            return False
        if expr == "self":
            return env.get("self")
        if expr == "super":
            return env.get("self")

        # String literal
        m = re.match(r"^'((?:[^']|'')*)'$", expr)
        if m:
            return m.group(1).replace("''", "'")

        # Symbol #name
        m = re.match(r"^#(\w+)$", expr)
        if m:
            return m.group(1)

        # Character $x
        m = re.match(r"^\$(.)", expr)
        if m:
            return m.group(1)

        # Array literal #( ... )
        m = re.match(r"^#\((.+)\)$", expr)
        if m:
            items = m.group(1).split()
            return [self._eval_expr(i, env) for i in items]

        # Number
        try:
            if "." in expr and not expr.startswith("."):
                return float(expr)
            return int(expr)
        except ValueError:
            pass

        # Block: [ statements ]  — only if the opening '[' matches the final ']'
        if expr.startswith("[") and expr.endswith("]"):
            depth = 0
            match_end = -1
            for _bi, _bc in enumerate(expr):
                if _bc == "[":
                    depth += 1
                elif _bc == "]":
                    depth -= 1
                    if depth == 0:
                        match_end = _bi
                        break
            if match_end == len(expr) - 1:
                return STBlock(expr[1:-1].strip(), env, self)

        # Parenthesised
        if expr.startswith("(") and expr.endswith(")"):
            return self._eval_expr(expr[1:-1].strip(), env)

        # Cascade (;) — execute multiple messages on same receiver
        if ";" in expr:
            parts = expr.split(";")
            receiver_expr = parts[0].strip()
            # The receiver is the result of the first message
            recv = self._eval_expr(receiver_expr, env)
            for msg_part in parts[1:]:
                self._send_message(recv, msg_part.strip(), env)
            return recv

        # Keyword message: receiver keyword: arg keyword2: arg2
        # Use bracket-aware parser instead of regex
        kw_result = self._try_keyword_message(expr, env)
        if kw_result is not None:
            return kw_result

        # Binary message: receiver + arg
        for op in [
            "~~",
            "=",
            "~=",
            "<",
            ">",
            "<=",
            ">=",
            "+",
            "-",
            "*",
            "/",
            "//",
            "\\\\",
            "@",
            ",",
            "->",
            "**",
        ]:
            idx = _find_op(expr, op)
            if idx > 0:
                recv = self._eval_expr(expr[:idx].strip(), env)
                arg = self._eval_expr(expr[idx + len(op) :].strip(), env)
                return self._send_binary(recv, op, arg)

        # Unary messages (chained): recv msg1 msg2
        # Handle #(...) array literals before splitting on spaces
        if expr.startswith("#("):
            # Find matching close paren
            depth = 0
            end_idx = -1
            for ci, ch in enumerate(expr):
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        end_idx = ci
                        break
            if end_idx >= 0:
                arr_expr = expr[: end_idx + 1]
                rest = expr[end_idx + 1 :].strip()
                recv = self._eval_expr(arr_expr, env)
                if rest:
                    for msg in rest.split():
                        recv = self._send_unary(recv, msg)
                return recv

        parts = expr.split()
        if len(parts) > 1:
            recv = self._eval_expr(parts[0], env)
            for msg in parts[1:]:
                recv = self._send_unary(recv, msg)
            return recv

        # Variable
        if expr in env:
            return env[expr]
        if expr in self._globals:
            return self._globals[expr]
        if expr in self._classes:
            return self._classes[expr]
        # Unknown identifier returns the name (like Smalltalk-80 unbound)
        return expr

    def _send_message(self, recv: Any, msg: str, env: dict) -> Any:
        """Dispatch unary/binary/keyword message."""
        return self._eval_expr(str(recv) + " " + msg, env)

    def _send_unary(self, recv: Any, msg: str) -> Any:
        """Handle unary messages."""
        if msg == "printString":
            return repr(recv)
        if msg == "key":
            # Association key  — returns first element of a (key, value) pair
            if isinstance(recv, tuple) and len(recv) == 2:
                return recv[0]
            return recv
        if msg == "value":
            # Block evaluation takes priority over Association value
            if isinstance(recv, STBlock):
                return recv.value([])
            # Association value  — returns second element of a (key, value) pair
            if isinstance(recv, tuple) and len(recv) == 2:
                return recv[1]
            return recv
        if msg == "printNl":
            self._emit(str(recv))
            return recv
        if msg == "class":
            return type(recv).__name__
        if msg == "isNil":
            return recv is None
        if msg == "notNil":
            return recv is not None
        if msg == "not":
            return not recv
        if msg == "negated":
            return -recv
        if msg == "abs":
            return abs(recv)
        if msg == "sqrt":
            import math

            return math.sqrt(recv)
        if msg == "factorial":
            import math

            return math.factorial(int(recv))
        if msg == "asFloat":
            return float(recv)
        if msg == "asInteger" or msg == "truncated":
            return int(recv)
        if msg == "rounded":
            return round(recv)
        if msg == "asString":
            return str(recv)
        if msg == "reversed":
            if isinstance(recv, (str, list)):
                return recv[::-1]
        if msg == "size":
            if hasattr(recv, "__len__"):
                return len(recv)
        if msg == "isEmpty":
            return len(recv) == 0 if hasattr(recv, "__len__") else recv is None
        if msg == "notEmpty":
            return len(recv) > 0 if hasattr(recv, "__len__") else recv is not None
        if msg == "asUppercase":
            return recv.upper() if isinstance(recv, str) else recv
        if msg == "asLowercase":
            return recv.lower() if isinstance(recv, str) else recv
        if msg == "new":
            if isinstance(recv, STClass):
                return recv.instantiate()
            if recv is list:
                return []
            if recv is STOrderedCollection:
                return STOrderedCollection()
        if msg == "copy":
            if isinstance(recv, list):
                return recv[:]
        # Try method lookup on STClass instances
        if hasattr(recv, "_class") and isinstance(recv._class, STClass):
            method = recv._class.methods.get(msg)
            if method:
                return self._run_method(method, recv, [], recv._env.copy())
        return None

    def _send_binary(self, recv: Any, op: str, arg: Any) -> Any:
        if op == "+":
            return recv + arg
        if op == "-":
            return recv - arg
        if op == "*":
            return recv * arg
        if op == "/":
            return recv / arg
        if op == "//":
            return recv // arg
        if op == "\\\\":
            return recv % arg
        if op == "**":
            return recv**arg
        if op == "=":
            return recv == arg
        if op == "~=":
            return recv != arg
        if op == "<":
            return recv < arg
        if op == ">":
            return recv > arg
        if op == "<=":
            return recv <= arg
        if op == ">=":
            return recv >= arg
        if op == ",":
            if isinstance(recv, str):
                return str(recv) + str(arg)
            if isinstance(recv, list):
                return recv + (arg if isinstance(arg, list) else [arg])
        if op == "@":
            return (recv, arg)
        return None

    # ------------------------------------------------------------------
    # Bracket-aware keyword message parser
    # ------------------------------------------------------------------

    _SENTINEL = object()

    def _try_keyword_message(self, expr: str, env: dict):
        """Try to parse *expr* as a keyword message, respecting bracket nesting."""
        # Find the first  keyword:  at bracket-depth 0
        depth = 0
        first_kw_start = -1
        for i, ch in enumerate(expr):
            if ch in ("[", "("):
                depth += 1
            elif ch in ("]", ")"):
                depth -= 1
            elif ch == ":" and depth == 0 and i > 0:
                # Look back for an identifier preceding the colon
                j = i - 1
                while j >= 0 and (expr[j].isalnum() or expr[j] == "_"):
                    j -= 1
                kw_start = j + 1
                if kw_start < i and expr[kw_start].isalpha():
                    first_kw_start = kw_start
                    break
        if first_kw_start < 0:
            return None
        recv_str = expr[:first_kw_start].strip()
        kw_str = expr[first_kw_start:].strip()
        if not recv_str:
            return None
        recv = self._eval_expr(recv_str, env)
        pairs = self._split_keyword_pairs(kw_str)
        if pairs:
            selector = "".join(k for k, _ in pairs)
            args = [self._eval_expr(a.strip(), env) for _, a in pairs]
            return self._send_keyword(recv, selector, args, env)
        return None

    @staticmethod
    def _split_keyword_pairs(kw_str: str) -> list[tuple[str, str]]:
        """Split 'key1: arg1 key2: arg2' respecting [...] and (...) nesting."""
        pairs: list[tuple[str, str]] = []
        depth = 0
        current_key = ""
        arg_start = -1
        i = 0
        while i < len(kw_str):
            ch = kw_str[i]
            if ch in ("[", "("):
                depth += 1
            elif ch in ("]", ")"):
                depth -= 1
            elif ch == ":" and depth == 0:
                # Look back for keyword identifier
                j = i - 1
                while j >= 0 and (kw_str[j].isalnum() or kw_str[j] == "_"):
                    j -= 1
                kw_start = j + 1
                if kw_start < i and kw_str[kw_start].isalpha():
                    # Save previous pair
                    if current_key and arg_start >= 0:
                        pairs.append((current_key, kw_str[arg_start:kw_start].strip()))
                    current_key = kw_str[kw_start : i + 1]  # includes ':'
                    arg_start = i + 1
            i += 1
        # Last pair
        if current_key and arg_start >= 0:
            pairs.append((current_key, kw_str[arg_start:].strip()))
        return pairs

    def _send_keyword(self, recv: Any, selector: str, args: list, env: dict) -> Any:
        """Handle keyword messages."""
        # Transcript show: / showCr:
        tr = self._globals.get("Transcript")
        if recv is tr:
            if selector == "show:":
                self._emit(str(args[0]))
                return recv
            if selector == "showCr:":
                self._emit(str(args[0]))
                return recv
            if selector == "print:":
                self._emit(repr(args[0]))
                return recv

        # Control flow
        if selector == "ifTrue:":
            block = args[0]
            if recv and isinstance(block, STBlock):
                return block.value([])
            return None
        if selector == "ifFalse:":
            block = args[0]
            if not recv and isinstance(block, STBlock):
                return block.value([])
            return None
        if selector == "ifTrue:ifFalse:":
            tb, fb = args[0], args[1]
            if recv:
                return tb.value([]) if isinstance(tb, STBlock) else tb
            else:
                return fb.value([]) if isinstance(fb, STBlock) else fb
        if selector == "ifFalse:ifTrue:":
            fb, tb = args[0], args[1]
            if recv:
                return tb.value([]) if isinstance(tb, STBlock) else tb
            else:
                return fb.value([]) if isinstance(fb, STBlock) else fb
        if selector == "whileTrue:":
            cond_block = recv
            body_block = args[0]
            count = 0
            while (
                isinstance(cond_block, STBlock)
                and cond_block.value([])
                and count < 100000
            ):
                if isinstance(body_block, STBlock):
                    body_block.value([])
                count += 1
            return None
        if selector == "whileFalse:":
            cond_block = recv
            body_block = args[0]
            count = 0
            while (
                isinstance(cond_block, STBlock)
                and not cond_block.value([])
                and count < 100000
            ):
                if isinstance(body_block, STBlock):
                    body_block.value([])
                count += 1
            return None
        if selector == "timesRepeat:":
            block = args[0]
            n = int(recv)
            for _ in range(min(n, 100000)):
                if isinstance(block, STBlock):
                    block.value([])
            return None
        if selector == "to:do:":
            end_val = args[0]
            block = args[1]
            i = recv
            count = 0
            while i <= end_val and count < 100000:
                if isinstance(block, STBlock):
                    block.value([i])
                i += 1
                count += 1
            return None
        if selector == "to:by:do:":
            end_val = args[0]
            step = args[1]
            block = args[2]
            i = recv
            count = 0
            while (
                step > 0 and i <= end_val or step < 0 and i >= end_val
            ) and count < 100000:
                if isinstance(block, STBlock):
                    block.value([i])
                i += step
                count += 1
            return None
        if selector == "to:collect:":
            end_val = args[0]
            block = args[1]
            result = []
            i = recv
            while i <= end_val:
                if isinstance(block, STBlock):
                    result.append(block.value([i]))
                i += 1
            return result

        # Collection messages
        if selector == "do:":
            block = args[0]
            if isinstance(recv, (list, STOrderedCollection)):
                items = recv if isinstance(recv, list) else recv._items
                for item in items:
                    if isinstance(block, STBlock):
                        block.value([item])
            return None
        if selector == "collect:":
            block = args[0]
            if isinstance(recv, (list, STOrderedCollection)):
                items = recv if isinstance(recv, list) else recv._items
                return [
                    block.value([i]) if isinstance(block, STBlock) else i for i in items
                ]
        if selector == "select:":
            block = args[0]
            if isinstance(recv, (list, STOrderedCollection)):
                items = recv if isinstance(recv, list) else recv._items
                return [
                    i for i in items if isinstance(block, STBlock) and block.value([i])
                ]
        if selector == "inject:into:":
            acc = args[0]
            block = args[1]
            items = (
                recv
                if isinstance(recv, list)
                else (recv._items if isinstance(recv, STOrderedCollection) else [])
            )
            for item in items:
                if isinstance(block, STBlock):
                    acc = block.value([acc, item])
            return acc
        if selector == "add:":
            if isinstance(recv, STOrderedCollection):
                recv._items.append(args[0])
            elif isinstance(recv, list):
                recv.append(args[0])
            return args[0]
        if selector == "at:":
            idx = int(args[0])
            if isinstance(recv, list):
                return recv[idx - 1] if 1 <= idx <= len(recv) else None
            if isinstance(recv, STOrderedCollection):
                return recv._items[idx - 1] if 1 <= idx <= len(recv._items) else None
            if isinstance(recv, dict):
                return recv.get(args[0])
            if isinstance(recv, str):
                return recv[idx - 1] if 1 <= idx <= len(recv) else None
        if selector == "at:put:":
            idx = int(args[0])
            val = args[1]
            if isinstance(recv, list):
                while len(recv) < idx:
                    recv.append(None)
                recv[idx - 1] = val
            elif isinstance(recv, dict):
                recv[args[0]] = val
            return val
        if selector == "includes:":
            if isinstance(recv, (list, STOrderedCollection)):
                items = recv if isinstance(recv, list) else recv._items
                return args[0] in items
        if selector == "printString":
            return str(recv)
        if selector == "with:":
            return [recv, args[0]] if not isinstance(recv, list) else recv + [args[0]]
        if selector == "with:with:":
            return [recv, args[0], args[1]]
        if selector == "with:with:with:":
            return [recv, args[0], args[1], args[2]]
        if selector == "with:with:with:with:":
            return [recv, args[0], args[1], args[2], args[3]]
        if selector == "key:value:":
            # Association key:value:  — returns a (key, value) pair
            return (args[0], args[1])
        if selector == "respondsTo:":
            return hasattr(recv, args[0])

        # Numeric: max: / min:
        if selector == "max:":
            return max(recv, args[0])
        if selector == "min:":
            return min(recv, args[0])

        # Block value: with argument
        if selector == "value:" and isinstance(recv, STBlock):
            return recv.value(args)
        if selector == "value:value:" and isinstance(recv, STBlock):
            return recv.value(args)

        # Array new: n
        if selector == "new:":
            if recv is list or recv is STOrderedCollection:
                return [None] * int(args[0])

        # String
        if selector == "copyFrom:to:":
            if isinstance(recv, (str, list)):
                return recv[int(args[0]) - 1 : int(args[1])]
        if selector == "indexOf:":
            if isinstance(recv, str):
                idx = recv.find(str(args[0]))
                return idx + 1 if idx >= 0 else 0
        if selector == "replaceAll:with:":
            if isinstance(recv, str):
                return recv.replace(str(args[0]), str(args[1]))

        # Method call on instances
        if hasattr(recv, "_class") and isinstance(recv._class, STClass):
            method = recv._class.methods.get(selector)
            if method:
                return self._run_method(method, recv, args, env)

        return None

    def _run_method(self, body: str, self_obj: Any, args: list, env: dict) -> Any:
        env = env.copy()
        env["self"] = self_obj
        for stmt in self._split_statements(body):
            result = self._eval_stmt(stmt.strip(), env)
        return result


class STBlock:
    def __init__(self, body: str, env: dict, interp: "SmalltalkEnvironment"):
        self.body = body
        self.env = env  # share the same dict — closures see mutations
        self.interp = interp

    def value(self, args: list) -> Any:
        env = self.env  # use the shared env directly
        # Parse block args: [:x :y | body]
        m = re.match(r"^\s*((?::\w+\s*)+)\|(.*)", self.body, re.DOTALL)
        if m:
            param_str = m.group(1).strip()
            body = m.group(2).strip()
            params = re.findall(r":(\w+)", param_str)
            # Create a child env for block-local params while keeping parent refs
            if params:
                env = dict(env)  # shallow copy only for parameterised blocks
                for i, p in enumerate(params):
                    env[p] = args[i] if i < len(args) else None
        else:
            body = self.body
        stmts = self.interp._split_statements(body)
        result = None
        for stmt in stmts:
            if stmt.strip():
                result = self.interp._eval_stmt(stmt.strip(), env)
        return result


class STOrderedCollection:
    def __init__(self):
        self._items: list = []

    def __repr__(self):
        return f"OrderedCollection {self._items!r}"

    def __len__(self):
        return len(self._items)


class STClass:
    def __init__(self, name: str, ivars: list[str], interp: "SmalltalkEnvironment"):
        self.name = name
        self.ivars = ivars
        self.methods: dict[str, str] = {}
        self.interp = interp

    def add_method(self, selector: str, body: str):
        self.methods[selector] = body

    def instantiate(self) -> "STInstance":
        inst = STInstance(self)
        for ivar in self.ivars:
            inst._env[ivar] = None
        return inst


class STInstance:
    def __init__(self, cls: STClass):
        self._class = cls
        self._env: dict[str, Any] = {}

    def __repr__(self):
        return f"a {self._class.name}"


class STError(Exception):
    pass


def _find_op(expr: str, op: str) -> int:
    depth = 0
    in_str = False
    i = 0
    while i < len(expr):
        ch = expr[i]
        if in_str:
            if ch == "'":
                in_str = False
        elif ch == "'":
            in_str = True
        elif ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        elif depth == 0 and expr[i : i + len(op)] == op:
            if i > 0:
                return i
        i += 1
    return -1
