"""Rust language executor for Time Warp Studio.

Educational Rust interpreter — whole-program execution.
Implements a teaching subset of Rust:
  - fn main() and named functions
  - println!, print!, eprintln!, format!
  - let / let mut bindings (type annotations ignored)
  - Primitive types: i32, u32, i64, u64, f32, f64, bool, &str, String, char
  - Arithmetic, comparison, logical operators
  - if / else if / else expressions
  - while / loop / for..in
  - match expressions (literal + wildcard)
  - Structs (struct S { fields }; impl S { fn new }; s.field access)
  - Enums (enum E { A, B(T) }; simple variant matching)
  - Vec<T>, HashMap<K,V>, Option<T>, Result<T,E> (basic methods)
  - Closures |x| expr and |x| { body }
  - Ownership/borrow annotations shown educationally (not enforced)
  - Turtle graphics via turtle::forward(), etc.
"""

from __future__ import annotations

import math
import re
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_rust(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a complete Rust program and return all output as a string."""
    env = RustEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Sentinels
# ---------------------------------------------------------------------------


class _Return(Exception):
    def __init__(self, value: Any = None):
        self.value = value


class _Break(Exception):
    def __init__(self, value: Any = None):
        self.value = value


class _Continue(Exception):
    pass


class _RustError(Exception):
    pass


class _RustStruct:
    """A Rust struct instance."""
    def __init__(self, type_name: str, fields: Dict[str, Any]):
        self.type_name = type_name
        self.fields = fields

    def __repr__(self) -> str:
        fields = ", ".join(f"{k}: {_rust_display(v)}" for k, v in self.fields.items())
        return f"{self.type_name} {{ {fields} }}"


class _RustEnum:
    """A Rust enum variant value."""
    def __init__(self, enum_name: str, variant: str, payload: Any = None):
        self.enum_name = enum_name
        self.variant = variant
        self.payload = payload

    def __repr__(self) -> str:
        if self.payload is not None:
            return f"{self.enum_name}::{self.variant}({_rust_display(self.payload)})"
        return f"{self.enum_name}::{self.variant}"


class _RustClosure:
    """A Rust closure |params| { body }."""
    def __init__(self, params: List[str], body: str, env_snapshot: Dict[str, Any]):
        self.params = params
        self.body = body
        self.env_snapshot = env_snapshot


def _rust_display(val: Any) -> str:
    """Format a value like Rust's Display trait."""
    if val is None:
        return "()"
    if isinstance(val, bool):
        return "true" if val else "false"
    if isinstance(val, list):
        items = ", ".join(_rust_display(x) for x in val)
        return f"[{items}]"
    if isinstance(val, tuple):
        items = ", ".join(_rust_display(x) for x in val)
        return f"({items},)" if len(val) == 1 else f"({items})"
    if isinstance(val, dict):
        return repr(val)
    if isinstance(val, _RustStruct):
        return repr(val)
    if isinstance(val, _RustEnum):
        return repr(val)
    if isinstance(val, _RustClosure):
        return "<closure>"
    return str(val)


def _rust_debug(val: Any) -> str:
    """Format a value like Rust's Debug trait."""
    if isinstance(val, str):
        return repr(val)
    if isinstance(val, list):
        items = ", ".join(_rust_debug(x) for x in val)
        return f"[{items}]"
    if isinstance(val, tuple):
        items = ", ".join(_rust_debug(x) for x in val)
        return f"({items},)" if len(val) == 1 else f"({items})"
    if isinstance(val, dict):
        pairs = ", ".join(f"{_rust_debug(k)}: {_rust_debug(v)}" for k, v in val.items())
        return "{" + pairs + "}"
    return _rust_display(val)


# ---------------------------------------------------------------------------
# Struct / Enum type definitions
# ---------------------------------------------------------------------------


class _StructDef:
    def __init__(self, name: str, fields: List[Tuple[str, str]]):  # (field_name, type_name)
        self.name = name
        self.fields = fields


class _EnumDef:
    def __init__(self, name: str, variants: List[Tuple[str, Optional[str]]]):  # (variant_name, optional_payload_type)
        self.name = name
        self.variants = variants


class _FuncDef:
    def __init__(self, name: str, params: List[str], body: str):
        self.name = name
        self.params = params
        self.body = body


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------


class RustEnvironment:
    """Tree-walking Rust interpreter."""

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: List[str] = []
        self._functions: Dict[str, _FuncDef] = {}
        self._structs: Dict[str, _StructDef] = {}
        self._enums: Dict[str, _EnumDef] = {}
        self._scope_stack: List[Dict[str, Any]] = [{}]

    def _emit(self, text: str) -> None:
        self._output.append(text)

    # ------------------------------------------------------------------
    # Scope management
    # ------------------------------------------------------------------

    def _push(self) -> None:
        self._scope_stack.append({})

    def _pop(self) -> None:
        if len(self._scope_stack) > 1:
            self._scope_stack.pop()

    def _get(self, name: str) -> Any:
        for scope in reversed(self._scope_stack):
            if name in scope:
                return scope[name]
        return None

    def _set(self, name: str, value: Any) -> None:
        # Always set in current (innermost) scope
        self._scope_stack[-1][name] = value

    def _set_existing(self, name: str, value: Any) -> bool:
        """Modify an existing variable in any scope. Returns True if found."""
        for scope in reversed(self._scope_stack):
            if name in scope:
                scope[name] = value
                return True
        return False

    # ------------------------------------------------------------------
    # Entry point
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        try:
            # Parse top-level items
            self._parse_top_level(source)
            # Execute main
            if "main" in self._functions:
                self._call_function("main", [])
            else:
                self._emit("❌ Rust error: no main function found\n")
        except _Return:
            pass
        except _RustError as e:
            self._emit(f"❌ Rust error: {e}\n")
        except RecursionError:
            self._emit("❌ Rust error: thread 'main' has overflowed its stack\n")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}\n")
        return "".join(self._output)

    # ------------------------------------------------------------------
    # Top-level parser
    # ------------------------------------------------------------------

    def _parse_top_level(self, source: str) -> None:
        """Parse fn, struct, enum, impl, use, mod, const at top level."""
        # Strip comments
        source = re.sub(r"//[^\n]*", "", source)
        source = re.sub(r"/\*.*?\*/", "", source, flags=re.DOTALL)

        i = 0
        while i < len(source):
            # Skip whitespace
            while i < len(source) and source[i] in " \t\n\r":
                i += 1
            if i >= len(source):
                break

            # fn definition
            m = re.match(r"(?:pub\s+)?(?:async\s+)?fn\s+(\w+)\s*(<[^>]*>)?\s*\(([^)]*)\)\s*(?:->\s*[^{]+)?\s*\{", source[i:])
            if m:
                name = m.group(1)
                params_str = m.group(3).strip()
                start = i + m.end() - 1  # points to opening {
                body, end = self._extract_brace_block(source, start)
                params = self._parse_fn_params(params_str)
                self._functions[name] = _FuncDef(name, params, body)
                i = end + 1
                continue

            # struct definition
            m = re.match(r"(?:pub\s+)?struct\s+(\w+)\s*(?:<[^>]*>)?\s*\{([^}]*)\}", source[i:])
            if m:
                sname = m.group(1)
                fields_str = m.group(2)
                fields = []
                for field in fields_str.split(","):
                    field = field.strip()
                    if field:
                        fm = re.match(r"(?:pub\s+)?(\w+)\s*:\s*(.+)", field)
                        if fm:
                            fields.append((fm.group(1).strip(), fm.group(2).strip()))
                self._structs[sname] = _StructDef(sname, fields)
                i += m.end()
                continue

            # enum definition
            m = re.match(r"(?:pub\s+)?enum\s+(\w+)\s*(?:<[^>]*>)?\s*\{([^}]*)\}", source[i:])
            if m:
                ename = m.group(1)
                variants_str = m.group(2)
                variants = []
                for var in variants_str.split(","):
                    var = var.strip()
                    if not var:
                        continue
                    vm = re.match(r"(\w+)\s*(?:\((.+)\)|\{.+\})?", var)
                    if vm:
                        variants.append((vm.group(1), vm.group(2)))
                self._enums[ename] = _EnumDef(ename, variants)
                i += m.end()
                continue

            # impl block — extract method defs
            m = re.match(r"impl\s+(?:\w+\s+for\s+)?(\w+)\s*(?:<[^>]*>)?\s*\{", source[i:])
            if m:
                impl_name = m.group(1)
                start = i + m.end() - 1
                body, end = self._extract_brace_block(source, start)
                self._parse_impl(impl_name, body)
                i = end + 1
                continue

            # use / mod / const / type / extern — skip
            m = re.match(r"(use|mod|const|type|extern|static|trait|#\[)[^\n;]*[;\{]?", source[i:])
            if m:
                i += max(m.end(), 1)
                continue

            # Skip any stray character
            i += 1

    def _parse_fn_params(self, params_str: str) -> List[str]:
        """Extract parameter names from a Rust fn signature."""
        params = []
        for param in params_str.split(","):
            param = param.strip()
            if not param or param in ("self", "&self", "&mut self", "mut self"):
                if param in ("self", "&self", "&mut self", "mut self"):
                    params.append("self")
                continue
            # pattern: name: Type or mut name: Type
            pm = re.match(r"(?:mut\s+)?(\w+)\s*:", param)
            if pm:
                params.append(pm.group(1))
            elif re.match(r"^\w+$", param):
                params.append(param)
        return params

    def _parse_impl(self, type_name: str, body: str) -> None:
        """Parse methods from an impl block."""
        i = 0
        body = re.sub(r"//[^\n]*", "", body)
        while i < len(body):
            while i < len(body) and body[i] in " \t\n\r":
                i += 1
            if i >= len(body):
                break
            m = re.match(r"(?:pub\s+)?(?:async\s+)?fn\s+(\w+)\s*(?:<[^>]*>)?\s*\(([^)]*)\)\s*(?:->\s*[^{]+)?\s*\{", body[i:])
            if m:
                mname = m.group(1)
                params_str = m.group(2).strip()
                start = i + m.end() - 1
                fn_body, end = self._extract_brace_block(body, start)
                params = self._parse_fn_params(params_str)
                qual_name = f"{type_name}::{mname}"
                self._functions[qual_name] = _FuncDef(qual_name, params, fn_body)
                i = end + 1
                continue
            i += 1

    def _extract_brace_block(self, source: str, start: int) -> Tuple[str, int]:
        """Extract the content of a brace block starting at source[start] == '{'."""
        assert source[start] == "{"
        depth = 0
        in_string = False
        in_char = False
        i = start
        while i < len(source):
            ch = source[i]
            if in_string:
                if ch == "\\" and i + 1 < len(source):
                    i += 2
                    continue
                if ch == '"':
                    in_string = False
            elif in_char:
                if ch == "\\" and i + 1 < len(source):
                    i += 2
                    continue
                if ch == "'":
                    in_char = False
            else:
                if ch == '"':
                    in_string = True
                elif ch == "'":
                    in_char = True
                elif ch == "{":
                    depth += 1
                elif ch == "}":
                    depth -= 1
                    if depth == 0:
                        return source[start + 1:i], i
            i += 1
        return source[start + 1:], len(source) - 1

    # ------------------------------------------------------------------
    # Statement executor
    # ------------------------------------------------------------------

    def _exec_block(self, code: str) -> Any:
        """Execute a block of Rust code (inside braces)."""
        stmts = self._split_stmts(code)
        result = None
        for stmt in stmts:
            stmt = stmt.strip()
            if not stmt:
                continue
            result = self._exec_stmt(stmt)
        return result

    def _split_stmts(self, code: str) -> List[str]:
        """Split code into statements at ';' and block endings."""
        stmts = []
        current: List[str] = []
        depth_brace = 0
        depth_paren = 0
        in_string = False
        in_char = False
        i = 0
        while i < len(code):
            ch = code[i]
            if in_string:
                current.append(ch)
                if ch == "\\" and i + 1 < len(code):
                    current.append(code[i + 1])
                    i += 2
                    continue
                if ch == '"':
                    in_string = False
            elif in_char:
                current.append(ch)
                if ch == "'" :
                    in_char = False
            else:
                if ch == '"':
                    in_string = True
                    current.append(ch)
                elif ch == "'":
                    in_char = True
                    current.append(ch)
                elif ch == "{":
                    depth_brace += 1
                    current.append(ch)
                elif ch == "}":
                    depth_brace -= 1
                    current.append(ch)
                    if depth_brace == 0:
                        # Emit the block statement
                        s = "".join(current).strip()
                        if s:
                            stmts.append(s)
                        current = []
                        i += 1
                        # skip optional semicolon after block
                        while i < len(code) and code[i] in " \t\n\r":
                            i += 1
                        if i < len(code) and code[i] == ";":
                            i += 1
                        continue
                elif ch == "(":
                    depth_paren += 1
                    current.append(ch)
                elif ch == ")":
                    depth_paren -= 1
                    current.append(ch)
                elif ch == ";" and depth_brace == 0 and depth_paren == 0:
                    s = "".join(current).strip()
                    if s:
                        stmts.append(s)
                    current = []
                    i += 1
                    continue
                else:
                    current.append(ch)
            i += 1
        if current:
            s = "".join(current).strip()
            if s:
                stmts.append(s)
        return stmts

    def _exec_stmt(self, stmt: str) -> Any:
        """Execute a single Rust statement."""
        stmt = stmt.strip()
        if not stmt:
            return None

        # Block expression { ... }
        if stmt.startswith("{") and stmt.endswith("}"):
            self._push()
            try:
                return self._exec_block(stmt[1:-1])
            finally:
                self._pop()

        # let binding: let [mut] name [: Type] = expr
        m = re.match(r"^let\s+(?:mut\s+)?(\w+)\s*(?::\s*[^=]+?)?\s*=\s*(.+)$", stmt, re.DOTALL)
        if m:
            name = m.group(1)
            expr_str = m.group(2).strip().rstrip(";")
            val = self._eval_expr(expr_str)
            self._set(name, val)
            return val

        # Assignment: name = expr / name.field = expr
        m = re.match(r"^(\w+(?:\.\w+)*)\s*([+\-*/%|&^]=|<<=|>>=|\|\|=|&&=)?\s*=\s*(.+)$", stmt)
        if m and not m.group(2):  # plain assignment
            target = m.group(1)
            val = self._eval_expr(m.group(3).strip().rstrip(";"))
            if "." in target:
                parts = target.split(".", 1)
                obj = self._get(parts[0])
                if isinstance(obj, _RustStruct):
                    obj.fields[parts[1]] = val
                    return val
            if not self._set_existing(target, val):
                self._set(target, val)
            return val

        # Compound assignment
        m = re.match(r"^(\w+)\s*([+\-*/%]|<<|>>|\|\||&&)=\s*(.+)$", stmt)
        if m:
            name = m.group(1)
            op = m.group(2)
            rhs = self._eval_expr(m.group(3).rstrip(";"))
            lhs = self._get(name)
            result = self._apply_op(lhs, op, rhs)
            self._set_existing(name, result)
            return result

        # return statement
        if stmt == "return" or stmt.startswith("return ") or stmt.startswith("return;"):
            val_str = stmt[6:].strip().rstrip(";") if len(stmt) > 6 else None
            raise _Return(self._eval_expr(val_str) if val_str else None)

        # break / continue
        if stmt.startswith("break"):
            val_str = stmt[5:].strip().rstrip(";")
            raise _Break(self._eval_expr(val_str) if val_str else None)
        if stmt.startswith("continue"):
            raise _Continue()

        # panic!
        if stmt.startswith("panic!"):
            m = re.match(r"panic!\s*\((.+)\)", stmt)
            msg = self._eval_expr(m.group(1)) if m else "explicit panic"
            raise _RustError(f"thread 'main' panicked: {msg}")

        # if / else
        if stmt.startswith("if ") or stmt.startswith("if("):
            return self._exec_if(stmt)

        # while loop
        if stmt.startswith("while ") or stmt.startswith("while("):
            return self._exec_while(stmt)

        # loop
        if stmt.startswith("loop"):
            return self._exec_loop(stmt)

        # for loop
        if stmt.startswith("for ") and " in " in stmt:
            return self._exec_for(stmt)

        # match expression
        if stmt.startswith("match "):
            return self._exec_match(stmt)

        # Macro calls: println!, print!, eprintln!, dbg!, assert!, assert_eq!, vec!
        if "!" in stmt:
            return self._exec_macro(stmt)

        # Expression (function call, method call, etc.)
        return self._eval_expr(stmt.rstrip(";"))

    # ------------------------------------------------------------------
    # Control flow
    # ------------------------------------------------------------------

    def _exec_if(self, stmt: str) -> Any:
        """Execute if/else if/else."""
        # Tokenize: if <cond> { <body> } [else if ... | else { ... }]
        m = re.match(r"^if\s+(.+?)\s*(\{)", stmt, re.DOTALL)
        if not m:
            return None

        cond_str = m.group(1).strip()
        rest = stmt[m.start(2):]
        cond_body, cond_end = self._extract_brace_block(rest, 0)
        after = rest[cond_end + 1:].strip()

        cond_val = self._eval_expr(cond_str)
        if self._truthy(cond_val):
            self._push()
            try:
                return self._exec_block(cond_body)
            finally:
                self._pop()

        # else if / else
        if after.startswith("else"):
            else_part = after[4:].strip()
            if else_part.startswith("if"):
                return self._exec_if(else_part)
            if else_part.startswith("{"):
                else_body, _ = self._extract_brace_block(else_part, 0)
                self._push()
                try:
                    return self._exec_block(else_body)
                finally:
                    self._pop()

        return None

    def _exec_while(self, stmt: str) -> Any:
        m = re.match(r"^while\s+(.+?)\s*(\{)", stmt, re.DOTALL)
        if not m:
            return None
        cond_str = m.group(1).strip()
        rest = stmt[m.start(2):]
        body, _ = self._extract_brace_block(rest, 0)

        for _ in range(100_000):
            if not self._truthy(self._eval_expr(cond_str)):
                break
            self._push()
            try:
                self._exec_block(body)
            except _Continue:
                pass
            except _Break as br:
                return br.value
            finally:
                self._pop()
        return None

    def _exec_loop(self, stmt: str) -> Any:
        m = re.match(r"^loop\s*(\{)", stmt)
        if not m:
            return None
        rest = stmt[m.start(1):]
        body, _ = self._extract_brace_block(rest, 0)

        for _ in range(100_000):
            self._push()
            try:
                self._exec_block(body)
            except _Continue:
                pass
            except _Break as br:
                return br.value
            finally:
                self._pop()
        return None

    def _exec_for(self, stmt: str) -> Any:
        m = re.match(r"^for\s+(.+?)\s+in\s+(.+?)\s*(\{)", stmt, re.DOTALL)
        if not m:
            return None
        pat = m.group(1).strip()
        iter_str = m.group(2).strip()
        rest = stmt[m.start(3):]
        body, _ = self._extract_brace_block(rest, 0)

        iterable = self._eval_expr(iter_str)
        if not hasattr(iterable, "__iter__"):
            return None

        for item in iterable:
            self._push()
            try:
                self._bind_pattern(pat, item)
                self._exec_block(body)
            except _Continue:
                pass
            except _Break:
                break
            finally:
                self._pop()
        return None

    def _bind_pattern(self, pat: str, val: Any) -> None:
        """Bind a match pattern to a value in current scope."""
        pat = pat.strip()
        if pat == "_":
            return
        # Tuple pattern (a, b, c)
        if pat.startswith("(") and pat.endswith(")"):
            inner = pat[1:-1]
            parts = [p.strip() for p in inner.split(",") if p.strip()]
            if isinstance(val, (list, tuple)):
                for p, v in zip(parts, val):
                    self._bind_pattern(p, v)
            return
        # Struct pattern Foo { x, y }
        m = re.match(r"^(\w+)\s*\{(.+)\}$", pat)
        if m and isinstance(val, _RustStruct):
            fields = [f.strip() for f in m.group(2).split(",") if f.strip()]
            for field in fields:
                if field in val.fields:
                    self._set(field, val.fields[field])
            return
        # Simple binding
        if re.match(r"^(?:mut\s+)?(\w+)$", pat):
            name = re.match(r"^(?:mut\s+)?(\w+)$", pat).group(1)
            if name != "_":
                self._set(name, val)

    def _exec_match(self, stmt: str) -> Any:
        """Execute a match expression."""
        m = re.match(r"^match\s+(.+?)\s*(\{)", stmt, re.DOTALL)
        if not m:
            return None
        subject_str = m.group(1).strip()
        rest = stmt[m.start(2):]
        body, _ = self._extract_brace_block(rest, 0)
        subject = self._eval_expr(subject_str)
        return self._eval_match(subject, body)

    def _eval_match(self, subject: Any, arms_str: str) -> Any:
        """Evaluate match arms."""
        # Split arms at => and commas between arms
        arms = self._split_match_arms(arms_str)
        for pat, body in arms:
            pat = pat.strip()
            bindings: Dict[str, Any] = {}
            if self._match_rust_pattern(pat, subject, bindings):
                for k, v in bindings.items():
                    self._set(k, v)
                if body.strip().startswith("{"):
                    inner, _ = self._extract_brace_block(body.strip(), 0)
                    self._push()
                    try:
                        return self._exec_block(inner)
                    finally:
                        self._pop()
                else:
                    return self._eval_expr(body.strip().rstrip(","))
        raise _RustError(f"match is not exhaustive for {_rust_display(subject)}")

    def _split_match_arms(self, s: str) -> List[Tuple[str, str]]:
        """Split match arms: each 'pattern => body' pair."""
        arms = []
        current: List[str] = []
        depth = 0
        in_string = False
        i = 0
        while i < len(s):
            ch = s[i]
            if in_string:
                current.append(ch)
                if ch == '"':
                    in_string = False
            elif ch == '"':
                in_string = True
                current.append(ch)
            elif ch in ("{", "(", "["):
                depth += 1
                current.append(ch)
            elif ch in ("}", ")", "]"):
                depth -= 1
                current.append(ch)
            elif ch == "," and depth == 0:
                arm_str = "".join(current).strip()
                if "=>" in arm_str:
                    pat, _, body = arm_str.partition("=>")
                    arms.append((pat.strip(), body.strip()))
                current = []
            else:
                current.append(ch)
            i += 1
        if current:
            arm_str = "".join(current).strip()
            if "=>" in arm_str:
                pat, _, body = arm_str.partition("=>")
                arms.append((pat.strip(), body.strip()))
        return arms

    def _match_rust_pattern(self, pat: str, val: Any, bindings: Dict[str, Any]) -> bool:
        """Try to match val against Rust pattern. Return True if matched."""
        pat = pat.strip()

        # Wildcard
        if pat == "_":
            return True

        # Literal integers
        if re.match(r"^-?\d+$", pat):
            return int(pat) == val

        # Boolean literals
        if pat == "true":
            return val is True
        if pat == "false":
            return val is False

        # String literal
        if pat.startswith('"') and pat.endswith('"'):
            return pat[1:-1] == val

        # Or pattern: A | B
        if "|" in pat and not pat.startswith("("):
            for p in pat.split("|"):
                if self._match_rust_pattern(p.strip(), val, bindings):
                    return True
            return False

        # Range pattern: a..=b or a..b
        m = re.match(r"^(.+?)\.\.=?(.+)$", pat)
        if m:
            try:
                lo = self._eval_expr(m.group(1).strip())
                hi = self._eval_expr(m.group(2).strip())
                if pat.count("..=") > 0:
                    return isinstance(val, (int, float)) and lo <= val <= hi
                return isinstance(val, (int, float)) and lo <= val < hi
            except Exception:
                pass

        # Tuple pattern (a, b)
        if pat.startswith("(") and pat.endswith(")"):
            inner = pat[1:-1].strip()
            sub_pats = [p.strip() for p in inner.split(",") if p.strip()]
            if isinstance(val, (list, tuple)) and len(val) == len(sub_pats):
                for sp, sv in zip(sub_pats, val):
                    if not self._match_rust_pattern(sp, sv, bindings):
                        return False
                return True
            return False

        # Enum variant: SomeType::Variant(inner) or just Variant
        m = re.match(r"^(?:(\w+)::)?(\w+)\s*(?:\((.+)\)|\{(.+)\})?$", pat)
        if m:
            variant_name = m.group(2)
            inner_pat = m.group(3) or m.group(4)
            if isinstance(val, _RustEnum):
                if val.variant == variant_name:
                    if inner_pat and val.payload is not None:
                        # destructure payload
                        sub_pats = [p.strip() for p in inner_pat.split(",") if p.strip()]
                        if len(sub_pats) == 1:
                            return self._match_rust_pattern(sub_pats[0], val.payload, bindings)
                        if isinstance(val.payload, (list, tuple)) and len(val.payload) == len(sub_pats):
                            for sp, sv in zip(sub_pats, val.payload):
                                if not self._match_rust_pattern(sp, sv, bindings):
                                    return False
                            return True
                    return True
                return False
            # Option variants
            if variant_name == "Some" and val is not None:
                if inner_pat:
                    return self._match_rust_pattern(inner_pat.strip(), val, bindings)
                return True
            if variant_name == "None" and val is None:
                return True
            # Try as variable binding (lowercase = bind, uppercase = const)
            if re.match(r"^[a-z_]\w*$", pat):
                bindings[pat] = val
                return True

        # Variable binding
        if re.match(r"^[a-z_]\w*$", pat):
            bindings[pat] = val
            return True

        # ref / mut patterns
        m = re.match(r"^(?:ref\s+|mut\s+|ref\s+mut\s+)(\w+)$", pat)
        if m:
            bindings[m.group(1)] = val
            return True

        return False

    # ------------------------------------------------------------------
    # Macro execution
    # ------------------------------------------------------------------

    def _exec_macro(self, stmt: str) -> Any:
        """Handle Rust macro invocations."""
        # println!, print!, eprintln!, eprint!
        for macro in ("println!", "print!", "eprintln!", "eprint!"):
            if stmt.startswith(macro) or stmt.startswith(macro.replace("!", " !")):
                inner = self._extract_macro_args(stmt, macro)
                output = self._format_macro(inner)
                if macro in ("println!", "eprintln!"):
                    self._emit(output + "\n")
                else:
                    self._emit(output)
                return None

        # dbg!
        if stmt.startswith("dbg!"):
            inner = self._extract_macro_args(stmt, "dbg!")
            for arg in self._split_args(inner):
                val = self._eval_expr(arg.strip())
                self._emit(f"[debug] {arg.strip()} = {_rust_debug(val)}\n")
            return None

        # assert! / assert_eq! / assert_ne!
        if stmt.startswith("assert_eq!"):
            inner = self._extract_macro_args(stmt, "assert_eq!")
            parts = self._split_args(inner)
            if len(parts) >= 2:
                a = self._eval_expr(parts[0].strip())
                b = self._eval_expr(parts[1].strip())
                if a != b:
                    raise _RustError(f"assertion failed: `(left == right)` left: {_rust_debug(a)}, right: {_rust_debug(b)}")
            return None

        if stmt.startswith("assert_ne!"):
            inner = self._extract_macro_args(stmt, "assert_ne!")
            parts = self._split_args(inner)
            if len(parts) >= 2:
                a = self._eval_expr(parts[0].strip())
                b = self._eval_expr(parts[1].strip())
                if a == b:
                    raise _RustError(f"assertion failed: `(left != right)` both: {_rust_debug(a)}")
            return None

        if stmt.startswith("assert!"):
            inner = self._extract_macro_args(stmt, "assert!")
            parts = self._split_args(inner)
            cond = self._eval_expr(parts[0].strip())
            if not self._truthy(cond):
                msg = self._eval_expr(parts[1].strip()) if len(parts) > 1 else "assertion failed"
                raise _RustError(f"assertion failed: {msg}")
            return None

        # vec!
        if stmt.startswith("vec!"):
            inner = self._extract_macro_args(stmt, "vec!")
            return [self._eval_expr(a.strip()) for a in self._split_args(inner)]

        # format!
        if stmt.startswith("format!"):
            inner = self._extract_macro_args(stmt, "format!")
            return self._format_macro(inner)

        # todo! / unimplemented!
        if stmt.startswith("todo!") or stmt.startswith("unimplemented!"):
            raise _RustError("not yet implemented")

        # unreachable!
        if stmt.startswith("unreachable!"):
            raise _RustError("entered unreachable code")

        # Generic macro: ignore
        return None

    def _extract_macro_args(self, stmt: str, macro_name: str) -> str:
        """Extract the content of a macro call: macro!(content)."""
        # Find first ( or [ or !
        idx = stmt.find("!(")
        if idx == -1:
            idx = stmt.find("![")
            if idx == -1:
                return ""
            open_ch, close_ch = "[", "]"
        else:
            open_ch, close_ch = "(", ")"
        start = idx + 1
        end = len(stmt) - 1
        while end > start and stmt[end] in ") ;":
            end -= 1
        if end < len(stmt) and stmt[end] in (")", "]"):
            inner = stmt[start + 1:end]
        else:
            inner = stmt[start + 1:end + 1]
        return inner.strip()

    def _format_macro(self, inner: str) -> str:
        """Process println!/format! arguments with {} placeholders."""
        parts = self._split_args(inner)
        if not parts:
            return ""
        fmt = self._eval_expr(parts[0])
        if not isinstance(fmt, str):
            return _rust_display(fmt)
        args = [self._eval_expr(a.strip()) for a in parts[1:]]
        result = []
        arg_idx = 0
        i = 0
        while i < len(fmt):
            if fmt[i] == "{" and i + 1 < len(fmt):
                if fmt[i + 1] == "{":
                    result.append("{")
                    i += 2
                    continue
                end = fmt.find("}", i)
                if end == -1:
                    result.append(fmt[i:])
                    break
                spec = fmt[i + 1:end]
                if arg_idx < len(args):
                    val = args[arg_idx]
                    arg_idx += 1
                    if ":?" in spec or spec == "?":
                        result.append(_rust_debug(val))
                    else:
                        result.append(_rust_display(val))
                i = end + 1
            elif fmt[i] == "}" and i + 1 < len(fmt) and fmt[i + 1] == "}":
                result.append("}")
                i += 2
            else:
                result.append(fmt[i])
                i += 1
        return "".join(result)

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval_expr(self, expr: str) -> Any:
        """Evaluate a Rust expression string."""
        if expr is None:
            return None
        expr = expr.strip().rstrip(";")
        if not expr:
            return None

        # Integer literals (with type suffixes)
        m = re.match(r"^(-?\d+)(?:_\d+)*((?:i|u)\d+|usize|isize)?$", expr)
        if m:
            return int(m.group(1))

        # Float literals
        m = re.match(r"^(-?\d+\.\d*)(?:f32|f64)?$", expr)
        if m:
            return float(m.group(1))

        # Boolean
        if expr == "true":
            return True
        if expr == "false":
            return False

        # Unit / None
        if expr in ("()", "None"):
            return None

        # String literal
        m = re.match(r'^"(.*)"$', expr, re.DOTALL)
        if m:
            return m.group(1).replace("\\n", "\n").replace("\\t", "\t").replace('\\"', '"')

        # Char literal
        m = re.match(r"^'(.)'$", expr)
        if m:
            return m.group(1)

        # Parenthesised
        if expr.startswith("(") and expr.endswith(")"):
            inner = expr[1:-1].strip()
            if "," in inner:
                return tuple(self._eval_expr(p.strip()) for p in self._split_args(inner))
            return self._eval_expr(inner)

        # Block expression
        if expr.startswith("{") and expr.endswith("}"):
            self._push()
            try:
                return self._exec_block(expr[1:-1])
            finally:
                self._pop()

        # Macro call
        if "!" in expr and not "!=" in expr:
            return self._exec_macro(expr)

        # Array literal [a, b, c]
        if expr.startswith("[") and expr.endswith("]"):
            inner = expr[1:-1].strip()
            if not inner:
                return []
            # Range: [x; N]
            m = re.match(r"^(.+?)\s*;\s*(\d+)$", inner)
            if m:
                val = self._eval_expr(m.group(1).strip())
                n = int(m.group(2))
                return [val] * n
            return [self._eval_expr(a.strip()) for a in self._split_args(inner)]

        # Struct literal: TypeName { field: val, ... }
        m = re.match(r"^([A-Z]\w*)\s*\{(.+)\}$", expr, re.DOTALL)
        if m:
            type_name = m.group(1)
            fields_str = m.group(2)
            return self._eval_struct_literal(type_name, fields_str)

        # Enum variant: Type::Variant or Type::Variant(args)
        m = re.match(r"^([A-Z]\w*)::(\w+)\s*(?:\((.+)\))?$", expr, re.DOTALL)
        if m:
            enum_name = m.group(1)
            variant = m.group(2)
            payload_str = m.group(3)
            payload = self._eval_expr(payload_str.strip()) if payload_str else None
            if enum_name == "Some":
                return payload
            if enum_name == "Option":
                return payload
            return _RustEnum(enum_name, variant, payload)

        # Method chaining: expr.method(...) or expr.field
        if "." in expr:
            result = self._try_method_or_field(expr)
            if result is not _UNSET:
                return result

        # Closure: |params| expr or |params| { body }
        m = re.match(r"^\|([^|]*)\|\s*(.+)$", expr, re.DOTALL)
        if m:
            params = [p.strip().lstrip("mut ") for p in m.group(1).split(",") if p.strip()]
            params = [re.sub(r"^(?:mut\s+)?(\w+).*$", r"\1", p) for p in params]
            body = m.group(2).strip()
            snap = {}
            for scope in self._scope_stack:
                snap.update(scope)
            return _RustClosure(params, body, snap)

        # Range: a..b or a..=b
        m = re.match(r"^(.+?)\.\.(=?)(.+)$", expr)
        if m:
            lo = self._eval_expr(m.group(1).strip())
            hi = self._eval_expr(m.group(3).strip())
            inclusive = m.group(2) == "="
            if isinstance(lo, int) and isinstance(hi, int):
                return range(lo, hi + 1 if inclusive else hi)
            return []

        # if expression
        if expr.startswith("if "):
            return self._exec_if(expr)

        # match expression
        if expr.startswith("match "):
            return self._exec_match(expr)

        # Function call: name(args) or module::name(args)
        m = re.match(r"^([\w:]+)\s*\(([^)]*)\)$", expr)
        if m:
            fname = m.group(1)
            args_str = m.group(2).strip()
            args = [self._eval_expr(a.strip()) for a in self._split_args(args_str)] if args_str else []
            return self._call(fname, args)

        # Variable lookup
        if re.match(r"^[a-zA-Z_]\w*$", expr):
            return self._get(expr)

        # Unary operators
        if expr.startswith("!"):
            return not self._truthy(self._eval_expr(expr[1:]))
        if expr.startswith("-") and not re.match(r"^-\d", expr):
            return -(self._eval_expr(expr[1:]) or 0)
        if expr.startswith("&mut ") or expr.startswith("&"):
            # Reference: just evaluate the inner expression
            inner = re.sub(r"^&(?:mut\s+)?", "", expr)
            return self._eval_expr(inner)
        if expr.startswith("*"):
            return self._eval_expr(expr[1:])

        # Arithmetic / comparison via Python eval
        return self._safe_eval(expr)

    _UNSET = object()

    def _try_method_or_field(self, expr: str) -> Any:
        """Try to evaluate a method call or field access."""
        # Split on last dot that's not inside brackets
        dot_pos = self._find_method_dot(expr)
        if dot_pos == -1:
            return _UNSET

        receiver_str = expr[:dot_pos]
        rest = expr[dot_pos + 1:]

        receiver = self._eval_expr(receiver_str)

        # method call: method(args)
        m = re.match(r"^(\w+)\s*\(([^)]*)\)$", rest)
        if m:
            mname = m.group(1)
            args_str = m.group(2).strip()
            args = [self._eval_expr(a.strip()) for a in self._split_args(args_str)] if args_str else []
            return self._call_method(receiver, mname, args)

        # field access or zero-arg method
        field = rest.strip()
        if re.match(r"^\w+$", field):
            if isinstance(receiver, _RustStruct):
                return receiver.fields.get(field)
            if isinstance(receiver, tuple) and field.isdigit():
                return receiver[int(field)]
            # zero-arg method
            return self._call_method(receiver, field, [])

        return _UNSET

    def _find_method_dot(self, expr: str) -> int:
        """Find the rightmost . that separates object from method, not inside brackets."""
        depth = 0
        in_string = False
        best = -1
        for i, ch in enumerate(expr):
            if ch == '"':
                in_string = not in_string
            if in_string:
                continue
            if ch in ("(", "[", "{"):
                depth += 1
            elif ch in (")", "]", "}"):
                depth -= 1
            elif ch == "." and depth == 0:
                # Make sure it's not .. range
                if i + 1 < len(expr) and expr[i + 1] == ".":
                    continue
                if i > 0 and expr[i - 1] == ".":
                    continue
                best = i
        return best

    def _eval_struct_literal(self, type_name: str, fields_str: str) -> _RustStruct:
        """Evaluate a struct literal."""
        fields = {}
        for part in self._split_args(fields_str):
            part = part.strip()
            if ":" in part:
                k, _, v = part.partition(":")
                fields[k.strip()] = self._eval_expr(v.strip())
            elif part:
                # shorthand: `x` = binding `x`
                fields[part] = self._get(part)
        return _RustStruct(type_name, fields)

    # ------------------------------------------------------------------
    # Method dispatch
    # ------------------------------------------------------------------

    def _call_method(self, receiver: Any, method: str, args: List[Any]) -> Any:
        """Dispatch method calls on built-in types."""
        m0 = args[0] if args else None

        # Closure call
        if isinstance(receiver, _RustClosure):
            return self._call_closure(receiver, args)

        # String / &str methods
        if isinstance(receiver, str):
            return self._str_method(receiver, method, args)

        # Integer methods
        if isinstance(receiver, int):
            return self._int_method(receiver, method, args)

        # Float methods
        if isinstance(receiver, float):
            return self._float_method(receiver, method, args)

        # Vec / list methods
        if isinstance(receiver, list):
            return self._vec_method(receiver, method, args)

        # HashMap / dict methods
        if isinstance(receiver, dict):
            return self._map_method(receiver, method, args)

        # Option methods (None = None, Some = value)
        if method == "unwrap":
            if receiver is None:
                raise _RustError("called `Option::unwrap()` on a `None` value")
            if isinstance(receiver, _RustEnum) and receiver.variant == "None":
                raise _RustError("called `Option::unwrap()` on a `None` value")
            return receiver
        if method == "unwrap_or":
            if receiver is None:
                return m0
            return receiver
        if method == "is_some":
            return receiver is not None
        if method == "is_none":
            return receiver is None
        if method == "expect":
            if receiver is None:
                raise _RustError(str(m0))
            return receiver
        if method == "map" and isinstance(receiver, _RustClosure):
            return None  # stub

        # Range methods
        if isinstance(receiver, range):
            if method == "rev":
                return list(reversed(receiver))
            if method == "collect":
                return list(receiver)
            if method == "count":
                return len(receiver)
            if method == "sum":
                return sum(receiver)
            if method == "min":
                return min(receiver)
            if method == "max":
                return max(receiver)
            return list(receiver)

        # Tuple methods
        if isinstance(receiver, tuple):
            if method.isdigit():
                return receiver[int(method)]

        # Struct method
        if isinstance(receiver, _RustStruct):
            qual = f"{receiver.type_name}::{method}"
            if qual in self._functions:
                self._push()
                self._set("self", receiver)
                try:
                    return self._call_function(qual, args)
                finally:
                    self._pop()
            return receiver.fields.get(method)

        # Enum
        if isinstance(receiver, _RustEnum):
            if method == "is_ok":
                return receiver.variant == "Ok"
            if method == "is_err":
                return receiver.variant == "Err"
            if method == "unwrap":
                if receiver.variant in ("Err", "None"):
                    raise _RustError(f"called `unwrap()` on `{receiver}`")
                return receiver.payload

        return None

    def _call_closure(self, closure: _RustClosure, args: List[Any]) -> Any:
        self._push()
        # Restore closure environment
        for k, v in closure.env_snapshot.items():
            self._scope_stack[-1][k] = v
        for p, v in zip(closure.params, args):
            self._set(p, v)
        try:
            if closure.body.startswith("{") and closure.body.endswith("}"):
                return self._exec_block(closure.body[1:-1])
            return self._eval_expr(closure.body)
        except _Return as r:
            return r.value
        finally:
            self._pop()

    def _str_method(self, s: str, method: str, args: List[Any]) -> Any:
        m0 = args[0] if args else None
        if method in ("len",):
            return len(s)
        if method == "is_empty":
            return len(s) == 0
        if method == "to_uppercase":
            return s.upper()
        if method == "to_lowercase":
            return s.lower()
        if method == "trim":
            return s.strip()
        if method == "trim_start":
            return s.lstrip()
        if method == "trim_end":
            return s.rstrip()
        if method == "contains" and m0 is not None:
            return str(m0) in s
        if method == "starts_with" and m0 is not None:
            return s.startswith(str(m0))
        if method == "ends_with" and m0 is not None:
            return s.endswith(str(m0))
        if method == "replace" and len(args) >= 2:
            return s.replace(str(args[0]), str(args[1]))
        if method == "replacen" and len(args) >= 3:
            return s.replace(str(args[0]), str(args[1]), int(args[2]))
        if method == "split" and m0:
            return s.split(str(m0))
        if method == "splitn" and len(args) >= 2:
            return s.split(str(args[0]), int(args[1]) - 1)
        if method == "chars":
            return list(s)
        if method == "bytes":
            return list(s.encode("utf-8"))
        if method == "lines":
            return s.splitlines()
        if method == "split_whitespace":
            return s.split()
        if method == "to_string" or method == "to_owned":
            return s
        if method == "parse":
            try:
                if "." in s:
                    return float(s)
                return int(s)
            except ValueError:
                return _RustEnum("Result", "Err", "parse error")
        if method == "repeat" and m0:
            return s * int(m0)
        if method == "find" and m0:
            idx = s.find(str(m0))
            return idx if idx >= 0 else None
        if method in ("push_str",):
            return s + (str(m0) if m0 else "")
        if method == "as_str":
            return s
        if method in ("collect",):
            return s
        if method == "chars":
            return list(s)
        if method == "rev":
            return s[::-1]
        if method in ("into_iter", "iter"):
            return iter(s)
        return None

    def _int_method(self, n: int, method: str, args: List[Any]) -> Any:
        m0 = args[0] if args else None
        if method == "abs":
            return abs(n)
        if method == "pow" and m0 is not None:
            return n ** int(m0)
        if method in ("min", "min_value"):
            return min(n, int(m0)) if m0 is not None else n
        if method in ("max", "max_value"):
            return max(n, int(m0)) if m0 is not None else n
        if method == "to_string":
            return str(n)
        if method == "checked_add" and m0 is not None:
            return n + int(m0)
        if method == "saturating_add" and m0 is not None:
            return n + int(m0)
        if method == "wrapping_add" and m0 is not None:
            return (n + int(m0)) & 0xFFFFFFFF
        if method == "count_ones":
            return bin(n).count("1")
        if method == "leading_zeros":
            return 32 - n.bit_length() if n >= 0 else 0
        if method == "trailing_zeros":
            if n == 0:
                return 32
            return (n & -n).bit_length() - 1
        if method == "from_str_radix" and len(args) >= 2:
            try:
                return int(str(args[0]), int(args[1]))
            except ValueError:
                return None
        if method == "clamp" and len(args) >= 2:
            lo, hi = int(args[0]), int(args[1])
            return max(lo, min(hi, n))
        return n

    def _float_method(self, f: float, method: str, args: List[Any]) -> Any:
        m0 = args[0] if args else None
        if method == "abs":
            return abs(f)
        if method == "sqrt":
            return math.sqrt(f)
        if method == "powi" and m0 is not None:
            return f ** int(m0)
        if method == "powf" and m0 is not None:
            return f ** float(m0)
        if method == "floor":
            return math.floor(f)
        if method == "ceil":
            return math.ceil(f)
        if method == "round":
            return round(f)
        if method == "sin":
            return math.sin(f)
        if method == "cos":
            return math.cos(f)
        if method == "tan":
            return math.tan(f)
        if method == "ln":
            return math.log(f)
        if method == "log2":
            return math.log2(f)
        if method == "log10":
            return math.log10(f)
        if method == "exp":
            return math.exp(f)
        if method == "to_string":
            return str(f)
        if method in ("is_nan",):
            return math.isnan(f)
        if method in ("is_infinite",):
            return math.isinf(f)
        if method == "clamp" and len(args) >= 2:
            return max(float(args[0]), min(float(args[1]), f))
        if method in ("min",) and m0 is not None:
            return min(f, float(m0))
        if method in ("max",) and m0 is not None:
            return max(f, float(m0))
        return f

    def _vec_method(self, v: List[Any], method: str, args: List[Any]) -> Any:
        m0 = args[0] if args else None
        if method in ("len",):
            return len(v)
        if method == "is_empty":
            return len(v) == 0
        if method == "push":
            v.append(m0)
            return None
        if method == "pop":
            return v.pop() if v else None
        if method == "get" and m0 is not None:
            idx = int(m0)
            return v[idx] if 0 <= idx < len(v) else None
        if method == "first":
            return v[0] if v else None
        if method == "last":
            return v[-1] if v else None
        if method == "contains" and m0 is not None:
            return m0 in v
        if method == "insert" and len(args) >= 2:
            v.insert(int(args[0]), args[1])
            return None
        if method == "remove" and m0 is not None:
            return v.pop(int(m0)) if 0 <= int(m0) < len(v) else None
        if method in ("sort", "sort_unstable"):
            try:
                v.sort()
            except TypeError:
                pass
            return None
        if method in ("reverse", "sort_by"):
            v.reverse()
            return None
        if method in ("iter", "into_iter"):
            return iter(v)
        if method == "iter_mut":
            return iter(v)
        if method == "clone":
            return v[:]
        if method == "extend":
            if isinstance(m0, (list, range)):
                v.extend(m0)
            return None
        if method in ("collect",):
            return v
        if method == "join" and m0 is not None:
            return str(m0).join(_rust_display(x) for x in v)
        if method == "concat":
            result = []
            for item in v:
                if isinstance(item, list):
                    result.extend(item)
                else:
                    result.append(item)
            return result
        if method == "retain":
            if isinstance(m0, _RustClosure):
                v[:] = [x for x in v if self._truthy(self._call_closure(m0, [x]))]
            return None
        if method == "dedup":
            seen = []
            result = []
            for x in v:
                if x not in seen:
                    seen.append(x)
                    result.append(x)
            v[:] = result
            return None
        if method == "map":
            if isinstance(m0, _RustClosure):
                return [self._call_closure(m0, [x]) for x in v]
        if method == "filter":
            if isinstance(m0, _RustClosure):
                return [x for x in v if self._truthy(self._call_closure(m0, [x]))]
        if method == "any":
            if isinstance(m0, _RustClosure):
                return any(self._truthy(self._call_closure(m0, [x])) for x in v)
        if method == "all":
            if isinstance(m0, _RustClosure):
                return all(self._truthy(self._call_closure(m0, [x])) for x in v)
        if method == "sum":
            return sum(x for x in v if isinstance(x, (int, float)))
        if method == "min":
            return min(v) if v else None
        if method == "max":
            return max(v) if v else None
        if method == "count":
            return len(v)
        if method == "enumerate":
            return list(enumerate(v))
        if method == "zip" and m0 is not None:
            other = list(m0) if not isinstance(m0, list) else m0
            return list(zip(v, other))
        if method in ("flat_map", "flatten"):
            result = []
            for item in v:
                if isinstance(item, list):
                    result.extend(item)
                else:
                    result.append(item)
            return result
        if method == "chunks":
            n = int(m0) if m0 else 1
            return [v[i:i + n] for i in range(0, len(v), n)]
        if method == "windows":
            n = int(m0) if m0 else 1
            return [v[i:i + n] for i in range(len(v) - n + 1)]
        if method == "truncate":
            if m0 is not None:
                v[:] = v[:int(m0)]
            return None
        if method in ("clear",):
            v.clear()
            return None
        if method in ("drain",):
            result = v[:]
            v.clear()
            return result
        if method == "split_at" and m0 is not None:
            n = int(m0)
            return (v[:n], v[n:])
        return None

    def _map_method(self, d: Dict[Any, Any], method: str, args: List[Any]) -> Any:
        m0 = args[0] if args else None
        m1 = args[1] if len(args) > 1 else None
        if method == "insert":
            d[m0] = m1
            return None
        if method == "get":
            return d.get(m0)
        if method in ("get_or_insert",):
            if m0 not in d:
                d[m0] = m1
            return d[m0]
        if method == "remove":
            return d.pop(m0, None)
        if method == "contains_key":
            return m0 in d
        if method in ("len",):
            return len(d)
        if method == "is_empty":
            return len(d) == 0
        if method == "keys":
            return list(d.keys())
        if method == "values":
            return list(d.values())
        if method in ("iter", "into_iter"):
            return list(d.items())
        if method == "iter_mut":
            return list(d.items())
        if method == "entry":
            return (d, m0)
        if method in ("or_insert",) and isinstance(d, tuple):
            table, key = d
            if key not in table:
                table[key] = m0
            return table.get(key)
        if method == "clone":
            return dict(d)
        if method in ("clear",):
            d.clear()
            return None
        return None

    # ------------------------------------------------------------------
    # Function call
    # ------------------------------------------------------------------

    def _call(self, name: str, args: List[Any]) -> Any:
        """Call a function: built-in or user-defined."""
        # Built-in functions
        builtin = self._call_builtin(name, args)
        if builtin is not _UNSET:
            return builtin

        # User function
        if name in self._functions:
            return self._call_function(name, args)

        # Struct constructor
        if name in self._structs:
            sdef = self._structs[name]
            fields = {}
            for (fname_f, _), val in zip(sdef.fields, args):
                fields[fname_f] = val
            return _RustStruct(name, fields)

        return None

    def _call_function(self, name: str, args: List[Any]) -> Any:
        fdef = self._functions.get(name)
        if fdef is None:
            raise _RustError(f"undefined function: {name}")
        self._push()
        for p, v in zip(fdef.params, args):
            p_clean = re.sub(r"^(?:mut\s+)?", "", p)
            self._set(p_clean, v)
        try:
            return self._exec_block(fdef.body)
        except _Return as r:
            return r.value
        finally:
            self._pop()

    def _call_builtin(self, name: str, args: List[Any]) -> Any:
        """Handle standard library functions."""
        a0 = args[0] if args else None
        a1 = args[1] if len(args) > 1 else None

        if name in ("println", "print"):
            output = self._format_macro("".join(str(a) for a in args))
            self._emit(output + ("\n" if name == "println" else ""))
            return None

        if name == "String::new":
            return ""
        if name == "Vec::new":
            return []
        if name in ("HashMap::new",):
            return {}
        if name == "vec":
            return list(args)

        if name in ("std::cmp::min", "i32::min", "u32::min"):
            return min(a0, a1) if a0 is not None and a1 is not None else a0
        if name in ("std::cmp::max", "i32::max", "u32::max"):
            return max(a0, a1) if a0 is not None and a1 is not None else a0

        if name == "std::mem::swap":
            # Can't easily swap in Python without mut refs; no-op
            return None

        if name in ("f64::from", "f32::from"):
            return float(a0) if a0 is not None else 0.0
        if name in ("i32::from", "u32::from", "i64::from"):
            return int(a0) if a0 is not None else 0

        # Turtle graphics
        if name.startswith("turtle::") or name.startswith("Turtle::"):
            return self._call_turtle(name.split("::", 1)[1], args)
        # bare turtle commands
        if name in ("forward", "backward", "right", "left", "penup", "pendown", "home", "clear", "pencolor"):
            return self._call_turtle(name, args)

        # Math functions
        if name in ("f64::sqrt", "f32::sqrt"):
            return math.sqrt(float(a0)) if a0 is not None else 0.0
        if name == "f64::abs":
            return abs(float(a0)) if a0 is not None else 0.0
        if name in ("f64::sin",):
            return math.sin(float(a0))
        if name in ("f64::cos",):
            return math.cos(float(a0))
        if name in ("f64::tan",):
            return math.tan(float(a0))
        if name in ("f64::floor",):
            return math.floor(float(a0))
        if name in ("f64::ceil",):
            return math.ceil(float(a0))

        if name in ("std::f64::consts::PI", "f64::PI"):
            return math.pi
        if name in ("std::f64::consts::E", "f64::E"):
            return math.e

        # Process
        if name in ("std::process::exit", "process::exit"):
            raise _Return(a0)

        return _UNSET

    def _call_turtle(self, func: str, args: List[Any]) -> Any:
        t = self.turtle
        if t is None:
            return None
        a0 = float(args[0]) if args and isinstance(args[0], (int, float)) else 0
        if func == "forward" or func == "fd":
            t.forward(a0)
        elif func == "backward" or func == "bk" or func == "back":
            t.backward(a0)
        elif func == "right" or func == "rt":
            t.right(a0)
        elif func == "left" or func == "lt":
            t.left(a0)
        elif func == "penup" or func == "pu":
            t.pen_up()
        elif func == "pendown" or func == "pd":
            t.pen_down()
        elif func == "home":
            t.home()
        elif func == "clear":
            t.clear()
        elif func == "pencolor" or func == "color":
            t.set_color(str(args[0]) if args else "black")
        return None

    # ------------------------------------------------------------------
    # Safe eval for arithmetic
    # ------------------------------------------------------------------

    def _safe_eval(self, expr: str) -> Any:
        """Evaluate arithmetic/comparison via Python eval."""
        py = expr
        # Substitute variables
        for scope in reversed(self._scope_stack):
            for k, v in scope.items():
                if re.search(rf"\b{re.escape(k)}\b", py):
                    py = re.sub(rf"\b{re.escape(k)}\b", repr(v), py)
        # Rust → Python operators
        py = py.replace("&&", " and ").replace("||", " or ")
        py = re.sub(r"\bas\b\s+\w+", "", py)  # strip type casts
        try:
            result = eval(py, {"__builtins__": {}}, {  # noqa: S307
                "True": True, "False": False, "None": None,
                "abs": abs, "len": len, "min": min, "max": max,
                "int": int, "float": float, "round": round,
            })
            return result
        except Exception:
            return self._get(expr)

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _apply_op(self, lhs: Any, op: str, rhs: Any) -> Any:
        if op == "+":
            if isinstance(lhs, str):
                return lhs + str(rhs)
            return (lhs or 0) + rhs
        if op == "-":
            return (lhs or 0) - rhs
        if op == "*":
            return (lhs or 0) * rhs
        if op == "/":
            return (lhs or 0) // rhs if isinstance(lhs, int) and isinstance(rhs, int) else (lhs or 0) / rhs
        if op == "%":
            return (lhs or 0) % rhs
        if op == "**":
            return (lhs or 0) ** rhs
        if op == "<<":
            return (lhs or 0) << rhs
        if op == ">>":
            return (lhs or 0) >> rhs
        if op == "||":
            return lhs if self._truthy(lhs) else rhs
        if op == "&&":
            return rhs if self._truthy(lhs) else lhs
        return rhs

    def _truthy(self, val: Any) -> bool:
        return val is not None and val is not False and val != 0

    def _split_args(self, s: str) -> List[str]:
        """Split comma-separated args respecting brackets and strings."""
        if not s.strip():
            return []
        parts = []
        depth = 0
        current: List[str] = []
        in_string = False
        for ch in s:
            if ch == '"' and not in_string:
                in_string = True
                current.append(ch)
            elif ch == '"' and in_string:
                in_string = False
                current.append(ch)
            elif not in_string:
                if ch in ("(", "[", "{"):
                    depth += 1
                    current.append(ch)
                elif ch in (")", "]", "}"):
                    depth -= 1
                    current.append(ch)
                elif ch == "," and depth == 0:
                    parts.append("".join(current).strip())
                    current = []
                else:
                    current.append(ch)
            else:
                current.append(ch)
        if current:
            parts.append("".join(current).strip())
        return [p for p in parts if p]
