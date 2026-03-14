"""
Small Prolog-like executor (Turbo Prolog flavor, simplified)

Supported subset:
- Facts: `parent(john,mary).`
- Rules (multi-goal bodies with commas, supports cut `!`)
- Variables (uppercase-leading treated as variables)
- Queries with backtracking: `?- parent(X,mary).` -> multiple solutions
- Arithmetic helper: `add/3` infers missing operand
- Comparison built-ins: `lt/2`, `gt/2`, `ge/2`, `le/2`, `eq/2`, `neq/2`
- Cut `!` for pruning alternatives (commits choices in current rule)

Limitations:
- Limited list/structure support (basic [H|T] unification)
- Multi-goal queries supported; some advanced features missing
"""

from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

_FACT_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*\((.+)\)\s*\.\s*$")
_FACT0_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*\.\s*$")
_RULE_PART1 = r"^\s*([a-z][a-z0-9_]*)\s*"
_RULE_PART2 = r"\((.+)\)\s*:-\s*(.+)\s*\.\s*$"
_RULE_PATTERN = _RULE_PART1 + _RULE_PART2
_RULE_RE = re.compile(_RULE_PATTERN)
_RULE0_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*:-\s*(.+)\s*\.\s*$")
_QUERY_RE = re.compile(r"^\s*\?-\s*([a-z][a-z0-9_]*)\s*\((.+)\)\s*\.\s*$")


def _extract_outer_args(s: str, start: int = 0) -> Tuple[Optional[str], int]:
    """Given a string starting at '(', find the matching ')' respecting nesting.
    Returns (content_between_parens, index_after_close_paren) or (None, -1)."""
    if start >= len(s) or s[start] != "(":
        return None, -1
    depth = 0
    i = start
    while i < len(s):
        if s[i] == "(":
            depth += 1
        elif s[i] == ")":
            depth -= 1
            if depth == 0:
                return s[start + 1 : i], i + 1
        elif s[i] == "'" or s[i] == '"':
            q = s[i]
            i += 1
            while i < len(s) and s[i] != q:
                i += 1
        i += 1
    return None, -1


def _is_var(token: str) -> bool:
    if not token:
        return False
    if token == "_":
        return True
    return (token[0].isalpha() and token[0].isupper()) or token.startswith("_")


def _parse_terms(arg_str: str) -> Tuple[str, ...]:
    parts: List[str] = []
    buf: List[str] = []
    depth = 0
    for ch in arg_str:
        if ch in "([":
            depth += 1
        elif ch in ")]":
            depth = max(0, depth - 1)

        if ch == "," and depth == 0:
            parts.append("".join(buf).strip())
            buf = []
        else:
            buf.append(ch)
    if buf:
        parts.append("".join(buf).strip())
    return tuple(p for p in parts if p)


def _ensure_kb(interpreter: "Interpreter"):
    if not hasattr(interpreter, "prolog_kb"):
        # Use list to preserve insertion order for deterministic behavior
        kb_dict = {"facts": [], "rules": [], "cut_active": False, "buffer": ""}
        interpreter.prolog_kb = kb_dict  # type: ignore
    # Ensure required keys exist even if
    # prolog_kb was pre-initialized as an empty dict
    if "facts" not in interpreter.prolog_kb:
        interpreter.prolog_kb["facts"] = []
    if "rules" not in interpreter.prolog_kb:
        interpreter.prolog_kb["rules"] = []
    if "cut_active" not in interpreter.prolog_kb:
        interpreter.prolog_kb["cut_active"] = False  # type: ignore
    if "buffer" not in interpreter.prolog_kb:
        interpreter.prolog_kb["buffer"] = ""
    if "var_counter" not in interpreter.prolog_kb:
        interpreter.prolog_kb["var_counter"] = 0
    if "section" not in interpreter.prolog_kb:
        interpreter.prolog_kb["section"] = "NONE"
    # Store interpreter reference for I/O built-ins
    interpreter.prolog_kb["interpreter"] = interpreter


def _split_list_head_tail(term: str) -> Tuple[str, str]:
    # Assumes term is [...] and not []
    content = term[1:-1].strip()

    # Check for pipe | or comma ,
    depth = 0
    pipe_idx = -1
    comma_idx = -1

    for i, ch in enumerate(content):
        if ch in "([":
            depth += 1
        elif ch in ")]":
            depth -= 1
        elif depth == 0:
            if ch == "|" and pipe_idx == -1:
                pipe_idx = i
            elif ch == "," and comma_idx == -1:
                comma_idx = i
                # If we found a comma, it's a standard list element separator
                # We prioritize comma for splitting head.
                break

    # If we found a comma, that's the split for Head, Rest
    if comma_idx != -1:
        head = content[:comma_idx].strip()
        tail_content = content[comma_idx + 1 :].strip()
        return head, "[" + tail_content + "]"

    # If no comma, but pipe
    if pipe_idx != -1:
        head = content[:pipe_idx].strip()
        tail = content[pipe_idx + 1 :].strip()
        return head, tail

    # Neither comma nor pipe -> single element list
    return content, "[]"


def _unify(x: str, y: str, env: Dict[str, str]) -> Optional[Dict[str, str]]:
    x = x.strip()
    y = y.strip()

    # Anonymous variable optimization
    if x == "_" or y == "_":
        return env

    # Both variables: do not bind variable-to-variable; treat as placeholder
    if _is_var(x) and _is_var(y):
        # If either already bound, ensure consistency
        if x in env and y in env:
            return _unify(env[x], env[y], env)
        if x in env:
            return _unify(env[x], y, env)
        if y in env:
            return _unify(x, env[y], env)
        # Neither bound: leave both free
        # But if x == y, it's fine
        if x == y:
            return env
        # Bind one to the other?
        # In this simple interpreter, we avoid var-var binding chains if possible
        # But for correctness we should probably bind them.
        # For now, let's just leave them free as per original code,
        # but original code had a bug where it returned env if x!=y without binding.
        # If I have f(X, Y) and unify with f(A, A). X=A, Y=A. So X=Y.
        # If I have f(X, Y) and unify with f(A, B). X=A, Y=B.
        # If I have f(X, X) and unify with f(A, B). X=A, X=B -> A=B.

        # Let's stick to simple binding:
        e = env.copy()
        e[x] = y
        return e

    # List Unification
    if x.startswith("[") and x.endswith("]") and y.startswith("[") and y.endswith("]"):
        if x == "[]" and y == "[]":
            return env
        if x == "[]" or y == "[]":
            return None

        xh, xt = _split_list_head_tail(x)
        yh, yt = _split_list_head_tail(y)

        env2 = _unify(xh, yh, env)
        if env2 is None:
            return None
        return _unify(xt, yt, env2)

    # Variable vs ground term
    if _is_var(x) and not _is_var(y):
        if x in env:
            return _unify(env[x], y, env)
        e = env.copy()
        e[x] = y
        return e
    if _is_var(y) and not _is_var(x):
        if y in env:
            return _unify(env[y], x, env)
        e = env.copy()
        e[y] = x
        return e
    # Ground terms: case-insensitive match
    return env if x.lower() == y.lower() else None


def _parse_body_goals(body: str) -> List[Tuple[str, Tuple[str, ...]]]:
    # Split by commas not inside parentheses (simple heuristic)
    goals: List[Tuple[str, Tuple[str, ...]]] = []
    buf: List[str] = []
    depth = 0
    for ch in body:
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth = max(0, depth - 1)
        if ch == "," and depth == 0:
            goals.append(_parse_single_goal("".join(buf).strip()))
            buf = []
        else:
            buf.append(ch)
    if buf:
        goals.append(_parse_single_goal("".join(buf).strip()))
    return goals


def _parse_single_goal(text: str) -> Tuple[str, Tuple[str, ...]]:
    t = text.strip()
    if t == "!":
        return ("!", tuple())

    # Handle infix 'is'
    # Pattern: Term is Expression
    # Term can be a Variable or a Number/Atom
    is_match = re.match(r"^([a-zA-Z0-9_]+)\s+is\s+(.+)$", t)
    if is_match:
        return ("is", (is_match.group(1), is_match.group(2).strip()))

    # Handle infix '\='
    neq_match = re.match(r"^([a-zA-Z0-9_]+)\s*\\=\s*([a-zA-Z0-9_]+)$", t)
    if neq_match:
        return ("neq", (neq_match.group(1), neq_match.group(2)))

    # Handle infix comparison operators: <, >, <=, >=, =
    # Order matters: check >= and <= before > and <

    # >=
    ge_match = re.match(r"^([a-zA-Z0-9_]+)\s*>=\s*([a-zA-Z0-9_]+)$", t)
    if ge_match:
        return ("ge", (ge_match.group(1), ge_match.group(2)))

    # <=
    le_match = re.match(r"^([a-zA-Z0-9_]+)\s*<=\s*([a-zA-Z0-9_]+)$", t)
    if le_match:
        return ("le", (le_match.group(1), le_match.group(2)))

    # >
    gt_match = re.match(r"^([a-zA-Z0-9_]+)\s*>\s*([a-zA-Z0-9_]+)$", t)
    if gt_match:
        return ("gt", (gt_match.group(1), gt_match.group(2)))

    # <
    lt_match = re.match(r"^([a-zA-Z0-9_]+)\s*<\s*([a-zA-Z0-9_]+)$", t)
    if lt_match:
        return ("lt", (lt_match.group(1), lt_match.group(2)))

    # = (unification)
    eq_match = re.match(r"^([a-zA-Z0-9_]+)\s*=\s*([a-zA-Z0-9_]+)$", t)
    if eq_match:
        return ("eq", (eq_match.group(1), eq_match.group(2)))

    # Handle infix =:= (arithmetic equality)
    eqe_match = re.match(r"^(.+?)\s*=:=\s*(.+)$", t)
    if eqe_match:
        return ("=:=", (eqe_match.group(1).strip(), eqe_match.group(2).strip()))

    # Handle infix =\= (arithmetic inequality)
    ane_match = re.match(r"^(.+?)\s*=\\=\s*(.+)$", t)
    if ane_match:
        return ("=\\=", (ane_match.group(1).strip(), ane_match.group(2).strip()))

    # Compound term: functor(args...) with balanced parens
    fm = re.match(r"^([a-z][a-z0-9_]*)\s*\(", t)
    if fm:
        functor = fm.group(1)
        # Find matching closing paren
        start = t.index("(", len(functor))
        depth = 0
        for idx in range(start, len(t)):
            if t[idx] == "(":
                depth += 1
            elif t[idx] == ")":
                depth -= 1
                if depth == 0:
                    args_str = t[start + 1 : idx]
                    rest = t[idx + 1 :].strip()
                    if not rest:  # nothing after closing paren
                        return (functor.lower(), _parse_terms(args_str))
                    break
    # Treat bare atom as predicate with arity 0
    return (t.lower(), tuple())


def _body_has_cut(goals: List[Tuple[str, Tuple[str, ...]]]) -> bool:
    for gp, _ in goals:
        if gp == "!":
            return True
    return False


def _split_on_cut(
    goals: List[Tuple[str, Tuple[str, ...]]],
) -> Tuple[
    List[Tuple[str, Tuple[str, ...]]],
    List[Tuple[str, Tuple[str, ...]]],
]:
    pre: List[Tuple[str, Tuple[str, ...]]] = []
    post: List[Tuple[str, Tuple[str, ...]]] = []
    seen_cut = False
    for gp, gargs in goals:
        if gp == "!":
            seen_cut = True
            continue
        if not seen_cut:
            pre.append((gp, gargs))
        else:
            post.append((gp, gargs))
    return pre, post


def _solve_goals_first(
    kb: Dict[str, Any],
    goals: List[Tuple[str, Tuple[str, ...]]],
    env: Dict[str, str],
) -> List[Dict[str, str]]:
    # Like _solve_goals but returns at most the first successful environment
    if not goals:
        return [env]
    pred, args = goals[0]
    rest = goals[1:]
    if pred == "!":
        # Should not appear here (pre-cut only), but handle gracefully
        return _solve_goals_first(kb, rest, env)
    for env1 in _solve(kb, pred, args):
        merged = env.copy()
        merged.update(env1)
        child = _solve_goals_first(kb, rest, merged)
        if child:
            return child[:1]
    return []


def _num_value(token: str, env: Dict[str, str]) -> Optional[float]:
    if _is_var(token):
        target = token
        seen = set()
        while target in env and _is_var(env[target]):
            if target in seen:
                return None
            seen.add(target)
            target = env[target]

        if target in env and re.fullmatch(r"-?\d+(?:\.\d+)?", env[target]):
            return float(env[target])
        return None
    if re.fullmatch(r"-?\d+(?:\.\d+)?", token):
        return float(token)
    return None


def _bind_num(
    token: str, value: float, env: Dict[str, str]
) -> Optional[Dict[str, str]]:
    if _is_var(token):
        target = token
        seen = set()
        while target in env and _is_var(env[target]):
            if target in seen:
                return None
            seen.add(target)
            target = env[target]

        e = env.copy()
        # Store as integer if whole
        if abs(value - int(value)) < 1e-9:
            e[target] = str(int(value))
        else:
            e[target] = str(value)
        return e
    # If constant provided, must match
    if re.fullmatch(r"-?\d+(?:\.\d+)?", token):
        return env if abs(float(token) - value) < 1e-9 else None
    return None


def _solve_goals(
    kb: Dict[str, Any],
    goals: List[Tuple[str, Tuple[str, ...]]],
    env: Dict[str, str],
) -> List[Dict[str, str]]:
    envs_with_cut = _solve_goals_cut(kb, goals, env)
    # Filter out cut-failure sentinels
    return [e for (e, _cut) in envs_with_cut if "__CUTFAIL__" not in e]


def _eval_math(expr: str, env: Dict[str, str]) -> Optional[float]:
    """Evaluate arithmetic expression with variables - full set of functions."""
    import math as _math

    def replace_var(match):
        var_name = match.group(0)
        if var_name in env:
            val = env[var_name]
            if re.fullmatch(r"-?\d+(?:\.\d+)?", val):
                return val
        return var_name

    expr_sub = re.sub(r"\b[A-Z][a-zA-Z0-9_]*\b", replace_var, expr)
    expr_sub = re.sub(r"\bmod\b", "%", expr_sub)
    expr_sub = re.sub(r"\brem\b", "%", expr_sub)
    expr_sub = re.sub(r"\bdiv\b", "//", expr_sub)
    expr_sub = re.sub(r"\bxor\b", "^", expr_sub)
    expr_sub = re.sub(r"\b/\\b/", "//", expr_sub)

    # Map Prolog math functions to Python
    fn_map = {
        "abs": "abs",
        "sign": "sign",
        "sqrt": "_math.sqrt",
        "sin": "_math.sin",
        "cos": "_math.cos",
        "tan": "_math.tan",
        "asin": "_math.asin",
        "acos": "_math.acos",
        "atan": "_math.atan",
        "atan2": "_math.atan2",
        "exp": "_math.exp",
        "log": "_math.log",
        "log2": "_math.log2",
        "ceiling": "_math.ceil",
        "floor": "_math.floor",
        "round": "round",
        "truncate": "int",
        "float": "float",
        "float_integer_part": "float",
        "float_fractional_part": "lambda x: x % 1",
        "integer": "int",
        "max": "max",
        "min": "min",
        "pi": str(_math.pi),
        "e": str(_math.e),
        "inf": "float('inf')",
        "nan": "float('nan')",
        "succ": "lambda x: x+1",
        "plus": "lambda x,y: x+y",
        "msb": "lambda x: x.bit_length()-1 if x>0 else 0",
    }
    for k, v in fn_map.items():
        if k in ("pi", "e", "inf", "nan"):
            expr_sub = re.sub(r"\b" + k + r"\b", v, expr_sub)
        else:
            expr_sub = re.sub(r"\b" + k + r"\b", v, expr_sub)

    try:
        if not re.match(r"^[\d\s+\-*/%().a-zA-Z_,']+$", expr_sub):
            return None
        allowed_names: Dict[str, Any] = {
            "__builtins__": {},
            "_math": _math,
            "abs": abs,
            "round": round,
            "int": int,
            "float": float,
            "max": max,
            "min": min,
            "sign": lambda x: (1.0 if x > 0 else (-1.0 if x < 0 else 0.0)),
        }
        return float(eval(expr_sub, allowed_names, {}))  # noqa: S307
    except (ValueError, TypeError, SyntaxError, ZeroDivisionError, NameError):
        return None


def _substitute_term(term: str, env: Dict[str, str]) -> str:
    # Handle simple case first
    if term in env:
        val = env[term]
        if val == term:
            return val
        return _substitute_term(val, env)

    # If term has no variables, return as is
    if not any(c.isupper() or c == "_" for c in term):
        return term

    # Find all potential variables
    # We use a tokenizing approach to be safer
    tokens = re.split(r"([a-zA-Z0-9_]+)", term)
    result = []
    for token in tokens:
        if _is_var(token) and token != "_":
            if token in env:
                val = env[token]
                if val != token:
                    token = _substitute_term(val, env)
        result.append(token)
    return "".join(result)


def _solve_goals_cut(
    kb: Dict[str, Any],
    goals: List[Tuple[str, Tuple[str, ...]]],
    env: Dict[str, str],
) -> List[Tuple[Dict[str, str], bool]]:
    if not goals:
        return [(env, False)]
    pred, args = goals[0]
    rest = goals[1:]
    out: List[Tuple[Dict[str, str], bool]] = []
    # Built-ins
    if pred == "!":
        # Cut: mark cut active for current rule evaluation and continue
        kb["cut_active"] = True  # type: ignore
        child = _solve_goals_cut(kb, rest, env)
        if not child:
            # Even if the rest fails, cut commits: signal prune to caller
            return [({"__CUTFAIL__": "1"}, True)]
        return [(e, True) for (e, _c) in child]
    if pred == "is" and len(args) == 2:
        var_token, expr_token = args
        val = _eval_math(expr_token, env)
        if val is not None:
            e = _bind_num(var_token, val, env)
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []
    if pred == "add" and len(args) == 3:
        a_token, b_token, c_token = args
        av = _num_value(a_token, env)
        bv = _num_value(b_token, env)
        cv = _num_value(c_token, env)
        if av is not None and bv is not None:
            e = _bind_num(c_token, av + bv, env)
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        if av is not None and cv is not None:
            e = _bind_num(b_token, cv - av, env)
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        if bv is not None and cv is not None:
            e = _bind_num(a_token, cv - bv, env)
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []
    if pred == "lt" and len(args) == 2:
        a, b = args
        av = _num_value(a, env)
        bv = _num_value(b, env)
        if av is not None and bv is not None and av < bv:
            return _solve_goals_cut(kb, rest, env)
        return []
    if pred == "gt" and len(args) == 2:
        a, b = args
        av = _num_value(a, env)
        bv = _num_value(b, env)
        if av is not None and bv is not None and av > bv:
            return _solve_goals_cut(kb, rest, env)
        return []
    if pred == "ge" and len(args) == 2:
        a, b = args
        av = _num_value(a, env)
        bv = _num_value(b, env)
        if av is not None and bv is not None and av >= bv:
            return _solve_goals_cut(kb, rest, env)
        return []
    if pred == "le" and len(args) == 2:
        a, b = args
        av = _num_value(a, env)
        bv = _num_value(b, env)
        if av is not None and bv is not None and av <= bv:
            return _solve_goals_cut(kb, rest, env)
        return []
    if pred == "eq" and len(args) == 2:
        a, b = args
        # Try unify semantics allowing variable binding
        env2 = _unify(a, b, env.copy())
        if env2 is not None:
            return _solve_goals_cut(kb, rest, env2)
        return []
    if pred == "neq" and len(args) == 2:
        a, b = args
        # Success only if cannot unify given current bindings (no new bindings)
        # If both numeric, require inequality
        av = _num_value(a, env)
        bv = _num_value(b, env)
        if av is not None and bv is not None:
            if av != bv:
                return _solve_goals_cut(kb, rest, env)
            return []
        env2 = _unify(a, b, env.copy())
        if env2 is None:
            return _solve_goals_cut(kb, rest, env)
        return []

    # I/O Built-ins
    interpreter = kb.get("interpreter")
    if pred == "write" and len(args) >= 1:
        # write(X) or write("String")
        # Concatenate all args
        text_parts = []
        for arg in args:
            sval = _substitute_term(arg, env)
            # Strip quotes if it's a string literal
            if sval.startswith('"') and sval.endswith('"'):
                sval = sval[1:-1]
            text_parts.append(str(sval))

        if interpreter:
            # We don't have a direct "write without newline" on interpreter output list
            # But we can append to the last line if it exists?
            # No, interpreter.output is a list of lines.
            # We'll just append a new line for now, or maybe accumulate in a buffer?
            # Turbo Prolog 'write' doesn't add newline. 'writeln' does.
            # Since our UI displays lines, 'write' might be tricky.
            # Let's assume we just append to output list for now.
            # Or better: use a temporary buffer in kb?
            # Let's just append to output list as a new line for simplicity,
            # or try to merge with previous if it didn't end with newline?
            # The interpreter output is List[str].
            # Let's just append.
            interpreter.output.append("".join(text_parts))
        return _solve_goals_cut(kb, rest, env)

    if pred == "writeln" and len(args) >= 1:
        text_parts = []
        for arg in args:
            sval = _substitute_term(arg, env)
            if sval.startswith('"') and sval.endswith('"'):
                sval = sval[1:-1]
            text_parts.append(str(sval))
        if interpreter:
            interpreter.output.append("".join(text_parts))
        return _solve_goals_cut(kb, rest, env)

    if pred == "nl" and len(args) == 0:
        if interpreter:
            interpreter.output.append("")
        return _solve_goals_cut(kb, rest, env)

    if pred == "concat" and len(args) == 3:
        s1, s2, s3 = args
        v1 = _substitute_term(s1, env)
        v2 = _substitute_term(s2, env)
        v3 = _substitute_term(s3, env)

        # Mode: concat(in, in, out)
        if not _is_var(v1) and not _is_var(v2):
            # Strip quotes
            str1 = v1[1:-1] if v1.startswith('"') else v1
            str2 = v2[1:-1] if v2.startswith('"') else v2
            res = f'"{str1}{str2}"'
            e = _unify(v3, res, env.copy())
            if e is not None:
                return _solve_goals_cut(kb, rest, e)
        return []

    # ── Standard arithmetic comparison predicates ─────────────────────────────
    if pred == "=:=" and len(args) == 2:
        av, bv = _eval_math(args[0], env), _eval_math(args[1], env)
        return (
            _solve_goals_cut(kb, rest, env)
            if av is not None and bv is not None and av == bv
            else []
        )
    if pred in ("=\\=", "=\\\\=") and len(args) == 2:
        av, bv = _eval_math(args[0], env), _eval_math(args[1], env)
        return (
            _solve_goals_cut(kb, rest, env)
            if av is not None and bv is not None and av != bv
            else []
        )
    if pred == "<" and len(args) == 2:
        av, bv = _eval_math(args[0], env), _eval_math(args[1], env)
        return (
            _solve_goals_cut(kb, rest, env)
            if av is not None and bv is not None and av < bv
            else []
        )
    if pred == ">" and len(args) == 2:
        av, bv = _eval_math(args[0], env), _eval_math(args[1], env)
        return (
            _solve_goals_cut(kb, rest, env)
            if av is not None and bv is not None and av > bv
            else []
        )
    if pred == "=<" and len(args) == 2:
        av, bv = _eval_math(args[0], env), _eval_math(args[1], env)
        return (
            _solve_goals_cut(kb, rest, env)
            if av is not None and bv is not None and av <= bv
            else []
        )
    if pred == ">=" and len(args) == 2:
        av, bv = _eval_math(args[0], env), _eval_math(args[1], env)
        return (
            _solve_goals_cut(kb, rest, env)
            if av is not None and bv is not None and av >= bv
            else []
        )

    # ── Unification ==/\== ────────────────────────────────────────────────────
    if pred == "==" and len(args) == 2:
        a_sub = _substitute_term(args[0], env)
        b_sub = _substitute_term(args[1], env)
        return _solve_goals_cut(kb, rest, env) if a_sub == b_sub else []
    if pred in ("\\==", "\\\\==") and len(args) == 2:
        a_sub = _substitute_term(args[0], env)
        b_sub = _substitute_term(args[1], env)
        return _solve_goals_cut(kb, rest, env) if a_sub != b_sub else []
    if pred == "=" and len(args) == 2:
        e = _unify(args[0], args[1], env.copy())
        return _solve_goals_cut(kb, rest, e) if e is not None else []
    if pred in ("\\=", "\\\\=") and len(args) == 2:
        e = _unify(args[0], args[1], env.copy())
        return [] if e is not None else _solve_goals_cut(kb, rest, env)

    # ── Dynamic database modification ─────────────────────────────────────────
    if pred in ("assert", "assertz") and len(args) == 1:
        clause = _substitute_term(args[0], env)
        # Parse: head :- body  or just head
        if ":-" in clause:
            parts = clause.split(":-", 1)
            head_str = parts[0].strip()
        else:
            head_str = clause.strip()
        hm = re.match(r"^(\w+)\s*(?:\((.+)\))?$", head_str)
        if hm:
            name = hm.group(1)
            hargs = (
                tuple(a.strip() for a in (hm.group(2) or "").split(","))
                if hm.group(2)
                else ()
            )
            if ":-" in clause:
                body_str = clause.split(":-", 1)[1].strip()
                body = _parse_body_goals(body_str)
                kb["rules"].append((name, hargs, body))
            else:
                kb["facts"].append((name, hargs))
        return _solve_goals_cut(kb, rest, env)

    if pred == "asserta" and len(args) == 1:
        clause = _substitute_term(args[0], env)
        if ":-" in clause:
            parts = clause.split(":-", 1)
            head_str = parts[0].strip()
        else:
            head_str = clause.strip()
        hm = re.match(r"^(\w+)\s*(?:\((.+)\))?$", head_str)
        if hm:
            name = hm.group(1)
            hargs = (
                tuple(a.strip() for a in (hm.group(2) or "").split(","))
                if hm.group(2)
                else ()
            )
            if ":-" in clause:
                body_str = clause.split(":-", 1)[1].strip()
                body = _parse_body_goals(body_str)
                kb["rules"].insert(0, (name, hargs, body))
            else:
                kb["facts"].insert(0, (name, hargs))
        return _solve_goals_cut(kb, rest, env)

    if pred == "retract" and len(args) == 1:
        clause = _substitute_term(args[0], env)
        if ":-" in clause:
            head_str = clause.split(":-", 1)[0].strip()
        else:
            head_str = clause.strip()
        hm = re.match(r"^(\w+)\s*(?:\((.+)\))?$", head_str)
        if hm:
            name = hm.group(1)
            hargs = (
                tuple(a.strip() for a in (hm.group(2) or "").split(","))
                if hm.group(2)
                else ()
            )
            # Remove first matching fact
            for i, f in enumerate(kb.get("facts", [])):
                if f[0] == name and f[1] == hargs:
                    kb["facts"].pop(i)
                    break
        return _solve_goals_cut(kb, rest, env)

    if pred == "abolish" and len(args) == 1:
        name = _substitute_term(args[0], env).split("/")[0]
        kb["facts"] = [f for f in kb.get("facts", []) if f[0] != name]
        kb["rules"] = [r for r in kb.get("rules", []) if r[0] != name]
        return _solve_goals_cut(kb, rest, env)

    # ── findall/3: findall(Template, Goal, Bag) ────────────────────────────────
    if pred == "findall" and len(args) == 3:
        template, goal_term, bag_var = args
        template = _substitute_term(template, env)
        bag_var_sub = _substitute_term(bag_var, env)
        goal_parsed = _parse_body_goals(goal_term)
        # Use an isolated kb copy so cut inside the goal doesn't escape
        findall_kb = {**kb, "cut_active": False}
        solutions = _solve_goals(findall_kb, goal_parsed, env.copy())
        bag_items = [_substitute_term(template, sol) for sol in solutions]
        # Build prolog list
        bag_list = "[]"
        for item in reversed(bag_items):
            bag_list = f"[{item}|{bag_list}]" if bag_list != "[]" else f"[{item}]"
        # Join into comma list
        bag_str = "[" + ",".join(bag_items) + "]"
        e = _unify(bag_var_sub, bag_str, env.copy())
        return _solve_goals_cut(kb, rest, e) if e is not None else []

    # ── bagof/3: like findall but fails if no solutions ────────────────────────
    if pred == "bagof" and len(args) == 3:
        template, goal_term, bag_var = args
        template = _substitute_term(template, env)
        goal_parsed = _parse_body_goals(goal_term)
        # Isolated kb copy so cut doesn't escape
        bagof_kb = {**kb, "cut_active": False}
        solutions = _solve_goals(bagof_kb, goal_parsed, env.copy())
        if not solutions:
            return []
        bag_items = [_substitute_term(template, sol) for sol in solutions]
        bag_str = "[" + ",".join(bag_items) + "]"
        e = _unify(bag_var, bag_str, env.copy())
        return _solve_goals_cut(kb, rest, e) if e is not None else []

    # ── setof/3: like bagof but sorted and uniqued ─────────────────────────────
    if pred == "setof" and len(args) == 3:
        template, goal_term, bag_var = args
        template = _substitute_term(template, env)
        goal_parsed = _parse_body_goals(goal_term)
        # Isolated kb copy so cut doesn't escape
        setof_kb = {**kb, "cut_active": False}
        solutions = _solve_goals(setof_kb, goal_parsed, env.copy())
        if not solutions:
            return []
        bag_items = list(
            dict.fromkeys(_substitute_term(template, sol) for sol in solutions)
        )
        bag_items.sort()
        bag_str = "[" + ",".join(bag_items) + "]"
        e = _unify(bag_var, bag_str, env.copy())
        return _solve_goals_cut(kb, rest, e) if e is not None else []

    # ── aggregate_all/3 ───────────────────────────────────────────────────────
    if pred == "aggregate_all" and len(args) == 3:
        agg_type, goal_term, result_var = args
        goal_parsed = _parse_body_goals(goal_term)
        solutions = _solve_goals(kb, goal_parsed, env.copy())
        agg = _substitute_term(agg_type, env)
        if agg == "count":
            result = str(len(solutions))
        else:
            result = str(len(solutions))
        e = _unify(result_var, result, env.copy())
        return _solve_goals_cut(kb, rest, e) if e is not None else []

    # ── Type checking predicates ───────────────────────────────────────────────
    if pred == "var" and len(args) == 1:
        a_sub = _substitute_term(args[0], env)
        return _solve_goals_cut(kb, rest, env) if _is_var(a_sub) else []
    if pred == "nonvar" and len(args) == 1:
        a_sub = _substitute_term(args[0], env)
        return _solve_goals_cut(kb, rest, env) if not _is_var(a_sub) else []
    if pred == "atom" and len(args) == 1:
        a_sub = _substitute_term(args[0], env)
        is_atom = (
            not _is_var(a_sub)
            and not re.match(r"^-?\d", a_sub)
            and not a_sub.startswith("[")
        )
        return _solve_goals_cut(kb, rest, env) if is_atom else []
    if pred == "number" and len(args) == 1:
        a_sub = _substitute_term(args[0], env)
        try:
            float(a_sub)
            return _solve_goals_cut(kb, rest, env)
        except (ValueError, TypeError):
            return []
    if pred == "integer" and len(args) == 1:
        a_sub = _substitute_term(args[0], env)
        try:
            int(a_sub)
            return _solve_goals_cut(kb, rest, env)
        except (ValueError, TypeError):
            return []
    if pred == "float" and len(args) == 1:
        a_sub = _substitute_term(args[0], env)
        try:
            f = float(a_sub)
            return _solve_goals_cut(kb, rest, env) if "." in str(f) else []
        except (ValueError, TypeError):
            return []
    if pred == "atomic" and len(args) == 1:
        a_sub = _substitute_term(args[0], env)
        return (
            _solve_goals_cut(kb, rest, env)
            if not _is_var(a_sub) and not (a_sub.startswith("[") or "(" in a_sub)
            else []
        )
    if pred == "compound" and len(args) == 1:
        a_sub = _substitute_term(args[0], env)
        return (
            _solve_goals_cut(kb, rest, env)
            if "(" in a_sub or (a_sub.startswith("[") and a_sub != "[]")
            else []
        )
    if pred == "is_list" and len(args) == 1:
        a_sub = _substitute_term(args[0], env)
        return _solve_goals_cut(kb, rest, env) if a_sub.startswith("[") else []
    if pred == "callable" and len(args) == 1:
        a_sub = _substitute_term(args[0], env)
        return _solve_goals_cut(kb, rest, env) if not _is_var(a_sub) else []

    # ── List predicates ────────────────────────────────────────────────────────
    if pred == "append" and len(args) == 3:
        list1 = _substitute_term(args[0], env)
        list2 = _substitute_term(args[1], env)
        list3 = _substitute_term(args[2], env)
        if not _is_var(list1) and not _is_var(list2):
            items1 = [
                x.strip()
                for x in list1.strip("[]").split(",")
                if x.strip() and x.strip() != "[]"
            ]
            items2 = [
                x.strip()
                for x in list2.strip("[]").split(",")
                if x.strip() and x.strip() != "[]"
            ]
            merged = "[" + ",".join(items1 + items2) + "]"
            e = _unify(list3, merged, env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    if pred == "member" and len(args) == 2:
        elem, lst = args
        lst_sub = _substitute_term(lst, env)
        if not _is_var(lst_sub) and lst_sub.startswith("["):
            items = [
                x.strip()
                for x in lst_sub.strip("[]").split(",")
                if x.strip() and x.strip() != "[]"
            ]
            for item in items:
                e = _unify(elem, item, env.copy())
                if e is not None:
                    results = _solve_goals_cut(kb, rest, e)
                    out.extend(results)
            return out
        return []

    if pred == "memberchk" and len(args) == 2:
        elem, lst = args
        lst_sub = _substitute_term(lst, env)
        if not _is_var(lst_sub) and lst_sub.startswith("["):
            items = [
                x.strip()
                for x in lst_sub.strip("[]").split(",")
                if x.strip() and x.strip() != "[]"
            ]
            for item in items:
                e = _unify(elem, item, env.copy())
                if e is not None:
                    return _solve_goals_cut(kb, rest, e)
        return []

    if pred == "length" and len(args) == 2:
        lst_sub = _substitute_term(args[0], env)
        len_sub = _substitute_term(args[1], env)
        if not _is_var(lst_sub) and lst_sub.startswith("["):
            items = [
                x.strip()
                for x in lst_sub.strip("[]").split(",")
                if x.strip() and x.strip() != "[]"
            ]
            n = len(items if lst_sub != "[]" else [])
            e = _unify(len_sub, str(n), env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    if pred == "last" and len(args) == 2:
        lst_sub = _substitute_term(args[0], env)
        if not _is_var(lst_sub) and lst_sub.startswith("["):
            items = [
                x.strip()
                for x in lst_sub.strip("[]").split(",")
                if x.strip() and x.strip() != "[]"
            ]
            if items:
                e = _unify(args[1], items[-1], env.copy())
                return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    if pred in ("nth0", "nth1") and len(args) == 3:
        idx_sub = _substitute_term(args[0], env)
        lst_sub = _substitute_term(args[1], env)
        elem_var = args[2]
        if not _is_var(idx_sub) and not _is_var(lst_sub) and lst_sub.startswith("["):
            items = [
                x.strip()
                for x in lst_sub.strip("[]").split(",")
                if x.strip() and x.strip() != "[]"
            ]
            try:
                idx = int(idx_sub)
                if pred == "nth1":
                    idx -= 1
                e = _unify(elem_var, items[idx], env.copy())
                return _solve_goals_cut(kb, rest, e) if e is not None else []
            except (ValueError, IndexError):
                pass
        return []

    if pred == "reverse" and len(args) == 2:
        lst_sub = _substitute_term(args[0], env)
        if not _is_var(lst_sub) and lst_sub.startswith("["):
            items = [
                x.strip()
                for x in lst_sub.strip("[]").split(",")
                if x.strip() and x.strip() != "[]"
            ]
            rev = "[" + ",".join(reversed(items)) + "]"
            e = _unify(args[1], rev, env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    if pred in ("sort", "msort") and len(args) == 2:
        lst_sub = _substitute_term(args[0], env)
        if not _is_var(lst_sub) and lst_sub.startswith("["):
            items = [
                x.strip()
                for x in lst_sub.strip("[]").split(",")
                if x.strip() and x.strip() != "[]"
            ]
            sorted_items = sorted(set(items) if pred == "sort" else items)
            sorted_str = "[" + ",".join(sorted_items) + "]"
            e = _unify(args[1], sorted_str, env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    if pred == "flatten" and len(args) == 2:
        lst_sub = _substitute_term(args[0], env)
        flat = re.sub(r"[\[\]]", "", lst_sub)
        flat_items = [x.strip() for x in flat.split(",") if x.strip()]
        flat_str = "[" + ",".join(flat_items) + "]"
        e = _unify(args[1], flat_str, env.copy())
        return _solve_goals_cut(kb, rest, e) if e is not None else []

    if pred in ("sum_list", "sumlist") and len(args) == 2:
        lst_sub = _substitute_term(args[0], env)
        if not _is_var(lst_sub) and lst_sub.startswith("["):
            items = [
                x.strip()
                for x in lst_sub.strip("[]").split(",")
                if x.strip() and x.strip() != "[]"
            ]
            try:
                total = sum(float(x) for x in items)
                total_str = str(int(total)) if total == int(total) else str(total)
                e = _unify(args[1], total_str, env.copy())
                return _solve_goals_cut(kb, rest, e) if e is not None else []
            except (ValueError, TypeError):
                pass
        return []

    if pred == "max_list" and len(args) == 2:
        lst_sub = _substitute_term(args[0], env)
        if not _is_var(lst_sub) and lst_sub.startswith("["):
            items = [
                x.strip()
                for x in lst_sub.strip("[]").split(",")
                if x.strip() and x.strip() != "[]"
            ]
            try:
                mx = max(float(x) for x in items)
                mx_str = str(int(mx)) if mx == int(mx) else str(mx)
                e = _unify(args[1], mx_str, env.copy())
                return _solve_goals_cut(kb, rest, e) if e is not None else []
            except (ValueError, TypeError):
                pass
        return []

    if pred == "min_list" and len(args) == 2:
        lst_sub = _substitute_term(args[0], env)
        if not _is_var(lst_sub) and lst_sub.startswith("["):
            items = [
                x.strip()
                for x in lst_sub.strip("[]").split(",")
                if x.strip() and x.strip() != "[]"
            ]
            try:
                mn = min(float(x) for x in items)
                mn_str = str(int(mn)) if mn == int(mn) else str(mn)
                e = _unify(args[1], mn_str, env.copy())
                return _solve_goals_cut(kb, rest, e) if e is not None else []
            except (ValueError, TypeError):
                pass
        return []

    if pred == "numlist" and len(args) == 3:
        low_s = _substitute_term(args[0], env)
        high_s = _substitute_term(args[1], env)
        try:
            low, high = int(float(low_s)), int(float(high_s))
            lst = "[" + ",".join(str(i) for i in range(low, high + 1)) + "]"
            e = _unify(args[2], lst, env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        except (ValueError, TypeError):
            pass
        return []

    if pred == "between" and len(args) == 3:
        low_s = _substitute_term(args[0], env)
        high_s = _substitute_term(args[1], env)
        x_var = args[2]
        try:
            low, high = int(float(low_s)), int(float(high_s))
            for i in range(low, high + 1):
                e = _unify(x_var, str(i), env.copy())
                if e is not None:
                    results = _solve_goals_cut(kb, rest, e)
                    out.extend(results)
            return out
        except (ValueError, TypeError):
            return []

    if pred == "succ" and len(args) == 2:
        a_sub = _substitute_term(args[0], env)
        b_sub = _substitute_term(args[1], env)
        if not _is_var(a_sub):
            e = _unify(b_sub, str(int(float(a_sub)) + 1), env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        if not _is_var(b_sub):
            e = _unify(a_sub, str(int(float(b_sub)) - 1), env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    if pred == "plus" and len(args) == 3:
        a_v = _num_value(args[0], env)
        b_v = _num_value(args[1], env)
        c_v = _num_value(args[2], env)
        if a_v is not None and b_v is not None:
            e = _unify(args[2], str(int(a_v + b_v)), env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        if a_v is not None and c_v is not None:
            e = _unify(args[1], str(int(c_v - a_v)), env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        if b_v is not None and c_v is not None:
            e = _unify(args[0], str(int(c_v - b_v)), env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    # ── Atom/string predicates ─────────────────────────────────────────────────
    if pred == "atom_length" and len(args) == 2:
        atom_sub = _substitute_term(args[0], env).strip("'\"")
        e = _unify(args[1], str(len(atom_sub)), env.copy())
        return _solve_goals_cut(kb, rest, e) if e is not None else []

    if pred == "atom_concat" and len(args) == 3:
        a_sub = _substitute_term(args[0], env).strip("'\"")
        b_sub = _substitute_term(args[1], env).strip("'\"")
        result = a_sub + b_sub
        e = _unify(args[2], result, env.copy())
        return _solve_goals_cut(kb, rest, e) if e is not None else []

    if pred == "atom_chars" and len(args) == 2:
        atom_sub = _substitute_term(args[0], env).strip("'\"")
        if not _is_var(atom_sub):
            chars = "[" + ",".join(f"'{c}'" for c in atom_sub) + "]"
            e = _unify(args[1], chars, env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    if pred == "atom_codes" and len(args) == 2:
        atom_sub = _substitute_term(args[0], env).strip("'\"")
        if not _is_var(atom_sub):
            codes = "[" + ",".join(str(ord(c)) for c in atom_sub) + "]"
            e = _unify(args[1], codes, env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    if pred == "char_code" and len(args) == 2:
        char_sub = _substitute_term(args[0], env).strip("'\"")
        if not _is_var(char_sub) and char_sub:
            e = _unify(args[1], str(ord(char_sub[0])), env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    if pred == "number_codes" and len(args) == 2:
        num_sub = _substitute_term(args[0], env)
        if not _is_var(num_sub):
            codes = "[" + ",".join(str(ord(c)) for c in str(num_sub)) + "]"
            e = _unify(args[1], codes, env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    if pred == "number_chars" and len(args) == 2:
        num_sub = _substitute_term(args[0], env)
        if not _is_var(num_sub):
            chars = "[" + ",".join(f"'{c}'" for c in str(num_sub)) + "]"
            e = _unify(args[1], chars, env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    if pred in ("atom_string", "string_to_atom") and len(args) == 2:
        a_sub = _substitute_term(args[0], env)
        b_sub = _substitute_term(args[1], env)
        if not _is_var(a_sub):
            e = _unify(b_sub, a_sub.strip("'\""), env.copy())
        elif not _is_var(b_sub):
            e = _unify(a_sub, f"'{b_sub}'", env.copy())
        else:
            return []
        return _solve_goals_cut(kb, rest, e) if e is not None else []

    if pred == "upcase_atom" and len(args) == 2:
        a_sub = _substitute_term(args[0], env).strip("'\"")
        e = _unify(args[1], a_sub.upper(), env.copy())
        return _solve_goals_cut(kb, rest, e) if e is not None else []

    if pred == "downcase_atom" and len(args) == 2:
        a_sub = _substitute_term(args[0], env).strip("'\"")
        e = _unify(args[1], a_sub.lower(), env.copy())
        return _solve_goals_cut(kb, rest, e) if e is not None else []

    if pred == "sub_atom" and len(args) == 5:
        atom_sub = _substitute_term(args[0], env).strip("'\"")
        sub_sub = _substitute_term(args[4], env).strip("'\"")
        if not _is_var(sub_sub):
            idx = atom_sub.find(sub_sub)
            if idx >= 0:
                e = env.copy()
                e = _unify(args[1], str(idx), e)
                e = (
                    _unify(args[2], str(len(atom_sub) - idx - len(sub_sub)), e)
                    if e is not None
                    else None
                )
                e = _unify(args[3], str(len(sub_sub)), e) if e is not None else None
                return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    # ── Term inspection ────────────────────────────────────────────────────────
    if pred == "functor" and len(args) == 3:
        t_sub = _substitute_term(args[0], env)
        fm = re.match(r"^(\w+)\s*\((.+)\)$", t_sub)
        if fm:
            name_str = fm.group(1)
            f_args = [a.strip() for a in fm.group(2).split(",")]
            arity = str(len(f_args))
        else:
            name_str = t_sub
            arity = "0"
        e = _unify(args[1], name_str, env.copy())
        e = _unify(args[2], arity, e) if e is not None else None
        return _solve_goals_cut(kb, rest, e) if e is not None else []

    if pred == "arg" and len(args) == 3:
        n_sub = _substitute_term(args[0], env)
        t_sub = _substitute_term(args[1], env)
        fm = re.match(r"^\w+\s*\((.+)\)$", t_sub)
        if fm:
            f_args = [a.strip() for a in fm.group(1).split(",")]
            try:
                idx = int(n_sub) - 1
                e = _unify(args[2], f_args[idx], env.copy())
                return _solve_goals_cut(kb, rest, e) if e is not None else []
            except (ValueError, IndexError):
                pass
        return []

    if pred == "copy_term" and len(args) == 2:
        t_sub = _substitute_term(args[0], env)
        import random as _rand

        suffix = str(_rand.randint(10000, 99999))
        renamed = _rename_vars_in_term(t_sub, suffix)
        e = _unify(args[1], renamed, env.copy())
        return _solve_goals_cut(kb, rest, e) if e is not None else []

    # ── format/2 ─────────────────────────────────────────────────────────────
    if pred in ("format", "print") and len(args) >= 1:
        fmt = _substitute_term(args[0], env).strip("\"'")
        fmt_result = (
            fmt.replace("~w", "{}")
            .replace("~d", "{}")
            .replace("~a", "{}")
            .replace("~n", "\n")
            .replace("~N", "\n")
        )
        insert_vals = [_substitute_term(a, env).strip("\"'") for a in args[1:]]
        try:
            output = fmt_result.format(*insert_vals)
        except (IndexError, KeyError):
            output = fmt_result
        if interpreter:
            for line in output.splitlines():
                interpreter.output.append(line)
        return _solve_goals_cut(kb, rest, env)

    if pred == "tab" and len(args) == 1:
        n_sub = _substitute_term(args[0], env)
        try:
            n = int(float(n_sub))
            if interpreter:
                interpreter.output.append(" " * n)
        except (ValueError, TypeError):
            pass
        return _solve_goals_cut(kb, rest, env)

    # ── Control predicates ─────────────────────────────────────────────────────
    if pred == "true":
        return _solve_goals_cut(kb, rest, env)
    if pred in ("fail", "false"):
        return []
    if pred == "halt":
        return []
    if pred == "once" and len(args) == 1:
        inner_goals = _parse_body_goals(args[0])
        solutions = _solve_goals(kb, inner_goals, env.copy())
        if solutions:
            return _solve_goals_cut(kb, rest, solutions[0])
        return []
    if pred == "ignore" and len(args) == 1:
        inner_goals = _parse_body_goals(args[0])
        _solve_goals(kb, inner_goals, env.copy())  # ignore result
        return _solve_goals_cut(kb, rest, env)
    if pred in ("call", "\\+") or (pred == "not" and len(args) == 1):
        if pred in ("call",) and args:
            inner_goals = _parse_body_goals(args[0])
            return _solve_goals_cut(kb, _parse_body_goals(args[0]) + list(rest), env)
        if pred in ("not", "\\+"):
            inner_goals = _parse_body_goals(args[0])
            solutions = _solve_goals(kb, inner_goals, env.copy())
            return _solve_goals_cut(kb, rest, env) if not solutions else []
        return []
    if pred == "forall" and len(args) == 2:
        cond = _parse_body_goals(args[0])
        action = _parse_body_goals(args[1])
        cond_sols = _solve_goals(kb, cond, env.copy())
        for sol in cond_sols:
            if not _solve_goals(kb, action, sol):
                return []
        return _solve_goals_cut(kb, rest, env)

    # ── Arithmetic shortcuts (abs/max/min as predicates) ──────────────────────
    if pred == "succ_or_zero" and len(args) == 2:
        a_v = _num_value(args[0], env)
        if a_v is not None:
            e = _unify(args[1], str(max(0, int(a_v) - 1)), env.copy())
            return _solve_goals_cut(kb, rest, e) if e is not None else []
        return []

    # ── String number conversion ───────────────────────────────────────────────
    if pred == "term_to_atom" and len(args) == 2:
        t_sub = _substitute_term(args[0], env)
        e = _unify(args[1], f"'{t_sub}'", env.copy())
        return _solve_goals_cut(kb, rest, e) if e is not None else []

    # Regular predicate: resolve all matches and respect cut propagation
    prune_here = False

    # Substitute args with current env
    subbed_args = tuple(_substitute_term(a, env) for a in args)

    for env1 in _solve(kb, pred, subbed_args):
        merged = env.copy()
        merged.update(env1)
        child = _solve_goals_cut(kb, rest, merged)
        # Append only successful envs; track cut flags including cut-fail
        any_cut = False
        for e, c in child:
            if c:
                any_cut = True
            if "__CUTFAIL__" in e:
                # Do not keep as a solution; only use to trigger pruning
                continue
            out.append((e, c))
        if any_cut or kb.get("cut_active"):
            prune_here = True
            break
    if prune_here:
        # Propagate cut upward to caller to stop its alternatives
        out = [(e, True) for (e, _c) in out]
        if not out:
            # No successful solutions but cut committed:
            # return cut-fail sentinel
            return [({"__CUTFAIL__": "1"}, True)]
    return out


def _rename_vars_in_term(term: str, suffix: str) -> str:
    # Don't rename variables inside quoted strings
    stripped = term.strip()
    if (stripped.startswith("'") and stripped.endswith("'")) or (
        stripped.startswith('"') and stripped.endswith('"')
    ):
        return term

    def replace(match):
        v = match.group(0)
        if v == "_":
            return "_"
        return v + "_" + suffix

    return re.sub(r"\b[A-Z][a-zA-Z0-9_]*\b", replace, term)


def _rename_vars_in_rule(hargs, body_goals, suffix):
    new_hargs = tuple(_rename_vars_in_term(a, suffix) for a in hargs)
    new_body = []
    for pred, args in body_goals:
        new_args = tuple(_rename_vars_in_term(a, suffix) for a in args)
        new_body.append((pred, new_args))
    return new_hargs, new_body


def _solve(
    kb: Dict[str, Any], pred: str, args: Tuple[str, ...]
) -> List[Dict[str, str]]:
    solutions: List[Dict[str, str]] = []
    # Built-in predicates for direct queries and rule heads
    builtins = {"add", "lt", "gt", "ge", "le", "eq", "neq"}
    if pred in builtins:
        envs_with_cut = _solve_goals_cut(kb, [(pred, args)], {})
        for e, c in envs_with_cut:
            if "__CUTFAIL__" in e:
                continue
            solutions.append(e)
        return solutions
    # Facts
    for p, fact_args in kb["facts"]:
        if p != pred or len(fact_args) != len(args):
            continue
        fact_env: Optional[Dict[str, str]] = {}
        for a, b in zip(args, fact_args):
            fact_env = _unify(a, b, fact_env) if fact_env is not None else None
            if fact_env is None:
                break
        if fact_env is not None:
            solutions.append(fact_env)

    # Rules (multi-goal body)
    for hp, hargs, body_goals in kb["rules"]:
        if hp != pred or len(hargs) != len(args):
            continue

        # Rename variables to avoid collision in recursion
        if "var_counter" not in kb:
            kb["var_counter"] = 0
        kb["var_counter"] += 1
        suffix = str(kb["var_counter"])
        hargs_renamed, body_goals_renamed = _rename_vars_in_rule(
            hargs, body_goals, suffix
        )

        rule_env: Optional[Dict[str, str]] = {}
        for ha, qa in zip(hargs_renamed, args):
            if rule_env is not None:
                rule_env = _unify(ha, qa, rule_env)
            else:
                rule_env = None
            if rule_env is None:
                break
        if rule_env is None:
            continue
        if _body_has_cut(body_goals_renamed):
            pre, post = _split_on_cut(body_goals_renamed)
            # Get first solution for pre-cut part only
            first_envs = _solve_goals_first(kb, pre, rule_env)
            if not first_envs:
                # Cut not reached; allow other clauses
                continue
            # Cut reached: evaluate post goals; do not backtrack over pre
            post_envs = _solve_goals(kb, post, first_envs[0])
            for e in post_envs:
                solutions.append(e)
            # Cut commits: stop exploring other clauses
            break
        # Evaluate body goals sequentially without pre-binding
        # (sequential unification)
        envs_with_cut = _solve_goals_cut(kb, body_goals_renamed, rule_env)
        cut_committed = any(c for (e, c) in envs_with_cut) or kb.get(
            "cut_active", False
        )  # type: ignore
        for e, c in envs_with_cut:
            if "__CUTFAIL__" in e:
                continue
            solutions.append(e)
        if cut_committed:
            break

    return solutions


def execute_prolog(interpreter: "Interpreter", command: str, turtle: "TurtleState") -> str:
    """Execute Prolog language command."""
    raw_cmd = command.strip()

    # Handle single-line % comments
    if raw_cmd.startswith("%"):
        return ""

    # Strip inline % comments (not inside quotes)
    pct_idx = raw_cmd.find("%")
    if pct_idx > 0:
        # Simple check: not inside quotes
        in_quote = False
        for i, ch in enumerate(raw_cmd[:pct_idx]):
            if ch == "'":
                in_quote = not in_quote
        if not in_quote:
            raw_cmd = raw_cmd[:pct_idx].strip()

    # Strip inline /* ... */ block comments
    while "/*" in raw_cmd and "*/" in raw_cmd:
        start = raw_cmd.index("/*")
        end = raw_cmd.index("*/", start + 2)
        raw_cmd = (raw_cmd[:start] + raw_cmd[end + 2:]).strip()

    # Track multi-line block comments
    _ensure_kb(interpreter)
    if interpreter.prolog_kb.get("_in_block_comment", False):
        if "*/" in raw_cmd:
            raw_cmd = raw_cmd[raw_cmd.index("*/") + 2:].strip()
            interpreter.prolog_kb["_in_block_comment"] = False
            if not raw_cmd:
                return ""
        else:
            return ""  # still inside block comment
    if "/*" in raw_cmd:
        # Block comment starts but doesn't end on this line
        if "*/" not in raw_cmd:
            interpreter.prolog_kb["_in_block_comment"] = True
            raw_cmd = raw_cmd[:raw_cmd.index("/*")].strip()
            if not raw_cmd:
                return ""

    if not raw_cmd:
        return ""

    # Section handling
    upper_cmd = raw_cmd.upper()
    if upper_cmd in ("DOMAINS", "PREDICATES", "CLAUSES", "GOAL"):
        interpreter.prolog_kb["section"] = upper_cmd
        interpreter.prolog_kb["buffer"] = ""
        return ""

    section = interpreter.prolog_kb.get("section", "NONE")

    # If in DOMAINS or PREDICATES, just ignore for now (or validate)
    if section in ("DOMAINS", "PREDICATES"):
        return ""

    # Append to buffer
    interpreter.prolog_kb["buffer"] += " " + raw_cmd

    # Check if we have a complete statement (ends with .)
    full_cmd = interpreter.prolog_kb["buffer"].strip()

    if not full_cmd.endswith("."):
        return ""

    # Clear buffer for next command
    interpreter.prolog_kb["buffer"] = ""
    cmd = full_cmd[:-1].strip()  # Remove trailing dot

    # Pre-processing for infix operators
    cmd = re.sub(r"([a-zA-Z0-9_]+)\s*\\=\s*([a-zA-Z0-9_]+)", r"neq(\1, \2)", cmd)

    # GOAL section execution
    if section == "GOAL":
        # Treat as a query without ?- prefix
        goals = _parse_body_goals(cmd)
        interpreter.prolog_kb["cut_active"] = False
        sols = _solve_goals(interpreter.prolog_kb, goals, {})

        # In Turbo Prolog, GOAL usually just runs and prints output via write/writeln
        # But if there are variables, we might want to show them?
        # Turbo Prolog usually doesn't show bindings for the GOAL section
        # unless explicitly printed.
        # But for compatibility with our interactive shell, let's show bindings
        # if any.

        if not sols:
            return "❌ false"

        var_names = sorted(
            list(
                set(
                    re.findall(
                        r"\b[A-Z][a-zA-Z0-9_]*\b",
                        cmd,
                    )
                )
            )
        )
        if not var_names:
            return "✅ true"

        lines: List[str] = []
        for env in sols:
            assigns = []
            for v in var_names:
                if v in env:
                    val = _substitute_term(v, env)
                    assigns.append(f"{v}={val}")
            if assigns:
                lines.append("✅ " + ", ".join(assigns))
        return "\n".join(lines) if lines else "✅ true"

    # Interactive Query (?- ...)
    if cmd.startswith("?-"):
        body = cmd[2:].strip()
        goals = _parse_body_goals(body)
        interpreter.prolog_kb["cut_active"] = False
        sols = _solve_goals(interpreter.prolog_kb, goals, {})

        if not sols:
            return "❌ false"

        var_names = sorted(list(set(re.findall(r"\b[A-Z][a-zA-Z0-9_]*\b", body))))
        if not var_names:
            return "✅ true"

        lines = []
        for env in sols:
            assigns = []
            for v in var_names:
                if v in env:
                    val = _substitute_term(v, env)
                    assigns.append(f"{v}={val}")
            if assigns:
                lines.append("✅ " + ", ".join(assigns))
        return "\n".join(lines) if lines else "✅ true"

    # Rule (Head :- Body)
    # We need to check for :- inside the string, but be careful about strings/parens
    # Simple heuristic: if it matches _RULE_RE
    # We need to put the dot back for the regexes if they expect it?
    # The original regexes expected the dot.
    # Let's adjust the regexes or put the dot back.

    cmd_with_dot = cmd + "."

    # Directive: :- Goal.  (execute goal immediately)
    if cmd.startswith(":-"):
        directive_body = cmd[2:].strip()
        if directive_body:
            # Handle :- dynamic pred/arity declarations (no-op for our interpreter)
            if re.match(r"^dynamic\b", directive_body, re.IGNORECASE):
                return ""  # accept but ignore dynamic declarations
            # Handle :- discontiguous, :- module, :- use_module, etc.
            if re.match(r"^(discontiguous|module|use_module|ensure_loaded|style_check)\b",
                        directive_body, re.IGNORECASE):
                return ""  # accept but ignore these directives
            goals = _parse_body_goals(directive_body)
            interpreter.prolog_kb["cut_active"] = False
            sols = _solve_goals(interpreter.prolog_kb, goals, {})
            return ""  # directives produce side-effects via write/nl, no return val

    m = _RULE_RE.match(cmd_with_dot)
    if m:
        hp, hargs, body = m.groups()
        interpreter.prolog_kb["rules"].append(
            (
                hp.lower(),
                _parse_terms(hargs),
                _parse_body_goals(body),
            )
        )
        return ""

    # Zero-arity rule: name :- body.
    m = _RULE0_RE.match(cmd_with_dot)
    if m:
        hp, body = m.groups()
        interpreter.prolog_kb["rules"].append(
            (
                hp.lower(),
                [],
                _parse_body_goals(body),
            )
        )
        return ""

    # Fact
    m = _FACT_RE.match(cmd_with_dot)
    if m:
        pred, args = m.groups()
        key = (pred.lower(), _parse_terms(args))
        if key not in interpreter.prolog_kb["facts"]:
            interpreter.prolog_kb["facts"].append(key)
        return ""

    # Zero-arity fact: name.
    m = _FACT0_RE.match(cmd_with_dot)
    if m:
        pred = m.group(1)
        key = (pred.lower(), [])
        if key not in interpreter.prolog_kb["facts"]:
            interpreter.prolog_kb["facts"].append(key)
        return ""

    return f"❌ Error: Unknown Prolog statement '{command.strip()}'"
