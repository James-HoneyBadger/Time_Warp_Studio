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
- No lists/structures
- Queries limited to a single predicate (no multi-goal query syntax)
"""

from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter


_FACT_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*\(([^)]*)\)\s*\.\s*$")
_RULE_PART1 = r"^\s*([a-z][a-z0-9_]*)\s*"
_RULE_PART2 = r"\(([^)]*)\)\s*:-\s*(.+)\s*\.\s*$"
_RULE_PATTERN = _RULE_PART1 + _RULE_PART2
_RULE_RE = re.compile(_RULE_PATTERN)
_QUERY_RE = re.compile(r"^\s*\?-\s*([a-z][a-z0-9_]*)\s*\(([^)]*)\)\s*\.\s*$")


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

        env = _unify(xh, yh, env)
        if env is None:
            return None
        return _unify(xt, yt, env)

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

    m = re.match(r"^([a-z][a-z0-9_]*)\s*\(([^)]*)\)\s*$", t)
    if not m:
        # Treat bare atom as predicate with arity 0
        return (t.lower(), tuple())
    return (m.group(1).lower(), _parse_terms(m.group(2)))


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
    """Evaluate simple math expression with variables."""
    # Replace variables with values
    # Sort keys by length descending to avoid partial replacements
    # Tokenize to avoid replacing substrings of other words
    # Simple approach: split by non-alphanumeric

    # Better approach: use regex to find variables
    def replace_var(match):
        var_name = match.group(0)
        if var_name in env:
            val = env[var_name]
            if re.fullmatch(r"-?\d+(?:\.\d+)?", val):
                return val
        return var_name

    # Replace variables that look like Prolog vars (Capitalized)
    expr_sub = re.sub(r"\b[A-Z][a-zA-Z0-9_]*\b", replace_var, expr)

    # Handle 'mod' operator (python uses %)
    expr_sub = re.sub(r"\bmod\b", "%", expr_sub)

    try:
        # Safe evaluation: only allow numbers and operators
        if not re.match(r"^[\d\s+\-*/%().]+$", expr_sub):
            return None
        return float(eval(expr_sub))
    except Exception:
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
            val = _substitute_term(arg, env)
            # Strip quotes if it's a string literal
            if val.startswith('"') and val.endswith('"'):
                val = val[1:-1]
            text_parts.append(str(val))

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
            val = _substitute_term(arg, env)
            if val.startswith('"') and val.endswith('"'):
                val = val[1:-1]
            text_parts.append(str(val))
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


def execute_prolog(interpreter: "Interpreter", command: str, _turtle) -> str:
    """Execute Prolog language command."""
    raw_cmd = command.strip()

    # Handle comments immediately if they are the only thing on the line
    if raw_cmd.startswith("%") or raw_cmd.startswith("/*"):
        return ""

    if not raw_cmd:
        return ""

    _ensure_kb(interpreter)

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
        # Turbo Prolog usually doesn't show bindings for the GOAL section unless explicitly printed.
        # But for compatibility with our interactive shell, let's show bindings
        # if any.

        if not sols:
            return "❌ false"

        var_names = sorted(list(set(re.findall(r"\b[A-Z][a-zA-Z0-9_]*\b", cmd))))
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

    # Rule (Head :- Body)
    # We need to check for :- inside the string, but be careful about strings/parens
    # Simple heuristic: if it matches _RULE_RE
    # We need to put the dot back for the regexes if they expect it?
    # The original regexes expected the dot.
    # Let's adjust the regexes or put the dot back.

    cmd_with_dot = cmd + "."

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

    # Fact
    m = _FACT_RE.match(cmd_with_dot)
    if m:
        pred, args = m.groups()
        key = (pred.lower(), _parse_terms(args))
        if key not in interpreter.prolog_kb["facts"]:
            interpreter.prolog_kb["facts"].append(key)
        return ""

    return f"❌ Error: Unknown Prolog statement '{command.strip()}'"
