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
    return bool(token) and token[0].isalpha() and token[0].isupper()


def _parse_terms(arg_str: str) -> Tuple[str, ...]:
    parts = [p.strip() for p in arg_str.split(",") if p.strip()]
    return tuple(parts)


def _ensure_kb(interpreter: "Interpreter"):
    if not hasattr(interpreter, "prolog_kb"):
        # Use list to preserve insertion order for deterministic behavior
        kb_dict = {"facts": [], "rules": [], "cut_active": False}
        interpreter.prolog_kb = kb_dict  # type: ignore
    # Ensure required keys exist even if
    # prolog_kb was pre-initialized as an empty dict
    if "facts" not in interpreter.prolog_kb:
        interpreter.prolog_kb["facts"] = []
    if "rules" not in interpreter.prolog_kb:
        interpreter.prolog_kb["rules"] = []
    if "cut_active" not in interpreter.prolog_kb:
        interpreter.prolog_kb["cut_active"] = False  # type: ignore


def _unify(x: str, y: str, env: Dict[str, str]) -> Optional[Dict[str, str]]:
    # Both variables: do not bind variable-to-variable; treat as placeholder
    if _is_var(x) and _is_var(y):
        # If either already bound, ensure consistency
        if x in env and y in env:
            return env if env[x] == env[y] else None
        if x in env and y not in env:
            # y remains unbound, shares x's bound value implicitly
            # in later unifications
            return env
        if y in env and x not in env:
            return env  # x remains unbound
        # Neither bound: leave both free
        return env
    # Variable vs ground term
    if _is_var(x) and not _is_var(y):
        if x in env:
            return env if env[x] == y else None
        e = env.copy()
        e[x] = y
        return e
    if _is_var(y) and not _is_var(x):
        if y in env:
            return env if env[y] == x else None
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
        if token in env and re.fullmatch(r"-?\d+(?:\.\d+)?", env[token]):
            return float(env[token])
        return None
    if re.fullmatch(r"-?\d+(?:\.\d+)?", token):
        return float(token)
    return None


def _bind_num(
    token: str, value: float, env: Dict[str, str]
) -> Optional[Dict[str, str]]:
    if _is_var(token):
        e = env.copy()
        # Store as integer if whole
        if abs(value - int(value)) < 1e-9:
            e[token] = str(int(value))
        else:
            e[token] = str(value)
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

    # Regular predicate: resolve all matches and respect cut propagation
    prune_here = False
    for env1 in _solve(kb, pred, args):
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
        rule_env: Optional[Dict[str, str]] = {}
        for ha, qa in zip(hargs, args):
            if rule_env is not None:
                rule_env = _unify(ha, qa, rule_env)
            else:
                rule_env = None
            if rule_env is None:
                break
        if rule_env is None:
            continue
        if _body_has_cut(body_goals):
            pre, post = _split_on_cut(body_goals)
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
        envs_with_cut = _solve_goals_cut(kb, body_goals, rule_env)
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
    cmd = command.strip()
    if not cmd:
        return ""

    _ensure_kb(interpreter)

    # Query
    m = _QUERY_RE.match(cmd)
    if m:
        pred, args = m.groups()
        terms = _parse_terms(args)
        # Reset cut state for each query
        interpreter.prolog_kb["cut_active"] = False  # type: ignore
        sols = _solve(interpreter.prolog_kb, pred.lower(), terms)
        if not sols:
            return "❌ false"
        has_vars = any(_is_var(t) for t in terms)
        if not has_vars:
            return "✅ true"
        lines: List[str] = []
        var_names = [t for t in terms if _is_var(t)]
        for env in sols:
            assigns = []
            for v in var_names:
                if v in env:
                    assigns.append(f"{v}={env[v]}")
            if assigns:
                lines.append("✅ " + ", ".join(assigns))
        return "\n".join(lines) if lines else "✅ true"

    # Rule
    m = _RULE_RE.match(cmd)
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
    m = _FACT_RE.match(cmd)
    if m:
        pred, args = m.groups()
        key = (pred.lower(), _parse_terms(args))
        if key not in interpreter.prolog_kb["facts"]:
            interpreter.prolog_kb["facts"].append(key)
        return ""

    # Comments
    if cmd.startswith("%"):
        return ""

    return f"❌ Error: Unknown Prolog statement '{command.strip()}'"
