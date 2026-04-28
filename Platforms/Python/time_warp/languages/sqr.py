"""
SQR (Structured Query Reporter) executor for Time Warp Studio.

SQR is a mainframe report-generation language used heavily with PeopleSoft.
This executor supports the educational subset needed for learning:
  - begin-program / end-program structure
  - print / display / show  (output)
  - let #var = expr  (numeric variables with # prefix)
  - let $var = 'text'  (string variables with $ prefix)
  - move value to var
  - Arithmetic: +, -, *, /
  - if / else / end-if  (conditionals)
  - while / end-while  (loops)
  - begin-procedure / end-procedure + do name  (procedures)
  - string $a ' ' $b into $c  (concatenation)
  - input $var 'prompt'
  - evaluate var / when value / when-other / end-evaluate  (switch-case)
  - ! line comments
"""
from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

# ---------------------------------------------------------------------------
# Expression evaluator
# ---------------------------------------------------------------------------

def _eval_expr(expr: str, env: Dict[str, Any]) -> Any:
    """Evaluate a simple SQR expression."""
    expr = expr.strip()
    # String literal
    if (expr.startswith("'") and expr.endswith("'")) or \
       (expr.startswith('"') and expr.endswith('"')):
        return expr[1:-1]
    safe: Dict[str, Any] = {"__builtins__": {}, "abs": abs}
    # Inject variables (strip # and $ prefixes for eval)
    for k, v in env.items():
        safe_key = k.lstrip("#$")
        safe[safe_key] = v
        safe[k.replace("#", "_num_").replace("$", "_str_")] = v
    # Replace SQR variable refs in expression
    py_expr = re.sub(r"[#$](\w+)", lambda m: m.group(1), expr)
    try:
        return eval(py_expr, safe)  # noqa: S307
    except Exception:
        return expr


def _eval_cond(cond: str, env: Dict[str, Any]) -> bool:
    """Evaluate a boolean condition."""
    result = _eval_expr(cond, env)
    if isinstance(result, bool):
        return result
    if isinstance(result, (int, float)):
        return bool(result)
    return False


# ---------------------------------------------------------------------------
# Tokeniser helpers
# ---------------------------------------------------------------------------

def _strip_comment(line: str) -> str:
    """Remove ! comment from line."""
    in_q, q_ch = False, ""
    for i, ch in enumerate(line):
        if ch in ("'", '"') and not in_q:
            in_q, q_ch = True, ch
        elif ch == q_ch and in_q:
            in_q = False
        elif ch == "!" and not in_q:
            return line[:i]
    return line


def _parse_string_token(s: str) -> str:
    s = s.strip()
    if (s.startswith("'") and s.endswith("'")) or \
       (s.startswith('"') and s.endswith('"')):
        return s[1:-1]
    return s


# ---------------------------------------------------------------------------
# Interpreter
# ---------------------------------------------------------------------------

class _SQRInterp:
    MAX_STEPS = 100_000

    def __init__(self, input_cb):
        self.env: Dict[str, Any] = {}
        self.output: List[str] = []
        self.input_cb = input_cb
        self.procedures: Dict[str, List[str]] = {}

    # ------------------------------------------------------------------
    def run(self, source: str) -> None:
        lines = self._preprocess(source)
        self._collect_procedures(lines)
        # Find begin-program / end-program block
        start, end_line = self._find_program_block(lines)
        if start is not None:
            self._exec_lines(lines[start:end_line])
        else:
            # No begin-program: try to run everything
            self._exec_lines(lines)

    def _preprocess(self, source: str) -> List[str]:
        """Strip comments and blank lines."""
        result = []
        for raw in source.splitlines():
            cleaned = _strip_comment(raw).strip()
            if cleaned:
                result.append(cleaned)
        return result

    def _find_program_block(self, lines: List[str]) -> Tuple[Optional[int], int]:
        for i, line in enumerate(lines):
            if line.lower().startswith("begin-program"):
                # Find matching end-program
                for j in range(i + 1, len(lines)):
                    if lines[j].lower().startswith("end-program"):
                        return i + 1, j
                return i + 1, len(lines)
        return None, len(lines)

    def _collect_procedures(self, lines: List[str]) -> None:
        """Collect begin-procedure ... end-procedure blocks."""
        i = 0
        while i < len(lines):
            m = re.match(r"begin-procedure\s+(\S+)", lines[i], re.IGNORECASE)
            if m:
                name = m.group(1).lower()
                body = []
                i += 1
                while i < len(lines):
                    if lines[i].lower().startswith("end-procedure"):
                        i += 1
                        break
                    body.append(lines[i])
                    i += 1
                self.procedures[name] = body
            else:
                i += 1

    # ------------------------------------------------------------------
    def _exec_lines(self, lines: List[str], depth: int = 0) -> int:
        """Execute a list of lines. Returns index where execution stopped."""
        ip = 0
        steps = 0
        while ip < len(lines) and steps < self.MAX_STEPS:
            steps += 1
            line = lines[ip]
            lo = line.lower()

            # Skip structural markers
            if lo.startswith("begin-program") or lo.startswith("end-program"):
                ip += 1
                continue

            # Skip procedure definitions
            if lo.startswith("begin-procedure"):
                # Skip to end-procedure
                ip += 1
                while ip < len(lines) and not lines[ip].lower().startswith("end-procedure"):
                    ip += 1
                ip += 1
                continue

            # PRINT / DISPLAY / SHOW
            if lo.startswith(("print ", "print\t", "display ", "display\t",
                               "show ", "show\t")):
                self._exec_output(line)
                ip += 1
                continue

            # LET
            if lo.startswith("let "):
                self._exec_let(line[4:].strip())
                ip += 1
                continue

            # MOVE value TO var
            m = re.match(r"move\s+(.+)\s+to\s+([#$]\w+)", line, re.IGNORECASE)
            if m:
                val = _eval_expr(m.group(1).strip(), self.env)
                self.env[m.group(2)] = val
                ip += 1
                continue

            # INPUT
            m = re.match(r"input\s+([#$]\w+)\s*(.+)?", line, re.IGNORECASE)
            if m:
                prompt = _parse_string_token(m.group(2) or "? ") + " "
                raw = self.input_cb(prompt) if self.input_cb else ""
                var = m.group(1)
                if var.startswith("#"):
                    try:
                        self.env[var] = float(raw) if "." in raw else int(raw)
                    except ValueError:
                        self.env[var] = raw
                else:
                    self.env[var] = raw
                ip += 1
                continue

            # STRING concat: string $a ' ' $b into $c
            m = re.match(r"string\s+(.+)\s+into\s+([#$]\w+)", line, re.IGNORECASE)
            if m:
                parts_raw = m.group(1)
                target = m.group(2)
                # Split on whitespace outside quotes
                tokens = re.findall(r"'[^']*'|\"[^\"]*\"|[^\s]+", parts_raw)
                result = ""
                for tok in tokens:
                    if (tok.startswith("'") and tok.endswith("'")) or \
                       (tok.startswith('"') and tok.endswith('"')):
                        result += tok[1:-1]
                    else:
                        result += str(self.env.get(tok, _eval_expr(tok, self.env)))
                self.env[target] = result
                ip += 1
                continue

            # DO (procedure call)
            m = re.match(r"do\s+(\S+)", line, re.IGNORECASE)
            if m:
                name = m.group(1).lower()
                if name in self.procedures:
                    self._exec_lines(self.procedures[name], depth + 1)
                ip += 1
                continue

            # IF / ELSE / END-IF
            if lo.startswith("if "):
                ip = self._exec_if(lines, ip)
                continue

            # WHILE / END-WHILE
            if lo.startswith("while "):
                ip = self._exec_while(lines, ip)
                continue

            # EVALUATE / WHEN / END-EVALUATE
            if lo.startswith("evaluate "):
                ip = self._exec_evaluate(lines, ip)
                continue

            # END-IF / END-WHILE / END-EVALUATE / ELSE (handled by parent)
            if lo.startswith(("end-if", "end-while", "end-evaluate", "else")):
                return ip

            ip += 1
        return ip

    def _exec_output(self, line: str) -> None:
        """Handle print/display/show."""
        m = re.match(r"(?:print|display|show)\s+(.*)", line, re.IGNORECASE)
        if not m:
            self.output.append("")
            return
        rest = m.group(1).strip()
        # print 'text' ()  — strip trailing ()
        rest = re.sub(r"\s*\(\s*\)\s*$", "", rest)
        val = _eval_expr(rest, self.env)
        self.output.append(str(val))

    def _exec_let(self, stmt: str) -> None:
        """let #var = expr or let $var = 'string'"""
        m = re.match(r"([#$]\w+)\s*=\s*(.+)", stmt)
        if m:
            var = m.group(1)
            val = _eval_expr(m.group(2).strip(), self.env)
            if var.startswith("#") and isinstance(val, str):
                try:
                    val = float(val) if "." in val else int(val)
                except (ValueError, TypeError):
                    pass
            self.env[var] = val

    def _exec_if(self, lines: List[str], ip: int) -> int:
        """if cond ... [else ...] end-if"""
        cond_str = re.sub(r"^if\s+", "", lines[ip], flags=re.IGNORECASE).strip()
        cond_val = _eval_cond(cond_str, self.env)
        ip += 1

        then_lines: List[str] = []
        else_lines: List[str] = []
        depth, in_else = 0, False
        while ip < len(lines):
            s = lines[ip]
            slo = s.lower()
            if slo.startswith("if "):
                depth += 1
            if depth == 0:
                if slo.startswith("else"):
                    in_else = True
                    ip += 1
                    continue
                if slo.startswith("end-if"):
                    ip += 1
                    break
            if depth > 0 and slo.startswith("end-if"):
                depth -= 1
            if in_else:
                else_lines.append(s)
            else:
                then_lines.append(s)
            ip += 1

        block = then_lines if cond_val else else_lines
        if block:
            self._exec_lines(block)
        return ip

    def _exec_while(self, lines: List[str], ip: int) -> int:
        """while cond ... end-while"""
        cond_str = re.sub(r"^while\s+", "", lines[ip], flags=re.IGNORECASE).strip()
        ip += 1

        body: List[str] = []
        depth = 0
        while ip < len(lines):
            s = lines[ip]
            slo = s.lower()
            if slo.startswith("while "):
                depth += 1
            if depth == 0 and slo.startswith("end-while"):
                ip += 1
                break
            if depth > 0 and slo.startswith("end-while"):
                depth -= 1
            body.append(s)
            ip += 1

        iterations = 0
        while _eval_cond(cond_str, self.env) and iterations < 10_000:
            iterations += 1
            self._exec_lines(body)
        return ip

    def _exec_evaluate(self, lines: List[str], ip: int) -> int:
        """evaluate var / when value / when-other / end-evaluate"""
        subj_str = re.sub(r"^evaluate\s+", "", lines[ip], flags=re.IGNORECASE).strip()
        subj_val = _eval_expr(subj_str, self.env)
        ip += 1

        matched = False
        current_when_val = None
        when_block: List[str] = []
        other_block: List[str] = []
        in_when, in_other = False, False

        while ip < len(lines):
            s = lines[ip]
            slo = s.lower()
            if slo.startswith("end-evaluate"):
                ip += 1
                break
            m = re.match(r"when-other", slo)
            if m:
                # Check the pending when block before switching to other
                if in_when and not matched and current_when_val is not None:
                    when_val = _eval_expr(current_when_val, self.env)
                    if when_val == subj_val:
                        matched = True
                        self._exec_lines(when_block)
                in_when, in_other = False, True
                ip += 1
                continue
            m = re.match(r"when\s+(.+)", s, re.IGNORECASE)
            if m:
                if in_when and not matched:
                    when_val = _eval_expr(current_when_val, self.env)
                    if when_val == subj_val:
                        matched = True
                        self._exec_lines(when_block)
                in_when = True
                current_when_val = m.group(1).strip()
                when_block = []
                in_other = False
                ip += 1
                continue
            if in_when:
                when_block.append(s)
            elif in_other:
                other_block.append(s)
            ip += 1

        # Check last when block
        if in_when and not matched and current_when_val is not None:
            when_val = _eval_expr(current_when_val, self.env)
            if when_val == subj_val:
                matched = True
                self._exec_lines(when_block)

        if not matched and other_block:
            self._exec_lines(other_block)

        return ip


# ---------------------------------------------------------------------------
# Public executor
# ---------------------------------------------------------------------------

def execute_sqr(interpreter: "Interpreter", source: str, turtle: "TurtleState") -> str:
    """Execute an SQR program and return output text."""
    if not source.strip():
        return ""
    interp = _SQRInterp(input_cb=getattr(interpreter, "input_callback", None))
    try:
        interp.run(source)
    except Exception as exc:  # noqa: BLE001
        interp.output.append(f"❌ Runtime error: {exc}")
    return "\n".join(interp.output) + ("\n" if interp.output else "")
