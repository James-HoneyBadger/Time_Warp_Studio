"""SQR (Structured Query Reporter) language executor for Time Warp Studio.

SQR is a report-writing and data-processing language developed by Mosaix/
PeopleSoft (now Oracle PeopleSoft).  It combines procedural logic with
embedded SQL and a formatted-report engine.

Supported language features
============================
Program structure
  begin-program / end-program
  begin-procedure <name> / end-procedure
  begin-heading <n> / end-heading
  begin-footer <n> / end-footer
  begin-report / end-report (alias for program body)
  begin-sql / end-sql
  begin-select / end-select  (SQL query with column binding)
  begin-setup / end-setup
  #include 'file'   (acknowledged but not loaded)

Variables
  $string_var   – string variable (prefixed $)
  #numeric_var  – numeric variable (prefixed #)
  &column_var   – SQL column binding (prefixed &, also read as variable)
  Built-ins:  #page-count  #current-line  #current-column  #rowcount
              $current-date  $sql-error  $sqr-program

Assignment
  let #var = <expr>
  let $var = <expr>
  move <val> to #var / $var
  string $a ' ' $b into $c

Output / formatting
  print <val> (at <col> <line>) (fill)
  display <val>
  show <val>
  do print-line   (emit a newline in the report)
  next-listing
  new-page
  position (<col>, <line>)
  columns <list>

Control flow
  if <cond> / else / end-if
  while <cond> / end-while
  evaluate <val>
    when <val2>
    when-other
  end-evaluate
  break
  do <procedure-name> [(args)]
  call <procedure-name>
  exit
  stop

Arithmetic / string expressions
  Operators:  + - * / ** mod  for numbers
  String concat with  |  or  ||
  Comparison:  =  <>  !=  <  >  <=  >=  (and / or / not)

String functions
  uppercase($s) lowercase($s) ltrim($s) rtrim($s) trim($s)
  length($s)  strlen($s)
  substr($s,start,len)  substring($s,start,len)
  concat($a,$b)  instr($s,sub)  find($s,sub)
  ltrim($s,char)  rtrim($s,char)
  edit(n,'format')
  lpad($s,n)  rpad($s,n)
  replace($s,old,new)

Math functions
  round(n,d)  trunc(n,d)  floor(n)  ceiling(n)  abs(n)
  sqrt(n)  power(b,e)  mod(a,b)  sign(n)  min(a,b)  max(a,b)
  to-number($s)  number($s)

Date functions
  date-time()  strtodate($s,'fmt')  datetostr(d,'fmt')
  datenow()  today()
  add-to-date(d,unit,n)  extract(unit from d)

SQL helpers (simulated)
  begin-select ... from <table> <cond>
    (<column-list>)
  end-select          – loops over in-memory table rows
  begin-sql / end-sql – DML/DDL on the in-memory SQL engine

Reporting
  Number(n,'mask')   – formatted numeric output
  page-number        – current page counter
  print-line         – emit newline
"""

from __future__ import annotations

import datetime
import math
import re
import warnings
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState  # noqa: F401


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------


def execute_sqr(
    interpreter: "Interpreter",
    source: str,
    turtle: "TurtleState",
) -> str:
    """Execute a complete SQR program and return formatted report output."""
    import sys

    old_limit = sys.getrecursionlimit()
    sys.setrecursionlimit(5000)
    try:
        env = SQREnvironment(interpreter)
        return env.run(source)
    finally:
        sys.setrecursionlimit(old_limit)


# ---------------------------------------------------------------------------
# Helper: tokenise a simple expression
# ---------------------------------------------------------------------------

_TOKEN_RE = re.compile(
    r'"(?:[^"\\]|\\.)*"'  # double-quoted string
    r"|'(?:[^'\\]|\\.)*'"  # single-quoted string
    r"|[<>!]=|[<>]"  # two-char comparisons first
    r"|[+\-*/%|(),]"  # single operators / punct
    r"|\*\*"  # power
    r"|[a-zA-Z_#$&][a-zA-Z0-9_\-#$&]*"  # identifiers / variables
    r"|[0-9]+(?:\.[0-9]*)?",  # numbers
    re.ASCII,
)


def _tokenise(expr: str) -> List[str]:
    return _TOKEN_RE.findall(expr.strip())


# ---------------------------------------------------------------------------
# The SQR execution environment
# ---------------------------------------------------------------------------


class SQREnvironment:
    """Full SQR interpreter state and execution logic."""

    # ------------------------------------------------------------------ init
    def __init__(self, interpreter: "Interpreter") -> None:
        self._interp = interpreter
        self._out: List[str] = []  # accumulated report output
        self._report_buf: List[str] = []  # current page buffer
        self._vars: Dict[str, Any] = {}  # $strings and #numerics
        self._procs: Dict[str, List[str]] = {}  # procedure name -> lines
        self._tables: Dict[str, List[Dict[str, Any]]] = {}  # SQL table simulator
        self._page: int = 1
        self._line: int = 1
        self._col: int = 1
        self._rowcount: int = 0
        self._break: bool = False
        self._exit: bool = False
        self._heading_lines: int = 0
        self._heading_src: List[str] = []
        self._footer_lines: int = 0
        self._footer_src: List[str] = []
        # Built-in variables
        self._vars["$current-date"] = datetime.datetime.now().strftime("%Y-%m-%d")
        self._vars["$sqr-program"] = "TIME-WARP-SQR"
        self._vars["$sql-error"] = ""
        self._vars["#page-count"] = 1.0
        self._vars["#current-line"] = 1.0
        self._vars["#current-column"] = 1.0
        self._vars["#rowcount"] = 0.0
        self._vars["#sql-count"] = 0.0

    # ------------------------------------------------------------------ run
    def run(self, source: str) -> str:
        """Parse and execute the full SQR source."""
        lines = source.splitlines()
        self._first_pass(lines)  # collect procedure definitions
        self._execute_body(lines)
        return self._build_output()

    # ---- first pass: extract procedure definitions -------------------------
    def _first_pass(self, lines: List[str]) -> None:
        i = 0
        while i < len(lines):
            stripped = lines[i].strip().lower()
            if stripped.startswith("begin-procedure"):
                name = self._proc_name(lines[i])
                body: List[str] = []
                i += 1
                depth = 1
                while i < len(lines) and depth > 0:
                    s = lines[i].strip().lower()
                    if s.startswith("begin-procedure"):
                        depth += 1
                    elif s.startswith("end-procedure"):
                        depth -= 1
                        if depth == 0:
                            break
                    body.append(lines[i])
                    i += 1
                self._procs[name] = body
            i += 1

    @staticmethod
    def _proc_name(line: str) -> str:
        parts = line.strip().split()
        if len(parts) >= 2:
            return parts[1].lower().strip("()")
        return "unnamed"

    # ---- main body execution -----------------------------------------------
    def _execute_body(self, lines: List[str]) -> None:
        in_main = False
        # Find begin-program or begin-report
        i = 0
        while i < len(lines):
            s = lines[i].strip().lower()
            if s.startswith("begin-program") or s.startswith("begin-report"):
                in_main = True
                i += 1
                body: List[str] = []
                while i < len(lines):
                    s2 = lines[i].strip().lower()
                    if s2.startswith("end-program") or s2.startswith("end-report"):
                        break
                    body.append(lines[i])
                    i += 1
                self._exec_block(body)
                return
            elif s.startswith("begin-setup"):
                # skip setup block
                while i < len(lines) and "end-setup" not in lines[i].strip().lower():
                    i += 1
            elif s.startswith("begin-heading"):
                depth = 1
                i += 1
                while i < len(lines) and depth > 0:
                    sh = lines[i].strip().lower()
                    if sh.startswith("begin-heading"):
                        depth += 1
                    elif sh.startswith("end-heading"):
                        depth -= 1
                    else:
                        self._heading_src.append(lines[i])
                    i += 1
            elif s.startswith("begin-footer"):
                depth = 1
                i += 1
                while i < len(lines) and depth > 0:
                    sf = lines[i].strip().lower()
                    if sf.startswith("begin-footer"):
                        depth += 1
                    elif sf.startswith("end-footer"):
                        depth -= 1
                    else:
                        self._footer_src.append(lines[i])
                    i += 1
            elif s.startswith("begin-procedure"):
                # skip – already parsed
                while i < len(lines) and not lines[i].strip().lower().startswith(
                    "end-procedure"
                ):
                    i += 1
            i += 1
        # If no begin-program found, execute all non-procedure lines directly
        if not in_main:
            body = []
            i = 0
            while i < len(lines):
                s = lines[i].strip().lower()
                if s.startswith("begin-procedure") or s.startswith("end-procedure"):
                    pass
                elif not any(
                    s.startswith(kw)
                    for kw in (
                        "begin-heading",
                        "end-heading",
                        "begin-footer",
                        "end-footer",
                        "begin-setup",
                        "end-setup",
                    )
                ):
                    body.append(lines[i])
                i += 1
            self._exec_block(body)

    # ---- block executor ----------------------------------------------------
    def _exec_block(self, lines: List[str]) -> None:
        """Execute a list of source lines (body of a procedure or main)."""
        i = 0
        while i < len(lines):
            if self._exit or self._break:
                break
            line = lines[i]
            stripped = line.strip()
            if not stripped or stripped.startswith("!") or stripped.startswith(";"):
                i += 1
                continue
            s_lower = stripped.lower()

            # ---- begin-select block (inline SQL loop) ----
            if s_lower.startswith("begin-select"):
                i = self._exec_begin_select(lines, i)
                continue

            # ---- begin-sql block -----
            if s_lower.startswith("begin-sql"):
                i = self._exec_begin_sql(lines, i)
                continue

            # ---- if / else / end-if -----
            if s_lower.startswith("if ") or s_lower == "if":
                i = self._exec_if(lines, i)
                continue

            # ---- while / end-while -----
            if s_lower.startswith("while "):
                i = self._exec_while(lines, i)
                continue

            # ---- evaluate / when / end-evaluate -----
            if s_lower.startswith("evaluate "):
                i = self._exec_evaluate(lines, i)
                continue

            # ---- create-array (field= definitions may be on the next line) -----
            if s_lower.startswith("create-array"):
                stmt = stripped
                j = i + 1
                while j < len(lines):
                    next_s = lines[j].strip()
                    # Continuation if every token on the line is a field= clause
                    if next_s and all(
                        tok.lower().startswith("field=") for tok in next_s.split()
                    ):
                        stmt += " " + next_s
                        j += 1
                    else:
                        break
                self._exec_create_array(stmt)
                i = j
                continue

            # ---- simple statement -----
            self._exec_stmt(stripped)
            i += 1

    # ---- if block ---------------------------------------------------------
    def _exec_if(self, lines: List[str], start: int) -> int:
        """Execute an if/else/end-if block. Returns index after end-if."""
        cond_str = lines[start].strip()[3:].strip()
        condition_result = self._eval_condition(cond_str)

        # Collect true-branch and false-branch lines
        true_branch: List[str] = []
        false_branch: List[str] = []
        in_else = False
        depth = 1
        i = start + 1
        while i < len(lines) and depth > 0:
            s = lines[i].strip().lower()
            if s.startswith("if ") or s == "if":
                depth += 1
                (false_branch if in_else else true_branch).append(lines[i])
            elif s == "end-if":
                depth -= 1
                if depth == 0:
                    break
                (false_branch if in_else else true_branch).append(lines[i])
            elif s == "else" and depth == 1:
                in_else = True
            else:
                (false_branch if in_else else true_branch).append(lines[i])
            i += 1

        if condition_result:
            self._exec_block(true_branch)
        else:
            self._exec_block(false_branch)
        return i + 1

    # ---- while block -------------------------------------------------------
    def _exec_while(self, lines: List[str], start: int) -> int:
        cond_str = lines[start].strip()[6:].strip()
        body: List[str] = []
        depth = 1
        i = start + 1
        while i < len(lines) and depth > 0:
            s = lines[i].strip().lower()
            if s.startswith("while "):
                depth += 1
            elif s == "end-while":
                depth -= 1
                if depth == 0:
                    break
            body.append(lines[i])
            i += 1

        max_iters = 10_000
        iters = 0
        while self._eval_condition(cond_str) and not self._exit and not self._break:
            iters += 1
            if iters > max_iters:
                self._emit("❌ SQR: while loop exceeded iteration limit")
                break
            self._break = False
            self._exec_block(body)
        self._break = False
        return i + 1

    # ---- evaluate block ----------------------------------------------------
    def _exec_evaluate(self, lines: List[str], start: int) -> int:
        eval_val_str = lines[start].strip()[9:].strip()
        eval_val = self._eval_expr(eval_val_str)

        # Collect when-clauses
        clauses: List[Tuple[Optional[str], List[str]]] = []  # (cond, body_lines)
        current_cond: Optional[str] = None
        current_body: List[str] = []
        i = start + 1
        depth = 1
        while i < len(lines) and depth > 0:
            s = lines[i].strip().lower()
            if s.startswith("evaluate "):
                depth += 1
            elif s == "end-evaluate":
                depth -= 1
                if depth == 0:
                    if current_cond is not None:
                        clauses.append((current_cond, current_body))
                    break
            elif (s.startswith("when ") or s == "when-other") and depth == 1:
                if current_cond is not None:
                    clauses.append((current_cond, current_body))
                current_cond = (
                    None if s == "when-other" else lines[i].strip()[5:].strip()
                )
                current_body = []
            else:
                current_body.append(lines[i])
            i += 1

        for cond, body in clauses:
            if cond is None:  # when-other
                self._exec_block(body)
                break
            when_val = self._eval_expr(cond)
            match = False
            if isinstance(eval_val, (int, float)) and isinstance(
                when_val, (int, float)
            ):
                match = eval_val == when_val
            else:
                match = str(eval_val).strip().lower() == str(when_val).strip().lower()
            if match:
                self._exec_block(body)
                break
        return i + 1

    # ---- begin-select block -----------------------------------------------
    def _exec_begin_select(self, lines: List[str], start: int) -> int:
        """Simulate a begin-select … end-select SQL loop."""
        # Collect lines until end-select
        select_lines: List[str] = []
        i = start + 1
        while i < len(lines):
            s = lines[i].strip().lower()
            if s == "end-select":
                break
            select_lines.append(lines[i])
            i += 1

        # Separate column-binding lines (in parentheses) from procedural body
        # Column bindings look like:  (&col1, &col2, ...)  or  &col  per line
        col_bindings: List[str] = []
        from_line: Optional[str] = None
        body_lines: List[str] = []
        in_from = False
        for sl in select_lines:
            stripped = sl.strip()
            if stripped.startswith("(") and not in_from and not from_line:
                # column binding
                col_bindings.append(stripped.strip("()"))
                continue
            if stripped.startswith("&") and not in_from and not from_line:
                col_bindings.append(stripped)
                continue
            sl_lower = stripped.lower()
            if sl_lower.startswith("from ") and not in_from:
                from_line = stripped
                in_from = True
                continue
            if in_from and (
                sl_lower.startswith("where ")
                or sl_lower.startswith("order ")
                or sl_lower.startswith("group ")
                or sl_lower.startswith("having ")
            ):
                if from_line:
                    from_line += " " + stripped
                continue
            body_lines.append(sl)

        # Parse column names from bindings
        col_names: List[str] = []
        for cb in col_bindings:
            for name in re.split(r"[,\s]+", cb):
                name = name.strip().lstrip("&")
                if name:
                    col_names.append(name)

        # Identify table name from FROM clause
        table_name: Optional[str] = None
        where_clause: Optional[str] = None
        if from_line:
            m = re.match(r"from\s+(\S+)(?:\s+where\s+(.+))?", from_line, re.IGNORECASE)
            if m:
                table_name = m.group(1).lower()
                where_clause = m.group(2)

        # Get rows from simulated table store
        rows = self._tables.get(table_name or "", [])
        if not rows and table_name:
            # Try to find in sql_engine if available
            try:
                if hasattr(self._interp, "sql_session") and self._interp.sql_session:
                    pass  # future: query real session
            except Exception:
                pass

        self._rowcount = 0
        self._vars["#rowcount"] = 0.0
        for row in rows:
            # Bind columns
            for col in col_names:
                val = row.get(col.lower(), row.get(col, ""))
                if isinstance(val, (int, float)):
                    self._vars[f"#{col}"] = float(val)
                else:
                    self._vars[f"&{col.lower()}"] = str(val)
            self._rowcount += 1
            self._vars["#rowcount"] = float(self._rowcount)
            self._exec_block(body_lines)
            if self._exit or self._break:
                break
        self._break = False
        return i + 1

    # ---- begin-sql block --------------------------------------------------
    def _exec_begin_sql(self, lines: List[str], start: int) -> int:
        sql_lines: List[str] = []
        i = start + 1
        while i < len(lines):
            s = lines[i].strip().lower()
            if s == "end-sql":
                break
            sql_lines.append(lines[i])
            i += 1
        sql_text = " ".join(sql_lines).strip()
        self._exec_sql(sql_text)
        return i + 1

    # ---- execute SQL on internal engine or simulate -----------------------
    def _exec_sql(self, sql: str) -> None:
        if not sql:
            return
        try:
            if hasattr(self._interp, "sql_session") and self._interp.sql_session:
                result = self._interp.sql_session.run_script(sql)
                if result.strip():
                    self._emit(result)
                return
        except Exception as exc:
            self._vars["$sql-error"] = str(exc)

        # Minimal in-memory table simulator for SQR demos
        sql_upper = sql.strip().upper()
        m_create = re.match(
            r"CREATE\s+TABLE\s+(\w+)\s*\((.+)\)", sql_upper, re.IGNORECASE | re.DOTALL
        )
        if m_create:
            tname = m_create.group(1).lower()
            self._tables.setdefault(tname, [])
            self._emit(f"ℹ️  Table {tname} created")
            return

        m_insert = re.match(
            r"INSERT\s+INTO\s+(\w+)\s*\(([^)]+)\)\s*VALUES\s*\(([^)]+)\)",
            sql,
            re.IGNORECASE,
        )
        if m_insert:
            tname = m_insert.group(1).lower()
            cols = [c.strip().lower() for c in m_insert.group(2).split(",")]
            raw_vals = m_insert.group(3).split(",")
            vals: List[Any] = []
            for v in raw_vals:
                v = v.strip().strip("'\"")
                try:
                    vals.append(float(v) if "." in v else int(v))
                except ValueError:
                    vals.append(v)
            row = dict(zip(cols, vals))
            self._tables.setdefault(tname, []).append(row)
            return

        m_select = re.match(
            r"SELECT\b(.+)\bFROM\b\s*(\w+)", sql, re.IGNORECASE | re.DOTALL
        )
        if m_select:
            tname = m_select.group(2).strip().lower()
            rows = self._tables.get(tname, [])
            self._rowcount = len(rows)
            self._vars["#rowcount"] = float(self._rowcount)
            if rows:
                headers = list(rows[0].keys())
                self._emit(" | ".join(h.upper() for h in headers))
                self._emit("-" * 40)
                for row in rows:
                    self._emit(" | ".join(str(row.get(h, "")) for h in headers))
            else:
                self._emit(f"(0 rows from {tname})")

    # ---- single statement executor ----------------------------------------
    def _exec_stmt(self, stmt: str) -> None:
        """Dispatch a single SQR statement."""
        s = stmt.strip()
        if not s or s.startswith("!") or s.startswith(";"):
            return

        sl = s.lower()

        # Comments
        if sl.startswith("!") or sl.startswith("#include"):
            return

        # exit / stop
        if sl in ("exit", "stop", "exit program", "stop program"):
            self._exit = True
            return

        # break
        if sl == "break":
            self._break = True
            return

        # next-listing
        if sl in ("next-listing", "new-page"):
            self._emit(f"\f─── Page {self._page} ───")
            self._page += 1
            self._vars["#page-count"] = float(self._page)
            return

        # print-line / next-line
        if sl in ("print-line", "next-line", ""):
            self._emit("")
            return

        # display <expr>  /  show <expr>
        if sl.startswith("display ") or sl.startswith("show "):
            val = self._eval_expr(s.split(None, 1)[1].strip())
            self._emit(str(val))
            return

        # print <expr> [at <col> <line>] [fill] [bold] [narrow]
        if sl.startswith("print "):
            self._exec_print(s)
            return

        # let #var = expr   /  let $var = expr
        if sl.startswith("let "):
            self._exec_let(s[4:].strip())
            return

        # move <val> to <var>
        if sl.startswith("move "):
            self._exec_move(s[5:].strip())
            return

        # string  $a $b ... into $result
        if sl.startswith("string "):
            self._exec_string_concat(s[7:].strip())
            return

        # input $var 'prompt'  / input #var 'prompt'
        if sl.startswith("input "):
            self._exec_input(s[6:].strip())
            return

        # do <procedure>  / call <procedure>
        if sl.startswith("do ") or sl.startswith("call "):
            proc = s.split(None, 1)[1].strip().lower().strip("()")
            # strip optional args (simplified)
            proc = re.split(r"[\s(]", proc)[0]
            self._call_proc(proc)
            return

        # create-array name=<arr> size=<n> field=<f1> ...
        if sl.startswith("create-array"):
            self._exec_create_array(s)
            return

        # put <val> into <array>(<row>) . <field>
        if sl.startswith("put ") and " into " in sl:
            self._exec_put(s)
            return

        # get <var> from <array>(<row>) . <field>
        if sl.startswith("get ") and " from " in sl:
            self._exec_get(s)
            return

        # position (<col>, <row>)
        if sl.startswith("position"):
            self._exec_position(s)
            return

        # columns <list>
        if sl.startswith("columns "):
            return  # formatting hint, ignored in text output

        # page-number
        if sl.startswith("page-number"):
            self._emit(f"Page {self._page}")
            return

        # date-time() / today()
        if sl in ("date-time()", "datenow()", "today()"):
            now = datetime.datetime.now()
            self._emit(now.strftime("%Y-%m-%d %H:%M:%S"))
            return

        # Bare variable assignment:  #var = expr  or  $var = expr
        if re.match(r"[#$&]\w[\w\-]*\s*=", s):
            self._exec_let(s)
            return

        # Unknown statement – silently skip if empty or comment
        if s:
            # Try to emit nothing but keep going (robust handling)
            pass

    # ---- print statement --------------------------------------------------
    def _exec_print(self, stmt: str) -> None:
        """Handle:  print <val> [at <col> <row>] [fill] [bold] ..."""
        s = stmt[6:].strip()  # strip "print "

        # Extract AT clause
        at_match = re.search(r"\bat\s+(\d+)\s+(\d+)", s, re.IGNORECASE)
        if at_match:
            col = int(at_match.group(1))
            self._col = col
            s = (s[: at_match.start()] + s[at_match.end() :]).strip()

        # Strip modifiers
        for mod in ("fill", "bold", "narrow", "wide", "center", "noline"):
            s = re.sub(r"\b" + mod + r"\b", "", s, flags=re.IGNORECASE)
        s = s.strip()

        val = self._eval_expr(s) if s else ""
        self._emit(str(val))

    # ---- let <var> = <expr> -----------------------------------------------
    def _exec_let(self, rest: str) -> None:
        m = re.match(r"([#$&][\w\-]+)\s*=\s*(.+)", rest, re.DOTALL)
        if not m:
            return
        var_name = m.group(1).lower()
        expr_str = m.group(2).strip()
        val = self._eval_expr(expr_str)
        self._set_var(var_name, val)

    # ---- move <val> to <var> ---------------------------------------------
    def _exec_move(self, rest: str) -> None:
        m = re.match(
            r"(.+?)\s+to\s+([#$&][\w\-]+)\s*$", rest, re.IGNORECASE | re.DOTALL
        )
        if not m:
            return
        val = self._eval_expr(m.group(1).strip())
        var_name = m.group(2).lower()
        self._set_var(var_name, val)

    # ---- string $a $b into $c --------------------------------------------
    def _exec_string_concat(self, rest: str) -> None:
        m = re.match(
            r"(.+?)\s+into\s+([#$&][\w\-]+)\s*$", rest, re.IGNORECASE | re.DOTALL
        )
        if not m:
            return
        parts_str = m.group(1)
        target = m.group(2).lower()
        # Parts may be variables or string literals separated by whitespace
        parts: List[str] = []
        # Tokenise respecting quoted strings
        for tok in re.findall(r'"[^"]*"|\'[^\']*\'|[#$&][\w\-]+|\S+', parts_str):
            parts.append(str(self._eval_expr(tok)))
        self._set_var(target, "".join(parts))

    # ---- input $var 'prompt' ---------------------------------------------
    def _exec_input(self, rest: str) -> None:
        m = re.match(r"([#$&][\w\-]+)\s*(.*)", rest, re.DOTALL)
        if not m:
            return
        var_name = m.group(1).lower()
        prompt_str = m.group(2).strip().strip("'\"")
        # In simulation mode, emit the prompt and assign empty
        if prompt_str:
            self._emit(f"📝 {prompt_str}")
        self._set_var(var_name, "" if var_name.startswith("$") else 0.0)

    # ---- create-array ----------------------------------------------------
    def _exec_create_array(self, stmt: str) -> None:
        m_name = re.search(r"name\s*=\s*(\w+)", stmt, re.IGNORECASE)
        m_size = re.search(r"size\s*=\s*(\d+)", stmt, re.IGNORECASE)
        if not m_name:
            return
        arr_name = m_name.group(1).lower()
        size = int(m_size.group(1)) if m_size else 0
        fields = re.findall(r"field\s*=\s*(\w+)", stmt, re.IGNORECASE)
        self._tables[f"__arr_{arr_name}"] = [
            {f: "" for f in fields} for _ in range(size)
        ]

    # ---- put val into arr(row).field ------------------------------------
    def _exec_put(self, stmt: str) -> None:
        m = re.match(
            r"put\s+(.+?)\s+into\s+(\w+)\s*\(([^)]+)\)\s*[.\-]\s*(\w+)",
            stmt,
            re.IGNORECASE,
        )
        if not m:
            return
        val = self._eval_expr(m.group(1))
        arr = m.group(2).lower()
        idx = int(self._eval_expr(m.group(3).strip()))
        field = m.group(4).lower()
        tbl = self._tables.get(f"__arr_{arr}", [])
        if idx < len(tbl):
            tbl[idx][field] = val

    # ---- get val from arr(row).field ------------------------------------
    def _exec_get(self, stmt: str) -> None:
        m = re.match(
            r"get\s+([#$&][\w\-]+)\s+from\s+(\w+)\s*\(([^)]+)\)\s*[.\-]\s*(\w+)",
            stmt,
            re.IGNORECASE,
        )
        if not m:
            return
        var = m.group(1).lower()
        arr = m.group(2).lower()
        idx = int(self._eval_expr(m.group(3).strip()))
        field = m.group(4).lower()
        tbl = self._tables.get(f"__arr_{arr}", [])
        if idx < len(tbl):
            self._set_var(var, tbl[idx].get(field, ""))

    # ---- position (<col>, <line>) ----------------------------------------
    def _exec_position(self, stmt: str) -> None:
        m = re.search(r"\((\d+)\s*,\s*(\d+)\)", stmt)
        if m:
            self._col = int(m.group(1))
            self._line = int(m.group(2))

    # ---- call procedure --------------------------------------------------
    def _call_proc(self, name: str) -> None:
        if name in self._procs:
            self._exec_block(self._procs[name])
        else:
            self._emit(f"ℹ️  (procedure '{name}' not defined)")

    # ---- variable management ---------------------------------------------
    def _set_var(self, name: str, val: Any) -> None:
        name = name.lower()
        if name.startswith("#"):
            try:
                self._vars[name] = float(val)
            except (ValueError, TypeError):
                self._vars[name] = 0.0
        else:
            self._vars[name] = str(val)

    def _get_var(self, name: str) -> Any:
        name = name.lower()
        return self._vars.get(
            name, "" if name.startswith("$") or name.startswith("&") else 0.0
        )

    # ---- expression evaluator --------------------------------------------
    def _eval_expr(self, expr: str) -> Any:
        """Evaluate a SQR expression (arithmetic, string, function call)."""
        expr = expr.strip()
        if not expr:
            return ""

        # SQR || concat MUST be checked FIRST – before the quoted-string check,
        # because expressions like 'prefix [' || $var || ']' start AND end with '
        # and would be mistakenly treated as a plain string literal otherwise.
        if "||" in expr:
            return self._eval_concat(expr)

        # Quoted string literal (safe now that || is handled above)
        if (expr.startswith('"') and expr.endswith('"')) or (
            expr.startswith("'") and expr.endswith("'")
        ):
            return expr[1:-1]

        # Number literal
        try:
            return float(expr) if "." in expr else int(expr)
        except ValueError:
            pass

        # Variable
        if re.match(r"^[#$&][\w\-]+$", expr):
            return self._get_var(expr)

        # Function calls
        fn_match = re.match(r"^([\w\-]+)\s*\((.*)?\)$", expr, re.DOTALL)
        if fn_match:
            fn = fn_match.group(1).lower()
            args_str = fn_match.group(2) or ""
            return self._eval_function(fn, args_str)

        # Arithmetic / comparison – use safe Python eval with substituted vars
        return self._eval_arithmetic(expr)

    def _eval_concat(self, expr: str) -> str:
        """Evaluate SQR || (pipe-pipe) string concatenation expression."""
        parts = _split_concat(expr)
        return "".join(str(self._eval_expr(p.strip())) for p in parts)

    def _eval_arithmetic(self, expr: str) -> Any:
        """Safely evaluate an arithmetic/comparison expression."""
        # Substitute SQR variables with their values
        subst = expr

        # Replace string/column variables first (longest names first to avoid partial match)
        for var_name in sorted(self._vars.keys(), key=len, reverse=True):
            if var_name.startswith("$") or var_name.startswith("&"):
                val = self._vars[var_name]
                # Use repr for string substitution
                subst = re.sub(
                    re.escape(var_name),
                    repr(str(val)),
                    subst,
                    flags=re.IGNORECASE,
                )
        for var_name in sorted(self._vars.keys(), key=len, reverse=True):
            if var_name.startswith("#"):
                val = self._vars[var_name]
                subst = re.sub(
                    re.escape(var_name),
                    str(val),
                    subst,
                    flags=re.IGNORECASE,
                )

        # Replace SQR operators
        subst = re.sub(r"\bmod\b", "%", subst, flags=re.IGNORECASE)
        subst = re.sub(r"\band\b", " and ", subst, flags=re.IGNORECASE)
        subst = re.sub(r"\bor\b", " or ", subst, flags=re.IGNORECASE)
        subst = re.sub(r"\bnot\b", " not ", subst, flags=re.IGNORECASE)
        subst = subst.replace("<>", "!=")

        try:
            with warnings.catch_warnings():
                warnings.simplefilter("ignore", SyntaxWarning)
                result = eval(subst, {"__builtins__": {}}, {})  # noqa: S307
            return result
        except Exception:
            return subst  # return unevaluated if parse fails

    def _eval_function(self, fn: str, args_str: str) -> Any:
        """Evaluate a built-in SQR function."""
        # Split args (simple comma split, respecting quoted strings)
        args: List[Any] = []
        if args_str.strip():
            for part in _split_args(args_str):
                args.append(self._eval_expr(part.strip()))

        def _str(i: int = 0) -> str:
            return str(args[i]) if i < len(args) else ""

        def _num(i: int = 0) -> float:
            try:
                return float(args[i]) if i < len(args) else 0.0
            except (ValueError, TypeError):
                return 0.0

        match fn:
            # ── String functions ──────────────────────────────────────────
            case "uppercase" | "upper":
                return _str().upper()
            case "lowercase" | "lower":
                return _str().lower()
            case "ltrim":
                return _str().lstrip(_str(1) if len(args) > 1 else None)
            case "rtrim":
                return _str().rstrip(_str(1) if len(args) > 1 else None)
            case "trim":
                return _str().strip()
            case "length" | "strlen" | "len":
                return len(_str())
            case "substr" | "substring":
                s = _str(0)
                start = max(0, int(_num(1)) - 1)  # SQR is 1-based
                length = int(_num(2)) if len(args) > 2 else len(s) - start
                return s[start : start + length]
            case "concat":
                return "".join(str(a) for a in args)
            case "instr" | "find":
                haystack = _str(0)
                needle = _str(1)
                idx = haystack.find(needle)
                return idx + 1 if idx >= 0 else 0  # 1-based
            case "lpad":
                width = int(_num(1))
                return _str().rjust(width)
            case "rpad":
                width = int(_num(1))
                return _str().ljust(width)
            case "replace":
                return _str(0).replace(_str(1), _str(2))
            case "edit":
                # edit(number, 'mask') – simplified: just format the number
                return f"{_num(0):{'' if len(args) < 2 else _str(1)}}"
            case "number" | "to-number":
                try:
                    v = float(_str())
                    return int(v) if v == int(v) else v
                except (ValueError, TypeError):
                    return 0
            case "to-char" | "tochar":
                return str(args[0]) if args else ""
            case "ltrim0":
                return str(args[0]).lstrip("0") or "0"

            # ── Math functions ────────────────────────────────────────────
            case "abs":
                return abs(_num())
            case "sqrt":
                v = _num()
                return math.sqrt(v) if v >= 0 else 0.0
            case "round":
                dp = int(_num(1)) if len(args) > 1 else 0
                return round(_num(), dp)
            case "trunc" | "truncate":
                dp = int(_num(1)) if len(args) > 1 else 0
                factor = 10**dp
                return math.trunc(_num() * factor) / factor
            case "floor":
                return math.floor(_num())
            case "ceiling" | "ceil":
                return math.ceil(_num())
            case "power" | "pow":
                return _num(0) ** _num(1)
            case "mod":
                b = _num(1)
                return _num(0) % b if b != 0 else 0.0
            case "sign":
                v = _num()
                return 1.0 if v > 0 else (-1.0 if v < 0 else 0.0)
            case "min":
                return min(float(a) for a in args)
            case "max":
                return max(float(a) for a in args)
            case "exp":
                return math.exp(_num())
            case "log":
                v = _num()
                return math.log(v) if v > 0 else 0.0
            case "log10":
                v = _num()
                return math.log10(v) if v > 0 else 0.0
            case "sin":
                return math.sin(math.radians(_num()))
            case "cos":
                return math.cos(math.radians(_num()))
            case "tan":
                return math.tan(math.radians(_num()))

            # ── Date functions ────────────────────────────────────────────
            case "date-time" | "datenow":
                return datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            case "today":
                return datetime.date.today().strftime("%Y-%m-%d")
            case "datetostr" | "date-to-str":
                # datetostr(date_val, 'fmt')
                fmt = (
                    _str(1)
                    .replace("YYYY", "%Y")
                    .replace("MM", "%m")
                    .replace("DD", "%d")
                )
                try:
                    d = datetime.datetime.strptime(_str(0), "%Y-%m-%d")
                    return d.strftime(fmt)
                except ValueError:
                    return _str(0)
            case "strtodate" | "str-to-date":
                return _str(0)  # return as string for simplicity
            case "add-to-date":
                try:
                    d = datetime.datetime.strptime(_str(0), "%Y-%m-%d")
                    unit = _str(1).lower()
                    n = int(_num(2))
                    if unit == "year":
                        d = d.replace(year=d.year + n)
                    elif unit == "month":
                        mo = d.month - 1 + n
                        d = d.replace(year=d.year + mo // 12, month=mo % 12 + 1)
                    elif unit == "day":
                        d += datetime.timedelta(days=n)
                    return d.strftime("%Y-%m-%d")
                except Exception:
                    return _str(0)
            case "extract":
                # extract(unit from date)  handled as expression-level
                return _num()

            # ── Formatting ───────────────────────────────────────────────
            case "lzero" | "zerofill":
                width = int(_num(1))
                return str(int(_num())).zfill(width)
            case "format":
                return f"{_num(0):.{int(_num(1))}f}"

            case _:
                return f"[{fn}({args_str})]"

    def _eval_condition(self, cond: str) -> bool:
        """Evaluate a boolean condition string."""
        result = self._eval_arithmetic(cond)
        if isinstance(result, bool):
            return result
        if isinstance(result, (int, float)):
            return result != 0
        if isinstance(result, str):
            return result.strip().lower() not in ("", "false", "0", "no")
        return bool(result)

    # ---- output helpers --------------------------------------------------
    def _emit(self, text: str) -> None:
        self._out.append(text)

    def _build_output(self) -> str:
        banner = "─── SQR Report Output ───"
        lines = [banner] + self._out
        if not self._out:
            lines.append("(No output produced)")
        return "\n".join(lines)


# ---------------------------------------------------------------------------
# Argument splitter respecting quoted strings
# ---------------------------------------------------------------------------


def _split_args(s: str) -> List[str]:
    """Split on commas, but respect quoted strings and nested parens."""
    return _split_by(s, ",")


def _split_concat(s: str) -> List[str]:
    """Split on SQR || concatenation operator, respecting quotes/parens."""
    return _split_by(s, "||")


def _split_by(s: str, sep: str) -> List[str]:
    """Split *s* on *sep*, respecting quoted strings and nested parens."""
    parts: List[str] = []
    depth = 0
    current: List[str] = []
    in_quote: Optional[str] = None
    i = 0
    while i < len(s):
        ch = s[i]
        if in_quote:
            current.append(ch)
            if ch == in_quote:
                in_quote = None
        elif ch in ('"', "'"):
            in_quote = ch
            current.append(ch)
        elif ch == "(":
            depth += 1
            current.append(ch)
        elif ch == ")":
            depth -= 1
            current.append(ch)
        elif depth == 0 and s[i : i + len(sep)] == sep:
            parts.append("".join(current).strip())
            current = []
            i += len(sep)
            continue
        else:
            current.append(ch)
        i += 1
    if current:
        parts.append("".join(current).strip())
    return parts
