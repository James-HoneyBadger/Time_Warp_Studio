"""JCL (Job Control Language) language executor for Time Warp Studio.

Educational IBM mainframe JCL interpreter.
Simulates OS/390 / z/OS JCL job processing:

  //JOBNAME  JOB  (accounting),'description',CLASS=A,MSGCLASS=X
  //STEPNAME EXEC PGM=program[,PARM='params']
  //STEPNAME EXEC PROC=procname
  //DDNAME   DD   DSN=dataset,DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(10,5))
  //DDNAME   DD   *              (inline data, end with /*)
  //DDNAME   DD   SYSOUT=*       (spool output)
  //*                            (comment)
  //         DD   (continuation)
  /*                             (end of inline data)
  // SET     NAME=value          (symbolic parameter assignment)
  // IF      (cond) THEN         (conditional execution)
  // ELSE
  // ENDIF
  // JCLLIB  ORDER=(lib1,lib2)   (procedure library search order)
  //name     PROC [params]       (inline procedure definition)
  //         PEND                 (end inline procedure)
  // INCLUDE MEMBER=name         (include member simulation)

Built-in program simulation (PGM=):
  IEBGENER   — copy input DD to output DD
  IEFBR14    — no-op (return code 0)
  IDCAMS     — VSAM/DEFINE control statements
  SORT       — simulate sort with SORT FIELDS, RECORD definitions
  COBTEST    — run COBOL programs (calls COBOL executor)
  IKJEFT01   — TSO/REXX batch (calls REXX executor if SOURCE DD present)
  IEBCOPY    — dataset copy simulation
  IEHPROGM   — dataset management simulation
  USER-PGM   — any user program name → echo its PARM
"""

from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any, Dict, List, Optional

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_jcl(
    interpreter: "Interpreter",
    source: str,
    turtle: "TurtleState",
) -> str:
    """Execute a JCL job stream and return spool output."""
    try:
        env = JCLEnvironment(interpreter, turtle)
        return env.run(source)
    except RecursionError:
        return "❌ JCL error: Maximum recursion depth exceeded\n"
    except MemoryError:
        return "❌ JCL error: Out of memory\n"
    except Exception as exc:
        return f"❌ JCL error: {exc}\n"


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------


class _JCLStatement:
    """Parsed JCL statement."""

    def __init__(self, name: str, oper: str, params: str, raw: str):
        self.name = name  # //NAME part (may be empty)
        self.oper = oper  # JOB / EXEC / DD / * / blank
        self.params = params  # everything after the operand keyword
        self.raw = raw

    def __repr__(self) -> str:
        return f"<JCL {self.name!r} {self.oper!r}>"


# ---------------------------------------------------------------------------
# Parser
# ---------------------------------------------------------------------------

_CONT_RE = re.compile(r"^//\s")  # continuation line
_STMT_RE = re.compile(r"^//(\S*)\s+(\S+)(?:\s+(.*))?$")  # //NAME OPER [params]
_COMMENT_RE = re.compile(r"^//\*")
_EOD_RE = re.compile(r"^/\*")  # end of inline data
_BLANK_RE = re.compile(r"^//\s*$")  # blank JCL line


def _parse_params(params_str: str) -> Dict[str, str]:
    """Very light-weight JCL keyword=value parser."""
    result: Dict[str, str] = {}
    if not params_str:
        return result
    # Strip leading positional (accounting) parms in parens
    s = params_str.strip()
    if s.startswith("("):
        depth = 0
        i = 0
        for i, c in enumerate(s):
            if c == "(":
                depth += 1
            elif c == ")":
                depth -= 1
                if depth == 0:
                    break
        s = s[i + 1 :].lstrip(",").strip()
    for tok in re.split(r",(?![^(]*\))", s):
        tok = tok.strip()
        if "=" in tok:
            k, v = tok.split("=", 1)
            result[k.upper()] = v.strip("()")
        elif tok:
            result[tok.upper()] = ""
    return result


def _parse_jcl(source: str) -> List[_JCLStatement]:
    """Parse JCL source into a list of statements (handles continuation)."""
    raw_lines: List[str] = source.splitlines()
    stmts: List[_JCLStatement] = []
    assembled = ""
    in_inline = False
    inline_dd: Optional[str] = None

    for line in raw_lines:
        # Inside inline data section
        if in_inline:
            if _EOD_RE.match(line) or _BLANK_RE.match(line):
                in_inline = False
                # Store inline data as synthetic statement
                stmts.append(
                    _JCLStatement(
                        inline_dd or "INLINE", "_DATA_", assembled.rstrip(), assembled
                    )
                )
                assembled = ""
                inline_dd = None
            else:
                assembled += line + "\n"
            continue

        # Skip lines shorter than 2 chars
        if len(line) < 2:
            continue

        if not line.startswith("//"):
            # Data record outside inline data block (unusual but possible)
            continue

        if _COMMENT_RE.match(line):
            continue

        # Continuation line: append to previous assembled
        if assembled and _CONT_RE.match(line):
            assembled = assembled.rstrip() + " " + line[2:].lstrip()
            continue

        # Flush previous assembled statement
        if assembled:
            _emit_stmt(assembled, stmts)
            assembled = ""

        assembled = line

    if assembled:
        _emit_stmt(assembled, stmts)

    return stmts


def _emit_stmt(line: str, stmts: List[_JCLStatement]) -> None:
    m = _STMT_RE.match(line)
    if m:
        name = m.group(1)
        oper = m.group(2).upper()
        params = (m.group(3) or "").strip()
        # Recognize conditionals and control statements
        if oper in ("IF", "ELSE", "ENDIF", "SET", "JCLLIB", "INCLUDE", "PROC", "PEND"):
            stmts.append(_JCLStatement(name, oper, params, line))
        else:
            stmts.append(_JCLStatement(name, oper, params, line))
    else:
        stmts.append(_JCLStatement("", "UNKNOWN", line, line))


# ---------------------------------------------------------------------------
# Job execution environment
# ---------------------------------------------------------------------------


class JCLEnvironment:
    """Simulates OS/390 job execution."""

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._spool: List[str] = []
        self._dds: Dict[str, Any] = {}  # DD name → data or "SYSOUT"
        self._inline: Dict[str, str] = {}  # DD name → inline data
        self._return_codes: Dict[str, int] = {}  # step → RC
        self._job_name = "TWJOB001"
        self._job_desc = "Time Warp JCL Job"
        self._symbols: Dict[str, str] = {}  # SET symbolic parameters
        self._procs: Dict[str, List[_JCLStatement]] = {}  # inline PROC definitions
        self._jcllib: List[str] = []  # JCLLIB ORDER library list

    # ------------------------------------------------------------------ output

    def _emit(self, text: str) -> None:
        self._spool.append(str(text))

    def _sysout(self, text: str) -> None:
        self._spool.append(text)

    # ------------------------------------------------------------------ main

    def run(self, source: str) -> str:
        if not source or not source.strip():
            return ""
        stmts = _parse_jcl(source)
        if not stmts:
            return "❌ JCL error: no valid JCL statements found.\n"

        # Must start with JOB card
        first = stmts[0]
        if first.oper != "JOB":
            return "❌ JCL error: first statement must be a JOB card.\n"

        self._job_name = first.name or "NOJOBNAM"
        params = _parse_params(first.params)
        self._job_desc = params.get("'", self._job_name)

        self._emit("JES2 JOB LOG -- SYSTEM TWRP -- NODE TIMEWARP")
        self._emit(f"-------- {datetime.now().strftime('%a %b %d %H:%M:%S')} --------")
        self._emit(f"$HASP373 {self._job_name} STARTED")
        self._emit(f"IEF403I {self._job_name} - STARTED")
        self._emit("")

        # Process steps
        i = 1
        while i < len(stmts):
            stmt = stmts[i]

            # SET symbolic parameter
            if stmt.oper == "SET":
                m_set = re.match(r"(\w+)\s*=\s*(.+)", stmt.params)
                if m_set:
                    self._symbols[m_set.group(1).upper()] = m_set.group(2).strip()
                i += 1
                continue

            # JCLLIB ORDER
            if stmt.oper == "JCLLIB":
                m_lib = re.match(r"ORDER\s*=\s*\(([^)]+)\)", stmt.params, re.IGNORECASE)
                if m_lib:
                    self._jcllib = [l.strip() for l in m_lib.group(1).split(",")]
                    self._emit(f"IEF212I JCLLIB ORDER: {', '.join(self._jcllib)}")
                i += 1
                continue

            # INCLUDE MEMBER
            if stmt.oper == "INCLUDE":
                m_inc = re.match(r"MEMBER\s*=\s*(\w+)", stmt.params, re.IGNORECASE)
                if m_inc:
                    self._emit(f"IEF236I INCLUDE MEMBER={m_inc.group(1)} PROCESSED")
                i += 1
                continue

            # Inline PROC definition
            if stmt.oper == "PROC":
                proc_name = stmt.name.upper()
                proc_body: List[_JCLStatement] = []
                i += 1
                while i < len(stmts) and stmts[i].oper != "PEND":
                    proc_body.append(stmts[i])
                    i += 1
                if i < len(stmts):
                    i += 1  # skip PEND
                self._procs[proc_name] = proc_body
                self._emit(f"IEF236I PROC {proc_name} CATALOGED ({len(proc_body)} statements)")
                continue

            # IF/THEN/ELSE/ENDIF conditional execution
            if stmt.oper == "IF":
                cond_result = self._eval_jcl_condition(stmt.params)
                # Find matching ELSE/ENDIF
                then_stmts, else_stmts, end_idx = self._scan_if_block(stmts, i + 1)
                if cond_result:
                    self._emit(f"IEF272I IF CONDITION TRUE — executing THEN path")
                    for ts in then_stmts:
                        if ts.oper == "EXEC":
                            step_end = self._find_step_end(stmts, stmts.index(ts) + 1)
                            dd_s = [s for s in stmts[stmts.index(ts) + 1:step_end] if s.oper in ("DD", "_DATA_")]
                            rc = self._run_step(ts, dd_s, stmts)
                            self._return_codes[ts.name] = rc
                else:
                    self._emit(f"IEF272I IF CONDITION FALSE — executing ELSE path")
                    for es in else_stmts:
                        if es.oper == "EXEC":
                            step_end = self._find_step_end(stmts, stmts.index(es) + 1)
                            dd_s = [s for s in stmts[stmts.index(es) + 1:step_end] if s.oper in ("DD", "_DATA_")]
                            rc = self._run_step(es, dd_s, stmts)
                            self._return_codes[es.name] = rc
                i = end_idx
                continue

            if stmt.oper == "EXEC":
                # Resolve symbolic parameters in params
                resolved_params = self._resolve_symbols(stmt.params)
                resolved_stmt = _JCLStatement(stmt.name, stmt.oper, resolved_params, stmt.raw)
                step_end = self._find_step_end(stmts, i + 1)
                dd_stmts = stmts[i + 1 : step_end]
                rc = self._run_step(resolved_stmt, dd_stmts, stmts)
                self._return_codes[stmt.name] = rc
                i = step_end
            elif stmt.oper == "_DATA_":
                i += 1
            else:
                i += 1

        # Job epilog
        self._emit("")
        self._emit(f"IEF404I {self._job_name} - ENDED")
        worst_rc = max(self._return_codes.values()) if self._return_codes else 0
        self._emit(f"$HASP395 {self._job_name} ENDED  MAXCC={worst_rc:04d}")
        if worst_rc == 0:
            self._emit(f"IEF142I {self._job_name} STEP - NORMAL END")
        elif worst_rc <= 4:
            self._emit(
                f"IEF142I {self._job_name} - COMPLETED WITH WARNINGS (CC={worst_rc:04d})"
            )
        else:
            self._emit(f"IEF142I {self._job_name} - ABENDED  CC={worst_rc:04d}")
        return "\n".join(self._spool)

    # ------------------------------------------------------------------ step

    def _find_step_end(self, stmts: List[_JCLStatement], start: int) -> int:
        """Return index of the next EXEC statement or end of list."""
        for i in range(start, len(stmts)):
            if stmts[i].oper == "EXEC":
                return i
        return len(stmts)

    def _run_step(
        self,
        exec_stmt: _JCLStatement,
        dd_stmts: List[_JCLStatement],
        all_stmts: List[_JCLStatement],
    ) -> int:
        """Execute one JCL EXEC step. Returns return code."""
        params = _parse_params(exec_stmt.params)
        pgm = params.get("PGM", "")
        proc = params.get("PROC", "")
        parm = params.get("PARM", "")
        step_name = exec_stmt.name or "STEP0001"

        # Collect DD allocations for this step
        step_dds: Dict[str, str] = {}
        for dd in dd_stmts:
            if dd.oper in ("DD", "_DATA_"):
                step_dds[dd.name.upper()] = dd.params

        self._emit(f"IEF236I ALLOC FOR {self._job_name} {step_name}")
        for dd_name in step_dds:
            self._emit(f"IEF237I {dd_name:8s} ALLOCATED")
        self._emit("")

        rc = 0
        pgm_upper = pgm.upper()
        if pgm_upper == "IEFBR14":
            rc = self._pgm_iefbr14()
        elif pgm_upper == "IEBGENER":
            rc = self._pgm_iebgener(step_dds, all_stmts)
        elif pgm_upper == "IDCAMS":
            rc = self._pgm_idcams(step_dds, all_stmts)
        elif pgm_upper == "SORT":
            rc = self._pgm_sort(step_dds, all_stmts)
        elif pgm_upper in ("COBTEST", "COBOL"):
            rc = self._pgm_cobol(step_dds, all_stmts, parm)
        elif pgm_upper == "IKJEFT01":
            rc = self._pgm_ikjeft01(step_dds, all_stmts, parm)
        elif proc:
            # Check inline PROC catalog first
            if proc.upper() in self._procs:
                proc_body = self._procs[proc.upper()]
                self._emit(f"IEF385I   {proc:8s} -- EXECUTING INLINE PROCEDURE")
                for ps in proc_body:
                    if ps.oper == "EXEC":
                        inner_rc = self._run_step(ps, [], all_stmts)
                        rc = max(rc, inner_rc)
            else:
                self._emit(f"IEF385I   {proc:8s} -- PROCEDURE NOT FOUND IN CATALOG")
                rc = 4
        else:
            self._emit(f"IEF285I   {pgm:8s} -- PROGRAM EXECUTED  PARM='{parm}'")
            rc = 0

        self._emit(
            f"IEF142I {self._job_name} {step_name} - STEP WAS EXECUTED - COND CODE {rc:04d}"
        )
        self._emit("")
        return rc

    # ------------------------------------------------------------------ built-in PGMs

    def _pgm_iefbr14(self) -> int:
        self._emit("IEFBR14 - No operation performed.")
        return 0

    def _pgm_iebgener(
        self,
        step_dds: Dict[str, str],
        all_stmts: List[_JCLStatement],
    ) -> int:
        """Copy SYSUT1 → SYSUT2."""
        data = self._get_inline_data("SYSUT1", all_stmts)
        if data:
            self._sysout("-- IEBGENER SYSUT2 OUTPUT --")
            self._sysout(data)
            self._emit(f"IEBGENER - {len(data.splitlines())} record(s) generated.")
            return 0
        self._emit("IEBGENER - SYSUT1 data not found; 0 records copied.")
        return 0

    def _pgm_idcams(
        self,
        step_dds: Dict[str, str],
        all_stmts: List[_JCLStatement],
    ) -> int:
        """Simulate IDCAMS DEFINE/DELETE commands."""
        data = self._get_inline_data("SYSIN", all_stmts)
        lines = (data or "").splitlines()
        for line in lines:
            upper = line.strip().upper()
            if upper.startswith("DEFINE "):
                self._emit(f"IDC0001I DEFINE COMMAND PROCESSED: {line.strip()}")
            elif upper.startswith("DELETE "):
                self._emit(f"IDC0002I DELETE COMMAND PROCESSED: {line.strip()}")
            elif upper.startswith("PRINT "):
                self._emit(f"IDC3001I PRINT COMMAND PROCESSED: {line.strip()}")
            elif upper.startswith("LIST "):
                self._emit("IDC3501I LIST COMMAND PROCESSED")
            elif upper and not upper.startswith("/*") and not upper.startswith("//"):
                self._emit(f"IDC0005I COMMAND: {line.strip()}")
        return 0

    def _pgm_sort(
        self,
        step_dds: Dict[str, str],
        all_stmts: List[_JCLStatement],
    ) -> int:
        """Simulate DFSORT/SYNCSORT."""
        data = self._get_inline_data("SYSIN", all_stmts)
        in_data = self._get_inline_data("SORTIN", all_stmts)
        lines = (data or "").splitlines()
        self._emit("SORT PROGRAM ENTERED")
        for ctrl in lines:
            self._emit(f"  SORT CONTROL: {ctrl.strip()}")
        if in_data:
            sorted_lines = sorted(in_data.splitlines())
            self._sysout("-- SORT OUTPUT (SORTOUT) --")
            for l in sorted_lines:
                self._sysout(l)
            self._emit(f"SORT COMPLETED OK — {len(sorted_lines)} records sorted.")
        else:
            self._emit("SORT COMPLETED OK — No SORTIN data provided.")
        return 0

    def _pgm_cobol(
        self,
        step_dds: Dict[str, str],
        all_stmts: List[_JCLStatement],
        parm: str,
    ) -> int:
        """Invoke the COBOL executor against SOURCE DD inline data."""
        from ..languages.cobol import execute_cobol

        data = self._get_inline_data("SOURCE", all_stmts) or self._get_inline_data(
            "SYSIN", all_stmts
        )
        if data:
            result = execute_cobol(self.interpreter, data, self.turtle)
            self._sysout(result)
            return 0
        self._emit("COBTEST — No SOURCE DD found.")
        return 8

    def _pgm_ikjeft01(
        self,
        step_dds: Dict[str, str],
        all_stmts: List[_JCLStatement],
        parm: str,
    ) -> int:
        """Invoke the REXX executor against SYSTSIN or REXXSRC inline data."""
        from ..languages.rexx import execute_rexx

        data = self._get_inline_data("SYSTSIN", all_stmts) or self._get_inline_data(
            "REXXSRC", all_stmts
        )
        if data:
            result = execute_rexx(self.interpreter, data, self.turtle)
            self._sysout(result)
            return 0
        self._emit("IKJEFT01 — No SYSTSIN/REXXSRC DD found.")
        return 8

    # ------------------------------------------------------------------ helpers

    def _resolve_symbols(self, text: str) -> str:
        """Replace &SYMBOL references with their SET values."""
        def repl(m):
            sym = m.group(1).upper()
            return self._symbols.get(sym, m.group(0))
        return re.sub(r"&(\w+)\.?", repl, text)

    def _eval_jcl_condition(self, params: str) -> bool:
        """Evaluate a JCL IF condition like (STEP1.RC = 0) THEN."""
        cond = re.sub(r"\bTHEN\b", "", params, flags=re.IGNORECASE).strip()
        cond = cond.strip("()")

        # step.RC comparison
        m = re.match(r"(\w+)\.RC\s*(=|<>|<|>|<=|>=|NE|EQ|LT|GT|LE|GE)\s*(\d+)", cond, re.IGNORECASE)
        if m:
            step = m.group(1).upper()
            op = m.group(2).upper()
            val = int(m.group(3))
            rc = self._return_codes.get(step, 0)
            ops = {"=": rc == val, "EQ": rc == val, "<>": rc != val, "NE": rc != val,
                   "<": rc < val, "LT": rc < val, ">": rc > val, "GT": rc > val,
                   "<=": rc <= val, "LE": rc <= val, ">=": rc >= val, "GE": rc >= val}
            return ops.get(op, False)

        # ABEND check
        if re.search(r"ABEND\b", cond, re.IGNORECASE):
            return False  # no abends in simulation

        # Default: true
        return True

    def _scan_if_block(
        self, stmts: List[_JCLStatement], start: int
    ) -> tuple[List[_JCLStatement], List[_JCLStatement], int]:
        """Scan IF/ELSE/ENDIF block. Returns (then_stmts, else_stmts, endif_idx+1)."""
        then_stmts: List[_JCLStatement] = []
        else_stmts: List[_JCLStatement] = []
        in_else = False
        depth = 0
        i = start
        while i < len(stmts):
            s = stmts[i]
            if s.oper == "IF":
                depth += 1
            elif s.oper == "ENDIF":
                if depth == 0:
                    return then_stmts, else_stmts, i + 1
                depth -= 1
            elif s.oper == "ELSE" and depth == 0:
                in_else = True
                i += 1
                continue
            if in_else:
                else_stmts.append(s)
            else:
                then_stmts.append(s)
            i += 1
        return then_stmts, else_stmts, len(stmts)

    def _get_inline_data(
        self,
        dd_name: str,
        all_stmts: List[_JCLStatement],
    ) -> Optional[str]:
        """Find _DATA_ synthetic statement for a DD name."""
        for s in all_stmts:
            if s.oper == "_DATA_" and s.name.upper() == dd_name.upper():
                return s.params  # stored as params
        return None


# Avoid circular import — datetime must be top-level
from datetime import datetime  # noqa: E402
