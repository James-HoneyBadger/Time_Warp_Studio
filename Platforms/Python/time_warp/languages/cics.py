"""CICS (Customer Information Control System) language executor for Time Warp Studio.

Simulates IBM CICS/TS transaction execution.
CICS programs are written in COBOL, PL/I, or Assembler with embedded
EXEC CICS commands.  This executor accepts a simplified CICS pseudo-COBOL
dialect and processes EXEC CICS ... END-EXEC statements.

Supported EXEC CICS commands:
  SEND TEXT        FROM(data) LENGTH(n) ERASE
  SEND MAP         MAPNAME FROM(data) MAPSET(set)
  RECEIVE          INTO(var) LENGTH(len)
  RECEIVE MAP      MAPNAME MAPSET(set)
  READ             FILE(name) INTO(var) RIDFLD(key)
  READNEXT         FILE(name) INTO(var) RIDFLD(key)
  WRITE            FILE(name) FROM(var) RIDFLD(key)
  REWRITE          FILE(name) FROM(var)
  DELETE           FILE(name) RIDFLD(key)
  LINK             PROGRAM(name) COMMAREA(area)
  XCTL             PROGRAM(name) COMMAREA(area)
  RETURN           [TRANSID(id)] [COMMAREA(area)]
  HANDLE CONDITION condname(label)
  IGNORE CONDITION condname
  ABEND            [ABCODE(code)]
  ASSIGN           TERMINAL(term) USERID(user) SYSID(sys)
  GETMAIN          SET(ptr) LENGTH(n)
  FREEMAIN         DATA(ptr)
  ENQ / DEQ        RESOURCE(name)
  SYNCPOINT
  DELAY            INTERVAL(hhmmss)
  SET              (various)

Pseudo-COBOL data areas can be declared with:
  01  WS-VAR PIC X(n) VALUE 'text'.
  01  WS-NUM PIC 9(n) VALUE 0.

The executor also understands PERFORM, MOVE, DISPLAY (non-CICS COBOL verbs)
so that surrounding COBOL logic flows correctly.
"""

from __future__ import annotations

import re
from datetime import datetime, timedelta
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..core.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def execute_cics(
    interpreter: "Interpreter",
    source: str,
    turtle: "TurtleState",
) -> str:
    """Execute a CICS pseudo-COBOL program and return screen/spool output."""
    env = CICSEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# CICS condition codes
# ---------------------------------------------------------------------------

class CICSCondition(Exception):
    """Raised when a CICS exceptional condition occurs."""
    def __init__(self, cond: str, resp: int = 0):
        self.cond = cond.upper()
        self.resp = resp
        super().__init__(cond)


class CICSReturn(Exception):
    """Raised by EXEC CICS RETURN to end the task."""
    def __init__(self, transid: str = "", commarea: str = ""):
        self.transid = transid
        self.commarea = commarea


class CICSAbend(Exception):
    def __init__(self, code: str = "ASRE"):
        self.code = code


_CICS_RESP: Dict[str, int] = {
    "NORMAL": 0, "NOTFND": 13, "DUPREC": 14, "DUPKEY": 15, "INVREQ": 16,
    "IOERR": 17, "NOSPACE": 18, "NOTOPEN": 19, "ENDFILE": 20,
    "ILLOGIC": 21, "LENGERR": 22, "QZERO": 23, "MAPFAIL": 36,
    "PGMIDERR": 27, "TRANSIDERR": 28, "TERMERR": 81,
}


# ---------------------------------------------------------------------------
# BMS Map simulation
# ---------------------------------------------------------------------------

class BmsMap:
    """Extremely simplified BMS map (fixed 24×80 terminal model)."""

    ROWS = 24
    COLS = 80

    def __init__(self, name: str):
        self.name = name
        self._cells = [[" "] * self.COLS for _ in range(self.ROWS)]
        self._fields: Dict[str, Tuple[int, int, int]] = {}  # name → (row, col, len)
        self._field_data: Dict[str, str] = {}

    def define_field(self, name: str, row: int, col: int, length: int) -> None:
        self._fields[name.upper()] = (row, col, length)

    def set_field(self, name: str, value: str) -> None:
        self._field_data[name.upper()] = value

    def get_field(self, name: str) -> str:
        return self._field_data.get(name.upper(), "")

    def render(self) -> str:
        """Return a plain-text representation of the map."""
        lines = [f"  ╔{'═' * self.COLS}╗"]
        screen = [list(" " * self.COLS) for _ in range(self.ROWS)]
        for fname, (r, c, l) in self._fields.items():
            val = self._field_data.get(fname, "").ljust(l)[:l]
            for i, ch in enumerate(val):
                if c + i < self.COLS:
                    screen[r][c + i] = ch
        for r in range(self.ROWS):
            lines.append(f"  ║{''.join(screen[r])}║")
        lines.append(f"  ╚{'═' * self.COLS}╝")
        return "\n".join(lines)


# ---------------------------------------------------------------------------
# CICS execution environment
# ---------------------------------------------------------------------------

class CICSEnvironment:
    """Simulates a CICS task/transaction execution environment."""

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: List[str] = []
        # Working Storage
        self._vars: Dict[str, Any] = {}
        # Simulated VSAM files (name → {key: record})
        self._files: Dict[str, Dict[str, str]] = {}
        # BMS maps
        self._maps: Dict[str, BmsMap] = {}
        # Condition handlers name → label
        self._handlers: Dict[str, str] = {}
        # Labels in code
        self._labels: Dict[str, int] = {}
        # Current lines being executed
        self._lines: List[str] = []
        self._pc: int = 0
        # CICS environment data
        self._terminal = "TW01"
        self._userid = "TWUSER"
        self._sysid = "TWRP"
        self._transid = "TW00"
        self._commarea: str = ""
        self._task_number = 1
        # Terminal screen buffer
        self._screen: List[str] = []
        self._eib: Dict[str, Any] = {
            "EIBCALEN": 0,
            "EIBTRMID": "TW01",
            "EIBTRNID": "TW00",
            "EIBTASKN": 1,
            "EIBDATE": int(datetime.now().strftime("%y%j")),
            "EIBTIME": int(datetime.now().strftime("%H%M%S")),
            "EIBRESP": 0,
            "EIBRESP2": 0,
        }

    # ------------------------------------------------------------------

    def _emit(self, text: str) -> None:
        self._output.append(str(text))

    def _screen_write(self, text: str) -> None:
        """Write to simulated terminal screen."""
        self._screen.append(str(text))
        self._emit(str(text))

    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        """Execute CICS pseudo-COBOL source."""
        self._output.clear()
        self._screen.clear()

        # Print CICS banner
        self._emit("=" * 60)
        self._emit(f"  CICS/TS 1.3 (Time Warp Studio Educational Simulation)")
        self._emit(f"  TRANSID={self._transid}  TERMINAL={self._terminal}  USERID={self._userid}")
        self._emit("=" * 60)
        self._emit("")

        try:
            self._prepare(source)
            self._exec_program()
        except CICSReturn as r:
            self._emit("")
            if r.transid:
                self._emit(f"CICS RETURN: TRANSID({r.transid}) initiated.")
            else:
                self._emit("CICS RETURN: Task ended normally.")
        except CICSAbend as a:
            self._emit(f"")
            self._emit(f"❌ CICS ABEND: CODE={a.code}  TASK={self._task_number}")
        except Exception as e:
            self._emit(f"❌ CICS runtime error: {e}")

        # Show screen contents if any
        if self._screen:
            self._emit("")
            self._emit("── Terminal Screen ──")
            for line in self._screen:
                self._emit(line)

        return "\n".join(self._output)

    # ------------------------------------------------------------------ prepare

    def _prepare(self, source: str) -> None:
        """Pre-scan source: build label map, parse working-storage."""
        raw_lines = source.splitlines()
        self._lines = []
        # Merge EXEC CICS ... END-EXEC onto single logical lines
        merged = self._merge_exec_cics(raw_lines)
        # Parse WORKING-STORAGE for 01-level vars
        self._parse_working_storage(merged)
        # Build label map
        for i, line in enumerate(merged):
            stripped = line.strip()
            m = re.match(r"^([A-Z0-9][\w-]*)\s*\.", stripped, re.I)
            if m and not stripped.upper().startswith(("EXEC", "PERFORM", "MOVE")):
                self._labels[m.group(1).upper()] = i
        self._lines = merged

    def _merge_exec_cics(self, raw: List[str]) -> List[str]:
        """Merge multi-line EXEC CICS ... END-EXEC into single lines."""
        result: List[str] = []
        building = False
        buf: List[str] = []
        for line in raw:
            stripped = line.strip()
            if not building:
                if re.match(r"EXEC\s+CICS\b", stripped, re.I):
                    building = True
                    buf = [stripped]
                    if re.search(r"END-EXEC", stripped, re.I):
                        result.append("    " + " ".join(buf))
                        building = False
                        buf = []
                else:
                    result.append(line)
            else:
                buf.append(stripped)
                if re.search(r"END-EXEC", stripped, re.I):
                    result.append("    " + " ".join(buf))
                    building = False
                    buf = []
        if buf:
            result.append("    " + " ".join(buf))
        return result

    def _parse_working_storage(self, lines: List[str]) -> None:
        """Extract 01-level PIC variables from WORKING-STORAGE SECTION or top-level."""
        in_ws = False
        has_ws_section = any("WORKING-STORAGE SECTION" in l.upper() for l in lines)
        # If no explicit section headers, treat all 01-level PIC lines as data
        if not has_ws_section:
            for line in lines:
                upper = line.strip().upper()
                # Stop at PROCEDURE DIVISION or any SECTION header
                if re.match(r"(PROCEDURE|LINKAGE|FILE)\s+(SECTION|DIVISION)", upper):
                    break
                m = re.match(
                    r"\s*0?1\s+([\w-]+)\s+PIC\s+([X9]+(?:\([^)]+\))?)\s*(?:VALUE\s+(.*?))?\.?\s*$",
                    line, re.I
                )
                if m:
                    vname, pic, init_val = m.group(1), m.group(2), m.group(3)
                    init_val = (init_val or "").strip().strip("'\".").rstrip(".")
                    self._vars[vname.upper()] = init_val if init_val else ""
            return
        for line in lines:
            upper = line.strip().upper()
            if "WORKING-STORAGE SECTION" in upper:
                in_ws = True
                continue
            if in_ws and re.match(r"(PROCEDURE|LINKAGE|FILE)\s+SECTION", upper):
                in_ws = False
                continue
            if in_ws:
                m = re.match(
                    r"\s*0?1\s+([\w-]+)\s+PIC\s+([X9]+(?:\([^)]+\))?)\s*(?:VALUE\s+(.*?))?\.?\s*$",
                    line, re.I
                )
                if m:
                    vname, pic, init_val = m.group(1), m.group(2), m.group(3)
                    init_val = (init_val or "").strip().strip("'\".").rstrip(".")
                    self._vars[vname.upper()] = init_val if init_val else ""

    # ------------------------------------------------------------------ execute

    def _exec_program(self) -> None:
        """Execute lines starting from PROCEDURE DIVISION."""
        start = 0
        for i, line in enumerate(self._lines):
            if re.search(r"PROCEDURE\s+DIVISION", line, re.I):
                start = i + 1
                break
        self._pc = start
        max_steps = 50000
        steps = 0
        while self._pc < len(self._lines) and steps < max_steps:
            line = self._lines[self._pc].strip()
            self._pc += 1
            steps += 1
            if not line or line.startswith("*"):
                continue
            if line.upper().startswith("STOP RUN") or line.upper().startswith("GOBACK"):
                raise CICSReturn()
            self._exec_line(line)
        if steps >= max_steps:
            self._emit("❌ CICS: execution step limit reached (infinite loop guard)")

    def _exec_line(self, line: str) -> None:
        """Execute one logical line / statement."""
        upper = line.upper()

        # Skip division/section headers and 01-level declarations
        if re.search(r"DIVISION|SECTION", upper):
            return
        if re.match(r"0?[12345678]\s+", line.strip()):
            return  # data definition

        # EXEC CICS ... END-EXEC
        if re.match(r"EXEC\s+CICS\b", upper):
            self._exec_cics(line)
            return

        # DISPLAY
        if upper.startswith("DISPLAY "):
            val = line[8:].strip().strip('"\'').rstrip(".")
            self._screen_write(self._resolve(val))
            return

        # MOVE A TO B
        m = re.match(r"MOVE\s+(.+?)\s+TO\s+(\S+?)\.?\s*$", line, re.I)
        if m:
            src, dst = m.group(1).strip(), m.group(2).strip().upper()
            self._vars[dst] = self._resolve(src.strip("'\""))
            return

        # PERFORM label
        m = re.match(r"PERFORM\s+(\S+?)(?:\s+THRU\s+(\S+))?\s*\.?\s*$", line, re.I)
        if m:
            label = m.group(1).upper()
            self._perform(label)
            return

        # ADD a TO b
        m = re.match(r"ADD\s+(\S+)\s+TO\s+(\S+?)\.?\s*$", line, re.I)
        if m:
            src_val = self._resolve(m.group(1).strip())
            dst_name = m.group(2).strip().upper()
            try:
                dst_val = float(self._vars.get(dst_name, 0))
                self._vars[dst_name] = str(int(dst_val + float(src_val)))
            except (ValueError, TypeError):
                pass
            return

        # SUBTRACT a FROM b
        m = re.match(r"SUBTRACT\s+(\S+)\s+FROM\s+(\S+?)\.?\s*$", line, re.I)
        if m:
            src_val = self._resolve(m.group(1).strip())
            dst_name = m.group(2).strip().upper()
            try:
                dst_val = float(self._vars.get(dst_name, 0))
                self._vars[dst_name] = str(int(dst_val - float(src_val)))
            except (ValueError, TypeError):
                pass
            return

        # COMPUTE var = expr
        m = re.match(r"COMPUTE\s+(\S+)\s*=\s*(.+?)\.?\s*$", line, re.I)
        if m:
            dst_name = m.group(1).strip().upper()
            expr_str = m.group(2).strip()
            # Substitute variable names with their numeric values
            def _var_sub(vm):
                vn = vm.group(0).upper()
                v = self._vars.get(vn, vn)
                return str(v)
            expr_resolved = re.sub(r"[A-Za-z][\w-]*", _var_sub, expr_str)
            try:
                result = eval(expr_resolved, {"__builtins__": {}}, {})
                self._vars[dst_name] = str(int(result)) if isinstance(result, float) and result == int(result) else str(result)
            except Exception:
                pass
            return

        # IF / ELSE / END-IF  (very simple)
        if upper.startswith("IF "):
            self._exec_if(line)
            return

        # Labels (paragraph names ending with .)
        m = re.match(r"^([A-Z0-9][\w-]*)\s*\.\s*$", line, re.I)
        if m:
            return  # label itself is a no-op

    # ------------------------------------------------------------------ EXEC CICS dispatch

    def _exec_cics(self, line: str) -> None:
        """Parse and dispatch EXEC CICS command."""
        # Strip EXEC CICS ... END-EXEC envelope
        inner = re.sub(r"EXEC\s+CICS\b", "", line, flags=re.I)
        inner = re.sub(r"END-EXEC\.?", "", inner, flags=re.I).strip()

        cmd_m = re.match(r"(\w+(?:\s+\w+)?)\s*(.*)", inner, re.I)
        if not cmd_m:
            return
        cmd = cmd_m.group(1).upper().strip()
        args_str = cmd_m.group(2)
        args = self._parse_cics_opts(args_str)

        dispatch: Dict[str, Any] = {
            "SEND TEXT": self._cics_send_text,
            "SEND MAP": self._cics_send_map,
            "SEND": self._cics_send_text,
            "RECEIVE MAP": self._cics_receive_map,
            "RECEIVE": self._cics_receive,
            "READ": self._cics_read,
            "READNEXT": self._cics_readnext,
            "WRITE": self._cics_write,
            "REWRITE": self._cics_rewrite,
            "DELETE": self._cics_delete,
            "LINK": self._cics_link,
            "XCTL": self._cics_xctl,
            "RETURN": self._cics_return,
            "ABEND": self._cics_abend,
            "HANDLE CONDITION": self._cics_handle_condition,
            "HANDLE": self._cics_handle_condition,
            "IGNORE CONDITION": self._cics_ignore_condition,
            "IGNORE": self._cics_ignore_condition,
            "ASSIGN": self._cics_assign,
            "GETMAIN": self._cics_getmain,
            "FREEMAIN": self._cics_freemain,
            "ENQ": self._cics_enq,
            "DEQ": self._cics_deq,
            "SYNCPOINT": self._cics_syncpoint,
            "DELAY": self._cics_delay,
            "ASKTIME": self._cics_asktime,
            "FORMATTIME": self._cics_formattime,
            "RETRIEVE": self._cics_retrieve,
            "SET": self._cics_set,
            "INQUIRE": self._cics_inquire,
        }
        fn = dispatch.get(cmd)
        if fn:
            fn(args)
        else:
            self._emit(f"ℹ️  EXEC CICS {cmd} — simulated (no-op)")

    # ------------------------------------------------------------------ CICS commands

    def _parse_cics_opts(self, s: str) -> Dict[str, str]:
        """Parse CICS option(value) pairs."""
        opts: Dict[str, str] = {}
        for m in re.finditer(r"(\w+)\s*(?:\(\s*([^)]*)\s*\))?", s):
            k = m.group(1).upper()
            v = m.group(2) or ""
            opts[k] = v.strip("'\" ")
        return opts

    def _get_opt(self, opts: Dict[str, str], key: str, default: str = "") -> str:
        val = opts.get(key, default)
        return self._resolve(val)

    def _resolve(self, expr: str) -> str:
        """Resolve variable reference or literal."""
        expr = expr.strip()
        if expr.startswith("'") and expr.endswith("'"):
            return expr[1:-1]
        upper = expr.upper()
        if upper in self._vars:
            return str(self._vars[upper])
        if upper in self._eib:
            return str(self._eib[upper])
        return expr

    def _cics_send_text(self, opts: Dict[str, str]) -> None:
        data = self._get_opt(opts, "FROM")
        text = self._get_opt(opts, "TEXT", data)
        if not text:
            text = data
        if opts.get("ERASE"):
            self._screen.clear()
        self._screen_write(text)

    def _cics_send_map(self, opts: Dict[str, str]) -> None:
        mapname = list(opts.keys())[0] if opts else "UNKNOWN"
        mset = self._get_opt(opts, "MAPSET", mapname)
        self._emit(f"  [SEND MAP {mapname} MAPSET({mset})]")
        bmap = self._maps.get(mapname.upper())
        if bmap:
            self._emit(bmap.render())
        else:
            data = self._get_opt(opts, "FROM")
            if data:
                self._screen_write(data)

    def _cics_receive(self, opts: Dict[str, str]) -> None:
        into = self._get_opt(opts, "INTO")
        if into:
            self._vars[into.upper()] = "(INPUT)"
        self._emit(f"  [RECEIVE INTO({into or 'buffer'})]  → simulated empty input")

    def _cics_receive_map(self, opts: Dict[str, str]) -> None:
        mapname = list(opts.keys())[0] if opts else "UNKNOWN"
        self._emit(f"  [RECEIVE MAP {mapname}]  → simulated empty input")

    def _cics_read(self, opts: Dict[str, str]) -> None:
        fname = self._get_opt(opts, "FILE")
        into = self._get_opt(opts, "INTO")
        key = self._get_opt(opts, "RIDFLD")
        fdata = self._files.get(fname.upper(), {})
        rec = fdata.get(key, None)
        if rec is None:
            self._handle_cond("NOTFND")
        else:
            if into:
                self._vars[into.upper()] = rec
            self._emit(f"  READ FILE({fname}) RIDFLD({key}) → '{rec}'")

    def _cics_readnext(self, opts: Dict[str, str]) -> None:
        fname = self._get_opt(opts, "FILE")
        self._emit(f"  READNEXT FILE({fname}) — sequential browse simulated")

    def _cics_write(self, opts: Dict[str, str]) -> None:
        fname = self._get_opt(opts, "FILE")
        frm = self._get_opt(opts, "FROM")
        key = self._get_opt(opts, "RIDFLD")
        if fname:
            if fname.upper() not in self._files:
                self._files[fname.upper()] = {}
            self._files[fname.upper()][key] = self._resolve(frm)
            self._emit(f"  WRITE FILE({fname}) RIDFLD({key}) FROM('{self._resolve(frm)}')")

    def _cics_rewrite(self, opts: Dict[str, str]) -> None:
        fname = self._get_opt(opts, "FILE")
        frm = self._get_opt(opts, "FROM")
        self._emit(f"  REWRITE FILE({fname}) FROM('{self._resolve(frm)}')")

    def _cics_delete(self, opts: Dict[str, str]) -> None:
        fname = self._get_opt(opts, "FILE")
        key = self._get_opt(opts, "RIDFLD")
        if fname.upper() in self._files:
            self._files[fname.upper()].pop(key, None)
        self._emit(f"  DELETE FILE({fname}) RIDFLD({key})")

    def _cics_link(self, opts: Dict[str, str]) -> None:
        prog = self._get_opt(opts, "PROGRAM")
        comm = self._get_opt(opts, "COMMAREA")
        self._emit(f"  LINK PROGRAM({prog}) — simulated (no external programs loaded)")

    def _cics_xctl(self, opts: Dict[str, str]) -> None:
        prog = self._get_opt(opts, "PROGRAM")
        self._emit(f"  XCTL PROGRAM({prog}) — control transfer simulated")
        raise CICSReturn(transid=prog)

    def _cics_return(self, opts: Dict[str, str]) -> None:
        transid = self._get_opt(opts, "TRANSID")
        comm = self._get_opt(opts, "COMMAREA")
        raise CICSReturn(transid=transid, commarea=comm)

    def _cics_abend(self, opts: Dict[str, str]) -> None:
        code = self._get_opt(opts, "ABCODE", "ASRE")
        raise CICSAbend(code=code)

    def _cics_handle_condition(self, opts: Dict[str, str]) -> None:
        for cond, label in opts.items():
            self._handlers[cond.upper()] = label

    def _cics_ignore_condition(self, opts: Dict[str, str]) -> None:
        for cond in opts:
            self._handlers[cond.upper()] = "__IGNORE__"

    def _handle_cond(self, cond: str) -> None:
        """Invoke condition handler or raise CICSCondition."""
        label = self._handlers.get(cond.upper())
        if label == "__IGNORE__":
            return
        if label:
            self._perform(label)
            return
        raise CICSCondition(cond)

    def _cics_assign(self, opts: Dict[str, str]) -> None:
        for key, var in opts.items():
            val_map = {
                "TERMINAL": self._terminal,
                "USERID": self._userid,
                "SYSID": self._sysid,
                "TRANSID": self._transid,
                "TASKN": str(self._task_number),
            }
            if key in val_map and var:
                self._vars[var.upper()] = val_map[key]

    def _cics_getmain(self, opts: Dict[str, str]) -> None:
        length = self._get_opt(opts, "LENGTH", "0")
        self._emit(f"  GETMAIN LENGTH({length}) — storage allocated (simulated)")

    def _cics_freemain(self, opts: Dict[str, str]) -> None:
        self._emit("  FREEMAIN — storage freed (simulated)")

    def _cics_enq(self, opts: Dict[str, str]) -> None:
        res = self._get_opt(opts, "RESOURCE", "UNKNOWN")
        self._emit(f"  ENQ RESOURCE({res}) — enqueued")

    def _cics_deq(self, opts: Dict[str, str]) -> None:
        res = self._get_opt(opts, "RESOURCE", "UNKNOWN")
        self._emit(f"  DEQ RESOURCE({res}) — dequeued")

    def _cics_syncpoint(self, opts: Dict[str, str]) -> None:
        self._emit("  SYNCPOINT — unit of work committed")

    def _cics_delay(self, opts: Dict[str, str]) -> None:
        interval = self._get_opt(opts, "INTERVAL", "0")
        self._emit(f"  DELAY INTERVAL({interval}) — simulated")

    def _cics_asktime(self, opts: Dict[str, str]) -> None:
        now = datetime.now()
        abstime = self._get_opt(opts, "ABSTIME")
        if abstime:
            self._vars[abstime.upper()] = int(now.timestamp())

    def _cics_formattime(self, opts: Dict[str, str]) -> None:
        now = datetime.now()
        if "DATESEP" in opts or "DATE" in opts:
            target = self._get_opt(opts, "DATE") or self._get_opt(opts, "MMDDYY")
            if target:
                self._vars[target.upper()] = now.strftime("%m/%d/%y")
        if "TIMESEP" in opts or "TIME" in opts:
            target = self._get_opt(opts, "TIME")
            if target:
                self._vars[target.upper()] = now.strftime("%H:%M:%S")

    def _cics_retrieve(self, opts: Dict[str, str]) -> None:
        into = self._get_opt(opts, "INTO")
        if into:
            self._vars[into.upper()] = self._commarea
        self._emit(f"  RETRIEVE INTO({into or 'area'}) — commarea retrieved")

    def _cics_set(self, opts: Dict[str, str]) -> None:
        self._emit("  SET — option set (simulated)")

    def _cics_inquire(self, opts: Dict[str, str]) -> None:
        self._emit("  INQUIRE — system inquiry (simulated)")

    # ------------------------------------------------------------------ COBOL verbs

    def _perform(self, label: str) -> None:
        """PERFORM paragraph — jump to label, execute until next label."""
        idx = self._labels.get(label.upper())
        if idx is None:
            self._emit(f"❌ PERFORM {label}: paragraph not found")
            return
        save_pc = self._pc
        self._pc = idx + 1
        max_steps = 10000
        steps = 0
        while self._pc < len(self._lines) and steps < max_steps:
            line = self._lines[self._pc].strip()
            self._pc += 1
            steps += 1
            if not line or line.startswith("*"):
                continue
            # Stop when we hit another paragraph label
            if re.match(r"^[A-Z0-9][\w-]*\s*\.\s*$", line, re.I):
                break
            if line.upper().startswith("STOP RUN") or line.upper().startswith("GOBACK"):
                raise CICSReturn()
            self._exec_line(line)
        self._pc = save_pc

    def _exec_if(self, line: str) -> None:
        """Very simple IF condition THEN ... [ELSE ...] END-IF."""
        m = re.match(r"IF\s+(.+?)\s+THEN\s+(.+?)(?:\s+ELSE\s+(.+?))?(?:\s+END-IF)?\.?\s*$",
                     line, re.I)
        if not m:
            return
        cond = m.group(1).strip()
        then_clause = m.group(2).strip()
        else_clause = (m.group(3) or "").strip()
        if self._eval_condition(cond):
            self._exec_line(then_clause)
        elif else_clause:
            self._exec_line(else_clause)

    def _eval_condition(self, cond: str) -> bool:
        # Simple equality check: var = 'value'
        m = re.match(r"(\S+)\s*=\s*'?([^']*)'?", cond, re.I)
        if m:
            lhs = self._resolve(m.group(1))
            rhs = m.group(2).strip()
            return str(lhs) == str(rhs)
        return False
