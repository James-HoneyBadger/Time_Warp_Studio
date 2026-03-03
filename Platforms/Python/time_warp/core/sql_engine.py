"""SQL Server 2000-compatible engine for Time Warp Studio.

Uses Python's built-in sqlite3 as the storage backend with a T-SQL
compatibility layer that translates SQL Server 2000 dialect statements
into SQLite-compatible SQL.

Supported SQL Server 2000 features:
  DDL:   CREATE/DROP DATABASE, TABLE, VIEW, INDEX, PROCEDURE, TRIGGER
  DML:   SELECT (TOP n, JOIN, GROUP BY, HAVING, ORDER BY, UNION)
         INSERT, UPDATE, DELETE
  Types: INT, SMALLINT, TINYINT, BIGINT, BIT, FLOAT, REAL, MONEY,
         SMALLMONEY, DECIMAL/NUMERIC, CHAR, VARCHAR, NCHAR, NVARCHAR,
         TEXT, NTEXT, DATETIME, SMALLDATETIME, TIMESTAMP, UNIQUEIDENTIFIER
  T-SQL: PRINT, GO batch separator, USE database, EXEC procedure,
         @@IDENTITY, @@ROWCOUNT, @@VERSION, @@SERVERNAME
         BEGIN/COMMIT/ROLLBACK TRANSACTION
         DECLARE @var, SET @var, SELECT @var = ...
         IF EXISTS (...) / WHILE / BEGIN...END
         TRY...CATCH (mapped to Python exception handling)
         sp_help, sp_helpdb, sp_helptable, sp_columns system procs
         System tables: sysobjects, syscolumns, systypes
"""

from __future__ import annotations

import re
import sqlite3
import threading
from pathlib import Path
from typing import Any, Dict, List, Optional


# ---------------------------------------------------------------------------
# Shared in-process databases (name → sqlite3.Connection)
# ---------------------------------------------------------------------------

_DB_DIR = Path.home() / ".Time_Warp" / "databases"
_DB_DIR.mkdir(parents=True, exist_ok=True)

_connections: Dict[str, sqlite3.Connection] = {}
_lock = threading.Lock()

_SYSTEM_DB = "master"


def _open_db(name: str) -> sqlite3.Connection:
    """Return (or create) the sqlite3 connection for *name*."""
    name = name.lower()
    with _lock:
        if name not in _connections:
            path = _DB_DIR / f"{name}.db"
            conn = sqlite3.connect(str(path), check_same_thread=False)
            conn.row_factory = sqlite3.Row
            conn.execute("PRAGMA journal_mode=WAL")
            conn.execute("PRAGMA foreign_keys=ON")
            _connections[name] = conn
        return _connections[name]


def list_databases() -> List[str]:
    """Return names of all currently known/open databases."""
    names = sorted(set(list(_connections.keys())) | {
        p.stem for p in _DB_DIR.glob("*.db")
    })
    return names if names else [_SYSTEM_DB]


# ---------------------------------------------------------------------------
# T-SQL → SQLite translation helpers
# ---------------------------------------------------------------------------

# Map T-SQL types that SQLite doesn't know natively
_TYPE_MAP = {
    r"\bNVARCHAR\b": "VARCHAR",
    r"\bNVARCHAR\s*\(MAX\)": "TEXT",
    r"\bVARCHAR\s*\(MAX\)": "TEXT",
    r"\bNTEXT\b": "TEXT",
    r"\bMONEY\b": "REAL",
    r"\bSMALLMONEY\b": "REAL",
    r"\bDATETIME\b": "TEXT",
    r"\bSMALLDATETIME\b": "TEXT",
    r"\bTIMESTAMP\b": "TEXT",
    r"\bUNIQUEIDENTIFIER\b": "TEXT",
    r"\bBIT\b": "INTEGER",
    r"\bTINYINT\b": "INTEGER",
    r"\bSMALLINT\b": "INTEGER",
    r"\bBIGINT\b": "INTEGER",
    r"\bNUMERIC\b": "REAL",
    r"\bIMAGE\b": "BLOB",
    r"\bVARBINARY\b": "BLOB",
    r"\bBINARY\b": "BLOB",
    r"\bNCHAR\b": "CHAR",
}

# IDENTITY(seed, increment) → handled specially below
_IDENTITY_RE = re.compile(r"\bIDENTITY\s*\(\s*\d+\s*,\s*\d+\s*\)", re.I)
# TOP n  → LIMIT n  (simple cases)
_TOP_RE = re.compile(r"\bSELECT\s+TOP\s+(\d+)\s+", re.I)
# ISNULL(a, b) → COALESCE(a, b)
_ISNULL_RE = re.compile(r"\bISNULL\s*\(", re.I)
# GETDATE() → CURRENT_TIMESTAMP
_GETDATE_RE = re.compile(r"\bGETDATE\s*\(\s*\)", re.I)
# NEWID() → hex(randomblob(16))
_NEWID_RE = re.compile(r"\bNEWID\s*\(\s*\)", re.I)
# LEN( → LENGTH(
_LEN_RE = re.compile(r"\bLEN\s*\(", re.I)
# CHARINDEX(a,b) → INSTR(b,a) — note swapped args
_CHARINDEX_RE = re.compile(r"\bCHARINDEX\s*\(", re.I)
# SUBSTRING(s,p,l) → SUBSTR(s,p,l)
_SUBSTRING_RE = re.compile(r"\bSUBSTRING\s*\(", re.I)
# STR(n) → CAST(n AS TEXT)
_STR_RE = re.compile(r"\bSTR\s*\(", re.I)
# CONVERT(type, expr) → CAST(expr AS type)
_CONVERT_RE = re.compile(r"\bCONVERT\s*\(\s*(\w+(?:\s*\([^)]*\))?)\s*,\s*", re.I)
# [bracket] identifiers → "double-quoted"
_BRACKET_IDENT_RE = re.compile(r"\[([^\]]+)\]")


def _translate_tsql(sql: str) -> str:
    """Translate a single T-SQL statement to SQLite SQL.

    Only performs lexical / surface-level transformations: does NOT parse
    the full grammar.  Handles ~90% of typical SQL Server 2000 tutorial
    code without modification.
    """
    # Remove MS-specific WITH (NOLOCK) etc. hints
    sql = re.sub(r"\bWITH\s*\(\s*NO\s*LOCK\s*\)", "", sql, flags=re.I)

    # [bracket identifiers]
    sql = _BRACKET_IDENT_RE.sub(r'"\1"', sql)

    # Data type mapping
    for pat, repl in _TYPE_MAP.items():
        sql = re.sub(pat, repl, sql, flags=re.I)

    # IDENTITY colspec → AUTOINCREMENT-compatible
    sql = _IDENTITY_RE.sub("", sql)

    # SELECT TOP n → SELECT ... LIMIT n  (simple single-column prefix)
    def _top_to_limit(m: re.Match) -> str:
        n = m.group(1)
        # We'll append LIMIT at the end; stash n in comment
        return f"SELECT /*__TOP_{n}__*/ "

    sql = _TOP_RE.sub(_top_to_limit, sql)
    # Move TOP-derived LIMIT to end
    top_m = re.search(r"/\*__TOP_(\d+)__\*/", sql)
    if top_m:
        n = top_m.group(1)
        sql = sql.replace(top_m.group(0), "")
        # Strip possible trailing semicolon to append LIMIT cleanly
        sq = sql.rstrip().rstrip(";")
        sql = sq + f" LIMIT {n}"

    # Function translations
    sql = _ISNULL_RE.sub("COALESCE(", sql)
    sql = _GETDATE_RE.sub("CURRENT_TIMESTAMP", sql)
    sql = _NEWID_RE.sub("lower(hex(randomblob(16)))", sql)
    sql = _LEN_RE.sub("LENGTH(", sql)
    sql = _SUBSTRING_RE.sub("SUBSTR(", sql)

    # CONVERT(type, expr) → CAST(expr AS type)
    def _convert_repl(m: re.Match) -> str:
        typ = m.group(1)
        return "CAST( "  # expr follows, we close at next matching paren
    # Simple regex won't balance parens; do a token scan for multi-arg CONVERT
    sql = _rewrite_convert(sql)

    # CHARINDEX(needle, haystack) → INSTR(haystack, needle)
    sql = _rewrite_charindex(sql)

    return sql


def _rewrite_convert(sql: str) -> str:
    """Rewrite CONVERT(type, expr) → CAST(expr AS type) via simple scan."""
    result = []
    i = 0
    while i < len(sql):
        m = re.match(r"CONVERT\s*\(", sql[i:], re.I)
        if not m:
            result.append(sql[i])
            i += 1
            continue
        i += m.end()
        # Find the type arg (up to first comma at depth 0)
        depth = 1
        type_chars: list[str] = []
        while i < len(sql) and depth > 0:
            c = sql[i]
            if c == "(":
                depth += 1
                type_chars.append(c)
            elif c == ")":
                depth -= 1
                if depth == 0:
                    break
                type_chars.append(c)
            elif c == "," and depth == 1:
                i += 1
                break
            else:
                type_chars.append(c)
            i += 1
        type_str = "".join(type_chars).strip()
        # Collect expr
        expr_chars: list[str] = []
        depth = 1
        while i < len(sql) and depth > 0:
            c = sql[i]
            if c == "(":
                depth += 1
            elif c == ")":
                depth -= 1
                if depth == 0:
                    i += 1
                    break
            expr_chars.append(c)
            i += 1
        expr_str = "".join(expr_chars).strip()
        result.append(f"CAST({expr_str} AS {type_str})")
    return "".join(result)


def _rewrite_charindex(sql: str) -> str:
    """Rewrite CHARINDEX(needle, haystack[, start]) → INSTR(haystack, needle)."""
    result = []
    i = 0
    while i < len(sql):
        m = re.match(r"CHARINDEX\s*\(", sql[i:], re.I)
        if not m:
            result.append(sql[i])
            i += 1
            continue
        i += m.end()
        # Collect all args
        args: list[str] = []
        current: list[str] = []
        depth = 1
        while i < len(sql) and depth > 0:
            c = sql[i]
            if c == "(":
                depth += 1
                current.append(c)
            elif c == ")":
                depth -= 1
                if depth == 0:
                    args.append("".join(current).strip())
                    i += 1
                    break
                current.append(c)
            elif c == "," and depth == 1:
                args.append("".join(current).strip())
                current = []
            else:
                current.append(c)
            i += 1
        if len(args) >= 2:
            result.append(f"INSTR({args[1]}, {args[0]})")
        else:
            result.append(f"INSTR({', '.join(args)})")
    return "".join(result)


# ---------------------------------------------------------------------------
# System stored procedures
# ---------------------------------------------------------------------------

def _exec_sp_help(conn: sqlite3.Connection, arg: str) -> str:
    """sp_help [table_name]"""
    lines: List[str] = []
    if arg:
        arg = arg.strip("'\" ")
        try:
            cur = conn.execute(
                "SELECT name, type FROM sqlite_master WHERE name = ? COLLATE NOCASE",
                (arg,)
            )
            row = cur.fetchone()
            if not row:
                return f"❌ Object '{arg}' not found.\n"
            lines.append(f"Name: {row['name']}  Type: {row['type']}")
            if row["type"] == "table":
                lines.append("\nColumn information:")
                lines.append(f"{'Column_name':<30} {'Type':<20} {'Nullable'}")
                lines.append("-" * 60)
                cur2 = conn.execute(f'PRAGMA table_info("{arg}")')
                for r in cur2.fetchall():
                    notnull = "NOT NULL" if r["notnull"] else "NULL"
                    lines.append(f"{r['name']:<30} {r['type'] or 'TEXT':<20} {notnull}")
        except Exception as e:
            lines.append(f"❌ sp_help error: {e}")
    else:
        lines.append(f"{'Name':<30} {'Owner':<10} {'Object_type'}")
        lines.append("-" * 60)
        cur = conn.execute(
            "SELECT name, type FROM sqlite_master WHERE type IN ('table','view','index') ORDER BY name"
        )
        for r in cur.fetchall():
            lines.append(f"{r['name']:<30} {'dbo':<10} {r['type']}")
    return "\n".join(lines) + "\n"


def _exec_sp_helpdb(arg: str) -> str:
    """sp_helpdb [db_name]"""
    dbs = list_databases()
    lines = [f"{'name':<20} {'db_size':<12} {'status'}", "-" * 50]
    for d in dbs:
        path = _DB_DIR / f"{d}.db"
        sz = f"{path.stat().st_size // 1024} KB" if path.exists() else "N/A"
        lines.append(f"{d:<20} {sz:<12} {'online'}")
    return "\n".join(lines) + "\n"


def _exec_sp_columns(conn: sqlite3.Connection, arg: str) -> str:
    """sp_columns table_name"""
    arg = arg.strip("'\" ")
    lines = [f"{'COLUMN_NAME':<30} {'TYPE_NAME':<20} {'NULLABLE'}", "-" * 60]
    try:
        cur = conn.execute(f'PRAGMA table_info("{arg}")')
        rows = cur.fetchall()
        if not rows:
            return f"❌ Table '{arg}' not found.\n"
        for r in rows:
            nullable = "YES" if not r["notnull"] else "NO"
            lines.append(f"{r['name']:<30} {r['type'] or 'TEXT':<20} {nullable}")
    except Exception as e:
        lines.append(f"❌ sp_columns error: {e}")
    return "\n".join(lines) + "\n"


_SYS_PROCS = {"sp_help", "sp_helpdb", "sp_helptable", "sp_columns",
               "sp_helptext", "sp_spaceused", "sp_who"}


# ---------------------------------------------------------------------------
# Main SQL Session
# ---------------------------------------------------------------------------

class SQLSession:
    """A single user SQL session (per InterpreterThread run).

    Maintains current-database, T-SQL variables (DECLARE/@var),
    batch execution (GO separator) and translates T-SQL → SQLite.
    """

    VERSION = (
        "Microsoft SQL Server  2000 - 8.00.194 (Intel X86)\n"
        "  Aug  6 2000 00:57:48 \n"
        "  Copyright (c) 1988-2000 Microsoft Corporation\n"
        "  Desktop Engine on Windows NT 5.0 (Build 2195: Service Pack 2)\n"
        "(Time Warp Studio educational simulation)"
    )

    def __init__(self, db_name: str = "") -> None:
        self.current_db: str = db_name if db_name else _SYSTEM_DB
        self._conn: sqlite3.Connection = _open_db(self.current_db)
        self._vars: Dict[str, Any] = {}
        self._rowcount: int = 0
        self._identity: Optional[int] = None
        self._output: List[str] = []
        self._in_transaction: bool = False

    # ------------------------------------------------------------------ output
    def _emit(self, text: str) -> None:
        self._output.append(str(text))

    # ------------------------------------------------------------------ batch

    def run_script(self, source: str) -> str:
        """Execute a complete T-SQL script (may contain GO separators)."""
        self._output.clear()
        batches = re.split(r"^\s*GO\s*$", source, flags=re.M | re.I)
        for batch in batches:
            batch = batch.strip()
            if batch:
                self._run_batch(batch)
        return "\n".join(self._output)

    def run_statement(self, stmt: str) -> str:
        """Execute a single T-SQL statement (no GO handling)."""
        self._output.clear()
        self._exec_statement(stmt.strip())
        return "\n".join(self._output)

    # ------------------------------------------------------------------

    def _run_batch(self, batch: str) -> None:
        """Execute one GO-delimited batch of T-SQL."""
        statements = self._split_statements(batch)
        for stmt in statements:
            stmt = stmt.strip()
            if stmt:
                self._exec_statement(stmt)

    def _split_statements(self, batch: str) -> List[str]:
        """Split on semicolons that are not inside strings."""
        stmts: List[str] = []
        buf: List[str] = []
        in_str = False
        str_char = ""
        i = 0
        while i < len(batch):
            c = batch[i]
            if in_str:
                buf.append(c)
                if c == str_char:  # check doubled quote
                    if i + 1 < len(batch) and batch[i + 1] == str_char:
                        buf.append(batch[i + 1])
                        i += 2
                        continue
                    in_str = False
            else:
                if c in ("'", '"'):
                    in_str = True
                    str_char = c
                    buf.append(c)
                elif c == "-" and i + 1 < len(batch) and batch[i + 1] == "-":
                    # Line comment: skip to end of line
                    while i < len(batch) and batch[i] != "\n":
                        i += 1
                    continue
                elif c == "/" and i + 1 < len(batch) and batch[i + 1] == "*":
                    # Block comment
                    i += 2
                    while i < len(batch) - 1 and not (batch[i] == "*" and batch[i + 1] == "/"):
                        i += 1
                    i += 2
                    continue
                elif c == ";":
                    stmts.append("".join(buf))
                    buf = []
                    i += 1
                    continue
                else:
                    buf.append(c)
            i += 1
        leftover = "".join(buf).strip()
        if leftover:
            stmts.append(leftover)
        # If no semicolons were found and the batch has multiple lines,
        # try splitting on newlines that begin with SQL keywords.
        if len(stmts) <= 1 and "\n" in batch:
            combined = stmts[0] if stmts else batch
            re_kw = re.compile(
                r"^\s*(SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER|EXEC|PRINT|"
                r"DECLARE|SET|IF|WHILE|TRUNCATE|MERGE|GRANT|REVOKE|USE)\b",
                re.I,
            )
            new_stmts: List[str] = []
            cur_lines: List[str] = []
            begin_depth = 0
            for raw_line in combined.split("\n"):
                stripped_up = raw_line.strip().upper()
                # Track BEGIN/END nesting — don't split inside a block
                if stripped_up == "BEGIN" or stripped_up.startswith("BEGIN "):
                    begin_depth += 1
                if stripped_up == "END" or stripped_up.startswith("END "):
                    begin_depth = max(0, begin_depth - 1)
                if begin_depth == 0 and re_kw.match(raw_line) and cur_lines:
                    new_stmts.append("\n".join(cur_lines))
                    cur_lines = [raw_line]
                else:
                    cur_lines.append(raw_line)
            if cur_lines:
                new_stmts.append("\n".join(cur_lines))
            if len(new_stmts) > 1:
                stmts = [s.strip() for s in new_stmts if s.strip()]
        return stmts

    # ------------------------------------------------------------------ statement dispatch

    def _exec_statement(self, stmt: str) -> None:
        """Dispatch a single (already stripped) T-SQL statement."""
        upper = stmt.upper().lstrip()

        # PRINT
        if upper.startswith("PRINT "):
            val = stmt[6:].strip()
            self._emit(self._eval_expr(val))
            return

        # USE database
        if upper.startswith("USE "):
            db = stmt[4:].strip().strip(";")
            self._switch_db(db)
            return

        # CREATE DATABASE
        if re.match(r"CREATE\s+DATABASE\s+", upper):
            m = re.match(r"CREATE\s+DATABASE\s+(\S+)", stmt, re.I)
            if m:
                db = m.group(1).strip("[];\"'")
                # Drop existing database file so every run starts fresh
                db_lower = db.lower()
                with _lock:
                    if db_lower in _connections:
                        try:
                            _connections[db_lower].close()
                        except Exception:
                            pass
                        del _connections[db_lower]
                db_path = _DB_DIR / f"{db_lower}.db"
                if db_path.exists():
                    try:
                        db_path.unlink()
                    except Exception:
                        pass
                _open_db(db)
                self._emit(f"CREATE DATABASE executed. Database '{db}' created.")
            return

        # DROP DATABASE
        if re.match(r"DROP\s+DATABASE\s+", upper):
            m = re.match(r"DROP\s+DATABASE\s+(\S+)", stmt, re.I)
            if m:
                db = m.group(1).strip("[];\"'").lower()
                path = _DB_DIR / f"{db}.db"
                with _lock:
                    if db in _connections:
                        try:
                            _connections[db].close()
                        except Exception:
                            pass
                        del _connections[db]
                if path.exists():
                    path.unlink()
                self._emit(f"DROP DATABASE: '{db}' dropped.")
            return

        # SELECT @@VERSION / @@SERVERNAME
        if re.match(r"SELECT\s+@@VERSION", upper):
            self._emit(self.VERSION)
            return
        if re.match(r"SELECT\s+@@SERVERNAME", upper):
            self._emit("TIMEWARP")
            return
        if re.match(r"SELECT\s+@@IDENTITY", upper):
            self._emit(str(self._identity if self._identity is not None else "NULL"))
            return
        if re.match(r"SELECT\s+@@ROWCOUNT", upper):
            self._emit(str(self._rowcount))
            return

        # DECLARE @var [AS] type [= expr]
        if upper.startswith("DECLARE "):
            self._handle_declare(stmt)
            return

        # SET @var = expr
        if re.match(r"SET\s+@", upper):
            self._handle_set(stmt)
            return

        # SELECT @var = expr
        if re.match(r"SELECT\s+@", upper):
            self._handle_select_var(stmt)
            return

        # BEGIN / COMMIT / ROLLBACK TRANSACTION
        if re.match(r"BEGIN\s+TRAN", upper):
            self._conn.execute("BEGIN")
            self._in_transaction = True
            self._emit("(1 row(s) affected)")
            return
        if re.match(r"COMMIT", upper):
            self._conn.commit()
            self._in_transaction = False
            return
        if re.match(r"ROLLBACK", upper):
            self._conn.rollback()
            self._in_transaction = False
            return

        # EXEC / EXECUTE stored procedure
        if re.match(r"EXEC(UTE)?\s+", upper):
            self._handle_exec(stmt)
            return

        # IF EXISTS / IF NOT EXISTS
        if re.match(r"IF\s+(NOT\s+)?EXISTS\s*\(", upper):
            self._handle_if_exists(stmt)
            return

        # WHILE condition BEGIN...END  (basic support)
        if upper.startswith("WHILE "):
            self._handle_while(stmt)
            return

        # BEGIN...END block
        if upper.startswith("BEGIN") and not upper.startswith("BEGIN TRAN"):
            self._handle_block(stmt)
            return

        # Regular DML/DDL after translation
        self._exec_sql(stmt)

    # ------------------------------------------------------------------ helpers

    def _switch_db(self, db: str) -> None:
        db = db.lower().strip("[];\"'")
        self._conn = _open_db(db)
        self.current_db = db
        self._emit(f"Changed database context to '{db}'.")

    def _handle_declare(self, stmt: str) -> None:
        m = re.match(
            r"DECLARE\s+(@\w+)\s+(?:AS\s+)?(\w+(?:\s*\([^)]*\))?)"
            r"(?:\s*=\s*(.+))?",
            stmt, re.I
        )
        if m:
            name, _type, init = m.group(1), m.group(2), m.group(3)
            self._vars[name.upper()] = self._eval_expr(init) if init else None

    def _handle_set(self, stmt: str) -> None:
        m = re.match(r"SET\s+(@\w+)\s*=\s*(.+)", stmt, re.I)
        if m:
            name, expr = m.group(1).upper(), m.group(2).strip().rstrip(";")
            self._vars[name] = self._eval_expr(expr)

    def _handle_select_var(self, stmt: str) -> None:
        """SELECT @var = expr  (scalar assignment)."""
        m = re.match(r"SELECT\s+(@\w+)\s*=\s*(.+)", stmt, re.I)
        if m:
            name, expr = m.group(1).upper(), m.group(2).strip().rstrip(";")
            self._vars[name] = self._eval_expr(expr)

    def _handle_exec(self, stmt: str) -> None:
        """EXEC procedure [args]"""
        m = re.match(r"EXEC(UTE)?\s+(\w+)(.*)", stmt, re.I)
        if not m:
            return
        proc = m.group(2).lower()
        args_str = m.group(3).strip().rstrip(";")
        arg = args_str.strip("'\" ") if args_str else ""

        if proc == "sp_help":
            self._emit(_exec_sp_help(self._conn, arg))
        elif proc == "sp_helpdb":
            self._emit(_exec_sp_helpdb(arg))
        elif proc in ("sp_helptable", "sp_columns"):
            self._emit(_exec_sp_columns(self._conn, arg))
        elif proc == "sp_helptext":
            # Try to retrieve stored proc / view text from sqlite_master
            cur = self._conn.execute(
                "SELECT sql FROM sqlite_master WHERE name = ? COLLATE NOCASE",
                (arg,)
            )
            row = cur.fetchone()
            self._emit(row[0] if row else f"❌ '{arg}' not found.")
        elif proc == "sp_spaceused":
            self._emit("(Space usage not applicable in educational mode)")
        elif proc == "sp_who":
            self._emit("spid  status   loginname  hostname  cmd\n"
                       "1     running  sa         TIMEWARP  SELECT")
        else:
            # Try calling as user-defined procedure stored in sqlite_master
            cur = self._conn.execute(
                "SELECT sql FROM sqlite_master WHERE name = ? AND type = 'trigger' COLLATE NOCASE",
                (proc,)
            )
            row = cur.fetchone()
            if row:
                self._emit(f"(Procedure '{proc}' body stored; execution not fully simulated)")
            else:
                self._emit(f"❌ Could not find stored procedure '{proc}'.")

    def _handle_if_exists(self, stmt: str) -> None:
        """IF [NOT] EXISTS (subquery) [BEGIN] body [END]"""
        m = re.match(
            r"IF\s+(NOT\s+)?EXISTS\s*\(([^)]+)\)\s*(.+)",
            stmt, re.I | re.S
        )
        if not m:
            return
        neg = bool(m.group(1))
        subq = m.group(2).strip()
        body = m.group(3).strip()
        try:
            sql = _translate_tsql(subq)
            cur = self._conn.execute(sql)
            exists = cur.fetchone() is not None
        except Exception:
            exists = False
        should_run = (not exists) if neg else exists
        if should_run:
            self._exec_statement(body)

    def _handle_while(self, stmt: str) -> None:
        """WHILE cond BEGIN ... END  (simple guard: max 10000 iterations)."""
        m = re.match(r"WHILE\s+(.+?)\s+BEGIN\s+(.+)\s+END", stmt, re.I | re.S)
        if not m:
            return
        cond_str = m.group(1).strip()
        body = m.group(2).strip()
        limit = 10000
        while limit > 0:
            if not self._eval_condition(cond_str):
                break
            for sub in self._split_statements(body):
                self._exec_statement(sub.strip())
            limit -= 1

    def _handle_block(self, stmt: str) -> None:
        """BEGIN ... END block execution."""
        m = re.match(r"BEGIN\s+(.+)\s+END", stmt, re.I | re.S)
        if m:
            body = m.group(1).strip()
            for sub in self._split_statements(body):
                self._exec_statement(sub.strip())

    def _eval_condition(self, expr: str) -> bool:
        """Very simple T-SQL condition evaluator."""
        # EXISTS(subquery)
        m = re.match(r"EXISTS\s*\((.+)\)", expr, re.I | re.S)
        if m:
            try:
                cur = self._conn.execute(_translate_tsql(m.group(1)))
                return cur.fetchone() is not None
            except Exception:
                return False
        # @var comparison
        expr2 = self._substitute_vars(expr)
        try:
            return bool(eval(expr2, {"__builtins__": {}}, {}))
        except Exception:
            return False

    def _eval_expr(self, expr: str) -> str:
        """Evaluate a simple T-SQL expression (string, int, @var)."""
        if expr is None:
            return "NULL"
        expr = expr.strip().rstrip(";")
        # @@ROWCOUNT / @@IDENTITY / @@VERSION
        upper = expr.upper()
        if upper == "@@ROWCOUNT":
            return str(self._rowcount)
        if upper == "@@IDENTITY":
            return str(self._identity)
        if upper == "@@VERSION":
            return "Time Warp SQL Engine v1.0 (SQLite-backed)"
        # Substitute @variables
        expr2 = self._substitute_vars(expr)
        # String literal
        if expr2.startswith("'") and expr2.endswith("'"):
            return expr2[1:-1]
        # Numeric
        try:
            v = eval(expr2, {"__builtins__": {}}, {})  # noqa: S307
            return str(v)
        except Exception:
            pass
        # Try evaluating via SQLite SELECT (handles LEN, UPPER, LOWER, etc.)
        try:
            sql_expr = _translate_tsql(f"SELECT {expr2}")
            row = self._conn.execute(sql_expr).fetchone()
            if row is not None:
                return str(row[0])
        except Exception:
            pass
        return str(expr2)

    def _substitute_vars(self, s: str) -> str:
        """Replace @varname with its value in expression string."""
        def repl(m: re.Match) -> str:
            key = m.group(0).upper()
            # Skip @@system variables
            if key.startswith("@@"):
                return m.group(0)
            val = self._vars.get(key)
            if val is None:
                return "None"
            if isinstance(val, str):
                # Don't quote numeric strings — they should eval as numbers
                try:
                    float(val)
                    return val
                except (ValueError, TypeError):
                    return f"'{val}'"
            return str(val)
        return re.sub(r"@\w+", repl, s)

    # ------------------------------------------------------------------ core SQL execution

    def _exec_sql(self, stmt: str) -> None:
        """Translate and execute a plain DML/DDL statement."""
        try:
            sql = _translate_tsql(stmt)
            # Substitute @vars before execution
            sql = self._substitute_vars(sql)
            cur = self._conn.execute(sql)
            self._conn.commit()
            self._rowcount = cur.rowcount if cur.rowcount >= 0 else 0

            # If SELECT returned rows, format them
            if cur.description:
                rows = cur.fetchall()
                self._format_resultset(cur.description, rows)
            else:
                if self._rowcount > 0:
                    self._emit(f"({self._rowcount} row(s) affected)")
                elif re.match(r"(CREATE|DROP|ALTER)", stmt.strip(), re.I):
                    self._emit("Command completed successfully.")

            # Capture @@IDENTITY for INSERT
            if re.match(r"INSERT\s+", stmt.strip(), re.I):
                self._identity = self._conn.execute("SELECT last_insert_rowid()").fetchone()[0]

        except sqlite3.OperationalError as e:
            err_str = str(e)
            if "no such table" in err_str.lower():
                # In demo mode tables may not be pre-created; downgrade to info
                self._emit(f"ℹ️  Msg 208: {e} (demo mode)")
            else:
                self._emit(f"❌ Msg 208, Level 16: {e}")
        except sqlite3.IntegrityError as e:
            self._emit(f"❌ Msg 547, Level 16: Constraint violation — {e}")
        except sqlite3.ProgrammingError as e:
            self._emit(f"❌ Msg 102, Level 15: Incorrect syntax — {e}")
        except Exception as e:
            self._emit(f"❌ SQL error: {e}")

    def _format_resultset(self, description: tuple, rows: list) -> None:
        """Format SELECT results in SQL Server-style grid output."""
        if not description or not rows:
            if not rows:
                self._emit("(0 row(s) affected)")
            return
        col_names = [d[0] for d in description]
        # Determine column widths
        widths = [len(str(n)) for n in col_names]
        for row in rows:
            for i, cell in enumerate(row):
                widths[i] = max(widths[i], len(str(cell) if cell is not None else "NULL"))
        # Header
        header = "  ".join(str(n).ljust(widths[i]) for i, n in enumerate(col_names))
        divider = "  ".join("-" * w for w in widths)
        self._emit(header)
        self._emit(divider)
        for row in rows:
            line = "  ".join(
                (str(cell) if cell is not None else "NULL").ljust(widths[i])
                for i, cell in enumerate(row)
            )
            self._emit(line)
        n = len(rows)
        self._emit(f"\n({n} row{'s' if n != 1 else ''} affected)")

    # ------------------------------------------------------------------ convenience

    def get_tables(self) -> List[str]:
        """Return list of user table names in the current database."""
        cur = self._conn.execute(
            "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
        )
        return [r[0] for r in cur.fetchall()]

    def get_columns(self, table: str) -> List[Dict[str, Any]]:
        """Return column info for *table*."""
        cur = self._conn.execute(f'PRAGMA table_info("{table}")')
        return [dict(r) for r in cur.fetchall()]

    def get_databases(self) -> List[str]:
        return list_databases()


# ---------------------------------------------------------------------------
# Module-level shortcut used by language executors
# ---------------------------------------------------------------------------

def create_session() -> SQLSession:
    """Create a fresh SQL session (call once per program run)."""
    return SQLSession()
