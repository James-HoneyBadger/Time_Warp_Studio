"""SQL language executor for Time Warp Studio.

Provides an interactive T-SQL / SQL Server 2000-compatible scripting
environment backed by the built-in sql_engine module.

Supported syntax:
  Full T-SQL batch execution with GO separators, DECLARE/@var, PRINT,
  USE database, CREATE/DROP DATABASE and TABLE, SELECT/INSERT/UPDATE/DELETE,
  stored procedures (sp_help etc.), transactions, IF EXISTS, WHILE loops,
  system functions: GETDATE(), NEWID(), LEN(), ISNULL(), CONVERT(),
  CHARINDEX(), SUBSTRING(), UPPER(), LOWER(), LTRIM(), RTRIM()

Educational features:
  • Multi-database sessions (each run gets a fresh session)
  • SQL Server 2000-style result formatting with column-width alignment
  • @@IDENTITY, @@ROWCOUNT, @@VERSION, @@SERVERNAME globals
  • T-SQL → SQLite transparent translation
"""

from __future__ import annotations

import uuid
from typing import TYPE_CHECKING

from ..core.sql_engine import SQLSession, _connections

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


def execute_sql(
    interpreter: "Interpreter",
    source: str,
    turtle: "TurtleState",
) -> str:
    """Execute a complete T-SQL script and return all output."""
    # Use a fresh unique database name for each execution so that tables
    # created in one run don't persist into the next (avoids "already exists").
    fresh_db = f"_run_{uuid.uuid4().hex}"
    session = SQLSession(fresh_db)
    # Make session accessible for cross-language SQL (COBOL EXEC SQL, etc.)
    interpreter.sql_session = session
    try:
        result = session.run_script(source)
    except RecursionError:
        return "❌ SQL error: Maximum recursion depth exceeded\n"
    except MemoryError:
        return "❌ SQL error: Out of memory\n"
    except Exception as exc:
        return f"❌ SQL error: {exc}\n"
    finally:
        # Clean up the ephemeral connection so it doesn't accumulate in memory
        conn = _connections.pop(fresh_db, None)
        if conn is not None:
            try:
                conn.close()
            except Exception:
                pass
    return result if result.strip() else "(No output)"
