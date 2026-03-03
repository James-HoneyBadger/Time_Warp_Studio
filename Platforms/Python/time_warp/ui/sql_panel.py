"""SQL Server Explorer panel for Time Warp Studio.

Provides a dockable SQL Workbench panel with:
  • Database/table tree view
  • SQL query editor with T-SQL syntax highlighting
  • Result grid (tabular output)
  • Auto-connect to the in-process SQL engine
  • Toolbar: Execute (F5), Format, Clear, New DB, Refresh
"""

from __future__ import annotations

from typing import TYPE_CHECKING, List, Optional

from PySide6.QtCore import Qt, QThread, Signal
from PySide6.QtWidgets import (
    QHBoxLayout,
    QInputDialog,
    QLabel,
    QSplitter,
    QTableWidget,
    QTableWidgetItem,
    QTextEdit,
    QToolBar,
    QTreeWidget,
    QTreeWidgetItem,
    QVBoxLayout,
    QWidget,
)
from PySide6.QtGui import (
    QColor,
    QFont,
    QTextCharFormat,
    QSyntaxHighlighter,
    QTextDocument,
    QKeySequence,
    QAction,
)

from ..core.sql_engine import SQLSession, list_databases, _open_db

if TYPE_CHECKING:
    pass


# ---------------------------------------------------------------------------
# T-SQL Syntax Highlighter
# ---------------------------------------------------------------------------

class TSQLHighlighter(QSyntaxHighlighter):
    """Basic T-SQL colorizer for the SQL editor."""

    def __init__(self, document: QTextDocument, dark: bool = True):
        super().__init__(document)
        self._rules: List[tuple] = []
        fg = QColor("#ABB2BF") if dark else QColor("#383A42")
        kw_color = QColor("#C678DD") if dark else QColor("#A626A4")
        fn_color = QColor("#61AFEF") if dark else QColor("#4078F2")
        str_color = QColor("#98C379") if dark else QColor("#50A14F")
        num_color = QColor("#D19A66") if dark else QColor("#986801")
        cmt_color = QColor("#5C6370") if dark else QColor("#A0A1A7")
        type_color = QColor("#E5C07B") if dark else QColor("#C18401")
        op_color = QColor("#56B6C2") if dark else QColor("#0184BC")

        keywords = (
            "SELECT|FROM|WHERE|INSERT|INTO|VALUES|UPDATE|SET|DELETE|CREATE|DROP|ALTER|"
            "TABLE|VIEW|INDEX|DATABASE|PROCEDURE|TRIGGER|FUNCTION|"
            "IF|ELSE|WHILE|BEGIN|END|DECLARE|EXEC|EXECUTE|PRINT|RETURN|"
            "AND|OR|NOT|IN|EXISTS|BETWEEN|LIKE|IS|NULL|"
            "JOIN|INNER|LEFT|RIGHT|OUTER|FULL|CROSS|ON|"
            "GROUP BY|ORDER BY|HAVING|UNION|ALL|DISTINCT|TOP|"
            "GO|USE|BACKUP|RESTORE|GRANT|REVOKE|"
            "COMMIT|ROLLBACK|TRANSACTION|TRAN|SAVEPOINT|"
            "PRIMARY KEY|FOREIGN KEY|UNIQUE|DEFAULT|CONSTRAINT|REFERENCES|"
            "IDENTITY|NOCOUNT|WITH"
        )
        kw_fmt = QTextCharFormat()
        kw_fmt.setForeground(kw_color)
        kw_fmt.setFontWeight(QFont.Bold)
        self._rules.append((
            __import__("re").compile(
                rf"\b({'|'.join(keywords.split('|'))})\b", __import__("re").I
            ),
            kw_fmt,
        ))

        types = (
            "INT|SMALLINT|TINYINT|BIGINT|BIT|FLOAT|REAL|DECIMAL|NUMERIC|"
            "CHAR|VARCHAR|NCHAR|NVARCHAR|TEXT|NTEXT|"
            "DATETIME|SMALLDATETIME|DATE|TIME|"
            "MONEY|SMALLMONEY|UNIQUEIDENTIFIER|IMAGE|BINARY|VARBINARY|TIMESTAMP"
        )
        tp_fmt = QTextCharFormat()
        tp_fmt.setForeground(type_color)
        self._rules.append((
            __import__("re").compile(rf"\b({types})\b", __import__("re").I),
            tp_fmt,
        ))

        fns = (
            "COUNT|SUM|AVG|MIN|MAX|UPPER|LOWER|LEN|LTRIM|RTRIM|TRIM|"
            "ISNULL|COALESCE|NULLIF|CAST|CONVERT|GETDATE|NEWID|"
            "CHARINDEX|SUBSTRING|LEFT|RIGHT|REPLACE|STUFF|PATINDEX|"
            "ABS|CEILING|FLOOR|ROUND|POWER|SQRT|RAND|"
            "YEAR|MONTH|DAY|DATEADD|DATEDIFF|DATENAME|DATEPART|"
            "STR|CHAR|ASCII|UNICODE|NCHAR|REPLICATE|SPACE|REVERSE|SOUNDEX"
        )
        fn_fmt = QTextCharFormat()
        fn_fmt.setForeground(fn_color)
        self._rules.append((
            __import__("re").compile(rf"\b({fns})\s*\(", __import__("re").I),
            fn_fmt,
        ))

        # @@globals
        glob_fmt = QTextCharFormat()
        glob_fmt.setForeground(op_color)
        self._rules.append((
            __import__("re").compile(r"@@\w+", __import__("re").I),
            glob_fmt,
        ))

        # @variables
        var_fmt = QTextCharFormat()
        var_fmt.setForeground(QColor("#E06C75") if dark else QColor("#E45649"))
        self._rules.append((
            __import__("re").compile(r"@\w+"),
            var_fmt,
        ))

        # String literals
        str_fmt = QTextCharFormat()
        str_fmt.setForeground(str_color)
        self._rules.append((__import__("re").compile(r"'[^']*'"), str_fmt))

        # Numbers
        num_fmt = QTextCharFormat()
        num_fmt.setForeground(num_color)
        self._rules.append((__import__("re").compile(r"\b\d+(\.\d+)?\b"), num_fmt))

        # Comments
        cmt_fmt = QTextCharFormat()
        cmt_fmt.setForeground(cmt_color)
        cmt_fmt.setFontItalic(True)
        self._rules.append((__import__("re").compile(r"--[^\n]*"), cmt_fmt))

    def highlightBlock(self, text: str) -> None:
        for pattern, fmt in self._rules:
            for m in pattern.finditer(text):
                self.setFormat(m.start(), m.end() - m.start(), fmt)


# ---------------------------------------------------------------------------
# SQL execution thread
# ---------------------------------------------------------------------------

class SQLRunThread(QThread):
    result_ready = Signal(str)
    rows_ready = Signal(list, list)  # column_names, rows

    def __init__(self, session: SQLSession, sql: str, parent=None):
        super().__init__(parent)
        self._session = session
        self._sql = sql

    def run(self) -> None:
        output = self._session.run_script(self._sql)
        self.result_ready.emit(output)
        # Also try to give tabular data for the last SELECT
        try:
            stmts = [s.strip() for s in self._sql.split(";") if s.strip()]
            for stmt in reversed(stmts):
                if stmt.upper().lstrip().startswith("SELECT"):
                    from ..core.sql_engine import _translate_tsql
                    translated = _translate_tsql(stmt)
                    conn = self._session._conn
                    cur = conn.execute(translated)
                    if cur.description:
                        cols = [d[0] for d in cur.description]
                        rows = [list(r) for r in cur.fetchall()]
                        self.rows_ready.emit(cols, rows)
                    break
        except Exception:
            pass


# ---------------------------------------------------------------------------
# Database Tree
# ---------------------------------------------------------------------------

class DatabaseTree(QTreeWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setHeaderLabel("SQL Server (TIMEWARP)")
        self.setMinimumWidth(180)

    def refresh(self, session: Optional[SQLSession] = None) -> None:
        self.clear()
        databases = list_databases()
        for db in databases:
            db_item = QTreeWidgetItem(self, [f"📁 {db}"])
            db_item.setData(0, Qt.UserRole, ("db", db))
            try:
                conn = _open_db(db)
                cur = conn.execute(
                    "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
                )
                tables = [r[0] for r in cur.fetchall()]
                if tables:
                    tbl_root = QTreeWidgetItem(db_item, ["📋 Tables"])
                    for tbl in tables:
                        tbl_item = QTreeWidgetItem(tbl_root, [f"  {tbl}"])
                        tbl_item.setData(0, Qt.UserRole, ("table", db, tbl))
                        # Column sub-items
                        try:
                            cur2 = conn.execute(f'PRAGMA table_info("{tbl}")')
                            for col in cur2.fetchall():
                                col_name, col_type = col[1], col[2] or "TEXT"
                                col_item = QTreeWidgetItem(
                                    tbl_item, [f"    ▸ {col_name}  ({col_type})"]
                                )
                        except Exception:
                            pass
                # Views
                cur_v = conn.execute(
                    "SELECT name FROM sqlite_master WHERE type='view' ORDER BY name"
                )
                views = [r[0] for r in cur_v.fetchall()]
                if views:
                    vw_root = QTreeWidgetItem(db_item, ["👁  Views"])
                    for v in views:
                        QTreeWidgetItem(vw_root, [f"  {v}"])
            except Exception:
                pass
            db_item.setExpanded(True)


# ---------------------------------------------------------------------------
# Main SQL Workbench Panel
# ---------------------------------------------------------------------------

class SQLPanel(QWidget):
    """Dockable SQL Server Workbench panel."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._session = SQLSession()
        self._thread: Optional[SQLRunThread] = None
        self._setup_ui()
        self._db_tree.refresh(self._session)

    # ------------------------------------------------------------------ UI

    def _setup_ui(self) -> None:
        root = QVBoxLayout(self)
        root.setContentsMargins(2, 2, 2, 2)
        root.setSpacing(2)

        # Toolbar
        toolbar = QToolBar()
        toolbar.setMovable(False)
        toolbar.setIconSize(__import__("PySide6.QtCore", fromlist=["QSize"]).QSize(16, 16))

        act_execute = QAction("▶ Execute (F5)", self)
        act_execute.setShortcut(QKeySequence("F5"))
        act_execute.triggered.connect(self._run_sql)
        toolbar.addAction(act_execute)

        toolbar.addSeparator()

        act_new_db = QAction("+ New DB", self)
        act_new_db.triggered.connect(self._new_database)
        toolbar.addAction(act_new_db)

        act_refresh = QAction("⟳ Refresh", self)
        act_refresh.triggered.connect(self._refresh_tree)
        toolbar.addAction(act_refresh)

        toolbar.addSeparator()
        act_clear = QAction("✕ Clear", self)
        act_clear.triggered.connect(self._clear_results)
        toolbar.addAction(act_clear)

        root.addWidget(toolbar)

        # DB selector bar
        db_bar = QHBoxLayout()
        db_bar.addWidget(QLabel("Database:"))
        self._db_label = QLabel(self._session.current_db)
        self._db_label.setStyleSheet("font-weight: bold; color: #61AFEF;")
        db_bar.addWidget(self._db_label)
        db_bar.addStretch()
        root.addLayout(db_bar)

        # Main H-splitter: tree | editor+results
        h_split = QSplitter(Qt.Horizontal)

        # Database tree
        self._db_tree = DatabaseTree()
        self._db_tree.itemDoubleClicked.connect(self._tree_item_activated)
        h_split.addWidget(self._db_tree)

        # Right: editor + results splitter
        v_split = QSplitter(Qt.Vertical)

        # SQL Editor
        self._editor = QTextEdit()
        self._editor.setPlaceholderText(
            "-- Enter T-SQL here (Press F5 or click ▶ Execute)\n"
            "-- SQL Server 2000 compatible T-SQL\n"
            "USE master\nGO\n"
            "SELECT @@VERSION"
        )
        font = QFont("Cascadia Code, Courier New, monospace", 11)
        self._editor.setFont(font)
        self._editor.setStyleSheet(
            "background:#1E1E2E; color:#CDD6F4; "
            "border:1px solid #313244; border-radius:4px;"
        )
        self._highlighter = TSQLHighlighter(self._editor.document(), dark=True)
        v_split.addWidget(self._editor)

        # Results tabs: text + grid
        results_split = QSplitter(Qt.Vertical)

        # Text output
        self._output = QTextEdit()
        self._output.setReadOnly(True)
        self._output.setFont(QFont("Cascadia Code, Courier New, monospace", 10))
        self._output.setStyleSheet(
            "background:#1A1B26; color:#A9B1D6; "
            "border:1px solid #313244; border-radius:4px;"
        )
        results_split.addWidget(self._output)

        # Grid
        self._grid = QTableWidget()
        self._grid.setStyleSheet(
            "QTableWidget { background:#1E1E2E; color:#CDD6F4; "
            "  gridline-color:#313244; }"
            "QHeaderView::section { background:#313244; color:#89B4FA; "
            "  padding:4px; border:1px solid #45475A; }"
        )
        self._grid.setEditTriggers(QTableWidget.NoEditTriggers)
        self._grid.setAlternatingRowColors(True)
        self._grid.hide()
        results_split.addWidget(self._grid)

        v_split.addWidget(results_split)
        v_split.setSizes([300, 200])

        h_split.addWidget(v_split)
        h_split.setSizes([200, 600])

        root.addWidget(h_split)

        # Status bar
        self._status = QLabel("Ready  |  SQL Server 2000 (educational)")
        self._status.setStyleSheet("color:#6C7086; font-size:10px; padding:2px;")
        root.addWidget(self._status)

    # ------------------------------------------------------------------ actions

    def _run_sql(self) -> None:
        sql = self._editor.textCursor().selectedText()
        if not sql.strip():
            sql = self._editor.toPlainText()
        if not sql.strip():
            return
        self._output.setPlainText("⟳ Executing...")
        self._status.setText("Executing...")
        self._grid.hide()

        self._thread = SQLRunThread(self._session, sql)
        self._thread.result_ready.connect(self._on_result)
        self._thread.rows_ready.connect(self._on_rows)
        self._thread.start()

    def _on_result(self, text: str) -> None:
        self._output.setPlainText(text)
        # Update DB label
        self._db_label.setText(self._session.current_db)
        self._status.setText(
            f"Done  |  DB: {self._session.current_db}  |  Rows: {self._session._rowcount}"
        )
        self._refresh_tree()

    def _on_rows(self, cols: List[str], rows: List[list]) -> None:
        self._grid.show()
        self._grid.setColumnCount(len(cols))
        self._grid.setHorizontalHeaderLabels(cols)
        self._grid.setRowCount(len(rows))
        for r, row in enumerate(rows):
            for c, cell in enumerate(row):
                item = QTableWidgetItem(str(cell) if cell is not None else "NULL")
                self._grid.setItem(r, c, item)
        self._grid.resizeColumnsToContents()

    def _new_database(self) -> None:
        name, ok = QInputDialog.getText(self, "New Database", "Database name:")
        if ok and name.strip():
            self._session._exec_statement(f"CREATE DATABASE {name.strip()}")
            self._refresh_tree()
            self._output.setPlainText(
                f"Database '{name.strip()}' created successfully."
            )

    def _refresh_tree(self) -> None:
        self._db_tree.refresh(self._session)

    def _clear_results(self) -> None:
        self._output.clear()
        self._grid.clear()
        self._grid.hide()

    def _tree_item_activated(self, item: QTreeWidgetItem, col: int) -> None:
        data = item.data(0, Qt.UserRole)
        if not data:
            return
        if data[0] == "table":
            _, db, table = data
            self._editor.setPlainText(
                f"USE {db}\nGO\nSELECT TOP 100 * FROM [{table}]"
            )
        elif data[0] == "db":
            _, db = data
            self._editor.setPlainText(f"USE {db}\nGO\nEXEC sp_help")

    # ------------------------------------------------------------------ theme

    def apply_theme(self, theme_name: str) -> None:
        """Apply a basic dark/light theme to the panel."""
        dark = "dark" in theme_name.lower() or theme_name.lower() in (
            "dracula", "monokai", "ocean", "solarized dark"
        )
        self._highlighter = TSQLHighlighter(self._editor.document(), dark=dark)
        if dark:
            bg, fg = "#1E1E2E", "#CDD6F4"
            out_bg, out_fg = "#1A1B26", "#A9B1D6"
        else:
            bg, fg = "#FAFAFA", "#383A42"
            out_bg, out_fg = "#F0F0F0", "#383A42"
        self._editor.setStyleSheet(
            f"background:{bg}; color:{fg}; "
            "border:1px solid #313244; border-radius:4px;"
        )
        self._output.setStyleSheet(
            f"background:{out_bg}; color:{out_fg}; "
            "border:1px solid #313244; border-radius:4px;"
        )

    # ------------------------------------------------------------------ public API

    def get_session(self) -> SQLSession:
        """Return the active SQL session (for language executor injection)."""
        return self._session

    def set_sql(self, sql: str) -> None:
        """Populate the editor with given SQL text."""
        self._editor.setPlainText(sql)
