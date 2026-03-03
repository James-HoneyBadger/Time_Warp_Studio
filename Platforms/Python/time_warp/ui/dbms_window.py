"""
Time Warp Studio — Full Database Management System Window.

Provides a thorough GUI for managing SQL Server 2000-compatible databases
backed by SQLite via the Time Warp sql_engine.

Tabs:
  1. Query Analyzer   — T-SQL editor with syntax highlight + result grid
  2. Object Explorer  — navigation tree: databases, tables, views, procs
  3. Table Designer   — create/alter tables (column editor, PKs, FKs, indexes)
  4. Data Editor      — browse & edit rows in any table, pagination
  5. View Designer    — create/alter views with live SQL preview
  6. Stored Procs     — create/edit stored procedures & user functions
  7. Index Manager    — view, create, drop, rebuild indexes
  8. Users & Roles    — user accounts, role membership, permissions
  9. Backup & Restore — database file backup/restore; export CSV/SQL dump
 10. Query Plan       — EXPLAIN output in a tree/text view
 11. Server Info      — @@VERSION, db properties, statistics
"""

from __future__ import annotations

import csv
import io
import os
import re
from pathlib import Path
from typing import List, Optional

from PySide6.QtCore import (
    QAbstractTableModel, QModelIndex, QSortFilterProxyModel,
    Qt, QThread, Signal, Slot,
)
from PySide6.QtGui import (
    QColor, QFont, QSyntaxHighlighter, QTextCharFormat,
)
from PySide6.QtWidgets import (
    QAbstractItemView, QApplication, QCheckBox, QComboBox,
    QDialog, QDialogButtonBox, QFileDialog, QFormLayout,
    QGroupBox, QHBoxLayout, QHeaderView, QLabel, QLineEdit,
    QMainWindow, QMenu, QMessageBox, QPlainTextEdit,
    QPushButton, QSizePolicy, QSplitter, QStatusBar, QTabWidget,
    QTableView, QTableWidget, QTableWidgetItem, QToolBar,
    QTreeWidget, QTreeWidgetItem, QVBoxLayout, QWidget,
)

# ---------------------------------------------------------------------------
# Local imports
# ---------------------------------------------------------------------------
try:
    from ..core.sql_engine import SQLSession, list_databases, create_session
    from .sql_panel import TSQLHighlighter
except ImportError:
    from Platforms.Python.time_warp.core.sql_engine import (
        SQLSession, list_databases, create_session,
    )
    TSQLHighlighter = None   # type: ignore


# ============================================================================
# Shared helpers
# ============================================================================

def _make_label(text: str, bold: bool = False) -> QLabel:
    lb = QLabel(text)
    if bold:
        f = lb.font()
        f.setBold(True)
        lb.setFont(f)
    return lb


def _mono_font(size: int = 10) -> QFont:
    f = QFont("Courier New", size)
    f.setStyleHint(QFont.TypeWriter)
    f.setFixedPitch(True)
    return f


# ============================================================================
# 1. Query Analyzer Tab
# ============================================================================

class _RunThread(QThread):
    result_ready = Signal(str)
    rows_ready = Signal(list, list)

    def __init__(self, session: SQLSession, sql: str):
        super().__init__()
        self._session = session
        self._sql = sql

    def run(self):
        try:
            # Multi-statement: split on GO
            batches = re.split(r"^\s*GO\s*$", self._sql, flags=re.MULTILINE | re.IGNORECASE)
            out_parts = []
            last_rows: List = []
            last_cols: List = []
            for batch in batches:
                batch = batch.strip()
                if not batch:
                    continue
                stmts = [s.strip() for s in batch.split(";") if s.strip()]
                for stmt in stmts:
                    try:
                        conn = self._session._conn
                        cur = conn.cursor()
                        cur.execute(stmt)
                        if cur.description:
                            last_cols = [d[0] for d in cur.description]
                            last_rows = [list(r) for r in cur.fetchmany(5000)]
                            out_parts.append(
                                f"({len(last_rows)} row(s) returned)"
                            )
                        else:
                            conn.commit()
                            out_parts.append(
                                f"({cur.rowcount} row(s) affected)"
                            )
                    except Exception as e:
                        out_parts.append(f"❌ {e}")
            self.result_ready.emit("\n".join(out_parts))
            if last_rows:
                self.rows_ready.emit(last_cols, last_rows)
        except Exception as e:
            self.result_ready.emit(f"❌ Fatal: {e}")


class _ResultModel(QAbstractTableModel):
    def __init__(self, cols: List[str], rows: List[list]):
        super().__init__()
        self._cols = cols
        self._rows = rows

    def rowCount(self, _=QModelIndex()):
        return len(self._rows)

    def columnCount(self, _=QModelIndex()):
        return len(self._cols)

    def data(self, idx: QModelIndex, role=Qt.DisplayRole):
        if not idx.isValid():
            return None
        if role == Qt.DisplayRole:
            val = self._rows[idx.row()][idx.column()]
            return "" if val is None else str(val)
        if role == Qt.FontRole:
            return _mono_font(9)
        return None

    def headerData(self, section, orientation, role=Qt.DisplayRole):
        if role == Qt.DisplayRole:
            if orientation == Qt.Horizontal:
                return self._cols[section] if section < len(self._cols) else ""
            return str(section + 1)
        return None


class QueryAnalyzerTab(QWidget):
    """Full T-SQL query editor with result grid, execution plan, messages."""

    def __init__(self, get_session, parent=None):
        super().__init__(parent)
        self._get_session = get_session
        self._thread: Optional[_RunThread] = None
        self._build()

    def _build(self):
        layout = QVBoxLayout(self)
        layout.setContentsMargins(4, 4, 4, 4)

        # Toolbar
        tb_row = QHBoxLayout()
        self._db_combo = QComboBox()
        self._db_combo.setToolTip("Select database context")
        self._db_combo.setMinimumWidth(140)
        self._db_combo.currentTextChanged.connect(self._change_db)
        tb_row.addWidget(_make_label("Database:"))
        tb_row.addWidget(self._db_combo)
        tb_row.addSpacing(8)

        run_btn = QPushButton("▶  Execute  (F5)")
        run_btn.setShortcut("F5")
        run_btn.clicked.connect(self._run)
        run_btn.setStyleSheet("font-weight:bold;")
        tb_row.addWidget(run_btn)

        explain_btn = QPushButton("🔍 Query Plan")
        explain_btn.clicked.connect(self._explain)
        tb_row.addWidget(explain_btn)

        fmt_btn = QPushButton("✨ Format SQL")
        fmt_btn.clicked.connect(self._format_sql)
        tb_row.addWidget(fmt_btn)

        clr_btn = QPushButton("🗑 Clear")
        clr_btn.clicked.connect(lambda: self._editor.clear())
        tb_row.addWidget(clr_btn)

        exp_btn = QPushButton("📥 Export CSV")
        exp_btn.clicked.connect(self._export_csv)
        tb_row.addWidget(exp_btn)
        tb_row.addStretch()
        layout.addLayout(tb_row)

        # SQL editor + results splitter
        vsplit = QSplitter(Qt.Vertical)

        # Editor
        editor_frame = QWidget()
        ef_lay = QVBoxLayout(editor_frame)
        ef_lay.setContentsMargins(0, 0, 0, 0)
        self._editor = QPlainTextEdit()
        self._editor.setFont(_mono_font(11))
        self._editor.setPlaceholderText("-- Enter T-SQL here.   F5 = Execute\n"
                                        "SELECT @@VERSION\nGO")
        if TSQLHighlighter:
            self._hl = TSQLHighlighter(self._editor.document())
        ef_lay.addWidget(self._editor)
        vsplit.addWidget(editor_frame)

        # Results tabs
        res_tabs = QTabWidget()
        # Grid
        self._grid = QTableView()
        self._grid.setFont(_mono_font(9))
        self._grid.horizontalHeader().setStretchLastSection(True)
        self._grid.setAlternatingRowColors(True)
        self._grid.setSortingEnabled(True)
        res_tabs.addTab(self._grid, "📊 Results")
        # Messages
        self._messages = QPlainTextEdit()
        self._messages.setReadOnly(True)
        self._messages.setFont(_mono_font(9))
        res_tabs.addTab(self._messages, "💬 Messages")
        # Execution plan
        self._plan_view = QPlainTextEdit()
        self._plan_view.setReadOnly(True)
        self._plan_view.setFont(_mono_font(9))
        res_tabs.addTab(self._plan_view, "🔍 Query Plan")
        vsplit.addWidget(res_tabs)
        vsplit.setSizes([350, 250])
        layout.addWidget(vsplit)

        # Status bar
        self._status = QLabel("Ready")
        layout.addWidget(self._status)

    def refresh_databases(self):
        self._db_combo.blockSignals(True)
        current = self._db_combo.currentText()
        self._db_combo.clear()
        try:
            dbs = list_databases()
        except Exception:
            dbs = ["master"]
        for db in dbs:
            self._db_combo.addItem(db)
        idx = self._db_combo.findText(current)
        if idx >= 0:
            self._db_combo.setCurrentIndex(idx)
        self._db_combo.blockSignals(False)

    def _change_db(self, name: str):
        if not name:
            return
        try:
            sess = self._get_session(name)
            if sess:
                sess.run_statement(f"USE {name}")
        except Exception:
            pass

    def set_sql(self, sql: str):
        self._editor.setPlainText(sql)

    def _run(self):
        sql = self._editor.textCursor().selectedText() or self._editor.toPlainText()
        sql = sql.strip()
        if not sql:
            return
        db = self._db_combo.currentText() or "master"
        sess = self._get_session(db)
        if not sess:
            self._messages.setPlainText("❌ No database session available.")
            return
        self._status.setText("⏳ Executing…")
        self._thread = _RunThread(sess, sql)
        self._thread.result_ready.connect(self._on_messages)
        self._thread.rows_ready.connect(self._on_rows)
        self._thread.finished.connect(lambda: self._status.setText("Done."))
        self._thread.start()

    def _on_messages(self, text: str):
        self._messages.setPlainText(text)

    def _on_rows(self, cols: list, rows: list):
        model = _ResultModel(cols, rows)
        proxy = QSortFilterProxyModel()
        proxy.setSourceModel(model)
        self._grid.setModel(proxy)
        self._grid.resizeColumnsToContents()

    def _explain(self):
        sql = self._editor.toPlainText().strip()
        if not sql:
            return
        db = self._db_combo.currentText() or "master"
        sess = self._get_session(db)
        if not sess:
            return
        try:
            conn = sess._conn
            cur = conn.cursor()
            cur.execute(f"EXPLAIN QUERY PLAN {sql}")
            rows = cur.fetchall()
            text = "\n".join(str(r) for r in rows)
            self._plan_view.setPlainText(text or "(no plan)")
        except Exception as e:
            self._plan_view.setPlainText(f"❌ {e}")

    def _format_sql(self):
        sql = self._editor.toPlainText()
        formatted = self._do_format(sql)
        self._editor.setPlainText(formatted)

    @staticmethod
    def _do_format(sql: str) -> str:
        """Simple T-SQL formatter: uppercase keywords, newlines on clauses."""
        kw = ["SELECT", "FROM", "WHERE", "JOIN", "INNER JOIN", "LEFT JOIN",
              "RIGHT JOIN", "ON", "GROUP BY", "ORDER BY", "HAVING", "UNION",
              "INSERT INTO", "VALUES", "UPDATE", "SET", "DELETE FROM",
              "CREATE TABLE", "ALTER TABLE", "DROP TABLE", "BEGIN", "END",
              "DECLARE", "EXEC", "EXECUTE", "IF", "ELSE", "WHILE"]
        for k in sorted(kw, key=len, reverse=True):
            sql = re.sub(r"\b" + re.escape(k) + r"\b", "\n" + k, sql,
                         flags=re.IGNORECASE)
        return sql.strip()

    def _export_csv(self):
        model = self._grid.model()
        if not model or model.rowCount() == 0:
            QMessageBox.information(self, "Export", "No results to export.")
            return
        path, _ = QFileDialog.getSaveFileName(self, "Export CSV", "", "CSV Files (*.csv)")
        if not path:
            return
        with open(path, "w", newline="", encoding="utf-8") as f:
            w = csv.writer(f)
            cols = [model.headerData(c, Qt.Horizontal) for c in range(model.columnCount())]
            w.writerow(cols)
            for r in range(model.rowCount()):
                row = [model.data(model.index(r, c)) for c in range(model.columnCount())]
                w.writerow(row)
        QMessageBox.information(self, "Export", f"Exported to {path}")


# ============================================================================
# 2. Object Explorer Tree
# ============================================================================

class ObjectExplorer(QWidget):
    """Tree view of all databases → tables/views/procs/triggers."""

    object_activated = Signal(str, str, str)   # (kind, db, object_name)

    def __init__(self, get_session, parent=None):
        super().__init__(parent)
        self._get_session = get_session
        self._build()

    def _build(self):
        lay = QVBoxLayout(self)
        lay.setContentsMargins(2, 2, 2, 2)

        filter_row = QHBoxLayout()
        self._filter = QLineEdit()
        self._filter.setPlaceholderText("Filter objects…")
        self._filter.textChanged.connect(self._apply_filter)
        filter_row.addWidget(self._filter)
        refresh_btn = QPushButton("⟳")
        refresh_btn.setFixedWidth(30)
        refresh_btn.clicked.connect(self.refresh)
        filter_row.addWidget(refresh_btn)
        lay.addLayout(filter_row)

        self._tree = QTreeWidget()
        self._tree.setHeaderLabel("Database Objects")
        self._tree.setFont(_mono_font(9))
        self._tree.itemDoubleClicked.connect(self._on_double_click)
        self._tree.setContextMenuPolicy(Qt.CustomContextMenu)
        self._tree.customContextMenuRequested.connect(self._context_menu)
        lay.addWidget(self._tree)

    def refresh(self):
        self._tree.clear()
        try:
            dbs = list_databases()
        except Exception:
            dbs = ["master"]
        for db in dbs:
            db_item = QTreeWidgetItem([f"🗄 {db}"])
            db_item.setData(0, Qt.UserRole, ("db", db, db))
            self._tree.addTopLevelItem(db_item)
            self._populate_db(db_item, db)
        self._tree.expandToDepth(0)

    def _populate_db(self, parent: QTreeWidgetItem, db: str):
        sess = self._get_session(db)
        if not sess:
            return
        conn = sess._conn
        categories = {
            "📋 Tables":    "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name",
            "👁 Views":     "SELECT name FROM sqlite_master WHERE type='view'  ORDER BY name",
            "⚡ Indexes":   "SELECT name FROM sqlite_master WHERE type='index' ORDER BY name",
            "⚙ Triggers":  "SELECT name FROM sqlite_master WHERE type='trigger' ORDER BY name",
        }
        for cat_label, query in categories.items():
            cat_item = QTreeWidgetItem([cat_label])
            cat_item.setForeground(0, QColor("#6af"))
            parent.addChild(cat_item)
            try:
                cur = conn.cursor()
                cur.execute(query)
                for (name,) in cur.fetchall():
                    kind = cat_label.split()[-1].lower()
                    child = QTreeWidgetItem([name])
                    child.setData(0, Qt.UserRole, (kind, db, name))
                    cat_item.addChild(child)
            except Exception:
                pass

    def _on_double_click(self, item: QTreeWidgetItem, _col: int):
        data = item.data(0, Qt.UserRole)
        if data and data[0] != "db":
            self.object_activated.emit(*data)

    def _context_menu(self, pos):
        item = self._tree.itemAt(pos)
        if not item:
            return
        data = item.data(0, Qt.UserRole)
        if not data:
            return
        kind, db, name = data
        menu = QMenu(self)
        if kind in ("tables", "table"):
            menu.addAction(f"Select top 1000 from {name}",
                           lambda: self.object_activated.emit("select_top", db, name))
            menu.addAction(f"Edit table structure",
                           lambda: self.object_activated.emit("edit_table", db, name))
            menu.addAction(f"Drop table {name}",
                           lambda: self._drop_object(db, "TABLE", name))
        elif kind in ("views", "view"):
            menu.addAction(f"Show view definition",
                           lambda: self.object_activated.emit("view_def", db, name))
            menu.addAction(f"Drop view {name}",
                           lambda: self._drop_object(db, "VIEW", name))
        elif kind in ("indexes", "index"):
            menu.addAction(f"Drop index {name}",
                           lambda: self._drop_object(db, "INDEX", name))
        menu.exec(self._tree.mapToGlobal(pos))

    def _drop_object(self, db: str, kind: str, name: str):
        reply = QMessageBox.question(
            self, f"Drop {kind}",
            f"Drop {kind} [{name}] from database [{db}]?",
            QMessageBox.Yes | QMessageBox.No,
        )
        if reply != QMessageBox.Yes:
            return
        sess = self._get_session(db)
        if sess:
            result = sess.run_statement(f"DROP {kind} IF EXISTS {name}")
            QMessageBox.information(self, "Result", result)
            self.refresh()

    def _apply_filter(self, text: str):
        text = text.lower()
        for i in range(self._tree.topLevelItemCount()):
            db_item = self._tree.topLevelItem(i)
            for ci in range(db_item.childCount()):
                cat = db_item.child(ci)
                visible_any = False
                for oi in range(cat.childCount()):
                    obj = cat.child(oi)
                    match = not text or text in obj.text(0).lower()
                    obj.setHidden(not match)
                    if match:
                        visible_any = True
                cat.setHidden(not visible_any)


# ============================================================================
# 3. Table Designer
# ============================================================================

class TableDesigner(QWidget):
    """Visual table creation/modification tool."""

    def __init__(self, get_session, parent=None):
        super().__init__(parent)
        self._get_session = get_session
        self._editing_table = ""
        self._build()

    def _build(self):
        lay = QVBoxLayout(self)
        form = QFormLayout()

        self._db_combo = QComboBox()
        form.addRow("Database:", self._db_combo)
        self._table_name = QLineEdit()
        self._table_name.setPlaceholderText("new_table")
        form.addRow("Table Name:", self._table_name)
        lay.addLayout(form)

        # Column grid
        col_label = _make_label("Columns:", bold=True)
        lay.addWidget(col_label)
        self._col_table = QTableWidget(0, 6)
        self._col_table.setHorizontalHeaderLabels(
            ["Column Name", "Data Type", "Length", "Not Null", "PK", "Default"]
        )
        self._col_table.horizontalHeader().setStretchLastSection(True)
        self._col_table.setFont(_mono_font(9))
        lay.addWidget(self._col_table)

        # Add/remove column row
        btn_row = QHBoxLayout()
        add_col = QPushButton("+ Add Column")
        add_col.clicked.connect(self._add_column_row)
        btn_row.addWidget(add_col)
        del_col = QPushButton("✕ Remove Selected")
        del_col.clicked.connect(self._remove_column_row)
        btn_row.addWidget(del_col)
        btn_row.addStretch()
        lay.addLayout(btn_row)

        # Generate SQL preview
        self._sql_preview = QPlainTextEdit()
        self._sql_preview.setFont(_mono_font(10))
        self._sql_preview.setReadOnly(True)
        self._sql_preview.setMaximumHeight(120)
        lay.addWidget(_make_label("Generated SQL:", bold=True))
        lay.addWidget(self._sql_preview)

        # Action buttons
        action_row = QHBoxLayout()
        gen_btn = QPushButton("🔄 Generate SQL")
        gen_btn.clicked.connect(self._generate_sql)
        action_row.addWidget(gen_btn)
        apply_btn = QPushButton("✅ Apply (Execute)")
        apply_btn.clicked.connect(self._apply)
        apply_btn.setStyleSheet("font-weight:bold;")
        action_row.addWidget(apply_btn)
        action_row.addStretch()
        lay.addLayout(action_row)

        # Start with 3 default rows
        for _ in range(3):
            self._add_column_row()
        # Default pk row
        self._col_table.setItem(0, 0, QTableWidgetItem("id"))
        self._col_table.setItem(0, 1, QTableWidgetItem("INT"))
        self._col_table.setItem(0, 4, QTableWidgetItem("PK"))

    def refresh_databases(self):
        self._db_combo.clear()
        try:
            for db in list_databases():
                self._db_combo.addItem(db)
        except Exception:
            pass

    def load_table(self, db: str, table: str):
        """Load an existing table structure for editing."""
        self._db_combo.setCurrentText(db)
        self._table_name.setText(table)
        self._editing_table = table
        sess = self._get_session(db)
        if not sess:
            return
        conn = sess._conn
        cur = conn.cursor()
        try:
            cur.execute(f"PRAGMA table_info({table})")
            rows = cur.fetchall()
            self._col_table.setRowCount(0)
            for r in rows:
                self._add_column_row()
                row_idx = self._col_table.rowCount() - 1
                self._col_table.setItem(row_idx, 0, QTableWidgetItem(r[1]))   # name
                self._col_table.setItem(row_idx, 1, QTableWidgetItem(r[2]))   # type
                self._col_table.setItem(row_idx, 3, QTableWidgetItem("Y" if r[3] else ""))
                self._col_table.setItem(row_idx, 4, QTableWidgetItem("PK" if r[5] else ""))
                self._col_table.setItem(row_idx, 5, QTableWidgetItem(str(r[4]) if r[4] else ""))
        except Exception as e:
            QMessageBox.warning(self, "Load Table", str(e))

    def _add_column_row(self):
        row = self._col_table.rowCount()
        self._col_table.insertRow(row)
        # Data type combo
        type_combo = QComboBox()
        for t in ["INT", "BIGINT", "SMALLINT", "TINYINT", "BIT",
                  "DECIMAL", "NUMERIC", "FLOAT", "REAL", "MONEY",
                  "VARCHAR", "NVARCHAR", "CHAR", "NCHAR", "TEXT", "NTEXT",
                  "DATETIME", "DATE", "TIME", "TIMESTAMP",
                  "BINARY", "VARBINARY", "IMAGE", "UNIQUEIDENTIFIER"]:
            type_combo.addItem(t)
        self._col_table.setCellWidget(row, 1, type_combo)
        self._col_table.setItem(row, 2, QTableWidgetItem("50"))
        cb_nn = QCheckBox()
        self._col_table.setCellWidget(row, 3, cb_nn)
        cb_pk = QCheckBox()
        self._col_table.setCellWidget(row, 4, cb_pk)
        self._col_table.setItem(row, 5, QTableWidgetItem(""))

    def _remove_column_row(self):
        rows = set(i.row() for i in self._col_table.selectedItems())
        for r in sorted(rows, reverse=True):
            self._col_table.removeRow(r)

    def _generate_sql(self) -> str:
        cols = []
        pks = []
        for r in range(self._col_table.rowCount()):
            name_item = self._col_table.item(r, 0)
            if not name_item or not name_item.text().strip():
                continue
            name = name_item.text().strip()
            type_w = self._col_table.cellWidget(r, 1)
            dtype = type_w.currentText() if type_w else "VARCHAR"
            len_item = self._col_table.item(r, 2)
            length = len_item.text().strip() if len_item else ""
            nn_w = self._col_table.cellWidget(r, 3)
            not_null = " NOT NULL" if isinstance(nn_w, QCheckBox) and nn_w.isChecked() else ""
            pk_w = self._col_table.cellWidget(r, 4)
            is_pk = isinstance(pk_w, QCheckBox) and pk_w.isChecked()
            def_item = self._col_table.item(r, 5)
            default = f" DEFAULT {def_item.text().strip()}" if def_item and def_item.text().strip() else ""
            full_type = f"{dtype}({length})" if length and dtype in ("VARCHAR", "NVARCHAR", "CHAR", "DECIMAL") else dtype
            if dtype in ("INT", "BIGINT") and is_pk:
                col_def = f"  [{name}] {full_type} IDENTITY(1,1) NOT NULL"
            else:
                col_def = f"  [{name}] {full_type}{not_null}{default}"
            cols.append(col_def)
            if is_pk:
                pks.append(f"[{name}]")
        if pks:
            cols.append(f"  CONSTRAINT [PK_{self._table_name.text()}] PRIMARY KEY ({', '.join(pks)})")
        tname = self._table_name.text().strip() or "new_table"
        if self._editing_table:
            # ALTER TABLE … we just regenerate as DROP+CREATE for simplicity
            sql = (f"-- Recreate table (backup data first!)\n"
                   f"DROP TABLE IF EXISTS [{self._editing_table}];\n"
                   f"CREATE TABLE [{tname}] (\n" + ",\n".join(cols) + "\n);")
        else:
            sql = f"CREATE TABLE [{tname}] (\n" + ",\n".join(cols) + "\n);"
        self._sql_preview.setPlainText(sql)
        return sql

    def _apply(self):
        sql = self._generate_sql()
        db = self._db_combo.currentText()
        sess = self._get_session(db)
        if not sess:
            QMessageBox.warning(self, "No Session", "Select a database first.")
            return
        result = sess.run_script(sql)
        QMessageBox.information(self, "Result", result or "Done.")


# ============================================================================
# 4. Data Editor (row-level browse & edit)
# ============================================================================

class DataEditorTab(QWidget):
    """Browse, edit, insert, delete rows in any table."""

    PAGE_SIZE = 500

    def __init__(self, get_session, parent=None):
        super().__init__(parent)
        self._get_session = get_session
        self._page = 0
        self._total_rows = 0
        self._cols: List[str] = []
        self._current_db = ""
        self._current_table = ""
        self._build()

    def _build(self):
        lay = QVBoxLayout(self)
        nav = QHBoxLayout()
        self._db_combo = QComboBox()
        self._db_combo.currentTextChanged.connect(self._load_tables)
        nav.addWidget(_make_label("DB:"))
        nav.addWidget(self._db_combo)
        self._tbl_combo = QComboBox()
        self._tbl_combo.setMinimumWidth(160)
        nav.addWidget(_make_label("Table:"))
        nav.addWidget(self._tbl_combo)
        load_btn = QPushButton("Load")
        load_btn.clicked.connect(self._load_data)
        nav.addWidget(load_btn)
        self._where_edit = QLineEdit()
        self._where_edit.setPlaceholderText("WHERE clause (optional)")
        nav.addWidget(_make_label("WHERE:"))
        nav.addWidget(self._where_edit)
        nav.addStretch()
        lay.addLayout(nav)

        self._table = QTableWidget()
        self._table.setFont(_mono_font(9))
        self._table.setAlternatingRowColors(True)
        self._table.horizontalHeader().setStretchLastSection(True)
        self._table.itemChanged.connect(self._on_cell_changed)
        lay.addWidget(self._table)

        # Pagination + action buttons
        btn_row = QHBoxLayout()
        self._prev_btn = QPushButton("◀ Prev")
        self._prev_btn.clicked.connect(self._prev_page)
        btn_row.addWidget(self._prev_btn)
        self._page_label = QLabel("Page 1")
        btn_row.addWidget(self._page_label)
        self._next_btn = QPushButton("Next ▶")
        self._next_btn.clicked.connect(self._next_page)
        btn_row.addWidget(self._next_btn)
        btn_row.addSpacing(20)
        add_row_btn = QPushButton("➕ New Row")
        add_row_btn.clicked.connect(self._add_row)
        btn_row.addWidget(add_row_btn)
        del_row_btn = QPushButton("🗑 Delete Row(s)")
        del_row_btn.clicked.connect(self._delete_rows)
        btn_row.addWidget(del_row_btn)
        commit_btn = QPushButton("💾 Commit Changes")
        commit_btn.setStyleSheet("font-weight:bold;")
        commit_btn.clicked.connect(self._commit)
        btn_row.addWidget(commit_btn)
        btn_row.addStretch()
        lay.addLayout(btn_row)

        self._status = QLabel("")
        lay.addWidget(self._status)
        self._pending_changes: dict = {}

    def refresh_databases(self):
        self._db_combo.blockSignals(True)
        self._db_combo.clear()
        try:
            for db in list_databases():
                self._db_combo.addItem(db)
        except Exception:
            pass
        self._db_combo.blockSignals(False)
        if self._db_combo.count():
            self._load_tables(self._db_combo.itemText(0))

    def _load_tables(self, db: str):
        self._tbl_combo.clear()
        if not db:
            return
        sess = self._get_session(db)
        if not sess:
            return
        conn = sess._conn
        cur = conn.cursor()
        cur.execute("SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")
        for (name,) in cur.fetchall():
            self._tbl_combo.addItem(name)

    def load_table(self, db: str, table: str):
        self._db_combo.setCurrentText(db)
        self._load_tables(db)
        self._tbl_combo.setCurrentText(table)
        self._load_data()

    def _load_data(self):
        db = self._db_combo.currentText()
        table = self._tbl_combo.currentText()
        if not db or not table:
            return
        self._current_db = db
        self._current_table = table
        self._pending_changes.clear()
        sess = self._get_session(db)
        if not sess:
            return
        conn = sess._conn
        where = self._where_edit.text().strip()
        where_clause = f" WHERE {where}" if where else ""
        cur = conn.cursor()
        try:
            cur.execute(f"SELECT COUNT(*) FROM [{table}]{where_clause}")
            self._total_rows = cur.fetchone()[0]
            offset = self._page * self.PAGE_SIZE
            cur.execute(f"SELECT * FROM [{table}]{where_clause} LIMIT {self.PAGE_SIZE} OFFSET {offset}")
            self._cols = [d[0] for d in cur.description]
            rows = cur.fetchall()
            self._table.blockSignals(True)
            self._table.setColumnCount(len(self._cols))
            self._table.setRowCount(len(rows))
            self._table.setHorizontalHeaderLabels(self._cols)
            for ri, row in enumerate(rows):
                for ci, val in enumerate(row):
                    it = QTableWidgetItem("" if val is None else str(val))
                    it.setData(Qt.UserRole, val)
                    self._table.setItem(ri, ci, it)
            self._table.blockSignals(False)
            self._table.resizeColumnsToContents()
            pages = max(1, (self._total_rows + self.PAGE_SIZE - 1) // self.PAGE_SIZE)
            self._page_label.setText(f"Page {self._page + 1}/{pages}  ({self._total_rows} rows)")
            self._status.setText(f"Loaded {len(rows)} rows from [{table}]")
        except Exception as e:
            self._status.setText(f"❌ {e}")

    def _on_cell_changed(self, item: QTableWidgetItem):
        key = (item.row(), item.column())
        self._pending_changes[key] = item.text()

    def _prev_page(self):
        if self._page > 0:
            self._page -= 1
            self._load_data()

    def _next_page(self):
        max_page = (self._total_rows - 1) // self.PAGE_SIZE
        if self._page < max_page:
            self._page += 1
            self._load_data()

    def _add_row(self):
        r = self._table.rowCount()
        self._table.insertRow(r)
        for c in range(len(self._cols)):
            self._table.setItem(r, c, QTableWidgetItem(""))

    def _delete_rows(self):
        rows_to_del = sorted(
            set(i.row() for i in self._table.selectedItems()), reverse=True
        )
        if not rows_to_del:
            return
        sess = self._get_session(self._current_db)
        if not sess:
            return
        conn = sess._conn
        for row in rows_to_del:
            # Build DELETE using rowid-like first column value
            if self._table.item(row, 0):
                pk_col = self._cols[0]
                pk_val = self._table.item(row, 0).text()
                sql = f"DELETE FROM [{self._current_table}] WHERE [{pk_col}] = ?"
                conn.execute(sql, (pk_val,))
            self._table.removeRow(row)
        conn.commit()
        self._status.setText(f"Deleted {len(rows_to_del)} row(s).")

    def _commit(self):
        if not self._pending_changes:
            self._status.setText("Nothing to commit.")
            return
        sess = self._get_session(self._current_db)
        if not sess:
            return
        conn = sess._conn
        errors = []
        pk_col = self._cols[0] if self._cols else None
        for (ri, ci), value in self._pending_changes.items():
            item_pk = self._table.item(ri, 0)
            if not item_pk or not pk_col:
                continue
            pk_val = item_pk.data(Qt.UserRole) or item_pk.text()
            col_name = self._cols[ci]
            sql = f"UPDATE [{self._current_table}] SET [{col_name}] = ? WHERE [{pk_col}] = ?"
            try:
                conn.execute(sql, (value, pk_val))
            except Exception as e:
                errors.append(str(e))
        conn.commit()
        self._pending_changes.clear()
        if errors:
            self._status.setText("⚠ Partial commit: " + "; ".join(errors))
        else:
            self._status.setText("✅ All changes committed.")


# ============================================================================
# 5. View Designer
# ============================================================================

class ViewDesignerTab(QWidget):
    def __init__(self, get_session, parent=None):
        super().__init__(parent)
        self._get_session = get_session
        self._build()

    def _build(self):
        lay = QVBoxLayout(self)
        f = QFormLayout()
        self._db_combo = QComboBox()
        f.addRow("Database:", self._db_combo)
        self._view_name = QLineEdit()
        self._view_name.setPlaceholderText("vw_my_view")
        f.addRow("View Name:", self._view_name)
        lay.addLayout(f)
        lay.addWidget(_make_label("SELECT Statement:", bold=True))
        self._editor = QPlainTextEdit()
        self._editor.setFont(_mono_font(11))
        self._editor.setPlainText("SELECT\n    t.column1,\n    t.column2\nFROM [table] t\nWHERE t.active = 1")
        if TSQLHighlighter:
            TSQLHighlighter(self._editor.document())
        lay.addWidget(self._editor)
        btn_row = QHBoxLayout()
        preview_btn = QPushButton("▶ Preview")
        preview_btn.clicked.connect(self._preview)
        btn_row.addWidget(preview_btn)
        save_btn = QPushButton("💾 Save View")
        save_btn.clicked.connect(self._save)
        btn_row.addWidget(save_btn)
        drop_btn = QPushButton("🗑 Drop View")
        drop_btn.clicked.connect(self._drop)
        btn_row.addWidget(drop_btn)
        btn_row.addStretch()
        lay.addLayout(btn_row)
        self._result = QPlainTextEdit()
        self._result.setReadOnly(True)
        self._result.setFont(_mono_font(9))
        self._result.setMaximumHeight(120)
        lay.addWidget(self._result)

    def refresh_databases(self):
        self._db_combo.clear()
        try:
            for db in list_databases():
                self._db_combo.addItem(db)
        except Exception:
            pass

    def load_view(self, db: str, view: str):
        self._db_combo.setCurrentText(db)
        self._view_name.setText(view)
        sess = self._get_session(db)
        if not sess:
            return
        cur = sess._conn.cursor()
        cur.execute("SELECT sql FROM sqlite_master WHERE type='view' AND name=?", (view,))
        row = cur.fetchone()
        if row:
            # Extract SELECT part
            sql = row[0]
            m = re.search(r"AS\s+(.+)", sql, re.DOTALL | re.IGNORECASE)
            if m:
                self._editor.setPlainText(m.group(1).strip())

    def _preview(self):
        db = self._db_combo.currentText()
        sess = self._get_session(db)
        if not sess:
            return
        sql = f"SELECT * FROM ({self._editor.toPlainText()}) LIMIT 100"
        result = sess.run_statement(sql)
        self._result.setPlainText(result)

    def _save(self):
        db = self._db_combo.currentText()
        name = self._view_name.text().strip()
        if not name:
            return
        sess = self._get_session(db)
        if not sess:
            return
        sql = f"CREATE OR REPLACE VIEW [{name}] AS\n{self._editor.toPlainText()}"
        result = sess.run_statement(sql)
        self._result.setPlainText(result or "View saved.")

    def _drop(self):
        db = self._db_combo.currentText()
        name = self._view_name.text().strip()
        if not name:
            return
        if QMessageBox.question(self, "Drop View", f"Drop view [{name}]?") != QMessageBox.Yes:
            return
        sess = self._get_session(db)
        if sess:
            result = sess.run_statement(f"DROP VIEW IF EXISTS [{name}]")
            self._result.setPlainText(result or "Dropped.")


# ============================================================================
# 6. Stored Procedures / User Functions
# ============================================================================

class StoredProcTab(QWidget):
    def __init__(self, get_session, parent=None):
        super().__init__(parent)
        self._get_session = get_session
        self._build()

    def _build(self):
        lay = QHBoxLayout(self)
        # Left: list of procs
        left = QVBoxLayout()
        self._db_combo = QComboBox()
        self._db_combo.currentTextChanged.connect(self._load_procs)
        left.addWidget(_make_label("Database:"))
        left.addWidget(self._db_combo)
        self._proc_list = QTreeWidget()
        self._proc_list.setHeaderLabel("Stored Procedures")
        self._proc_list.itemClicked.connect(self._load_proc)
        left.addWidget(self._proc_list)
        new_btn = QPushButton("+ New Procedure")
        new_btn.clicked.connect(self._new_proc)
        left.addWidget(new_btn)
        lay.addLayout(left, 1)

        # Right: editor
        right = QVBoxLayout()
        right.addWidget(_make_label("Procedure Body:", bold=True))
        self._editor = QPlainTextEdit()
        self._editor.setFont(_mono_font(11))
        if TSQLHighlighter:
            TSQLHighlighter(self._editor.document())
        right.addWidget(self._editor)
        btn_row = QHBoxLayout()
        save_btn = QPushButton("💾 Save")
        save_btn.clicked.connect(self._save)
        btn_row.addWidget(save_btn)
        exec_btn = QPushButton("▶ Execute")
        exec_btn.clicked.connect(self._execute)
        btn_row.addWidget(exec_btn)
        drop_btn = QPushButton("🗑 Drop")
        drop_btn.clicked.connect(self._drop)
        btn_row.addWidget(drop_btn)
        btn_row.addStretch()
        right.addLayout(btn_row)
        self._output = QPlainTextEdit()
        self._output.setReadOnly(True)
        self._output.setFont(_mono_font(9))
        self._output.setMaximumHeight(100)
        right.addWidget(self._output)
        lay.addLayout(right, 3)

    def refresh_databases(self):
        self._db_combo.clear()
        try:
            for db in list_databases():
                self._db_combo.addItem(db)
        except Exception:
            pass

    def _load_procs(self, db: str):
        self._proc_list.clear()
        if not db:
            return
        # SQL Server stores procs in sys.objects; we simulate with a tw_procs table
        sess = self._get_session(db)
        if not sess:
            return
        conn = sess._conn
        # Create proc table if not exists
        conn.execute(
            "CREATE TABLE IF NOT EXISTS _tw_procs "
            "(name TEXT PRIMARY KEY, body TEXT, created TEXT)"
        )
        conn.commit()
        cur = conn.cursor()
        cur.execute("SELECT name FROM _tw_procs ORDER BY name")
        for (name,) in cur.fetchall():
            QTreeWidgetItem(self._proc_list, [f"⚙ {name}"])

    def _load_proc(self, item: QTreeWidgetItem, _col):
        name = item.text(0).lstrip("⚙ ")
        db = self._db_combo.currentText()
        sess = self._get_session(db)
        if not sess:
            return
        cur = sess._conn.cursor()
        cur.execute("SELECT body FROM _tw_procs WHERE name=?", (name,))
        row = cur.fetchone()
        if row:
            self._editor.setPlainText(row[0])

    def _new_proc(self):
        self._editor.setPlainText(
            "CREATE PROCEDURE sp_my_proc\n    @param1 INT,\n    @param2 VARCHAR(50)\nAS\nBEGIN\n"
            "    SET NOCOUNT ON;\n    SELECT @param1 AS value, @param2 AS label;\nEND\nGO"
        )

    def _save(self):
        db = self._db_combo.currentText()
        body = self._editor.toPlainText().strip()
        import re
        m = re.search(r"CREATE\s+PROCEDURE\s+(\w+)", body, re.IGNORECASE)
        if not m:
            QMessageBox.warning(self, "Save", "Cannot detect procedure name.")
            return
        name = m.group(1)
        sess = self._get_session(db)
        if not sess:
            return
        import datetime
        conn = sess._conn
        conn.execute(
            "INSERT OR REPLACE INTO _tw_procs (name, body, created) VALUES (?,?,?)",
            (name, body, str(datetime.datetime.now()))
        )
        conn.commit()
        self._load_procs(db)
        self._output.setPlainText(f"✅ Procedure [{name}] saved.")

    def _execute(self):
        db = self._db_combo.currentText()
        sess = self._get_session(db)
        if not sess:
            return
        body = self._editor.toPlainText()
        result = sess.run_script(body)
        self._output.setPlainText(result)

    def _drop(self):
        db = self._db_combo.currentText()
        body = self._editor.toPlainText()
        m = re.search(r"CREATE\s+PROCEDURE\s+(\w+)", body, re.IGNORECASE)
        name = m.group(1) if m else ""
        if not name:
            return
        if QMessageBox.question(self, "Drop", f"Drop procedure [{name}]?") != QMessageBox.Yes:
            return
        sess = self._get_session(db)
        if sess:
            sess._conn.execute("DELETE FROM _tw_procs WHERE name=?", (name,))
            sess._conn.commit()
            self._load_procs(db)
            self._output.setPlainText(f"Dropped [{name}].")


# ============================================================================
# 7. Index Manager
# ============================================================================

class IndexManagerTab(QWidget):
    def __init__(self, get_session, parent=None):
        super().__init__(parent)
        self._get_session = get_session
        self._build()

    def _build(self):
        lay = QVBoxLayout(self)
        top = QHBoxLayout()
        self._db_combo = QComboBox()
        self._db_combo.currentTextChanged.connect(self._load_indexes)
        top.addWidget(_make_label("Database:"))
        top.addWidget(self._db_combo)
        refresh_btn = QPushButton("⟳ Refresh")
        refresh_btn.clicked.connect(lambda: self._load_indexes(self._db_combo.currentText()))
        top.addWidget(refresh_btn)
        top.addStretch()
        lay.addLayout(top)

        self._idx_table = QTableWidget(0, 5)
        self._idx_table.setHorizontalHeaderLabels(
            ["Index Name", "Table", "Columns", "Unique", "Actions"]
        )
        self._idx_table.horizontalHeader().setStretchLastSection(True)
        self._idx_table.setFont(_mono_font(9))
        lay.addWidget(self._idx_table)

        # Create index form
        grp = QGroupBox("Create New Index")
        gf = QFormLayout(grp)
        self._idx_name = QLineEdit()
        gf.addRow("Name:", self._idx_name)
        self._idx_table_combo = QComboBox()
        gf.addRow("Table:", self._idx_table_combo)
        self._idx_cols = QLineEdit()
        self._idx_cols.setPlaceholderText("col1, col2")
        gf.addRow("Columns:", self._idx_cols)
        self._idx_unique = QCheckBox("UNIQUE")
        gf.addRow("", self._idx_unique)
        create_btn = QPushButton("➕ Create Index")
        create_btn.clicked.connect(self._create_index)
        gf.addRow("", create_btn)
        lay.addWidget(grp)

        self._status = QLabel("")
        lay.addWidget(self._status)

    def refresh_databases(self):
        self._db_combo.clear()
        try:
            for db in list_databases():
                self._db_combo.addItem(db)
        except Exception:
            pass

    def _load_indexes(self, db: str):
        self._idx_table.setRowCount(0)
        self._idx_table_combo.clear()
        if not db:
            return
        sess = self._get_session(db)
        if not sess:
            return
        conn = sess._conn
        cur = conn.cursor()
        cur.execute("SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")
        tables = [r[0] for r in cur.fetchall()]
        for t in tables:
            self._idx_table_combo.addItem(t)
        cur.execute(
            "SELECT name, tbl_name, sql FROM sqlite_master WHERE type='index' ORDER BY tbl_name, name"
        )
        for (iname, tbl, isql) in cur.fetchall():
            r = self._idx_table.rowCount()
            self._idx_table.insertRow(r)
            self._idx_table.setItem(r, 0, QTableWidgetItem(iname or ""))
            self._idx_table.setItem(r, 1, QTableWidgetItem(tbl or ""))
            cols = ""
            if isql:
                m = re.search(r"\((.+)\)", isql)
                cols = m.group(1) if m else ""
            self._idx_table.setItem(r, 2, QTableWidgetItem(cols))
            unique = "YES" if isql and "UNIQUE" in isql.upper() else "NO"
            self._idx_table.setItem(r, 3, QTableWidgetItem(unique))
            drop_btn = QPushButton("Drop")
            drop_btn.clicked.connect(lambda chk=False, n=iname, d=db: self._drop_index(d, n))
            self._idx_table.setCellWidget(r, 4, drop_btn)

    def _create_index(self):
        db = self._db_combo.currentText()
        name = self._idx_name.text().strip()
        table = self._idx_table_combo.currentText()
        cols = self._idx_cols.text().strip()
        if not (name and table and cols):
            self._status.setText("❌ Fill in Name, Table, and Columns.")
            return
        unique = "UNIQUE " if self._idx_unique.isChecked() else ""
        sql = f"CREATE {unique}INDEX [{name}] ON [{table}] ({cols})"
        sess = self._get_session(db)
        if sess:
            result = sess.run_statement(sql)
            self._status.setText(result or "Index created.")
            self._load_indexes(db)

    def _drop_index(self, db: str, name: str):
        if QMessageBox.question(self, "Drop Index", f"Drop index [{name}]?") != QMessageBox.Yes:
            return
        sess = self._get_session(db)
        if sess:
            result = sess.run_statement(f"DROP INDEX IF EXISTS [{name}]")
            self._status.setText(result or "Dropped.")
            self._load_indexes(db)


# ============================================================================
# 8. Users & Roles (simulated)
# ============================================================================

class UsersRolesTab(QWidget):
    def __init__(self, get_session, parent=None):
        super().__init__(parent)
        self._get_session = get_session
        self._build()

    def _build(self):
        lay = QHBoxLayout(self)
        # Users list
        ul = QVBoxLayout()
        ul.addWidget(_make_label("Users:", bold=True))
        self._user_list = QTreeWidget()
        self._user_list.setHeaderLabel("Login / User")
        ul.addWidget(self._user_list)
        add_btn = QPushButton("+ Add User")
        add_btn.clicked.connect(self._add_user_dialog)
        ul.addWidget(add_btn)
        del_btn = QPushButton("🗑 Remove")
        del_btn.clicked.connect(self._remove_user)
        ul.addWidget(del_btn)
        lay.addLayout(ul, 1)

        # Permissions panel
        rp = QVBoxLayout()
        rp.addWidget(_make_label("Permissions:", bold=True))
        self._perm_table = QTableWidget(0, 3)
        self._perm_table.setHorizontalHeaderLabels(["Object", "Permission", "Grant"])
        rp.addWidget(self._perm_table)
        self._roles_label = QLabel("Roles: db_datareader, db_datawriter, db_owner")
        self._roles_label.setWordWrap(True)
        rp.addWidget(self._roles_label)
        rp.addWidget(QLabel("(Simulated security model – educational purposes)"))
        lay.addLayout(rp, 2)
        self._populate_users()

    def _populate_users(self):
        self._user_list.clear()
        sa = QTreeWidgetItem(["sa  (sysadmin)"])
        sa.setForeground(0, QColor("#fa0"))
        self._user_list.addTopLevelItem(sa)
        for u in ["dbo", "guest", "timeuser"]:
            QTreeWidgetItem(self._user_list, [u])

    def _add_user_dialog(self):
        dlg = QDialog(self)
        dlg.setWindowTitle("Add User")
        f = QFormLayout(dlg)
        name_edit = QLineEdit()
        f.addRow("Username:", name_edit)
        pwd_edit = QLineEdit()
        pwd_edit.setEchoMode(QLineEdit.Password)
        f.addRow("Password:", pwd_edit)
        role_combo = QComboBox()
        role_combo.addItems(["db_datareader", "db_datawriter", "db_owner", "sysadmin"])
        f.addRow("Role:", role_combo)
        bb = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        bb.accepted.connect(dlg.accept)
        bb.rejected.connect(dlg.reject)
        f.addRow(bb)
        if dlg.exec() == QDialog.Accepted and name_edit.text():
            name = name_edit.text()
            role = role_combo.currentText()
            QTreeWidgetItem(self._user_list, [f"{name}  ({role})"])

    def _remove_user(self):
        item = self._user_list.currentItem()
        if item:
            root = self._user_list.invisibleRootItem()
            root.removeChild(item)

    def refresh_databases(self):
        pass   # users are global in this sim


# ============================================================================
# 9. Backup & Restore
# ============================================================================

class BackupRestoreTab(QWidget):
    def __init__(self, get_session, parent=None):
        super().__init__(parent)
        self._get_session = get_session
        self._build()

    def _build(self):
        lay = QVBoxLayout(self)
        lay.addWidget(_make_label("Database Backup & Restore", bold=True))

        # Backup group
        bg = QGroupBox("Backup Database")
        bf = QFormLayout(bg)
        self._bk_db = QComboBox()
        bf.addRow("Database:", self._bk_db)
        self._bk_path = QLineEdit()
        self._bk_path.setPlaceholderText("/path/to/backup.sql")
        bk_browse = QPushButton("Browse…")
        bk_browse.clicked.connect(self._browse_backup)
        bk_row = QHBoxLayout()
        bk_row.addWidget(self._bk_path)
        bk_row.addWidget(bk_browse)
        bf.addRow("Backup File:", bk_row)
        bk_type = QComboBox()
        bk_type.addItems(["SQL Dump", "CSV (one file per table)", "SQLite .db copy"])
        bf.addRow("Format:", bk_type)
        self._bk_type = bk_type
        bk_btn = QPushButton("💾 Backup Now")
        bk_btn.clicked.connect(self._backup)
        bf.addRow("", bk_btn)
        lay.addWidget(bg)

        # Restore group
        rg = QGroupBox("Restore Database")
        rf = QFormLayout(rg)
        self._rs_db = QComboBox()
        rf.addRow("Target Database:", self._rs_db)
        self._rs_path = QLineEdit()
        rs_browse = QPushButton("Browse…")
        rs_browse.clicked.connect(self._browse_restore)
        rs_row = QHBoxLayout()
        rs_row.addWidget(self._rs_path)
        rs_row.addWidget(rs_browse)
        rf.addRow("Restore File:", rs_row)
        rs_btn = QPushButton("🔄 Restore Now")
        rs_btn.clicked.connect(self._restore)
        rf.addRow("", rs_btn)
        lay.addWidget(rg)

        self._log = QPlainTextEdit()
        self._log.setReadOnly(True)
        self._log.setFont(_mono_font(9))
        lay.addWidget(_make_label("Log:", bold=True))
        lay.addWidget(self._log)

    def refresh_databases(self):
        for combo in (self._bk_db, self._rs_db):
            combo.clear()
            try:
                for db in list_databases():
                    combo.addItem(db)
            except Exception:
                pass

    def _browse_backup(self):
        path, _ = QFileDialog.getSaveFileName(
            self, "Backup To", str(Path.home()), "SQL Files (*.sql);;All (*)")
        if path:
            self._bk_path.setText(path)

    def _browse_restore(self):
        path, _ = QFileDialog.getOpenFileName(
            self, "Restore From", str(Path.home()), "SQL Files (*.sql);;All (*)")
        if path:
            self._rs_path.setText(path)

    def _backup(self):
        db = self._bk_db.currentText()
        path = self._bk_path.text().strip()
        if not db or not path:
            self._log.appendPlainText("❌ Specify database and path.")
            return
        sess = self._get_session(db)
        if not sess:
            return
        conn = sess._conn
        fmt = self._bk_type.currentText()
        try:
            if "CSV" in fmt:
                base = path.rstrip(".sql")
                cur = conn.cursor()
                cur.execute("SELECT name FROM sqlite_master WHERE type='table'")
                tables = [r[0] for r in cur.fetchall()]
                for tbl in tables:
                    cur.execute(f"SELECT * FROM [{tbl}]")
                    cols = [d[0] for d in cur.description]
                    rows = cur.fetchall()
                    tbl_path = f"{base}_{tbl}.csv"
                    with open(tbl_path, "w", newline="") as f:
                        w = csv.writer(f)
                        w.writerow(cols)
                        w.writerows(rows)
                self._log.appendPlainText(f"✅ CSV export: {len(tables)} tables → {base}_*.csv")
            elif "SQLite" in fmt:
                import shutil
                db_dir = Path.home() / ".Time_Warp" / "databases"
                src = db_dir / f"{db}.db"
                if src.exists():
                    shutil.copy2(str(src), path)
                    self._log.appendPlainText(f"✅ SQLite copy: {src} → {path}")
                else:
                    self._log.appendPlainText(f"❌ DB file not found: {src}")
            else:
                # SQL dump
                lines = []
                cur = conn.cursor()
                cur.execute("SELECT name, sql FROM sqlite_master WHERE sql IS NOT NULL ORDER BY type, name")
                for (name, sql) in cur.fetchall():
                    lines.append(f"-- Object: {name}")
                    lines.append(sql + ";")
                    lines.append("")
                # Data
                cur.execute("SELECT name FROM sqlite_master WHERE type='table'")
                for (tbl,) in cur.fetchall():
                    cur2 = conn.cursor()
                    cur2.execute(f"SELECT * FROM [{tbl}]")
                    cols = [d[0] for d in cur2.description]
                    for row in cur2.fetchall():
                        vals = ", ".join(
                            "NULL" if v is None else f"'{str(v).replace(chr(39), chr(39)*2)}'"
                            for v in row
                        )
                        lines.append(f"INSERT INTO [{tbl}] ({','.join(cols)}) VALUES ({vals});")
                with open(path, "w", encoding="utf-8") as f:
                    f.write("\n".join(lines))
                self._log.appendPlainText(f"✅ SQL dump → {path}")
        except Exception as e:
            self._log.appendPlainText(f"❌ Backup failed: {e}")

    def _restore(self):
        db = self._rs_db.currentText()
        path = self._rs_path.text().strip()
        if not path or not db:
            self._log.appendPlainText("❌ Specify target DB and restore file.")
            return
        if not Path(path).exists():
            self._log.appendPlainText(f"❌ File not found: {path}")
            return
        sess = self._get_session(db)
        if not sess:
            return
        try:
            with open(path, encoding="utf-8") as f:
                sql = f.read()
            result = sess.run_script(sql)
            self._log.appendPlainText(f"✅ Restore complete.\n{result[:500]}")
        except Exception as e:
            self._log.appendPlainText(f"❌ Restore failed: {e}")


# ============================================================================
# 10. Server Info / Statistics
# ============================================================================

class ServerInfoTab(QWidget):
    def __init__(self, get_session, parent=None):
        super().__init__(parent)
        self._get_session = get_session
        self._build()

    def _build(self):
        lay = QVBoxLayout(self)
        hdr = QHBoxLayout()
        self._db_combo = QComboBox()
        hdr.addWidget(_make_label("Database:"))
        hdr.addWidget(self._db_combo)
        refresh_btn = QPushButton("⟳ Refresh")
        refresh_btn.clicked.connect(self._refresh)
        hdr.addWidget(refresh_btn)
        hdr.addStretch()
        lay.addLayout(hdr)

        self._info = QPlainTextEdit()
        self._info.setReadOnly(True)
        self._info.setFont(_mono_font(10))
        lay.addWidget(self._info)

    def refresh_databases(self):
        self._db_combo.clear()
        try:
            for db in list_databases():
                self._db_combo.addItem(db)
        except Exception:
            pass
        self._refresh()

    def _refresh(self):
        db = self._db_combo.currentText()
        sess = self._get_session(db)
        lines = []
        lines.append(sess.run_statement("SELECT @@VERSION") if sess else "(no session)")
        lines.append("")
        if sess:
            conn = sess._conn
            cur = conn.cursor()
            # Table stats
            cur.execute("SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")
            tables = [r[0] for r in cur.fetchall()]
            lines.append(f"Database: {db}   Tables: {len(tables)}")
            lines.append("-" * 60)
            lines.append(f"{'Table':<30} {'Rows':>8} {'Est.Size':>12}")
            lines.append("-" * 60)
            for tbl in tables:
                try:
                    cur.execute(f"SELECT COUNT(*) FROM [{tbl}]")
                    cnt = cur.fetchone()[0]
                    cur.execute(f"PRAGMA table_info([{tbl}])")
                    ncols = len(cur.fetchall())
                    est_size = cnt * ncols * 20   # rough estimate in bytes
                    lines.append(f"{tbl:<30} {cnt:>8,} {est_size:>10,}B")
                except Exception:
                    lines.append(f"{tbl:<30} {'?':>8}")
            # SQLite version
            cur.execute("SELECT sqlite_version()")
            sqlite_ver = cur.fetchone()[0]
            lines.append("")
            lines.append(f"SQLite engine: {sqlite_ver}")
            db_dir = Path.home() / ".Time_Warp" / "databases"
            db_file = db_dir / f"{db}.db"
            if db_file.exists():
                sz = db_file.stat().st_size
                lines.append(f"File: {db_file}  ({sz:,} bytes)")
        self._info.setPlainText("\n".join(lines))


# ============================================================================
# Main DBMS Window
# ============================================================================

class DBMSWindow(QMainWindow):
    """
    Comprehensive database management window with 10 functional tabs.
    Used standalone or embedded in Time Warp Studio.
    """

    def __init__(self, interpreter=None, parent=None):
        super().__init__(parent)
        self._interpreter = interpreter
        self._sessions: dict[str, SQLSession] = {}
        self.setWindowTitle("Time Warp Studio — Database Manager (SQL Server 2000)")
        self.setMinimumSize(1200, 750)
        self._build()
        self.refresh_all()

    def _get_session(self, db: str = "master") -> Optional[SQLSession]:
        """Get or create a SQLSession for the named database."""
        if not db:
            db = "master"
        if db not in self._sessions:
            try:
                self._sessions[db] = create_session(db)
            except Exception:
                return None
        # Also update interpreter's sql_session
        if self._interpreter is not None:
            self._interpreter.sql_session = self._sessions[db]
        return self._sessions[db]

    def _build(self):
        # Toolbar
        tb = QToolBar("DBMS")
        tb.setMovable(False)
        tb.addAction("⟳ Refresh All", self.refresh_all)
        tb.addSeparator()
        tb.addAction("➕ New Database", self._new_database)
        tb.addAction("🗑 Drop Database", self._drop_database)
        tb.addSeparator()
        tb.addAction("📤 Quick Export", self._quick_export)
        tb.addAction("📥 Quick Import", self._quick_import)
        tb.addSeparator()
        tb.addAction("❓ sp_help", self._sp_help)
        tb.addAction("📊 sp_helpdb", self._sp_helpdb)
        self.addToolBar(tb)

        # Central tab widget
        self._tabs = QTabWidget()
        self._tabs.setDocumentMode(True)

        # Construct all tabs
        self.query_tab    = QueryAnalyzerTab(self._get_session)
        self.obj_explorer = ObjectExplorer(self._get_session)
        self.table_designer = TableDesigner(self._get_session)
        self.data_editor   = DataEditorTab(self._get_session)
        self.view_designer = ViewDesignerTab(self._get_session)
        self.proc_tab      = StoredProcTab(self._get_session)
        self.index_tab     = IndexManagerTab(self._get_session)
        self.users_tab     = UsersRolesTab(self._get_session)
        self.backup_tab    = BackupRestoreTab(self._get_session)
        self.info_tab      = ServerInfoTab(self._get_session)

        self._all_subtabs = [
            self.query_tab, self.table_designer, self.data_editor,
            self.view_designer, self.proc_tab, self.index_tab,
            self.users_tab, self.backup_tab, self.info_tab,
        ]

        # Left splitter: object explorer + main tabs
        splitter = QSplitter(Qt.Horizontal)
        splitter.addWidget(self.obj_explorer)
        splitter.addWidget(self._tabs)
        splitter.setSizes([230, 970])
        self.setCentralWidget(splitter)

        self._tabs.addTab(self.query_tab,      "📝 Query Analyzer")
        self._tabs.addTab(self.table_designer, "🔧 Table Designer")
        self._tabs.addTab(self.data_editor,    "📋 Data Editor")
        self._tabs.addTab(self.view_designer,  "👁 View Designer")
        self._tabs.addTab(self.proc_tab,       "⚙ Stored Procs")
        self._tabs.addTab(self.index_tab,      "⚡ Index Manager")
        self._tabs.addTab(self.users_tab,      "👤 Users & Roles")
        self._tabs.addTab(self.backup_tab,     "💾 Backup/Restore")
        self._tabs.addTab(self.info_tab,       "ℹ Server Info")

        # Status bar
        self._sb = QStatusBar()
        self._sb.showMessage("DBMS ready — SQL Server 2000 simulation")
        self.setStatusBar(self._sb)

        # Wire explorer to tabs
        self.obj_explorer.object_activated.connect(self._on_object_activated)

    def refresh_all(self):
        for tab in self._all_subtabs:
            if hasattr(tab, "refresh_databases"):
                tab.refresh_databases()
        self.obj_explorer.refresh()
        self.query_tab.refresh_databases()

    def _new_database(self):
        name, ok = _input_dialog(self, "New Database", "Database name:")
        if ok and name:
            sess = self._get_session(name)
            if sess:
                self._sb.showMessage(f"Database [{name}] created.")
                self.refresh_all()

    def _drop_database(self):
        dbs = list_databases()
        if not dbs:
            return
        from PySide6.QtWidgets import QInputDialog
        db, ok = QInputDialog.getItem(
            self, "Drop Database", "Select database to drop:", dbs, 0, False
        )
        if not ok or not db or db == "master":
            return
        reply = QMessageBox.warning(
            self, "Drop Database",
            f"⚠ ALL DATA in [{db}] will be destroyed!\nContinue?",
            QMessageBox.Yes | QMessageBox.No,
        )
        if reply != QMessageBox.Yes:
            return
        db_dir = Path.home() / ".Time_Warp" / "databases"
        db_file = db_dir / f"{db}.db"
        if db_file.exists():
            db_file.unlink()
        self._sessions.pop(db, None)
        self._sb.showMessage(f"Database [{db}] dropped.")
        self.refresh_all()

    def _quick_export(self):
        self._tabs.setCurrentWidget(self.backup_tab)

    def _quick_import(self):
        self._tabs.setCurrentWidget(self.backup_tab)

    def _sp_help(self):
        sess = self._get_session("master")
        if sess:
            result = sess.run_statement("EXEC sp_help")
            self.query_tab._messages.setPlainText(result)
            self._tabs.setCurrentWidget(self.query_tab)

    def _sp_helpdb(self):
        sess = self._get_session("master")
        if sess:
            result = sess.run_statement("EXEC sp_helpdb")
            self.query_tab._messages.setPlainText(result)
            self._tabs.setCurrentWidget(self.query_tab)

    def _on_object_activated(self, kind: str, db: str, name: str):
        if kind == "select_top":
            sql = f"SELECT TOP 1000 * FROM [{name}]"
            self.query_tab.set_sql(sql)
            self.query_tab._db_combo.setCurrentText(db)
            self._tabs.setCurrentWidget(self.query_tab)
        elif kind == "edit_table":
            self.table_designer.load_table(db, name)
            self._tabs.setCurrentWidget(self.table_designer)
        elif kind == "view_def":
            self.view_designer.load_view(db, name)
            self._tabs.setCurrentWidget(self.view_designer)
        elif kind == "table":
            self.data_editor.load_table(db, name)
            self._tabs.setCurrentWidget(self.data_editor)

    def apply_theme(self, theme_name: str):
        """Apply IDE theme to all sub-panels."""
        dark = theme_name.lower() not in ("spring", "candy", "paper")
        bg = "#1e1e1e" if dark else "#f5f5f5"
        fg = "#d4d4d4" if dark else "#1e1e1e"
        stylebase = f"background:{bg}; color:{fg};"
        for tab in self._all_subtabs:
            tab.setStyleSheet(stylebase)


def _input_dialog(parent, title: str, label: str) -> tuple[str, bool]:
    from PySide6.QtWidgets import QInputDialog
    return QInputDialog.getText(parent, title, label)
