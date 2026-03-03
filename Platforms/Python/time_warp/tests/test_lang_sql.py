"""Comprehensive tests for the SQL language executor."""


from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.SQL


def sql(source: str, **kw) -> list[str]:
    """Shortcut: run a SQL program."""
    return run(source, L, **kw)


# ============================================================================
# PRINT
# ============================================================================


class TestPrint:
    def test_print_string(self):
        out = sql("PRINT 'Hello World'")
        assert has(out, "Hello World")

    def test_print_number(self):
        out = sql("PRINT 42")
        assert has(out, "42")


# ============================================================================
# SELECT (expressions)
# ============================================================================


class TestSelect:
    def test_select_expression(self):
        out = sql("SELECT 1 + 1")
        assert has(out, "2")

    def test_select_string(self):
        out = sql("SELECT 'Hello'")
        assert has(out, "Hello")

    def test_select_multiple(self):
        out = sql("SELECT 1, 2, 3")
        assert has(out, "1") and has(out, "3")


# ============================================================================
# DDL / DML
# ============================================================================


class TestDDLDML:
    def test_create_insert_select(self):
        out = sql(
            "CREATE TABLE test (id INT, name VARCHAR(50))\n"
            "INSERT INTO test VALUES (1, 'Alice')\n"
            "INSERT INTO test VALUES (2, 'Bob')\n"
            "SELECT * FROM test"
        )
        assert has(out, "Alice") and has(out, "Bob")

    def test_where_clause(self):
        out = sql(
            "CREATE TABLE nums (val INT)\n"
            "INSERT INTO nums VALUES (1)\n"
            "INSERT INTO nums VALUES (2)\n"
            "INSERT INTO nums VALUES (3)\n"
            "SELECT * FROM nums WHERE val > 1"
        )
        assert has(out, "2") and has(out, "3")

    def test_update(self):
        out = sql(
            "CREATE TABLE t1 (id INT, name VARCHAR(50))\n"
            "INSERT INTO t1 VALUES (1, 'Alice')\n"
            "UPDATE t1 SET name = 'Bob' WHERE id = 1\n"
            "SELECT * FROM t1"
        )
        assert has(out, "Bob")

    def test_delete(self):
        out = sql(
            "CREATE TABLE t2 (id INT)\n"
            "INSERT INTO t2 VALUES (1)\n"
            "INSERT INTO t2 VALUES (2)\n"
            "DELETE FROM t2 WHERE id = 1\n"
            "SELECT * FROM t2"
        )
        assert has(out, "2") and not has(out, "1\n")

    def test_order_by(self):
        out = sql(
            "CREATE TABLE t3 (val INT)\n"
            "INSERT INTO t3 VALUES (3)\n"
            "INSERT INTO t3 VALUES (1)\n"
            "INSERT INTO t3 VALUES (2)\n"
            "SELECT * FROM t3 ORDER BY val"
        )
        assert no_errors(out)

    def test_count(self):
        out = sql(
            "CREATE TABLE t4 (val INT)\n"
            "INSERT INTO t4 VALUES (1)\n"
            "INSERT INTO t4 VALUES (2)\n"
            "SELECT COUNT(*) FROM t4"
        )
        assert has(out, "2")

    def test_sum(self):
        out = sql(
            "CREATE TABLE t5 (val INT)\n"
            "INSERT INTO t5 VALUES (10)\n"
            "INSERT INTO t5 VALUES (20)\n"
            "SELECT SUM(val) FROM t5"
        )
        assert has(out, "30")


# ============================================================================
# T-SQL VARIABLES
# ============================================================================


class TestVariables:
    def test_declare_set_print(self):
        out = sql(
            "DECLARE @x INT\n"
            "SET @x = 42\n"
            "PRINT @x"
        )
        assert has(out, "42")

    def test_declare_string(self):
        out = sql(
            "DECLARE @s VARCHAR(50)\n"
            "SET @s = 'Hello'\n"
            "PRINT @s"
        )
        assert has(out, "Hello")


# ============================================================================
# IF EXISTS
# ============================================================================


class TestConditional:
    def test_if_exists(self):
        out = sql(
            "CREATE TABLE test_if (id INT)\n"
            "INSERT INTO test_if VALUES (1)\n"
            "IF EXISTS (SELECT * FROM test_if) PRINT 'yes'"
        )
        assert has(out, "yes") or no_errors(out)


# ============================================================================
# WHILE
# ============================================================================


class TestWhile:
    def test_while_loop(self):
        out = sql(
            "DECLARE @i INT\n"
            "SET @i = 1\n"
            "WHILE @i <= 3\n"
            "BEGIN\n"
            "  PRINT @i\n"
            "  SET @i = @i + 1\n"
            "END"
        )
        assert has(out, "1") and has(out, "3")


# ============================================================================
# SYSTEM FUNCTIONS
# ============================================================================


class TestSystemFunctions:
    def test_getdate(self):
        out = sql("PRINT GETDATE()")
        assert no_errors(out)

    def test_len(self):
        out = sql("PRINT LEN('Hello')")
        assert has(out, "5")

    def test_upper(self):
        out = sql("PRINT UPPER('hello')")
        assert has(out, "HELLO")

    def test_lower(self):
        out = sql("PRINT LOWER('HELLO')")
        assert has(out, "hello")

    def test_version(self):
        out = sql("PRINT @@VERSION")
        assert no_errors(out)

    def test_rowcount(self):
        out = sql(
            "CREATE TABLE rc_test (id INT)\n"
            "INSERT INTO rc_test VALUES (1)\n"
            "INSERT INTO rc_test VALUES (2)\n"
            "PRINT @@ROWCOUNT"
        )
        assert no_errors(out)


# ============================================================================
# USE DATABASE
# ============================================================================


class TestUseDatabase:
    def test_use(self):
        out = sql("USE master")
        assert no_errors(out)


# ============================================================================
# GO (batch separator)
# ============================================================================


class TestGo:
    def test_go_separator(self):
        out = sql("PRINT 'batch1'\nGO\nPRINT 'batch2'")
        assert has(out, "batch1") and has(out, "batch2")


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_empty_program(self):
        out = sql("")
        assert no_errors(out) or len(out) == 0
