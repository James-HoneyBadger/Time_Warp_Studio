"""Comprehensive tests for all 24 language demo programs in Examples/.

Each test loads the corresponding demo file and runs it through the interpreter,
validating that:
1. The program produces output (no total failure)
2. No error lines are present
3. Key output fragments appear (language features work)
"""

from __future__ import annotations

import pathlib

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

EXAMPLES = pathlib.Path(__file__).resolve().parents[4] / "Examples"


# ── Helpers ──────────────────────────────────────────────────────────────────

def _load(lang_dir: str, filename: str) -> str:
    """Read an example file and return its contents."""
    path = EXAMPLES / lang_dir / filename
    assert path.exists(), f"Demo file not found: {path}"
    return path.read_text(encoding="utf-8")


def _run_demo(lang_dir: str, filename: str, language: Language,
              input_val: str = "4") -> list[str]:
    """Load and run a demo program, returning output lines."""
    source = _load(lang_dir, filename)
    return run(source, language, input_val=input_val)


# ── BASIC ────────────────────────────────────────────────────────────────────

class TestBasicDemo:
    def test_runs(self):
        out = _run_demo("basic", "hello.bas", Language.BASIC)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("basic", "hello.bas", Language.BASIC)
        assert has(out, "HELLO WORLD", "Welcome to BASIC")

    def test_variables(self):
        out = _run_demo("basic", "hello.bas", Language.BASIC)
        assert has(out, "VARIABLES")

    def test_for_loop(self):
        out = _run_demo("basic", "hello.bas", Language.BASIC)
        assert has(out, "FOR LOOP")

    def test_conditionals(self):
        out = _run_demo("basic", "hello.bas", Language.BASIC)
        assert has(out, "CONDITIONALS")

    def test_arrays(self):
        out = _run_demo("basic", "hello.bas", Language.BASIC)
        assert has(out, "ARRAYS", "100", "200", "300")

    def test_done(self):
        out = _run_demo("basic", "hello.bas", Language.BASIC)
        assert has(out, "DONE")

    def test_no_errors(self):
        out = _run_demo("basic", "hello.bas", Language.BASIC)
        assert no_errors(out)


# ── PILOT ────────────────────────────────────────────────────────────────────

class TestPilotDemo:
    def test_runs(self):
        out = _run_demo("pilot", "hello.pilot", Language.PILOT, input_val="YES")
        assert ok(out)

    def test_hello(self):
        out = _run_demo("pilot", "hello.pilot", Language.PILOT, input_val="YES")
        assert has(out, "HELLO WORLD", "Welcome to PILOT")

    def test_variables(self):
        out = _run_demo("pilot", "hello.pilot", Language.PILOT, input_val="YES")
        assert has(out, "VARIABLES")

    def test_jumps(self):
        out = _run_demo("pilot", "hello.pilot", Language.PILOT, input_val="YES")
        assert has(out, "After jump")

    def test_subroutines(self):
        out = _run_demo("pilot", "hello.pilot", Language.PILOT, input_val="YES")
        assert has(out, "subroutine")

    def test_done(self):
        out = _run_demo("pilot", "hello.pilot", Language.PILOT, input_val="YES")
        assert has(out, "DONE")

    def test_no_errors(self):
        out = _run_demo("pilot", "hello.pilot", Language.PILOT, input_val="YES")
        assert no_errors(out)


# ── Logo ─────────────────────────────────────────────────────────────────────

class TestLogoDemo:
    def test_runs(self):
        out = _run_demo("logo", "hello.logo", Language.LOGO)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("logo", "hello.logo", Language.LOGO)
        assert has(out, "HELLO WORLD")

    def test_variables(self):
        out = _run_demo("logo", "hello.logo", Language.LOGO)
        assert has(out, "VARIABLES", "10")

    def test_loops(self):
        out = _run_demo("logo", "hello.logo", Language.LOGO)
        assert has(out, "LOOPS")

    def test_procedures(self):
        out = _run_demo("logo", "hello.logo", Language.LOGO)
        assert has(out, "PROCEDURES")

    def test_done(self):
        out = _run_demo("logo", "hello.logo", Language.LOGO)
        assert has(out, "DONE")


# ── C ────────────────────────────────────────────────────────────────────────

class TestCDemo:
    def test_runs(self):
        out = _run_demo("c", "hello.c", Language.C)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("c", "hello.c", Language.C)
        assert has(out, "HELLO WORLD", "Welcome to C")

    def test_variables(self):
        out = _run_demo("c", "hello.c", Language.C)
        assert has(out, "VARIABLES", "10")

    def test_arithmetic(self):
        out = _run_demo("c", "hello.c", Language.C)
        assert has(out, "ARITHMETIC", "13", "30")

    def test_loops(self):
        out = _run_demo("c", "hello.c", Language.C)
        assert has(out, "FOR LOOP", "1", "5")

    def test_strings(self):
        out = _run_demo("c", "hello.c", Language.C)
        assert has(out, "STRINGS")

    def test_done(self):
        out = _run_demo("c", "hello.c", Language.C)
        assert has(out, "DONE")

    def test_no_errors(self):
        out = _run_demo("c", "hello.c", Language.C)
        assert no_errors(out)


# ── Pascal ───────────────────────────────────────────────────────────────────

class TestPascalDemo:
    def test_runs(self):
        out = _run_demo("pascal", "hello.pas", Language.PASCAL)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("pascal", "hello.pas", Language.PASCAL)
        assert has(out, "HELLO WORLD", "Hello from Pascal")

    def test_variables(self):
        out = _run_demo("pascal", "hello.pas", Language.PASCAL)
        assert has(out, "VARIABLES", "10")

    def test_loops(self):
        out = _run_demo("pascal", "hello.pas", Language.PASCAL)
        assert has(out, "FOR LOOP")

    def test_procedures(self):
        out = _run_demo("pascal", "hello.pas", Language.PASCAL)
        assert has(out, "PROCEDURES", "Hello")

    def test_functions(self):
        out = _run_demo("pascal", "hello.pas", Language.PASCAL)
        assert has(out, "FUNCTIONS", "10")

    def test_done(self):
        out = _run_demo("pascal", "hello.pas", Language.PASCAL)
        assert has(out, "DONE")

    def test_no_errors(self):
        out = _run_demo("pascal", "hello.pas", Language.PASCAL)
        assert no_errors(out)


# ── Prolog ───────────────────────────────────────────────────────────────────

class TestPrologDemo:
    def test_runs(self):
        out = _run_demo("prolog", "hello.pl", Language.PROLOG)
        assert ok(out)

    def test_facts(self):
        out = _run_demo("prolog", "hello.pl", Language.PROLOG)
        assert has(out, "FACTS")

    def test_arithmetic(self):
        out = _run_demo("prolog", "hello.pl", Language.PROLOG)
        assert has(out, "ARITHMETIC")

    def test_lists(self):
        out = _run_demo("prolog", "hello.pl", Language.PROLOG)
        assert has(out, "LISTS")

    def test_done(self):
        out = _run_demo("prolog", "hello.pl", Language.PROLOG)
        assert has(out, "DONE")


# ── Forth ────────────────────────────────────────────────────────────────────

class TestForthDemo:
    def test_runs(self):
        out = _run_demo("forth", "hello.f", Language.FORTH)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("forth", "hello.f", Language.FORTH)
        assert has(out, "HELLO WORLD", "Welcome to Forth")

    def test_arithmetic(self):
        out = _run_demo("forth", "hello.f", Language.FORTH)
        assert has(out, "ARITHMETIC", "7")

    def test_word_definitions(self):
        out = _run_demo("forth", "hello.f", Language.FORTH)
        assert has(out, "WORD DEFINITIONS", "25")

    def test_variables(self):
        out = _run_demo("forth", "hello.f", Language.FORTH)
        assert has(out, "VARIABLES", "42")

    def test_done(self):
        out = _run_demo("forth", "hello.f", Language.FORTH)
        assert has(out, "DONE")


# ── Python ───────────────────────────────────────────────────────────────────

class TestPythonDemo:
    def test_runs(self):
        out = _run_demo("python", "hello.py", Language.PYTHON)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("python", "hello.py", Language.PYTHON)
        assert has(out, "HELLO WORLD", "Welcome to Python")

    def test_variables(self):
        out = _run_demo("python", "hello.py", Language.PYTHON)
        assert has(out, "VARIABLES")

    def test_lists(self):
        out = _run_demo("python", "hello.py", Language.PYTHON)
        assert has(out, "LISTS")

    def test_functions(self):
        out = _run_demo("python", "hello.py", Language.PYTHON)
        assert has(out, "FUNCTIONS", "120")

    def test_classes(self):
        out = _run_demo("python", "hello.py", Language.PYTHON)
        assert has(out, "CLASSES", "Rex says Woof")

    def test_done(self):
        out = _run_demo("python", "hello.py", Language.PYTHON)
        assert has(out, "DONE")

    def test_no_errors(self):
        out = _run_demo("python", "hello.py", Language.PYTHON)
        assert no_errors(out)


# ── Lua ──────────────────────────────────────────────────────────────────────

class TestLuaDemo:
    def test_runs(self):
        out = _run_demo("lua", "hello.lua", Language.LUA)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("lua", "hello.lua", Language.LUA)
        assert has(out, "HELLO WORLD", "Welcome to Lua")

    def test_strings(self):
        out = _run_demo("lua", "hello.lua", Language.LUA)
        assert has(out, "STRINGS")

    def test_tables(self):
        out = _run_demo("lua", "hello.lua", Language.LUA)
        assert has(out, "TABLES", "apple")

    def test_functions(self):
        out = _run_demo("lua", "hello.lua", Language.LUA)
        assert has(out, "FUNCTIONS", "120")

    def test_math(self):
        out = _run_demo("lua", "hello.lua", Language.LUA)
        assert has(out, "MATH")

    def test_done(self):
        out = _run_demo("lua", "hello.lua", Language.LUA)
        assert has(out, "DONE")

    def test_no_errors(self):
        out = _run_demo("lua", "hello.lua", Language.LUA)
        assert no_errors(out)


# ── Scheme ───────────────────────────────────────────────────────────────────

class TestSchemeDemo:
    def test_runs(self):
        out = _run_demo("scheme", "hello.scm", Language.SCHEME)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("scheme", "hello.scm", Language.SCHEME)
        assert has(out, "HELLO WORLD", "Welcome to Scheme")

    def test_arithmetic(self):
        out = _run_demo("scheme", "hello.scm", Language.SCHEME)
        assert has(out, "ARITHMETIC", "13", "1024")

    def test_functions(self):
        out = _run_demo("scheme", "hello.scm", Language.SCHEME)
        assert has(out, "FUNCTIONS", "120")

    def test_lists(self):
        out = _run_demo("scheme", "hello.scm", Language.SCHEME)
        assert has(out, "LISTS")

    def test_done(self):
        out = _run_demo("scheme", "hello.scm", Language.SCHEME)
        assert has(out, "DONE")

    def test_no_errors(self):
        out = _run_demo("scheme", "hello.scm", Language.SCHEME)
        assert no_errors(out)


# ── COBOL ────────────────────────────────────────────────────────────────────

class TestCobolDemo:
    def test_runs(self):
        out = _run_demo("cobol", "hello.cob", Language.COBOL)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("cobol", "hello.cob", Language.COBOL)
        assert has(out, "HELLO WORLD", "Welcome to COBOL")

    def test_variables(self):
        out = _run_demo("cobol", "hello.cob", Language.COBOL)
        assert has(out, "VARIABLES", "COBOL")

    def test_arithmetic(self):
        out = _run_demo("cobol", "hello.cob", Language.COBOL)
        assert has(out, "ARITHMETIC")

    def test_conditionals(self):
        out = _run_demo("cobol", "hello.cob", Language.COBOL)
        assert has(out, "CONDITIONALS")

    def test_done(self):
        out = _run_demo("cobol", "hello.cob", Language.COBOL)
        assert has(out, "DONE")


# ── Brainfuck ────────────────────────────────────────────────────────────────

class TestBrainfuckDemo:
    def test_runs(self):
        out = _run_demo("brainfuck", "hello.bf", Language.BRAINFUCK)
        assert ok(out)

    def test_output(self):
        out = _run_demo("brainfuck", "hello.bf", Language.BRAINFUCK)
        assert has(out, "Hello World")


# ── Assembly ─────────────────────────────────────────────────────────────────

class TestAssemblyDemo:
    def test_runs(self):
        out = _run_demo("assembly", "hello.asm", Language.ASSEMBLY)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("assembly", "hello.asm", Language.ASSEMBLY)
        assert has(out, "HELLO WORLD", "Welcome to Assembly")

    def test_arithmetic(self):
        out = _run_demo("assembly", "hello.asm", Language.ASSEMBLY)
        assert has(out, "ARITHMETIC")

    def test_conditionals(self):
        out = _run_demo("assembly", "hello.asm", Language.ASSEMBLY)
        assert has(out, "CONDITIONALS", "equal")

    def test_subroutines(self):
        out = _run_demo("assembly", "hello.asm", Language.ASSEMBLY)
        assert has(out, "SUBROUTINES")

    def test_done(self):
        out = _run_demo("assembly", "hello.asm", Language.ASSEMBLY)
        assert has(out, "DONE")

    def test_no_errors(self):
        out = _run_demo("assembly", "hello.asm", Language.ASSEMBLY)
        assert no_errors(out)


# ── JavaScript ───────────────────────────────────────────────────────────────

class TestJavaScriptDemo:
    def test_runs(self):
        out = _run_demo("javascript", "hello.js", Language.JAVASCRIPT)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("javascript", "hello.js", Language.JAVASCRIPT)
        assert has(out, "HELLO WORLD", "Hello from JavaScript")

    def test_variables(self):
        out = _run_demo("javascript", "hello.js", Language.JAVASCRIPT)
        assert has(out, "VARIABLES")

    def test_arrays(self):
        out = _run_demo("javascript", "hello.js", Language.JAVASCRIPT)
        assert has(out, "ARRAYS")

    def test_classes(self):
        out = _run_demo("javascript", "hello.js", Language.JAVASCRIPT)
        assert has(out, "CLASSES", "Rex says Woof")

    def test_done(self):
        out = _run_demo("javascript", "hello.js", Language.JAVASCRIPT)
        assert has(out, "DONE")

    def test_no_errors(self):
        out = _run_demo("javascript", "hello.js", Language.JAVASCRIPT)
        assert no_errors(out)


# ── FORTRAN ──────────────────────────────────────────────────────────────────

class TestFortranDemo:
    def test_runs(self):
        out = _run_demo("fortran", "hello.f77", Language.FORTRAN)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("fortran", "hello.f77", Language.FORTRAN)
        assert has(out, "HELLO WORLD", "Welcome to FORTRAN")

    def test_variables(self):
        out = _run_demo("fortran", "hello.f77", Language.FORTRAN)
        assert has(out, "VARIABLES")

    def test_arithmetic(self):
        out = _run_demo("fortran", "hello.f77", Language.FORTRAN)
        assert has(out, "ARITHMETIC")

    def test_subroutine(self):
        out = _run_demo("fortran", "hello.f77", Language.FORTRAN)
        assert has(out, "SUBROUTINE", "Hello from subroutine")

    def test_done(self):
        out = _run_demo("fortran", "hello.f77", Language.FORTRAN)
        assert has(out, "DONE")


# ── REXX ─────────────────────────────────────────────────────────────────────

class TestRexxDemo:
    def test_runs(self):
        out = _run_demo("rexx", "hello.rex", Language.REXX)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("rexx", "hello.rex", Language.REXX)
        assert has(out, "HELLO WORLD", "Welcome to REXX")

    def test_strings(self):
        out = _run_demo("rexx", "hello.rex", Language.REXX)
        assert has(out, "STRINGS", "5")

    def test_loops(self):
        out = _run_demo("rexx", "hello.rex", Language.REXX)
        assert has(out, "DO LOOP")

    def test_procedures(self):
        out = _run_demo("rexx", "hello.rex", Language.REXX)
        assert has(out, "PROCEDURES", "Hello from procedure")

    def test_done(self):
        out = _run_demo("rexx", "hello.rex", Language.REXX)
        assert has(out, "DONE")


# ── Smalltalk ────────────────────────────────────────────────────────────────

class TestSmalltalkDemo:
    def test_runs(self):
        out = _run_demo("smalltalk", "hello.st", Language.SMALLTALK)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("smalltalk", "hello.st", Language.SMALLTALK)
        assert has(out, "HELLO WORLD", "Welcome to Smalltalk")

    def test_arithmetic(self):
        out = _run_demo("smalltalk", "hello.st", Language.SMALLTALK)
        assert has(out, "VARIABLES")

    def test_conditionals(self):
        out = _run_demo("smalltalk", "hello.st", Language.SMALLTALK)
        assert has(out, "CONDITIONALS")

    def test_loops(self):
        out = _run_demo("smalltalk", "hello.st", Language.SMALLTALK)
        assert has(out, "LOOPS")

    def test_done(self):
        out = _run_demo("smalltalk", "hello.st", Language.SMALLTALK)
        assert has(out, "DONE")


# ── HyperTalk ────────────────────────────────────────────────────────────────

class TestHyperTalkDemo:
    def test_runs(self):
        out = _run_demo("hypertalk", "hello.htalk", Language.HYPERTALK)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("hypertalk", "hello.htalk", Language.HYPERTALK)
        assert has(out, "HELLO WORLD", "Welcome to HyperTalk")

    def test_variables(self):
        out = _run_demo("hypertalk", "hello.htalk", Language.HYPERTALK)
        assert has(out, "VARIABLES")

    def test_handlers(self):
        out = _run_demo("hypertalk", "hello.htalk", Language.HYPERTALK)
        assert has(out, "HANDLERS")

    def test_done(self):
        out = _run_demo("hypertalk", "hello.htalk", Language.HYPERTALK)
        assert has(out, "DONE")


# ── Haskell ──────────────────────────────────────────────────────────────────

class TestHaskellDemo:
    def test_runs(self):
        out = _run_demo("haskell", "hello.hs", Language.HASKELL)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("haskell", "hello.hs", Language.HASKELL)
        assert has(out, "HELLO WORLD", "Welcome to Haskell")

    def test_arithmetic(self):
        out = _run_demo("haskell", "hello.hs", Language.HASKELL)
        assert has(out, "ARITHMETIC", "13")

    def test_functions(self):
        out = _run_demo("haskell", "hello.hs", Language.HASKELL)
        assert has(out, "FUNCTIONS")

    def test_lists(self):
        out = _run_demo("haskell", "hello.hs", Language.HASKELL)
        assert has(out, "LISTS")

    def test_done(self):
        out = _run_demo("haskell", "hello.hs", Language.HASKELL)
        assert has(out, "DONE")


# ── APL ──────────────────────────────────────────────────────────────────────

class TestAplDemo:
    def test_runs(self):
        out = _run_demo("apl", "hello.apl", Language.APL)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("apl", "hello.apl", Language.APL)
        assert has(out, "HELLO WORLD", "Welcome to APL")

    def test_arithmetic(self):
        out = _run_demo("apl", "hello.apl", Language.APL)
        assert has(out, "ARITHMETIC")

    def test_iota(self):
        out = _run_demo("apl", "hello.apl", Language.APL)
        assert has(out, "IOTA")

    def test_reduce(self):
        out = _run_demo("apl", "hello.apl", Language.APL)
        assert has(out, "REDUCE", "15")

    def test_done(self):
        out = _run_demo("apl", "hello.apl", Language.APL)
        assert has(out, "DONE")


# ── SQL ──────────────────────────────────────────────────────────────────────

class TestSqlDemo:
    def test_runs(self):
        out = _run_demo("sql", "hello.sql", Language.SQL)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("sql", "hello.sql", Language.SQL)
        assert has(out, "HELLO WORLD", "Welcome to SQL")

    def test_variables(self):
        out = _run_demo("sql", "hello.sql", Language.SQL)
        assert has(out, "VARIABLES")

    def test_table_ops(self):
        out = _run_demo("sql", "hello.sql", Language.SQL)
        assert has(out, "TABLE OPERATIONS")

    def test_done(self):
        out = _run_demo("sql", "hello.sql", Language.SQL)
        assert has(out, "DONE")


# ── JCL ──────────────────────────────────────────────────────────────────────

class TestJclDemo:
    def test_runs(self):
        out = _run_demo("jcl", "hello.jcl", Language.JCL)
        assert ok(out)

    def test_no_errors(self):
        out = _run_demo("jcl", "hello.jcl", Language.JCL)
        assert no_errors(out)


# ── CICS ─────────────────────────────────────────────────────────────────────

class TestCicsDemo:
    def test_runs(self):
        out = _run_demo("cics", "hello.cics", Language.CICS)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("cics", "hello.cics", Language.CICS)
        assert has(out, "HELLO WORLD", "Welcome to CICS")

    def test_variables(self):
        out = _run_demo("cics", "hello.cics", Language.CICS)
        assert has(out, "VARIABLES")

    def test_done(self):
        out = _run_demo("cics", "hello.cics", Language.CICS)
        assert has(out, "DONE")


# ── SQR ──────────────────────────────────────────────────────────────────────

class TestSqrDemo:
    def test_runs(self):
        out = _run_demo("sqr", "hello.sqr", Language.SQR)
        assert ok(out)

    def test_hello(self):
        out = _run_demo("sqr", "hello.sqr", Language.SQR)
        assert has(out, "HELLO WORLD", "Welcome to SQR")

    def test_variables(self):
        out = _run_demo("sqr", "hello.sqr", Language.SQR)
        assert has(out, "VARIABLES")

    def test_procedures(self):
        out = _run_demo("sqr", "hello.sqr", Language.SQR)
        assert has(out, "PROCEDURES", "Hello from procedure")

    def test_done(self):
        out = _run_demo("sqr", "hello.sqr", Language.SQR)
        assert has(out, "DONE")

    def test_no_errors(self):
        out = _run_demo("sqr", "hello.sqr", Language.SQR)
        assert no_errors(out)
