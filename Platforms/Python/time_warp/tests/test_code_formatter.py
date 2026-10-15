"""Tests for time_warp.utils.code_formatter module."""

from time_warp.utils.code_formatter import (
    BasicFormatter,
    LogoFormatter,
    PilotFormatter,
    CodeFormatter,
    get_formatter,
)


# ---------------------------------------------------------------------------
# BasicFormatter
# ---------------------------------------------------------------------------


def test_basic_format_for_loop_indents_body():
    bf = BasicFormatter()
    code = "10 FOR I = 1 TO 3\n20 PRINT I\n30 NEXT I"
    result = bf.format_code(code)
    # Body line should be indented
    assert "    PRINT I" in result


def test_basic_format_preserves_structure():
    bf = BasicFormatter()
    code = "10 PRINT \"HELLO\"\n20 LET X=5\n30 GOTO 10"
    result = bf.format_code(code)
    assert "PRINT" in result
    assert "GOTO" in result


def test_basic_normalize_keywords_uppercase():
    bf = BasicFormatter()
    code = '10 print "hello"\n20 let x = 5'
    result = bf.normalize_keywords(code)
    assert "PRINT" in result
    assert "LET" in result


# ---------------------------------------------------------------------------
# LogoFormatter
# ---------------------------------------------------------------------------


def test_logo_format_preserves_commands():
    lf = LogoFormatter()
    code = "FORWARD 100\nLEFT 90"
    result = lf.format_code(code)
    assert "FORWARD" in result
    assert "LEFT" in result


def test_logo_normalize_passes_through():
    lf = LogoFormatter()
    code = "FD 100\nLT 90\nRT 45"
    result = lf.normalize_keywords(code)
    # Should return some normalized form
    assert isinstance(result, str)
    assert len(result) > 0


# ---------------------------------------------------------------------------
# PilotFormatter
# ---------------------------------------------------------------------------


def test_pilot_normalize_uppercase_commands():
    pf = PilotFormatter()
    code = "t:Hello\nc:x = 5\nt:$x"
    result = pf.normalize_commands(code)
    # Commands should be uppercase
    assert "T:" in result
    assert "C:" in result


def test_pilot_format_preserves_content():
    pf = PilotFormatter()
    code = "T:Hello World\nC:X = 10\nT:$X"
    result = pf.format_code(code)
    assert "Hello World" in result
    assert "X = 10" in result


# ---------------------------------------------------------------------------
# CodeFormatter / get_formatter
# ---------------------------------------------------------------------------


def test_get_formatter_returns_code_formatter():
    cf = get_formatter()
    assert isinstance(cf, CodeFormatter)


def test_code_formatter_format_basic():
    cf = get_formatter()
    result, msg = cf.format_code("10 PRINT 'HELLO'\n20 LET X = 5", "basic")
    assert isinstance(result, str)
    assert "PRINT" in result
    assert "✅" in msg


def test_code_formatter_normalize_basic():
    cf = get_formatter()
    result, msg = cf.normalize_keywords("10 print HELLO", "basic")
    assert "PRINT" in result
    assert "✅" in msg


def test_code_formatter_format_and_normalize():
    cf = get_formatter()
    result, msg = cf.format_and_normalize("10 PRINT HELLO", "basic")
    assert isinstance(result, str)
    assert isinstance(msg, str)


def test_format_basic_with_end():
    cf = get_formatter()
    result, msg = cf.format_code("10 PRINT 'HELLO'\n20 END", "basic")
    assert "PRINT" in result
    assert "✅" in msg


def test_normalize_let_lowercase():
    cf = get_formatter()
    result, msg = cf.normalize_keywords("10 let x = 5", "basic")
    assert "LET" in result
    assert "✅" in msg


def test_format_unknown_language():
    cf = get_formatter()
    result, msg = cf.format_code("print(42)", "unknown_lang")
    assert "❌" in msg


def test_format_and_normalize_all_active_languages():
    from time_warp.core.interpreter import Language

    cf = get_formatter()
    samples = {
        Language.BASIC: (
            "10 for i = 1 to 3\n20 print i\n30 next i",
            "10 FOR i = 1 TO 3\n20     PRINT i\n30 NEXT i",
        ),
        Language.PILOT: (
            "t:Hello\na:name",
            "T:Hello\nA:name",
        ),
        Language.LOGO: (
            "TO SQ\nFORWARD 10\nEND",
            "to SQ\n    forward 10\nend",
        ),
        Language.C: (
            "int main() {\nif (1) {\nreturn 0;\n}\n}",
            "int main() {\n    if (1) {\n        return 0;\n    }\n}",
        ),
        Language.PASCAL: (
            "PROGRAM Demo;\nBEGIN\nWHILE x < 5 DO BEGIN\nx := x + 1;\nEND;\nEND.",
            "program Demo;\nbegin\n  while x < 5 do begin\n    x := x + 1;\n  end;\nend.",
        ),
        Language.PROLOG: (
            "grandparent(X, Z) :-\nparent(X, Y),\nparent(Y, Z).",
            "grandparent(X, Z) :-\n    parent(X, Y),\n    parent(Y, Z).",
        ),
        Language.FORTH: (
            ": double\ndup   +\n;",
            ": double\n  DUP +\n;",
        ),
        Language.BRAINFUCK: (
            "  +++[>+++<-]>.   \n",
            "+++[>+++<-]>.\n",
        ),
        Language.PYTHON_LANG: (
            "def f():\n\treturn 42   ",
            "def f():\n    return 42",
        ),
    }

    for lang, (raw, expected) in samples.items():
        formatted, msg = cf.format_and_normalize(raw, lang.name)
        assert "✅" in msg, f"Expected success for {lang.name}, got {msg}"
        assert formatted == expected, f"Mismatch for {lang.name}: {formatted!r} != {expected!r}"
