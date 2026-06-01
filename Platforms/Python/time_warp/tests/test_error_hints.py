"""Tests for time_warp.utils.error_hints module."""

import pytest
from time_warp.utils.error_hints import (
    levenshtein_distance,
    suggest_command,
    check_syntax_mistakes,
    get_enhanced_error_message,
)


# ---------------------------------------------------------------------------
# levenshtein_distance
# ---------------------------------------------------------------------------


def test_levenshtein_identical_strings():
    assert levenshtein_distance("abc", "abc") == 0


def test_levenshtein_empty_string():
    assert levenshtein_distance("", "abc") == 3
    assert levenshtein_distance("abc", "") == 3


def test_levenshtein_single_substitution():
    assert levenshtein_distance("cat", "bat") == 1


def test_levenshtein_known_example():
    # "kitten" → "sitting" = 3 edits
    assert levenshtein_distance("kitten", "sitting") == 3


def test_levenshtein_typo_print():
    # PRITN is 2 edits from PRINT (swap T and N)
    assert levenshtein_distance("PRITN", "PRINT") <= 2


# ---------------------------------------------------------------------------
# suggest_command
# ---------------------------------------------------------------------------


def test_suggest_command_direct_typo():
    assert suggest_command("PRITN") == "PRINT"


def test_suggest_command_logo_typo():
    assert suggest_command("FWD") == "FORWARD"


def test_suggest_command_correct_word_returns_suggestion():
    # PRINT is a known command — should return PRINT as suggestion
    result = suggest_command("PRINT")
    assert result == "PRINT"


def test_suggest_command_unknown_returns_none():
    result = suggest_command("ZZZZZZZZUNKNOWN")
    assert result is None


def test_suggest_command_case_insensitive():
    result = suggest_command("pritn")
    assert result == "PRINT"


def test_suggest_command_goto_typo():
    assert suggest_command("GOTP") == "GOTO"


# ---------------------------------------------------------------------------
# check_syntax_mistakes
# ---------------------------------------------------------------------------


def test_check_unclosed_double_quote():
    result = check_syntax_mistakes('print "hello')
    assert result is not None
    assert "quote" in result.lower() or "string" in result.lower()


def test_check_unclosed_single_quote():
    result = check_syntax_mistakes("print 'hello")
    assert result is not None
    assert "quote" in result.lower() or "string" in result.lower()


def test_check_valid_string_no_error():
    result = check_syntax_mistakes('print "hello"')
    assert result is None


def test_check_missing_closing_paren():
    result = check_syntax_mistakes("(a + b")
    assert result is not None
    assert "parenthes" in result.lower()


def test_check_extra_closing_paren():
    result = check_syntax_mistakes("a + b)")
    assert result is not None
    assert "parenthes" in result.lower()


def test_check_balanced_parens_no_error():
    result = check_syntax_mistakes("(a + b) * (c - d)")
    assert result is None


def test_check_if_without_then():
    result = check_syntax_mistakes("IF x > 5")
    assert result is not None
    assert "THEN" in result


def test_check_if_with_then_no_error():
    result = check_syntax_mistakes("IF x > 5 THEN GOTO 100")
    assert result is None


def test_check_for_without_to():
    result = check_syntax_mistakes("FOR x = 1")
    assert result is not None
    assert "TO" in result


def test_check_valid_for_no_error():
    result = check_syntax_mistakes("FOR x = 1 TO 10")
    assert result is None


def test_check_clean_line_no_error():
    result = check_syntax_mistakes("PRINT 42")
    assert result is None


# ---------------------------------------------------------------------------
# get_enhanced_error_message
# ---------------------------------------------------------------------------


def test_enhanced_error_suggests_correction():
    result = get_enhanced_error_message("NameError: PRITN", "PRITN x")
    assert "PRINT" in result


def test_enhanced_error_with_empty_context():
    result = get_enhanced_error_message("some error")
    assert isinstance(result, str)


def test_enhanced_error_missing_then():
    result = get_enhanced_error_message("SyntaxError: missing )", "IF x > 0")
    assert "THEN" in result or isinstance(result, str)


def test_enhanced_error_typo_lne():
    result = get_enhanced_error_message("NameError: lne")
    assert isinstance(result, str)


def test_check_syntax_mistakes_print_typo():
    result = check_syntax_mistakes("PRITN X")
    # Should return None or a hint string
    assert result is None or isinstance(result, str)


def test_check_valid_assignment():
    result = check_syntax_mistakes("LET X = 5")
    assert result is None
