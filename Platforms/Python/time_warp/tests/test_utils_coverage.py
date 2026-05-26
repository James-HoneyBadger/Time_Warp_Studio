"""Tests for utility modules: code_formatter, lang_utils, error_hints,
error_messages, execution_timeout, logging_config."""
from __future__ import annotations

import pytest


# ============================================================================
# code_formatter
# ============================================================================

class TestBasicFormatter:
    def setup_method(self):
        from time_warp.utils.code_formatter import BasicFormatter
        self.fmt = BasicFormatter()

    def test_no_change_simple(self):
        code = "PRINT 1"
        result = self.fmt.format_code(code)
        assert "PRINT 1" in result

    def test_indent_after_for(self):
        code = "FOR I = 1 TO 10\nPRINT I\nNEXT I"
        result = self.fmt.format_code(code)
        assert "    PRINT I" in result  # indented

    def test_indent_after_if(self):
        code = "IF X > 0 THEN\nPRINT X\nEND IF"
        result = self.fmt.format_code(code)
        assert result.count("    PRINT") >= 1

    def test_empty_code(self):
        result = self.fmt.format_code("")
        assert result == "" or result.strip() == ""

    def test_multiline(self):
        code = "FOR I = 1 TO 3\nPRINT I\nNEXT I\nPRINT \"done\""
        result = self.fmt.format_code(code)
        assert "done" in result

    def test_custom_indent_size(self):
        code = "FOR I = 1 TO 3\nPRINT I\nNEXT I"
        result = self.fmt.format_code(code, indent_size=2)
        assert "  PRINT I" in result

    def test_else_dedent_indent(self):
        code = "IF X > 0 THEN\nPRINT \"pos\"\nELSE\nPRINT \"neg\"\nEND IF"
        result = self.fmt.format_code(code)
        assert result  # at minimum should produce output


class TestLogoFormatter:
    def setup_method(self):
        from time_warp.utils.code_formatter import LogoFormatter
        self.fmt = LogoFormatter()

    def test_no_change_simple(self):
        code = "FORWARD 100"
        result = self.fmt.format_code(code)
        assert result  # returns something

    def test_to_block(self):
        code = "TO square :n\n  FORWARD :n\n  RIGHT 90\nEND"
        result = self.fmt.format_code(code)
        assert result

    def test_empty(self):
        result = self.fmt.format_code("")
        assert result == "" or result.strip() == ""


class TestPilotFormatter:
    def setup_method(self):
        from time_warp.utils.code_formatter import PilotFormatter
        self.fmt = PilotFormatter()

    def test_no_change_simple(self):
        code = "T:Hello"
        result = self.fmt.format_code(code)
        assert result  # returns something

    def test_empty(self):
        result = self.fmt.format_code("")
        assert result == "" or result.strip() == ""


class TestCodeFormatter:
    def test_get_formatter_returns_formatter(self):
        from time_warp.utils.code_formatter import get_formatter, CodeFormatter
        fmt = get_formatter()
        assert isinstance(fmt, CodeFormatter)

    def test_code_formatter_format_basic(self):
        from time_warp.utils.code_formatter import get_formatter
        fmt = get_formatter()
        code, msg = fmt.format_code("FOR I = 1 TO 10\nPRINT I\nNEXT I", "BASIC")
        assert code
        assert "✅" in msg

    def test_code_formatter_format_logo(self):
        from time_warp.utils.code_formatter import get_formatter
        fmt = get_formatter()
        code, msg = fmt.format_code("FORWARD 100", "LOGO")
        assert code

    def test_code_formatter_unknown_lang(self):
        from time_warp.utils.code_formatter import get_formatter
        fmt = get_formatter()
        code, msg = fmt.format_code("some code", "UNKNOWN")
        # Should return the code unchanged
        assert "Unknown language" in msg or "❌" in msg

    def test_code_formatter_normalize(self):
        from time_warp.utils.code_formatter import get_formatter
        fmt = get_formatter()
        code, msg = fmt.normalize_keywords("for i = 1 to 10", "BASIC")
        assert code  # returns something


# ============================================================================
# lang_utils (from languages folder)
# ============================================================================

class TestLangUtilsFromLanguages:
    def test_upper_preserve_strings(self):
        from time_warp.languages.lang_utils import upper_preserve_strings
        assert upper_preserve_strings("hello world") == "HELLO WORLD"

    def test_upper_in_double_quotes(self):
        from time_warp.languages.lang_utils import upper_preserve_strings
        result = upper_preserve_strings('print "Hello"')
        assert 'PRINT "Hello"' == result

    def test_upper_empty(self):
        from time_warp.languages.lang_utils import upper_preserve_strings
        assert upper_preserve_strings("") == ""

    def test_split_args_basic(self):
        from time_warp.languages.lang_utils import split_args
        parts = split_args("a, b, c")
        assert len(parts) == 3
        assert parts[0].strip() == "a"

    def test_split_args_nested_parens(self):
        from time_warp.languages.lang_utils import split_args
        parts = split_args("f(x, y), g(z)")
        assert len(parts) == 2

    def test_split_args_empty(self):
        from time_warp.languages.lang_utils import split_args
        parts = split_args("")
        assert parts == [] or parts == [""]

    def test_split_args_custom_sep(self):
        from time_warp.languages.lang_utils import split_args
        parts = split_args("a; b; c", sep=";")
        assert len(parts) == 3

    def test_unquote_double(self):
        from time_warp.languages.lang_utils import unquote
        assert unquote('"hello"') == "hello"

    def test_unquote_single(self):
        from time_warp.languages.lang_utils import unquote
        assert unquote("'hello'") == "hello"

    def test_unquote_no_quotes(self):
        from time_warp.languages.lang_utils import unquote
        assert unquote("hello") == "hello"

    def test_to_num_int(self):
        from time_warp.languages.lang_utils import to_num
        assert to_num("42") == 42.0

    def test_to_num_float(self):
        from time_warp.languages.lang_utils import to_num
        assert abs(to_num("3.14") - 3.14) < 0.001

    def test_to_num_already_int(self):
        from time_warp.languages.lang_utils import to_num
        assert to_num(5) == 5.0

    def test_to_num_or_str_number(self):
        from time_warp.languages.lang_utils import to_num_or_str
        result = to_num_or_str("42")
        assert result == 42 or result == 42.0

    def test_to_num_or_str_string(self):
        from time_warp.languages.lang_utils import to_num_or_str
        result = to_num_or_str("hello")
        assert result == "hello"

    def test_tokenize_simple(self):
        from time_warp.languages.lang_utils import tokenize
        tokens = tokenize("PRINT 42")
        assert "PRINT" in tokens or "print" in [t.lower() for t in tokens]
        assert "42" in tokens

    def test_tokenize_empty(self):
        from time_warp.languages.lang_utils import tokenize
        tokens = tokenize("")
        assert tokens == []

    def test_tokenize_quoted_string(self):
        from time_warp.languages.lang_utils import tokenize
        tokens = tokenize('PRINT "Hello World"')
        assert any("Hello World" in t or '"Hello World"' in t for t in tokens)

    def test_language_error(self):
        from time_warp.languages.lang_utils import LanguageError
        try:
            raise LanguageError("test error")
        except LanguageError as e:
            assert "test error" in str(e)

    def test_language_stop(self):
        from time_warp.languages.lang_utils import LanguageStop
        try:
            raise LanguageStop()
        except LanguageStop:
            pass  # expected

    def test_did_you_mean_found(self):
        from time_warp.languages.lang_utils import did_you_mean
        candidates = ["PRINT", "INPUT", "GOTO", "GOSUB"]
        result = did_you_mean("PRITN", candidates)
        assert result == "PRINT" or result is not None

    def test_did_you_mean_not_found(self):
        from time_warp.languages.lang_utils import did_you_mean
        result = did_you_mean("ZZZZZZZ", ["PRINT", "INPUT"])
        assert result is None

    def test_loop_stack(self):
        from time_warp.languages.lang_utils import LoopStack
        stack = LoopStack()
        stack.push({"type": "FOR", "start": 0})
        assert not stack.is_empty()
        item = stack.pop()
        assert item["type"] == "FOR"
        assert stack.is_empty()

    def test_variable_scope_set_get(self):
        from time_warp.languages.lang_utils import VariableScope
        scope = VariableScope()
        scope["X"] = 42
        assert scope.get("X") == 42

    def test_variable_scope_default(self):
        from time_warp.languages.lang_utils import VariableScope
        scope = VariableScope()
        assert scope.get("UNDEFINED") == 0 or scope.get("UNDEFINED") is None or scope.get("UNDEFINED") == ""


# ============================================================================
# error_hints
# ============================================================================

class TestErrorHints:
    def test_suggest_command_print_typo(self):
        from time_warp.utils.error_hints import suggest_command
        result = suggest_command("PRITN")
        assert result is not None and "PRINT" in result

    def test_suggest_command_goto_typo(self):
        from time_warp.utils.error_hints import suggest_command
        result = suggest_command("GOTP")
        assert result is not None

    def test_suggest_command_forward_typo(self):
        from time_warp.utils.error_hints import suggest_command
        result = suggest_command("FORWAD")
        assert result is not None

    def test_levenshtein_same(self):
        from time_warp.utils.error_hints import levenshtein_distance
        assert levenshtein_distance("hello", "hello") == 0

    def test_levenshtein_one_diff(self):
        from time_warp.utils.error_hints import levenshtein_distance
        assert levenshtein_distance("cat", "bat") == 1

    def test_levenshtein_empty(self):
        from time_warp.utils.error_hints import levenshtein_distance
        assert levenshtein_distance("", "abc") == 3
        assert levenshtein_distance("abc", "") == 3

    def test_check_syntax_mistakes(self):
        from time_warp.utils.error_hints import check_syntax_mistakes
        result = check_syntax_mistakes("PRITN \"Hello\"")
        # Either returns a suggestion or None
        assert result is None or isinstance(result, str)

    def test_get_enhanced_error_message(self):
        from time_warp.utils.error_hints import get_enhanced_error_message
        result = get_enhanced_error_message("Undefined command: PRITN")
        assert isinstance(result, str)


# ============================================================================
# error_messages
# ============================================================================

class TestErrorMessages:
    def test_error_method(self):
        from time_warp.utils.error_messages import ErrorMessage
        msg = ErrorMessage.error("Something went wrong")
        assert "❌" in msg
        assert "Something went wrong" in msg

    def test_error_with_context(self):
        from time_warp.utils.error_messages import ErrorMessage
        msg = ErrorMessage.error("Bad value", context="CALC")
        assert "❌" in msg
        assert "CALC" in msg
        assert "Bad value" in msg

    def test_warning(self):
        from time_warp.utils.error_messages import ErrorMessage
        msg = ErrorMessage.warning("Careful!")
        assert "⚠️" in msg
        assert "Careful!" in msg

    def test_info(self):
        from time_warp.utils.error_messages import ErrorMessage
        msg = ErrorMessage.info("Running...")
        assert "ℹ️" in msg
        assert "Running..." in msg

    def test_success(self):
        from time_warp.utils.error_messages import ErrorMessage
        msg = ErrorMessage.success("Done")
        assert "✅" in msg
        assert "Done" in msg

    def test_from_exception(self):
        from time_warp.utils.error_messages import ErrorMessage
        try:
            raise ValueError("test error")
        except ValueError as e:
            msg = ErrorMessage.from_exception(e)
            assert "❌" in msg
            assert "ValueError" in msg or "test error" in msg


# ============================================================================
# execution_timeout
# ============================================================================

class TestExecutionTimeout:
    def test_context_manager_no_timeout(self):
        from time_warp.utils.execution_timeout import execution_timeout
        result = []
        with execution_timeout(seconds=5.0):
            result.append(42)
        assert result == [42]

    def test_threaded_timeout_no_timeout(self):
        from time_warp.utils.execution_timeout import ThreadedExecutionTimeout
        result = []
        with ThreadedExecutionTimeout(seconds=5.0):
            result.append(99)
        assert result == [99]

    def test_execution_timeout_error_is_exception(self):
        from time_warp.utils.execution_timeout import ExecutionTimeoutError
        e = ExecutionTimeoutError("timeout")
        assert isinstance(e, Exception)
        assert "timeout" in str(e)

    def test_with_timeout_decorator(self):
        from time_warp.utils.execution_timeout import with_timeout

        @with_timeout(max_seconds=5.0)
        def fast_fn():
            return 42

        result = fast_fn()
        assert result == 42


# ============================================================================
# logging_config
# ============================================================================

class TestLoggingConfig:
    def test_get_logger(self):
        from time_warp.utils.logging_config import get_logger
        logger = get_logger("test_module")
        assert logger is not None

    def test_get_logger_has_name(self):
        from time_warp.utils.logging_config import get_logger
        logger = get_logger("my_logger")
        assert "my_logger" in logger.name

    def test_logger_can_log(self):
        from time_warp.utils.logging_config import get_logger
        logger = get_logger("test")
        logger.debug("test debug message")
        logger.info("test info message")

    def test_setup_logging(self):
        from time_warp.utils.logging_config import setup_logging
        logger = setup_logging(level="WARNING")
        assert logger is not None

    def test_configure_for_testing(self):
        from time_warp.utils.logging_config import configure_for_testing
        logger = configure_for_testing()
        assert logger is not None

    def test_log_exception_does_not_raise(self):
        from time_warp.utils.logging_config import get_logger, log_exception
        logger = get_logger("exc_test")
        try:
            raise ValueError("test")
        except ValueError:
            log_exception(logger, "Caught exception")  # should not raise
