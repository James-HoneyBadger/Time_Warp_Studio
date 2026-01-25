"""Standardized error message formatting for Time Warp Studio.

Provides consistent emoji-prefixed error messages across all language
executors and UI components.
"""

from typing import Optional

from .logging_config import get_logger

logger = get_logger(__name__)


class ErrorMessage:
    """Standardized error message formatting with emoji prefixes."""

    # Emoji prefixes (standard across codebase)
    ERROR = "❌"
    WARNING = "⚠️"
    INFO = "ℹ️"
    SUCCESS = "✅"
    TURTLE = "🐢"
    RUN = "🚀"
    THEME = "🎨"
    INPUT = "📝"

    @staticmethod
    def error(message: str, context: Optional[str] = None) -> str:
        """Format error message.

        Args:
            message: Error message text
            context: Optional context (e.g., command name, file)

        Returns:
            Formatted error message

        Example:
            >>> ErrorMessage.error("Division by zero", "CALC")
            '❌ [CALC] Division by zero\\n'
        """
        if context:
            return f"{ErrorMessage.ERROR} [{context}] {message}\n"
        return f"{ErrorMessage.ERROR} {message}\n"

    @staticmethod
    def warning(message: str, context: Optional[str] = None) -> str:
        """Format warning message."""
        if context:
            return f"{ErrorMessage.WARNING} [{context}] {message}\n"
        return f"{ErrorMessage.WARNING} {message}\n"

    @staticmethod
    def info(message: str) -> str:
        """Format informational message."""
        return f"{ErrorMessage.INFO} {message}\n"

    @staticmethod
    def success(message: str) -> str:
        """Format success message."""
        return f"{ErrorMessage.SUCCESS} {message}\n"

    @staticmethod
    def from_exception(
        exc: Exception,
        context: Optional[str] = None,
        log_level: str = "error",
    ) -> str:
        """Convert exception to formatted error message and log it.

        Args:
            exc: Exception instance
            context: Optional context information
            log_level: Logging level (error, warning, debug)

        Returns:
            Formatted error message
        """
        exc_type = type(exc).__name__
        exc_msg = str(exc)
        full_msg = f"{exc_type}: {exc_msg}" if exc_msg else exc_type

        # Log at appropriate level
        if log_level == "warning":
            if context:
                logger.warning(f"[{context}] {full_msg}")
            else:
                logger.warning(full_msg)
        elif log_level == "debug":
            logger.debug(f"{full_msg}", exc_info=True)
        else:  # error (default)
            if context:
                logger.error(f"[{context}] {full_msg}")
            else:
                logger.error(full_msg)

        return ErrorMessage.error(exc_msg, context)

    @staticmethod
    def validation_error(
        expected: str,
        got: str,
        param_name: Optional[str] = None,
    ) -> str:
        """Format validation error message.

        Args:
            expected: What was expected
            got: What was actually received
            param_name: Name of parameter (optional)

        Returns:
            Formatted validation error

        Example:
            >>> ErrorMessage.validation_error("number", "text", "radius")
            '❌ Invalid radius: expected number, got text\\n'
        """
        if param_name:
            msg = f"Invalid {param_name}: expected {expected}, got {got}"
        else:
            msg = f"Expected {expected}, got {got}"
        return ErrorMessage.error(msg)

    @staticmethod
    def syntax_error(
        message: str,
        line_number: Optional[int] = None,
        line_content: Optional[str] = None,
    ) -> str:
        """Format syntax error message.

        Args:
            message: Error description
            line_number: Line number (optional)
            line_content: Content of erroneous line (optional)

        Returns:
            Formatted syntax error

        Example:
            >>> ErrorMessage.syntax_error("Missing )", 5, 'PRINT "HELLO')
            '❌ [Line 5] Missing )\\n  PRINT "HELLO\\n'
        """
        if line_number:
            result = f"{ErrorMessage.ERROR} [Line {line_number}] {message}\n"
            if line_content:
                result += f"  {line_content}\n"
            return result
        return ErrorMessage.error(message)

    @staticmethod
    def undefined_variable(var_name: str) -> str:
        """Format undefined variable error."""
        return ErrorMessage.error(f"Undefined variable: {var_name}")

    @staticmethod
    def division_by_zero() -> str:
        """Format division by zero error."""
        return ErrorMessage.error("Division by zero")

    @staticmethod
    def type_mismatch(expected: str, got: str) -> str:
        """Format type mismatch error."""
        return ErrorMessage.error(f"Type mismatch: expected {expected}, got {got}")

    @staticmethod
    def file_error(filename: str, reason: str) -> str:
        """Format file I/O error.

        Args:
            filename: File name
            reason: Error reason (e.g., "not found", "permission denied")

        Returns:
            Formatted file error

        Example:
            >>> ErrorMessage.file_error("data.txt", "not found")
            '❌ File error: test.txt - not found\\n'
        """
        return ErrorMessage.error(
            f"File error: {filename} - {reason}",
            context="FILE",
        )

    @staticmethod
    def command_not_found(command: str, language: Optional[str] = None) -> str:
        """Format unknown command error."""
        if language:
            return ErrorMessage.error(f"Unknown {language} command: {command}")
        return ErrorMessage.error(f"Unknown command: {command}")
