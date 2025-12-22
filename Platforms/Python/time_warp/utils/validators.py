"""Input validation utilities for language interpreters.

Provides consistent validation patterns for commands and function arguments
across all language executors (BASIC, Logo, PILOT, etc.)
"""

from typing import Any, List, Optional, Tuple, Union


class ValidationError(Exception):
    """Raised when input validation fails."""

    pass


def validate_arg_count(
    args: Union[List[Any], str],
    expected: Union[int, Tuple[int, int]],
    command_name: str = "",
) -> None:
    """Validate that argument list has expected count.
    
    Args:
        args: List or string of arguments
        expected: Exact count (int) or range (min, max) tuple
        command_name: Name of command (for error message)
    
    Raises:
        ValidationError: If argument count doesn't match
    
    Example:
        >>> validate_arg_count(["10", "20"], 2, "FOR")  # OK
        >>> validate_arg_count(["10"], (1, 3), "PRINT")  # OK (1-3 args)
        >>> validate_arg_count([], 1, "GOTO")  # Raises ValidationError
    """
    arg_count = len(args)
    
    if isinstance(expected, int):
        if arg_count != expected:
            raise ValidationError(
                f"{command_name} expects {expected} argument(s), "
                f"got {arg_count}"
            )
    elif isinstance(expected, tuple):
        min_count, max_count = expected
        if not (min_count <= arg_count <= max_count):
            raise ValidationError(
                f"{command_name} expects {min_count}-{max_count} "
                f"argument(s), got {arg_count}"
            )


def validate_numeric(value: Any, param_name: str = "") -> Any:
    """Validate that value is numeric (can be converted to float).
    
    Args:
        value: Value to validate
        param_name: Parameter name (for error message)
    
    Returns:
        Original value (unchanged)
    
    Raises:
        ValidationError: If value cannot be converted to number
    
    Example:
        >>> validate_numeric("42", "distance")
        '42'
        >>> validate_numeric("3.14", "ratio")
        '3.14'
        >>> validate_numeric("abc", "size")  # Raises ValidationError
    """
    try:
        float(value)
    except (ValueError, TypeError) as e:
        name_str = f" for {param_name}" if param_name else ""
        raise ValidationError(
            f"Expected numeric value{name_str}, got '{value}'"
        ) from e
    return value


def validate_variable_name(name: str, allow_suffix: bool = False) -> str:
    """Validate that name is a valid variable identifier.
    
    Args:
        name: Variable name to validate
        allow_suffix: If True, allow BASIC suffixes (%, $, !, #)
    
    Returns:
        Validated variable name
    
    Raises:
        ValidationError: If name is invalid
    
    Example:
        >>> validate_variable_name("X")
        'X'
        >>> validate_variable_name("COUNT")
        'COUNT'
        >>> validate_variable_name("123BAD")  # Raises ValidationError
        >>> validate_variable_name("X%", allow_suffix=True)
        'X%'
    """
    # Reserved BASIC keywords
    RESERVED = {
        "FOR", "NEXT", "WHILE", "WEND", "IF", "THEN", "ELSE", "ENDIF",
        "SELECT", "CASE", "END", "SUB", "FUNCTION", "END SUB", "END FUNCTION",
        "RETURN", "GOSUB", "GOTO", "PRINT", "INPUT", "READ", "DATA",
        "DIM", "LET", "REM", "LOCATE", "CLS", "END", "STOP"
    }
    
    if not name:
        raise ValidationError("Variable name cannot be empty")
    
    # Check if it's a reserved keyword
    if name.upper() in RESERVED:
        raise ValidationError(
            f"'{name}' is a reserved keyword and cannot be used as variable name"
        )
    
    # Check if it starts with a letter or underscore
    if not (name[0].isalpha() or name[0] == "_"):
        raise ValidationError(
            f"Variable name must start with letter or underscore, got '{name}'"
        )
    
    # Check rest of name
    suffix_chars = set("%$!#") if allow_suffix else set()
    for i, char in enumerate(name):
        if not (char.isalnum() or char == "_" or (i == len(name) - 1 and char in suffix_chars)):
            raise ValidationError(
                f"Invalid character '{char}' in variable name '{name}'"
            )
    
    return name


def validate_file_path(
    filename: str,
    base_dir: Optional[str] = None,
    allow_relative: bool = False,
) -> str:
    """Validate file path for safety.
    
    Prevents directory traversal attacks by ensuring paths stay within
    the designated base directory.
    
    Args:
        filename: File path from user code
        base_dir: Base directory to confine access. If None, uses current dir
        allow_relative: If False, rejects paths with ".." or absolute paths
    
    Returns:
        Validated file path
    
    Raises:
        ValidationError: If path escapes sandbox or is invalid
    
    Example:
        >>> validate_file_path("data.txt")
        'data.txt'
        >>> validate_file_path("/etc/passwd")  # Raises ValidationError
        >>> validate_file_path("../../../etc/passwd")  # Raises ValidationError
    """
    from pathlib import Path
    
    if not filename:
        raise ValidationError("File name cannot be empty")
    
    # Reject paths with directory traversal
    if ".." in filename:
        raise ValidationError(
            f"Path traversal not allowed: '{filename}'"
        )
    
    # Reject absolute paths
    if filename.startswith(("/", "~")):
        raise ValidationError(
            f"Absolute paths not allowed: '{filename}'"
        )
    
    # If base_dir specified, validate confinement
    if base_dir:
        file_path = (Path(base_dir) / filename).resolve()
        base_resolved = Path(base_dir).resolve()
        
        try:
            file_path.relative_to(base_resolved)
        except ValueError as e:
            raise ValidationError(
                f"Path '{filename}' escapes sandbox directory"
            ) from e
    
    return filename


def validate_range(
    value: float,
    min_val: Optional[float] = None,
    max_val: Optional[float] = None,
    param_name: str = "",
) -> float:
    """Validate that numeric value is within range.
    
    Args:
        value: Numeric value to validate
        min_val: Minimum allowed value (inclusive), or None for no limit
        max_val: Maximum allowed value (inclusive), or None for no limit
        param_name: Parameter name (for error message)
    
    Returns:
        Validated value
    
    Raises:
        ValidationError: If value is outside range
    
    Example:
        >>> validate_range(50, min_val=0, max_val=100, param_name="angle")
        50
        >>> validate_range(150, max_val=100)  # Raises ValidationError
    """
    if min_val is not None and value < min_val:
        name_str = f" for {param_name}" if param_name else ""
        raise ValidationError(
            f"Value {value}{name_str} is below minimum {min_val}"
        )
    
    if max_val is not None and value > max_val:
        name_str = f" for {param_name}" if param_name else ""
        raise ValidationError(
            f"Value {value}{name_str} exceeds maximum {max_val}"
        )
    
    return value
