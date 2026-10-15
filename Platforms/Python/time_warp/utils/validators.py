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
                f"{command_name} expects {expected} argument(s), got {arg_count}"
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
        raise ValidationError(f"Expected numeric value{name_str}, got '{value}'") from e
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
        "FOR",
        "NEXT",
        "WHILE",
        "WEND",
        "IF",
        "THEN",
        "ELSE",
        "ENDIF",
        "SELECT",
        "CASE",
        "END",
        "SUB",
        "FUNCTION",
        "END SUB",
        "END FUNCTION",
        "RETURN",
        "GOSUB",
        "GOTO",
        "PRINT",
        "INPUT",
        "READ",
        "DATA",
        "DIM",
        "LET",
        "REM",
        "LOCATE",
        "CLS",
        "STOP",
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
        if not (
            char.isalnum()
            or char == "_"
            or (i == len(name) - 1 and char in suffix_chars)
        ):
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
        raise ValidationError(f"Path traversal not allowed: '{filename}'")

    # Reject absolute paths
    if filename.startswith(("/", "~")):
        raise ValidationError(f"Absolute paths not allowed: '{filename}'")

    # If base_dir specified, validate confinement
    if base_dir:
        file_path = (Path(base_dir) / filename).resolve()
        base_resolved = Path(base_dir).resolve()

        try:
            file_path.relative_to(base_resolved)
        except ValueError as e:
            raise ValidationError(f"Path '{filename}' escapes sandbox directory") from e

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
        raise ValidationError(f"Value {value}{name_str} is below minimum {min_val}")

    if max_val is not None and value > max_val:
        name_str = f" for {param_name}" if param_name else ""
        raise ValidationError(f"Value {value}{name_str} exceeds maximum {max_val}")

    return value


class WorkspaceFileService:
    """Confined file service for language interpreters.

    Enforces symlink-aware path confinement, file handle limits, and maximum
    file size bounds across BASIC, PILOT, Pascal, Forth, and C.
    """

    MAX_OPEN_HANDLES = 32
    MAX_FILE_BYTES = 1_048_576  # 1 MiB

    def __init__(
        self,
        base_dir: Optional[str] = None,
        extra_read_dirs: Optional[List[str]] = None,
    ) -> None:
        from pathlib import Path

        self._base_dir = Path(base_dir).resolve() if base_dir else Path.cwd().resolve()
        self._extra_read_dirs = [
            Path(d).resolve() for d in (extra_read_dirs or []) if d
        ]
        self._handles: dict[str, Any] = {}

    @property
    def base_dir(self):
        """Return resolved base directory."""
        return self._base_dir

    def set_base_dir(self, base_dir: str) -> None:
        """Update the confinement base directory."""
        from pathlib import Path

        self._base_dir = Path(base_dir).resolve()

    def resolve_path(
        self,
        filename: str,
        *,
        for_write: bool = False,
        must_exist: bool = False,
        allowed_extensions: Optional[Tuple[str, ...]] = None,
    ):
        """Validate and resolve *filename* within the confined workspace."""

        raw = (filename or "").strip().strip("\"'")
        validate_file_path(raw, base_dir=str(self._base_dir))
        candidate = (self._base_dir / raw).resolve()

        allowed_roots = [self._base_dir]
        if not for_write:
            allowed_roots.extend(self._extra_read_dirs)

        if not any(self._is_within(candidate, root) for root in allowed_roots):
            raise ValidationError(f"Path '{raw}' escapes sandbox directory")

        if allowed_extensions:
            ext = candidate.suffix.lower()
            norm_exts = tuple(e.lower() for e in allowed_extensions)
            if ext not in norm_exts:
                raise ValidationError(
                    f"File extension '{ext}' not permitted (expected {norm_exts})"
                )

        if must_exist and not candidate.is_file():
            raise ValidationError(f"File not found: '{raw}'")

        if candidate.exists() and candidate.is_file():
            if candidate.stat().st_size > self.MAX_FILE_BYTES:
                raise ValidationError(
                    f"File '{raw}' exceeds maximum size ({self.MAX_FILE_BYTES} bytes)"
                )

        return candidate

    @staticmethod
    def _is_within(target, root) -> bool:
        try:
            target.relative_to(root)
            return True
        except ValueError:
            return False

    def open_handle(
        self,
        key: str,
        filename: str,
        mode: str = "r",
        *,
        allowed_extensions: Optional[Tuple[str, ...]] = None,
    ):
        """Open a confined text file and register its handle under *key*."""
        if key in self._handles:
            self.close_handle(key)
        if len(self._handles) >= self.MAX_OPEN_HANDLES:
            raise ValidationError(
                f"Maximum open files ({self.MAX_OPEN_HANDLES}) exceeded"
            )
        for_write = any(ch in mode for ch in ("w", "a", "+", "x"))
        must_exist = "r" in mode and not for_write
        resolved = self.resolve_path(
            filename,
            for_write=for_write,
            must_exist=must_exist,
            allowed_extensions=allowed_extensions,
        )
        resolved.parent.mkdir(parents=True, exist_ok=True)
        handle = open(resolved, mode, encoding="utf-8")  # noqa: SIM115
        self._handles[key] = handle
        return handle

    def get_handle(self, key: str):
        """Retrieve an open file handle by *key*, or None."""
        return self._handles.get(key)

    def close_handle(self, key: str) -> bool:
        """Close and remove a registered file handle."""
        handle = self._handles.pop(key, None)
        if handle is None:
            return False
        try:
            handle.close()
        except OSError:
            pass
        return True

    def close_all(self) -> None:
        """Close all registered file handles."""
        for key in list(self._handles.keys()):
            self.close_handle(key)

    def read_text(
        self,
        filename: str,
        *,
        allowed_extensions: Optional[Tuple[str, ...]] = None,
    ) -> str:
        """Read text from a confined file."""
        resolved = self.resolve_path(
            filename,
            for_write=False,
            must_exist=True,
            allowed_extensions=allowed_extensions,
        )
        return resolved.read_text(encoding="utf-8")

    def write_text(self, filename: str, content: str, *, append: bool = False) -> None:
        """Write or append bounded text to a confined file."""
        encoded = content.encode("utf-8")
        if len(encoded) > self.MAX_FILE_BYTES:
            raise ValidationError(
                f"Write exceeds maximum file size ({self.MAX_FILE_BYTES} bytes)"
            )
        resolved = self.resolve_path(filename, for_write=True, must_exist=False)
        resolved.parent.mkdir(parents=True, exist_ok=True)
        if append and resolved.exists():
            if resolved.stat().st_size + len(encoded) > self.MAX_FILE_BYTES:
                raise ValidationError(
                    f"Append exceeds maximum file size ({self.MAX_FILE_BYTES} bytes)"
                )
            with open(resolved, "a", encoding="utf-8") as fh:
                fh.write(content)
        else:
            resolved.write_text(content, encoding="utf-8")
