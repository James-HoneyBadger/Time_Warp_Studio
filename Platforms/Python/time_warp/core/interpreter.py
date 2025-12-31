"""
Core interpreter for Time Warp IDE
Time Warp interpreter for executing code in multiple languages
"""

# The interpreter is intentionally large and exposes many public methods
# because it implements behavior across multiple embedded languages. Silence
# the 'too-many-public-methods' pylint rule for this file — refactoring the
# interpreter into smaller classes would be a large architecture change.
# pylint: disable=too-many-public-methods

import re
import threading
import time
from dataclasses import dataclass
from enum import Enum, auto
from typing import TYPE_CHECKING, Any, Callable, Dict, List, Optional, Tuple, TextIO

# Import language executors
from ..languages.basic import execute_basic
from ..languages.c_lang_fixed import execute_c
from ..languages.logo import execute_logo
from ..languages.pascal import execute_pascal
from ..languages.pilot import execute_pilot
from ..languages.prolog import execute_prolog
from ..languages.forth import execute_forth

# Project utilities and language executors
from ..utils.error_hints import check_syntax_mistakes, suggest_command
from ..utils.expression_evaluator import ExpressionEvaluator

# Compact alias used for debug callback annotations in this module.
# Using a short alias keeps signatures readable while still being typed.
DebugCallback = Optional[Callable[[int, Dict[str, Any]], None]]


class ExecutionResult(Enum):
    """Control flow result from executing a command"""

    CONTINUE = auto()
    END = auto()
    JUMP = auto()
    WAIT_FOR_INPUT = auto()

    def __init__(self, value):
        self._value_ = value
        self.jump_line: Optional[int] = None

    @classmethod
    def jump(cls, line: int):
        """Create a Jump result with target line"""
        result = cls.JUMP
        result.jump_line = line
        return result


class ScreenMode(Enum):
    """Unified screen modes akin to GW-BASIC"""

    TEXT = auto()
    GRAPHICS = auto()

    def __init__(self, value):
        self._value_ = value
        self.cols: int = 80
        self.rows: int = 25
        self.width: int = 800
        self.height: int = 600


class Language(Enum):
    """Supported languages in Time Warp"""

    BASIC = auto()
    PILOT = auto()
    LOGO = auto()
    C = auto()
    PROLOG = auto()
    PASCAL = auto()
    FORTH = auto()

    @classmethod
    def from_extension(cls, ext: str) -> "Language":
        """Map file extension to Language enum."""
        ext = ext.lower()
        mapping = {
            ".bas": cls.BASIC,
            ".pilot": cls.PILOT,
            ".logo": cls.LOGO,
            ".c": cls.C,
            ".pro": cls.PROLOG,
            ".prolog": cls.PROLOG,
            ".pas": cls.PASCAL,
            ".f": cls.FORTH,
            ".fs": cls.FORTH,
            ".forth": cls.FORTH,
        }
        return mapping.get(ext, cls.BASIC)

    def friendly_name(self) -> str:
        """Human-friendly display name for language."""
        names = {
            Language.BASIC: "BASIC",
            Language.PILOT: "PILOT",
            Language.LOGO: "Logo",
            Language.C: "C",
            Language.PROLOG: "Prolog",
            Language.PASCAL: "Pascal",
            Language.FORTH: "Forth",
        }
        return names.get(self, "Unknown")


@dataclass
class ForContext:
    """FOR loop context for BASIC"""

    var_name: str
    end_value: float
    step: float
    for_line: int


@dataclass
class InputRequest:
    """Pending input request from UI"""

    prompt: str
    var_name: str
    is_numeric: bool


if TYPE_CHECKING:
    from ..graphics.turtle_state import TurtleState


class Interpreter:
    """
    Central execution engine for Time Warp (BASIC, PILOT, Logo).

    Manages program state, variable storage, and control flow.
    Time Warp interpreter for executing code

    Security Features:
    - Max iterations: 100,000 (prevents infinite loops)
    - Execution timeout: 10 seconds (prevents DoS)
    - Safe expression evaluation (no eval/exec)

    Example:
        >>> interp = Interpreter()
        >>> turtle = TurtleState()
        >>> interp.load_program("T:Hello\\nT:World\\nE:")
        >>> output = interp.execute(turtle)
        >>> print(output)
        ['Hello', 'World']
    """

    # This interpreter maintains many runtime attributes and state for
    # multiple embedded languages. Allow extra instance attributes rather
    # than performing a large refactor right now.
    # pylint: disable=too-many-instance-attributes

    # Security limits
    MAX_ITERATIONS = 100_000
    MAX_EXECUTION_TIME = 10.0  # seconds

    # Variable interpolation patterns
    # *VAR* syntax (BASIC style)
    VAR_INTERPOLATION_PATTERN = re.compile(r"\*([A-Z_][A-Z0-9_]*)\*")
    # #VAR syntax (PILOT style) - matches #VAR followed by non-alphanumeric or end
    PILOT_VAR_PATTERN = re.compile(r"#([A-Z_][A-Z0-9_]*)", re.IGNORECASE)

    def __init__(self):
        # The __init__ constructs a large amount of interpreter state;
        # keep the initialization explicit and readable rather than
        # splitting into many helper methods.
        # pylint: disable=too-many-statements
        # Core state
        # Aggregate numeric variables (base name → float) for back-compat
        self.variables: Dict[str, float] = {}
        # Typed variable stores
        self.int_variables: Dict[str, int] = {}
        self.long_variables: Dict[str, int] = {}
        self.single_variables: Dict[str, float] = {}
        self.double_variables: Dict[str, float] = {}
        self.string_variables: Dict[str, str] = {}
        self.arrays: Dict[str, List[float]] = {}  # BASIC arrays
        # Logo data types
        self.logo_lists: Dict[str, List] = {}  # Logo lists
        self.logo_arrays: Dict[str, List] = {}  # Logo arrays (1-indexed)
        self.property_lists: Dict[str, Dict[str, object]] = {}  # Properties
        self.logo_procedures: Dict[str, str] = {}
        self.logo_procedure_params: Dict[str, List[str]] = {}
        self.logo_lists: Dict[str, List[object]] = {}
        self.logo_arrays: Dict[str, Dict[int, object]] = {}
        self.property_lists: Dict[str, Dict[str, object]] = {}  # Procedures
        self.output: List[str] = []

        # Program state
        self.program_lines: List[Tuple[Optional[int], str]] = []
        self.current_line: int = 0
        self.labels: Dict[str, int] = {}
        self.line_number_map: Dict[int, int] = {}  # BASIC line number -> index

        # Control flow
        self.gosub_stack: List[int] = []
        self.for_stack: List[ForContext] = []

        # PILOT-specific
        self.match_flag: bool = False
        self.last_match_set: bool = False
        self.last_match_succeeded: bool = False  # For PILOT M:/Y:/N: commands
        self.stored_condition: Optional[bool] = None
        self.running: bool = True  # Program execution flag
        self.subroutine_stack: List[int] = []  # For PILOT S:/R: commands
        self.open_files: Dict[str, TextIO] = {}  # For PILOT F: commands

        # I/O handling
        self.input_callback: Optional[Callable[[str], str]] = None
        self.output_callback: Optional[Callable[[str], None]] = None
        self.last_input: str = ""
        self.pending_input: Optional[InputRequest] = None
        self.pending_resume_line: Optional[int] = None

        # INKEY$ support
        self.inkey_callback: Optional[Callable[[], Optional[str]]] = None
        self.last_key_pressed: Optional[str] = None

        # Screen state
        self.screen_mode = ScreenMode.GRAPHICS
        self.text_lines: List[str] = []
        self.cursor_row: int = 0
        self.cursor_col: int = 0

        # BASIC-specific state
        self.basic_while_stack: List[str] = []
        self.basic_do_stack: List[Tuple[str, str]] = []
        self.basic_select_expression: str = ""
        self.basic_in_select: bool = False
        self.basic_in_sub: bool = False
        self.basic_in_function: bool = False

        # BASIC DATA/READ state
        self.data_values: List[str] = []
        self.data_pointer: int = 0

        # Prolog-specific state
        self.prolog_kb: Dict[str, List] = {}  # Prolog knowledge base

        # Pascal-specific state
        self.pascal_procs: Dict[str, Dict[str, Any]] = {}  # Pascal procedures
        self.pascal_types: Dict[str, str] = {}  # Pascal type definitions
        self.pascal_block_stack: List[Dict[str, Any]] = []  # Pascal block tracking
        self.pascal_call_stack: List[Dict[str, Any]] = []  # Pascal call stack

        # C-specific state
        self.c_block_stack: List[Dict[str, Any]] = (
            []
        )  # C block tracking for control flow

        # Debugging state
        self.debug_mode: bool = False
        self.debug_event = threading.Event()
        self.debug_callback: DebugCallback = None
        self.breakpoints: set = set()
        self.step_mode: bool = False

        # Language mode
        self.language = Language.BASIC
        # Default type mapping
        self._reset_type_defaults()

    def reset(self):
        """Reset interpreter state"""
        self.variables.clear()
        self.int_variables.clear()
        self.long_variables.clear()
        self.single_variables.clear()
        self.double_variables.clear()
        self.string_variables.clear()
        self.arrays.clear()
        self.logo_lists.clear()
        self.logo_arrays.clear()
        self.property_lists.clear()
        self.logo_procedures.clear()
        self.logo_procedure_params.clear()
        self.output.clear()
        self.text_lines.clear()
        self.program_lines.clear()
        self.current_line = 0
        self.labels.clear()
        self.line_number_map.clear()
        self.gosub_stack.clear()
        self.for_stack.clear()
        self.match_flag = False
        self.last_match_set = False
        self.stored_condition = None
        self.cursor_row = 0
        self.step_mode = False
        self.running = True
        # Ensure debugger starts in a paused-clear state; do not auto-resume
        self.debug_event.clear()
        self.cursor_col = 0
        self.logo_procedures.clear()
        # BASIC-specific reset
        self.basic_while_stack.clear()
        self.basic_do_stack.clear()
        self.basic_select_expression = ""
        self.basic_in_select = False
        self.basic_in_sub = False
        self.basic_in_function = False
        self.data_values.clear()
        self.data_pointer = 0
        self._reset_type_defaults()

    # ---------- Typed variable support ----------
    def _reset_type_defaults(self):
        """Initialize default type map to DOUBLE for A-Z."""
        self.default_type_map: Dict[str, str] = {
            chr(c): "double" for c in range(ord("A"), ord("Z") + 1)
        }

    def set_default_type_for_spec(self, type_code: str, spec: str):
        """Set default type for letter ranges specified in spec.

        spec examples: "A-Z", "A-C,H,L-P", "X", "A,B,C"
        type_code: one of 'int','long','single','double','string'
        """
        if not spec:
            return
        for part in spec.replace(" ", "").split(","):
            if not part:
                continue
            if "-" in part and len(part) >= 3:
                start, end = part.split("-", 1)
                if start and end:
                    s = start[0].upper()
                    e = end[0].upper()
                    for c in range(ord(s), ord(e) + 1):
                        self.default_type_map[chr(c)] = type_code
            else:
                ch = part[0].upper()
                if "A" <= ch <= "Z":
                    self.default_type_map[ch] = type_code

    def _resolve_var_base_and_type(self, name: str) -> Tuple[str, str]:
        """Return (base_name, type_code) for a variable name.

        type_code in {'string','int','long','single','double'}
        Suffix overrides defaults. Base name is uppercased without suffix.
        """
        n = name.strip().upper()
        if not n:
            return "", "double"
        if n[-1] in ("$", "%", "&", "!", "#"):
            base = n[:-1]
            suf = n[-1]
            if suf == "$":
                return base, "string"
            mapping = {"%": "int", "&": "long", "!": "single", "#": "double"}
            return base, mapping[suf]
        # No suffix: use default by first letter
        initial = n[0]
        type_code = self.default_type_map.get(initial, "double")
        return n, type_code

    def get_var_base_and_type(self, name: str) -> Tuple[str, str]:
        """Public helper to get (base_name, type_code) for a variable name.

        Provides a stable API for language executors without exposing
        the private implementation details.
        """
        return self._resolve_var_base_and_type(name)

    def _coerce_numeric(self, value: float, type_code: str):
        if type_code == "int":
            return int(float(value))
        if type_code == "long":
            return int(float(value))
        if type_code in ("single", "double"):
            return float(value)
        return float(value)

    def set_typed_variable(self, var_name: str, value: str | float):
        """Assign typed value and mirror into aggregate stores."""
        base, t = self._resolve_var_base_and_type(var_name)
        if not base:
            return
        if t == "string":
            self.string_variables[base + "$"] = str(value)
            return
        # Numeric
        try:
            num = float(value)
        except (ValueError, TypeError):
            num = 0.0
        coerced = self._coerce_numeric(num, t)
        if t == "int":
            # Always store int-typed variables as int
            self.int_variables[base + "%"] = int(coerced)
        elif t == "long":
            # Long variables treated as integers
            self.long_variables[base + "&"] = int(coerced)
        elif t == "single":
            self.single_variables[base + "!"] = float(coerced)
        else:
            self.double_variables[base + "#"] = float(coerced)
        self.variables[base] = float(coerced)

    def get_numeric_value(self, var_name: str) -> Optional[float]:
        """Get numeric value by name (with or without suffix)."""
        base, t = self._resolve_var_base_and_type(var_name)
        if not base:
            return None
        # Try exact typed key first, falling back to the aggregate store.
        key_map = {"int": "%", "long": "&", "single": "!", "double": "#"}
        result: Optional[float] = None

        if t in key_map:
            key = base + key_map[t]
            if key in self.int_variables:
                result = float(self.int_variables[key])
            elif key in self.long_variables:
                result = float(self.long_variables[key])
            elif key in self.single_variables:
                result = float(self.single_variables[key])
            elif key in self.double_variables:
                result = float(self.double_variables[key])

        # Fallback to aggregate base if not yet resolved
        if result is None and base in self.variables:
            result = float(self.variables[base])

        return result

    def set_language(self, language: Language):
        """Set the active language for execution."""
        self.language = language

    def load_program(
        self,
        program_text: str,
        language: Optional[Language] = None,
    ):
        """
        Parse and load program into memory

        Args:
            program_text: Source code to parse
            language: Optional language override

        Extracts line numbers, PILOT labels, and builds
        execution index
        """
        self.reset()
        if language:
            self.language = language

        lines = program_text.splitlines()
        self.program_lines.clear()
        self.line_number_map.clear()

        self._load_from_lines(lines, language or self.language)

        # Collect BASIC DATA values after lines are populated
        if self.language == Language.BASIC:
            self._collect_basic_data_values()

    def _load_from_lines(self, lines: List[str], language: Language):
        """Populate program_lines, labels and the line-number map.
        Handles per-language parsing.
        """
        if language == Language.LOGO:
            self._parse_logo_program(lines)
            return
        if language == Language.FORTH:
            self._parse_forth_program(lines)
            return
        self._parse_standard_program(lines)

    def _parse_forth_program(self, lines: List[str]):
        """Parse Forth program (no line numbers)."""
        for idx, line in enumerate(lines):
            # Forth doesn't use line numbers, so we just store the whole line
            # We use the index as the line number for internal tracking
            self.program_lines.append((idx + 1, line))

    def _parse_standard_program(self, lines: List[str]):
        """Parse BASIC/PILOT/C/PASCAL/PROLOG lines and build indices."""
        for idx, line in enumerate(lines):
            line_num, command_str = self._parse_line(line)

            if line_num is not None:
                self.line_number_map[line_num] = idx

            if command_str.startswith("L:"):
                label = command_str[2:].strip()
                self.labels[label] = idx
            elif command_str.startswith("*"):
                label = command_str.strip()
                self.labels[label] = idx

            self.program_lines.append((line_num, command_str))

    def _collect_basic_data_values(self):
        """Extract BASIC DATA values into data_values list."""
        for _, command_str in self.program_lines:
            cmd = command_str.strip()
            if cmd.upper().startswith("DATA "):
                values = cmd[5:].split(",")
                for v in values:
                    val = v.strip()
                    if val.startswith('"') and val.endswith('"'):
                        val = val[1:-1]
                    self.data_values.append(val)

    def _parse_logo_program(self, lines: List[str]):
        """
        Parse Logo program, handling TO/END procedure definitions
        and multi-line commands.
        """
        i = 0
        current_command_lines: List[str] = []
        bracket_depth = 0

        while i < len(lines):
            line = lines[i].strip()
            if not line:
                i += 1
                continue

            # Check for TO command (only at top level)
            if bracket_depth == 0 and line.upper().startswith("TO "):
                # Flush any pending command
                if current_command_lines:
                    full_cmd = " ".join(current_command_lines)
                    line_num, command_str = self._parse_line(full_cmd)
                    self.program_lines.append((line_num, command_str))
                    current_command_lines = []

                # Parse procedure definition
                proc_name = self._parse_logo_procedure(lines, i)
                if proc_name:
                    # Skip to END
                    while i < len(lines) and lines[i].strip().upper() != "END":
                        i += 1
                    if i < len(lines):  # Skip the END line
                        i += 1
                    continue

            # Count brackets in this line
            open_brackets = line.count("[")
            close_brackets = line.count("]")

            # If we are inside a comment, ignore brackets?
            # Simple counting might be enough for now.

            current_command_lines.append(line)
            bracket_depth += open_brackets - close_brackets

            if bracket_depth <= 0:
                # Command complete
                full_cmd = " ".join(current_command_lines)
                line_num, command_str = self._parse_line(full_cmd)
                self.program_lines.append((line_num, command_str))
                current_command_lines = []
                bracket_depth = 0

            i += 1

        # Flush any remaining lines
        if current_command_lines:
            full_cmd = " ".join(current_command_lines)
            line_num, command_str = self._parse_line(full_cmd)
            self.program_lines.append((line_num, command_str))

    def _parse_logo_procedure(
        self,
        lines: List[str],
        start_idx: int,
    ) -> Optional[str]:
        """Parse a Logo procedure definition starting at TO"""
        line = lines[start_idx].strip()
        parts = line.upper().split()
        if len(parts) < 2 or parts[0] != "TO":
            return None

        proc_name = parts[1]

        # Parse parameters (starting with :)
        params = []
        for part in parts[2:]:
            if part.startswith(":"):
                params.append(part[1:])  # Store without colon

        # Find END
        end_idx = start_idx + 1
        while end_idx < len(lines) and lines[end_idx].strip().upper() != "END":
            end_idx += 1

        if end_idx >= len(lines):
            return None  # No END found

        # Extract procedure body (lines between TO and END)
        body_lines = []
        for j in range(start_idx + 1, end_idx):
            body_lines.append(lines[j].strip())

        # Store procedure body and params
        self.logo_procedures[proc_name] = "\n".join(body_lines)
        self.logo_procedure_params[proc_name] = params
        return proc_name

    def execute(self, turtle: "TurtleState") -> List[str]:
        """
        Execute loaded program with timeout and iteration protection

        Args:
            turtle: Graphics state for turtle commands

        Returns:
            List of output lines (text and error messages)

        Raises:
            RuntimeError: On timeout or fatal execution error

        Features error recovery: continues on non-fatal errors
        """
        # Setup runtime (fresh run detection, counters, timer)
        iterations, start_time = self._setup_runtime()

        # Execute main loop with error recovery and debugging hooks
        iterations = self._run_and_collect(turtle, iterations, start_time)

        # Finalize and emit any warnings/output
        return self._finalize_and_emit(iterations)

    def _setup_runtime(self) -> Tuple[int, float]:
        """Prepare execution state: fresh-run output clear, counters and timer.

        Returns (iterations, start_time).
        """
        if self.current_line == 0:
            self.output.clear()
        return 0, time.time()

    def _run_and_collect(
        self, turtle: "TurtleState", iterations: int, start_time: float
    ) -> int:
        """Run main execution loop; returns total iterations performed."""
        while (
            self.current_line < len(self.program_lines)
            and iterations < self.MAX_ITERATIONS
        ):
            _, current_command = self.program_lines[self.current_line]

            # Use physical line number (1-based) for breakpoints and UI
            physical_line = self.current_line + 1

            if self._should_break(current_command, physical_line):
                self._do_debug_pause(physical_line)

            self._check_timeout(start_time)

            iterations += 1

            command = current_command
            # print(f"DEBUG: Line {self.current_line}: {command}")

            if not command.strip():
                self.current_line += 1
                continue

            success = self._execute_line_safely(command, turtle)
            if not success:
                self.current_line += 1
                continue

            if not self.running:
                break

            if self.pending_input:
                break

            self.current_line += 1

        return iterations

    def _finalize_and_emit(self, iterations: int) -> List[str]:
        """Emit warnings if needed and return a copy of the output buffer."""
        if iterations >= self.MAX_ITERATIONS:
            self.log_output("⚠️ Warning: Maximum iterations reached")
        return self.output.copy()

    def _should_break(
        self, current_command: str, line_number: int
    ) -> bool:
        if not (self.debug_mode and current_command.strip()):
            return False
        if self.step_mode:
            return True
        return line_number in self.breakpoints

    def _do_debug_pause(self, line_number: int):
        if self.debug_callback:
            # pylint: disable=not-callable
            self.debug_callback(line_number, self.get_variables())
        self.debug_event.wait()
        self.debug_event.clear()

    def _check_timeout(self, start_time: float):
        if time.time() - start_time > self.MAX_EXECUTION_TIME:
            self.log_output("❌ Error: Execution timeout (10 seconds exceeded)")

    def _execute_line_safely(
        self,
        command: str,
        turtle: "TurtleState",
    ) -> bool:
        """Execute a line with error recovery and suggestion hints.
        Returns True when execution succeeded.
        """
        try:
            self._execute_line(command, turtle)
            return True
        except Exception as e:  # pylint: disable=broad-exception-caught
            # Re-raise critical system signals rather than silently swallowing
            if isinstance(e, (KeyboardInterrupt, SystemExit)):
                raise
            error_msg = f"❌ Error at line {self.current_line + 1}: {e}"
            syntax_error = check_syntax_mistakes(command)
            if syntax_error:
                error_msg += f"\n   💡 {syntax_error}"
            if "Unknown" in str(e) or "Invalid" in str(e):
                first_word = command.split()[0] if command.split() else ""
                suggestion = suggest_command(first_word)
                if suggestion:
                    error_msg += f"\n   💡 Did you mean '{suggestion}'?"
            self.log_output(error_msg)
            return False

    def _execute_line(self, command: str, turtle: "TurtleState") -> str:
        """Execute a single line and return output text.

        Args:
            command: Command string to execute
            turtle: Turtle state for graphics

        Returns:
            Output text from command execution
        """
        # Dispatch to language-specific executor
        if self.language == Language.BASIC:
            output = execute_basic(self, command, turtle)
        elif self.language == Language.PILOT:
            output = execute_pilot(self, command, turtle)
        elif self.language == Language.LOGO:
            output = execute_logo(self, command, turtle)
        elif self.language == Language.C:
            output = execute_c(self, command, turtle)
        elif self.language == Language.PASCAL:
            output = execute_pascal(self, command, turtle)
        elif self.language == Language.PROLOG:
            output = execute_prolog(self, command, turtle)
        elif self.language == Language.FORTH:
            output = execute_forth(self, command, turtle)
        else:
            raise ValueError(f"Unsupported language: {self.language}")

        self.log_output(output)
        return output  # Note: _determine_command_type removed - now using
        # language-specific executors.

    def _parse_line(self, line: str) -> Tuple[Optional[int], str]:
        """Parse line number if present, return (line_num, command)"""
        line = line.rstrip()
        parts = line.split(maxsplit=1)

        if parts and parts[0].isdigit():
            line_num = int(parts[0])
            command = parts[1] if len(parts) > 1 else ""
            return (line_num, command.strip())

        return (None, line)

    def log_output(self, text: str):
        """Add text to output buffer (helper method)."""
        if text:  # Include all output, even blank lines
            self.output.append(text)
            if self.output_callback:
                # pylint: disable=not-callable
                self.output_callback(text)

    def evaluate_expression(self, expr: str) -> float:
        """
        Safely evaluate mathematical expression

        Args:
            expr: Expression like "2 + 3 * X"

        Returns:
            Computed result

        Uses safe expression evaluator (no eval/exec)
        """
        # Build numeric variables from typed stores using base names
        num_vars: Dict[str, float] = {}
        for k, v in self.int_variables.items():
            num_vars[k[:-1]] = float(v)
        for k, v in self.long_variables.items():
            num_vars[k[:-1]] = float(v)
        for k, v in self.single_variables.items():  # type: ignore[assignment]
            num_vars[k[:-1]] = float(v)  # type: ignore[assignment]
        for k, v in self.double_variables.items():  # type: ignore[assignment]
            num_vars[k[:-1]] = float(v)  # type: ignore[assignment]
        # Back-compat overlay
        num_vars.update(self.variables)

        evaluator = ExpressionEvaluator(num_vars, self.arrays.copy())
        return evaluator.evaluate(expr)

    def interpolate_text(self, text: str) -> str:
        """
        Replace *VAR* or #VAR with variable values

        Args:
            text: String like "Hello *NAME*" or "X is #X"

        Returns:
            Interpolated string like "Hello World" or "X is 5"

        Supports both BASIC (*VAR*) and PILOT (#VAR) syntax
        """
        result = text

        # Handle PILOT #VAR syntax first
        if "#" in result:

            def replace_hash_var(match):
                var_name = match.group(1).upper()
                if var_name in self.variables:
                    return str(self.variables[var_name])
                if var_name in self.string_variables:
                    return self.string_variables[var_name]
                return match.group(0)  # Keep original #VAR

            result = self.PILOT_VAR_PATTERN.sub(replace_hash_var, result)

        # Handle BASIC *VAR* syntax
        if "*" not in result:
            return result

        # Build result incrementally (O(n) vs O(n*m) for repeated replace)
        parts = []
        last_end = 0

        for match in self.VAR_INTERPOLATION_PATTERN.finditer(result):
            parts.append(result[last_end : match.start()])  # noqa: E203
            var_name = match.group(1)

            if var_name in self.variables:
                parts.append(str(self.variables[var_name]))
            elif var_name in self.string_variables:
                parts.append(self.string_variables[var_name])
            else:
                parts.append(match.group(0))  # Keep original *VAR*

            last_end = match.end()

        parts.append(result[last_end:])
        return "".join(parts)

    def request_input(self, prompt: str) -> str:
        """Request input synchronously via callback"""
        if self.input_callback:
            return self.input_callback(prompt)  # pylint: disable=not-callable
        return ""

    def start_input_request(
        self,
        prompt: str,
        var_name: str,
        is_numeric: bool = False,
    ):
        """Initiate pending input request for UI"""
        self.pending_input = InputRequest(prompt, var_name, is_numeric)
        self.pending_resume_line = self.current_line

    def provide_input(self, value: str):
        """Provide input value and resume execution"""
        if not self.pending_input:
            return

        req = self.pending_input
        self.last_input = value

        # Store respecting typed variables
        self.set_typed_variable(req.var_name, value)

        # Clear pending state
        self.pending_input = None
        if self.pending_resume_line is not None:
            self.current_line = self.pending_resume_line + 1

    def get_inkey(self) -> str:
        """Get last key pressed (INKEY$ support)"""
        if self.inkey_callback:
            key = self.inkey_callback()  # pylint: disable=not-callable
            return key if key else ""

        if self.last_key_pressed:
            key = self.last_key_pressed
            self.last_key_pressed = None
            return key

        return ""

    def jump_to_label(self, label: str):
        """Jump execution to a labeled line (PILOT L: labels).

        Args:
            label: Label name to jump to
        """
        if label in self.labels:
            self.current_line = self.labels[label]
        else:
            raise ValueError(f"Label '{label}' not found")

    def jump_to_line_number(self, line_num: int):
        """Jump execution to a BASIC line number.

        Args:
            line_num: Line number to jump to
        """
        if line_num in self.line_number_map:
            # Set to index - 1 because the main loop increments current_line
            self.current_line = self.line_number_map[line_num] - 1
        else:
            raise ValueError(f"Line number {line_num} not found")

    # Debugging methods
    def set_debug_mode(self, enabled: bool):
        """Enable or disable debug mode"""
        self.debug_mode = enabled

    def set_debug_callback(self, callback: DebugCallback):
        """Set callback for debug events."""
        self.debug_callback = callback

    def add_breakpoint(self, line: int):
        """Add a breakpoint at the specified line"""
        self.breakpoints.add(line)

    def remove_breakpoint(self, line: int):
        """Remove a breakpoint from the specified line"""
        self.breakpoints.discard(line)

    def clear_breakpoints(self):
        """Clear all breakpoints"""
        self.breakpoints.clear()

    def resume_execution(self):
        """Resume execution until next breakpoint"""
        self.step_mode = False
        self.debug_event.set()

    def step_execution(self):
        """Execute one line and pause"""
        self.step_mode = True
        self.debug_event.set()

    def pause_execution(self):
        """Pause execution at the next line"""
        self.step_mode = True

    def get_variables(self) -> Dict[str, Any]:
        """Get all current variables for debugging"""
        vars_dict: Dict[str, Any] = {}
        # Include typed variables with suffixes to avoid collisions
        vars_dict.update(self.int_variables)
        vars_dict.update(self.long_variables)
        vars_dict.update(self.single_variables)
        vars_dict.update(self.double_variables)
        vars_dict.update(self.string_variables)
        # Also include aggregate numeric (base names)
        vars_dict.update(self.variables)
        # Add arrays and other state if needed
        return vars_dict
