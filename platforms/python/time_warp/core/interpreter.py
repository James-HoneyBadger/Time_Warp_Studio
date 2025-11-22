"""
Core interpreter for Time Warp IDE
Ported from Rust: time_warp_unified::interpreter::mod.rs
"""

import re
import time
import threading
from enum import Enum, auto
from typing import Optional, List, Dict, Tuple, Callable, TYPE_CHECKING, Any
from dataclasses import dataclass

from ..utils.error_hints import check_syntax_mistakes, suggest_command

# Import language executors
from ..languages.basic import execute_basic
from ..languages.pilot import execute_pilot
from ..languages.logo import execute_logo


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

    @classmethod
    def from_extension(cls, ext: str) -> "Language":
        ext = ext.lower()
        if ext == ".bas":
            return cls.BASIC
        elif ext == ".pilot":
            return cls.PILOT
        elif ext == ".logo":
            return cls.LOGO
        else:  # Default to BASIC for unknown extensions
            return cls.BASIC

    def friendly_name(self) -> str:
        """Human-friendly display name for language."""
        if self == Language.BASIC:
            return "BASIC"
        if self == Language.PILOT:
            return "PILOT"
        if self == Language.LOGO:
            return "Logo"
        return "Unknown"


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
    Ported from Rust time_warp_unified::interpreter::Interpreter

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

    # Security limits
    MAX_ITERATIONS = 100_000
    MAX_EXECUTION_TIME = 10.0  # seconds

    # Variable interpolation pattern (matches *VAR*)
    VAR_INTERPOLATION_PATTERN = re.compile(r"\*([A-Z_][A-Z0-9_]*)\*")

    def __init__(self):
        # Core state
        self.variables: Dict[str, float] = {}
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
        self.property_lists: Dict[str, Dict[str, object]] = {}  # Logo procedures
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
        self.open_files: Dict[str, object] = {}  # For PILOT F: commands

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

        # Debugging state
        self.debug_mode: bool = False
        self.debug_event = threading.Event()
        self.debug_callback: Optional[Callable[[int, Dict[str, Any]], None]] = None
        self.breakpoints: set = set()
        self.step_mode: bool = False

        # Language mode
        self.language = Language.BASIC

    def reset(self):
        """Reset interpreter state"""
        self.variables.clear()
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
        self.debug_event.set()  # Ensure we don't get stuck if reset while paused
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

        # Handle Logo procedure definitions during loading
        if language == Language.LOGO:
            self._parse_logo_program(lines)
        else:
            # Standard parsing for BASIC and PILOT
            for idx, line in enumerate(lines):
                line_num, command_str = self._parse_line(line)

                # Build line number mapping for BASIC GOTO/GOSUB
                if line_num is not None:
                    self.line_number_map[line_num] = idx

                # Collect PILOT labels
                if command_str.startswith("L:"):
                    label = command_str[2:].strip()
                    self.labels[label] = idx
                elif command_str.startswith("*"):
                    label = command_str.strip()
                    self.labels[label] = idx

                self.program_lines.append((line_num, command_str))

            # Collect BASIC DATA values
            if self.language == Language.BASIC:
                for _, command_str in self.program_lines:
                    cmd = command_str.strip().upper()
                    if cmd.startswith("DATA "):
                        # Parse values
                        values = command_str[5:].split(",")
                        for v in values:
                            self.data_values.append(v.strip())

    def _parse_logo_program(self, lines: List[str]):
        """
        Parse Logo program, handling TO/END procedure definitions
        and multi-line commands.
        """
        i = 0
        current_command_lines = []
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
        # Only reset output at start of fresh run
        if self.current_line == 0:
            self.output.clear()

        iterations = 0
        start_time = time.time()

        while (
            self.current_line < len(self.program_lines)
            and iterations < self.MAX_ITERATIONS
        ):
            # Debugging support
            should_break = self.debug_mode and (
                self.step_mode or (self.current_line + 1) in self.breakpoints
            )

            if should_break:
                if self.debug_callback:
                    # pylint: disable=not-callable
                    self.debug_callback(self.current_line + 1, self.get_variables())
                self.debug_event.wait()
                self.debug_event.clear()

            # Security check: Timeout protection
            if time.time() - start_time > self.MAX_EXECUTION_TIME:
                self.log_output(
                    "❌ Error: Execution timeout (10 seconds exceeded)"
                )  # noqa: E501

            iterations += 1

            _, command = self.program_lines[self.current_line]

            if not command.strip():
                self.current_line += 1
                continue

            # Error recovery: Continue on non-fatal errors
            try:
                self._execute_line(command, turtle)
            except Exception as e:  # pylint: disable=broad-exception-caught
                # Enhanced error message with context and suggestions
                error_msg = f"❌ Error at line {self.current_line + 1}: {e}"

                # Check for syntax mistakes
                syntax_error = check_syntax_mistakes(command)
                if syntax_error:
                    error_msg += f"\n   💡 {syntax_error}"

                # Suggest command corrections
                if "Unknown" in str(e) or "Invalid" in str(e):
                    first_word = command.split()[0] if command.split() else ""
                    suggestion = suggest_command(first_word)
                    if suggestion:
                        error_msg += f"\n   💡 Did you mean '{suggestion}'?"

                self.log_output(error_msg)
                self.current_line += 1
                continue

            # Continue to next line (unless jump or end occurred)
            if not self.running:
                break

            # Check if waiting for input
            if self.pending_input:
                break

            self.current_line += 1

        if iterations >= self.MAX_ITERATIONS:
            self.log_output("⚠️ Warning: Maximum iterations reached")

        return self.output.copy()

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
        if text and text.strip():
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
        from ..utils.expression_evaluator import ExpressionEvaluator

        evaluator = ExpressionEvaluator(self.variables.copy(), self.arrays.copy())
        return evaluator.evaluate(expr)

    def interpolate_text(self, text: str) -> str:
        """
        Replace *VAR* with variable values

        Args:
            text: String like "Hello *NAME*"

        Returns:
            Interpolated string like "Hello World"

        Fast path: Skip regex if no asterisks present
        """
        if "*" not in text:
            return text

        # Build result incrementally (O(n) vs O(n*m) for repeated replace)
        result = []
        last_end = 0

        for match in self.VAR_INTERPOLATION_PATTERN.finditer(text):
            result.append(text[last_end : match.start()])  # noqa: E203
            var_name = match.group(1)

            if var_name in self.variables:
                result.append(str(self.variables[var_name]))
            elif var_name in self.string_variables:
                result.append(self.string_variables[var_name])
            else:
                result.append(match.group(0))  # Keep original *VAR*

            last_end = match.end()

        result.append(text[last_end:])
        return "".join(result)

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

        # Store value in appropriate variable type
        try:
            num_value = float(value)
            self.variables[req.var_name] = num_value
        except ValueError:
            self.string_variables[req.var_name] = value

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
            self.current_line = self.line_number_map[line_num]
        else:
            raise ValueError(f"Line number {line_num} not found")

    # Debugging methods
    def set_debug_mode(self, enabled: bool):
        """Enable or disable debug mode"""
        self.debug_mode = enabled

    def set_debug_callback(self, callback: Callable[[int, Dict[str, Any]], None]):
        """Set callback for debug events"""
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
        vars_dict = {}
        vars_dict.update(self.variables)
        vars_dict.update(self.string_variables)
        # Add arrays and other state if needed
        return vars_dict
