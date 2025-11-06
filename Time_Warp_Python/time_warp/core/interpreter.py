"""
Core interpreter for Time Warp IDE
Ported from Rust: time_warp_unified::interpreter::mod.rs
"""

import re
import time
from enum import Enum, auto
from typing import Optional, List, Dict, Tuple, Callable, TYPE_CHECKING
from dataclasses import dataclass


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
    """Single unified language for Time Warp"""
    TEMPLECODE = auto()

    @classmethod
    def from_extension(cls, ext: str) -> 'Language':
        """All supported extensions map to TempleCode."""
        return cls.TEMPLECODE

    def name(self) -> str:
        return "TempleCode"


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
    Central execution engine for TempleCode (unified BASIC, PILOT, Logo
    semantics).
    
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
    VAR_INTERPOLATION_PATTERN = re.compile(r'\*([A-Z_][A-Z0-9_]*)\*')
    
    def __init__(self):
        # Core state
        self.variables: Dict[str, float] = {}
        self.string_variables: Dict[str, str] = {}
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
        
        # I/O handling
        self.input_callback: Optional[Callable[[str], str]] = None
        self.last_input: str = ""
        self.pending_input: Optional[InputRequest] = None
        self.pending_resume_line: Optional[int] = None
        
        # INKEY$ support
        self.inkey_callback: Optional[Callable[[], Optional[str]]] = None
        self.last_key_pressed: Optional[str] = None
        
        # Logo/TempleCode procedures (user-defined)
        self.logo_procedures: Dict[str, object] = {}

        # Screen state
        self.screen_mode = ScreenMode.GRAPHICS
        self.text_lines: List[str] = []
        self.cursor_row: int = 0
        self.cursor_col: int = 0

        # Language mode (TempleCode only)
        self.current_language = Language.TEMPLECODE
    
    def reset(self):
        """Reset interpreter state"""
        self.variables.clear()
        self.string_variables.clear()
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
        self.cursor_col = 0
        self.logo_procedures.clear()
    
    def load_program(self, program_text: str):
        """
        Parse and load program into memory
        
        Args:
            program_text: Source code to parse
            
        Extracts line numbers, PILOT labels, and builds execution index
        """
        self.reset()
        
        lines = program_text.splitlines()
        self.program_lines.clear()
        self.line_number_map.clear()
        
        for idx, line in enumerate(lines):
            line_num, command_str = self._parse_line(line)
            
            # Build line number mapping for BASIC GOTO/GOSUB
            if line_num is not None:
                self.line_number_map[line_num] = idx
            
            # Collect PILOT labels
            if command_str.startswith("L:"):
                label = command_str[2:].strip()
                self.labels[label] = idx
            
            self.program_lines.append((line_num, command_str))
    
    def execute(self, turtle: 'TurtleState') -> List[str]:
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
        
        while self.current_line < len(self.program_lines) and iterations < self.MAX_ITERATIONS:
            # Security check: Timeout protection
            if time.time() - start_time > self.MAX_EXECUTION_TIME:
                self.log_output("❌ Error: Execution timeout (10 seconds exceeded)")
                raise RuntimeError("Execution timeout exceeded")
            
            iterations += 1
            
            line_num, command = self.program_lines[self.current_line]
            
            if not command.strip():
                self.current_line += 1
                continue
            
            # Error recovery: Continue on non-fatal errors
            try:
                self._execute_line(command, turtle)
            except Exception as e:
                # Enhanced error message with context and suggestions
                error_msg = f"❌ Error at line {self.current_line + 1}: {e}"
                
                # Check for syntax mistakes
                from ..utils.error_hints import check_syntax_mistakes, suggest_command
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
                
            self.current_line += 1
        
        if iterations >= self.MAX_ITERATIONS:
            self.log_output("⚠️ Warning: Maximum iterations reached")
        
        return self.output.copy()
    
    def _execute_line(self, command: str, turtle: 'TurtleState') -> str:
        """Execute a single line and return output text.
        
        Args:
            command: Command string to execute
            turtle: Turtle state for graphics
            
        Returns:
            Output text from command execution
        """
        # Unified TempleCode execution path
        from ..languages.templecode import execute_templecode
        output = execute_templecode(self, command, turtle)
        self.log_output(output)
        return output
    
    # Note: _determine_command_type removed in TempleCode-only mode.
    
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
            # Executors already append to output list
            pass
    
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
        evaluator = ExpressionEvaluator(self.variables.copy())
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
        if '*' not in text:
            return text
        
        # Build result incrementally (O(n) vs O(n*m) for repeated replace)
        result = []
        last_end = 0
        
        for match in self.VAR_INTERPOLATION_PATTERN.finditer(text):
            result.append(text[last_end:match.start()])
            var_name = match.group(1)
            
            if var_name in self.variables:
                result.append(str(self.variables[var_name]))
            elif var_name in self.string_variables:
                result.append(self.string_variables[var_name])
            else:
                result.append(match.group(0))  # Keep original *VAR*
            
            last_end = match.end()
        
        result.append(text[last_end:])
        return ''.join(result)
    
    def request_input(self, prompt: str) -> str:
        """Request input synchronously via callback"""
        if self.input_callback:
            return self.input_callback(prompt)
        return ""
    
    def start_input_request(self, prompt: str, var_name: str, is_numeric: bool = False):
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
            key = self.inkey_callback()
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
