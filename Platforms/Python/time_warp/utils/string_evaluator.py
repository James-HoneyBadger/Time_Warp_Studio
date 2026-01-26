"""
String expression evaluator for Time Warp Studio
Handles BASIC string functions like LEN, LEFT, RIGHT, MID, INSTR, UPPER, LOWER, TRIM, STR, VAL
"""

import re
from typing import Dict, List, Optional, Union


class StringExpressionEvaluator:
    """Evaluator for BASIC string expressions and functions.

    Supports:
    - String functions: LEN, LEFT, RIGHT, MID, INSTR, UPPER, LOWER, TRIM, STR, VAL
    - String variables: A$, B$, etc.
    - String concatenation: A$ & B$ or A$ + B$
    - Nested function calls
    """

    # Pattern to match function calls: FUNC(args)
    FUNCTION_PATTERN = re.compile(r"([A-Z]+)\s*\(", re.IGNORECASE)

    # Pattern to match string literals: "..." or '...'
    STRING_LITERAL_PATTERN = re.compile(r'("[^"]*"|\'[^\']*\')')

    def __init__(
        self,
        string_variables: Optional[Dict[str, str]] = None,
        numeric_variables: Optional[Dict[str, float]] = None,
    ):
        """Initialize string evaluator.

        Args:
            string_variables: Dict of string variables (e.g., {"A$": "hello"})
            numeric_variables: Dict of numeric variables for VAL context
        """
        self.string_variables = string_variables or {}
        self.numeric_variables = numeric_variables or {}

    def evaluate(self, expr: str) -> str:
        """Evaluate string expression.

        Args:
            expr: Expression like 'LEFT(A$, 3)' or 'UPPER("hello")'

        Returns:
            Evaluated string result

        Raises:
            ValueError: On invalid expression or function
        """
        expr = expr.strip()
        return self._evaluate_expr(expr)

    def _evaluate_expr(self, expr: str) -> str:
        """Recursively evaluate expression."""
        expr = expr.strip()

        # Handle string literals
        if (expr.startswith('"') and expr.endswith('"')) or (
            expr.startswith("'") and expr.endswith("'")
        ):
            return expr[1:-1]

        # Handle string variables
        if expr.endswith("$") and expr.replace("$", "").replace("_", "").isalnum():
            return self.string_variables.get(expr, "")

        # Handle function calls
        func_match = self.FUNCTION_PATTERN.match(expr)
        if func_match:
            return self._evaluate_function(expr)

        # Handle string concatenation
        if "&" in expr:
            return self._evaluate_concatenation(expr, "&")
        if "+" in expr and not self._is_numeric(expr):
            return self._evaluate_concatenation(expr, "+")

        # If it looks like a variable reference, return empty if not found
        if expr.isalpha() or (expr.isalnum() and expr.endswith("$")):
            return self.string_variables.get(expr.upper(), "")

        return str(expr)

    def _is_numeric(self, expr: str) -> bool:
        """Check if expression is purely numeric."""
        try:
            float(expr)
            return True
        except (ValueError, TypeError):
            return False

    def _evaluate_function(self, expr: str) -> str:
        """Evaluate function call."""
        # Find function name and arguments
        paren_idx = expr.find("(")
        if paren_idx == -1:
            return str(expr)

        func_name = expr[:paren_idx].strip().upper()

        # Find matching closing paren
        close_idx = self._find_matching_paren(expr, paren_idx)
        if close_idx == -1:
            raise ValueError(f"Mismatched parentheses in: {expr}")

        args_str = expr[paren_idx + 1:close_idx].strip()
        args = self._parse_arguments(args_str)

        # Route to appropriate function handler
        if func_name == "LEN":
            return self._func_len(args)
        elif func_name == "LEFT":
            return self._func_left(args)
        elif func_name == "RIGHT":
            return self._func_right(args)
        elif func_name == "MID":
            return self._func_mid(args)
        elif func_name == "INSTR":
            return self._func_instr(args)
        elif func_name == "UPPER":
            return self._func_upper(args)
        elif func_name == "LOWER":
            return self._func_lower(args)
        elif func_name == "TRIM":
            return self._func_trim(args)
        elif func_name == "STR":
            return self._func_str(args)
        elif func_name == "VAL":
            return self._func_val(args)
        else:
            raise ValueError(f"Unknown string function: {func_name}")

    def _find_matching_paren(self, expr: str, start_idx: int) -> int:
        """Find matching closing parenthesis."""
        depth = 1
        i = start_idx + 1
        in_string = False
        string_char = None

        while i < len(expr) and depth > 0:
            if not in_string:
                if expr[i] in ('"', "'"):
                    in_string = True
                    string_char = expr[i]
                elif expr[i] == "(":
                    depth += 1
                elif expr[i] == ")":
                    depth -= 1
            else:
                if expr[i] == string_char and (i == 0 or expr[i - 1] != "\\"):
                    in_string = False
            i += 1

        return i - 1 if depth == 0 else -1

    def _parse_arguments(self, args_str: str) -> List[str]:
        """Parse comma-separated arguments, respecting strings and parens."""
        args = []
        current_arg = []
        depth = 0
        in_string = False
        string_char = None

        for char in args_str:
            if not in_string:
                if char in ('"', "'"):
                    in_string = True
                    string_char = char
                    current_arg.append(char)
                elif char == "(":
                    depth += 1
                    current_arg.append(char)
                elif char == ")":
                    depth -= 1
                    current_arg.append(char)
                elif char == "," and depth == 0:
                    args.append("".join(current_arg).strip())
                    current_arg = []
                else:
                    current_arg.append(char)
            else:
                current_arg.append(char)
                if char == string_char:
                    in_string = False

        if current_arg:
            args.append("".join(current_arg).strip())

        return args

    def _evaluate_arg(self, arg: str) -> Union[str, float]:
        """Evaluate argument - could be string or numeric."""
        arg = arg.strip()

        # If it's a function, evaluate it
        if self.FUNCTION_PATTERN.match(arg):
            return self._evaluate_function(arg)

        # If it looks like a numeric literal
        try:
            return float(arg)
        except (ValueError, TypeError):
            pass

        # Try to evaluate as string expression
        return self._evaluate_expr(arg)

    # String function implementations
    def _func_len(self, args: List[str]) -> str:
        """LEN(string) - Return length of string."""
        if len(args) < 1:
            raise ValueError("LEN requires 1 argument")

        string_val = self._evaluate_arg(args[0])
        if isinstance(string_val, (int, float)):
            string_val = str(int(string_val))

        return str(len(string_val))

    def _func_left(self, args: List[str]) -> str:
        """LEFT(string, length) - Return leftmost characters."""
        if len(args) < 2:
            raise ValueError("LEFT requires 2 arguments")

        string_val = self._evaluate_arg(args[0])
        if isinstance(string_val, (int, float)):
            string_val = str(int(string_val))

        try:
            length = int(self._evaluate_arg(args[1]))
        except (ValueError, TypeError):
            length = 0

        return string_val[: max(0, length)]

    def _func_right(self, args: List[str]) -> str:
        """RIGHT(string, length) - Return rightmost characters."""
        if len(args) < 2:
            raise ValueError("RIGHT requires 2 arguments")

        string_val = self._evaluate_arg(args[0])
        if isinstance(string_val, (int, float)):
            string_val = str(int(string_val))

        try:
            length = int(self._evaluate_arg(args[1]))
        except (ValueError, TypeError):
            length = 0

        if length == 0:
            return ""
        return string_val[-length:] if length > 0 else ""

    def _func_mid(self, args: List[str]) -> str:
        """MID(string, start, length) - Return substring."""
        if len(args) < 2:
            raise ValueError("MID requires at least 2 arguments")

        string_val = self._evaluate_arg(args[0])
        if isinstance(string_val, (int, float)):
            string_val = str(int(string_val))

        try:
            start = int(self._evaluate_arg(args[1]))
        except (ValueError, TypeError):
            start = 1

        try:
            length = (
                int(self._evaluate_arg(args[2])) if len(args) >= 3 else len(string_val)
            )
        except (ValueError, TypeError):
            length = len(string_val)

        # BASIC uses 1-based indexing
        start = max(1, start)
        length = max(0, length)

        return string_val[start - 1:start - 1 + length]

    def _func_instr(self, args: List[str]) -> str:
        """INSTR(string, search) - Find position of substring."""
        if len(args) < 2:
            raise ValueError("INSTR requires 2 arguments")

        string_val = self._evaluate_arg(args[0])
        if isinstance(string_val, (int, float)):
            string_val = str(int(string_val))

        search_val = self._evaluate_arg(args[1])
        if isinstance(search_val, (int, float)):
            search_val = str(int(search_val))

        if not search_val:
            return "1"  # Empty string found at position 1

        pos = string_val.find(search_val)
        return str(pos + 1) if pos >= 0 else "0"  # BASIC uses 1-based indexing

    def _func_upper(self, args: List[str]) -> str:
        """UPPER(string) - Convert to uppercase."""
        if len(args) < 1:
            raise ValueError("UPPER requires 1 argument")

        string_val = self._evaluate_arg(args[0])
        if isinstance(string_val, (int, float)):
            string_val = str(int(string_val))

        return string_val.upper()

    def _func_lower(self, args: List[str]) -> str:
        """LOWER(string) - Convert to lowercase."""
        if len(args) < 1:
            raise ValueError("LOWER requires 1 argument")

        string_val = self._evaluate_arg(args[0])
        if isinstance(string_val, (int, float)):
            string_val = str(int(string_val))

        return string_val.lower()

    def _func_trim(self, args: List[str]) -> str:
        """TRIM(string) - Remove leading/trailing spaces."""
        if len(args) < 1:
            raise ValueError("TRIM requires 1 argument")

        string_val = self._evaluate_arg(args[0])
        if isinstance(string_val, (int, float)):
            string_val = str(int(string_val))

        return string_val.strip()

    def _func_str(self, args: List[str]) -> str:
        """STR(number) - Convert number to string."""
        if len(args) < 1:
            raise ValueError("STR requires 1 argument")

        # Evaluate as numeric
        val = self._evaluate_arg(args[0])
        try:
            num = float(val)
            # Format as integer if whole number, otherwise as float
            if num == int(num):
                return str(int(num))
            return str(num)
        except (ValueError, TypeError):
            return str(val)

    def _func_val(self, args: List[str]) -> str:
        """VAL(string) - Convert string to number (return as string for compatibility)."""
        if len(args) < 1:
            raise ValueError("VAL requires 1 argument")

        string_val = self._evaluate_arg(args[0])
        if isinstance(string_val, (int, float)):
            return str(string_val)

        # Try to parse as float
        string_str = str(string_val).strip()

        # Extract leading numeric part
        match = re.match(r"^[+-]?(\d+\.?\d*|\.\d+)", string_str)
        if match:
            try:
                num = float(match.group())
                if num == int(num):
                    return str(int(num))
                return str(num)
            except (ValueError, TypeError):
                return "0"

        return "0"

    def _evaluate_concatenation(self, expr: str, op: str) -> str:
        """Evaluate string concatenation with & or + operator."""
        parts = expr.split(op)
        result_parts = []

        for part in parts:
            val = self._evaluate_expr(part.strip())
            result_parts.append(str(val))

        return "".join(result_parts)
