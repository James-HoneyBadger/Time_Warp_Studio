"""
Safe mathematical expression evaluator for Time Warp IDE
Ported from Rust: time_warp_unified::utils::expr_eval.rs

Provides secure evaluation without eval() or exec()
"""

import math
import random
from typing import Dict, List, Optional
from enum import Enum, auto


class Token:
    """Expression token"""

    class Type(Enum):
        NUMBER = auto()
        VARIABLE = auto()
        FUNCTION = auto()
        OPERATOR = auto()
        COMPARISON = auto()
        LEFT_PAREN = auto()
        RIGHT_PAREN = auto()
        COMMA = auto()

    def __init__(self, token_type, value=None):
        self.type = token_type
        self.value = value


class ExpressionEvaluator:
    """
    Safe expression evaluator supporting math expressions, variables, and functions

    Features:
    - Operators: +, -, *, /, %, ^ (power)
    - Functions: sin, cos, tan, sqrt, abs, log, exp, min, max, pow, rand
    - Variables: Dynamic via constructor or set_variable()
    - Parentheses: Arbitrary nesting

    Security:
    - No eval() or code execution - pure arithmetic only
    - Complexity limits: MAX_TOKENS=1000

    Example:
        >>> evaluator = ExpressionEvaluator({"X": 10})
        >>> evaluator.evaluate("2 * X + 5")
        25.0
    """

    MAX_TOKENS = 1000

    FUNCTIONS = {
        "SIN": math.sin,
        "COS": math.cos,
        "TAN": math.tan,
        "ASIN": math.asin,
        "ACOS": math.acos,
        "ATAN": math.atan,
        "SINH": math.sinh,
        "COSH": math.cosh,
        "TANH": math.tanh,
        "SQRT": math.sqrt,
        "ABS": abs,
        "FLOOR": math.floor,
        "CEIL": math.ceil,
        "ROUND": round,
        "EXP": math.exp,
        "LOG": math.log,
        "LOG10": math.log10,
        "INT": int,
        "RAND": lambda: random.random(),
    }

    def __init__(self, variables: Optional[Dict[str, float]] = None):
        self.variables = variables or {}
        self.token_cache: Dict[str, List[Token]] = {}

    def set_variable(self, name: str, value: float):
        """Set or update a variable value"""
        self.variables[name] = value

    def evaluate(self, expr: str) -> float:
        """
        Evaluate mathematical expression

        Args:
            expr: Expression like "2 + 3 * X" or "sin(PI / 2)"

        Returns:
            Computed result

        Raises:
            ValueError: On invalid expression or syntax error
        """
        # Check cache first
        if expr in self.token_cache:
            tokens = self.token_cache[expr]
        else:
            tokens = self._tokenize(expr)
            self.token_cache[expr] = tokens

        rpn = self._to_rpn(tokens)
        return self._evaluate_rpn(rpn)

    def _tokenize(self, expr: str) -> List[Token]:
        """Convert expression string to tokens"""
        tokens = []
        i = 0

        while i < len(expr):
            if len(tokens) >= self.MAX_TOKENS:
                raise ValueError(
                    f"Expression too complex (max {self.MAX_TOKENS} tokens)"
                )

            ch = expr[i]

            # Skip whitespace
            if ch in " \t\n":
                i += 1
                continue

            # Numbers
            if ch.isdigit() or ch == ".":
                num_str = ""
                while i < len(expr) and (expr[i].isdigit() or expr[i] == "."):
                    num_str += expr[i]
                    i += 1
                tokens.append(Token(Token.Type.NUMBER, float(num_str)))
                continue

            # Variables and functions
            if ch.isalpha() or ch == "_":
                name = ""
                while i < len(expr) and (expr[i].isalnum() or expr[i] == "_"):
                    name += expr[i]
                    i += 1

                name_upper = name.upper()

                # Check if it's a function (followed by '(')
                if i < len(expr) and expr[i] == "(":
                    tokens.append(Token(Token.Type.FUNCTION, name_upper))
                else:
                    tokens.append(Token(Token.Type.VARIABLE, name_upper))
                continue

            # Operators
            if ch in "+-*/%^":
                tokens.append(Token(Token.Type.OPERATOR, ch))
                i += 1
                continue

            # Comparisons
            if ch in "<>=!":
                comp = ch
                i += 1
                if i < len(expr) and expr[i] in ">=":
                    comp += expr[i]
                    i += 1
                # Convert <> to !=
                if comp == "<>":
                    comp = "!="
                # Convert single = to ==
                elif comp == "=":
                    comp = "=="
                tokens.append(Token(Token.Type.COMPARISON, comp))
                continue

            # Parentheses
            if ch == "(":
                tokens.append(Token(Token.Type.LEFT_PAREN))
                i += 1
                continue

            if ch == ")":
                tokens.append(Token(Token.Type.RIGHT_PAREN))
                i += 1
                continue

            # Comma
            if ch == ",":
                tokens.append(Token(Token.Type.COMMA))
                i += 1
                continue

            raise ValueError(f"Unexpected character: {ch}")

        return tokens

    def _to_rpn(self, tokens: List[Token]) -> List[Token]:
        """Convert infix tokens to Reverse Polish Notation using Shunting Yard"""
        output = []
        operator_stack = []

        precedence = {
            "+": 1,
            "-": 1,
            "*": 2,
            "/": 2,
            "%": 2,
            "^": 3,
            "<": 0,
            ">": 0,
            "<=": 0,
            ">=": 0,
            "==": 0,
            "!=": 0,
        }

        right_associative = {"^"}

        for token in tokens:
            if token.type == Token.Type.NUMBER:
                output.append(token)

            elif token.type == Token.Type.VARIABLE:
                output.append(token)

            elif token.type == Token.Type.FUNCTION:
                operator_stack.append(token)

            elif (
                token.type == Token.Type.OPERATOR or token.type == Token.Type.COMPARISON
            ):
                op = token.value
                while operator_stack:
                    top = operator_stack[-1]
                    if top.type == Token.Type.LEFT_PAREN:
                        break

                    if top.type in (Token.Type.OPERATOR, Token.Type.COMPARISON):
                        top_op = top.value
                        if (precedence[top_op] > precedence[op]) or (
                            precedence[top_op] == precedence[op]
                            and op not in right_associative
                        ):
                            output.append(operator_stack.pop())
                        else:
                            break
                    elif top.type == Token.Type.FUNCTION:
                        output.append(operator_stack.pop())
                    else:
                        break

                operator_stack.append(token)

            elif token.type == Token.Type.LEFT_PAREN:
                operator_stack.append(token)

            elif token.type == Token.Type.RIGHT_PAREN:
                while (
                    operator_stack and operator_stack[-1].type != Token.Type.LEFT_PAREN
                ):
                    output.append(operator_stack.pop())

                if not operator_stack:
                    raise ValueError("Mismatched parentheses")

                operator_stack.pop()  # Remove left paren

                # If there's a function on stack, pop it to output
                if operator_stack and operator_stack[-1].type == Token.Type.FUNCTION:
                    output.append(operator_stack.pop())

            elif token.type == Token.Type.COMMA:
                while (
                    operator_stack and operator_stack[-1].type != Token.Type.LEFT_PAREN
                ):
                    output.append(operator_stack.pop())

        while operator_stack:
            if operator_stack[-1].type == Token.Type.LEFT_PAREN:
                raise ValueError("Mismatched parentheses")
            output.append(operator_stack.pop())

        return output

    def _evaluate_rpn(self, rpn: List[Token]) -> float:
        """Evaluate RPN expression"""
        stack = []

        for token in rpn:
            if token.type == Token.Type.NUMBER:
                stack.append(token.value)

            elif token.type == Token.Type.VARIABLE:
                var_name = token.value
                if var_name not in self.variables:
                    raise ValueError(f"Undefined variable: {var_name}")
                stack.append(self.variables[var_name])

            elif (
                token.type == Token.Type.OPERATOR or token.type == Token.Type.COMPARISON
            ):
                if len(stack) < 2:
                    raise ValueError("Invalid expression")

                b = stack.pop()
                a = stack.pop()

                op = token.value
                if op == "+":
                    result = a + b
                elif op == "-":
                    result = a - b
                elif op == "*":
                    result = a * b
                elif op == "/":
                    if b == 0:
                        raise ValueError("Division by zero")
                    result = a / b
                elif op == "%":
                    result = a % b
                elif op == "^":
                    result = a**b
                elif op == "<":
                    result = 1.0 if a < b else 0.0
                elif op == ">":
                    result = 1.0 if a > b else 0.0
                elif op == "<=":
                    result = 1.0 if a <= b else 0.0
                elif op == ">=":
                    result = 1.0 if a >= b else 0.0
                elif op == "==":
                    result = 1.0 if abs(a - b) < 1e-10 else 0.0
                elif op == "!=":
                    result = 1.0 if abs(a - b) >= 1e-10 else 0.0
                else:
                    raise ValueError(f"Unknown operator: {op}")

                stack.append(result)

            elif token.type == Token.Type.FUNCTION:
                func_name = token.value
                if func_name not in self.FUNCTIONS:
                    raise ValueError(f"Unknown function: {func_name}")

                func = self.FUNCTIONS[func_name]

                # Handle functions with special arities
                if func_name in ("MIN", "MAX", "POW"):
                    if len(stack) < 2:
                        raise ValueError(f"{func_name} requires 2 arguments")
                    b = stack.pop()
                    a = stack.pop()
                    if func_name == "MIN":
                        result = min(a, b)
                    elif func_name == "MAX":
                        result = max(a, b)
                    else:  # POW
                        result = a**b
                    stack.append(result)
                elif func_name == "RAND":
                    stack.append(func())
                else:
                    if len(stack) < 1:
                        raise ValueError(f"{func_name} requires an argument")
                    arg = stack.pop()
                    result = func(arg)
                    stack.append(result)

        if len(stack) != 1:
            raise ValueError("Invalid expression")

        return stack[0]
