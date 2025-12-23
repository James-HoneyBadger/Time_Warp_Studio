
from typing import List, Dict, Callable, TYPE_CHECKING, Optional

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


class ForthExecutor:
    def __init__(self, interpreter: "Interpreter"):
        self.interpreter = interpreter
        self.stack: List[int] = []
        self.return_stack: List[int] = []
        self.dictionary: Dict[str, Callable] = {}
        self.compiling = False
        self.new_word_name = ""
        self.new_word_definition: List[str] = []
        self.output_buffer = ""
        self.turtle: Optional["TurtleState"] = None

        # Initialize standard dictionary
        self._init_dictionary()

    def _init_dictionary(self):
        # Stack manipulation
        self.dictionary["DUP"] = self._dup
        self.dictionary["DROP"] = self._drop
        self.dictionary["SWAP"] = self._swap
        self.dictionary["OVER"] = self._over
        self.dictionary["ROT"] = self._rot
        self.dictionary["."] = self._dot
        self.dictionary[".S"] = self._dot_s
        self.dictionary["CR"] = self._cr
        
        # Arithmetic
        self.dictionary["+"] = self._add
        self.dictionary["-"] = self._sub
        self.dictionary["*"] = self._mul
        self.dictionary["/"] = self._div
        self.dictionary["MOD"] = self._mod
        
        # Logic
        self.dictionary["="] = self._eq
        self.dictionary["<"] = self._lt
        self.dictionary[">"] = self._gt
        self.dictionary["AND"] = self._and
        self.dictionary["OR"] = self._or
        self.dictionary["INVERT"] = self._invert
        
        # Graphics (Time Warp extensions)
        self.dictionary["FD"] = self._fd
        self.dictionary["BK"] = self._bk
        self.dictionary["RT"] = self._rt
        self.dictionary["LT"] = self._lt_turn
        self.dictionary["PU"] = self._pu
        self.dictionary["PD"] = self._pd
        self.dictionary["HOME"] = self._home
        self.dictionary["CLEAN"] = self._clean
        self.dictionary["PEN"] = self._pen

    # --- Primitives ---
    def _dup(self):
        if self.stack:
            self.stack.append(self.stack[-1])

    def _drop(self):
        if self.stack:
            self.stack.pop()

    def _swap(self):
        if len(self.stack) >= 2:
            a = self.stack.pop()
            b = self.stack.pop()
            self.stack.append(a)
            self.stack.append(b)

    def _over(self):
        if len(self.stack) >= 2:
            self.stack.append(self.stack[-2])

    def _rot(self):
        if len(self.stack) >= 3:
            c = self.stack.pop()
            b = self.stack.pop()
            a = self.stack.pop()
            self.stack.append(b)
            self.stack.append(c)
            self.stack.append(a)
            
    def _dot(self):
        if self.stack:
            val = self.stack.pop()
            self.output_buffer += f"{val} "
            
    def _dot_s(self):
        self.output_buffer += (
            f"<{len(self.stack)}> " + " ".join(map(str, self.stack)) + " "
        )
        
    def _cr(self):
        self.interpreter.log_output(self.output_buffer)
        self.output_buffer = ""

    def _add(self):
        if len(self.stack) >= 2:
            self.stack.append(self.stack.pop() + self.stack.pop())

    def _sub(self):
        if len(self.stack) >= 2:
            b = self.stack.pop()
            a = self.stack.pop()
            self.stack.append(a - b)

    def _mul(self):
        if len(self.stack) >= 2:
            self.stack.append(self.stack.pop() * self.stack.pop())

    def _div(self):
        if len(self.stack) >= 2:
            b = self.stack.pop()
            a = self.stack.pop()
            if b == 0:
                self.interpreter.log_output("❌ Division by zero")
                self.stack.append(0)
            else:
                self.stack.append(int(a / b))

    def _mod(self):
        if len(self.stack) >= 2:
            b = self.stack.pop()
            a = self.stack.pop()
            if b == 0:
                self.interpreter.log_output("❌ Division by zero")
                self.stack.append(0)
            else:
                self.stack.append(a % b)

    def _eq(self):
        if len(self.stack) >= 2:
            self.stack.append(-1 if self.stack.pop() == self.stack.pop() else 0)

    def _lt(self):
        if len(self.stack) >= 2:
            b = self.stack.pop()
            a = self.stack.pop()
            self.stack.append(-1 if a < b else 0)

    def _gt(self):
        if len(self.stack) >= 2:
            b = self.stack.pop()
            a = self.stack.pop()
            self.stack.append(-1 if a > b else 0)

    def _and(self):
        if len(self.stack) >= 2:
            self.stack.append(self.stack.pop() & self.stack.pop())

    def _or(self):
        if len(self.stack) >= 2:
            self.stack.append(self.stack.pop() | self.stack.pop())

    def _invert(self):
        if self.stack:
            self.stack.append(~self.stack.pop())

    # --- Graphics ---
    def _fd(self):
        if self.stack and self.turtle:
            self.turtle.forward(self.stack.pop())

    def _bk(self):
        if self.stack and self.turtle:
            self.turtle.backward(self.stack.pop())

    def _rt(self):
        if self.stack and self.turtle:
            self.turtle.right(self.stack.pop())

    def _lt_turn(self):
        if self.stack and self.turtle:
            self.turtle.left(self.stack.pop())

    def _pu(self):
        if self.turtle:
            self.turtle.penup()

    def _pd(self):
        if self.turtle:
            self.turtle.pendown()

    def _home(self):
        if self.turtle:
            self.turtle.home()

    def _clean(self):
        if self.turtle:
            self.turtle.clear()

    def _pen(self):
        if self.stack and self.turtle:
            color_idx = self.stack.pop()
            # Map index to color string if needed, or pass index
            self.turtle.pencolor(color_idx)

    def execute_token(self, token: str):
        token = token.upper()
        
        if self.compiling:
            if token == ";":
                self.compiling = False
                # Define the new word
                definition = list(self.new_word_definition)  # Copy

                def new_word_func():
                    for t in definition:
                        self.execute_token(t)

                self.dictionary[self.new_word_name] = new_word_func
                self.interpreter.log_output(f"Defined {self.new_word_name}")
            else:
                self.new_word_definition.append(token)
            return

        if token == ":":
            self.compiling = True
            self.new_word_name = ""  # Will be set by next token logic in execute_line
            self.new_word_definition = []
            return

        # Check dictionary
        if token in self.dictionary:
            self.dictionary[token]()
            return
            
        # Check number
        try:
            val = int(token)
            self.stack.append(val)
            return
        except ValueError:
            pass
            
        # Check string literal (." Hello ") - simplified
        if token.startswith('."') and token.endswith('"'):
            self.output_buffer += token[2:-1]
            return

        self.interpreter.log_output(f"❌ Unknown word: {token}")

    def execute_line(self, line: str, turtle=None):
        self.turtle = turtle
        # Handle string literals with spaces: ." Hello World "
        # This is a quick hack; a real parser would be better
        parts = line.split()
        i = 0
        while i < len(parts):
            t = parts[i]
            if t.upper() == '."':
                # Start of string
                s = ""
                i += 1
                while i < len(parts):
                    if parts[i].endswith('"'):
                        s += parts[i][:-1]
                        break
                    s += parts[i] + " "
                    i += 1
                self.output_buffer += s
            elif t == ":" and i + 1 < len(parts):
                # Start definition
                self.compiling = True
                self.new_word_name = parts[i + 1].upper()
                self.new_word_definition = []
                i += 1  # Skip name
            elif self.compiling:
                if t == ";":
                    self.compiling = False
                    # Define
                    definition = list(self.new_word_definition)

                    # pylint: disable=dangerous-default-value
                    def new_word_func(d=definition):
                        for tok in d:
                            self.execute_token(tok)

                    self.dictionary[self.new_word_name] = new_word_func
                    self.interpreter.log_output(f"Defined {self.new_word_name}")
                else:
                    self.new_word_definition.append(t)
            else:
                self.execute_token(t)
            i += 1

        # Flush buffer at end of line
        if self.output_buffer:
            self.interpreter.log_output(self.output_buffer)
            self.output_buffer = ""


# Global instance for persistence across lines
_forth_executor = None


def execute_forth(interpreter: "Interpreter", command: str, _turtle=None) -> str:
    global _forth_executor  # pylint: disable=global-statement
    if _forth_executor is None or _forth_executor.interpreter != interpreter:
        _forth_executor = ForthExecutor(interpreter)

    _forth_executor.execute_line(command, _turtle)
    return ""
