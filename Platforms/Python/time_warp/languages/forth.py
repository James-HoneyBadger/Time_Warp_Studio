from typing import TYPE_CHECKING, Callable, Dict, List, Optional

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


class ForthExecutor:
    def __init__(self, interpreter: "Interpreter"):
        self.interpreter = interpreter
        self.stack: List[int] = []
        self.return_stack: List[int] = []
        self.dictionary: Dict[str, Callable] = {}
        self.memory: List[int] = []  # Linear memory for variables
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

        # Memory
        self.dictionary["@"] = self._fetch
        self.dictionary["!"] = self._store
        self.dictionary["VARIABLE"] = self._variable

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

    # --- Memory ---
    def _fetch(self):
        if self.stack:
            addr = self.stack.pop()
            if 0 <= addr < len(self.memory):
                self.stack.append(self.memory[addr])
            else:
                self.interpreter.log_output(f"❌ Invalid memory address: {addr}")
                self.stack.append(0)

    def _store(self):
        if len(self.stack) >= 2:
            addr = self.stack.pop()
            val = self.stack.pop()
            if 0 <= addr < len(self.memory):
                self.memory[addr] = val
            else:
                self.interpreter.log_output(f"❌ Invalid memory address: {addr}")

    def _variable(self):
        # This is tricky because VARIABLE reads the NEXT token.
        # But _variable is called by execute_tokens which is iterating.
        # We need access to the token stream.
        # For now, we'll handle VARIABLE in execute_tokens directly.
        pass

    # --- Graphics ---
    def _fd(self):
        if self.stack and self.turtle:
            self.turtle.forward(self.stack.pop())

    def _bk(self):
        if self.stack and self.turtle:
            self.turtle.back(self.stack.pop())

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
            color_idx = int(self.stack.pop())
            # Map color index to color name
            color_map = {
                0: "BLACK", 1: "RED", 2: "GREEN", 3: "BLUE",
                4: "YELLOW", 5: "CYAN", 6: "MAGENTA", 7: "WHITE",
                8: "ORANGE", 9: "PURPLE", 10: "BROWN", 11: "PINK",
                12: "GRAY", 13: "LIME", 14: "NAVY", 15: "TEAL",
            }
            color_name = color_map.get(color_idx, "WHITE")
            self.turtle.pencolor(color_name)

    def execute_tokens(self, tokens: List[str]):
        i = 0
        while i < len(tokens):
            token = tokens[i]
            token_upper = token.upper()

            if self.compiling:
                if token == ";":
                    self.compiling = False
                    # Define the new word
                    definition = list(self.new_word_definition)

                    def new_word_func(d=definition):
                        self.execute_tokens(d)

                    self.dictionary[self.new_word_name] = new_word_func
                    self.interpreter.log_output(f"Defined {self.new_word_name}")
                else:
                    self.new_word_definition.append(token)
                i += 1
                continue

            # Control Structures
            if token_upper == "IF":
                if not self.stack:
                    self.interpreter.log_output("❌ Stack underflow for IF")
                    i += 1
                    continue
                cond = self.stack.pop()
                if cond == 0:
                    # Skip to ELSE or THEN
                    depth = 1
                    while i + 1 < len(tokens):
                        i += 1
                        t = tokens[i].upper()
                        if t == "IF":
                            depth += 1
                        if t == "THEN":
                            depth -= 1
                        if t == "ELSE" and depth == 1:
                            # Found ELSE at same level, stop skipping and
                            # execute ELSE block
                            break
                        if depth == 0:
                            # Found THEN, stop skipping
                            break
                # If cond != 0, just continue executing
                i += 1
                continue

            if token_upper == "ELSE":
                # We hit ELSE after executing the True branch. Skip to THEN.
                depth = 1
                while i + 1 < len(tokens):
                    i += 1
                    t = tokens[i].upper()
                    if t == "IF":
                        depth += 1
                    if t == "THEN":
                        depth -= 1
                    if depth == 0:
                        break
                i += 1
                continue

            if token_upper == "THEN":
                # Just a marker
                i += 1
                continue

            if token_upper == "DO":
                # DO ( limit start -- )
                # We need to loop back to here.
                # This is complex in a linear scan.
                # Standard Forth: limit start DO ... LOOP
                # We need to save the loop parameters and the index 'i'.
                if len(self.stack) < 2:
                    self.interpreter.log_output("❌ Stack underflow for DO")
                    i += 1
                    continue
                start = self.stack.pop()
                limit = self.stack.pop()
                self.return_stack.append(limit)
                self.return_stack.append(start)
                self.return_stack.append(i)  # Save loop start index
                i += 1
                continue

            if token_upper == "LOOP":
                if len(self.return_stack) < 3:
                    self.interpreter.log_output("❌ Return stack underflow for LOOP")
                    i += 1
                    continue

                loop_start_idx = self.return_stack.pop()
                index = self.return_stack.pop()
                limit = self.return_stack.pop()

                index += 1
                if index < limit:
                    # Loop again
                    self.return_stack.append(limit)
                    self.return_stack.append(index)
                    self.return_stack.append(loop_start_idx)
                    i = loop_start_idx + 1  # Jump back
                else:
                    # Loop finished
                    pass
                continue

            if token_upper == "I":
                if len(self.return_stack) >= 2:
                    # Index is second on return stack
                    # (top is loop_start_idx if kept there? No, we popped it)
                    # In DO we pushed limit, start, idx.
                    # In LOOP we popped them.
                    # So inside the loop, they are on the return stack.
                    # Top is loop_start_idx. Next is index.
                    self.stack.append(self.return_stack[-2])
                else:
                    self.interpreter.log_output("❌ I used outside loop")
                i += 1
                continue

            if token_upper == "VARIABLE":
                if i + 1 < len(tokens):
                    var_name = tokens[i + 1].upper()
                    addr = len(self.memory)
                    self.memory.append(0)

                    # Define word that pushes address
                    def var_func(a=addr):
                        self.stack.append(a)

                    self.dictionary[var_name] = var_func
                    self.interpreter.log_output(
                        f"Variable {var_name} allocated at {addr}"
                    )
                    i += 2  # Skip VARIABLE and Name
                    continue
                else:
                    self.interpreter.log_output("❌ VARIABLE requires name")
                    i += 1
                    continue

            if token_upper == ":":
                self.compiling = True
                if i + 1 < len(tokens):
                    self.new_word_name = tokens[i + 1].upper()
                    self.new_word_definition = []
                    i += 2  # Skip : and Name
                else:
                    self.interpreter.log_output("❌ : requires name")
                    i += 1
                continue

            # Check dictionary
            if token_upper in self.dictionary:
                self.dictionary[token_upper]()
                i += 1
                continue

            # Check number
            try:
                val = int(token)
                self.stack.append(val)
                i += 1
                continue
            except ValueError:
                pass

            # Check string literal (." Hello ")
            if token.startswith('."') and token.endswith('"'):
                self.output_buffer += token[2:-1]
                i += 1
                continue

            self.interpreter.log_output(f"❌ Unknown word: {token}")
            i += 1

    def execute_line(self, line: str, turtle=None):
        self.turtle = turtle

        # Tokenizer
        tokens = []
        parts = line.split()
        j = 0
        while j < len(parts):
            t = parts[j]
            if t.upper() == '."':
                # Start of string
                s = '."'
                j += 1
                while j < len(parts):
                    if parts[j].endswith('"'):
                        s += parts[j]
                        break
                    s += parts[j] + " "
                    j += 1
                tokens.append(s)
            elif t == "(":
                # Comment ( ... )
                j += 1
                while j < len(parts):
                    if parts[j] == ")":
                        break
                    j += 1
            elif t == "\\":
                # Comment \ ...
                break  # Ignore rest of line
            else:
                tokens.append(t)
            j += 1

        self.execute_tokens(tokens)

        # Flush buffer at end of line
        if self.output_buffer:
            self.interpreter.log_output(self.output_buffer)
            self.output_buffer = ""


# Global instance for persistence across lines
_forth_executor = None


def reset_forth():
    """Reset the global Forth executor so state doesn't leak between runs."""
    global _forth_executor  # pylint: disable=global-statement
    _forth_executor = None


def execute_forth(interpreter: "Interpreter", command: str, _turtle=None) -> str:
    global _forth_executor  # pylint: disable=global-statement
    if _forth_executor is None or _forth_executor.interpreter != interpreter:
        _forth_executor = ForthExecutor(interpreter)

    _forth_executor.execute_line(command, _turtle)
    return ""
