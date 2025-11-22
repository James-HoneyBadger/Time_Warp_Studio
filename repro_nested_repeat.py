from time_warp.core.interpreter import Interpreter
from time_warp.languages.logo import execute_logo
from time_warp.graphics.turtle_state import TurtleState


def test_nested_repeat():
    interpreter = Interpreter()
    turtle = TurtleState()

    # Simple nested repeat
    # REPEAT 4 [ FD 100 REPEAT 4 [ FD 10 RT 90 ] RT 90 ]
    code = "REPEAT 4 [ FD 100 REPEAT 4 [ FD 10 RT 90 ] RT 90 ]"

    print(f"Executing: {code}")
    result = execute_logo(interpreter, code, turtle)
    print(f"Result: {result}")

    if "malformed" in result or "Error" in result:
        print("FAIL: Nested REPEAT failed")
    else:
        print("SUCCESS: Nested REPEAT executed")


if __name__ == "__main__":
    test_nested_repeat()
