from time_warp.core.interpreter import Interpreter
from time_warp.graphics.turtle_state import TurtleState

code = """
TO POLYGON :SIDES :SIZE
  REPEAT :SIDES [
    FORWARD :SIZE
    RIGHT 360 / :SIDES
  ]
END

TO FLOWER :PETALS
  REPEAT :PETALS [
    SETCOLOR #FF69B4
    POLYGON 4 50
    RIGHT 360 / :PETALS
  ]
END

FLOWER 8
"""

interp = Interpreter()
T = TurtleState()
interp.load_program(code)
out = interp.execute(T)
print("output lines:", len(out))
print("turtle segments:", len(T.lines))
# Dump first 3 segments for sanity
for seg in T.lines[:3]:
    print((seg.start_x, seg.start_y, seg.end_x, seg.end_y))
