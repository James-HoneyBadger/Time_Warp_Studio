#!/usr/bin/env python3
"""Fix indentation in interpreter.py"""

with open("time_warp/core/interpreter.py", "r", encoding="utf-8") as f:
    lines = f.readlines()

# Fix lines 360-367 (0-indexed: 359-366)
# They currently have 12 spaces, should have 8
for i in range(359, 367):
    if (
        lines[i].startswith("            # BASIC")
        or lines[i].startswith("            if '='")
        or lines[i].startswith("                #")
        or lines[i].startswith("                parts")
        or lines[i].startswith("                if len")
        or lines[i].startswith("                    if not")
        or lines[i].startswith("                        #")
        or lines[i].startswith("                        return")
    ):
        # Remove 4 spaces
        lines[i] = lines[i][4:]

with open("time_warp/core/interpreter.py", "w", encoding="utf-8") as f:
    f.writelines(lines)

print("Fixed indentation")
