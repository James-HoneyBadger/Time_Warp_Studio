from Platforms.Python.time_warp.core.interpreter import Interpreter, Language

interp = Interpreter()
with open("Examples/logo/03_polygons.logo", "r", encoding="utf-8") as f:
    code = f.read()

print(f"Code length: {len(code)}")
interp.load_program(code, Language.LOGO)

print(f"Program lines loaded: {len(interp.program_lines)}")
for i, (line_num, cmd) in enumerate(interp.program_lines):
    print(f"  Line {i}: {cmd}")

print(f"Procedures defined: {list(interp.logo_procedures.keys())}")
