#!/usr/bin/env python3
"""Fix E501 long lines in app.py by wrapping them properly."""

file_path = "/home/james/Temple_Code/Python/time_warp/app.py"

with open(file_path, "r", encoding="utf-8") as f:
    lines = f.readlines()

output = []
i = 0
while i < len(lines):
    line = lines[i]

    # Fix line 829: show_line_numbers
    if (
        'self.settings["show_line_numbers"] = bool(self.show_line_numbers_var.get())'
        in line
    ):
        indent = len(line) - len(line.lstrip())
        output.append(" " * indent + 'self.settings["show_line_numbers"] = bool(\n')
        output.append(" " * (indent + 4) + "self.show_line_numbers_var.get()\n")
        output.append(" " * indent + ")\n")
        i += 1
        continue

    # Fix MySQL status configure lines
    if (
        'self.status_label.configure(text="MySQL connection' in line
        and line.strip().endswith('")')
    ):
        indent = len(line) - len(line.lstrip())
        if '"MySQL connection OK"' in line:
            text = '"MySQL connection OK"'
        else:
            text = '"MySQL connection failed"'
        output.append(" " * indent + "self.status_label.configure(\n")
        output.append(" " * (indent + 4) + "text=" + text + "\n")
        output.append(" " * indent + ")\n")
        i += 1
        continue

    output.append(line)
    i += 1

with open(file_path, "w", encoding="utf-8") as f:
    f.writelines(output)

print(f"Fixed E501 lines in {file_path}")
