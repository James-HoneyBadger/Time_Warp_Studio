"""
Code Formatter Plugin for Time Warp IDE
Provides code formatting and beautification features
"""

import re

# Plugin state
_plugin_state = {"context": None, "auto_format_enabled": False}


def initialize(context):
    """Initialize the code formatter plugin"""
    print("🎨 Code Formatter Plugin initialized")

    # Store context for later use
    _plugin_state["context"] = context

    # Add formatter menu items
    context.add_menu_item(
        "Tools/Format Code", "Format Current File", format_current_file
    )
    context.add_menu_item(
        "Tools/Format Code",
        "Format Selection",
        format_selection,
    )
    context.add_menu_item(
        "Tools/Format Code", "Auto-Format on Save", toggle_auto_format
    )

    # Add toolbar button
    context.add_toolbar_button("🎨", "Format Code", format_current_file)

    return True


def cleanup(context):
    """Clean up code formatter plugin resources"""
    print("🎨 Code Formatter Plugin cleaned up")
    _ = context
    _plugin_state["context"] = None


def format_current_file():
    """Format the currently open file"""
    print("🎨 Formatting current file...")

    # Get current file content (would need IDE integration)
    # For demonstration, show what would happen
    demonstrate_formatting("current_file")


def format_selection():
    """Format the selected text"""
    print("🎨 Formatting selected text...")

    # Get selected text (would need IDE integration)
    # For demonstration, show what would happen
    demonstrate_formatting("selection")


def toggle_auto_format():
    """Toggle auto-formatting on save"""
    old_state = _plugin_state["auto_format_enabled"]
    _plugin_state["auto_format_enabled"] = not old_state

    status = "enabled" if _plugin_state["auto_format_enabled"] else "disabled"
    print(f"🎨 Auto-format on save {status}")


def demonstrate_formatting(target):
    """Demonstrate code formatting with sample code"""
    print(f"🎨 Demonstrating formatting for {target}...")

    # Sample code for different languages
    samples = {
        "basic": """print"Hello World"
for i=1 to 10
print i
next i
if x=5 then print"yes" else print"no"
""",
        "logo": (
            "forward 100 right 90 forward 100 right 90 "
            "forward 100 right 90 forward 100 right 90\n"
            "repeat 4[forward 100 right 90]\n"
        ),
        "pilot": """T:Hello World
A:What is your name?
T:Hello, #A1#
""",
        "pascal": """program hello;begin writeln('Hello World');end.
""",
    }

    for language, code in samples.items():
        print(f"\n📝 {language.upper()} - Before formatting:")
        print(code)
        print(f"📝 {language.upper()} - After formatting:")
        formatted = format_code(code, language)
        print(formatted)
        print("-" * 40)


def format_code(code, language):
    """Format code for the specified language"""
    if language.lower() == "basic":
        return format_basic(code)
    elif language.lower() == "logo":
        return format_logo(code)
    elif language.lower() == "pilot":
        return format_pilot(code)
    elif language.lower() == "pascal":
        return format_pascal(code)
    else:
        return code  # No formatting available


def format_basic(code):
    """Format BASIC code"""
    lines = code.strip().split("\n")
    formatted_lines = []

    for line in lines:
        line = line.strip()
        if not line:
            continue

        # Add proper spacing around operators
        line = re.sub(r"(\w)([=\+\-\*/<>])", r"\1 \2", line)
        line = re.sub(r"([=\+\-\*/<>])(\w)", r"\1 \2", line)

        # Capitalize keywords
        keywords = [
            "PRINT",
            "FOR",
            "TO",
            "NEXT",
            "IF",
            "THEN",
            "ELSE",
            "END",
            "GOTO",
            "GOSUB",
            "RETURN",
        ]
        for keyword in keywords:
            pattern = rf"\b{re.escape(keyword.lower())}\b"
            line = re.sub(pattern, keyword, line, flags=re.IGNORECASE)

        # Fix spacing in FOR loops
        line = re.sub(r"FOR\s+(\w+)\s*=\s*", r"FOR \1 = ", line)
        line = re.sub(r"TO\s+", "TO ", line)

        formatted_lines.append(line)

    return "\n".join(formatted_lines)


def format_logo(code):
    """Format Logo code"""
    # Logo is typically written in uppercase with proper spacing
    code = code.upper()

    # Add spaces around brackets and operators
    code = re.sub(r"\[", " [ ", code)
    code = re.sub(r"\]", " ] ", code)
    code = re.sub(r"\s+", " ", code)  # Normalize spaces

    # Fix common Logo commands
    replacements = {
        "FORWARD": "FORWARD",
        "BACK": "BACK",
        "RIGHT": "RIGHT",
        "LEFT": "LEFT",
        "REPEAT": "REPEAT",
        "END": "END",
    }

    for old, new in replacements.items():
        code = re.sub(rf"\b{re.escape(old)}\b", new, code)

    return code.strip()


def format_pilot(code):
    """Format PILOT code"""
    lines = code.strip().split("\n")
    formatted_lines = []

    for line in lines:
        line = line.strip()
        if not line:
            continue

        # Ensure proper colon after labels
        if re.match(r"^[A-Z]:", line):
            # Already has colon
            pass
        elif re.match(r"^[A-Z]\s", line):
            # Add colon after single letter label
            line = re.sub(r"^([A-Z])\s", r"\1:", line)

            # Capitalize labels and commands
            line = re.sub(
                r"^([a-zA-Z]):",
                lambda m: m.group(1).upper() + ":",
                line,
            )

        formatted_lines.append(line)

    return "\n".join(formatted_lines)


def format_pascal(code):
    """Format Pascal code (basic)"""
    # Basic Pascal formatting - this would need a proper parser
    # for full formatting
    code = code.strip()

    # Add semicolons if missing (basic heuristic)
    if not code.endswith(";") and not code.endswith("."):
        code += ";"

    # Capitalize program keyword
    code = re.sub(r"\bprogram\b", "PROGRAM", code, flags=re.IGNORECASE)
    code = re.sub(r"\bbegin\b", "BEGIN", code, flags=re.IGNORECASE)
    code = re.sub(r"\bend\b", "END", code, flags=re.IGNORECASE)

    return code


def on_file_save(file_path, content):
    """Hook called when a file is saved"""
    if _plugin_state["auto_format_enabled"]:
        file_info = f"{file_path} (len={len(content)} bytes)"
        print(f"🎨 Auto-formatting {file_info} on save")
        # Would format and save the file


# Plugin metadata
PLUGIN_NAME = "Code Formatter Plugin"
PLUGIN_VERSION = "1.0.0"
PLUGIN_DESCRIPTION = "Code formatting and beautification for Time Warp IDE"

# Plugin state (kept for external inspection during runtime)
_plugin_state = {"context": None, "auto_format_enabled": False}
