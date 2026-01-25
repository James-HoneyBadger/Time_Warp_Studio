"""Error hint and suggestion system for Time Warp Studio.

Provides typo detection, command suggestions, and syntax checking
to help users fix common programming mistakes.
"""

from typing import Optional

# Common typos and their corrections (50+ entries)
TYPO_SUGGESTIONS = {
    # BASIC typos
    "PRITN": "PRINT",
    "PIRNT": "PRINT",
    "PRNT": "PRINT",
    "PRNIT": "PRINT",
    "PINT": "PRINT",
    "GOTP": "GOTO",
    "GTOO": "GOTO",
    "GOOT": "GOTO",
    "GOT0": "GOTO",
    "GOTO0": "GOTO",
    "IMPUT": "INPUT",
    "INPT": "INPUT",
    "INPTU": "INPUT",
    "INUT": "INPUT",
    "LOACTE": "LOCATE",
    "LOCAT": "LOCATE",
    "LCATE": "LOCATE",
    "CLEAE": "CLEAR",
    "CLAR": "CLEAR",
    "CLER": "CLEAR",
    # Logo typos
    "FORWAD": "FORWARD",
    "FORWADR": "FORWARD",
    "FORWAR": "FORWARD",
    "FORWRD": "FORWARD",
    "FWD": "FORWARD",
    "FORARD": "FORWARD",
    "BACKWAD": "BACK",
    "BAKC": "BACK",
    "BCK": "BACK",
    "BKWD": "BACK",
    "LEFF": "LEFT",
    "LFT": "LEFT",
    "LETF": "LEFT",
    "RIGTH": "RIGHT",
    "RIGT": "RIGHT",
    "RIHT": "RIGHT",
    "RGT": "RIGHT",
    "RGHT": "RIGHT",
    "PENP": "PENUP",
    "PENU": "PENUP",
    "PNUP": "PENUP",
    "PNEUP": "PENUP",
    "PEND": "PENDOWN",
    "PENDN": "PENDOWN",
    "PNDOWN": "PENDOWN",
    "PENDONW": "PENDOWN",
    "REPAT": "REPEAT",
    "REPEATT": "REPEAT",
    "REPET": "REPEAT",
    "RPT": "REPEAT",
    "REPTEAT": "REPEAT",
    "HIME": "HOME",
    "HOEM": "HOME",
    "HME": "HOME",
    "HOMW": "HOME",
    "CLAER": "CLEAR",
    # PILOT typos
    "TPYE": "T:",
    "TYEP": "T:",
    "ACCPET": "A:",
    "ACEPT": "A:",
    "ACPT": "A:",
    "MACTH": "M:",
    "MTCH": "M:",
    "MATHC": "M:",
    "JUPM": "J:",
    "JMP": "J:",
    "JUM": "J:",
    "YESS": "Y:",
    "YEA": "Y:",
    "YEP": "Y:",
    "NOO": "N:",
    "NOP": "N:",
    "COPMUTE": "C:",
    "COMPUE": "C:",
    "COMPT": "C:",
    "LABLE": "L:",
    "LABL": "L:",
    "LBL": "L:",
    "USE": "U:",
    "USSE": "U:",
    # Common words
    "VARAIBLE": "VARIABLE",
    "VARIBLE": "VARIABLE",
    "VAIABLE": "VARIABLE",
    "VARIALE": "VARIABLE",
    "THN": "THEN",
    "TAHN": "THEN",
    "TEHN": "THEN",
    "ESLE": "ELSE",
    "ELS": "ELSE",
    "ELSSE": "ELSE",
    "FRO": "FOR",
    "OFRI": "FOR",
    "NEX": "NEXT",
    "NXET": "NEXT",
    "NECT": "NEXT",
    "SETP": "STEP",
    "STP": "STEP",
    "STPE": "STEP",
    "END": "END",
    "ENF": "END",
    "ENN": "END",
}


def levenshtein_distance(s1: str, s2: str) -> int:
    """Calculate Levenshtein distance between two strings.

    Args:
        s1: First string
        s2: Second string

    Returns:
        Edit distance (number of single-character edits needed)
    """
    if len(s1) < len(s2):
        # Swap via keywords to avoid positional-argument warnings from static
        # analyzers (the logic intentionally swaps shorter/longer strings).
        return levenshtein_distance(s1=s2, s2=s1)

    if len(s2) == 0:
        return len(s1)

    # Create two rows (current and previous)
    previous_row = list(range(len(s2) + 1))

    for i, c1 in enumerate(s1):
        current_row = [i + 1]
        for j, c2 in enumerate(s2):
            # Cost of insertion, deletion, substitution
            insertions = previous_row[j + 1] + 1
            deletions = current_row[j] + 1
            substitutions = previous_row[j] + (c1 != c2)
            current_row.append(min(insertions, deletions, substitutions))
        previous_row = current_row

    return previous_row[-1]


def suggest_command(command: str) -> Optional[str]:
    """Suggest a correction for a potentially mistyped command.

    Args:
        command: Command string to check

    Returns:
        Suggested command if found, None otherwise
    """
    # Normalize to uppercase for comparison
    cmd_upper = command.upper().strip()

    # Check direct typo dictionary first
    if cmd_upper in TYPO_SUGGESTIONS:
        return TYPO_SUGGESTIONS[cmd_upper]

    # Define known commands for each language
    known_commands = {
        # BASIC commands
        "PRINT",
        "LET",
        "INPUT",
        "GOTO",
        "IF",
        "THEN",
        "ELSE",
        "FOR",
        "TO",
        "NEXT",
        "STEP",
        "GOSUB",
        "RETURN",
        "REM",
        "END",
        "SCREEN",
        "CLS",
        "LOCATE",
        "INKEY$",
        "COLOR",
        "DIM",
        "DATA",
        "READ",
        "RESTORE",
        # Logo commands
        "FORWARD",
        "BACK",
        "LEFT",
        "RIGHT",
        "PENUP",
        "PENDOWN",
        "HOME",
        "CLEAR",
        "REPEAT",
        "SETCOLOR",
        "SETPENCOLOR",
        "SETBGCOLOR",
        "SETPENWIDTH",
        "SETHEADING",
        "HIDETURTLE",
        "SHOWTURTLE",
        # "TO",  # Duplicate
        # "END", # Duplicate
        # PILOT commands
        "T:",
        "A:",
        "M:",
        "Y:",
        "N:",
        "C:",
        "U:",
        "J:",
        "L:",
        "E:",
        "R:",
    }

    # Find closest match using Levenshtein distance
    best_match = None
    best_distance = float("inf")

    for known_cmd in known_commands:
        distance = levenshtein_distance(cmd_upper, known_cmd)

        # Only suggest if distance is small (1-2 edits)
        if distance <= 2 and distance < best_distance:
            best_distance = distance
            best_match = known_cmd

    return best_match


def check_syntax_mistakes(line: str) -> Optional[str]:
    # The function performs many early-exit checks for different syntax
    # problems which results in multiple returns/branches. That's intentional
    # and keeps each check simple and readable.
    # pylint: disable=too-many-return-statements,too-many-branches
    """Check for common syntax mistakes in a line of code.

    Args:
        line: Line of code to check

    Returns:
        Error message if mistake found, None otherwise
    """
    # Check for unclosed quotes
    if line.count('"') % 2 != 0:
        return "Unclosed string literal (missing closing quote)"

    if line.count("'") % 2 != 0:
        return "Unclosed string literal (missing closing quote)"

    # Check for unmatched parentheses
    paren_count = 0
    for char in line:
        if char == "(":
            paren_count += 1
        elif char == ")":
            paren_count -= 1
        if paren_count < 0:
            return "Unmatched parentheses (extra closing parenthesis)"

    if paren_count > 0:
        return "Unmatched parentheses (missing closing parenthesis)"

    # Check for common BASIC mistakes
    line_upper = line.upper().strip()

    # IF without THEN
    if line_upper.startswith("IF ") and " THEN" not in line_upper:
        # Check if it's not a PILOT command (which doesn't need THEN)
        if not line_upper.startswith("IF("):
            return "IF statement missing THEN keyword"

    # FOR without TO
    if line_upper.startswith("FOR ") and " TO " not in line_upper:
        return "FOR loop missing TO keyword"

    # GOTO without line number
    if line_upper.startswith("GOTO "):
        target = line[5:].strip()
        if not target:
            return "GOTO missing line number or label"

    # GOSUB without line number
    if line_upper.startswith("GOSUB "):
        target = line[6:].strip()
        if not target:
            return "GOSUB missing line number"

    # Check for Logo REPEAT without count
    if line_upper.startswith("REPEAT "):
        parts = line[7:].strip().split()
        # Normalize the first token and check if it's a numeric literal
        num_token = parts[0] if parts else ""
        # Normalize the token by removing '.' and '-' before digit check.
        num_token_clean = num_token.replace(".", "").replace("-", "")
        if not parts or not num_token_clean.isdigit():
            return "REPEAT missing count number"

    # Check for assignment without variable
    if "=" in line and not line.strip().startswith("="):
        parts = line.split("=", 1)
        if not parts[0].strip():
            return "Assignment missing variable name"

    return None


def get_enhanced_error_message(error: str, context: str = "") -> str:
    """Enhance an error message with suggestions.

    Args:
        error: Original error message
        context: Optional context (line of code, etc.)

    Returns:
        Enhanced error message with suggestions
    """
    enhanced = f"❌ Error: {error}"

    # Try to extract command from context
    if context:
        # Check for syntax mistakes
        syntax_error = check_syntax_mistakes(context)
        if syntax_error:
            enhanced += f"\n   💡 {syntax_error}"

        # Try to suggest command corrections
        words = context.upper().strip().split()
        if words:
            first_word = words[0].rstrip(":")
            suggestion = suggest_command(first_word)
            if suggestion:
                enhanced += f"\n   💡 Did you mean '{suggestion}'?"

    return enhanced
