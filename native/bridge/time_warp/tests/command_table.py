"""The command examination table: one row per command, and the waivers.

Each row is the *smallest program that exercises one command* plus what should
happen. This file is data, not a test, so pytest does not collect it; the reader
is `native/tools/command_matrix.py`, which drives these rows through the real
bridge.

Two rules keep the table honest:

* **Written in documented syntax.** Every invocation here comes from
  `docs/LANGUAGE_GUIDE.md` or `Examples/**`, not from whatever the interpreter
  happens to tolerate. A row that only passes because the parser is lenient is
  testing the wrong thing.
* **Observable, or it is not a row.** A statement whose whole effect is internal
  state (`X = 5`) cannot be judged, so it is written together with the smallest
  thing that shows the state changed (`X = 5` then `PRINT X`). This is also why
  `expect` exists: "it produced nothing at all" is the failure this whole table
  is built to catch.

`WAIVERS` is the other half of the completeness guarantee: a command that is
implemented and can never be exercised automatically has to be listed with the
reason, so "not tested" is a decision on the record rather than an oversight.
`native/tools/command_inventory.py` extracts the implemented set to check the
table and the waivers against.
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class Row:
    """One command, the source that exercises it, and what should happen."""

    language: str
    command: str
    source: str
    #: any | prints:TEXT | draws | error
    expect: str
    #: Other commands this same source also exercises. A round trip through a file
    #: channel is one program but four commands, and the completeness check should
    #: see all four as covered rather than leaving three looking untested.
    also: tuple[str, ...] = ()


@dataclass(frozen=True)
class Waiver:
    """An implemented command that cannot be exercised automatically."""

    language: str
    command: str
    reason: str


#: The name the file-channel rows round-trip through. The engine's file service
#: resolves a relative name against the working directory, and the harness deletes
#: this file after the run.
SCRATCH = "tws_matrix_scratch.txt"


# ---------------------------------------------------------------------------
# BASIC
# ---------------------------------------------------------------------------

BASIC: list[Row] = [
    Row("BASIC", "PRINT string", 'PRINT "HELLO WORLD"', "prints:HELLO WORLD"),
    Row("BASIC", "PRINT numeric", "PRINT 6 * 7", "prints:42"),
    Row("BASIC", "PRINT trailing separator", 'PRINT "A", "B"', "prints:A"),
    Row("BASIC", "assignment", "X = 42\nPRINT X", "prints:42"),
    Row("BASIC", "string variable", 'NAME$ = "Ann"\nPRINT NAME$', "prints:Ann"),
    Row("BASIC", "string concatenation", 'A$ = "X" + "Y"\nPRINT A$', "prints:XY"),
    Row("BASIC", "DIM and index", "DIM N(10)\nN(1) = 7\nPRINT N(1)", "prints:7"),
    Row(
        "BASIC",
        "IF / ELSE / END IF",
        'X = 20\nIF X > 10 THEN\nPRINT "BIG"\nELSE\nPRINT "SMALL"\nEND IF',
        "prints:BIG",
    ),
    Row(
        "BASIC",
        "IF false branch",
        'X = 1\nIF X > 10 THEN\nPRINT "BIG"\nELSE\nPRINT "SMALL"\nEND IF',
        "prints:SMALL",
    ),
    Row("BASIC", "FOR / NEXT", "FOR I = 1 TO 3\nPRINT I\nNEXT I", "prints:3"),
    Row(
        "BASIC", "FOR with STEP", "FOR I = 0 TO 10 STEP 5\nPRINT I\nNEXT I", "prints:10"
    ),
    Row(
        "BASIC",
        "WHILE / WEND",
        "X = 0\nWHILE X < 3\nX = X + 1\nWEND\nPRINT X",
        "prints:3",
    ),
    Row(
        "BASIC",
        "DO / LOOP UNTIL",
        "X = 0\nDO\nX = X + 1\nLOOP UNTIL X >= 3\nPRINT X",
        "prints:3",
    ),
    Row(
        "BASIC",
        "GOSUB and RETURN",
        "10 X = 4\n20 GOSUB 100\n30 PRINT RESULT\n40 END\n100 RESULT = X * 2\n110 RETURN",
        "prints:8",
    ),
    Row("BASIC", "DEF FN", "DEF FNA(N) = N * N\nPRINT FNA(5)", "prints:25"),
    Row("BASIC", "LEN", 'PRINT LEN("HELLO")', "prints:5"),
    Row("BASIC", "MID$", 'PRINT MID$("HELLO", 1, 3)', "prints:HEL"),
    Row("BASIC", "CHR$", "PRINT CHR$(65)", "prints:A"),
    Row("BASIC", "REPEAT$", 'PRINT REPEAT$("ab", 3)', "prints:ababab"),
    # `FORMAT$` takes either a % conversion or a # mask; both are checked
    # because the reference tries them in that order.
    Row("BASIC", "FORMAT$ mask", 'PRINT FORMAT$("#.##", 3.14159)', "prints:3.14"),
    Row("BASIC", "FORMAT$ printf", 'PRINT FORMAT$("%05.2f", 3.14159)', "prints:03.14"),
    # The clock is not a constant, so each row asserts its shape rather than a
    # value: HH:MM:SS is 8 characters and MM-DD-YYYY is 10. The assignment is
    # the form both engines substitute the clock into; `LEN(TIME$)` reads the
    # name as a string variable and answers 0.
    Row("BASIC", "TIME$", "A$ = TIME$\nPRINT LEN(A$)", "prints:8"),
    Row("BASIC", "DATE$", "A$ = DATE$\nPRINT LEN(A$)", "prints:10"),
    Row("BASIC", "TIMER", "PRINT TIMER >= 0", "prints:1"),
    Row("BASIC", "ABS", "PRINT ABS(-5)", "prints:5"),
    Row("BASIC", "SQR", "PRINT SQR(16)", "prints:4"),
    Row("BASIC", "INT", "PRINT INT(3.7)", "prints:3"),
    Row("BASIC", "SIN", "PRINT SIN(0)", "prints:0"),
    Row("BASIC", "COS", "PRINT COS(0)", "prints:1"),
    Row("BASIC", "TAN", "PRINT TAN(0)", "prints:0"),
    Row("BASIC", "ATAN", "PRINT ATAN(1)", "prints:"),
    Row("BASIC", "EXP", "PRINT EXP(0)", "prints:1"),
    Row("BASIC", "LOG", "PRINT LOG(1)", "prints:0"),
    Row("BASIC", "RND", "PRINT RND(1)", "prints:"),
    Row(
        "BASIC",
        "INPUT through the protocol",
        'INPUT "Name: ", N\nPRINT "GOT:", N',
        "prints:GOT",
    ),
    Row("BASIC", "CLS", "CLS", "any"),
    Row("BASIC", "SCREEN", "SCREEN 1", "any"),
    Row("BASIC", "COLOR then PSET", "SCREEN 1\nCOLOR 15, 0\nPSET (1, 1)", "draws"),
    Row("BASIC", "LINE", "SCREEN 1\nLINE (10, 10) - (100, 50)", "draws"),
    Row("BASIC", "CIRCLE", "SCREEN 1\nCIRCLE (160, 100), 40", "draws"),
    Row("BASIC", "PSET", "SCREEN 1\nPSET (5, 5)", "draws"),
    Row(
        "BASIC",
        "PAINT",
        "SCREEN 1\nLINE (10, 10) - (100, 50)\nPAINT (50, 30), 4",
        "draws",
    ),
    Row("BASIC", "LOCATE then PRINT", 'LOCATE 5, 5\nPRINT "X"', "any"),
    # -- The Logo blend. A statement BASIC does not recognise, whose first word
    #    is a Logo command, is handed to the Logo executor -- so a `.bas` file
    #    can use turtle words. The row proves the forwarding still happens and
    #    still draws; an unknown word must remain an error, which is the
    #    "unknown statement" row further down.
    Row(
        "BASIC",
        "Logo turtle words",
        "CLEARSCREEN\nFORWARD 15\nRIGHT 90\nPENUP\nPENDOWN",
        "draws",
        ("FORWARD", "RIGHT", "PENUP", "PENDOWN"),
    ),
    # -- Structural keywords. Each is already a piece of a program above; giving
    #    it a row of its own is what makes a break in the keyword attributable
    #    instead of showing up as a mystery failure in the row that used it.
    Row("BASIC", "NEXT", "FOR I = 1 TO 2\nNEXT I\nPRINT I", "prints:2"),
    Row("BASIC", "WEND", "X = 0\nWHILE X < 2\nX = X + 1\nWEND\nPRINT X", "prints:2"),
    Row("BASIC", "LOOP", "X = 0\nDO\nX = X + 1\nLOOP UNTIL X = 2\nPRINT X", "prints:2"),
    Row(
        "BASIC",
        "UNTIL",
        'X = 0\nDO\nX = X + 1\nLOOP UNTIL X = 2\nPRINT "DONE"',
        "prints:DONE",
    ),
    Row(
        "BASIC", "ELSE", 'IF 1 > 2 THEN\nPRINT "A"\nELSE\nPRINT "B"\nEND IF', "prints:B"
    ),
    Row("BASIC", "END IF", 'IF 1 < 2 THEN\nPRINT "A"\nEND IF', "prints:A"),
    Row("BASIC", "ENDIF alias", 'IF 1 < 2 THEN\nPRINT "A"\nENDIF', "prints:A"),
    Row("BASIC", "END stops the program", 'PRINT "A"\nEND\nPRINT "B"', "prints:A"),
    Row(
        "BASIC",
        "RETURN",
        "GOSUB 100\nPRINT RESULT\nEND\n100 RESULT = 9\n110 RETURN",
        "prints:9",
    ),
    Row("BASIC", "REM", 'REM ignored\nPRINT "A"', "prints:A"),
    Row("BASIC", "apostrophe comment", '\'ignored\nPRINT "A"', "prints:A"),
    Row("BASIC", "colon separates statements", 'PRINT "A": PRINT "B"', "prints:B"),
    Row(
        "BASIC",
        "SELECT / CASE / END SELECT",
        'X = 2\nSELECT CASE X\nCASE 2\nPRINT "TWO"\nEND SELECT',
        "prints:TWO",
        ("CASE", "END SELECT", "SELECT"),
    ),
    Row(
        "BASIC",
        "SUB / CALL / END SUB",
        "SUB DOUBLE(N)\nPRINT N * 2\nEND SUB\nCALL DOUBLE(21)",
        "prints:42",
        ("CALL", "END SUB"),
    ),
    Row(
        "BASIC",
        "FUNCTION / END FUNCTION",
        "FUNCTION TWICE(N)\nTWICE = N * 2\nEND FUNCTION\nCALL TWICE(21)\nPRINT TWICE",
        "prints:42",
        ("END FUNCTION",),
    ),
    Row(
        "BASIC",
        "TYPE / END TYPE",
        'TYPE POINT\nX AS INTEGER\nY AS INTEGER\nEND TYPE\nPRINT "OK"',
        "prints:OK",
        ("END TYPE",),
    ),
    Row("BASIC", "GOTO", 'GOTO 20\nPRINT "SKIPPED"\n20 PRINT "HERE"', "prints:HERE"),
    # A variable whose name merely *starts* with a keyword used to be swallowed by
    # that keyword's statement (DOG = 42 became a DO loop), leaving the variable
    # unset and PRINT echoing the name. These are the regressions for that.
    Row("BASIC", "variable starting with DO", "DOUBLE = 42\nPRINT DOUBLE", "prints:42"),
    Row("BASIC", "variable starting with RUN", "RUNNER = 7\nPRINT RUNNER", "prints:7"),
    Row(
        "BASIC", "variable starting with CLOSE", "CLOSER = 3\nPRINT CLOSER", "prints:3"
    ),
    Row("BASIC", "variable starting with NEXT", "NEXTX = 5\nPRINT NEXTX", "prints:5"),
    Row("BASIC", "variable starting with LOOP", "LOOPS = 9\nPRINT LOOPS", "prints:9"),
    Row("BASIC", "LET", "LET X = 7\nPRINT X", "prints:7"),
    Row("BASIC", "MAKE", 'MAKE "X" 7\nPRINT X', "prints:7"),
    Row(
        "BASIC",
        "SETXY then read the position",
        'SETXY 10 20\nPRINT "MOVED"',
        "prints:MOVED",
    ),
    # -- DATA / READ / RESTORE are one mechanism, so each row carries a program that
    #    shows the pointer moved rather than trusting the statement ran.
    Row(
        "BASIC",
        "DATA with READ",
        "DATA 3, 4\nREAD A\nREAD B\nPRINT A + B",
        "prints:7",
        ("DATA",),
    ),
    Row(
        "BASIC",
        "RESTORE",
        "DATA 3, 4\nREAD A\nRESTORE\nREAD B\nPRINT B",
        "prints:3",
        ("DATA", "READ"),
    ),
    # -- Error handling. ON ERROR GOTO must actually divert, so the row proves the
    #    handler ran rather than that the statement parsed.
    Row(
        "BASIC",
        "ON ERROR GOTO diverts",
        'ON ERROR GOTO 100\nX = 1 / 0\nPRINT "NO"\nEND\n100 PRINT "CAUGHT"',
        "prints:CAUGHT",
        ("ON ERROR GOTO",),
    ),
    Row(
        "BASIC",
        "ON ERROR disables the handler",
        'ON ERROR GOTO 100\nON ERROR\nPRINT "OK"\nEND\n100 PRINT "CAUGHT"',
        "prints:OK",
        ("ON ERROR GOTO", "ON ERROR"),
    ),
    Row(
        "BASIC",
        "RESUME NEXT",
        '10 ON ERROR GOTO 100\n20 X = 1 / 0\n30 PRINT "AFTER"\n40 END\n100 RESUME NEXT',
        "prints:AFTER",
        ("RESUME", "ON ERROR GOTO"),
    ),
    Row(
        "BASIC",
        "RESUME",
        'ON ERROR GOTO 100\nX = 1 / 0\nEND\n100 RESUME 30\n30 PRINT "RESUMED"',
        "prints:RESUMED",
        ("ON ERROR GOTO",),
    ),
    # -- Declarations. These return no text of their own, so the row carries the
    #    smallest statement that shows the declaration took effect.
    Row("BASIC", "CONST", "CONST SIZE = 5\nPRINT SIZE", "prints:5"),
    Row("BASIC", "DEFINT truncates", "DEFINT X\nX = 3.7\nPRINT X", "prints:3"),
    Row("BASIC", "DEFLNG", 'DEFLNG A-Z\nPRINT "OK"', "prints:OK"),
    Row("BASIC", "DEFSNG", 'DEFSNG A-Z\nPRINT "OK"', "prints:OK"),
    Row("BASIC", "DEFDBL", 'DEFDBL A-Z\nPRINT "OK"', "prints:OK"),
    Row("BASIC", "DEFSTR", 'DEFSTR A-Z\nA = "HI"\nPRINT A$', "prints:HI"),
    Row("BASIC", "SWAP", "A = 1\nB = 2\nSWAP A, B\nPRINT A", "prints:2"),
    Row(
        "BASIC", "SWAP strings", 'A$ = "X"\nB$ = "Y"\nSWAP A$, B$\nPRINT A$', "prints:Y"
    ),
    Row("BASIC", "ERASE", "DIM A(3)\nA(0) = 9\nERASE A\nPRINT A(0)", "prints:0"),
    Row("BASIC", "WIDTH", "WIDTH 80", "prints:Set width to 80"),
    Row("BASIC", "PRESET", "SCREEN 1\nPSET (5, 5), 1\nPRESET (5, 5)", "draws"),
    Row("BASIC", "PRINT USING", 'PRINT USING "##.##"; 3.14159', "prints:3.14"),
    Row(
        "BASIC",
        "PALETTE CYCLE",
        "SCREEN 1\nPALETTE CYCLE 1, 15, 1",
        "prints:Palette cycled",
        ("CYCLE",),
    ),
    # -- Sound, speech, animation and effects. The optional backends (pygame,
    #    pyttsx3) are absent on a bare machine, so these rows assert the command
    #    is *acknowledged* rather than that a speaker moved.
    Row("BASIC", "BEEP", "BEEP", "prints:BEEP"),
    Row("BASIC", "SOUND", "SOUND 440, 2", "prints:SOUND 440Hz"),
    Row("BASIC", "SLEEP", "SLEEP 1", "prints:SLEEP"),
    Row("BASIC", "SPEED", "SPEED 5", "prints:Turtle speed set to 5"),
    Row("BASIC", "RANDOMIZE", 'RANDOMIZE 42\nPRINT "OK"', "prints:OK"),
    Row(
        "BASIC",
        "RANDOMIZE TIMER",
        'RANDOMIZE TIMER\nPRINT "OK"',
        "prints:OK",
        ("TIMER",),
    ),
    Row("BASIC", "PLAY", 'PLAY "CDEFG"', "prints:Playing"),
    Row("BASIC", "SAY", 'SAY "HELLO"', "any"),
    Row("BASIC", "SPEAK", 'SPEAK "HELLO"', "any"),
    Row("BASIC", "SPRITE define", "SPRITE SHIP, 10, 20, 30, 40", "prints:Sprite SHIP"),
    Row(
        "BASIC",
        "SPRITE remove",
        "SPRITE SHIP, 10, 20, 30, 40\nSPRITE SHIP, OFF",
        "prints:Removed sprite",
    ),
    Row("BASIC", "ON TIMER", "ON TIMER(2) GOSUB 100", "prints:Timer set"),
    Row("BASIC", "TIMER ON", "ON TIMER(2) GOSUB 100\nTIMER ON", "prints:Timer enabled"),
    Row(
        "BASIC",
        "TIMER OFF",
        "ON TIMER(2) GOSUB 100\nTIMER ON\nTIMER OFF",
        "prints:Timer disabled",
    ),
    Row("BASIC", "FRACTAL lists presets", "FRACTAL", "prints:Available fractals"),
    Row("BASIC", "FRACTAL KOCH draws", "FRACTAL KOCH", "draws"),
    Row("BASIC", "SHAPE STAR", "SHAPE STAR, 40", "draws"),
    Row("BASIC", "SHAPE POLYGON with a count", "SHAPE POLYGON 6, 40", "draws"),
    Row("BASIC", "SHAPE unknown name", "SHAPE NOSUCH, 10", "error"),
    Row(
        "BASIC",
        "PARTICLE",
        "PARTICLE EXPLOSION, 100, 100",
        "prints:Created EXPLOSION effect",
    ),
    # -- Machine-facing statements. SHELL is deliberately inert, and the port and
    #    memory commands are readable through the statement forms.
    Row(
        "BASIC",
        "SHELL is blocked",
        'SHELL "echo hi"',
        "prints:SHELL commands are disabled",
    ),
    Row("BASIC", "SYSTEM", 'PRINT "A"\nSYSTEM', "prints:A"),
    Row("BASIC", "RUN", 'PRINT "A"\nRUN', "any"),
    Row(
        "BASIC",
        "POKE then PEEK",
        "POKE 1000, 7\nPEEK 1000",
        "prints:PEEK(1000) = 7",
        ("PEEK",),
    ),
    Row(
        "BASIC",
        "PEEK as an expression",
        "POKE 1000, 7\nX = PEEK(1000)\nPRINT X",
        "prints:7",
        ("POKE",),
    ),
    Row("BASIC", "OUT then IN", "OUT 100, 5\nIN 100", "prints:IN(100) = 5", ("IN",)),
    Row(
        "BASIC",
        "IN as an expression",
        "OUT 100, 5\nPRINT IN(100)",
        "prints:5",
        ("OUT",),
    ),
    Row("BASIC", "JOYINIT reports its backend", "JOYINIT", "any"),
    Row(
        "BASIC",
        "STICK / STRIG",
        "PRINT STICK(0)\nPRINT STRIG(0)",
        "prints:0",
        ("JOYINIT",),
    ),
    Row("BASIC", "TYPE$", "X = 5\nPRINT TYPE$(X)", "prints:DOUBLE"),
    Row(
        "BASIC",
        "RESUME NEXT without line numbers",
        'ON ERROR GOTO 100\nX = 1 / 0\nPRINT "AFTER"\nEND\n100 RESUME NEXT',
        "prints:AFTER",
        ("RESUME", "ON ERROR GOTO"),
    ),
    # -- File channels, exercised as a real round trip rather than waived. The
    #    harness deletes the scratch file afterwards.
    Row(
        "BASIC",
        "WRITE # round trip",
        f'OPEN "{SCRATCH}" FOR OUTPUT AS #3\nWRITE #3, "BYE"\nCLOSE #3\nPRINT "WROTE"',
        "prints:WROTE",
        ("OPEN", "WRITE #", "CLOSE"),
    ),
    # -- INPUT requires the protocol to answer, so the row asserts the program
    #    resumed with the line the harness sent rather than hanging.
    Row(
        "BASIC", "LINE INPUT", 'LINE INPUT "Name: ", N$\nPRINT "GOT:", N$', "prints:GOT"
    ),
    # -- File channels. The workspace file service confines these to a scratch
    #    name, and the round trip is the only way to know PRINT # actually wrote.
    Row(
        "BASIC",
        "OPEN / PRINT # / CLOSE round trip",
        f'OPEN "{SCRATCH}" FOR OUTPUT AS #1\nPRINT #1, "HELLO"\nCLOSE #1\nPRINT "WROTE"',
        "prints:WROTE",
        ("PRINT #", "CLOSE"),
    ),
    Row(
        "BASIC",
        "INPUT # reads it back",
        f'OPEN "{SCRATCH}" FOR INPUT AS #1\nLINE INPUT #1, L$\nCLOSE #1\nPRINT L$',
        "prints:HELLO",
        ("INPUT #", "LINE INPUT"),
    ),
    Row("BASIC", "PUT without an open channel", 'PUT #9, , "X"', "error"),
    Row("BASIC", "GET without an open channel", "GET #9, 1, V$", "error"),
    Row("BASIC", "CLOSE with nothing open", "CLOSE", "prints:Closed all files"),
    Row("BASIC", "error: unknown statement", "NOSUCHSTATEMENT 1 2", "error"),
    Row("BASIC", "error: incomplete FOR", "FOR I = 1 TO", "error"),
    Row("BASIC", "error: division by zero", "PRINT 1 / 0", "error"),
]

# ---------------------------------------------------------------------------
# Logo
# ---------------------------------------------------------------------------

LOGO: list[Row] = [
    Row("Logo", "PRINT string", 'PRINT "HELLO', "prints:HELLO"),
    Row("Logo", "PRINT number", "PRINT 42", "prints:42"),
    Row("Logo", "MAKE then PRINT", 'MAKE "X 5\nPRINT :X', "prints:5"),
    Row("Logo", "SUM reporter", "PRINT SUM 1 2", "prints:3"),
    Row("Logo", "FD", "FD 50", "draws"),
    Row("Logo", "BK", "BK 50", "draws"),
    Row("Logo", "RT then FD", "RT 90\nFD 50", "draws"),
    Row("Logo", "LT then FD", "LT 90\nFD 50", "draws"),
    Row("Logo", "REPEAT", "REPEAT 4 [FD 10 RT 90]", "draws"),
    Row(
        "Logo",
        "REPEAT with nested variable",
        'MAKE "N 3\nREPEAT :N [FD 10 RT 120]',
        "draws",
    ),
    Row("Logo", "PU suppresses ink", "PU\nFD 50", "any"),
    Row("Logo", "PD resumes ink", "PU\nFD 50\nPD\nFD 50", "draws"),
    Row("Logo", "SETPENCOLOR", 'SETPENCOLOR "RED\nFD 20', "draws"),
    Row("Logo", "SETPENWIDTH", "SETPENWIDTH 3\nFD 20", "draws"),
    Row("Logo", "SETXY", "SETXY 10 10", "draws"),
    Row("Logo", "SETH then FD", "SETH 90\nFD 20", "draws"),
    Row("Logo", "HOME", "FD 20\nHOME", "draws"),
    Row("Logo", "CS", "FD 20\nCS", "any"),
    Row(
        "Logo",
        "procedure and call",
        "TO SQUARE :S\nREPEAT 4 [FD :S RT 90]\nEND\nSQUARE 20",
        "draws",
    ),
    Row("Logo", "IF true branch", "IF 1 = 1 [FD 10]", "draws"),
    Row(
        "Logo",
        "IF false branch falls through",
        'IF 1 = 2 [FD 10]\nPRINT "DONE',
        "prints:DONE",
    ),
    Row("Logo", "WHILE", 'MAKE "I 0\nWHILE [:I < 3] [MAKE "I :I + 1 FD 10]', "draws"),
    # -- Turtle motion. The short forms (FD, BK, RT, LT) have rows above; these
    #    are the long names and the ones that set a position or an angle directly.
    Row("Logo", "FORWARD", "FORWARD 50", "draws"),
    Row("Logo", "BACK", "BACK 30", "draws"),
    Row("Logo", "BACKWARD", "BACKWARD 30", "draws"),
    Row("Logo", "LEFT", "LEFT 60\nFORWARD 20", "draws"),
    Row("Logo", "RIGHT", "RIGHT 120\nFORWARD 20", "draws"),
    Row("Logo", "UP", "UP 40", "draws"),
    Row("Logo", "UPWARD", "UPWARD 40", "draws"),
    Row("Logo", "DOWN", "DOWN 40", "draws"),
    Row("Logo", "DOWNWARD", "DOWNWARD 40", "draws"),
    Row("Logo", "SETX", "SETX 30", "draws"),
    Row("Logo", "SETY", "SETY 30", "draws"),
    Row("Logo", "SETPOS", "SETPOS [10 20]", "draws"),
    Row("Logo", "SETPOSITION", "SETPOSITION [10 20]", "draws"),
    Row("Logo", "SETHEADING", "SETHEADING 135\nFORWARD 20", "draws"),
    Row("Logo", "ARC", "ARC 90 40", "draws"),
    Row("Logo", "DOT", "DOT 6", "any", ("PX",)),
    Row("Logo", "STAMP", "STAMP", "draws"),
    Row("Logo", "STAMPCLEAN", "STAMP\nSTAMPCLEAN", "any", ("STAMP",)),
    Row("Logo", "WINDOW", "WINDOW", "any"),
    Row("Logo", "WRAP", "WRAP", "any"),
    Row("Logo", "FENCE", "FENCE", "any"),
    # -- Pen. Each alias is exercised through the row of the command it aliases,
    #    so a break in the alias spelling is still attributable.
    Row("Logo", "PENSIZE", "PENSIZE 3\nFORWARD 20", "draws"),
    Row("Logo", "SETPENSIZE", "SETPENSIZE 3\nFORWARD 20", "draws"),
    Row("Logo", "PENWIDTH", "PENWIDTH 3\nFORWARD 20", "draws"),
    Row("Logo", "SETPW", "SETPW 3\nFORWARD 20", "draws"),
    Row("Logo", "SETPC", 'SETPC "RED\nFORWARD 20', "draws"),
    Row("Logo", "SETCOLOR", 'SETCOLOR "RED\nFORWARD 20', "draws"),
    Row("Logo", "SETBG", 'SETBG "BLUE', "any"),
    Row("Logo", "SETBGCOLOR", 'SETBGCOLOR "BLUE', "any"),
    Row("Logo", "PENERASE", "PENERASE\nFORWARD 20", "any", ("PE",)),
    Row("Logo", "PENPAINT", "PENERASE\nPENPAINT\nFORWARD 20", "draws"),
    Row("Logo", "PENREVERSE", "PENREVERSE\nFORWARD 20", "any"),
    Row("Logo", "PENMODE", "PENMODE", "any"),
    Row("Logo", "PPT", "PPT", "any"),
    Row("Logo", "PX", "PX", "any"),
    # -- Screen and turtle visibility.
    Row("Logo", "CLEAN", "FORWARD 20\nCLEAN", "any"),
    Row("Logo", "CLEAR", "FORWARD 20\nCLEAR", "any", ("CLEARSCREEN",)),
    Row("Logo", "HIDETURTLE", "HIDETURTLE", "any", ("HT",)),
    Row("Logo", "SHOWTURTLE", "HIDETURTLE\nSHOWTURTLE", "any", ("ST",)),
    Row("Logo", "STOP", "FORWARD 10\nSTOP", "any"),
    Row("Logo", "BYE", "BYE", "any"),
    # -- Reporters and printers. The value of a reporter is only visible through
    #    PRINT or SHOW, so each row shows it.
    Row("Logo", "SHOW", "SHOW 42", "prints:42"),
    Row("Logo", "THING", 'MAKE "X 5\nSHOW THING "X', "prints:5"),
    Row("Logo", "FIRST", "SHOW FIRST [1 2 3]", "prints:1"),
    Row("Logo", "LAST", "SHOW LAST [1 2 3]", "prints:3"),
    Row("Logo", "BUTFIRST", "SHOW BUTFIRST [1 2 3]", "prints:2"),
    Row("Logo", "BUTLAST", "SHOW BUTLAST [1 2 3]", "prints:1"),
    Row("Logo", "COUNT", "SHOW COUNT [1 2 3]", "prints:3"),
    Row("Logo", "ITEM", "SHOW ITEM 2 [1 2 3]", "prints:2"),
    Row("Logo", "LIST", "SHOW LIST 1 2", "prints:1"),
    Row("Logo", "SENTENCE", "SHOW SENTENCE 1 2", "prints:1"),
    Row("Logo", "PRODUCT", "SHOW PRODUCT 3 4", "prints:12"),
    Row("Logo", "QUOTIENT", "SHOW QUOTIENT 8 2", "prints:4"),
    Row("Logo", "DIFFERENCE", "SHOW DIFFERENCE 5 3", "prints:2"),
    Row("Logo", "RANDOM", "SHOW RANDOM 10", "prints:"),
    Row("Logo", "REDUCE", 'SHOW REDUCE "SUM [1 2 3]', "prints:"),
    Row("Logo", "ROLL", "SHOW ROLL [1 2 3]", "prints:"),
    Row("Logo", "FILLED", 'FILLED "RED [FD 20 RT 90 FD 20 RT 90 FD 20]', "draws"),
    # -- Control.
    Row("Logo", "IFELSE", 'IFELSE 1 = 1 [PRINT "YES"] [PRINT "NO"]', "prints:YES"),
    Row("Logo", "TO ... END", "TO PAIR\nREPEAT 2 [FD 20 RT 180]\nEND\nPAIR", "draws"),
    Row("Logo", "FOREACH", "FOREACH [1 2 3] [SHOW ?]", "any"),
    Row("Logo", "FOREVER stops on STOP", "FOREVER [STOP]", "any"),
    Row("Logo", "REPCOUNT", "REPEAT 2 [SHOW REPCOUNT]", "prints:1"),
    Row("Logo", "LOCAL", 'TO P\nLOCAL "X\nMAKE "X 5\nSHOW :X\nEND\nP', "prints:5"),
    Row("Logo", "OUTPUT", "TO P\nOUTPUT 5\nEND\nP", "prints:5", ("OP",)),
    Row(
        "Logo",
        "OUTPUT as a value",
        "TO DOUBLE :N\nOUTPUT :N * 2\nEND\nPRINT DOUBLE 21",
        "prints:42",
        ("OUTPUT",),
    ),
    Row("Logo", "LABEL", 'LABEL "HI', "any"),
    Row("Logo", "SETLABELHEIGHT", "SETLABELHEIGHT 12", "any"),
    Row("Logo", "TEXTSIZE", "TEXTSIZE 12", "any"),
    Row("Logo", "WAIT", "WAIT 1", "any"),
    # -- 3D. The canvas has no depth, so these are judged on being acknowledged
    #    rather than on producing visible geometry.
    Row("Logo", "ENABLE3D", "ENABLE3D", "any"),
    Row("Logo", "DISABLE3D", "ENABLE3D\nDISABLE3D", "any", ("ENABLE3D",)),
    Row("Logo", "PITCH", "ENABLE3D\nPITCH 30\nFORWARD 40", "any", ("ENABLE3D",)),
    Row("Logo", "TILTUP", "TILTUP 10", "any"),
    Row("Logo", "TILTDOWN", "TILTDOWN 10", "any"),
    Row("Logo", "TILTROLL", "TILTROLL 10", "any"),
    Row("Logo", "ROLL", "ROLL 10", "any"),
    Row("Logo", "YAW", "YAW 45", "any"),
    Row("Logo", "2DON", "3DON\n2DON", "any", ("3DON",)),
    Row("Logo", "3DON", "3DON", "any"),
    Row("Logo", "3DOFF", "3DON\n3DOFF", "any"),
    Row("Logo", "error: unknown command", "NOTACOMMAND", "error"),
    Row("Logo", "error: missing argument", "FD", "error"),
]

# ---------------------------------------------------------------------------
# PILOT
# ---------------------------------------------------------------------------

PILOT: list[Row] = [
    # A row is labelled with prose ("T: text"), but PILOT dispatches on the bare
    # unit letter, so `also` carries the letter the inventory knows.
    Row("PILOT", "T: text", "T: Hello there", "prints:Hello there", ("T",)),
    Row("PILOT", "several T: units", "T: One\nT: Two", "prints:Two"),
    # `also` also covers the M: this row depends on, so the match unit is
    # exercised by name rather than by accident.
    Row(
        "PILOT",
        "A: accept, then TY/TN on no match",
        "T: Q\nA:\nM: PARIS\nTY: Correct\nTN: Wrong",
        "prints:Wrong",
        ("A", "M"),
    ),
    Row(
        "PILOT",
        "J: jump",
        "J: SKIP\nT: NOT REACHED\nE:\n*SKIP\nT: LANDED",
        "prints:LANDED",
        ("J",),
    ),
    Row(
        "PILOT",
        "U: subroutine",
        "U: *SUB\nT: BACK\nE:\n*SUB\nT: INSIDE\nE:",
        "prints:INSIDE",
        ("U",),
    ),
    Row(
        "PILOT",
        "E: ends the program",
        "T: BEFORE\nE:\nT: AFTER",
        "prints:BEFORE",
        ("E",),
    ),
    Row(
        "PILOT",
        "REMARK is ignored",
        "REMARK a note\nT: AFTER REMARK",
        "prints:AFTER REMARK",
        ("R",),
    ),
    Row(
        "PILOT",
        "C: compute stores a string",
        "C:$GREETING = Hello\nT: Said $GREETING",
        "prints:Said Hello",
        ("C",),
    ),
    Row(
        "PILOT",
        "B: branch taken on a non-zero condition",
        "C:X = 5\nB:X=*HIT\nT: MISSED\nE:\n*HIT\nT: BRANCHED",
        "prints:BRANCHED",
        ("B",),
    ),
    Row(
        "PILOT",
        "Y: jumps when the match succeeded",
        "T: Q\nA:\nM:*\nY: HIT\nT: MISSED\nE:\n*HIT\nT: HIT",
        "prints:HIT",
        ("Y",),
    ),
    Row(
        "PILOT",
        "N: prints when the match failed",
        "N: NO MATCH\nT: DONE",
        "prints:NO MATCH",
        ("N",),
    ),
    Row(
        "PILOT",
        "D: delay of zero seconds",
        "D: 0\nT: AFTER DELAY",
        "prints:AFTER DELAY",
        ("D",),
    ),
    Row(
        "PILOT",
        "P: pause resumes on Enter",
        "P:\nT: AFTER PAUSE",
        "prints:AFTER PAUSE",
        ("P",),
    ),
    Row(
        "PILOT",
        "H: hint message",
        "H: Try the other one",
        "prints:Try the other one",
        ("H",),
    ),
    Row(
        "PILOT",
        "S: subroutine, the alias of U:",
        "S: *SUB\nT: BACK\nE:\n*SUB\nT: INSIDE\nE:",
        "prints:INSIDE",
        ("S",),
    ),
    Row(
        "PILOT",
        "TU: long-form subroutine call",
        "TU SUB\nT: BACK\nE:\n*SUB\nT: INSIDE\nE:",
        "prints:INSIDE",
        ("TU",),
    ),
    Row("PILOT", "G: turtle move", "G: FD 50\nG: RT 90\nG: FD 50", "draws", ("G",)),
    Row(
        "PILOT",
        "G: FILL uses the pen colour",
        "G: FD 50\nG: RT 120\nG: FD 50\nG: RT 120\nG: FD 50\nG: FILL",
        "draws",
        ("FILL",),
    ),
    Row(
        "PILOT",
        "G: TEXT labels the drawing",
        "G: TEXT A label",
        "prints:A label",
        ("TEXT",),
    ),
    Row(
        "PILOT",
        "L: declares a label that J: can reach",
        "J: MARK\nT: NOT REACHED\nL: MARK\nT: LANDED",
        "prints:LANDED",
        ("L:",),
    ),
    Row(
        "PILOT",
        "LINK reports that chaining is not implemented",
        "LINK lesson.pilot\nT: DONE",
        "error",
        ("LINK",),
    ),
    Row(
        "PILOT",
        "F: rejects a mode it does not know",
        "F: OPEN notes.txt X",
        "error",
        ("F",),
    ),
    Row(
        "PILOT", "error: jump to a missing label", "J: NOWHERE\nT: NOT REACHED", "error"
    ),
]

# ---------------------------------------------------------------------------
# COMAL
# ---------------------------------------------------------------------------

COMAL: list[Row] = [
    Row("COMAL", "PRINT string", 'PRINT "Hello, World!"', "prints:Hello, World!"),
    Row("COMAL", "PRINT expression", "PRINT 6 * 7", "prints:42"),
    Row("COMAL", "assignment and print", "COUNT := 5\nPRINT COUNT", "prints:5"),
    Row(
        "COMAL", "string variable", 'TITLE$ := "Report"\nPRINT TITLE$', "prints:Report"
    ),
    Row(
        "COMAL",
        "IF / ENDIF",
        'SCORE := 87\nIF SCORE >= 90 THEN\nPRINT "Grade A"\nELSE\nPRINT "Keep going"\nENDIF',
        "prints:Keep going",
    ),
    Row(
        "COMAL",
        "IF / ELIF / ENDIF",
        'SCORE := 85\nIF SCORE >= 90 THEN\nPRINT "A"\nELIF SCORE >= 80 THEN\nPRINT "B"\nELSE\nPRINT "C"\nENDIF',
        "prints:B",
        ("ELIF",),
    ),
    Row("COMAL", "FOR / NEXT", "FOR I := 1 TO 3\nPRINT I\nNEXT I", "prints:3"),
    Row(
        "COMAL",
        "WHILE / ENDWHILE",
        "N := 0\nWHILE N < 3\nN := N + 1\nENDWHILE\nPRINT N",
        "prints:3",
    ),
    Row(
        "COMAL",
        "REPEAT / UNTIL",
        "N := 0\nREPEAT\nN := N + 1\nUNTIL N >= 3\nPRINT N",
        "prints:3",
    ),
    Row(
        "COMAL",
        "PROC and CALL",
        'PROC GREET(NAME$)\nPRINT "Hello, "; NAME$\nENDPROC\nCALL GREET("World")',
        "prints:Hello, World",
    ),
    Row(
        "COMAL",
        "DIM and index",
        "DIM SCORES(5)\nSCORES(1) := 9\nPRINT SCORES(1)",
        "prints:9",
    ),
    Row(
        "COMAL",
        "DATA and READ",
        "DATA 4, 5\nREAD A, B\nPRINT A + B",
        "prints:9",
        ("DATA",),
    ),
    # -- Turtle graphics. MOVE draws forward and TURN rotates, so each row moves
    #    first and turns second, otherwise there is nothing on the canvas to see.
    Row("COMAL", "MOVE", "MOVE 100", "draws"),
    Row("COMAL", "TURN", "MOVE 50\nTURN 90\nMOVE 50", "draws", ("MOVE",)),
    Row(
        "COMAL",
        "PENUP / PENDOWN",
        "MOVE 50\nPENUP\nMOVE 50\nPENDOWN\nMOVE 50",
        "draws",
        ("PENDOWN",),
    ),
    Row("COMAL", "PLOT", "MOVE 20\nPLOT", "draws", ("MOVE",)),
    Row(
        "COMAL",
        "FUNC / ENDFUNC",
        "10 FUNC DOUBLE(N)\n20 RETURN N * 2\n30 ENDFUNC\n40 CALL DOUBLE(21)\n50 PRINT RESULT",
        "prints:42",
        ("CALL", "RETURN"),
    ),
    Row(
        "COMAL",
        "FUNC called in an expression",
        "10 FUNC DOUBLE(N)\n20 RETURN N * 2\n30 ENDFUNC\n40 PRINT DOUBLE(21)",
        "prints:42",
        ("FUNC", "RETURN"),
    ),
    Row("COMAL", "error: unknown statement", "NOSUCH", "error"),
]

# ---------------------------------------------------------------------------
# REXX
# ---------------------------------------------------------------------------

REXX: list[Row] = [
    Row("REXX", "SAY string", "SAY 'Hello, World!'", "prints:Hello, World!"),
    Row("REXX", "SAY expression", "SAY 6 * 7", "prints:42"),
    Row("REXX", "assignment and SAY", "name = 'Ann'\nSAY name", "prints:Ann"),
    Row(
        "REXX",
        "concatenation with ||",
        "SAY 'Hello' || ' ' || 'World'",
        "prints:Hello World",
    ),
    Row(
        "REXX",
        "IF / THEN",
        "score = 70\nIF score > 50 THEN SAY 'pass'",
        "prints:pass",
        ("THEN",),
    ),
    Row(
        "REXX",
        "IF / THEN DO / ELSE DO",
        "score = 95\nIF score >= 90 THEN DO\nSAY 'Grade A'\nEND\nELSE DO\nSAY 'Keep going'\nEND",
        "prints:Grade A",
    ),
    Row("REXX", "DO counted loop", "DO i = 1 TO 3\nSAY i\nEND", "prints:3"),
    Row("REXX", "DO with BY", "DO n = 10 TO 1 BY -3\nSAY n\nEND", "prints:1", ("BY",)),
    Row(
        "REXX",
        "DO WHILE",
        "count = 0\nDO WHILE count < 3\ncount = count + 1\nEND\nSAY count",
        "prints:3",
    ),
    Row(
        "REXX",
        "DO OVER",
        "DO word OVER 'alpha beta gamma'\nSAY word\nEND",
        "prints:gamma",
        ("OVER",),
    ),
    Row(
        "REXX",
        "SELECT / WHEN / OTHERWISE",
        "score = 95\nSELECT\nWHEN score > 90 THEN SAY 'huge'\nOTHERWISE SAY 'small'\nEND",
        "prints:huge",
        ("WHEN", "OTHERWISE"),
    ),
    Row(
        "REXX",
        "PARSE VAR",
        "line = 'red green blue'\nPARSE VAR line first second third\nSAY second",
        "prints:green",
    ),
    Row(
        "REXX",
        "internal subroutine and RESULT",
        "CALL DOUBLE 5\nSAY RESULT\nEXIT\nDOUBLE:\nRETURN ARG(1) * 2",
        "prints:10",
        ("ARG",),
    ),
    Row("REXX", "LENGTH", "SAY LENGTH('HELLO')", "prints:5"),
    Row("REXX", "SUBSTR", "SAY SUBSTR('HELLO', 2, 3)", "prints:ELL"),
    Row("REXX", "WORD", "SAY WORD('alpha beta gamma', 2)", "prints:beta"),
    # The harness answers a prompt with an empty line, so the PULL rows assert
    # that the statement ran and the program continued rather than the value read.
    Row(
        "REXX",
        "PULL reads a line",
        "SAY 'before'\nPULL name\nSAY 'after' name",
        "prints:after",
    ),
    Row(
        "REXX",
        "PARSE PULL splits a line",
        "PARSE PULL first rest\nSAY 'got' first",
        "prints:got",
        ("PARSE PULL",),
    ),
    Row("REXX", "COPIES", "SAY COPIES('ab', 3)", "prints:ababab"),
    Row("REXX", "DATATYPE", "SAY DATATYPE('12')", "prints:NUM"),
    Row("REXX", "INDEX", "SAY INDEX('hello', 'll')", "prints:3"),
    Row("REXX", "LOWER", "SAY LOWER('ABC')", "prints:abc"),
    Row("REXX", "SIGN", "SAY SIGN(-7)", "prints:-1"),
    Row("REXX", "SPACE", "SAY SPACE('a b c', 3)", "prints:a   b   c"),
    Row("REXX", "STRIP", "SAY STRIP('  hi  ')", "prints:hi"),
    Row("REXX", "TRUNC", "SAY TRUNC(3.7)", "prints:3"),
    Row("REXX", "UPPER", "SAY UPPER('abc')", "prints:ABC"),
    Row("REXX", "WORDS", "SAY WORDS('a b c')", "prints:3"),
    Row("REXX", "error: unknown statement", "@@@ nonsense @@@", "error"),
]

# ---------------------------------------------------------------------------
# Scheme
# ---------------------------------------------------------------------------

SCHEME: list[Row] = [
    Row("Scheme", "display string", '(display "HELLO")', "prints:HELLO"),
    Row("Scheme", "display arithmetic", "(display (* 6 7))", "prints:42"),
    Row("Scheme", "newline", "(display 1)\n(newline)\n(display 2)", "prints:2"),
    Row("Scheme", "define and use", "(define pi 3.14159)\n(display pi)", "prints:3.14"),
    Row(
        "Scheme",
        "define a procedure",
        "(define (square n) (* n n))\n(display (square 7))",
        "prints:49",
    ),
    Row(
        "Scheme",
        "cond",
        '(define (sign n) (cond ((> n 0) "positive") ((< n 0) "negative") (else "zero")))\n(display (sign -3))',
        "prints:negative",
    ),
    Row(
        "Scheme",
        "if and recursion",
        "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))\n(display (fact 5))",
        "prints:120",
    ),
    Row("Scheme", "car of a list", "(display (car (list 1 2 3)))", "prints:1"),
    Row(
        "Scheme",
        "map with lambda",
        "(display (map (lambda (x) (* x x)) (list 1 2 3 4)))",
        "prints:",
    ),
    Row(
        "Scheme",
        "filter with a predicate",
        "(display (filter odd? (list 1 2 3 4 5)))",
        "prints:",
    ),
    Row("Scheme", "apply", "(display (apply + (list 10 20 30)))", "prints:60"),
    Row("Scheme", "let", "(display (let ((x 3) (y 4)) (+ x y)))", "prints:7"),
    Row(
        "Scheme",
        "closure",
        "(define (make-adder n) (lambda (x) (+ x n)))\n(define add10 (make-adder 10))\n(display (add10 5))",
        "prints:15",
    ),
    Row("Scheme", "turtle: forward", "(forward 100)", "draws"),
    Row(
        "Scheme",
        "turtle: square procedure",
        "(define (square size) (define (loop n) (when (> n 0) (forward size) (right 90) (loop (- n 1)))) (loop 4))\n(square 100)",
        "draws",
    ),
    Row("Scheme", "error: arity", "(car)", "error"),
    Row("Scheme", "error: unbound procedure", "(no-such-procedure 1)", "error"),
]


FOCAL: list[Row] = [
    Row("FOCAL", "TYPE", '01.10 T "HELLO"', "prints:HELLO"),
    Row("FOCAL", "TYPE with a format", '01.10 T %0,"N=",7/2', "prints:N=4"),
    Row("FOCAL", "SET", "01.10 S X=7\n01.20 T X", "prints:7.00"),
    Row("FOCAL", "SET several at once", "01.10 S A=1,B=2\n01.20 T A+B", "prints:3.00"),
    Row(
        "FOCAL",
        "COMMENT",
        '01.10 C this text is ignored\n01.20 T "after"',
        "prints:after",
    ),
    Row("FOCAL", "ASK", '01.10 ASK "How many? " N\n01.20 T "asked"', "prints:asked"),
    Row(
        "FOCAL",
        "IF on a sign",
        '01.10 S X=0\n01.20 IF (X) 1.40,1.30,1.40\n01.30 T "ZERO"\n01.40 Q',
        "prints:ZERO",
    ),
    Row("FOCAL", "FOR", "01.10 FOR I=1,3;T I", "prints:3.00"),
    Row(
        "FOCAL",
        "DO a group",
        '01.10 DO 2.10\n01.20 T "AFTER"\n01.30 Q\n02.10 T "GROUP"',
        "prints:GROUP",
    ),
    Row(
        "FOCAL",
        "GOTO",
        '01.10 GOTO 1.30\n01.20 T "SKIPPED"\n01.30 T "THERE"',
        "prints:THERE",
    ),
    Row("FOCAL", "ERASE", "01.10 S A=1\n01.20 E ALL\n01.30 T A", "prints:0.00"),
    Row("FOCAL", "QUIT", '01.10 T "BEFORE"\n01.20 Q\n01.30 T "AFTER"', "prints:BEFORE"),
    Row("FOCAL", "FPI", "01.10 T FPI", "prints:3.14"),
    Row("FOCAL", "FRAN", "01.10 T FRAN", "prints"),
    Row("FOCAL", "turtle: FORWARD", "01.10 FORWARD 100", "draws", also=("FD",)),
    Row("FOCAL", "turtle: BACK", "01.10 BACK 50", "draws", also=("BK",)),
    Row("FOCAL", "turtle: RIGHT", "01.10 RIGHT 90", "draws", also=("RT",)),
    Row("FOCAL", "turtle: LEFT", "01.10 LEFT 90", "draws", also=("LT",)),
    Row("FOCAL", "turtle: PENUP", "01.10 PENUP", "any"),
    Row("FOCAL", "turtle: PENDOWN", "01.10 PENDOWN", "any"),
    Row("FOCAL", "turtle: HOME", "01.10 HOME", "any"),
    Row("FOCAL", "error: unknown command", "01.10 TIPE X", "error"),
    Row("FOCAL", "error: unquoted text", "01.10 T HELLO", "error"),
    Row("FOCAL", "error: missing label", "01.10 GOTO 9.9", "error"),
]


# CHIP-8 -- the assembler's mnemonics and directives. The rows read "CHIP8"
# rather than the display name "CHIP-8", because a row's language string is the
# key `command_matrix.py` puts on the wire and the worker resolves it as
# `Language[key.upper()]`, whose member here is `CHIP8`. Every other language's
# display name happens to uppercase to its enum member, so this is the only place
# the two spellings part company.
CHIP8: list[Row] = [
    Row(
        "CHIP8",
        "CLS",
        "start: CLS\n       LD I, dot\n       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "LD a byte",
        "start: LD V0, 4\n       LD V1, 4\n       LD I, dot\n       DRW V0, V1, 1\n"
        "       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "LD one register from another",
        "start: LD V0, 6\n       LD V1, V0\n       LD I, dot\n       DRW V0, V1, 1\n"
        "       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "LD I",
        "start: LD I, dot\n       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "LD I from the font",
        "start: LD V0, 3\n       LD F, V0\n       LD V0, 0\n       DRW V0, V1, 5\n"
        "       HALT",
        "draws",
        also=("LD F",),
    ),
    Row(
        "CHIP8",
        "LD BCD of a register",
        "start: LD V0, 205\n       LD I, buf\n       LD B, V0\n       LD V0, 0\n"
        "       LD I, buf\n       DRW V0, V1, 1\n       HALT\nbuf: DB 0\n     DB 0\n     DB 0",
        "draws",
        also=("LD B",),
    ),
    Row(
        "CHIP8",
        "LD a block through I",
        "start: LD V0, 0xFF\n       LD I, buf\n       LD [I], V0\n       LD I, buf\n"
        "       DRW V0, V1, 1\n       HALT\nbuf: DB 0",
        "draws",
        also=("LD [I], V0", "LD V0, [I]"),
    ),
    Row(
        "CHIP8",
        "LD the delay timer",
        "start: LD V0, 30\n       LD DT, V0\n       LD V0, 0\n       LD I, dot\n"
        "       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
        also=("LD DT, V0", "LD V0, DT"),
    ),
    Row(
        "CHIP8",
        "LD from the keypad",
        "start: LD V0, K\n       LD I, dot\n       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
        also=("LD V0, K",),
    ),
    Row(
        "CHIP8",
        "ADD",
        "start: LD V0, 3\n       ADD V0, 1\n       LD I, dot\n       DRW V0, V1, 1\n"
        "       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "ADD two registers",
        "start: LD V0, 2\n       LD V1, 3\n       ADD V0, V1\n       LD I, dot\n"
        "       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "AND",
        "start: LD V0, 3\n       LD V1, 1\n       AND V0, V1\n       LD I, dot\n"
        "       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "OR",
        "start: LD V0, 4\n       LD V1, 3\n       OR V0, V1\n       LD I, dot\n"
        "       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "XOR",
        "start: LD V0, 6\n       LD V1, 3\n       XOR V0, V1\n       LD I, dot\n"
        "       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "SUB",
        "start: LD V0, 5\n       LD V1, 2\n       SUB V0, V1\n       LD I, dot\n"
        "       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "SUBN",
        "start: LD V0, 2\n       LD V1, 5\n       SUBN V0, V1\n       LD I, dot\n"
        "       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "SHR",
        "start: LD V0, 6\n       SHR V0\n       LD I, dot\n       DRW V0, V1, 1\n"
        "       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "SHL",
        "start: LD V0, 3\n       SHL V0\n       LD I, dot\n       DRW V0, V1, 1\n"
        "       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "RND",
        "start: RND V0, 0x0F\n       LD V1, 0\n       LD I, dot\n       DRW V0, V1, 1\n"
        "       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "DRW",
        "start: LD I, dot\n       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "DB data for a sprite",
        "start: LD I, spr\n       DRW V0, V1, 2\n       HALT\nspr: DB 0xFF\n     DB 0x81",
        "draws",
    ),
    Row(
        "CHIP8",
        "DW data for a sprite",
        "start: LD I, word\n       DRW V0, V1, 2\n       HALT\nword: DW 0xFF81",
        "draws",
    ),
    Row(
        "CHIP8",
        "JP",
        "start: JP draw\n       HALT\ndraw:  LD I, dot\n       DRW V0, V1, 1\n"
        "       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "JP with an offset in V0",
        "start: LD V0, 2\n       JP V0, draw\n       HALT\ndraw: LD I, dot\n"
        "       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
        also=("JP V0, addr",),
    ),
    Row(
        "CHIP8",
        "CALL and RET",
        "start: CALL draw\n       HALT\ndraw:  LD I, dot\n       DRW V0, V1, 1\n"
        "       RET\ndot: DB 0x80",
        "draws",
        also=("RET",),
    ),
    Row(
        "CHIP8",
        "SE skips when equal",
        "start: LD V0, 1\n       SE V0, 1\n       HALT\n       LD I, dot\n"
        "       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "SNE skips when different",
        "start: LD V0, 1\n       SNE V0, 2\n       HALT\n       LD I, dot\n"
        "       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "; keys presents a held key",
        "; keys 3\nstart: LD V0, 3\n       SKP V0\n       HALT\n       LD I, dot\n"
        "       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
        also=("SKP", "; keys"),
    ),
    Row(
        "CHIP8",
        "SKNP skips when the key is up",
        "start: LD V0, 3\n       SKNP V0\n       HALT\n       LD I, dot\n"
        "       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "HALT",
        "start: LD I, dot\n       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
    ),
    Row(
        "CHIP8",
        "SYS is ignored",
        "start: SYS 0x123\n       LD I, dot\n       DRW V0, V1, 1\n       HALT\ndot: DB 0x80",
        "draws",
        also=("SYS",),
    ),
    Row("CHIP8", "error: unknown mnemonic", "start: FROB V0", "error"),
    Row("CHIP8", "error: byte out of range", "start: LD V0, 999", "error"),
    Row("CHIP8", "error: unknown opcode", "start: DW 0xF0FF", "error"),
]


APL: list[Row] = [
    Row("APL", "IOTA", "IOTA 5", "prints:1 2 3 4 5"),
    Row("APL", "RHO reshape", "2 3 RHO IOTA 6", "prints:1 2 3"),
    Row("APL", "RHO shape", "RHO 2 3 RHO IOTA 6", "prints:2 3", also=("RHO",)),
    Row("APL", "TIMES", "3 TIMES 4", "prints:12"),
    Row("APL", "DIVIDE", "6 DIVIDE 2", "prints:3"),
    Row("APL", "POWER", "2 POWER 10", "prints:1024"),
    Row("APL", "CEILING", "CEILING 2.2", "prints:3"),
    Row("APL", "FLOOR", "FLOOR 2.8", "prints:2"),
    Row("APL", "MAX", "3 MAX 5", "prints:5"),
    Row("APL", "MIN", "3 MIN 5", "prints:3"),
    Row("APL", "ABS", "ABS -3", "prints:3", also=("BAR",)),
    Row("APL", "RESIDUE", "3 RESIDUE 10", "prints:1"),
    Row("APL", "SIGNUM", "SIGNUM -3", "prints:\u00af1"),
    Row("APL", "RECIPROCAL", "RECIPROCAL 4", "prints:0.25"),
    Row("APL", "AND", "1 AND 1", "prints:1"),
    Row("APL", "OR", "0 OR 1", "prints:1"),
    Row("APL", "NAND", "1 NAND 1", "prints:0"),
    Row("APL", "NOR", "0 NOR 0", "prints:1"),
    Row("APL", "NOT", "NOT 0", "prints:1"),
    Row("APL", "REVERSE", "REVERSE 1 2 3", "prints:3 2 1"),
    Row("APL", "ROTATE", "1 ROTATE 1 2 3", "prints:2 3 1"),
    Row("APL", "TAKE", "3 TAKE 1 2", "prints:1 2 0"),
    Row("APL", "DROP", "1 DROP 1 2 3", "prints:2 3"),
    Row("APL", "CATENATE", "1 2 CATENATE 3", "prints:1 2 3"),
    Row("APL", "RAVEL", "RAVEL 2 2 RHO IOTA 4", "prints:1 2 3 4"),
    Row("APL", "TRANSPOSE", "TRANSPOSE 2 3 RHO IOTA 6", "prints:1 4"),
    Row("APL", "GRADEUP", "GRADEUP 3 1 2", "prints:2 3 1"),
    Row("APL", "GRADEDOWN", "GRADEDOWN 1 3 2", "prints:2 3 1"),
    Row("APL", "MEMBERSHIP", "1 MEMBERSHIP 1 2 3", "prints:1 0 0"),
    Row("APL", "FORMAT", "FORMAT 42", "prints:42"),
    Row("APL", "INDEXOF", "1 2 3 INDEXOF 2", "prints:2"),
    Row("APL", "REDUCE", "+REDUCE IOTA 5", "prints:15", also=("+/",)),
    Row("APL", "SCAN", "+SCAN IOTA 3", "prints:1 3 6", also=("+\\",)),
    Row("APL", "SHAPE", "RHO 1 2 3", "prints:3"),
    Row("APL", "RESHAPE", "3 RHO 1", "prints:1 1 1"),
    Row(
        "APL",
        "a defined function",
        "∇ R ← DOUBLE X\n  R ← X TIMES 2\n∇\nDOUBLE 21",
        "prints:42",
    ),
    Row("APL", "turtle: FD", "FD 100", "draws", also=("FORWARD",)),
    Row("APL", "turtle: BK", "BK 50", "draws", also=("BACK",)),
    Row("APL", "turtle: RT", "RT 90", "draws", also=("RIGHT",)),
    Row("APL", "turtle: LT", "LT 90", "draws", also=("LEFT",)),
    Row("APL", "turtle: PU and PD", "PU\nPD", "any", also=("PENUP", "PENDOWN")),
    Row("APL", "turtle: HOME", "HOME", "any"),
    Row("APL", "error: mismatched lengths", "1 2 + 4 5 6", "error"),
    Row("APL", "error: unknown variable", "NOSUCHNAME", "error"),
    Row("APL", "error: unmatched parenthesis", "(IOTA 3", "error"),
]


SMALLTALK: list[Row] = [
    Row(
        "Smalltalk",
        "subclass:instanceVariableNames: defines a class",
        "Object subclass: #Thing\n  instanceVariableNames: ''\n\nThing >> greet\n  ^'hi'\n\nThing new greet",
        "prints:hi",
    ),
    Row(
        "Smalltalk",
        "new",
        "Object subclass: #Bare\n  instanceVariableNames: ''\n\nBare >> hello\n  ^'made'\n\nBare new hello",
        "prints:made",
    ),
    Row(
        "Smalltalk",
        "initialize",
        "Object subclass: #Seeded\n  instanceVariableNames: 'seed'\n\n"
        "Seeded >> initialize\n  seed := 7\n\nSeeded >> seed\n  ^seed\n\nSeeded new seed",
        "prints:7",
    ),
    Row(
        "Smalltalk",
        "an instance variable through messages",
        "Object subclass: #Counter\n  instanceVariableNames: 'count'\n\n"
        "Counter >> initialize\n  count := 0\n\nCounter >> bump\n  count := count + 1\n\n"
        "Counter >> value\n  ^count\n\n| c |\nc := Counter new.\nc bump.\nc bump.\nc value",
        "prints:2",
    ),
    Row(
        "Smalltalk",
        "a block and its closure",
        "| total |\ntotal := 10.\n[:n | total := total + n] value: 5.\ntotal",
        "prints:15",
    ),
    Row(
        "Smalltalk",
        "a block controls the loop",
        "| s |\ns := 0.\n(1 to: 4) do: [:i | s := s + i].\ns",
        "prints:10",
    ),
    Row(
        "Smalltalk",
        "show: writes through the Transcript",
        "Transcript show: 'hello'; cr",
        "prints:hello",
    ),
    Row(
        "Smalltalk",
        "cr ends a transcript line",
        "Transcript show: 'a'; cr. Transcript show: 'b'; cr",
        "prints:a",
        also=("show:",),
    ),
    Row(
        "Smalltalk",
        "turtle: forward:",
        "| t |\nt := Turtle new.\nt forward: 100",
        "draws",
        also=("forward:",),
    ),
    Row(
        "Smalltalk",
        "turtle: back:",
        "| t |\nt := Turtle new.\nt back: 40",
        "draws",
        also=("back:",),
    ),
    Row(
        "Smalltalk",
        "turtle: right:",
        "| t |\nt := Turtle new.\nt right: 90",
        "draws",
        also=("right:",),
    ),
    Row(
        "Smalltalk",
        "turtle: left:",
        "| t |\nt := Turtle new.\nt left: 90",
        "draws",
        also=("left:",),
    ),
    Row(
        "Smalltalk",
        "turtle: penUp",
        "| t |\nt := Turtle new.\nt forward: 50.\nt penUp.\nt forward: 50",
        "draws",
        also=("penDown",),
    ),
    Row(
        "Smalltalk",
        "turtle: home",
        "| t |\nt := Turtle new.\nt forward: 50.\nt home",
        "any",
    ),
    Row("Smalltalk", "turtle: clear", "| t |\nt := Turtle new.\nt clear", "any"),
    Row(
        "Smalltalk",
        "super sends above the defining class",
        "Object subclass: #Base\n  instanceVariableNames: ''\n\nBase >> talk\n  ^'base'\n\n"
        "Base subclass: #Kid\n  instanceVariableNames: ''\n\nKid >> talk\n  ^'kid and ', super talk\n\n"
        "Kid new talk",
        "prints:kid and base",
        also=("super",),
    ),
    Row(
        "Smalltalk",
        "a cascade sends to the original receiver",
        "Transcript show: 'a'; show: 'b'; cr",
        "prints:ab",
    ),
    Row("Smalltalk", "error: does not understand", "3 foo", "error"),
    Row("Smalltalk", "error: unknown variable", "noSuchThing", "error"),
    Row("Smalltalk", "error: division by zero", "1 / 0", "error"),
    Row("Smalltalk", "error: index out of range", "#(1 2) at: 9", "error"),
]


ROWS: list[Row] = (
    BASIC + LOGO + PILOT + COMAL + REXX + SCHEME + FOCAL + CHIP8 + APL + SMALLTALK
)


#: Implemented commands that cannot be exercised by this harness.
WAIVERS: list[Waiver] = [
    Waiver(
        "BASIC",
        "INKEY$",
        "polls a keyboard buffer that only an interactive session fills",
    ),
    Waiver(
        "Scheme",
        "turtle shapes that the bridge reports as unsupported",
        "bezier curves, gradient fills and sprites have no CanvasCommand; the bridge "
        "reports them in 'unsupported' rather than dropping them, which "
        "python_bridge_tests asserts",
    ),
]
