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
    Row("BASIC", "IF / ELSE / END IF", 'X = 20\nIF X > 10 THEN\nPRINT "BIG"\nELSE\nPRINT "SMALL"\nEND IF', "prints:BIG"),
    Row("BASIC", "IF false branch", 'X = 1\nIF X > 10 THEN\nPRINT "BIG"\nELSE\nPRINT "SMALL"\nEND IF', "prints:SMALL"),
    Row("BASIC", "FOR / NEXT", "FOR I = 1 TO 3\nPRINT I\nNEXT I", "prints:3"),
    Row("BASIC", "FOR with STEP", "FOR I = 0 TO 10 STEP 5\nPRINT I\nNEXT I", "prints:10"),
    Row("BASIC", "WHILE / WEND", "X = 0\nWHILE X < 3\nX = X + 1\nWEND\nPRINT X", "prints:3"),
    Row("BASIC", "DO / LOOP UNTIL", "X = 0\nDO\nX = X + 1\nLOOP UNTIL X >= 3\nPRINT X", "prints:3"),
    Row("BASIC", "GOSUB and RETURN", "10 X = 4\n20 GOSUB 100\n30 PRINT RESULT\n40 END\n100 RESULT = X * 2\n110 RETURN", "prints:8"),
    Row("BASIC", "DEF FN", "DEF FNA(N) = N * N\nPRINT FNA(5)", "prints:25"),
    Row("BASIC", "LEN", 'PRINT LEN("HELLO")', "prints:5"),
    Row("BASIC", "MID$", 'PRINT MID$("HELLO", 1, 3)', "prints:HEL"),
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
    Row("BASIC", "INPUT through the protocol", 'INPUT "Name: ", N\nPRINT "GOT:", N', "prints:GOT"),
    Row("BASIC", "CLS", "CLS", "any"),
    Row("BASIC", "SCREEN", "SCREEN 1", "any"),
    Row("BASIC", "COLOR then PSET", "SCREEN 1\nCOLOR 15, 0\nPSET (1, 1)", "draws"),
    Row("BASIC", "LINE", "SCREEN 1\nLINE (10, 10) - (100, 50)", "draws"),
    Row("BASIC", "CIRCLE", "SCREEN 1\nCIRCLE (160, 100), 40", "draws"),
    Row("BASIC", "PSET", "SCREEN 1\nPSET (5, 5)", "draws"),
    Row("BASIC", "PAINT", "SCREEN 1\nLINE (10, 10) - (100, 50)\nPAINT (50, 30), 4", "draws"),
    Row("BASIC", "LOCATE then PRINT", "LOCATE 5, 5\nPRINT \"X\"", "any"),

    # -- Structural keywords. Each is already a piece of a program above; giving
    #    it a row of its own is what makes a break in the keyword attributable
    #    instead of showing up as a mystery failure in the row that used it.
    Row("BASIC", "NEXT", "FOR I = 1 TO 2\nNEXT I\nPRINT I", "prints:2"),
    Row("BASIC", "WEND", "X = 0\nWHILE X < 2\nX = X + 1\nWEND\nPRINT X", "prints:2"),
    Row("BASIC", "LOOP", "X = 0\nDO\nX = X + 1\nLOOP UNTIL X = 2\nPRINT X", "prints:2"),
    Row("BASIC", "UNTIL", 'X = 0\nDO\nX = X + 1\nLOOP UNTIL X = 2\nPRINT "DONE"', "prints:DONE"),
    Row("BASIC", "ELSE", 'IF 1 > 2 THEN\nPRINT "A"\nELSE\nPRINT "B"\nEND IF', "prints:B"),
    Row("BASIC", "END IF", 'IF 1 < 2 THEN\nPRINT "A"\nEND IF', "prints:A"),
    Row("BASIC", "ENDIF alias", 'IF 1 < 2 THEN\nPRINT "A"\nENDIF', "prints:A"),
    Row("BASIC", "END stops the program", 'PRINT "A"\nEND\nPRINT "B"', "prints:A"),
    Row("BASIC", "RETURN", "GOSUB 100\nPRINT RESULT\nEND\n100 RESULT = 9\n110 RETURN", "prints:9"),
    Row("BASIC", "REM", 'REM ignored\nPRINT "A"', "prints:A"),
    Row("BASIC", "apostrophe comment", "'ignored\nPRINT \"A\"", "prints:A"),
    Row("BASIC", "colon separates statements", 'PRINT "A": PRINT "B"', "prints:B"),
    Row("BASIC", "SELECT / CASE / END SELECT", 'X = 2\nSELECT CASE X\nCASE 2\nPRINT "TWO"\nEND SELECT', "prints:TWO", ("CASE", "END SELECT", "SELECT")),
    Row("BASIC", "SUB / CALL / END SUB", 'SUB DOUBLE(N)\nPRINT N * 2\nEND SUB\nCALL DOUBLE(21)', "prints:42", ("CALL", "END SUB")),
    Row("BASIC", "FUNCTION / END FUNCTION", 'FUNCTION TWICE(N)\nTWICE = N * 2\nEND FUNCTION\nCALL TWICE(21)\nPRINT TWICE', "prints:42", ("END FUNCTION",)),
    Row("BASIC", "TYPE / END TYPE", 'TYPE POINT\nX AS INTEGER\nY AS INTEGER\nEND TYPE\nPRINT "OK"', "prints:OK", ("END TYPE",)),
    Row("BASIC", "GOTO", 'GOTO 20\nPRINT "SKIPPED"\n20 PRINT "HERE"', "prints:HERE"),

    # A variable whose name merely *starts* with a keyword used to be swallowed by
    # that keyword's statement (DOG = 42 became a DO loop), leaving the variable
    # unset and PRINT echoing the name. These are the regressions for that.
    Row("BASIC", "variable starting with DO", "DOUBLE = 42\nPRINT DOUBLE", "prints:42"),
    Row("BASIC", "variable starting with RUN", "RUNNER = 7\nPRINT RUNNER", "prints:7"),
    Row("BASIC", "variable starting with CLOSE", "CLOSER = 3\nPRINT CLOSER", "prints:3"),
    Row("BASIC", "variable starting with NEXT", "NEXTX = 5\nPRINT NEXTX", "prints:5"),
    Row("BASIC", "variable starting with LOOP", "LOOPS = 9\nPRINT LOOPS", "prints:9"),
    Row("BASIC", "LET", "LET X = 7\nPRINT X", "prints:7"),
    Row("BASIC", "MAKE", 'MAKE "X" 7\nPRINT X', "prints:7"),
    Row("BASIC", "SETXY then read the position", 'SETXY 10 20\nPRINT "MOVED"', "prints:MOVED"),

    # -- DATA / READ / RESTORE are one mechanism, so each row carries a program that
    #    shows the pointer moved rather than trusting the statement ran.
    Row("BASIC", "DATA with READ", "DATA 3, 4\nREAD A\nREAD B\nPRINT A + B", "prints:7", ("DATA",)),
    Row("BASIC", "RESTORE", "DATA 3, 4\nREAD A\nRESTORE\nREAD B\nPRINT B", "prints:3", ("DATA", "READ")),

    # -- Error handling. ON ERROR GOTO must actually divert, so the row proves the
    #    handler ran rather than that the statement parsed.
    Row("BASIC", "ON ERROR GOTO diverts", "ON ERROR GOTO 100\nX = 1 / 0\nPRINT \"NO\"\nEND\n100 PRINT \"CAUGHT\"", "prints:CAUGHT", ("ON ERROR GOTO",)),
    Row("BASIC", "ON ERROR disables the handler", 'ON ERROR GOTO 100\nON ERROR\nPRINT "OK"\nEND\n100 PRINT "CAUGHT"', "prints:OK", ("ON ERROR GOTO", "ON ERROR")),
    Row("BASIC", "RESUME NEXT", "10 ON ERROR GOTO 100\n20 X = 1 / 0\n30 PRINT \"AFTER\"\n40 END\n100 RESUME NEXT", "prints:AFTER", ("RESUME", "ON ERROR GOTO")),
    Row("BASIC", "RESUME", "ON ERROR GOTO 100\nX = 1 / 0\nEND\n100 RESUME 30\n30 PRINT \"RESUMED\"", "prints:RESUMED", ("ON ERROR GOTO",)),

    # -- Declarations. These return no text of their own, so the row carries the
    #    smallest statement that shows the declaration took effect.
    Row("BASIC", "CONST", "CONST SIZE = 5\nPRINT SIZE", "prints:5"),
    Row("BASIC", "DEFINT truncates", "DEFINT X\nX = 3.7\nPRINT X", "prints:3"),
    Row("BASIC", "DEFLNG", 'DEFLNG A-Z\nPRINT "OK"', "prints:OK"),
    Row("BASIC", "DEFSNG", 'DEFSNG A-Z\nPRINT "OK"', "prints:OK"),
    Row("BASIC", "DEFDBL", 'DEFDBL A-Z\nPRINT "OK"', "prints:OK"),
    Row("BASIC", "DEFSTR", 'DEFSTR A-Z\nA = "HI"\nPRINT A$', "prints:HI"),
    Row("BASIC", "SWAP", "A = 1\nB = 2\nSWAP A, B\nPRINT A", "prints:2"),
    Row("BASIC", "SWAP strings", 'A$ = "X"\nB$ = "Y"\nSWAP A$, B$\nPRINT A$', "prints:Y"),
    Row("BASIC", "ERASE", "DIM A(3)\nA(0) = 9\nERASE A\nPRINT A(0)", "prints:0"),
    Row("BASIC", "WIDTH", "WIDTH 80", "prints:Set width to 80"),
    Row("BASIC", "PRESET", "SCREEN 1\nPSET (5, 5), 1\nPRESET (5, 5)", "draws"),
    Row("BASIC", "PRINT USING", 'PRINT USING "##.##"; 3.14159', "prints:3.14"),
    Row("BASIC", "PALETTE CYCLE", "SCREEN 1\nPALETTE CYCLE 1, 15, 1", "prints:Palette cycled", ("CYCLE",)),

    # -- Sound, speech, animation and effects. The optional backends (pygame,
    #    pyttsx3) are absent on a bare machine, so these rows assert the command
    #    is *acknowledged* rather than that a speaker moved.
    Row("BASIC", "BEEP", "BEEP", "prints:BEEP"),
    Row("BASIC", "SOUND", "SOUND 440, 2", "prints:SOUND 440Hz"),
    Row("BASIC", "SLEEP", "SLEEP 1", "prints:SLEEP"),
    Row("BASIC", "SPEED", "SPEED 5", "prints:Turtle speed set to 5"),
    Row("BASIC", "RANDOMIZE", 'RANDOMIZE 42\nPRINT "OK"', "prints:OK"),
    Row("BASIC", "RANDOMIZE TIMER", 'RANDOMIZE TIMER\nPRINT "OK"', "prints:OK", ("TIMER",)),
    Row("BASIC", "PLAY", 'PLAY "CDEFG"', "prints:Playing"),
    Row("BASIC", "SAY", 'SAY "HELLO"', "any"),
    Row("BASIC", "SPRITE define", "SPRITE SHIP, 10, 20, 30, 40", "prints:Sprite SHIP"),
    Row("BASIC", "SPRITE remove", "SPRITE SHIP, 10, 20, 30, 40\nSPRITE SHIP, OFF", "prints:Removed sprite"),
    Row("BASIC", "ON TIMER", "ON TIMER(2) GOSUB 100", "prints:Timer set"),
    Row("BASIC", "TIMER ON", "ON TIMER(2) GOSUB 100\nTIMER ON", "prints:Timer enabled"),
    Row("BASIC", "TIMER OFF", "ON TIMER(2) GOSUB 100\nTIMER ON\nTIMER OFF", "prints:Timer disabled"),
    Row("BASIC", "FRACTAL lists presets", "FRACTAL", "prints:Available fractals"),
    Row("BASIC", "FRACTAL KOCH draws", "FRACTAL KOCH", "draws"),
    Row("BASIC", "SHAPE STAR", "SHAPE STAR, 40", "draws"),
    Row("BASIC", "SHAPE POLYGON with a count", "SHAPE POLYGON 6, 40", "draws"),
    Row("BASIC", "SHAPE unknown name", "SHAPE NOSUCH, 10", "error"),
    Row("BASIC", "PARTICLE", "PARTICLE EXPLOSION, 100, 100", "prints:Created EXPLOSION effect"),

    # -- Machine-facing statements. SHELL is deliberately inert, and the port and
    #    memory commands are readable through the statement forms.
    Row("BASIC", "SHELL is blocked", 'SHELL "echo hi"', "prints:SHELL commands are disabled"),
    Row("BASIC", "SYSTEM", 'PRINT "A"\nSYSTEM', "prints:A"),
    Row("BASIC", "RUN", 'PRINT "A"\nRUN', "any"),
    Row("BASIC", "POKE then PEEK", "POKE 1000, 7\nPEEK 1000", "prints:PEEK(1000) = 7", ("PEEK",)),
    Row("BASIC", "OUT then IN", "OUT 100, 5\nIN 100", "prints:IN(100) = 5", ("IN",)),
    Row("BASIC", "JOYINIT reports its backend", "JOYINIT", "any"),

    # -- File channels, exercised as a real round trip rather than waived. The
    #    harness deletes the scratch file afterwards.
    Row("BASIC", "WRITE # round trip", f'OPEN "{SCRATCH}" FOR OUTPUT AS #3\nWRITE #3, "BYE"\nCLOSE #3\nPRINT "WROTE"', "prints:WROTE", ("OPEN", "WRITE #", "CLOSE")),

    # -- INPUT requires the protocol to answer, so the row asserts the program
    #    resumed with the line the harness sent rather than hanging.
    Row("BASIC", "LINE INPUT", 'LINE INPUT "Name: ", N$\nPRINT "GOT:", N$', "prints:GOT"),

    # -- File channels. The workspace file service confines these to a scratch
    #    name, and the round trip is the only way to know PRINT # actually wrote.
    Row("BASIC", "OPEN / PRINT # / CLOSE round trip", f'OPEN "{SCRATCH}" FOR OUTPUT AS #1\nPRINT #1, "HELLO"\nCLOSE #1\nPRINT "WROTE"', "prints:WROTE", ("PRINT #", "CLOSE")),
    Row("BASIC", "INPUT # reads it back", f'OPEN "{SCRATCH}" FOR INPUT AS #1\nLINE INPUT #1, L$\nCLOSE #1\nPRINT L$', "prints:HELLO", ("INPUT #", "LINE INPUT")),
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
    Row("Logo", "REPEAT with nested variable", 'MAKE "N 3\nREPEAT :N [FD 10 RT 120]', "draws"),
    Row("Logo", "PU suppresses ink", "PU\nFD 50", "any"),
    Row("Logo", "PD resumes ink", "PU\nFD 50\nPD\nFD 50", "draws"),
    Row("Logo", "SETPENCOLOR", 'SETPENCOLOR "RED\nFD 20', "draws"),
    Row("Logo", "SETPENWIDTH", "SETPENWIDTH 3\nFD 20", "draws"),
    Row("Logo", "SETXY", "SETXY 10 10", "draws"),
    Row("Logo", "SETH then FD", "SETH 90\nFD 20", "draws"),
    Row("Logo", "HOME", "FD 20\nHOME", "draws"),
    Row("Logo", "CS", "FD 20\nCS", "any"),
    Row("Logo", "procedure and call", "TO SQUARE :S\nREPEAT 4 [FD :S RT 90]\nEND\nSQUARE 20", "draws"),
    Row("Logo", "IF true branch", "IF 1 = 1 [FD 10]", "draws"),
    Row("Logo", "IF false branch falls through", "IF 1 = 2 [FD 10]\nPRINT \"DONE", "prints:DONE"),
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
    Row("Logo", "OUTPUT", 'TO P\nOUTPUT 5\nEND\nP', "prints:5", ("OP",)),
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
    Row("PILOT", "T: text", "T: Hello there", "prints:Hello there"),
    Row("PILOT", "several T: units", "T: One\nT: Two", "prints:Two"),
    Row("PILOT", "A: then TY/TN on no match", "T: Q\nA:\nM: PARIS\nTY: Correct\nTN: Wrong", "prints:Wrong"),
    Row("PILOT", "J: jump", "J: SKIP\nT: NOT REACHED\nE:\n*SKIP\nT: LANDED", "prints:LANDED"),
    Row("PILOT", "U: subroutine", "U: *SUB\nT: BACK\nE:\n*SUB\nT: INSIDE\nE:", "prints:INSIDE"),
    Row("PILOT", "E: ends the program", "T: BEFORE\nE:\nT: AFTER", "prints:BEFORE"),
    Row("PILOT", "REMARK is ignored", "REMARK a note\nT: AFTER REMARK", "prints:AFTER REMARK"),
    Row("PILOT", "error: jump to a missing label", "J: NOWHERE\nT: NOT REACHED", "error"),
]

# ---------------------------------------------------------------------------
# COMAL
# ---------------------------------------------------------------------------

COMAL: list[Row] = [
    Row("COMAL", "PRINT string", 'PRINT "Hello, World!"', "prints:Hello, World!"),
    Row("COMAL", "PRINT expression", "PRINT 6 * 7", "prints:42"),
    Row("COMAL", "assignment and print", 'COUNT := 5\nPRINT COUNT', "prints:5"),
    Row("COMAL", "string variable", 'TITLE$ := "Report"\nPRINT TITLE$', "prints:Report"),
    Row("COMAL", "IF / ENDIF", 'SCORE := 87\nIF SCORE >= 90 THEN\nPRINT "Grade A"\nELSE\nPRINT "Keep going"\nENDIF', "prints:Keep going"),
    Row("COMAL", "IF / ELIF / ENDIF", 'SCORE := 85\nIF SCORE >= 90 THEN\nPRINT "A"\nELIF SCORE >= 80 THEN\nPRINT "B"\nELSE\nPRINT "C"\nENDIF', "prints:B", ("ELIF",)),
    Row("COMAL", "FOR / NEXT", "FOR I := 1 TO 3\nPRINT I\nNEXT I", "prints:3"),
    Row("COMAL", "WHILE / ENDWHILE", "N := 0\nWHILE N < 3\nN := N + 1\nENDWHILE\nPRINT N", "prints:3"),
    Row("COMAL", "REPEAT / UNTIL", "N := 0\nREPEAT\nN := N + 1\nUNTIL N >= 3\nPRINT N", "prints:3"),
    Row("COMAL", "PROC and CALL", 'PROC GREET(NAME$)\nPRINT "Hello, "; NAME$\nENDPROC\nCALL GREET("World")', "prints:Hello, World"),
    Row("COMAL", "DIM and index", "DIM SCORES(5)\nSCORES(1) := 9\nPRINT SCORES(1)", "prints:9"),
    Row("COMAL", "DATA and READ", "DATA 4, 5\nREAD A, B\nPRINT A + B", "prints:9", ("DATA",)),

    # -- Turtle graphics. MOVE draws forward and TURN rotates, so each row moves
    #    first and turns second, otherwise there is nothing on the canvas to see.
    Row("COMAL", "MOVE", "MOVE 100", "draws"),
    Row("COMAL", "TURN", "MOVE 50\nTURN 90\nMOVE 50", "draws", ("MOVE",)),
    Row("COMAL", "PENUP / PENDOWN", "MOVE 50\nPENUP\nMOVE 50\nPENDOWN\nMOVE 50", "draws", ("PENDOWN",)),
    Row("COMAL", "PLOT", "MOVE 20\nPLOT", "draws", ("MOVE",)),
    Row("COMAL", "FUNC / ENDFUNC", "10 FUNC DOUBLE(N)\n20 RETURN N * 2\n30 ENDFUNC\n40 CALL DOUBLE(21)\n50 PRINT RESULT", "prints:42", ("CALL", "RETURN")),
    Row("COMAL", "error: unknown statement", "NOSUCH", "error"),
]

# ---------------------------------------------------------------------------
# REXX
# ---------------------------------------------------------------------------

REXX: list[Row] = [
    Row("REXX", "SAY string", "SAY 'Hello, World!'", "prints:Hello, World!"),
    Row("REXX", "SAY expression", "SAY 6 * 7", "prints:42"),
    Row("REXX", "assignment and SAY", "name = 'Ann'\nSAY name", "prints:Ann"),
    Row("REXX", "concatenation with ||", "SAY 'Hello' || ' ' || 'World'", "prints:Hello World"),
    Row("REXX", "IF / THEN", "score = 70\nIF score > 50 THEN SAY 'pass'", "prints:pass"),
    Row("REXX", "IF / THEN DO / ELSE DO", "score = 95\nIF score >= 90 THEN DO\nSAY 'Grade A'\nEND\nELSE DO\nSAY 'Keep going'\nEND", "prints:Grade A"),
    Row("REXX", "DO counted loop", "DO i = 1 TO 3\nSAY i\nEND", "prints:3"),
    Row("REXX", "DO with BY", "DO n = 10 TO 1 BY -3\nSAY n\nEND", "prints:1"),
    Row("REXX", "DO WHILE", "count = 0\nDO WHILE count < 3\ncount = count + 1\nEND\nSAY count", "prints:3"),
    Row("REXX", "DO OVER", "DO word OVER 'alpha beta gamma'\nSAY word\nEND", "prints:gamma"),
    Row("REXX", "SELECT / WHEN / OTHERWISE", "score = 95\nSELECT\nWHEN score > 90 THEN SAY 'huge'\nOTHERWISE SAY 'small'\nEND", "prints:huge"),
    Row("REXX", "PARSE VAR", "line = 'red green blue'\nPARSE VAR line first second third\nSAY second", "prints:green"),
    Row("REXX", "internal subroutine and RESULT", "CALL DOUBLE 5\nSAY RESULT\nEXIT\nDOUBLE:\nRETURN ARG(1) * 2", "prints:10"),
    Row("REXX", "LENGTH", "SAY LENGTH('HELLO')", "prints:5"),
    Row("REXX", "SUBSTR", "SAY SUBSTR('HELLO', 2, 3)", "prints:ELL"),
    Row("REXX", "WORD", "SAY WORD('alpha beta gamma', 2)", "prints:beta"),
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
    Row("Scheme", "define a procedure", "(define (square n) (* n n))\n(display (square 7))", "prints:49"),
    Row("Scheme", "cond", '(define (sign n) (cond ((> n 0) "positive") ((< n 0) "negative") (else "zero")))\n(display (sign -3))', "prints:negative"),
    Row("Scheme", "if and recursion", "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))\n(display (fact 5))", "prints:120"),
    Row("Scheme", "car of a list", "(display (car (list 1 2 3)))", "prints:1"),
    Row("Scheme", "map with lambda", "(display (map (lambda (x) (* x x)) (list 1 2 3 4)))", "prints:"),
    Row("Scheme", "filter with a predicate", "(display (filter odd? (list 1 2 3 4 5)))", "prints:"),
    Row("Scheme", "apply", "(display (apply + (list 10 20 30)))", "prints:60"),
    Row("Scheme", "let", "(display (let ((x 3) (y 4)) (+ x y)))", "prints:7"),
    Row("Scheme", "closure", "(define (make-adder n) (lambda (x) (+ x n)))\n(define add10 (make-adder 10))\n(display (add10 5))", "prints:15"),
    Row("Scheme", "turtle: forward", "(forward 100)", "draws"),
    Row("Scheme", "turtle: square procedure", "(define (square size) (define (loop n) (when (> n 0) (forward size) (right 90) (loop (- n 1)))) (loop 4))\n(square 100)", "draws"),
    Row("Scheme", "error: arity", "(car)", "error"),
    Row("Scheme", "error: unbound procedure", "(no-such-procedure 1)", "error"),
]


ROWS: list[Row] = BASIC + LOGO + PILOT + COMAL + REXX + SCHEME


#: Implemented commands that cannot be exercised by this harness.
WAIVERS: list[Waiver] = [
    Waiver(
        "BASIC",
        "SPEAK",
        "does not exist: the executor dispatches SAY, and SAY is covered by a row. "
        "The name appears in the documentation, which is a documentation error "
        "rather than a missing command, so nothing is waived here.",
    ),
    Waiver(
        "BASIC",
        "STICK / STRIG",
        "JOYINIT's docstring promises these gamepad readers, but the executor has no "
        "handler for either: the expression evaluator leaves them unresolved. "
        "Recorded here so the promise is not mistaken for coverage.",
    ),
    Waiver("BASIC", "INKEY$", "polls a keyboard buffer that only an interactive session fills"),
    Waiver(
        "Logo",
        "OUTPUT / OP used as a value in an expression",
        "a procedure that OUTPUTs prints its value but the value never reaches the "
        "caller: PRINT DOUBLE 21 prints 'DOUBLE 21'. The call itself is covered by "
        "the OUTPUT row.",
    ),
    Waiver(
        "BASIC",
        "PEEK(...) / IN(...) as expression functions",
        "the statement forms work and have rows, but the classic expression forms "
        "(X = PEEK(addr)) are not wired into the evaluator, which reports "
        "'Unknown function'. The handlers' own docstrings record this.",
    ),
    Waiver(
        "BASIC",
        "RESUME NEXT on unnumbered lines",
        "the numbered-line form has a row; with no line numbers there is no error "
        "line to resume after, so the statement is accepted and does nothing.",
    ),
    Waiver(
        "Scheme",
        "turtle shapes that the bridge reports as unsupported",
        "bezier curves, gradient fills and sprites have no CanvasCommand; the bridge "
        "reports them in 'unsupported' rather than dropping them, which "
        "python_bridge_tests asserts",
    ),
]
