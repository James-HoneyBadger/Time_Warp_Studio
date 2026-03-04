"""
Code snippets library for Time Warp Studio.
Provides insertable code templates for common patterns in all supported languages.
"""

from dataclasses import dataclass
from typing import Dict, List, Optional


@dataclass
class CodeSnippet:
    """A code snippet template."""

    name: str
    description: str
    code: str
    language: str  # BASIC, PILOT, LOGO, or ALL
    category: str
    cursor_position: Optional[int] = None  # Where to place cursor after insert


# BASIC Snippets
BASIC_SNIPPETS: List[CodeSnippet] = [
    CodeSnippet(
        name="Hello World",
        description="Simple Hello World program",
        code='10 PRINT "Hello, World!"\n20 END',
        language="BASIC",
        category="Basics",
    ),
    CodeSnippet(
        name="FOR Loop",
        description="Basic FOR...NEXT loop",
        code="10 FOR I = 1 TO 10\n20   PRINT I\n30 NEXT I",
        language="BASIC",
        category="Control Flow",
    ),
    CodeSnippet(
        name="WHILE Loop",
        description="WHILE...WEND loop",
        code=(
            "10 X = 1\n"
            "20 WHILE X <= 10\n"
            "30   PRINT X\n"
            "40   X = X + 1\n"
            "50 WEND"
        ),
        language="BASIC",
        category="Control Flow",
    ),
    CodeSnippet(
        name="DO...LOOP UNTIL",
        description="DO...LOOP with UNTIL condition",
        code=(
            "10 X = 1\n"
            "20 DO\n"
            "30   PRINT X\n"
            "40   X = X + 1\n"
            "50 LOOP UNTIL X > 10"
        ),
        language="BASIC",
        category="Control Flow",
    ),
    CodeSnippet(
        name="IF...THEN...ELSE",
        description="Conditional statement",
        code=(
            '10 INPUT "Enter a number: "; N\n'
            '20 IF N > 0 THEN PRINT "Positive"\n'
            '30 IF N < 0 THEN PRINT "Negative"\n'
            '40 IF N = 0 THEN PRINT "Zero"'
        ),
        language="BASIC",
        category="Control Flow",
    ),
    CodeSnippet(
        name="Subroutine",
        description="GOSUB subroutine pattern",
        code=(
            '10 PRINT "Start"\n'
            "20 GOSUB 100\n"
            '30 PRINT "End"\n'
            "40 END\n"
            "100 REM Subroutine\n"
            '110 PRINT "In subroutine"\n'
            "120 RETURN"
        ),
        language="BASIC",
        category="Subroutines",
    ),
    CodeSnippet(
        name="Input Example",
        description="Get user input",
        code=('10 INPUT "What is your name? "; NAME$\n' '20 PRINT "Hello, "; NAME$'),
        language="BASIC",
        category="Input/Output",
    ),
    CodeSnippet(
        name="Array Usage",
        description="Declare and use an array",
        code=(
            "10 DIM A(10)\n"
            "20 FOR I = 0 TO 10\n"
            "30   A(I) = I * 2\n"
            "40 NEXT I\n"
            "50 FOR I = 0 TO 10\n"
            "60   PRINT A(I)\n"
            "70 NEXT I"
        ),
        language="BASIC",
        category="Data Structures",
    ),
    CodeSnippet(
        name="DATA/READ",
        description="Use DATA and READ statements",
        code=(
            '10 DATA 10, 20, 30, "Hello", "World"\n'
            "20 FOR I = 1 TO 3\n"
            "30   READ N\n"
            "40   PRINT N\n"
            "50 NEXT I\n"
            "60 READ A$, B$\n"
            '70 PRINT A$; " "; B$'
        ),
        language="BASIC",
        category="Data Structures",
    ),
    CodeSnippet(
        name="Random Number",
        description="Generate random numbers",
        code=(
            "10 FOR I = 1 TO 5\n"
            "20   R = INT(RND * 100) + 1\n"
            "30   PRINT R\n"
            "40 NEXT I"
        ),
        language="BASIC",
        category="Math",
    ),
    CodeSnippet(
        name="Line Drawing",
        description="Draw lines on screen",
        code=(
            "10 CLS\n"
            "20 LINE (0, 0)-(100, 100)\n"
            "30 LINE (100, 0)-(0, 100)\n"
            "40 LINE (50, 0)-(50, 100)"
        ),
        language="BASIC",
        category="Graphics",
    ),
    CodeSnippet(
        name="Circle Drawing",
        description="Draw circles on screen",
        code=(
            "10 CLS\n"
            "20 CIRCLE (0, 0), 50\n"
            "30 CIRCLE (0, 0), 75\n"
            "40 CIRCLE (0, 0), 100"
        ),
        language="BASIC",
        category="Graphics",
    ),
    CodeSnippet(
        name="Game Loop (INKEY$)",
        description="Basic game loop with keyboard input",
        code=(
            "10 CLS\n"
            "20 X = 160\n"
            "30 Y = 100\n"
            "40 REM Game Loop\n"
            "50 K$ = INKEY$\n"
            '60 IF K$ = "w" THEN Y = Y - 5\n'
            '70 IF K$ = "s" THEN Y = Y + 5\n'
            '80 IF K$ = "a" THEN X = X - 5\n'
            '90 IF K$ = "d" THEN X = X + 5\n'
            '100 IF K$ = "q" THEN END\n'
            "110 CIRCLE (X, Y), 10\n"
            "120 GOTO 50"
        ),
        language="BASIC",
        category="Games",
    ),
    CodeSnippet(
        name="Timer Example",
        description="Using TIMER for timing",
        code=(
            "10 T = TIMER\n"
            '20 PRINT "Press any key..."\n'
            '30 K$ = ""\n'
            '40 WHILE K$ = ""\n'
            "50   K$ = INKEY$\n"
            "60 WEND\n"
            '70 PRINT "Elapsed: "; TIMER - T; " seconds"'
        ),
        language="BASIC",
        category="Games",
    ),
    CodeSnippet(
        name="Sound Effect",
        description="Play sound effects",
        code=(
            "10 BEEP\n" "20 SOUND 440, 10\n" "30 SOUND 880, 10\n" "40 SOUND 1320, 10"
        ),
        language="BASIC",
        category="Sound",
    ),
]

# PILOT Snippets
PILOT_SNIPPETS: List[CodeSnippet] = [
    CodeSnippet(
        name="Hello World",
        description="Simple Hello World in PILOT",
        code="T:Hello, World!\nE:",
        language="PILOT",
        category="Basics",
    ),
    CodeSnippet(
        name="Question/Answer",
        description="Simple quiz pattern",
        code=(
            "T:What is 2 + 2?\n"
            "A:\n"
            "M:4,four\n"
            "TY:Correct!\n"
            "TN:The answer is 4.\n"
            "E:"
        ),
        language="PILOT",
        category="Basics",
    ),
    CodeSnippet(
        name="Multiple Choice Quiz",
        description="Multiple choice question",
        code=(
            "T:What color is the sky?\n"
            "T:A) Red  B) Blue  C) Green\n"
            "A:\n"
            "M:B,b,blue\n"
            "TY:Correct! The sky is blue.\n"
            "TN:Sorry, the answer is B) Blue.\n"
            "E:"
        ),
        language="PILOT",
        category="Education",
    ),
    CodeSnippet(
        name="Loop with Counter",
        description="Loop using compute and jump",
        code=(
            "C:COUNT=0\n"
            "*LOOP\n"
            "C:COUNT=COUNT+1\n"
            "T:Count is *COUNT*\n"
            "J(COUNT<5):*LOOP\n"
            "T:Done counting!\n"
            "E:"
        ),
        language="PILOT",
        category="Control Flow",
    ),
    CodeSnippet(
        name="Math Quiz",
        description="Simple addition quiz",
        code=(
            "C:A=5\n"
            "C:B=3\n"
            "T:What is *A* + *B*?\n"
            "A:\n"
            "M:8,eight\n"
            "TY:Correct! *A* + *B* = 8\n"
            "TN:Sorry, *A* + *B* = 8\n"
            "E:"
        ),
        language="PILOT",
        category="Education",
    ),
    CodeSnippet(
        name="Subroutine Pattern",
        description="Using subroutine calls",
        code=(
            "T:Starting program\n"
            "S:*GREET\n"
            "T:Back to main\n"
            "E:\n"
            "*GREET\n"
            "T:Hello from subroutine!\n"
            "R:"
        ),
        language="PILOT",
        category="Subroutines",
    ),
]

# Logo Snippets
LOGO_SNIPPETS: List[CodeSnippet] = [
    CodeSnippet(
        name="Square",
        description="Draw a square",
        code="REPEAT 4 [FD 100 RT 90]",
        language="LOGO",
        category="Shapes",
    ),
    CodeSnippet(
        name="Triangle",
        description="Draw an equilateral triangle",
        code="REPEAT 3 [FD 100 RT 120]",
        language="LOGO",
        category="Shapes",
    ),
    CodeSnippet(
        name="Circle",
        description="Draw a circle using small steps",
        code="REPEAT 360 [FD 1 RT 1]",
        language="LOGO",
        category="Shapes",
    ),
    CodeSnippet(
        name="Star",
        description="Draw a 5-pointed star",
        code="REPEAT 5 [FD 100 RT 144]",
        language="LOGO",
        category="Shapes",
    ),
    CodeSnippet(
        name="Spiral Square",
        description="Draw a square spiral",
        code=(
            "TO SPIRAL :SIZE\n"
            "  IF :SIZE > 200 [STOP]\n"
            "  FD :SIZE RT 90\n"
            "  SPIRAL :SIZE + 5\n"
            "END\n"
            "SPIRAL 5"
        ),
        language="LOGO",
        category="Patterns",
    ),
    CodeSnippet(
        name="Recursive Tree",
        description="Draw a recursive tree",
        code=(
            "TO TREE :SIZE\n"
            "  IF :SIZE < 5 [STOP]\n"
            "  FD :SIZE\n"
            "  LT 30 TREE :SIZE * 0.7\n"
            "  RT 60 TREE :SIZE * 0.7\n"
            "  LT 30 BK :SIZE\n"
            "END\n"
            "TREE 50"
        ),
        language="LOGO",
        category="Patterns",
    ),
    CodeSnippet(
        name="Colorful Pattern",
        description="Draw with changing colors",
        code="REPEAT 36 [\n  SETPC [255 0 0]\n  FD 100\n  RT 170\n]",
        language="LOGO",
        category="Patterns",
    ),
    CodeSnippet(
        name="Procedure Example",
        description="Define and use a procedure",
        code=(
            "TO HEXAGON :SIZE\n"
            "  REPEAT 6 [FD :SIZE RT 60]\n"
            "END\n"
            "HEXAGON 50\n"
            "RT 60\n"
            "HEXAGON 50"
        ),
        language="LOGO",
        category="Procedures",
    ),
    CodeSnippet(
        name="Flower Pattern",
        description="Draw a flower using circles",
        code="REPEAT 36 [\n  REPEAT 360 [FD 0.5 RT 1]\n  RT 10\n]",
        language="LOGO",
        category="Patterns",
    ),
]


def _s(name: str, desc: str, code: str, lang: str, cat: str) -> CodeSnippet:
    """Shorthand constructor for CodeSnippet."""
    return CodeSnippet(
        name=name, description=desc, code=code, language=lang, category=cat
    )


_C = "C"
_FORTH = "FORTH"

_C_SNIPPETS: List[CodeSnippet] = [
    _s(
        "Hello World",
        "Print Hello World",
        '#include <stdio.h>\nint main() {\n    printf("Hello, World!\\n");\n    return 0;\n}\n',
        _C,
        "Basics",
    ),
    _s(
        "FOR loop",
        "Standard for loop",
        '#include <stdio.h>\nint main() {\n    for (int i = 0; i < 10; i++) {\n        printf("%d\\n", i);\n    }\n    return 0;\n}\n',
        _C,
        "Control Flow",
    ),
    _s(
        "Function",
        "Define and call",
        '#include <stdio.h>\nint square(int n) { return n * n; }\nint main() {\n    printf("%d\\n", square(7));\n    return 0;\n}\n',
        _C,
        "Functions",
    ),
    _s(
        "Array",
        "Declare and iterate",
        '#include <stdio.h>\nint main() {\n    int arr[] = {1,2,3,4,5};\n    for (int i = 0; i < 5; i++)\n        printf("%d ", arr[i]);\n    printf("\\n");\n    return 0;\n}\n',
        _C,
        "Data",
    ),
    _s(
        "String copy",
        "strcpy / strcmp",
        '#include <stdio.h>\n#include <string.h>\nint main() {\n    char src[] = "Hello";\n    char dst[20];\n    strcpy(dst, src);\n    printf("%s\\n", dst);\n    return 0;\n}\n',
        _C,
        "Strings",
    ),
    _s(
        "Struct",
        "Define a struct",
        '#include <stdio.h>\ntypedef struct { int x; int y; } Point;\nint main() {\n    Point p = {10, 20};\n    printf("%d %d\\n", p.x, p.y);\n    return 0;\n}\n',
        _C,
        "Data",
    ),
]

_PASCAL_SNIPPETS: List[CodeSnippet] = [
    _s(
        "Hello World",
        "Print Hello World",
        "PROGRAM Hello;\nBEGIN\n  WRITELN('Hello, World!');\nEND.\n",
        "PASCAL",
        "Basics",
    ),
    _s(
        "FOR loop",
        "FOR..DO loop",
        "PROGRAM Counting;\nVAR I: Integer;\nBEGIN\n  FOR I := 1 TO 10 DO\n    WRITELN(I);\nEND.\n",
        "PASCAL",
        "Control Flow",
    ),
    _s(
        "WHILE loop",
        "WHILE..DO",
        "PROGRAM Loop;\nVAR X: Integer;\nBEGIN\n  X := 0;\n  WHILE X < 5 DO BEGIN\n    WRITELN(X);\n    X := X + 1;\n  END;\nEND.\n",
        "PASCAL",
        "Control Flow",
    ),
    _s(
        "Procedure",
        "Procedure with param",
        "PROGRAM Procs;\nPROCEDURE Greet(Name: String);\nBEGIN\n  WRITELN('Hello, ', Name);\nEND;\nBEGIN\n  Greet('World');\nEND.\n",
        "PASCAL",
        "Functions",
    ),
    _s(
        "Function",
        "Function returning val",
        "PROGRAM Funcs;\nFUNCTION Square(N: Integer): Integer;\nBEGIN\n  Square := N * N;\nEND;\nBEGIN\n  WRITELN(Square(7));\nEND.\n",
        "PASCAL",
        "Functions",
    ),
    _s(
        "RECORD type",
        "Record with fields",
        "PROGRAM Recs;\nTYPE TPoint = RECORD\n  X: Integer;\n  Y: Integer;\nEND;\nVAR P: TPoint;\nBEGIN\n  P.X := 10;\n  P.Y := 20;\n  WRITELN(P.X, ' ', P.Y);\nEND.\n",
        "PASCAL",
        "Data",
    ),
]

_PROLOG_SNIPPETS: List[CodeSnippet] = [
    _s(
        "Facts & query",
        "Parent/ancestor KB",
        "parent(tom, bob).\nparent(bob, ann).\nancestor(X, Y) :- parent(X, Y).\nancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).\n?- ancestor(tom, ann).\n",
        "PROLOG",
        "Basics",
    ),
    _s("List member", "member/2", "?- member(X, [1,2,3]).\n", "PROLOG", "Lists"),
    _s(
        "Findall",
        "Collect all solutions",
        "color(red). color(green). color(blue).\n?- findall(X, color(X), Colors).\n",
        "PROLOG",
        "Control",
    ),
    _s("Arithmetic", "Arithmetic eval", "?- X is 3 + 4 * 2.\n", "PROLOG", "Basics"),
    _s("Between", "Generate integers", "?- between(1, 5, X).\n", "PROLOG", "Basics"),
    _s(
        "Append",
        "List concatenation",
        "?- append([a,b],[c,d], R).\n",
        "PROLOG",
        "Lists",
    ),
]

_FORTH_SNIPPETS: List[CodeSnippet] = [
    _s(
        "Hello World",
        "Print greeting",
        ': GREETING  ." Hello, World!" CR ;\nGREETING\n',
        _FORTH,
        "Basics",
    ),
    _s(
        "Square word",
        "Double-times word",
        ": SQUARE  DUP * ;\n5 SQUARE .\n",
        _FORTH,
        "Words",
    ),
    _s(
        "Factorial",
        "Recursive factorial",
        ": FACT  DUP 1 <= IF DROP 1 ELSE DUP 1 - RECURSE * THEN ;\n5 FACT .\n",
        _FORTH,
        "Words",
    ),
    _s(
        "DO loop",
        "Counted loop",
        ": COUNTDOWN  11 1 DO I . LOOP ;\nCOUNTDOWN\n",
        _FORTH,
        "Control Flow",
    ),
    _s(
        "Stack ops", "DUP SWAP DROP", "10 20 30\nDUP .\nSWAP .\nDROP\n", _FORTH, "Stack"
    ),
    _s(
        "DEFER / IS",
        "Deferred execution",
        'DEFER ACTION\n: HELLO  ." Hello" CR ;\n: GOODBYE  ." Goodbye" CR ;\n\' HELLO IS ACTION\nACTION\n\' GOODBYE IS ACTION\nACTION\n',
        _FORTH,
        "Advanced",
    ),
]

_HASKELL_SNIPPETS: List[CodeSnippet] = [
    _s(
        "Hello World",
        "IO putStrLn",
        'main :: IO ()\nmain = putStrLn "Hello, World!"\n',
        "HASKELL",
        "Basics",
    ),
    _s(
        "Recursive factorial",
        "Pattern matching",
        "factorial :: Int -> Int\nfactorial 0 = 1\nfactorial n = n * factorial (n - 1)\n\nmain :: IO ()\nmain = print (factorial 10)\n",
        "HASKELL",
        "Recursion",
    ),
    _s(
        "Map & filter",
        "List operations",
        "main :: IO ()\nmain = do\n  let nums = [1..10]\n  print $ map (*2) $ filter even nums\n",
        "HASKELL",
        "Lists",
    ),
    _s(
        "List comprehension",
        "Guards",
        "main :: IO ()\nmain = print [x^2 | x <- [1..5]]\n",
        "HASKELL",
        "Lists",
    ),
    _s(
        "where clause",
        "Local binding",
        "hypotenuse :: Double -> Double -> Double\nhypotenuse a b = sqrt s\n  where\n    s = a*a + b*b\n\nmain :: IO ()\nmain = print (hypotenuse 3 4)\n",
        "HASKELL",
        "Functions",
    ),
]

_JS_SNIPPETS: List[CodeSnippet] = [
    _s(
        "Hello World",
        "console.log",
        'console.log("Hello, World!");\n',
        "JAVASCRIPT",
        "Basics",
    ),
    _s(
        "Arrow function",
        "Arrow fn syntax",
        "const square = n => n * n;\nconsole.log(square(7));\n",
        "JAVASCRIPT",
        "Functions",
    ),
    _s(
        "Array methods",
        "filter + map",
        "const nums = [1,2,3,4,5];\nconst result = nums.filter(n => n % 2 === 0).map(n => n * 10);\nconsole.log(result);\n",
        "JAVASCRIPT",
        "Arrays",
    ),
    _s(
        "Destructuring",
        "Array destructure",
        "const [a, b, c] = [10, 20, 30];\nconsole.log(a + b + c);\n",
        "JAVASCRIPT",
        "Syntax",
    ),
    _s(
        "Template literal",
        "String interpolation",
        'const name = "World";\nconsole.log(`Hello, ${name}!`);\n',
        "JAVASCRIPT",
        "Strings",
    ),
    _s(
        "Object literal",
        "Key-value pairs",
        'const person = { name: "Alice", age: 30 };\nconsole.log(person.name, person.age);\n',
        "JAVASCRIPT",
        "Objects",
    ),
]

_PYTHON_SNIPPETS: List[CodeSnippet] = [
    _s(
        "Hello World", "print statement", 'print("Hello, World!")\n', "PYTHON", "Basics"
    ),
    _s(
        "FOR loop",
        "range iteration",
        "for i in range(10):\n    print(i)\n",
        "PYTHON",
        "Control Flow",
    ),
    _s(
        "List comprehension",
        "Compact list",
        "squares = [x**2 for x in range(1, 6)]\nprint(squares)\n",
        "PYTHON",
        "Lists",
    ),
    _s(
        "Function",
        "def and return",
        'def greet(name):\n    return f"Hello, {name}!"\n\nprint(greet("World"))\n',
        "PYTHON",
        "Functions",
    ),
    _s(
        "Class",
        "OOP class",
        'class Dog:\n    def __init__(self, name):\n        self.name = name\n    def bark(self):\n        print(f"{self.name} says woof!")\n\ndog = Dog("Rex")\ndog.bark()\n',
        "PYTHON",
        "OOP",
    ),
    _s(
        "Try/except",
        "Error handling",
        'try:\n    x = 1 / 0\nexcept ZeroDivisionError as e:\n    print(f"Error: {e}")\n',
        "PYTHON",
        "Error Handling",
    ),
]

_SCHEME_SNIPPETS: List[CodeSnippet] = [
    _s(
        "Hello World",
        "display + newline",
        '(display "Hello, World!") (newline)\n',
        "SCHEME",
        "Basics",
    ),
    _s(
        "Factorial",
        "Recursive define",
        "(define (factorial n)\n  (if (<= n 1) 1\n      (* n (factorial (- n 1)))))\n(display (factorial 10)) (newline)\n",
        "SCHEME",
        "Recursion",
    ),
    _s(
        "Map",
        "List mapping",
        "(display (map (lambda (x) (* x x)) '(1 2 3 4 5)))\n(newline)\n",
        "SCHEME",
        "Lists",
    ),
    _s(
        "Let binding",
        "Local variables",
        "(let ((x 10) (y 20))\n  (display (+ x y))\n  (newline))\n",
        "SCHEME",
        "Basics",
    ),
    _s(
        "Cond",
        "Multi-branch cond",
        '(define (sign n)\n  (cond ((> n 0) "positive")\n        ((< n 0) "negative")\n        (else "zero")))\n(display (sign -5)) (newline)\n',
        "SCHEME",
        "Control Flow",
    ),
]

_LUA_SNIPPETS: List[CodeSnippet] = [
    _s("Hello World", "print statement", 'print("Hello, World!")\n', "LUA", "Basics"),
    _s(
        "FOR loop",
        "Numeric for",
        "for i = 1, 10 do\n  print(i)\nend\n",
        "LUA",
        "Control Flow",
    ),
    _s(
        "Function",
        "Function definition",
        "function square(n)\n  return n * n\nend\nprint(square(7))\n",
        "LUA",
        "Functions",
    ),
    _s(
        "Table",
        "Array-like table",
        "local t = {10, 20, 30, 40, 50}\nfor i, v in ipairs(t) do\n  print(i, v)\nend\n",
        "LUA",
        "Data",
    ),
    _s(
        "String ops",
        "String methods",
        'local s = "Hello, World!"\nprint(#s)\nprint(string.upper(s))\nprint(string.sub(s, 1, 5))\n',
        "LUA",
        "Strings",
    ),
    _s(
        "Class table",
        "OOP via tables",
        'Animal = {}\nAnimal.__index = Animal\nfunction Animal.new(name)\n  return setmetatable({name=name}, Animal)\nend\nfunction Animal:speak()\n  print(self.name .. " makes a sound")\nend\nlocal a = Animal.new("Dog")\na:speak()\n',
        "LUA",
        "OOP",
    ),
]

_COBOL_SNIPPETS: List[CodeSnippet] = [
    _s(
        "Hello World",
        "DISPLAY statement",
        "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.\nPROCEDURE DIVISION.\n    DISPLAY 'Hello, World!'.\n    STOP RUN.\n",
        "COBOL",
        "Basics",
    ),
    _s(
        "Arithmetic",
        "COMPUTE verb",
        "IDENTIFICATION DIVISION.\nPROGRAM-ID. ARITH.\nDATA DIVISION.\nWORKING-STORAGE SECTION.\n  01 A PIC 9(4) VALUE 100.\n  01 B PIC 9(4) VALUE 250.\n  01 C PIC 9(4).\nPROCEDURE DIVISION.\n  COMPUTE C = A + B.\n  DISPLAY C.\n  STOP RUN.\n",
        "COBOL",
        "Arithmetic",
    ),
    _s(
        "PERFORM loop",
        "PERFORM VARYING",
        "IDENTIFICATION DIVISION.\nPROGRAM-ID. LOOP.\nDATA DIVISION.\nWORKING-STORAGE SECTION.\n  01 IDX PIC 9(2).\nPROCEDURE DIVISION.\n  PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5\n    DISPLAY IDX\n  END-PERFORM.\n  STOP RUN.\n",
        "COBOL",
        "Control Flow",
    ),
]

_BRAINFUCK_SNIPPETS: List[CodeSnippet] = [
    _s(
        "Hello World",
        "Classic BF hello",
        "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.\n",
        "BRAINFUCK",
        "Classics",
    ),
]

_FORTRAN_SNIPPETS: List[CodeSnippet] = [
    _s(
        "Hello World",
        "PRINT statement",
        "PROGRAM hello\n  IMPLICIT NONE\n  PRINT *, 'Hello, World!'\nEND PROGRAM hello\n",
        "FORTRAN",
        "Basics",
    ),
    _s(
        "DO loop",
        "Counted DO loop",
        "PROGRAM counting\n  IMPLICIT NONE\n  INTEGER :: i\n  DO i = 1, 10\n    PRINT *, i\n  END DO\nEND PROGRAM counting\n",
        "FORTRAN",
        "Control Flow",
    ),
    _s(
        "Function",
        "FUNCTION definition",
        "PROGRAM funcs\n  IMPLICIT NONE\n  INTEGER :: n\n  n = 7\n  PRINT *, n * n\nEND PROGRAM funcs\n",
        "FORTRAN",
        "Functions",
    ),
    _s(
        "Array",
        "Array declaration",
        "PROGRAM arrays\n  IMPLICIT NONE\n  INTEGER, DIMENSION(5) :: arr\n  INTEGER :: i\n  DO i = 1, 5\n    arr(i) = i * i\n    PRINT *, arr(i)\n  END DO\nEND PROGRAM arrays\n",
        "FORTRAN",
        "Data",
    ),
]

_REXX_SNIPPETS: List[CodeSnippet] = [
    _s("Hello World", "SAY statement", "SAY 'Hello, World!'\n", "REXX", "Basics"),
    _s(
        "DO loop",
        "DO..END loop",
        "DO i = 1 TO 10\n  SAY i\nEND\n",
        "REXX",
        "Control Flow",
    ),
    _s(
        "IF statement",
        "IF..THEN..ELSE",
        "x = 5\nIF x > 3 THEN\n  SAY 'big'\nELSE\n  SAY 'small'\nEND\n",
        "REXX",
        "Control Flow",
    ),
]

_SMALLTALK_SNIPPETS: List[CodeSnippet] = [
    _s(
        "Hello World",
        "Transcript show",
        "Transcript show: 'Hello, World!'; nl.\n",
        "SMALLTALK",
        "Basics",
    ),
    _s(
        "FOR loop",
        "timesRepeat",
        "1 to: 10 do: [:i | Transcript show: i printString; nl].\n",
        "SMALLTALK",
        "Control Flow",
    ),
]

_HYPERTALK_SNIPPETS: List[CodeSnippet] = [
    _s("Hello World", "put statement", 'put "Hello, World!"\n', "HYPERTALK", "Basics"),
    _s(
        "IF statement",
        "if..then..else",
        'if x > 3 then\n  put "big"\nelse\n  put "small"\nend if\n',
        "HYPERTALK",
        "Control Flow",
    ),
]

_SQL_SNIPPETS: List[CodeSnippet] = [
    _s("SELECT all", "SELECT * FROM", "SELECT * FROM employees;\n", "SQL", "Queries"),
    _s(
        "SELECT WHERE",
        "Filtered query",
        "SELECT first_name, last_name FROM employees WHERE department = 'HR';\n",
        "SQL",
        "Queries",
    ),
    _s(
        "CREATE TABLE",
        "Table definition",
        "CREATE TABLE employees (\n    id INT PRIMARY KEY,\n    first_name VARCHAR(50),\n    last_name VARCHAR(50),\n    department VARCHAR(50)\n);\n",
        "SQL",
        "DDL",
    ),
    _s(
        "INSERT",
        "Insert row",
        "INSERT INTO employees (id, first_name, last_name, department)\nVALUES (1, 'Alice', 'Smith', 'Engineering');\n",
        "SQL",
        "DML",
    ),
    _s(
        "UPDATE",
        "Update rows",
        "UPDATE employees SET department = 'Management' WHERE id = 1;\n",
        "SQL",
        "DML",
    ),
    _s(
        "DELETE",
        "Delete rows",
        "DELETE FROM employees WHERE department = 'Temp';\n",
        "SQL",
        "DML",
    ),
    _s(
        "JOIN",
        "INNER JOIN",
        "SELECT e.first_name, d.name\nFROM employees e\nINNER JOIN departments d ON e.department_id = d.id;\n",
        "SQL",
        "Queries",
    ),
]


class SnippetLibrary:
    """Manager for code snippets."""

    def __init__(self):
        self.snippets: Dict[str, List[CodeSnippet]] = {
            "BASIC": BASIC_SNIPPETS.copy(),
            "PILOT": PILOT_SNIPPETS.copy(),
            "LOGO": LOGO_SNIPPETS.copy(),
            "PYTHON": _PYTHON_SNIPPETS.copy(),
            "LUA": _LUA_SNIPPETS.copy(),
            "SCHEME": _SCHEME_SNIPPETS.copy(),
            "COBOL": _COBOL_SNIPPETS.copy(),
            "BRAINFUCK": _BRAINFUCK_SNIPPETS.copy(),
            "ASSEMBLY": [],
            "JAVASCRIPT": _JS_SNIPPETS.copy(),
            "FORTRAN": _FORTRAN_SNIPPETS.copy(),
            "REXX": _REXX_SNIPPETS.copy(),
            "SMALLTALK": _SMALLTALK_SNIPPETS.copy(),
            "HYPERTALK": _HYPERTALK_SNIPPETS.copy(),
            "HASKELL": _HASKELL_SNIPPETS.copy(),
            "APL": [],
            "SQL": _SQL_SNIPPETS.copy(),
            "JCL": [],
            "CICS": [],
            "PASCAL": _PASCAL_SNIPPETS.copy(),
            "C": _C_SNIPPETS.copy(),
            "PROLOG": _PROLOG_SNIPPETS.copy(),
            "FORTH": _FORTH_SNIPPETS.copy(),
        }

    def get_snippets(self, language: str) -> List[CodeSnippet]:
        """Get all snippets for a language."""
        return self.snippets.get(language.upper(), [])

    def get_categories(self, language: str) -> List[str]:
        """Get all categories for a language."""
        snippets = self.get_snippets(language)
        categories = sorted(set(s.category for s in snippets))
        return categories

    def get_by_category(
        self,
        language: str,
        category: str,
    ) -> List[CodeSnippet]:
        """Get snippets filtered by category."""
        snippets = self.get_snippets(language)
        return [s for s in snippets if s.category == category]

    def search(self, language: str, query: str) -> List[CodeSnippet]:
        """Search snippets by name or description."""
        snippets = self.get_snippets(language)
        query_lower = query.lower()
        return [
            s
            for s in snippets
            if query_lower in s.name.lower() or query_lower in s.description.lower()
        ]

    def add_snippet(self, snippet: CodeSnippet):
        """Add a custom snippet."""
        lang = snippet.language.upper()
        if lang not in self.snippets:
            self.snippets[lang] = []
        self.snippets[lang].append(snippet)


# Global singleton
_snippet_library: Optional[SnippetLibrary] = None


def get_snippet_library() -> SnippetLibrary:
    """Get or create the global snippet library."""
    global _snippet_library  # pylint: disable=global-statement
    if _snippet_library is None:
        _snippet_library = SnippetLibrary()
    return _snippet_library
