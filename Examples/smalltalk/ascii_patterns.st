"ASCII ART PATTERNS — Smalltalk Language Showcase"
"Pascal triangle, number spirals, fractals, diamonds, checkerboards"
"Time Warp Studio — Smalltalk Language Demo"

| ascii |

"============================================================"
"Helper: print a line of characters"

"============================================================"
"SECTION 1: PASCAL'S TRIANGLE"
"============================================================"

Transcript showCr: '============================================================'.
Transcript showCr: '  ASCII ART PATTERNS — Smalltalk Showcase'.
Transcript showCr: '  Pascal triangle * Spirals * Diamonds * Checkerboards'.
Transcript showCr: '============================================================'.
Transcript nl.

Transcript showCr: 'SECTION 1: PASCAL''S TRIANGLE (15 rows)'.
Transcript showCr: '------------------------------------------------------------'.

| rows maxWidth row val indent rowStr |
rows := 15.
maxWidth := 80.

"Compute binomial coefficients row by row"
| prevRow currRow |
prevRow := OrderedCollection with: 1.

1 to: rows do: [:r |
    "Print row with centering indent"
    rowStr := ''.
    prevRow do: [:v |
        rowStr := rowStr, v printString, ' '.
    ].
    indent := (maxWidth - rowStr size) // 2.
    Transcript showCr: (String new: indent withAll: $ ), rowStr.

    "Compute next row"
    currRow := OrderedCollection new.
    currRow add: 1.
    1 to: prevRow size - 1 do: [:i |
        currRow add: (prevRow at: i) + (prevRow at: i + 1).
    ].
    currRow add: 1.
    prevRow := currRow.
].

Transcript nl.

"============================================================"
"SECTION 2: NUMBER SPIRAL"
"============================================================"

Transcript showCr: 'SECTION 2: NUMBER SPIRAL (7x7)'.
Transcript showCr: '------------------------------------------------------------'.

| n grid dx dy x y val stepCount stepLen layer |
n := 7.
grid := Array new: n * n withAll: 0.

x := n // 2.
y := n // 2.
dx := 1. dy := 0.
val := 1.
stepLen := 1.
stepCount := 0.
layer := 0.

"Fill grid in spiral order"
1 to: n * n do: [:i |
    grid at: (y * n + x + 1) put: val.
    val := val + 1.
    x := x + dx.
    y := y + dy.
    stepCount := stepCount + 1.
    (stepCount = stepLen) ifTrue: [
        "Turn left"
        | tmp |
        tmp := dx.
        dx := dy negated.
        dy := tmp.
        stepCount := 0.
        layer := layer + 1.
        (layer \\ 2 = 0) ifTrue: [ stepLen := stepLen + 1 ].
    ].
].

"Print the spiral"
0 to: n - 1 do: [:row |
    | lineStr |
    lineStr := '  '.
    0 to: n - 1 do: [:col |
        | cellVal |
        cellVal := grid at: (row * n + col + 1).
        lineStr := lineStr, (cellVal printString paddedWith: $ to: 4).
    ].
    Transcript showCr: lineStr.
].

Transcript nl.

"============================================================"
"SECTION 3: DIAMOND PATTERN"
"============================================================"

Transcript showCr: 'SECTION 3: DIAMOND PATTERNS'.
Transcript showCr: '------------------------------------------------------------'.

| size half |
Transcript showCr: '  Size 9:'.

size := 9.
half := size // 2.

0 to: half do: [:row |
    | spaces stars line |
    spaces := half - row.
    stars := row * 2 + 1.
    line := (String new: spaces withAll: $ ), (String new: stars withAll: $*).
    Transcript showCr: '  ', line.
].

half - 1 to: 0 by: -1 do: [:row |
    | spaces stars line |
    spaces := half - row.
    stars := row * 2 + 1.
    line := (String new: spaces withAll: $ ), (String new: stars withAll: $*).
    Transcript showCr: '  ', line.
].

Transcript nl.

"Size 13 filled diamond with numbers"
Transcript showCr: '  Size 13 (numbered):'.
size := 13.
half := size // 2.

0 to: half do: [:row |
    | spaces line ch |
    spaces := half - row.
    line := (String new: spaces withAll: $ ).
    ch := (row \\ 10) digitValue asString.
    1 to: row * 2 + 1 do: [:i |
        line := line, ((i = 1 or: [i = row * 2 + 1]) ifTrue: [ch] ifFalse: [' ']).
    ].
    Transcript showCr: '  ', line.
].
half - 1 to: 0 by: -1 do: [:row |
    | spaces line ch |
    spaces := half - row.
    line := (String new: spaces withAll: $ ).
    ch := (row \\ 10) digitValue asString.
    1 to: row * 2 + 1 do: [:i |
        line := line, ((i = 1 or: [i = row * 2 + 1]) ifTrue: [ch] ifFalse: [' ']).
    ].
    Transcript showCr: '  ', line.
].

Transcript nl.

"============================================================"
"SECTION 4: CHECKERBOARD"
"============================================================"

Transcript showCr: 'SECTION 4: CHECKERBOARD (8x8)'.
Transcript showCr: '------------------------------------------------------------'.

0 to: 7 do: [:row |
    | line |
    line := '  '.
    0 to: 7 do: [:col |
        ((row + col) \\ 2 = 0)
            ifTrue:  [ line := line, '##' ]
            ifFalse: [ line := line, '  ' ].
    ].
    Transcript showCr: line.
].

Transcript nl.

"============================================================"
"SECTION 5: ASCII SIERPINSKI TRIANGLE"
"============================================================"

Transcript showCr: 'SECTION 5: ASCII SIERPINSKI TRIANGLE (via Pascal mod 2)'.
Transcript showCr: '------------------------------------------------------------'.

| sRows sPrev sCurr bits |
sRows := 16.
sPrev := OrderedCollection with: 1.

1 to: sRows do: [:r |
    | line indent |
    line := ''.
    sPrev do: [:v |
        line := line, ((v \\ 2 = 1) ifTrue: ['* '] ifFalse: ['  ']).
    ].
    indent := sRows - r.
    Transcript showCr: (String new: indent withAll: $ ), line.

    sCurr := OrderedCollection new.
    sCurr add: 1.
    1 to: sPrev size - 1 do: [:i |
        sCurr add: (sPrev at: i) + (sPrev at: i + 1).
    ].
    sCurr add: 1.
    sPrev := sCurr.
].

Transcript nl.

"============================================================"
"SECTION 6: MULTIPLICATION TABLE"
"============================================================"

Transcript showCr: 'SECTION 6: MULTIPLICATION TABLE (10x10)'.
Transcript showCr: '------------------------------------------------------------'.

| header separator row col |
header := '     '.
1 to: 10 do: [:i | header := header, (i printString paddedWith: $ to: 4) ].
Transcript showCr: '  ', header.
separator := '  ' , (String new: 45 withAll: $-).
Transcript showCr: separator.

1 to: 10 do: [:r |
    | line |
    line := '  ', r printString, '  | '.
    1 to: 10 do: [:c |
        line := line, ((r * c) printString paddedWith: $ to: 4).
    ].
    Transcript showCr: line.
].

Transcript nl.

"============================================================"
"SECTION 7: BOX DRAWING"
"============================================================"

Transcript showCr: 'SECTION 7: NESTED BOXES'.
Transcript showCr: '------------------------------------------------------------'.

1 to: 5 do: [:level |
    | w h top bottom side |
    w := 3 + (level - 1) * 4.
    h := 3 + (level - 1) * 2.
    top    := '  ' , (String new: level * 2 withAll: $ ),
              '+' , (String new: w withAll: $-) , '+'.
    bottom := top copy.
    side   := '  ' , (String new: level * 2 withAll: $ ),
              '|' , (String new: w withAll: $ ) , '|'.
    Transcript showCr: top.
    1 to: h - 2 do: [:_ | Transcript showCr: side ].
    Transcript showCr: bottom.
].

Transcript nl.
Transcript showCr: '============================================================'.
Transcript showCr: '  ASCII Art Patterns complete!'.
Transcript showCr: '  Pascal * Spiral * Diamond * Checker * Sierpinski * Tables'.
Transcript showCr: '============================================================'.
