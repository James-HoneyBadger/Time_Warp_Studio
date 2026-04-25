' ============================================================
'  MATH EXPLORER — A Complete Mathematical Journey in BASIC
'  Mandelbrot Set * Fibonacci * Prime Sieve * Statistics
'  Lissajous Curves drawn on the turtle canvas
'  Time Warp Studio — BASIC Language Showcase
' ============================================================
RANDOMIZE TIMER
CLS
PRINT "============================================================"
PRINT "   MATHEMATICS OF BEAUTY — BASIC LANGUAGE SHOWCASE"
PRINT "   Explore the hidden patterns that rule our universe!"
PRINT "============================================================"
PRINT

' ===== SECTION 1: ASCII MANDELBROT SET =====
PRINT "[ 1 ] THE MANDELBROT SET"
PRINT "      z(n+1) = z(n)^2 + c  — does it diverge?"
PRINT

DIM shade$(7)
shade$(0) = " " : shade$(1) = "." : shade$(2) = ","
shade$(3) = "-" : shade$(4) = "=" : shade$(5) = "*"
shade$(6) = "#"

FOR py = -11 TO 11
  FOR px = -31 TO 18
    cx = px * 0.072
    cy = py * 0.1
    zr = 0 : zi = 0 : n = 0
    WHILE n < 40 AND zr * zr + zi * zi < 4
      tr = zr * zr - zi * zi + cx
      zi = 2 * zr * zi + cy
      zr = tr : n = n + 1
    WEND
    IF n >= 40 THEN
      idx = 6
    ELSE
      idx = INT(n * 6 / 40)
    END IF
    PRINT shade$(idx);
  NEXT px
  PRINT
NEXT py

PRINT
PRINT "  '#' = in the set (never escapes infinity)"
PRINT "  Other chars = escape speed (fast to slow: . , - = * )"
PRINT

' ===== SECTION 2: FIBONACCI & GOLDEN RATIO =====
PRINT "[ 2 ] FIBONACCI SEQUENCE & GOLDEN RATIO"
PRINT "      Nature's favorite numbers — found in shells, galaxies, flowers"
PRINT

DIM fib(28)
fib(0) = 1 : fib(1) = 1
FOR i = 2 TO 27
  fib(i) = fib(i-1) + fib(i-2)
NEXT i

phi = (1 + SQR(5)) / 2

PRINT "  N   | Fibonacci     | Ratio -> phi"
PRINT "  ----|---------------|------------------"
FOR i = 1 TO 20
  ratio = INT(fib(i) * 1000000 / fib(i-1)) / 1000000
  PRINT "  "; i; "   | "; fib(i); "         | "; ratio
NEXT i

PRINT
PRINT "  Converges to phi = "; INT(phi * 1000000) / 1000000
PRINT "  Remarkable: phi^2 = phi + 1 = "; INT(phi * phi * 1000000) / 1000000
PRINT "  And 1/phi = phi - 1 = "; INT((phi - 1) * 1000000) / 1000000
PRINT

' ===== SECTION 3: PRIME SIEVE =====
PRINT "[ 3 ] SIEVE OF ERATOSTHENES"
PRINT "      Finding every prime up to 300"
PRINT

DIM isprime(300)
FOR i = 0 TO 300
  isprime(i) = 1
NEXT i
isprime(0) = 0 : isprime(1) = 0
FOR i = 2 TO 17
  IF isprime(i) = 1 THEN
    j = i * i
    WHILE j <= 300
      isprime(j) = 0
      j = j + i
    WEND
  END IF
NEXT i

pcount = 0 : psum = 0
PRINT "  Primes <= 300:"
PRINT "  ";
FOR i = 2 TO 300
  IF isprime(i) = 1 THEN
    PRINT i; " ";
    pcount = pcount + 1
    psum = psum + i
  END IF
NEXT i
PRINT
PRINT
PRINT "  Count: "; pcount; "   Sum: "; psum
PRINT "  Average prime: "; INT(psum / pcount * 10) / 10
PRINT "  Largest twin primes <= 300: 281 & 283"
PRINT

' ===== SECTION 4: STATISTICAL ANALYSIS =====
PRINT "[ 4 ] STATISTICAL ANALYSIS"
PRINT "      Analyzing 20 data points: mean, median, std dev"
PRINT

DIM dat(19)
dat(0) = 47 : dat(1) = 83 : dat(2) = 12 : dat(3) = 65 : dat(4) = 91
dat(5) = 38 : dat(6) = 74 : dat(7) = 29 : dat(8) = 56 : dat(9) = 88
dat(10) = 43 : dat(11) = 67 : dat(12) = 22 : dat(13) = 81 : dat(14) = 54
dat(15) = 39 : dat(16) = 76 : dat(17) = 95 : dat(18) = 31 : dat(19) = 62
N = 20

' Bubble sort for median and display
FOR pass = 0 TO N - 2
  FOR j = 0 TO N - pass - 2
    IF dat(j) > dat(j+1) THEN
      tmp = dat(j) : dat(j) = dat(j+1) : dat(j+1) = tmp
    END IF
  NEXT j
NEXT pass

total = 0
FOR i = 0 TO N - 1
  total = total + dat(i)
NEXT i
mn = total / N

variance = 0
FOR i = 0 TO N - 1
  d = dat(i) - mn
  variance = variance + d * d
NEXT i
stddev = SQR(variance / N)
median = (dat(9) + dat(10)) / 2

PRINT "  Sorted: ";
FOR i = 0 TO N - 1
  PRINT dat(i); " ";
NEXT i
PRINT
PRINT
PRINT "  Mean:        "; INT(mn * 100) / 100
PRINT "  Median:      "; median
PRINT "  Std Dev:     "; INT(stddev * 100) / 100
PRINT "  Min / Max:   "; dat(0); " / "; dat(N-1)
PRINT "  Range:       "; dat(N-1) - dat(0)
PRINT

' ASCII Bar Chart
PRINT "  Bar Chart:"
FOR i = 0 TO N - 1
  PRINT "  "; dat(i); " | ";
  bars = INT(dat(i) / 3)
  FOR b = 0 TO bars - 1
    PRINT "█";
  NEXT b
  PRINT
NEXT i
PRINT

' ===== SECTION 5: LISSAJOUS CURVES ON CANVAS =====
PRINT "[ 5 ] LISSAJOUS CURVES — Drawing on Turtle Canvas"
PRINT "      Parametric art: x=sin(at+d), y=sin(bt)"
PRINT

CLEARSCREEN
HIDETURTLE
SETPENWIDTH 2

' Curve 1: a=3, b=2 (Cyan) — figure-8 family
SETPENCOLOR "CYAN"
xv = INT(SIN(0) * 140)
yv = INT(SIN(0) * 100)
PENUP
SETXY xv yv
PENDOWN
FOR t = 1 TO 630
  a = t / 100.0
  xv = INT(SIN(3 * a) * 140)
  yv = INT(SIN(2 * a) * 100)
  SETXY xv yv
NEXT t

' Curve 2: a=5, b=4, phase offset (Gold)
SETPENCOLOR "GOLD"
xv = INT(SIN(0.785) * 110)
yv = INT(SIN(0) * 85)
PENUP
SETXY xv yv
PENDOWN
FOR t = 1 TO 630
  a = t / 100.0
  xv = INT(SIN(5 * a + 0.785) * 110)
  yv = INT(SIN(4 * a) * 85)
  SETXY xv yv
NEXT t

' Archimedean Spiral (Magenta)
SETPENCOLOR "MAGENTA"
SETPENWIDTH 1
PENUP
HOME
PENDOWN
FOR t = 1 TO 1260
  a = t / 100.0
  r = a * 4.5
  xv = INT(COS(a) * r)
  yv = INT(SIN(a) * r)
  SETXY xv yv
NEXT t

PENUP
HOME
SHOWTURTLE

PRINT "  Three curves drawn:"
PRINT "    Cyan    - 3:2 Lissajous (130x100, classic trefoil)"
PRINT "    Gold    - 5:4 Lissajous with pi/4 phase (complex knot)"
PRINT "    Magenta - Archimedean spiral r=4.5*theta"
PRINT

PRINT "============================================================"
PRINT "  BASIC Showcase Complete!"
PRINT "  From fractals to spirals — BASIC shows its mathematical soul!"
PRINT "============================================================"
