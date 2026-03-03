"=============================================
  Smalltalk Comprehensive Demo - Time Warp Studio
 =============================================".

" --- Hello World --- "
Transcript showCr: '===== HELLO WORLD ====='.
Transcript showCr: 'Welcome to Smalltalk!'.
Transcript showCr: ''.

" --- Variables and Arithmetic --- "
Transcript showCr: '===== VARIABLES & MATH ====='.
| x y |
x := 10.
y := 3.
Transcript showCr: x.
Transcript showCr: y.
Transcript showCr: x + y.
Transcript showCr: x - y.
Transcript showCr: x * y.
Transcript showCr: x / y.
Transcript showCr: x \\ y.
Transcript showCr: ''.

" --- Number Messages --- "
Transcript showCr: '===== NUMBER OPS ====='.
Transcript showCr: -5 abs.
Transcript showCr: 16 sqrt.
Transcript showCr: 5 factorial.
Transcript showCr: (3 max: 7).
Transcript showCr: (3 min: 7).
Transcript showCr: 7 isPrime.
Transcript showCr: ''.

" --- Strings --- "
Transcript showCr: '===== STRINGS ====='.
Transcript showCr: 'Hello' size.
Transcript showCr: 'Hello' reversed.
Transcript showCr: 'hello' asUppercase.
Transcript showCr: 'HELLO' asLowercase.
Transcript showCr: 'Hello', ' ', 'World'.
Transcript showCr: ''.

" --- Conditionals --- "
Transcript showCr: '===== CONDITIONALS ====='.
x > 5 ifTrue: [Transcript showCr: 'x is greater than 5'].
y = 3 ifTrue: [Transcript showCr: 'y equals 3']
      ifFalse: [Transcript showCr: 'y does not equal 3'].
Transcript showCr: ''.

" --- Blocks --- "
Transcript showCr: '===== BLOCKS ====='.
| double |
double := [:n | n * 2].
Transcript showCr: (double value: 5).
Transcript showCr: (double value: 21).
Transcript showCr: ''.

" --- Loops --- "
Transcript showCr: '===== LOOPS ====='.
1 to: 5 do: [:i | Transcript showCr: i].
Transcript showCr: ''.

Transcript showCr: '===== TIMES REPEAT ====='.
3 timesRepeat: [Transcript showCr: 'Hello'].
Transcript showCr: ''.

" --- Collections --- "
Transcript showCr: '===== COLLECTIONS ====='.
#(10 20 30 40 50) do: [:each | Transcript showCr: each].
Transcript showCr: ''.

Transcript showCr: '===== DONE ====='.
