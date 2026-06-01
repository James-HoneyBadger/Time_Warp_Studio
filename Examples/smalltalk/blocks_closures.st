"Blocks and Closures in Smalltalk"

"-- Basic blocks --"
Transcript showCr: '=== Blocks (Closures) ==='.

| greet |
greet := [:name | 'Hello, ' , name , '!'].
Transcript showCr: (greet value: 'World').
Transcript showCr: (greet value: 'Smalltalk').

"-- Blocks as first-class objects --"
Transcript showCr: ''.
Transcript showCr: '=== Blocks as Values ==='.

| add multiply compose |
add      := [:a :b | a + b].
multiply := [:a :b | a * b].
compose  := [:f :g :x | f value: (g value: x)].

Transcript showCr: 'add(3, 5) = '       , (add value: 3 value: 5) printString.
Transcript showCr: 'multiply(4, 6) = '  , (multiply value: 4 value: 6) printString.

| double inc double_then_inc |
double         := [:x | x * 2].
inc            := [:x | x + 1].
double_then_inc := [:x | compose value: inc value: double value: x].
Transcript showCr: 'double_then_inc(5) = ' , (double_then_inc value: 5) printString.

"-- Blocks with collection protocol --"
Transcript showCr: ''.
Transcript showCr: '=== Collection Operations with Blocks ==='.

| nums evens squares sum |
nums    := #(1 2 3 4 5 6 7 8 9 10).
evens   := nums select: [:x | x even].
squares := nums collect: [:x | x * x].
sum     := nums inject: 0 into: [:acc :x | acc + x].

Transcript showCr: 'Numbers:  ' , nums printString.
Transcript showCr: 'Evens:    ' , evens printString.
Transcript showCr: 'Squares:  ' , squares printString.
Transcript showCr: 'Sum:      ' , sum printString.

"-- Recursion via blocks --"
Transcript showCr: ''.
Transcript showCr: '=== Recursive Block (Factorial) ==='.

| fib |
fib := [:n |
    n <= 1
        ifTrue: [n]
        ifFalse: [(fib value: n - 1) + (fib value: n - 2)]
].

0 to: 10 do: [:i |
    Transcript show: 'fib(' , i printString , ') = '.
    Transcript showCr: (fib value: i) printString.
].
