"Smalltalk Algorithms Demo"

"Factorial"
| factorial |
factorial := [:n |
    | result |
    result := 1.
    1 to: n do: [:i | result := result * i].
    result].

Transcript showCr: '10! = ', (factorial value: 10) printString.

"Fibonacci"
| fib |
fib := [:n |
    | a b temp |
    a := 0. b := 1.
    n timesRepeat: [
        temp := b.
        b := a + b.
        a := temp].
    a].

0 to: 9 do: [:i |
    Transcript show: (fib value: i) printString, ' '].
Transcript showCr: ''.

"Bubble sort"
| arr n swapped temp |
arr := OrderedCollection withAll: #(64 34 25 12 22 11 90).
n := arr size.
[swapped := false.
 1 to: n - 1 do: [:i |
     (arr at: i) > (arr at: i + 1) ifTrue: [
         temp := arr at: i.
         arr at: i put: (arr at: i + 1).
         arr at: i + 1 put: temp.
         swapped := true]].
 swapped] whileTrue.
Transcript showCr: 'Sorted: ', arr printString.
