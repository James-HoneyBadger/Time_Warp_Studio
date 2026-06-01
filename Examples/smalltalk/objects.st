"Smalltalk Objects Demo"

"Simulating object-like behaviour with closures"
| makeCounter counter1 counter2 |

"A counter 'object' as a block closure"
makeCounter := [:start |
    | count |
    count := start.
    [:msg |
        msg = #increment ifTrue: [count := count + 1].
        msg = #decrement ifTrue: [count := count - 1].
        msg = #reset    ifTrue: [count := start].
        msg = #value    ifTrue: [count]]].

counter1 := makeCounter value: 0.
counter2 := makeCounter value: 10.

counter1 value: #increment.
counter1 value: #increment.
counter1 value: #increment.
counter2 value: #decrement.

Transcript showCr: 'Counter1: ', (counter1 value: #value) printString.
Transcript showCr: 'Counter2: ', (counter2 value: #value) printString.

counter1 value: #reset.
Transcript showCr: 'Counter1 after reset: ', (counter1 value: #value) printString.
