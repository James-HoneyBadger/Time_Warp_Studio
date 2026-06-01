"Smalltalk Collections Demo"

"OrderedCollection — a dynamic list"
| numbers odds squares |
numbers := OrderedCollection new.
1 to: 10 do: [:i | numbers add: i].
Transcript showCr: 'Numbers: ', numbers printString.

"select: — filter"
odds := numbers select: [:x | (x \\ 2) = 1].
Transcript showCr: 'Odds: ', odds printString.

"collect: — transform"
squares := numbers collect: [:x | x * x].
Transcript showCr: 'Squares: ', squares printString.

"inject:into: — reduce (sum)"
| total |
total := numbers inject: 0 into: [:acc :x | acc + x].
Transcript showCr: 'Sum: ', total printString.

"Array literals"
| vowels |
vowels := #($a $e $i $o $u).
Transcript showCr: 'Vowels size: ', vowels size printString.
