"Pattern Matching and Conditionals in Smalltalk"

"-- ifTrue / ifFalse / ifTrue:ifFalse --"
Transcript showCr: '=== Boolean Messages ==='.

| x |
x := 42.
(x > 0) ifTrue:  [ Transcript showCr: x printString , ' is positive' ].
(x even) ifTrue:  [ Transcript showCr: x printString , ' is even' ]
         ifFalse: [ Transcript showCr: x printString , ' is odd' ].

"-- caseOf / otherwise --"
Transcript showCr: ''.
Transcript showCr: '=== caseOf (switch/case) ==='.

#(1 2 3 4 5 99) do: [:n |
    | result |
    result := n caseOf: {
        [1] -> ['one'].
        [2] -> ['two'].
        [3] -> ['three'].
        [4] -> ['four'].
        [5] -> ['five']
    } otherwise: ['unknown'].
    Transcript showCr: n printString , ' => ' , result.
].

"-- Numeric cascade and respond-to ---"
Transcript showCr: ''.
Transcript showCr: '=== respondsTo: (duck typing) ==='.

| objects |
objects := OrderedCollection withAll: #(42 3.14 'hello' $A #symbol).
objects do: [:obj |
    Transcript show: obj printString.
    Transcript show: ' — isNumber: '.
    Transcript show: (obj respondsTo: #+) printString.
    Transcript show: ', isString: '.
    Transcript showCr: (obj isKindOf: String) printString.
].

"-- Chained conditions --"
Transcript showCr: ''.
Transcript showCr: '=== Grade classifier ==='.

#(95 82 73 65 55 40) do: [:score |
    | grade |
    grade := (score >= 90) ifTrue: ['A']
             ifFalse: [
                 (score >= 80) ifTrue: ['B']
                 ifFalse: [
                     (score >= 70) ifTrue: ['C']
                     ifFalse: [
                         (score >= 60) ifTrue: ['D']
                         ifFalse: ['F']
                     ]
                 ]
             ].
    Transcript showCr: 'Score ' , score printString , ' => Grade ' , grade.
].
