# Smalltalk Programming Tutorial

Smalltalk was created at Xerox PARC in the 1970s by Alan Kay, Dan Ingalls, and Adele Goldberg. It pioneered object-oriented programming — everything is an object, and all computation is done by sending messages.

## Hello World

```smalltalk
Transcript showCrLn: 'Hello from Smalltalk!'.
Transcript showCrLn: 'Welcome to Time Warp Studio'.
```

## Messages

In Smalltalk every operation is a **message send**:

```smalltalk
"Unary message (no args)"
10 factorial.
'hello' reversed.
42 printString.

"Binary message (one operand)"
3 + 4.         "7"
10 - 3.        "7"
5 * 6.         "30"
10 / 2.        "5"

"Keyword message (one or more args)"
10 max: 20.
'Hello' copyFrom: 1 to: 3.
```

## Variables

```smalltalk
"Temp vars declared with | ... |"
| x y name |
x := 42.
y := 3.14.
name := 'Alice'.

Transcript showCrLn: name.
Transcript showCrLn: (x + y) printString.
```

## Conditionals

```smalltalk
| score |
score := 85.

"ifTrue: / ifFalse: / ifTrue:ifFalse:"
(score >= 90)
    ifTrue:  [ Transcript showCrLn: 'Grade A' ]
    ifFalse: [ Transcript showCrLn: 'Below A' ].

| grade |
grade := (score >= 90) ifTrue: ['A']
         ifFalse: [
             (score >= 80) ifTrue: ['B']
             ifFalse: [
                 (score >= 70) ifTrue: ['C']
                 ifFalse: ['F'] ] ].
Transcript showCrLn: ('Grade: ', grade).
```

## Loops

```smalltalk
"timesRepeat:"
5 timesRepeat: [ Transcript showCrLn: 'Hello!' ].

"to:do:"
1 to: 10 do: [:i |
    Transcript showCrLn: i printString ].

"to:by:do:"
10 to: 1 by: -2 do: [:i |
    Transcript show: i printString; show: ' ' ].
Transcript nl.

"whileTrue:"
| n |
n := 1.
[ n <= 32 ] whileTrue: [
    Transcript show: n printString; show: ' '.
    n := n * 2 ].
Transcript nl.
```

## Collections

```smalltalk
"OrderedCollection (dynamic list)"
| list |
list := OrderedCollection new.
list add: 'Apple'.
list add: 'Banana'.
list add: 'Cherry'.
list addFirst: 'Avocado'.
Transcript showCrLn: list printString.
Transcript showCrLn: 'Size: ', list size printString.

"Array"
| arr |
arr := #(10 20 30 40 50).
arr do: [:each | Transcript show: each printString; show: ' '].
Transcript nl.

"Dictionary"
| dict |
dict := Dictionary new.
dict at: 'name' put: 'Bob'.
dict at: 'age' put: 30.
Transcript showCrLn: (dict at: 'name').
```

## Blocks (Closures)

```smalltalk
| adder result |
"Blocks are first-class closures"
adder := [:x :y | x + y].
result := adder value: 10 value: 20.
Transcript showCrLn: result printString.

"Iterating with inject:into: (fold/reduce)"
| numbers sum |
numbers := #(1 2 3 4 5 6 7 8 9 10).
sum := numbers inject: 0 into: [:acc :each | acc + each].
Transcript showCrLn: 'Sum: ', sum printString.
```

## Classes

```smalltalk
"Class definition"
Object subclass: #Point
    instanceVariableNames: 'x y'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Geometry'.

Point >> x [ ^x ]
Point >> y [ ^y ]
Point >> x: anX y: aY [ x := anX. y := aY ]
Point >> printOn: aStream [
    aStream nextPutAll: '(', x printString, ', ', y printString, ')' ]
Point >> distanceTo: other [
    | dx dy |
    dx := x - other x.
    dy := y - other y.
    ^ ((dx * dx) + (dy * dy)) sqrt ]

| p1 p2 |
p1 := Point new x: 0 y: 0.
p2 := Point new x: 3 y: 4.
Transcript showCrLn: (p1 distanceTo: p2) printString.  "5.0"
```

## Further Reading

- [Examples/smalltalk/](../Examples/smalltalk/) — 5 Smalltalk example programs
- [Language Guide: Smalltalk](LANGUAGE_GUIDE.md#smalltalk)
