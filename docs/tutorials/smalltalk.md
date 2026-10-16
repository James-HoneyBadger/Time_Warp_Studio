# Smalltalk Tutorial

Smalltalk-80 is the language that made "everything is an object" a working
proposal rather than a slogan. It is the one language in Time Warp Studio that is
object-oriented, and the one with the fewest reserved words: there is **no syntax
for control flow at all**. `ifTrue:`, `whileTrue:` and `to:do:` are messages sent
to objects.

Time Warp Studio runs Smalltalk whole-program, like Scheme and CHIP-8: a file
defines classes and methods and then runs some code, and a method body is more
than one line, so the whole source is read before anything runs.

---

## 1. Everything is a message

```smalltalk
1 + 2                a binary message: + 2, sent to 1
'hello' size         a unary message: size, sent to 'hello'
Transcript show: 'hi'    a keyword message: show:, sent to Transcript
```

There is no `print` statement. To produce output you send messages to
`Transcript`:

```smalltalk
Transcript show: 'Hello, World!'; cr
```

`;` is a **cascade**, and `cr` is the newline message.

## 2. Precedence

Unary first, then binary, then keyword, left to right within each level.
Parentheses group.

```smalltalk
2 + 3 * 4          20 - both are binary, so left to right
1 max: 2 + 3       5 - binary binds tighter than keyword
(1 + 2) * 3        9
```

`2 + 3 * 4` being 20 rather than 14 is the single most common surprise. In
Smalltalk, `*` is just another binary message and binary messages have no
precedence over each other.

## 3. Variables

Method arguments and temporaries are declared in `| ... |` at the top of a method
or block. Assignment is `:=`.

```smalltalk
| total |
total := 0.
1 to: 10 do: [ :i | total := total + i ].
total
```

`|` appears twice and does two jobs: it **groups** temporary names, and it
**closes** a block's argument list in `[ :i | ... ]`. It is not a message.

## 4. Blocks

A block is an object - a piece of code you can hold and run later.

```smalltalk
[ 1 + 2 ] value               3
[ :x | x * 2 ] value: 21      42
[ :x :y | x + y ] value: 1 value: 2
```

Blocks are **closures**: one captures the activation it was created in, so it
still reads and writes the variables where it was written.

```smalltalk
Object subclass: #Accumulator
  instanceVariableNames: 'total'

Accumulator >> initialize
  total := 0

Accumulator >> add: aBlock
  total := total + aBlock value

Accumulator >> value
  ^total

| a |
a := Accumulator new.
a add: [ 5 ].
a add: [ 10 ].
a value
```

## 5. Non-local return

`^` inside a **block** returns from the *method that created the block*, not from
the block. This is the part of Smalltalk that surprises people, and faking it -
returning from the block instead - gives a wrong answer that looks right in a
simple test.

```smalltalk
Object subclass: #Finder
  instanceVariableNames: ''

Finder >> firstOver: aLimit in: aCollection
  aCollection do: [ :each |
    each > aLimit ifTrue: [ ^each ] ].
  ^nil
```

The `^each` leaves `firstOver:in:`, not just the block.

## 6. Conditions and loops

There are no keywords for these. They are messages, and the branches are blocks:

```smalltalk
score >= 90
  ifTrue: [ Transcript show: 'pass'; cr ]
  ifFalse: [ Transcript show: 'try again'; cr ]
```

```smalltalk
1 to: 5 do: [ :i | Transcript show: i printString; cr ]
[ count < 3 ] whileTrue: [ count := count + 1 ]
5 timesRepeat: [ Transcript show: 'x' ]
(1 to: 5) inject: 0 into: [ :sum :each | sum + each ]      15
```

`inject:into:` is the loop that carries a value along, and it is how you write a
sum without a temporary:

```smalltalk
(1 to: 5) inject: 0 into: [ :sum :each | sum + each ]      15
#(1 2 3) inject: 0 into: [ :a :b | a + b ]                 6
```

## 7. Classes

A class definition is not special syntax. It is the keyword message
`subclass:instanceVariableNames:` sent to an existing class:

```smalltalk
Object subclass: #Counter
  instanceVariableNames: 'count'
```

Methods are defined against the class with `>>`, and **indentation** is what
marks the body: a definition runs until the next line that begins in column 0.

```smalltalk
Counter >> initialize
  count := 0

Counter >> increment
  count := count + 1

Counter >> value
  ^count
```

A **class-side** method is defined against `Counter class`, and `self` there is
the class itself:

```smalltalk
Counter class >> withCount: aNumber
  | counter |
  counter := self new.
  counter setCount: aNumber.
  ^counter

Counter >> setCount: aNumber
  count := aNumber
```

`self` inside `Counter class >> withCount:` is the class `Counter`, so `self new`
makes an instance. A class-side method is how a class gains a constructor; it is
looked up in the class's own dictionary rather than in its instances'.

Send a message by walking the receiver's class chain and stopping at the first
selector that matches. A selector that reaches the top is a `doesNotUnderstand:`
error, reported with the selector and the receiver's class.

## 8. What gets displayed

A block of top-level code displays its **last value, and only if that value is not
nil**. This is the workspace's "print it" applied once per block.

```smalltalk
1 + 1                    2
| x |                     nothing: an assignment answers the value assigned
x := 5                    but x is 5 afterwards
```

A **definition reports itself**, so a class or method definition appears in the
output once. That is the chunk's value, not a stray message:

```smalltalk
Object subclass: #Counter
  instanceVariableNames: 'count'
```

```text
Counter
```

Real output belongs in `Transcript show:`, as it does in Smalltalk.

## 9. Turtle graphics

A **Time Warp extension**, as for COMAL, REXX, FOCAL and APL: `t := Turtle new`
and then `forward:`, `back:`, `right:`, `left:`, `home`.

```smalltalk
| t |
t := Turtle new.
1 to: 4 do: [ :i | t forward: 100. t right: 90 ]
```

Because there is no loop keyword, drawing a family of figures is a `do:` over a
range, with the figure told to draw itself. That is the shape of the example:

```smalltalk
Object subclass: #Polygon
  instanceVariableNames: 'sides length'

Polygon >> setSides: aCount length: aLength
  sides := aCount.
  length := aLength

Polygon >> interiorAngle
  ^(sides - 2) * 180 / sides

Polygon >> drawOn: aTurtle
  | turn |
  turn := 360 / sides.
  sides timesRepeat: [ aTurtle forward: length. aTurtle right: turn ]

| t |
t := Turtle new.
2 to: 6 do: [ :n | (Polygon withSides: n length: 34) drawOn: t ].
Transcript show: 'A pentagon''s interior angle is '; cr.
Transcript show: (Polygon withSides: 5 length: 10) interiorAngle printString; cr
```

Two details in that last line: `''` inside a string is one quote character, and
`printString` is how a number becomes text.

## What is not here

- **No metaclass tower.** A class object holds two method dictionaries rather than
  being an instance of a metaclass that is an instance of a metaclass.
  `Counter class` answers the class object itself.
- **No exceptions, processes, `thisContext`, `perform:` or compiler.** There is no
  `ensure:`, `on:do:` or semaphore.
- **No `!` chunk terminator**, so indentation carries the job it does in a file-in.

## Quick reference

| Message | Does |
| ------- | ---- |
| `show:` `cr` | Write to `Transcript` |
| `value` `value:` | Run a block |
| `ifTrue:` `ifFalse:` | Condition |
| `whileTrue:` `whileFalse:` | Loop |
| `to:do:` `timesRepeat:` | Counted loop |
| `inject:into:` | Loop carrying a value |
| `do:` `collect:` `select:` | Iterate a collection |
| `printString` | A value as text |
| `subclass:instanceVariableNames:` | Define a class |
| `X >> selector` | Define a method |
| `^` | Answer from a method |
| `;` | Cascade |
| `:=` | Assign |

See also
[`Examples/smalltalk/shapes.st`](../../Examples/smalltalk/shapes.st), and the
[Smalltalk reference](../LANGUAGE_GUIDE.md#smalltalk).
