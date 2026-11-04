# Smalltalk: everything is a message

Smalltalk-80 is the language that made "everything is an object" a working system,
not a principle. It has no control-flow syntax at all: `ifTrue:`, `whileTrue:` and
`to:do:` are messages sent to objects, and the branches are blocks sent along with
them. It is the one object-oriented language here, and the one where a definition's
shape is decided by indentation, because a file-in has no terminator to carry it.
The interpreter runs Smalltalk whole-program: the file is read, classes and methods
are installed, and then the top-level code runs.

Press **F9** to run a document. The output blocks below are what the program prints.

## What a program looks like

```smalltalk
Transcript show: 'Hello, World!'; cr.
```

```text
Hello, World!
```

`Transcript` is the object that answers to `show:`, and `show:` is a keyword message
with its argument. `;` is a cascade: `cr` is sent to the same receiver as `show:`,
not to what `show:` answered, and `cr` ends the line. There is no print statement.
Output is a message to an object that writes.

## Step 1: messages and what is echoed

Send three messages and see which ones report anything.

```smalltalk
1 + 2.
'hello' size.
2 + 3 * 4
```

```text
20
```

`+ 2` is a binary message, `size` is a unary one, and both answer values. Only the
last statement of a run is displayed, and only when its value is not nil, so the first
two answers are computed and dropped. That is the workspace's "print it" applied once
at the end of the file. A program that wants several answers calls `Transcript` for
the ones it must see.

## Step 2: precedence

Look at what `2 + 3 * 4` answers.

```smalltalk
2 + 3 * 4
```

```text
20
```

Unary messages bind tightest, then binary, then keyword, and within one level the
order is left to right. Both `+` and `*` are binary, so `2 + 3` goes first and 5 is
multiplied by 4. There is no arithmetic precedence: `*` is another message and has no
rank over `+`. Parentheses are what say otherwise, so `2 + (3 * 4)` is 14.

## Step 3: variables and blocks

Total the numbers from 1 to 10 with a block and a temporary.

```smalltalk
| total |
total := 0.
1 to: 10 do: [ :i | total := total + i ].
total
```

```text
55
```

`| total |` declares a temporary and `:=` assigns it. A block is `[ … ]`, and
`[ :i | … ]` is a block that takes a parameter, marked by the second `|`. `to:do:`
is a keyword message that runs the block once per number. Blocks are closures. One
remembers the variables where it was written, so the block adds into `total`, not
into a copy.

## Step 4: a class and its methods

Define a counter, send it two messages, ask for its value.

```smalltalk
Object subclass: #Counter
  instanceVariableNames: 'count'

Counter >> initialize
  count := 0

Counter >> increment
  count := count + 1

Counter >> value
  ^count

| c |
c := Counter new.
c increment.
c increment.
c value
```

```text
Counter
Counter >> initialize
Counter >> increment
Counter >> value
2
```

A class definition is not special syntax. It is the keyword message
`subclass:instanceVariableNames:` sent to `Object`, and it reports itself, which is
the first line of the output. Each method is `Class >> selector` followed by an
indented body, and a definition ends at the next line that begins in column 0. `^`
answers from the method. `Counter new` finds `initialize` and `value` through the
instance's class, so the two increments are visible to the third message.

## Step 5: the turtle

Draw a square by sending messages to a turtle.

```smalltalk
| t |
t := Turtle new.
1 to: 4 do: [ :i | t forward: 80. t right: 90 ].
Transcript show: 'square'; cr.
```

```text
square
```

`Turtle` is a Time Warp extension, not part of Smalltalk-80, which shipped without
graphics of this kind. `forward:` and `right:` are keyword messages, so the distance
and the angle are arguments. There is no loop keyword to reach for: `to:do:` over a
range is the count, and the block tells the turtle what to do on each pass.

## Where it catches people out

**`2 + 3 * 4` is 20.** The rule is left to right among binary messages, and a reader
who expects arithmetic precedence gets a wrong number without an error.

```smalltalk
2 + 3 * 4
```

```text
20
```

`2 + (3 * 4)` is 14. When a reader would expect precedence, the parentheses are part
of the program, not a comment on it.

**Only the last statement is echoed.** A file of expressions prints one value, and
statement separators decide what "last" means.

```smalltalk
1 + 2
3 + 4
```

```text
3
```

The two lines ran together as one statement because a `.` separates statements and a
newline does not. End every statement but the last with a `.`, or ask `Transcript`
for the values that matter.

**Indentation ends a definition.** A method body is the indented lines under its
header, and the first line back at column 0 closes it.

```smalltalk
Object subclass: #Box
  instanceVariableNames: 'value'

Box >> value
^value
```

That method has no body. `^value` sits at column 0, so it is top-level code and the
definition above it is empty; the report says `Box >> value has an empty body`. Indent
the body by any amount and the method is installed.

## Messages used here

| Message | What it does |
| --- | --- |
| `show:`, `cr` | Write to `Transcript`, and end the line |
| `+`, `*`, `size` | Binary and unary messages |
| `to:do:` | Counted loop over a range |
| `ifTrue:`, `ifFalse:`, `whileTrue:` | A condition or a loop, with blocks for the branches |
| `subclass:instanceVariableNames:` | Define a class |
| `Class >> selector` | Define a method |
| `^` | Answer from a method |
| `:=` | Assign |
| `;` | Cascade: send to the same receiver again |

## Next

The [language guide](../reference.md#smalltalk) covers method lookup,
class-side methods, non-local return and `doesNotUnderstand:`, and lists what this
Smalltalk deliberately leaves out.
[`Examples/smalltalk/shapes.st`](../../../Examples/smalltalk/shapes.st) builds
turtle figures out of classes, and
[`Examples/smalltalk/ledger.st`](../../../Examples/smalltalk/ledger.st) is a longer
object model.
