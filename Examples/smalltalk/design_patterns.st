"Design Patterns in Smalltalk"
"============================================================"
"DESIGN PATTERNS — Smalltalk OOP Showcase"
"Observer * Strategy * Composite * Command * Iterator"
"Time Warp Studio — Smalltalk Language Demo"
"============================================================"

"===== OBSERVER PATTERN ====="
"Subject notifies all registered observers on state change"

Object subclass: #EventEmitter
    instanceVariableNames: 'listeners name'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Patterns'.

EventEmitter >> #initialize: aName [
    name := aName.
    listeners := OrderedCollection new.
]

EventEmitter >> #addListener: aBlock [
    listeners add: aBlock.
]

EventEmitter >> #emit: anEvent with: data [
    listeners do: [ :listener |
        listener value: anEvent value: data.
    ].
]

EventEmitter >> #name [ ^name ]

"Stock price observable"
Object subclass: #StockTicker
    instanceVariableNames: 'symbol price emitter history'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Patterns'.

StockTicker >> #initialize: sym at: initialPrice [
    symbol  := sym.
    price   := initialPrice.
    history := OrderedCollection new.
    emitter := EventEmitter new initialize: sym.
    history add: initialPrice.
]

StockTicker >> #updatePrice: newPrice [
    | change pct |
    change := newPrice - price.
    pct    := (change * 100.0 / price) roundTo: 0.01.
    price  := newPrice.
    history add: newPrice.
    emitter emit: #priceChanged with: (Array with: symbol with: newPrice with: change with: pct).
]

StockTicker >> #subscribe: aBlock [
    emitter addListener: aBlock.
]

StockTicker >> #symbol [ ^symbol ]
StockTicker >> #price  [ ^price  ]
StockTicker >> #history [ ^history ]

StockTicker >> #stats [
    | prices mn mx avg |
    prices := history asArray.
    mn := prices inject: prices first into: [ :m :x | m min: x ].
    mx := prices inject: prices first into: [ :m :x | m max: x ].
    avg := (prices inject: 0 into: [ :s :x | s + x ]) / prices size.
    ^(Array with: mn with: mx with: avg roundTo: 0.01).
]

"===== STRATEGY PATTERN ====="
"Pluggable sorting strategies"

Object subclass: #Sorter
    instanceVariableNames: 'strategy'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Patterns'.

Sorter >> #useStrategy: aBlock [
    strategy := aBlock.
]

Sorter >> #sort: aCollection [
    ^strategy value: aCollection copy.
]

"===== COMPOSITE PATTERN ====="
"Tree structure — files and folders"

Object subclass: #FileSystemItem
    instanceVariableNames: 'name size'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Patterns'.

FileSystemItem >> #initialize: aName size: aSize [
    name := aName. size := aSize.
]
FileSystemItem >> #name    [ ^name ]
FileSystemItem >> #size    [ ^size ]
FileSystemItem >> #isDir   [ ^false ]
FileSystemItem >> #totalSize [ ^size ]
FileSystemItem >> #printOn: aStream depth: d [
    1 to: d do: [ :i | aStream nextPutAll: '  ' ].
    aStream nextPutAll: name.
    aStream nextPutAll: ' ('.
    aStream print: size.
    aStream nextPutAll: ' bytes)'.
]

FileSystemItem subclass: #Directory
    instanceVariableNames: 'children'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Patterns'.

Directory >> #initialize: aName [
    name := aName. size := 0. children := OrderedCollection new.
]
Directory >> #add: item     [ children add: item. ]
Directory >> #isDir         [ ^true ]
Directory >> #totalSize     [ ^children inject: 0 into: [ :sum :c | sum + c totalSize ] ]
Directory >> #printOn: aStream depth: d [
    1 to: d do: [ :i | aStream nextPutAll: '  ' ].
    aStream nextPutAll: '[DIR] '.
    aStream nextPutAll: name.
    aStream nextPutAll: ' ('.
    aStream print: self totalSize.
    aStream nextPutAll: ' bytes total)'.
    children do: [ :c | c printOn: aStream depth: d + 1 ].
]

"===== COMMAND PATTERN ====="
"Undo-able operations via command objects"

Object subclass: #TextEditor
    instanceVariableNames: 'content commandHistory'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Patterns'.

TextEditor >> #initialize [
    content        := ''.
    commandHistory := OrderedCollection new.
]

TextEditor >> #execute: aCommand [
    aCommand executeOn: self.
    commandHistory add: aCommand.
]

TextEditor >> #undo [
    commandHistory isEmpty ifTrue: [ ^self ].
    commandHistory last undoOn: self.
    commandHistory removeLast.
]

TextEditor >> #content [ ^content ]
TextEditor >> #setContent: s [ content := s ]

Object subclass: #InsertCommand
    instanceVariableNames: 'text position savedContent'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Patterns'.

InsertCommand >> #initialize: aText at: pos [
    text := aText. position := pos.
]
InsertCommand >> #executeOn: editor [
    | c before after |
    c := editor content.
    savedContent := c.
    before := c copyFrom: 1 to: (position min: c size).
    after  := c copyFrom: ((position + 1) min: (c size + 1)) to: c size.
    editor setContent: before , text , after.
]
InsertCommand >> #undoOn: editor [
    editor setContent: savedContent.
]

"===== MAIN PROGRAM ====="

| header |
header := [ :title |
    Transcript showCr: ''.
    Transcript showCr: ('═' * 60).
    Transcript showCr: ('  ' , title).
    Transcript showCr: ('═' * 60).
].

header value: 'DESIGN PATTERNS — Smalltalk OOP Showcase'.

"---- Section 1: Observer (Stock Market) ----"
Transcript showCr: ''.
Transcript showCr: '[ 1 ] OBSERVER PATTERN — Stock Market Ticker'.
Transcript showCr: '  Observers react automatically to price changes'.
Transcript showCr: ''.

| aapl goog alerts portfolio |
aapl = StockTicker new initialize: 'AAPL' at: 150.
goog = StockTicker new initialize: 'GOOG' at: 2800.
alerts = OrderedCollection new.
portfolio = 0.

"Alert subscriber"
aapl subscribe: [ :evt :data |
    | sym price chg pct msg |
    sym := data at: 1.
    price := data at: 2.
    chg  := data at: 3.
    pct  := data at: 4.
    msg  := '  ' , sym , ': $' , price printString ,
            ' (change: ' , chg printString , ', ' , pct printString , '%)'.
    Transcript showCr: msg.
    (pct abs > 3) ifTrue: [
        Transcript showCr: '    ⚠ ALERT: Significant price move!'.
    ].
].

goog subscribe: [ :evt :data |
    | sym price |
    sym   := data at: 1.
    price := data at: 2.
    Transcript showCr: ('  ' , sym , ': $' , price printString).
].

Transcript showCr: '  Price updates:'.
aapl updatePrice: 152.
aapl updatePrice: 148.
aapl updatePrice: 165.
aapl updatePrice: 140.
goog updatePrice: 2850.
goog updatePrice: 2750.

| aStats |
aStats := aapl stats.
Transcript showCr: ''.
Transcript showCr: '  AAPL summary:'.
Transcript showCr: ('    Min: $' , (aStats at: 1) printString).
Transcript showCr: ('    Max: $' , (aStats at: 2) printString).
Transcript showCr: ('    Avg: $' , (aStats at: 3) printString).

"---- Section 2: Strategy (Sorting) ----"
Transcript showCr: ''.
Transcript showCr: '[ 2 ] STRATEGY PATTERN — Pluggable Sorting'.
Transcript showCr: '  Swap algorithms at runtime without changing client code'.
Transcript showCr: ''.

| sorter data ascStrategy descStrategy absStrategy |
sorter := Sorter new.
data   := #(64 25 -12 22 -11 90 55 -44 77 33 88).

ascStrategy  := [ :arr | arr sort: [ :a :b | a <= b ] ].
descStrategy := [ :arr | arr sort: [ :a :b | a >= b ] ].
absStrategy  := [ :arr | arr sort: [ :a :b | a abs <= b abs ] ].

sorter useStrategy: ascStrategy.
Transcript showCr: ('  Ascending:  ' , (sorter sort: data) printString).

sorter useStrategy: descStrategy.
Transcript showCr: ('  Descending: ' , (sorter sort: data) printString).

sorter useStrategy: absStrategy.
Transcript showCr: ('  By |value|:  ' , (sorter sort: data) printString).

"---- Section 3: Composite (File System) ----"
Transcript showCr: ''.
Transcript showCr: '[ 3 ] COMPOSITE PATTERN — Filesystem Tree'.
Transcript showCr: '  Uniform treatment of files and directories'.
Transcript showCr: ''.

| root src docs bin |
root := Directory new initialize: 'project'.
src  := Directory new initialize: 'src'.
docs := Directory new initialize: 'docs'.
bin  := Directory new initialize: 'bin'.

src  add: (FileSystemItem new initialize: 'main.py'     size: 4200).
src  add: (FileSystemItem new initialize: 'utils.py'    size: 1800).
src  add: (FileSystemItem new initialize: 'models.py'   size: 5600).
docs add: (FileSystemItem new initialize: 'README.md'   size: 3100).
docs add: (FileSystemItem new initialize: 'API.md'      size: 8900).
bin  add: (FileSystemItem new initialize: 'app.exe'     size: 45000).
root add: src.
root add: docs.
root add: bin.
root add: (FileSystemItem new initialize: 'config.json' size: 512).
root add: (FileSystemItem new initialize: '.gitignore'  size: 128).

Transcript showCr: ('  Total project size: ' , root totalSize printString , ' bytes').
| stream |
stream := ReadWriteStream on: ''.
root printOn: stream depth: 0.
Transcript showCr: stream contents.

"---- Section 4: Command Pattern (Text Editor) ----"
Transcript showCr: ''.
Transcript showCr: '[ 4 ] COMMAND PATTERN — Undo/Redo Text Editor'.
Transcript showCr: '  Every operation is an object — enables undo'.
Transcript showCr: ''.

| editor c1 c2 c3 |
editor := TextEditor new initialize.

c1 := InsertCommand new initialize: 'Hello' at: 0.
c2 := InsertCommand new initialize: ', World' at: 5.
c3 := InsertCommand new initialize: '!' at: 12.

editor execute: c1.
Transcript showCr: ('  After insert "Hello":     "' , editor content , '"').
editor execute: c2.
Transcript showCr: ('  After insert ", World":   "' , editor content , '"').
editor execute: c3.
Transcript showCr: ('  After insert "!":         "' , editor content , '"').
editor undo.
Transcript showCr: ('  After undo:               "' , editor content , '"').
editor undo.
Transcript showCr: ('  After undo again:         "' , editor content , '"').

"---- Section 5: Collections Deep Dive ----"
Transcript showCr: ''.
Transcript showCr: '[ 5 ] SMALLTALK COLLECTIONS SHOWCASE'.
Transcript showCr: '  OrderedCollection | inject:into: | collect: | select:'.
Transcript showCr: ''.

| numbers evens squares sum avg |
numbers := OrderedCollection withAll: #(1 2 3 4 5 6 7 8 9 10 11 12).
evens   := numbers select: [ :n | n \\ 2 = 0 ].
squares := numbers collect: [ :n | n * n ].
sum     := numbers inject: 0 into: [ :acc :n | acc + n ].
avg     := sum / numbers size.

Transcript showCr: ('  Numbers: ' , numbers printString).
Transcript showCr: ('  Evens:   ' , evens printString).
Transcript showCr: ('  Squares: ' , squares printString).
Transcript showCr: ('  Sum: ' , sum printString , '  Average: ' , avg printString).

| primes |
primes := (2 to: 50) select: [ :n |
    | isPrime |
    isPrime := true.
    (2 to: n - 1) do: [ :d |
        (n \\ d = 0) ifTrue: [ isPrime := false ].
    ].
    isPrime.
].
Transcript showCr: ('  Primes up to 50: ' , primes printString).

Transcript showCr: ''.
Transcript showCr: ('═' * 60).
Transcript showCr: '  Smalltalk Design Patterns Complete!'.
Transcript showCr: '  OOP | Blocks | Collections | Observer | Strategy | Composite'.
Transcript showCr: ('═' * 60).
