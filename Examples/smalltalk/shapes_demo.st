"Shapes Demo — Object-Oriented Geometry in Smalltalk"
"Demonstrates: classes, inheritance, polymorphism, collections"

"══════════════════════════════════════"
"   🔷 Shape Hierarchy Demo"
"══════════════════════════════════════"

Object subclass: #Shape
    instanceVariableNames: 'name color'.

Shape >> initialize: aName color: aColor
    name := aName.
    color := aColor.

Shape >> name
    ^ name.

Shape >> color
    ^ color.

Shape >> area
    "Abstract — subclasses override"
    ^ 0.

Shape >> perimeter
    "Abstract — subclasses override"
    ^ 0.

Shape >> describe
    Transcript show: '  ', name, ' (', color, ')'.
    Transcript show: '  Area: ', self area printString.
    Transcript show: '  Perimeter: ', self perimeter printString.
    Transcript cr.

"── Circle ──"
Shape subclass: #Circle
    instanceVariableNames: 'radius'.

Circle >> initialize: aName color: aColor radius: r
    super initialize: aName color: aColor.
    radius := r.

Circle >> radius
    ^ radius.

Circle >> area
    ^ 3.14159 * radius * radius.

Circle >> perimeter
    ^ 2 * 3.14159 * radius.

"── Rectangle ──"
Shape subclass: #Rectangle
    instanceVariableNames: 'width height'.

Rectangle >> initialize: aName color: aColor width: w height: h
    super initialize: aName color: aColor.
    width := w.
    height := h.

Rectangle >> area
    ^ width * height.

Rectangle >> perimeter
    ^ 2 * (width + height).

"── Triangle ──"
Shape subclass: #Triangle
    instanceVariableNames: 'base height sideA sideB'.

Triangle >> initialize: aName color: aColor base: b height: h sideA: a sideB: s
    super initialize: aName color: aColor.
    base := b. height := h. sideA := a. sideB := s.

Triangle >> area
    ^ 0.5 * base * height.

Triangle >> perimeter
    ^ base + sideA + sideB.

"── Square (inherits Rectangle) ──"
Rectangle subclass: #Square.

Square >> initialize: aName color: aColor side: s
    super initialize: aName color: aColor width: s height: s.

"══════════════════════════════════════"
"   Create shapes and demonstrate"
"══════════════════════════════════════"

| shapes totalArea |
shapes := OrderedCollection new.

shapes add: (Circle new initialize: 'Big Circle' color: 'red' radius: 10).
shapes add: (Circle new initialize: 'Small Circle' color: 'blue' radius: 5).
shapes add: (Rectangle new initialize: 'Door' color: 'brown' width: 3 height: 7).
shapes add: (Square new initialize: 'Tile' color: 'white' side: 4).
shapes add: (Triangle new initialize: 'Roof' color: 'gray' base: 8 height: 6 sideA: 7 sideB: 7).

Transcript show: '╔══════════════════════════════════════╗'.
Transcript show: '║   🔷 Shape Gallery                   ║'.
Transcript show: '╚══════════════════════════════════════╝'.
Transcript cr.

"Print all shapes"
shapes do: [:shape |
    shape describe.
].

"Calculate total area"
totalArea := 0.
shapes do: [:shape |
    totalArea := totalArea + shape area.
].

Transcript cr.
Transcript show: '━━━ Summary ━━━'.
Transcript show: '  Total shapes: ', shapes size printString.
Transcript show: '  Total area:   ', totalArea printString.

"Find largest shape"
| largest |
largest := shapes first.
shapes do: [:shape |
    (shape area > largest area) ifTrue: [largest := shape].
].
Transcript show: '  Largest:      ', largest name, ' (area: ', largest area printString, ')'.

"Filter by type"
| circles |
circles := shapes select: [:s | s isKindOf: Circle].
Transcript show: '  Circles:      ', circles size printString.

Transcript cr.
Transcript show: '✅ Shape demo complete!'.
