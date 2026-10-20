"THE SHAPE FACTORY
A Smalltalk showcase: classes, instance variables, class-side
constructors, inheritance with super, blocks and closures,
collections, and turtle graphics."

Object subclass: #Polygon
  instanceVariableNames: 'sides length colour'

Polygon class >> sides: aCount length: aLength colour: aName
  | shape |
  shape := self new.
  shape setSides: aCount length: aLength colour: aName.
  ^shape

Polygon >> setSides: aCount length: aLength colour: aName
  sides := aCount.
  length := aLength.
  colour := aName

Polygon >> sides
  ^sides

Polygon >> length
  ^length

Polygon >> colour
  ^colour

Polygon >> interiorAngle
  ^(sides - 2) * 180 / sides

Polygon >> drawOn: aTurtle
  | turn |
  turn := 360 / sides.
  sides timesRepeat: [aTurtle forward: length. aTurtle right: turn]

Polygon >> describe
  ^colour , ' ' , sides printString , '-gon, ' , length printString , ' units a side'

Polygon subclass: #Star
  instanceVariableNames: 'points'

Star class >> points: aCount length: aLength
  | star |
  star := self new.
  star setSides: aCount length: aLength colour: 'gold'.
  star setPoints: aCount.
  ^star

Star >> setPoints: aCount
  points := aCount

Star >> drawOn: aTurtle
  points timesRepeat: [aTurtle forward: length. aTurtle right: 144]

Star >> describe
  ^'a ' , points printString , '-pointed star - ' , super describe

| t shapes angles large total count addOne |
Transcript show: 'THE SHAPE FACTORY'; cr.
Transcript show: '----------------------------------------'; cr.

shapes := OrderedCollection new.
shapes add: (Polygon sides: 3 length: 50 colour: 'red').
shapes add: (Polygon sides: 4 length: 45 colour: 'blue').
shapes add: (Polygon sides: 5 length: 40 colour: 'green').
shapes add: (Polygon sides: 6 length: 30 colour: 'orange').
shapes add: (Star points: 5 length: 45).

shapes do: [:each | Transcript show: each describe; cr].
Transcript cr.

angles := shapes collect: [:each | each interiorAngle].
Transcript show: 'Interior angles: '; show: angles printString; cr.

large := shapes select: [:each | each sides > 4].
Transcript show: 'Shapes with more than four sides: '; show: large size printString; cr.

total := shapes inject: 0 into: [:sum :each | sum + each sides].
Transcript show: 'Sides in total: '; show: total printString; cr.
Transcript cr.

count := 0.
addOne := [:n | count := count + n].
addOne value: 5.
addOne value: 7.
Transcript show: 'The closure counted '; show: count printString; cr.
Transcript cr.

t := Turtle new.
shapes do: [:each |
  each drawOn: t.
  t penUp.
  t forward: 40.
  t penDown].
Transcript show: 'Drawn '; show: shapes size printString; show: ' shapes.'; cr.
Transcript show: 'The turtle ended at '; show: t x printString; show: ', '; show: t y printString; cr.
nil.
