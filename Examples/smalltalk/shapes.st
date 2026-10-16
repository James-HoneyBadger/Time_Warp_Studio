"Smalltalk in Time Warp Studio.

Everything here is a message: the class definition is the keyword message
subclass:instanceVariableNames: sent to Object, the loop is 2 to: 6 do:, and the
drawing is a Polygon being told to draw itself."

Object subclass: #Polygon
  instanceVariableNames: 'sides length'

"A method answers its last statement's value, and the last statement of
setSides:length: is an assignment — so the instance has to be answered by name."
Polygon class >> withSides: aCount length: aLength
  | polygon |
  polygon := self new.
  polygon setSides: aCount length: aLength.
  ^polygon

Polygon >> setSides: aCount length: aLength
  sides := aCount.
  length := aLength

Polygon >> interiorAngle
  ^(sides - 2) * 180 / sides

Polygon >> drawOn: aTurtle
  | turn |
  turn := 360 / sides.
  sides timesRepeat: [aTurtle forward: length. aTurtle right: turn]

"Five nested figures, each one turn further round. Temporaries are declared once,
here at the top, which is the only place Smalltalk allows them."
| t total |
t := Turtle new.
2 to: 6 do: [:n | (Polygon withSides: n length: 34) drawOn: t].

Transcript show: 'A pentagon''s interior angle is '; cr.
Transcript show: (Polygon withSides: 5 length: 10) interiorAngle printString; cr.

"The same figures again, walked round this time by inject:into: and a block."
total := (1 to: 5) inject: 0 into: [:sum :each | sum + each].
'Sides drawn in total: ' , total printString
