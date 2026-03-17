"Shapes Demo — Geometry calculations in Smalltalk"
"Demonstrates: variables, blocks, collections, arithmetic, conditionals"

"══════════════════════════════════════"
"   🔷 Shape Gallery Demo"
"══════════════════════════════════════"

Transcript showCr: '╔══════════════════════════════════════╗'.
Transcript showCr: '║   🔷 Shape Gallery                   ║'.
Transcript showCr: '╚══════════════════════════════════════╝'.
Transcript showCr: ''.

"── Circles ──"
Transcript showCr: '--- Circles ---'.
| r1 r2 area1 area2 perim1 perim2 |
r1 := 10.
r2 := 5.
area1 := 3.14159 * r1 * r1.
area2 := 3.14159 * r2 * r2.
perim1 := 2 * 3.14159 * r1.
perim2 := 2 * 3.14159 * r2.
Transcript showCr: '  Big Circle (red, radius=' , r1 printString , ')'.
Transcript showCr: '  Area: ' , area1 printString.
Transcript showCr: '  Perimeter: ' , perim1 printString.
Transcript showCr: '  Small Circle (blue, radius=' , r2 printString , ')'.
Transcript showCr: '  Area: ' , area2 printString.
Transcript showCr: '  Perimeter: ' , perim2 printString.
Transcript showCr: ''.

"── Rectangles ──"
Transcript showCr: '--- Rectangles ---'.
| w1 h1 w2 h2 rectArea1 rectArea2 rectPerim1 rectPerim2 |
w1 := 3.
h1 := 7.
w2 := 4.
h2 := 4.
rectArea1 := w1 * h1.
rectArea2 := w2 * h2.
rectPerim1 := 2 * (w1 + h1).
rectPerim2 := 2 * (w2 + h2).
Transcript showCr: '  Door (brown, ' , w1 printString , 'x' , h1 printString , ')'.
Transcript showCr: '  Area: ' , rectArea1 printString.
Transcript showCr: '  Perimeter: ' , rectPerim1 printString.
Transcript showCr: '  Tile/Square (white, side=' , w2 printString , ')'.
Transcript showCr: '  Area: ' , rectArea2 printString.
Transcript showCr: '  Perimeter: ' , rectPerim2 printString.
Transcript showCr: ''.

"── Triangle ──"
Transcript showCr: '--- Triangle ---'.
| tb th tsa tsb triArea triPerim |
tb := 8.
th := 6.
tsa := 7.
tsb := 7.
triArea := 0.5 * tb * th.
triPerim := tb + tsa + tsb.
Transcript showCr: '  Roof (gray, base=' , tb printString , ' height=' , th printString , ')'.
Transcript showCr: '  Area: ' , triArea printString.
Transcript showCr: '  Perimeter: ' , triPerim printString.
Transcript showCr: ''.

"── Summary using collection ──"
Transcript showCr: '--- Summary ---'.
| areas shapes totalArea largest largestName |
areas := OrderedCollection new.
areas add: area1.
areas add: area2.
areas add: rectArea1.
areas add: rectArea2.
areas add: triArea.

shapes := OrderedCollection new.
shapes add: 'Big Circle'.
shapes add: 'Small Circle'.
shapes add: 'Door'.
shapes add: 'Tile'.
shapes add: 'Roof'.

totalArea := 0.
areas do: [:a | totalArea := totalArea + a].

Transcript showCr: '  Total shapes: ' , areas size printString.
Transcript showCr: '  Total area: ' , totalArea printString.

"Find largest by area"
largest := areas at: 1.
largestName := shapes at: 1.
| idx |
idx := 1.
areas do: [:a |
    (a > largest) ifTrue: [
        largest := a.
        largestName := shapes at: idx.
    ].
    idx := idx + 1.
].
Transcript showCr: '  Largest: ' , largestName , ' (area: ' , largest printString , ')'.

"Count circles (first 2 shapes)"
| circleCount |
circleCount := 2.
Transcript showCr: '  Circles: ' , circleCount printString.

Transcript showCr: ''.
Transcript showCr: '✅ Shape demo complete!'.
