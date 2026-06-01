"Inheritance and Polymorphism in Smalltalk"

Object subclass: #Animal
    instanceVariableNames: 'name age sound'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Examples'.

Animal class >> new: aName age: anAge sound: aSound [
    | a |
    a := super new.
    a name: aName; age: anAge; sound: aSound.
    ^a
]

Animal >> name  [ ^name ]
Animal >> age   [ ^age ]
Animal >> sound [ ^sound ]
Animal >> name: n  [ name  := n ]
Animal >> age: a   [ age   := a ]
Animal >> sound: s [ sound := s ]

Animal >> speak [
    Transcript showCr: name , ' says: ' , sound.
]

Animal >> describe [
    Transcript showCr: self class name , ': ' , name ,
        ' (age ' , age printString , ')'.
]

Animal >> isOlderThan: other [
    ^age > other age
]

"-- Subclasses --"
Animal subclass: #Dog
    instanceVariableNames: 'breed'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Examples'.

Dog class >> new: aName age: anAge breed: aBreed [
    | d |
    d := super new: aName age: anAge sound: 'Woof!'.
    d breed: aBreed.
    ^d
]
Dog >> breed  [ ^breed ]
Dog >> breed: b [ breed := b ]
Dog >> describe [
    super describe.
    Transcript showCr: '  Breed: ' , breed.
]

Animal subclass: #Cat
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Examples'.

Cat class >> new: aName age: anAge [
    ^super new: aName age: anAge sound: 'Meow!'.
]
Cat >> speak [
    Transcript showCr: name , ' purrs... then says: ' , sound.
]

"-- Demo --"
| animals |
animals := OrderedCollection new.
animals add: (Dog new: 'Rex'    age: 5 breed: 'German Shepherd').
animals add: (Dog new: 'Buddy'  age: 3 breed: 'Labrador').
animals add: (Cat new: 'Whiskers' age: 7).
animals add: (Cat new: 'Luna'    age: 2).

Transcript showCr: '=== Animal Kingdom ==='.
animals do: [:a | a describe].

Transcript showCr: ''.
Transcript showCr: '=== All Animals Speak (Polymorphism) ==='.
animals do: [:a | a speak].

Transcript showCr: ''.
Transcript showCr: '=== Oldest Animal ==='.
| oldest |
oldest := animals inject: animals first
                  into: [:max :a | (a isOlderThan: max) ifTrue: [a] ifFalse: [max]].
Transcript showCr: oldest name , ' is oldest at ' , oldest age printString , ' years'.
