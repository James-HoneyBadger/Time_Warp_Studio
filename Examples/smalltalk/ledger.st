"THE LEDGER
A Smalltalk program about objects with state: an Account
remembers its own balance and refuses what it cannot pay. The
report at the end is built with the collection protocol rather
than a loop over indexes."

Object subclass: #Account
  instanceVariableNames: 'owner balance refusals entries'

Account class >> owner: aName
  | account |
  account := self new.
  account open: aName.
  ^account

Account >> open: aName
  owner := aName.
  balance := 0.
  refusals := 0.
  entries := 0

Account >> owner
  ^owner

Account >> balance
  ^balance

Account >> refusals
  ^refusals

Account >> entries
  ^entries

Account >> deposit: anAmount
  balance := balance + anAmount.
  entries := entries + 1

Account >> withdraw: anAmount
  anAmount > balance
    ifTrue: [refusals := refusals + 1. ^false].
  balance := balance - anAmount.
  entries := entries + 1.
  ^true

Account >> describe
  ^owner , ': ' , balance printString , ' after ' , entries printString , ' entries, '
    , refusals printString , ' refused'

| ada grace alan accounts takings refusals richest |
Transcript show: 'T H E   L E D G E R'; cr.
Transcript show: '----------------------------------------'; cr.

ada := Account owner: 'Ada'.
grace := Account owner: 'Grace'.
alan := Account owner: 'Alan'.

ada deposit: 120.
ada withdraw: 30.
ada withdraw: 500.

grace deposit: 80.
grace withdraw: 20.
grace withdraw: 15.

alan deposit: 40.
alan withdraw: 40.

"Now they are a collection, because the rest of the report is the
 collection protocol rather than a loop over indexes."
accounts := OrderedCollection new.
accounts add: ada.
accounts add: grace.
accounts add: alan.

accounts do: [:each | Transcript show: each describe; cr].
Transcript cr.

takings := accounts inject: 0 into: [:sum :each | sum + each balance].
Transcript show: 'In the bank: '; show: takings printString; cr.

refusals := accounts inject: 0 into: [:sum :each | sum + each refusals].
Transcript show: 'Refused withdrawals: '; show: refusals printString; cr.

richest := accounts select: [:each | each balance > 40].
Transcript show: 'Accounts over 40: '; show: richest size printString; cr.

Transcript show: 'Average balance: '; show: (takings / accounts size) printString; cr.
Transcript show: 'Every figure above came from a message an object answered for itself.'; cr.
nil.
