"================================================================"
"  SMALLTALK OBJECT-ORIENTED PROGRAMMING SHOWCASE"
"  Bank Account System demonstrating Smalltalk's pure OOP,"
"  message passing, collections, and class hierarchy."
"================================================================"

"────── ACCOUNT BASE CLASS ──────────────────────────────────────"
Object subclass: #BankAccount
    instanceVariableNames: 'accountNumber balance owner transactions'
    classVariableNames: 'NextAccountNumber TotalAccounts'
    poolDictionaries: ''
    category: 'AcmeBank'!

!BankAccount class methodsFor: 'initialization'!
initialize
    NextAccountNumber := 10001.
    TotalAccounts := 0.
!

new
    TotalAccounts isNil ifTrue: [BankAccount initialize].
    ^super new init
! !

!BankAccount methodsFor: 'initialization'!
init
    accountNumber := BankAccount nextAccountNumber.
    balance := 0.
    transactions := OrderedCollection new.
    TotalAccounts := TotalAccounts + 1.
    ^self
!

initWithOwner: aName balance: anAmount
    self init.
    owner := aName.
    balance := anAmount.
    self recordTransaction: 'OPEN' amount: anAmount description: 'Account opened'.
    ^self
! !

!BankAccount class methodsFor: 'class utilities'!
nextAccountNumber
    | n |
    n := NextAccountNumber.
    NextAccountNumber := NextAccountNumber + 1.
    ^n
!

totalAccounts
    ^TotalAccounts
! !

!BankAccount methodsFor: 'accessing'!
accountNumber ^accountNumber!
balance       ^balance!
owner         ^owner!
transactions  ^transactions! !

!BankAccount methodsFor: 'operations'!
deposit: amount
    amount <= 0 ifTrue: [^Error signal: 'Deposit amount must be positive'].
    balance := balance + amount.
    self recordTransaction: 'DEP' amount: amount description: 'Deposit'.
    ^balance
!

withdraw: amount
    amount <= 0 ifTrue: [^Error signal: 'Withdrawal amount must be positive'].
    balance < amount ifTrue: [^Error signal: 'Insufficient funds'].
    balance := balance - amount.
    self recordTransaction: 'WDR' amount: amount negated description: 'Withdrawal'.
    ^balance
!

transfer: amount to: targetAccount
    self withdraw: amount.
    targetAccount deposit: amount.
    ^amount
!

recordTransaction: typeCode amount: anAmount description: aDesc
    | txn |
    txn := Association key: (Array with: typeCode with: anAmount
                                   with: aDesc
                                   with: DateAndTime now)
                        value: balance.
    transactions add: txn.
! !

!BankAccount methodsFor: 'reporting'!
printStatement
    Transcript showCr: '──────────────────────────────────────────'.
    Transcript showCr: 'ACCOUNT STATEMENT'.
    Transcript showCr: 'Account: ', accountNumber printString.
    Transcript showCr: 'Owner:   ', owner.
    Transcript showCr: '──────────────────────────────────────────'.
    Transcript showCr: 'TYPE   AMOUNT        BALANCE   DESCRIPTION'.
    transactions do: [:t |
        | data |
        data := t key.
        Transcript showCr: (data at: 1), '    ',
            (data at: 2) printString, '   ',
            t value printString, '   ',
            (data at: 3).
    ].
    Transcript showCr: '──────────────────────────────────────────'.
    Transcript showCr: 'Current Balance: $', balance printString.
    Transcript showCr: ''.
!

printSummary
    Transcript showCr: '  Acct#', accountNumber printString,
        '  Owner: ', owner,
        '  Balance: $', balance printString.
! !

"────── SAVINGS ACCOUNT (subclass) ─────────────────────────────"
BankAccount subclass: #SavingsAccount
    instanceVariableNames: 'interestRate withdrawalCount maxWithdrawals'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'AcmeBank'!

!SavingsAccount methodsFor: 'initialization'!
initWithOwner: aName balance: anAmount rate: aRate
    self initWithOwner: aName balance: anAmount.
    interestRate := aRate.
    withdrawalCount := 0.
    maxWithdrawals := 6.
    ^self
! !

!SavingsAccount methodsFor: 'operations'!
withdraw: amount
    withdrawalCount >= maxWithdrawals
        ifTrue: [^Error signal: 'Monthly withdrawal limit reached'].
    withdrawalCount := withdrawalCount + 1.
    ^super withdraw: amount
!

applyInterest
    | interest |
    interest := balance * interestRate.
    balance := balance + interest.
    self recordTransaction: 'INT' amount: interest
         description: ('Interest @ ', (interestRate * 100) printString, '%').
    ^interest
!

resetWithdrawalCount
    withdrawalCount := 0
! !

!SavingsAccount methodsFor: 'accessing'!
interestRate     ^interestRate!
withdrawalCount  ^withdrawalCount! !

"────── CHECKING ACCOUNT (subclass) ─────────────────────────────"
BankAccount subclass: #CheckingAccount
    instanceVariableNames: 'overdraftLimit overdraftFee'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'AcmeBank'!

!CheckingAccount methodsFor: 'initialization'!
initWithOwner: aName balance: anAmount overdraft: limit
    self initWithOwner: aName balance: anAmount.
    overdraftLimit := limit.
    overdraftFee := 35.
    ^self
! !

!CheckingAccount methodsFor: 'operations'!
withdraw: amount
    | available |
    available := balance + overdraftLimit.
    amount > available
        ifTrue: [^Error signal: 'Exceeds overdraft limit'].
    balance := balance - amount.
    balance < 0 ifTrue: [
        "Charge overdraft fee"
        balance := balance - overdraftFee.
        self recordTransaction: 'FEE' amount: overdraftFee negated
             description: 'Overdraft fee'.
    ].
    self recordTransaction: 'WDR' amount: amount negated description: 'Withdrawal'.
    ^balance
! !

"────── BANK CLASS (Manages accounts) ───────────────────────────"
Object subclass: #Bank
    instanceVariableNames: 'name accounts'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'AcmeBank'!

!Bank methodsFor: 'initialization'!
initWithName: aName
    name := aName.
    accounts := Dictionary new.
    ^self
! !

!Bank methodsFor: 'account management'!
openSavings: ownerName balance: amount rate: rate
    | acct |
    acct := SavingsAccount new initWithOwner: ownerName balance: amount rate: rate.
    accounts at: acct accountNumber put: acct.
    ^acct
!

openChecking: ownerName balance: amount overdraft: limit
    | acct |
    acct := CheckingAccount new initWithOwner: ownerName balance: amount overdraft: limit.
    accounts at: acct accountNumber put: acct.
    ^acct
!

findAccount: acctNum
    ^accounts at: acctNum ifAbsent: [nil]
! !

!Bank methodsFor: 'reporting'!
printReport
    | totalBalance |
    totalBalance := 0.
    Transcript showCr: '══════════════════════════════════════════'.
    Transcript showCr: name, ' — ALL ACCOUNTS'.
    Transcript showCr: '══════════════════════════════════════════'.
    accounts do: [:acct |
        acct printSummary.
        totalBalance := totalBalance + acct balance.
    ].
    Transcript showCr: '──────────────────────────────────────────'.
    Transcript showCr: 'Total accounts: ', accounts size printString.
    Transcript showCr: 'Total deposits: $', totalBalance printString.
! !

"════════════════════════════════════════════════════════════════"
"  DEMO — Run after loading the above class definitions         "
"════════════════════════════════════════════════════════════════"

| bank alice bob carol savAcct chkAcct |

BankAccount initialize.

bank := Bank new initWithName: 'Acme National Bank'.

"Open accounts"
savAcct := bank openSavings: 'Alice Smith'  balance: 5000 rate: 0.025.
chkAcct := bank openChecking: 'Bob Johnson' balance: 1500 overdraft: 500.
        bank openSavings: 'Carol White'  balance: 10000 rate: 0.030.

"Perform transactions on Alice's savings"
savAcct deposit: 2000.
savAcct deposit: 500.
savAcct withdraw: 750.
savAcct applyInterest.

"Bob's checking"
chkAcct deposit: 300.
chkAcct withdraw: 200.
chkAcct withdraw: 1500.   "Goes into overdraft"

"Print statements"
savAcct printStatement.
chkAcct printStatement.

"Transfer between accounts"
Transcript showCr: 'Transfer $200 from Alice to Bob...'.
[savAcct transfer: 200 to: chkAcct]
    on: Error
    do: [:e | Transcript showCr: 'Transfer error: ', e messageText].

"Bank summary report"
bank printReport.

"Collections demo — find high-balance accounts"
Transcript showCr: ''.
Transcript showCr: 'High balance accounts (>$5000):'.
bank accounts do: [:acct |
    acct balance > 5000
        ifTrue: [acct printSummary]
].
