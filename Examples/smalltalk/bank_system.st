"================================================================"
"  SMALLTALK BANK ACCOUNT SYSTEM"
"  Demonstrates OOP, message passing, collections."
"================================================================"

Transcript showCr: '================================================================'.
Transcript showCr: '  SMALLTALK BANK ACCOUNT SYSTEM'.
Transcript showCr: '================================================================'.
Transcript showCr: ''.

Transcript showCr: '--- Account Summary ---'.

| acct1Balance acct2Balance acct3Balance |
acct1Balance := 1500.
acct2Balance := 3000.
acct3Balance := 10000.

Transcript showCr: 'Account 1: Alice'.
Transcript showCr: '  Balance: $' , acct1Balance printString.
Transcript showCr: 'Account 2: Bob'.
Transcript showCr: '  Balance: $' , acct2Balance printString.
Transcript showCr: 'Account 3: Carol'.
Transcript showCr: '  Balance: $' , acct3Balance printString.
Transcript showCr: ''.

Transcript showCr: '--- Transactions ---'.
| deposit withdrawal newBalance |
deposit := 500.
withdrawal := 200.
newBalance := acct1Balance + deposit.
Transcript showCr: 'Alice deposits $' , deposit printString.
Transcript showCr: 'New balance: $' , newBalance printString.
newBalance := newBalance - withdrawal.
Transcript showCr: 'Alice withdraws $' , withdrawal printString.
Transcript showCr: 'New balance: $' , newBalance printString.
Transcript showCr: ''.

Transcript showCr: '--- Interest (5%) ---'.
| interestRate interest totalSavings |
interestRate := 0.05.
interest := acct2Balance * interestRate.
totalSavings := acct2Balance + interest.
Transcript showCr: 'Savings: $' , acct2Balance printString.
Transcript showCr: 'Interest: $' , interest printString.
Transcript showCr: 'Total: $' , totalSavings printString.
Transcript showCr: ''.

Transcript showCr: '--- Transaction History ---'.
| history |
history := OrderedCollection new.
history add: 'Deposit $500'.
history add: 'Withdrawal $200'.
history add: 'Interest $150'.
history do: [:t | Transcript showCr: '  ' , t].
Transcript showCr: ''.

Transcript showCr: '--- Account Status ---'.
| minBalance |
minBalance := 1000.
(newBalance >= minBalance)
    ifTrue: [ Transcript showCr: 'Status: GOOD STANDING' ]
    ifFalse: [ Transcript showCr: 'Status: LOW BALANCE WARNING' ].
Transcript showCr: ''.

Transcript showCr: '--- Portfolio Summary ---'.
| total numAccounts |
total := acct1Balance + acct2Balance + acct3Balance.
numAccounts := 3.
Transcript showCr: 'Total accounts: ' , numAccounts printString.
Transcript showCr: 'Total portfolio value: $' , total printString.
Transcript showCr: ''.

Transcript showCr: '--- 5-Year Compound Interest (Carol, 7%) ---'.
| principalBalance year rate |
principalBalance := acct3Balance.
rate := 0.07.
year := 1.
[year <= 5] whileTrue: [
    principalBalance := principalBalance + (principalBalance * rate).
    Transcript showCr: 'Year ' , year printString , ': $' , principalBalance printString.
    year := year + 1.
].
Transcript showCr: ''.
Transcript showCr: 'Bank System Complete.'.
