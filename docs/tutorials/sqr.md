# SQR Programming Tutorial

SQR (Structured Query Reporter) is a programming language developed by SQR Systems (later acquired by PeopleSoft/Oracle) for generating reports from relational databases. It combines SQL with procedural logic for enterprise reporting.

## Program Structure

```sqr
!  Comments start with !
!  An SQR program has sections:

begin-program
    do Main
end-program

procedure Main
    display 'Hello from SQR!'
    display 'Welcome to Time Warp Studio'
end-procedure
```

## Setup / Variables

```sqr
begin-setup
    declare-variable
        local string    $name
        local integer   #count
        local float     #total
        local date      $today
    end-declare
end-setup

begin-program
    do Main
end-program

procedure Main
    let $name  = 'Alice'
    let #count = 42
    let #total = 3.14 * #count
    let $today = strtodate('2026-01-15', 'YYYY-MM-DD')

    display $name
    display #count
    display #total
    display $today
end-procedure
```

Note: SQR conventions — `$` prefix for strings, `#` for numbers, `&` for column values from SQL.

## SQL Query Block

```sqr
begin-select
EMPLOYEE_ID     &eid
FIRST_NAME      &fname
LAST_NAME       &lname
SALARY          &salary

    let #total_salary = #total_salary + &salary
    print &eid    (+1, 1, 10)
    print &fname  (  , 12, 20)
    print &lname  (  , 33, 20)
    print &salary (  , 54, 12) edit '999,999.99'

from employees
where department_id = 10
order by last_name

end-select

display 'Total salary: ' noline
display #total_salary
```

## Print Statements

```sqr
! Print at absolute position  (row, col, length)
print 'Name:'        (1, 1, 10)
print $name          (1, 12, 30)

! Relative row movement
print 'Line 1'       (+1, 1)    ! next line
print 'Line 2'       (+1, 1)
print 'Same line'    ( 0, 20)   ! same row, col 20

! Numeric format edit
print #salary        (+1, 30, 15) edit '$999,999.99'
print #percent       (+1, 30, 10) edit '99.99%'

! Display to stdout (not report)
display 'Debug: ' noline
display #count
```

## Conditionals

```sqr
procedure CheckGrade
    input #score 'Enter score: ' type=integer

    if #score >= 90
        let $grade = 'A'
    else-if #score >= 80
        let $grade = 'B'
    else-if #score >= 70
        let $grade = 'C'
    else
        let $grade = 'F'
    end-if

    display 'Grade: ' noline
    display $grade
end-procedure
```

## Loops

```sqr
procedure CountLoop
    let #i = 1

    while #i <= 10
        display #i
        let #i = #i + 1
    end-while

    do Until (#flag = 'Y')
        display 'Doing work...'
        let #flag = 'Y'
    end-do
end-procedure
```

## Headings and Report Layout

```sqr
begin-heading 4
    print 'Time Warp Studio - Employee Report'    (1, 30)
    print $current-date                           (1, 70)
    print 'Employee ID'                           (3, 1)
    print 'First Name'                            (3, 12)
    print 'Last Name'                             (3, 33)
    print 'Salary'                                (3, 54)
    print '-' (4, 1, 65) fill
end-heading
```

## Procedures and Parameters

```sqr
procedure FormatCurrency
    input #amount   from caller
    input $currency from caller
    output $result  to caller

    if $currency = 'USD'
        let $result = edit(#amount, '$999,999.99')
    else-if $currency = 'EUR'
        let $result = edit(#amount, '€999,999.99')
    else
        let $result = edit(#amount, '999,999.99')
    end-if
end-procedure

procedure Main
    do FormatCurrency(9875.50, 'USD', $formatted)
    display $formatted
end-procedure
```

## Common String Functions

```sqr
let #len     = length($name)
let $upper   = upper($name)
let $lower   = lower($name)
let $trimmed = ltrim(rtrim($name))
let $sub     = substr($name, 1, 5)
let $pos     = instr($name, 'li')
let $rep     = replace($name, 'Alice', 'Bob')
let $concat  = $first_name || ' ' || $last_name
```

## Common Math Functions

```sqr
let #abs  = abs(#value)
let #ceil = ceiling(#value)
let #flr  = floor(#value)
let #rnd  = round(#value, 2)
let #sqr  = sqrt(#value)
let #mod  = mod(#a, #b)
let #max  = max(#a, #b)
let #min  = min(#a, #b)
```

## Further Reading

- [Examples/sqr/](../Examples/sqr/) — 10 SQR example programs
- [Language Guide: SQR](LANGUAGE_GUIDE.md#sqr)
