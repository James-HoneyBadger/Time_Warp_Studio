# REXX Programming Tutorial

REXX (Restructured Extended Executor) was designed by Mike Cowlishaw at IBM in 1979. It is a powerful, human-readable scripting language, the standard shell of IBM mainframes and OS/2.

## Hello World

```rexx
/* Hello World in REXX */
say "Hello from REXX!"
say "Welcome to Time Warp Studio"
```

## Variables and Types

```rexx
/* REXX is dynamically typed; all values are strings */
name   = "Alice"
age    = 30
salary = 75000.50
flag   = 1    /* REXX uses 1/0 for true/false */

say "Name:"   name
say "Age:"    age
say "Salary:" salary

/* String operations */
greeting = "Hello, " || name || "!"
say greeting
say length(greeting)
say translate(name)          /* UPPER CASE */
say substr(name, 1, 3)       /* Ali */
say pos("li", name)          /* 2 */
say copies("ha", 3)          /* hahaha */
```

## Arithmetic

```rexx
say 10 + 3      /* 13   */
say 10 - 3      /* 7    */
say 10 * 3      /* 30   */
say 10 / 3      /* frac */
say 10 % 3      /* 3  integer divide */
say 10 // 3     /* 1  remainder */
say 2 ** 10     /* 1024 */

say abs(-42)
say max(4, 9, 2, 7)
say min(4, 9, 2, 7)
say trunc(3.7)
say format(3.14159, 1, 2)   /* 3.14 */
```

## Control Flow

```rexx
/* IF / THEN / ELSE / END */
score = 85
if score >= 90 then
    say "Grade A"
else if score >= 80 then
    say "Grade B"
else
    say "Grade C or below"

/* DO loop (counted) */
do i = 1 to 10
    say i
end

/* DO WHILE */
n = 1
do while n <= 64
    call charout , n " "
    n = n * 2
end
say ""

/* DO UNTIL */
total = 0
i = 1
do until total >= 100
    total = total + i
    i = i + 1
end
say "Total:" total "reached in" i-1 "steps"

/* SELECT (like switch) */
day = "MON"
select
    when day = "MON" then say "Monday"
    when day = "FRI" then say "Friday"
    when day = "SAT" | day = "SUN" then say "Weekend"
    otherwise say "Midweek"
end
```

## Subroutines and Functions

```rexx
/* Call a subroutine */
call greet "World"
call greet "REXX"

/* Call a function (returns value) */
say factorial(10)
say fibonacci(10)
exit

greet: procedure
    parse arg who
    say "Hello, " || who || "!"
    return

factorial: procedure
    parse arg n
    if n <= 1 then return 1
    return n * factorial(n - 1)

fibonacci: procedure
    parse arg n
    if n <= 1 then return n
    return fibonacci(n-1) + fibonacci(n-2)
```

## String Parsing

```rexx
/* REXX's crown jewel: PARSE */
fullname = "James Temple"
parse var fullname first last
say "First:" first
say "Last:"  last

/* Parse with template */
datestr = "2026-01-15"
parse var datestr year "-" month "-" day
say "Year:" year "Month:" month "Day:" day

/* Parse numeric tokens */
line = "  42   3.14   99  "
parse var line a b c
say a + b + c
```

## Arrays (Compound Variables / Stems)

```rexx
/* REXX uses stem variables as arrays */
fruits.1 = "Apple"
fruits.2 = "Banana"
fruits.3 = "Cherry"
fruits.0 = 3      /* convention: .0 = count */

do i = 1 to fruits.0
    say i fruits.i
end
```

## Further Reading

- [Examples/rexx/](../Examples/rexx/) — 5 REXX example programs
- [Language Guide: REXX](LANGUAGE_GUIDE.md#rexx)
