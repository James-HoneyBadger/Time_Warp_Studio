# HyperTalk Programming Tutorial

HyperTalk was created by Bill Atkinson at Apple in 1987 as the scripting language for HyperCard. It pioneered natural-language programming — scripts read almost like English sentences.

## Hello World

```hypertalk
on startup
    put "Hello from HyperTalk!"
    put "Welcome to Time Warp Studio"
end startup
```

## Variables and put / get

```hypertalk
-- put assigns a value
put "Alice" into name
put 42 into age
put 3.14 into pi

-- Concatenation with &
put "Hello, " & name & "!" into greeting
put greeting

-- get retrieves a value (stored in 'it')
get length of greeting
put it & " characters"

-- Direct string operations
put the length of "hello"      -- 5
put the reverse of "hello"     -- olleh
put "hello" into x
put char 1 of x                -- h
put char 1 to 3 of x           -- hel
```

## Arithmetic

```hypertalk
put 10 + 3      -- 13
put 10 - 3      -- 7
put 10 * 3      -- 30
put 10 / 3      -- 3.3333...
put 10 ^ 2      -- 100
put 10 mod 3    -- 1
put abs(-42)    -- 42
put sqrt(144)   -- 12
put trunc(3.7)  -- 3
put round(3.5)  -- 4
```

## Conditionals

```hypertalk
put 85 into score

if score >= 90 then
    put "Grade A"
else if score >= 80 then
    put "Grade B"
else if score >= 70 then
    put "Grade C"
else
    put "Below C"
end if
```

## Loops

```hypertalk
-- repeat N times
repeat 5 times
    put "Hello!"
end repeat

-- repeat with counter
repeat with i = 1 to 10
    put i
end repeat

-- repeat while
put 1 into n
repeat while n <= 64
    put n
    put n * 2 into n
end repeat

-- repeat until
put 0 into total
put 1 into i
repeat until total >= 100
    put total + i into total
    put i + 1 into i
end repeat
put "Total: " & total

-- repeat with countdown
repeat with i = 10 down to 1
    put i & "..."
end repeat
put "Liftoff!"
```

## Handlers (Subroutines)

```hypertalk
on greet who
    put "Hello, " & who & "!"
end greet

on run
    greet "World"
    greet "HyperCard"
end run

-- Functions return a value
function factorial n
    if n <= 1 then return 1
    return n * factorial(n - 1)
end factorial

on run
    repeat with i = 0 to 10
        put factorial(i)
    end repeat
end run
```

## String Functions

```hypertalk
put "Hello, World!" into s

put the length of s             -- 13
put the number of chars in s    -- 13
put the number of words in s    -- 2
put word 1 of s                 -- Hello,
put word 2 of s                 -- World!
put char 1 to 5 of s            -- Hello
put s contains "World"          -- true
put offset("World", s)          -- 8
put toUpper(s)                  -- HELLO, WORLD!
put toLower(s)                  -- hello, world!
```

## Lists (Using It Lines)

```hypertalk
-- HyperTalk uses newline-delimited lists
put "Apple" & return & "Banana" & return & "Cherry" into fruits
put the number of lines in fruits   -- 3
put line 1 of fruits                -- Apple
put line 2 of fruits                -- Banana

repeat with i = 1 to number of lines in fruits
    put line i of fruits
end repeat
```

## Further Reading

- [Examples/hypertalk/](../Examples/hypertalk/) — 10 HyperTalk example programs
- [Language Guide: HyperTalk](LANGUAGE_GUIDE.md#hypertalk)
