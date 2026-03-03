# Lua Programming Tutorial

Lua is a lightweight, embeddable scripting language created in 1993 at PUC-Rio. It is famous for its speed, simplicity, and use in game engines and embedded systems.

## Hello World

```lua
print("Hello from Lua!")
print("Welcome to Time Warp Studio")
```

## Variables and Types

```lua
-- Local variables (preferred)
local name   = "Alice"
local age    = 30
local height = 1.75
local flag   = true

print(name, age, height, flag)

-- String operations
print("Length: " .. #name)
print("Upper:  " .. string.upper(name))
print("Lower:  " .. string.lower(name))
print("Sub:    " .. string.sub(name, 1, 3))

-- Type inspection
print(type(name))    -- string
print(type(age))     -- number
print(type(flag))    -- boolean
print(type(nil))     -- nil
```

## Control Flow

```lua
-- If / elseif / else
local x = 42
if x > 100 then
    print("large")
elseif x > 50 then
    print("medium")
elseif x > 0 then
    print("small")
else
    print("non-positive")
end

-- While loop
local n = 1
while n <= 32 do
    io.write(n .. " ")
    n = n * 2
end
print()

-- Repeat ... until
local i = 1
repeat
    io.write(i .. " ")
    i = i + 1
until i > 10
print()

-- Numeric for
for i = 1, 10 do
    io.write(i .. " ")
end
print()

-- Generic for (ipairs, pairs)
local fruits = {"apple", "banana", "cherry"}
for idx, v in ipairs(fruits) do
    print(idx, v)
end
```

## Functions

```lua
-- Basic function
local function greet(who)
    return "Hello, " .. who .. "!"
end
print(greet("World"))

-- Multiple return values
local function minmax(t)
    local mn, mx = t[1], t[1]
    for _, v in ipairs(t) do
        if v < mn then mn = v end
        if v > mx then mx = v end
    end
    return mn, mx
end

local lo, hi = minmax({3, 1, 4, 1, 5, 9, 2, 6})
print("min=" .. lo .. " max=" .. hi)

-- Recursive function
local function fib(n)
    if n <= 1 then return n end
    return fib(n-1) + fib(n-2)
end

for i = 0, 10 do
    io.write(fib(i) .. " ")
end
print()
```

## Tables

Tables are Lua's only data structure — they serve as arrays, dicts, objects, and modules.

```lua
-- Array-style
local primes = {2, 3, 5, 7, 11, 13}
for _, p in ipairs(primes) do
    io.write(p .. " ")
end
print()

-- Dictionary-style
local person = {
    name = "Bob",
    age  = 25,
    city = "Retro City",
}
for k, v in pairs(person) do
    print(k .. " = " .. tostring(v))
end

-- Nested
local matrix = {{1,2,3},{4,5,6},{7,8,9}}
print(matrix[2][3])   -- 6
```

## Closures

```lua
local function make_adder(n)
    return function(x) return x + n end
end

local add10 = make_adder(10)
print(add10(5))   -- 15
print(add10(20))  -- 30
```

## Further Reading

- [Examples/lua/](../Examples/lua/) — 5 Lua example programs
- [Language Guide: Lua](LANGUAGE_GUIDE.md#lua)
