# Haskell Tutorial

## Introduction

Haskell (1990) is a purely functional, lazily evaluated language with a powerful static type system. It enforces mathematical purity — functions have no side effects and always return the same output for the same input. Haskell is widely used in academia and for high-assurance software.

**Key characteristics:**
- **Pure**: no side effects (I/O wrapped in `IO` monad)
- **Lazy**: expressions are evaluated only when needed
- **Strong static typing**: type errors caught at compile time
- **Type inference**: types deduced automatically
- **Pattern matching**: elegant deconstruction of data

## Hello World

```haskell
main :: IO ()
main = putStrLn "Hello, World!"
```

## Variables (Immutable Bindings)

```haskell
-- Top-level definitions (immutable)
x :: Int
x = 42

name :: String
name = "Alice"

pi' :: Double
pi' = 3.14159

main :: IO ()
main = do
    print x
    putStrLn name
    print pi'
```

`::` declares a type. All bindings are immutable.

## Arithmetic

```haskell
main :: IO ()
main = do
    print (3 + 4)          -- 7
    print (10 - 3)         -- 7
    print (6 * 7)          -- 42
    print (10 `div` 3)     -- 3  (integer division)
    print (10 `mod` 3)     -- 1
    print (2 ^ 10)         -- 1024
    print (abs (-7))       -- 7
    print (sqrt 16.0)      -- 4.0
    print (fromIntegral 5 :: Double)  -- 5.0
```

## Functions

```haskell
-- Type signature then definition
square :: Int -> Int
square n = n * n

-- Currying: all functions take one argument
add :: Int -> Int -> Int
add x y = x + y

add5 :: Int -> Int
add5 = add 5          -- partial application

-- Guards
classify :: Int -> String
classify n
    | n < 0    = "negative"
    | n == 0   = "zero"
    | n < 10   = "small"
    | otherwise = "large"

main :: IO ()
main = do
    print (square 7)       -- 49
    print (add5 3)         -- 8
    putStrLn (classify 42) -- large
```

## Pattern Matching

```haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Pattern match on lists
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- Pattern match with tuples
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

main :: IO ()
main = do
    print (factorial 6)       -- 720
    print (myLength [1,2,3])  -- 3
    print (fst3 (1, 2, 3))    -- 1
```

## Lists

```haskell
main :: IO ()
main = do
    let lst = [1, 2, 3, 4, 5]
    
    print (head lst)           -- 1
    print (tail lst)           -- [2,3,4,5]
    print (last lst)           -- 5
    print (init lst)           -- [1,2,3,4]
    print (length lst)         -- 5
    print (reverse lst)        -- [5,4,3,2,1]
    print (take 3 lst)         -- [1,2,3]
    print (drop 3 lst)         -- [4,5]
    print (sum lst)            -- 15
    print (product lst)        -- 120
    print (maximum lst)        -- 5
    print (minimum lst)        -- 1
    
    -- Ranges
    print [1..5]               -- [1,2,3,4,5]
    print [1,3..10]            -- [1,3,5,7,9]
    
    -- Cons operator
    print (0 : lst)            -- [0,1,2,3,4,5]
    
    -- Concatenation
    print ([1,2] ++ [3,4])     -- [1,2,3,4]
```

## List Comprehensions

```haskell
main :: IO ()
main = do
    let squares = [n * n | n <- [1..5]]
    let evens   = [n | n <- [1..10], even n]
    let pairs   = [(x, y) | x <- [1..3], y <- [1..3], x /= y]
    let pythag  = [(a,b,c) | c <- [1..20],
                              a <- [1..c],
                              b <- [a..c],
                              a^2 + b^2 == c^2]
    
    print squares   -- [1,4,9,16,25]
    print evens     -- [2,4,6,8,10]
    print pythag    -- [(3,4,5),(5,12,13),...]
```

## Higher-Order Functions

```haskell
main :: IO ()
main = do
    print (map (*2) [1..5])              -- [2,4,6,8,10]
    print (filter even [1..10])          -- [2,4,6,8,10]
    print (foldl (+) 0 [1..10])          -- 55
    print (foldr (:) [] [1..5])          -- [1,2,3,4,5]
    print (zipWith (+) [1,2,3] [10,20,30])  -- [11,22,33]
    print (takeWhile (<5) [1..10])       -- [1,2,3,4]
    print (dropWhile (<5) [1..10])       -- [5,6,7,8,9,10]
    print (any even [1,3,5,6])           -- True
    print (all odd [1,3,5,7])            -- True
```

## Lambda Expressions

```haskell
main :: IO ()
main = do
    let double = \x -> x * 2
    let add    = \x y -> x + y
    
    print (double 5)         -- 10
    print (map (\n -> n^2) [1..5])  -- [1,4,9,16,25]
    
    -- Function composition with (.)
    let doubleSquare = double . square
    print (map doubleSquare [1..5])  -- [2,8,18,32,50]
  where
    square n = n * n
```

## Data Types

```haskell
-- Algebraic data types
data Shape = Circle Double
           | Rectangle Double Double
           | Triangle Double Double Double

area :: Shape -> Double
area (Circle r)       = pi * r * r
area (Rectangle w h)  = w * h
area (Triangle a b c) = 
    let s = (a + b + c) / 2
    in sqrt (s * (s-a) * (s-b) * (s-c))

main :: IO ()
main = do
    print (area (Circle 5))          -- 78.53...
    print (area (Rectangle 4 6))     -- 24.0
```

## Quick Reference

| Feature | Syntax |
|---------|--------|
| Print | `putStrLn str` / `print val` |
| Bind | `let x = expr` (in do-block) |
| Function def | `f x y = expr` |
| Lambda | `\x -> expr` |
| Pattern match | `f [] = ...` / `f (x:xs) = ...` |
| Guards | `\| cond = expr` |
| List range | `[1..10]` |
| List comp | `[f x \| x <- lst, pred x]` |
| Map | `map f lst` |
| Filter | `filter pred lst` |
| Fold | `foldl f acc lst` |
| Composition | `f . g` |
| Type annotation | `expr :: Type` |
