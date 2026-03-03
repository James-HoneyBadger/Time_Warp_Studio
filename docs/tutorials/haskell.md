# Haskell Programming Tutorial

Haskell is a purely functional, statically typed language with lazy evaluation, created in 1990. It is famous for its powerful type system, elegant syntax, and correctness guarantees.

## Hello World

```haskell
main :: IO ()
main = putStrLn "Hello from Haskell!"
```

## Type Annotations

Every expression has a type. Haskell infers types but you can (and should) annotate:

```haskell
-- Type annotations
x :: Int
x = 42

name :: String
name = "Alice"

pi' :: Double
pi' = 3.14159

flag :: Bool
flag = True
```

## Functions

```haskell
-- Function definition (pattern matching)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Guards
classify :: Int -> String
classify n
    | n < 0    = "negative"
    | n == 0   = "zero"
    | n < 10   = "small"
    | n < 100  = "medium"
    | otherwise = "large"

-- Where clause
circleArea :: Double -> Double
circleArea r = pi * r * r
    where pi = 3.14159265358979

main :: IO ()
main = do
    print (map factorial [0..10])
    mapM_ (putStrLn . classify) [-1, 0, 5, 50, 500]
    print (circleArea 7)
```

## Lists

```haskell
-- List literals and ranges
nums :: [Int]
nums = [1..10]

evens :: [Int]
evens = [2, 4..20]

-- List operations
main :: IO ()
main = do
    print nums
    print (head nums)         -- 1
    print (tail nums)         -- [2..10]
    print (last nums)         -- 10
    print (length nums)       -- 10
    print (reverse nums)      -- [10,9..1]
    print (take 3 nums)       -- [1,2,3]
    print (drop 7 nums)       -- [8,9,10]
    print (sum nums)          -- 55
    print (product [1..5])    -- 120
```

## List Comprehensions

```haskell
main :: IO ()
main = do
    -- Pythagorean triples up to 20
    let triples = [(a,b,c) | c <- [1..20], b <- [1..c], a <- [1..b],
                              a^2 + b^2 == c^2]
    print triples

    -- Sieve of Eratosthenes (simple version)
    let sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
        primes = sieve [2..]
    print (take 20 primes)
```

## Higher-Order Functions

```haskell
main :: IO ()
main = do
    let nums = [1..10]

    -- map, filter, foldl
    print (map (*2) nums)
    print (filter even nums)
    print (foldl (+) 0 nums)

    -- Function composition (.)
    let doubleAndAdd1 = (+1) . (*2)
    print (map doubleAndAdd1 [1..5])

    -- flip, const, id
    print (flip (-) 3 10)      -- 10 - 3 = 7
    print (map (const 0) nums) -- [0,0,0,0,0,0,0,0,0,0]
```

## Maybe Type

```haskell
-- Safe division using Maybe
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

-- Pattern matching on Maybe
describe :: Maybe Int -> String
describe Nothing  = "No result (division by zero)"
describe (Just n) = "Result: " ++ show n

main :: IO ()
main = do
    putStrLn (describe (safeDivide 10 2))
    putStrLn (describe (safeDivide 10 0))
```

## Further Reading

- [Examples/haskell/](../Examples/haskell/) — 10 Haskell example programs
- [Language Guide: Haskell](LANGUAGE_GUIDE.md#haskell)
