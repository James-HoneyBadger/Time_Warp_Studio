-- ════════════════════════════════════════════════════════════════
--  HASKELL FUNCTIONAL PROGRAMMING SHOWCASE
--  Demonstrates: type classes, higher-order functions, monads,
--  lazy evaluation, pattern matching, algebraic data types,
--  functors, folds, infinite lists, and IO.
-- ════════════════════════════════════════════════════════════════
module Main where

import Data.List (sort, group, nub, isPrefixOf, intercalate)
import Data.Char (toLower, toUpper, isAlpha, isDigit)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map.Strict as Map

-- ─── ALGEBRAIC DATA TYPES ────────────────────────────────────────

data Shape
    = Circle    { radius :: Double }
    | Rectangle { width :: Double, height :: Double }
    | Triangle  { sideA :: Double, sideB :: Double, sideC :: Double }
    deriving (Show)

area :: Shape -> Double
area (Circle r)        = pi * r * r
area (Rectangle w h)   = w * h
area (Triangle a b c)  =
    let s = (a + b + c) / 2
    in  sqrt (s * (s-a) * (s-b) * (s-c))

perimeter :: Shape -> Double
perimeter (Circle r)       = 2 * pi * r
perimeter (Rectangle w h)  = 2 * (w + h)
perimeter (Triangle a b c) = a + b + c

describeShape :: Shape -> String
describeShape s = show s ++ " → area=" ++ show (round2 (area s))
                  ++ " perimeter=" ++ show (round2 (perimeter s))
  where round2 x = fromIntegral (round (x * 100) :: Int) / 100.0

-- ─── BINARY TREE ─────────────────────────────────────────────────

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show)

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y l r)
    | x < y    = Node y (insert x l) r
    | x > y    = Node y l (insert x r)
    | otherwise = Node y l r

inOrder :: Tree a -> [a]
inOrder Leaf         = []
inOrder (Node x l r) = inOrder l ++ [x] ++ inOrder r

treeHeight :: Tree a -> Int
treeHeight Leaf         = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

-- ─── MAYBE MONAD CHAINING ────────────────────────────────────────

safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeLast :: [a] -> Maybe a
safeLast []  = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

-- Chain multiple safe operations
complexCalc :: Double -> Double -> [Double] -> Maybe Double
complexCalc x y zs = do
    d   <- safeDivide x y
    h   <- safeHead zs
    avg <- safeDivide (d + h) 2
    return avg

-- ─── INFINITE LISTS (Lazy Evaluation) ───────────────────────────

naturals :: [Integer]
naturals = [0..]

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

primes :: [Integer]
primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
        sieve []     = []

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n
    | even n    = n : collatz (n `div` 2)
    | otherwise = n : collatz (3 * n + 1)

-- ─── HIGHER-ORDER FUNCTIONS & FOLDS ─────────────────────────────

myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
    | p x       = x : myFilter p xs
    | otherwise = myFilter p xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ z []     = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

-- Useful derived functions
mySum     :: Num a => [a] -> a
mySum     = myFoldl (+) 0

myProduct :: Num a => [a] -> a
myProduct = myFoldl (*) 1

myReverse :: [a] -> [a]
myReverse = myFoldl (flip (:)) []

myLength  :: [a] -> Int
myLength  = myFoldl (\acc _ -> acc + 1) 0

-- ─── FUNCTION COMPOSITION PIPELINE ──────────────────────────────

wordFrequency :: String -> [(String, Int)]
wordFrequency = Map.toList
    . Map.fromListWith (+)
    . map (\w -> (w, 1))
    . words
    . map toLower
    . filter (\c -> isAlpha c || c == ' ')

topN :: Int -> [(String, Int)] -> [(String, Int)]
topN n = take n . reverse . sort . map (\(w,c) -> (c,w)) >>= \xs ->
    return $ map (\(c,w) -> (w,c)) xs

-- Caesar cipher in Haskell style
caesar :: Int -> String -> String
caesar n = map shift
  where
    shift c
        | 'a' <= c && c <= 'z' = toEnum $ (fromEnum c - 97 + n) `mod` 26 + 97
        | 'A' <= c && c <= 'Z' = toEnum $ (fromEnum c - 65 + n) `mod` 26 + 65
        | otherwise             = c

-- ─── CURRYING & PARTIAL APPLICATION ─────────────────────────────

add :: Int -> Int -> Int
add x y = x + y

multiply :: Int -> Int -> Int
multiply x y = x * y

add5    :: Int -> Int
add5    = add 5

double  :: Int -> Int
double  = multiply 2

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- ─── MAIN ────────────────────────────────────────────────────────

section :: String -> IO ()
section t = do
    putStrLn ""
    putStrLn $ "  ─── " ++ t ++ " ───"

main :: IO ()
main = do
    putStrLn "╔═════════════════════════════════════════╗"
    putStrLn "║    HASKELL FUNCTIONAL PROGRAMMING       ║"
    putStrLn "╚═════════════════════════════════════════╝"

    section "ALGEBRAIC DATA TYPES — Shapes"
    let shapes = [Circle 5, Rectangle 4 6, Triangle 3 4 5]
    mapM_ (putStrLn . ("  " ++) . describeShape) shapes

    section "BINARY SEARCH TREE"
    let nums  = [5,3,7,1,4,6,8,2] :: [Int]
        bst   = fromList nums
        sorted = inOrder bst
    putStrLn $ "  Input:    " ++ show nums
    putStrLn $ "  In-order: " ++ show sorted
    putStrLn $ "  Height:   " ++ show (treeHeight bst)

    section "MAYBE MONAD CHAINING"
    print $ complexCalc 10 2 [4, 8, 12]      -- Just 4.5
    print $ complexCalc 10 0 [4, 8, 12]      -- Nothing (div by 0)
    print $ complexCalc 10 2 []              -- Nothing (empty list)

    section "INFINITE LAZY LISTS"
    putStrLn $ "  First 10 fibonacci: " ++ show (take 10 fibonacci)
    putStrLn $ "  First 10 primes:    " ++ show (take 10 primes)
    putStrLn $ "  Collatz(27) length: " ++ show (length (collatz 27))
    putStrLn $ "  1000th prime:       " ++ show (primes !! 999)

    section "HIGHER-ORDER FUNCTIONS & FOLDS"
    let xs = [1..10] :: [Int]
    putStrLn $ "  mySum [1..10]     = " ++ show (mySum xs)
    putStrLn $ "  myProduct [1..5]  = " ++ show (myProduct [1..5 :: Int])
    putStrLn $ "  myReverse [1..5]  = " ++ show (myReverse [1..5 :: Int])
    putStrLn $ "  filter even [1..10] = " ++ show (myFilter even xs)

    section "FUNCTION COMPOSITION"
    let enc = caesar 13 "Hello, World!"
    putStrLn $ "  ROT13 'Hello, World!' = " ++ enc
    putStrLn $ "  ROT13 again (decode) = " ++ caesar 13 enc

    section "CURRYING & PARTIAL APPLICATION"
    putStrLn $ "  add5 10        = " ++ show (add5 10)
    putStrLn $ "  double 7       = " ++ show (double 7)
    putStrLn $ "  applyTwice double 3 = " ++ show (applyTwice double 3)
    let pipeline = filter even . map (*3) . filter (>5)
    putStrLn $ "  pipeline [1..10] = " ++ show (pipeline ([1..10] :: [Int]))

    putStrLn ""
    putStrLn "  ✓ Haskell showcase complete!"
