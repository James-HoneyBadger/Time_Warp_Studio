-- ============================================================
-- FUNCTIONAL MASTERPIECE — Haskell Showcase
-- Expression Parser * Type Classes * Church Numerals * Streams
-- Time Warp Studio — Haskell Language Demo
-- ============================================================

module Main where

import Data.List  (sort, nub, group, intercalate, isPrefixOf, tails)
import Data.Char  (digitToInt, isDigit, isAlpha, toLower, toUpper)
import qualified Data.Map.Strict as Map

-- ===== SECTION 1: ARITHMETIC EXPRESSION PARSER =====
-- A complete recursive-descent evaluator
-- Supports: +, -, *, /, ^, parentheses, unary minus

data Expr
    = Num  Double
    | BinOp Char Expr Expr
    | UnaryMinus Expr

evalExpr :: Expr -> Double
evalExpr (Num n)          = n
evalExpr (UnaryMinus e)   = negate (evalExpr e)
evalExpr (BinOp '+' l r)  = evalExpr l + evalExpr r
evalExpr (BinOp '-' l r)  = evalExpr l - evalExpr r
evalExpr (BinOp '*' l r)  = evalExpr l * evalExpr r
evalExpr (BinOp '/' l r)  = evalExpr l / evalExpr r
evalExpr (BinOp '^' l r)  = evalExpr l ** evalExpr r
evalExpr (BinOp _ _ _)    = 0

-- Tokenizer
data Token = TNum Double | TOp Char | TLParen | TRParen deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize []       = []
tokenize (c:cs)
    | c == ' '    = tokenize cs
    | c == '('    = TLParen  : tokenize cs
    | c == ')'    = TRParen  : tokenize cs
    | c `elem` "+-*/^" = TOp c : tokenize cs
    | isDigit c || c == '.'  =
        let (num, rest) = span (\x -> isDigit x || x == '.') (c:cs)
        in  TNum (read num) : tokenize rest
    | otherwise   = tokenize cs

-- Recursive descent parser (precedence climbing)
parseExpr :: [Token] -> (Expr, [Token])
parseExpr tokens = parseAddSub tokens

parseAddSub :: [Token] -> (Expr, [Token])
parseAddSub tokens =
    let (left, rest) = parseMulDiv tokens
    in  parseAddSub' left rest

parseAddSub' :: Expr -> [Token] -> (Expr, [Token])
parseAddSub' left (TOp '+' : rest) =
    let (right, rest') = parseMulDiv rest
    in  parseAddSub' (BinOp '+' left right) rest'
parseAddSub' left (TOp '-' : rest) =
    let (right, rest') = parseMulDiv rest
    in  parseAddSub' (BinOp '-' left right) rest'
parseAddSub' left tokens = (left, tokens)

parseMulDiv :: [Token] -> (Expr, [Token])
parseMulDiv tokens =
    let (left, rest) = parsePower tokens
    in  parseMulDiv' left rest

parseMulDiv' :: Expr -> [Token] -> (Expr, [Token])
parseMulDiv' left (TOp '*' : rest) =
    let (right, rest') = parsePower rest
    in  parseMulDiv' (BinOp '*' left right) rest'
parseMulDiv' left (TOp '/' : rest) =
    let (right, rest') = parsePower rest
    in  parseMulDiv' (BinOp '/' left right) rest'
parseMulDiv' left tokens = (left, tokens)

parsePower :: [Token] -> (Expr, [Token])
parsePower tokens =
    let (base, rest) = parseUnary tokens
    in  case rest of
          (TOp '^' : rest') ->
              let (exp_, rest'') = parsePower rest'
              in  (BinOp '^' base exp_, rest'')
          _ -> (base, rest)

parseUnary :: [Token] -> (Expr, [Token])
parseUnary (TOp '-' : rest) =
    let (e, rest') = parsePrimary rest
    in  (UnaryMinus e, rest')
parseUnary tokens = parsePrimary tokens

parsePrimary :: [Token] -> (Expr, [Token])
parsePrimary (TNum n : rest)  = (Num n, rest)
parsePrimary (TLParen : rest) =
    let (e, rest') = parseExpr rest
    in  case rest' of
          (TRParen : rest'') -> (e, rest'')
          _                  -> (e, rest')
parsePrimary tokens = (Num 0, tokens)

calculate :: String -> Double
calculate s = fst (parseExpr (tokenize s))

-- ===== SECTION 2: TYPE CLASS SHOWCASE =====

class Describable a where
    describe :: a -> String

data Shape = Circle Double
           | Rectangle Double Double
           | Triangle Double Double Double

instance Describable Shape where
    describe (Circle r)       = "Circle(r=" ++ show r ++ ")"
    describe (Rectangle w h)  = "Rect(" ++ show w ++ "x" ++ show h ++ ")"
    describe (Triangle a b c) = "Triangle(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"

class HasArea a where
    area :: a -> Double

class HasPerimeter a where
    perimeter :: a -> Double

instance HasArea Shape where
    area (Circle r)       = pi * r * r
    area (Rectangle w h)  = w * h
    area (Triangle a b c) = let s = (a+b+c)/2
                             in  sqrt (s*(s-a)*(s-b)*(s-c))

instance HasPerimeter Shape where
    perimeter (Circle r)       = 2 * pi * r
    perimeter (Rectangle w h)  = 2 * (w + h)
    perimeter (Triangle a b c) = a + b + c

-- ===== SECTION 3: CHURCH NUMERALS =====
-- Encoding natural numbers as higher-order functions

type Church = forall a. (a -> a) -> a -> a

zero  :: (a -> a) -> a -> a
zero  f x = x

one   :: (a -> a) -> a -> a
one   f x = f x

two   :: (a -> a) -> a -> a
two   f x = f (f x)

three :: (a -> a) -> a -> a
three f x = f (f (f x))

churchToInt :: ((Int -> Int) -> Int -> Int) -> Int
churchToInt n = n (+1) 0

churchAdd :: ((a -> a) -> a -> a)
          -> ((a -> a) -> a -> a)
          -> (a -> a) -> a -> a
churchAdd m n f x = m f (n f x)

churchMul :: ((a -> a) -> a -> a)
          -> ((a -> a) -> a -> a)
          -> (a -> a) -> a -> a
churchMul m n = m . n

-- ===== SECTION 4: INFINITE STREAMS =====

-- Sieve of Eratosthenes as an infinite list
primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve []     = []

-- Fibonacci stream
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Pascal's triangle as infinite list of rows
pascal :: [[Integer]]
pascal = iterate nextRow [1]
  where
    nextRow row = zipWith (+) (0:row) (row ++ [0])

-- Collatz sequence
collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n
    | even n    = n : collatz (n `div` 2)
    | otherwise = n : collatz (3 * n + 1)

-- ===== SECTION 5: HIGHER-ORDER FUNCTIONS =====

-- Composition pipeline
applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n-1) f (f x)

-- Fixed-point combinator (Y combinator in Haskell)
fix :: (a -> a) -> a
fix f = let x = f x in x

factFix :: Int -> Int
factFix = fix (\rec n -> if n <= 1 then 1 else n * rec (n-1))

-- ===== SECTION 6: DATA PROCESSING =====

type FreqMap = Map.Map Char Int

letterFreq :: String -> FreqMap
letterFreq = foldr (\c m -> Map.insertWith (+) c 1 m) Map.empty
           . filter isAlpha
           . map toLower

topN :: Int -> FreqMap -> [(Char, Int)]
topN n = take n . reverse . sort . map (\(k,v) -> (v,k))
       . Map.toList
       >>= \pairs -> return (map (\(v,k) -> (k,v)) pairs)

-- Caesar cipher
caesar :: Int -> String -> String
caesar shift = map encrypt
  where
    encrypt c
        | c >= 'a' && c <= 'z' = toEnum ((fromEnum c - fromEnum 'a' + shift) `mod` 26 + fromEnum 'a')
        | c >= 'A' && c <= 'Z' = toEnum ((fromEnum c - fromEnum 'A' + shift) `mod` 26 + fromEnum 'A')
        | otherwise             = c

-- Run-length encoding
rle :: Eq a => [a] -> [(Int, a)]
rle = map (\g -> (length g, head g)) . group

rleEncode :: String -> String
rleEncode = concatMap (\(n, c) -> if n == 1 then [c] else show n ++ [c]) . rle

-- ===== SECTION 7: LIST COMPREHENSIONS & PATTERNS =====

-- Pythagorean triples
pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = [(a, b, c) | c <- [1..n], b <- [1..c], a <- [1..b],
                              a^2 + b^2 == c^2]

-- Magic squares check (3x3)
isMagic :: [[Int]] -> Bool
isMagic sq =
    let n       = length sq
        target  = sum (head sq)
        rows    = all ((== target) . sum) sq
        cols    = all ((== target) . sum) [[sq !! r !! c | r <- [0..n-1]] | c <- [0..n-1]]
        diag1   = sum [sq !! i !! i         | i <- [0..n-1]] == target
        diag2   = sum [sq !! i !! (n-1-i)   | i <- [0..n-1]] == target
    in  rows && cols && diag1 && diag2

-- ===== MAIN =====

main :: IO ()
main = do
    putStrLn (replicate 60 '=')
    putStrLn "  FUNCTIONAL MASTERPIECE — Haskell Showcase"
    putStrLn "  Parser | Types | Church | Streams | HOF | Patterns"
    putStrLn (replicate 60 '=')

    -- Section 1: Expression Parser
    putStrLn "\n[ 1 ] ARITHMETIC EXPRESSION PARSER"
    let exprs = [ "2 + 3 * 4"
                , "(2 + 3) * 4"
                , "2 ^ 10"
                , "100 / (4 + 6) - 3"
                , "-(3 + 5) * 2"
                , "2 ^ 3 ^ 2"
                ]
    mapM_ (\e -> putStrLn $ "  " ++ e ++ " = " ++ show (calculate e)) exprs

    -- Section 2: Type Classes
    putStrLn "\n[ 2 ] TYPE CLASSES — Polymorphic Geometry"
    let shapes = [Circle 5, Rectangle 4 7, Triangle 3 4 5, Circle 1, Rectangle 10 2]
    mapM_ (\s -> putStrLn $ "  " ++ describe s ++
                             "  area=" ++ show (round (area s) :: Int) ++
                             "  perim=" ++ show (round (perimeter s) :: Int)) shapes

    -- Section 3: Church Numerals
    putStrLn "\n[ 3 ] CHURCH NUMERALS — Lambda Calculus"
    putStrLn $ "  zero  = " ++ show (churchToInt zero)
    putStrLn $ "  one   = " ++ show (churchToInt one)
    putStrLn $ "  two   = " ++ show (churchToInt two)
    putStrLn $ "  three = " ++ show (churchToInt three)
    putStrLn $ "  2+3   = " ++ show (churchToInt (churchAdd two three))
    putStrLn $ "  2*3   = " ++ show (churchToInt (churchMul two three))
    putStrLn $ "  3*3   = " ++ show (churchToInt (churchMul three three))

    -- Section 4: Infinite Streams
    putStrLn "\n[ 4 ] INFINITE LISTS (Lazy Evaluation)"
    putStrLn $ "  First 20 primes:    " ++ show (take 20 primes)
    putStrLn $ "  First 15 Fibonacci: " ++ show (take 15 fibs)
    putStrLn $ "  Pascal's triangle (rows 0-5):"
    mapM_ (\row -> putStrLn ("    " ++ show row)) (take 6 pascal)
    putStrLn $ "  Collatz(27) length: " ++ show (length (collatz 27))
    putStrLn $ "  Collatz(27) max:    " ++ show (maximum (collatz 27))

    -- Section 5: Higher-Order Functions
    putStrLn "\n[ 5 ] HIGHER-ORDER FUNCTIONS"
    putStrLn $ "  applyTwice (+3) 10    = " ++ show (applyTwice (+3) 10)
    putStrLn $ "  applyN 5 (*2) 1       = " ++ show (applyN 5 (*2) 1)
    putStrLn $ "  fix factorial 10      = " ++ show (factFix 10)
    let pipeline = filter even . map (*3) . filter (>5)
    putStrLn $ "  filter even . map *3 . filter >5 $ [1..12]:"
    putStrLn $ "    = " ++ show (pipeline [1..12])

    -- Section 6: Data Processing
    putStrLn "\n[ 6 ] DATA PROCESSING"
    let text = "the quick brown fox jumps over the lazy dog"
    let freq = letterFreq text
    let top5 = take 5 . reverse . sort $ map (\(k,v)->(v,k)) (Map.toList freq)
    putStrLn $ "  Text: \"" ++ text ++ "\""
    putStrLn $ "  Top 5 letters: " ++ show (map (\(v,k)->(k,v)) top5)
    putStrLn $ "  Caesar +13: " ++ caesar 13 text
    putStrLn $ "  RLE encode \"aabbbcccc\": " ++ rleEncode "aabbbcccc"
    putStrLn $ "  RLE encode \"aaabbaaa\": " ++ rleEncode "aaabbaaa"

    -- Section 7: Comprehensions
    putStrLn "\n[ 7 ] LIST COMPREHENSIONS"
    putStrLn $ "  Pythagorean triples ≤ 20: " ++ show (pythagorean 20)

    let loShu = [[2,7,6],[9,5,1],[4,3,8]]
    putStrLn $ "  Lo Shu magic square:\n    " ++ show (loShu !! 0)
    putStrLn $ "    " ++ show (loShu !! 1)
    putStrLn $ "    " ++ show (loShu !! 2)
    putStrLn $ "  Is magic: " ++ show (isMagic loShu)
    putStrLn $ "  Sum of each row/col/diagonal = " ++ show (sum (loShu !! 0))

    -- Summary
    putStrLn $ '\n' : replicate 60 '='
    putStrLn "  Haskell Functional Masterpiece Complete!"
    putStrLn "  Pure functions | Lazy eval | Type classes | Pattern matching"
    putStrLn $ replicate 60 '='
