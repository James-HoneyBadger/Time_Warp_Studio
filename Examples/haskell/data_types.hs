-- Algebraic Data Types in Haskell
-- Custom types, Maybe/Either, pattern matching, type classes

module Main where

-- ── Custom data types ────────────────────────────────────────────────
data Shape
  = Circle Double
  | Rectangle Double Double
  | Triangle Double Double Double
  deriving (Show, Eq)

area :: Shape -> Double
area (Circle r)        = pi * r * r
area (Rectangle w h)   = w * h
area (Triangle a b c)  =
  let s = (a + b + c) / 2
  in sqrt (s * (s-a) * (s-b) * (s-c))

perimeter :: Shape -> Double
perimeter (Circle r)        = 2 * pi * r
perimeter (Rectangle w h)   = 2 * (w + h)
perimeter (Triangle a b c)  = a + b + c

-- ── Maybe chaining ────────────────────────────────────────────────────
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

safeSqrt :: Double -> Maybe Double
safeSqrt x
  | x < 0    = Nothing
  | otherwise = Just (sqrt x)

-- ── Either for error handling ─────────────────────────────────────────
parseAge :: String -> Either String Int
parseAge s =
  case reads s of
    [(n, "")] -> if n >= 0 && n <= 150
                 then Right n
                 else Left $ "Age out of range: " ++ show n
    _ -> Left $ "Not a number: " ++ s

-- ── Linked list ADT ──────────────────────────────────────────────────
data List a = Nil | Cons a (List a) deriving (Show)

toList :: [a] -> List a
toList []     = Nil
toList (x:xs) = Cons x (toList xs)

lengthL :: List a -> Int
lengthL Nil = 0
lengthL (Cons _ rest) = 1 + lengthL rest

-- ── Demo ──────────────────────────────────────────────────────────────
main :: IO ()
main = do
  putStrLn "=== Algebraic Data Types ==="
  let shapes = [Circle 5, Rectangle 4 6, Triangle 3 4 5]
  mapM_ (\s -> putStrLn $ "  " ++ show s ++
                           " — area=" ++ show (round (area s) :: Int) ++
                           ", perimeter=" ++ show (round (perimeter s) :: Int)) shapes

  putStrLn "\n=== Maybe chaining ==="
  print $ safeDiv 10 2
  print $ safeDiv 10 0
  print $ safeSqrt 16.0
  print $ safeSqrt (-1.0)

  putStrLn "\n=== Either error handling ==="
  print $ parseAge "25"
  print $ parseAge "abc"
  print $ parseAge "200"

  putStrLn "\n=== Custom List ADT ==="
  let myList = toList [1..5 :: Int]
  print myList
  putStrLn $ "Length: " ++ show (lengthL myList)
