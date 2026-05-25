-- Recursion patterns in Haskell
-- Demonstrates recursive function definitions and pattern matching

module Main where

-- Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Fibonacci
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Length of a list (custom)
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- Reverse a list (custom)
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Sum of a list (custom)
mySum :: [Int] -> Int
mySum []     = 0
mySum (x:xs) = x + mySum xs

-- Flatten nested list (using concat)
myFlatten :: [[a]] -> [a]
myFlatten []       = []
myFlatten (xs:xss) = xs ++ myFlatten xss

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) =
  let smaller = filter (<= x) xs
      bigger  = filter (> x)  xs
  in  quicksort smaller ++ [x] ++ quicksort bigger

-- Merge sort
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs =
  let (left, right) = splitAt (length xs `div` 2) xs
  in merge (mergesort left) (mergesort right)
  where
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

main :: IO ()
main = do
  putStrLn "=== Factorials ==="
  mapM_ (\n -> putStrLn (show n ++ "! = " ++ show (factorial n))) [0..10]

  putStrLn "\n=== Fibonacci ==="
  let fibs = map fib [0..12]
  putStrLn ("First 13: " ++ show fibs)

  putStrLn "\n=== Custom List Functions ==="
  let lst = [3,1,4,1,5,9,2,6,5,3,5]
  putStrLn ("List:       " ++ show lst)
  putStrLn ("myLength:   " ++ show (myLength lst))
  putStrLn ("myReverse:  " ++ show (myReverse lst))
  putStrLn ("mySum:      " ++ show (mySum lst))
  let nested = [[1,2,3],[4,5],[6,7,8,9]]
  putStrLn ("Flatten:    " ++ show (myFlatten nested))

  putStrLn "\n=== Sorting ==="
  let unsorted = [8,3,7,1,9,2,5,4,6]
  putStrLn ("Unsorted:   " ++ show unsorted)
  putStrLn ("Quicksort:  " ++ show (quicksort unsorted))
  putStrLn ("Mergesort:  " ++ show (mergesort unsorted))

  let strList = ["banana","apple","cherry","date","elderberry"]
  putStrLn ("Words:      " ++ show strList)
  putStrLn ("Sorted:     " ++ show (quicksort strList))
