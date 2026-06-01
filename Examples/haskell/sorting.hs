-- Sorting algorithms in Haskell
-- Quicksort, merge sort, insertion sort

module Main where

import Data.List (intercalate)

-- ── Quicksort ──────────────────────────────────────────────────────────
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  quicksort smaller ++ [x] ++ quicksort larger
  where
    smaller = filter (<= x) xs
    larger  = filter (>  x) xs

-- ── Merge sort ────────────────────────────────────────────────────────
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs  = merge (mergesort left) (mergesort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- ── Insertion sort ────────────────────────────────────────────────────
insertionsort :: Ord a => [a] -> [a]
insertionsort = foldr insert []
  where
    insert x [] = [x]
    insert x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : insert x ys

-- ── Show results ──────────────────────────────────────────────────────
showList' :: Show a => [a] -> String
showList' xs = "[" ++ intercalate ", " (map show xs) ++ "]"

main :: IO ()
main = do
  let unsorted = [64, 25, 12, 22, 11, 90, 3, 77] :: [Int]
  putStrLn "=== Haskell Sorting Algorithms ==="
  putStrLn $ "Unsorted:       " ++ showList' unsorted
  putStrLn $ "Quicksort:      " ++ showList' (quicksort unsorted)
  putStrLn $ "Merge sort:     " ++ showList' (mergesort unsorted)
  putStrLn $ "Insertion sort: " ++ showList' (insertionsort unsorted)

  putStrLn "\n=== Sorting strings ==="
  let words' = ["banana", "apple", "cherry", "date", "elderberry"]
  putStrLn $ "Unsorted: " ++ showList' words'
  putStrLn $ "Sorted:   " ++ showList' (quicksort words')
