-- Higher-order functions in Haskell
-- Demonstrates map, filter, fold, function composition, and currying

module Main where

-- Apply a function n times
applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n - 1) f (f x)

-- Compose a list of functions
composeAll :: [a -> a] -> a -> a
composeAll = foldr (.) id

main :: IO ()
main = do
  putStrLn "=== Function Composition ==="
  let double  = (*2)
  let addThree = (+3)
  let square  = \x -> x * x
  let f = double . addThree . square
  putStrLn ("(double . addThree . square) 5 = " ++ show (f 5))
  putStrLn ("applyN 3 double 1 = " ++ show (applyN 3 double 1))

  putStrLn "\n=== Currying and Partial Application ==="
  let add     = \a b -> a + b
  let add5    = add 5
  let times3  = (*3)
  let nums    = [1..8]
  putStrLn ("map (add5)  [1..8] = " ++ show (map add5 nums))
  putStrLn ("map (times3)[1..8] = " ++ show (map times3 nums))
  putStrLn ("map (^2)    [1..8] = " ++ show (map (^2) nums))

  putStrLn "\n=== flip, const, id ==="
  let sub  = \a b -> a - b
  putStrLn ("sub  10 3 = " ++ show (sub 10 3))
  putStrLn ("flip sub 3 10 = " ++ show (flip sub 3 10))
  putStrLn ("map (const 0) [1..5] = " ++ show (map (const 0) [1..5]))
  putStrLn ("map id [1..5] = " ++ show (map id [1..5]))

  putStrLn "\n=== ZipWith ==="
  let a = [1..5]
  let b = [10,20..50]
  putStrLn ("a:          " ++ show a)
  putStrLn ("b:          " ++ show b)
  putStrLn ("zipWith (+) = " ++ show (zipWith (+) a b))
  putStrLn ("zipWith (*) = " ++ show (zipWith (*) a b))
  putStrLn ("zipWith max = " ++ show (zipWith max a b))

  putStrLn "\n=== Any, All, Elem ==="
  let lst = [2,4,6,8,11,12]
  putStrLn ("List: " ++ show lst)
  putStrLn ("any odd  = " ++ show (any odd lst))
  putStrLn ("all even = " ++ show (all even lst))
  putStrLn ("elem 11  = " ++ show (elem 11 lst))
  putStrLn ("elem 5   = " ++ show (elem 5 lst))

  putStrLn "\n=== Scan (using iterate) ==="
  let runningSums = take 10 (map (sum . flip take [1..]) [1..])
  putStrLn ("Running sums 1-10: " ++ show runningSums)
