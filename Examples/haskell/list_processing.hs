-- List processing in Haskell
-- Demonstrates Prelude list functions, map, filter, fold

module Main where

main :: IO ()
main = do
  putStrLn "=== List Basics ==="
  let nums = [1..10]
  putStrLn ("Range [1..10]: " ++ show nums)
  putStrLn ("Head: "    ++ show (head nums))
  putStrLn ("Tail: "    ++ show (tail nums))
  putStrLn ("Last: "    ++ show (last nums))
  putStrLn ("Init: "    ++ show (init nums))
  putStrLn ("Length: "  ++ show (length nums))
  putStrLn ("Reverse: " ++ show (reverse nums))

  putStrLn "\n=== Map and Filter ==="
  let squares  = map (\x -> x * x) [1..8]
  let evens    = filter even [1..20]
  let odds     = filter odd  [1..20]
  putStrLn ("Squares:   " ++ show squares)
  putStrLn ("Evens:     " ++ show evens)
  putStrLn ("Odds:      " ++ show odds)

  putStrLn "\n=== Folds ==="
  let sumR  = foldr (+) 0 [1..10]
  let prodL = foldl (*) 1 [1..6]
  putStrLn ("Sum 1-10 (foldr):   " ++ show sumR)
  putStrLn ("Product 1-6 (foldl): " ++ show prodL)

  putStrLn "\n=== List Comprehensions ==="
  let pythagorean = [(a,b,c) | c <- [1..20], b <- [1..c], a <- [1..b], a*a + b*b == c*c]
  putStrLn "Pythagorean triples up to 20:"
  mapM_ (putStrLn . ("  " ++) . show) pythagorean

  putStrLn "\n=== Zip and ZipWith ==="
  let letters = ["alpha","beta","gamma","delta","epsilon"]
  let indexed = zip [1..] letters
  mapM_ (\(i,s) -> putStrLn (show i ++ ". " ++ s)) indexed

  putStrLn "\n=== Take and Drop ==="
  let big = [1..30]
  putStrLn ("take 5:    " ++ show (take 5 big))
  putStrLn ("drop 25:   " ++ show (drop 25 big))
  putStrLn ("takeWhile (<10): " ++ show (takeWhile (<10) big))
  putStrLn ("dropWhile (<26): " ++ show (dropWhile (<26) big))
