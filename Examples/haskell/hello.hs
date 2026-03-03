-- Haskell Comprehensive Demo - Time Warp Studio

-- Helper functions
double x = x * 2
square x = x * x
fact n = if n <= 1 then 1 else n * fact (n - 1)
fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)

main = do
  putStrLn "===== HELLO WORLD ====="
  putStrLn "Welcome to Haskell!"
  putStrLn "===== VARIABLES ====="
  let x = 10
  let y = 3
  let name = "Haskell"
  print x
  print y
  putStrLn name
  putStrLn "===== ARITHMETIC ====="
  print (x + y)
  print (x - y)
  print (x * y)
  print (div x y)
  print (mod x y)
  print (2 ^ 10)
  putStrLn "===== STRINGS ====="
  putStrLn ("Hello" ++ " " ++ "World")
  print (length "Hello")
  print (head "Hello")
  print (tail "Hello")
  print (reverse "Hello")
  putStrLn "===== CONDITIONALS ====="
  putStrLn (if x > 5 then "x is greater than 5" else "x is not greater than 5")
  putStrLn (if y == 3 then "y equals 3" else "y does not equal 3")
  putStrLn "===== FUNCTIONS ====="
  print (double 5)
  print (square 7)
  print (fact 5)
  print (fact 10)
  putStrLn "===== LAMBDA ====="
  print ((\x -> x * 3) 7)
  print (map (\x -> x + 1) [1, 2, 3])
  putStrLn "===== LISTS ====="
  print [1, 2, 3, 4, 5]
  print (head [1, 2, 3])
  print (tail [1, 2, 3])
  print (length [1, 2, 3, 4])
  print (sum [1, 2, 3, 4, 5])
  print (product [1, 2, 3, 4])
  print (reverse [1, 2, 3])
  print [1..5]
  print (take 3 [1, 2, 3, 4, 5])
  print (drop 2 [1, 2, 3, 4, 5])
  putStrLn "===== HIGHER-ORDER ====="
  print (map double [1, 2, 3, 4])
  print (filter (> 3) [1, 2, 3, 4, 5])
  print (foldl (+) 0 [1, 2, 3, 4])
  putStrLn "===== MATH ====="
  print (sqrt 16)
  print (abs (-5))
  print (min 3 7)
  print (max 3 7)
  putStrLn "===== DONE ====="
