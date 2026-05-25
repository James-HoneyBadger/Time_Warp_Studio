-- Algorithms in Haskell
-- Number theory, string processing, and combinatorics

module Main where

-- Check if a number is prime
isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | n == 2    = True
  | even n    = False
  | otherwise = all (\i -> n `mod` i /= 0) [3,5..isqrt n]
  where isqrt = floor . sqrt . fromIntegral

-- Primes up to n (sieve via list comprehension)
primesUpTo :: Int -> [Int]
primesUpTo n = filter isPrime [2..n]

-- GCD and LCM
myGcd :: Int -> Int -> Int
myGcd a 0 = abs a
myGcd a b = myGcd b (a `mod` b)

myLcm :: Int -> Int -> Int
myLcm a b = abs (a * b) `div` myGcd a b

-- Collatz sequence
collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
  | even n    = n : collatz (n `div` 2)
  | otherwise = n : collatz (3 * n + 1)

-- Caesar cipher
caesarEncode :: Int -> String -> String
caesarEncode shift = map encodeChar
  where
    encodeChar c
      | c >= 'a' && c <= 'z' =
          toEnum ((fromEnum c - fromEnum 'a' + shift) `mod` 26 + fromEnum 'a')
      | c >= 'A' && c <= 'Z' =
          toEnum ((fromEnum c - fromEnum 'A' + shift) `mod` 26 + fromEnum 'A')
      | otherwise = c

-- Run-length encoding
runLength :: Eq a => [a] -> [(Int, a)]
runLength []     = []
runLength (x:xs) =
  let (same, rest) = span (== x) xs
  in (1 + length same, x) : runLength rest

runLengthDecode :: [(Int, a)] -> [a]
runLengthDecode = concatMap (\(n, c) -> replicate n c)

main :: IO ()
main = do
  putStrLn "=== Prime Numbers ==="
  let primes50 = primesUpTo 50
  putStrLn ("Primes up to 50: " ++ show primes50)
  putStrLn ("Count: " ++ show (length primes50))

  putStrLn "\n=== GCD and LCM ==="
  let pairs = [(48,18),(100,75),(17,5),(144,60)]
  mapM_ (\(a,b) -> putStrLn ("gcd(" ++ show a ++ "," ++ show b ++ ")=" ++ show (myGcd a b)
                           ++ "  lcm=" ++ show (myLcm a b))) pairs

  putStrLn "\n=== Collatz Sequences ==="
  let starts = [6, 11, 27]
  mapM_ (\n -> putStrLn ("collatz(" ++ show n ++ "): length=" ++ show (length (collatz n))
                        ++ " seq=" ++ show (take 12 (collatz n)) ++ "...")) starts

  putStrLn "\n=== Caesar Cipher ==="
  let msg     = "Hello, Haskell!"
  let encoded = caesarEncode 13 msg
  let decoded = caesarEncode (-13) encoded
  putStrLn ("Original: " ++ msg)
  putStrLn ("ROT13:    " ++ encoded)
  putStrLn ("Decoded:  " ++ decoded)

  putStrLn "\n=== Run-Length Encoding ==="
  let s = "aaabbbccddddeeefff"
  let encoded2 = runLength s
  putStrLn ("Input:    " ++ s)
  putStrLn ("Encoded:  " ++ show encoded2)
  putStrLn ("Decoded:  " ++ runLengthDecode encoded2)
