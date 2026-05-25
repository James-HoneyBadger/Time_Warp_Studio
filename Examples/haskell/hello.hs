-- Hello World in Haskell
-- Demonstrates basic I/O and string operations

module Main where

main :: IO ()
main = do
  putStrLn "Hello, World!"
  let name = "Haskell"
  putStrLn ("Welcome to " ++ name ++ "!")
  let year = 1990
  putStrLn ("Haskell was created around " ++ show year)
  let greeting = greet "Alice"
  putStrLn greeting
  putStrLn (greet "Bob")
  putStrLn (greet "Charlie")

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"
