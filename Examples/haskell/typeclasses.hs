-- Type Classes and Polymorphism in Haskell

module Main where

import Data.List (intercalate, nub, sort)

-- ── Custom type class ────────────────────────────────────────────────
class Describable a where
  describe :: a -> String

class (Show a) => Printable a where
  prettyPrint :: a -> IO ()
  prettyPrint = putStrLn . show

-- ── A Card game type ─────────────────────────────────────────────────
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Enum, Bounded)

data Rank = Two | Three | Four | Five | Six | Seven | Eight
          | Nine | Ten | Jack | Queen | King | Ace
          deriving (Eq, Ord, Enum, Bounded)

data Card = Card Rank Suit deriving (Eq)

instance Show Suit where
  show Clubs    = "♣"
  show Diamonds = "♦"
  show Hearts   = "♥"
  show Spades   = "♠"

instance Show Rank where
  show Ace   = "A"
  show King  = "K"
  show Queen = "Q"
  show Jack  = "J"
  show Ten   = "10"
  show r     = show (fromEnum r + 2)

instance Show Card where
  show (Card r s) = show r ++ show s

instance Ord Card where
  compare (Card r1 _) (Card r2 _) = compare r1 r2

instance Describable Card where
  describe (Card r s) = show r ++ " of " ++ show s

-- ── Full deck ────────────────────────────────────────────────────────
fullDeck :: [Card]
fullDeck = [Card r s | s <- [minBound..maxBound], r <- [minBound..maxBound]]

-- ── Functor-like operations ───────────────────────────────────────────
applyToAll :: (a -> b) -> [a] -> [b]
applyToAll = map

-- ── Demo ──────────────────────────────────────────────────────────────
main :: IO ()
main = do
  putStrLn "=== Type Classes Demo ==="

  let hand = [Card Ace Spades, Card King Hearts, Card Queen Diamonds,
              Card Jack Clubs, Card Ten Spades]
  putStrLn "\nHand:"
  mapM_ (\c -> putStrLn $ "  " ++ describe c) hand

  putStrLn $ "\nSorted hand: " ++ unwords (map show (sort hand))

  putStrLn $ "\nFull deck size: " ++ show (length fullDeck)
  putStrLn "First 8 cards:"
  putStrLn $ "  " ++ intercalate " " (map show (take 8 fullDeck))

  putStrLn "\n=== Bounded/Enum type class ==="
  putStrLn $ "All suits: " ++ unwords (map show [minBound..maxBound :: Suit])
  putStrLn $ "Ranks 2-6: " ++ unwords (map show (take 5 [minBound..maxBound :: Rank]))

  putStrLn "\n=== Num type class polymorphism ==="
  let nums = [1..10] :: [Int]
  putStrLn $ "sum [1..10]     = " ++ show (sum nums)
  putStrLn $ "product [1..10] = " ++ show (product nums)
  putStrLn $ "maximum         = " ++ show (maximum nums)
