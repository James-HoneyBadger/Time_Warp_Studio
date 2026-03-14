-- Parser Combinator Library in Haskell
-- Demonstrates: type classes, higher-order functions, pattern matching,
-- Maybe/Either, list comprehensions, where clauses

module ParserCombinator where

-- ══════════════════════════════════════
--   🔧 Parser Combinator Library
-- ══════════════════════════════════════

-- A Parser takes a string and returns possible (result, remaining) pairs
type Parser a = String -> [(a, String)]

-- === Primitive Parsers ===

-- Parse a single character matching a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p [] = []
satisfy p (c:cs)
    | p c       = [(c, cs)]
    | otherwise = []

-- Parse a specific character
char :: Char -> Parser Char
char c = satisfy (== c)

-- Parse a specific string
string :: String -> Parser String
string [] input = [("", input)]
string (c:cs) input = case satisfy (== c) input of
    [(_, rest)] -> case string cs rest of
        [(s, rest2)] -> [(c:s, rest2)]
        _ -> []
    _ -> []

-- Always succeed with a value
pure' :: a -> Parser a
pure' x input = [(x, input)]

-- Always fail
empty' :: Parser a
empty' _ = []

-- === Combinators ===

-- Sequential: run p1 then p2, combine results
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f input = [(b, rest2) | (a, rest1) <- p input, (b, rest2) <- f a rest1]

-- Alternative: try p1, if it fails try p2
alt :: Parser a -> Parser a -> Parser a
alt p1 p2 input = case p1 input of
    [] -> p2 input
    result -> result

-- Apply a function to parser result
fmap' :: (a -> b) -> Parser a -> Parser b
fmap' f p input = [(f a, rest) | (a, rest) <- p input]

-- Parse zero or more
many :: Parser a -> Parser [a]
many p = alt (many1 p) (pure' [])

-- Parse one or more
many1 :: Parser a -> Parser [a]
many1 p = bind p (\x -> fmap' (x:) (many p))

-- Parse with separator
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = alt (sepBy1 p sep) (pure' [])

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = bind p (\x -> fmap' (x:) (many (bind sep (\_ -> p))))

-- === Useful Parsers ===

digit :: Parser Char
digit = satisfy (\c -> c >= '0' && c <= '9')

letter :: Parser Char
letter = satisfy (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

space :: Parser Char
space = satisfy (== ' ')

spaces :: Parser String
spaces = many space

-- Parse a natural number
natural :: Parser Int
natural = fmap' read (many1 digit)
    where read cs = foldl (\acc c -> acc * 10 + digitToInt c) 0 cs
          digitToInt c = fromEnum c - fromEnum '0'

-- Parse an identifier (letter followed by alphanumerics)
identifier :: Parser String
identifier = bind letter (\c -> fmap' (c:) (many (alt letter digit)))

-- === Expression Parser (arithmetic) ===

-- expr   = term (('+' | '-') term)*
-- term   = factor (('*' | '/') factor)*
-- factor = number | '(' expr ')'

expr :: Parser Int
expr = bind term exprRest
    where
        exprRest x = alt
            (bind (char '+') (\_ -> bind term (\y -> exprRest (x + y))))
            (alt
                (bind (char '-') (\_ -> bind term (\y -> exprRest (x - y))))
                (pure' x))

term :: Parser Int
term = bind factor termRest
    where
        termRest x = alt
            (bind (char '*') (\_ -> bind factor (\y -> termRest (x * y))))
            (alt
                (bind (char '/') (\_ -> bind factor (\y -> termRest (x `div` y))))
                (pure' x))

factor :: Parser Int
factor = alt natural (bind (char '(') (\_ -> bind expr (\e -> fmap' (const e) (char ')'))))

-- === Test Runner ===

runParser :: Parser a -> String -> Maybe a
runParser p input = case p input of
    [(result, "")] -> Just result
    [(result, _)]  -> Just result
    _              -> Nothing

showResult :: Show a => String -> Parser a -> String -> String
showResult label p input =
    label ++ " \"" ++ input ++ "\" = " ++
    case runParser p input of
        Just x  -> show x ++ "  ✅"
        Nothing -> "FAIL ❌"

-- === Main ===
main :: IO ()
main = do
    putStrLn "╔══════════════════════════════════════╗"
    putStrLn "║  🔧 Parser Combinators in Haskell   ║"
    putStrLn "╚══════════════════════════════════════╝"
    putStrLn ""

    putStrLn "── Character Parsers ──"
    putStrLn (showResult "  char 'H'" (char 'H') "Hello")
    putStrLn (showResult "  digit"    digit       "42abc")
    putStrLn (showResult "  letter"   letter      "abc123")
    putStrLn ""

    putStrLn "── String Parsers ──"
    putStrLn (showResult "  string 'Hello'" (string "Hello") "Hello World")
    putStrLn (showResult "  identifier"     identifier       "myVar123 rest")
    putStrLn (showResult "  natural"        natural          "12345xyz")
    putStrLn ""

    putStrLn "── Combinators ──"
    putStrLn (showResult "  many digit"   (fmap' length (many digit))   "12345abc")
    putStrLn (showResult "  many1 letter" (fmap' length (many1 letter)) "hello123")
    putStrLn ""

    putStrLn "── Arithmetic Expressions ──"
    putStrLn (showResult "  expr" expr "2+3")
    putStrLn (showResult "  expr" expr "2+3*4")
    putStrLn (showResult "  expr" expr "(2+3)*4")
    putStrLn (showResult "  expr" expr "10-3+2")
    putStrLn (showResult "  expr" expr "100/5/4")
    putStrLn ""

    putStrLn "── CSV Parser ──"
    let csvField = many1 (satisfy (\c -> c /= ',' && c /= '\n'))
    let csvLine  = sepBy1 csvField (char ',')
    putStrLn (showResult "  csv" csvLine "name,age,city")
    putStrLn ""

    putStrLn "✅ Parser combinator tests complete!"
