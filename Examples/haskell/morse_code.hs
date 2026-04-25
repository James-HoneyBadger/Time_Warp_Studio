-- ============================================================
-- MORSE CODE — Haskell Language Showcase
-- Encode/decode Morse, binary tree lookup, ITU alphabet
-- Time Warp Studio — Haskell Language Demo
-- ============================================================

import Data.Char (toUpper, toLower, isAlpha, isDigit)
import Data.List (intercalate, isPrefixOf, sortBy)
import Data.Ord  (comparing)
import Data.Maybe (fromMaybe, mapMaybe)

-- ============================================================
-- MORSE CODE TABLE
-- ============================================================

morseAlphabet :: [(Char, String)]
morseAlphabet =
  [ ('A', ".-"),    ('B', "-..."),  ('C', "-.-."),  ('D', "-.."),
    ('E', "."),     ('F', "..-."),  ('G', "--."),   ('H', "...."),
    ('I', ".."),    ('J', ".---"),  ('K', "-.-"),   ('L', ".-.."),
    ('M', "--"),    ('N', "-."),    ('O', "---"),   ('P', ".--."),
    ('Q', "--.-"),  ('R', ".-."),   ('S', "..."),   ('T', "-"),
    ('U', "..-"),   ('V', "...-"),  ('W', ".--"),   ('X', "-..-"),
    ('Y', "-.--"),  ('Z', "--.."),
    ('0', "-----"), ('1', ".----"), ('2', "..---"), ('3', "...--"),
    ('4', "....-"), ('5', "....."), ('6', "-...."), ('7', "--..."),
    ('8', "---.."), ('9', "----."),
    ('.', ".-.-.-"), (',', "--..--"), ('?', "..--.."), ('!', "-.-.--"),
    ('-', "-....-"), ('/', "-..-.")
  ]

-- ============================================================
-- BINARY TREE FOR O(log n) DECODE
-- ============================================================

data MorseTree = Leaf | Node (Maybe Char) MorseTree MorseTree
  deriving (Show)

-- Build a tree from the morse table
-- dot = go left, dash = go right
insertMorse :: MorseTree -> String -> Char -> MorseTree
insertMorse Leaf [] c = Node (Just c) Leaf Leaf
insertMorse (Node x l r) [] c = Node (Just c) l r
insertMorse Leaf (s:ss) c
  | s == '.'  = Node Nothing (insertMorse Leaf ss c) Leaf
  | otherwise = Node Nothing Leaf (insertMorse Leaf ss c)
insertMorse (Node x l r) (s:ss) c
  | s == '.'  = Node x (insertMorse l ss c) r
  | otherwise = Node x l (insertMorse r ss c)

buildMorseTree :: MorseTree
buildMorseTree = foldr (\(c, code) t -> insertMorse t code c) Leaf morseAlphabet

-- Decode a morse code string using the tree
decodeMorseCode :: MorseTree -> String -> Maybe Char
decodeMorseCode tree [] = case tree of
  Node (Just c) _ _ -> Just c
  _                  -> Nothing
decodeMorseCode Leaf _ = Nothing
decodeMorseCode (Node _ l r) (s:ss)
  | s == '.'  = decodeMorseCode l ss
  | s == '-'  = decodeMorseCode r ss
  | otherwise  = Nothing

-- ============================================================
-- ENCODE / DECODE FUNCTIONS
-- ============================================================

-- Encode a single character to morse
encodeChar :: Char -> Maybe String
encodeChar c = lookup (toUpper c) morseAlphabet

-- Encode a word (space between letters)
encodeWord :: String -> String
encodeWord w = unwords $ mapMaybe encodeChar w

-- Encode a sentence (/ between words, space between letters)
encodeSentence :: String -> String
encodeSentence = intercalate " / " . map encodeWord . words

-- Decode a single morse symbol
decodeSym :: MorseTree -> String -> Maybe Char
decodeSym = decodeMorseCode

-- Decode a full morse string (".. -- .." / "--- ..-")
decodeSentence :: MorseTree -> String -> String
decodeSentence tree encoded =
  let wordGroups = splitOn "/" (words encoded)
      decodeWord ws = map (\sym -> fromMaybe '?' (decodeSym tree sym)) ws
  in unwords $ map (concatMap return . decodeWord) wordGroups

-- Split a list on a delimiter
splitOn :: Eq a => a -> [[a]] -> [[[a]]]
splitOn _ [] = [[]]
splitOn delim (x:xs)
  | x == [delim] = [] : splitOn delim xs
  | otherwise    = let (cur:rest) = splitOn delim xs
                   in (x:cur) : rest

-- ============================================================
-- TIMING ANALYSIS (dot/dash/space durations)
-- ============================================================

-- Morse timing ratios
-- dot = 1 unit, dash = 3 units
-- intra-char gap = 1 unit, inter-char gap = 3 units, word gap = 7 units

data MorseElement = Dot | Dash | IntraGap | InterGap | WordGap
  deriving (Show, Eq)

morseTiming :: String -> [(MorseElement, Int)]
morseTiming [] = []
morseTiming (c:cs)
  | c == '.'  = (Dot, 1) : gaps cs
  | c == '-'  = (Dash, 3) : gaps cs
  | otherwise = morseTiming cs
  where
    gaps [] = []
    gaps (x:xs)
      | x == ' ' = (InterGap, 3) : morseTiming xs
      | otherwise = (IntraGap, 1) : morseTiming (x:xs)

totalDuration :: String -> Int
totalDuration = sum . map snd . morseTiming

-- ============================================================
-- MORSE CODE STATISTICS
-- ============================================================

dotDashRatio :: String -> (Int, Int)
dotDashRatio s = (length $ filter (=='.') s, length $ filter (=='-') s)

morseComplexity :: Char -> Int
morseComplexity c = maybe 0 length (encodeChar c)

-- Most and least complex letters
lettersByComplexity :: [(Char, String, Int)]
lettersByComplexity =
  sortBy (comparing (\(_,_,n) -> n))
  [ (c, code, length code) | (c, code) <- filter (\(c,_) -> isAlpha c) morseAlphabet ]

-- ============================================================
-- PRETTY PRINTING
-- ============================================================

printMorseTable :: IO ()
printMorseTable = do
  putStrLn "  A-Z Morse Code Reference:"
  putStrLn "  ┌─────┬──────────┬────┬─────────────┐"
  putStrLn "  │ Chr │ Morse    │ Chr│ Morse       │"
  putStrLn "  ├─────┼──────────┼────┼─────────────┤"
  let pairs = zip (take 13 morseAlphabet) (drop 13 $ take 26 morseAlphabet)
  mapM_ (\((c1,m1),(c2,m2)) ->
    putStrLn $ "  │  " ++ [c1] ++ "  │ " ++ padR 8 m1 ++ " │ "
             ++ [c2] ++ "  │ " ++ padR 11 m2 ++ " │"
    ) pairs
  putStrLn "  └─────┴──────────┴────┴─────────────┘"

padR :: Int -> String -> String
padR n s = s ++ replicate (n - length s) ' '

padL :: Int -> String -> String
padL n s = replicate (n - length s) ' ' ++ s

-- ============================================================
-- MAIN
-- ============================================================

main :: IO ()
main = do
  let tree = buildMorseTree

  putStrLn "============================================================"
  putStrLn "  MORSE CODE — Haskell Showcase"
  putStrLn "  Binary tree decode, encode, timing analysis"
  putStrLn "============================================================"
  putStrLn ""

  -- Section 1: Morse table
  putStrLn "SECTION 1: ITU MORSE CODE REFERENCE TABLE"
  putStrLn "------------------------------------------------------------"
  printMorseTable
  putStrLn ""

  -- Section 2: Encode messages
  putStrLn "SECTION 2: ENCODING MESSAGES"
  putStrLn "------------------------------------------------------------"
  let messages = [ "SOS", "HELLO WORLD", "HASKELL", "TIME WARP STUDIO",
                   "THE QUICK BROWN FOX" ]
  mapM_ (\msg -> do
    let encoded = encodeSentence msg
    putStrLn $ "  Original: " ++ msg
    putStrLn $ "  Morse:    " ++ encoded
    let (dots, dashes) = dotDashRatio encoded
    putStrLn $ "  Dots: " ++ show dots ++ "  Dashes: " ++ show dashes
    putStrLn ""
    ) messages

  -- Section 3: Decode messages
  putStrLn "SECTION 3: DECODING MORSE"
  putStrLn "------------------------------------------------------------"
  let morseMsgs =
        [ "... --- ..."
        , ".... . .-.. .-.. ---"
        , "-- --- .-. ... ."
        , ".-- --- .-. .-.. -.."
        , ".... .- ... -.- . .-.. .-.. "
        ]
  mapM_ (\morse -> do
    let decoded = concatMap (\sym -> maybe "?" return (decodeSym tree sym)) (words morse)
    putStrLn $ "  Morse:   " ++ morse
    putStrLn $ "  Decoded: " ++ decoded
    putStrLn ""
    ) morseMsgs

  -- Section 4: Letter complexity
  putStrLn "SECTION 4: LETTER COMPLEXITY (by code length)"
  putStrLn "------------------------------------------------------------"
  putStrLn "  Simplest letters first:"
  mapM_ (\(c, code, n) ->
    putStrLn $ "    " ++ [c] ++ " : " ++ padR 6 code ++ " (" ++ show n ++ " elements)"
    ) (take 10 lettersByComplexity)
  putStrLn "  Most complex letters:"
  mapM_ (\(c, code, n) ->
    putStrLn $ "    " ++ [c] ++ " : " ++ padR 6 code ++ " (" ++ show n ++ " elements)"
    ) (reverse $ drop (length lettersByComplexity - 10) lettersByComplexity)
  putStrLn ""

  -- Section 5: Timing analysis
  putStrLn "SECTION 5: TIMING ANALYSIS"
  putStrLn "------------------------------------------------------------"
  putStrLn "  Morse timing units: dot=1, dash=3, intra=1, inter=3, word=7"
  putStrLn ""
  let timingMsgs = ["SOS", "HELLO", "THE"]
  mapM_ (\msg -> do
    let encoded = encodeSentence msg
    let duration = totalDuration encoded
    putStrLn $ "  '" ++ msg ++ "' => " ++ encoded
    putStrLn $ "    Duration (in unit-dots): " ++ show duration
    putStrLn ""
    ) timingMsgs

  -- Section 6: Encode/decode round-trip test
  putStrLn "SECTION 6: ROUND-TRIP ENCODE/DECODE VERIFICATION"
  putStrLn "------------------------------------------------------------"
  let testWords = ["HASKELL", "MORSE", "FUNCTIONAL", "TREE"]
  let allPass = all (\w ->
        let encoded = encodeSentence w
            decoded = map (\sym -> fromMaybe '?' (decodeSym tree sym)) (words encoded)
        in concat decoded == w
        ) testWords
  mapM_ (\w -> do
    let encoded = encodeSentence w
    let decoded = map (\sym -> fromMaybe '?' (decodeSym tree sym)) (words encoded)
    let pass = concat decoded == w
    putStrLn $ "  " ++ w ++ " => decode(encode) = " ++ concat decoded
             ++ if pass then " ✓" else " ✗ MISMATCH"
    ) testWords
  putStrLn ""
  putStrLn $ "  All round-trip tests passed: " ++ show allPass
  putStrLn ""

  -- Section 7: Number encoding
  putStrLn "SECTION 7: NUMBER ENCODING"
  putStrLn "------------------------------------------------------------"
  mapM_ (\n -> do
    let encoded = encodeWord (show n)
    putStrLn $ "  " ++ show n ++ " => " ++ encoded
    ) ([0..9] :: [Int])
  putStrLn ""

  putStrLn "============================================================"
  putStrLn "  Morse Code complete!"
  putStrLn "  Binary tree decode * ITU alphabet * Timing * Round-trip"
  putStrLn "============================================================"
