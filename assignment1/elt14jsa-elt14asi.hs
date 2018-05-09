module Chatterbot where
  import Utilities
  import System.Random
  import Data.Char

  chatterbot :: String -> [(String, [String])] -> IO ()
  chatterbot botName botRules = do
      putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
      botloop
    where
      brain = rulesCompile botRules
      botloop = do
        putStr "\n: "
        question <- getLine
        answer <- stateOfMind brain
        putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
        if (not . endOfDialog) question then botloop else return ()

  --------------------------------------------------------

  type Phrase = [String]
  type PhrasePair = (Phrase, Phrase)
  type BotBrain = [(Phrase, [Phrase])]

  --------------------------------------------------------

  stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
  stateOfMind bb = do --ok
    r <- randomIO :: IO Float
    let tmp1 = fst $ unzip bb
        tmp2 = snd $ unzip bb
        tmp3 = map (pick r) tmp2
        patterns = zip tmp1 tmp3
    return (rulesApply patterns)

  rulesApply :: [PhrasePair] -> Phrase -> Phrase
  -- rulesApply pattern phrase
        -- | tmp == Nothing = words ""
        -- | otherwise      = words (try f (unwords phrase))
        -- where tmp = f (unwords phrase)
              -- f x = transformationsApply '*' (unwords . reflect . words) stringPattern' (map toLower x)
              -- stringPattern = phrasePairToStringPair pattern
              -- stringPattern' = map (\x -> map2 (map toLower, id) (fst x, snd x)) stringPattern
  rulesApply = (maybe [] id .) . transformationsApply "*" reflect -- that works but this is another way to make the AARGH test case work

  reflect :: Phrase -> Phrase
  reflect = (map.try) (flip lookup reflections) --nice

  reflections =
    [ ("am",     "are"),
      ("was",    "were"),
      ("i",      "you"),
      ("i'm",    "you are"),
      ("i'd",    "you would"),
      ("i've",   "you have"),
      ("i'll",   "you will"),
      ("my",     "your"),
      ("me",     "you"),
      ("are",    "am"),
      ("you're", "i am"),
      ("you've", "i have"),
      ("you'll", "i will"),
      ("your",   "my"),
      ("yours",  "mine"),
      ("you",    "me")
    ]

  ---------------------------------------------------------------------------------

  endOfDialog :: String -> Bool
  endOfDialog = (=="quit") . map toLower

  present :: Phrase -> String
  present = unwords

  prepare :: String -> Phrase
  prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

  rulesCompile :: [(String, [String])] -> BotBrain
  rulesCompile = (map . map2) (words, map words) --nice

  --------------------------------------

  reductions :: [PhrasePair]
  reductions = (map.map2) (words, words)
    [ ( "please *", "*" ),
      ( "can you *", "*" ),
      ( "could you *", "*" ),
      ( "tell me if you are *", "are you *" ),
      ( "tell me who * is", "who is *" ),
      ( "tell me what * is", "what is *" ),
      ( "do you know who * is", "who is *" ),
      ( "do you know what * is", "what is *" ),
      ( "are you very *", "are you *" ),
      ( "i am very *", "i am *" ),
      ( "hi *", "hello *")
    ]

  reduce :: Phrase -> Phrase
  reduce = fix $ reductionsApply reductions

  reductionsApply :: [PhrasePair] -> Phrase -> Phrase
  -- reductionsApply reductions =  words . try (transformationsApply '*' id $ phrasePairToStringPair reductions) . unwords --ok
  reductionsApply reductions =  fix $ try (transformationsApply "*" id reductions) --can be written like this and fix should also be used here

  --phrasePairToStringPair :: [([String], [String])] -> [(String, String)]
  --phrasePairToStringPair = (map . map2) (unwords, unwords)

  --stringPairToPhrasePair :: [(String, String)] -> [([String], [String])]
  --stringPairToPhrasePair = (map . map2) (words, words)

  -------------------------------------------------------
  -- Match and substitute
  --------------------------------------------------------

  substitute :: Eq a => a -> [a] -> [a] -> [a]
  substitute _ [] _ = [] -- try to avoid if statements and use guards and functions instead
  substitute wc (t:ts) s = (if t == wc then s else [t]) ++ (substitute wc ts s)

  match :: Eq a => a -> [a] -> [a] -> Maybe [a]
  match _ [] [] = Just []
  match _ [] _ = Nothing
  match _ _ [] = Nothing
  match wc (p:ps) (s:ss) -- avoid if, maybe use orElse
      | p /= wc   = if p == s then match wc ps ss else Nothing
      | otherwise = single `orElse` longer
      where single = singleWildcardMatch (p:ps) (s:ss)
            longer = longerWildcardMatch (p:ps) (s:ss)

  singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
  singleWildcardMatch (wc:ps) (x:xs) = if match wc ps xs == Nothing then Nothing else Just [x] -- avoid if, eg. use mmap

  longerWildcardMatch (wc:ps) (x:xs) -- use mmap instead
      | tmp == Nothing  = Nothing
      | otherwise       = mmap ([x] ++) tmp
      where tmp = match wc (wc:ps) xs

  -- Test cases --------------------

  testPattern =  "a=*;"
  testSubstitutions = "32"
  testString = "a=32;"

  substituteTest = substitute '*' testPattern testSubstitutions
  substituteCheck = substituteTest == testString

  matchTest = match '*' testPattern testString
  matchCheck = matchTest == Just testSubstitutions

  -------------------------------------------------------
  -- Applying patterns
  --------------------------------------------------------

  transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
  transformationApply wc f s pt = mmap (substitute wc (snd pt)) (mmap f (match wc (fst pt) s)) --nice

  transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
  transformationsApply _ _ [] _ = Nothing
  transformationsApply wc f (pt:pts) s --ok
    | tmp == Nothing   = transformationsApply wc f pts s
    | otherwise        = tmp
    where tmp = transformationApply wc f s pt
