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
  rulesApply pattern phrase
        | tmp == Nothing = words ""
        | otherwise      = words (try f (unwords phrase))
        where tmp = f (unwords phrase)
              f x = transformationsApply '*' (unwords . reflect . words) stringPattern' (map toLower x)
              stringPattern = phrasePairToStringPair pattern
              stringPattern' = map (\x -> map2 (map toLower, id) (fst x, snd x)) stringPattern

  phrasePairToStringPair :: [([String], [String])] -> [(String, String)]
  phrasePairToStringPair = (map . map2) (unwords, unwords)

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
  
  -- fixed
  reductionsApply :: [PhrasePair] -> Phrase -> Phrase
  reductionsApply reductions =  fix $ try (transformationsApply "*" id reductions)

  -------------------------------------------------------
  -- Match and substitute
  --------------------------------------------------------

  -- fixed
  substitute :: Eq a => a -> [a] -> [a] -> [a]
  substitute _ [] _ = []
  substitute wc (t:ts) s
    | t == wc   = s ++ substitute wc ts s
    | otherwise = [t] ++ substitute wc ts s
  
  -- fixed
  match :: Eq a => a -> [a] -> [a] -> Maybe [a]
  match _ [] [] = Just []
  match _ [] _ = Nothing
  match _ _ [] = Nothing
  match wc (p:ps) (s:ss)
      | p == wc   = orElse single longer
      | p == s    = match wc ps ss
      | otherwise = Nothing
      where single = singleWildcardMatch (p:ps) (s:ss)
            longer = longerWildcardMatch (p:ps) (s:ss)
  
  -- fixed          
  singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
  singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) (match wc ps xs)
  -- fixed      
  longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)

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
