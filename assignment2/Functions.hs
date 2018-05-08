type AlignmentType = (String, String)

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] (y:ys) = score '-' y + similarityScore [] ys
similarityScore (x:xs) [] = score x '-' + similarityScore xs []
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + score x y, 
                                        similarityScore xs (y:ys) + score x '-', 
                                        similarityScore (x:xs) ys + score '-' y]

score :: Char -> Char -> Int
score _ '-' = scoreSpace
score '-' _ = scoreSpace
score x y
    | x == y    = scoreMatch
    | otherwise = scoreMismatch

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]     
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy valueFcn xs = [tmp | tmp <- xs, valueFcn tmp == maxFuncVal]
    where maxFuncVal = maximum $ map valueFcn xs

optAlignments :: String -> String -> [AlignmentType]
optAlignments string1 string2 = maximaBy (\(string1, string2) -> calcScore string1 string2) list
    where list = generateStrings string1 string2

generateStrings :: String -> String -> [AlignmentType]
generateStrings [] [] = [([], [])]
generateStrings [] (y:ys) = attachHeads '-' y (generateStrings [] ys)
generateStrings (x:xs) [] = attachHeads x '-' (generateStrings xs [])
generateStrings (x:xs) (y:ys) = attachHeads x y (generateStrings xs ys) ++ 
                                attachHeads x '-' (generateStrings xs (y:ys)) ++ 
                                attachHeads '-' y (generateStrings (x:xs) ys)

calcScore :: String -> String -> Int                                
calcScore [] []         = 0
calcScore (x:xs) (y:ys) = score x y + calcScore xs ys
                                
-- outputOptAlignments string1 string2 = id


